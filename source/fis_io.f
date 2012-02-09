Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $

      SUBROUTINE INPFIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER :: NEXtr, NPOints
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8 :: bb2, bb3, bb4, rmiu
      INTEGER :: bfi, i, ia, iarr, ib, ibar, ih, ii, iw, iz, izrr, j, k, 
     &           ka, kz, m, nh, nr, nrmod, nrsm
      REAL*8 :: centr, heigth, ucentr, uheigth, uwidth
      CHARACTER(1), DIMENSION(70) :: chstar
      REAL :: dd
      CHARACTER(50) :: filename
      INTEGER :: FIND_EXTREM
      INTEGER :: INT
      REAL*8, DIMENSION(NFParab) :: width
C
C*** End of declarations rewritten by SPAG
C
C Creates fission.inp  which contains all the fission
C parameters independent of energy.
C
      DATA chstar/70*'='/
 
      iz = INT(Z(Nnuc))
      ia = INT(A(Nnuc))
      nrsm = NRSmooth(Nnuc)
 
      DO i = 1, NFParab
        EFB(i) = 0.D0
      ENDDO
      DO ih = 1, NFHump
        ROHfbp_sd(ih) = 0.D0
        ROHfba_sd(ih) = 0.D0
        ROHfb_norm(ih) = 1.D0
        BARnorm(ih) = 0.D0
      ENDDO
      HNOrm = 1.D0
      NRBar = 0
      NRWel = 0
      NRHump = 0
C-----Fundamental barrier heights
C-----FISBAR(Nnuc)=0 EMPIRE
C-----FISBAR(Nnuc)=1 Maslov
C-----FISBAR(Nnuc)=2 HFB parabolic
C-----FISBAR(Nnuc)=3 HFB numeric
C
C-----FISBAR(Nnuc)= 0 Empire internal library
      IF(FISbar(Nnuc).EQ.0.)THEN
        OPEN(81,FILE = TRIM(EMPiredir)//'/data/EMPIRE-fisbar.dat',
     &       STATUS = 'OLD',ERR = 10)
        READ(81,*,ERR = 10,END = 10)
        READ(81,*,ERR = 10,END = 10)
        READ(81,*,ERR = 10,END = 10)
        READ(81,*,ERR = 10,END = 10)
    5   READ(81,*,ERR = 10,END = 10)kz, ka, NRBar, NRWel, 
     &                              (EFB(i),H(1,i),i = 1,NRBar)
        IF(kz.NE.INT(Z(Nnuc)).OR.ka.NE.INT(A(Nnuc)))GOTO 5
        CLOSE(81)
        GOTO 30
   10   WRITE(8,*)' ERROR: NO fission barrier for Z=', INT(Z(Nnuc)), 
     &            ' A=', INT(A(Nnuc)), 
     &           ' in internal EMPIRE library (/data/EMPIRE-fisbar.dat)'
        WRITE(8,*)
     &    ' ERROR: or the file ../data/EMPIRE-fisbar.dat may be missing'
        WRITE(8,*)
     &' ERROR: You may use RIPL barriers (FISBAR 1) instead of local fis
     &sion barriers (FISBAR 0) in your input file'
        STOP ' FATAL: Internal fission barriers can not be retrieved'
      ENDIF
C
C-----FISBAR(Nnuc)= 1 RIPL "empirical" values for humps' heights and widths
C-----wells' parameters provided by code
      IF(FISbar(Nnuc).EQ.1.)THEN
        OPEN(52,FILE = TRIM(EMPiredir)//'/RIPL/fission'//
     &       '/empirical-barriers.dat',STATUS = 'OLD',ERR = 20)
        READ(52,*,ERR = 20,END = 20)
        READ(52,*,ERR = 20,END = 20)
        READ(52,*,ERR = 20,END = 20)
        READ(52,*,ERR = 20,END = 20)
   15   READ(52,'(2i4,1x,a2,1x,2(3x,a2,2f8.2))',ERR = 20,END = 20)kz, 
     &       ka, dd, dd, EFB(1), H(1,1), dd, EFB(2), H(1,2)
                                                  !,f9.3)'
C         write (*,'(2i4,1x,a2,1x,2(3x,a2,2f8.2))' )!,f9.3)'
C     &     kz, ka, dd,dd,EFB(1),H(1,1),
C     &     dd,EFB(2),H(1,2)
        IF(kz.NE.INT(Z(Nnuc)).OR.ka.NE.INT(A(Nnuc)))GOTO 15
        CLOSE(52)
 
        NRBar = 2
        IF(EFB(2).EQ.0.)THEN
          H(1,1) = 0.3D0
          NRBar = 1
          NRWel = 0
        ENDIF
        GOTO 30
   20   WRITE(8,*)' ERROR: NO fission barrier for Z=', INT(Z(Nnuc)), 
     &            ' A=', INT(A(Nnuc)), 
     &            ' in RIPL empirical fission library'
        WRITE(8,*)
     &' ERROR: or the file ../RIPL/fission/empirical-barriers.dat may be
     & missing'
        WRITE(8,*)
     &' ERROR: You may use local barriers (FISBAR 0) instead of RIPL bar
     &riers (FISBAR 1) in your input file'
        STOP ' FATAL: Fission barriers can not be retrieved'
      ENDIF
C
C-----adding default parameters of the isomeric well for double-humped barrier if missing
C
   30 IF(NRBar.EQ.2)THEN
        EFB(3) = 2.D0
        H(1,3) = 1.D0
        NRBar = 3
        NRWel = 1
      ENDIF
C
C-----FISBAR(Nnuc)=2  HFB microscopic parameters for parabolic barriers
C-----                extracted from HFB l.d.files and stored in
C-----                ../data/HFB-fisbar.dat (default)
      IF(FISbar(Nnuc).EQ.2.)THEN
        OPEN(81,FILE = TRIM(EMPiredir)//'/data/HFB-parab-fisbar.dat',
     &       STATUS = 'OLD',ERR = 40)
        READ(81,*,ERR = 40,END = 40)
        READ(81,*,ERR = 40,END = 40)
        READ(81,*,ERR = 40,END = 40)
        READ(81,*,ERR = 40,END = 40)
   35   READ(81,*,ERR = 40,END = 40)kz, ka, NRBar, NRWel, 
     &                              (EFB(i),H(1,i),i = 1,NRBar)
        IF(kz.NE.INT(Z(Nnuc)).OR.ka.NE.INT(A(Nnuc)))GOTO 35
        CLOSE(81)
        GOTO 50
   40   WRITE(8,*)' ERROR: NO fission barriers FOR Z=', iz, ' A=', ia, 
     &            ' in file ../data/HFB-parab-fisbar.dat'
        WRITE(8,*)
     &' ERROR: You may use RIPL barriers (FISBAR 1) instead of parabolic
     & approximation of HFB barriers (FISBAR 2) in your input file'
        STOP 
     &     ' FATAL: No fission barriers in ../data/HFB-parab-fisbar.dat'
      ENDIF
 
   50 NRHump = NRBar - NRWel
      DO i = 1, NRBar
        HCOnt(i) = H(1,i)
      ENDDO
      CALL DEFO_FIS(Nnuc)
C
C-----FISBAR(Nnuc)=3.  RIPL-3 HFB numerical barriers-------------------
      IF(FISbar(Nnuc).EQ.3.)THEN
        WRITE(filename,1010)iz
 1010   FORMAT('/RIPL/fission/HFB2007/z',i3.3,'.tab')
        OPEN(UNIT = 52,FILE = TRIM(EMPiredir)//TRIM(filename),
     &       STATUS = 'old',ERR = 60)
   55   READ(52,*,ERR = 60,END = 60)izrr, iarr, NPOints
C
C        When the mass number is smaller than that of all existing barriers,
C         the barrier of smallest mass number is used (BVC, MS - 05/10)
C         if(izrr.ne.iz .or. iarr.ne.ia) then
C
        IF(izrr.NE.iz.OR.iarr.LT.ia)THEN
          DO ii = 1, NPOints
            READ(52,*,ERR = 60,END = 60)
          ENDDO
          GOTO 55
        ENDIF
        IF(ia.LT.iarr)THEN
          WRITE(8,*)'WARNING: NO RIPL HFB fission barriers FOR Z=', iz, 
     &              ' A=', ia, ' in ../RIPL/fission/HFB2007 '
          WRITE(8,*)'WARNING: Using HFB numerical barrier FOR Z=', iz, 
     &              ' A=', iarr
        ENDIF
        DO ii = 1, NPOints
          READ(52,*,END = 65)bb2, bb3, bb4, VDEf_1d(ii)
          EPS_1d(ii) = bb2
        ENDDO
        GOTO 65
   60   WRITE(8,*)
     &' ERROR: You may use RIPL barriers (FISBAR 1) instead of HFB BARRI
     &ERS (FISBAR 3) in your input file'
        WRITE(8,*)' ERROR: NO fission barriers FOR Z=', iz, ' A=', ia, 
     &            ' in ../RIPL/fission/HFB2007 '
        STOP 
     &     ' FATAL: HFB numerical fission barriers can not be retrieved'
   65   CLOSE(52)
 
        NEXtr = FIND_EXTREM(Nnuc)
 
        NRHump = NEXtr/2 + 1
        NRWel = NEXtr/2
        NRBar = NRHump + NRWel
C-------Fitting parabola
        rmiu = 0.054D0*A(Nnuc)**(5.D0/3.D0)
        DO j = 1, NEXtr
          CALL PARABFIT(IIExtr(j),nrsm,rmiu,EPS_1d,VDEf_1d,centr,heigth,
     &                  width(j),ucentr,uheigth,uwidth)
          IF(width(j).LT.0.05D0)THEN    ! Skipping very narrow peaks
          ENDIF
        ENDDO
        DO k = 1, NRBar, 2
          EFB(INT(k/2) + 1) = VDEf_1d(IIExtr(k))
          H(1,INT(k/2) + 1) = width(k)
          DEFfis(INT(k/2) + 1) = EPS_1d(IIExtr(k))
        ENDDO
        DO k = 2, NRBar, 2
          EFB(NRHump + INT(k/2)) = VDEf_1d(IIExtr(k))
          H(1,NRHump + INT(k/2)) = width(k)
          DEFfis(NRHump + INT(k/2)) = EPS_1d(IIExtr(k))
        ENDDO
      ENDIF
C
C-----Default value for curvatures and protection !!
C
      DO i = 1, NRBar
        IF(H(1,i).EQ.0)H(1,i) = 1.
      ENDDO
C
C----------------------input fundamental fission barrier *** done
C------- discrete barriers---------------------------------------
C
      DO ibar = 1, NRBar
        EFDis(1,ibar) = 0.
        IF(A(Nnuc)/2.EQ.INT(A(Nnuc)/2))THEN
          SFDis(1,ibar) = 0.0
        ELSE
          SFDis(1,ibar) = 0.5
        ENDIF
        IF(XJLv(1,Nnuc).GE.0.)SFDis(1,ibar) = XJLv(1,Nnuc)
        IPFdis(1,ibar) = 1
        IF(LVP(1,Nnuc).NE.IPFdis(1,ibar))IPFdis(1,ibar) = LVP(1,Nnuc)
        NRFdis(ibar) = 1
      ENDDO
C
C-----Maslov's selected transition bandheads for humps
      IF(FISdis(Nnuc).EQ.1.)THEN
        IF(A(Nnuc)/2.EQ.INT(A(Nnuc)/2))THEN
C-----------even-even
          IF(Z(Nnuc)/2.EQ.INT(Z(Nnuc)/2))THEN
            DO ibar = 1, NRBar
              NRFdis(ibar) = 4
            ENDDO
            EFDis(1,1) = 0.0
            EFDis(2,1) = 0.1
            EFDis(3,1) = 0.4
            EFDis(4,1) = 0.4
            EFDis(1,2) = 0.0
            EFDis(2,2) = 0.5
            EFDis(3,2) = 0.2
            EFDis(4,2) = 0.5
            DO ibar = 1, NRBar
              SFDis(1,ibar) = 0.
              IPFdis(1,ibar) = 1
              SFDis(2,ibar) = 2.
              IPFdis(2,ibar) = 1
              SFDis(3,ibar) = 0.
              IPFdis(3,ibar) = -1
              SFDis(4,ibar) = 1.
              IPFdis(4,ibar) = -1
            ENDDO
          ENDIF
C-----------odd-odd
          IF(Z(Nnuc)/2.NE.INT(Z(Nnuc)/2))THEN
            DO ibar = 1, NRBar
              NRFdis(ibar) = 7
            ENDDO
            DO ib = 1, 2
              EFDis(1,ib) = 0.0
              EFDis(2,ib) = 0.044
              EFDis(3,ib) = 0.049
              EFDis(4,ib) = 0.170
              EFDis(5,ib) = 0.220
              EFDis(6,ib) = 0.242
              EFDis(7,ib) = 0.288
            ENDDO
            DO ibar = 1, NRBar
              SFDis(1,ibar) = 1.
              IPFdis(1,ibar) = -1
              SFDis(2,ibar) = 0.
              IPFdis(2,ibar) = -1
              SFDis(3,ibar) = 5.
              IPFdis(3,ibar) = -1
              SFDis(4,ibar) = 6.
              IPFdis(4,ibar) = -1
              SFDis(5,ibar) = 1.
              IPFdis(5,ibar) = -1
              SFDis(6,ibar) = 3.
              IPFdis(6,ibar) = -1
              SFDis(7,ibar) = 2.
              IPFdis(7,ibar) = -1
            ENDDO
          ENDIF
        ENDIF
        IF(A(Nnuc)/2.NE.INT(A(Nnuc)/2))THEN
C-----------even-odd
          IF(Z(Nnuc)/2.EQ.INT(Z(Nnuc)/2))THEN
            DO ibar = 1, NRBar
              NRFdis(ibar) = 4
            ENDDO
            EFDis(1,1) = 0.0
            EFDis(2,1) = 0.08
            EFDis(3,1) = 0.05
            EFDis(4,1) = 0.01
            EFDis(1,2) = 0.0
            EFDis(2,2) = 0.01
            EFDis(3,2) = 0.01
            EFDis(4,2) = 0.08
            DO ibar = 1, NRBar
              SFDis(1,ibar) = 0.5
              IPFdis(1,ibar) = 1
              SFDis(2,ibar) = 2.5
              IPFdis(2,ibar) = 1
              SFDis(3,ibar) = 0.5
              IPFdis(3,ibar) = -1
              SFDis(4,ibar) = 1.5
              IPFdis(4,ibar) = -1
            ENDDO
          ENDIF
C-----------odd-even
          IF(Z(Nnuc)/2.NE.INT(Z(Nnuc)/2))THEN
            DO ibar = 1, NRBar
              NRFdis(ibar) = 3
            ENDDO
            EFDis(1,1) = 0.0
            EFDis(2,1) = 0.140
            EFDis(3,1) = 0.180
            EFDis(1,2) = 0.08
            EFDis(2,2) = 0.0
            EFDis(3,2) = 0.0
            DO ibar = 1, NRBar
              SFDis(1,ibar) = 1.5
              IPFdis(1,ibar) = -1
              SFDis(2,ibar) = 2.5
              IPFdis(2,ibar) = 1
              SFDis(3,ibar) = 2.5
              IPFdis(3,ibar) = -1
            ENDDO
          ENDIF
        ENDIF
C-------default excitation energies in well
        DO j = 1, NRFdis(3)
          EFDis(j,3) = 0.0
        ENDDO
      ENDIF
C-----by default all widths are equal
      DO ibar = 1, NRBar
        DO k = 1, NRFdis(ibar)
          H(k,ibar) = H(1,ibar)
        ENDDO
      ENDDO
 
 
 
C-----h**2/2J from RIPL
      IF(NRHump.EQ.1)HJ(Nnuc,1) = 0.005
      IF(NRBar.EQ.2)THEN
        HJ(Nnuc,1) = 0.0050
        HJ(Nnuc,2) = 0.0025
      ENDIF
      IF(NRBar.EQ.3)THEN
        HJ(Nnuc,1) = 0.0050
        HJ(Nnuc,2) = 0.0025
        HJ(Nnuc,3) = 0.0035
      ENDIF
      IF(NRBar.EQ.5)THEN
        HJ(Nnuc,1) = 0.0050
        HJ(Nnuc,2) = 0.0025
        HJ(Nnuc,3) = 0.0017
        HJ(Nnuc,4) = 0.0035
        HJ(Nnuc,5) = 0.0020
      ENDIF
      DO iw = 1, NRWel
        WIMag(iw,1) = 0.02
        WIMag(iw,2) = 0.0001
        WIMag(iw,3) = 0.02
      ENDDO
 
 
C================  level densities at saddles  ===============================
C-----nuclear shape asymmetry at saddles
C           bff=1 axial, mass symmetry
C           bff=2 axial asymmetry,mass symmetry
C           bff=3 axial symmetry,mass asymmetry
C           bff=4 axial asymmetry,mass asymmetry
      DO ih = 1, NRHump
        BFF(ih) = 1
      ENDDO
      IF(A(Nnuc) - Z(Nnuc).GE.144)BFF(1) = 2
      BFF(2) = 3
      BFF(3) = 3
C-----shell corrections at saddles according to RIPL-2
      SHCfis(1) = 2.6
      IF(Z(Nnuc).GT.97.)SHCfis(1) = 2.6 - 0.1*(Z(Nnuc) - 97.)
      SHCfis(2) = 0.6 + 0.1*(Z(Nnuc) - 97.)
     &            + 0.04*(A(Nnuc) - Z(Nnuc) - 143.)
      IF(Z(Nnuc).GT.97.)SHCfis(2) = 0.6 + 0.04*(A(Nnuc) - Z(Nnuc) - 143.
     &                              )
      SHCfis(3) = SHCfis(2)
      DO ib = 1, NRHump
C--------energy shift
        DELtafis(ib) = 0.300       !14./SQRT(A(Nnuc))
        GAMmafis(ib) = 0.6D0  !Gamma
C--------multiplier of atil
        AFIs(ib) = 1.D0
        ECFis(ib) = 0.D0
        VIBf12(ib) = 1.D0
        VIBfdt(ib) = 0.1D0
        VIBfnorm(ib) = 1.D0
      ENDDO
 
      IF(FISmod(Nnuc).GT.0)THEN
        EFBm(1) = EFB(2) + 2.0
        EFBm(2) = EFB(2)
        EFBm(3) = EFB(2) + 0.1
        HM(1,1) = 1.2
        HM(1,2) = H(1,1)
        HM(1,3) = H(1,1)
        BFFm(1) = 1.
        BFFm(2) = 2.
        BFFm(3) = 2.
        DO m = 1, INT(FISmod(Nnuc)) + 1
          DO nr = 1, NRFdis(2)
            EFDism(nr,m) = EFDis(nr,2)
          ENDDO
          SHCfism(m) = SHCfis(2)
          DELtafism(m) = DELtafis(2)
          GAMmafism(m) = GAMmafis(2)
          AFIsm(m) = AFIs(2)
          ECFism(m) = ECFis(2)
          VIBf12m(m) = VIBf12(2)
          VIBfdtm(m) = VIBfdt(2)
          VIBfnormm(m) = VIBfnorm(2)
        ENDDO
      ENDIF
C=================================================================
C---- writing data in FISSION.INP
      WRITE(79,'(a8)')'Isotope:'
      WRITE(79,'(a40)')'----------------------------------------'
      WRITE(79,'(4x,a2,i3,2x,a2,i3)')'Z=', INT(Z(Nnuc)), 'A=', 
     &                               INT(A(Nnuc))
      WRITE(79,'(a40)')'----------------------------------------'
      WRITE(79,*)' '
C
      IF(FISbar(Nnuc).EQ.3.)THEN
        WRITE(79,*)chstar
        WRITE(79,*)' RIPL-3 HFB numerical fission barrier'
        WRITE(79,*)chstar
        WRITE(79,'(i3)')NPOints
        DO ii = 1, NPOints
          WRITE(79,'(i4,2f10.3)')ii, VDEf_1d(ii), EPS_1d(ii)
        ENDDO
        WRITE(79,*)
        WRITE(79,'(a15,i1,a15,i1)')' Nr.parabolas =', NRBar, 
     &                             '      Nr.wells=', NRWel
        NRHump = NRBar - NRWel
        NEXtr = NRBar
        WRITE(79,*)' '
        WRITE(79,*)' Index of extrema'
        WRITE(79,*)(IIExtr(j),j = 1,NRBar)
        WRITE(79,*)' Normalization factors for humps'
        IF(NRHump.EQ.1)WRITE(79,'(2f10.3)')(BARnorm(nh),nh = 1,NRHump), 
     &                       HNOrm
        IF(NRHump.EQ.2)WRITE(79,'(3f10.3)')(BARnorm(nh),nh = 1,NRHump), 
     &                       HNOrm
        IF(NRHump.EQ.3)WRITE(79,'(4f10.3)')(BARnorm(nh),nh = 1,NRHump), 
     &                       HNOrm
        WRITE(79,*)chstar
      ENDIF
C
      IF(FISbar(Nnuc).LT.3)THEN
        WRITE(79,'(a15,i1,a15,i1)')' Nr.parabolas =', NRBar, 
     &                             '      Nr.wells=', NRWel
        NRHump = NRBar - NRWel
        WRITE(79,*)
      ENDIF
 
      IF(NRBar.EQ.1)THEN
        WRITE(79,'(a)')'    Va      ha    (in Mev) '
        WRITE(79,'(2f8.3)')EFB(1), HCOnt(1)
        WRITE(79,*)' '
        WRITE(79,'(2a10)')'h2/2J(A)', '(in MeV)'
        WRITE(79,'(f9.4)')HJ(Nnuc,1)
        WRITE(79,*)' '
        WRITE(79,'(a10)')'Beta2(A)'
        WRITE(79,'(f9.4)')DEFfis(1)
        WRITE(79,*)' '
      ENDIF
 
      IF(NRBar.EQ.3)THEN
        IF(FISmod(Nnuc).EQ.0.)THEN
          WRITE(79,'(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
          WRITE(79,'(6f8.3,15x)')(EFB(i),H(1,i),i = 1,NRBar)
        ENDIF
        IF(FISmod(Nnuc).EQ.1.)THEN
          WRITE(79,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '    Vi      hi  (in Mev) '
          WRITE(79,'(8f9.3,15x)')EFB(1), H(1,1), EFBm(1), HM(1,1), 
     &                           EFBm(2), HM(1,2), EFB(3), H(1,3)
        ENDIF
        IF(FISmod(Nnuc).EQ.2.)THEN
          WRITE(79,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)   Vi      hi  (in Mev) '
          WRITE(79,'(10f9.3,15x)')EFB(1), H(1,1), EFBm(1), HM(1,1), 
     &                            EFBm(2), HM(1,2), EFBm(3), HM(1,3), 
     &                            EFB(3), H(1,3)
        ENDIF
        WRITE(79,*)' '
        WRITE(79,'(4a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)', '(in MeV)'
        WRITE(79,'(3f9.4)')(HJ(Nnuc,i),i = 1,NRBar)
        WRITE(79,*)' '
        WRITE(79,'(3a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
        WRITE(79,'(3f9.4)')(DEFfis(i),i = 1,NRBar)
        WRITE(79,*)'  '
      ENDIF
 
      IF(NRBar.EQ.5)THEN
        WRITE(79,'(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi      hi  
     &    Vo      ho  (in Mev) '
        WRITE(79,'(10f8.3,15x)')(EFB(i),H(1,i),i = 1,NRBar)
        WRITE(79,*)' '
        WRITE(79,'(6a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)', 
     &                    'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
        WRITE(79,'(5f9.4)')(HJ(Nnuc,i),i = 1,NRBar)
        WRITE(79,*)' '
        WRITE(79,'(6a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(C)', 
     &                    'Beta2(I)', 'Beta2(O)', '        '
        WRITE(79,'(5f9.4)')(DEFfis(i),i = 1,NRBar)
        WRITE(79,*)' '
      ENDIF
 
      WRITE(79,*)chstar
      WRITE(79,*)
     &    ' Parameters of the imaginary potential used only if FISOPT>0'
      WRITE(79,*)chstar
      WRITE(79,*)'      W0         W1         W2'
      DO iw = 1, NRWel
        WRITE(79,'(3f11.4)')(WIMag(iw,i),i = 1,3)
      ENDDO
      WRITE(79,*)
      WRITE(79,*)chstar
      WRITE(79,*)' Discrete transitional states'
      WRITE(79,*)chstar
C
      DO ibar = 1, NRBar
        IF(ibar.LE.NRHump)THEN
          WRITE(79,'(a39,I2,a2,I2)')
     &                            ' Number  of  discrete states at hump'
     &                            , ibar, ' =', NRFdis(ibar)
        ELSE
          WRITE(79,'(a39,I2,a2,I2)')
     &                            ' Number  of  discrete states in well'
     &                            , ibar, ' =', NRFdis(ibar)
        ENDIF
        WRITE(79,*)'Kdis  Pidis   Edis    homega'
        DO nr = 1, NRFdis(ibar)
          IF(FISmod(Nnuc).EQ.0..OR.(FISmod(Nnuc).GT.0..AND.ibar.NE.2))
     &       WRITE(79,'(1x, 1f3.1, 1x, 1i4, 2x, 1f8.3, 1x, 1f8.3)')
     &             SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar), 
     &             H(nr,ibar)
          IF(FISmod(Nnuc).EQ.1..AND.ibar.EQ.2)
     &       WRITE(79,'(1x, 1f3.1, 1x, 1i4,1x, 4f9.3)')SFDis(nr,ibar), 
     &       IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1), EFDism(nr,2), 
     &       HM(nr,2)
          IF(FISmod(Nnuc).EQ.2..AND.ibar.EQ.2)
     &       WRITE(79,'(1x, 1f3.1, 1x, 1i4,1x, 6f9.3)')SFDis(nr,ibar), 
     &       IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1), EFDism(nr,2), 
     &       HM(nr,2), EFDism(nr,3), HM(nr,3)
        ENDDO
      ENDDO
      WRITE(79,*)
      WRITE(79,*)chstar
      WRITE(79,*)
     &    ' Quantities used only if FISDEN=0 to calculate LD at saddles'
      WRITE(79,*)chstar
      WRITE(79,
     &'(14x,a4,1x,a9,1x,a5,4x,a4,2x,a10,3x,a3,5x,a6,3x,a5,             4
     &x,a4)')'Asym', 'shellcorr', 'Ushif', 'gamma', 'atilf/atil', 'Ecf', 
     &       'VIB1/2', 'VIBdt', 'Norm'
      DO nr = 1, NRHump
        IF(FISmod(Nnuc).EQ.0..OR.(FISmod(Nnuc).GT.0..AND.nr.NE.2))
     &     WRITE(79,'(1x, A8, 1x, I1,4x,I1, 8f9.3)')'Barrier', nr, 
     &     BFF(nr), SHCfis(nr), DELtafis(nr), GAMmafis(nr), AFIs(nr), 
     &     ECFis(nr), VIBf12(nr), VIBfdt(nr), VIBfnorm(nr)
        IF(FISmod(Nnuc).GT.0..AND.nr.EQ.2)THEN
          nrmod = INT(FISmod(Nnuc)) + 1
          DO m = 1, nrmod
            WRITE(79,'(1x, A8, 1x, I1, 2x, I1, 1x, I1, 8f9.3)')
     &            'Barrier', nr, m, BFFm(m), SHCfism(m), DELtafism(m), 
     &            GAMmafism(m), AFIsm(m), ECFism(m), VIBf12m(m), 
     &            VIBfdtm(m), VIBfnormm(m)
          ENDDO
        ENDIF
      ENDDO
      WRITE(79,*)
 
      WRITE(79,*)chstar
      WRITE(79,*)
     &'  Coefficients used only if FISDEN=3 to adjust HFB LD at saddles 
     &'
      WRITE(79,*)chstar
      WRITE(79,*)'            Asym    Delta    alpha   Norm'
      DO ih = 1, NRHump
        bfi = INT(BFF(ih))
        WRITE(79,'(1x, A8, 1x, I1,4x,I1, 3f9.3)')'Barrier ', ih, bfi, 
     &        ROHfbp_sd(ih), ROHfba_sd(ih), ROHfb_norm(ih)
      ENDDO
      WRITE(79,*)
 
      WRITE(79,*)chstar
 
      WRITE(79,*)
     &'  Coefficient(s) used to calculate the direct continuum weight(s)
     & '
      WRITE(79,*)chstar
      DO nr = 1, NRWel
        AWF(nr) = 0.D0
        WRITE(79,'(1x, A8, 1x, I1, 1f9.3)')'    Well', nr, AWF(nr)
      ENDDO
      WRITE(79,*)
      WRITE(79,*)'****************************************************'
      WRITE(79,*)'****************************************************'
      RETURN
      END SUBROUTINE INPFIS
 
!---------------------------------------------------------------------------
C
C
      SUBROUTINE READ_INPFIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      INTEGER :: NEXtr, NPOints
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL :: barmin
      INTEGER :: bfi, i, ia, ib, ibar, ibaro, id, ih, ii, iw, iz, j, k, 
     &           m, mm, n, nh, nr, nrmod, nrsm
      CHARACTER(2) :: cara3, carz
      CHARACTER(10) :: cara8
      REAL*8 :: centr, heigth, ucentr, uheigth, uwidth
      REAL :: FLOAT
      INTEGER :: INT
      CHARACTER(40) :: line
      REAL*8 :: rmiu
      REAL*8, DIMENSION(NFParab) :: width
C
C*** End of declarations rewritten by SPAG
C
C
 
C
 
C Dummy arguments
C
 
      DO nh = 1, NFHump
        BARnorm(nh) = 0.D0
      ENDDO
      HNOrm = 1.D0
 
      IF(NRSmooth(Nnuc).EQ.0)NRSmooth(Nnuc) = 5
      nrsm = NRSmooth(Nnuc)
 
      OPEN(79,FILE = 'FISSION.INP',STATUS = 'OLD',ERR = 30)
   10 READ(79,'(A8)',ERR = 30,END = 30)cara8
      IF(cara8.NE.'Isotope:')GOTO 10
      READ(79,*,ERR = 20,END = 20)
      READ(79,'(4x,a2,i3,2x,a2,i3)',ERR = 20,END = 20)carz, iz, cara3, 
     &     ia
      IF(carz.EQ.'Z='.AND.iz.NE.INT(Z(Nnuc)).OR.ia.NE.INT(A(Nnuc)))
     &   GOTO 10
      READ(79,*)
      READ(79,*)
 
      IF(FISbar(Nnuc).EQ.3.)THEN
        READ(79,'(//)',ERR = 20,END = 20)
        READ(79,'(i3)',ERR = 20,END = 20)NPOints
        DO i = 1, NPOints
          READ(79,'(i4,2f10.3)',ERR = 20,END = 20) ii, VDEf_1d(i), 
     &         EPS_1d(i)
        ENDDO
        READ(79,*,ERR = 20,END = 20)
        READ(79,'(15x,i1,15x,i1)',ERR = 20,END = 20)NRBar, NRWel
        NRHump = NRBar - NRWel
        NEXtr = NRBar
        READ(79,'(/)',ERR = 20,END = 20)
        READ(79,*,ERR = 20,END = 20)(IIExtr(j),j = 1,NRBar)
        READ(79,*,ERR = 20,END = 20)
        IF(NRHump.EQ.1)READ(79,'(2f10.3)',ERR = 20,END = 20)
     &                      (BARnorm(nh),nh = 1,NRHump), HNOrm
        IF(NRHump.EQ.2)READ(79,'(3f10.3)',ERR = 20,END = 20)
     &                      (BARnorm(nh),nh = 1,NRHump), HNOrm
        IF(NRHump.EQ.3)READ(79,'(4f10.3)',ERR = 20,END = 20)
     &                      (BARnorm(nh),nh = 1,NRHump), HNOrm
        READ(79,'(/)',ERR = 20,END = 20)
      ENDIF
 
      IF(FISbar(Nnuc).LT.3.)THEN
        READ(79,'(15x,i1,15x,i1)',ERR = 20,END = 20)NRBar, NRWel
        NRHump = NRBar - NRWel
        READ(79,'(/)',ERR = 20,END = 20)
      ENDIF
C
      nrmod = INT(FISmod(Nnuc)) + 1
 
      IF(FISmod(Nnuc).EQ.0.)READ(79,*,ERR = 20,END = 20)
     &                           (EFB(i),HCOnt(i),i = 1,NRBar)
      IF(FISmod(Nnuc).EQ.1..AND.NRWel.EQ.1)READ(79,*,ERR = 20,END = 20)
     &   EFB(1), H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFB(3), 
     &   H(1,3)
      IF(FISmod(Nnuc).EQ.1..AND.NRWel.EQ.0)READ(79,*,ERR = 20,END = 20)
     &   EFB(1), H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2)
      IF(FISmod(Nnuc).EQ.2..AND.NRWel.EQ.1)READ(79,*,ERR = 20,END = 20)
     &   EFB(1), H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFBm(3), 
     &   HM(1,3), EFB(3), H(1,3)
      IF(FISmod(Nnuc).EQ.2..AND.NRWel.EQ.0)READ(79,*,ERR = 20,END = 20)
     &   EFB(1), H(1,1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFBm(3), 
     &   HM(1,3)
C
      READ(79,' (/)',ERR = 20,END = 20)
      READ(79,*,ERR = 20,END = 20)(HJ(Nnuc,i),i = 1,NRBar)
      READ(79,' (/)',ERR = 20,END = 20)
      READ(79,*,ERR = 20,END = 20)(DEFfis(i),i = 1,NRBar)
      READ(79,'(////)',ERR = 20,END = 20)
      DO iw = 1, NRWel
        READ(79,'(3f11.4)',ERR = 20,END = 20)(WIMag(iw,i),i = 1,3)
      ENDDO
      READ(79,'(///)',ERR = 20,END = 20)
 
      DO ibar = 1, NRBar
        READ(79,'(a39,I2,a2,I2)',ERR = 20,END = 20) line, ibaro, cara8, 
     &       NRFdis(ibar)
        READ(79,*,ERR = 20,END = 20)
        DO nr = 1, NRFdis(ibar)
          IF(FISmod(Nnuc).EQ.0..OR.(FISmod(Nnuc).GT.0..AND.ibar.NE.2))
     &       READ(79,*,ERR = 20,END = 20)SFDis(nr,ibar), IPFdis(nr,ibar)
     &            , EFDis(nr,ibar), H(nr,ibar)
          IF(FISmod(Nnuc).EQ.1..AND.ibar.EQ.2)
     &       READ(79,*,ERR = 20,END = 20)SFDis(nr,ibar), IPFdis(nr,ibar)
     &       , EFDism(nr,1), HM(nr,1), EFDism(nr,2), HM(nr,2)
          IF(FISmod(Nnuc).EQ.2..AND.ibar.EQ.2)
     &       READ(79,*,ERR = 20,END = 20)SFDis(nr,ibar), IPFdis(nr,ibar)
     &       , EFDism(nr,1), HM(nr,1), EFDism(nr,2), HM(nr,2), 
     &       EFDism(nr,3), HM(nr,3)
        ENDDO
      ENDDO
      READ(79,*,ERR = 20,END = 20)
 
C-----FISDEN(Nnuc)= 0 EMPIRE
C-----FISDEN(Nnuc)= 3 HFB
 
      IF(FISden(Nnuc).LE.1)THEN
        READ(79,'(///)',ERR = 20,END = 20)
        DO ib = 1, NRHump
          IF(FISmod(Nnuc).EQ.0..OR.(FISmod(Nnuc).GT.0..AND.ib.NE.2))THEN
            READ(79,'(10x,  I1,4x, I1, 8f9.3)',ERR = 20,END = 20) i, 
     &           BFF(ib), SHCfis(ib), DELtafis(ib), GAMmafis(ib), 
     &           AFIs(ib), ECFis(ib), VIBf12(ib), VIBfdt(ib), 
     &           VIBfnorm(ib)
          ENDIF
          IF(FISmod(Nnuc).GT.0..AND.ib.EQ.2)THEN
            DO m = 1, nrmod
              READ(79,'(10x, I1, 2x, I1, 1x, I1, 8f9.3)',ERR = 20,
     &             END = 20) i, mm, BFFm(m), SHCfism(m), DELtafism(m), 
     &                      GAMmafism(m), AFIsm(m), ECFism(m), 
     &                      VIBf12m(m), VIBfdtm(m), VIBfnormm(m)
            ENDDO
          ENDIF
        ENDDO
        IF(NRHump.EQ.1)READ(79,'(//////)',ERR = 20,END = 20)
        IF(NRHump.EQ.2)READ(79,'(///////)',ERR = 20,END = 20)
        IF(NRHump.EQ.3)READ(79,'(////////)',ERR = 20,END = 20)
C         READ (79,*)
      ENDIF
 
      IF(FISden(Nnuc).EQ.3)THEN
        IF(NRHump.EQ.1)READ(79,'(/////////)',ERR = 20,END = 20)
        IF(NRHump.EQ.2)READ(79,'(//////////)',ERR = 20,END = 20)
        IF(NRHump.EQ.3)READ(79,'(///////////)',ERR = 20,END = 20)
 
        DO ih = 1, NRHump
          READ(79,'(1x, A8, 1x, I1,4x,I1, 3f9.3)',ERR = 20,END = 20)
     &         cara8, i, bfi, ROHfbp_sd(ih), ROHfba_sd(ih), 
     &         ROHfb_norm(ih)
          BFF(i) = FLOAT(bfi)
        ENDDO
        READ(79,*)
      ENDIF
 
      READ(79,'(//)')
      DO nr = 1, NRWel
        READ(79,'(1x, A8, 1x, I1, 1f9.3)',ERR = 20,END = 20)cara8, i, 
     &       AWF(nr)
      ENDDO
      GOTO 40
   20 CLOSE(79)
      WRITE(8,*)' ERROR: Reading the fission input file'
      WRITE(8,*)' ERROR: Delete the fission input and restarts'
      WRITE(8,*)' ERROR: EMPIRE execution stopped'
      STOP ' ERROR: Delete the fission input and restarts'
   30 CLOSE(79)
      OPEN(79,FILE = 'FISSION.INP',STATUS = 'OLD',ACCESS = 'APPEND')
      CALL INPFIS(Nnuc)
   40 CLOSE(79)
C-----reading Fission input---done-------------------------
C----------------------------------------------------------
      IF(FISbar(Nnuc).EQ.3.)THEN
C--------width normalization
        DO i = 1, NPOints
          EPS_1d(i) = EPS_1d(i)*HNOrm
        ENDDO
C--------global height normalization
        IF(BARnorm(1).EQ.10.)THEN
          DO i = 1, NPOints
            VDEf_1d(i) = VDEf_1d(i)*BARnorm(2)
          ENDDO
        ELSE
C--------height normalization per hump
          IIExtr(0) = 1
          IIExtr(NEXtr + 1) = NPOints
          barmin = VDEf_1d(IIExtr(2))
          DO n = 3, NEXtr - 1
            IF(VDEf_1d(IIExtr(n)).LT.barmin)barmin = VDEf_1d(IIExtr(n))
          ENDDO
          DO nh = 1, NRBar, 2
            IF(BARnorm(nh/2 + 1).NE.0.)THEN
              DO id = IIExtr(nh - 1), IIExtr(nh + 1)
                IF(VDEf_1d(id).GE.barmin)VDEf_1d(id) = VDEf_1d(id)
     &             + BARnorm(nh/2 + 1)*(VDEf_1d(id) - barmin)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
C------- Fitting parabola
        rmiu = 0.054D0*A(Nnuc)**(5.D0/3.D0)
        DO j = 1, NRBar
          CALL PARABFIT(IIExtr(j),nrsm,rmiu,EPS_1d,VDEf_1d,centr,heigth,
     &                  width(j),ucentr,uheigth,uwidth)
          IF(width(j).LT.0.05D0)THEN     ! Skipping very narrow peaks
          ENDIF
        ENDDO
        DO k = 1, NRBar, 2
          EFB(INT(k/2) + 1) = VDEf_1d(IIExtr(k))
          HCOnt(INT(k/2) + 1) = width(k)
          H(1,INT(k/2) + 1) = width(k)
          DEFfis(INT(k/2) + 1) = EPS_1d(IIExtr(k))
        ENDDO
        DO k = 2, NRBar, 2
          EFB(NRHump + INT(k/2)) = VDEf_1d(IIExtr(k))
          HCOnt(NRHump + INT(k/2)) = width(k)
          H(1,NRHump + INT(k/2)) = width(k)
          DEFfis(NRHump + INT(k/2)) = EPS_1d(IIExtr(k))
        ENDDO
      ENDIF
 
      IF(NRBar.EQ.2)THEN
        EFB(NRBar + 1) = 2.
        H(1,NRBar + 1) = 1.
        NRBar = 3
      ENDIF
 
C     For covariance
C      EFB(1) = EFB(1) * FISbin (Nnuc)
      IF(NRHump.GE.2)THEN
        DO i = 1, NRHump
          EFB(i) = EFB(i)*FISv_n(i,Nnuc)
          HCOnt(i) = HCOnt(i)*FISh_n(i,Nnuc)
          AFIs(i) = AFIs(i)*FISa_n(i,Nnuc)
          DELtafis(i) = DELtafis(i)*FISd_n(i,Nnuc)
          VIBfnorm(i) = VIBfnorm(i)*FISn_n(i,Nnuc)
        ENDDO
      ENDIF
 
      IF(FISbar(Nnuc).LE.2.AND.FISmod(Nnuc).EQ.0.)CALL DEFO_FIS(Nnuc)
 
      RETURN
      END SUBROUTINE READ_INPFIS
 
!---------------------------------------------------------------------------
C
C**************************************************************
      FUNCTION FIND_EXTREM(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER :: IEXt, NPOints
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, IEXt
C
C Dummy arguments
C
      INTEGER :: Nnuc
      INTEGER :: FIND_EXTREM
C
C Local variables
C
      INTEGER :: j, k
      LOGICAL :: logmax, logmin
      INTEGER :: NINT
C
C*** End of declarations rewritten by SPAG
C
C**************************************************************
C     Find all extrema of the smoothed deformation energy curve
C     Nrsmooth is defined globally(default) and it could be
C             modified in input for each nucleus
C
      IEXt = 0
      FIND_EXTREM = 0
      IIExtr(0) = 1
CC--------------------------------------------------------------------------
C     determination of the minima   and maxima
      IEXt = 0
      DO j = NRSmooth(Nnuc) + 1, NPOints - NRSmooth(Nnuc)
        logmax = .TRUE.
        DO k = j - NRSmooth(Nnuc), j + NRSmooth(Nnuc)
          IF(k.EQ.j)CYCLE
          IF(VDEf_1d(k).GT.VDEf_1d(j))logmax = .FALSE.
        ENDDO
        IF(logmax)THEN
          IEXt = IEXt + 1
          IIExtr(IEXt) = j
        ENDIF
        logmin = .TRUE.
        DO k = j - NRSmooth(Nnuc), j + NRSmooth(Nnuc)
          IF(k.EQ.j.OR.k.LT.1)CYCLE
          IF(VDEf_1d(k).LT.VDEf_1d(j))logmin = .FALSE.
        ENDDO
        IF(logmin)THEN
          IEXt = IEXt + 1
          IIExtr(IEXt) = j
        ENDIF
      ENDDO
      IF(IEXt.GT.5)THEN
        WRITE(8,*)
     &      ' WARNING: HFB numerical barrier of suspicious shape for Z='
     &      , NINT(Z(Nnuc)), ' A=', NINT(A(Nnuc))
        WRITE(8,*)
     &        ' WARNING: More than 5 extrema in HFB barrier, RESET to 5'
        WRITE(8,*)
     &        ' WARNING: EMPIRE can not deal with more than 3H barriers'
        WRITE(8,*)
     &          ' WARNING: You may want to use RIPL barriers (FISBAR 1)'
        IEXt = 5
        WRITE(8,*)' WARNING: --------------------------------------'
      ENDIF
      FIND_EXTREM = IEXt
      IIExtr(IEXt + 1) = NPOints
      RETURN
      END FUNCTION FIND_EXTREM
 
!---------------------------------------------------------------------------
C===================================================================
      SUBROUTINE DEFO_FIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL*8, DIMENSION(2*NFParab) :: ejoin
      REAL*8, DIMENSION(NFParab) :: epsil, ho, vjj
      INTEGER :: i, k, nrbarm
      INTEGER :: INT
      REAL*8 :: smiu
C
C*** End of declarations rewritten by SPAG
C
C===================================================================
C-----deformations at saddles and wells and matching points-----------
      smiu = 0.1643167*A(Nnuc)**(5./6.)
      IF(NRBar.EQ.1)DEFfis(1) = SQRT(EFB(1))/(smiu*HCOnt(1))
     &                          + DEF(1,Nnuc)
      IF(NRHump.EQ.2)THEN
        nrbarm = 3
        IF(NRWel.EQ.0)THEN
          EFB(3) = 2.
          HCOnt(3) = 1.
        ENDIF
      ENDIF
      IF(NRHump.EQ.3)THEN
        nrbarm = 5
        IF(NRWel.EQ.0)THEN
          EFB(4) = 2.
          HCOnt(4) = 1.
          EFB(5) = 5.
          HCOnt(5) = 1.2
        ENDIF
      ENDIF
      DO k = 1, NRBar, 2
        ho(k) = HCOnt(INT(k/2) + 1)
        vjj(k) = EFB(INT(k/2) + 1)
      ENDDO
      DO k = 2, NRBar, 2
        ho(k) = HCOnt(NRHump + INT(k/2))
        vjj(k) = EFB(NRHump + INT(k/2))
      ENDDO
      DO i = 1, nrbarm
        epsil(i) = 0.
        ejoin(i) = 0.
        DEFfis(i) = 0.
      ENDDO
      epsil(1) = SQRT(vjj(1))/(smiu*ho(1)) + DEF(1,Nnuc)
      ejoin(2) = epsil(1)
     &           + SQRT((vjj(1) - vjj(2))/(1.D0 + (ho(1)/ho(2))**2))
     &           /(smiu*ho(1))
      ejoin(1) = 2*epsil(1) - ejoin(2)
      DO k = 2, nrbarm
        ejoin(2*k - 1) = ejoin(2*(k - 1))
        epsil(k) = ejoin(2*(k - 1)) + (ho(k - 1)/ho(k))
     &             **2*(ejoin(2*(k-1)) - epsil(k - 1))
        IF(k.LT.nrbarm)ejoin(2*k) = epsil(k)
     &                              + SQRT(( - 1)**k*((vjj(k+1)-vjj(k)))
     &                              /(1.D0 + (ho(k)/ho(k+1))**2))
     &                              /(smiu*ho(k))
      ENDDO
      DO k = 1, NRHump
        DEFfis(k) = epsil(2*(k - 1) + 1)
      ENDDO
      DO k = 1, NRWel
        DEFfis(NRHump + k) = epsil(2*k)
      ENDDO
      RETURN
      END SUBROUTINE DEFO_FIS
 
!---------------------------------------------------------------------------
C
C===================================================================
      SUBROUTINE WRITE_OUTFIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NFHump) :: ACRtf, DETcrtf, ECOndf, SCRtf, TCRtf, 
     &                             UCRtf
      REAL*8, DIMENSION(NFIsbarpnt) :: EPS_1d, VDEf_1d
      INTEGER, DIMENSION(0:2*NFParab) :: IIExtr
      REAL*8, DIMENSION(NFParab) :: MORtcrt, MPArcrt
      INTEGER :: NEXtr, NPOints
      REAL*8 :: PFIso, RFIso, TFIso, TGIso, TISo
      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt, 
     &                 MPArcrt, ECOndf
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /NUMBAR/ EPS_1d, VDEf_1d, NPOints, IIExtr, NEXtr
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      INTEGER :: bfi, i, ib, ibar, ih, ii, iw, j, m, nh, nr, nrmod
      CHARACTER(36) :: cara1
      CHARACTER(1), DIMENSION(70) :: chstar
      INTEGER :: INT
C
C*** End of declarations rewritten by SPAG
C
C===================================================================
C
 
 
 
 
 
 
      DATA chstar/70*'='/
C
C
      WRITE(80,'(a8)')'Isotope:'
      WRITE(80,'(a40)')'----------------------------------------'
      WRITE(80,'(4x,a2,i3,2x,a2,i3)')'Z=', INT(Z(Nnuc)), 'A=', 
     &                               INT(A(Nnuc))
      WRITE(80,'(a40)')'----------------------------------------'
 
      IF(FISmod(Nnuc).EQ.0.)cara1 = '  Single-modal fission          '
      IF(FISmod(Nnuc).GT.0.)cara1 = '  Multimodal fission (in devel.)'
 
      WRITE(80,'(a8,f2.0,a36)')' FISMOD = ', FISmod(Nnuc), cara1
 
      IF(FISbar(Nnuc).EQ.0.)cara1 = '  EMPIRE  internal library      '
      IF(FISbar(Nnuc).EQ.1.)cara1 = '  RIPL-3  (Exp.) values         '
      IF(FISbar(Nnuc).EQ.2.)cara1 = '  RIPL-3  HFB parabolic barriers'
      IF(FISbar(Nnuc).EQ.3.)cara1 = '  RIPL-3  HFB Numerical barriers'
 
      WRITE(80,'(a8,f2.0,a36)')' FISBAR = ', FISbar(Nnuc), cara1
 
      IF(FISden(Nnuc).EQ.0.)cara1 = '  RIPL-3  EGSM LD               '
C      IF (FISden(Nnuc).EQ.1.) cara1 = '  Ignatyuk GSM LD               '
      IF(FISden(Nnuc).EQ.3.)cara1 = '  RIPL-3  HFB microscopic LD    '
      WRITE(80,'(a8,f2.0,a36)')' FISDEN = ', FISden(Nnuc), cara1
 
      IF(FISopt(Nnuc).EQ.0.)cara1 = '  Full damping model (Ind.Barr.)'
      IF(FISopt(Nnuc).EQ.1.)cara1 = '  Optical model for fission     '
      IF(FISopt(Nnuc).EQ.2.)cara1 = 
     &                   '  Complex fission potential, isomeric fission'
      WRITE(80,'(a8,f2.0,a36)')' FISOPT = ', FISopt(Nnuc), cara1
      WRITE(80,*)' '
      WRITE(80,*)chstar
      IF(FISbar(Nnuc).EQ.3.)THEN
        WRITE(80,*)' RIPL-3 HFB numerical fission barrier'
        WRITE(80,*)chstar
        WRITE(80,'(i3)')NPOints
        DO ii = 1, NPOints
          WRITE(80,'(i4,2f10.3)')ii, VDEf_1d(ii), EPS_1d(ii)
        ENDDO
        WRITE(80,*)
        WRITE(80,*)' Index of extrema'
        WRITE(80,*)(IIExtr(j),j = 1,NRBar)
        WRITE(80,*)' Normalization factors for humps'
        WRITE(80,'(4f10.3)')(BARnorm(nh),nh = 1,NRHump), HNOrm
      ENDIF
      WRITE(80,'(a15,i1,a15,i1)')' Nr.parabolas =', NRBar, 
     &                           '      Nr.wells=', NRWel
C     WRITE (80,*)' '
      WRITE(80,*)chstar
C
      IF(NRBar.EQ.1)THEN
        WRITE(80,'(a)')'    Va      ha    (in Mev) '
        WRITE(80,'(2f8.3)')EFB(1), HCOnt(1)
        WRITE(80,*)' '
        WRITE(80,'(2a10)')'h2/2J(A)', '(in MeV)'
        WRITE(80,'(f9.4)')HJ(Nnuc,1)
        WRITE(80,*)' '
        WRITE(80,'(a10)')'Beta2(A)'
        WRITE(80,'(f9.4)')DEFfis(1)
        WRITE(80,*)' '
      ENDIF
      IF(NRBar.EQ.2)THEN
        IF(FISmod(Nnuc).EQ.0.)THEN
          WRITE(80,'(a)')'    Va      ha      Vb      hb     (in Mev) '
          WRITE(80,'(4f8.3)')(EFB(i),HCOnt(i),i = 1,NRBar)
        ENDIF
        IF(FISmod(Nnuc).EQ.1.)THEN
          WRITE(80,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '  (in Mev) '
          WRITE(80,'(6f9.3,15x)')EFB(1), H(1,1), EFBm(1), HM(1,1), 
     &                           EFBm(2), HM(1,2)
        ENDIF
        IF(FISmod(Nnuc).EQ.2.)THEN
          WRITE(80,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)  (in Mev) '
          WRITE(80,'(8f9.3,15x)')EFB(1), H(1,1), EFBm(1), HM(1,1), 
     &                           EFBm(2), HM(1,2), EFBm(3), HM(1,3)
        ENDIF
        WRITE(80,*)' '
        WRITE(80,'(3a10)')'h2/2J(A)', 'h2/2J(B)', '(in MeV)'
        WRITE(80,'(2f9.4)')(HJ(Nnuc,i),i = 1,NRBar)
        WRITE(80,*)' '
        WRITE(80,'(2a10)')'Beta2(A)', 'Beta2(B)'
        WRITE(80,'(2f9.4)')(DEFfis(i),i = 1,NRBar)
        WRITE(80,*)' '
      ENDIF
      IF(NRBar.EQ.3)THEN
        IF(FISmod(Nnuc).EQ.0.)THEN
          WRITE(80,'(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
          WRITE(80,'(6f8.3,15x)')(EFB(i),HCOnt(i),i = 1,NRBar)
        ENDIF
        IF(FISmod(Nnuc).EQ.1.)THEN
          WRITE(80,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '    Vi      hi  (in Mev) '
          WRITE(80,'(8f9.3,15x)')EFB(1), H(1,1), EFBm(1), HM(1,1), 
     &                           EFBm(2), HM(1,2), EFB(3), H(1,3)
        ENDIF
        IF(FISmod(Nnuc).EQ.2.)THEN
          WRITE(80,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)   Vi      hi  (in Mev) '
          WRITE(80,'(10f9.3,15x)')EFB(1), H(1,1), EFBm(1), HM(1,1), 
     &                            EFBm(2), HM(1,2), EFBm(3), HM(1,3), 
     &                            EFB(3), H(1,3)
        ENDIF
        WRITE(80,*)' '
        WRITE(80,'(4a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)', '(in MeV)'
        WRITE(80,'(3f9.4)')(HJ(Nnuc,i),i = 1,NRBar)
        WRITE(80,*)' '
        WRITE(80,'(3a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
        WRITE(80,'(3f9.4)')(DEFfis(i),i = 1,NRBar)
        WRITE(80,*)'  '
      ENDIF
      IF(NRBar.EQ.5)THEN
        WRITE(80,'(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi      hi  
     &    Vo      ho  (in Mev) '
        WRITE(80,'(10f8.3,15x)')(EFB(i),H(1,i),i = 1,NRBar)
        WRITE(80,*)' '
        WRITE(80,'(6a10)')'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)', 
     &                    'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
        WRITE(80,'(5f9.4)')(HJ(Nnuc,i),i = 1,NRBar)
        WRITE(80,*)' '
        WRITE(80,'(6a10)')'Beta2(A)', 'Beta2(B)', 'Beta2(C)', 
     &                    'Beta2(I)', 'Beta2(O)', '        '
        WRITE(80,'(5f9.4)')(DEFfis(i),i = 1,NRBar)
        WRITE(80,*)' '
      ENDIF
 
C      IF (FISmod(Nnuc).EQ.0. .AND. NRBar.EQ.3) THEN
C         WRITE (80,*) ' '
C         WRITE (80,*) '  Tiso1/2 fission = ', TFIso, ' (s)'
C         WRITE (80,*) '  Tiso1/2 gamma   = ', TGIso, ' (s)'
C         WRITE (80,*) '  Tiso1/2 total   = ', TISo, ' (s)'
C         WRITE (80,*) '  Rfiso   = ', RFIso
C      ENDIF
      WRITE(80,*)' '
 
      IF(FISopt(Nnuc).GT.0)THEN
        WRITE(80,*)'      W0         W1         W2'
        DO iw = 1, NRWel
          WRITE(80,'(3f11.4)')(WIMag(iw,i),i = 1,3)
        ENDDO
        WRITE(80,*)
      ENDIF
 
      WRITE(80,*)' Discrete transitional states'
 
C
      DO ibar = 1, NRBar
        IF(ibar.LE.NRHump)THEN
          WRITE(80,'(a39,I2,a2,I2)')
     &                            ' Number  of  discrete states at hump'
     &                            , ibar, ' =', NRFdis(ibar)
        ELSE
          WRITE(80,'(a39,I2,a2,I2)')
     &                            ' Number  of  discrete states in well'
     &                            , ibar, ' =', NRFdis(ibar)
        ENDIF
 
        WRITE(80,*)'Kdis  Pidis   Edis    homega'
        DO nr = 1, NRFdis(ibar)
          IF(FISmod(Nnuc).EQ.0..OR.(FISmod(Nnuc).GT.0..AND.ibar.NE.2))
     &       WRITE(80,'(1x, 1f3.1, 1x, 1i4, 2x, 1f8.3, 1x, 1f8.3)')
     &             SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar), 
     &             H(nr,ibar)
          IF(FISmod(Nnuc).EQ.1..AND.ibar.EQ.2)
     &       WRITE(80,'(1x, 1f3.1, 1x, 1i4,1x, 4f9.3)')SFDis(nr,ibar), 
     &       IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1), EFDism(nr,2), 
     &       HM(nr,2)
          IF(FISmod(Nnuc).EQ.2..AND.ibar.EQ.2)
     &       WRITE(80,'(1x, 1f3.1, 1x, 1i4,1x, 6f9.3)')SFDis(nr,ibar), 
     &       IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1), EFDism(nr,2), 
     &       HM(nr,2), EFDism(nr,3), HM(nr,3)
        ENDDO
      ENDDO
      WRITE(80,*)
 
      IF(FISden(Nnuc).LE.1)THEN
 
        WRITE(80,*)'Parameters of EGSM LD'
 
        WRITE(80,
     &'(14x,a4,1x,a9,1x,a5,4x,a4,2x,a10,3x,a3,5x,a6,3x,a5,             4
     &x,a4)')'Asym', 'shellcorr', 'Ushif', 'gamma', 'atilf/atil', 'Ecf', 
     &       'VIB1/2', 'VIBdt', 'Norm'
 
        DO nr = 1, NRHump
          IF(FISmod(Nnuc).EQ.0..OR.(FISmod(Nnuc).GT.0..AND.nr.NE.2))
     &       WRITE(80,'(1x, A8, 1x, I1,4x,I1, 8f9.3)')'Barrier', nr, 
     &             BFF(nr), SHCfis(nr), DELtafis(nr), GAMmafis(nr), 
     &             AFIs(nr), ECFis(nr), VIBf12(nr), VIBfdt(nr), 
     &             VIBfnorm(nr)
          IF(FISmod(Nnuc).GT.0..AND.nr.EQ.2)THEN
            nrmod = INT(FISmod(Nnuc)) + 1
            DO m = 1, nrmod
              WRITE(80,'(1x, A8, 1x, I1, 2x, I1, 1x, I1, 8f9.3)')
     &              'Barrier', nr, m, BFFm(m), SHCfism(m), DELtafism(m), 
     &              GAMmafism(m), AFIsm(2), ECFism(m), VIBf12m(m), 
     &              VIBfdtm(m), VIBfnormm(m)
            ENDDO
          ENDIF
        ENDDO
        WRITE(80,*)
 
        DO ib = 1, NRHump
          WRITE(80,*)'Barrier  ', ib
          WRITE(80,'(3(A9,f9.5),a9,f11.5)')'Acrt=', ACRtf(ib), 'Ucrt=', 
     &          UCRtf(ib), 'Econd=', ECOndf(ib), 'DETcrt=', DETcrtf(ib)
          WRITE(80,'(A9,f9.5,A9,f9.5)')'Tcrt=', TCRtf(ib), 'Scrt=', 
     &                                 SCRtf(ib)
        ENDDO
        WRITE(80,*)
      ENDIF
 
      IF(FISden(Nnuc).EQ.3)THEN
        WRITE(80,*)'Normalization factors for HFB LD'
        WRITE(80,*)'                   Delta    alpha   Norm'
        DO ih = 1, NRHump
          bfi = INT(BFF(ih))
          WRITE(80,'(1x, A8, 1x, I1,4x,I1, 3f9.3)')'Barrier ', ih, bfi, 
     &          ROHfbp_sd(ih), ROHfba_sd(ih), ROHfb_norm(ih)
        ENDDO
      ENDIF
 
      WRITE(80,*)
      WRITE(80,*)
     &'Coefficient(s) used to calculate the direct continuum weight(s) '
      DO nr = 1, NRWel
        WRITE(80,'(1x, A8, 1x, I1, 1f9.3)')'    Well', nr, AWF(nr)
      ENDDO
      WRITE(80,*)
 
      RETURN
      END SUBROUTINE WRITE_OUTFIS
