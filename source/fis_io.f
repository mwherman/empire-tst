Ccc   * $Rev: 2576 $
Ccc   * $Author: gnobre $
Ccc   * $Date: 2012-02-15 15:27:34 +0100 (Mi, 15 Feb 2012) $

C
      SUBROUTINE INPFIS(Nnuc)
C Creates fission.inp  which contains all the fission
C parameters independent of energy.
C
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl    ! CRIT

      DOUBLE PRECISION ACRtf(NFHUMP), UCRtf(NFHUMP), TCRtf(NFHUMP),       ! CRITFIS
     & DETcrtf(NFHUMP),SCRtf(NFHUMP),MORtcrt(NFPARAB),
     & MPArcrt(NFPARAB), ECOndf(NFHUMP)

      DOUBLE PRECISION AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                                       ! PARAM

      DOUBLE PRECISION vdef_1d(NFISBARPNT),eps_1d(NFISBARPNT)             ! NUMBAR
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr                         ! NUMBAR

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl

      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt,
     &                 MPArcrt, ECOndf

      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*1 chstar(70)
      INTEGER i, ib, ibar,ih, ka, kz, m, nr,  nrmod, nrsm,bfi
      INTEGER INT,iz,ia,iarr,izrr
      CHARACTER*50 filename
      DOUBLE PRECISION rmiu, bb2, bb3, bb4
      INTEGER Find_Extrem
      DATA chstar/70*'='/
      REAL*8 centr,  heigth,  width(NFPARAB), ucentr, uheigth, uwidth

      iz=INT(Z(Nnuc))
      ia=INT(A(Nnuc))
      nrsm = NRSmooth(Nnuc)

      DO i = 1, NFPARAB
         EFB(i) = 0.d0
      ENDDO
      DO ih = 1, NFHUMP
         rohfbp_sd(ih) = 0.d0
         rohfba_sd(ih) = 0.d0
         rohfb_norm(ih)= 1.d0
         barnorm(ih) = 0.d0
      ENDDO
      hnorm=1.d0
      NRBar = 0
      NRWel = 0
      NRHump =0
c-----Fundamental barrier heights
C-----FISBAR(Nnuc)=0 EMPIRE
C-----FISBAR(Nnuc)=1 Maslov
C-----FISBAR(Nnuc)=2 HFB parabolic
C-----FISBAR(Nnuc)=3 HFB numeric
C
C-----FISBAR(Nnuc)= 0 Empire internal library
      IF (FISbar(Nnuc).EQ.0.) THEN
         OPEN (81,FILE = trim(EMPiredir)//'/data/EMPIRE-fisbar.dat'
     &      ,STATUS = 'OLD',ERR = 400)
         READ (81,*,ERR=400,END = 400)
         READ (81,*,ERR=400,END = 400)
         READ (81,*,ERR=400,END = 400)
         READ (81,*,ERR=400,END = 400)
  350    READ (81,*,ERR=400, END = 400) kz, ka, NRBar, NRWel,
     &                         (EFB(i),H(1,i),i = 1,NRBar)
         IF (kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc))) GOTO 350
         CLOSE (81)
         GOTO 300
  400    WRITE (8,*) ' ERROR: NO fission barrier for Z=', INT(Z(Nnuc)),
     &    ' A=',INT(A(Nnuc)),
     &    ' in internal EMPIRE library (/data/EMPIRE-fisbar.dat)'
         WRITE (8,*) 
     &    ' ERROR: or the file ../data/EMPIRE-fisbar.dat may be missing'
         WRITE (8,*)
     &     ' ERROR: You may use RIPL barriers (FISBAR 1) instead of loca
     &l fission barriers (FISBAR 0) in your input file'
         STOP ' FATAL: Internal fission barriers can not be retrieved'
      ENDIF
C
C-----FISBAR(Nnuc)= 1 RIPL "empirical" values for humps' heights and widths
C-----wells' parameters provided by code
      IF (FISbar(Nnuc).EQ.1.) THEN
         OPEN (52,FILE = trim(EMPiredir)//'/RIPL/fission'
     &      //'/empirical-barriers.dat',STATUS = 'OLD',ERR = 200)
         READ (52,*,ERR=200,END = 200)
         READ (52,*,ERR=200,END = 200)
         READ (52,*,ERR=200,END = 200)
         READ (52,*,ERR=200,END = 200)
  150    READ (52,'(2i4,1x,a2,1x,2(3x,a2,2f8.2))',!,f9.3)'
     &     ERR = 200, END = 200) kz, ka, dd,dd,EFB(1),H(1,1), 
     &     dd,EFB(2),H(1,2)
c         write (*,'(2i4,1x,a2,1x,2(3x,a2,2f8.2))' )!,f9.3)'
c     &     kz, ka, dd,dd,EFB(1),H(1,1), 
c     &     dd,EFB(2),H(1,2)        
         IF (kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc))) GOTO 150        
         CLOSE (52)

         NRBar = 2
         IF (EFB(2).EQ.0.)THEN
            H(1,1)=0.3d0
            NRBar = 1
            NRWel = 0
         ENDIF
         GOTO 300
  200    WRITE (8,*) ' ERROR: NO fission barrier for Z=', INT(Z(Nnuc)),
     &    ' A=',INT(A(Nnuc)),' in RIPL empirical fission library'
         WRITE (8,*) 
     &    ' ERROR: or the file ../RIPL/fission/empirical-barriers.dat ma
     &y be missing'
         WRITE (8,*)
     &     ' ERROR: You may use local barriers (FISBAR 0) instead of RIP
     &L barriers (FISBAR 1) in your input file'
         STOP ' FATAL: Fission barriers can not be retrieved'
      ENDIF  
C
C-----adding default parameters of the isomeric well for double-humped barrier if missing
c
 300  IF(NRBar.EQ.2)THEN
         EFB(3)=2.d0
         H(1,3)=1.d0
         NRBar=3
         NRWel=1
      ENDIF
C
C-----FISBAR(Nnuc)=2  HFB microscopic parameters for parabolic barriers
C-----                extracted from HFB l.d.files and stored in
c-----                ../data/HFB-fisbar.dat (default)
 310  IF (FISbar(Nnuc).EQ.2.) THEN
         OPEN (81,FILE = trim(EMPiredir)//
     &     '/data/HFB-parab-fisbar.dat'
     &      ,STATUS = 'OLD',ERR = 401)
         READ (81,*,ERR=401,END = 401)
         READ (81,*,ERR=401,END = 401)
         READ (81,*,ERR=401,END = 401)
         READ (81,*,ERR=401,END = 401)
  351    READ (81,*,ERR = 401,END = 401) kz, ka, NRBar, NRWel,
     &                         (EFB(i),H(1,i),i = 1,NRBar)
         IF (kz.NE.INT(Z(Nnuc)) .OR. ka.NE.INT(A(Nnuc))) GOTO 351
         CLOSE (81)
         GOTO 402
  401    WRITE (8,*) ' ERROR: NO fission barriers FOR Z=', 
     &     iz, ' A=', ia,' in file ../data/HFB-parab-fisbar.dat'
         WRITE (8,*)
     &     ' ERROR: You may use RIPL barriers (FISBAR 1) instead of para 
     &bolic approximation of HFB barriers (FISBAR 2) in your input file'
         STOP 
     &     ' FATAL: No fission barriers in ../data/HFB-parab-fisbar.dat'
      ENDIF

 402  NRHump = NRBar - NRWel
      DO i = 1, NRBar
         Hcont(i)= H(1,i)
      ENDDO

      IF(FISbar(Nnuc).LE.2.1) CALL DEFO_FIS(Nnuc)
C
C-----FISBAR(Nnuc)=3.  RIPL-3 HFB numerical barriers-------------------
      IF(FISbar(Nnuc).EQ.3.)THEN
         WRITE (filename,99900)iz
99900    FORMAT ('/RIPL/fission/HFB2007/z',i3.3,'.tab')
         OPEN (UNIT = 52,FILE = trim(EMPiredir)//trim(filename)
     &      ,STATUS = 'old',ERR = 460)
 410     read(52,*,ERR = 460,END=460) izrr,iarr,npoints
C         
C        When the mass number is smaller than that of all existing barriers,
C         the barrier of smallest mass number is used (BVC, MS - 05/10)
C         if(izrr.ne.iz .or. iarr.ne.ia) then
C
         IF(izrr.NE.iz .OR. iarr.LT.ia) THEN
            DO ii=1,npoints
               READ (52,*,ERR = 460,END = 460)
            ENDDO
            GOTO 410
         ENDIF
         IF(ia.LT.iarr) THEN
            WRITE (8,*) 'WARNING: NO RIPL HFB fission barriers FOR Z=',
     &                   iz, ' A=', ia,' in ../RIPL/fission/HFB2007 '
            WRITE (8,*) 'WARNING: Using HFB numerical barrier FOR Z=', 
     &                   iz, ' A=', iarr
         ENDIF    
         DO ii=1,npoints
           READ (52,*,END = 480) bb2, bb3, bb4, vdef_1d(ii)
           eps_1d(ii) = bb2
        ENDDO
        GOTO 480
 460    WRITE (8,*)
     &     ' ERROR: You may use RIPL barriers (FISBAR 1) instead of HFB 
     &BARRIERS (FISBAR 3) in your input file'
        WRITE (8,*) ' ERROR: NO fission barriers FOR Z=', 
     &              iz, ' A=', ia,' in ../RIPL/fission/HFB2007 '
        STOP 
     &     ' FATAL: HFB numerical fission barriers can not be retrieved'
 480    CLOSE (52)

        nextr = Find_Extrem(Nnuc)
        
        nrhump = nextr/2 + 1
        nrwel = nextr/2
        nrbar = nrhump + nrwel
C-------Fitting parabola
        rmiu = 0.054d0*A(Nnuc)**(5.d0/3.d0)
        DO j=1,nextr
           CALL ParabFit(iiextr(j),nrsm,rmiu,eps_1d,vdef_1d,
     &        centr,  heigth,  width(j), ucentr, uheigth, uwidth)
           IF(width(j).LT.0.05d0) CYCLE ! Skipping very narrow peaks
        ENDDO
        DO k=1, NRBar,2
            EFB(int(k/2)+1)    = Vdef_1d(iiextr(k))
            h(1,int(k/2)+1)    = width(k)
            DEFfis(int(k/2)+1) = eps_1d(iiextr(k))
        ENDDO
        DO k=2, NRBar,2
            EFB(NRHump+int(k/2))    = Vdef_1d(iiextr(k))
            h(1,NRHump+int(k/2))    = width(k)
            DEFfis(NRHump+int(k/2)) = eps_1d(iiextr(k))
        ENDDO
      ENDIF
C
C-----Default value for curvatures and protection !!
C
      DO i = 1, NRBar
         IF (H(1,i).EQ.0) H(1,i) = 1.
      ENDDO
C
C----------------------input fundamental fission barrier *** done
C------- discrete barriers---------------------------------------
C
 500  DO ibar = 1, NRBar
         EFDis(1,ibar) = 0.
         IF (A(Nnuc)/2.EQ.INT(A(Nnuc)/2)) THEN
            SFDis(1,ibar) = 0.0
         ELSE
            SFDis(1,ibar) = 0.5
         ENDIF
         IF (XJLv(1,Nnuc).GE.0.) SFDis(1,ibar) = XJLv(1,Nnuc)
         IPFdis(1,ibar) = 1
         IF (LVP(1,Nnuc).NE.IPFdis(1,ibar)) IPFdis(1,ibar) = LVP(1,Nnuc)
         NRFdis(ibar) = 1
      ENDDO
C
c-----Maslov's selected transition bandheads for humps
      IF (FISdis(Nnuc).EQ.1.) THEN
         IF (A(Nnuc)/2.EQ.INT(A(Nnuc)/2)) THEN
c-----------even-even
            IF (Z(Nnuc)/2.EQ.INT(Z(Nnuc)/2)) THEN
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
                  IPFdis(3,ibar) =-1
                  SFDis(4,ibar) = 1.
                  IPFdis(4,ibar) = -1
               ENDDO
            ENDIF
c-----------odd-odd
            IF (Z(Nnuc)/2.NE.INT(Z(Nnuc)/2)) THEN
               DO ibar = 1, NRBar
                  NRFdis(ibar) = 7
               ENDDO
               DO ib=1,2
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
                  IPFdis(1,ibar) =-1
                  SFDis(2,ibar) = 0.
                  IPFdis(2,ibar) =-1
                  SFDis(3,ibar) = 5.
                  IPFdis(3,ibar) =-1
                  SFDis(4,ibar) = 6.
                  IPFdis(4,ibar) = -1
                  SFDis(5,ibar) = 1.
                  IPFdis(5,ibar) = -1
                  SFDis(6,ibar) = 3.
                  IPFdis(6,ibar) =-1
                  SFDis(7,ibar) = 2.
                  IPFdis(7,ibar) = -1
               ENDDO
            ENDIF
         ENDIF
         IF (A(Nnuc)/2.NE.INT(A(Nnuc)/2)) THEN
c-----------even-odd
            IF (Z(Nnuc)/2.EQ.INT(Z(Nnuc)/2)) THEN
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
                  IPFdis(3,ibar) =-1
                  SFDis(4,ibar) = 1.5
                  IPFdis(4,ibar) = -1
               ENDDO
            ENDIF
c-----------odd-even
            IF (Z(Nnuc)/2.NE.INT(Z(Nnuc)/2)) THEN
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
                  IPFdis(3,ibar) =-1
               ENDDO
            ENDIF
         ENDIF
C-------default excitation energies in well
        DO j=1,nrfdis(3)
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
      IF (NRHump.EQ.1) HJ(Nnuc,1) = 0.005
      IF (NRBar.EQ.2) THEN
         HJ(Nnuc,1) = 0.0050
         HJ(Nnuc,2) = 0.0025
      ENDIF
      IF (NRBar.EQ.3) THEN
         HJ(Nnuc,1) = 0.0050
         HJ(Nnuc,2) = 0.0025
         HJ(Nnuc,3) = 0.0035
      ENDIF
      IF (NRBar.EQ.5) THEN
         HJ(Nnuc,1) = 0.0050
         HJ(Nnuc,2) = 0.0025
         HJ(Nnuc,3) = 0.0017
         HJ(Nnuc,4) = 0.0035
         HJ(Nnuc,5) = 0.0020
      ENDIF
      DO iw=1,NRWel
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
      DO ih=1, NRHump
         BFF(ih) = 1
      ENDDO
      IF (A(Nnuc)-Z(Nnuc).GE.144) BFF(1) = 2
      BFF(2) = 3
      BFF(3) = 3
C-----shell corrections at saddles according to RIPL-2
      SHCfis(1) = 2.6
      IF (Z(Nnuc).GT.97.) SHCfis(1) = 2.6 - 0.1*(Z(Nnuc) - 97.)
      SHCfis(2) = 0.6 + 0.1*(Z(Nnuc) - 97.)
     &            + 0.04*(A(Nnuc) - Z(Nnuc) - 143.)
      IF (Z(Nnuc).GT.97.) SHCfis(2)
     &    = 0.6 + 0.04*(A(Nnuc) - Z(Nnuc) - 143.)
      SHCfis(3) = SHCfis(2)
      DO ib = 1, NRHump
C--------energy shift
         DELtafis(ib) = 0.300      !14./SQRT(A(Nnuc))
         GAMmafis(ib) = 0.6d0 !Gamma
C--------multiplier of atil
         AFIs(ib) = 1.d0
         ECFis(ib) = 0.d0
         vibf12(ib) = 1.d0
         vibfdt(ib) = 0.1d0
         vibfnorm(ib)= 1.d0
      ENDDO

      IF(FISmod(Nnuc).GT.0)THEN
         EFBm(1)=EFB(2)+2.0
         EFBm(2)=EFB(2)
         EFBm(3)=EFB(2)+0.1
         HM(1,1)= 1.2
         HM(1,2)=H(1,1)
         HM(1,3)=H(1,1)
 	   BFFm(1)=1.
  	   BFFm(2)=2.
  	   BFFm(3)=2.
         DO m=1,int(FISmod(Nnuc))+1
            DO nr = 1, NRFdis(2)
C--------------by default all widths are equal
               HM(nr,m) = HM(1,m)
               EFDism(nr,m)= EFDis(nr,2)
            ENDDO
            SHCfism(m)=SHCfis(2)
            DELtafism(m)=DELtafis(2)
            GAMmafism(m)=GAMmafis(2)
            AFIsm(m)=AFIs(2)
            ECFism(m)=ECFis(2)
            vibf12m(m)=vibf12(2)
            vibfdtm(m)=vibfdt(2)
            vibfnormm(m)=vibfnorm(2)
         ENDDO
      ENDIF  
c=================================================================
C---- writing data in FISSION.INP
      WRITE (79,'(a8)') 'Isotope:'
      WRITE (79,'(a40)') '----------------------------------------'
      WRITE (79,'(4x,a2,i3,2x,a2,i3)') 'Z=', INT(Z(Nnuc)), 'A=',
     &                                 INT(A(Nnuc))
      WRITE (79,'(a40)') '----------------------------------------'
      WRITE (79,*)' '
c
      IF(FISbar(Nnuc).EQ.3.)THEN
         WRITE (79,*)chstar
         WRITE (79,*)' RIPL-3 HFB numerical fission barrier'
         WRITE (79,*)chstar
         WRITE(79,'(i3)')npoints
         DO ii=1, npoints
            WRITE(79,'(i4,2f10.3)')ii, vdef_1d(ii),eps_1d(ii)
         ENDDO
         WRITE (79,*)
         WRITE (79,'(a15,i1,a15,i1)') ' Nr.parabolas =', NRBar,
     &                                '      Nr.wells=', NRWel
         NRHump = NRBar - NRWel
         nextr = nrbar
         WRITE (79,*)' '
         WRITE (79,*)' Index of extrema'
         WRITE (79,*)(iiextr(j), j = 1, nrbar)
         WRITE (79,*) ' Normalization factors for humps'
         IF(Nrhump.EQ.1)WRITE (79,'(2f10.3)')(barnorm(nh), nh=1,nrhump),
     &                                        hnorm
         IF(Nrhump.EQ.2)WRITE (79,'(3f10.3)')(barnorm(nh), nh=1,nrhump),
     &                                        hnorm
         IF(Nrhump.EQ.3)WRITE (79,'(4f10.3)')(barnorm(nh), nh=1,nrhump),
     &                                        hnorm
         WRITE (79,*)chstar
      ENDIF
c
      IF(FISbar(Nnuc).LT.3)THEN
         WRITE (79,'(a15,i1,a15,i1)') ' Nr.parabolas =', NRBar,
     &                                '      Nr.wells=', NRWel
         NRHump = NRBar - NRWel
         WRITE(79,*)
      ENDIF

      IF (Nrbar.EQ.1) THEN
         WRITE (79,'(a)') '    Va      ha    (in Mev) '
         WRITE (79,'(2f8.3)') EFB(1), Hcont(1)
         WRITE (79,*) ' '
         WRITE (79,'(2a10)') 'h2/2J(A)', '(in MeV)'
         WRITE (79,'(f9.4)') HJ(Nnuc,1)
         WRITE (79,*) ' '
         WRITE (79,'(a10)') 'Beta2(A)'
         WRITE (79,'(f9.4)') DEFfis(1)
         WRITE (79,*) ' '
      ENDIF
      
      IF (NRBar.EQ.3) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            WRITE (79,'(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
            WRITE (79,'(6f8.3,15x)') (EFB(i),H(1,i),i = 1,NRBar)
         ENDIF
         IF (FISmod(Nnuc).EQ.1.) THEN
            WRITE (79,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '    Vi      hi  (in Mev) '
            WRITE (79,'(8f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2), EFB(3), H(1,3)
         ENDIF
         IF (FISmod(Nnuc).EQ.2.) THEN
            WRITE (79,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)   Vi      hi  (in Mev) '
            WRITE (79,'(10f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                                EFBm(2), HM(1,2), EFBm(3), HM(1,3)
     &                                , EFB(3), H(1,3)
         ENDIF
         WRITE (79,*) ' '
         WRITE (79,'(4a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)',
     &                       '(in MeV)'
         WRITE (79,'(3f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (79,*) ' '
         WRITE (79,'(3a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
         WRITE (79,'(3f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (79,*) '  '
      ENDIF
      
      IF (NRBar.EQ.5) THEN
         WRITE (79,'(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi      hi   
     &    Vo      ho  (in Mev) '
         WRITE (79,'(10f8.3,15x)') (EFB(i),H(1,i),i = 1,NRBar)
         WRITE (79,*) ' '
         WRITE (79,'(6a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)',
     &                       'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
         WRITE (79,'(5f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (79,*) ' '
         WRITE (79,'(6a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(C)',
     &                       'Beta2(I)', 'Beta2(O)', '        '
         WRITE (79,'(5f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (79,*) ' '
      ENDIF
      
      WRITE (79,*)chstar
      WRITE (79,*)' Parameters of the imaginary potential used only if
     & FISOPT>0'
      WRITE (79,*)chstar
      WRITE (79,*) '      W0         W1         W2'
      DO iw=1,NRWel
         WRITE (79,'(3f11.4)') (WIMag(iw,i),i = 1,3)
      ENDDO  
      WRITE (79,*)
      WRITE (79,*)chstar
      WRITE (79,*)' Discrete transitional states'
      WRITE (79,*)chstar
c
      DO ibar = 1, NRBar
         IF(ibar.LE.NRHump)THEN
            WRITE (79,'(a39,I2,a2,I2)')
     &            ' Number  of  discrete states at hump'
     &             , ibar, ' =', NRFdis(ibar)
         ELSE
            WRITE (79,'(a39,I2,a2,I2)')
     &           ' Number  of  discrete states in well'
     &             , ibar, ' =', NRFdis(ibar)
         ENDIF
         WRITE (79,*) 'Kdis  Pidis   Edis    homega'
         DO nr = 1, NRFdis(ibar)
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))
     &           WRITE (79,'(1x, 1f3.1, 1x, 1i4, 2x, 1f8.3, 1x, 1f8.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar),
     &          H(nr,ibar)
            IF (FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2)
     &           WRITE (79,'(1x, 1f3.1, 1x, 1i4,1x, 4f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2)
            IF (FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2)
     &           WRITE (79,'(1x, 1f3.1, 1x, 1i4,1x, 6f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2), EFDism(nr,3), HM(nr,3)
         ENDDO
      ENDDO
      WRITE (79,*)
      WRITE (79,*)chstar
      WRITE (79,*)' Quantities used only if FISDEN=0 to calculate LD at
     & saddles'
      WRITE (79,*)chstar
      WRITE (79,'(14x,a4,1x,a9,1x,a5,4x,a4,2x,a10,3x,a3,5x,a6,3x,a5,
     &             4x,a4)')
     &           'Asym','shellcorr', 'Ushif','gamma','atilf/atil',
     &          'Ecf','VIB1/2','VIBdt','Norm'
      DO nr = 1, NRHump
            IF (FISmod(Nnuc).EQ.0. .OR.
     &         (FISmod(Nnuc).GT.0. .AND. nr.NE.2))
     &          WRITE (79,'(1x, A8, 1x, I1,4x,I1, 8f9.3)') 'Barrier',
     &          nr, BFF(nr), SHCfis(nr), DELtafis(nr), GAMmafis(nr),
     &          AFIs(nr), ECFis(nr),vibf12(nr),vibfdt(nr),vibfnorm(nr)
            IF (FISmod(Nnuc).GT.0. .AND. nr.EQ.2) THEN
               nrmod = INT(FISmod(Nnuc)) + 1
               DO m = 1, nrmod
                  WRITE (79,'(1x, A8, 1x, I1, 2x, I1, 1x, I1, 8f9.3)')
     &                  'Barrier', nr, m, BFFm(m), SHCfism(m),
     &                   DELtafism(m), GAMmafism(m), AFIsm(m), ECFism(m)
     &                   ,vibf12m(m),vibfdtm(m),vibfnormm(m)
               ENDDO
            ENDIF
      ENDDO
      WRITE (79,*)

      WRITE (79,*)chstar
      WRITE (79,*)'  Coefficients used only if FISDEN=3 to adjust HFB LD
     & at saddles '
      WRITE (79,*)chstar
      WRITE (79,*) '            Asym    Delta    alpha   Norm'
      DO ih = 1, nrhump
         bfi=int(bff(ih))
         WRITE (79,'(1x, A8, 1x, I1,4x,I1, 3f9.3)') 'Barrier ', ih,
     &          bfi, rohfbp_sd(ih), rohfba_sd(ih),rohfb_norm(ih)
      ENDDO
      WRITE (79,*)

      WRITE (79,*)chstar
      WRITE (79,*)'  Coefficient(s) used to calculate the direct continu

     &um weight(s) '
      WRITE (79,*)chstar
      DO nr = 1, nrwel
         awf(nr) = 0.d0
         WRITE (79,'(1x, A8, 1x, I1, 1f9.3)') '    Well', nr, awf(nr)
      ENDDO
      WRITE (79,*)
      WRITE (79,*)'****************************************************'
      WRITE (79,*)'****************************************************'
      RETURN
      END
C
C
      SUBROUTINE READ_INPFIS(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION vdef_1d(NFISBARPNT),eps_1d(NFISBARPNT)             ! NUMBAR
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr                         ! NUMBAR

      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr
C

C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*2 cara3, carz
      CHARACTER*10 cara8
      INTEGER i, ia, ibar,ih, ibaro, iz, m, mm, nr, nrmod,bfi
      INTEGER INT
      CHARACTER*40 line
      DOUBLE PRECISION rmiu
      REAL*8 centr,  heigth,  width(NFPARAB), ucentr, uheigth, uwidth

      DO nh =1, nfhump
         barnorm(nh) = 0.d0
      ENDDO
      hnorm=1.d0

      IF(NRSmooth(Nnuc).EQ.0) NRSmooth(Nnuc)= 5
      nrsm = NRSmooth(Nnuc)

      OPEN (79,FILE = 'FISSION.INP',STATUS = 'OLD',ERR = 400)
  100 READ (79,'(A8)',ERR=400,END=400) cara8
      IF (cara8.NE.'Isotope:') GOTO 100
      READ (79,*,ERR=385,END=385)
      READ (79,'(4x,a2,i3,2x,a2,i3)',ERR=385,END=385) 
     &    carz, iz, cara3, ia
      IF (carz.EQ.'Z=' .AND. iz.NE.INT(Z(Nnuc)) .OR. ia.NE.INT(A(Nnuc)))
     &    GOTO 100
      READ (79,*)
      READ (79,*)

      IF(FISbar(Nnuc).EQ.3.)THEN
         READ (79,'(//)',ERR=385,END=385)
         READ (79,'(i3)',ERR=385,END=385) npoints
         DO i = 1, npoints
            READ(79,'(i4,2f10.3)',ERR=385,END=385)
     &         ii, vdef_1d(i),eps_1d(i)
         ENDDO
         READ (79,*,ERR=385,END=385)
         READ (79,'(15x,i1,15x,i1)',ERR=385,END=385) NRBar, NRWel
         NRHump = NRBar - NRWel
         nextr = nrbar
         READ (79,'(/)',ERR=385,END=385)
         iiextr(0) = 1
         READ (79,*,ERR=385,END=385)(iiextr(j), j = 1, nrbar)
         READ (79,*,ERR=385,END=385)
         IF(Nrhump.EQ.1) READ (79,'(2f10.3)',ERR=385,END=385)
     &                       (barnorm(nh), nh=1,nrhump),hnorm
         IF(Nrhump.EQ.2) READ (79,'(3f10.3)',ERR=385,END=385)
     &                       (barnorm(nh), nh=1,nrhump),hnorm
         IF(Nrhump.EQ.3) READ (79,'(4f10.3)',ERR=385,END=385)
     &                       (barnorm(nh), nh=1,nrhump),hnorm
         READ (79,'(/)',ERR=385,END=385)
      ENDIF

      IF(FISbar(Nnuc).LT.3.)THEN
         READ (79,'(15x,i1,15x,i1)',ERR=385,END=385) NRBar, NRWel
         NRHump = NRBar - NRWel
         READ (79,'(/)',ERR=385,END=385)
      ENDIF
c
      nrmod = INT(FISmod(Nnuc)) + 1

      IF (FISmod(Nnuc).EQ.0.) READ (79,*,ERR=385,END=385) 
     &                          (EFB(i),HCOnt(i),i = 1,NRBar)
      IF (FISmod(Nnuc).EQ.1. .AND. NRWel.EQ.1) 
     &    READ (79,*,ERR=385,END=385) EFB(1),
     &    Hcont(1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFB(3),H(1,3)
      IF (FISmod(Nnuc).EQ.1. .AND. NRWel.EQ.0)
     &    READ (79,*,ERR=385,END=385) EFB(1),
     &    Hcont(1), EFBm(1), HM(1,1), EFBm(2), HM(1,2)
      IF (FISmod(Nnuc).EQ.2. .AND. NRWel.EQ.1) 
     &    READ (79,*,ERR=385,END=385) EFB(1),
     &    Hcont(1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFBm(3),HM(1,3),
     &    EFB(3), H(1,3)
      IF (FISmod(Nnuc).EQ.2. .AND. NRWel.EQ.0) 
     &    READ (79,*,ERR=385,END=385) EFB(1),
     &    Hcont(1), EFBm(1), HM(1,1), EFBm(2), HM(1,2), EFBm(3), HM(1,3)
c
      READ (79,' (/)',ERR=385,END=385)
      READ (79,*,ERR=385,END=385) (HJ(Nnuc,i),i = 1,NRBar)
      READ (79,' (/)',ERR=385,END=385)
      READ (79,*,ERR=385,END=385) (DEFfis(i),i = 1,NRBar)
      READ (79,'(////)',ERR=385,END=385)
      DO iw=1,NRWel
         READ (79,'(3f11.4)',ERR=385,END=385) (WIMag(iw,i),i = 1,3)
      ENDDO
      READ (79,'(///)',ERR=385,END=385)

      DO ibar = 1, NRBar
         READ (79,'(a39,I2,a2,I2)',ERR=385,END=385) 
     &      line, ibaro, cara8, NRFdis(ibar)
         READ (79,*,ERR=385,END=385)
         DO nr = 1, NRFdis(ibar)
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ibar.NE.2)) 
     &          READ (79,*,ERR=385,END=385)
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar),
     &          H(nr,ibar)
            IF (FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2) 
     &          READ (79,*,ERR=385,END=385)
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2)
            IF (FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2) 
     &          READ (79,*,ERR=385,END=385)
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2), EFDism(nr,3), HM(nr,3)
         ENDDO
      ENDDO
      READ (79,*,ERR=385,END=385)

C-----FISDEN(Nnuc)= 0 EMPIRE
C-----FISDEN(Nnuc)= 3 HFB

      IF(FISDEN(Nnuc).LE.1)THEN
         READ (79,'(///)',ERR=385,END=385)
         DO ib = 1, nrhump
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ib.NE.2)) THEN
                READ (79,'(10x,  I1,4x, I1, 8f9.3)',ERR=385,END=385)  i,
     &          BFF(ib), SHCfis(ib), DELtafis(ib), GAMmafis(ib),
     &          AFIs(ib),ECFis(ib),vibf12(ib),vibfdt(ib),vibfnorm(ib)
             ENDIF
             IF (FISmod(Nnuc).GT.0. .AND. ib.EQ.2) THEN
               DO m = 1, nrmod
                  READ (79,'(10x, I1, 2x, I1, 1x, I1, 8f9.3)',ERR=385,
     &                  END=385) i, mm,
     &                  BFFm(m), SHCfism(m), DELtafism(m), GAMmafism(m),
     &                  AFIsm(m), ECFism(m),
     &                  vibf12m(m),vibfdtm(m),vibfnormm(m)
               ENDDO
            ENDIF
         ENDDO
         IF(NRHump.EQ.1) READ (79,'(//////)',ERR=385,END=385)
         IF(NRHump.EQ.2) READ (79,'(///////)',ERR=385,END=385)
         IF(NRHump.EQ.3) READ (79,'(////////)',ERR=385,END=385)
c         READ (79,*)
      ENDIF

      IF(FISDEN(Nnuc).EQ.3)THEN
         IF(NRHump.EQ.1)READ (79,'(/////////)',ERR=385,END=385)
         IF(NRHump.EQ.2)READ (79,'(//////////)',ERR=385,END=385)
         IF(NRHump.EQ.3)READ (79,'(///////////)',ERR=385,END=385)

         DO ih = 1, nrhump
            READ (79,'(1x, A8, 1x, I1,4x,I1, 3f9.3)',ERR=385,END=385) 
     &        cara8, i, bfi,rohfbp_sd(ih), rohfba_sd(ih),rohfb_norm(ih)
            bff(i)= float(bfi)
         ENDDO
         READ (79,*)
      ENDIF

      READ (79,'(//)')
      DO nr = 1, nrwel
         READ (79,'(1x, A8, 1x, I1, 1f9.3)',ERR=385,END=385) 
     &          cara8, i, awf(nr)
      ENDDO
      GOTO 500
 385  CLOSE(79)
      WRITE(8,*) ' ERROR: Reading the fission input file' 
      WRITE(8,*) ' ERROR: Delete the fission input and restarts' 
      WRITE(8,*) ' ERROR: EMPIRE execution stopped' 
      STOP ' ERROR: Delete the fission input and restarts'
 400  CLOSE(79)
      OPEN(79,FILE='FISSION.INP',STATUS='OLD',ACCESS='APPEND')
      CALL INPFIS(nnuc)
 500  CLOSE (79)
c-----reading Fission input---done-------------------------
c----------------------------------------------------------
      IF(FISbar(Nnuc).EQ.3.)THEN
c--------width normalization
         DO i=1,npoints
            eps_1d(i) = eps_1d(i) * hnorm
         ENDDO
c--------global height normalization
         IF (barnorm(1).eq.10.) THEN
            DO i=1,npoints
               vdef_1d(i)=vdef_1d(i)*barnorm(2)
            ENDDO
         ELSE
c--------height normalization per hump
            iiextr(0) = 1
            iiextr(nextr+1)= npoints
            barmin =  vdef_1d(iiextr(2))
            DO n = 3, nextr-1
               IF( vdef_1d(iiextr(n)).lt.barmin)
     &              barmin= vdef_1d(iiextr(n))
            ENDDO
            DO nh = 1, nrbar, 2
               IF(barnorm(nh/2 + 1).ne.0.)THEN
                  DO id = iiextr(nh - 1),  iiextr(nh + 1)
                     IF(vdef_1d(id).ge.barmin)
     &                    vdef_1d(id) = vdef_1d(id) +  barnorm(nh/2 + 1)
     &                           * (vdef_1d(id) - barmin)
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
C------- Fitting parabola
         rmiu = 0.054d0*A(Nnuc)**(5.d0/3.d0)
         DO j = 1, nrbar
            CALL ParabFit(iiextr(j),nrsm,rmiu,eps_1d,vdef_1d,
     &        centr,  heigth,  width(j), ucentr, uheigth, uwidth)
            IF(width(j).LT.0.05d0) CYCLE ! Skipping very narrow peaks
         ENDDO
         DO k = 1, NRBar, 2
            EFB(int(k/2) + 1)    = Vdef_1d(iiextr(k))
            hcont(int(k/2) + 1)    = width(k)
            h(1,int(k/2) + 1)    = width(k)
            DEFfis(int(k/2) + 1) = eps_1d(iiextr(k))
         ENDDO
         DO k=2, NRBar,2
            EFB(NRHump + int(k/2))    = Vdef_1d(iiextr(k))
            hcont(NRHump + int(k/2))    = width(k)
            h(1,NRHump + int(k/2))    = width(k)
            DEFfis(NRHump + int(k/2)) = eps_1d(iiextr(k))
         ENDDO
      ENDIF

      IF (NRBar.EQ.2)THEN
         efb(nrbar+1) = 2.
         h(1,nrbar+1) = 1.
         NRBar=3
      ENDIF

C     For covariance
c      EFB(1) = EFB(1) * FISbin (Nnuc)
      IF(NRHump.GE.2) THEN
      DO i = 1, NRHump
         EFB(i) = EFB(i) * FISv_n (i,Nnuc)
         Hcont(i)=hcont(i)*FISh_n(i,Nnuc)
         AFIs(i)=AFIs(i)*FISa_n(i,Nnuc)
         DELtafis(i)= DEltafis(i)*FISd_n(i,Nnuc)
         vibfnorm(i)=vibfnorm(i)*FISn_n(i,Nnuc)
      ENDDO
      ENDIF

      IF(FISbar(Nnuc).LE.2.AND.FISmod(Nnuc).LT.0.1)CALL DEFO_FIS(Nnuc)

      RETURN
      END
c
c**************************************************************
      INTEGER FUNCTION Find_Extrem(Nnuc)
c**************************************************************
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C     Find all extrema of the smoothed deformation energy curve
C     Nrsmooth is defined globally(default) and it could be
C             modified in input for each nucleus
C
      DOUBLE PRECISION vdef_1d(NFISBARPNT), eps_1d(NFISBARPNT)
      INTEGER npoints
      LOGICAL logmin, logmax
      INTEGER j,k
      INTEGER iext
      INTEGER iiextr(0:2*NFPARAB),nnuc
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, iext
      iext = 0
      Find_Extrem = 0
      iiextr(0)=1
CC--------------------------------------------------------------------------
C     determination of the minima   and maxima
 10   iext=0
      do j = NRSmooth(Nnuc) + 1 , npoints - NRSmooth(Nnuc)
         logmax=.true.
         do k = j - NRSmooth(Nnuc), j + NRSmooth(Nnuc)
            if (k.eq.j) cycle
            if (Vdef_1D(k).gt.Vdef_1D(j)) logmax=.false.
         enddo
         if (logmax) then
            iext=iext+1
            iiextr(iext)=j
         endif
         logmin=.true.
         do k=j - NRSmooth(Nnuc),j + NRSmooth(Nnuc)
            if (k.eq.j.or.k.lt.1) cycle
            if (Vdef_1D(k).lt.Vdef_1D(j)) logmin=.false.
         enddo
         if (logmin) then
            iext=iext+1
            iiextr(iext)=j
         endif
      enddo
      IF(iext.GT.5) THEN
         WRITE (8,*) 
     &    ' WARNING: HFB numerical barrier of suspicious shape for Z=', 
     &     NINT(Z(nnuc)), ' A=', NINT(A(nnuc))
         WRITE(8,*) 
     &    ' WARNING: More than 5 extrema in HFB barrier, RESET to 5' 
         WRITE(8,*) 
     &    ' WARNING: EMPIRE can not deal with more than 3H barriers' 
         WRITE(8,*) 
     &    ' WARNING: You may want to use RIPL barriers (FISBAR 1)'
         iext = 5 
         WRITE(8,*) ' WARNING: --------------------------------------'
      ENDIF
      Find_Extrem = iext
      iiextr(iext+1)= npoints
      return
      end
c===================================================================
      SUBROUTINE DEFO_FIS(Nnuc)
c===================================================================
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION ejoin(2*NFPARAB),epsil(NFPARAB), ho(NFPARAB),
     &                 vjj(NFPARAB), smiu
      INTEGER i,  k, nrbarm
C-----deformations at saddles and wells and matching points-----------
      smiu = 0.1643167*A(Nnuc)**(5./6.)
      IF (NRBar.EQ.1)
     &    DEFfis(1) = SQRT(EFB(1))/(smiu * Hcont(1)) + DEF(1,Nnuc)
      IF (NRHump.EQ.2) THEN
         nrbarm = 3
         IF (NRWel.EQ.0) THEN
            EFB(3) = 2.
            Hcont(3) = 1.
         ENDIF
      ENDIF
      IF (NRHump.EQ.3) THEN
         nrbarm = 5
         IF (NRWel.EQ.0) THEN
            EFB(4) = 2.
            Hcont(4) = 1.
            EFB(5) = 5.
            Hcont(5) = 1.2
         ENDIF
      ENDIF
      DO k=1, NRBar,2
         HO(k) = Hcont(int(k/2)+1)
         VJJ(k) = EFB(int(k/2)+1)
      ENDDO
      DO k=2, NRBar,2
         HO(k) = Hcont(NRhump + int(k/2))
         VJJ(k) = EFB(NRhump + int(k/2))
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
     &              **2*(ejoin(2*(k-1)) - epsil(k - 1))
         IF (k.LT.nrbarm) ejoin(2*k) = epsil(k)
     &                                 + SQRT(( - 1)**k*((vjj(k+1)
     &                                 -vjj(k)))
     &                                 /(1.D0 + (ho(k)/ho(k+1))**2))
     &                                 /(smiu*ho(k))
      ENDDO
      DO k = 1, NRhump
         DEFfis(k) = epsil(2 * (k-1) + 1)
      ENDDO
      DO k = 1, NRwel
         DEFfis(NRhump + k) = epsil(2 * k)
      ENDDO
      RETURN
      END
C
c===================================================================
      SUBROUTINE WRITE_OUTFIS(Nnuc)
c===================================================================
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ACRtf(NFHUMP), UCRtf(NFHUMP), TCRtf(NFHUMP),       ! CRITFIS
     & DETcrtf(NFHUMP),SCRtf(NFHUMP),MORtcrt(NFPARAB),
     & MPArcrt(NFPARAB), ECOndf(NFHUMP)

      DOUBLE PRECISION vdef_1d(NFISBARPNT),eps_1d(NFISBARPNT)             ! NUMBAR
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr                         ! NUMBAR

      DOUBLE PRECISION  TFIso, TGIso, TISo, RFIso, PFIso                  ! FIS_ISO

      COMMON /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt,
     &                 MPArcrt, ECOndf

      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr

      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso

C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*36 cara1
      CHARACTER*1 chstar(70)
      INTEGER i, ib, ibar,ih, m, nr, j,bfi
      INTEGER INT
      DATA chstar/70*'='/
C
C
      WRITE (80,*)
      WRITE (80,'(a40)')
     &            '++++++++++++++++++++++++++++++++++++++++' 
      WRITE (80,'(a8)') 'Isotope:'
      WRITE (80,'(a40)') 
     &            '----------------------------------------'
      WRITE (80,'(4x,a2,i3,2x,a2,i3)') 'Z=', INT(Z(Nnuc)), 'A=',
     &                                 INT(A(Nnuc))
      WRITE (80,'(a40)') '----------------------------------------'

      IF (FISmod(Nnuc).EQ.0.) cara1 = '  Single-modal fission          '
      IF (FISmod(Nnuc).GT.0.) cara1 = '  Multimodal fission (in devel.)'

      WRITE (80,'(a8,f2.0,a36)') ' FISMOD = ', FISmod(Nnuc),cara1

      IF (FISbar(Nnuc).EQ.0.) cara1 = '  EMPIRE  internal library      '
      IF (FISbar(Nnuc).EQ.1.) cara1 = '  RIPL-3  (Exp.) values         '
      IF (FISbar(Nnuc).EQ.2.) cara1 = '  RIPL-3  HFB parabolic barriers'
      IF (FISbar(Nnuc).EQ.3.) cara1 = '  RIPL-3  HFB Numerical barriers'

      WRITE (80,'(a8,f2.0,a36)') ' FISBAR = ', FISbar(Nnuc),cara1

      IF (FISden(Nnuc).EQ.0.) cara1 = '  RIPL-3  EGSM LD               '
c      IF (FISden(Nnuc).EQ.1.) cara1 = '  Ignatyuk GSM LD               '
      IF (FISden(Nnuc).EQ.3.) cara1 = '  RIPL-3  HFB microscopic LD    '
      WRITE (80,'(a8,f2.0,a36)') ' FISDEN = ', FISden(Nnuc), cara1

      IF (FISopt(Nnuc).EQ.0.) cara1 = '  Full damping model (Ind.Barr.)'
      IF (FISopt(Nnuc).EQ.1.) cara1 = '  Optical model for fission     '
      IF (FISopt(Nnuc).EQ.2.)
     &    cara1 = '  Complex fission potential, isomeric fission'
      WRITE (80,'(a8,f2.0,a36)') ' FISOPT = ', FISopt(Nnuc),cara1
      WRITE(80,*)' '
      WRITE(80,*)chstar
      IF(FISbar(Nnuc).EQ.3.)THEN
         WRITE (80,*)' RIPL-3 HFB numerical fission barrier'
         WRITE(80,*)chstar
         WRITE(80,'(i3)')npoints
         DO ii=1, npoints
            WRITE(80,'(i4,2f10.3)')ii, vdef_1d(ii),eps_1d(ii)
         ENDDO
         WRITE (80,*)
         WRITE (80,*)' Index of extrema'
         WRITE (80,*)(iiextr(j), j = 1, nrbar)
         WRITE (80,*) ' Normalization factors for humps'
         WRITE (80,'(4f10.3)')(barnorm(nh), nh=1,nrhump),hnorm
      ENDIF
      WRITE (80,'(a15,i1,a15,i1)') ' Nr.parabolas =', NRBar,
     &                             '      Nr.wells=', NRWel
c     WRITE (80,*)' '
      WRITE(80,*)chstar
c
      IF (NRBar.EQ.1) THEN
         WRITE (80,'(a)') '    Va      ha    (in Mev) '
         WRITE (80,'(2f8.3)') EFB(1), Hcont(1)
         WRITE (80,*) ' '
         WRITE (80,'(2a10)') 'h2/2J(A)', '(in MeV)'
         WRITE (80,'(f9.4)') HJ(Nnuc,1)
         WRITE (80,*) ' '
         WRITE (80,'(a10)') 'Beta2(A)'
         WRITE (80,'(f9.4)') DEFfis(1)
         WRITE (80,*) ' '
      ENDIF
      IF (NRBar.EQ.2) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            WRITE (80,'(a)')
     &                    '    Va      ha      Vb      hb     (in Mev) '
            WRITE (80,'(4f8.3)') (EFB(i),Hcont(i),i = 1,NRBar)
         ENDIF
         IF (FISmod(Nnuc).EQ.1.) THEN
            WRITE (80,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '  (in Mev) '
            WRITE (80,'(6f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2)
         ENDIF
         IF (FISmod(Nnuc).EQ.2.) THEN
            WRITE (80,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)  (in Mev) '
            WRITE (80,'(8f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2), EFBm(3), HM(1,3)
         ENDIF
         WRITE (80,*) ' '
         WRITE (80,'(3a10)') 'h2/2J(A)', 'h2/2J(B)', '(in MeV)'
         WRITE (80,'(2f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(2a10)') 'Beta2(A)', 'Beta2(B)'
         WRITE (80,'(2f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (80,*) ' '
      ENDIF
      IF (NRBar.EQ.3) THEN
         IF (FISmod(Nnuc).EQ.0.) THEN
            WRITE (80,'(a,1x,a)')
     &       '    Va      ha      Vb      hb      Vi      hi  (in Mev) '
            WRITE (80,'(6f8.3,15x)') (EFB(i),Hcont(i),i = 1,NRBar)
         ENDIF
         IF (FISmod(Nnuc).EQ.1.) THEN
            WRITE (80,'(a,1x,a)')
     &         '       Va      ha     Vb(SL)   hb(SL)   Vb(ST)   hb(ST)'
     &         , '    Vi      hi  (in Mev) '
            WRITE (80,'(8f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                               EFBm(2), HM(1,2), EFB(3), H(1,3)
         ENDIF
         IF (FISmod(Nnuc).EQ.2.) THEN
            WRITE (80,'(a,1x,a)')
     &         '      Va      ha     Vb(SL)    hb(SL)  Vb(ST1)  hb(ST1)'
     &         , '  Vb(ST2)  hb(ST2)   Vi      hi  (in Mev) '
            WRITE (80,'(10f9.3,15x)') EFB(1), H(1,1), EFBm(1), HM(1,1),
     &                                EFBm(2), HM(1,2), EFBm(3), HM(1,3)
     &                              , EFB(3), H(1,3)
         ENDIF
         WRITE (80,*) ' '
         WRITE (80,'(4a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(I)',
     &                       '(in MeV)'
         WRITE (80,'(3f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(3a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(I)'
         WRITE (80,'(3f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (80,*) '  '
      ENDIF
      IF (NRBar.EQ.5) THEN
         WRITE (80,'(a,1x,a)')
     &'    Va      ha      Vb      hb      Vc       hc      Vi      hi   
     &    Vo      ho  (in Mev) '
         WRITE (80,'(10f8.3,15x)') (EFB(i),H(1,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(6a10)') 'h2/2J(A)', 'h2/2J(B)', 'h2/2J(C)',
     &                       'h2/2J(I)', 'h2/2J(O)', '(in MeV)'
         WRITE (80,'(5f9.4)') (HJ(Nnuc,i),i = 1,NRBar)
         WRITE (80,*) ' '
         WRITE (80,'(6a10)') 'Beta2(A)', 'Beta2(B)', 'Beta2(C)',
     &                       'Beta2(I)', 'Beta2(O)', '        '
         WRITE (80,'(5f9.4)') (DEFfis(i),i = 1,NRBar)
         WRITE (80,*) ' '
      ENDIF

c      IF (FISmod(Nnuc).EQ.0. .AND. NRBar.EQ.3) THEN
c         WRITE (80,*) ' '
c         WRITE (80,*) '  Tiso1/2 fission = ', TFIso, ' (s)'
c         WRITE (80,*) '  Tiso1/2 gamma   = ', TGIso, ' (s)'
c         WRITE (80,*) '  Tiso1/2 total   = ', TISo, ' (s)'
c         WRITE (80,*) '  Rfiso   = ', RFIso
c      ENDIF
      WRITE (80,*) ' '

      IF(FISOPT(Nnuc).GT.0)THEN
         WRITE (80,*) '      W0         W1         W2'
         DO iw=1,NRWel
            WRITE (80,'(3f11.4)') (WIMag(iw,i),i = 1,3)
         ENDDO
         WRITE (80,*)
      ENDIF

      WRITE (80,*)' Discrete transitional states'

c
      DO ibar = 1, NRBar
         IF(ibar.LE.NRHump)THEN
            WRITE (80,'(a39,I2,a2,I2)')
     &            ' Number  of  discrete states at hump'
     &             , ibar, ' =', NRFdis(ibar)
         ELSE
            WRITE (80,'(a39,I2,a2,I2)')
     &           ' Number  of  discrete states in well'
     &             , ibar, ' =', NRFdis(ibar)
         ENDIF
        
         WRITE (80,*) 'Kdis  Pidis   Edis    homega'
         DO nr = 1, NRFdis(ibar)
            IF (FISmod(Nnuc).EQ.0. .OR.
     &          (FISmod(Nnuc).GT.0. .AND. ibar.NE.2))
     &           WRITE (80,'(1x, 1f3.1, 1x, 1i4, 2x, 1f8.3, 1x, 1f8.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDis(nr,ibar),
     &          H(nr,ibar)
            IF (FISmod(Nnuc).EQ.1. .AND. ibar.EQ.2)
     &           WRITE (80,'(1x, 1f3.1, 1x, 1i4,1x, 4f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2)
            IF (FISmod(Nnuc).EQ.2. .AND. ibar.EQ.2)
     &           WRITE (80,'(1x, 1f3.1, 1x, 1i4,1x, 6f9.3)')
     &          SFDis(nr,ibar), IPFdis(nr,ibar), EFDism(nr,1), HM(nr,1),
     &          EFDism(nr,2), HM(nr,2), EFDism(nr,3), HM(nr,3)
         ENDDO
      ENDDO
      WRITE (80,*)

      IF(FISDEN(Nnuc).LE.1)THEN

         WRITE(80,*)'Parameters of EGSM LD'

         WRITE (80,'(14x,a4,1x,a9,1x,a5,4x,a4,2x,a10,3x,a3,5x,a6,3x,a5,
     &             4x,a4)')
     &           'Asym','shellcorr', 'Ushif','gamma','atilf/atil',
     &          'Ecf','VIB1/2','VIBdt','Norm'

         DO nr = 1, NRHump
            IF (FISmod(Nnuc).EQ.0. .OR.
     &         (FISmod(Nnuc).GT.0. .AND. nr.NE.2))
     &          WRITE (80,'(1x, A8, 1x, I1,4x,I1, 8f9.3)') 'Barrier',
     &          nr, BFF(nr), SHCfis(nr), DELtafis(nr), GAMmafis(nr),
     &          AFIs(nr), ECFis(nr), vibf12(nr), vibfdt(nr),vibfnorm(nr)
            IF (FISmod(Nnuc).GT.0. .AND. nr.EQ.2) THEN
               nrmod = INT(FISmod(Nnuc)) + 1
               DO m = 1, nrmod
                  WRITE (80,'(1x, A8, 1x, I1, 2x, I1, 1x, I1, 8f9.3)')
     &                  'Barrier', nr, m, BFFm(m), SHCfism(m),
     &                   DELtafism(m), GAMmafism(m), AFIsm(2), ECFism(m)
     &                   , vibf12m(m), vibfdtm(m),vibfnormm(m)
               ENDDO
            ENDIF
         ENDDO
         WRITE (80,*)

         DO ib = 1, nrhump
            WRITE (80,*) 'Barrier  ', ib
            WRITE (80,'(3(A9,f9.5),a9,f11.5)') 'Acrt=', ACRtf(ib),
     &             'Ucrt=', UCRtf(ib), 'Econd=', ECOndf(ib), 'DETcrt=',
     &             DETcrtf(ib)
            WRITE (80,'(A9,f9.5,A9,f9.5)') 'Tcrt=', TCRtf(ib), 'Scrt=',
     &             SCRtf(ib)
         ENDDO 
         WRITE (80,*)
      ENDIF

      IF(FISDEN(Nnuc).EQ.3)THEN
         WRITE (80,*)'Normalization factors for HFB LD'
         WRITE (80,*) '                   Delta    alpha   Norm'
         DO ih = 1, nrhump
            bfi=int(bff(ih))
            WRITE (80,'(1x, A8, 1x, I1,4x,I1, 3f9.3)') 'Barrier ', ih,
     &          bfi, rohfbp_sd(ih), rohfba_sd(ih),rohfb_norm(ih)
         ENDDO
      ENDIF  

      WRITE (80,*)
      WRITE (80,*)'Coefficient(s) used to calculate the direct continuum
     & weight(s) '
      DO nr = 1, nrwel
         WRITE (80,'(1x, A8, 1x, I1, 1f9.3)') '    Well', nr, awf(nr)
      ENDDO
      WRITE (80,*)

      RETURN
      END
