Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
C
      SUBROUTINE PCROSS(Sigr,Totemis,Xsinl)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      REAL*8, PARAMETER :: C1 = 3.6824121D17, EPS = 1.D-4
      INTEGER*4, PARAMETER :: PMAx = 50
C
C COMMON variables
C
      REAL*8, DIMENSION(0:NDEjc) :: CROss
      REAL*8, DIMENSION(2*PMAx + 3) :: FA, LFA
      REAL*8, DIMENSION(0:NDEjc,PMAx) :: L
      INTEGER*4 :: NHEq
      REAL*8, DIMENSION(0:NDEjc,NDEx) :: SPEc
      REAL*8 :: VV
      REAL*8, DIMENSION(NDAngecis) :: XCOs
      COMMON /CALC5 / L, NHEq
      COMMON /KALB  / XCOs
      COMMON /PEXS  / CROss, SPEc
      COMMON /PFACT / FA, LFA
      COMMON /VWELL / VV
C
C Dummy arguments
C
      REAL*8 :: Sigr, Totemis, Xsinl
C
C Local variables
C
      REAL*8 :: aat, azt, cme, dbreak, dpickup, ebind, ec, eee, emaxi, 
     &          emini, emis, er, excnq, fanisot, ff, ff1, ff2, ff3, fr, 
     &          ftmp, gc, hlp1, pc, scompn, sg, step, theta, vsurf, vvf, 
     &          wb, wda
      INTEGER*4 :: ac, ao, ap, ar, h1, hh, i, icon, icon3, ien, ienerg, 
     &             ihmax, j, p, zc, zo, zp, zr
      LOGICAL :: callpcross
      REAL*8 :: DBLE
      REAL*8, DIMENSION(NDAngecis) :: ddxs
      REAL*8 :: DENSW, SGAM
      REAL*8, DIMENSION(NDEx) :: eint, fint
      REAL*8, DIMENSION(PMAx) :: em
      REAL*8, DIMENSION(4,4) :: flm
      REAL :: FLOAT
      REAL*8, DIMENSION(0:NDEjc) :: g, pair
      REAL :: ggg
      INTEGER :: iang, ie, nejc, nexrt, nnur
      INTEGER*2, DIMENSION(0:NDEjc) :: iemax, iemin
      INTEGER :: INT, NINT
      REAL*8, DIMENSION(4,PMAx,NDEjc), SAVE :: r
      CHARACTER(12) :: status
      REAL*8, DIMENSION(0:NDEjc,PMAx,NDEx) :: we
C
C*** End of declarations rewritten by SPAG
C
C
C     FORMAL PARAMETERS:
C     SIGR = REACTION CROSS SECTION
C                ---- OUTPUT -------
C     CROss(NDEjc+1) = CHANNEL EMISSION CROSS SECTION
C     FR = DEPLETION FACTOR FOR PREEQUILIBRIUM
C     SPEc(0:NDEjc,NDEx) = Channel emission spectra, 0 means gamma channel
C
C     Some variables:
C     AC,ZC = MASS AND ATOMIC NUMBER OF THE COMPOUND NUCLEUS
C     EC CN EXCITATION ENERGY, LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     GC,PC LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     G(NEJc),PAIR(NEJc) THE SAME FOR RESIDUAL NUCLEI AFTER EMISSION
C     MFPp = MEAN FREE PATH PARAMETER
 
 
 
 
 
C     To correct bug found by M Pigni and C Mattoon, a variable "callpcross" value is saved between calls
C
C
      DATA callpcross/.FALSE./
C
C     NPRoject - projectile nejc number
C
C     INITIALIZATION
 
C     Projectile mass and charge number
      ap = AEJc(NPRoject)
      IF(NPRoject.EQ.0)ap = 0
      zp = ZEJc(NPRoject)
      cme = MFPp/1.4D21
C-----Excitation energy (MeV)
      ec = EXCn
C-----Compound nucleus
      ac = A(1)
      zc = Z(1)
C
C     Parametrization of the well depth for p-h LD calculations
C     According to TALYS manual,v.0.64 (2004) A.Koning et al,
C
      vvf = 38.D0
      vsurf = vvf
      IF(ap.EQ.1.AND.zp.EQ.1)vsurf = 22.D0 + 
     &                               16.D0*EINl**4/(EINl**4 + (450.D0/
     &                               FLOAT(ac)**0.333333D0)**4)
      IF(ap.EQ.1.AND.zp.EQ.0)vsurf = 12.D0 + 
     &                               26.D0*EINl**4/(EINl**4 + (245.D0/
     &                               FLOAT(ac)**0.333333D0)**4)
 
      fr = 0.D0
      Totemis = 0.D0
      DO i = 0, NDEjc
        CROss(i) = 0.
      ENDDO
      DO hh = 1, PMAx
        em(hh) = 0.D0
      ENDDO
      em(1) = 1.D0 - QDFrac
      em(2) = QDFrac
C
      ggg = GDIv
      IF(GDIv.EQ.0)ggg = 13.D0
      gc = FLOAT(ac)/ggg*GTIlnor(1)
      pc = 0.D0
C     pc = ROPar(3,1)
      IF(pc.EQ.0.D0)THEN
        ftmp = 0.
        IF(ac.GT.0.D0)ftmp = 12./SQRT(DBLE(FLOAT(ac)))
        pc = ftmp                                             ! odd
        IF(MOD(ac,2).EQ.0.AND.MOD(zc,2).EQ.0)pc = 2*ftmp      ! e-e
        IF(MOD(ac,2).EQ.0.AND.MOD(zc,2).EQ.1)pc = 0.D0        ! o-o
      ENDIF
C     We supress pairing if requested in input
C     Pairing is implicitly considered in the distribution of discrete levels
      IF(NPAirpe.EQ.0)pc = 0.D0
C-----Compound gamma emitting nucleus
      g(0) = gc
      pair(0) = pc
C
C-----MAXIMUM EXCITON NUMBER NHEq
C
C     CHMax determines the maximum allowable HOLE number
C     for exciton model emission spectra calculations
C
C     It is much better aproximation than using fixed
C     exciton number as the emission loops increase
C     with energy as it is expected on physical basis.
C
C     Nmax = Aproj + NHEq -1 (is equal to NHEq for nucleon induced reactions)
C
C     Chinese group uses CHMax = 0.88, but it gives to hard PE spectrum in Th232
C     so it was reduced by IAEA to 0.2.
C     IF(CHMax . EQ. 0.d0) CHMax = 0.2d0 ! default value
Cig   However it was increased by Ignatyuk to the standard value =ln2gt=.540Sqrt(gEc).
      IF(CHMax.EQ.0.D0)CHMax = 0.540D0     ! default value
      NHEq = MAX(5,MIN(PMAx - 1,NINT(CHMax*SQRT(gc*ec))) + 1)
C-----ZERO ARRAY INITIALIZATION
      DO nejc = 0, NDEjc
        iemin(nejc) = 2
        iemax(nejc) = NDEx
        DO ienerg = 1, NDEx
          SPEc(nejc,ienerg) = 0.D0
          DO hh = 1, PMAx
            we(nejc,hh,ienerg) = 0.D0
          ENDDO
        ENDDO
        DO hh = 1, PMAx
          L(nejc,hh) = 0.D0
        ENDDO
      ENDDO
      pair = 0
C
C-----NEJcm is the maximum number of particles emitted
C
      DO nejc = 1, NEJcm
        ar = ac - AEJc(nejc)
        zr = zc - ZEJc(nejc)
        nnur = NREs(nejc)
        IF(nnur.LT.0)CYCLE
        g(nejc) = FLOAT(ar)/ggg*GTIlnor(nnur)
        ftmp = 0.
        IF(ar.GT.0.D0)ftmp = 12./SQRT(DBLE(FLOAT(ar)))
        pair(nejc) = ftmp
        IF(MOD(ar,2).EQ.0.AND.MOD(zr,2).EQ.0)pair(nejc) = 2*ftmp
        IF(MOD(ar,2).EQ.0.AND.MOD(zr,2).EQ.1)pair(nejc) = 0.D0
C        We supress pairing if requested in input
C        Pairing is implicitly considered in the distribution of discrete levels
        IF(NPAirpe.EQ.0)pair(nejc) = 0.D0
C--------Maximum and minimum energy bin
        excnq = EXCn - Q(nejc,1)
C
C        Assuming PE calculates over discrete levels' region as well
        nexrt = MAX(INT(excnq/DE + 1.0001),1)
 
C        IDNa(1,6) = 0  ! discrete N is included even with ECIS active
C        IDNa(3,6) = 0  ! discrete P is included even with ECIS active
C        IDNa(5,6) = 1  ! gammas
C        IDNa(11,6) = 0  ! discrete A is included even with ECIS active
C        IDNa(12,6) = 0  ! discrete D is included even with ECIS active
C        IDNa(13,6) = 0  ! discrete T is included even with ECIS active
C        IDNa(14,6) = 0  ! discrete H is included even with ECIS active
        IF(nejc.EQ.1.AND.IDNa(1,6).EQ.0)
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
        IF(nejc.EQ.2.AND.IDNa(3,6).EQ.0)
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
        IF(nejc.EQ.3.AND.IDNa(11,6).EQ.0)
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
        IF(nejc.EQ.4.AND.IDNa(12,6).EQ.0)
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
        IF(nejc.EQ.5.AND.IDNa(13,6).EQ.0)
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
        IF(nejc.EQ.6.AND.IDNa(14,6).EQ.0)
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
 
        iemax(nejc) = nexrt
        DO ienerg = 2, nexrt
          eee = DE*(ienerg - 1)
          IF(EMAx(nnur).GT.eee)iemax(nejc) = ienerg
          IF(ETL(5,nejc,nnur).EQ.0)THEN
            sg = SIGabs(ienerg + 5,nejc,nnur)
          ELSE
            sg = SIGabs(ienerg,nejc,nnur)
          ENDIF
          IF((sg.LT.1.D-15).AND.nejc.GT.1.AND.ienerg.LE.iemax(nejc))
     &       iemin(nejc) = ienerg
        ENDDO
      ENDDO
 
      WRITE(8,1100)
C
Cig---Direct reaction spectra for d,p and d,t only
C
      dbreak = 0.D0
      dpickup = 0.D0
      scompn = Sigr
      IF(ZEJc(0).EQ.1.D0.AND.AEJc(0).EQ.2.D0)THEN
        WRITE(8,1010)
 1010   FORMAT(/5X,
     &   ' Deuteron Stripping and Pick-up Parameterization (C. Kalbach)'
     &   ,//)
        CALL DTRANS(iemin,iemax)
 1020   FORMAT(7x,5F8.2)
        IF(DXSred.GT.0.D0)THEN
          scompn = Sigr - CROss(2) - CROss(5)
          scompn = Sigr - CROss(2) - CROss(5)
C
          IF(scompn.LE.0.D0)THEN
            scompn = 0.D0
            dbreak = Sigr/(CROss(2) + CROss(5))*CROss(2)
            dpickup = Sigr/(CROss(2) + CROss(5))*CROss(5)
            CROss(2) = dbreak
            CROss(5) = dpickup
            DO ienerg = 1, NDEx
              SPEc(2,ienerg) = Sigr/(CROss(2) + CROss(5))*SPEc(2,ienerg)
              SPEc(5,ienerg) = Sigr/(CROss(2) + CROss(5))*SPEc(5,ienerg)
            ENDDO
            WRITE(8,1020)EINl, Sigr, CROss(2), CROss(5)
            WRITE(8,1030)
 1030       FORMAT(/,1X,
     &       'Warning: Direct emission exhausted reaction cross section'
     &       )
          ENDIF
C         write(8,99003) Einl,sigr,cross(2),cross(5)
          dbreak = CROss(2)
          dpickup = CROss(5)
        ENDIF
      ENDIF
C
      IF(scompn.NE.0.D0)THEN
 
        WRITE(8,1040)
 1040   FORMAT(//5X,' Preequilibrium decay (PCROSS)',/)
        WRITE(8,1050)MFPp
 1050   FORMAT(/,1X,'Mean free path parameter = ',F4.2,/)
 
C-----Maximum and minimum energy bin for gamma emission
        nnur = NREs(0)
C     No PE contribution to discrete for gammas
        nexrt = MAX(INT((EXCn-ECUt(nnur))/DE + 1.0001),1)
C     Assuming PE calculates over discrete levels' region as well
C     nexrt = MAX(INT(EXCn/DE + 1.0001),1)
        iemax(0) = nexrt
 
        IF(.NOT.callpcross)CALL RQFACT(NHEq,r)
        callpcross = .TRUE.
                           ! To avoid r factor recalculation at each call
C
C-----EMISSION RATES CALCULATIONS FOLLOWS
C
C-----PRIMARY PARTICLE LOOP
C
        DO nejc = 0, NEJcm
          nnur = NREs(nejc)
          IF(nnur.LT.0)CYCLE
          IF(nejc.NE.0)THEN
            ao = AEJc(nejc)
            zo = ZEJc(nejc)
            ar = ac - ao
            zr = zc - zo
 
C              mn    neutron mass  1.008 665 amu
C              mp    proton mass   1.007 276 amu
C              ma    4He mass      4.001 506 amu
C              md    deuteron mass 2.013 553 amu
C              mt    triton mass   3.015 501 amu
C              m3He  3He mass      3.014 932 amu
 
C              me    electron mass 5.485 79910-4 amu
 
C           ff1 = (2*Sin+1)*redmass
            IF(ao.EQ.1.AND.zo.EQ.0)ff1 = 2.0173*ar/(ar + 1.0087)     ! n
            IF(ao.EQ.1.AND.zo.EQ.1)ff1 = 2.0145*ar/(ar + 1.0073)     ! p
            IF(ao.EQ.4.AND.zo.EQ.2)ff1 = 4.0015*ar/(ar + 4.0015)     ! alpha
            IF(ao.EQ.2.AND.zo.EQ.1)ff1 = 6.0408*ar/(ar + 2.0136)     ! d
            IF(ao.EQ.3.AND.zo.EQ.1)ff1 = 6.0312*ar/(ar + 3.0156)     ! t
          ELSE
            ar = ac
            zr = zc
            ao = 0
            zo = 0
            ff1 = 1.D0/AMUmev
          ENDIF
C        EMPIRE tuning factor is used (important to describe capture) RCN, june 2005
          ff1 = ff1*TUNe(nejc,1)*TUNepe(nejc)
C
C--------PARTICLE-HOLE LOOP
C
          DO h1 = 1, NHEq
            icon = 0
            hh = h1 - 1
C           Found bug that decreased PE contribution, thanks to A Voinov
C         Oct 1, 2010 MH & RC, next line was commented to correct the bug
C           IF (hh.EQ.0 .AND. nejc.NE.0) GOTO 50
            p = hh + ap
C
C           Defining well depth for p-h LD calculations
C
            VV = vvf
C           hh .eq. 1 is recommended in the TALYS manual
C           I use hh .LE. 1 to enhance PE photon emission
C           if (hh.eq.1) VV = vsurf
            IF(hh.EQ.1.AND.nejc.LE.2)VV = vsurf
C
C
            ff2 = DENSW(gc,pc,p,hh,ec)
            IF(ff2.NE.0.)THEN
C
C-----------PRIMARY ENERGY CYCLE
C
              DO ienerg = iemin(nejc), iemax(nejc)
                eee = DE*(ienerg - 1)
C--------------Inverse cross section
                IF(nejc.EQ.0)THEN
                  aat = ac
                  azt = zc
                  sg = SGAM(aat,azt,eee)
                ELSEIF(ETL(5,nejc,nnur).EQ.0)THEN
                  sg = SIGabs(ienerg + 5,nejc,nnur)
                ELSE
                  sg = SIGabs(ienerg,nejc,nnur)
                ENDIF
                IF(sg.NE.0.)THEN
                  er = EMAx(nnur) - eee
                  IF(er.GT.EPS)THEN
                    hlp1 = 0.D0
C--------------PREEQ n & p EMISSION
                    IF(nejc.GT.0.AND.nejc.LE.2)THEN
                      wda = DENSW(g(nejc),pair(nejc),p - ao,hh,er)
                      IF(wda.GT.0.)hlp1 = wda*r(1,h1,nejc)
C--------------PREEQ CLUSTER EMISSION
                    ELSEIF(nejc.GT.2.AND.nejc.LE.NEJcm)THEN
                      CALL PREFORMATION(flm,eee)
                      DO j = 1, ao
                        wda = DENSW(g(nejc),pair(nejc),p - j,hh,er)
                        IF(wda.GT.0.)hlp1 = hlp1 + flm(ao,j)
     &                     *wda*r(j,h1,nejc)
                      ENDDO
C--------------PREEQ GAMMA EMISSION
                    ELSEIF(nejc.EQ.0)THEN
                      wda = DENSW(gc,pc,p,hh,er)
                      hlp1 = wda*DBLE(p + hh)/(DBLE(p + hh) + eee*gc)
                      IF(ap.LE.1.AND.hh.GE.1)hlp1 = hlp1 + 
     &                   gc*eee*DENSW(gc,pc,p - 1,hh - 1,er)
     &                   /(DBLE(p + hh - 2) + eee*gc)
                    ENDIF
                    IF(hlp1.NE.0.)THEN
                      ff3 = hlp1*eee*sg
                      IF(nejc.EQ.0)ff3 = ff3*eee
                      ff = ff1*ff3/ff2
                      we(nejc,h1,ienerg) = ff*C1
                      icon = icon + 1
                      icon3 = ienerg
                      eint(icon) = eee
                      fint(icon) = ff
                    ENDIF
                  ENDIF
                ENDIF
C              write(8,'(1x,3i3,1x,7(d12.6,1x))')
C    &             nejc,h1,ienerg,hlp1,sg,eee,wda,ff1,ff2,ff3
              ENDDO
C-----------END OF EMISSION ENERGY LOOP
C
C-----------INTEGRATION PROCEDURE #1 (EMISSION RATES)
C
              IF(icon.GT.0)THEN
                IF(nejc.NE.0)THEN
                  emaxi = EMAx(nnur)
                  IF(DE*icon3.LT.emaxi)emaxi = DE*icon3
                  emini = 0.D0
                  IF(iemin(nejc).GT.2)emini = DE*(iemin(nejc) - 2)
                  hlp1 = fint(1)*(eint(1) - emini) + fint(icon)
     &                   *(emaxi - eint(icon))
                ELSE
                  hlp1 = fint(1)*eint(1) + fint(icon)
     &                   *(EMAx(nnur) - eint(icon))
                ENDIF
                DO ien = 2, icon
                  hlp1 = hlp1 + (eint(ien) - eint(ien - 1))
     &                   *(fint(ien - 1) + fint(ien))
                ENDDO
                L(nejc,h1) = 0.5D0*hlp1*C1
              ENDIF
            ENDIF
          ENDDO
C--------END OF PARTICLE-HOLE LOOP
        ENDDO
C-----END OF EMITTED PARTICLE LOOP
C
C-----TRANSITION RATES CALCULATIONS FOLLOWS
C
        CALL TRANSIT(ec,gc,ap,NHEq)
C
C-----MASTER EQUATION SOLUTION
C
        CALL RESOL(em,ihmax,ap,cme)
      ENDIF
 
C--------setup model matrix (IDNa) defining which model is used where
C                        ECIS   MSD   MSC   ????    HMS   PCROSS
C                        1     2     3      4      5      6
C        1 neut. disc.   x     x     0      0      x      x
C        2 neut. cont.   0     x     x      x      x      x
C        3 prot. disc.   x     x     0      0      x      x
C        4 prot. cont.   0     x     x      x      x      x
C        5 gamma         0     0     x      x      0      x
C        6 alpha. cont.  0     0     0      0      0      x
C        7 deut . cont.  0     0     0      0      0      x
C        8 trit . cont.  0     0     0      0      0      x
C        9 He-3 . cont.  0     0     0      0      0      x
C       10 LI   . cont.  0     0     0      0      0      x
C       11 alpha. cont.  0     0     0      0      0      x
C       12 deut . cont.  0     0     0      0      0      x
C       13 trit . cont.  0     0     0      0      0      x
C       14 He-3 . cont.  0     0     0      0      0      x
C
C-----PARTICLE LOOP FOR EMISSION SPECTRA CALCULATIONS
C
      Totemis = 0.D0
      DO nejc = 0, NEJcm
C        Skipping cross sections if MSD and MSC active
        IF(nejc.EQ.0.AND.IDNa(5,6).EQ.0)CYCLE
        IF(nejc.EQ.1.AND.IDNa(2,6).EQ.0)CYCLE
        IF(nejc.EQ.2.AND.IDNa(4,6).EQ.0)CYCLE
        IF(nejc.EQ.3.AND.IDNa(6,6).EQ.0)CYCLE
        IF(nejc.EQ.4.AND.IDNa(7,6).EQ.0)CYCLE
        IF(nejc.EQ.5.AND.IDNa(8,6).EQ.0)CYCLE
        IF(nejc.EQ.6.AND.IDNa(9,6).EQ.0)CYCLE
 
        hlp1 = 0.D0
        DO ienerg = iemin(nejc), iemax(nejc)
          emis = 0.D0
          DO h1 = 1, ihmax
            wb = we(nejc,h1,ienerg)
            IF(wb.GT.0.)emis = emis + wb*em(h1)
          ENDDO
          step = 1.D0
          IF(ienerg.EQ.iemin(nejc).OR.ienerg.EQ.iemax(nejc))step = 0.5D0
          SPEc(nejc,ienerg) = SPEc(nejc,ienerg) + scompn*emis*step
          hlp1 = hlp1 + scompn*emis*DE*step
        ENDDO
        CROss(nejc) = hlp1 + CROss(nejc)
        Totemis = Totemis + CROss(nejc)
      ENDDO
 
      IF(IOUt.GE.1)THEN
        DO nejc = 0, NEJcm
          IF(nejc.GT.0.AND.nejc.LE.3)THEN       !nucleons and alpha
            IF(IDNa(2*nejc,6).EQ.0)THEN
              status = '  (ignored) '
            ELSE
              status = '  (accepted)'
            ENDIF
            WRITE(8,
     &    '(1X,A2,'' PCROSS emission cross section'',G12.5,'' mb'',A12)'
     &    )SYMbe(nejc), CROss(nejc), status
          ELSEIF(nejc.GE.4)THEN     !complex particle
            IF(IDNa(3 + nejc,6).EQ.0)THEN
              status = '  (ignored) '
            ELSE
              status = '  (accepted)'
            ENDIF
            WRITE(8,
     &    '(1X,A2,'' PCROSS emission cross section'',G12.5,'' mb'',A12)'
     &    )SYMbe(nejc), CROss(nejc), status
          ELSE   !gamma
            IF(IDNa(5,6).EQ.0)THEN
              status = '  (ignored) '
            ELSE
              status = '  (accepted)'
            ENDIF
            WRITE(8,
     &    '(1X,A2,'' PCROSS emission cross section'',G12.5,'' mb'',A12)'
     &    )'g ', CROss(nejc), status
          ENDIF
C---------We don't need this spectrum printout any more but I leave it
C---------commented for checking in case anything goes wrong.
C         DO ienerg = iemin(nejc), iemax(nejc)
C                  WRITE(8,'(1x, F7.2, 2x, 7(D12.6, 1x))')
C    &             DE*(ienerg - 1), spec(nejc, ienerg)
C         ENDDO
C         WRITE(8, *)'==========================='
        ENDDO
      ENDIF
      IF(MSD + MSC.EQ.0)THEN
        fr = Totemis/Sigr
        WRITE(8,1070)Totemis, fr
      ENDIF
      IF(MSD + MSC.GT.0)THEN
        fr = (Totemis + Xsinl)/Sigr
        WRITE(8,1060)Xsinl, Totemis, fr
      ENDIF
C
C     write(*,*) 'Middle of PCROSS :',
C    >            sngl(totemis),sngl(xsinl),sngl(SIGr)
C
 1060 FORMAT(/1X,'MSD+MSC preequilibrium total cross section   =',F8.2,
     &       /1X,'PCROSS  preequilibrium total cross section   =',F8.2,
     &       ' mb'/1X,'total   preequilibrium fraction              =',
     &       F8.2)
 1070 FORMAT(/1X,'PCROSS preequilibrium total cross section   =',F8.2,
     &       ' mb'/1X,'PCROSS preequilibrium fraction              =',
     &       F8.2)
      IF(ZEJc(0).EQ.1.D0.AND.AEJc(0).EQ.2.D0)THEN
        WRITE(8,1080)
 1080   FORMAT(/1x,'Kalbach parameterization for pick-up and stripping',
     &         ' is considered')
        WRITE(8,1090)dbreak, dpickup
 1090   FORMAT(1X,'PCROSS d,p breakup cross section   =',F8.2,' mb'/1X,
     &         'PCROSS d,t pickup  cross section   =',F8.2)
      ENDIF
C
C-----Transfer PCROSS results into EMPIRE. Call to ACCUMSD is needed later
      DO i = 1, NDAng
        theta = DBLE(i - 1)/DBLE(NDAng - 1)*PI
        XCOs(i) = COS(theta)
      ENDDO
 
      Totemis = 0.D0
      DO nejc = 0, NEJcm  ! over ejectiles
        nnur = NREs(nejc)
        IF(nnur.LT.0.OR.CROss(nejc).LE.0.D0)CYCLE
        IF(nejc.GT.0)THEN    !particle
          IF(nejc.EQ.1.AND.IDNa(2,6).EQ.0)CYCLE
          IF(nejc.EQ.2.AND.IDNa(4,6).EQ.0)CYCLE
          IF(nejc.EQ.3.AND.IDNa(6,6).EQ.0)CYCLE
          IF(nejc.EQ.4.AND.IDNa(7,6).EQ.0)CYCLE
          IF(nejc.EQ.5.AND.IDNa(8,6).EQ.0)CYCLE
          IF(nejc.EQ.6.AND.IDNa(9,6).EQ.0)CYCLE
          ao = AEJc(nejc)
          zo = ZEJc(nejc)
          excnq = EXCn - Q(nejc,1)
          ebind = Q(nejc,1)
        ELSE  !gamma
          IF(IDNa(5,6).EQ.0)CYCLE
          ao = 0
          zo = 0
          nnur = 1
          excnq = EXCn
          ebind = 0.D0
        ENDIF
 
        CSMsd(nejc) = CSMsd(nejc) + CROss(nejc)
        Totemis = Totemis + CROss(nejc)
        DO ie = iemin(nejc), iemax(nejc)
          eee = DE*(ie - 1)
          ftmp = SPEc(nejc,ie)
          IF(ftmp.LE.0.D0)CYCLE
          CSEmsd(ie,nejc) = CSEmsd(ie,nejc) + ftmp
          DO iang = 1, NDAng
            ddxs(iang) = 0.D0
          ENDDO
C
C           Kalbach systematic for PCROSS DDX calculations
C           fanisot is assumed 1, i.e. pure PE emission
C
          fanisot = 1.D0
C           fmsd set to 0.d0 means isotropic distribution
          CALL KALBACH(ac,zp,ap - zp,zo,ao - zo,EINl,EXCn,ebind,eee,
     &                 ftmp,fanisot,ddxs,NDAng)
          DO iang = 1, NDAng
            CSEa(ie,iang,nejc,1) = CSEa(ie,iang,nejc,1) + ddxs(iang)
          ENDDO
        ENDDO
      ENDDO
C
C      write(8,*) 'End of PCROSS    :',sngl(totemis),sngl(Xsinl)
C      write(*,*) 'End of PCROSS    :',sngl(totemis),sngl(Xsinl)
C
Cig ***  totemis includes the preequilibrium contribution only !  ******
C     totemis=sigr*fr
      WRITE(8,*)
      WRITE(8,1100)
 1100 FORMAT(/' ',57('-')/)
      END SUBROUTINE PCROSS
 
!---------------------------------------------------------------------------
 
      SUBROUTINE KALBACH(Jcom,Jpin,Jninp,Jpout,Jnout,Elab,Esys,Bin,Eps,
     &                   Total,Fmsd,Sigma,Ndang)
C
C     Converted to subroutine for EMPIRE by Roberto Capote (May 2005)
C
C     Taken from RIPL-1 files
C
C     Compound nucleus mass and charge (ac,zc) -> jcom,jzcom
C     Projectile charge and neutron numbers    -> jpin,jnin
C     Ejectile charge and neutron numbers      -> jpout,jnout
C     Incident energy in MeV [lab system]      -> elab
C     Excitation energy in MeV [cms]           -> esys
C     Binding energy of the ejectile in MeV    -> bin
C     Emission energy in MeV                   -> eps
C     Emitted cross section [mb/MeV]           -> total
C     Prequilibrium fraction of total          -> fmsd (assumed pure PE here)
C     sigma(NDAng)                             -> DDXS(angle) [mb/MeV/str]
C
C     MB Chadwick modified angel92 to include photuclear reactions,
C     Oct 95, according to the paper to be published in J. Nucl.
C     Sci Eng. (Japan), Nov 1995.  Changes labeled with "cmbc"
C     ANGEL92
C     continuum angular distributions
C     from empirical systematics based on exponentials of cos theta
C          see Phys. Rev. C 37 (1988) 2350
C
C     Written by C. Kalbach
C          April 1987
C     Revised February 1992 to give a smooth transitions
C     Single formulae for E1 and E3.
C          see Nucl. Sci. Eng. 115 (1993) 43
C
C     Third term in 'a' needed for incident N and d but not alpha.
C          Tentatively used for incident t and He-3
C     Transition energies tentatively set at
C          Et1 = 130 MeV (a=n,p,d,t,He3) or 260 MeV (a=alpha)
C          Et3 = 35 MeV (a=n,p,d,t,He3)
C     (only nucleon values of Et1; N and d values of Et3 known)
C
C     *The angel of the Lord encampeth round about them that fear him,
C     *and delivereth them.  (Psalm 34.7)
C
      IMPLICIT REAL*8(a - H,O - Z)
      IMPLICIT INTEGER(i - N)
      INCLUDE 'dimension.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(NDAngecis) :: XCOs
      COMMON /KALB  / XCOs
C
C Dummy arguments
C
      REAL*8 :: Bin, Elab, Eps, Esys, Fmsd, Total
      INTEGER :: Jcom, Jninp, Jnout, Jpin, Jpout, Ndang
      REAL*8, DIMENSION(NDAngecis) :: Sigma
C
C Local variables
C
      REAL*8 :: a, arg, xnorm
      REAL*8 :: aphnuc, e1, e3, epscm, er, esysr, facmom, facrefr, gth, 
     &          sig, x, xmb, y
      REAL*8 :: DCOSH, DEXP, DSINH, DSQRT
      INTEGER :: i, jflagph, jin, jnin, jout
C
C*** End of declarations rewritten by SPAG
C
Cmbc  MB Chadwick, added coding Oct 95, for photonuclear reactions
      jflagph = 0
      jnin = Jninp
      IF(Jpin.EQ.0.AND.jnin.EQ.0)THEN
C        the following is a modification for photonuclear reactions
        jflagph = 1
        jnin = 1
      ENDIF
Cmbc  I follow the prescription in the paper "Photonuclear angular
Cmbc  distribution systematics in the quasideuteron regime", MB Chadwick,
Cmbc  PG Young, and S Chiba, to be published in the Nov 1995 issue of
Cmbc  the Journal of Nucl. Sci and Tech. (Japan). The basic idea is
Cmbc  to first calculate the Kalbach MSD a-parameter assuming a neutron
Cmbc  projectile instead of a photon, and the modify a to account for
Cmbc  (a) reduced forwward peaking since a photon carries less momentum;
Cmbc  (b) less diffraction/refraction effects since the photon is undistorted.
Cmbc  end of mbc code (more later)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     Calculate energy independent quantities
C     and print headings
C
      jin = Jpin + jnin
      jout = Jpout + Jnout
C
C     Start of parametrization
C
      er = 1.
      IF(jin.GT.3)er = 2.
C     er = Et1/130.
      esysr = Esys/er
      e1 = Esys
      IF(esysr.GT.190.)THEN
        e1 = 130.*er
      ELSEIF(esysr.GT.80.)THEN
        x = (esysr - 130.)/12.
        x = 1. + EXP(x)
        e1 = Esys/x
        x = (136. - esysr)/12.
        x = 1. + EXP(x)
        e1 = e1 + 130.*er/x
      ENDIF
      IF(jin.LE.3)THEN
        e3 = Esys
        IF(esysr.GT.51.)THEN
          e3 = 35.*er
        ELSEIF(esysr.GT.21.)THEN
          x = (esysr - 35.)/3.2
          x = 1. + EXP(x)
          e3 = Esys/x
          x = (36.6 - esysr)/3.2
          x = 1. + EXP(x)
          e3 = e3 + 35.*er/x
        ENDIF
        xmb = 1
        IF(jout.EQ.4)xmb = 2.
        IF(Jpout.EQ.0)xmb = 0.5
      ENDIF
C
C     energy dependent input
C     calculate and print angular distributions
C
      IF(Fmsd.LE.0.)Fmsd = 1.D0
      epscm = Eps + Bin
      y = epscm*e1/(Esys*130.)
      a = 5.2*y + 4.*y*y*y
      IF(jin.LE.3)THEN
        y = epscm*e3/(Esys*35.)
        a = a + 1.9*y*y*y*y*xmb
      ENDIF
 
      xnorm = a*Total/(12.5664*DSINH(a))
 
      DO i = 1, Ndang
        arg = a*XCOs(i)
        sig = Fmsd*DSINH(arg) + DCOSH(arg)
Cmbc%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Cmbc    added code for photonuclear reactions. Modify
Cmbc    a which was calculated for a neutron, to be valid for a
Cmbc    photon projectile:
        IF(jflagph.EQ.1)THEN
          facmom = DSQRT(Elab/(2.D0*939))
          facrefr = 9.3/DSQRT(Eps)
          IF(facrefr.LT.1.)facrefr = 1.
          IF(facrefr.GT.4.)facrefr = 4.
          aphnuc = a*facmom*facrefr
          gth = ((2.*aphnuc)/(DEXP(aphnuc) - DEXP(-aphnuc)))
          gth = gth*(1./(12.5664))*DEXP(aphnuc*XCOs(i))
Cmbc      Now put in MSC as isotropic, giving:
          Sigma(i) = ((1. - Fmsd)/(12.5664)) + (Fmsd*gth)
          Sigma(i) = Sigma(i)*Total
          CYCLE
        ENDIF
        Sigma(i) = sig*xnorm
      ENDDO
      RETURN
      END SUBROUTINE KALBACH
 
!---------------------------------------------------------------------------
 
      SUBROUTINE RQFACT(Heq,R)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER*4, PARAMETER :: PMAx = 50
C
C COMMON variables
C
      REAL*8, DIMENSION(2*PMAx + 3) :: FA, LFA
      COMMON /PFACT / FA, LFA
C
C Dummy arguments
C
      INTEGER*4 :: Heq
      REAL*8, DIMENSION(4,PMAx,NDEjc) :: R
C
C Local variables
C
      INTEGER*4 :: ab, ac, ap, at, h1, hhh, i, j, j1, l, m, maxj, minj, 
     &             nb, np, nt, p, zb, zc, zp, zt
      REAL :: ALOG, FLOAT
      REAL*8 :: f1, f2, f21, f22, f23, f24, s1, s2, ta, tn, tz, uuu, zzz
      INTEGER :: nejc
C
C*** End of declarations rewritten by SPAG
C
C
C     INDC(CPR)-014 and references there in
C
C     FACTORIAL CALCULATION
C
      LFA(1) = 0.
      LFA(2) = 0.
      LFA(3) = 0.
      FA(1) = 1.
      FA(2) = 1.
      FA(3) = 1.
      DO i = 4, 2*PMAx + 3
        LFA(i) = ALOG(FLOAT(i - 3)) + LFA(i - 1)
        FA(i) = (i - 3)*FA(i - 1)
      ENDDO
      DO nejc = 1, NEJcm
        DO h1 = 1, PMAx
          DO l = 1, 4
            R(l,h1,nejc) = 1.D0
          ENDDO
        ENDDO
      ENDDO
      ac = A(1)
      zc = Z(1)
      ap = AEJc(NPRoject)
      at = ac - ap
      zp = ZEJc(NPRoject)
      zt = zc - zp
      nt = at - zt
      np = ap - zp
      tn = nt
      ta = at
      tz = zt
      zzz = tz/ta
      uuu = 1.D0 - zzz
C
C-----Q-FACTOR CALCULATION FOR PARTICLE EMISSION
C
      DO nejc = 1, NEJcm
        ab = AEJc(nejc)
        zb = ZEJc(nejc)
        nb = ab - zb
        DO l = 1, ab
          DO h1 = 1, Heq
            hhh = h1 - 1
            p = hhh + ap
            IF(p.GE.l)THEN
              m = ab - l
              minj = MAX(0,l - nb)
              maxj = MIN(l,zb)
              s1 = 0.D0
              DO i = 0, hhh
                f1 = zzz**i*uuu**(hhh - i)
     &               *EXP(LFA(hhh + 3) - LFA(hhh - i + 3) - LFA(i + 3))
                s2 = 0.D0
                DO j = minj, maxj
                  f21 = 1.D0
                  IF(j.GE.1)THEN
                    DO j1 = 0, j - 1
                      f21 = f21*FLOAT(zp + i - j1)
                    ENDDO
                    f21 = f21/FA(j + 3)
                  ENDIF
                  f22 = 1.D0
                  IF(l - j.GE.1)THEN
                    DO j1 = 0, l - j - 1
                      f22 = f22*FLOAT(np + hhh - i - j1)
                    ENDDO
                    f22 = f22/FA(l - j + 3)
                  ENDIF
                  f23 = 1.D0
                  IF(zb - j.GE.1)THEN
                    DO j1 = 0, zb - j - 1
                      f23 = f23*FLOAT(zt - i - j1)
                    ENDDO
                    f23 = f23/FA(zb - j + 3)
                  ENDIF
                  f24 = 1.D0
                  IF(nb - l + j.GE.1)THEN
                    DO j1 = 0, 2 - l + j - 1
                      f24 = f24*FLOAT(nt - hhh + i - j1)
                    ENDDO
                    f24 = f24/FA(nb - l + j + 3)
                  ENDIF
                  s2 = f21*f22*f23*f24 + s2
                ENDDO
                s1 = s1 + f1*s2
              ENDDO
              f2 = 1.D0
              DO j = 0, m - 1
                f2 = f2*(at - hhh - j)
              ENDDO
              f2 = EXP(LFA(l + 3) + LFA(m + 3) + LFA(p - l + 3)
     &             - LFA(p + 3) + LFA(ab + 3) - LFA(zb + 3)
     &             - LFA(nb + 3))/f2
              R(l,h1,nejc) = s1*f2*(ta/tz)**zb*(ta/tn)**nb
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      DO nejc = 1, NEJcm
        ab = AEJc(nejc)
        DO h1 = 1, Heq
          DO l = 1, ab
            R(l,h1,nejc) = R(l,h1,nejc)/R(l,Heq,nejc)
          ENDDO
        ENDDO
      ENDDO
      END SUBROUTINE RQFACT
 
!---------------------------------------------------------------------------
 
      SUBROUTINE TRANSIT(E,Gc,Ap,Nheq)
      IMPLICIT LOGICAL*1(A - Z)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER*4, PARAMETER :: PMAx = 50
C
C COMMON variables
C
      REAL*8, DIMENSION(PMAx) :: LM, LP
      INTEGER*4 :: NMAx
      COMMON /CALC3 / LP, LM, NMAx
C
C Dummy arguments
C
      INTEGER*4 :: Ap, Nheq
      REAL*8 :: E, Gc
C
C Local variables
C
      REAL*8 :: a0p, uc0
      REAL :: FLOAT
      INTEGER*4 :: h, h1, i, n, p
C
C*** End of declarations rewritten by SPAG
C
C
C-----TRANSITION RATES CALCULATIONS FOLLOWS
C
      DO i = 1, PMAx
        LP(i) = 0.
        LM(i) = 0.
      ENDDO
      NMAx = Nheq
      DO h1 = 1, Nheq
        h = h1 - 1
        p = h + Ap
        n = p + h
        a0p = FLOAT(p*(p - 1) + h*(h - 1))*.25
        uc0 = E - a0p/Gc
        IF(uc0.LE.0.D0)THEN
          NMAx = h1 - 1
          RETURN
        ENDIF
C
C--------3/8 is coming from the Gupta paper
C
        LM(h1) = FLOAT(p*h)*FLOAT((n - 2)*(n - 1))/(Gc*Gc*uc0)*(3./8.)
        LP(h1) = uc0*(3./8.)
      ENDDO
C
C     END OF TRANSITION RATES CALCULATIONS [ NORMALIZED TO CME ]
C
      END SUBROUTINE TRANSIT
 
!---------------------------------------------------------------------------
 
      SUBROUTINE RESOL(Em,Ih2,Ap,Cme)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER*4, PARAMETER :: PMAx = 50
C
C COMMON variables
C
      REAL*8, DIMENSION(0:NDEjc,PMAx) :: L
      REAL*8, DIMENSION(PMAx) :: LM, LP
      INTEGER*4 :: NHEq, NMAx
      COMMON /CALC3 / LP, LM, NMAx
      COMMON /CALC5 / L, NHEq
C
C Dummy arguments
C
      INTEGER*4 :: Ap, Ih2
      REAL*8 :: Cme
      REAL*8, DIMENSION(PMAx) :: Em
C
C Local variables
C
      REAL*8, DIMENSION(PMAx) :: b, c, e, ln, ls
      INTEGER*4 :: h1, hhh, i, ij, n
      REAL*8 :: hlp1, hlp2, hlp4
C
C*** End of declarations rewritten by SPAG
C
      DO i = 1, PMAx
        ln(i) = 0.
      ENDDO
      n = NMAx
      IF(n.GT.PMAx)n = PMAx
      DO h1 = 1, n
        DO i = 0, NEJcm
          ln(h1) = ln(h1) + L(i,h1)
        ENDDO
C-----------------------------------------
C--------NEVER COME BACK ASUMPTION
        LM(h1) = LM(h1)*1.D-7
C-----------------------------------------
        ls(h1) = Cme*ln(h1) + LP(h1) + LM(h1)
      ENDDO
      IF(IOUt.GE.3.AND.NEJcm.LE.7)WRITE(8,1010)
 1010 FORMAT(/2X,'N',5X,'T r a n s i t i o n   r a t e s   ',
     &       '        E  m  i  s  s  i  o  n     r  a  t  e  s',//,5X,
     &'   plus     minus     TOTAL              TOTAL     gammas   neutr
     &ons   protons   alphas   deuteron   triton     He-3'/)
      IF(IOUt.GE.3.AND.NEJcm.EQ.7)WRITE(8,1020)
 1020 FORMAT(/2X,'N',5X,'T r a n s i t i o n   r a t e s   ',
     &       '        E  m  i  s  s  i  o  n     r  a  t  e  s',//,5X,
     &'   plus     minus     TOTAL              TOTAL     gammas   neutr
     &ons   protons   alphas   deuteron   triton     He-3    light ions'
     &/)
      DO h1 = 1, n
        hhh = h1 - 1
        ij = 2*hhh + Ap
        hlp1 = LP(h1)/Cme
        hlp2 = LM(h1)/Cme
        hlp4 = hlp1 + hlp2
        IF(IOUt.GE.3)WRITE(8,1030)ij, hlp1, hlp2, hlp4, ln(h1), 
     &                            (L(i,h1),i = 0,NEJcm)
 1030   FORMAT(I3,3E10.3,10X,9E10.3)
      ENDDO
C
C-----INITIAL CONDITIONS
C
      DO i = 1, PMAx
        b(i) = Em(i)
        Em(i) = 0.
      ENDDO
      c(1) = 0.D0
      DO h1 = 1, n - 1
        c(h1 + 1) = -LP(h1)
        e(h1) = -LM(h1 + 1)
      ENDDO
      CALL MASTER(c,ls,e,b,n,PMAx)
C--------------------------------------------------------------------
C-----NEVER COME BACK ASSUMPTION(ONLY PE-CONTRIBUTION)
      DO hhh = NHEq, n
        b(hhh) = 0.
      ENDDO
C--------------------------------------------------------------------
      DO h1 = 1, n
        Em(h1) = b(h1)*Cme
        IF(Em(h1).NE.0.)Ih2 = h1
      ENDDO
      Ih2 = MIN(Ih2,NHEq)
      IF(IOUt.GE.3)WRITE(8,1040)2*(Ih2 - 1) + Ap, Ih2, CHMax
 1040 FORMAT(/3X,'Nmax',I3,' Hmax =',I3,' Coeff. CHMax =',F4.2/)
      IF(IOUt.GE.3)WRITE(8,1050)
 1050 FORMAT(/3X,'TIME INTEGRALS OF TAU(N)'/3X,' N   UP TO Nmax       ')
      IF(IOUt.GE.3)THEN
        DO h1 = 1, Ih2
          hhh = 2*(h1 - 1) + Ap
          WRITE(8,1060)hhh, Em(h1)
 1060     FORMAT(I5,2E14.3)
        ENDDO
        WRITE(8,*)
      ENDIF
      END SUBROUTINE RESOL
 
!---------------------------------------------------------------------------
 
      SUBROUTINE MASTER(Subd,Diag,Superd,B,N,Ndim)
      IMPLICIT LOGICAL(A - Z)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER*4 :: N, Ndim
      REAL*8, DIMENSION(Ndim) :: B, Diag, Subd, Superd
C
C Local variables
C
      REAL*8 :: DABS
      INTEGER*4 :: k, kb, kp1, nm1, nm2
      REAL*8 :: t
C
C*** End of declarations rewritten by SPAG
C
      Subd(1) = Diag(1)
      nm1 = N - 1
      IF(nm1.GE.1)THEN
        Diag(1) = Superd(1)
        Superd(1) = 0.0D0
        Superd(N) = 0.0D0
        DO k = 1, nm1
          kp1 = k + 1
          IF(DABS(Subd(kp1)).GE.DABS(Subd(k)))THEN
            t = Subd(kp1)
            Subd(kp1) = Subd(k)
            Subd(k) = t
            t = Diag(kp1)
            Diag(kp1) = Diag(k)
            Diag(k) = t
            t = Superd(kp1)
            Superd(kp1) = Superd(k)
            Superd(k) = t
            t = B(kp1)
            B(kp1) = B(k)
            B(k) = t
          ENDIF
          IF(Subd(k).EQ.0.0D0)STOP 
     &            ' DIAGONAL ELEMENT = 0 in SOLUTION of MASTER equation'
          t = -Subd(kp1)/Subd(k)
          Subd(kp1) = Diag(kp1) + t*Diag(k)
          Diag(kp1) = Superd(kp1) + t*Superd(k)
          Superd(kp1) = 0.0D0
          B(kp1) = B(kp1) + t*B(k)
        ENDDO
      ENDIF
      IF(Subd(N).NE.0.0D0)THEN
C
C--------BACK ITERATION
C
        nm2 = N - 2
        B(N) = B(N)/Subd(N)
        IF(N.GT.1)THEN
          B(nm1) = (B(nm1) - Diag(nm1)*B(N))/Subd(nm1)
          IF(nm2.GE.1)THEN
            DO kb = 1, nm2
              k = nm2 - kb + 1
              B(k) = (B(k) - Diag(k)*B(k + 1) - Superd(k)*B(k + 2))
     &               /Subd(k)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      END SUBROUTINE MASTER
 
!---------------------------------------------------------------------------
 
      SUBROUTINE PREFORMATION(Flm,E)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      REAL*8 :: E
      REAL*8, DIMENSION(4,4) :: Flm
C
C Local variables
C
      INTEGER :: l
      REAL*8, DIMENSION(5) :: x
C
C*** End of declarations rewritten by SPAG
C
C
C IWAMOTO,HARADA COALESCENCE PICK-UP MODEL, PHYS.REV.1982,V.26,P.1821
C
C Fitted and coded by R. Capote Noy, November 2002
C
C     Input : Energy E in cms
C     Output: FLM(Ejectile Mass,L)
C             L=1,Aejectile
C
C     i.e. FLM(4,L) means alpha particle preformation factor (L=1,4)
C
C-----FOR FAST POLYNOMIAL EVALUATION
      x(1) = E
      DO l = 2, 5
        x(l) = E*x(l - 1)
      ENDDO
C-----DEUTERON (MASS = 2)
      IF(E.LT.80.D0)THEN
        Flm(2,2) = MAX(0.00821D0 + 0.02038D0*x(1) + 5.95941D-4*x(2)
     &             - 2.24726D-5*x(3) + 2.38917D-7*x(4)
     &             - 8.34053D-10*x(5),0.0D0)
      ELSE
        Flm(2,2) = 1.
      ENDIF
      Flm(2,1) = MAX(1.D0 - Flm(2,2),0.0D0)
C-----TRITIUM or HELIUM-3 (MASS = 3)
      IF(E.LT.70.D0)THEN
        Flm(3,1) = MAX(0.57315D0 - 0.02083D0*x(1) + 3.19204D-4*x(2)
     &             - 2.85876D-6*x(3) + 1.26332D-8*x(4),0.D0)
      ELSE
        Flm(3,1) = 0.D0
      ENDIF
      IF(E.LT.170.)THEN
        Flm(3,3) = MAX(0.00705D0 - 0.00164D0*x(1) + 2.16549D-4*x(2)
     &             - 1.73867D-6*x(3) + 5.24069D-9*x(4)
     &             - 5.79848D-12*x(5),0.D0)
      ELSE
        Flm(3,3) = 1.D0
      ENDIF
      Flm(3,2) = MAX(1.D0 - Flm(3,1) - Flm(3,3),0.D0)
C-----ALPHA PARTICLES (M=4)
      IF(E.LT.70.D0)THEN
        Flm(4,1) = MAX(0.29119 - 0.01434*x(1) + 3.34045E-4*x(2)
     &             - 4.10957E-6*x(3) + 2.02375E-8*x(4),0.D0)
      ELSE
        Flm(4,1) = 0.D0
      ENDIF
      IF(E.LT.130.D0)THEN
        Flm(4,2) = 0.60522 + 1.30071E-4*x(1) - 1.77653E-4*x(2)
     &             + 1.64048E-6*x(3) - 4.55562E-9*x(4)
     &             + 2.09521E-12*x(5)
      ELSE
        Flm(4,2) = 0.D0
      ENDIF
      IF(E.LT.30.D0)THEN
        Flm(4,4) = 0.D0
      ELSEIF(E.LT.300.D0)THEN
        Flm(4,4) = 0.01917 - 0.00307*x(1) + 9.57966E-5*x(2)
     &             - 4.07085E-7*x(3) + 5.28037E-10*x(4)
     &             + 7.92029E-16*x(5)
      ELSE
        Flm(4,4) = 1.D0
      ENDIF
      IF(E.LT.300.D0)THEN
        Flm(4,3) = MAX(1.D0 - Flm(4,1) - Flm(4,2) - Flm(4,4),0.D0)
      ELSE
        Flm(4,3) = 0.D0
      ENDIF
      END SUBROUTINE PREFORMATION
 
!---------------------------------------------------------------------------
 
      FUNCTION DENSW(G,D,P,H,E)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER*4, PARAMETER :: PMAx = 50
C
C COMMON variables
C
      REAL*8, DIMENSION(2*PMAx + 3) :: FA, LFA
      REAL*8 :: VV
      COMMON /PFACT / FA, LFA
      COMMON /VWELL / VV
C
C Dummy arguments
C
      REAL*8 :: D, E, G
      INTEGER*4 :: H, P
      REAL*8 :: DENSW
C
C Local variables
C
      REAL*8 :: a, fac, sumx, u
      REAL*8 :: DEXP, DLOG
      INTEGER*4 :: j, n
C
C*** End of declarations rewritten by SPAG
C
C
C  RIPL FORMULATION
C
C  G - LEVEL DENSITY PARAMETER
C  D - PAIRING CORRECTION
C  P(H) - PARTICLE(HOLE) NUMBER
C  E - EXCITATION ENERGY
 
      DENSW = 0.D0
      n = P + H
      IF(n.LE.0)RETURN
 
C     Following RIPL TECDOC (LD chapter)
C
      a = .5D0*(P*P + H*H)
      sumx = 0.D0
      DO j = 0, H
        fac = LFA(P + 3) + LFA(n + 2) + LFA(j + 3) + LFA(H - j + 3)
        u = G*(E - D - j*VV) - a
C       Changed Sept. 2010
        IF(u.LE.0.)CYCLE
        sumx = sumx + ( - 1)**j*G*(DEXP((n-1)*DLOG(u) - fac))
      ENDDO
      IF(sumx.LT.0.D0)RETURN
      DENSW = sumx
      RETURN
      END FUNCTION DENSW
 
!---------------------------------------------------------------------------
 
      FUNCTION DENSW1(G,D,P,H,E)
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER*4, PARAMETER :: PMAx = 50
C
C COMMON variables
C
      REAL*8, DIMENSION(2*PMAx + 3) :: FA, LFA
      COMMON /PFACT / FA, LFA
C
C Dummy arguments
C
      REAL*8 :: D, E, G
      INTEGER*4 :: H, P
      REAL*8 :: DENSW1
C
C Local variables
C
      REAL*8 :: a, fac, u
      REAL*8 :: DEXP, DLOG
      INTEGER*4 :: n
C
C*** End of declarations rewritten by SPAG
C
C
C  RIPL FORMULATION
C
C  G - LEVEL DENSITY PARAMETER
C  D - PAIRING CORRECTION
C  P(H) - PARTICLE(HOLE) NUMBER
C  E - EXCITATION ENERGY
      DENSW1 = 0.D0
      n = P + H
      IF(n.LE.0)RETURN
 
      a = .5D0*(P*P + H*H)
 
      fac = LFA(P + 3) + LFA(n + 2) + LFA(H + 3)
      u = G*(E - D) - a
      IF(u.LE.0.)RETURN
      DENSW1 = G*(DEXP((n-1)*DLOG(u) - fac))
 
      RETURN
      END FUNCTION DENSW1
 
!---------------------------------------------------------------------------
 
      FUNCTION SGAM(A,Z,Eg)
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      INTEGER :: KEY_gdrgfl, KEY_shape
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
C
C Dummy arguments
C
      REAL*8 :: A, Eg, Z
      REAL*8 :: SGAM
C
C Local variables
C
      REAL*8 :: a3, egr, gam, sgm
      REAL*8 :: GAMMA_STRENGTH
C
C*** End of declarations rewritten by SPAG
C
C
C     Photoabsorption cross-section in "mb"
C     If "Key_shape = 0" old expression for photoabsorption
C     cross-section  is used, namely:
C     [1] S.A.FAYANS, "Lectures,DUBNA,feb.1982"
C     SGAM = GAMMA ABSORPTION CROSS SECTION in mb
      IF(KEY_shape.NE.0)THEN
        SGAM = (10.D0/8.674D-7)*Eg*GAMMA_STRENGTH(Z,A,0.D0,0.D0,Eg,
     &         KEY_shape)
        RETURN
      ELSE
C        If "Key_shape = 0" old expression for photoabsorption
C        cross-section  is used, namely:
C        SGAM_NEW = SGAM
C        [1] S.A.FAYANS
C        "Lectures,DUBNA,feb.1982"
C        SGAM = GAMMA ABSORPTION CROSS SECTION in mb
        a3 = A**(.3333333333333333D0)
        egr = 29.*SQRT((1.D0 + 2.D0/a3)/a3)
        gam = 5.D0
        sgm = 53.2D0*(A - Z)*Z/A
        SGAM = sgm*gam*Eg*Eg/((Eg*Eg - egr*egr)**2 + (gam*Eg)**2)
      ENDIF
      END FUNCTION SGAM
 
 
 
