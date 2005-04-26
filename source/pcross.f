Ccc   * $Author: Capote $
Ccc   * $Date: 2005-04-26 17:28:20 $
Ccc   * $Id: pcross.f,v 1.23 2005-04-26 17:28:20 Capote Exp $
C
      SUBROUTINE PCROSS(Sigr,Totemis)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C PARAMETER definitions
C
      REAL*8 C1, EPS, PI26
      PARAMETER (C1 = 3.6824121D17,EPS = 1.D-4,PI26 = 1.6449340107D0)
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 L(0:NDEJC,PMAX)
      INTEGER*4 NHEq
      COMMON /CALC5 / L, NHEq
C
C Dummy arguments
C
      REAL*8 Sigr, Totemis
C
C Local variables
C
      DOUBLE PRECISION aat, azt, cme, ec, eee, eint(NDEX), em(PMAX),
     &       emaxi, emini, emis, er, excnq, ff, ff1, ff2, ff3,
     &       fint(NDEX), flm(4,4), fr, ftmp, gc, hlp1, pc,
     &       r(4,PMAX,NDEJC), sg, wb, wda , zo

      DOUBLE PRECISION cross(0:NDEJC), g(0:NDEJC), pair(0:NDEJC),
     &       spec(0:NDEJC,NDEX), we(0:NDEJC,PMAX,NDEX)

      INTEGER*4 ac, ao, ap, ar, h1, hh, i, icon, icon3, ien, ienerg,
     &          ihmax, j, p, zc, zr

      LOGICAL callpcross
      DOUBLE PRECISION DBLE, DENSW
      REAL FLOAT
      INTEGER ie, iloc, izares, nejc, nexrt, nnur
      INTEGER*2 iemax(0:NDEJC), iemin(0:NDEJC), nures(0:NDEJC)
      INTEGER INT, NINT
      DOUBLE PRECISION SGAM
      CHARACTER*12 status
      SAVE r
C
C
C
C     FORMAL PARAMETERS:
C     SIGR = REACTION CROSS SECTION
C                ---- OUTPUT -------
C     CROss(NDEjc+1) = CHANNEL EMISSION CROSS SECTION
C     FR = DEPLETION FACTOR FOR PREEQUILIBRIUM
C     SPEc(NDEjc+1,NDEx) = Channel emission spectra, +1 means gamma channel
C
C     Some variables:
C     AC,ZC = MASS AND ATOMIC NUMBER OF THE COMPOUND NUCLEUS
C     EC CN EXCITATION ENERGY, LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     GC,PC LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     G(NEJc),PAIR(NEJc) THE SAME FOR RESIDUAL NUCLEI AFTER EMISSION
C     MFPp = MEAN FREE PATH PARAMETER
C
C
      DATA callpcross/.FALSE./
C
C     NPRoject - projectile nejc number
C
C     INITIALIZATION

C      Projectile mass number
      ap = AEJc(NPRoject)
      cme = MFPp/1.4D21
      fr = 0.D0
      totemis = 0.D0
      DO i = 0, NDEJC
         cross(i) = 0.
      ENDDO
      DO hh = 1, PMAX
         em(hh) = 0.D0
      ENDDO
      em(1) = 1.D0 - QDFrac
      em(2) = QDFrac
C-----Excitation energy (MeV)
      ec = EXCn
C-----Compound nucleus
      ac = A(1)
      zc = Z(1)
      gc = ROPar(1,1)/PI26*GTIlnor(1)
      pc = ROPar(3,1)
C
C-----By setting GTIlnor(1) to zero we are setting gc=A/13 !!
C
      IF (gc.EQ.0.D0) THEN
         gc = ac/13.
         IF (GTIlnor(1).GT.0.) gc = ac/13.*GTIlnor(1)
         ftmp = 0.
         IF (ac.GT.0.D0) ftmp = 12./SQRT(DBLE(FLOAT(ac)))
         pc = ftmp                                             ! odd
         IF (MOD(ac,2).EQ.0 .AND. MOD(zc,2).EQ.0) pc = 2*ftmp  ! e-e
         IF (MOD(ac,2).EQ.0 .AND. MOD(zc,2).EQ.1) pc = 0       ! o-o
      ENDIF
C-----NEJcm + 1 correspond to the compound gamma emitting nucleus
      g(0) = gc
      pair(0) = pc
C
C-----EXCITON EQUILIBRIUM NUMBER NHEq
C
      NHEq = MIN(PMAX - 1,NINT(SQRT(1.4*gc*ec))) + 1
C-----ZERO ARRAY INITIALIZATION
      DO nejc = 0, NDEJC
         iemin(nejc) = 2
         iemax(nejc) = NDEX
         nures(nejc) = 1
         DO ienerg = 1, NDEX
            spec(nejc,ienerg) = 0.D0
            DO hh = 1, PMAX
               we(nejc,hh,ienerg) = 0.D0
            ENDDO
         ENDDO
         DO hh = 1, PMAX
            L(nejc,hh) = 0.D0
         ENDDO
      ENDDO
      WRITE (6,99020)
      WRITE (6,99005)
99005 FORMAT (5X,' Preequilibrium decay (PCROSS)',/)
      WRITE (6,99010) MFPp
99010 FORMAT (/,1X,'Mean free path parameter = ',F4.2,/)
C
C-----NEJcm is the maximum number of particles emitted
C
      DO nejc = 1, NEJcm
         ar = ac - AEJc(nejc)
         zr = zc - ZEJc(nejc)
         izares = INT(1000*zr + ar)
         CALL WHERE(izares,nnur,iloc)
         IF (iloc.EQ.1) THEN
            WRITE (6,'('' NO LOCATION ASCRIBED TO NUCLEUS '')') izares
            STOP 'EXCITON'
         ENDIF
         nures(nejc) = nnur
         g(nejc) = ROPar(1,nnur)/PI26*GTIlnor(nnur)
         pair(nejc) = ROPar(3,nnur)
C
C--------By setting GTIlnor(nnur) to zero we are setting g(nejc)=A/13 !!
C
         IF (g(nejc).EQ.0.) THEN
            g(nejc) = ar/13.
            IF (GTIlnor(nnur).GT.0.) g(nejc) = ar/13.*GTIlnor(nnur)
            ftmp = 0.
            IF (ar.GT.0.D0) ftmp = 12./SQRT(DBLE(FLOAT(ar)))
            pair(nejc) = ftmp
            IF (MOD(ar,2).EQ.0 .AND. MOD(zr,2).EQ.0) pair(nejc) = 2*ftmp
            IF (MOD(ar,2).EQ.0 .AND. MOD(zr,2).EQ.1) pair(nejc) = 0
         ENDIF
C--------Maximum and minimum energy bin
         excnq = EXCn - Q(nejc,1)
C--------last continuum energy bin is calculated, RCN 11/2004
         nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)

         DO ienerg = 2, NEX(nnur)
            eee = DE*(ienerg - 1)
            IF (EMAx(nnur).GT.eee) iemax(nejc) = ienerg
C-----------Limiting iemax(nejc) to last continuum energy bin , RCN 11/2004
            IF (iemax(nejc).GE.nexrt) iemax(nejc) = nexrt

            IF (ETL(5,nejc,nnur).EQ.0) THEN
               sg = SIGabs(ienerg + 5,nejc,nnur)
            ELSE
               sg = SIGabs(ienerg,nejc,nnur)
            ENDIF
            IF (sg.LT.1.D-6) iemin(nejc) = ienerg
         ENDDO
      ENDDO
      izares = INT(1000*zc + ac)
      CALL WHERE(izares,nnur,iloc)
      nures(0) = nnur
C-----Maximum and minimum energy bin for gamma emission
C-----Last continuum energy bin is calculated, RCN 11/2004
      nexrt = MAX(INT((EXCn-ECUt(nnur))/DE + 1.0001),1)
      DO ienerg = 2, NDEX
         eee = DE*(ienerg - 1)
         IF (ec.GT.eee) iemax(0) = ienerg
         IF (iemax(0).GE.nexrt) iemax(0) = nexrt
      ENDDO
      IF (.NOT.callpcross) CALL RQFACT(NHEq,r)
      callpcross = .TRUE.  ! To avoid r factor recalculation at each call
C
C-----EMISSION RATES CALCULATIONS FOLLOWS
C
C-----PRIMARY PARTICLE LOOP
C
      DO nejc = 0, NEJcm
         nnur = nures(nejc)
         IF (nejc.NE.0) THEN
            ao = AEJc(nejc)
            zo = ZEJc(nejc)
            ar = ac - ao
            zr = zc - zo
            IF (ao.EQ.1 .AND. zo.EQ.0) ff1 = 2.0173*ar/(ar + 1.0087)
                                                                   ! n
            IF (ao.EQ.1 .AND. zo.EQ.1) ff1 = 2.0145*ar/(ar + 1.0073)
                                                                   ! p
            IF (ao.EQ.4 .AND. zo.EQ.2) ff1 = 4.0015*ar/(ar + 4.0015)
                                                                   ! alpha
            IF (ao.EQ.3 .AND. zo.EQ.1) ff1 = 6.0298*ar/(ar + 3.0149)
                                                                   ! t
            IF (ao.EQ.2 .AND. zo.EQ.1) ff1 = 6.0408*ar/(ar + 2.0136)
                                                                   ! d
         ELSE
            ar = ac
            zr = zc
            ao = 0
            zo = 0
            ff1 = 1.D0/AMUmev
         ENDIF
C
C--------PARTICLE-HOLE LOOP
C
         DO h1 = 1, NHEq
            icon = 0
            hh = h1 - 1
            IF (hh.EQ.0 .AND. nejc.NE.0) GOTO 50
            p = hh + ap
            ff2 = DENSW(gc,pc,p,hh,ec)
            IF (ff2.EQ.0.) GOTO 50
C
C-----------PRIMARY ENERGY CYCLE
C
            DO ienerg = iemin(nejc), iemax(nejc)
               eee = DE*(ienerg - 1)
C--------------Inverse cross section
               IF (nejc.EQ.0) THEN
                  aat = ac
                  azt = zc
                  sg = SGAM(aat,azt,eee)
               ELSEIF (ETL(5,nejc,nnur).EQ.0) THEN
                  sg = SIGabs(ienerg + 5,nejc,nnur)
               ELSE
                  sg = SIGabs(ienerg,nejc,nnur)
               ENDIF
               IF (sg.EQ.0.) GOTO 20
               er = EMAx(nnur) - eee
               IF (er.LE.EPS) GOTO 20
               hlp1 = 0.D0
C--------------PREEQ n & p EMISSION
               IF (nejc.gt.0 .and. nejc.LE.2) THEN
                  wda = DENSW(g(nejc),pair(nejc),p - ao,hh,er)
                  IF (wda.GT.0.) hlp1 = wda*r(1,h1,nejc)
C--------------PREEQ CLUSTER EMISSION
               ELSEIF (nejc.GT.2 .AND. nejc.LE.NEJcm) THEN
                  CALL PREFORMATION(flm,eee)
                  DO j = 1, ao
                     wda = DENSW(g(nejc),pair(nejc),p - j,hh,er)
                     IF (wda.GT.0.) hlp1 = hlp1 + flm(ao,j)
     &                   *wda*r(j,h1,nejc)
                  ENDDO
C--------------PREEQ GAMMA EMISSION
               ELSEIF (nejc.EQ.0) THEN
                  hlp1 = DENSW(gc,pc,p,hh,er)*DBLE(p + hh)
     &                   /(DBLE(p + hh) + eee*gc)
                  IF (hh.GE.1) hlp1 = hlp1 +
     &                                gc*eee*DENSW(gc,pc,p - 1,hh - 1,
     &                                er)/(DBLE(p + hh - 2) + eee*gc)
               ENDIF
               IF (hlp1.EQ.0.) GOTO 20
               ff3 = hlp1*eee*sg
               IF (nejc.EQ.0) ff3 = ff3*eee
               ff = ff1*ff3/ff2
               we(nejc,h1,ienerg) = ff*C1
               icon = icon + 1
               icon3 = ienerg
               eint(icon) = eee
               fint(icon) = ff
   20       ENDDO
C-----------END OF EMISSION ENERGY LOOP
C
C-----------INTEGRATION PROCEDURE #1 (EMISSION RATES)
C
            IF (icon.GT.0) THEN
               IF (nejc.NE.0) THEN
                  emaxi = EMAx(nnur)
                  IF (DE*icon3.LT.emaxi) emaxi = DE*icon3
                  emini = 0.D0
                  IF (iemin(nejc).GT.2) emini = DE*(iemin(nejc) - 2)
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
   50    ENDDO
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
C
C-----PARTICLE LOOP FOR EMISSION SPECTRA CALCULATIONS
C
      totemis = 0.D0
      DO nejc = 0, NEJcm
         hlp1 = 0.D0
         DO ienerg = iemin(nejc), iemax(nejc)
            emis = 0.D0
            DO h1 = 1, ihmax
               wb = we(nejc,h1,ienerg)
               IF (wb.GT.0.) emis = emis + wb*em(h1)
            ENDDO
            spec(nejc,ienerg) = Sigr*emis
            hlp1 = hlp1 + spec(nejc,ienerg)*DE
         ENDDO
         totemis = totemis + hlp1
         cross(nejc) = hlp1
      ENDDO

      IF (IOUt.GE.1) THEN
         DO nejc = 0, NEJcm
            IF (nejc.gt.0 .and. nejc.LE.3) THEN !nucleons and alpha
               IF (IDNa(2*nejc,6).EQ.0) THEN
                  status = "  (ignored) "
               ELSE
                  status = "  (accepted)"
               ENDIF
               WRITE (6,
     &'(1X,A2,'' PCROSS emission cross section'',G12.5,
     &'' mb'',A12)') SYMbe(nejc), cross(nejc), status
            ELSEIF (nejc.EQ.4 .AND. NEJcm.NE.3) THEN !light ion (always accepted)
               status = "  (accepted)"
               WRITE (6,
     &'(1X,A2,'' PCROSS emission cross section'',G12.5,
     &'' mb'',A12)') SYMbe(nejc), cross(nejc), status
            ELSE !gamma
               IF (IDNa(5,6).EQ.0) THEN
                  status = "  (ignored) "
               ELSE
                  status = "  (accepted)"
               ENDIF
               WRITE (6,
     &'(1X,A2,'' PCROSS emission cross section'',G12.5,
     &'' mb'',A12)') 'g ', cross(nejc), status
            ENDIF
C---------We don't need this spectrum printout any more but I leave it
C---------commented for checking in case anything goes wrong.
C         DO ienerg = iemin(nejc), iemax(nejc)
C                  WRITE(6,'(1x, F7.2, 2x, 7(D12.6, 1x))')
C    &             DE*(ienerg - 1), spec(nejc, ienerg)
C         ENDDO
C         WRITE(6, *)'==========================='
         ENDDO
      ENDIF

      fr = totemis/Sigr
      WRITE (6,99015) totemis, fr
99015 FORMAT (/1X,'PCROSS preequilibrium total cross section   =',F8.2,
     &   ' mb'/1X,'PCROSS preequilibrium fraction              =',F8.2)
C
C-----Transfer PCROSS results into EMPIRE. Call to ACCUMSD is needed later
C     Note, that PCROSS only calculates emission into the continuum
      totemis = 0.D0
      DO nejc = 0, NEJcm  ! over ejectiles
         nnur = nures(nejc)
         IF (nejc.gt.0) THEN !particle
            IF (nejc.EQ.1 .AND. IDNa(2,6).EQ.0) cycle
            IF (nejc.EQ.2 .AND. IDNa(4,6).EQ.0) cycle
            IF (nejc.EQ.3 .AND. IDNa(6,6).EQ.0) cycle
C-----------number of spectrum bins to continuum WARNING! might be negative!
            excnq = EXCn - Q(nejc,1)
         ELSE !gamma
            IF (IDNa(5,6).EQ.0) cycle
            nnur = 1
            excnq = EXCn
         ENDIF
         CSMsd(nejc) = CSMsd(nejc) + cross(nejc)
         totemis = totemis + cross(nejc)
C--------number of spectrum bins to continuum WARNING! might be negative!
         nexrt = INT((excnq - ECUt(nnur))/DE + 1.0001)
         DO ie = iemin(nejc), nexrt
            ftmp = spec(nejc,ie)
            CSEmsd(ie,nejc) = CSEmsd(ie,nejc) + ftmp
            DO iang = 1, NDANG
C             Using fr = totemis/Sigr it should be quite easy to implement
C             Kalbach systematic for PE DDX calculations here
C             (for the time being isotropic distribution is assumed)
              CSEa(ie,iang,nejc,1) = CSEa(ie,iang,nejc,1)
     &                             + ftmp/(4.*PI)
             ENDDO
         ENDDO
       ENDDO

      WRITE (6,99020)
99020 FORMAT (/' ',57('-')/)
      END


      SUBROUTINE RQFACT(Heq,R)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C PARAMETER definitions
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 FA(2*PMAX + 3), LFA(2*PMAX + 3)
      COMMON /PFACT / FA, LFA
C
C Dummy arguments
C
      INTEGER*4 Heq
      REAL*8 R(4,PMAX,NDEJC)
C
C Local variables
C
      INTEGER*4 ab, ac, ap, at, h1, hhh, i, j, j1, l, m, maxj, minj, nb,
     &          np, nt, p, zb, zc, zp, zt
      REAL ALOG, FLOAT
      REAL*8 f1, f2, f21, f22, f23, f24, s1, s2, ta, tn, tz, uuu, zzz
      INTEGER nejc
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
      DO i = 4, 2*PMAX + 3
         LFA(i) = ALOG(FLOAT(i - 3)) + LFA(i - 1)
         FA(i) = (i - 3)*FA(i - 1)
      ENDDO
      DO nejc = 1, NEJcm
         DO h1 = 1, PMAX
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
               IF (p.GE.l) THEN
                  m = ab - l
                  minj = MAX(0,l - nb)
                  maxj = MIN(l,zb)
                  s1 = 0.D0
                  DO i = 0, hhh
                     f1 = zzz**i*uuu**(hhh - i)
     &                    *EXP(LFA(hhh + 3) - LFA(hhh - i + 3)
     &                    - LFA(i + 3))
                     s2 = 0.D0
                     DO j = minj, maxj
                        f21 = 1.D0
                        IF (j.GE.1) THEN
                           DO j1 = 0, j - 1
                              f21 = f21*FLOAT(zp + i - j1)
                           ENDDO
                           f21 = f21/FA(j + 3)
                        ENDIF
                        f22 = 1.D0
                        IF (l - j.GE.1) THEN
                           DO j1 = 0, l - j - 1
                              f22 = f22*FLOAT(np + hhh - i - j1)
                           ENDDO
                           f22 = f22/FA(l - j + 3)
                        ENDIF
                        f23 = 1.D0
                        IF (zb - j.GE.1) THEN
                           DO j1 = 0, zb - j - 1
                              f23 = f23*FLOAT(zt - i - j1)
                           ENDDO
                           f23 = f23/FA(zb - j + 3)
                        ENDIF
                        f24 = 1.D0
                        IF (nb - l + j.GE.1) THEN
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
     &                 - LFA(p + 3) + LFA(ab + 3) - LFA(zb + 3)
     &                 - LFA(nb + 3))/f2
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
      END



      SUBROUTINE TRANSIT(E,Gc,Ap,Nheq)
      IMPLICIT LOGICAL*1(A - Z)
C
C PARAMETER definitions
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 LM(PMAX), LP(PMAX)
      INTEGER*4 NMAx
      COMMON /CALC3 / LP, LM, NMAx
C
C Dummy arguments
C
      INTEGER*4 Ap, Nheq
      REAL*8 E, Gc
C
C Local variables
C
      REAL*8 a0p, uc0
      REAL FLOAT
      INTEGER*4 h, h1, i, n, p
C
C-----TRANSITION RATES CALCULATIONS FOLLOWS
C
      DO i = 1, PMAX
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
         IF (uc0.LE.0.D0) THEN
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
      END


      SUBROUTINE RESOL(Em,Ih2,Ap,Cme)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C PARAMETER definitions
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 L(0:NDEJC,PMAX), LM(PMAX), LP(PMAX)
      INTEGER*4 NHEq, NMAx
      COMMON /CALC3 / LP, LM, NMAx
      COMMON /CALC5 / L, NHEq
C
C Dummy arguments
C
      INTEGER*4 Ap, Ih2
      REAL*8 Cme
      REAL*8 Em(PMAX)
C
C Local variables
C
      REAL*8 b(PMAX), c(PMAX), e(PMAX), hlp1, hlp2, hlp4, ln(PMAX),
     &       ls(PMAX)
      INTEGER*4 h1, hhh, i, ij, n
      DO i = 1, PMAX
         ln(i) = 0.
      ENDDO
      n = NMAx
      IF (n.GT.PMAX) n = PMAX
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
      IF (IOUt.GE.3 .AND. NEJcm.EQ.3) WRITE (6,99005)
99005 FORMAT (/2X,'N',5X,'T r a n s i t i o n   r a t e s   ',
     &        '        E  m  i  s  s  i  o  n     r  a  t  e  s',//,5X,
     &'   plus     minus     TOTAL              TOTAL     gammas   neutr
     &ons   protons   alphas'/)
      IF (IOUt.GE.3 .AND. NEJcm.EQ.4) WRITE (6,99010)
99010 FORMAT (/2X,'N',5X,'T r a n s i t i o n   r a t e s   ',
     &        '        E  m  i  s  s  i  o  n     r  a  t  e  s',//,5X,
     &'   plus     minus     TOTAL              TOTAL     gammas   neutr
     &ons   protons   alphas   light ions'/)
      DO h1 = 1, n
         hhh = h1 - 1
         ij = 2*hhh + Ap
         hlp1 = LP(h1)/Cme
         hlp2 = LM(h1)/Cme
         hlp4 = hlp1 + hlp2
         IF (IOUt.GE.3) WRITE (6,99015) ij, hlp1, hlp2, hlp4, ln(h1),
     &                                            (L(i,h1),i = 0,NEJcm)
99015    FORMAT (I3,3E10.3,10X,6E10.3)
      ENDDO
C
C-----INITIAL CONDITIONS
C
      DO i = 1, PMAX
         b(i) = Em(i)
         Em(i) = 0.
      ENDDO
      c(1) = 0.D0
      DO h1 = 1, n - 1
         c(h1 + 1) = -LP(h1)
         e(h1) = -LM(h1 + 1)
      ENDDO
      CALL MASTER(c,ls,e,b,n,PMAX)
C--------------------------------------------------------------------
C-----NEVER COME BACK ASSUMPTION(ONLY PE-CONTRIBUTION)
      DO hhh = NHEq, n
         b(hhh) = 0.
      ENDDO
C--------------------------------------------------------------------
      DO h1 = 1, n
         Em(h1) = b(h1)*Cme
         IF (Em(h1).NE.0.) Ih2 = h1
      ENDDO
      IF (IOUt.GE.3) WRITE (6,99020)
99020 FORMAT (/3X,'TIME INTEGRALS OF TAU(N)'/3X,
     &        ' N   UP TO Nequil       ')
      Ih2 = MIN(Ih2,NHEq)
      IF (IOUt.GE.3) THEN
         DO h1 = 1, Ih2
            hhh = 2*(h1 - 1) + Ap
            WRITE (6,99025) hhh, Em(h1)
99025       FORMAT (I5,2E14.3)
         ENDDO
         WRITE (6,*)
      ENDIF
      END


      SUBROUTINE MASTER(Subd,Diag,Superd,B,N,Ndim)
      IMPLICIT LOGICAL(A - Z)
C
C Dummy arguments
C
      INTEGER*4 N, Ndim
      REAL*8 B(Ndim), Diag(Ndim), Subd(Ndim), Superd(Ndim)
C
C Local variables
C
      DOUBLE PRECISION DABS
      INTEGER*4 k, kb, kp1, nm1, nm2
      REAL*8 t
      Subd(1) = Diag(1)
      nm1 = N - 1
      IF (nm1.GE.1) THEN
         Diag(1) = Superd(1)
         Superd(1) = 0.0D0
         Superd(N) = 0.0D0
         DO k = 1, nm1
            kp1 = k + 1
            IF (DABS(Subd(kp1)).GE.DABS(Subd(k))) THEN
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
            IF (Subd(k).EQ.0.0D0) STOP
     &            ' DIAGONAL ELEMENT = 0 in SOLUTION of MASTER equation'
            t = -Subd(kp1)/Subd(k)
            Subd(kp1) = Diag(kp1) + t*Diag(k)
            Diag(kp1) = Superd(kp1) + t*Superd(k)
            Superd(kp1) = 0.0D0
            B(kp1) = B(kp1) + t*B(k)
         ENDDO
      ENDIF
      IF (Subd(N).NE.0.0D0) THEN
C
C--------BACK ITERATION
C
         nm2 = N - 2
         B(N) = B(N)/Subd(N)
         IF (N.GT.1) THEN
            B(nm1) = (B(nm1) - Diag(nm1)*B(N))/Subd(nm1)
            IF (nm2.GE.1) THEN
               DO kb = 1, nm2
                  k = nm2 - kb + 1
                  B(k) = (B(k) - Diag(k)*B(k + 1) - Superd(k)*B(k + 2))
     &                   /Subd(k)
               ENDDO
            ENDIF
         ENDIF
      ENDIF
      END


      SUBROUTINE PREFORMATION(Flm,E)
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
C
C Dummy arguments
C
      REAL*8 E
      REAL*8 Flm(4,4)
C
C Local variables
C
      INTEGER l
      REAL*8 x(5)
C-----FOR FAST POLYNOMIAL EVALUATION
      x(1) = E
      DO l = 2, 5
         x(l) = E*x(l - 1)
      ENDDO
C-----DEUTERON (MASS = 2)
      IF (E.LT.80.D0) THEN
         Flm(2,2) = MAX(0.00821 + 0.02038*x(1) + 5.95941E-4*x(2)
     &              - 2.24726E-5*x(3) + 2.38917E-7*x(4)
     &              - 8.34053E-10*x(5),0.)
      ELSE
         Flm(2,2) = 1.
      ENDIF
      Flm(2,1) = MAX(1. - Flm(2,2),0.)
C-----TRITIUM or HELIUM-3 (MASS = 3)
      IF (E.LT.70.D0) THEN
         Flm(3,1) = MAX(0.57315 - 0.02083*x(1) + 3.19204E-4*x(2)
     &              - 2.85876E-6*x(3) + 1.26332E-8*x(4),0.D0)
      ELSE
         Flm(3,1) = 0.D0
      ENDIF
      IF (E.LT.170.) THEN
         Flm(3,3) = MAX(0.00705 - 0.00164*x(1) + 2.16549E-4*x(2)
     &              - 1.73867E-6*x(3) + 5.24069E-9*x(4)
     &              - 5.79848E-12*x(5),0.D0)
      ELSE
         Flm(3,3) = 1.D0
      ENDIF
      Flm(3,2) = MAX(1.D0 - Flm(3,1) - Flm(3,3),0.D0)
C-----ALPHA PARTICLES (M=4)
      IF (E.LT.70.D0) THEN
         Flm(4,1) = MAX(0.29119 - 0.01434*x(1) + 3.34045E-4*x(2)
     &              - 4.10957E-6*x(3) + 2.02375E-8*x(4),0.D0)
      ELSE
         Flm(4,1) = 0.D0
      ENDIF
      IF (E.LT.130.D0) THEN
         Flm(4,2) = 0.60522 + 1.30071E-4*x(1) - 1.77653E-4*x(2)
     &              + 1.64048E-6*x(3) - 4.55562E-9*x(4)
     &              + 2.09521E-12*x(5)
      ELSE
         Flm(4,2) = 0.D0
      ENDIF
      IF (E.LT.30.D0) THEN
         Flm(4,4) = 0.D0
      ELSEIF (E.LT.300.D0) THEN
         Flm(4,4) = 0.01917 - 0.00307*x(1) + 9.57966E-5*x(2)
     &              - 4.07085E-7*x(3) + 5.28037E-10*x(4)
     &              + 7.92029E-16*x(5)
      ELSE
         Flm(4,4) = 1.D0
      ENDIF
      IF (E.LT.300.D0) THEN
         Flm(4,3) = MAX(1.D0 - Flm(4,1) - Flm(4,2) - Flm(4,4),0.D0)
      ELSE
         Flm(4,3) = 0.D0
      ENDIF
      END


      FUNCTION DENSW(G,D,P,H,E)
C
C  WILLIAMS FORMULATION, PAULI CORRECTION BY KALBACH
C
C  G - LEVEL DENSITY PARAMETER
C  D - PAIRING CORRECTION
C  P(H) - PARTICLE(HOLE) NUMBER
C  E - ENERGIA DE EXCITACION
C
C PARAMETER definitions
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 FA(2*PMAX + 3), LFA(2*PMAX + 3)
      COMMON /PFACT / FA, LFA
C
C Dummy arguments
C
      REAL*8 D, E, G
      INTEGER*4 H, P
      REAL*8 DENSW
C
C Local variables
C
      REAL*8 a, fac, u
      DOUBLE PRECISION DEXP, DLOG
      INTEGER*4 n
      DENSW = 0.D0
      n = P + H
      IF (n.LE.0) RETURN
      fac = LFA(P + 3) + LFA(H + 3) + LFA(n + 2)
      a = .25D0*(P*(P - 1) + H*(H - 1))
      u = G*(E - D) - a
      IF (u.LE.0.) RETURN
      DENSW = G*(DEXP((n-1)*DLOG(u) - fac))
      END

      FUNCTION SGAM(A,Z,Eg)
C
C     Photoabsorption cross-section in "mb"
C     If "Key_shape = 0" old expression for photoabsorption
C     cross-section  is used, namely:
C     [1] S.A.FAYANS, "Lectures,DUBNA,feb.1982"
C     SGAM = GAMMA ABSORPTION CROSS SECTION in mb
C
C COMMON variables
C
      INTEGER KEY_gdrgfl, KEY_shape
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
C
C Dummy arguments
C
      REAL*8 A, Eg, Z
      REAL*8 SGAM
C
C Local variables
C
      REAL*8 a3, egr, gam, sgm
      REAL*8 GAMMA_STRENGTH
      IF (KEY_shape.NE.0) THEN
         SGAM = (10.D0/8.674D-7)*Eg*GAMMA_STRENGTH(Z,A,0.D0,0.D0,Eg,
     &          KEY_shape)
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
      END