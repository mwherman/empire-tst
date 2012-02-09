Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
      SUBROUTINE PLOT_ZVV_GSLD(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      INTEGER :: NLWst
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      CHARACTER(5) :: caz
      CHARACTER(17) :: ctmp1
      CHARACTER(7) :: fname
      INTEGER :: INT
      INTEGER :: j, kk
      REAL :: rolowint1, rolowint2
      CHARACTER(50) :: title
      REAL*8 :: u
C
C*** End of declarations rewritten by SPAG
C
                                                                       ! PARAM
 
 
      IF(NLV(Nnuc).LE.3)RETURN                                         ! PARAM
 
      IF(SYMb(Nnuc)(2:2).EQ.' ')THEN
        WRITE(caz,'(A1,A1,I3.3)')SYMb(Nnuc)(1:1), '_', INT(A(Nnuc))
      ELSE
        WRITE(caz,'(A2,I3.3)')SYMb(Nnuc), INT(A(Nnuc))
      ENDIF
 
      IF(ADIv.EQ.0)WRITE(fname,'(A7)')'LD_EGSM'
      IF(ADIv.EQ.1)WRITE(fname,'(A7)')'LD__GSM'
      IF(ADIv.EQ.2)WRITE(fname,'(A7)')'LD__GCM'
      IF(ADIv.EQ.3)WRITE(fname,'(A7)')'LD_HFBM'
      IF(ADIv.EQ.4)WRITE(fname,'(A7)')'LD_OGCM'
      WRITE(ctmp1,'(A17)')fname//'_'//caz//'.zvd'
 
C     write(title,'('' Nucleus : '',
C    &     i3,''-'',A2,''-'',I3)')
C    &     int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
      title = '  '
 
      OPEN(36,FILE = ctmp1,STATUS = 'unknown')
      CALL OPEN_ZVV(36,'RHO(U)   at GS of '//caz,' ')
 
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)
 
        IF(u.LT.ELV(NLV(Nnuc),Nnuc))CYCLE
 
        rolowint1 = 0.D0
        rolowint2 = 0.D0
        DO j = 1, NLWst
          rolowint1 = rolowint1 + RO(kk,j,1,Nnuc)
          rolowint2 = rolowint2 + RO(kk,j,2,Nnuc)
        ENDDO
        IF(rolowint1 + rolowint2.GT.1E30)EXIT
C
C       Avoiding printing the first point
C       as LDs are defined above the discrete levels
C
        IF(rolowint1 + rolowint2.GT.0.D0)
     &     WRITE(36,'(G10.3,2X,1P,(90E12.5))')1E6*u, 
     &     rolowint1 + rolowint2
      ENDDO
 
      IF(ADIv.NE.3)THEN
        IF(ADIv.EQ.0)CALL CLOSE_ZVV_LEVDEN(36,
     &     'GS level density - EGSM (RIPL-3) = EMPIRE specific',title)
 
        IF(ADIv.EQ.1)
     &     CALL CLOSE_ZVV_LEVDEN(36,'GS level density - GSM (RIPL-2)',
     &     title)
 
        IF(ADIv.EQ.2)CALL CLOSE_ZVV_LEVDEN(36,
     &     'GS level density - Gilbert-Cameron ',title)
 
        IF(ADIv.EQ.4)CALL CLOSE_ZVV_LEVDEN(36,
     &     'GS level density - Gilbert-Cameron (as in EMPIRE 2.19)',
     &     title)
 
        CLOSE(36)
 
        RETURN
 
      ELSE
 
        CALL CLOSE_ZVV(36,' ',' ')
 
      ENDIF
 
      CALL OPEN_ZVV(36,'RHO(U,+) at GS ',' ')
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)
        rolowint1 = 0.D0
        DO j = 1, NLWst
          rolowint1 = rolowint1 + RO(kk,j,1,Nnuc)
        ENDDO
        IF(rolowint1.GT.1E30)EXIT
        IF(rolowint1.GT.0.D0)WRITE(36,'(G10.3,2X,1P,(90E12.5))')1E6*u, 
     &                             rolowint1
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')
 
      CALL OPEN_ZVV(36,'RHO(U,-) at GS ',' ')
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)
        rolowint2 = 0.D0
        DO j = 1, NLWst
          rolowint2 = rolowint2 + RO(kk,j,2,Nnuc)
        ENDDO
        IF(rolowint2.GT.1E30)EXIT
        IF(rolowint2.GT.0.D0)WRITE(36,'(G10.3,2X,1P,(90E12.5))')1E6*u, 
     &                             rolowint2
      ENDDO
 
      CALL CLOSE_ZVV_LEVDEN(36,
     &                      'GS level density - HFB tabulated (RIPL-3)',
     &                      title)
 
      CLOSE(36)
      RETURN
      END SUBROUTINE PLOT_ZVV_GSLD
 
!---------------------------------------------------------------------------
C===============================================================
      SUBROUTINE PLOT_ZVV_SADLD(Nnuc,Ib)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Ib, Nnuc
C
C Local variables
C
      CHARACTER(7) :: caz
      CHARACTER(20) :: ctmp
      CHARACTER(5) :: ctmp1
      CHARACTER(10) :: fname
      INTEGER :: INT
      INTEGER :: j, kk
      REAL*8 :: rocumul1, rocumul2, u
      CHARACTER(50) :: title
C
C*** End of declarations rewritten by SPAG
C
 
 
      IF(SYMb(Nnuc)(2:2).EQ.' ')THEN
        WRITE(ctmp1,'(A1,A1,I3.3)')SYMb(Nnuc)(1:1), '_', INT(A(Nnuc))
      ELSE
        WRITE(ctmp1,'(A2,I3.3)')SYMb(Nnuc), INT(A(Nnuc))
      ENDIF
 
      IF(FISden(Nnuc).LE.1)THEN
        WRITE(fname,'(A9,I1)')'LD_EGSM_S', Ib
        WRITE(ctmp,'(A20)')fname//'_'//ctmp1//'.zvd'
        WRITE(caz,'(A6,I1)')'EGSM-S', Ib
      ENDIF
 
      IF(FISden(Nnuc).EQ.3)THEN
        WRITE(fname,'(A9,I1)')'LD_HFBM_S', Ib
        WRITE(ctmp,'(A20)')fname//'_'//ctmp1//'.zvd'
        WRITE(caz,'(A6,I1)')'HFBM-S', Ib
      ENDIF
 
C     write(title,'('' Fissioning nucleus: '', i3,''-'',A2,''-'',I3)')
C    &     int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
      title = '    '
 
      OPEN(36,FILE = ctmp,STATUS = 'unknown')
      CALL OPEN_ZVV(36,'RHO(U)   at saddle of '//ctmp1,' ')
      DO kk = 1, NRBinfis(Ib)
        u = UGRid(kk,Ib)
        rocumul1 = 0.D0
        rocumul2 = 0.D0
        DO j = 1, NLW
          rocumul1 = rocumul1 + ROFisp(kk,j,1,Ib)
          rocumul2 = rocumul2 + ROFisp(kk,j,2,Ib)
        ENDDO
        IF(rocumul1 + rocumul2.GT.1E30)EXIT
        WRITE(36,'(G10.3,2X,1P,(90E12.5))')1E6*u, 
     &        MAX(0.1D0,rocumul2 + rocumul1)
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')
 
 
      IF(FISden(Nnuc).NE.3)THEN
 
        IF(FISden(Nnuc).EQ.0)CALL CLOSE_ZVV_LEVDEN(36,
     &   'Saddle-point  level density - EGSM (RIPL-3) = EMPIRE specific'
     &   ,title)
 
        IF(FISden(Nnuc).EQ.1)CALL CLOSE_ZVV_LEVDEN(36,
     &     'Saddle-point  level density - GSM (RIPL-2)',title)
 
        IF(FISden(Nnuc).EQ.2)CALL CLOSE_ZVV_LEVDEN(36,
     &     'Saddle-point  level density - Gilbert-Cameron ',title)
 
        IF(FISden(Nnuc).EQ.4)CALL CLOSE_ZVV_LEVDEN(36,
     &'Saddle-point level density - Gilbert-Cameron (as in EMPIRE 2.19)'
     &,title)
        CLOSE(36)
 
        RETURN
 
      ELSE
 
        CALL CLOSE_ZVV(36,' ',' ')
 
      ENDIF
 
 
      IF(FISden(Nnuc).LT.3)RETURN
 
      CALL OPEN_ZVV(36,'RHO(U,+) at saddle point',' ')
      DO kk = 1, NRBinfis(Ib)
        u = UGRid(kk,Ib)
        rocumul1 = 0.D0
        DO j = 1, NLW
          rocumul1 = rocumul1 + ROFisp(kk,j,1,Ib)
        ENDDO
        IF(rocumul1.GT.1E30)EXIT
        WRITE(36,'(G10.3,2X,1P,(90E12.5))')1E6*u, MAX(rocumul1,0.1D0)
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')
 
      CALL OPEN_ZVV(36,'RHO(U,-) at saddle point',' ')
      DO kk = 1, NRBinfis(Ib)
        u = UGRid(kk,Ib)
        rocumul2 = 0.D0
        DO j = 1, NLW
          rocumul2 = rocumul2 + ROFisp(kk,j,2,Ib)
        ENDDO
        IF(rocumul2.GT.1E30)EXIT
        WRITE(36,'(G10.3,2X,1P,(90E12.5))')1E6*u, MAX(0.1D0,rocumul2)
      ENDDO
      CALL CLOSE_ZVV_LEVDEN(36,
     &             'Saddle-point level density - HFB tabulated (RIPL-3)'
     &             ,title)
      CLOSE(36)
      RETURN
      END SUBROUTINE PLOT_ZVV_SADLD
 
!---------------------------------------------------------------------------
 
C==============================================================
      SUBROUTINE PLOT_ZVV_NUMCUMUL(Nnuc,Defit,Nplot,Nlwst)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C PARAMETER definitions
C
      INTEGER, PARAMETER :: NLDgrid = 60
C
C COMMON variables
C
      REAL*8 :: AM, EO, T, UX
      REAL*8, DIMENSION(0:NLDgrid,2) :: CGRid
      INTEGER :: IUGrid
      REAL*8, DIMENSION(0:NLDgrid) :: UUGrid
      COMMON /CT    / AM, UX, EO, T
      COMMON /UCGRID/ UUGrid, CGRid, IUGrid
C
C Dummy arguments
C
      REAL*8 :: Defit
      INTEGER :: Nlwst, Nnuc, Nplot
C
C Local variables
C
      CHARACTER(5) :: caz
      CHARACTER(24) :: ctmp1
      REAL :: FLOAT
      CHARACTER(7) :: fname
      INTEGER :: ij, kk
      INTEGER :: INT
      REAL :: rocumul
      CHARACTER(50) :: title
C
C*** End of declarations rewritten by SPAG
C
 
 
 
 
 
      IF(NLV(Nnuc).LE.3)RETURN
                              !,JMAX = 5
 
      IF(SYMb(Nnuc)(2:2).EQ.' ')THEN
        WRITE(caz,'(A1,A1,I3.3)')SYMb(Nnuc)(1:1), '_', INT(A(Nnuc))
      ELSE
        WRITE(caz,'(A2,I3.3)')SYMb(Nnuc), INT(A(Nnuc))
      ENDIF
 
      IF(ADIv.EQ.0)WRITE(fname,'(A7)')'NL_EGSM'
      IF(ADIv.EQ.1)WRITE(fname,'(A7)')'NL__GSM'
      IF(ADIv.EQ.2)WRITE(fname,'(A7)')'NL__GCM'
      IF(ADIv.EQ.3)WRITE(fname,'(A7)')'NL_HFBM'
      IF(ADIv.EQ.4)WRITE(fname,'(A7)')'NL_OGCM'
      WRITE(ctmp1,'(A24)')fname//'_'//caz//'.zvd'
 
      WRITE(title,'('' Nucleus: '',i3,''-'',A2,''-'',I3)')INT(Z(Nnuc)), 
     &      SYMb(Nnuc), INT(A(Nnuc))
 
 
      OPEN(36,FILE = ctmp1,STATUS = 'unknown')
      CALL OPEN_ZVV(36,'Exp Cumul Levels',' ')
      WRITE(36,*)'0.0 1.0'
      DO kk = 2, NLV(Nnuc)
        WRITE(36,*)ELV(kk,Nnuc)*1D6, FLOAT(kk - 1)
        WRITE(36,*)ELV(kk,Nnuc)*1D6, FLOAT(kk)
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')
 
      CALL OPEN_ZVV(36,'Int of RHO(E)',' ')
C
      rocumul = 1.D0
      WRITE(36,*)'0.0 1.0'
 
C-----EGSM,GSM,GCM,OGCM
      IF(ADIv.NE.3)THEN
        DO kk = 2, Nplot
 
          IF(Defit*(kk - 1).GT.ELV(NLV(Nnuc),Nnuc) + 2.D0)EXIT
 
          IF(ADIv.EQ.2)THEN
            rocumul = 1.D0 + EXP(( - EO/T))*(EXP(Defit*(kk-1)/T) - 1.)
          ELSE
            DO ij = 1, Nlwst
              rocumul = rocumul + 0.5D0*Defit*(RO(kk - 1,ij,1,Nnuc)
     &                  + RO(kk,ij,1,Nnuc) + RO(kk - 1,ij,2,Nnuc)
     &                  + RO(kk,ij,2,Nnuc))
            ENDDO
          ENDIF
          WRITE(36,*)Defit*(kk - 1)*1.D6, rocumul
        ENDDO
      ENDIF
 
C-----HFB
      IF(ADIv.EQ.3)THEN
        DO kk = 1, IUGrid
          IF(UUGrid(kk).GT.ELV(NLV(Nnuc),Nnuc) + 2.D0)EXIT
          WRITE(36,*)UUGrid(kk)*1D6, CGRid(kk,1) + CGRid(kk,2)
        ENDDO
      ENDIF
      CALL CLOSE_ZVV_CUMUL(36,'Cumulative number of levels',title)
      CLOSE(36)
      RETURN
      END SUBROUTINE PLOT_ZVV_NUMCUMUL
 
!---------------------------------------------------------------------------
 
C======================================================
      SUBROUTINE OPEN_ZVV(Iout,Tfunct,Title)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iout
      CHARACTER(*) :: Tfunct, Title
C
C*** End of declarations rewritten by SPAG
C
      WRITE(Iout,'(A19)')'#begin LSTTAB.CUR/u'
      IF(Title(1:1).NE.' ')WRITE(Iout,*)TRIM(Title)
      WRITE(Iout,'(A12,A)')'fun: ', Tfunct
      WRITE(Iout,'(A10)')'thick: 2   '
      WRITE(Iout,'(A10/2H//)')'length: 92 '
      RETURN
      END SUBROUTINE OPEN_ZVV
 
!---------------------------------------------------------------------------
 
      SUBROUTINE CLOSE_ZVV(Iout,Titlex,Titley)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iout
      CHARACTER(*) :: Titlex, Titley
C
C*** End of declarations rewritten by SPAG
C
      WRITE(Iout,'(A2)')'//'
      WRITE(Iout,'(A17)')'#end LSTTAB.CUR/u'
      WRITE(Iout,'(A19)')'#begin LSTTAB.CUR/c'
      IF(Titlex(1:1).NE.' ')WRITE(Iout,*)'x: ', TRIM(Titlex)
      IF(Titley(1:1).NE.' ')WRITE(Iout,*)'y: ', TRIM(Titley)
      WRITE(Iout,'(A19)')'x-scale: auto      '
      WRITE(Iout,'(A17)')'y-scale: auto      '
      WRITE(Iout,'(A2)')'//'
      WRITE(Iout,'(A17)')'#end LSTTAB.CUR/c  '
      RETURN
      END SUBROUTINE CLOSE_ZVV
 
!---------------------------------------------------------------------------
 
      SUBROUTINE CLOSE_ZVV_CUMUL(Iout,Titlex,Titley)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iout
      CHARACTER(*) :: Titlex, Titley
C
C*** End of declarations rewritten by SPAG
C
      WRITE(Iout,'(A2)')'//'
      WRITE(Iout,'(A17)')'#end LSTTAB.CUR/u'
      WRITE(Iout,*)'#begin aa/c'
      WRITE(Iout,*)'tit:', TRIM(Titlex)
      WRITE(Iout,*)'tit2:', TRIM(Titley)
      WRITE(Iout,*)'x-unit:1000000, (MeV)'
      WRITE(Iout,*)'ix-unit: 1  '
      WRITE(Iout,*)'y-unit:1,   '
      WRITE(Iout,*)'iy-unit: 1'
      WRITE(Iout,*)'X-SCALE: lin'
      WRITE(Iout,*)'Y-SCALE: lin'
      WRITE(Iout,*)'X-RANGE: 0'
      WRITE(Iout,*)'Y-RANGE: 1'
      WRITE(Iout,*)'X-GRID: 1'
      WRITE(Iout,*)'NOSTAT: 1'
      WRITE(Iout,*)'legend: 1'
      WRITE(Iout,*)'X: E'
      WRITE(Iout,*)'x-long: Energy       '
      WRITE(Iout,*)'Y: Cumulative number of levels '
      WRITE(Iout,*)'x-scale: auto        '
      WRITE(Iout,*)'y-scale: auto        '
      WRITE(Iout,'(A2)')'//'
      WRITE(Iout,*)'#end aa/c'
      RETURN
      END SUBROUTINE CLOSE_ZVV_CUMUL
 
!---------------------------------------------------------------------------
 
      SUBROUTINE CLOSE_ZVV_LEVDEN(Iout,Titlex,Titley)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iout
      CHARACTER(*) :: Titlex, Titley
C
C*** End of declarations rewritten by SPAG
C
      WRITE(Iout,'(A2)')'//'
      WRITE(Iout,'(A17)')'#end LSTTAB.CUR/u'
      WRITE(Iout,'(A19)')'#begin aa/c'
      WRITE(Iout,*)'tit:', TRIM(Titlex)
      WRITE(Iout,*)'tit2:', TRIM(Titley)
      WRITE(Iout,*)'x-unit:1000000, (MeV)'
      WRITE(Iout,*)'ix-unit: 1'
      WRITE(Iout,*)'y-unit:1, (1/MeV)'
      WRITE(Iout,*)'iy-unit: 1'
      WRITE(Iout,*)'X-SCALE: lin'
      WRITE(Iout,*)'Y-SCALE: log'
C     write(iout,*) 'X-RANGE: 0'
C     write(iout,*) 'Y-RANGE: 1'
      WRITE(Iout,*)'X-GRID: 1'
      WRITE(Iout,*)'NOSTAT: 1'
      WRITE(Iout,*)'legend: 1'
      WRITE(Iout,*)'X: E'
      WRITE(Iout,*)'x-long: Energy       '
      WRITE(Iout,*)'Y: Level density     '
C     write(iout,*) 'x-scale: auto        '
C     write(iout,*) 'y-scale: auto        '
      WRITE(Iout,'(A2)')'//'
      WRITE(Iout,'(A17)')'#end aa/c  '
      RETURN
      END SUBROUTINE CLOSE_ZVV_LEVDEN
 
!---------------------------------------------------------------------------
 
C==============================================================
      SUBROUTINE PLOT_GNU_NUMCUMUL(Nnuc,Nplot,Defit,Dshift,Del,Nlwst)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: ACR, ACRt, AM, ATIl, DETcrt, ECOnd, EO, SCR, T, TCRt, 
     &          UCRt, UX
      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      COMMON /CT    / AM, UX, EO, T
C
C Dummy arguments
C
      REAL*8 :: Defit, Del, Dshift
      INTEGER :: Nlwst, Nnuc, Nplot
C
C Local variables
C
      INTEGER :: ij, il, kk
      INTEGER :: INT
      INTEGER*4 :: iwin
      INTEGER*4 :: PIPE
      REAL :: REAL
      REAL :: rocumul
C
C*** End of declarations rewritten by SPAG
C
 
 
 
 
 
 
 
 
      WRITE(8,1010)INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), ATIlnor(Nnuc)
     &             , ATIl, NLV(Nnuc)                         ! CRIT
 1010 FORMAT('Cumulative plot for ',I3,'-',A2,'-',I3,' norm=',F6.4,
     &       ' atil=',F4.1,' Ncut=',I3)
      OPEN(35,FILE = 'fort.35')
      WRITE(35,*)
     &'set terminal postscript enhanced color lw 2 solid "Helvetica" 20'
      WRITE(35,*)'set output "|cat >>CUMULPLOT.PS"'
      WRITE(35,1020)INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), Dshift, 
     &              UCRt - Del - Dshift, DEF(1,Nnuc), ATIl, NLV(Nnuc)
 1020 FORMAT('set title "',I3,'-',A2,'-',I3,'   Ushift = ',F6.3,
     &       ' Ucrt = ',F5.2,' Def = ',F6.2,' atil=',F4.1,' Ncut=',I3,
     &       '"')
      WRITE(35,*)'set logscale y'
      WRITE(35,*)'set xlabel "Excitation energy (MeV)" 0,0'
      WRITE(35,*)'set ylabel "Cumulative number of levels" 0,0'
      WRITE(35,*)'set style line 1 lt 1 lw 2'
      WRITE(35,*)'set style line 2 lt 5 lw 2'
      WRITE(35,
     &'(''plot "fort.36" w filledcu y2 ls 2 t "Discrete levels", "fort.3
     &4" w l ls 1 t "Level density" '')')
      CLOSE(35)
      OPEN(34,FILE = 'fort.34')
      OPEN(36,FILE = 'fort.36')
      WRITE(36,*)'0.0 1.0'
      DO il = 2, NLV(Nnuc)
        WRITE(36,*)ELV(il,Nnuc), REAL(il - 1)
        WRITE(36,*)ELV(il,Nnuc), REAL(il)
      ENDDO
      rocumul = 1.0
      WRITE(34,*)'0.0  ', rocumul
 
      DO kk = 2, Nplot
        DO ij = 1, Nlwst
          rocumul = rocumul + 0.5D0*Defit*(RO(kk - 1,ij,1,Nnuc)
     &              + RO(kk,ij,1,Nnuc) + RO(kk - 1,ij,2,Nnuc)
     &              + RO(kk,ij,2,Nnuc))
        ENDDO
        WRITE(34,*)Defit*(kk - 1), rocumul
      ENDDO
      CLOSE(36)
      CLOSE(34)
      IF(IOPsys.EQ.0)THEN
        iwin = PIPE('gnuplot fort.35')
        CLOSE(35)
      ENDIF
 
      RETURN
      END SUBROUTINE PLOT_GNU_NUMCUMUL
 
!---------------------------------------------------------------------------
 
C==============================================================
      SUBROUTINE PLOT_GNU_NUMCUMUL_GC(Nnuc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8 :: AM, EO, T, UX
      COMMON /CT    / AM, UX, EO, T
C
C Dummy arguments
C
      INTEGER :: Nnuc
C
C Local variables
C
      REAL :: FLOAT
      INTEGER :: il
      INTEGER :: INT
      INTEGER*4 :: iwin
      INTEGER*4 :: PIPE
      REAL :: rolowint
C
C*** End of declarations rewritten by SPAG
C
 
 
 
 
 
      WRITE(8,*)' A=', A(Nnuc), 'Z=', Z(Nnuc), ' Ncut=', NLV(Nnuc)
      WRITE(8,*)' a=', AM, ' Ux=', UX, ' T=', T, ' EO=', EO
      OPEN(35,FILE = 'fort.35')
      WRITE(35,*)
     &'set terminal postscript enhanced color lw 2 solid "Helvetica" 20'
      WRITE(35,*)'set output "|cat >>CUMULPLOT.PS"'
      WRITE(35,1010)INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), AM, T, EO, 
     &              NLV(Nnuc)
 1010 FORMAT('set title "',I3,'-',A2,'-',I3,': a=',F4.1,' T=',F4.1,
     &       ' E0=',F4.1,' Ncut=',I3,'"')
      WRITE(35,*)'set logscale y'
      WRITE(35,*)'set xlabel "Excitation energy (MeV)" 0,0'
      WRITE(35,*)'set ylabel "Cumulative number of levels" 0,0'
      WRITE(35,*)'set style line 1 lt 1 lw 2'
      WRITE(35,*)'set style line 2 lt 5 lw 2'
      WRITE(35,
     &'(''plot "fort.36" w filledcu y2 ls 2 t "Discrete levels", "fort.3
     &4" w l ls 1 t "Level density" '')')
      CLOSE(35)
      OPEN(34,FILE = 'fort.34')
      OPEN(36,FILE = 'fort.36')
      WRITE(36,*)'0.0 1.0'
      DO il = 1, NLV(Nnuc)
        WRITE(36,*)ELV(il,Nnuc), FLOAT(il - 1)
        WRITE(36,*)ELV(il,Nnuc), FLOAT(il)
        rolowint = EXP(( - EO/T))*(EXP(ELV(il,Nnuc)/T) - 1.)
        WRITE(34,*)ELV(il,Nnuc), rolowint + 1.0
      ENDDO
      CLOSE(36)
      CLOSE(34)
      IF(IOPsys.EQ.0)THEN
        iwin = PIPE('gnuplot fort.35')
        CLOSE(35)
      ENDIF
 
      RETURN
      END SUBROUTINE PLOT_GNU_NUMCUMUL_GC
