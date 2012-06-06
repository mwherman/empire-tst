Ccc   * $Rev: 2872 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-06-06 18:57:15 +0200 (Mi, 06 Jun 2012) $

      SUBROUTINE PLOT_ZVV_GSLD(Nnuc) 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION AP1, AP2, GAMma, DEL, DELp, BF, A23, A2         ! PARAM
      INTEGER NLWst                                                    ! PARAM
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*5 caz
      CHARACTER*7 fname
      CHARACTER*7 ldname
      CHARACTER*17 ctmp1
      CHARACTER*36 title

      DOUBLE PRECISION u, frho1, frho2
      INTEGER nene, kminex

      IF(NLV(Nnuc).le.3) return

      if(SYMb(Nnuc)(2:2).eq.' ') then
        write(caz,'(A1,A1,I3.3)')
     >      SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
      else
        write(caz,'(A2,I3.3)')
     >      SYMb(Nnuc), int(A(Nnuc))
      endif

      IF(ADIv.eq.0)write(ldname,'(A7)') ' (EGSM)'
      IF(ADIv.eq.1)write(ldname,'(A7)') ' (GSM) '
      IF(ADIv.eq.2)write(ldname,'(A7)') ' (GCM) '
      IF(ADIv.eq.3)write(ldname,'(A7)') ' (HFBM)'
      IF(ADIv.eq.4)write(ldname,'(A7)') ' (OGCM)'

      IF(ADIv.eq.0)write(fname,'(A7)') 'LD_EGSM'
      IF(ADIv.eq.1)write(fname,'(A7)') 'LD__GSM'
      IF(ADIv.eq.2)write(fname,'(A7)') 'LD__GCM'
      IF(ADIv.eq.3)write(fname,'(A7)') 'LD_HFBM'
      IF(ADIv.eq.4)write(fname,'(A7)') 'LD_OGCM'

      write(ctmp1,'(A17)') fname//'_'//caz//'.zvd'

      OPEN (36, FILE=ctmp1, STATUS='unknown')

      CALL OPEN_ZVV(36,'Exp RHO(U)=DN/DU of '//caz,' ')
      
      WRITE (36,*) '0.0 1.0'
      DO kk = 2, NLV(Nnuc)-1
         delta_energy = ELV(kk,Nnuc) - ELV(kk-1,Nnuc)
         if(delta_energy.le.0.d0) cycle
C        f'(a) = ( f(a) - f(a-h) )/h  : backward difference
C        f(a) - f(a-h) = Nlev(kk) - Nlev(kk-1) = 1
         frho1 = 1.d0/delta_energy
         delta_energy = ELV(kk+1,Nnuc) - ELV(kk,Nnuc)
         if(delta_energy.le.0.d0) cycle
C        f'(a) = ( f(a+h) - f(a) )/h  : forward  difference
C        f(a+h) - f(a) = Nlev(kk+1) - Nlev(kk) = 1
         frho2 = 1.d0/delta_energy
         WRITE (36,*) ELV(kk,Nnuc)*1d6,
     >     0.5d0 * (frho1 + frho2) ! average of forward/backward difference
      ENDDO
      WRITE (36,*) ELV(NLV(Nnuc),Nnuc)*1d6,frho2 
      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'RHO(U)   at GS of '//caz//ldname,' ')

      kminex = 1
      DO kk = 1, NEX(Nnuc)
        IF(ADIv.ne.3) then
          u = EX(kk,Nnuc)
      ELSE
          u = UEXcit(kk,Nnuc)
      ENDIF
      if(u.gt.ELV(NLV(Nnuc),Nnuc)) then
        if(FITlev.GT.0) then
            kminex = kk 
        else
           kminex = max(kk-1,1)
        endif
          exit
      endif
      ENDDO

      nene = 0

      DO kk = kminex, NEX(Nnuc)

        IF(ADIv.ne.3) then
          u = EX(kk,Nnuc)
        ELSE
          u = UEXcit(kk,Nnuc)
        ENDIF

        rolowint1 = 0.D0
        rolowint2 = 0.D0
        DO j = 1, NLW
          rolowint1 = rolowint1 + RO(kk,j,1,Nnuc)
          rolowint2 = rolowint2 + RO(kk,j,2,Nnuc)          
        ENDDO

        IF(rolowint1+rolowint2.gt.1e30) exit
        nene = nene + 1
C
C       Avoiding printing the first point 
C       as LDs are defined above the discrete levels  
C
        IF(rolowint1+rolowint2.gt.0.d0) 
     &    WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &          1e6*u,rolowint1+rolowint2
      ENDDO      

      IF(rolowint1+rolowint2.le.0.d0 .or. nene.lt.5) then
        CLOSE (36,STATUS='DELETE')
        RETURN 
      ENDIF

      IF(ADIv.NE.3) THEN 
        write(title,'(A27,F5.2,A4)') 'Energy continuum starts at ',
     >  Elv(NLV(Nnuc),Nnuc),' MeV' 

        CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ',title)
        CLOSE (36)
        RETURN 
      ENDIF

      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'RHO(U,+) at GS of '//caz//ldname,' ')

      DO kk = kminex, NEX(Nnuc)

        u = UEXcit(kk,Nnuc)

        rolowint1 = 0.D0
        DO j = 1, NLW
          rolowint1 = rolowint1 + RO(kk,j,1,Nnuc)
        ENDDO
        IF(rolowint1.gt.1e30) exit
        IF(rolowint1.gt.0.d0) 
     &    WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &          1e6*u,rolowint1
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'RHO(U,-) at GS of '//caz//ldname,' ')

      DO kk = kminex, NEX(Nnuc)

        u = UEXcit(kk,Nnuc)

        rolowint2 = 0.D0
        DO j = 1, NLWst
           rolowint2 = rolowint2 + RO(kk,j,2,Nnuc)
        ENDDO
        IF(rolowint2.gt.1e30) exit
        IF(rolowint2.gt.0.d0) 
     &      WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &          1e6*u,rolowint2
      ENDDO

      write(title,'(A27,F5.2,A4)') 'Energy continuum starts at ',
     >  Elv(NLV(Nnuc),Nnuc),' MeV' 

      CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ',title)

      CLOSE (36)
      RETURN
      END
C===============================================================
      SUBROUTINE PLOT_ZVV_SadLD(Nnuc,Ib) 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc,Ib
C
C Local variables
C
      CHARACTER*20 ctmp
      CHARACTER*7 caz, ldname
      CHARACTER*5 ctmp1
      CHARACTER*1 cbar
      CHARACTER*10 fname
C     CHARACTER*37 ctmp2
 
      DOUBLE PRECISION u,rocumul1,rocumul2

      if(SYMb(Nnuc)(2:2).eq.' ') then
        write(ctmp1,'(A1,A1,I3.3)')
     >      SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
      else
        write(ctmp1,'(A2,I3.3)')
     >      SYMb(Nnuc), int(A(Nnuc))
      endif

      write(cbar,'(I1)') ib
      IF(FISden(Nnuc).LE.1) then
        write(fname,'(A9,I1)') 'LD_EGSM_S',ib
        write(ctmp,'(A20)') fname//'_'//ctmp1//'.zvd'
        write(caz,'(A6,I1)') 'EGSM-S',Ib
        write(ldname,'(A7)') ' (EGSM)'
      ENDIF

      IF(FISden(Nnuc).eq.3) then
        write(fname,'(A9,I1)') 'LD_HFBM_S',ib
        write(ctmp,'(A20)') fname//'_'//ctmp1//'.zvd'
        write(caz,'(A6,I1)') 'HFBM-S',Ib
        write(ldname,'(A7)') ' (HFBM)'
      ENDIF
 
      OPEN (36, FILE=ctmp, STATUS='unknown')
      CALL OPEN_ZVV(36,
     &   'RHO(U)   at saddle '//cbar//' of '//ctmp1//ldname,' ')
      DO kk = 1,NRBinfis(Ib)
        u = UGRid(kk,Ib)
        rocumul1 = 0.d0
        rocumul2 = 0.d0
        DO j = 1,NLW
          rocumul1 = rocumul1 + ROFisp(kk,j,1,Ib)
          rocumul2 = rocumul2 + ROFisp(kk,j,2,Ib)
        ENDDO
        IF(rocumul1+rocumul2.gt.1e30) exit
        WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &        1e6*u,max(0.1d0,rocumul2+rocumul1)
      ENDDO

      CALL CLOSE_ZVV(36,' ',' ')


      IF(FISden(Nnuc).NE.3) THEN 

C       write(ctmp2,'(A28,F5.2,A4)') 'Fission continuum starts at ',
C    >  Elv(NLV(Nnuc),Nnuc),' MeV' 
C       CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ', ctmp2)

        CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ', ' ')
        CLOSE (36)
      RETURN

      ENDIF

      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'RHO(U,+) at saddle of '//ctmp1,' ')
      DO kk = 1,NRBinfis(Ib)
        u = UGRid(kk,Ib)
        rocumul1 = 0.d0
        DO j = 1,NLW
          rocumul1 = rocumul1 + ROFisp(kk,j,1,Ib)
        ENDDO
        IF(rocumul1.gt.1e30) exit
          WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &          1e6*u,max(rocumul1,0.1d0)
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')
      
      CALL OPEN_ZVV(36,'RHO(U,-) at saddle of '//ctmp1,' ')
      DO kk = 1,NRBinfis(Ib)
        u = UGRid(kk,Ib)
        rocumul2 = 0.d0
        DO j = 1,NLW
          rocumul2 = rocumul2 + ROFisp(kk,j,2,Ib)
        ENDDO
        IF(rocumul2.gt.1e30) exit
        WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &      1e6*u,max(0.1d0,rocumul2)
      ENDDO

C       write(ctmp2,'(A28,F5.2,A4)') 'Fission continuum starts at ',
C    >  Elv(NLV(Nnuc),Nnuc),' MeV' 
C     CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ', ctmp2)

      CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ', ' ')
 
      CLOSE (36)
      return
      end
     
c==============================================================
      SUBROUTINE PLOT_ZVV_NumCumul(Nnuc) 

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      COMMON /CT/ am,ux,eo,T
      REAL*8 am,ux,t,eo
C
C Dummy arguments
C
      REAL*8 Defit
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*5 caz
      CHARACTER*7 fname
      CHARACTER*6 ldname
      CHARACTER*24 ctmp1
      CHARACTER*36 ctmp2

      REAL FLOAT
      INTEGER ij, kk, nplot
      INTEGER INT

      if(NLV(Nnuc).le.3) return

      if(SYMb(Nnuc)(2:2).eq.' ') then
         write(caz,'(A1,A1,I3.3)')
     &         SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
      else
         write(caz,'(A2,I3.3)')
     &         SYMb(Nnuc), int(A(Nnuc))
      endif

      IF(ADIv.eq.0)write(ldname,'(A6)') '(EGSM)'
      IF(ADIv.eq.1)write(ldname,'(A6)') '(GSM) '
      IF(ADIv.eq.2)write(ldname,'(A6)') '(GCM) '
      IF(ADIv.eq.3)write(ldname,'(A6)') '(HFBM)'
      IF(ADIv.eq.4)write(ldname,'(A6)') '(OGCM)'

      IF(ADIv.eq.0)write(fname,'(A7)') 'NL_EGSM'
      IF(ADIv.eq.1)write(fname,'(A7)') 'NL__GSM'
      IF(ADIv.eq.2)write(fname,'(A7)') 'NL__GCM'
      IF(ADIv.eq.3)write(fname,'(A7)') 'NL_HFBM'
      IF(ADIv.eq.4)write(fname,'(A7)') 'NL_OGCM'
      write(ctmp1,'(A17)') fname//'_'//caz//'.zvd'

      OPEN (36, FILE=ctmp1, STATUS='unknown')

      CALL OPEN_ZVV(36,'Exp Cumul Discrete Levels of '//caz,' ')
      
      WRITE (36,*) '0.0 1.0'
      DO kk = 2, NLV(Nnuc)
         WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk - 1)
         WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk)
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'Integral [RHO(U)] of '//caz//ldname,' ')

      ncalc = 0 
      rocumul = 1.D0
C     WRITE (36,*) '0.0 1.0'

      if(ADIv.eq.0 .or. ADIv.eq.2 .or. ADIv.eq.4) then

        defit = (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc)+2.d0)
     &           /(NEXreq-1) 
        nplot = (ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc)+2.d0)/defit

        DO kk = 2, nplot

          u = defit*(kk - 1) 

          IF(ADIv.EQ.2 .OR. ADIv.EQ.4)THEN
C---------GCM,OGCM
            rocumul = 1.d0 + 
     &              EXP(( - eo/t))*(EXP(u/t) - 1.)
          ELSE
            DO ij = 1, NLW
              rocumul = rocumul + 0.5d0*defit*
     &          (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &           RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
            ENDDO
          ENDIF

          IF(u.lt.0.5*ELV(NLV(Nnuc),Nnuc)) cycle

          WRITE (36,*) u*1.d6, rocumul            
          ncalc = ncalc + 1

        ENDDO

      else

        DO kk = 2, NEX(Nnuc)
        defit = EX(kk,Nnuc) - EX(kk-1,Nnuc)
        u     = EX(kk,Nnuc)  

        IF(u .gt. ELV(NLV(Nnuc),Nnuc)+2.d0) exit

        IF(ADIv.EQ.2 .OR. ADIv.EQ.4)THEN
C---------GCM,OGCM
          rocumul = 1.d0 + 
     &              EXP(( - eo/t))*(EXP(u/t) - 1.)
        ELSE
          DO ij = 1, NLW
            rocumul = rocumul + 0.5d0*defit*
     &          (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &           RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
          ENDDO
        ENDIF

      IF(u.lt.0.5*ELV(NLV(Nnuc),Nnuc)) cycle

        WRITE (36,*) u*1.d6, rocumul            
        ncalc = ncalc + 1

        ENDDO

      endif

 14   write(ctmp2,'(A27,F5.2,A4)') 'Energy continuum starts at ',
     >  Elv(NLV(Nnuc),Nnuc),' MeV' 
 
      CALL CLOSE_ZVV_CUMUL(36,' CUMULATIVE NUMBER OF LEVELS ',ctmp2)
      if(ncalc.le.5) then
        close(36,status='DELETE')
      else
        close(36)
      endif
      RETURN
      END

C======================================================
      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:2).ne.'  ') write(iout,*) trim(title)      
      write(iout,'(A12,A)') 'fun: ',tfunct
      write(iout,'(A10)') 'thick: 2   '
      write(iout,'(A10/2H//)') 'length:250'
      return
      end

      SUBROUTINE CLOSE_ZVV(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/u'
      write(iout,'(A19)') '#begin LSTTAB.CUR/c'
      if(titlex(1:2).ne.'  ') write(iout,*) 'x: ',trim(titlex)
      if(titley(1:2).ne.'  ') write(iout,*) 'y: ',trim(titley)
      write(iout,'(A19)') 'x-scale: auto      '
      write(iout,'(A17)') 'y-scale: auto      '
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/c  '
      return
      end   

      SUBROUTINE CLOSE_ZVV_CUMUL(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/u'
      write(iout,*) '#begin aa/c'
      write(iout,*) 'tit: ',trim(titlex)
      write(iout,*) 'tit2: ',trim(titley)
      write(iout,*) 'x-unit:1000000, (MeV)'
      write(iout,*) 'ix-unit: 1  '
      write(iout,*) 'y-unit:1,   '
      write(iout,*) 'iy-unit: 1'
      write(iout,*) 'X-SCALE: lin'
      write(iout,*) 'Y-SCALE: lin'
      write(iout,*) 'X-RANGE: 0'
      write(iout,*) 'Y-RANGE: 1'
      write(iout,*) 'X-GRID: 1'
      write(iout,*) 'NOSTAT: 1'
      write(iout,*) 'legend: 1'
      write(iout,*) 'X: U'
      write(iout,*) 'x-long: Excitation energy U '
      write(iout,*) 'Y: Cumulative number of levels '
      write(iout,*) 'x-scale: auto '
      write(iout,*) 'y-scale: auto '
      write(iout,'(A2)') '//'
      write(iout,*) '#end aa/c'
      return
      end   

      SUBROUTINE CLOSE_ZVV_LEVDEN(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/u'
      write(iout,*) '#begin aa/c'
      write(iout,*) 'tit: ',trim(titlex)
      write(iout,*) 'tit2: ',trim(titley)
      write(iout,*) 'x-unit:1000000, (MeV)'
      write(iout,*) 'ix-unit: 1'
      write(iout,*) 'y-unit:1, (1/MeV)'
      write(iout,*) 'iy-unit: 1'
      write(iout,*) 'X-SCALE: lin'
      write(iout,*) 'Y-SCALE: log'
C     write(iout,*) 'X-RANGE: 0'
C     write(iout,*) 'Y-RANGE: 1'
      write(iout,*) 'X-GRID: 1'
      write(iout,*) 'NOSTAT: 1'
      write(iout,*) 'legend: 1'
      write(iout,*) 'X: U'
      write(iout,*) 'x-long: Excitation energy U '
      write(iout,*) 'Y: Level density     '
C     write(iout,*) 'x-scale: auto        '
C     write(iout,*) 'y-scale: auto        '
      write(iout,'(A2)') '//'
      write(iout,*) '#end aa/c  '
      return
      end   


c==============================================================
      SUBROUTINE PLOT_GNU_NumCumul(Nnuc,Dshift,Del)

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      REAL*8 Dshift, Del
      INTEGER Nnuc

      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl ! CRIT

      REAL*8 am,ux,t,eo
      COMMON /CT/ am,ux,eo,T
C
C Local variables
C
      INTEGER ij, il, kk, ncalc, nplot
      INTEGER INT
      INTEGER*4 iwin
      INTEGER*4 PIPE
      REAL*8 defit
 
      OPEN (35,FILE = 'fort.35')
      WRITE (35,*) 'set terminal postscript enhanced color lw 2
     & solid "Helvetica" 20'
      WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'

      IF(ADIv.eq.3) then 
        WRITE (35,99007) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &          NLV(Nnuc),DEF(1,Nnuc),ROHfbp(Nnuc),ROHfba(Nnuc)
99007 FORMAT ('set title "',I3,'-',A2,'-',I3,' Ncut=',I3,
     &        ' Def = ',F6.2,' ROHFBp = ',F6.3,' ROHFBa = ',F6.3,'"')
      ELSEIF(ADIv.eq.2 .or. ADIv.eq.4) then 
        WRITE (8,*) ' a=', am, ' Ux=', ux, ' T=', t, ' EO=', eo
        WRITE (35,99009) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &    t, ux, DEF(1,Nnuc), am, eo, NLV(Nnuc)
99009   FORMAT ('set title "',I3,'-',A2,'-',I3,
     &        '   T = ',F6.3,' Ux = ',F5.2,' Def = ',F6.2,
     &        ' atil=',F4.1,' E0=',F4.1,' Ncut=',I3,'"')
      ELSE
        WRITE (35,99010) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                    dshift, UCRt - DEL - dshift, DEF(1,Nnuc),
     &                    ATIl, NLV(Nnuc)
99010   FORMAT ('set title "',I3,'-',A2,'-',I3,
     &           '   Ushift = ',F6.3,' Ucrt = ',F5.2,' Def = ',F6.2,
     &           ' atil=',F4.1,' Ncut=',I3,'"')
      ENDIF

      WRITE (35,*) 'set logscale y'
      WRITE (35,*) 'set xlabel '' Excitation energy U [MeV]'' '
      WRITE (35,*) 'set ylabel '' Cumulative number of levels'' '
      WRITE (35,*) 'set style line 1 lt 1 lw 2'
      WRITE (35,*) 'set style line 2 lt 5 lw 2'
      WRITE (35,'(''plot "fort.36" w filledcu y2 ls 2 t "Experimental di
     &screte levels", "fort.34" w l ls 1 t "Integral of RHO(U)" '')')
      CLOSE (35)
      OPEN (34,FILE = 'fort.34')
      OPEN (36,FILE = 'fort.36')
      WRITE (36,*) '0.0 1.0'
      DO il = 2, NLV(Nnuc)
         WRITE (36,*) ELV(il,Nnuc), REAL(il - 1)
         WRITE (36,*) ELV(il,Nnuc), REAL(il)
      ENDDO
      rocumul = 1.0
      WRITE (34,*) '0.0  ', rocumul

      ncalc =0
      if(ADIv.eq.0 .or. ADIv.eq.2 .or. ADIv.eq.4) then
        defit = (ELV(NLV(Nnuc),Nnuc) + LDShif(Nnuc)+2.d0)
     &           /(NEXreq-1) 
        nplot = (ELV(NLV(Nnuc),Nnuc)+ LDShif(Nnuc)+2.d0)/defit

        DO kk = 2, nplot

          u     = defit*(kk - 1)

          IF(ADIv.EQ.2 .OR. ADIv.EQ.4)THEN
C---------GCM,OGCM
            rocumul = 1.d0 + 
     &              EXP(( - eo/t))*(EXP(u/t) - 1.)
          ELSE
            DO ij = 1, NLW
              rocumul = rocumul + 0.5d0*defit*
     &          (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &           RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
            ENDDO
          ENDIF

C         IF(u.lt.0.5*ELV(NLV(Nnuc),Nnuc)) cycle

          WRITE (34,*) u, rocumul            
          ncalc = ncalc + 1

        ENDDO

      else

        DO kk = 2, NEX(Nnuc)
        defit = EX(kk,Nnuc) - EX(kk-1,Nnuc)
        u     = EX(kk,Nnuc)  

        IF(u .gt. ELV(NLV(Nnuc),Nnuc) + 2.d0) exit

        IF(ADIv.EQ.2 .OR. ADIv.EQ.4)THEN
C---------GCM,OGCM
          rocumul = 1.d0 + 
     &              EXP(( - eo/t))*(EXP(u/t) - 1.)
        ELSE
          DO ij = 1, NLW
            rocumul = rocumul + 0.5d0*defit*
     &          (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &           RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
          ENDDO
        ENDIF

C       IF(u.lt.0.5*ELV(NLV(Nnuc),Nnuc)) cycle

        WRITE (34,*) u, rocumul            
        ncalc = ncalc + 1

        ENDDO

      endif

      if(ncalc.le.5) then
        close(36,status='DELETE')
        close(35,status='DELETE')
        close(34,status='DELETE')
      else
        close(34)
        close(35)
        close(36)
      endif

      IF (IOPsys.EQ.0) iwin = PIPE('gnuplot fort.35')

      RETURN
      END

