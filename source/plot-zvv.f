Ccc   * $Rev: 2591 $
Ccc   * $Author: gnobre $
Ccc   * $Date: 2012-02-23 23:11:14 +0100 (Do, 23 Feb 2012) $

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

      DOUBLE PRECISION u
	INTEGER nene

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

C     write(title,'('' Nucleus : '',
C    &     i3,''-'',A2,''-'',I3)')
C    &     int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))

      OPEN (36, FILE=ctmp1, STATUS='unknown')

      CALL OPEN_ZVV(36,'RHO(U)   at GS of '//caz//ldname,' ')

	nene = 0
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)

        if(u.lt.ELV(NLV(Nnuc),Nnuc)) cycle

        rolowint1 = 0.D0
        rolowint2 = 0.D0
        DO j = 1, NLWst
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
         CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ', ' ')
         CLOSE (36)
	   RETURN 
	ENDIF  

      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'RHO(U,+) at GS of '//caz//ldname,' ')
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)
        rolowint1 = 0.D0
        DO j = 1, NLWst
          rolowint1 = rolowint1 + RO(kk,j,1,Nnuc)
        ENDDO
        IF(rolowint1.gt.1e30) exit
        IF(rolowint1.gt.0.d0) 
     &    WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &          1e6*u,rolowint1
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'RHO(U,-) at GS of '//caz//ldname,' ')
      DO kk = 1, NEX(Nnuc)
         u = EX(kk,Nnuc)
         rolowint2 = 0.D0
         DO j = 1, NLWst
            rolowint2 = rolowint2 + RO(kk,j,2,Nnuc)
         ENDDO
         IF(rolowint2.gt.1e30) exit
         IF(rolowint2.gt.0.d0) 
     &       WRITE (36,'(G10.3,2X,1P,(90E12.5))')
     &          1e6*u,rolowint2
       ENDDO

       CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ',' ')

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
      CHARACTER*10 fname
      CHARACTER*50 title

      DOUBLE PRECISION u,rocumul1,rocumul2

      if(SYMb(Nnuc)(2:2).eq.' ') then
        write(ctmp1,'(A1,A1,I3.3)')
     >      SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
      else
        write(ctmp1,'(A2,I3.3)')
     >      SYMb(Nnuc), int(A(Nnuc))
      endif

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
 
C     write(title,'('' Fissioning nucleus: '', i3,''-'',A2,''-'',I3)')
C    &     int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
      title = '    '

      OPEN (36, FILE=ctmp, STATUS='unknown')
      CALL OPEN_ZVV(36,'RHO(U)   at saddle of '//ctmp1//ldname,' ')
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

        CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ', title)
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

      CALL CLOSE_ZVV_LEVDEN(36,' LEVEL DENSITY ', title)
      CLOSE (36)
      return
      end
     
c==============================================================
      SUBROUTINE PLOT_ZVV_NumCumul(Nnuc,Defit,Nplot,Nlwst) 

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      INTEGER NLDGRID
      PARAMETER (NLDGRID = 60)!,JMAX = 5
      COMMON /UCGRID/ uugrid, cgrid, iugrid 
      COMMON /CT/ am,ux,eo,T

      REAL*8 am,ux,t,eo
      REAL*8 uugrid(0:NLDGRID), cgrid(0:NLDGRID,2)
C
C Dummy arguments
C
      REAL*8 Defit
      INTEGER Nnuc, Nlwst, Nplot
C
C Local variables
C
      CHARACTER*5 caz
      CHARACTER*7 fname
      CHARACTER*6 ldname
      CHARACTER*24 ctmp1
C     CHARACTER*50 title

      REAL FLOAT
      INTEGER ij, kk,iugrid
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

C     write(title,'('' Nucleus: '',i3,''-'',A2,''-'',I3)')
C    &  int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))

      OPEN (36, FILE=ctmp1, STATUS='unknown')

      CALL OPEN_ZVV(36,'Exp Cumul Levels of '//caz,' ')
      
      WRITE (36,*) '0.0 1.0'
      DO kk = 2, NLV(Nnuc)
         WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk - 1)
         WRITE (36,*) ELV(kk,Nnuc)*1d6,FLOAT(kk)
      ENDDO
      CALL CLOSE_ZVV(36,' ',' ')

      CALL OPEN_ZVV(36,'Integral [RHO(U)] of '//caz//ldname,' ')
	
      rocumul = 1.D0
      WRITE (36,*) '0.0 1.0'

C-----EGSM,GSM,GCM,OGCM
      IF(ADIv.NE.3)THEN
         DO kk = 2, nplot

            IF(defit*(kk - 1) .gt. ELV(NLV(Nnuc),Nnuc)+2.d0) exit

            IF(ADIv.EQ.2 .OR. ADIv.EQ.4)THEN
               rocumul = 1.d0 + 
     &            EXP(( - eo/t))*(EXP(defit*(kk - 1)/t) - 1.)
            ELSE
               DO ij = 1, NLWst
                  rocumul = rocumul + 0.5d0*defit*
     &                    (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &                     RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
               ENDDO
            ENDIF
            WRITE (36,*) defit*(kk - 1)*1.d6, rocumul            
         ENDDO
      ENDIF

C-----HFB
      IF(ADIv.eq.3)THEN
         DO kk = 1, iugrid
            IF(uugrid(kk) .gt. ELV(NLV(Nnuc),Nnuc)+2.d0) exit
            WRITE (36,*) uugrid(kk)*1d6,cgrid(kk,1)+cgrid(kk,2)
         ENDDO
      ENDIF   
 14   CALL CLOSE_ZVV_CUMUL(36,' CUMULATIVE NUMBER OF LEVELS ',' ')
      close(36)
      RETURN
      END

C======================================================
      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:1).ne.' ') write(iout,*) trim(title)      
      write(iout,'(A12,A)') 'fun: ',tfunct
      write(iout,'(A10)') 'thick: 2   '
      write(iout,'(A10/2H//)') 'length: 92 '
      return
      end

      SUBROUTINE CLOSE_ZVV(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/u'
      write(iout,'(A19)') '#begin LSTTAB.CUR/c'
      if(titlex(1:1).ne.' ') write(iout,*) 'x: ',trim(titlex)
      if(titley(1:1).ne.' ') write(iout,*) 'y: ',trim(titley)
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
      SUBROUTINE PLOT_GNU_NumCumul(Nnuc,Nplot,Defit,Dshift,Del,
     &                             NLwst) 


      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

C
C Dummy arguments
C
      REAL*8 Defit, Dshift, Del
      INTEGER Nnuc, Nlwst, Nplot


      COMMON /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      REAL*8 TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl ! CRIT

      REAL*8 am,ux,t,eo
      COMMON /CT/ am,ux,eo,T
C
C Local variables
C
      INTEGER ij, il, kk
      INTEGER INT
      INTEGER*4 iwin
      INTEGER*4 PIPE

 
      WRITE (8,99005) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                   ATIlnor(Nnuc), ATIl, NLV(Nnuc)
99005 FORMAT ('Cumulative plot for ',I3,'-',A2,'-',I3,' norm=',F6.4,
     &           ' atil=',F4.1,' Ncut=',I3)
      OPEN (35,FILE = 'fort.35')
      WRITE (35,*) 'set terminal postscript enhanced color lw 2
     & solid "Helvetica" 20'
      WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
      WRITE (35,99010) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)),
     &                    dshift, UCRt - DEL - dshift, DEF(1,Nnuc),
     &                    ATIl, NLV(Nnuc)
99010 FORMAT ('set title "',I3,'-',A2,'-',I3,
     &           '   Ushift = ',F6.3,' Ucrt = ',F5.2,' Def = ',F6.2,
     &           ' atil=',F4.1,' Ncut=',I3,'"')
      WRITE (35,*) 'set logscale y'
      WRITE (35,*) 'set xlabel "Excitation energy (MeV)" 0,0'
      WRITE (35,*) 'set ylabel "Cumulative number of levels" 0,0'
      WRITE (35,*) 'set style line 1 lt 1 lw 2'
      WRITE (35,*) 'set style line 2 lt 5 lw 2'
      WRITE (35,'(''plot "fort.36" w filledcu y2 ls 2 t "Discrete lev
     &els", "fort.34" w l ls 1 t "Level density" '')')
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
 
      DO kk = 2, nplot
         DO ij = 1, NLWst
            rocumul = rocumul + 0.5d0*defit*
     &           (RO(kk - 1,ij,1,Nnuc) + RO(kk,ij,1,Nnuc) +
     &           RO(kk - 1,ij,2,Nnuc) + RO(kk,ij,2,Nnuc))
         ENDDO
         WRITE (34,*) defit*(kk - 1), rocumul
      ENDDO
      CLOSE (36)
      CLOSE (34)
      IF (IOPsys.EQ.0) THEN
         iwin = PIPE('gnuplot fort.35')
         CLOSE (35)
      ENDIF

      RETURN
      END

c==============================================================
      SUBROUTINE PLOT_GNU_NumCumul_GC(Nnuc) 

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 am,ux,t,eo
      COMMON /CT/ am,ux,eo,T
 
      INTEGER Nnuc
      INTEGER*4 iwin
      INTEGER*4 PIPE


      WRITE (8,*) ' A=', A(Nnuc), 'Z=', Z(Nnuc), ' Ncut=', NLV(Nnuc)
      WRITE (8,*) ' a=', am, ' Ux=', ux, ' T=', t, ' EO=', eo
      OPEN (35,FILE = 'fort.35')
      WRITE (35,*) 'set terminal postscript enhanced color lw 2
     & solid "Helvetica" 20'
      WRITE (35,*) 'set output "|cat >>CUMULPLOT.PS"'
      WRITE (35,99010) INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), am, t,
     &                    eo, NLV(Nnuc)
99010 FORMAT ('set title "',I3,'-',A2,'-',I3,': a=',
     &           F4.1,' T=',F4.1,' E0=',F4.1,' Ncut=',I3,'"')
      WRITE (35,*) 'set logscale y'
      WRITE (35,*) 'set xlabel "Excitation energy (MeV)" 0,0'
      WRITE (35,*) 'set ylabel "Cumulative number of levels" 0,0'
      WRITE (35,*) 'set style line 1 lt 1 lw 2'
      WRITE (35,*) 'set style line 2 lt 5 lw 2'
      WRITE (35,'(''plot "fort.36" w filledcu y2 ls 2 t "Discrete lev
     &els", "fort.34" w l ls 1 t "Level density" '')')
      CLOSE (35)
      OPEN (34,FILE = 'fort.34')
      OPEN (36,FILE = 'fort.36')
      WRITE (36,*) '0.0 1.0'
      DO il = 1, NLV(Nnuc)
         WRITE (36,*) ELV(il,Nnuc), FLOAT(il - 1)
         WRITE (36,*) ELV(il,Nnuc), FLOAT(il)
         rolowint = EXP(( - eo/t))*(EXP(ELV(il,Nnuc)/t) - 1.)
         WRITE (34,*) ELV(il,Nnuc), rolowint + 1.0
      ENDDO
      CLOSE (36)
      CLOSE (34)
      IF (IOPsys.EQ.0) THEN
         iwin = PIPE('gnuplot fort.35')
         CLOSE (35)
      ENDIF

      RETURN
      END


