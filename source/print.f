Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
C
      SUBROUTINE PRINT_TOTAL(Nejc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nejc
C
C Local variables
C
      REAL*8 :: csemax, e, s0, s1, s2, s3, totspec
      REAL*8 :: DMAX1
      REAL :: FLOAT, SNGL
      CHARACTER :: haha, hstar
      INTEGER :: i, ia, ij, kmax, l, n
      INTEGER :: IFIX, MIN0
      CHARACTER, DIMENSION(93) :: symc
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         A U E R S T                              *
Ccc   *                                                                  *
Ccc   *   Prints histogram of NEJC-spectrum emitted from nucleus NNUC    *
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       NNUR-residual nucleus index                                *
Ccc   *       NEJC-ejectile index                                        *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:    2.Feb.1994                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      DATA hstar, haha/'*', ' '/
 
      csemax = 0.
      kmax = 1
      DO i = 1, NDEcse
        IF(CSEt(i,Nejc).GT.0.D0)kmax = i
        csemax = DMAX1(CSEt(i,Nejc),csemax)
      ENDDO
C
C     Stringest test to avoid plotting problems.
C     Cross sections smaller than 1.d-4 mb are not relevant at all.
C
      IF(csemax.LE.1.D-5)RETURN
 
      kmax = kmax + 1
      kmax = MIN0(kmax,NDEcse)
 
      totspec = 0.D0
      DO i = 1, kmax
        totspec = totspec + CSEt(i,Nejc)
      ENDDO
      totspec = totspec - 0.5D0*(CSEt(1,Nejc) + CSEt(kmax,Nejc))
      totspec = totspec*DE
      IF(totspec.LE.1.D-4)RETURN
 
      ia = AEJc(Nejc)
      IF(Nejc.EQ.0)THEN
        WRITE(8,1010)
 1010   FORMAT(1X,/,1X,54('*'),1X,'gamma spectrum  ',54('*'),//)
      ELSE
        IF(AEJc(Nejc).EQ.1.0D0.AND.ZEJc(Nejc).EQ.0.0D0)THEN
          WRITE(8,1020)
 1020     FORMAT(1X,/,1X,54('*'),1X,'neutron spectrum  ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.1.0D0.AND.ZEJc(Nejc).EQ.1.0D0)THEN
          WRITE(8,1030)
 1030     FORMAT(1X,/,1X,54('*'),1X,'proton spectrum  ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.4.0D0.AND.ZEJc(Nejc).EQ.2.0D0)THEN
          WRITE(8,1040)
 1040     FORMAT(1X,/,1X,54('*'),1X,'alpha spectrum   ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.2.0D0.AND.ZEJc(Nejc).EQ.1.0D0)THEN
          WRITE(8,1050)
 1050     FORMAT(1X,/,1X,54('*'),1X,'deuteron spectrum',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.3.0D0.AND.ZEJc(Nejc).EQ.1.0D0)THEN
          WRITE(8,1060)
 1060     FORMAT(1X,/,1X,54('*'),1X,'triton spectrum  ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.3.0D0.AND.ZEJc(Nejc).EQ.2.0D0)THEN
          WRITE(8,1070)
 1070     FORMAT(1X,/,1X,54('*'),1X,'he-3 spectrum    ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).GT.4.0D0)THEN
          WRITE(8,1080)ia, SYMbe(Nejc)
 1080     FORMAT(1X,/,1X,54('*'),1X,I3,'-',A2,' spectrum  ',54('*'),//)
        ENDIF
      ENDIF
 
      n = IFIX(SNGL(LOG10(csemax) + 1.))
      s3 = 10.**n
      s2 = s3*0.1
      s1 = s2*0.1
      s0 = s1*0.1
 
      WRITE(8,1090)s0, s1, s2, s3
 1090 FORMAT(1X,'Ener. ',5X,'Spectr. ',4X,E6.1,25X,E6.1,25X,E6.1,25X,
     &       E6.1)
      WRITE(8,1100)
 1100 FORMAT(2X,'MeV ',6X,'mb/MeV ',5X,'I ',3(29X,'I '))
      WRITE(8,1120)
 
      totspec = 0.0
      DO i = 1, kmax
        totspec = totspec + CSEt(i,Nejc)
        e = FLOAT(i - 1)*DE
        IF(CSEt(i,Nejc).GE.s0)THEN
          l = IFIX(SNGL(LOG10(CSEt(i,Nejc)) - n + 3)*31. + 0.5)
          l = MIN0(93,l)
          DO ij = 1, l
            symc(ij) = hstar
          ENDDO
          IF(l.NE.93)THEN
            l = l + 1
            DO ij = l, 93
              symc(ij) = haha
            ENDDO
          ENDIF
          GOTO 5
        ENDIF
        DO ij = 1, 93
          symc(ij) = haha
        ENDDO
    5   WRITE(8,1110)e, CSEt(i,Nejc), symc
 1110   FORMAT(1X,F6.2,3X,E11.4,2X,'I ',93A1,'I ')
      ENDDO
      totspec = totspec - 0.5*(CSEt(1,Nejc) + CSEt(kmax,Nejc))
      totspec = totspec*DE
      WRITE(8,1120)
      WRITE(8,'(1x,''    Integrated spectrum   '',G12.5,''  mb'')')
     &      totspec
 1120 FORMAT(24X,93('-'))
      END SUBROUTINE PRINT_TOTAL
 
!---------------------------------------------------------------------------
 
      SUBROUTINE AUERST(Nnuc,Nejc,Iflag)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Iflag, Nejc, Nnuc
C
C Local variables
C
      REAL*8 :: csemax, e, recorp, s0, s1, s2, s3, totspec
      REAL*8 :: DMAX1
      REAL :: FLOAT, SNGL
      CHARACTER :: haha, hstar
      INTEGER :: i, ia, ij, kmax, l, n
      INTEGER :: IFIX, MIN0
      CHARACTER, DIMENSION(93) :: symc
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         A U E R S T                              *
Ccc   *                                                                  *
Ccc   *   Prints histogram of NEJC-spectrum emitted from nucleus NNUC    *
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       NEJC-ejectile index                                        *
Ccc   *       Iflag=1 for integral of inclusive spectra (special case)   *
Ccc   *               Usually Iflag=0 for normal exclusive spectra       *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:    2.Feb.1994                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      DATA hstar, haha/'*', ' '/
 
      csemax = 0.
      kmax = 1
      DO i = 1, NDEcse
        IF(CSE(i,Nejc,Nnuc).GT.0.D0)kmax = i
        csemax = DMAX1(CSE(i,Nejc,Nnuc),csemax)
      ENDDO
C
C     Stringest test to avoid plotting problems.
C     Cross sections smaller than 0.05 mb are not relevant at all.
C
      IF(csemax.LE.1.D-5)RETURN
 
      kmax = kmax + 1
      kmax = MIN0(kmax,NDEcse)
      totspec = 0.D0
      DO i = 1, kmax
        totspec = totspec + CSE(i,Nejc,Nnuc)
      ENDDO
      IF(Iflag.EQ.0)totspec = totspec - 
     &                        0.5D0*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,
     &                        Nnuc))
      totspec = totspec*DE
      IF(totspec.LE.1.D-4)RETURN
 
      ia = AEJc(Nejc)
      IF(Nejc.EQ.0)THEN
        WRITE(8,1010)
 1010   FORMAT(1X,///,1X,54('*'),1X,'gamma spectrum  ',54('*'),//)
      ELSE
        IF(AEJc(Nejc).EQ.1.0D0.AND.ZEJc(Nejc).EQ.0.0D0)THEN
          WRITE(8,1020)
 1020     FORMAT(1X,///,1X,54('*'),1X,'neutron spectrum  ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.1.0D0.AND.ZEJc(Nejc).EQ.1.0D0)THEN
          WRITE(8,1030)
 1030     FORMAT(1X,///,1X,54('*'),1X,'proton spectrum  ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.4.0D0.AND.ZEJc(Nejc).EQ.2.0D0)THEN
          WRITE(8,1040)
 1040     FORMAT(1X,///,1X,54('*'),1X,'alpha spectrum   ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.2.0D0.AND.ZEJc(Nejc).EQ.1.0D0)THEN
          WRITE(8,1050)
 1050     FORMAT(1X,///,1X,54('*'),1X,'deuteron spectrum',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.3.0D0.AND.ZEJc(Nejc).EQ.1.0D0)THEN
          WRITE(8,1060)
 1060     FORMAT(1X,///,1X,54('*'),1X,'triton spectrum  ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).EQ.3.0D0.AND.ZEJc(Nejc).EQ.2.0D0)THEN
          WRITE(8,1070)
 1070     FORMAT(1X,///,1X,54('*'),1X,'he-3 spectrum    ',54('*'),//)
        ENDIF
        IF(AEJc(Nejc).GT.4.0D0)THEN
          WRITE(8,1080)ia, SYMbe(Nejc)
 1080     FORMAT(1X,///,1X,54('*'),1X,I3,'-',A2,' spectrum  ',54('*'),
     &           //)
        ENDIF
      ENDIF
 
      recorp = 1.D0
      IF(Nejc.GT.0)recorp = 1.D0 + EJMass(Nejc)/AMAss(Nnuc)
 
      n = IFIX(SNGL(LOG10(csemax*recorp) + 1.))
      s3 = 10.**n
      s2 = s3*0.1
      s1 = s2*0.1
      s0 = s1*0.1
 
      WRITE(8,1090)s0, s1, s2, s3
 1090 FORMAT(1X,'Ener. ',5X,'Spectr. ',4X,E6.1,25X,E6.1,25X,E6.1,25X,
     &       E6.1)
      WRITE(8,1100)
 1100 FORMAT(2X,'MeV ',6X,'mb/MeV ',5X,'I ',3(29X,'I '))
      WRITE(8,1120)
 
      totspec = 0.0
      DO i = 1, kmax
        totspec = totspec + CSE(i,Nejc,Nnuc)
        e = FLOAT(i - 1)*DE
        IF(CSE(i,Nejc,Nnuc).GE.s0)THEN
          l = IFIX(SNGL(LOG10(CSE(i,Nejc,Nnuc)) - n + 3)*31. + 0.5)
          l = MIN0(93,l)
          DO ij = 1, l
            symc(ij) = hstar
          ENDDO
          IF(l.NE.93)THEN
            l = l + 1
            DO ij = l, 93
              symc(ij) = haha
            ENDDO
          ENDIF
          GOTO 5
        ENDIF
        DO ij = 1, 93
          symc(ij) = haha
        ENDDO
    5   WRITE(8,1110)e/recorp, CSE(i,Nejc,Nnuc)*recorp, symc
 1110   FORMAT(1X,F6.2,3X,E11.4,2X,'I ',93A1,'I ')
      ENDDO
 
      IF(Iflag.EQ.0)totspec = totspec - 
     &                        0.5*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,
     &                        Nnuc))
      totspec = totspec*DE
 
      WRITE(8,1120)
      WRITE(8,'(1x,''    Integrated spectrum   '',G12.5,''  mb'')')
     &      totspec
 
      RETURN
 1120 FORMAT(24X,93('-'))
      END SUBROUTINE AUERST
 
!---------------------------------------------------------------------------
 
      SUBROUTINE PLOT_EMIS_SPECTRA(Nnuc,Nejc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nejc, Nnuc
C
C Local variables
C
      CHARACTER(16) :: caz
      REAL*8 :: csemax, recorp, totspec
      REAL*8 :: DMAX1
      REAL :: FLOAT
      INTEGER :: i, kmax
      INTEGER :: INT, MIN0
      CHARACTER(1), DIMENSION(0:6) :: part
      CHARACTER(31) :: title
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *   Produce zvview plots of NEJC-spectrum emitted from nucleus NNUC*
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       NEJC-ejectile index                                        *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: R. Capote                                                *
Ccc   * date:    March 2008                                              *
Ccc   * revision:#    by:name                     on:xx.mon.2009         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      DATA part/'g', 'n', 'p', 'a', 'd', 't', 'h'/
 
      csemax = 0.D0
      kmax = 1
      DO i = 1, NDEcse
        IF(CSE(i,Nejc,Nnuc).GT.0.D0)kmax = i
        csemax = DMAX1(CSE(i,Nejc,Nnuc),csemax)
      ENDDO
 
      IF(csemax.LE.1.D-5)RETURN
      kmax = kmax + 1
      kmax = MIN0(kmax,NDEcse)
 
      totspec = 0.0
      DO i = 1, kmax
        totspec = totspec + CSE(i,Nejc,Nnuc)
      ENDDO
      totspec = totspec - 0.5*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,Nnuc))
      totspec = totspec*DE
      IF(totspec.LE.1.D-4)RETURN
 
      IF(SYMb(Nnuc)(2:2).EQ.' ')THEN
        WRITE(caz,'(A3,I2.2,A1,A1,I3.3,A1,A1,A4)')'sp_', INT(Z(Nnuc)), 
     &        SYMb(Nnuc)(1:1), '_', INT(A(Nnuc)), '_', part(Nejc), 
     &        '.zvd'
      ELSE
        WRITE(caz,'(A3,I2.2,A2,I3.3,A1,A1,A4)')'sp_', INT(Z(Nnuc)), 
     &        SYMb(Nnuc), INT(A(Nnuc)), '_', part(Nejc), '.zvd'
      ENDIF
 
      OPEN(36,FILE = caz,STATUS = 'unknown')
 
      WRITE(title,'(a5, i2,1h-,A2,1h-,I3,3h(x, ,a1, 2h): ,F8.2, 2Hmb)')
     &      'tit: ', INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), part(Nejc), 
     &      totspec
 
      recorp = 1.D0
      IF(Nejc.GT.0)recorp = 1.D0 + EJMass(Nejc)/AMAss(Nnuc)
 
      CALL OPEN_ZVV(36,'SP_'//part(Nejc),title)
      DO i = 1, kmax
        IF(CSE(i,Nejc,Nnuc).LE.0.D0)CYCLE
        WRITE(36,'(1X,E12.6,3X,E12.6)')FLOAT(i - 1)*DE*1.D6/recorp, 
     &                                 CSE(i,Nejc,Nnuc)*recorp*1.D-3
                                           ! Energy, Spectra in b/MeV
      ENDDO
      CALL CLOSE_ZVV(36,'Energy','EMISSION SPECTRA')
      CLOSE(36)
      RETURN
      END SUBROUTINE PLOT_EMIS_SPECTRA
 
!---------------------------------------------------------------------------
 
      SUBROUTINE PLOT_TOTAL_EMIS_SPECTRA(Nejc)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER :: Nejc
C
C Local variables
C
      CHARACTER(8) :: caz
      REAL*8 :: csemax, totspec
      REAL*8 :: DMAX1
      REAL :: FLOAT
      INTEGER :: i, kmax
      INTEGER :: MIN0
      CHARACTER(1), DIMENSION(0:6) :: part
      CHARACTER(31) :: title
C
C*** End of declarations rewritten by SPAG
C
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *   Produce zvview plots of NEJC-inclusive spectrum                *
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   * input:NEJC-ejectile index                                        *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: R. Capote                                                *
Ccc   * date:    March 2008                                              *
Ccc   * revision:#    by:name                     on:xx.mon.2009         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      DATA part/'g', 'n', 'p', 'a', 'd', 't', 'h'/
 
      csemax = 0.D0
      kmax = 1
      DO i = 1, NDEcse
        IF(CSEt(i,Nejc).GT.0.D0)kmax = i
        csemax = DMAX1(CSEt(i,Nejc),csemax)
      ENDDO
 
      IF(csemax.LE.1.D-5)RETURN
 
      kmax = kmax + 1
      kmax = MIN0(kmax,NDEcse)
 
      totspec = 0.0
      DO i = 1, kmax
        totspec = totspec + CSEt(i,Nejc)
      ENDDO
      totspec = totspec - 0.5*(CSEt(1,Nejc) + CSEt(kmax,Nejc))
      totspec = totspec*DE
      IF(totspec.LE.1.D-4)RETURN
 
      WRITE(caz,'(A3,A1,A4)')'sp_', part(Nejc), '.zvd'
      OPEN(36,FILE = caz,STATUS = 'unknown')
      WRITE(title,'(a13,3h(x, ,a1, 2h): ,F8.2, 2Hmb)')
     &      'tit: Total Emission Spectra ', part(Nejc), totspec
 
      CALL OPEN_ZVV(36,'sp_'//part(Nejc),title)
      DO i = 1, kmax
        IF(CSEt(i,Nejc).LE.0.D0)CYCLE
        WRITE(36,'(1X,E12.6,3X,E12.6)')FLOAT(i - 1)*DE*1.D6, 
     &                                 CSEt(i,Nejc)*1.D-3
                                ! Energy, Spectra in b/MeV
      ENDDO
      CALL CLOSE_ZVV(36,'Energy','EMISSION SPECTRA')
      CLOSE(36)
      RETURN
      END SUBROUTINE PLOT_TOTAL_EMIS_SPECTRA
 
 
 
