Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
 
 
      SUBROUTINE DTRANS(Iemin,Iemax)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
      REAL*8, DIMENSION(0:NDEjc) :: CROss
      REAL*8, DIMENSION(0:NDEjc,NDEx) :: SPEc
      COMMON /PEXS  / CROss, SPEc
C
C Dummy arguments
C
      INTEGER*2, DIMENSION(0:NDEjc) :: Iemax, Iemin
C
C Local variables
C
      REAL*8 :: ab, fac1, fac2, fk, gdel, ggg, sg, spectr
      REAL :: ares, ek, zres
      INTEGER :: ip, irea, ke, nnur
      INTEGER, DIMENSION(4) :: jno, jpo, jst
C
C*** End of declarations rewritten by SPAG
C
C
C     DEUTERON INDUCED DIRECT STRIPPING d,p AND PICK-UP d,t
C     following C.Kalbach /PRECOC-1997/
C
C     Introduced into EMPIRE by A.V.Ignatyuk, April 2007
C
C
C
C     Local variables
C
C     DOUBLE PRECISION bndd
 
      DATA jno/1, 0, 2, 2/
      DATA jpo/0, 1, 2, 1/
      DATA jst/2, 2, 1, 2/
C
C
C
C-----Only deuteron reactions allowed
      IF(ZEJc(0).NE.1.D0.AND.AEJc(0).NE.2.D0)RETURN
      IF(DXSred.LE.0.D0)RETURN
 
      ggg = GDIv
      IF(GDIv.EQ.0)ggg = 13.D0
C-----gdel is the single-particle density for neutrons
      gdel = (A(0) - Z(0))/ggg*GTIlnor(1)
 
      IF(IOUt.GE.3)THEN
        WRITE(8,*)
        WRITE(8,*)' EMISSION SPECTRA : DIRECT reactions'
        WRITE(8,*)
      ENDIF
C     irea=NDEjc
      irea = 4
      DO ip = 2, irea, 2
        IF(IOUt.GE.3.AND.ip.EQ.2)WRITE(8,*)'(d,p) break-up reaction'
        IF(IOUt.GE.3.AND.ip.EQ.4)WRITE(8,*)'(d,t) pick-up  reaction'
        ab = jno(ip) + jpo(ip)
        ares = A(1) - ab
        zres = Z(1) - jpo(ip)
C        residual nuclei must be heavier than alpha
        IF(ares.LE.4.AND.zres.LE.2.)CYCLE
 
        nnur = NREs(ip)
        IF(nnur.LT.0)CYCLE
C--------Factors for stripping and pickup
        fac1 = jst(ip)/3.*ab*(3800./ares)/EINl/(EINl + 50.D0)**2
        fk = 12.D0
        IF(ip.EQ.2)fk = fk*(3.0 + 35.0/(1. + EXP((EINl-11.)/2.25)))
        fk = MAX(0.D0,fk)
        fac2 = .0127*fk*gdel
        DO ke = Iemin(ip), Iemax(ip)
          ek = DE*(ke - 1)
C-----------sigabs(ke,ip,nnur) was estimated in global
          IF(ETL(5,ip,nnur).EQ.0)THEN
            sg = SIGabs(ke + 5,ip,nnur)
          ELSE
            sg = SIGabs(ke,ip,nnur)
          ENDIF
          spectr = sg*ek*fac1*fac2*DXSred
          IF(spectr.LE.0.D0)CYCLE
          SPEc(ip,ke) = spectr
          CROss(ip) = CROss(ip) + spectr*DE
          IF(IOUt.GE.3)WRITE(8,1010)ek, SPEc(ip,ke)
C           write(8,102)ke,ek,fac1,fac2,sg,spec(ip,ke)
        ENDDO
        IF(IOUt.GE.3)WRITE(8,*)
      ENDDO
      RETURN
 1010 FORMAT(1x,f6.2,3x,d12.6)
 1020 FORMAT(i5,f8.4,6(1pe11.3))
      END SUBROUTINE DTRANS
 
 
