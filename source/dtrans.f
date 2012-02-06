Ccc   * $Rev: 2443 $
Ccc   * $Author: mherman $
Ccc   * $Date: 2012-02-06 04:12:36 +0100 (Mo, 06 Feb 2012) $


      SUBROUTINE DTRANS(iemin,iemax)
C
C     DEUTERON INDUCED DIRECT STRIPPING d,p AND PICK-UP d,t
C     following C.Kalbach /PRECOC-1997/
C
C     Introduced into EMPIRE by A.V.Ignatyuk, April 2007
C
      include 'dimension.h'
      include 'global.h'
C
      INTEGER*2 iemax(0:NDEJC), iemin(0:NDEJC)
      DOUBLE PRECISION cross(0:NDEJC), spec(0:NDEJC,NDEX)
      COMMON /PEXS/ cross, spec
C
C     Local variables
C
      DOUBLE PRECISION ab,gdel,fac1,fac2,fk,ggg
C     DOUBLE PRECISION bndd
      DOUBLE PRECISION spectr,sg
      DIMENSION jno(4),jpo(4),jst(4)

      data jno/1,0,2,2/                  
      data jpo/0,1,2,1/
      data jst/2,2,1,2/
C
C 
C
C-----Only deuteron reactions allowed
      if(Zejc(0).ne.1.D0.and.Aejc(0).ne.2.D0) return
      if(DXSred.LE.0.d0) return

      ggg = GDIV
      if(GDIV.eq.0) ggg=13.d0
C-----gdel is the single-particle density for neutrons
      gdel = (A(0)-Z(0))/ggg*GTIlnor(1)

      IF(IOUt.GE.3) then
        write(8,*) 
        write(8,*) ' EMISSION SPECTRA : DIRECT reactions'
        write(8,*) 
      ENDIF
C     irea=NDEjc
      irea = 4 
      do ip=2,irea,2
         IF(IOUt.GE.3 .and. ip.eq.2) write(8,*)'(d,p) break-up reaction'
         IF(IOUt.GE.3 .and. ip.eq.4) write(8,*)'(d,t) pick-up  reaction'
         ab = jno(ip)+jpo(ip)
         ares  = A(1) - ab
         zres  = Z(1) - jpo(ip)
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle

         nnur = NREs(ip)
         if (nnur.lt.0) cycle
C--------Factors for stripping and pickup
         fac1=jst(ip)/3.*ab*(3800./ares)/Einl/(Einl+50.D0)**2
         fk=12.D0
         if(ip.eq.2) fk=fk*(3.0+35.0/(1.+exp((Einl-11.)/2.25)))
         fk=max(0.D0,fk)
         fac2=.0127*fk*gdel
         DO ke = iemin(ip), iemax(ip)
            ek = DE*(ke - 1)
C-----------sigabs(ke,ip,nnur) was estimated in global
            IF (ETL(5,ip,nnur).EQ.0) THEN
               sg = SIGabs(ke+5,ip,nnur)
            ELSE
               sg = SIGabs(ke,ip,nnur)
            ENDIF
            spectr=sg*ek*fac1*fac2*DXSred
            if(spectr.le.0.D0) cycle
            spec(ip,ke)=spectr
            cross(ip)=cross(ip)+spectr*DE
            IF(IOUt.GE.3) write(8,100) ek, spec(ip,ke)
C           write(8,102)ke,ek,fac1,fac2,sg,spec(ip,ke)
         enddo
         IF(IOUt.GE.3) write(8,*) 
      enddo
      return
100   format(1x,f6.2,3x,d12.6)
102   format(i5,f8.4,6(1pe11.3))
      end


