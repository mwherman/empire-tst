Ccc   * $Author: Capote $
Ccc   * $Date: 2009-01-16 08:31:48 $
Ccc   * $Id: dtrans.f,v 1.9 2009-01-16 08:31:48 Capote Exp $

      SUBROUTINE DTRANS(XMAX,ED,spec,cross)
C
C     DEUTERON INDUCED DIRECT STRIPPING d,p AND PICK-UP d,t
C     following C.Kalbach /PRECOC-1997/
C
C     Introduced into EMPIRE by A.V.Ignatyuk, April 2007
C
      include 'dimension.h'
      include 'global.h'
      DOUBLE PRECISION aa,ab,abb,gdel,fac1,fac2,fk,ED,xmax,uu
      DOUBLE PRECISION cross(0:ndejc),spectr,sg,spec(0:ndejc,ndex)
      DIMENSION jno(4),jpo(4),jst(4)
      data jno/1,0,2,2/                  
      data jpo/0,1,2,1/
      data jst/2,2,1,2/
      irea=ndejc
cig   cross(2)=0.D0
cig   cross(4)=0.D0
C-----Only deuteron reactions allowed
      if(Zejc(0).ne.1.D0.and.Aejc(0).ne.2.D0) return
      if(DXSred.LE.0.d0) return
             
      do ip=2,irea,2
         nnur=Nres(ip)
         UU=XMAX-Q(ip,1)-ED/2.D0
         NU=IDINT(UU/ED)+1
         if(NU.LE.0 .or. NU.GT.ndex) cycle
	 cross(ip) = 0.d0
         iab=jno(ip)+jpo(ip)
         ab=float(iab)
         aa=Aejc(0)
         abB=aa+A(0)-ab
         ndel=1
C--------gdel is the single-particle density for neutrons
         gdel=(A(0)-Z(0))/13.D0
C        write(8,100)ip,ndel,aa,ab,abb,Q(ip,1)
C--------Factors for stripping and pickup
         fac1=jst(ip)/3.*ab*(3800./abB)/Ein/(Ein+50.D0)**2
         fk=12.D0
         if(ip.eq.2) fk=fk*(3.0+35.0/(1.+exp((Ein-11.)/2.25)))
         fk=max(0.D0,fk)
         fac2=.0127*fk*gdel
         do ke=2,nu+1
            Ek=dfloat(ke-1)*ED-ED/2.D0
C-----------sigabs(ke,ip,1) was estimated in global
            IF (ETL(5,ip,nnur).EQ.0) THEN
               sg = SIGabs(ke+5,ip,nnur)
            ELSE
               sg = SIGabs(ke,ip,nnur)
            ENDIF
            spectr=sg*Ek*fac1*fac2*DXSred
            if(spectr.le.0.D0) cycle
            spec(ip,ke)=spectr
            cross(ip)=cross(ip)+spectr*de
C           write(8,102)ke,ek,fac1,fac2,sg,spec(ip,ke)
         enddo
      enddo
      return
100   format(2i5,4(f6.2))
102   format(i5,f8.4,6(1pe11.3))
      end
