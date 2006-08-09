Ccc   * $Author: Capote $ 
Ccc   * $Date: 2006-08-09 12:37:42 $
Ccc   * $Id: systematics.f,v 1.4 2006-08-09 12:37:42 Capote Exp $
            
      
      SUBROUTINE SYSTEMATICS(Atrg,Ztrg,iprojectile)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                      S Y S T E M A T I C S                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * Calculates sytematics for the most important neutron induced     *
Ccc   * reactions at 14 and 20 MeV. Cross sections are in mb.            *
Ccc   * Systematics were taken from EAF-3.0 manual (2005).               *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * inputi:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc 
       IMPLICIT NONE
       REAL*4 Atrg, Ztrg,csnp, csna,  csng, csn2p, csnna, csnnp2, csnp2,
     &        csna2, csn2n, s
       INTEGER iprojectile
       IF(iprojectile.NE.1) THEN
          RETURN
       ENDIF
       s = (Atrg - 2*Ztrg)/Atrg
C
C------14.5 MeV
C
C------(n,p) at 14.5 MeV acccording to Forrest and Doczi
       IF(Atrg.GE.40) THEN
          csnp = 7.657*(Atrg**0.3333 + 1.0)**2*EXP(-28.80*s + 0.2365*
     &           Atrg**0.5)
       ELSE
          csnp = 23.659*(Atrg**0.3333 + 1.0)**2*EXP(-23.041*(s+s**2))
       ENDIF
C------(n,a) at 14.5 MeV acccording to Majdeddin
       csna = 15.0678*(Atrg**0.3333 + 1.0)**2*EXP(-27.55*(s+s**2))
C------(n,g) at 14.5 MeV acccording to Kopecky at al.
       csng = 1.18-1.13*EXP(-0.01338*Atrg)
C------(n,2n) at 14.5 MeV acccording to Badikov et al.
       IF(Atrg.LE.210) THEN
          csn2n = 47.015*(Atrg**0.3333 + 1.0)**2*(1.0-3.9777*
     &            EXP(-24.116*s))
       ELSE
          csn2n = -1.0
       ENDIF
C------(n,2p) at 14.5 MeV acccording to Kopecky at al.
       csn2p = 48.49*EXP(-2.99*s)/1000.
C------(n,na) at 14.5 MeV acccording to Kopecky at al.
       csnna = 17.48*EXP(-24.2*s)

C
C------20.0 MeV
C
       IF(Atrg.GT.40) THEN
C------(n,np) at 20 MeV according to Konobeyev
       csnnp2 = 53.066*(Atrg**0.3333 + 1.0)**2*(Atrg**(-0.3333)*(-2.7098
     &    *(s+1.5/Atrg)+0.67115)**2 + EXP(-496.74*(s+1.5/Atrg)**2 + 
     &    48.162*(s+1.5/Atrg)-1.6714))
C------(n,p) at 20 MeV according to Konobeyev
       IF(Ztrg.LT.50) THEN
          csnp2 = 53.066*(Atrg**0.3333 + 1.0)**2*EXP(-47.384*
     &               (s+1.0/Atrg)**2 - 2.3294*(s+0.5/Atrg) - 
     &               0.10405*Ztrg/Atrg**0.3333 - 2.3483)
       ELSE
          csnp2 = 53.066*(Atrg**0.3333 + 1.0)**2*
     &            (-1.2477*(s+1.0/Atrg)+0.4087)**3
       ENDIF
C------(n,a) at 20 MeV according to Konobeyev
       csna2 = -1. 
       IF(Ztrg.LT.50)
     & csna2 = 53.066*(Atrg**0.3333 + 1.0)**2*
     &         EXP(-37.317*(s+1.0/Atrg)**2 - 7.2027*(s+0.5/Atrg) - 
     &         0.22669*Ztrg/Atrg*0.3333 - 2.027)
       ELSE
          csnnp2 = -1. 
          csnp2 = -1. 
          csna2 = -1. 
       ENDIF
        
C
C------Printout
C
       OPEN(48, FILE='SYSTEMATICS.TXT', STATUS='unknown') 
       WRITE(48,*)'Cross sections provided by systematics (mb)'
       WRITE(48,*)'(-1.000 indicates systematics out of range)'
       WRITE(48,*)' '
       WRITE(48,'(''          A = '',I3,''    Z = '',I3)') 
     & INT(Atrg), INT(Ztrg)
       WRITE(48,*)' '
       WRITE(48,*)'Reaction         14.5 MeV         20 MeV'
       WRITE(48,*)' '
       WRITE(48,'('' (n,g)        '',2G15.4)') csng
       WRITE(48,'('' (n,p)        '',2G15.4)') csnp, csnp2
       WRITE(48,'('' (n,a)        '',2G15.4)') csna, csna2
       WRITE(48,'('' (n,na)       '',2G15.4)') csnna 
       WRITE(48,'('' (n,2n)       '',2G15.4)') csn2n 
       WRITE(48,'('' (n,2p)       '',2G15.4)') csn2p 
       WRITE(48,'('' (n,np)       '',15x, G15.4)') csnnp2  
       RETURN
       END
       
