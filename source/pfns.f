Ccc
Ccc   * $Date: 2006-05-04 07:53:13 $
Ccc   * $Id: pfns.f,v 1.2 2006-05-04 07:53:13 Capote Exp $
C
      real*8 function fniu(en,iaf,izf,acc)
C
C    Following Malinovskii 
C
      implicit real*8 (A-H,O-Z)
      real*8 CNdef,HFdef,LFdef,ftmp,Erel
      real*8 en, eg0, egn, bn, acc, uexcit
      real*8 deltaz,deltan,delta
C     Mass of heavy fragment fixed following Malinovskii
      data iah/140/
C     Average gamma energy release Eg = eg0 + niu*egn
      data eg0/4.42d0/,egn/0.99d0/
C
C     Normal distribution
C     P(j)=1/sqrt(2*pi)*exp(-j^2/2) ; s^2=1.
C     P(0)=0.399   P(1)=P(-1)=0.242  P(-2)=0.054
C     Normalization factor:
C     P0=P(-1)+P(0)+P(1)+P(-2)=0.937
C     data p2/0.054d0/,p1/0.242d0/,p0/0.399d0/,pnorm/0.937d0/
C
C     Unik UCD distribution
C     P(j)=1/sqrt(c*pi)*exp(-j^2/c) ; c=2*(s^2 + 1/12); s^2=0.40 +/- 0.05
C     P(0)=0.574   P(1)=P(-1)=0.204 P(2)=P(-2)=0.009
C     Normalization factor:
C     P0=P(-1)+P(0)+P(1)=0.982 for three points
C     data p1/0.204d0/,p0/0.574d0/,pnorm/0.982d0/
C     P0=P(-2)+P(-1)+P(0)+P(1)=0.991 for four points
      data p2/0.009/,p1/0.204d0/,p0/0.574d0/,pnorm/0.991d0/
C     P0=P(-2)+P(-1)+P(0)+P(1)+P(2)=1.000 for five points
C     data p2/0.009/,p1/0.204d0/,p0/0.574d0/,pnorm/1.d0/

C     Following Vladuca
      ftmp = real(izf)**2/real(iaf)
      egn = 6.71 - 0.156*ftmp ! p
      eg0 = 0.75 + 0.088*ftmp ! q

      bn = bind(iaf-izf,izf,CNdef)  ! get mass excess CNdef for fiss.nucleus
     &   - bind(iaf-izf-1,izf,ftmp) ! get Bn of the neutron in fiss. nucleus

C     For tighly bound even-even nuclei pairing is considered
      deltaz = 0.d0
      deltan = 0.d0
      delta = 0.d0
C     if(mod(izf,2).eq.0) deltaz = 12.d0/SQRT(float(iaf))
C     if(mod(iaf-izf,2).eq.0) deltan = 12.d00/SQRT(float(iaf))
      if(mod(izf,2).eq.0) 
     &         deltaz = 4.7d0/float(izf)**0.333333d0
      if(mod(iaf-izf,2).eq.0) 
     &         deltan = 4.93d0/float(iaf-izf)**0.333333d0
C     delta = (deltan + deltaz)*(1.d0 + beta2) 
      delta = deltan + deltaz
      uexcit = en + bn - delta

      Ekin_ave = TKE(izf,iaf,en)
CC----------------------------------
C     First fragment Ak (Reference)
      iah0 = iah
C     Malinovskii parametrization of the heavy fragment charge
      izh0 = float(izf)/float(iaf)*iah - 0.5
      izh = izh0
      izl = izf - izh
      ial = iaf - iah
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-1,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-1,izl,ftmp)
      Sn1 = 0.5*(Bnh+Bnl)
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-2,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-2,izl,ftmp)
      S2n1 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel1 = CNdef - HFdef - LFdef
C---------------------------
C     Second fragment Zh - 1
      izh = izh0 - 1
      izl = izf - izh
      ial = iaf - iah
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-1,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-1,izl,ftmp)
      Sn2 = 0.5*(Bnh+Bnl)
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-2,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-2,izl,ftmp)
      S2n2 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel2 = CNdef - HFdef - LFdef
C---------------------------
C     Third fragment Zh + 1
      izh = izh0 + 1
      izl = izf - izh
      ial = iaf - iah
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-1,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-1,izl,ftmp)
      Sn3 = 0.5*(Bnh+Bnl)
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-2,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-2,izl,ftmp)
      S2n3 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel3 = CNdef - HFdef - LFdef
C---------------------------
C     Fourth fragment Zh - 2
      izh = izh0 - 2
      izl = izf - izh
      ial = iaf - iah
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-1,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-1,izl,ftmp)
      Sn4 = 0.5*(Bnh+Bnl)
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-2,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-2,izl,ftmp)
      S2n4 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel4 = CNdef - HFdef - LFdef
C---------------------------
C     Fifth fragment Zh + 2 (to avergae selecting either (Zh + 2) or (Zh - 2)
C     Only four fragments considered to avoid odd-even effect
      izh = izh0 + 2
      izl = izf - izh
      ial = iaf - iah
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-1,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-1,izl,ftmp)
      Sn5 = 0.5*(Bnh+Bnl)
      Bnh = bind(iah-izh,izh,HFdef) - bind(iah-izh-2,izh,ftmp)
      Bnl = bind(ial-izl,izl,LFdef) - bind(ial-izl-2,izl,ftmp)
      S2n5 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel5 = CNdef - HFdef - LFdef

C===========================================================================
C     FINAL AVERAGE

      S1n  = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn4*p2)/pnorm
      S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n4*p2)/pnorm
	Sn = S1n
      Sn = 0.5*( S1n + 0.5*S2n)
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.92
      Erel = (Erel1*p0 + (Erel2 + Erel3)*p1 + Erel4*p2)/pnorm

C     eps   = 4./3.*dsqrt((Erel - Ekin_ave + uexcit)/(iaf/11.d0)) 
      eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/acc) 
      fniuA = (Erel + uexcit - Ekin_ave - eg0)/(eps + Sn + egn)
C     write(*,'('' Erel='',f6.1, '' En='',f5.2,'' Bn='',f5.2,
C    &          '' Sn='',f5.2, '' S2n/2='',f5.2,'' TKE='',f6.1, 
C    &          '' eps='',f6.2,'' nu='',f5.3)')
C    &      erel,en,bn,s1n,0.5*s2n,ekin_ave,eps,fniuA
      write(*,'('' Erel='',f5.1,'' En='',f6.3,'' Bn='',f5.2,
     &          '' Sn='',f5.2, '' Pc='',f4.2,'' TKE='',f5.1, 
     &          '' eps='',f5.2, '' nu='',f5.3)')
     &      erel,en,bn,sn,delta,ekin_ave,eps,fniuA

      S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm
      S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n5*p2)/pnorm
	Sn = S1n
      Sn = 0.5*( S1n + 0.5*S2n)      
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.92
      Erel = (Erel1*p0 + (Erel2 + Erel3)*p1 + Erel5*p2)/pnorm

C     eps   = 4./3.*dsqrt((Erel - Ekin_ave + uexcit)/(iaf/11.d0)) 
      eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/acc) 
      fniuB = (Erel + uexcit - Ekin_ave - eg0)/(eps + Sn + egn)
      fniu = 0.5 * (fniuA + fniuB)

C     write(*,'('' Erel='',f6.1, ''  En='',f5.2,''  Bn='',f5.2,
C    &          ''  Sn='',f5.2, '' S2n/2='',f5.2,''  TKE='',f6.1, 
C    &          ''  eps='',f6.2, '' nu='',f5.3)')
C    &      erel,en,bn,s1n,0.5*s2n,ekin_ave,eps,fniuB
      write(*,'('' Erel='',f5.1,'' En='',f6.3,'' Bn='',f5.2,
     &          '' Sn='',f5.2, '' Pc='',f4.2,'' TKE='',f5.1, 
     &          '' eps='',f5.2,'' nu='',f5.3)')
     &      erel,en,bn,sn,delta,ekin_ave,eps,fniuB

      return
      end

      real*8 function TKE(izf,iaf,en)
      integer izf,iaf
      real*8 ftmp,en
C     Itkis et al, Yad.Fiz.52(1990) 23; Fiz.Elem.Ch.At. Yadra 29(1998) 389
      ftmp = real(izf)**2/real(iaf)**(1.d0/3.d0)
      IF(ftmp .le. 900.) then
        TKE = 0.131*ftmp
      ELSE
        TKE = 0.104*ftmp + 24.3
      ENDIF
      if(izf.eq.89) then  ! Actinium
        TKE = 0.104*ftmp + 23.3 
        TKE = TKE + 0.4*en
        return
      endif
      if(izf.eq.90) then  ! Thorium
C       TKE = 0.104*ftmp + 23.85             
C       TKE = 0.104*ftmp + 24.05                 ! for delta <> 0 and beta2 = 0
C       TKE = 0.104*ftmp + 23.7                  ! for delta <> 0 and beta2 <>0 
C       TKE = 0.104*ftmp + 25.1                  ! for delta = 0 and S2n = 0
C       TKE = 0.104*ftmp + 24.85                 ! for delta = 0 and S2n <> 0
C       TKE = 0.112*ftmp + 13.7                  ! Vladuca
        TKE = 0.104*ftmp + 23.95                 ! for delta <> 0 and S2n <> 0
        TKE = TKE + 0.37*en                      ! eval
        return
      endif
      if(izf.eq.91) then  ! Protoactinium
        TKE = 166.7 + 0.016d0*(ftmp-1332.d0) 
C       TKE = 166.1 + 0.021d0*(ftmp-1332.d0)
        TKE = TKE + 0.37*en
        return
      endif
      if(izf.eq.92) then  ! Uranium
        TKE = 0.104*ftmp + 27.35
        TKE = TKE + 0.4*en
        return
      endif
C     Malinovskii et al, INDC(CCP)-277, IAEA 1987 (VANT N2,1987) in russian
      if(izf.eq.92) then  ! Uranium
        TKE = 169.24
        IF(en.lt.0.0001d0) TKE = TKE + 0.4*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For U-238 Bn=6.15
        IF(en.gt.0.0001d0 .and. en.le.1.65)
     >        TKE = TKE + (-0.423 + 0.044*(iaf-230))*en
        IF(en.gt.1.65)
     >        TKE = TKE + ( 0.144 - 0.047*(iaf-230))*en
      endif
      if(izf.eq.93) then  ! Neptunium
        TKE = 172.43
        IF(en.lt.0.0001d0) TKE = TKE + 0.25*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For Np-237 Bn=6.57
        IF(en.gt.0.0001d0 .and. en.le.1.23) TKE = TKE - 0.14*en
        IF(en.gt.1.23) TKE = TKE - 0.22*en
      endif
      if(izf.eq.94) then  ! Plutonium
        TKE = 176.4
        IF(en.lt.0.0001d0) TKE = TKE + 0.06*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For Pu-239 Bn=5.65
        IF(en.gt.0.0001d0 .and. en.le.2.15) TKE = TKE - 0.12*en
        IF(en.gt.2.15) TKE = TKE - 0.21*en
      endif
      if(izf.eq.95) then  ! Americium
        TKE = 179.74
        IF(en.lt.0.0001d0) TKE = TKE + 0.08*en
        IF(en.gt.0.0001d0) TKE = TKE -0.25*en
      endif
      if(izf.eq.96) then  ! Curium
        TKE = 183.23 - 0.338*(iaf-240)
        if(iaf.le.242) TKE = 183.23 - 0.338*2
        IF(en.lt.0.0001d0) TKE = TKE + 0.07*en
        IF(en.gt.0.0001d0) TKE = TKE + 0.11*en
      endif
      if(izf.eq.97) then  ! Bekerelium
        TKE = 185.08 - 0.405*(iaf-249)
        TKE = TKE + 0.1*en
      endif
      if(izf.eq.98) then  ! Californium
        TKE = 193.79 - 0.472*(iaf-240)
        TKE = TKE + 0.1*en
      endif
      return
      end

      subroutine get_fragmPFNS (fragmPFNS, emiss_en, nen_emis,
     >      eincid, af, zf, emed, tequiv)
C
C     See N.V.Kornilov, A.B.Kagalenko and F.-J.Hambsch
C     Phys. At. Nuclei 62 (1999) pp 173-185
C
      implicit real*8 (A-H,O-Z)
      integer nen_emis
      real*8 fragmPFNS(nen_emis), emiss_en(nen_emis)
      real*8 af,zf, eincid, delta, deltaz, deltan, bn 
      real*8 CNdef,HFdef,LFdef,ftmp, Erel,ThCF,TlCF,r,e,Efkin
C     real*8 fpost, fnscn, tscn, wscn
      integer iah
      data ThCF/0.8868d0/,r/1.248d0/,U0CF/32.9d0/
      data iah/140/ ! Assumed fixed mass of the heavy fragment
C     Scission neutrons for Th-232 following Lovchikova et al (Phys.At.Nuclei 67 (2004) p890)
C     data wscn/0.10d0/ ! fitted to get
C     data tscn/0.40d0/ ! scission neutron temperature (0.38 +/- 0.04)
      emed = 0.d0
      tequiv = 0.d0
      do i =1,nen_emis
        fragmPFNS(i) = 0.d0
	enddo
      iaf = nint(af)
      izf = nint(zf)

C     Parametrization of the Total Kinetic Energy in fission
      Efkin = TKE(izf,iaf,eincid)

      bn = bind(iaf-izf,izf,CNdef) - bind(iaf-izf-1,izf,ftmp)
      
C     Malinovskii parametrization of the heavy fragment charge
      izh = (zf/af)*iah - 0.5
      izl = izf - izh
      ial = iaf - iah

      ftmp = bind(iah-izh,izh,HFdef)
      ftmp = bind(ial-izl,izl,LFdef)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel = CNdef - HFdef - LFdef
      alpha0 = 1.d0
C--------------------------------------------------------------------------
C     Following Kornilov's paper
C     if(iaf.eq.233 .and. izf.eq.90) alpha = 0.947d0  ! Th232 target
C     if(izf.eq.90) alpha0 = 0.947d0  ! Th232 target and all thorium chain
C     if(iaf.eq.234 .and. izf.eq.92) alpha0 = 0.92d0   ! U233  target
C     if(iaf.eq.236 .and. izf.eq.92) alpha0 = 0.936d0  ! U235  target
C     if(iaf.eq.239 .and. izf.eq.92) alpha0 = 0.88d0   ! U238  target
C     if(iaf.eq.238 .and. izf.eq.93) alpha0 = 0.808d0  ! Np237 target
C     if(iaf.eq.240 .and. izf.eq.94) alpha0 = 0.873d0  ! Np237 target
C     if(iaf.eq.253 .and. izf.eq.98) alpha0 = 0.809d0  ! Np237 target
C--------------------------------------------------------------------------
C     Adjusted values (RCN, February 2006)
C     if(izf.le.91) alpha0 = 0.87d0  ! Th, Pa and Ac chains
      if(izf.le.91) alpha0 = 0.947d0 ! Th, Pa and Ac chains
C     if scission neutrons are considered then no reduction of
C            the CMS energy is needed
C     if(wscn.ne.0.d0) alpha0 = 1.d0
C     Following Maslov we interpolate alpha from 10 to 12.1 MeV
      alpha = alpha0
      if(eincid.gt.10.and.eincid.le.12.)  
     >                  alpha = alpha0 + 0.05*(10.d0 - eincid)
      if(eincid.gt.12.) alpha = 0.847d0   ! for Th,Pa,Ac chains
C
      EniuL =  float(iah)/float(ial*iaf)*alpha*Efkin
      EniuH =  float(ial)/float(iah*iaf)*alpha*Efkin
      TlCF = ThCF * r

C     For tighly bound even-even nuclei pairing is considered
      deltaz = 0.d0
      deltan = 0.d0
	delta = 0.d0
      if(mod(izf,2).eq.0) 
     &         deltaz = 4.7d0/float(izf)**0.333333d0
      if(mod(iaf-izf,2).eq.0) 
     &         deltan = 4.93d0/float(iaf-izf)**0.333333d0

C     delta = (deltan + deltaz)*(1.d0 + beta2) 
      delta = deltan + deltaz

      Ux = Erel - Efkin + eincid + bn - delta
      IF(UX.lt.0) return

      coeff = DSQRT( 252./af * Ux/U0CF)
C     Following formulae (7) of the paper
      Tlf = TlCF * coeff
      Thf = ThCF * coeff
      ftmp = 0.D0
      do i =1,nen_emis
        e = emiss_en(i)
        fragmPFNS(i) = 0.5d0*(fwatt(e,EniuH,Thf) + fwatt(e,EniuL,Tlf))
C       fpost = 0.5d0*( fwatt(e,EniuH,Thf) + fwatt(e,EniuL,Tlf) )
C       fnscn = fscn(e,tscn)
C       fragmPFNS(i) = wscn*fnscn + (1.d0 - wscn)*fpost
        IF(i.gt.1) then
          deltae = emiss_en(i)-emiss_en(i-1)
          fmed = (fragmPFNS(i)+fragmPFNS(i-1))*0.5
          emed = emed + fmed*deltae*(emiss_en(i)+emiss_en(i-1))*0.5d0
          ftmp = ftmp + fmed*deltae
        ENDIF
      enddo
      if(ftmp.GT.0) emed = emed/ftmp
      tequiv = 2.D0/3.D0*emed
      return
      end

      real*8 function fwatt(E,Eniu,T)
      real*8 E,T,pi,Eniu,b
      data pi/3.1415926d0/
      fwatt = 0.d0
      if(E.le.0.1d-5) E=0.1d-5
      if(T.le.0.1d-5) return
      if((E+Eniu).GT.50.d0*T) return
      b = 4*Eniu/T/T
      fwatt = 2.d0/T*DSQRT(E/(pi*T))*DEXP(-(E+Eniu)/T)*
     >             sinh(DSQRT(b*E))/DSQRT(b*E)
      return
      end

      real*8 function fmaxw(E,T)
      real*8 E,T,pi
      data pi/3.1415926d0/
      fmaxw = 0.d0
      if(E.le.0.1d-5) E=0.1d-5
      if(T.le.0.1d-5) return
      if(E.GT.50.d0*T) return
      fmaxw = 2.d0/T*DSQRT(E/(pi*T))*DEXP(-E/T)
      return
      end

      real*8 function fscn(E,T)
      real*8 E,T
      fscn = 0.d0
      if(E.le.0.1d-5) E=0.1d-5
      if(T.le.0.1d-5) return
      if(E.GT.50.d0*T) return
      fscn = 1.d0/T*(E/T)*DEXP(-E/T)
      return
      end

      real*8 function bind(nn,nz,exc)
      integer nn,nz
      real*8 exc
c     real*4 e  ! for mass10 CALL
      REAL*8 AMUmev, PI, CETa, CSO, AMPi, ELE2, HHBarc,
     &       AMUneu, AMUpro, AMUele,
     &       EXCessmass(0:130,0:400), RESmas(0:130,0:400)
      COMMON /XMASS / EXCessmass, RESmas
      COMMON /CONSTANT/ AMUmev, PI, CETa, CSO, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro, AMUele
c--------------------------------------------------------------------c
c       File "du_zu_10.feb96"                                        c
c                                                                    c
c       J. Duflo and A.P. Zuker  Feb 23, 1996  10 parameters formula c
c       Reference:                                                   c
c             Phys. Rev. C52, 1995  (for the 28 param. formula)      c
c             and Private Communication to AMDC, February 1996.      c
c                                                                    c
c       Microscopic   calculation   of   nuclear  masses   with   10 c
c       parameters.  Fit  to the 1810 measured  masses from Helium-4 c
c       and up with a root-mean-square deviation (rms) of 506 keV.   c
c--------------------------------------------------------------------c
c     call mass10(nn,nz,e)
c     exc=nz*7.28903+nn*8.07138-e
c     bind = dble(e)
      exc = EXCessmass(nz,nz+nn)
      bind = dble(nz*7.28903+nn*8.07138 - exc)
C     print *, ' N=',nn,' Z=',nz,' B.E.=',e,' MassExcess=', exc
      return
      end
      
      subroutine mass10(nx,nz,E)     ! Duflo-Zuker fevrier 1996
c Calculation of binding energy E (nx neutrons,nz protons)
      dimension b(10),dyda(10),op(2),n2(2),dx(2),qx(2),os(2),
     &          onp(0:8,2,2),oei(2),dei(2),nn(2),noc(18,2),pp(2),y(2)
      data b/0.7043,17.7418,16.2562,37.5562,53.9017,0.4711,2.1307,
     &       0.0210,40.5356,6.0632/
c*********
      nn(1)=nx
      nn(2)=nz
      a=nx+nz
      t=abs(nx-nz)
      r=a**(1./3.)
      s=r*r
      rc=r*(1.-.25*(t/a)**2)       !      Charge radius
      ra=(rc*rc)/r
c--------
      z2=nz*(nz-1)
      dyda(1)=(-z2+.76*z2**(2./3.))/rc  ! Coulomb energy
c********                          ! beginning of main loop
      do ndef=1,2                  !      ndef=1  spherical
      ju=0                         !      ndef=2  deformed
      y(ndef)=0.
      if(ndef.eq.2) ju=4           !      nucleons associated to deform.
      do kk=2,10
        dyda(kk)=0.
      enddo
c--------                          ! beginning of loop over N and Z
      do j=1,2
        do l=1,18
          noc(l,j)=0
        enddo
        do l=1,2
          do k=0,8
            onp(k,l,j)=0.
          enddo
        enddo
        n2(j)=2*(nn(j)/2)          !      (for pairing calculation)
        ncum=0
        i=0
c--------
  20    i=i+1                      !     sub-shells (ssh) j and r filling
        i2=(i/2)*2
        if(i2.ne.i)then
          id=i+1                   !             for ssh j
        else
          id=i*(i-2)/4             !             for ssc r
        endif
        ncum=ncum+id
        if(ncum.lt.nn(j))then
          noc(i,j)=id              !     nb of nucleons in each ssh
          go to 20
        endif
c--------
        imax=i+1                   !     imax = last subshell nb
        ip=(i-1)/2                 !     HO number (p)
        ipm=i/2
        pp(j)=ip
        moc=nn(j)-ncum+id
        noc(i,j)=moc-ju            !     nb of nucleons in last ssh
        noc(i+1,j)=ju
        if(i2.ne.i)then            !     ssh j
          oei(j)=moc+ip*(ip-1)     !       nb of nucleons in last EI shell
          dei(j)=ip*(ip+1)+2       !       size of the EI shell
        else                       !     ssh r
          oei(j)=moc-ju            !       nb of nucleons in last EI shell
          dei(j)=(ip+1)*(ip+2)+2   !       size of the EI shell
        endif
        qx(j)=oei(j)*(dei(j)-oei(j)-ju)/dei(j)  ! n*(D-n)/D        S3(j)
        dx(j)=qx(j)*(2*oei(j)-dei(j))           ! n*(D-n)*(2n-D)/D  Q
        if(ndef.eq.2)qx(j)=qx(j)/sqrt(dei(j))   ! scaling for deformed
c--------
        do i=1,imax                             ! Amplitudes
          ip=(i-1)/2
          fact=sqrt((ip+1.)*(ip+2.))
          onp(ip,1,j)=onp(ip,1,j)+noc(i,j)/fact !    for FM term
          vm=-1.
          if((2*(i/2)).ne.i)vm=.5*ip            !    for spin-orbit term
          onp(ip,2,j)=onp(ip,2,j)+noc(i,j)*vm
        enddo
c--------
        op(j)=0.
        os(j)=0.
        do ip=0,ipm                !       FM and SO terms
          pi=ip
          den=((pi+1)*(pi+2))**(3./2.)
          op(j)=op(j)+onp(ip,1,j)                                ! FM
          os(j)=os(j)+onp(ip,2,j)*(1.+onp(ip,1,j))*(pi*pi/den)   ! SO
     &               +onp(ip,2,j)*(1.-onp(ip,1,j))*((4*pi-5)/den)
        enddo
        op(j)=op(j)*op(j)
      enddo
c--------                          ! end of loop over  N and Z
      dyda(2)=op(1)+op(2)                 !   Master term (FM): volume
      dyda(3)=-dyda(2)/ra                 !                     surface
      dyda(2)=dyda(2)+os(1)+os(2)         !   FM + SO
      dyda(4)=-t*(t+2)/(r*r)              !   isospin term : volume
      dyda(5)=-dyda(4)/ra                 !                : surface
      if(ndef.eq.1)then                   ! sph.
        dyda(6)=dx(1)+dx(2)               !   S3  volume
        dyda(7)=-dyda(6)/ra               !       surface
        px=sqrt(pp(1))+sqrt(pp(2))
        dyda(8)=qx(1)*qx(2)*(2**px)       !   QQ sph.
      else                                ! def.
        dyda(9)=qx(1)*qx(2)               !   QQ deform.
      endif
      dyda(5)=t*(1-t)/(a*ra**3)+dyda(5)   !   "Wigner term"
c--------                                 !   PAIRING
      if(n2(1).ne.nn(1).and.n2(2).ne.nn(2))dyda(10)= t/a
      if(nx.gt.nz)then
        if(n2(1).eq.nn(1).and.n2(2).ne.nn(2))dyda(10)= 1-t/a
        if(n2(1).ne.nn(1).and.n2(2).eq.nn(2))dyda(10)= 1
      else
        if(n2(1).eq.nn(1).and.n2(2).ne.nn(2))dyda(10)= 1
        if(n2(1).ne.nn(1).and.n2(2).eq.nn(2))dyda(10)= 1-t/a
      endif
      if(n2(2).eq.nn(2).and.n2(1).eq.nn(1))dyda(10)= 2-t/a
c--------
      do mss=2,10
        dyda(mss)=dyda(mss)/ra
      enddo
      do mss=1,10
        y(ndef)=y(ndef)+dyda(mss)*b(mss)
      enddo
c--------                            ! end of main loop
      enddo
      de=y(2)-y(1)
      E=y(2)                         ! Binding Energy for def. nuclides
      if(de.le.0..or.nz.le.50)E=y(1) !                spherical nuclides
      return
      end

      real*8 function fniuTH232(en)
      implicit real*8 (A-H,O-Z)
      real*8 Eniu(20),Vniu(20),en
      integer i
      data Eniu/
     > 1.D-11, 1.D0, 3.d0, 4.d0, 5.7d0, 7.d0, 10.d0,14.7d0, 20.d0,
     > 22.d0 ,24.d0,26.d0,28.d0,30.d0 ,35.d0,40.d0, 45.d0 , 50.d0,
     > 55.d0 ,60.d0/
      data Vniu/
     > 2.05D0, 2.127D0, 2.263D0, 2.4023D0, 2.64D0, 2.996D0, 3.37D0,
     > 3.97D0, 4.79D0, 5.052D0, 5.2731D0, 5.5143D0, 5.7053D0, 5.9263D0,
     > 6.4284D0, 6.8801D0, 7.3217D0, 7.7434D0, 8.1242D0, 8.5053D0/
      fniu = Vniu(1)
      if(en.lt.1.d-11) RETURN
c     if(en.gt.60) STOP 'En > 60 MeV, NO PFNM data'
      do i=1,20
        if(Eniu(i).gt.en) exit
      enddo
      fniuTH232 = Vniu(i-1) +
     >   (Vniu(i)-Vniu(i-1))*(en-Eniu(i-1))/(Eniu(i)-Eniu(i-1))
      return
      end
