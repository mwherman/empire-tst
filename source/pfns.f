Ccc   * $Rev: 2443 $
Ccc   * $Author: mherman $
Ccc   * $Date: 2012-02-06 04:12:36 +0100 (Mo, 06 Feb 2012) $

      SUBROUTINE get_fragmPFNS (fragmPFNS, emiss_en, nen_emis,
     &      eincid, af, zf, emed, tequiv, qval, deltae,
     &      PFNtke, PFNrat, PFNalp)
C
C     See N.V.Kornilov, A.B.Kagalenko and F.-J.Hambsch
C     Phys. At. Nuclei 62 (1999) pp 173-185
C
C
      implicit none
C     Dummy parameters
      integer nen_emis
      real*8 fragmPFNS(nen_emis), emiss_en(nen_emis)
      real*8 eincid, af, zf, emed, tequiv, qval, deltae
      real*8 PFNtke, PFNrat, PFNalp

C     Local variables
      real*8 CNdef,HFdef,LFdef,ftmp, Erel
      real*8 ThCF,TlCF,r,e,Efkin,Tlf,Thf
	real*8 Ux,EniuL,EniuH
      real*8 fmed
      integer iah,ial,iaf,izf,izh,izl,i
C     real*8 fpost, fnscn, tscn, wscn
      real*8 alpha,alpha0,coeff,U0Cf
      real*8 TKE, bind, fwatt
      
C     Mass of heavy fragment fixed following Malinovskii
      data iah/140/ 

C     Kornilov model parameters
      data ThCF/0.8868d0/,r/1.248d0/,U0Cf/32.9d0/

C     Scission neutrons for Th-232 following Lovchikova et al (Phys.At.Nuclei 67 (2004) p890)
C     data wscn/0.10d0/ ! "wscn,tscn" fitted to get scission neutron temperature (0.38 +/- 0.04)
C     data tscn/0.40d0/ !

      emed = 0.d0
      tequiv = 0.d0
      fragmPFNS = 0.d0

      iaf = nint(af)
      izf = nint(zf)

C     Parametrization of the Total Kinetic Energy in fission
      Efkin = TKE(izf,iaf,eincid) * PFNtke  ! PFNtke is the scaling factor
      
      ftmp = bind(iaf-izf,izf,CNdef) 

C     Malinovskii parametrization of the heavy fragment charge
      izh = (zf/af)*iah - 0.5
      izl = izf - izh
      ial = iaf - iah

      ftmp = bind(iah-izh,izh,HFdef)
      ftmp = bind(ial-izl,izl,LFdef)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel = CNdef - HFdef - LFdef

      alpha0 = 1.d0
C     Adjusted values (RCN, February 2006)
      if(izf.le.91) alpha0 = 0.947d0 ! Th, Pa and Ac chains
      if(izf.eq.92) alpha0 = 0.933d0 ! U chain
      if(izf.eq.94) alpha0 = 0.873d0 ! Pu chain
      if(izf.eq.95) alpha0 = 0.808d0 ! Am chain = Np237 
C--------------------------------------------------------------------------
C     Following Kornilov's paper
      if(izf.eq.90) alpha0 = 0.947d0  ! Th232 target and all thorium chain
      if(iaf.eq.234 .and. izf.eq.92) alpha0 = 0.92d0   ! U233  target
      if(iaf.eq.236 .and. izf.eq.92) alpha0 = 0.936d0  ! U235  target
      if(iaf.eq.239 .and. izf.eq.92) alpha0 = 0.88d0   ! U238  target
      if(iaf.eq.238 .and. izf.eq.93) alpha0 = 0.808d0  ! Np237 target
      if(iaf.eq.240 .and. izf.eq.94) alpha0 = 0.873d0  ! Pu240 target
      if(iaf.eq.253 .and. izf.eq.98) alpha0 = 0.809d0  ! Cf252 target
C--------------------------------------------------------------------------

C     if scission neutrons are considered then no reduction of
C            the CMS energy is needed
C     if(wscn.ne.0.d0) alpha0 = 1.d0
C     Following Maslov we interpolate alpha from 10 to 12.1 MeV
      alpha = alpha0
      if(eincid.gt.10.and.eincid.le.12.)  
     &                  alpha = alpha0 + 0.05*(10.d0 - eincid)
      if(eincid.gt.12.) alpha = alpha0 - 0.05*2.d0  ! the value at eincid = 12
C
      alpha = alpha * PFNalp  ! reduction of TKE for fragments

      EniuL =  float(iah)/float(ial*iaf)*alpha*Efkin
      EniuH =  float(ial)/float(iah*iaf)*alpha*Efkin

      TlCF = ThCF * r * PFNrat

      Ux = Erel - Efkin + eincid + qval 

      IF(UX.lt.0) return

      coeff = DSQRT( 252./af * Ux/U0CF)
C     Following formulae (7) of the paper
      Tlf = TlCF * coeff
      Thf = ThCF * coeff
      ftmp = 0.D0
	emed = 0.d0
      do i =1,nen_emis
        e = emiss_en(i)
        fragmPFNS(i) = 0.5d0*(fwatt(e,EniuH,Thf) + fwatt(e,EniuL,Tlf))
C       fpost = 0.5d0*( fwatt(e,EniuH,Thf) + fwatt(e,EniuL,Tlf) )
C       fnscn = fscn(e,tscn)
C       fragmPFNS(i) = wscn*fnscn + (1.d0 - wscn)*fpost
        IF(i.gt.1) then
          fmed = (fragmPFNS(i)+fragmPFNS(i-1))*0.5
          emed = emed + fmed*deltae*(emiss_en(i)+emiss_en(i-1))*0.5d0
          ftmp = ftmp + fmed*deltae
        ENDIF
      enddo
      if(ftmp.GT.0) emed = emed/ftmp
      tequiv = 2.D0/3.D0*emed
      
      return
      end

      SUBROUTINE get_fragmPFNS_LANL (fragmPFNS, emiss_en, nen_emis,
     &      eincid, af, zf, emed, tequiv, qval, deltae,
     &      PFNtke, PFNrat, PFNalp)
C
C     See D. Madland original paper (NSE) on LA model
C
      implicit none
C     Dummy parameters
      integer nen_emis
      real*8 fragmPFNS(nen_emis), emiss_en(nen_emis)
      real*8 eincid, af, zf, emed, tequiv, qval, deltae
      real*8 PFNtke, PFNrat, PFNalp

C     Local variables
      real*8 ftmp1, ftmp2, eplus, emin 
      real*8 CNdef,HFdef,LFdef,ftmp, Erel,r,e,Efkin,Tlf,Thf
	real*8 Ux,EniuL,EniuH,Tm
      real*8 fmed, abserr
      integer iah,ial,iaf,izf,izh,izl,i
C     real*8 fpost, fnscn, tscn, wscn

      COMMON /eparam/eplus,emin

C     Mass of heavy fragment fixed following Malinovskii
      data iah/140/ 
C     Ratio of the light to heavy fragments' Temperature taken from Kornilov
C     data r/1.248d0/  !Kornilov value
      data r/1.000d0/  !Original LANL value
C     Scission neutrons for Th-232 following Lovchikova et al (Phys.At.Nuclei 67 (2004) p890)
C     data wscn/0.10d0/ ! "wscn,tscn" fitted to get scission neutron temperature (0.38 +/- 0.04)
C     data tscn/0.40d0/ !

      real*8 GAUSS_INT1, SL_LAB, SH_LAB, TKE, BIND
      external SL_LAB, SH_LAB

      emed = 0.d0
      tequiv = 0.d0
      fragmPFNS = 0.d0

      iaf = nint(af)
      izf = nint(zf)

      ftmp = bind(iaf-izf,izf,CNdef) 

C     Parametrization of the Total Kinetic Energy in fission
      Efkin = TKE(izf,iaf,eincid) * PFNtke   ! PFNtke is the scaling factor
      
C     Malinovskii parametrization of the heavy fragment charge
      izh = (zf/af)*iah - 0.5
      izl = izf - izh
      ial = iaf - iah

      ftmp = bind(iah-izh,izh,HFdef)
      ftmp = bind(ial-izl,izl,LFdef)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel = CNdef - HFdef - LFdef

C     if scission neutrons are considered 
C     if(wscn.ne.0.d0) alpha0 = 1.d0
C
      EniuL =  float(iah)/float(ial*iaf)*Efkin * PFNalp ! reduction of TKE for fragments
      EniuH =  float(ial)/float(iah*iaf)*Efkin * PFNalp

      Ux = Erel - Efkin + eincid + qval 

      IF(UX.lt.0) return
C
C     Level density assumed porportional to A/11 
      Tm = dsqrt(Ux/(iaf/11.d0))   
C
C     LA model (eq.11 CPC)
      Tlf = Tm * r * PFNrat ! r = Tlf/Thf ~ expected around 1.2 (original LA model value =1) 
      Thf = Tm

      ftmp = 0.D0
      do i =1,nen_emis
        e = emiss_en(i)

        if (e.lt.0.00001d0) cycle

        eplus = (DSQRT(e) + DSQRT(EniuL))**2
        emin  = (DSQRT(e) - DSQRT(EniuL))**2
        ftmp1=
     &  GAUSS_INT1(SL_LAB,0.d0,Tlf,abserr)/(2*Tlf*Tlf*DSQRT(EniuL))

        eplus = (DSQRT(e) + DSQRT(EniuH))**2
        emin  = (DSQRT(e) - DSQRT(EniuH))**2
        ftmp2=
     &  GAUSS_INT1(SH_LAB,0.d0,Thf,abserr)/(2*Thf*Thf*DSQRT(EniuH))

        fragmPFNS(i) = 0.5d0*(ftmp1 + ftmp2)

C       fpost = 0.5d0*( fwatt(e,EniuH,Thf) + fwatt(e,EniuL,Tlf) )
C       fnscn = fscn(e,tscn)
C       fragmPFNS(i) = wscn*fnscn + (1.d0 - wscn)*fpost

        IF(i.gt.1) then
          fmed = (fragmPFNS(i)+fragmPFNS(i-1))*0.5
          emed = emed + fmed*deltae*(emiss_en(i)+emiss_en(i-1))*0.5d0
          ftmp = ftmp + fmed*deltae
        ENDIF
      enddo

      if(ftmp.GT.0) emed = emed/ftmp
      tequiv = 2.D0/3.D0*emed

      return
      end

      real*8 FUNCTION TKE(izf,iaf,einc)
C
C     Malinovskii et al, INDC(CCP)-277, IAEA 1987 (VANT N2,1987) in russian
C     Itkis et al, Yad.Fiz.52(1990) 23; Fiz.Elem.Ch.At. Yadra 29(1998) 389
C
      integer izf,iaf
      real*8 ftmp, einc
      real*8 en
 
      en = einc
      if(en.lt.0.d0) en = 0.d0
 
      if(izf.eq.92) then  ! Uranium
        TKE = 169.24
        IF(en.lt.0.0001d0) THEN
          TKE = TKE + 0.4*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For U-238 Bn=6.15
        ELSEIF(en.gt.0.0001d0 .and. en.le.1.65) THEN
          TKE = TKE + (-0.423 + 0.044*(iaf-230))*en
        ELSE
          TKE = TKE + ( 0.144 - 0.047*(iaf-230))*en
        ENDIF
        return
      endif

      if(izf.eq.93) then  ! Neptunium
        TKE = 172.43
        IF(en.le.0.0001d0) THEN
          TKE = TKE + 0.25*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For Np-237 Bn=6.57
        ELSEIF(en.gt.0.0001d0 .and. en.le.1.23) THEN
          TKE = TKE - 0.14*en
        ELSE 
          TKE = TKE - 0.22*en
        ENDIF
        return
      endif

      if(izf.eq.94) then  ! Plutonium
        TKE = 176.4
        IF(en.le.0.0001d0) THEN
          TKE = TKE + 0.06*en
C       Exact energy should be 7.80 Mev - Bn(IAF,ZAF), For Pu-239 Bn=5.65
        ELSEIF(en.gt.0.0001d0 .and. en.le.2.15) THEN
          TKE = TKE - 0.12*en
	  ELSE
          TKE = TKE - 0.21*en
        ENDIF
        return
      endif

      if(izf.eq.95) then  ! Americium
        TKE = 179.74
        IF(en.le.0.0001d0) THEN
          TKE = TKE + 0.08*en
        ELSE
          TKE = TKE -0.25*en
        ENDIF
        return
      endif

      if(izf.eq.96) then  ! Curium
        TKE = 183.23 - 0.338*(iaf-240)
        if(iaf.le.242) TKE = 183.23 - 0.338*2
        IF(en.le.0.0001d0) THEN
          TKE = TKE + 0.07*en
        ELSE
          TKE = TKE + 0.11*en
        ENDIF
        return
      endif

      if(izf.eq.97) then  ! Bekerelium
        TKE = 185.08 - 0.405*(iaf-249)
        TKE = TKE + 0.1*en
        return
      endif

      if(izf.eq.98) then  ! Californium
        TKE = 193.79 - 0.472*(iaf-240)
        TKE = TKE + 0.1*en
        return
      endif
C-------------------------------------------------------------------------
C
C     Itkis et al, Yad.Fiz.52(1990) 23; Fiz.Elem.Ch.At. Yadra 29(1998) 389
C
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
C       TKE = 0.104*ftmp + 24.05                 ! for delta <& 0 and beta2 = 0
C       TKE = 0.104*ftmp + 23.7                  ! for delta <& 0 and beta2 <>0 
C       TKE = 0.104*ftmp + 25.1                  ! for delta = 0 and S2n = 0
C       TKE = 0.104*ftmp + 24.85                 ! for delta = 0 and S2n <& 0
C       TKE = 0.112*ftmp + 13.7                  ! Vladuca
C       TKE = 0.104*ftmp + 23.95                 ! for delta <& 0 and S2n <& 0

C       TKE = 0.104*ftmp + 23.7                  ! for delta = 0 and S2n = 0
C       TKE = TKE + 0.37*en                      ! eval

C       TKE = 0.104*ftmp + 25.1                 
C       TKE = 0.104*ftmp + 25.6                 
C       TKE = TKE + 0.15*en                     

        TKE = 0.104*ftmp + 25.75                 ! for delta = 0 and S2n <& 0
        TKE = TKE + 0.50*en                      ! eval     
        return
      endif

      if(izf.eq.91) then  ! Protoactinium
        TKE = 166.7 + 0.016d0*(ftmp-1332.d0) 
C       TKE = 166.1 + 0.021d0*(ftmp-1332.d0)
        TKE = TKE + 0.37*en
        return
      endif
C
C     Itkis
      if(izf.eq.92) then  ! Uranium
        TKE = 0.104*ftmp + 27.35
        TKE = TKE + 0.4*en
        return
      endif

      return
      end

      real*8 FUNCTION fwatt(E,Eniu,T)
      real*8 E,T,pi,Eniu,b
      data pi/3.1415926d0/
      fwatt = 0.d0
      if(E.le.0.1d-5) E=0.1d-5
      if(T.le.0.1d-5) return
      if((E+Eniu).GT.50.d0*T) return
      b = 4*Eniu/T/T
      fwatt = 2.d0/T*DSQRT(E/(pi*T))*DEXP(-(E+Eniu)/T)*
     &             sinh(DSQRT(b*E))/DSQRT(b*E)
      return
      end

      real*8 FUNCTION fmaxw(E,T)
      real*8 E,T,pi
      data pi/3.1415926d0/
      fmaxw = 0.d0
      if(E.le.0.1d-5) E=0.1d-5
      if(T.le.0.1d-5) return
      if(E.GT.50.d0*T) return
      fmaxw = 2.d0/T*DSQRT(E/(pi*T))*DEXP(-E/T)
      return
      end

      real*8 FUNCTION fscn(E,T)
      real*8 E,T
      fscn = 0.d0
      if(E.le.0.1d-5) E=0.1d-5
      if(T.le.0.1d-5) return
      if(E.GT.50.d0*T) return
      fscn = 1.d0/T*(E/T)*DEXP(-E/T)
      return
      end

      real*8 FUNCTION bind(nn,nz,exc)
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

C     Rounded values
C
C mn    neutron mass  1.008 665 amu 
C me    electron mass 5.485 799�10-4 amu 
C mp    proton mass   1.007 276 amu 
C md    deuteron mass 2.013 553 amu 1
C mt    triton mass   3.015 501 amu 3
C m3He  3He mass      3.014 932 amu 1
C ma    4He mass      4.001 506 amu 1


      exc = EXCessmass(nz,nz+nn)
C     bind = dble(nz*7.28903+nn*8.07138 - exc)
C     Corrected values
     
      bind = dble(nz*(AMUpro+AMUele) + nn*AMUneu - exc)
C     print *, ' N=',nn,' Z=',nz,' B.E.=',e,' MassExcess=', exc
      return
      end
      
      SUBROUTINE MASS10(nx,nz,E)     ! Duflo-Zuker fevrier 1996
c Calculation of binding energy E (nx neutrons,nz protons)
      REAL*8 E
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

      REAL*8 FUNCTION GAUSS_LAGUERRE_INT(F,Abserr)
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 Abserr
      REAL*8 F
C
C Local variables
C
      REAL*8 resk10,resk08 
      REAL*8 wg10(10), xg10(10), wg08(10), xg08(10)
      INTEGER j
      EXTERNAL F
C
C     THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (0,INF).
C
C     GAUSS LAGUERRE values for n=10 POINTS 
C
C1     0.13779347054     0.308441115765    
C2     0.729454549503    0.401119929155    
C3     1.80834290174     0.218068287612    
C4     3.40143369785     0.0620874560987   
C5     5.55249614006     0.00950151697517  
C6     8.33015274676     0.000753008388588 
C7     11.8437858379     2.82592334963E-005
C8     16.2792578314     4.24931398502E-007
C9     21.996585812      1.83956482398E-009
C10    29.9206970123     9.91182721958E-013
C
      DATA wg10(1) /0.308441115765D0/
      DATA wg10(2) /0.401119929155D0/
      DATA wg10(3) /0.218068287612D0/
      DATA wg10(4) /0.0620874560987D0/
      DATA wg10(5) /0.00950151697517D0/
      DATA wg10(6) /0.000753008388588D0/
      DATA wg10(7) /2.82592334963D-05/
      DATA wg10(8) /4.24931398502D-07/
      DATA wg10(9) /1.83956482398D-009/
      DATA wg10(10)/9.91182721958D-013/

      DATA xg10(1) /0.13779347054D0/
      DATA xg10(2) /0.729454549503D0/
      DATA xg10(3) /1.80834290174D0/
      DATA xg10(4) /3.40143369785D0/
      DATA xg10(5) /5.55249614006D0/
      DATA xg10(6) /8.33015274676D0/
      DATA xg10(7) /11.8437858379D0/
      DATA xg10(8) /16.2792578314D0/
      DATA xg10(9) /21.996585812D0/
      DATA xg10(10)/29.9206970123D0/
C
C     GAUSS LAGUERRE values for n=8 POINTS 
C
C1    0.170279632305    0.369188589342    
C2    0.903701776799    0.418786780814    
C3    2.25108662987     0.175794986637    
C4    4.26670017029     0.0333434922612   
C5    7.04590540239     0.00279453623523  
C6    10.7585160102     9.07650877338E-005
C7    15.7406786413     8.48574671626E-007
C8    22.8631317369     1.04800117487E-009
C
      DATA wg08(1)/0.369188589342D0/
      DATA wg08(2)/0.418786780814D0/
      DATA wg08(3)/0.175794986637D0/
      DATA wg08(4)/0.0333434922612D0/
      DATA wg08(5)/0.00279453623523D0/
      DATA wg08(6)/9.07650877338D-05/
      DATA wg08(7)/8.48574671626D-07/
      DATA wg08(8)/1.04800117487D-09/
C
      DATA xg08(1)/0.170279632305D0/
      DATA xg08(2)/0.903701776799D0/
      DATA xg08(3)/2.25108662987D0/
      DATA xg08(4)/4.26670017029D0/
      DATA xg08(5)/7.04590540239D0/
      DATA xg08(6)/10.7585160102D0/
      DATA xg08(7)/15.7406786413D0/
      DATA xg08(8)/22.8631317369D0/
C
C
      resk08 = 0.d0  
      DO j = 1, 8
        resk08 = resk08 + wg08(j)*F(xg08(j))
      ENDDO      

      resk10 = 0.d0  
      DO j = 1, 10
        resk10 = resk10 + wg10(j)*F(xg10(j))
      ENDDO      

      GAUSS_LAGUERRE_INT = resk10
      Abserr = ABS(resk10 - resk08)

      RETURN
      END

      REAL*8 FUNCTION SL_LAB(T)
      IMPLICIT NONE
      REAL*8 T, TT, eplus, emin
      COMMON /tparam/TT
      COMMON /eparam/eplus,emin
      REAL*8 ftmp1, ftmp2, abserr
      REAL*8 SKLT, FKLT, GAUSS_LAGUERRE_INT, GAUSS_INT1
      EXTERNAL SKLT, FKLT
      TT = T
      
      SL_LAB = 0.d0
      ftmp1 = GAUSS_LAGUERRE_INT(FKLT,abserr) ! eq.7 for normalization
      if(ftmp1.eq.0.d0) return

      ftmp2 = GAUSS_INT1(SKLT,emin,eplus,abserr) 

      SL_LAB = T * ftmp2 / ftmp1

      RETURN
      END

      REAL*8 FUNCTION SH_LAB(T)
      IMPLICIT NONE
      REAL*8 T, TT, eplus, emin
      COMMON /tparam/TT
      COMMON /eparam/eplus,emin
      REAL*8 ftmp1, ftmp2, abserr
      REAL*8 SKHT, FKHT, GAUSS_LAGUERRE_INT, GAUSS_INT1
      EXTERNAL SKHT, FKHT
      TT = T
      
      SH_LAB = 0.d0
      ftmp1 =     GAUSS_LAGUERRE_INT(FKHT,abserr) ! eq.7 for normalization
      if(ftmp1.eq.0.d0) return

      ftmp2 = GAUSS_INT1(SKHT,emin,eplus,abserr) 
C     write(*,*) sngl(ftmp2),sngl(abserr/ftmp2)
      SH_LAB = T * ftmp2 / ftmp1

      RETURN
      END

      REAL*8 FUNCTION FEPSL(T)
      IMPLICIT NONE
      REAL*8 T, TT
      COMMON /tparam/TT
      REAL*8 ftmp1, ftmp2, abserr
      REAL*8 EKLT, FKLT, GAUSS_LAGUERRE_INT
      EXTERNAL EKLT, FKLT
      TT = T
      
      FEPSL = 0.d0
      ftmp1 =     GAUSS_LAGUERRE_INT(FKLT,abserr) ! eq.7 for normalization
      if(ftmp1.eq.0.d0) return

      ftmp2 = GAUSS_LAGUERRE_INT(EKLT,abserr) ! eq.11 for light, integrand 
      FEPSL = ftmp2 / ftmp1

      RETURN
      END

      REAL*8 FUNCTION FEPSH(T)
      IMPLICIT NONE
      REAL*8 T, TT
      COMMON /tparam/TT
      REAL*8 ftmp1, ftmp2, abserr
      REAL*8 EKHT, FKHT, GAUSS_LAGUERRE_INT
      EXTERNAL EKHT, FKHT
      TT = T
      
      FEPSH = 0.d0
      ftmp1 =     GAUSS_LAGUERRE_INT(FKHT,abserr) ! eq.7 for normalization
      if(ftmp1.eq.0.d0) return

      ftmp2 = GAUSS_LAGUERRE_INT(EKHT,abserr) ! eq.11 for light, integrand 
      FEPSH = ftmp2 / ftmp1

      RETURN
      END

      REAL*8 FUNCTION FKLT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, T
      COMMON /tparam/T
      xx = T*x
      CALL INTERPOL1(1,xx,sigc)
      FKLT = sigc*x*T*T
      RETURN
      END

      REAL*8 FUNCTION FKHT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, T
      COMMON /tparam/T
      xx = T*x
      CALL INTERPOL1(2,xx,sigc)
      FKHT = sigc*x*T*T
      RETURN
      END

      REAL*8 FUNCTION EKLT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, T
      COMMON /tparam/T
      xx = T*x
      CALL INTERPOL1(1,xx,sigc)
      EKLT = sigc*x*x*T**4
      RETURN
      END

      REAL*8 FUNCTION EKHT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, T
      COMMON /tparam/T
      xx = T*x
      CALL INTERPOL1(2,xx,sigc)
      EKHT = sigc*x*x*T**4
      RETURN
      END

      REAL*8 FUNCTION SKLT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, T
      COMMON /tparam/T
      SKLT = 0.d0
      xx = x/T
      if(xx.GT.30.d0) return
      CALL INTERPOL1(1,x,sigc)
      SKLT = DEXP(-xx)*sigc*DSQRT(x)
      RETURN
      END

      REAL*8 FUNCTION SKHT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, T
      COMMON /tparam/T
      SKHT = 0.d0
      xx = x/T
      if(xx.GT.30.d0) return
      CALL INTERPOL1(2,x,sigc)
      SKHT = DEXP(-xx)*sigc*DSQRT(x)
      RETURN
      END

C     REAL*8 FUNCTION FFKT(x)
C     REAL*8 xx, x, sigc
C     COMMON /tparam/T
C     xx = T*x
C     CALL INTERPOL1(3,xx,sigc)
C     FFKT = sigc*xx*tres
C     RETURN
C     END


c====================================================================
      SUBROUTINE INTERPOL1(iopt,x,sigc)
c=========================================
	INTEGER iopt
      DOUBLE PRECISION x,sigc

      DOUBLE PRECISION eel(500),sigl(500)
      DOUBLE PRECISION eeh(500),sigh(500)  
      DOUBLE PRECISION eenc(500),signcf(500)      
      INTEGER nrl,nrh,nrnc

      COMMON /siglh/ nrl,nrh,eel,sigl,eeh,sigh
      COMMON /inp_sp5/ eenc,signcf,nrnc

      DOUBLE PRECISION ee(500),ss(500)
      INTEGER nrc
c
      if(iopt.eq.1) then
         nrc=nrl
         do i=1,nrc
            ee(i)=eel(i)
            ss(i)=sigl(i)
         end do
      end if
c
      if(iopt.eq.2) then
         nrc=nrh
         do i=1,nrc
            ee(i)=eeh(i)
            ss(i)=sigh(i)
         end do
      end if
c
      if(iopt.eq.3.or.iopt.eq.4) then
         nrc=nrnc
         do i=1,nrc
            ee(i)=eenc(i)
            ss(i)=signcf(i)
         end do
      end if

      if(x.lt.ee(1) ) then
        sigc=ss(1)
        return
      endif   

      if(x.gt.ee(nrc)) then
        sigc=ss(nrc)
        return
      endif   
c
c     interpolation lineaire  
c
      do j=1,nrc-1
         if(x.ge.ee(j)) then
            if(x.le.ee(j+1)) then
               aa=(ss(j+1)-ss(j))/(ee(j+1)-ee(j))
               b=ss(j)-aa*ee(j)
               sigc=aa*x+b
               return
            end if
         end if
      end do
      return
      end

c=========================================
      SUBROUTINE INPUT_SPEC
c=========================================
      DOUBLE PRECISION eell(500),sigll(500)
      DOUBLE PRECISION eehh(500),sighh(500)  
      DOUBLE PRECISION eenc(500),signcf(500)      
      INTEGER nrll,nrhh,nrnc,i

      COMMON /inp_sp5/ eenc,signcf,nrnc
      COMMON /siglh/ nrll,nrhh,eell,sigll,eehh,sighh

      CHARACTER*64 EMPiredir
      CHARACTER*72 EMPtitle
      COMMON /GLOBAL_E/ EMPiredir, EMPtitle
   
      open(unit=23,file=trim(EMPiredir)//'/data/CNxs.dat',
     >  status='old',ERR=100)
           read(23,*) nrnc
           do i=1,nrnc
              read(23,*)eenc(i),signcf(i)
c              write(*,*)eenc(i),signcf(i)
           enddo
      close (23)

      open(unit=23,file=trim(EMPiredir)//'/data/LFxs.dat',
     >  status='old',ERR=200)
           read(23,*) nrll
           do i=1,nrll
              read(23,*)eell(i),sigll(i)
c              write(*,*)eell(i),sigll(i)
           enddo
      close (23)
      open(unit=23,file=trim(EMPiredir)//'/data/HFxs.dat',
     >  status='old',ERR=300)
           read(23,*) nrhh
           do i=1,nrhh
              read(23,*)eehh(i),sighh(i)
c              write(*,*)eehh(i),sighh(i)
           enddo
      close (23)
      return
 100  write(8,*) 'ERROR: ../data/CNxs.data not found !'
      STOP 'ERROR: ../data/CNxs.data not found !'
 200  write(8,*) 'ERROR: ../data/LFxs.data not found !'
      STOP 'ERROR: ../data/LFxs.data not found !'
 300  write(8,*) 'ERROR: ../data/HFxs.data not found !'
      STOP 'ERROR: ../data/HFxs.data not found !'
      return
      end


      REAL*8 FUNCTION GAUSS_INT1(F,Ea,Eb,Abserr)
      IMPLICIT NONE
C
C
C Dummy arguments
C
      REAL*8 Abserr
      REAL*8 Ea, Eb
      REAL*8 F
C
C Local variables
C
      REAL*8 absc, abscm1, fsum, fval1, fval1m1, fval2, fval2m1, resk1
      REAL*8 centr1, hlgth1, resg1, wg(10), wgk(21), xgk(21)
      INTEGER j, jtw, jtwm1
C     EXTERNAL F
C
C
C
C     THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C     BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C     CORRESPONDING WEIGHTS ARE GIVEN.
C
C     XG - ABSCISSAE OF THE 41-POINT GAUSS-KRONROD RULE
C     WG - WEIGHTS OF THE 20-POINT GAUSS RULE
C
C GAUSS QUADRATURE WEIGHTS AND KRONROD QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA wg(1)/0.017614007139152118311861962351853D0/
      DATA wg(2)/0.040601429800386941331039952274932D0/
      DATA wg(3)/0.062672048334109063569506535187042D0/
      DATA wg(4)/0.083276741576704748724758143222046D0/
      DATA wg(5)/0.101930119817240435036750135480350D0/
      DATA wg(6)/0.118194531961518417312377377711382D0/
      DATA wg(7)/0.131688638449176626898494499748163D0/
      DATA wg(8)/0.142096109318382051329298325067165D0/
      DATA wg(9)/0.149172986472603746787828737001969D0/
      DATA wg(10)/0.152753387130725850698084331955098D0/
C
      DATA xgk(1)/0.998859031588277663838315576545863D0/
      DATA xgk(2)/0.993128599185094924786122388471320D0/
      DATA xgk(3)/0.981507877450250259193342994720217D0/
      DATA xgk(4)/0.963971927277913791267666131197277D0/
      DATA xgk(5)/0.940822633831754753519982722212443D0/
      DATA xgk(6)/0.912234428251325905867752441203298D0/
      DATA xgk(7)/0.878276811252281976077442995113078D0/
      DATA xgk(8)/0.839116971822218823394529061701521D0/
      DATA xgk(9)/0.795041428837551198350638833272788D0/
      DATA xgk(10)/0.746331906460150792614305070355642D0/
      DATA xgk(11)/0.693237656334751384805490711845932D0/
      DATA xgk(12)/0.636053680726515025452836696226286D0/
      DATA xgk(13)/0.575140446819710315342946036586425D0/
      DATA xgk(14)/0.510867001950827098004364050955251D0/
      DATA xgk(15)/0.443593175238725103199992213492640D0/
      DATA xgk(16)/0.373706088715419560672548177024927D0/
      DATA xgk(17)/0.301627868114913004320555356858592D0/
      DATA xgk(18)/0.227785851141645078080496195368575D0/
      DATA xgk(19)/0.152605465240922675505220241022678D0/
      DATA xgk(20)/0.076526521133497333754640409398838D0/
      DATA xgk(21)/0.000000000000000000000000000000000D0/
C
      DATA wgk(1)/0.003073583718520531501218293246031D0/
      DATA wgk(2)/0.008600269855642942198661787950102D0/
      DATA wgk(3)/0.014626169256971252983787960308868D0/
      DATA wgk(4)/0.020388373461266523598010231432755D0/
      DATA wgk(5)/0.025882133604951158834505067096153D0/
      DATA wgk(6)/0.031287306777032798958543119323801D0/
      DATA wgk(7)/0.036600169758200798030557240707211D0/
      DATA wgk(8)/0.041668873327973686263788305936895D0/
      DATA wgk(9)/0.046434821867497674720231880926108D0/
      DATA wgk(10)/0.050944573923728691932707670050345D0/
      DATA wgk(11)/0.055195105348285994744832372419777D0/
      DATA wgk(12)/0.059111400880639572374967220648594D0/
      DATA wgk(13)/0.062653237554781168025870122174255D0/
      DATA wgk(14)/0.065834597133618422111563556969398D0/
      DATA wgk(15)/0.068648672928521619345623411885368D0/
      DATA wgk(16)/0.071054423553444068305790361723210D0/
      DATA wgk(17)/0.073030690332786667495189417658913D0/
      DATA wgk(18)/0.074582875400499188986581418362488D0/
      DATA wgk(19)/0.075704497684556674659542775376617D0/
      DATA wgk(20)/0.076377867672080736705502835038061D0/
      DATA wgk(21)/0.076600711917999656445049901530102D0/
C
C     Integrating from Ea to Eint
      centr1 = 0.5D+00*(Ea + Eb)
      hlgth1 = 0.5D+00*(Eb - Ea)
C
C     COMPUTE THE 41-POINT GAUSS-KRONROD APPROXIMATION TO
C     THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      resg1 = 0.0D+00
      resk1 = wgk(21)*F(centr1)
      DO j = 1, 10
         jtw = j*2
         jtwm1 = jtw - 1
         absc = hlgth1*xgk(jtw)
         fval1 = F(centr1 - absc)
         fval2 = F(centr1 + absc)
         fsum = fval1 + fval2
         abscm1 = hlgth1*xgk(jtwm1)
         fval1m1 = F(centr1 - abscm1)
         fval2m1 = F(centr1 + abscm1)
         resg1 = resg1 + wg(j)*fsum
         resk1 = resk1 + wgk(jtw)*fsum + wgk(jtwm1)*(fval1m1 + fval2m1)
      ENDDO
      GAUSS_INT1 = resk1*hlgth1
      Abserr = ABS((resk1 - resg1)*hlgth1)
C     RETURN
      END

C*******************************************************************
C     NOT USED FOR THE TIME BEING
C     These functions could calculate NUBAR, they need to be tested
C
C     calculation of bn for all nuclei has to be imporved as done in GET_PFNS_...
C 
      DOUBLE PRECISION FUNCTION fniuLANL(en,iaf,izf)
C
C     See D. Madland original paper (NSE) on LA model
C
C     Following Malinovskii prescription for nubar calculation
C

      real*8 CNdef,HFdef,LFdef,ftmp,Erel
      real*8 en, eg, eg0, egn, bn, uexcit, ftmp1, ftmp2
      real*8 TmL, TmH, r, Ux
      real*8 Tm,abserr
      real*8 Ekin_ave,Bnl,Bnh,Sn,eps,fniuA,fniuB
      real*8 Sn1, S2n1, Erel1
      real*8 Sn2, S2n2, Erel2
      real*8 Sn3, S2n3, Erel3
      real*8 Sn4, S2n4, Erel4
      real*8 Sn5, S2n5, Erel5
      integer iaf,izf,ial,izl,iah,iah0,izh0,izh
      real*8 p0,p1,p2,pnorm

      real*8 TKE,GAUSS_INT1,FEPSL,FEPSH,bind
      external FEPSL,FEPSH

C     Mass of heavy fragment fixed following Malinovskii
      data iah/140/
C     Average gamma energy release Eg = eg0 + niu*egn
      data eg0/4.42d0/,egn/0.99d0/
C     Ratio of the light to heavy fragments' Temperature taken from Kornilov
      data r/1.248d0/
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

      fniuLANL = 0.d0 

C     Following Vladuca
C     ftmp = real(izf)**2/real(iaf)
C     egn = 6.71 - 0.156*ftmp ! p
C     eg0 = 0.75 + 0.088*ftmp ! q

      bn = bind(iaf-izf,izf,CNdef)  ! get mass excess CNdef for fiss.nucleus
     &   - bind(iaf-izf-1,izf,ftmp) ! get Bn of the neutron in fiss. nucleus
     
      uexcit = en + abs(bn)
     
      Ekin_ave = TKE(izf, iaf, en)      
      
C----------------------------------
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
C     Fifth fragment Zh + 2 (to average selecting either (Zh + 2) or (Zh - 2)
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
C
C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n4*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)      
      Sn = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn4*p2)/pnorm

      Erel = (Erel1*p0 + (Erel2 + Erel3)*p1 + Erel4*p2)/pnorm

      Ux = Erel + uexcit - Ekin_ave
      IF(UX.lt.0) return
C
C     Level density assumed porportional to A/11 
      Tm = dsqrt(Ux/(iaf/11.d0))   
C
C     LA model (eq.11 CPC)
      TmL = Tm*r
      TmH = Tm

      ftmp1 = GAUSS_INT1(FEPSL,0.d0,TmL,abserr)/(TmL*TmL)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)

      ftmp2 = GAUSS_INT1(FEPSH,0.d0,TmH,abserr)/(TmH*TmH)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)

C     The two lines below correspond to LA model with constant reaction cross section 
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/(iaf/11.d0)) = 4/3*Tm
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/acc) 
      eps = ftmp1 + ftmp2

C     Prompt gamma emission
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9

      fniuA = (Ux - eg0)/(eps + Sn + egn)
      eg = eg0 + fniuA * egn

      write(*,'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,
     &          '' Ex='',f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')
     &          en,Ux,Erel,uexcit,ekin_ave,bn

      write(*,'('' Eg='',f6.3,'' Enf='',f6.3,
     &          '' Sn='',f6.3,'' nuA='',f6.3,'' LANL'')')
     &          eg0,eps,Sn,fniuA

C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n5*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)      
      Sn = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm

      Erel = (Erel1*p0 + (Erel2 + Erel3)*p1 + Erel5*p2)/pnorm

      Ux = Erel + uexcit - Ekin_ave
      IF(UX.lt.0) return
C
C     Level density assumed porportional to A/11 
      Tm = dsqrt(Ux/(iaf/11.d0))   
C
C     LA model (eq.11 CPC)
      TmL = Tm*r
      TmH = Tm

      ftmp1 = GAUSS_INT1(FEPSL,0.d0,TmL,abserr)/(TmL*TmL)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)

      ftmp2 = GAUSS_INT1(FEPSH,0.d0,TmH,abserr)/(TmH*TmH)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)

C     The two lines below correspond to LA model with constant reaction cross section 
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/(iaf/11.d0)) = 4/3*Tm
C     eps   = 4./3.*dsqrt((Erel + uexcit - Ekin_ave)/acc) 
      eps = ftmp1 + ftmp2

      fniuB = (Ux - eg0)/(eps + Sn + egn)

      eg = eg0 + fniuB * egn

      write(*,'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,
     &          '' Ex='',f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')
     &          en,Ux,Erel,uexcit,ekin_ave,bn

      write(*,'('' Eg='',f6.3,'' Enf='',f6.3,
     &          '' Sn='',f6.3,'' nuB='',f6.3,'' LANL'')')
     &          eg0,eps,Sn,fniuB

      fniuLANL = 0.5 * (fniuA + fniuB)

      return
      end
C
      DOUBLE PRECISION FUNCTION fniu(en,iaf,izf)
C
C     See N.V.Kornilov, A.B.Kagalenko and F.-J.Hambsch
C     Phys. At. Nuclei 62 (1999) pp 173-185
C
C     Following Malinovskii prescription for nubar calculation
C
      implicit none
      real*8 en, eg, eg0, egn, bn, uexcit, ftmp, ftmp1, ftmp2
      real*8 TmL, TmH, Ux
      real*8 TlCF, ThCF, r, U0CF, coeff, abserr
      real*8 Ekin_ave,Bnl,Bnh,Sn,eps,fniuA,fniuB
      real*8 Sn1,Sn2,Sn3,Sn4,Sn5 
      real*8 CNdef,HFdef,LFdef
      real*8 Erel,Erel1,Erel2,Erel3,Erel4,Erel5
C     real*8 S2n1,S2n2,S2n3,S2n4,S2n5 
      integer iaf,izf,ial,izl,iah,iah0,izh0,izh
      real*8 p0,p1,p2,pnorm
C
      real*8 TKE,GAUSS_INT1,FEPSL,FEPSH,bind
      external FEPSL,FEPSH

C     Mass of heavy fragment fixed following Malinovskii
      data iah/140/
C     Average gamma energy release Eg = eg0 + niu*egn
C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9
      data eg0/4.42d0/,egn/0.99d0/
C     Kornilov parameterization
      data ThCF/0.8868d0/,r/1.248d0/,U0CF/32.9d0/
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
C     ftmp = real(izf)**2/real(iaf)
C     egn = 6.71 - 0.156*ftmp ! p
C     eg0 = 0.75 + 0.088*ftmp ! q

      fniu = 0.d0

      bn = bind(iaf-izf,izf,CNdef)  ! get mass excess CNdef for fiss.nucleus
     &   - bind(iaf-izf-1,izf,ftmp) ! get Bn of the neutron in fiss. nucleus

      uexcit = en + bn
     
      Ekin_ave = TKE(izf, iaf, en)      
      
C----------------------------------
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
C     S2n1 = 0.5*(Bnh+Bnl)
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
C     S2n2 = 0.5*(Bnh+Bnl)
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
C     S2n3 = 0.5*(Bnh+Bnl)
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
C     S2n4 = 0.5*(Bnh+Bnl)
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
C     S2n5 = 0.5*(Bnh+Bnl)
C     Total energy release in fission from mass excess for CN, heavy and light fragments
      Erel5 = CNdef - HFdef - LFdef

C===========================================================================

C     FINAL AVERAGE
C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn4*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n4*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)

      Sn = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn4*p2)/pnorm

      Erel = (Erel1*p0 + (Erel2 + Erel3)*p1 + Erel4*p2)/pnorm

      Ux = Erel + uexcit - Ekin_ave
      IF(UX.lt.0) return

C     Kornilov is based on Cf-252 PFNS fitting
      coeff = DSQRT( 252.d0/iaf * Ux/U0CF)
C     Following formulae (7) of the paper
      TlCF = ThCF * r
      TmL = TlCF * coeff
      TmH = ThCF * coeff

      ftmp1 = GAUSS_INT1(FEPSL,0.d0,TmL,abserr)/(TmL*TmL)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)

      ftmp2 = GAUSS_INT1(FEPSH,0.d0,TmH,abserr)/(TmH*TmH)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)

      eps = ftmp1 + ftmp2

C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9

      fniuA = (Ux - eg0)/(eps + Sn + egn)

      eg = eg0 + fniuA * egn

      write(*,'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,
     &          '' Ex='',f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')
     &          en,Ux,Erel,uexcit,ekin_ave,bn

      write(*,'('' Eg='',f6.3,'' Enf='',f6.3,
     &          '' Sn='',f6.3,'' nuB='',f6.3,'' LANL'')')
     &          eg,eps,abs(Sn),fniuA

C     S1n = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm
C     S2n = (S2n1*p0 + (S2n2 + S2n3)*p1 + S2n5*p2)/pnorm
C     Sn = 0.5*( S1n + 0.5*S2n)      
      Sn = (Sn1*p0 + (Sn2 + Sn3)*p1 + Sn5*p2)/pnorm

      Erel = (Erel1*p0 + (Erel2 + Erel3)*p1 + Erel5*p2)/pnorm

      Ux = Erel + uexcit - Ekin_ave
      IF(UX.lt.0) return

C     Kornilov is based on Cf-252 PFNS fitting
      coeff = DSQRT( 252.d0/iaf * Ux/U0CF)
C     Following formulae (7) of the paper
      TlCF = ThCF * r
      TmL = TlCF * coeff
      TmH = ThCF * coeff

      ftmp1 = GAUSS_INT1(FEPSL,0.d0,TmL,abserr)/(TmL*TmL)
C     write(*,*) sngl(TmL), sngl(abserr/ftmp1), sngl(ftmp1)

      ftmp2 = GAUSS_INT1(FEPSH,0.d0,TmH,abserr)/(TmH*TmH)
C     write(*,*) sngl(TmH), sngl(abserr/ftmp2), sngl(ftmp2)

      eps = ftmp1 + ftmp2

C     Following Kornilov (instead of constant 4.42 according to Malinovskii)
C     eg0 = Sn*0.9

      fniuB = (Ux - eg0)/(eps + Sn + egn)

      eg = eg0 + fniuB * egn

      write(*,'('' Einc='',f6.3,'' Ux='',f6.3,'' Erel='',f5.1,
     &          '' Ex='',f6.3,'' TKE='',f5.1,'' Bn='',f5.1)')
     &          en,Ux,Erel,uexcit,ekin_ave,bn

      write(*,'('' Eg='',f6.3,'' Enf='',f6.3,
     &          '' Sn='',f6.3,'' nuB='',f6.3,'' LANL'')')
     &          eg,eps,abs(Sn),fniuB

      fniu = 0.5 * (fniuA + fniuB)

      return
      end


