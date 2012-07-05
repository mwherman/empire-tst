Ccc   * $Rev: 2909 $
Ccc   * $Author: mherman $
Ccc   * $Date: 2012-07-05 22:59:44 +0200 (Do, 05 Jul 2012) $

      SUBROUTINE get_fragmPFNS (fragmPFNS, emiss_en, nen_emis,
     &      eincid, af, zf, emed, tequiv, qval, deltae,
     &      PFNtke, PFNrat, PFNalp, PFNere)
C
C     See N.V.Kornilov, A.B.Kagalenko and F.-J.Hambsch
C     Phys. At. Nuclei 62 (1999) pp 173-185
C
      implicit none
C     Dummy parameters
      integer nen_emis
      real*8 fragmPFNS(nen_emis), emiss_en(nen_emis)
      real*8 eincid, af, zf, emed, tequiv, qval, deltae
      real*8 PFNtke, PFNrat, PFNalp, PFNere

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
C     data ThCF/0.8868d0/,r/1.248d0/
C     Tuned model parameters 
      data ThCF/0.9020d0/, r/1.1752d0/
      data U0Cf/32.9d0/

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
      Erel = (CNdef - HFdef - LFdef)*PFNere ! PFNere is the scaling factor

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

      SUBROUTINE get_fragmPFNS_LANL (fragmPFNS, emiss_en, nen_emis,
     &      eincid, af, zf, emed, tequiv, qval, deltae,
     &      PFNtke, PFNrat, PFNalp, PFNere)
C
C     See D. Madland original paper (NSE) on LA model
C     See G. Vladuca, A. Tudora, Comp. Phys. Comm. 125 (2000) 221-238
C
      implicit none
C     Dummy parameters
      integer nen_emis
      real*8 fragmPFNS(nen_emis), emiss_en(nen_emis)
      real*8 eincid, af, zf, emed, tequiv, qval, deltae
      real*8 PFNtke, PFNrat, PFNalp, PFNere

C     Local variables
      real*8 ftmp1, ftmp2
      real*8 CNdef,HFdef,LFdef,ftmp, Erel,r,e,Efkin,Tlf,Thf
      real*8 Ux,EniuL,EniuH,Tm


      real*8 eplus, emin 
      COMMON /eparam/eplus,emin

C     real*8 fpost, fnscn, tscn, wscn
      real*8 fmed, abserr
      integer iah,ial,iaf,izf,izh,izl,i

C     Mass of heavy fragment fixed following Malinovskii
      data iah/140/ 
C     Ratio of the light to heavy fragments' Temperature taken from Kornilov
C     data r/1.248d0/  !Kornilov value
      data r/1.000d0/  !Original LANL value
C     Scission neutrons for Th-232 following Lovchikova et al (Phys.At.Nuclei 67 (2004) p890)
C     data wscn/0.10d0/ ! "wscn,tscn" fitted to get scission neutron temperature (0.38 +/- 0.04)
C     data tscn/0.40d0/ !

      real*8 GAUSS_INT2, SLAB, TKE, BIND
      external SLAB
   
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
      Erel = (CNdef - HFdef - LFdef)*PFNere ! PFNere is the scaling factor

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
      emed = 0.d0
      do i =1,nen_emis
        e = emiss_en(i)

        if (e.lt.1.d-12) cycle

        eplus = (DSQRT(e) + DSQRT(EniuL))**2
        emin  = (DSQRT(e) - DSQRT(EniuL))**2
        ftmp1=
     >  GAUSS_INT2(SLAB,0.d0,Tlf,abserr)/(2*Tlf*Tlf*DSQRT(EniuL))

        eplus = (DSQRT(e) + DSQRT(EniuH))**2
        emin  = (DSQRT(e) - DSQRT(EniuH))**2
        ftmp2=
     >  GAUSS_INT2(SLAB,0.d0,Thf,abserr)/(2*Thf*Thf*DSQRT(EniuH))

        fragmPFNS(i) = 0.5d0*(ftmp1 + ftmp2)

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
      if(E.le.0.1d-11) E=0.1d-11
      if(T.le.0.1d-11) return
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
      if(E.le.0.1d-11) E=0.1d-11
      if(T.le.0.1d-11) return
      if(E.GT.50.d0*T) return
      fmaxw = 2.d0/T*DSQRT(E/(pi*T))*DEXP(-E/T)
      return
      end

      real*8 FUNCTION fscn(E,T)
      real*8 E,T
      fscn = 0.d0
      if(E.le.0.1d-11) E=0.1d-11
      if(T.le.0.1d-11) return
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

      REAL*8 FUNCTION SLAB(T)
      IMPLICIT NONE
      REAL*8 T, TT, eplus, emin
      COMMON /tparam/TT      ! to pass T to FKLT,SKLT
      COMMON /eparam/eplus,emin
      REAL*8 ftmp1, ftmp2, abserr
      REAL*8 SKT, FKT, GAUSS_LAGUERRE_INT, GAUSS_INT1
      EXTERNAL SKT, FKT
   
      TT = T
      
      SLAB = 0.d0
      ftmp1 = GAUSS_LAGUERRE_INT(FKT,abserr) ! eq.7 for normalization
      if(ftmp1.eq.0.d0) return

      ftmp2 = GAUSS_INT1(SKT,emin,eplus,abserr) 

      SLAB = T * ftmp2 / ftmp1

      RETURN
      END

      REAL*8 FUNCTION FKT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, TT
      COMMON /tparam/TT
      xx = TT*x
      CALL INTERPOL1(1,xx,sigc)
      FKT = sigc*x*TT*TT
      RETURN
      END

      REAL*8 FUNCTION SKT(x)
      IMPLICIT NONE
      REAL*8 xx, x, sigc, TT
      COMMON /tparam/TT
      SKT = 0.d0
      xx = x/TT
      if(xx.GT.30.d0) return
      CALL INTERPOL1(1,x,sigc)
      SKT = DEXP(-xx)*sigc*DSQRT(x)
      RETURN
      END

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
      EXTERNAL F
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
      RETURN
      END

      REAL*8 FUNCTION GAUSS_INT2(F,Ea,Eb,Abserr)
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
      EXTERNAL F
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
      GAUSS_INT2 = resk1*hlgth1
      Abserr = ABS((resk1 - resg1)*hlgth1)
      RETURN
      END

