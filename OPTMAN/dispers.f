C==========================================================================
C     AUTHOR: Dr. Roberto Capote Noy
c
C     e-mail: r.capotenoy@iaea.org ; rcapotenoy@yahoo.com; 
C
C     DISPERSIVE OPTICAL MODEL POTENTIAL PACKAGE
C
c     Analytical dispersive integrals are included
c     see Quesada JM, Capote R et al,
C             Computer Physics Communications 153(2003) 97
C             Phys. Rev. C67(2003) 067601
C
C     Dispersive integral's derivatives calculated by Dr.J.M.Quesada
C
      REAL*8 FUNCTION DOM_INT_Wv(Ef,Ep,Av,Bv,n,Einc,DerivIntWv)
C
C     Analytical dispersive integral and its derivative for
C     Wv(E)=Av*(E-Ep)**n/( (E-Ep)**n + Bv**n )  for E>Ep
C     Wv(E)=Wv(2*Ef-E)                          for E<2Ef-Ep
C     Wv(E)=0                                     OTHERWISE
C
      IMPLICIT NONE
      REAL*8 Ef,Ep,Av,Bv,E,pi,Einc
      REAL*8 E0,Ex,Eplus,Emin,Rs,ResEmin,ResEplus
      REAL*8 DerEmin, DerEplus, Rds, DerivIntWv
      COMPLEX*16 Pj,I,Zj,Ztmp
      COMPLEX*8 Fs,Ds

      INTEGER N,j,IS

      DATA I/(0.d0,1.d0)/

      pi=4.d0*atan(1.d0)

      IS = 1
      E = Einc
      IF(Einc.LE.Ef) THEN
        E=2.d0*Ef-Einc
C       Odd function
        IS = -1
      ENDIF

      E0 = Ep - Ef
      Ex = E  - Ef
      Eplus = Ex + E0
      Emin  = Ex - E0
      DOM_INT_Wv = 0.d0
      DerivIntWv = 0.d0

      ResEmin  =  Emin**n / (Emin**n + Bv**n)

      DerEmin  =  Emin**(n-1) *
     >            ( Emin**n + Bv**n*(1.d0 + n*log(dabs(Emin)) ) )
     >            / (Emin**n + Bv**n)**2

      ResEplus = -Eplus**n / (Eplus**n + Bv**n)

      DerEplus = -Eplus**(n-1) *
     >            ( Eplus**n + Bv**n*(1.d0+n*log(dabs(Eplus))) )
     >            / (Eplus**n + Bv**n)**2

C----------------------------------
C     Complex arithmetic follows
C
      Fs = (0.d0,0.d0)
      Ds = (0.d0,0.d0)
      do j=1,n
       Ztmp = I*(2*j-1)/dble(n)*pi
       Pj = Bv*exp(Ztmp)
       Zj = Pj * (2*Pj +Eplus -Emin) * Ex
       Zj = Zj / ( (Pj+E0) * (Pj+Eplus) * (Pj-Emin) )
       Fs = Fs + Zj*log(-Pj)
       Ds = Ds + 2*Pj*(Ex*Ex + (Pj+E0)**2)*log(-Pj)
     >           /( (Pj+Eplus)**2 * (Pj-Emin)**2 )
      enddo

      IF(ABS(IMAG(Fs)).gt.1.e-4) STOP 'Too big imag part in Wv'
      IF(ABS(IMAG(Ds)).gt.1.e-4) STOP 'Too big imag deriv in Wv'
      Rs  = REAL(Fs)
      Rds = REAL(Ds)
C----------------------------------

      DOM_INT_Wv = -Av/pi*IS*
     &  (Rs/n  + ResEplus*log(dabs(Eplus)) + ResEmin*log(dabs(Emin)))

C     Sign of derivative changed
C     DerivIntWv = -Av/pi*IS*( Rds/n + DerEplus + DerEmin)
      DerivIntWv =  Av/pi*IS*( Rds/n + DerEplus + DerEmin)

      RETURN
      END

      REAL*8 FUNCTION DOM_INT_Ws(Ef,Ep,As,Bs,Cs,m,Einc,DerivIntWs)
C
C     Analytical dispersive integral and its derivative for
C     Ws(E)=As*(E-Ep)**m/( (E-Ep)**m + Bs**m ) * exp(-Cs*(E-Ep)) for E>Ep
C     Ws(E)=Ws(2*Ef-E)                                           for E<2Ef-Ep
C     Ws(E)=0                                                    OTHERWISE
C
      IMPLICIT NONE
      REAL*8 Ef,Ep,As,Bs,Cs,E,EIn,Einc
      COMPLEX*16 I,Pj,Zj,Ztmp,zfi
      COMPLEX*8 Fs,Ds
      REAL*8 E0,Ex,Eplus,Emin,pi
      REAL*8 Rs,ResEmin,ResEplus
      REAL*8 DerivIntWs,DerEmin,DerEplus,Rds
      INTEGER m,j,IS

      DATA I/(0.d0,1.d0)/

      pi=4.d0*atan(1.d0)

      IS = 1
      E = Einc
      IF(Einc.LE.Ef) THEN
        E=2.d0*Ef-Einc
C       Odd function
        IS = -1
      ENDIF

      E0 = Ep - Ef
      Ex = E  - Ef
      Eplus = Ex + E0
      Emin  = Ex - E0
      DOM_INT_Ws = 0.d0
      DerivIntWs = 0.d0

      ResEmin  =  Emin**m / (Emin**m + Bs**m)

      DerEmin  = -Emin**(m-1) *
     >           ( Emin**m + Bs**m + ( -Cs*Emin**(m+1) +
     >            Bs**m *(-Cs*Emin+m) ) * exp(-Cs*Emin)*EIn(Cs*Emin) )
     >            / (Emin**m + Bs**m)**2

      ResEplus = -Eplus**m / (Eplus**m + Bs**m)

      DerEplus =  Eplus**(m-1) *
     >           ( Eplus**m + Bs**m + ( Cs*Eplus**(m+1) +
     >            Bs**m *(Cs*Eplus+m) ) * exp(Cs*Eplus)*EIn(-Cs*Eplus) )
     >            / (Eplus**m + Bs**m)**2

C----------------------------------
C     Complex arithmetic follows
C
      Fs = (0.d0,0.d0)
      Ds = (0.d0,0.d0)
      do j=1,m
       Ztmp = I*(2*j-1)/dble(m)*pi
       Pj = Bs*exp(Ztmp)
       Zj = Pj * (2*Pj +Eplus -Emin) * Ex
       Zj = Zj / (Pj+E0) / (Pj+Eplus) / (Pj-Emin)
       Fs = Fs + Zj* zfi(-Pj*Cs)
       Ds = Ds + 2*Pj*(Ex*Ex + (Pj+E0)**2)*zfi(-Pj*Cs)
     >           /( (Pj+Eplus)**2 * (Pj-Emin)**2 )
      enddo

      IF(ABS(IMAG(Fs)).gt.1.e-4) STOP 'Too big imag part in Ws'
      IF(ABS(IMAG(Ds)).gt.1.e-4) STOP 'Too big imag deriv in Ws'
      Rs = REAL(Fs)
      Rds = REAL(Ds)
C----------------------------------

      DOM_INT_Ws = As/pi*IS*(Rs/m
     &                  - ResEplus*exp(Cs*Eplus)*EIn(-Cs*Eplus)
     &                  - ResEmin*exp(-Cs*Emin)*EIn(Cs*Emin) )
C     Sign of derivative changed
C     DerivIntWs =  As/pi*IS*( Rds/m + DerEplus + DerEmin)
      DerivIntWs = -As/pi*IS*( Rds/m + DerEplus + DerEmin)

      RETURN
      END

      real*8 function WV(A,B,Ep,Ef,E,n)
      IMPLICIT NONE
      real*8 A,B,Ep,Ef,E,ee
      integer n

      WV=0.d0
      if(E.LE.Ef) E=2.d0*Ef-E
      if(E.LT.Ep) return

      ee=(E-Ep)**n
      WV=A*ee/(ee+B**n)

      return
      end

      real*8 function WDD(A,B,C,Ep,Ef,E,m)
      IMPLICIT NONE
      real*8 A,B,C,Ep,Ef,E,ee,arg
      integer m

      WDD=0.d0
      if(E.LE.Ef) E=2.d0*Ef-E
      if(E.LT.Ep) return

      arg=C*(E-Ep)
      IF(arg.GT.15) return
      ee=(E-Ep)**m
      WDD=A*ee/(ee+B**m)*EXP(-arg)
      return
      end


      REAL*8 FUNCTION DOM_int_T1(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T1(E'<<0)
C
      IMPLICIT NONE

      real*8 E,Ea,Ef,Ex,Ea2,Eax,Pi,T11,T12,T13
      Pi=4.d0*ATAN(1.d0)

      Ex=E-Ef
      Ea2=Ea**2
      Eax=Ex+Ea

      T11 = 0.5d0*log(Ea)/Ex
      T12 =  ( (2*Ea+Ex)*log(Ea)+0.5d0*pi*Ex )
     >      /(2.*(Eax**2 + Ea2))
      T13 = -Eax**2*log(Eax)/(Ex*(Eax**2+Ea2))

      DOM_int_T1 = Ex/Pi*(T11+T12+T13)
C
      RETURN
      END
C
      REAL*8 FUNCTION DOM_int_T2(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T2(E'>>0)
C
      IMPLICIT NONE
      real*8 E,Ea,Ef,EL,Pi

      Pi=4.d0*ATAN(1.d0)
      EL=Ef+Ea
      DOM_int_T2= 1.d0 / Pi * (
     >      sqrt(abs(Ef)) * atan( (2*sqrt(EL*abs(Ef)))/(EL-abs(Ef)) )
     > +    EL**1.5d0/(2*Ef)*log(Ea/EL) )

      IF(E.GT.EL) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi* (
     >  sqrt(E) * log( (sqrt(E)+sqrt(EL)) / (sqrt(E)-sqrt(EL)) ) +
     >  1.5d0*sqrt(EL)*log((E-EL)/Ea) + EL**1.5d0/(2*E)*log(EL/(E-EL)) )

      ELSEIF(E.EQ.EL) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi*1.5d0*sqrt(EL)
     > *log((2**(4.d0/3.d0)*EL)/Ea)

      ELSEIF(E.GT.0.d0 .AND. E.LE.EL) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi * (
     > sqrt(e) * log( (sqrt(E)+sqrt(EL)) / (sqrt(EL)-sqrt(E)) ) +
     > 1.5d0*sqrt(EL)*log((EL-E)/Ea)+EL**1.5d0/(2.d0*E)*log(EL/(EL-E)) )

      ELSEIF(E.EQ.0.d0) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi*( 0.5*EL**(1./3.)
     > + log(EL/Ea) + 0.5d0*sqrt(EL) )

      ELSE

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi * (
     > -sqrt(abs(E))*atan( 2*(sqrt(EL-abs(E))) / (EL-abs(E)) ) +
     > 1.5d0*sqrt(EL)*log((EL-E)/Ea)+EL**1.5d0/(2.d0*E)*log(EL/(EL-E)) )

      ENDIF
      RETURN
      END
C
C-----FUNCTION TO EVALUATE exp(Z)*E1(Z)
C
      complex*16 function zfi(za)
C
C Complex exponential integral function multiplied by exponential
C
C AUTHOR: J. Raynal
C
      IMPLICIT NONE
      real*8 aj
      complex*16 za,y
      integer m,i
      zfi=0.d0
      if (za.eq.0.) return
      if (dabs(dreal(za)+18.5d0).ge.25.d0) go to 3
      if (dsqrt(625.d0-(dreal(za)+18.5d0)**2)/1.665d0.lt.dabs(dimag(za))
     1) go to 3
      zfi=-.57721566490153d0-cdlog(za)
      y=1.d0
      do 1 m=1,2000
      aj=m
      y=-y*za/aj
      if (cdabs(y).lt.1.d-15*cdabs(zfi)) go to 2
    1 zfi=zfi-y/aj
    2 zfi=cdexp(za)*zfi
      return
    3 do 4 i=1,20
      aj=21-i
      zfi=aj/(za+zfi)
    4 zfi=aj/(1.d0+zfi)
      zfi=1.d0/(zfi+za)
      return
      end

C
C-----FUNCTION TO EVALUATE Ei(X)
C
      REAL*8 FUNCTION EIn(X)
      IMPLICIT NONE
      REAL*8 FAC, H, X
      INTEGER N
      EIn = 0.57721566490153d0+LOG(ABS(X))
      FAC = 1.0
      DO N = 1,100
      H = FLOAT(N)
      FAC = FAC*H
      EIn = EIn + X**N/(H*FAC)
      ENDDO
      RETURN
      END
