
C     looking for energy range
      do j=1,jrange(i)
c     Capote 2001
c     Excluded last energy range to avoid potential undefined for E>emax(RIPL)
        if(j.eq.jrange(i)) exit
        if(E.le.epot(i,j)) exit
      enddo
c----------------------------------------------------------------------------
c  rco(i,j,k)= coefficients for multiplying A**(1/3) for
c              specification of radius R in fm where:
c     R(i,j) = {abs[rco(i,j,1)] + rco(i,j,2)*E + rco(i,j,3)*eta
c             + rco(i,j,4)/A + rco(i,j,5)/sqrt(A)
c             + rco(i,j,6)*A**(2/3) + rco(i,j,7)*A
c             + rco(i,j,8)*A**2  + rco(i,j,9)*A**3
c             + rco(i,j,10)*A**(1/3)
c             + rco(i,j,11)*A**(-1/3)} * [A**(1/3)]
c
c  [Note that the A dependence of rco(i,j,11) cancels out so that rco(i,j,11)
c  is equivalent to adding a constant of that magnitude to the radius R(i,j)].
c     RRRR = radius (fm)
      RRRR = ( abs(rco(i,j,1)) +
     >            rco(i,j,2)*E +
     >            rco(i,j,3)*eta +
     >            rco(i,j,4)/atar +
     >            rco(i,j,5)/sqrt(atar) +
     >            rco(i,j,6)*atar**(2.d0/3.d0) +
     >            rco(i,j,7)*atar +
     >            rco(i,j,8)*atar**2 +
     >            rco(i,j,9)*atar**3 +
     >            rco(i,j,10)*atar**(1.d0/3.d0) +
     >            rco(i,j,11)*atar**(-1.d0/3.d0) )
c
c aco(i,j,k) = coefficients for specification of diffuseness a in
c              fm where:
c
c     a(i,j) = abs(aco(i,j,1)) + aco(i,j,2)*E + aco(i,j,3)*eta
c                + aco(i,j,4)/A + aco(i,j,5)/sqrt(A)
c                + aco(i,j,6)*A**(2/3) + aco(i,j,7)*A
c                + aco(i,j,8)*A**2 + aco(i,j,9)*A**3
c                + aco(i,j,10)*A**(1/3) + aco(i,j,11)*A**(-1/3)

c     AAAA = diffuseness (fm)
      AAAA = abs(aco(i,j,1)) +
     >          aco(i,j,2)*E +
     >          aco(i,j,3)*eta +
     >          aco(i,j,4)/atar +
     >          aco(i,j,5)/sqrt(atar) +
     >          aco(i,j,6)*atar**(2.d0/3.d0) +
     >          aco(i,j,7)*atar +
     >          aco(i,j,8)*atar**2 +
     >          aco(i,j,9)*atar**3 +
     >          aco(i,j,10)*atar**(1.d0/3.d0) +
     >          aco(i,j,11)*atar**(-1.d0/3.d0)

c     pot(i,j,k) = strength parameters, as follows:
c     VSTR = strength in MeV

      If(pot(i,j,22).eq.0.AND.
     >   pot(i,j,23).eq.0.AND.
     >   pot(i,j,24).eq.0) THEN
c     Standard potential formulas
      VSTR = pot(i,j,1) + pot(i,j,7)*eta + pot(i,j,8)*Ecoul
     >       + pot(i,j,9)*atar + pot(i,j,10)*atar**(1/3)
     >       + pot(i,j,11)*atar**(-2/3) + pot(i,j,12)*Ecoul2
     >       + (pot(i,j,2) + pot(i,j,13)*eta + pot(i,j,14)*atar)*E
     >       + pot(i,j,3)*E*E + pot(i,j,4)*E*E*E + pot(i,j,6)*sqrt(E)
     >       + (pot(i,j,5) + pot(i,j,15)*eta + pot(i,j,16)*E)*log(E)
     >       + pot(i,j,17)*Ecoul/E**2
      ELSE
          If(pot(i,j,22).ne.0) THEN
c          Special Smith-type potential formulas
           VSTR = pot(i,j,1) + pot(i,j,2)*eta
     >       + pot(i,j,3)*cos(2*pi*(atar - pot(i,j,4))/pot(i,j,5))
     >       + pot(i,j,6)*sexp(pot(i,j,7)*E + pot(i,j,8)*E*E)
     >       + pot(i,j,9)*E*sexp(pot(i,j,10)*E**pot(i,j,11))
          ELSEif(pot(i,j,23).ne.0) THEN
c          Special Varner-type potential formulas
           VSTR = (pot(i,j,1) + pot(i,j,2)*eta)/
     >       (1 + sexp((pot(i,j,3) - E + pot(i,j,4)*Ecoul2)/pot(i,j,5)))
     >       + pot(i,j,6)*sexp((pot(i,j,7)*E - pot(i,j,8))/pot(i,j,6))

          ELSEif(pot(i,j,24).ne.0) THEN
c          Special Koning-type potential formulas
           Ea=dble(int(100000*pot(i,j,21)))/100000
           if(Ea.eq.0.) Ea=1000.1d0
           if(pot(i,j,18).ne.0.)
     >      ef=dble(int(100000*pot(i,j,18)))/100000 + pot(i,j,19)*atar
           Ep=dble(int(100000*pot(i,j,20)))/100000
           if(Ep.eq.0.) Ep=Ef

           if(i.eq.1) then
C            call bcoget(b,j)
             do ii=1,6
              do jj=1,ndim1
               do kk=1,12
                b(ii,jj,kk)=0.
               end do
              end do
             enddo
c
             b(1,j,1)  =  pot(1,j,1) + pot(1,j,2)*atar + pot(1,j,8)*eta
             b(1,j,2)  =  pot(1,j,3) + pot(1,j,4)*atar
             b(1,j,3)  =  pot(1,j,5) + pot(1,j,6)*atar
             b(1,j,4)  =  pot(1,j,7)
             b(1,j,5)  =  pot(1,j,9)*(ztar/atar**(1./3.))
             b(1,j,11) =  pot(1,j,10) + pot(1,j,11)*atar
             b(1,j,12) =  pot(1,j,12)
             b(2,j,6)  =  pot(2,j,1) + pot(2,j,2)*atar
             b(2,j,7)  =  pot(2,j,3) + pot(2,j,4)*atar
             b(4,j,8)  =  pot(4,j,1) + pot(4,j,8)*eta
             if(pot(4,j,3).eq.0.d0) then
                 b(4,j,9)  =  pot(4,j,2)
             else
                 b(4,j,9)  =  pot(4,j,2) +
     +              pot(4,j,3)/(1. + sexp((atar-pot(4,j,4))/pot(4,j,5)))
             endif
             b(4,j,10) =  pot(4,j,6)
             b(5,j,11) =  pot(5,j,10) + pot(5,j,11)*atar
             b(5,j,12) =  pot(5,j,12)
             b(6,j,6)  =  pot(6,j,1)
             b(6,j,7)  =  pot(6,j,3)

          endif

C          Introducing average energy of particle states ep
C          only for imaginary surface or volume potential
C
          eff=ef
          if( (i.eq.2 .OR. i.eq.4) .and. (ep.ne.ef) ) eff=ep

          nn = int(pot(i,j,13))
          VSTR =
     +    b(i,j,1)*(1.- b(i,j,2)*(E-eff) + b(i,j,3)*(E-eff)**2 -
     +    b(i,j,4)*(E-eff)**3) + b(i,j,5) +
     +    b(i,j,6)*((E-eff)**nn/((E-eff)**nn + b(i,j,7)**nn)) +
     +    b(i,j,8)*sexp(-b(i,j,9)*(E-eff))*((E-eff)**nn/
     +    ((E-eff)**nn + b(i,j,10)**nn)) +
     +    b(i,j,11)*sexp(-b(i,j,12)*(E-eff))

          if(i.eq.2 .and. ea.lt.1000. .and. E.gt.(ef+ea) ) VSTR=VSTR+
     +         1.65*(SQRT(E)+(ef+ea)**1.5d0/(2.d0*E)-1.5d0*SQRT(ef+ea))

         ENDIF
      ENDIF

C     For Madland type potentials
      if(irel.eq.2) VSTR=gamma*VSTR
