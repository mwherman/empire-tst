C
C-----included file starts here
C
C     looking for energy range
      DO j = 1, JRAnge(i)
C        Capote 2001
C        Excluded last energy range to avoid potential undefined for
C        E>emax(RIPL)
         IF(j.EQ.JRAnge(i))EXIT
         IF(E.LE.EPOt(i, j))EXIT
      ENDDO
C----------------------------------------------------------------------------
C     rco(i,j,k)= coefficients for multiplying A**(1/3) for
C     specification of radius R in fm where:
C     R(i,j) = {abs[rco(i,j,1)] + rco(i,j,2)*E + rco(i,j,3)*eta
C     + rco(i,j,4)/A + rco(i,j,5)/sqrt(A)
C     + rco(i,j,6)*A**(2/3) + rco(i,j,7)*A
C     + rco(i,j,8)*A**2  + rco(i,j,9)*A**3
C     + rco(i,j,10)*A**(1/3)
C     RCN, 08/2004, to handle new extension to the OMP RIPL-2 format
C     + rco(i,j,12)*E*E
C     + rco(i,j,11)*A**(-1/3)} * [A**(1/3)]
C      

C
C     [Note that the A dependence of rco(i,j,11) cancels out so that rco(i,j,11)
C     is equivalent to adding a constant of that magnitude to the radius
C     R(i,j)]. RRRR = radius (fm)
      rrrr = (ABS(RCO(i,j,1)) + RCO(i, j, 2)*E + RCO(i, j, 3)
     &       *eta + RCO(i, j, 4)/atar + RCO(i, j, 5)/SQRT(atar)
     &       + RCO(i, j, 6)*atar**(2.D0/3.D0) + RCO(i, j, 7)
     &       *atar + RCO(i, j, 8)*atar**2 + RCO(i, j, 9)*atar**3 + 
     &       RCO(i, j, 10)*atar**(1.D0/3.D0) + RCO(i, j, 11)
C     RCN, 08/2004, to handle new extension to the OMP RIPL-2 format	 
     &       *atar**( - 1.D0/3.D0) + RCO(i,j,12)*E*E)
C
C     aco(i,j,k) = coefficients for specification of diffuseness a in
C     fm where:
C
C     a(i,j) = abs(aco(i,j,1)) + aco(i,j,2)*E + aco(i,j,3)*eta
C     + aco(i,j,4)/A + aco(i,j,5)/sqrt(A)
C     + aco(i,j,6)*A**(2/3) + aco(i,j,7)*A
C     + aco(i,j,8)*A**2 + aco(i,j,9)*A**3
C     + aco(i,j,10)*A**(1/3) + aco(i,j,11)*A**(-1/3)
C
C     AAAA = diffuseness (fm)
      aaaa = ABS(ACO(i, j, 1)) + ACO(i, j, 2)*E + ACO(i, j, 3)
     &       *eta + ACO(i, j, 4)/atar + ACO(i, j, 5)/SQRT(atar)
     &       + ACO(i, j, 6)*atar**(2.D0/3.D0) + ACO(i, j, 7)
     &       *atar + ACO(i, j, 8)*atar**2 + ACO(i, j, 9)*atar**3 + 
     &       ACO(i, j, 10)*atar**(1.D0/3.D0) + ACO(i, j, 11)
     &       *atar**( - 1.D0/3.D0)
C
C     pot(i,j,k) = strength parameters, as follows:
C     VSTR = strength in MeV
C
      IF(POT(i, j, 22).EQ.0 .AND. POT(i, j, 23).EQ.0 .AND. POT(i, j, 24)
     &   .EQ.0)THEN
C        Standard potential formulas
         vstr = POT(i, j, 1) + POT(i, j, 7)*eta + POT(i, j, 8)
     &          *ecoul + POT(i, j, 9)*atar + POT(i, j, 10)*atar**(1/3)
     &          + POT(i, j, 11)*atar**( - 2/3) + POT(i, j, 12)
     &          *ecoul2 + (POT(i, j, 2) + POT(i, j, 13)
     &          *eta + POT(i, j, 14)*atar)*E + POT(i, j, 3)
     &          *E*E + POT(i, j, 4)*E*E*E + POT(i, j, 6)*SQRT(E)
     &          + (POT(i, j, 5) + POT(i, j, 15)*eta + POT(i, j, 16)*E)
     &          *LOG(E) + POT(i, j, 17)*ecoul/E**2
      ELSEIF(POT(i, j, 22).NE.0)THEN
C        Special Smith-type potential formulas
         vstr = POT(i, j, 1) + POT(i, j, 2)*eta + POT(i, j, 3)
     &          *COS(2*PI*(atar - POT(i,j,4))/POT(i, j, 5))
     &          + POT(i, j, 6)*SEXP(POT(i, j, 7)*E + POT(i, j, 8)*E*E)
     &          + POT(i, j, 9)*E*SEXP(POT(i, j, 10)*E**POT(i, j, 11))
      ELSEIF(POT(i, j, 23).NE.0)THEN
C        Special Varner-type potential formulas
         vstr = (POT(i, j, 1) + POT(i, j, 2)*eta)
     &          /(1 + SEXP((POT(i,j,3)-E+POT(i,j,4)*ecoul2)/POT(i,j,5)))
     &          + POT(i, j, 6)
     &          *SEXP((POT(i,j,7)*E - POT(i,j,8))/POT(i, j, 6))
C
      ELSEIF(POT(i, j, 24).NE.0)THEN
C        Special Koning-type potential formulas
         ea = DBLE(INT(100000*POT(i,j,21)))/100000
         IF(ea.EQ.0.)ea = 1000.1D0
         IF(POT(i, j, 18).NE.0.)EF = DBLE(INT(100000*POT(i,j,18)))
     &                               /100000 + POT(i, j, 19)*atar
         EP = DBLE(INT(100000*POT(i,j,20)))/100000
         IF(EP.EQ.0.)EP = EF
C
         IF(i.EQ.1)THEN
C           call bcoget(b,j)
            DO ii = 1, 6
               DO jj = 1, NDIM1
                  DO kk = 1, 12
                     b(ii, jj, kk) = 0.
                  ENDDO
               ENDDO
            ENDDO
C
            b(1, j, 1) = POT(1, j, 1) + POT(1, j, 2)*atar + POT(1, j, 8)
     &                   *eta
            b(1, j, 2) = POT(1, j, 3) + POT(1, j, 4)*atar
            b(1, j, 3) = POT(1, j, 5) + POT(1, j, 6)*atar
            b(1, j, 4) = POT(1, j, 7)
            b(1, j, 5) = POT(1, j, 9)*(ztar/atar**(1./3.))
            b(1, j, 11) = POT(1, j, 10) + POT(1, j, 11)*atar
            b(1, j, 12) = POT(1, j, 12)
            b(2, j, 6) = POT(2, j, 1) + POT(2, j, 2)*atar
            b(2, j, 7) = POT(2, j, 3) + POT(2, j, 4)*atar
            b(4, j, 8) = POT(4, j, 1) + POT(4, j, 8)*eta
            IF(POT(4, j, 3).EQ.0.D0)THEN
               b(4, j, 9) = POT(4, j, 2)
            ELSE
               b(4, j, 9) = POT(4, j, 2) + POT(4, j, 3)
     &                      /(1. + SEXP((atar-POT(4,j,4))/POT(4,j,5)))
            ENDIF
            b(4, j, 10) = POT(4, j, 6)
            b(5, j, 11) = POT(5, j, 10) + POT(5, j, 11)*atar
            b(5, j, 12) = POT(5, j, 12)
            b(6, j, 6) = POT(6, j, 1)
            b(6, j, 7) = POT(6, j, 3)
C
         ENDIF
C
C        Introducing average energy of particle states ep
C        only for imaginary surface or volume potential
C
         eff = EF
         IF((i.EQ.2 .OR. i.EQ.4) .AND. (EP.NE.EF))eff = EP
C
         nn = INT(POT(i, j, 13))
         vstr = b(i, j, 1)
     &          *(1. - b(i, j, 2)*(E - eff) + b(i, j, 3)*(E - eff)
     &          **2 - b(i, j, 4)*(E - eff)**3) + b(i, j, 5) + b(i, j, 6)
     &          *((E - eff)**nn/((E-eff)**nn + b(i,j,7)**nn))
     &          + b(i, j, 8)*SEXP( - b(i, j, 9)*(E - eff))
     &          *((E - eff)**nn/((E-eff)**nn + b(i,j,10)**nn))
     &          + b(i, j, 11)*SEXP( - b(i, j, 12)*(E - eff))
C
         IF(i.EQ.2 .AND. ea.LT.1000. .AND. E.GT.(EF + ea))vstr = vstr + 
     &      1.65*(SQRT(E) + (EF + ea)**1.5D0/(2.D0*E)
     &      - 1.5D0*SQRT(EF + ea))
C
      ENDIF
C
C     For Madland type potentials
      IF(IREl.EQ.2)vstr = gamma*vstr
C
C-----included file ends here
C
