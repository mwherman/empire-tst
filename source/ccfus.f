Ccc   * $Rev: 2537 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-02-10 14:07:34 +0100 (Fr, 10 Feb 2012) $
C
      SUBROUTINE CCFUS(Stl,Rkey)
C
C     CCFUS $ FUSION COUPLED-CHANNELS KOBENHAVN CODE FOR CALCULATION OF
C     CW POTENTIAL  (-20 MEV),   BARRIER PENETRATION PARAMETERS,  CROSS
C     SECTIONS AND ANGULAR MOMENTUM DISTRIBUTIONS
C
C     CODED BY C.H. DASSO AND S. LANDOWNE          KOBENHAVN MCMLXXXIII
C     ------------------------------------------------------------------
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION A0R, AU, ETAk, HC, RAB, REDm, V0R
      COMMON /ONE   / V0R, A0R, RAB, REDm, AU, HC, ETAk
C
C Dummy arguments
C
      DOUBLE PRECISION Stl(NDLW), Rkey
C
C Local variables
C
      DOUBLE PRECISION aux, aux0, ddur, delb, delta, dfl, dfl2,
     &                 dfla(NDCC,2), dfla2(NDCC,2), dur, eps, f,
     &                 facw, facwd, fkap, fl, fla(NDCC,2), flam1,
     &                 flamem(NDCC), flo, fpi, gl, h2m, homega, p,
     &                 pa(NDCC,2), ra, rb, rbar, rcal, rcald, rr, rred,
     &                 s0, s1, s2, sq, su0, su1, su2,
     &                 sum, ur, vb, vbl, vbw, vbwl, r00, sumunc
      DOUBLE PRECISION ecrit1, bfu, critl, sigl0(NDLW), sigl1(NDLW) 
      REAL FLOAT
      INTEGER i1, ic1, ick, il, ilim, k, n, n1(NDCC), n1t, nd, nmax,
     &        np(NDCC), ns1
C
      sum = 0.d0
      sumunc = 0.d0
      sigl0 = 0.d0 
      sigl1 = 0.d0

      DO k = 1, NDLW
         Stl(k) = 0.d0
      ENDDO
      
      nmax = NSCc + NACc
      ns1 = NSCc + 1

      r00 = 1.233d0

      ra = r00*AEJc(0)**(1./3.) - 0.978/AEJc(0)**(1./3.)
      rb = r00*A(0)**(1./3.) - 0.978/A(0)**(1./3.)
      RAB = ra + rb + 0.29d0
      rred = ra*rb/(ra + rb)
      REDm = AEJc(0)*A(0)/(AEJc(0) + A(0))

      AU = AMUmev
      HC =  HHBarc
      h2m = HC*HC/AU

      V0R = 30.08*(1. - 1.8*(1. - 2.*ZEJc(0)/AEJc(0))
     &      *(1. - 2.*Z(0)/A(0)))*rred + DV - 20.
      A0R = 0.63
      delb = 0.
      ETAk = 1.43997*ZEJc(0)*Z(0)
      fpi = 3.544908
      CALL BAR(rbar,vb,homega)
      eps = homega/6.283185
      rcal = rbar + delb
      facw  = 31.415926d0 * rcal**2*eps
      sumunc = facw *LOG(1. + EXP((EIN-vb )/eps))/EIN

Ccc   *                         B A S S
Ccc   * Calculates fusion x-section critical l-value for a heavy-ion
Ccc   * induced reaction according to Bass model. E1 is the energy at
Ccc   * which the linear dependence of l critical begins.
Ccc   * ref: formulae  from Bass, Nucl. Phys. A231(1974)45,
Ccc   * and nuclear potential from Phys. Rev. Lett. 39(1977)265
Ccc   *
Ccc   * input:EIN-incident energy (c.m.)
Ccc   *       ZP -Z of a projectile
Ccc   *       AP -A of a projectile
Ccc   *       ZT -Z of a target
Ccc   *       AT -A of a target
Ccc   *
Ccc   * output:BFUS-fusion barrier
Ccc   *        E1   -see above
Ccc   *        CRL  -critical angular momentum
Ccc   *        CSFUS-fusion x-section
      CALL BASS(EIN,ZEJc(0),AEJc(0),Z(0),A(0),Bfu,ecrit1,critl,csfus)
      WRITE (8,*) ' =================================================='
c -- generate printout
      WRITE (8,*) ' '
      WRITE (8,*)
     & ' Fusion cross section calculated using coupled channel approach'
      WRITE (8,*)
     &          ' by Dasso and Landowne (Comp. Phys. Comm. 46(1987)187)'
      WRITE (8,*) ' '
      WRITE (8,*) 
      WRITE (8,*) 
     > 'DV is the parameter used to adjust the barrier (Default 20)'
      WRITE (8,*) 
     > 'DV = 20 corresponds to the Christensen-Winther potential'
      WRITE (8,*) 
      WRITE (8,*) 
     > 'P.R.Christensen and A.Winther, Phys. Lett. B65 (1976) 19'
      WRITE (8,*) 
     > ' (& R.A.Broglia and A.Winther, Heavy Ion Reactions, Benjamin, NY
     >, 1981)'
      WRITE (8,*) 
      WRITE (8,*) 'DV =',sngl(DV),' (Barrier scaling parameter)' 
      WRITE (8,*) 
      WRITE (8,*) 'VB =',sngl(vb) ,' (Unperturbed CCFUS barr)' 
      WRITE (8,*) 
      WRITE (8,*) 'RB =',sngl(rbar),' (CCFUS barrier position)' 
      WRITE (8,*) 
      WRITE (8,*) 'hw =',sngl(homega),' (CCFUS barrier thickness)' 
      WRITE (8,*) 

C      WRITE (8,99005) DV, vb, rbar, homega
C99005 FORMAT (/,'  Parameters for DV=',F6.2,//,'      VB=',F6.1,
C     &        '       RB=',F6.2,'       H-OMEGA=',F5.2,/)


      IF (NSCc.NE.0) THEN
         WRITE (8,99010)
99010    FORMAT (/,2X,35('*'),/,'  *    BETA    * LAMDA *   Q(MEV)   *',
     &           /,2X,35('*'))
         DO n = 1, NSCc
            WRITE (8,99015) BETcc(n), FLAm(n), QCC(n)
99015       FORMAT ('  *  ',F6.2,'    *  ',F3.0,'  *  ',F6.2,'    *')
            IF (BETcc(n).EQ.0.0D0 .AND. FIRst_ein) THEN
               WRITE (8,*) ' WARNING:'
               WRITE (8,*) ' WARNING: Deformation for channel ', n,
     &                     ' in CCFUS is 0'
               WRITE (8,*) ' WARNING: It was set internally to 0.1'
               BETcc(n) = 0.1
            ENDIF
            flamem(n) = FLAm(n)
            rr = rb
            IF (FLAm(n).LT.0.D0) rr = ra
C           IF (BETcc(n).LT.0.D0) BETcc(n) = -BETcc(n)/rr
            FLAm(n) = ABS(FLAm(n))
            flam1 = FLAm(n) - 1
            CALL POT(rcal,ur,dur,ddur)
            f = BETcc(n)
     &          *rr*(( - dur) + 3.*ETAk*(rr/rcal)**flam1/(2.*FLAm(n)
     &          + 1.)/rcal**2)/fpi
            fkap = -ddur/dur
            sq = SQRT(QCC(n)*QCC(n) + 4.*f*f)
            fla(n,1) = .5*(( - QCC(n)) - sq)
            fla(n,2) = .5*(( - QCC(n)) + sq)
            dfla(n,1) = 2.*fkap*f**2/sq
            dfla(n,2) = -dfla(n,1)
            dfla2(n,1) = ( - 4.*(fkap*f)**2/sq) + 8.*(fkap*f*f)**2/sq**3
            dfla2(n,2) = -dfla2(n,1)
            pa(n,1) = f*f/(f*f + fla(n,1)**2)
            pa(n,2) = f*f/(f*f + fla(n,2)**2)
         ENDDO
         WRITE (8,99020)
99020    FORMAT (2X,35('*'),/)
      ENDIF
      IF (NACc.NE.0) THEN
         WRITE (8,99025)
99025    FORMAT (2X,27('*'),/,'  *   F(MEV)   *   Q(MEV)   *',/,2X,
     &           27('*'))
         DO n = ns1, nmax
            fkap = 0.71
C           READ (5,*) FCD(N),QCC(N)
            WRITE (8,99030) FCD(n), QCC(n)
99030       FORMAT ('  *  ',F6.2,'    *  ',F6.2,'    *')
            sq = SQRT(QCC(n)*QCC(n) + 4.*FCD(n)*FCD(n))
            fla(n,1) = .5*(( - QCC(n)) - sq)
            fla(n,2) = .5*(( - QCC(n)) + sq)
            pa(n,1) = FCD(n)*FCD(n)/(FCD(n)*FCD(n) + fla(n,1)**2)
            pa(n,2) = FCD(n)*FCD(n)/(FCD(n)*FCD(n) + fla(n,2)**2)
            dfla(n,1) = 2.*fkap*FCD(n)**2/sq
            dfla(n,2) = -dfla(n,1)
            dfla2(n,1) = ( - 4.*(fkap*FCD(n))**2/sq)
     &                   + 8.*(fkap*FCD(n)*FCD(n))**2/sq**3
            dfla2(n,2) = -dfla2(n,1)
         ENDDO
         WRITE (8,99035)
99035    FORMAT (2X,27('*'),/)
      ENDIF
      ilim = nmax
      IF (nmax.GT.NDCC) STOP '  MAXIMUM NUMBER OF CHANNELS EXCEEDED'
      nd = 2**nmax
      np(1) = 1
      DO n = 2, nmax
         np(n) = np(n - 1)*2
      ENDDO
      DO i1 = 1, nd
         ic1 = i1 - 1
         p = 1.
         fl = 0.
         dfl = 0.
         dfl2 = 0.
         ick = 0
         DO n = 1, nmax
            n1(n) = ic1/np(nmax + 1 - n)
            n1t = n1(n) + 1
            ick = ick + n1(n)
            IF (ick.GT.ilim) GOTO 100
            p = p*pa(n,n1t)
            fl = fl + fla(n,n1t)
            dfl = dfl + dfla(n,n1t)
            dfl2 = dfl2 + dfla2(n,n1t)
            ic1 = ic1 - n1(n)*np(nmax + 1 - n)
         ENDDO
         delta = FCC*dfl/(REDm*homega**2/h2m - dfl2)
         IF (ABS(delta).GT.0.99D0) delta = -.99*fl/ABS(fl)
         vbw = vb - .5*REDm*(homega*delta)**2/h2m + fl + dfl*delta +
     &         .5*dfl2*delta**2
         rcald = rcal + delta
         facwd = 31.415926d0 *rcald**2*eps
         sum    = sum    + facwd*p*LOG(1. + EXP((EIN-vbw)/eps))/EIN
C
C        momentum decomposition
C
         DO il = 1, NDLW
            gl = FLOAT(il - 1)
            vbwl = vbw + 0.5*HC**2*gl*(gl + 1.)/(REDm*AU*rcald**2)
            vbl  = vb  + 0.5*HC**2*gl*(gl + 1.)/(REDm*AU*rcal **2)
            aux = EXP((EIN - vbwl)/eps)
            sigl1(il) = sigl1(il)   + p*aux /(1. + aux )
            aux0= EXP((EIN - vbl )/eps)
            if(i1.eq.1) sigl0(il) =     aux0/(1. + aux0) ! uncoupled calc
         ENDDO
  100 ENDDO

      DO il = 1, NDLW
        if(sigl0(il).le.1.d-10) exit
      ENDDO
      NLW = min(NDLW,il)
C
C     Example of the format to create FUSION file
C
C     OPEN(111,file='FUSION')
C     DO il = 1, NDLW
C     if(Stl(il).le.1.d-16) exit
C       write(111,*) sigl0(il)
C     ENDDO
C     NLW = min(NDLW,il)
C     CLOSE(111)

      WRITE (8,34) EIN,sum,sumunc

      s0 = 0.d0
      s1 = 0.d0
      s2 = 0.d0
      su0 = 0.d0
      su1 = 0.d0
      su2 = 0.d0
      DO il = 1, NLW
         flo = FLOAT(il - 1)
         s0 = s0 + sigl1(il)
         s1 = s1 + flo*sigl1(il)
         s2 = s2 + flo**2*sigl1(il)
         su0 = su0 + sigl0(il)
         su1 = su1 + flo*sigl0(il)
         su2 = su2 + flo**2*sigl0(il)
      ENDDO
      DO n = 1, NSCc
         FLAm(n) = flamem(n)
      ENDDO

      WRITE (8,35)     s1/s0  , s2/s0 ,
     >                 su1/su0,su2/su0

   34 FORMAT(/,'  CROSS SECTIONS FOR Ecms =',1F6.1,' MEV ARE:',/,
     >'  COUPLED =',1PE10.3E2,' mb       UNCOUPLED =',1PE10.3E2,' mb')
   35 FORMAT(/,'  ANGULAR MOMENTUM DISTRIBUTIONS'/,
     >'  COUPLED   - <L>=',F6.1,' <L2>=',F7.0/
     >'  UNCOUPLED - <L>=',F6.1,' <L2>=',F7.0/)


      if(rkey.lt.-2.5) then
        write(8,*) ' CCFUS uncoupled fusion barrier  used !'
        DO k = 1, NLW
         Stl(k) = sigl0(k)
        ENDDO
        SIG = sumunc
      else
        write(8,*) ' CCFUS   coupled fusion barrier  used !'
        DO k = 1, NLW
         Stl(k) = sigl1(k)
        ENDDO
        SIG = sum
      endif
      WRITE (8,*) ' CCFUS XS =',sngl(SIG),' mb, Lmax=',NLW
      WRITE (8,*) ' =================================================='
      CSFus = SIG
      ABScs = SIG

      RETURN
      END


      SUBROUTINE BAR(Rbar,Vb,Homega)
C
C
C COMMON variables
C
      DOUBLE PRECISION A0R, AU, ETAk, HC, RAB, REDm, V0R
      COMMON /ONE   / V0R, A0R, RAB, REDm, AU, HC, ETAk
C
C Dummy arguments
C
      DOUBLE PRECISION Homega, Rbar, Vb
C
C Local variables
C
      DOUBLE PRECISION dr, rb, rmax, s, v0, v1, v2, y1, y2
C
      rmax = 20.
      dr = .5
      s = -1.
      y1 = -1.
      rb = rmax
  100 CALL POTENT(rb,v0,v1,v2)
      y2 = v1
      IF (y2*y1.LE.0.D0) THEN
         IF (dr.LT.0.01D0) THEN
            Homega = HC*SQRT(( - v2/(REDm*AU)))
            Vb = v0
            Rbar = rb
            RETURN
         ELSE
            dr = .5*dr
            s = -s
            y1 = y2
         ENDIF
      ENDIF
      rb = rb + s*dr
      IF (rb.LE.rmax .AND. rb.GE.0.8D0) GOTO 100
      Homega = -1.D0
      RETURN
      END

      SUBROUTINE POTENT(Rb,V0,V1,V2)
C
C COMMON variables
C
      DOUBLE PRECISION A0R, AU, ETAk, HC, RAB, REDm, V0R
      COMMON /ONE   / V0R, A0R, RAB, REDm, AU, HC, ETAk
C
C Dummy arguments
C
      DOUBLE PRECISION Rb, V0, V1, V2
C
C Local variables
C
      DOUBLE PRECISION v1n, v2n, vn, x, x2, x3
C
      x = Rb
      x2 = x*x
      x3 = x2*x
      V0 = ETAk/x
      V1 = -ETAk/x2
      V2 = 2*ETAk/x3
      CALL POT(x,vn,v1n,v2n)
      V0 = V0 + vn
      V1 = V1 + v1n
      V2 = V2 + v2n
      END


      SUBROUTINE POT(Rr,Ur,Dur,Ddur)
C
C COMMON variables
C
      DOUBLE PRECISION A0R, AU, ETAk, HC, RAB, REDm, V0R
      COMMON /ONE   / V0R, A0R, RAB, REDm, AU, HC, ETAk
C
C Dummy arguments
C
      DOUBLE PRECISION Ddur, Dur, Rr, Ur
C
C Local variables
C
      DOUBLE PRECISION arg, arg1, ss
C
      ss = Rr - RAB
      arg = EXP(( - ss/A0R))
      arg1 = 1. + arg
      Ur = -V0R*arg/arg1
      Dur = -Ur/arg1/A0R
      Ddur = Dur*(1. - 2./arg1)/A0R
      END


