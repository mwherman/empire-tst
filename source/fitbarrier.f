Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-01-25 04:20:05 +0100 (Mi, 25 Jän 2012) $
Ccc   * $Id: fitbarrier.f,v 1.7 2009/06/15 21:52:21 Capote Exp $

      SUBROUTINE WKBFIS(Ee, nnuc, tfdd, tdirp, tabsp)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      DOUBLE PRECISION vdef_1d(NFISBARPNT), eps_1d(NFISBARPNT)
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr

      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr
      COMMON /NUMBAR1/Vheigth,Vwidth,Vpos
      COMMON /VARGS/ Uexc, Smiu, K


      COMMON /VBAR  / vbarex, ho,faza2
      REAL*8 VBArex(NFPARAB),HO(NFPARAB) 

      DOUBLE PRECISION Vheigth(NFPARAB),Vwidth(NFPARAB),Vpos(NFPARAB+1)
      DOUBLE PRECISION ee,uexc
      DOUBLE PRECISION phase(2*NFPARAB),phase_h(NFPARAB)

      DOUBLE PRECISION dmom,dnum, rmiu, Smiu, W

      DOUBLE PRECISION exp2del(NFPARAB), exm2del(NFPARAB)
      DOUBLE PRECISION tdirp(NFPARAB,NFPARAB), tabsp(NFPARAB,NFPARAB)
      DOUBLE PRECISION phasep(NFPARAB),delt(NFPARAB),deltt(NFPARAB),
     &                 tfdd(NFPARAB), tdr

      INTEGER j,k,kh,kw,iphas_opt

 
      REAL*8  abserr, epsa, epsb, phase_sub, faza2
      LOGICAL discrete

C     FUNCTIONS
      REAL*8   Fmoment1,  GaussLegendre41
      EXTERNAL Fmoment1

      rmiu = 0.054d0 * A(Nnuc) * * (5.d0/3.d0)
      smiu = DSQRT(0.5d0 * rmiu)
      uexc = Ee
      discrete = .TRUE.

      DO k = 1, 2 * NRBar
        phase(k) = 0.d0
      ENDDO
c-----reasigning humps and wells
      DO kh = 1, NRBar,2
         Vheigth(kh) = VBArex(int(kh/2)+1)
         Vwidth(kh) = h(1,int(kh/2)+1)
      ENDDO

      DO kw = 2, NRBar,2
         Vheigth(kw) = VBArex(NRHump+int(kw/2))
         Vwidth(kw) = h(1,NRHump+int(kw/2))
      ENDDO
     
      IF(FISbar(Nnuc).EQ.3.)THEN
         DO j = 1, nextr
            Vpos(j) = DEFfis(j)
         ENDDO
         Vpos(nextr+1) = 100.d0 
      ENDIF
      
C---- Momentum integrals are calculated
      iphas_opt=0  ! phases calculated on decoupled parabolas

      IF(FISbar(Nnuc).EQ.3 .or. FISopt(Nnuc).GT.0 ) iphas_opt=1
      IF(FISbar(Nnuc).EQ.3)THEN
        CALL PHASES(ee, phase, phase_h, nnuc, iphas_opt, discrete)
      ELSE
        CALL PHASES_Parab(ee, nnuc, phase, discrete)
      ENDIF

C-----Calculating transmission from phases
      DO ih = 1, nrbar, 2
         TFDd(ih/2 + 1) = 1.d0/
     &                    (1.d0 + DEXP(2.d0 *  phase(ih)))
      ENDDO

      IF(FISopt(Nnuc).EQ.0)RETURN  

C-------Imaginary potential strengths
      w = 0.d0
      DO iw = 2, nrbar, 2
         phasep(iw/2 + 1) = phase(iw)
      ENDDO

      DO iw = 2, nrbar,2
         deltt(iw) = 0.d0
         dmom = max(Vheigth(iw-1),Vheigth(iw+1))
c         W = wimag(1)*2.d0 * (Ee - Vheigth(iw)) /
c     &       ((dmom - Vheigth(iw)) *
c     &       (1.d0 + (1.d0/wimag(1))*  ! to get w(2) and w(3)
c     &        dexp( - (Ee - dmom) / wimag(iw/2+1)))) 

c         w = wimag(1)+wimag(2)*(Ee - Vheigth(iw))+
c     &              wimag(3)*(Ee - Vheigth(iw))**2
cc         IF(iw.eq.2)w = wimag(1)+wimag(3)*(Ee - Vheigth(iw))
cc         IF(iw.eq.4)w = wimag(2)+wimag(3)*(Ee - Vheigth(iw))
         w = wimag(1)+wimag(2)*(Ee - Vheigth(iw))+
     &              wimag(3)*dexp(Ee - dmom)


         if(Ee.le. Vheigth(iw)) W = 0.d0
c         if(Ee.gt. dmom) W = 1.d0          
         if(ee.lt.Vheigth(iw )) phase(iw) = 0.d0
         deltt(iw) = W * phase(iw)
      ENDDO

      DO iw = 2, nrbar, 2
         delt(iw/2 + 1) =(1.d0)* deltt(iw)
      ENDDO
      DO iw = 2, NRWel + 1
         IF(2.d0*delt(iw).GT.EXPmax)delt(iw)=EXPmax/2.d0
         exp2del(iw) = dexp( 2.d0*delt(iw))
         exm2del(iw) = dexp(-2.d0*delt(iw))
         if (delt(iw).eq.0.d0) exp2del(iw) = 1.d0
         if (delt(iw).eq.0.d0) exm2del(iw) = 1.d0
      ENDDO   
c-----Direct transmission coefficients
      DO ih = 1, nrhump
         DO ih1 = 1, nrhump
            tdirp(ih, ih1) = 0.d0
            tdirp(ih1, ih) = 0.d0
            IF(ih.EQ.ih1) tdirp(ih, ih1) = tfdd(ih)
         ENDDO
      ENDDO
c-----direct forward
      DO ih1 = nrhump, 2, -1
         DO ih = ih1 - 1, 1, -1
            dmom = (1.d0 - tdirp(ih, ih)) *
     &             (1.d0 - tdirp(ih+1, ih1))
            dnum= exp2del(ih + 1) + 2.d0*  dSQRT(dmom) *
     &           dCOS(2.d0 * phasep(ih + 1)) +
     &           dmom* exm2del(ih + 1)
            
            IF(dnum.eq.0.d0)THEN
               tdirp(ih, ih1) = 0.d0
            ELSE
               tdirp(ih, ih1) = tdirp(ih, ih) *  tdirp(ih+1, ih1)/
     &                          dnum
            ENDIF   
         ENDDO
      ENDDO

c-----direct backward   
      DO ih1 = 1, nrhump - 1
         DO ih = ih1 + 1, nrhump - 1
                 dmom = (1.d0 - tdirp(ih, ih)) *
     &                  (1.d0 - tdirp(ih+1, ih1))
                 tdirp(ih, ih1) = tdirp(ih, ih) *  tdirp(ih+1, ih1)/
     &                         (exp2del(ih + 1) + 2.d0 * dSQRT(dmom) *
     &                          dCOS(2.d0 * phasep(ih + 1)) +
     &                          dmom * exm2del(ih + 1))
         ENDDO
      ENDDO
c-----to be generalized, valid now only for Th like cases
      IF(ee.LE.Vheigth(4))THEN
         epsa = Vpos(3)- SQRT(Vheigth(3)-Ee)/
     &                   (SMIu*Vwidth(3))
                  epsb = Vpos(5)+ SQRT(Vheigth(5)-Ee)/
     &                   (SMIu*Vwidth(5))
                  dmom = GaussLegendre41(Fmoment1,epsa,epsb,abserr)
                  IF(dmom.gt.0.d0 .and. abserr.gT.dmom*0.03) THEN
                     write(*,*) ' WARNING: For extremum ',k,
     &                       ' phase integral is not accurate (',
     &                  sngl(abserr/dmom*100.d0),' %)'
                  ENDIF
                  phase_sub = min(dmom,50.d0)
                  tdirp(2,3) = 1.d0/(1.d0 + DEXP(2.d0 *  phase_sub))
                  tdirp(3,2)=tdirp(2,3)
      ENDIF

c--------Absorption coefficients
c--------forward
       DO iw = 1, nrwel
          DO iw1 = iw + 1, nrwel + 1
             IF(delt( iw1).GT.0.D0) THEN
                dmom = (1.d0 - tdirp(iw, iw1 - 1)) *
     &                 (1.d0 - tdirp(iw1, nrhump))
                tdr  = tdirp(iw, iw1 - 1) *  tdirp(iw1, nrhump)/
     &                 (exp2del(iw1) + 2.d0 * dSQRT(dmom) *
     &                  dCOS(2.d0 * phasep(iw1)) +
     &                  dmom * exm2del(iw1))

                tabsp(iw, iw1) = tdr * (exp2del(iw1)
     &                           - (1.d0 - tdirp(iw1, nrhump)) *
     &                           exm2del(iw1) -
     &                           tdirp(iw1, nrhump)) /tdirp(iw1, nrhump)
             ELSE
                tabsp(iw, iw1) = 0.D0
             ENDIF
          ENDDO
       ENDDO
c------backward
       DO iw = nrwel + 1, 3, -1
          DO iw1 = iw -1, 2, -1
             IF(delt( iw1).GT.0.D0) THEN
                dmom = (1.d0 - tdirp(iw - 1, iw1)) *
     &                 (1.d0 - tdirp(iw1-1, 1))
                tdr  = tdirp(iw-1, iw1) *  tdirp(iw1-1, 1)/
     &                 (exp2del(iw1) + 2.d0 * dSQRT(dmom) *
     &                  dCOS(2.d0 * phasep(iw1)) +
     &                  dmom * exm2del(iw1))
                tabsp(iw, iw1) = tdr * (exp2del(iw1)
     &                          - (1.d0-  tdirp(iw1-1, 1)) *
     &                           exm2del(iw1) -
     &                           tdirp(iw1-1, 1)) /tdirp(iw1-1, 1)
             ELSE
                tabsp(iw, iw1) = 0.D0
             ENDIF
          ENDDO
       ENDDO
  
      RETURN
      END

c================================================================
      SUBROUTINE PHASES_Parab(ee, nnuc, phase, discrete)
C================================================================
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'


      COMMON /VBAR  / vbarex, ho,faza2
      COMMON/ PARAB/  smiu,EPSil, EJOin, VJJ,ho_p
      COMMON /VARGS/ Uexc, Smiu_p, Kbarr

      REAL*8 VBArex(NFPARAB),HO(NFPARAB) 
      REAL*8 smiu,EJOin(2*NFPARAB), EPSil(NFPARAB), VJJ(NFPARAB),
     &       HO_p(NFPARAB)
 

      REAL*8 uexc,smiu_p
      REAL*8 rmiu
      REAL*8 Einters(2*NFPARAB)
      REAL*8 phase(NFPARAB)

      LOGICAL discrete
      REAL*8 ftmp, es, ee, dmom, faza2, abserr
      REAL*8  FmomentParab, GaussLegendre41
      EXTERNAL FmomentParab

      INTEGER kbarr
C     INTEGER iphas_opt

      rmiu = 0.054d0*A(Nnuc)**(5.d0/3.d0)        !mu in the formula
      smiu = DSQRT(0.5d0*rmiu)
      smiu_p=smiu
      uexc=ee

      DO k=1, NRBar,2
         VJJ(k) = VBArex(int(k/2)+1)
      ENDDO
      DO k=2, NRBar,2
         VJJ(k) = VBArex(NRHump+int(k/2))
      ENDDO

      DO k=1,NRBAR
         ho_p(k)=ho(k)
      ENDDO

      IF(.NOT.discrete)THEN
         DO k=1, NRBar,2
            VJJ(k) = EFB(int(k/2)+1)
            ho(k)  = hcont(int(k/2)+1)  
         ENDDO
         DO k=2, NRBar,2
            VJJ(k) = EFB(NRHump+int(k/2))
            ho(k)  = hcont(NRHump+int(k/2))
         ENDDO
      ENDIF

C-----deformations at saddles and wells and matching points-----------
C     Fission barriers are modelled by NRBar parabolas
C     EPSil(i) are the parabolas vortex
C     EJOin(i) are the corresponding deformation at which parabolas join
      EPSil(1) = SQRT(VJJ(1))/(SMIu*HO(1))
      EJOin(2) = EPSil(1)
     &           + SQRT((VJJ(1) - VJJ(2))/(1.D0 + (HO(1)/HO(2))**2))
     &           /(SMIu*HO(1))
      EJOin(1) = 0.d0!2*EPSil(1) - EJOin(2)
      DO k = 2, NRBar
         EJOin(2*k - 1) = EJOin(2*(k - 1))
         EPSil(k) = EJOin(2*(k - 1)) + (HO(k - 1)/HO(k))
     &              **2*(EJOin(2*(k-1)) - EPSil(k - 1))
         IF (k.LT.NRBar) EJOin(2*k) = EPSil(k)
     &        + SQRT(( - 1)**k*(VJJ(k+1)- VJJ(k))
     &        /(1.D0 + (HO(k)/HO(k+1))**2))/(SMIu*HO(k))
      ENDDO
      EJOin(2*NRBar) = 2*EPSil(NRBar) - EJOin(2*NRBar - 1)
      EJOin(2*NRBar) = EPSil(NRBar) +SQRT(VJJ(NRBar))/(SMIu*HO(NRBar))

      DO j = 1, 2*NRBar
         einters(j) = -1.
      ENDDO

      DO j = 1, NRBar
         ftmp = ( - 1)**j*(Ee - VJJ(j))
         IF (ftmp.GE.0.D0) THEN
            IF (Ee.EQ.VJJ(j)) THEN
               einters(2*j - 1) = epsil(j)!Ee
               einters(2*j) = epsil(j)!Ee
               cycle
            ENDIF
            es = SQRT(ftmp)/(SMIu*HO(j))
            einters(2*j - 1) = EPSil(j) - es
            einters(2*j) = EPSil(j) + es
         ENDIF
      ENDDO    

C     IF(iphas_opt.NE.0)THEN
         DO j=2,2*NRBar-1,2
            IF(einters(j).LT.ejoin(j))THEN
               einters(j+1)=einters(j)
            ELSE   
               einters(j)=einters(j+1)
            ENDIF
         ENDDO 
C     ENDIF

C     Momentum integrals 
      UEXc = ee
      DO k = 1, NRBar
         phase(k) = 0.D0
         IF (einters(2*k).GE.0. .AND. einters(2*k - 1).GE.0.)then
            dmom = GaussLegendre41(FmomentParab,
     &             einters(2*k - 1),einters(2*k),abserr) 
         ELSE
            dmom =(-1)**(k+1)* pi * (Vjj(k) - ee)/ho(k)
         ENDIF
         phase(k)   = min(dmom,50.d0)  
      ENDDO              

      RETURN
      END
C========================================================================
      REAL*8 function FmomentParab(Eps)
C
C     Integrand (To be called from Gauss-Legendre integration routine)
C     To be defined as external function
C
      IMPLICIT NONE
      REAL*8 Eps, Vdef,Uexc, Smiu
      INTEGER Kbarr
      COMMON /VARGS/ Uexc, Smiu, Kbarr
   
      FmomentParab = 2.d0*Smiu* DSQRT( DABS (Uexc - Vdef(Eps)) )
      RETURN
      END


      REAL*8 FUNCTION VDEF(Eps)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C COMMON variables

      COMMON/PARAB/  smiu,EPSil, EJOin, VJJ,ho
      COMMON /VARGS/ Uexc, Smiu_p, Kbarr
 
      REAL*8 smiu,EJOin(2*NFPARAB), EPSil(NFPARAB), VJJ(NFPARAB),
     &       HO(NFPARAB), uexc,smiu_p,eps
      INTEGER j
C
C     calculation of the deformation potential energy
      VDEF = 0.D0
c      IF (Eps.LE.EJOin(2)) THEN
c         VDEF = VJJ(1) - (SMIu*HO(1)*(Eps - EPSil(1)))**2
c         RETURN
c      ENDIF
c      IF (Eps.GE.EJOin(2*NRBar - 1)) THEN
c         VDEF = VJJ(NRBar) - (SMIu*HO(NRBar)*(Eps - EPSil(NRBar)))**2
c         RETURN
c      ENDIF

c      DO j = 2, NRBar - 1
      DO j = 1, NRBar
         IF (Eps.GE.EJOin(2*j - 1) .AND. Eps.LE.EJOin(2*j)) THEN
            VDEF = VJJ(j) + ( - 1)**j*(SMIu*HO(j)*(Eps - EPSil(j)))
     &                **2
            RETURN
         ENDIF
      ENDDO
      RETURN
      END

C============NUMERICAL BARRIERS=================================

      SUBROUTINE NUMBARR(Nnuc,Vbarex,ho)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr
      COMMON /NUMBAR1/Vheigth,Vwidth,Vpos
    

      DOUBLE PRECISION vdef_1d(NFISBARPNT), eps_1d(NFISBARPNT)
      DOUBLE PRECISION Vheigth(NFPARAB),Vwidth(NFPARAB),Vpos(NFPARAB+1)
      DOUBLE PRECISION centr, heigth, width, ucentr, uheigth, uwidth
      DOUBLE PRECISION rmiu,smiu

      INTEGER npoints, iiextr(0:2*NFPARAB), nextr

      DOUBLE PRECISION VBArex(NFPARAB), VJJ(NFPARAB), HO(NFPARAB)
      DOUBLE PRECISION EJOin(2*NFPARAB), EPSil(NFPARAB)
C     Functions
      INTEGER Find_Extrem

      rmiu = 0.054d0*A(Nnuc)**(5.d0/3.d0)
      smiu = DSQRT(0.5d0*rmiu)
      nrsm = NRSmooth(Nnuc)

c-----Generating numerical barrier starting from parabolas' parameters
      IF(FISbar(Nnuc).LE.2.)THEN
         DO k=1, NRBar,2
            VJJ(k) = VBArex(int(k/2)+1)
         ENDDO
         DO k=2, NRBar,2
            VJJ(k) = VBArex(NRHump+int(k/2))
         ENDDO

C--------deformations at saddles and wells and matching points-----------
C        Fission barriers are modelled by NRBar parabolas
C        EPSil(i) are the parabolas vortex
C        EJOin(i) are the corresponding deformation at which parabolas join
         EPSil(1) = SQRT(VJJ(1))/(SMIu*HO(1))
         EJOin(2) = EPSil(1)
     &              + SQRT((VJJ(1) - VJJ(2))/(1.D0 + (HO(1)/HO(2))**2))
     &              /(SMIu*HO(1))
         EJOin(1) = 2*EPSil(1) - EJOin(2)
         DO k = 2, NRBar
            EJOin(2*k - 1) = EJOin(2*(k - 1))
            EPSil(k) = EJOin(2*(k - 1)) + (HO(k - 1)/HO(k))
     &                 **2*(EJOin(2*(k-1)) - EPSil(k - 1))
            IF (k.LT.NRBar) EJOin(2*k) = EPSil(k)
     &                                   + SQRT(( - 1)**k*(VJJ(k+1)
     &                                   - VJJ(k))
     &                                   /(1.D0 + (HO(k)/HO(k+1))**2))
     &                                   /(SMIu*HO(k))
         ENDDO
         EJOin(2*NRBar) = 2*EPSil(NRBar) - EJOin(2*NRBar - 1)
         defstep = 0.01d0
         npoints= int((EPSil(NRBar)+ SQRT(VJJ(NRBar))/(SMIu*HO(NRBar)))/
     &            defstep)

         DO id=1,  npoints
            eps_1d(id) = defstep*id
            IF (eps_1d(id).LE.EJOin(2)) vdef_1d(id) =
     &      VJJ(1) - (SMIu*HO(1)*(eps_1d(id) - EPSil(1)))**2
            IF (eps_1d(id).GE.EJOin(2*NRBar - 1)) vdef_1d(id) =
     &      VJJ(NRBar) - (SMIu*HO(NRBar)*(eps_1d(id) - EPSil(NRBar)))**2
            DO j = 2, NRBar - 1
               IF (eps_1d(id).GE.EJOin(2*j - 1) .AND.
     &             eps_1d(id).LE.EJOin(2*j))
     &             vdef_1d(id) = VJJ(j) + ( - 1)**j*(SMIu*HO(j)*
     &                           (eps_1d(id) - EPSil(j)))**2
            ENDDO
         ENDDO
       ENDIF

C-----Finding maxima and minima of the deformation energy curve
C                   initializes iiextr() and nextr
 100  nextr = Find_Extrem(Nnuc)
      DO j = 1, nextr
         Vpos(j) = DEFfis(j)
      ENDDO
      Vpos(nextr + 1) = 100.d0
C==================================================================
C     Fitting parabola
      DO j = 1, nextr
        CALL ParabFit(iiextr(j), nrsm, rmiu, eps_1d, vdef_1d,
     &        centr,  heigth,  width, ucentr, uheigth, uwidth)
        IF(width.LT.0.05d0) CYCLE      ! Skipping very narrow peaks
c       write(*,*) ' Def: ',sngl(EPS_1d(iiextr(j))),
c     &         ' (',sngl(centr),' +/- ',sngl(ucentr),')'
c       write(*,*) ' Heigth :',sngl(Vdef_1d(iiextr(j))),
c     &         ' (',sngl(heigth),' +/- ',sngl(uheigth),')'
c       write(*,*) ' Width :', sngl(width), ' +/- ', sngl(uwidth)
C--------------------------------------------------
C       Initializes parabola's parameters
C
C       The real height and position of the barrier are used
c       (not the fitted parabola height or center)

        nextr = nrbar
        Vheigth(j) = Vdef_1d(iiextr(j))
        Vwidth(j) = width
        Vpos(j) = EPS_1d(iiextr(j))
      ENDDO
C     Vpos(0) = 0.d0
      Vpos(nextr+1) = 100.d0

      RETURN
      END


c================================================================
      SUBROUTINE PHASES(uexcit1,phase,phase_h,nnuc,iphas_opt,discrete)
C================================================================
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 vdef_1d(NFISBARPNT), eps_1d(NFISBARPNT)
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr

      COMMON /NUMBAR1/Vheigth,Vwidth,Vpos
      COMMON /VARGS/ Uexc, Smiu, K

      REAL*8 Vheigth(NFPARAB),Vwidth(NFPARAB),Vpos(NFPARAB+1)
      REAL*8 uexcit1, phase(2*NFPARAB),phase_h(NFPARAB),
     &                phase_w(NFPARAB)
C-------------------------------------------------------------------
      REAL*8 Uexc, Smiu
      REAL*8 dmom, abserr, rmiu, epsa, epsb
      INTEGER K,iphas_opt
      LOGICAL discrete
C     FUNCTIONS
      REAL*8   Fmoment1, FmomentParab, GaussLegendre41, FindIntersect
      EXTERNAL FmomentParab, Fmoment1

      rmiu = 0.054d0*A(Nnuc)**(5.d0/3.d0)
      Uexc   = uexcit1
      smiu = DSQRT(0.5d0 * rmiu)
c
      DO k = 1, 2 * NRBar
         phase(k) = 0.d0
      ENDDO
     
      IF(.NOT.discrete)THEN
         i=0
         DO k=1, NRHump     
            Vheigth(k + i) = EFB(k)
            Vwidth (k + i) = hcont(k)
            i=i+1
         ENDDO
         i=1
         DO k= NRHump + 1, NRBar     
            Vheigth(k -nrhump + i) = EFB(k)
            Vwidth (k -nrhump + i) = hcont(k)
            i=i+1
         ENDDO
      ENDIF

C---- Momentum integrals are calculated
      IF(iphas_opt.eq.1) THEN
         DO k = 1, nextr                                  ! humps and wells
            IF(mod(k, 2).eq.1) then                       ! humps
               IF(uexcit1.ge.Vheigth(k)) THEN             ! Hill-Wheeler formula
                  dmom = pi * (Vheigth(k) - uexcit1)/Vwidth(k)
                  phase(k)   = min(dmom,50.d0)
                ELSE                                      !WKB         
                   epsa = FindIntersect(uexcit1,iiextr(k-1),iiextr(k),
     &                   .false.)
                   epsb = FindIntersect(uexcit1,iiextr(k),iiextr(k+1),
     &                    .false.)
                   dmom = GaussLegendre41(Fmoment1,epsa,epsb,abserr)
                   IF(dmom.gt.0.d0 .and. abserr.gt.dmom*0.053) THEN
                        write(*,*) ' WARNING: For extremum ',k,
     &                       ' phase integral is not accurate (',
     &                        sngl(abserr/dmom*100.d0),' %)',uexcit1,
     &                  vheigth(k),vwidth(k),discrete
                   ENDIF
                   phase(k) = min(dmom,50.d0)
                ENDIF
            ELSE                                          ! WELLS
               IF(uexcit1.LE.Vheigth(k)) THEN
                  phase(k) = 0.d0
               ELSE                                       ! WKB
                  epsa = FindIntersect(uexcit1,iiextr(k-1),iiextr(k),
     &                   .true.)
                  epsb = FindIntersect(uexcit1,iiextr(k),iiextr(k+1),
     &                   .true.)
                  dmom = GaussLegendre41(Fmoment1,epsa,epsb,abserr)
                  IF(dmom.gt.0.d0 .and. abserr.gT.dmom*0.053) THEN
                     write(*,*) ' WARNING: For extremum ',k,
     &                       ' phase integral is not accurate (',
     &                  sngl(abserr/dmom*100.d0),' %)',uexcit1,
     &                  vheigth(k),vwidth(k),discrete
                  ENDIF
                  phase(k) = min(dmom,50.d0)
               ENDIF
            ENDIF
         ENDDO
      ELSE
        DO k=1, nextr                                ! only parabolic barriers
          dmom = pi*(Vheigth(k) - uexcit1)/Vwidth(k) !Hill-Wheeler
          phase(k)   = min(dmom,50.d0)
        ENDDO
      ENDIF
C
      DO k=1, nextr  
         IF(mod(k, 2).eq.1) then   
            phase_h(k-int(k/2))=phase(k)
         ELSE
            phase_w(k-int(k/2))=phase(k)
         ENDIF     
      ENDDO

      RETURN
      END


      REAL*8 function Fmoment1(Eps)
C
C     Integrand (To be called from Gauss-Legendre integration routine)
C
C     To be defined as external function
C
      IMPLICIT NONE
      REAL*8 Eps, Vdef1
      REAL*8 Uexc, Smiu
      INTEGER Kbarr
      COMMON /VARGS/ Uexc, Smiu, Kbarr
      Fmoment1 = 2.d0*SMIU* DSQRT( DABS (UEXC - Vdef1(Eps)) )
      return
      end


      
      REAL*8 function Vdef1(EPS)
      REAL*8 EPS
C
C     This function calculates real shape of the deformation energy
C     by linear interpolation to obtain the value of the barrier Vdef
C     at deformation EPS (needed to integrte this function)
C
C     Called by gaussian integration
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      REAL*8 vdef_1d(NFISBARPNT), eps_1d(NFISBARPNT)
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr
C
C     Local variables
      INTEGER idef
      REAL*8 vi, vip, ei, eip
C
C     The four lines below is a very simple (and unefficient) search
C     Should be replaced by efficient search routine to locate the element idef of the array EPS_1D()
      idef=1
      do while (EPS.GT.EPS_1D(idef) .and. idef.LE.npoints)
        idef = idef + 1
      enddo
      if (idef.ne.1) idef = idef - 1

      vi  = Vdef_1d(idef)
      vip = Vdef_1d(idef+1)
      ei  = EPS_1d(idef)
      eip = EPS_1d(idef+1)

      if(ei.eq.eip) then
C       Special case treated here to avoid division by zero
C       We assume that in this case vi = vip
        Vdef1 = vi
        return
      endif

      Vdef1 = vi + (EPS-ei)/(eip-ei)*(vip-vi)
      return
      end

c===================================================
      SUBROUTINE ParabFit(Imax,Npfit,RMIU,EPSs,VDEFf,
     &                     CENTR,  HEIGTH,  WIDTH,
     &                    uCENTR, uHEIGTH, uWIDTH)
      IMPLICIT NONE
      INTEGER Imax  ! Position of the maximum
      INTEGER Npfit ! Number of points to fit
      REAL*8 RMIU   ! MIU = 0.054d0*ANUC**(5.d0/3.d0)
      REAL*8 EPSs(*) ! Deformation Array        (X)
      REAL*8 VDEFf(*)! Deformation Energy Array (Y)
      REAL*8  CENTR,  HEIGTH,  WIDTH ! BARRIER'S PARAMETER
      REAL*8 uCENTR, uHEIGTH, uWIDTH ! BARRIER'S PARAMETER UNCERTAINTIES

C     Local variables
      integer i,npts,ierr
      real*8 x(1024),y(1024),aa(3),siga(3),chisqr

      npts = 0

      do i = Imax - Npfit, Imax + Npfit
        npts = npts + 1
        x(npts) = EPSs(i) - EPSs(imax)
        y(npts) = VDEFf(i)
      enddo


C
C FITS A POLYNOMIAL OF THE FORM:
C   Y=A(1)+A(2)*X+A(3)*X**2+...+A(NTERMS)*X**(NTERMS-1)
C THOUGH THE NPTS POINTS (X,Y)
C IERR=0 OK
C IERR=2 SINGULAR MATRIX
C IERR=3 REQUIREMENTS FOR USE OF THIS ROUTINE NOT FULLFILLED
C
      CALL WPLFT(npts,3,ierr,1,x,y,aa,siga,chisqr)
C     do i = 1,npts
C       write(*,'(1x,F5.3,2x,d15.8,3x,d15.8)') x(i), y(i),
C    &              a(1)+a(2)*x(i)+a(3)*x(i)**2
C     enddo
C     write(*,*) a(1),a(2),a(3)
C     write(*,*) sigmaa(1),sigmaa(2),sigmaa(3)
C
c      write(*,*) ' PARABOLIC FITTING CHISQR = ',sngl(CHISQR)

       HEIGTH = aa(1)
      uHEIGTH = siga(1)
       WIDTH  =  SQRT(2.d0*abs(  aa(3))/RMIU)
      uWIDTH  =  SQRT(2.d0*abs(siga(3))/RMIU)
       CENTR  = EPSs(imax)
      IF(aa(3).ne.0.d0) CENTR = 0.5d0 * aa(2) / aa(3)
      uCENTR  = ABS(CENTR) * SQRT((siga(2)/aa(2))**2 +
     &          (siga(3)/aa(3))**2)
      CENTR = EPSs(imax) + CENTR
      return
      end

      SUBROUTINE WPLFT(NPTS,NTERMS,IERR,MODE,X,Y,AA,SIGMAA,CHISQR)

C FITS A POLYNOMIAL OF THE FORM:
C   Y=A(1)+A(2)*X+A(3)*X**2+...+A(NTERMS)*X**(NTERMS-1)
C THOUGH THE NTPS POINTS (X,Y)
C MODE 0 = WEIGTH 0 != UNWEIGHTED
C IERR=0 OK
C IERR=2 SINGULAR MATRIX
C IERR=3 REQUIREMENTS FOR USE OF THIS ROUTINE NOT FULLFILLED
C REQUIREMENTS ARE:
C        MAX NUM TERMS = 12
C        MIN NUM TERMS = 2
C        NTPS >= NTERMS
        INTEGER MAXDEG
        PARAMETER (MAXDEG=3)

        INTEGER NPTS, NTERMS, IERR, MODE
        REAL*8 X(NPTS), Y(NPTS), W(NPTS)
        REAL*8 AA(NTERMS), SIGMAA(NTERMS), CHISQR
        DOUBLE PRECISION ARRAY(MAXDEG,MAXDEG)

        DOUBLE PRECISION XMEAN(MAXDEG)
        DOUBLE PRECISION SIGMAX(MAXDEG), WMEAN, FNPTS
        DOUBLE PRECISION SUM,YMEAN,DET,FREE1,SIGMA,VARNCE,CHISQ
        DOUBLE PRECISION R(MAXDEG)
        INTEGER WEIGHT
        PARAMETER (WEIGHT=0)
        INTEGER DEG,I,J,K

        IERR = 0
        DEG=NTERMS-1
        IF(DEG.LT.1.OR.DEG.GT.MAXDEG.OR.NPTS.LT.NTERMS) THEN
          IERR=3
          RETURN
        ENDIF

C       INITIALIZE SUMS AND ARRAYS
        SUM = 0.D0
        YMEAN=0.D0
        SIGMA = 0.D0
        CHISQ = 0.D0
        AA(1) = 0.
        SIGMAA(1) = 0.
        DO 28 J=1,DEG
        XMEAN(J)=0.D0
        SIGMAX(J)=0.D0
        R(J)=0.D0
        AA(1+J) = 0.
        SIGMAA(1+J) = 0.
C       DO 28 K=1,DEG
C28      ARRAY(J,K)=0.D0
        DO 28 K=1,NTERMS
28      ARRAY(J,K)=0.D0


C       ACCUMULATE WEIGHTED SUMS

        DO 50 I = 1, NPTS
        W(I) = ABS(Y(I))
        IF( MODE.NE.WEIGHT ) W(I) = 1.
        SUM = SUM + DBLE(W(I))
C       YMEAN = Sy
        YMEAN = YMEAN + DBLE(W(I)*Y(I))
C       XMEAN(1)=Sx, XMEAN(2)=Sxx
C       S=NPTS*(NPTS+1)/2
        DO 50 J = 1, DEG
50      XMEAN(J) = XMEAN(J) + DBLE(W(I)*X(I)**J)
        YMEAN = YMEAN / SUM
        DO 53 J = 1, DEG
53      XMEAN(J) = XMEAN(J) / SUM
        FNPTS = NPTS
        WMEAN = SUM / FNPTS
        DO 57 I = 1, NPTS
57      W(I) = W(I) / WMEAN

        DO 67 I=1,NPTS
        SIGMA=SIGMA+DBLE(W(I)*(Y(I)-YMEAN)**2)
        DO 67 J=1,DEG
        SIGMAX(J)=SIGMAX(J)+DBLE(W(I)*(X(I)**J-XMEAN(J))**2)
        R(J)=R(J)+DBLE(W(I)*(X(I)**J-XMEAN(J))*(Y(I)-YMEAN))
        DO 67 K=1,J
67      ARRAY(J,K)=ARRAY(J,K)+
     1             DBLE(W(I)*(X(I)**J-XMEAN(J))*(X(I)**K-XMEAN(K)))
        FREE1 = NPTS-1
        SIGMA=DSQRT(SIGMA/FREE1)
        DO 78 J=1,DEG
        SIGMAX(J)=DSQRT(SIGMAX(J)/FREE1)
        R(J)=R(J)/(FREE1*SIGMAX(J)*SIGMA)
        DO 78 K=1,J
        ARRAY(J,K)=ARRAY(J,K)/(FREE1*SIGMAX(J)*SIGMAX(K))
78      ARRAY(K,J)=ARRAY(J,K)

C       INVERT SYMMETRIC MATRIX

        CALL MATINVM( DEG, DET, ARRAY )
        IF(DET.NE.0.D0) GO TO 101
        IERR = 2
        RETURN

C       CALCULATE COEFFICIENTS

101     AA(1)=YMEAN
        DO J=1,DEG
          DO K=1,DEG
            AA(J+1)=AA(J+1)+R(K)*ARRAY(J,K)
          ENDDO
          AA(J+1)=AA(J+1)*SIGMA/SIGMAX(J)
          AA(1)=AA(1)-AA(J+1)*XMEAN(J)
        ENDDO

        DO 113 I = 1, NPTS
        YFIT = AA(1)
        DO 112 J = 1, DEG
112     YFIT = YFIT + AA(J+1)*X(I)**J
113     CHISQ = CHISQ + W(I)*(Y(I)-YFIT)**2
        FREEN = NPTS - NTERMS
        IF( FREEN.EQ.0 ) FREEN = 1.
        CHISQR = CHISQ*WMEAN/FREEN

C       CALCULATE UNCERTAINTIES

        IF( MODE.EQ.WEIGHT ) THEN
          VARNCE = 1./WMEAN
        ELSE
          VARNCE = CHISQR
        ENDIF
        DO 133 J = 1, DEG
        SIGMAA(1+J) = ARRAY(J,J)*VARNCE/(FREE1*SIGMAX(J)**2)
133     SIGMAA(1+J) = SQRT(SIGMAA(1+J))
        SIGMAA(1) = VARNCE / FNPTS
        DO 145 J = 1, DEG
        DO 145 K = 1, DEG
145     SIGMAA(1) = SIGMAA(1) + VARNCE*XMEAN(J)*XMEAN(K)*ARRAY(J,K) /
     1              (FREE1*SIGMAX(J)*SIGMAX(K))

        SIGMAA(1) = SQRT(SIGMAA(1))
        DO 160 J = 1, DEG
        DO 160 K = 1, DEG
160     ARRAY(J,K) = ARRAY(J,K)*VARNCE/(FREE1*SIGMAX(J)*SIGMAX(K))

        RETURN
        END
C++++++++++++++++++++++++++++

        SUBROUTINE MATINVM( NORDER, DET, ARRAY )

        INTEGER MAXDEG
        PARAMETER (MAXDEG=3)
        INTEGER NORDER,IK(MAXDEG),JK(MAXDEG)
        DOUBLE PRECISION DET
        DOUBLE PRECISION ARRAY(MAXDEG,MAXDEG)

        INTEGER I,J,K,L
        DOUBLE PRECISION AMAX,SAVE

        DET=1.D0
        DO 100 K=1,NORDER

C       FIND LARGEST ELEMENT ARRAY(I,J) IN REST OF MATRIX

        AMAX=0.D0
21      DO 30 I=K,NORDER
        DO 30 J=K,NORDER
        IF(DABS(AMAX).GT.DABS(ARRAY(I,J))) GO TO 30
        AMAX=ARRAY(I,J)
        IK(K)=I
        JK(K)=J
30      CONTINUE

C       INTERCHANGE ROWS AND COLUMNS TO PUT AMAX IN ARRAY(K,K)

        IF(AMAX.NE.0.D0) GO TO 41
        DET=0.D0
        RETURN
41      I=IK(K)
        IF(I-K)21,51,43
43      DO 50 J=1,NORDER
        SAVE=ARRAY(K,J)
        ARRAY(K,J)=ARRAY(I,J)
50      ARRAY(I,J)=-SAVE
51      J=JK(K)
        IF(J-K)21,61,53
53      DO 60 I=1,NORDER
        SAVE=ARRAY(I,K)
        ARRAY(I,K)=ARRAY(I,J)
60      ARRAY(I,J)=-SAVE

C       ACCUMULATE ELEMENTS OF INVERSE MATRIX

61      DO 70 I=1,NORDER
        IF(I-K)63,70,63
63      ARRAY(I,K)=-ARRAY(I,K)/AMAX
70      CONTINUE
        DO 80 I=1,NORDER
        DO 80 J=1,NORDER
        IF(I-K)74,80,74
74      IF (J-K)75,80,75
75      ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)
80      CONTINUE
        DO 90 J=1,NORDER
        IF(J-K)83,90,83
83      ARRAY(K,J)=ARRAY(K,J)/AMAX
90      CONTINUE
        ARRAY(K,K)=1./AMAX
100     DET=DET*AMAX

C       RESTORE ORDENING OF MATRIX

        DO 130 L=1,NORDER
        K= NORDER-L+1
        J=IK(K)
        IF(J-K)111,111,105
105     DO 110 I=1,NORDER
        SAVE=ARRAY(I,K)
        ARRAY(I,K)=-ARRAY(I,J)
110     ARRAY(I,J)=SAVE
111     I=JK(K)
        IF(I-K)130,130,113
113     DO 120 J=1,NORDER
        SAVE=ARRAY(K,J)
        ARRAY(K,J)=-ARRAY(I,J)
120     ARRAY(I,J)=SAVE
130     CONTINUE
        RETURN
        END


      REAL*8 FUNCTION FindIntersect(uexc,ja,jb,iswell)
C
C     Calculates solutions (beta2(i)) of the equation
C         V(beta2) = Excitation Energy
C
C     If solution not found, then assign first or last point
C     of the interval depending on whether we are solving the equation
C     in the rigth(left) side of the barrier(well) case
C
C     IMPLICIT NONE
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      INTEGER ja, jb, npoints, iiextr(0:2*NFPARAB), nextr
      REAL*8 uexc
      REAL*8 vdef_1d(NFISBARPNT), eps_1d(NFISBARPNT)
      LOGICAL iswell
C
C     Local variables
      INTEGER j, is0, is1
      REAL*8 slope

      is0 = -1
      IF(ABS(uexc-Vdef_1d(ja)).EQ.uexc-Vdef_1d(ja)) is0 = 1
      DO j=ja,jb
C      checking for a sign change in Uexc-Vdef
       is1 = -1
       IF(ABS(uexc-Vdef_1d(j)).EQ.uexc-Vdef_1d(j)) is1 = 1
       IF(is1.EQ.is0) CYCLE
C      Sign of (Uexc-Vdef_1d(j)) changed, calculating the precise value
C      of the deformation EPS at which Uexc = Vdef
       FindIntersect = EPS_1d(j-1) + (EPS_1d(j)-EPS_1d(j-1))*
     >       (uexc-Vdef_1d(j-1))/(Vdef_1d(j)-Vdef_1d(j-1))
       RETURN
      ENDDO
C
C     Below is the analysis if intersection not found in [ja,jb]
C
      slope = Vdef_1d(jb) - Vdef_1d(ja)
      IF(iswell) then
C       WELLS
        IF(slope . ge. 0) then
          FindIntersect = EPS_1d(jb) ! ascending
        ELSE
          FindIntersect = EPS_1d(ja) ! descending
        ENDIF
      ELSE
C       BARRIERS
        IF(slope . ge. 0) then
          FindIntersect = EPS_1d(ja) ! ascending
        ELSE
          FindIntersect = EPS_1d(jb) ! descending
        ENDIF
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION GaussLegendre41(F,Ea,Eb,ABSERR)
      IMPLICIT NONE
      REAL*8 F
      REAL*8 Eb,Ea,ABSERR
      REAL*8 wg(10),xgk(21),wgk(21)
      REAL*8 CENTR1,HLGTH1,RESG1,RESK1
      INTEGER J,JTW,JTWM1
      REAL*8 ABSC,FSUM,ABSCM1
      EXTERNAL F
      SAVE WG, XGK, WGK
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
      DATA WG  (  1) / 0.0176140071 3915211831 1861962351 853 D0 /
      DATA WG  (  2) / 0.0406014298 0038694133 1039952274 932 D0 /
      DATA WG  (  3) / 0.0626720483 3410906356 9506535187 042 D0 /
      DATA WG  (  4) / 0.0832767415 7670474872 4758143222 046 D0 /
      DATA WG  (  5) / 0.1019301198 1724043503 6750135480 350 D0 /
      DATA WG  (  6) / 0.1181945319 6151841731 2377377711 382 D0 /
      DATA WG  (  7) / 0.1316886384 4917662689 8494499748 163 D0 /
      DATA WG  (  8) / 0.1420961093 1838205132 9298325067 165 D0 /
      DATA WG  (  9) / 0.1491729864 7260374678 7828737001 969 D0 /
      DATA WG  ( 10) / 0.1527533871 3072585069 8084331955 098 D0 /
C
      DATA XGK (  1) / 0.9988590315 8827766383 8315576545 863 D0 /
      DATA XGK (  2) / 0.9931285991 8509492478 6122388471 320 D0 /
      DATA XGK (  3) / 0.9815078774 5025025919 3342994720 217 D0 /
      DATA XGK (  4) / 0.9639719272 7791379126 7666131197 277 D0 /
      DATA XGK (  5) / 0.9408226338 3175475351 9982722212 443 D0 /
      DATA XGK (  6) / 0.9122344282 5132590586 7752441203 298 D0 /
      DATA XGK (  7) / 0.8782768112 5228197607 7442995113 078 D0 /
      DATA XGK (  8) / 0.8391169718 2221882339 4529061701 521 D0 /
      DATA XGK (  9) / 0.7950414288 3755119835 0638833272 788 D0 /
      DATA XGK ( 10) / 0.7463319064 6015079261 4305070355 642 D0 /
      DATA XGK ( 11) / 0.6932376563 3475138480 5490711845 932 D0 /
      DATA XGK ( 12) / 0.6360536807 2651502545 2836696226 286 D0 /
      DATA XGK ( 13) / 0.5751404468 1971031534 2946036586 425 D0 /
      DATA XGK ( 14) / 0.5108670019 5082709800 4364050955 251 D0 /
      DATA XGK ( 15) / 0.4435931752 3872510319 9992213492 640 D0 /
      DATA XGK ( 16) / 0.3737060887 1541956067 2548177024 927 D0 /
      DATA XGK ( 17) / 0.3016278681 1491300432 0555356858 592 D0 /
      DATA XGK ( 18) / 0.2277858511 4164507808 0496195368 575 D0 /
      DATA XGK ( 19) / 0.1526054652 4092267550 5220241022 678 D0 /
      DATA XGK ( 20) / 0.0765265211 3349733375 4640409398 838 D0 /
      DATA XGK ( 21) / 0.0000000000 0000000000 0000000000 000 D0 /
C
      DATA WGK (  1) / 0.0030735837 1852053150 1218293246 031 D0 /
      DATA WGK (  2) / 0.0086002698 5564294219 8661787950 102 D0 /
      DATA WGK (  3) / 0.0146261692 5697125298 3787960308 868 D0 /
      DATA WGK (  4) / 0.0203883734 6126652359 8010231432 755 D0 /
      DATA WGK (  5) / 0.0258821336 0495115883 4505067096 153 D0 /
      DATA WGK (  6) / 0.0312873067 7703279895 8543119323 801 D0 /
      DATA WGK (  7) / 0.0366001697 5820079803 0557240707 211 D0 /
      DATA WGK (  8) / 0.0416688733 2797368626 3788305936 895 D0 /
      DATA WGK (  9) / 0.0464348218 6749767472 0231880926 108 D0 /
      DATA WGK ( 10) / 0.0509445739 2372869193 2707670050 345 D0 /
      DATA WGK ( 11) / 0.0551951053 4828599474 4832372419 777 D0 /
      DATA WGK ( 12) / 0.0591114008 8063957237 4967220648 594 D0 /
      DATA WGK ( 13) / 0.0626532375 5478116802 5870122174 255 D0 /
      DATA WGK ( 14) / 0.0658345971 3361842211 1563556969 398 D0 /
      DATA WGK ( 15) / 0.0686486729 2852161934 5623411885 368 D0 /
      DATA WGK ( 16) / 0.0710544235 5344406830 5790361723 210 D0 /
      DATA WGK ( 17) / 0.0730306903 3278666749 5189417658 913 D0 /
      DATA WGK ( 18) / 0.0745828754 0049918898 6581418362 488 D0 /
      DATA WGK ( 19) / 0.0757044976 8455667465 9542775376 617 D0 /
      DATA WGK ( 20) / 0.0763778676 7208073670 5502835038 061 D0 /
      DATA WGK ( 21) / 0.0766007119 1799965644 5049901530 102 D0 /
C     Integrating from Ea to Eb, converting to symmetric grid from -1 to +1
      CENTR1 = 0.5D+00*(Ea+Eb)
      HLGTH1 = 0.5D+00*(Eb-Ea)
C
C     COMPUTE THE 41-POINT GAUSS-KRONROD APPROXIMATION TO
C     THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR USING ONLY 21 points
C
      RESG1 = 0.0D+00
      RESK1 = WGK(21)*F(CENTR1)
      DO J=1,10
        JTW = J*2
        JTWM1 = JTW-1
        ABSC = HLGTH1*XGK(JTW)
        FSUM = F(CENTR1-ABSC) + F(CENTR1+ABSC)
        ABSCM1 = HLGTH1*XGK(JTWM1)
        RESG1 = RESG1+WG(J)*FSUM
        RESK1 = RESK1+WGK(JTW)*FSUM+WGK(JTWM1)*
     &            (F(CENTR1-ABSCM1)+F(CENTR1+ABSCM1))
      ENDDO

      GaussLegendre41 = RESK1*HLGTH1
      ABSERR = ABS((RESK1-RESG1)*HLGTH1)

      RETURN
      END



