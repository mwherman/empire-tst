Ccc   * $Author: Capote $
Ccc   * $Date: 2005-05-12 17:55:46 $
Ccc   * $Id: scnd-preeq.f,v 1.14 2005-05-12 17:55:46 Capote Exp $
C
      SUBROUTINE SCNDPREEQ(Nnuc,Nnur,Nejc,Last)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                      S C N D P R E E Q                           *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates second-chance preequilibrium decay of nucleus NNUC    *
Ccc   * (populated by the MSD emission) to the residual nucleus NNUR     *
Ccc   * through the emission of the ejectile NEJC using M.B. Chadwick's  *
Ccc   * model.                                                           *
Ccc   * Approximations:                                                  *
Ccc   * 1. the first residue (after MSD emission) is assumed to contain  *
Ccc   *    2 excitons only                                               *
Ccc   * 2. s-wave transmission coefficient is used to characterize       *
Ccc   *    emission probablity                                           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index (MSD residue)                *
Ccc   *       NNUR - residual nucleus index                              *
Ccc   *       NEJC - ejectile index                                      *
Ccc   *       LAST - 0 when called for the first time for a given residue*
Ccc   *            - 1 when called for the last time for a given residual*
Ccc   *              nucleus decay (i.e., when both neutrons and         *
Ccc   *              protons were emitted). It will cause subtraction of *
Ccc   *              the second chance preequilibrium emission from the  *
Ccc   *              first residue population.                           *
Ccc   *              2 to cause subtraction only if last decay is missing*
Ccc   *                                                                  *
Ccc   * output: none                                                     *
Ccc   *                                                                  *
Ccc   * calls:TLLOC                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION B, E, FACt(100), G
      COMMON /EB    / E, B, FACt, G
C
C Dummy arguments
C
      INTEGER Last, Nejc, Nnuc, Nnur
C
C Local variables
C
      DOUBLE PRECISION bind, corr, excnq, pnratio, pop1, popp, 
     &                 popsub(NDEX,NDLW,2), ratioro, sum, sumem, 
     &                 sumpopsub, welld
      REAL FLOAT, REAL
      INTEGER i, icse, ie, iec, iejc, ier, iermax, ietl, iexc, ip, itlc,
     &        j, jc, nh, np
      INTEGER MAX0
      DOUBLE PRECISION WOB1
      IF (Last.NE.2) THEN
C--------clean storing matrix for second chance preequilibrium emission
         IF (Last.EQ.0) THEN
            DO ie = 1, NDEX
               DO j = 1, NDLW
                  popsub(ie,j,1) = 0.0D0
                  popsub(ie,j,2) = 0.0D0
               ENDDO
            ENDDO
         ENDIF
C--------calculation of factorial (note notation n!= FACT(n+1))
         IF (FACt(1).NE.1.0D0) THEN
            FACt(1) = 1.
            DO i = 1, 99
               FACt(i + 1) = FACt(i)*FLOAT(i)
            ENDDO
         ENDIF
C--------calculation of factorial      ***** done *******
         iexc = NEX(Nnuc) - NEXr(Nejc,Nnuc)
         itlc = iexc - 5
         B = 1000.  !ignore binding condition in level densities
         bind = Q(Nejc,Nnuc)
         G = A(Nnuc)/GDIv
         np = 1 ! assume 1p-1h configuration in the first residue
         nh = 1 ! assume 1p-1h configuration in the first residue
         welld = 35.0 ! potential well depth (for p-h state densiies)
C--------divide single nucleon in the continuum into neutron and proton components
C--------assume TORY ratio between unlike and like interaction
         pnratio = TORy/(TORy + 1.0) !unlike
         IF (ZEJc(0).EQ.0.0D0 .AND. Nejc.EQ.1) pnratio = 1.0 - pnratio
         IF (ZEJc(0).EQ.1.0D0 .AND. Nejc.EQ.2) pnratio = 1.0 - pnratio
C--------do loop over 1-st residue excitation energy
         DO iec = NEX(Nnuc), 1, -1
            excnq = EX(iec,Nnuc) - Q(Nejc,Nnuc)
            iermax = iec - iexc
            IF (iermax.LT.1) GOTO 50
C-----------do loop over 1-st residue parity
C-----------(the 2-nd residue parity is the same since only l=0 is considered)
            DO ip = 1, 2
C--------------do loop over 1-st residue spins
               DO jc = 1, NLW, LTUrbo
                  popp = POP(iec,jc,ip,Nnuc)*pnratio
                  sumem = 0.0
C-----------------decay to the highest possible bin (non neutron only)
                  IF (ZEJc(Nejc).NE.0.0D0) THEN
                     icse = (excnq - EX(iermax,Nnur))/DE + 1.0001
C--------------------probability of finding a particle at energy E+B inside a
C--------------------1p-1h configuration of energy EX(IEC,NNUC).
C--------------------Eq.5 of Phys. Rev C50(1994)996
                     ratioro = WOB1((icse - 1)*DE + bind,np,0,welld)
     &                         *WOB1(excnq - (icse - 1)*DE,np - 1,nh,
     &                         welld)/WOB1(EX(iec,Nnuc),np,nh,welld)
     &                         /REAL(np)
C--------------------probability *** done ***
                     icse = MAX0(2,icse)
                     pop1 = popp*TL(5,1,Nejc,Nnur)*ratioro*TURbo
                     POP(iermax,jc,ip,Nnur) = POP(iermax,jc,ip,Nnur)
     &                  + pop1
                     CSE(icse,Nejc,Nnuc) = CSE(icse,Nejc,Nnuc) + pop1
                     sumem = sumem + pop1
                  ENDIF
C-----------------decay to the highest but one bin (neutrons only)
                  IF (ZEJc(Nejc).EQ.0.0D0 .AND. iec.EQ.NEX(Nnuc) - 1)
     &                THEN
                     icse = (excnq - EX(iermax,Nnur))/DE + 1.0001
C--------------------probability of finding a particle at energy ...
                     ratioro = WOB1((icse - 1)*DE + bind,np,0,welld)
     &                         *WOB1(excnq - (icse - 1)*DE,np - 1,nh,
     &                         welld)/WOB1(EX(iec,Nnuc),np,nh,welld)
     &                         /REAL(np)
C--------------------probability *** done ***
                     icse = MAX0(2,icse)
                     pop1 = popp*TL(6,1,Nejc,Nnur)*ratioro
C--------------------CORR in the next lines accounts for the Tl interpolation
C--------------------and integration over overlaping bins (2/3), it turned
C--------------------out it must be energy step and also emission step dependent
                     corr = 0.4444/(DE - XN(Nnur) + XN(1))*TURbo
                     pop1 = pop1*corr
                     POP(iermax,jc,ip,Nnur) = POP(iermax,jc,ip,Nnur)
     &                  + pop1
                     CSE(icse,Nejc,Nnuc) = CSE(icse,Nejc,Nnuc) + pop1
                     sumem = sumem + pop1
                  ENDIF
C-----------------do loop over r.n. energies (highest bin and eventually
C-----------------the second bin from the top excluded as already done)
                  DO ier = iermax - 1, 1, -1
                     icse = (excnq - EX(ier,Nnur))/DE + 1.0001
C--------------------probability of finding a particle at energy ...
                     ratioro = WOB1((icse - 1)*DE + bind,np,0,welld)
     &                         *WOB1(excnq - (icse - 1)*DE,np - 1,nh,
     &                         welld)/WOB1(EX(iec,Nnuc),np,nh,welld)
     &                         /REAL(np)
C--------------------probability *** done ***
                     ietl = iec - ier - itlc
                     pop1 = popp*TL(ietl,1,Nejc,Nnur)*ratioro*TURbo
	               IF (pop1.GT.0) THEN
                       POP(ier,jc,ip,Nnur) = POP(ier,jc,ip,Nnur) + pop1
                       CSE(icse,Nejc,Nnuc) = CSE(icse,Nejc,Nnuc) + pop1
C                      IF (ENDf(Nnuc).EQ.1.D0)
                       IF (ENDf(Nnuc).GT.0.D0)
     &                   CALL EXCLUSIVEC(iec,ier,Nejc,Nnuc,Nnur,pop1)
                       sumem = sumem + pop1
	               ENDIF
                  ENDDO         !end do on 2-nd residue excitation energy
                  popsub(iec,jc,ip) = popsub(iec,jc,ip) + sumem
               ENDDO            !end do on 1-st residue spins
            ENDDO               !end do on 1-st residue parity
         ENDDO                  !end do on 1-st residue excitation energy
C--------trapezoidal integration of continuum population for ejectile nejc
   50    sum = 0.0
         DO j = 1, NLW, LTUrbo
            DO i = 1, NEX(Nnur)
               sum = sum + POP(i,j,1,Nnur) + POP(i,j,2,Nnur)
            ENDDO
         ENDDO
         sum = sum*DE
C--------integration of ro*tl in continuum for ejectile nejc -- done ----
         WRITE (6,*) ' '
         WRITE (6,*) ' '
         IF (Nejc.EQ.1) THEN
            WRITE (6,*) ' n second-chance preequilibrium emission ', 
     &                  'crosss section ', sum, ' mb'
         ELSEIF (Nejc.EQ.2) THEN
            WRITE (6,*) ' p second-chance preequilibrium emission ', 
     &                  'crosss section ', sum, ' mb'
         ENDIF
C--------store second chance emission cross section on the appropriate emission x-s
         CSEmis(Nejc,Nnuc) = CSEmis(Nejc,Nnuc) + sum
      ENDIF
C-----reduce 1-st residue population on the last entry
      IF (Last.GE.1) THEN
         DO iec = 1, NEX(Nnuc)
            IF (POPbin(iec,Nnuc).NE.0) THEN
               sumpopsub = 0
               DO jc = 1, NLW, LTUrbo
                  POP(iec,jc,1,Nnuc) = POP(iec,jc,1,Nnuc)
     &                                 - popsub(iec,jc,1)
                  POP(iec,jc,2,Nnuc) = POP(iec,jc,2,Nnuc)
     &                                 - popsub(iec,jc,2)
                  sumpopsub = sumpopsub + popsub(iec,jc,1)
     &                        + popsub(iec,jc,2)
               ENDDO
C-----------reduce 1-st residue population DDX spectra (using portions)
C-----------on the last entry
               DO ie = 1, NDECSE
                  DO iejc = 1, NDEJCD
                     POPcseaf(iec,iejc,ie,Nnuc)
     &                  = POPcseaf(iec,iejc,ie,Nnuc)
     &                  *(1 - sumpopsub/POPbin(iec,Nnuc))
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      END
