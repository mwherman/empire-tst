/***********************************************************************
 *
 * Filename: kernel.cpp
 * Purpose : calculates average c/s based on Atlas
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

/*
 *
 NOTICE: Fission part is not complete !!!
 *
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "kernel.h"

CKernel::CKernel()
{
  m_pPotXS = m_pPotUN = NULL;
  m_fCorrNN = .5;
  m_fCorrGG = .5;
  m_fCorrRP = .5;
  m_fCorrNG = .0;
  m_fCorrNGS = .0;
  m_fR = m_fdR = 0;
  m_fMin = 0;
  m_fMax = 1e38;
  m_fDefaultScatUnc = .1;
  m_fGammaFactor = 4;
}

CKernel::~CKernel()
{
  if (m_pPotXS) delete[] m_pPotXS;
  if (m_pPotUN) delete[] m_pPotUN;
}

void CKernel::SetR(double R, double dR)
{
  m_fR = R;
  m_fdR = dR;
}

void CKernel::SetCorr(double fNN, double fGG, double fRP, double fNG, double fNGS)
{
  m_fCorrNN = fNN;
  m_fCorrGG = fGG;
  m_fCorrRP = fRP;
  m_fCorrNG = fNG;
  m_fCorrNGS = fNGS;
}

void CKernel::SetRange(double fMin, double fMax)
{
  m_fMin = fMin;
  m_fMax = fMax;
}

void CKernel::SetDefaultScatUnc(double f)
{
  m_fDefaultScatUnc = f;
}

void CKernel::SetGammaFactor(double f)
{
  m_fGammaFactor = f;
}

void CKernel::InitPotential(int nGroup, double *pGroup)
{
  if (m_pPotXS) delete[] m_pPotXS;
  if (m_pPotUN) delete[] m_pPotUN;

  m_pPotXS = new double[nGroup];
  m_pPotUN = new double[nGroup];

  for (int i=0;i<nGroup;i++) {
    m_pPotXS[i] = GetPotentialXS(pGroup[i], pGroup[i+1]);
    m_pPotUN[i] = GetPotentialUnc(pGroup[i], pGroup[i+1]);
  }
}

double CKernel::GetPotentialXS(double g1, double g2)
{
  double e = (g1+g2)/2;					// average group energy in eV
  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);		// k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;					// kR. 1fm = 0.1*sqrt(barn)
/*
  double fR0_inf = .22;
  double fR1_inf = .34;

  double phi0 = -kR + asin(kR*fR0_inf);
  double phi1 = -kR + atan(kR) + asin(kR*kR*kR*fR1_inf/(1+kR*kR));
*/
  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double phi2 = kR - atan(3*kR/(3-kR*kR));
/*
  double T0 = 1-exp(-2*PI*3.79e-4*sqrt(e));
  double T1 = 1-exp(-2*PI*.5e-4*kR*kR/(1+kR*kR)*sqrt(e));
  return 4*PI*lambda2*sin(phi0)*sin(phi0)*(1-0.5*T0) +
         12*PI*lambda2*sin(phi1)*sin(phi1)*(1-.5*T1) +
*/
  return 4*PI*lambda2*sin(phi0)*sin(phi0) +
         12*PI*lambda2*sin(phi1)*sin(phi1) +
         20*PI*lambda2*sin(phi2)*sin(phi2);
/*
  return 4*PI*lambda2*sin(kR)*sin(kR) +
         12*PI*lambda2*sin(kR-atan(kR))*sin(kR-atan(kR)) +
         20*PI*lambda2*sin(kR-atan(3*kR/(3-kR*kR)))*sin(kR-atan(3*kR/(3-kR*kR)));
*/
}

double CKernel::GetPotentialUnc(double g1, double g2)
{
  double e = (g1+g2)/2;
  double kR = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e)*m_fR*.1;
  double phi0 = kR;
  double phi1 = kR-atan(kR);

  return 2*m_fdR/m_fR*kR*(sin(phi0)*cos(phi0)+3*(1-(1/(1+kR*kR)))*sin(phi1)*cos(phi1))/
         (sin(phi0)*sin(phi0)+3*sin(phi1)*sin(phi1));
}

double CKernel::GetScatXS(int n, double g1, double g2)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);

  double kR = 2.19677e-3*m_nA/(m_nA+1)*sqrt(E)*m_fR*.1;
  double phi; 
  int l = GetL(n);
  if (l == 0) phi = kR;
  else if (l == 1) phi = kR-atan(kR);
  else phi = kR-atan(3*kR/(3-kR*kR));
//  printf("E = %lg, phi = %lg, sin(phi) = %lg value = %lg\n", E, phi, sin(phi),
//         4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*gGn*2*(Gn+Gg)*sin(phi)*sin(phi)/(Gn+Gg)/(E*(g2-g1)));
  double sctxs = 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*gGn*(Gn-2*(Gn+Gg)*sin(phi)*sin(phi))/
                 (Gn+Gg)/(E*(g2-g1));
  if (sctxs < 0) sctxs = 0;
  return sctxs;
}

double CKernel::GetCaptXS(int n, double g1, double g2, bool usearea)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);
  if (!usearea || area == 0) area = gGn*Gg/(Gn+Gg);
  return 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*area/(E*(g2-g1));
}

double CKernel::GetFissXS(int n, double g1, double g2, bool usearea)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);
  if (!usearea || farea == 0) farea = gGn*Gf/(Gn+Gf);
  return 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*farea/(E*(g2-g1));
}

bool CKernel::GetXSnUNC(int nth, int nReaction, int nGroup, double *pGroup, int &nFirstResGroup, double *pXS, double *pUN, bool bUseArea)
{
  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double E2,dE2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  memset(pXS, 0, nGroup*sizeof(double));
  memset(pUN, 0, nGroup*sizeof(double));

  if (nth == 0 && nReaction == SCAT) InitPotential(nGroup, pGroup);

// calculate average cross section in each energy group
  nFirstResGroup = -1;
  int n = 0;
  for (int i=0;i<NoRes();i++) {
    GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
    if (E < m_fMin) continue;
    if (E > m_fMax) break;
    if (Gn+Gg == 0) {
      if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %lf... skipped!\n", E);
      continue;
    }
    if (E >= pGroup[n+1]) {
      for (n++;n < nGroup && E >= pGroup[n+1];n++);
//      if (nFirstResGroup == -1) nFirstResGroup = n;
      if (n == nGroup) break;
    }
    if (nFirstResGroup == -1) nFirstResGroup = n;

//    printf("group %d: E=%lf,Gn=%lf,Gg=%lf,area=%lf\n", n+1, E, Gn, Gg, area);
    if (nReaction == SCAT) {
      pXS[n] += GetScatXS(i, pGroup[n], pGroup[n+1]);
//      printf("group %d: %lf from E=%lf, width=%lf\n", n+1, GetScatXS(i, pGroup[n], pGroup[n+1]), E, Gn+Gg);
    } else if (nReaction == CAPT) {
      pXS[n] += GetCaptXS(i, pGroup[n], pGroup[n+1], bUseArea);
    } else if (nReaction == FISS) {
      pXS[n] += GetFissXS(i, pGroup[n], pGroup[n+1], bUseArea);
    }
    for (int l=n;l>0;l--) {
      if (E-m_fGammaFactor*(Gn+Gg) < pGroup[l]) {
        fprintf(stderr, "WARNING: group %d: from E = %lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+1, E);
/*
        if (nReaction == SCAT) pXS[l-1] += GetScatXS(i, pGroup[l-1], pGroup[l]);
        else if (nReaction == CAPT) pXS[l-1] += GetCaptXS(i, pGroup[l-1], pGroup[l], bUseArea);
        else if (nReaction == FISS) pXS[l-1] += GetFissXS(i, pGroup[l-1], pGroup[l], bUseArea);
*/
      }
    }
    for (int l=n;l+2<nGroup;l++) {
      if (E+m_fGammaFactor*(Gn+Gg) >= pGroup[l+1]) {
        fprintf(stderr, "WARNING: group %d: from E = %lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+2, E);
/*
        if (nReaction == SCAT) pXS[l+1] += GetScatXS(i, pGroup[l+1], pGroup[l+2]);
        else if (nReaction == CAPT) pXS[l+1] += GetCaptXS(i, pGroup[l+1], pGroup[l+2], bUseArea);
        else if (nReaction == FISS) pXS[l+1] += GetFissXS(i, pGroup[l+1], pGroup[l+2], bUseArea);
*/
      }
    }
  }

// calculate uncertainties due to resonance parameters in each energy group
  n = 0;
  double factor1, factor2;
  for (int i=0;i<NoRes();i++) {
    GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
/*
    printf("E=%4.1lf,J=%3.1lf,g=%5.3lf,2gGn=%10.4lf+-%7.3lf,Gg=%10.3lf+-%6.3lf\n",
            E/1000, J, (2*J+1)/(2*(2*GetSpin()+1)),2*gGn,2*(2*J+1)/(2*(2*GetSpin()+1))*dGn, Gg, dGg);
*/
    if (E < m_fMin) continue;
    if (E > m_fMax) break;
    if (Gn+Gg == 0) {
      if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %lf... skipped!\n", E);
      continue;
    }
    if (E >= pGroup[n+1]) {
      for (n++;n < nGroup && E >= pGroup[n+1];n++);
      if (n == nGroup) break;
    }
    if (nReaction == SCAT && pXS[n] == 0) {
      pUN[n] = 0;
      if (nth == 0) fprintf(stderr, "WARNING: average scattering c/s is 0 at group %d... zero uncertainty is assigned!\n", n+1);
      continue;
    } else if (nReaction == CAPT && pXS[n] == 0) {
      pUN[n] = 0;
      if (nth == 0) fprintf(stderr, "WARNING: average capture c/s is 0 at group %d... zero uncertainty is assigned!\n", n+1);
      continue;
    } else if (nReaction == FISS && pXS[n] == 0) {
      pUN[n] = 0;
      if (nth == 0) fprintf(stderr, "WARNING: average fission c/s is 0 at group %d... zero uncertainty is assigned!\n", n+1);
      continue;
    }
    if (Gn == 0) dGn = 0;
    else {
      if (dGn == 0) dGn = m_fDefaultScatUnc;
      else dGn /= Gn;
    }
    if (Gg == 0) dGg = 0;
    else dGg /= Gg;

    if (nReaction == SCAT) factor1 = GetScatXS(i, pGroup[n], pGroup[n+1]);
    else if (nReaction == CAPT) factor1 = GetCaptXS(i, pGroup[n], pGroup[n+1], bUseArea);
    else if (nReaction == FISS) factor1 = GetFissXS(i, pGroup[n], pGroup[n+1], bUseArea);

    for (int j=0;j<NoRes();j++) {
      GetParameter(j, E2, dE2, J2, gGn2, Gn2, dGn2, Gg2, dGg2, Gf2, dGf2, area2, darea2, farea2, dfarea2);
      if (E < m_fMin) continue;
      if (E > m_fMax) break;
      if (Gn2+Gg2 == 0) {
        if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %lf... skipped!\n", E2);
        continue;
      }
      if (Gn2 == 0) dGn2 = 0;
      else {
        if (dGn2 == 0) dGn2 = m_fDefaultScatUnc;
        else dGn2 /= Gn2;
      }
      if (Gg2 == 0) dGg2= 0;
      else dGg2 /= Gg2;

      if (nReaction == SCAT) factor2 = GetScatXS(j, pGroup[n], pGroup[n+1]);
      else if (nReaction == CAPT) factor2 = GetCaptXS(j, pGroup[n], pGroup[n+1], bUseArea);
      else if (nReaction == FISS) factor2 = GetFissXS(j, pGroup[n], pGroup[n+1], bUseArea);

      if (E2 >= pGroup[n] && E2 < pGroup[n+1]) {
        if (i == j) {
          if (nReaction == SCAT) pUN[n] += factor1*factor2/(pXS[n]*pXS[n])*(
                                  (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2-(Gn+Gg+Gg)*Gg2*dGn*dGg2*m_fCorrNGS-
                                  Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*m_fCorrNGS+Gg*Gg2*dGg*dGg2)/
                                  (Gn+Gg)/(Gn2+Gg2);
          else if (nReaction == CAPT) {
            if (bUseArea && darea != 0 && darea2 != 0)
              pUN[n] += factor1*factor2*darea*darea2/area/area2/(pXS[n]*pXS[n]);
            else
              pUN[n] += factor1*factor2/(pXS[n]*pXS[n])*(
                           Gg*Gg2*dGn*dGn2+
                           Gg*Gn2*dGn*dGg2*m_fCorrNGS+Gn*Gg2*dGg*dGn2*m_fCorrNGS+
                           Gn*Gn2*dGg*dGg2)/
                           (Gn+Gg)/(Gn2+Gg2);
          } else if (nReaction == FISS) {
            if (bUseArea && dfarea != 0 && dfarea2 != 0)
              pUN[n] += factor1*factor2*dfarea*dfarea2/farea/farea2/(pXS[n]*pXS[n]);
            else
              pUN[n] += factor1*factor2/(pXS[n]*pXS[n])*(
                           Gf*Gf2*dGn*dGn2+
                           Gf*Gn2*dGn*dGf2*m_fCorrNGS+Gn*Gf2*dGf*dGn2*m_fCorrNGS+
                           Gn*Gn2*dGf*dGf2)/
                           (Gn+Gg+Gf)/(Gn2+Gg2+Gf2);
          }
/*
          if (n==79 && nReaction == SCAT) printf("DEBUG: %5.1lf & %5.1lf: factor1=%9.3lE,factor2=%9.3lE,Gn1+Gg1=%lg,"
                                     "Gn2+Gg2=%lg,SctXS=%lg,g1=%lg,g2=%lg,value=%lg\n",
                                     E, E2, factor1, factor2, Gn+Gg, Gn2+Gg2, pXS[n],pGroup[n],pGroup[n+1],
                                     factor1*factor2/(pXS[n]*pXS[n])*(
                                     (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2-(Gn+Gg+Gg)*Gg2*dGn*dGg2*m_fCorrNGS-
                                     Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*m_fCorrNGS+Gg*Gg2*dGg*dGg2)/
                                     (Gn+Gg)/(Gn2+Gg2));
*/
        } else {
          if (nReaction == SCAT) pUN[n] += factor1*factor2/(pXS[n]*pXS[n])*(
                                  (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2*m_fCorrNN-(Gn+Gg+Gg)*Gg2*dGn*dGg2*m_fCorrNG-
                                  Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*m_fCorrNG+Gg*Gg2*dGg*dGg2*m_fCorrGG)/
                                  (Gn+Gg)/(Gn2+Gg2);
          else if (nReaction == CAPT) {
            if (bUseArea && darea != 0 && darea2 != 0)
              pUN[n] += m_fCorrGG*factor1*factor2*darea*darea2/area/area2/(pXS[n]*pXS[n]);
            else
              pUN[n] += factor1*factor2/(pXS[n]*pXS[n])*(
                           Gg*Gg2*dGn*dGn2*m_fCorrNN+Gg*Gn2*dGn*dGg2*m_fCorrNG+
                           Gn*Gg2*dGg*dGn2*m_fCorrNG+Gn*Gn2*dGg*dGg2*m_fCorrGG)/
                           (Gn+Gg)/(Gn2+Gg2);
          } else if (nReaction == FISS) {
            if (bUseArea && dfarea != 0 && dfarea2 != 0)
              pUN[n] += m_fCorrGG*factor1*factor2*dfarea*dfarea2/farea/farea2/(pXS[n]*pXS[n]);
            else
              pUN[n] += factor1*factor2/(pXS[n]*pXS[n])*(
                           Gf*Gf2*dGn*dGn2*m_fCorrNN+Gf*Gn2*dGn*dGf2*m_fCorrNG+
                           Gn*Gf2*dGf*dGn2*m_fCorrNG+Gn*Gn2*dGf*dGf2*m_fCorrGG)/
                           (Gn+Gg+Gf)/(Gn2+Gg2+Gf2);
          }
/*
          if (n==79 && nReaction == SCAT) printf("DEBUG: %5.1lf & %5.1lf: factor1=%9.3lE,factor2=%9.3lE,Gn1+Gg1=%lg,"
                                     "Gn2+Gg2=%lg,SctXS=%lg,g1=%lg,g2=%lg,value=%lg\n",
                                     E, E2, factor1, factor2, Gn+Gg, Gn2+Gg2, pXS[n],pGroup[n],pGroup[n+1],
                                     factor1*factor2*(
                                     (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2*m_fCorrNN-(Gn+Gg+Gg)*Gg2*dGn*dGg2*m_fCorrNG-
                                     Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*m_fCorrNG+Gg*Gg2*dGg*dGg2*m_fCorrGG)/(pXS[n]*pXS[n])/
                                     (Gn+Gg)/(Gn2+Gg2));
*/
        }
      }
    }
  }

// calculate overall scattering uncertainties including potential scattering in each energy group
  for (int i=0;i<nGroup;i++) {
    if (nReaction == SCAT) {
//      printf("Sct. Unc. [%d] = %lg\n", i, sqrt(pUN[i]));
      if (i >= nFirstResGroup) {
        double total = pXS[i]+m_pPotXS[i];
        if (m_fR == 0) pUN[i] = pUN[i]*pXS[i]*pXS[i]/total/total;
        else {
/*
          pUN[i] = pUN[i]*pXS[i]*pXS[i]/total/total+
                   4*m_fdR/m_fR*m_fdR/m_fR*m_pPotXS[i]*m_pPotXS[i]/total/total+
                   m_fCorrRP*2*sqrt(pUN[i])*2*m_fdR/m_fR*pXS[i]*m_pPotXS[i]/total/total;
*/
          pUN[i] = pUN[i]*pXS[i]*pXS[i]/total/total+
                   m_pPotUN[i]*m_pPotUN[i]*m_pPotXS[i]*m_pPotXS[i]/total/total+
                   2*m_fCorrRP*sqrt(pUN[i])*m_pPotUN[i]*pXS[i]*m_pPotXS[i]/total/total;
        }
        pXS[i] += m_pPotXS[i];
      }
//      printf("Sct. Unc. [%d] = %lg\n", i, sqrt(pUN[i]));
    }
    pUN[i] = sqrt(pUN[i]);
  }
  return true;
}
