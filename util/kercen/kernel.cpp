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
#include <stdlib.h>
#include "kernel.h"

int sfirst = 1;

CKernel::CKernel()
{
  m_bInit = false;
  m_pPotXS = m_pPotUN = NULL;
  m_pExtXS = NULL;
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
  if (m_pExtXS) delete[] m_pExtXS;
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

void CKernel::Init(int nGroup, double *pGroup, bool bBound)
{
  if (m_bInit) return;
  int i;
  m_pPotXS = new double[nGroup];
  m_pPotUN = new double[nGroup];
  m_pExtXS = new double[nGroup];

  memset(m_pExtXS, 0, nGroup*sizeof(double));

  for (i=0;i<nGroup;i++) {
    m_pPotXS[i] = GetPotentialXS(pGroup[i], pGroup[i+1]);
    m_pPotUN[i] = GetPotentialUnc(pGroup[i], pGroup[i+1]);
  }

  if (bBound) {
    double E, J, gGn, Gn, Gg, Gf, area, farea;
    int nFirst;
    for (i=0;i<NoRes();i++) {
      E = GetE(i);
      if (E > 0) {
       for (nFirst=0; nFirst < nGroup && E >= pGroup[nFirst+1]; nFirst++);
       break;
      }
    }
    printf("nFirst = %d\n", nFirst);
    for (i=0;i<NoRes();i++) {
      GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
      if (E >= 0) break;
      for (int n=nFirst;n<nGroup;n++) {
        m_pExtXS[n] += GetScatXSFrom(E, GetL(i), (2*J+1)/(2*(2*m_fSpin+1)), Gn, Gg, pGroup[n], pGroup[n+1]);
//        printf("Ext. XS = %lf\n", m_pExtXS[n]);
      }
    }    
  }
  m_bInit = true;
}

double CKernel::GetScatXSFrom(double e0, double l, double g, double gn,double gg, double g1, double g2)
// This subroutine is not complete !!
// Currently returns 0.
{
  if (l < 0 || l > 2) {
    fprintf(stderr, "l=%d not supported\n");
    exit(1);
  }
  double sum = 0;
  double kR_e0 = 2.19677e-3*m_nA/(m_nA+1)*sqrt(fabs(e0))*m_fR*.1;
  double p_e0;
  double gn_e;
  double e, k, lambda2, kR, phi, p;
  if (l == 0) p_e0 = kR_e0;
  else if (l == 1) p_e0 = kR_e0*kR_e0*kR_e0/(1+kR_e0*kR_e0);
  else if (l == 2) p_e0 = pow(kR_e0, 5.0)/(9+3*pow(kR_e0, 2.0)+pow(kR_e0, 4.0));
  for (int i=1;i<=49;i++) {
    e = g1 + i*(g2-g1)/50;
    k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);	// k in units of sqrt(barn) = 1.0E-14 m
    lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
    kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)
    if (l == 0) phi = kR;
    else if (l == 1) phi = kR - atan(kR);
    else if (l == 2) phi = kR - atan(3*kR/(3-kR*kR));

    if (l == 0) p = kR;
    else if (l == 1) p = kR*kR*kR/(1+kR*kR);
    else if (l == 2) p = pow(kR, 5.0)/(9+3*pow(kR, 2.0)+pow(kR, 4.0));

    gn_e = gn*p/p_e0;

    sum += PI*lambda2*g*(gn_e*gn_e-2*gn_e*(gn_e+gg)*sin(phi)*sin(phi)+2*(e-e0)*gn_e*sin(2*phi))/
                        ((e-e0)*(e-e0)+(gn_e+gg)*(gn_e+gg)/4);
//    printf(" %lf,%le,%lf", e,kR_e0, PI*lambda2*g*(gn_e*gn_e-2*gn_e*(gn_e+gg)*sin(phi)*sin(phi)+2*(e-e0)*gn_e*sin(2*phi))/
//                                    ((e-e0)*(e-e0)+(gn_e+gg)*(gn_e+gg)/4));
  }
//  puts("");
  return sum/49;
}

double CKernel::GetPotentialXS(double g1, double g2)
{
  double e = (g1+g2)/2;					// average group energy in eV
  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);		// k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)
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

bool CKernel::GetXSnUNC(int nth, int nReaction, int nGroup, double *pGroup, int &nFirstResGroup, double *pXS, double *pUN, int nFlag)
{
  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double E2,dE2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  memset(pXS, 0, nGroup*sizeof(double));
  memset(pUN, 0, nGroup*sizeof(double));

  if (nth == 0 && nReaction == SCAT) Init(nGroup, pGroup, nFlag&BOUND);

// calculate average cross section in each energy group

  nFirstResGroup = -1;
  int ng = 0;
  for (int i=0;i<NoRes();i++) {

    GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

    if (E < m_fMin) continue;
    if (E > m_fMax) break;

    if (Gn+Gg == 0) {
      if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %10.2lf... skipped!\n", E);
      continue;
    }

    if (E >= pGroup[ng+1]) {
      for (ng++;ng < nGroup && E >= pGroup[ng+1];ng++);
      if (ng == nGroup) break;
    }

    if (nFirstResGroup == -1) nFirstResGroup = ng;

    // printf("group %d: E=%lf,Gn=%lf,Gg=%lf,area=%lf\n", ng+1, E, Gn, Gg, area);

    if (nReaction == SCAT) {
      pXS[ng] += GetScatXS(i, pGroup[ng], pGroup[ng+1]);
      // printf("group %d: %lf from E=%lf, width=%lf\n", ng+1, GetScatXS(i, pGroup[ng], pGroup[ng+1]), E, Gn+Gg);
    } else if (nReaction == CAPT) {
      pXS[ng] += GetCaptXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
    } else if (nReaction == FISS) {
      pXS[ng] += GetFissXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
    }

    for (int l=ng;l>0;l--) {
      if (E-m_fGammaFactor*(Gn+Gg) < pGroup[l]) {
        fprintf(stderr, "WARNING: group %d: from E = %10.2lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+1, E);
/*
        if (nReaction == SCAT) pXS[l-1] += GetScatXS(i, pGroup[l-1], pGroup[l]);
        else if (nReaction == CAPT) pXS[l-1] += GetCaptXS(i, pGroup[l-1], pGroup[l], nFlag&USEAREA);
        else if (nReaction == FISS) pXS[l-1] += GetFissXS(i, pGroup[l-1], pGroup[l], nFlag&USEAREA);
*/
      }
    }

    for (int l=ng;l+2<nGroup;l++) {
      if (E+m_fGammaFactor*(Gn+Gg) >= pGroup[l+1]) {
        fprintf(stderr, "WARNING: group %d: from E = %10.2lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+2, E);
/*
        if (nReaction == SCAT) pXS[l+1] += GetScatXS(i, pGroup[l+1], pGroup[l+2]);
        else if (nReaction == CAPT) pXS[l+1] += GetCaptXS(i, pGroup[l+1], pGroup[l+2], nFlag&USEAREA);
        else if (nReaction == FISS) pXS[l+1] += GetFissXS(i, pGroup[l+1], pGroup[l+2], nFlag&USEAREA);
*/
      }
    }

  }


// calculate uncertainties due to resonance parameters in each energy group

  ng = 0;
  double G1, G2, xs1, xs2, fx, NN, GG, NG;

  for (int i=0;i<NoRes();i++) {

    GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);

    //if(sfirst == 1) {
    //printf("E=%4.1lf,J=%3.1lf,g=%5.3lf,2gGn=%10.4lf+-%7.3lf,Gg=%10.3lf+-%6.3lf\n",
    //        E/1000, J, (2*J+1)/(2*(2*GetSpin()+1)),2*gGn,2*(2*J+1)/(2*(2*GetSpin()+1))*dGn, Gg, dGg);
    //}

    if (E < m_fMin) continue;
    if (E > m_fMax) break;
    G1 = Gn + Gg;

    if (G1 == 0) {
      if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %lf... skipped!\n", E);
      continue;
    }

    if (E >= pGroup[ng+1]) {
      for (ng++;ng < nGroup && E >= pGroup[ng+1];ng++);
      if (ng == nGroup) break;
    }

    if (nReaction == SCAT && pXS[ng] == 0.0) {
      pUN[ng] = 0.0;
      if (nth == 0) fprintf(stderr, "WARNING: average scattering c/s is 0 at group %d... zero uncertainty is assigned!\n", ng+1);
      continue;
    } else if (nReaction == CAPT && pXS[ng] == 0.0) {
      pUN[ng] = 0.0;
      if (nth == 0) fprintf(stderr, "WARNING: average capture c/s is 0 at group %d... zero uncertainty is assigned!\n", ng+1);
      continue;
    } else if (nReaction == FISS && pXS[ng] == 0.0) {
      pUN[ng] = 0.0;
      if (nth == 0) fprintf(stderr, "WARNING: average fission c/s is 0 at group %d... zero uncertainty is assigned!\n", ng+1);
      continue;
    }

    if (Gn == 0.0) dGn = 0.0;
    else {
      if (dGn == 0.0) dGn = m_fDefaultScatUnc;
      else dGn /= Gn;
    }

    if (Gg == 0.0) dGg = 0.0;
    else dGg /= Gg;

    if (nReaction == SCAT) xs1 = GetScatXS(i, pGroup[ng], pGroup[ng+1]);
    else if (nReaction == CAPT) xs1 = GetCaptXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
    else if (nReaction == FISS) xs1 = GetFissXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);

    for (int j=0;j<NoRes();j++) {

      GetParameter(j, E2, dE2, J2, gGn2, Gn2, dGn2, Gg2, dGg2, Gf2, dGf2, area2, darea2, farea2, dfarea2);

      if (E2 < m_fMin) continue;
      if (E2 > m_fMax) break;
      if (E2 < pGroup[ng]) continue;
      if (E2 >= pGroup[ng+1]) continue;

      G2 = Gn2 + Gg2;

      if (G2 == 0.0) {
        if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %lf... skipped!\n", E2);
        continue;
      }

      if (Gn2 == 0.0) dGn2 = 0.0;
      else {
        if (dGn2 == 0.0) dGn2 = m_fDefaultScatUnc;
        else dGn2 /= Gn2;
      }
      if (Gg2 == 0.0) dGg2= 0.0;
      else dGg2 /= Gg2;

      if (nReaction == SCAT) xs2 = GetScatXS(j, pGroup[ng], pGroup[ng+1]);
      else if (nReaction == CAPT) xs2 = GetCaptXS(j, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
      else if (nReaction == FISS) xs2 = GetFissXS(j, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);

      fx = xs1*xs2/(pXS[ng]*pXS[ng]);

      if (i == j) {
         NN = GG = 1.0;
         NG = m_fCorrNGS;
      } else {
         NN = m_fCorrNN;
         NG = m_fCorrNG;
         GG = m_fCorrGG;
      }

      if (nReaction == SCAT) {
         pUN[ng] += fx*(NN*(G1+Gg)*(G2+Gg2)*dGn*dGn2 + GG*Gg*Gg2*dGg*dGg2
                 - NG*((G1+Gg)*Gg2*dGn*dGg2 + Gg*(G2+Gg2)*dGg*dGn2))/(G1*G2);
      } else if (nReaction == CAPT) {
        if (nFlag&USEAREA && darea != 0 && darea2 != 0)
          pUN[ng] += GG*fx*darea*darea2/area/area2;
        else
          pUN[ng] += fx*(NN*Gg*Gg2*dGn*dGn2 + GG*Gn*Gn2*dGg*dGg2
                  + NG*(Gg*Gn2*dGn*dGg2 + Gn*Gg2*dGg*dGn2))/(G1*G2);
      } else if (nReaction == FISS) {
        if (nFlag&USEAREA && dfarea != 0 && dfarea2 != 0)
          pUN[ng] += GG*fx*dfarea*dfarea2/farea/farea2;
        else
          pUN[ng] += fx*(NN*Gf*Gf2*dGn*dGn2 + GG*Gn*Gn2*dGf*dGf2
                  + NG*(Gf*Gn2*dGn*dGf2 + Gn*Gf2*dGf*dGn2))/(G1+Gf)/(G2+Gf2);
      }

    }
  }

// calculate overall scattering uncertainties including potential scattering in each energy group

  for (int i=0;i<nGroup;i++) {
    if (nReaction == SCAT) {
//      printf("Sct. Unc. [%d] = %lg\n", i, sqrt(pUN[i]));
      if (i >= nFirstResGroup) {
        double total = pXS[i] + m_pPotXS[i];
        double tot2 = total*total;
        if (m_fR == 0)
          pUN[i] = pUN[i]*pXS[i]*pXS[i]/tot2;
        else
          pUN[i] = (pUN[i]*pXS[i]*pXS[i] + m_pPotUN[i]*m_pPotUN[i]*m_pPotXS[i]*m_pPotXS[i] + 2.0*m_fCorrRP*sqrt(pUN[i])*m_pPotUN[i]*pXS[i]*m_pPotXS[i])/tot2;
        pXS[i] += m_pPotXS[i] + m_pExtXS[i];
      }
//      printf("Sct. Unc. [%d] = %lg\n", i, sqrt(pUN[i]));
    }
    pUN[i] = sqrt(pUN[i]);
  }

  sfirst = 0;

  return true;

}
