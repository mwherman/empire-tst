/***********************************************************************
 *
 * Filename: resxs.cpp
 * Purpose : calculates average c/s based on Atlas
 *
 * Written by Youngsik Cho (Inspired by Pavel Oblozinsky)
 * Modified by Samuel Hoblit
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
#include <algorithm>
#include "resxs.h"

using namespace std;

double m_pStdGroup[NSTDGROUP+1] = {
  1.0000E-05, 1.0000E-01, 5.4000E-01, 4.0000E+00, 8.3153E+00,
  1.3710E+01, 2.2603E+01, 4.0169E+01, 6.7904E+01, 9.1661E+01,
  1.4863E+02, 3.0433E+02, 4.5400E+02, 7.4852E+02, 1.2341E+03,
  2.0347E+03, 3.3546E+03, 5.5308E+03, 9.1188E+03, 1.5034E+04,
  2.4788E+04, 4.0868E+04, 6.7380E+04, 1.1109E+05, 1.8316E+05,
  3.0197E+05, 4.9787E+05, 8.2085E+05, 1.3534E+06, 2.2313E+06,
  3.6788E+06, 6.0653E+06, 1.0000E+07, 1.9640E+07
};

static bool bWarning = true;

CResXS::CResXS()
{
  m_bInitScat = false;
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
  m_pStdXS = new double[NSTDGROUP];
  m_pStdUN = new double[NSTDGROUP];
  m_pStdPotXS = new double[NSTDGROUP];
  m_pStdPotUN = new double[NSTDGROUP];
  m_pStdExtXS = new double[NSTDGROUP];
  m_pPf = NULL;
  m_pSf = NULL;
}

CResXS::~CResXS()
{
  if (m_pExtXS) delete[] m_pExtXS;
  if (m_pStdPotXS) delete[] m_pStdPotXS;
  if (m_pStdPotUN) delete[] m_pStdPotUN;
  if (m_pStdExtXS) delete[] m_pStdExtXS;
  if (m_pStdXS) delete[] m_pStdXS;
  if (m_pStdUN) delete[] m_pStdUN;
  if (m_pPf) delete[] m_pPf;
  if (m_pSf) delete[] m_pSf;
}

void CResXS::SetR(double R, double dR)
{
  m_fR = R;
  m_fdR = dR;
}

void CResXS::SetCorr(double fNN, double fGG, double fRP, double fNG, double fNGS)
{
  m_fCorrNN = fNN;
  m_fCorrGG = fGG;
  m_fCorrRP = fRP;
  m_fCorrNG = fNG;
  m_fCorrNGS = fNGS;
}

void CResXS::SetRange(double fMin, double fMax)
{
  m_fMin = fMin;
  m_fMax = fMax;
}

void CResXS::SetDefaultScatUnc(double f)
{
  m_fDefaultScatUnc = f;
}

void CResXS::SetGammaFactor(double f)
{
  m_fGammaFactor = f;
}

void CResXS::InitScat(int nGroup, double *pGroup, bool bBound)
{
  if (m_bInitScat) return;
  int i;
  double e;

  m_pPotXS = new double[nGroup];
  m_pPotUN = new double[nGroup];
  m_pExtXS = new double[nGroup];

  memset(m_pExtXS, 0, nGroup*sizeof(double));

  for (i=0;i<nGroup;i++) {
    e = (pGroup[i] + pGroup[i+1])/2.0;
    GetPotentialXS(e, m_pPotXS[i], m_pPotUN[i]);
  }

  for (i=0;i<NSTDGROUP;i++) {
    e = (m_pStdGroup[i] + m_pStdGroup[i+1])/2.0;
    GetPotentialXS(e, m_pStdPotXS[i], m_pStdPotUN[i]);
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
//    printf("nFirst = %d\n", nFirst);
    for (i=0;i<NoRes();i++) {
      GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
      if (E >= 0) break;
      for (int n=nFirst;n<nGroup;n++) {
        m_pExtXS[n] += GetScatXSFrom(E, GetL(i), (2*J+1)/(2*(2*m_fSpin+1)), Gn, Gg, pGroup[n], pGroup[n+1]);
//        printf("Ext. XS = %lf\n", m_pExtXS[n]);
      }
    }    

    for (i=0;i<NoRes();i++) {
      E = GetE(i);
      if (E > 0) {
       for (nFirst=0; nFirst < NSTDGROUP && E >= m_pStdGroup[nFirst+1]; nFirst++);
       break;
      }
    }
//    printf("nFirst = %d\n", nFirst);
    for (i=0;i<NoRes();i++) {
      GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
      if (E >= 0) break;
      for (int n=nFirst;n<NSTDGROUP;n++) {
        m_pStdExtXS[n] += GetScatXSFrom(E, GetL(i), (2*J+1)/(2*(2*m_fSpin+1)), Gn, Gg, m_pStdGroup[n], m_pStdGroup[n+1]);
//        printf("Ext. XS = %lf\n", m_pStdExtXS[n]);
      }
    }    
  }
  m_bInitScat = true;
}

double CResXS::GetScatXSFrom(double e0, double l, double g, double gn,double gg, double g1, double g2)
// This subroutine is not complete !!
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

double CResXS::GetPotentialXS(double e)
{
  if (m_fR == 0) return 0;

  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);		// k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)

  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double kR2 = kR*kR;
  double t1 = 3.0 - kR2;
  double phi2 = kR - atan(3.0*kR/t1);

  double s0 = sin(phi0);
  double s1 = sin(phi1);
  double s2 = sin(phi2);
  double c0 = cos(phi0);
  double c1 = cos(phi1);
  double c2 = cos(phi2);

  double x1 = s0*s0 + 3.0*s1*s1 + 5.0*s2*s2;
  return 4.0*PI*lambda2*x1;
}

double CResXS::GetPotentialUN(double e)
{
  if (m_fR == 0) return 0;

  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);		// k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)

  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double kR2 = kR*kR;
  double t1 = 3.0 - kR2;
  double phi2 = kR - atan(3.0*kR/t1);

  double s0 = sin(phi0);
  double s1 = sin(phi1);
  double s2 = sin(phi2);
  double c0 = cos(phi0);
  double c1 = cos(phi1);
  double c2 = cos(phi2);

  double x1 = s0*s0 + 3.0*s1*s1 + 5.0*s2*s2;
  double x2 = 1.0 - 3.0*(3.0+kR2)/(t1*t1 + 9.0*kR2);
  return 2.0*m_fdR/m_fR*kR*(s0*c0 + 3.0*kR2/(1.0+kR2)*s1*c1 + 5.0*s2*c2*x2)/x1;
}

void CResXS::GetPotentialXS(double e, double& pot_xs, double& pot_un)
{
  if (m_fR == 0) {
    pot_xs = pot_un = 0;
    return;
  }
  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);		// k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)

  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double kR2 = kR*kR;
  double t1 = 3.0 - kR2;
  double phi2 = kR - atan(3.0*kR/t1);

  double s0 = sin(phi0);
  double s1 = sin(phi1);
  double s2 = sin(phi2);
  double c0 = cos(phi0);
  double c1 = cos(phi1);
  double c2 = cos(phi2);

  double x1 = s0*s0 + 3.0*s1*s1 + 5.0*s2*s2;
  pot_xs = 4.0*PI*lambda2*x1;
  //pot_un = 2.0*m_fdR/m_fR*kR*(s0*c0 + 3.0*kR2/(1.0+kR2)*s1*c1)/(s0*s0 + 3.0*s1*s1);
  double x2 = 1.0 - 3.0*(3.0+kR2)/(t1*t1 + 9.0*kR2);
  pot_un = 2.0*m_fdR/m_fR*kR*(s0*c0 + 3.0*kR2/(1.0+kR2)*s1*c1 + 5.0*s2*c2*x2)/x1;

  //double t1 = 4.0*PI*lambda2*s0*s0;
  //double t2 = 12.0*PI*lambda2*s1*s1;
  //double t3 = 20.0*PI*lambda2*s2*s2;
  //fprintf(stderr, "  Potential at E = %10.2lf  %10.3lf  %10.3lf   %10.3lf \n", e, t1, t2, t3);
  //double t1 = 2.0*m_fdR/m_fR;
  //double t2 = kR*(s0*c0 + 3.0*kR*kR/(1.0+kR*kR)*s1*c1)/(s0*s0 + 3.0*s1*s1);
  //fprintf(stderr, "  Potent  unc at E = %10.2lf  %e  %e \n", e, pot_un, x3);
}

double CResXS::GetScatXS(int n, double g1, double g2)
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

double CResXS::GetCaptXS(int n, double g1, double g2, bool usearea)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);
  if (!usearea || area == 0) area = gGn*Gg/(Gn+Gg);
  return 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*area/(E*(g2-g1));
}

double CResXS::GetFissXS(int n, double g1, double g2, bool usearea)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);
  if (!usearea || farea == 0) farea = gGn*Gf/(Gn+Gf);
  return 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*farea/(E*(g2-g1));
}

int CResXS::GetFirstResGroup(int nGroup, double *pGroup)
{
  double E, dE, J, gGn, Gn, Gg, Gf, area, farea;

  int ng = 0;
  for (int i=0;i<NoRes();i++) {
    GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

    if (E < m_fMin) continue;
    if (E > m_fMax) break;
    if (Gn+Gg == 0) continue;

    if (E >= pGroup[ng+1]) {
      for (ng++;ng < nGroup && E >= pGroup[ng+1];ng++);
    }
    return ng;
  }
}

// calculate average cross section in each energy group
bool CResXS::GetAvgResXS(int nReaction, int nGroup, double *pGroup, double *pXS, int nFlag)
{
  if (!m_bInitScat && nReaction == SCAT) InitScat(nGroup, pGroup, nFlag&BOUND);

  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double E2,dE2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  memset(pXS, 0, nGroup*sizeof(double));

  int ng = 0;
  for (int i=0;i<NoRes();i++) {

    GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

    if (E < m_fMin) continue;
    if (E > m_fMax) break;

    if (Gn+Gg == 0) {
      if (bWarning) fprintf(stderr, "WARNING: total width = 0 at E = %10.2lf... skipped!\n", E);
      continue;
    }

    if (E >= pGroup[ng+1]) {
      for (ng++;ng < nGroup && E >= pGroup[ng+1];ng++);
      if (ng == nGroup) break;
    }

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
//        fprintf(stderr, "WARNING: group %d: from E = %10.2lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+1, E);
/*
        if (nReaction == SCAT) pXS[l-1] += GetScatXS(i, pGroup[l-1], pGroup[l]);
        else if (nReaction == CAPT) pXS[l-1] += GetCaptXS(i, pGroup[l-1], pGroup[l], nFlag&USEAREA);
        else if (nReaction == FISS) pXS[l-1] += GetFissXS(i, pGroup[l-1], pGroup[l], nFlag&USEAREA);
*/
      }
    }

    for (int l=ng;l+2<nGroup;l++) {
      if (E+m_fGammaFactor*(Gn+Gg) >= pGroup[l+1]) {
//        fprintf(stderr, "WARNING: group %d: from E = %10.2lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+2, E);
/*
        if (nReaction == SCAT) pXS[l+1] += GetScatXS(i, pGroup[l+1], pGroup[l+2]);
        else if (nReaction == CAPT) pXS[l+1] += GetCaptXS(i, pGroup[l+1], pGroup[l+2], nFlag&USEAREA);
        else if (nReaction == FISS) pXS[l+1] += GetFissXS(i, pGroup[l+1], pGroup[l+2], nFlag&USEAREA);
*/
      }
    }
  }
  bWarning = false;
}

// calculate absolute uncertainties due to resonance parameters in each energy group
bool CResXS::GetAbsResUN(int nReaction, int nGroup, double *pGroup, double *pUN, int nFlag)
{
  if (!m_bInitScat && nReaction == SCAT) InitScat(nGroup, pGroup, nFlag&BOUND);

  memset(pUN, 0, nGroup*sizeof(double));

  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double E2,dE2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  int ng = 0;
  double G1, G2, xs1, xs2, fx, NN, GG, NG;

  for (int i=0;i<NoRes();i++) {

    GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);

    if (E < m_fMin) continue;
    if (E > m_fMax) break;
    G1 = Gn + Gg;

    if (G1 == 0) continue;

    if (E >= pGroup[ng+1]) {
      if (pUN[ng] != 0) pUN[ng] = sqrt(pUN[ng]);
      for (ng++;ng < nGroup && E >= pGroup[ng+1];ng++);
      if (ng == nGroup) break;
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

      if (G2 == 0.0) continue;

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

      fx = xs1*xs2;

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
  if (ng < nGroup && pUN[ng] != 0) pUN[ng] = sqrt(pUN[ng]);
}

bool CResXS::AddPotentialXS(int nFirstResGroup, int nGroup, double *pGroup, double *pXS)
{
  for (int i=0;i<nGroup;i++) {
    if (i >= nFirstResGroup) {
      pXS[i] += GetPotentialXS((pGroup[i] + pGroup[i+1])/2.0) + m_pExtXS[i];
    }
  }
}

bool CResXS::AddPotentialUN(int nFirstResGroup, int nGroup, double *pGroup, double *pUN)
{
  double pot_xs, pot_un;
  for (int i=0;i<nGroup;i++) {
    if (i >= nFirstResGroup) {
      pot_xs = GetPotentialXS((pGroup[i] + pGroup[i+1])/2.0);
      pot_un = pot_xs*GetPotentialUN((pGroup[i] + pGroup[i+1])/2.0);
//      printf("%d: %lE\n", i+1, pot_un);
      pUN[i] = sqrt(pUN[i]*pUN[i] + pot_un*pot_un + 2.0*m_fCorrRP*pUN[i]*pot_un);
    }
  }
}

int CResXS::GetStdGroupXSnUN(int nGroup, double *pGroup, int nSubGroup, double *pSubGroup, double *pXS, double *pUN, double **pNewUN)
{
  *pNewUN = new double[nSubGroup];

  for (int i=0;i<nSubGroup;i++) {
//    printf("%d: %lE - %lE %lE\n", i+1, pSubGroup[i], pSubGroup[i+1], pXS[i]);
    for (int j=0;j<nGroup;j++) {
      if (pSubGroup[i] >= pGroup[j] && pSubGroup[i+1] <= pGroup[j+1]) {
        (*pNewUN)[i] = pUN[j]/pXS[i];
//        printf("STD %2d: %.5lE - %.5lE %.5lE/%.5lE = %.5lE\n", i+1, pSubGroup[i], pSubGroup[i+1], pUN[j], pXS[i], (*pNewUN)[i]);
      }
    }
  }
  return nSubGroup;
}

/*
int CResXS::GetStdGroupXSnUN(int nGroup, double *pGroup, double *pXS, double *pUN, double **pNewGroup, double **pNewXS, double **pNewUN)
{
  int nNewGroup = NSTDGROUP + nGroup + 2;
  *pNewGroup = new double[nNewGroup+1];
  *pNewXS = new double[nNewGroup];
  *pNewUN = new double[nNewGroup];

  double e1, e2;
  int n = 0;
  for (int i=0;i<NSTDGROUP;i++) {
    for (int j=0;j<nGroup && pGroup[j] < m_pStdGroup[i+1];j++) {
      e1 = max(pGroup[j], m_pStdGroup[i]);
      e2 = min(pGroup[j+1],m_pStdGroup[i+1]);
      if (e2 > e1 && pXS[j] > 0) {
        (*pNewGroup)[n] = e1;
        (*pNewGroup)[n+1] = e2;
        (*pNewXS)[n] = pXS[j];
        (*pNewUN)[n] = pUN[j];
        ++n;
      }
    }
  }
  nNewGroup = n;
  return nNewGroup;
}
*/

// calcualte the penetration and shift factors at each resonance energy
void CResXS::CalcFactor()
{
  if (m_pPf != NULL && m_pSf != NULL) return;

  double k;
  double rho;
  double rho2,rho3,rho4, rho6, rho8;
  double E, l;

  if (m_pPf) delete[] m_pPf;
  if (m_pSf) delete[] m_pSf;

  m_pPf = new double[NoRes()];
  m_pSf = new double[NoRes()];

  for (int i=0;i<NoRes();i++) {

    E = fabs(GetE(i));

    l = GetL(i);

    k = 2.196771e-3*m_nA/(m_nA+1)*sqrt(E);       // k in units of sqrt(barn) = 1.0E-14 m
    rho = k*m_fR*.1;                             // kR. 1fm = 0.1*sqrt(barn)

    if (l == 0) {
      m_pPf[i] = rho;
      m_pSf[i] = 0;
    } else if (l == 1) {
      rho2 = rho*rho;
      m_pPf[i] = rho*rho2/(1+rho2);
      m_pSf[i] = -1/(1+rho2);
    } else if (l == 2) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      m_pPf[i] = rho*rho4/(9+3*rho2+rho4);
      m_pSf[i] = -(18+3*rho2)/(9+3*rho2+rho4);
    } else if (l == 3) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      m_pPf[i] = rho*rho6/(225+45*rho2+6*rho4+rho6);
      m_pSf[i] = -(675+90*rho2+6*rho4)/(225+45*rho2+6*rho4+rho6);
    } else if (l == 4) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      rho8 = rho4*rho4;
      m_pPf[i] = rho*rho8/(11025+1575*rho2+135*rho4+10*rho6+rho8);
      m_pSf[i] = -(44100+4725*rho2+270*rho4+10*rho6)/(11025+1575*rho2+135*rho4+10*rho6+rho8);
    }
  }
}

// calculate multi-level Breit-Wigner cross section at energy e
// xs[0]: total cross section
// xs[1]: neutron cross section
// xs[2]: radiative capture cross section
// xs[3]: fission cross section
void CResXS::GetXS(double e, double *xs)
{
  CalcFactor();
  
  memset(xs, 0, 4*sizeof(double));
  double k = 2.196771e-3*m_nA/(m_nA+1)*sqrt(e);       // k in units of sqrt(barn) = 1.0E-14 m
  double rho = k*m_fR*.1;                             // kR. 1fm = 0.1*sqrt(barn)
  double pf;
  double sf;
  double phi;
  double rho2,rho3,rho4, rho6, rho8;

  double E, J, gGn, Gn, Gg, Gf, area, farea;
  double I, basej, Gne, Gt, Ep, x;
  double gjsum, gj[12], sum1[12], sum2[12], factor;
  double cos2p, sin2p;
  int j, nj;

  I = GetSpin();

  for (int l=0;l<GetNLS();l++) {

    if (l == 0) {
      pf = rho;
      sf = 0;
      phi = rho;
    } else if (l == 1) {
      rho2 = rho*rho;
      pf = rho*rho2/(1+rho2);
      sf = -1/(1+rho2);
      phi = rho - atan(rho);
    } else if (l == 2) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      pf = rho*rho4/(9+3*rho2+rho4);
      sf = -(18+3*rho2)/(9+3*rho2+rho4);
      phi = rho - atan(3*rho/(3-rho2));
    } else if (l == 3) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      pf = rho*rho6/(225+45*rho2+6*rho4+rho6);
      sf = -(675+90*rho2+6*rho4)/(225+45*rho2+6*rho4+rho6);
      phi = rho-atan((15*rho-rho2)/(15-6*rho2));
    } else if (l == 4) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      rho8 = rho4*rho4;
      pf = rho*rho8/(11025+1575*rho2+135*rho4+10*rho6+rho8);
      sf = -(44100+4725*rho2+270*rho4+10*rho6)/(11025+1575*rho2+135*rho4+10*rho6+rho8);
      phi = rho-atan((105*rho-10*rho*rho2)/(105-45*rho2+rho4));
    }

    basej = fabs(fabs(I-l)-0.5);
    nj = I+l+0.5 - basej + 1;

    gjsum = 0;
    for (int i=0;i<nj;i++) {
      gj[i] = (2*(basej+i)+1)/(4*I+2);
      gjsum += gj[i];
    }

    memset(sum1, 0, 12*sizeof(double));
    memset(sum2, 0, 12*sizeof(double));

    for (int i=0;i<NoRes();i++) {

      GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

      if (GetL(i) != l) continue;

      Gne = pf*Gn/m_pPf[i];
      Gt = Gne + Gg + Gf;
      Ep = E + Gn*(m_pSf[i]-sf)/(2*m_pPf[i]);
      x = 2*(e-Ep)/Gt;
      factor = 2*Gne/Gt/(1+x*x);
      j = int(J-basej);
      sum1[j] += factor;
      sum2[j] += factor*x;
      factor *= gj[int(J-basej)]/Gt;
      xs[2] += factor*Gg;
      xs[3] += factor*Gf;
    }

    cos2p = 1-cos(2*phi);
    sin2p = sin(2*phi);

    for (int i=0;i<nj;i++)
      xs[1] += gj[i]*((cos2p-sum1[i])*(cos2p-sum1[i]) + (sin2p+sum2[i])*(sin2p+sum2[i]));
    xs[1] += 2*(2*l+1-gjsum)*cos2p;
  }
  xs[1] *= PI/(k*k);
  xs[2] *= 2*PI/(k*k);
  xs[3] *= 2*PI/(k*k);
  xs[0] = xs[1]+xs[2]+xs[3];
}

// build energy grid for computing the average cross sections
void CResXS::SetupEnergyGrid()
{
  double E;

  for (int i=0;i<NoRes();i++) {
    E = GetE(i);

    if (E < 0) continue;

    link.Add(E);
    for (int j=1;j<10;j++) {
      if (E-j > 0) link.Add(E-0.1*j);
      link.Add(E+0.1*j);
    }
    for (int j=1;j<10;j++) {
      if (E-j > 0) link.Add(E-j);
      link.Add(E+j);
    }
    for (int j=1;j<10;j++) {
      if (E-10*j > 0) link.Add(E-10*j);
      link.Add(E+10*j);
    }
    for (int j=1;j<=20;j++) {
      if (E-100*j > 0) link.Add(E-100*j);
      link.Add(E+100*j);
    }
  }
  E = 1e-5;
  while (E <= 1e6) {
    for (int i=1;i<=9;i++) {
      link.Add(i*E);
    }
    E *= 10;
  }
  link.Add(1e7);

/*
  // for verification
  double xs[4];
  E = link.GetFirst();
  while (E > 0) {
    GetXS(E, xs);
    printf("%12.6lE,%12.6lE\n", E, xs[1]);
    E = link.GoNext();
  }
*/
}

void CResXS::AddEnergyPoints(int n, double *e)
{
  for (int i=0;i<n;i++) link.Add(e[i]);
}

// compute the average cross sections int the specified energy region using MLBW
void CResXS::GetAvgXS(double E1, double E2, double *xs)
{
  double xs1[4], xs2[4];
  double sum[4];
  double e1, e2;

  memset(xs, 0, 4*sizeof(double));
  memset(sum, 0, 4*sizeof(double));
/*
  int n, length;
  double *pArray = link.GetArray(length);
  for (n=0;n<length && pArray[n] < E1;n++) ;
  if (n+1 >= length) return;

  GetXS(e1=pArray[n], xs1);
  for (++n;n < length && e2 <= E2; n++) {
    GetXS(e2=pArray[n], xs2);
    sum[0] += (xs2[0]+xs1[0])*(e2-e1)/2;
    sum[1] += (xs2[1]+xs1[1])*(e2-e1)/2;
    sum[2] += (xs2[2]+xs1[2])*(e2-e1)/2;
    sum[3] += (xs2[3]+xs1[3])*(e2-e1)/2;
    e1 = e2;
    memcpy(xs1, xs2, 4*sizeof(double));
  }

  xs[0] = sum[0]/(E2-E1);
  xs[1] = sum[1]/(E2-E1);
  xs[2] = sum[2]/(E2-E1);
  xs[3] = sum[3]/(E2-E1);
*/

/*
  if ((e1 = link.GetCurrent()) == 0 || e1 > E1) {
    e1 = link.GetFirst();
  }
  while (e1 > 0 && e1 < E1) e1 = link.GoNext();
*/
  e1 = link.LocateValue(E1);
  if (e1 == 0) return;

  GetXS(e1, xs1);
  e2 = link.GoNext();
  while (e2 > 0 && e2 <= E2) {
    GetXS(e2, xs2);
    sum[0] += (xs2[0]+xs1[0])*(e2-e1)/2;
    sum[1] += (xs2[1]+xs1[1])*(e2-e1)/2;
    sum[2] += (xs2[2]+xs1[2])*(e2-e1)/2;
    sum[3] += (xs2[3]+xs1[3])*(e2-e1)/2;
    e1 = e2;
    memcpy(xs1, xs2, 4*sizeof(double));
    e2 = link.GoNext();
  }

  xs[0] = sum[0]/(E2-E1);
  xs[1] = sum[1]/(E2-E1);
  xs[2] = sum[2]/(E2-E1);
  xs[3] = sum[3]/(E2-E1);

}

// compute the average cross sections int the energy groups using MLBW
void CResXS::GetAvgXS(int nReaction, int nGroup, double *pGroup, double *pXS)
{
  memset(pXS, 0, nGroup*sizeof(double));

  double xs[4];

  link.GoFirst();
  for (int i=0;i<nGroup;i++) {
    GetAvgXS(pGroup[i], pGroup[i+1], xs);
    switch (nReaction) {
      case TOTL: pXS[i] = xs[0]; break;
      case SCAT: pXS[i] = xs[1]; break;
      case CAPT: pXS[i] = xs[2]; break;
      case FISS: pXS[i] = xs[3]; break;
    }
  }
}
