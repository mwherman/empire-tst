/***********************************************************************
 *
 * Filename: kercen.cpp
 * Purpose : Calculate average cross sections and their uncertainties
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "kernel.h"
#include "util.h"
#include "endf.h"

#define PI		3.14159

#define SCAT		0
#define CAPT		1
#define FISS		2

static CKernel kernel;
static char szScatGroup[PATH_MAX];	// file containing energy group information for scattering
static char szCaptGroup[PATH_MAX];	// file containing energy group information for capture
static char szFissGroup[PATH_MAX];	// file containing energy group information for fission
static char szAtlasDir[PATH_MAX];	// Atlas directory
static bool bPotential = true;		// flag to take into account potential scattering
static bool bUseArea = false;		// flag to use kernel from Atlas for capture cross section calculation
static bool bScattering = false;	// flag to calculate scattering cross sections and their uncertainties
static bool bCapture = false;		// flag to calculate capture cross sections and their uncertainties
static bool bFission = false;		// flag to calculate fission cross sections and their uncertainties
static bool bFile2 = true;		// flag to create file 33 instead of file 2
static bool bFile3 = true;		// flag to create file 33 instead of file 3
static bool bFile32 = true;		// flag to create file 33 instead of file 32
static bool bFile33 = false;		// flag to create file 33 instead of file 33
static double fCorrNGS = 0;		// correlation between scattering and capture widths of single resonance
static double fCorrNN = .5;		// correlation between scattering widths of different resonances
static double fCorrGG = .5;		// correlation between capture widths of different resonances
static double fCorrFF = .5;		// correlation between fission widths of different resonances
static double fCorrNG = 0;		// correlation between scattering and capture widths of different resonances
static double fCorrNNB = .5;		// correlation between scattering cross sections in different energy bins
static double fCorrGGB = .5;		// correlation between capture cross sections in different energy bins
static double fCorrRP = -.5;		// correlation between resonance and potential scatterings
static double fCorrNNRT = 0;		// correlation between scattering cross sections in the thermal and resonance regions
static double fCorrGGRT = 0;		// correlation between capture cross sections in the thermal and resonance regions
static int nZ=-1, nA=-1, nMat=0;	// Z, A, MAt number
static double fAwr = 0;			// AWR
static double fMin = .0, fMax = 1e38;	// lower and upper energy of resonance region
static double fGammaFactor = 4;		// factor multiplied to the total width to estimate the resonance area
static double fDefaultScatUnc = .1;	// default scattering width uncertainty
static double fR = -1, fdR = -1;			// scattering radius
static int nScatGroup = 0, nCaptGroup = 0, nFissGroup = 0;	// number of energy groups for scattering and capture reactions
static int nFirstResScatGroup = -1, nFirstResCaptGroup = -1, nFirstResFissGroup = 0;	// index of first energy group where resonance starts
static double *pScatGroup = NULL;	// scattering group energy boundariies
static double *pCaptGroup = NULL;	// capture group energy boundariies
static double *pFissGroup = NULL;	// fission group energy boundariies
static double *pPotXS = NULL;		// potential scattering cross sections
static double *pSctXS = NULL;		// average scattering cross sections
static double *pCapXS = NULL;		// average capture cross sections
static double *pFisXS = NULL;		// average fission cross sections
static double *pPotUN = NULL;		// potential scattering cross sections uncertainties
static double *pSctUN = NULL;		// scattering cross section uncertainties
static double *pCapUN = NULL;		// capture cross section uncertainties
static double *pFisUN = NULL;		// fission cross section uncertainties
static double *pCumSctXS = NULL;
static double *pCumCapXS = NULL;
static double *pCumFisXS = NULL;
static double *pCumSctUN = NULL;
static double *pCumCapUN = NULL;
static double *pCumFisUN = NULL;

double GetPotentialXS(int n)
{
  double e = (pScatGroup[n+1]+pScatGroup[n])/2;
  double lambda2 = 1/((2.19677e-3*nA/(nA+1)*sqrt(e))*(2.19677e-3*nA/(nA+1)*sqrt(e)));
  double kR = 2.19677e-3*nA/(nA+1)*sqrt(e)*fR*.1;
/*
  double fR0_inf = .22;
  double fR1_inf = .34;

  double phi0 = -kR + asin(kR*fR0_inf);
  double phi1 = -kR + atan(kR) + asin(kR*kR*kR*fR1_inf/(1+kR*kR));
*/
  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double phi2 = kR-atan(3*kR/(3-kR*kR));
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

double GetPotentialUnc(int n)
{
  double e = (pScatGroup[n+1]+pScatGroup[n])/2;
  double kR = 2.19677e-3*nA/(nA+1)*sqrt(e)*fR*.1;
  double phi0 = kR;
  double phi1 = kR-atan(kR);

  return 2*fdR/fR*kR*(sin(phi0)*cos(phi0)+3*(1-(1/(1+kR*kR)))*sin(phi1)*cos(phi1))/
         (sin(phi0)*sin(phi0)+3*sin(phi1)*sin(phi1));
}

bool Calculate(int nth, double *pGroup, int nGroup, int &nFirstResGroup, int nReaction)
{
  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double E2,dE2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

// calculate average cross section in each energy group
  nFirstResGroup = -1;
  int n = 0;
  for (int i=0;i<kernel.NoRes();i++) {
    kernel.GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
    if (E < fMin) continue;
    if (E > fMax) break;
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
      pSctXS[n] += kernel.GetScatXS(i, pGroup[n], pGroup[n+1]);
//      printf("group %d: %lf from E=%lf, width=%lf\n", n+1, kernel.GetScatXS(i, pGroup[n], pGroup[n+1]), E, Gn+Gg);
    } else if (nReaction == CAPT) {
      pCapXS[n] += kernel.GetCaptXS(i, pGroup[n], pGroup[n+1], bUseArea);
    } else if (nReaction == FISS) {
      pFisXS[n] += kernel.GetFissXS(i, pGroup[n], pGroup[n+1], bUseArea);
    }
    for (int l=n;l>0;l--) {
      if (E-fGammaFactor*(Gn+Gg) < pGroup[l]) {
        fprintf(stderr, "WARNING: group %d: from E = %lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+1, E);
/*
        if (nReaction == SCAT) pSctXS[l-1] += kernel.GetScatXS(i, pGroup[l-1], pGroup[l]);
        else if (nReaction == CAPT) pCapXS[l-1] += kernel.GetCaptXS(i, pGroup[l-1], pGroup[l], bUseArea);
        else if (nReaction == FISS) pFisXS[l-1] += kernel.GetFissXS(i, pGroup[l-1], pGroup[l], bUseArea);
*/
      }
    }
    for (int l=n;l+2<nGroup;l++) {
      if (E+fGammaFactor*(Gn+Gg) >= pGroup[l+1]) {
        fprintf(stderr, "WARNING: group %d: from E = %lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+2, E);
/*
        if (nReaction == SCAT) pSctXS[l+1] += kernel.GetScatXS(i, pGroup[l+1], pGroup[l+2]);
        else if (nReaction == CAPT) pCapXS[l+1] += kernel.GetCaptXS(i, pGroup[l+1], pGroup[l+2], bUseArea);
        else if (nReaction == FISS) pFisXS[l+1] += kernel.GetFissXS(i, pGroup[l+1], pGroup[l+2], bUseArea);
*/
      }
    }
  }

// calculate uncertainties due to resonance parameters in each energy group
  n = 0;
  double factor1, factor2;
  for (int i=0;i<kernel.NoRes();i++) {
    kernel.GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
/*
    printf("E=%4.1lf,J=%3.1lf,g=%5.3lf,2gGn=%10.4lf+-%7.3lf,Gg=%10.3lf+-%6.3lf\n",
            E/1000, J, (2*J+1)/(2*(2*kernel.GetSpin()+1)),2*gGn,2*(2*J+1)/(2*(2*kernel.GetSpin()+1))*dGn, Gg, dGg);
*/
    if (E < fMin) continue;
    if (E > fMax) break;
    if (Gn+Gg == 0) {
      if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %lf... skipped!\n", E);
      continue;
    }
    if (E >= pGroup[n+1]) {
      for (n++;n < nGroup && E >= pGroup[n+1];n++);
      if (n == nGroup) break;
    }
    if (nReaction == SCAT && pSctXS[n] == 0) {
      pSctUN[n] = 0;
      if (nth == 0) fprintf(stderr, "WARNING: average scattering c/s is 0 at group %d... zero uncertainty is assigned!\n", n+1);
      continue;
    } else if (nReaction == CAPT && pCapXS[n] == 0) {
      pCapUN[n] = 0;
      if (nth == 0) fprintf(stderr, "WARNING: average capture c/s is 0 at group %d... zero uncertainty is assigned!\n", n+1);
      continue;
    } else if (nReaction == FISS && pFisXS[n] == 0) {
      pFisUN[n] = 0;
      if (nth == 0) fprintf(stderr, "WARNING: average fission c/s is 0 at group %d... zero uncertainty is assigned!\n", n+1);
      continue;
    }
    if (Gn == 0) dGn = 0;
    else {
      if (dGn == 0) dGn = fDefaultScatUnc;
      else dGn /= Gn;
    }
    if (Gg == 0) dGg = 0;
    else dGg /= Gg;

    if (nReaction == SCAT) factor1 = kernel.GetScatXS(i, pGroup[n], pGroup[n+1]);
    else if (nReaction == CAPT) factor1 = kernel.GetCaptXS(i, pGroup[n], pGroup[n+1], bUseArea);
    else if (nReaction == FISS) factor1 = kernel.GetFissXS(i, pGroup[n], pGroup[n+1], bUseArea);

    for (int j=0;j<kernel.NoRes();j++) {
      kernel.GetParameter(j, E2, dE2, J2, gGn2, Gn2, dGn2, Gg2, dGg2, Gf2, dGf2, area2, darea2, farea2, dfarea2);
      if (E < fMin) continue;
      if (E > fMax) break;
      if (Gn2+Gg2 == 0) {
        if (nth == 0) fprintf(stderr, "WARNING: total width = 0 at E = %lf... skipped!\n", E2);
        continue;
      }
      if (Gn2 == 0) dGn2 = 0;
      else {
        if (dGn2 == 0) dGn2 = fDefaultScatUnc;
        else dGn2 /= Gn2;
      }
      if (Gg2 == 0) dGg2= 0;
      else dGg2 /= Gg2;

      if (nReaction == SCAT) factor2 = kernel.GetScatXS(j, pGroup[n], pGroup[n+1]);
      else if (nReaction == CAPT) factor2 = kernel.GetCaptXS(j, pGroup[n], pGroup[n+1], bUseArea);
      else if (nReaction == FISS) factor2 = kernel.GetFissXS(j, pGroup[n], pGroup[n+1], bUseArea);

      if (E2 >= pGroup[n] && E2 < pGroup[n+1]) {
        if (i == j) {
          if (nReaction == SCAT) pSctUN[n] += factor1*factor2/(pSctXS[n]*pSctXS[n])*(
                                  (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2-(Gn+Gg+Gg)*Gg2*dGn*dGg2*fCorrNGS-
                                  Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*fCorrNGS+Gg*Gg2*dGg*dGg2)/
                                  (Gn+Gg)/(Gn2+Gg2);
          else if (nReaction == CAPT) {
            if (bUseArea && darea != 0 && darea2 != 0)
              pCapUN[n] += factor1*factor2*darea*darea2/area/area2/(pCapXS[n]*pCapXS[n]);
            else
              pCapUN[n] += factor1*factor2/(pCapXS[n]*pCapXS[n])*(
                           Gg*Gg2*dGn*dGn2+
                           Gg*Gn2*dGn*dGg2*fCorrNGS+Gn*Gg2*dGg*dGn2*fCorrNGS+
                           Gn*Gn2*dGg*dGg2)/
                           (Gn+Gg)/(Gn2+Gg2);
          } else if (nReaction == FISS) {
            if (bUseArea && dfarea != 0 && dfarea2 != 0)
              pFisUN[n] += factor1*factor2*dfarea*dfarea2/farea/farea2/(pFisXS[n]*pFisXS[n]);
            else
              pFisUN[n] += factor1*factor2/(pFisXS[n]*pFisXS[n])*(
                           Gf*Gf2*dGn*dGn2+
                           Gf*Gn2*dGn*dGf2*fCorrNGS+Gn*Gf2*dGf*dGn2*fCorrNGS+
                           Gn*Gn2*dGf*dGf2)/
                           (Gn+Gg+Gf)/(Gn2+Gg2+Gf2);
          }
/*
          if (n==79 && nReaction == SCAT) printf("DEBUG: %5.1lf & %5.1lf: factor1=%9.3lE,factor2=%9.3lE,Gn1+Gg1=%lg,"
                                     "Gn2+Gg2=%lg,SctXS=%lg,g1=%lg,g2=%lg,value=%lg\n",
                                     E, E2, factor1, factor2, Gn+Gg, Gn2+Gg2, pSctXS[n],pGroup[n],pGroup[n+1],
                                     factor1*factor2/(pSctXS[n]*pSctXS[n])*(
                                     (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2-(Gn+Gg+Gg)*Gg2*dGn*dGg2*fCorrNGS-
                                     Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*fCorrNGS+Gg*Gg2*dGg*dGg2)/
                                     (Gn+Gg)/(Gn2+Gg2));
*/
        } else {
          if (nReaction == SCAT) pSctUN[n] += factor1*factor2/(pSctXS[n]*pSctXS[n])*(
                                  (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2*fCorrNN-(Gn+Gg+Gg)*Gg2*dGn*dGg2*fCorrNG-
                                  Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*fCorrNG+Gg*Gg2*dGg*dGg2*fCorrGG)/
                                  (Gn+Gg)/(Gn2+Gg2);
          else if (nReaction == CAPT) {
            if (bUseArea && darea != 0 && darea2 != 0)
              pCapUN[n] += fCorrGG*factor1*factor2*darea*darea2/area/area2/(pCapXS[n]*pCapXS[n]);
            else
              pCapUN[n] += factor1*factor2/(pCapXS[n]*pCapXS[n])*(
                           Gg*Gg2*dGn*dGn2*fCorrNN+Gg*Gn2*dGn*dGg2*fCorrNG+
                           Gn*Gg2*dGg*dGn2*fCorrNG+Gn*Gn2*dGg*dGg2*fCorrGG)/
                           (Gn+Gg)/(Gn2+Gg2);
          } else if (nReaction == FISS) {
            if (bUseArea && dfarea != 0 && dfarea2 != 0)
              pFisUN[n] += fCorrGG*factor1*factor2*dfarea*dfarea2/farea/farea2/(pFisXS[n]*pFisXS[n]);
            else
              pFisUN[n] += factor1*factor2/(pFisXS[n]*pFisXS[n])*(
                           Gf*Gf2*dGn*dGn2*fCorrNN+Gf*Gn2*dGn*dGf2*fCorrNG+
                           Gn*Gf2*dGf*dGn2*fCorrNG+Gn*Gn2*dGf*dGf2*fCorrGG)/
                           (Gn+Gg+Gf)/(Gn2+Gg2+Gf2);
          }
/*
          if (n==79 && nReaction == SCAT) printf("DEBUG: %5.1lf & %5.1lf: factor1=%9.3lE,factor2=%9.3lE,Gn1+Gg1=%lg,"
                                     "Gn2+Gg2=%lg,SctXS=%lg,g1=%lg,g2=%lg,value=%lg\n",
                                     E, E2, factor1, factor2, Gn+Gg, Gn2+Gg2, pSctXS[n],pGroup[n],pGroup[n+1],
                                     factor1*factor2*(
                                     (Gn+Gg+Gg)*(Gn2+Gg2+Gg2)*dGn*dGn2*fCorrNN-(Gn+Gg+Gg)*Gg2*dGn*dGg2*fCorrNG-
                                     Gg*(Gn2+Gg2+Gg2)*dGg*dGn2*fCorrNG+Gg*Gg2*dGg*dGg2*fCorrGG)/(pSctXS[n]*pSctXS[n])/
                                     (Gn+Gg)/(Gn2+Gg2));
*/
        }
      }
    }
  }

// calculate overall uncertainties including potential scattering in each energy group
  for (int i=0;i<nGroup;i++) {
    if (nReaction == SCAT) {
//      printf("Sct. Unc. [%d] = %lg\n", i, sqrt(pSctUN[i]));
      pCumSctXS[i] += pSctXS[i];
      if (i >= nFirstResGroup) {
        if (bPotential) {
          double total = pSctXS[i]+pPotXS[i];
          if (fR == 0) pSctUN[i] = pSctUN[i]*pSctXS[i]*pSctXS[i]/total/total;
          else {
/*
            pSctUN[i] = pSctUN[i]*pSctXS[i]*pSctXS[i]/total/total+
                        4*fdR/fR*fdR/fR*pPotXS[i]*pPotXS[i]/total/total+
                        fCorrRP*2*sqrt(pSctUN[i])*2*fdR/fR*pSctXS[i]*pPotXS[i]/total/total;
*/
            pSctUN[i] = pSctUN[i]*pSctXS[i]*pSctXS[i]/total/total+
                        pPotUN[i]*pPotUN[i]*pPotXS[i]*pPotXS[i]/total/total+
                        2*fCorrRP*sqrt(pSctUN[i])*pPotUN[i]*pSctXS[i]*pPotXS[i]/total/total;
          }
        }
      }
//      printf("Sct. Unc. [%d] = %lg\n", i, sqrt(pSctUN[i]));
      pCumSctUN[i] += sqrt(pSctUN[i]);
    } else if (nReaction == CAPT) {
      pCumCapXS[i] += pCapXS[i];
      pCumCapUN[i] += sqrt(pCapUN[i]);
    } else if (nReaction == FISS) {
      pCumFisXS[i] += pFisXS[i];
      pCumFisUN[i] += sqrt(pFisUN[i]);
    }
  }
  return true;
}

void Plot()
{
  char tmp[20];
  FILE *fp;

  // create a data file including the average scattering cross sections
  if ((fp = fopen("SCAT-XS.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'SCAT-XS.dat'\n", stderr);
    exit(1);
  }
  for (int i=0;i<nScatGroup;i++)
    fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[i]*1E-3, pCumSctXS[i]);
//    fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[i], pCumSctXS[i]);
  fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[nScatGroup]*1E-3, pCumSctXS[nScatGroup-1]);
//  fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[nScatGroup], pCumSctXS[nScatGroup-1]);
  fclose(fp);

  // create a data file including the uncertainties of scattering cross sections
  if ((fp = fopen("SCAT-UNC.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'SCAT-UNC.dat'\n", stderr);
    exit(1);
  }
  for (int i=0;i<nScatGroup;i++)
    fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[i]*1E-3, pCumSctUN[i]*100);
//    fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[i], pCumSctUN[i]*100);
  fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[nScatGroup]*1E-3, pCumSctUN[nScatGroup-1]*100);
//  fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[nScatGroup], pCumSctUN[nScatGroup-1]*100);
  fclose(fp);

  if ((fp = fopen("scattering.gp", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'scattering.gp'\n", stderr);
    exit(1);
  }
  sprintf(tmp, "%s-%d", GetSymbol(nZ), nA);
  fprintf(fp, "# Gnuplot script file for plotting scattering c/s uncertainties for %s\n", tmp);
  fprintf(fp, "# Type the command: gnuplot> load 'scattering.gp'\n");
  fprintf(fp, "set terminal postscript enhanced color\n");
  fprintf(fp, "set output 'scattering.ps'\n");
  fprintf(fp, "set title 'Scattering c/s uncertainties for %s' font 'Times-Roman,24'\n", tmp);
  fprintf(fp, "set xlabel 'Neutron energy (keV)' font 'Times-Roman,20'\n");
  fprintf(fp, "set ylabel 'Uncertainty, %%' font 'Times-Roman,20'\n");
  fprintf(fp, "set y2label 'Cross section, b' font 'Times-Roman,20'\n");
  fprintf(fp, "set log x\n");
  fprintf(fp, "set log y2\n");
  fprintf(fp, "set format y2 \"10^{%%L}\"\n");
  fprintf(fp, "set key left top\n");
  fprintf(fp, "set xrange [1E-3:%lf]\n", pScatGroup[nScatGroup]/1000);
  fprintf(fp, "set ytics nomirror\n");
  fprintf(fp, "set y2tics 1e-5,0.1,1e5\n");
  fprintf(fp, "set my2tics 10\n");
/*
  fprintf(fp, "set style line 1 lt 1 lc rgb 'dark-green' lw 3\n");
  fprintf(fp, "set style line 2 lt 2 lc rgb 'blue' lw 3\n");
  fprintf(fp, "set style line 3 lt 3 lc rgb 'red' lw 3\n");
  fprintf(fp, "set style line 4 lt 6 lc rgb 'purple' lw 3\n");
  fprintf(fp, "set style line 5 lt 5 lc rgb 'cyan' lw 3\n");
*/
//  fprintf(fp, "plot 'SCAT-UNC.dat' using 1:2 title 'scattering xs' with steps lt -1");
  fprintf(fp, "plot 'SCAT-UNC.dat' using 1:2 axis x1y1 title 'Uncertainty' with steps lt -1 lw 1, \\\n");
  fprintf(fp, "     'SCAT-XS.dat' using 1:2 axis x1y2 title 'Cross section' with steps lt 3 lw 2");
  fclose(fp);
  system("gnuplot scattering.gp");

  // create a data file including the average capture cross sections
  if ((fp = fopen("CAPT-XS.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'CAPT-XS.dat'\n", stderr);
    exit(1);
  }
  for (int i=0;i<nCaptGroup;i++)
    fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[i]*1E-3, pCumCapXS[i]);
//    fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[i], pCumCapXS[i]);
  fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[nCaptGroup]*1E-3, pCumCapXS[nCaptGroup-1]);
//  fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[nCaptGroup], pCumCapXS[nCaptGroup-1]);
  fclose(fp);

  // create a data file including the uncertainties of capture cross sections
  if ((fp = fopen("CAPT-UNC.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'CAPT-UNC.dat'\n", stderr);
    exit(1);
  }
  for (int i=0;i<nCaptGroup;i++)
    fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[i]*1E-3, pCumCapUN[i]*100);
//    fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[i], pCumCapUN[i]*100);
  fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[nCaptGroup]*1E-3, pCumCapUN[nCaptGroup-1]*100);
//  fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[nCaptGroup], pCumCapUN[nCaptGroup-1]*100);
  fclose(fp);

  if ((fp = fopen("capture.gp", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'capture.gp'\n", stderr);
    exit(1);
  }
  sprintf(tmp, "%s-%d", GetSymbol(nZ), nA);
  fprintf(fp, "# Gnuplot script file for plotting capture c/s uncertainties for %s\n", tmp);
  fprintf(fp, "# Type the command: gnuplot> load 'capture.gp'\n");
  fprintf(fp, "set terminal postscript enhanced color\n");
  fprintf(fp, "set output 'capture.ps'\n");
  fprintf(fp, "set title 'Capture c/s uncertainties for %s' font 'Times-Roman,24'\n", tmp);
  fprintf(fp, "set xlabel 'Neutron energy (keV)' font 'Times-Roman,20'\n");
  fprintf(fp, "set ylabel 'Uncertainty, %%' font 'Times-Roman,20'\n");
  fprintf(fp, "set y2label 'Cross section, b' font 'Times-Roman,20'\n");
  fprintf(fp, "set log x\n");
  fprintf(fp, "set log y2\n");
  fprintf(fp, "set format y2 \"10^{%%L}\"\n");
  fprintf(fp, "set key left top\n");
  fprintf(fp, "set xrange [1E-3:%lf]\n", pCaptGroup[nCaptGroup]/1000);
  fprintf(fp, "set ytics nomirror\n");
  fprintf(fp, "set y2tics 1e-5,0.1,1e5\n");
  fprintf(fp, "set my2tics 10\n");
/*
  fprintf(fp, "set style line 1 lt 1 lc rgb 'dark-green' lw 3\n");
  fprintf(fp, "set style line 2 lt 2 lc rgb 'blue' lw 3\n");
  fprintf(fp, "set style line 3 lt 3 lc rgb 'red' lw 3\n");
  fprintf(fp, "set style line 4 lt 6 lc rgb 'purple' lw 3\n");
  fprintf(fp, "set style line 5 lt 5 lc rgb 'cyan' lw 3\n");
*/
//  fprintf(fp, "plot 'CAPT-UNC.dat' using 1:2 title 'capture xs' with steps lt -1");
  fprintf(fp, "plot 'CAPT-UNC.dat' using 1:2 axis x1y1 title 'Uncertainty' with steps lt -1 lw 1, \\\n");
  fprintf(fp, "     'CAPT-XS.dat' using 1:2 axis x1y2 title 'Cross section' with steps lt 3 lw 2");
  fclose(fp);
  system("gnuplot capture.gp");
}

void WriteMatrix()
{
  FILE *fp;
  if (bScattering && (fp=fopen("scattering.matrix", "w")) != NULL) {
    for (int i=0;i<nScatGroup;i++) {
      for (int j=0;j<nScatGroup;j++) {
        if (i == j)
          fprintf(fp, " %8.3lE", pCumSctUN[i]*pCumSctUN[j]);
        else if (i < nFirstResScatGroup && j < nFirstResScatGroup)
          fprintf(fp, " %8.3lE", pCumSctUN[i]*pCumSctUN[j]);
        else if ((i < nFirstResScatGroup && j >= nFirstResScatGroup) ||
                 (i >= nFirstResScatGroup && j < nFirstResScatGroup))
          fprintf(fp, " %8.3lE", fCorrNNRT*pCumSctUN[i]*pCumSctUN[j]);
        else fprintf(fp, " %8.3lE", fCorrNNB*pCumSctUN[i]*pCumSctUN[j]);
      }
      fputs("\n", fp);
    }
    fclose(fp);
  }
  if (bCapture && (fp=fopen("capture.matrix", "w")) != NULL) {
    for (int i=0;i<nCaptGroup;i++) {
      for (int j=0;j<nCaptGroup;j++) {
        if (i == j)
          fprintf(fp, " %8.3lE", pCumCapUN[i]*pCumCapUN[j]);
        else if (i < nFirstResCaptGroup && j < nFirstResCaptGroup)
          fprintf(fp, " %8.3lE", pCumCapUN[i]*pCumCapUN[j]);
        else if ((i < nFirstResCaptGroup && j >= nFirstResCaptGroup) ||
                 (i >= nFirstResCaptGroup && j < nFirstResCaptGroup))
          fprintf(fp, " %8.3lE", fCorrGGRT*pCumCapUN[i]*pCumCapUN[j]);
        else fprintf(fp, " %8.3lE", fCorrGGB*pCumCapUN[i]*pCumCapUN[j]);
      }
      fputs("\n", fp);
    }
    fclose(fp);
  }
}

void WriteEndf()
{
  char s[67];
  CEndf endf;
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  double dE, dGn, dGg, dGf, darea, dfarea;
  int i, j, n;
  sprintf(s, "%d-%s-%d.endf", nZ, GetSymbol(nZ), nA);
  if (!endf.Open(s, "w")) return;
  printf("ENDF file '%s' is created\n", s);
  endf.WriteText("", 1, 0, 0);
  endf.WriteCont(nZ*1000.+nA, fAwr, 1, 0, 0, 1, nMat, 1, 451);
  endf.WriteCont(0., .0, 0, 0, 0, 6, nMat, 1, 451);
  endf.WriteCont(1., 2e7, 0, 0, 10, 7, nMat, 1, 451);
  n = 1;
  if (bFile2) n++;
  if (bFile3) n += 2;
  if (bFile32) n++;
  if (bFile33) n += 2;
  endf.WriteCont(0., 0., 0, 0, 3, n, nMat, 1, 451);
  sprintf(s, "%3d-%2s-%3d Y.S. CHO", nZ, GetSymbol(nZ), nA);
  endf.WriteText(s, nMat, 1, 451);
  endf.WriteText(" -OUTPUT OF KERCEN", nMat, 1, 451);
  sprintf(s, " --MATERIAL %4d", nMat);
  endf.WriteText(s, nMat, 1, 451);
  endf.WriteCont(0., 0., 1, 451, 9999, 0, nMat, 1, 451);
  if (bFile2) endf.WriteCont(0., 0., 2, 151, 9999, 0, nMat, 1, 451);
  if (bFile3) {
    endf.WriteCont(0., 0., 3, 2, 9999, 0, nMat, 1, 451);
    endf.WriteCont(0., 0., 3, 102, 9999, 0, nMat, 1, 451);
  }
  if (bFile32) endf.WriteCont(0., 0., 32, 151, 9999, 0, nMat, 1, 451);
  if (bFile33) {
    endf.WriteCont(0., 0., 33, 2, 9999, 0, nMat, 1, 451);
    endf.WriteCont(0., 0., 33, 102, 9999, 0, nMat, 1, 451);
  }
  endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 1, 0);
  endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);

  if (bFile2) {
    int nRes = 0;
    int nL = 0;
    int lcount[3];
    int l;
    memset(lcount, 0, 3*sizeof(int));
    for (i=0;i<kernel.NoRes();i++) {
      kernel.GetParameter(i, E, l);
      if (E > fMax) break;
      ++lcount[l];
    }
    nRes = i;
    for (i=0;i<3;i++)
      if (lcount[i]) ++nL;

    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 2, 151);
    endf.WriteCont(nZ*1000.+nA, 1., 0, 0, 1, 0, nMat, 2, 151);
    if (bScattering || bCapture) {
      double fE = 0;
      if (bScattering) fE = pScatGroup[nScatGroup];
      if (bCapture && pCaptGroup[nCaptGroup] > fE) fE = pCaptGroup[nCaptGroup];
      endf.WriteCont(1e-5, fE, 1, 2, 0, 0, nMat, 2, 151);
    } else endf.WriteCont(1e-5, kernel.GetE(kernel.NoRes()-1), 1, 2, 0, 0, nMat, 2, 151);
    endf.WriteCont(kernel.GetSpin(), .1*fR, 0, 0, nL, 0, nMat, 2, 151);
    for (n=0;n<3;n++) {
      if (lcount[n] <= 0) continue;
      endf.WriteCont(fAwr, .1*fR, n, 0, 6*lcount[n], lcount[n], nMat, 2, 151);
      for (i=0;i<nRes;i++) {
        if (kernel.GetL(i) == n) {
          kernel.GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
          endf.WriteListItem(E, J, Gn+Gg+Gf, Gn, Gg, Gf, nMat, 2, 151);
        }
      }
    }
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 2, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
  }

  if (bFile3) {
    int eint[2] = {2, 2};
    double sigma[4] = {1e-5, 0, 2e7, 0};
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 3, 2);
    endf.WriteTab1(0., 0., 0, 0, 1, 2, nMat, 3, 2, eint, sigma);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 3, 0);
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 3, 102);
    endf.WriteTab1(0., 0., 0, 0, 1, 2, nMat, 3, 102, eint, sigma);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 3, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
  }

  if (bFile32) {
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 32, 151);
    endf.WriteCont(nZ*1000.+nA, 1., 0, 0, 1, 0, nMat, 32, 151);
    endf.WriteCont(1e-5, kernel.GetE(kernel.NoRes()-1), 1, 2, 0, 0, nMat, 32, 151);
    endf.WriteCont(kernel.GetSpin(), .1*fR, 0, 2, 0, 0, nMat, 32, 151);
    endf.WriteCont(fAwr, .0, 0, 0, 12*kernel.NoRes(), kernel.NoRes(), nMat, 32, 151);
    for (i=0;i<kernel.NoRes();i++) {
      kernel.GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
      endf.WriteListItem(E, J, Gn+Gg+Gf, Gn, Gg, Gf, nMat, 32, 151);
      endf.WriteListItem(dE, 0., 0., dGn, dGg, dGf, nMat, 32, 151);
    }
    endf.WriteCont(0., 0., 3, 0, 0, 0, nMat, 32, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 32, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
  }

  if (bFile33) {
    double *pCov = (double*)malloc(sizeof(double)*nScatGroup*(nScatGroup+1)/2);
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 0, 1, nMat, 33, 2);
    endf.WriteCont(.0, .0, 0, 2, 0, 1, nMat, 33, 2);
    for (i=0,n=0;i<nScatGroup;i++) {
      pCov[n++] = pCumSctUN[i]*pCumSctUN[i];
      for (j=0;j<nScatGroup-i-1;j++) {
        if (i < nFirstResScatGroup && j+i+1 < nFirstResScatGroup)
          pCov[n++] = pCumSctUN[i]*pCumSctUN[j+i+1];
        else if (i < nFirstResScatGroup && j+i+1 >= nFirstResScatGroup)
           pCov[n++] = fCorrNNRT*pCumSctUN[i]*pCumSctUN[j+i+1];
        else pCov[n++] = fCorrNNB*pCumSctUN[i]*pCumSctUN[j+i+1];
      }
    }
    endf.WriteList(.0, .0, 1, 5, nScatGroup*(nScatGroup+1)/2+nScatGroup+1, nScatGroup+1, nMat, 33, 2, pCov, pScatGroup);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 33, 0);

    pCov = (double*)realloc(pCov, sizeof(double)*nCaptGroup*(nCaptGroup+1)/2);
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 0, 1, nMat, 33, 102);
    endf.WriteCont(.0, .0, 0, 102, 0, 1, nMat, 33, 102);
    for (i=0,n=0;i<nCaptGroup;i++) {
      pCov[n++] = pCumCapUN[i]*pCumCapUN[i];
      for (j=0;j<nCaptGroup-i-1;j++) {
        if (i < nFirstResScatGroup && j+i+1 < nFirstResScatGroup)
          pCov[n++] = pCumCapUN[i]*pCumCapUN[j+i+1];
        else if (i < nFirstResCaptGroup && j+i+1 >= nFirstResCaptGroup)
          pCov[n++] = fCorrGGRT*pCumCapUN[i]*pCumCapUN[j+i+1];
        else pCov[n++] = fCorrGGB*pCumCapUN[i]*pCumCapUN[j+i+1];
      }
    }
    endf.WriteList(.0, .0, 1, 5, nCaptGroup*(nCaptGroup+1)/2+nCaptGroup+1, nCaptGroup+1, nMat, 33, 102, pCov, pCaptGroup);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 33, 0);

    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
    free(pCov);
  }

  endf.WriteCont(0., 0., 0, 0, 0, 0, 0, 0, 0);
  endf.WriteCont(0., 0., 0, 0, 0, 0, -1, 0, 0);
  endf.Close();
}

int main(int argc, char **argv)
{
  FILE *fp;
  char s[1024];
  char szEndfFiles[1024] = "";
  bool bReassignLJ = false;		// flag to reassign L and J
  bool bPrintAtlas = false;		// flag to print resonance parameters
  bool bPrintPotential = false;		// flag to print potential cross sections
  double fScatteringXS = -1 , fdScatteringXS = -1;	// thermal scattering cross section
  double fCaptureXS = -1, fdCaptureXS = -1;		// thermal capture cross section
  int nIteration = 1000;
  double fdGg0 = -1, fdGg1 = -1, fdGg2 = -1;
  if (argc < 2) {
    fputs("usage: kercen inputfile\n", stderr);
    exit(1);
  }
  if ((fp = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "cannot open '%s'\n", argv[1]);
    exit(1);
  }
  char *key, *value, *p;
  while (fgets(s, 1024, fp)) {
    if (s[0] == '#') continue;
    if ((p = strchr(s, '#')) != NULL) *p = 0;
    if ((key=strtok(s, " \t\n")) == NULL || (value = strtok(NULL, " \t\n")) == NULL) continue;
//    printf("key=[%s] value=[%s]\n", key, value);
    if (!strcasecmp(key, "z")) nZ = atoi(value);
    else if (!strcasecmp(key, "a")) nA = atoi(value);
    else if (!strcasecmp(key, "mat")) nMat = atoi(value);
    else if (!strcasecmp(key, "awr")) fAwr = atof(value);
    else if (!strcasecmp(key, "iteration")) nIteration = atoi(value);
    else if (!strcasecmp(key, "sxs")) fScatteringXS = atof(value);
    else if (!strcasecmp(key, "dsxs")) fdScatteringXS = atof(value);
    else if (!strcasecmp(key, "cxs")) fCaptureXS = atof(value);
    else if (!strcasecmp(key, "dcxs")) fdCaptureXS = atof(value);
    else if (!strcasecmp(key, "r")) fR = atof(value);
    else if (!strcasecmp(key, "dr")) fdR = atof(value);
    else if (!strcasecmp(key, "atlasdir")) strcpy(szAtlasDir, value);
    else if (!strcasecmp(key, "defaultscatunc")) fDefaultScatUnc = atof(value);
    else if (!strcasecmp(key, "dgg0")) fdGg0 = atof(value);
    else if (!strcasecmp(key, "dgg1")) fdGg1 = atof(value);
    else if (!strcasecmp(key, "dgg2")) fdGg2 = atof(value);
    else if (!strcasecmp(key, "scatgroup")) strcpy(szScatGroup, value);
    else if (!strcasecmp(key, "captgroup")) strcpy(szCaptGroup, value);
    else if (!strcasecmp(key, "fissgroup")) strcpy(szFissGroup, value);
    else if (!strcasecmp(key, "emin")) fMin = atof(value);
    else if (!strcasecmp(key, "emax")) fMax = atof(value);
    else if (!strcasecmp(key, "gammafactor")) fGammaFactor = atof(value);
    else if (!strcasecmp(key, "reassignlj")) bReassignLJ = atoi(value);
    else if (!strcasecmp(key, "usearea")) bUseArea = atoi(value);
    else if (!strcasecmp(key, "potential")) bPotential = atoi(value);
    else if (!strcasecmp(key, "endffiles")) strcpy(szEndfFiles, value);
    else if (!strcasecmp(key, "printatlas")) bPrintAtlas = atoi(value);
    else if (!strcasecmp(key, "printpotential")) bPrintPotential = atoi(value);
    else if (!strcasecmp(key, "corrngs")) fCorrNGS = atof(value);
    else if (!strcasecmp(key, "corrnn")) fCorrNN = atof(value);
    else if (!strcasecmp(key, "corrng")) fCorrNG = atof(value);
    else if (!strcasecmp(key, "corrgg")) fCorrGG = atof(value);
    else if (!strcasecmp(key, "corrff")) fCorrFF = atof(value);
    else if (!strcasecmp(key, "corrrp")) fCorrRP = atof(value);
    else if (!strcasecmp(key, "corrnnb")) fCorrNNB = atof(value);
    else if (!strcasecmp(key, "corrggb")) fCorrGGB = atof(value);
    else if (!strcasecmp(key, "corrnnrt")) fCorrNNRT = atof(value);
    else if (!strcasecmp(key, "corrggrt")) fCorrGGRT = atof(value);
  }
  fclose(fp);

  if (nZ == -1) {
    fputs("ERROR: Z should be provided\n", stderr);
    exit(1);
  }
  if (nA == -1) {
    fputs("ERROR: A should be provided\n", stderr);
    exit(1);
  }
  if (szAtlasDir[0] == 0) {
    fputs("ERROR: The name of the directory containig the Atlas should be provided\n", stderr);
    exit(1);
  }
  if (szScatGroup[0] != 0) bScattering = true;
  if (szCaptGroup[0] != 0) bCapture = true;
  if (szFissGroup[0] != 0) bFission = true;

  if (szEndfFiles[0] != 0) {
    bFile2 = bFile3 = bFile32 = bFile33 = false;
    char *p = strtok(szEndfFiles, ",");
    while (p) {
      int n = atoi(p);
      switch (n) {
        case 2: bFile2 = true; break;
        case 3: bFile3 = true; break;
        case 32: bFile32 = true; break;
        case 33: bFile33 = true; break;
      }
      p = strtok(NULL, ",");
    }
  }

  puts("#############################################################");
  puts("##                                                         ##");
  puts("##                        KERCEN v1.0                      ##");
  puts("##                                                         ##");
  puts("#############################################################");
  puts("");
  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############       INPUT DESCRIPTION        ##############");
  puts("###############                                ##############");
  puts("#############################################################");
  printf("Z = %d, A = %d, AWR = %lf, MAT = %d\n", nZ, nA, fAwr, nMat);
  printf("EMin = %9.3lE, EMax = %9.3lE\n", fMin, fMax);
  printf("Atlas directory: %s\n", szAtlasDir);
  printf("Groupfile for scattering: %s\n", szScatGroup);
  printf("Groupfile for capture   : %s\n", szCaptGroup);
  printf("Groupfile for fission   : %s\n", szFissGroup);

  if (bScattering) {
    if ((fp = fopen(szScatGroup, "r")) == NULL) {
      fprintf(stderr, "ERROR: cannot open '%s'\n", szScatGroup);
      exit(1);
    }
    fgets(s, 1024, fp);
    sscanf(s, "%d", &nScatGroup);
    pScatGroup = new double[nScatGroup+1];
    for (int i=0;i<nScatGroup+1;i++) fscanf(fp, "%lf", pScatGroup+i);
    fclose(fp);
  }

  if (bCapture) {
    if ((fp = fopen(szCaptGroup, "r")) == NULL) {
      fprintf(stderr, "ERROR: cannot open '%s'\n", szCaptGroup);
      exit(1);
    }
    fgets(s, 1024, fp);
    sscanf(s, "%d", &nCaptGroup);
    pCaptGroup = new double[nCaptGroup+1];
    for (int i=0;i<nCaptGroup+1;i++) fscanf(fp, "%lf", pCaptGroup+i);
    fclose(fp);
  }
 
  if (bFission) {
    if ((fp = fopen(szFissGroup, "r")) == NULL) {
      fprintf(stderr, "ERROR: cannot open '%s'\n", szFissGroup);
      exit(1);
    }
    fgets(s, 1024, fp);
    sscanf(s, "%d", &nFissGroup);
    pFissGroup = new double[nFissGroup+1];
    for (int i=0;i<nFissGroup+1;i++) fscanf(fp, "%lf", pFissGroup+i);
    fclose(fp);
  }

  kernel.SetBaseDirectory(szAtlasDir);
  if (!kernel.ReadProperties(nZ, nA)) {
    fprintf(stderr, "ERROR: Error occured reading Atlas properties from the directory %s\n", szAtlasDir);
    exit(1);
  }

  if (bReassignLJ) kernel.ReassignLJ();
  if (fdGg0 != -1) kernel.SetdGg0(fdGg0);
  if (fdGg1 != -1) kernel.SetdGg1(fdGg1);
  if (fdGg2 != -1) kernel.SetdGg2(fdGg2);
  if (fAwr != -1) kernel.SetAwr(fAwr);
  else {
    fprintf(stderr, "ERROR: Atomic weight should be provided\n");
    exit(1);
  }


  if (!kernel.ReadParameters(nZ, nA)) {
    fprintf(stderr, "ERROR: Error occured reading Atlas paraemters from the directory %s\n", szAtlasDir);
    exit(1);
  }
  if (bPrintAtlas) {
    kernel.AssignJ();
    double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
    for (int i=0;i<kernel.NoRes();i++) {
      kernel.GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
      if (dGn == 0) dGn = fDefaultScatUnc * Gn;
      printf("E=%6.2lf, J=%3.1lf, g=%4.2lf, gGn=%9.3lE+-%9.3lE, Gg=%9.3lE+-%9.3lE, Gf=%9.3lE+-%9.3lE, A=%9.3lE+-%9.3lE, Af=%9.3lE+-%9.3lE\n",
              E/1000, J, (2*J+1)/(2*(2*kernel.GetSpin()+1)),gGn,(2*J+1)/(2*(2*kernel.GetSpin()+1))*dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
    }
  }

  if (fScatteringXS == -1) kernel.GetScatteringXS(fScatteringXS, fdScatteringXS);
  if (fCaptureXS == -1) kernel.GetCaptureXS(fCaptureXS, fdCaptureXS);
  if (fR == -1) fR = kernel.GetR();
  if (fdR == -1) fdR = kernel.GetdR();

  if (fR == -1) {
    fputs("ERROR: Scattering radius was not assigned\n", stderr);
    exit(1);
  }
  if (fdR == -1) {
    fputs("ERROR: Scattering radius uncertainty was not assigned\n", stderr);
    exit(1);
  }
  if (fScatteringXS == -1) {
    fputs("ERROR: Thermal scattering xs was not assigned\n", stderr);
    exit(1);
  }
  if (fdScatteringXS == -1) {
    fputs("ERROR: Thermal scattering xs uncertainty was not assigned\n", stderr);
    exit(1);
  }
  if (fCaptureXS == -1) {
    fputs("ERROR: Thermal capture xs was not assigned\n", stderr);
    exit(1);
  }
  if (fdCaptureXS == -1) {
    fputs("ERROR: Thermal capture xs uncertainty was not assigned\n", stderr);
    exit(1);
  }
  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############    GLOBAL INPUT PARAMETERS     ##############");
  puts("###############                                ##############");
  puts("#############################################################");
  printf("R = %lf +- %lf\n", fR, fdR);
  printf("Thermal scattering xs = %6.3lf += %lf\n", fScatteringXS, fdScatteringXS);
  printf("Thermal capture xs    = %6.3lf += %lf\n", fCaptureXS, fdCaptureXS);
  printf("Default scattering width uncertainty = %lf %%\n", 100*fDefaultScatUnc);
  printf("CorrNGS = %lf\n", fCorrNGS);
  printf("CorrNN  = %lf, CorrGG  = %lf, CorrNG  = %lf\n", fCorrNN, fCorrGG, fCorrNG);
  printf("CorrNNB = %lf, CorrGGB = %lf\n", fCorrNNB, fCorrGGB);
  printf("CorrRP  = %lf\n", fCorrRP);

  if (bScattering) {
    pPotXS = new double[nScatGroup];
    pPotUN = new double[nScatGroup];
    pSctXS = new double[nScatGroup];
    pSctUN = new double[nScatGroup];
    pCumSctXS = new double[nScatGroup];
    pCumSctUN = new double[nScatGroup];
    memset(pPotXS, 0, nScatGroup*sizeof(double));
    memset(pPotUN, 0, nScatGroup*sizeof(double));
    memset(pCumSctXS, 0, nScatGroup*sizeof(double));
    memset(pCumSctUN, 0, nScatGroup*sizeof(double));
    for (int i=0;i<nScatGroup;i++) {
      pPotXS[i] = GetPotentialXS(i);
      pPotUN[i] = GetPotentialUnc(i);
    }
  }
  if (bCapture) {
    pCapXS = new double[nCaptGroup];
    pCapUN = new double[nCaptGroup];
    pCumCapXS = new double[nCaptGroup];
    pCumCapUN = new double[nCaptGroup];
    memset(pCumCapXS, 0, nCaptGroup*sizeof(double));
    memset(pCumCapUN, 0, nCaptGroup*sizeof(double));
  }
  if (bFission) {
    pFisXS = new double[nFissGroup];
    pFisUN = new double[nFissGroup];
    pCumFisXS = new double[nFissGroup];
    pCumFisUN = new double[nFissGroup];
    memset(pCumFisXS, 0, nFissGroup*sizeof(double));
    memset(pCumFisUN, 0, nFissGroup*sizeof(double));
  }



  if (bPrintPotential) {
    puts("#############################################################");
    puts("###############                                ##############");
    puts("###############    POTENTIAL SCATTERING C/S    ##############");
    puts("###############                                ##############");
    puts("#############################################################");
    for (int i=0;i<nScatGroup;i++)
      printf("%3d %10.4lE - %10.4lE: %11.5lE\n", i+1, pScatGroup[i], pScatGroup[i+1], pPotXS[i]);
  }

  for (int i=0;i<nIteration;i++) {
    kernel.AssignJ();
    if (bScattering) {
      memset(pSctXS, 0, nScatGroup*sizeof(double));
      memset(pSctUN, 0, nScatGroup*sizeof(double));
      Calculate(i, pScatGroup, nScatGroup, nFirstResScatGroup, SCAT);
    }
    if (bCapture) {
      memset(pCapXS, 0, nCaptGroup*sizeof(double));
      memset(pCapUN, 0, nCaptGroup*sizeof(double));
      Calculate(i, pCaptGroup, nCaptGroup, nFirstResCaptGroup, CAPT);
    }
    if (bFission) {
      memset(pFisXS, 0, nFissGroup*sizeof(double));
      memset(pFisUN, 0, nFissGroup*sizeof(double));
      Calculate(i, pFissGroup, nFissGroup, nFirstResFissGroup, FISS);
    }
  }

  if (bScattering) {
    for (int n=0;n<nScatGroup;n++) {
      pCumSctXS[n] /= nIteration;
      pCumSctUN[n] /= nIteration;
    }
  }

  if (bCapture) {
    for (int n=0;n<nCaptGroup;n++) {
      pCumCapXS[n] /= nIteration;
      pCumCapUN[n] /= nIteration;
    }
  }

  if (bFission) {
    for (int n=0;n<nFissGroup;n++) {
      pCumFisXS[n] /= nIteration;
      pCumFisUN[n] /= nIteration;
    }
  }

  if (bScattering) {
    for (int n=0;n<nScatGroup;n++) {
      if (n >= nFirstResScatGroup) {
        if (bPotential) pCumSctXS[n] += pPotXS[n];
//        printf("pot xs %d = %lf\n", n, GetPotentialXS(n));
        continue;
      }
      pCumSctUN[n] += fdScatteringXS/fScatteringXS;
      pCumSctXS[n] += fScatteringXS;
    }
  }

  // calculate thermal capture cross section
  if (bCapture) {
    for (int n=0;n<nCaptGroup;n++) {
      if (n >= nFirstResCaptGroup) break;
      pCumCapUN[n] += fdCaptureXS/fCaptureXS;
      double sum=0;
      double step=(pCaptGroup[n+1]-pCaptGroup[n])/10000;
      for (int i=0;i<10000;i+=2)
        sum+=fCaptureXS*(sqrt(0.0253/(pCaptGroup[n]+i*step))+
             4*sqrt(0.0253/(pCaptGroup[n]+(i+1)*step))+sqrt(0.0253/(pCaptGroup[n]+(i+2)*step)))*step/3;
      pCumCapXS[n] += sum/(pCaptGroup[n+1]-pCaptGroup[n]);
    }
  }

  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############             RESULTS            ##############");
  puts("###############                                ##############");
  puts("#############################################################");

// calculate resonance integral and its uncertainty
  if (bScattering) {
    double sri=0, dsri=0;
    double u1, u2;
    for (int i=0;i<nScatGroup;i++) {
      if (pScatGroup[i+1] <= 0.5) continue;
      if (pScatGroup[i] < 0.5) {
        sri += pCumSctXS[i]*(pScatGroup[i+1]-0.5)/(pScatGroup[i+1]-pScatGroup[i])/sqrt(0.5*pScatGroup[i+1])*(pScatGroup[i+1]-0.5);
/*
        printf("%10.4lE - %10.4lE: ri for scat = %8.4lf\n",
                0.5, pScatGroup[i+1],
                pCumSctXS[i]*(pScatGroup[i+1]-0.5)/(pScatGroup[i+1]-pScatGroup[i])/sqrt(0.5*pScatGroup[i+1])*(pScatGroup[i+1]-0.5));
*/
        u1 = pCumSctUN[i]*pCumSctXS[i]*(pScatGroup[i+1]-0.5)/(pScatGroup[i+1]-pScatGroup[i])/sqrt(0.5*pScatGroup[i+1])*(pScatGroup[i+1]-0.5);
      } else {
        sri += pCumSctXS[i]/sqrt(pScatGroup[i]*pScatGroup[i+1])*(pScatGroup[i+1]-pScatGroup[i]);
        u1 = pCumSctUN[i]*pCumSctXS[i]/sqrt(pScatGroup[i]*pScatGroup[i+1])*(pScatGroup[i+1]-pScatGroup[i]);
      }

      for (int j=0;j<nScatGroup;j++) {
        if (pScatGroup[j+1] <= 0.5) continue;
        if (pScatGroup[j] < 0.5)
          u2 = pCumSctUN[j]*pCumSctXS[j]*(pScatGroup[j+1]-0.5)/(pScatGroup[j+1]-pScatGroup[j])/sqrt(0.5*pScatGroup[j+1])*(pScatGroup[j+1]-0.5);
        else
          u2 = pCumSctUN[j]*pCumSctXS[j]/sqrt(pScatGroup[j]*pScatGroup[j+1])*(pScatGroup[j+1]-pScatGroup[j]);
        if (i == j) dsri += u1 * u2;
        else {
          if (i < nFirstResScatGroup && j < nFirstResScatGroup) dsri += u1 * u2;
          else if ((i < nFirstResScatGroup && j >= nFirstResScatGroup) ||
                   (i >= nFirstResScatGroup && j < nFirstResScatGroup)) dsri += fCorrNNRT * u1 * u2;
          else if (i >= nFirstResScatGroup && j >= nFirstResScatGroup) dsri += fCorrNNB * u1 * u2;
        }
      }
    }
    dsri = sqrt(dsri);
    printf("RI for scattering = %7.2lf +- %7.2lf (%6.2lf %%)\n", sri, dsri, dsri/sri*100);
  }

  if (bCapture) {
    double cri=0, dcri=0;
    double u1, u2;
    for (int i=0;i<nCaptGroup;i++) {
      if (pCaptGroup[i+1] <= 0.5) continue;
      if (pCaptGroup[i] < 0.5) {
        cri += pCumCapXS[i]*(pCaptGroup[i+1]-0.5)/(pCaptGroup[i+1]-pCaptGroup[i])/sqrt(0.5*pCaptGroup[i+1])*(pCaptGroup[i+1]-0.5);
/*
        printf("%10.4lE - %10.4lE: ri for capt = %8.4lf\n",
                0.5, pCaptGroup[i+1],
                pCumCapXS[i]*(pCaptGroup[i+1]-0.5)/(pCaptGroup[i+1]-pCaptGroup[i])/sqrt(0.5*pCaptGroup[i+1])*(pCaptGroup[i+1]-0.5));
*/
        u1 = pCumCapUN[i]*pCumCapXS[i]*(pCaptGroup[i+1]-0.5)/(pCaptGroup[i+1]-pCaptGroup[i])/sqrt(0.5*pCaptGroup[i+1])*(pCaptGroup[i+1]-0.5);
      } else {
        cri += pCumCapXS[i]/sqrt(pCaptGroup[i]*pCaptGroup[i+1])*(pCaptGroup[i+1]-pCaptGroup[i]);
        u1 = pCumCapUN[i]*pCumCapXS[i]/sqrt(pCaptGroup[i]*pCaptGroup[i+1])*(pCaptGroup[i+1]-pCaptGroup[i]);
      }
      for (int j=0;j<nCaptGroup;j++) {
        if (pCaptGroup[j+1] <= 0.5) continue;
        if (pCaptGroup[j] < 0.5)
          u2 = pCumCapUN[j]*pCumCapXS[j]*(pCaptGroup[j+1]-0.5)/(pCaptGroup[j+1]-pCaptGroup[j])/sqrt(0.5*pCaptGroup[j+1])*(pCaptGroup[j+1]-0.5);
        else
          u2 = pCumCapUN[j]*pCumCapXS[j]/sqrt(pCaptGroup[j]*pCaptGroup[j+1])*(pCaptGroup[j+1]-pCaptGroup[j]);
        if (i == j) dcri += u1 * u2;
        else {
          if (i < nFirstResCaptGroup && j < nFirstResCaptGroup) dcri += u1 * u2;
          else if ((i < nFirstResCaptGroup && j >= nFirstResCaptGroup) ||
                   (i >= nFirstResCaptGroup && j < nFirstResCaptGroup)) dcri += fCorrGGRT * u1 * u2;
          else if (i >= nFirstResCaptGroup && j >= nFirstResCaptGroup) dcri += fCorrGGB * u1 * u2;
        }
      }
    }
    dcri = sqrt(dcri);
    printf("RI for capture    = %7.2lf +- %7.2lf (%6.2lf %%)\n", cri, dcri, dcri/cri*100);
  }
//  printf("first res. group = %d, %d\n", nFirstResScatGroup, nFirstResCaptGroup);

  if (bScattering) {
    puts("Average scattering cross sections");
    for (int i=0;i<nScatGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pScatGroup[i], pScatGroup[i+1], pCumSctXS[i], pCumSctUN[i]*100);
    }
  }
  if (bCapture) {
    puts("Average capture cross sections");
    for (int i=0;i<nCaptGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pCaptGroup[i], pCaptGroup[i+1], pCumCapXS[i], pCumCapUN[i]*100);
    }
  }

  if (bFission) {
    puts("Average fission cross sections");
    for (int i=0;i<nFissGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pFissGroup[i], pFissGroup[i+1], pCumFisXS[i], pCumFisUN[i]*100);
    }
  }

//  WriteMatrix();

  WriteEndf();

//  Plot();

  if (pScatGroup) delete[] pScatGroup;
  if (pCaptGroup) delete[] pCaptGroup;
  if (pFissGroup) delete[] pFissGroup;
  if (pPotXS) delete[] pPotXS;
  if (pPotUN) delete[] pPotUN;
  if (pSctXS) delete[] pSctXS;
  if (pCapXS) delete[] pCapXS;
  if (pFisXS) delete[] pFisXS;
  if (pSctUN) delete[] pSctUN;
  if (pCapUN) delete[] pCapUN;
  if (pFisUN) delete[] pFisUN;
  if (pCumSctXS) delete[] pCumSctXS;
  if (pCumCapXS) delete[] pCumCapXS;
  if (pCumFisXS) delete[] pCumFisXS;
  if (pCumSctUN) delete[] pCumSctUN;
  if (pCumCapUN) delete[] pCumCapUN;
  if (pCumFisUN) delete[] pCumFisUN;
}
