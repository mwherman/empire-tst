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
#include <algorithm>
#include <math.h>
#include "kernel.h"
#include "util.h"
#include "endf.h"

using namespace std;

static CKernel kernel;
static bool bScattering = false;	// flag to calculate scattering cross sections and their uncertainties
static bool bCapture = false;		// flag to calculate capture cross sections and their uncertainties
static bool bFission = false;		// flag to calculate fission cross sections and their uncertainties
static int nZ=-1, nA=-1, nMat=0;	// Z, A, MAt number
static double fAwr = 0;			// AWR
static double fMin = .0, fMax = 1e38;	// lower and upper energy of resonance region
static double fR = -1, fdR = -1;			// scattering radius
static int nScatGroup = 0, nCaptGroup = 0, nFissGroup = 0;	// number of energy groups for scattering and capture reactions
static int nFirstResScatGroup = -1, nFirstResCaptGroup = -1, nFirstResFissGroup = 0;	// index of first energy group where resonance starts
static double *pScatGroup = NULL;	// scattering group energy boundariies
static double *pCaptGroup = NULL;	// capture group energy boundariies
static double *pFissGroup = NULL;	// fission group energy boundariies
static double *pCumSctXS = NULL;
static double *pCumSctXS2 = NULL;
static double *pCumCapXS = NULL;
static double *pCumCapXS2 = NULL;
static double *pCumFisXS = NULL;
static double *pCumFisXS2 = NULL;
static double *pCumSctUN = NULL;
static double *pCumCapUN = NULL;
static double *pCumFisUN = NULL;

void Plot()
{
  char tmp[20];
  FILE *fp;
  int i;
  double xer;

  // create a data file including the average scattering cross sections
  if ((fp = fopen("SCAT-XS.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'SCAT-XS.dat'\n", stderr);
    exit(1);
  }
  for (i=0;i<nScatGroup;i++)
    fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[i], pCumSctXS[i]);
  fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[i], pCumSctXS[i-1]);
  fclose(fp);

  // create a data file including the uncertainties of scattering cross sections
  if ((fp = fopen("SCAT-UNC.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'SCAT-UNC.dat'\n", stderr);
    exit(1);
  }
  for (i=0;i<nScatGroup;i++) {
    xer = 100.0*sqrt(pCumSctUN[i]*pCumSctUN[i] + pCumSctXS2[i]);
    fprintf(fp, "%10.4lE %5.2lf %5.2lf %5.2lf\n", pScatGroup[i], 100.0*pCumSctUN[i], 100.0*sqrt(pCumSctXS2[i]), xer);
  }
  fprintf(fp, "%10.4lE %5.2lf %5.2lf %5.2lf\n", pScatGroup[i], 100.0*pCumSctUN[i-1], 100.0*sqrt(pCumSctXS2[i-1]), xer);
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
  fprintf(fp, "set xlabel 'Incident neutron energy, eV' font 'Times-Roman,20'\n");
  fprintf(fp, "set ylabel 'Uncertainty, %%' font 'Times-Roman,20'\n");
  fprintf(fp, "set y2label 'Cross section, b' font 'Times-Roman,20'\n");
  fprintf(fp, "set log x\n");
  fprintf(fp, "set log y2\n");
  fprintf(fp, "set format y2 \"10^{%%L}\"\n");
  fprintf(fp, "set key left top\n");
  fprintf(fp, "set xrange [1E-3:%lf]\n", pScatGroup[nScatGroup]);
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
  for (i=0;i<nCaptGroup;i++)
    fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[i], pCumCapXS[i]);
  fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[i], pCumCapXS[i-1]);
  fclose(fp);

  // create a data file including the uncertainties of capture cross sections
  if ((fp = fopen("CAPT-UNC.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'CAPT-UNC.dat'\n", stderr);
    exit(1);
  }
  for (i=0;i<nCaptGroup;i++) {
    xer = 100.0*sqrt(pCumCapUN[i]*pCumCapUN[i] + pCumCapXS2[i]);
    fprintf(fp, "%10.4lE %5.2lf %5.2lf %5.2lf\n", pCaptGroup[i], 100.0*pCumCapUN[i], 100.0*sqrt(pCumCapXS2[i]), xer);
  }
  fprintf(fp, "%10.4lE %5.2lf %5.2lf %5.2lf\n", pCaptGroup[i], 100.0*pCumCapUN[i-1], 100.0*sqrt(pCumCapXS2[i-1]), xer);
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
  fprintf(fp, "set xlabel 'Incident neutron energy, eV' font 'Times-Roman,20'\n");
  fprintf(fp, "set ylabel 'Uncertainty, %%' font 'Times-Roman,20'\n");
  fprintf(fp, "set y2label 'Cross section, b' font 'Times-Roman,20'\n");
  fprintf(fp, "set log x\n");
  fprintf(fp, "set log y2\n");
  fprintf(fp, "set format y2 \"10^{%%L}\"\n");
  fprintf(fp, "set key left top\n");
  fprintf(fp, "set xrange [1E-3:%lf]\n", pCaptGroup[nCaptGroup]);
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

void WriteMatrix(double fCorrNNRT, double fCorrGGRT, double fCorrNNB, double fCorrGGB)
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

void WriteEndf(char *szEndfFiles, double fCorrNNRT, double fCorrGGRT, double fCorrNNB, double fCorrGGB)
{
  char s[67];
  CEndf endf;
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  double dE, dGn, dGg, dGf, darea, dfarea;
  int i, j, n;

  bool bFile2 = true;		// flag to create file 33 instead of file 2
  bool bFile3 = true;		// flag to create file 33 instead of file 3
  bool bFile32 = true;		// flag to create file 33 instead of file 32
  bool bFile33 = false;		// flag to create file 33 instead of file 33
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
  char szAtlasDir[PATH_MAX];		// Atlas directory
  char szScatGroup[PATH_MAX];		// file containing energy group information for scattering
  char szCaptGroup[PATH_MAX];		// file containing energy group information for capture
  char szFissGroup[PATH_MAX];		// file containing energy group information for fission
  bool bReassignLJ = false;		// flag to reassign L and J
  bool bPrintAtlas = false;		// flag to print resonance parameters
  bool bPrintPotential = false;		// flag to print potential cross sections
  double fScatteringXS = -1 , fdScatteringXS = -1;	// thermal scattering cross section
  double fCaptureXS = -1, fdCaptureXS = -1;		// thermal capture cross section
  bool bPotential = true;		// flag to take into account potential scattering
  double fGammaFactor = 4;		// factor multiplied to the total width to estimate the resonance area
  double fDefaultScatUnc = .1;		// default scattering width uncertainty
  bool bUseArea = false;		// flag to use kernel from Atlas for capture cross section calculation
  int nIteration = 1000;
  double *pSctXS = NULL;		// average scattering cross sections
  double *pCapXS = NULL;		// average capture cross sections
  double *pFisXS = NULL;		// average fission cross sections
  double *pSctUN = NULL;		// scattering cross section uncertainties
  double *pCapUN = NULL;		// capture cross section uncertainties
  double *pFisUN = NULL;		// fission cross section uncertainties
  double fdGg0 = -1, fdGg1 = -1, fdGg2 = -1;
  double fCorrNGS = 0;			// correlation between scattering and capture widths of single resonance
  double fCorrNN = .5;			// correlation between scattering widths of different resonances
  double fCorrGG = .5;			// correlation between capture widths of different resonances
  double fCorrRP = -.5;			// correlation between resonance and potential scatterings
  double fCorrFF = .5;			// correlation between fission widths of different resonances
  double fCorrNG = 0;			// correlation between scattering and capture widths of different resonances
  double fCorrNNB = .5;		// correlation between scattering cross sections in different energy bins
  double fCorrGGB = .5;		// correlation between capture cross sections in different energy bins
  double fCorrNNRT = 0;		// correlation between scattering cross sections in the thermal and resonance regions
  double fCorrGGRT = 0;		// correlation between capture cross sections in the thermal and resonance regions

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

  kernel.SetR(fR, fdR);
  kernel.SetRange(fMin, fMax);
  kernel.SetGammaFactor(fGammaFactor);
  kernel.SetDefaultScatUnc(fDefaultScatUnc);
  kernel.SetCorr(fCorrNN, fCorrGG, fCorrRP, fCorrNG, fCorrNGS);

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
    pSctXS = new double[nScatGroup];
    pSctUN = new double[nScatGroup];
    pCumSctXS = new double[nScatGroup];
    pCumSctXS2 = new double[nScatGroup];
    pCumSctUN = new double[nScatGroup];
    memset(pCumSctXS, 0, nScatGroup*sizeof(double));
    memset(pCumSctXS2, 0, nCaptGroup*sizeof(double));
    memset(pCumSctUN, 0, nScatGroup*sizeof(double));
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
    pCapXS = new double[nCaptGroup];
    pCapUN = new double[nCaptGroup];
    pCumCapXS = new double[nCaptGroup];
    pCumCapXS2 = new double[nCaptGroup];
    pCumCapUN = new double[nCaptGroup];
    memset(pCumCapXS, 0, nCaptGroup*sizeof(double));
    memset(pCumCapXS2, 0, nCaptGroup*sizeof(double));
    memset(pCumCapUN, 0, nCaptGroup*sizeof(double));
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
    pFisXS = new double[nFissGroup];
    pFisUN = new double[nFissGroup];
    pCumFisXS = new double[nFissGroup];
    pCumFisXS2 = new double[nFissGroup];
    pCumFisUN = new double[nFissGroup];
    memset(pCumFisXS, 0, nFissGroup*sizeof(double));
    memset(pCumFisXS2, 0, nCaptGroup*sizeof(double));
    memset(pCumFisUN, 0, nFissGroup*sizeof(double));
  }

  if (bPrintPotential) {
    puts("#############################################################");
    puts("###############                                ##############");
    puts("###############    POTENTIAL SCATTERING C/S    ##############");
    puts("###############                                ##############");
    puts("#############################################################");
    for (int i=0;i<nScatGroup;i++)
      printf("%3d %10.4lE - %10.4lE: %11.5lE\n", i+1, pScatGroup[i], pScatGroup[i+1],
             kernel.GetPotentialXS(pScatGroup[i], pScatGroup[i+1]));
  }

  for (int i=0;i<nIteration;i++) {
    kernel.AssignJ();
    if (bScattering) {
      kernel.GetXSnUNC(i, SCAT, nScatGroup, pScatGroup, nFirstResScatGroup, pSctXS, pSctUN);
      for (int n=0;n<nScatGroup;n++) {
        pCumSctXS[n] += pSctXS[n];
        pCumSctXS2[n] += pSctXS[n]*pSctXS[n];
        pCumSctUN[n] += pSctUN[n];
      }
    }
    if (bCapture) {
      kernel.GetXSnUNC(i, CAPT, nCaptGroup, pCaptGroup, nFirstResCaptGroup, pCapXS, pCapUN, bUseArea);
      for (int n=0;n<nCaptGroup;n++) {
        pCumCapXS[n] += pCapXS[n];
        pCumCapXS2[n] += pCapXS[n]*pCapXS[n];
        pCumCapUN[n] += pCapUN[n];
      }
    }
    if (bFission) {
      kernel.GetXSnUNC(i, FISS, nFissGroup, pFissGroup, nFirstResFissGroup, pFisXS, pFisUN);
      for (int n=0;n<nFissGroup;n++) {
        pCumFisXS[n] += pFisXS[n];
        pCumFisXS2[n] += pFisXS[n]*pFisXS[n];
        pCumFisUN[n] += pFisUN[n];
      }
    }
  }

  double xs2;
  if (bScattering) {
    for (int n=0;n<nScatGroup;n++) {
      if (n < nFirstResScatGroup) {
      // assign thermal scattering cross section
        pCumSctXS[n] = fScatteringXS;
        pCumSctUN[n] = fdScatteringXS/fScatteringXS;
      } else {
        pCumSctXS[n] /= nIteration;
        xs2 = pCumSctXS[n]*pCumSctXS[n];
        pCumSctXS2[n] = max(pCumSctXS2[n]/nIteration - xs2, 0.0)/xs2;
        pCumSctUN[n] /= nIteration;
        // pCumSctUN[n] = sqrt(pCumSctUN[n]*pCumSctUN[n] + pCumSctXS2[n]);
      }
    }
  }
  if (bCapture) {
    for (int n=0;n<nCaptGroup;n++) {
      if (n < nFirstResCaptGroup) {
      // calculate thermal capture cross section
        pCumCapXS[n] = 2*fCaptureXS*sqrt(0.0253)*(sqrt(pCaptGroup[n+1])-sqrt(pCaptGroup[n]))/(pCaptGroup[n+1]-pCaptGroup[n]);
        pCumCapUN[n] = fdCaptureXS/fCaptureXS;
      } else {
        pCumCapXS[n] /= nIteration;
        xs2 = pCumCapXS[n]*pCumCapXS[n];
        pCumCapXS2[n] = max(pCumCapXS2[n]/nIteration - xs2, 0.0)/xs2;
        pCumCapUN[n] /= nIteration;
        // pCumCapUN[n] = sqrt(pCumCapUN[n]*pCumCapUN[n] + pCumCapXS2[n]);
      }
    }
  }
  if (bFission) {
    for (int n=0;n<nFissGroup;n++) {
      pCumFisXS[n] /= nIteration;
      xs2 = pCumFisXS[n]*pCumFisXS[n];
      pCumFisXS2[n] = max(pCumFisXS2[n]/nIteration - xs2,0.0)/xs2;
      pCumFisUN[n] /= nIteration;
      // pCumFisUN[n] = sqrt(pCumFisUN[n]*pCumFisUN[n] + pCumFisXS2[n]);
    }
  }


  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############             RESULTS            ##############");
  puts("###############                                ##############");
  puts("#############################################################");

// calculate resonance integral and its uncertainty for scattering
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

// calculate resonance integral and its uncertainty for capture
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

// print the results
  if (bScattering) {
    puts("Average scattering cross sections");
    for (int i=0;i<nScatGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
//        i+1, pScatGroup[i], pScatGroup[i+1], pCumSctXS[i], 100.0*pCumSctUN[i]);
        i+1, pScatGroup[i], pScatGroup[i+1], pCumSctXS[i], 100.0*sqrt(pCumSctUN[i]*pCumSctUN[i]+pCumSctXS2[i]));
    }
  }
  if (bCapture) {
    puts("Average capture cross sections");
    for (int i=0;i<nCaptGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pCaptGroup[i], pCaptGroup[i+1], pCumCapXS[i], 100.0*sqrt(pCumCapUN[i]*pCumCapUN[i]+pCumCapXS2[i]));
    }
  }

  if (bFission) {
    puts("Average fission cross sections");
    for (int i=0;i<nFissGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pFissGroup[i], pFissGroup[i+1], pCumFisXS[i], 100.0*sqrt(pCumFisUN[i]*pCumFisUN[i]+pCumFisXS2[i]));
    }
  }

  WriteMatrix(fCorrNNRT, fCorrGGRT, fCorrNNB, fCorrGGB);

  WriteEndf(szEndfFiles, fCorrNNRT, fCorrGGRT, fCorrNNB, fCorrGGB);

  Plot();

  if (pScatGroup) delete[] pScatGroup;
  if (pCaptGroup) delete[] pCaptGroup;
  if (pFissGroup) delete[] pFissGroup;
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
