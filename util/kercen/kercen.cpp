/***********************************************************************
 *
 * Filename: kercen.cpp
 * Purpose : Calculate average cross sections and their uncertainties
 * Authors : Youngsik Cho, Samuel Hoblit
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

#define  VERSION		1.0

typedef struct {
  int    nBin;
  double fUnc;
} BINUNC;

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
static double *pCumCapXS = NULL;
static double *pCumFisXS = NULL;
static double *pCumSctUN = NULL;
static double *pCumCapUN = NULL;
static double *pCumFisUN = NULL;

void Plot();
void WriteMatrix(double , double , double , double);
void WriteEndf(char *, double , double , double , double);

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
  double fScatteringXS = -1;		// thermal scattering cross section
  double fScatteringUN = -1;		// uncertainty of thermal scattering cross section (E < 0.5 eV)
  double fScatteringUN2 = -1;		// uncertainty of thermal scattering cross section (E >= 0.5 eV)
  double fCaptureXS = -1;		// thermal capture cross section
  double fCaptureUN = -1;		// uncertainty of thermal capture cross section (E < 0.5 eV)
  double fCaptureUN2 = -1;		// uncertainty of thermal capture cross section (E >= 0.5 eV)
  double fGammaFactor = 4;		// factor multiplied to the total width to estimate the resonance area
  double fDefaultScatUnc = .1;		// default scattering width uncertainty
  bool bUseArea = false;		// flag to use kernel from Atlas for capture cross section calculation
  bool bBound = false;			// flag to take bound resonances into account
  int nIteration = 1000;
  double *pCumSctXS2 = NULL;		// sums for calc the error due to MC spin assignments
  double *pCumCapXS2 = NULL;
  double *pCumFisXS2 = NULL;
  double *pSctXS = NULL;		// average scattering cross sections
  double *pCapXS = NULL;		// average capture cross sections
  double *pFisXS = NULL;		// average fission cross sections
  double *pSctUN = NULL;		// scattering cross section uncertainties
  double *pCapUN = NULL;		// capture cross section uncertainties
  double *pFisUN = NULL;		// fission cross section uncertainties
  double fGg0 = -1, fGg1 = -1, fGg2 = -1;
  double fdGg0 = -1, fdGg1 = -1, fdGg2 = -1;
  double fCorrNGS = 0;			// correlation between scattering and capture widths of single resonance
  double fCorrNN = .5;			// correlation between scattering widths of different resonances
  double fCorrGG = .5;			// correlation between capture widths of different resonances
  double fCorrRP = -.5;			// correlation between resonance and potential scatterings
  double fCorrFF = .5;			// correlation between fission widths of different resonances
  double fCorrNG = 0;			// correlation between scattering and capture widths of different resonances
  double fCorrNNB = .5;			// correlation between scattering cross sections in different energy bins
  double fCorrGGB = .5;			// correlation between capture cross sections in different energy bins
  double fCorrNNRT = 0;			// correlation between scattering cross sections in the thermal and resonance regions
  double fCorrGGRT = 0;			// correlation between capture cross sections in the thermal and resonance regions
  double fCaptlim = 0.0;		// lower limit on capture % uncertainties
  double fScatlim = 0.0;		// lower limit on scattering % uncertainties
  double fFisslim = 0.0;		// lower limit on fission % uncertainties
  int    nPreDefSctUN = 0;		// number of predefined scattering uncertainties
  int    nPreDefCapUN = 0;		// number of predefined capture uncertainties
  BINUNC *pPreDefSctUN = NULL;		// predefined scattering uncertainties
  BINUNC *pPreDefCapUN = NULL;		// predefined capture uncertainties
  double f1, f2;

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
    else if (!strcasecmp(key, "dsxs")) fScatteringUN = atof(value);
    else if (!strcasecmp(key, "dsxs2")) fScatteringUN2 = atof(value);
    else if (!strcasecmp(key, "cxs")) fCaptureXS = atof(value);
    else if (!strcasecmp(key, "dcxs")) fCaptureUN = atof(value);
    else if (!strcasecmp(key, "dcxs2")) fCaptureUN2 = atof(value);
    else if (!strcasecmp(key, "r")) fR = atof(value);
    else if (!strcasecmp(key, "dr")) fdR = atof(value);
    else if (!strcasecmp(key, "atlasdir")) strcpy(szAtlasDir, value);
    else if (!strcasecmp(key, "defaultscatunc")) fDefaultScatUnc = atof(value);
    else if (!strcasecmp(key, "gg0")) fGg0 = atof(value);
    else if (!strcasecmp(key, "gg1")) fGg1 = atof(value);
    else if (!strcasecmp(key, "gg2")) fGg2 = atof(value);
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
    else if (!strcasecmp(key, "bound")) bBound = atoi(value);
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
    else if (!strcasecmp(key, "captlim")) fCaptlim = atof(value)/100.;
    else if (!strcasecmp(key, "scatlim")) fScatlim = atof(value)/100.;
    else if (!strcasecmp(key, "fisslim")) fFisslim = atof(value)/100.;
    else if (!strncasecmp(key, "scatunc", 7)) {
      pPreDefSctUN = (BINUNC*)realloc(pPreDefSctUN, (nPreDefSctUN+1)*sizeof(BINUNC));
      pPreDefSctUN[nPreDefSctUN].nBin = atoi(&key[7])-1;
      pPreDefSctUN[nPreDefSctUN].fUnc = atof(value)/100;
      ++nPreDefSctUN;
    } else if (!strncasecmp(key, "captunc", 7)) {
      pPreDefCapUN = (BINUNC*)realloc(pPreDefCapUN, (nPreDefCapUN+1)*sizeof(BINUNC));
      pPreDefCapUN[nPreDefCapUN].nBin = atoi(&key[7])-1;
      pPreDefCapUN[nPreDefCapUN].fUnc = atof(value)/100;
      ++nPreDefCapUN;
    }
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
  printf("##                      KERCEN v%4.2lf                       ##\n", VERSION);
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
  if (fGg0 != -1) kernel.SetGg0(fGg0);
  if (fGg1 != -1) kernel.SetGg1(fGg1);
  if (fGg2 != -1) kernel.SetGg2(fGg2);
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
      printf("E=%10.2lf, J=%3.1lf, g=%4.2lf, gGn=%9.3lE+-%9.3lE, Gg=%9.3lE+-%9.3lE, Gf=%9.3lE+-%9.3lE, A=%9.3lE+-%9.3lE, Af=%9.3lE+-%9.3lE\n",
              E, J, (2*J+1)/(2*(2*kernel.GetSpin()+1)),gGn,(2*J+1)/(2*(2*kernel.GetSpin()+1))*dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
    }
  }

  kernel.GetScatteringXS(f1, f2);
  if (fScatteringXS == -1) fScatteringXS = f1;
  if (fScatteringUN == -1) fScatteringUN = f2;

  kernel.GetCaptureXS(f1, f2);
  if (fCaptureXS == -1) fCaptureXS = f1;
  if (fCaptureUN == -1) fCaptureUN = f2;

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
  if (fScatteringXS <= 0) {
    fputs("ERROR: Thermal scattering xs was not assigned\n", stderr);
    exit(1);
  }
  if (fScatteringUN <= 0) {
    fputs("ERROR: Thermal scattering xs uncertainty was not assigned\n", stderr);
    exit(1);
  }
  if (fCaptureXS <= 0) {
    fputs("ERROR: Thermal capture xs was not assigned\n", stderr);
    exit(1);
  }
  if (fCaptureUN <= 0) {
    fputs("ERROR: Thermal capture xs uncertainty was not assigned\n", stderr);
    exit(1);
  }
  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############    GLOBAL INPUT PARAMETERS     ##############");
  puts("###############                                ##############");
  puts("#############################################################");
  printf("R = %lf +- %lf\n", fR, fdR);
  printf("Thermal scattering xs = %6.3lf += %lf\n", fScatteringXS, fScatteringUN);
  printf("Thermal capture xs    = %6.3lf += %lf\n", fCaptureXS, fCaptureUN);
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

  fScatteringUN /= fScatteringXS;
  if (fScatteringUN2 == -1) fScatteringUN2 = 1.5*fScatteringUN;
  else fScatteringUN2 /= fScatteringXS;

  fCaptureUN /= fCaptureXS;
  if (fCaptureUN2 == -1) {
    kernel.GetIg(f1, f2);
    fCaptureUN2 = f2/f1;
  } else fCaptureUN2 /= fCaptureXS;

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
    memset(pCumSctXS,  0, nScatGroup*sizeof(double));
    memset(pCumSctXS2, 0, nScatGroup*sizeof(double));
    memset(pCumSctUN,  0, nScatGroup*sizeof(double));
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
    memset(pCumCapXS,  0, nCaptGroup*sizeof(double));
    memset(pCumCapXS2, 0, nCaptGroup*sizeof(double));
    memset(pCumCapUN,  0, nCaptGroup*sizeof(double));
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
    memset(pCumFisXS,  0, nFissGroup*sizeof(double));
    memset(pCumFisXS2, 0, nFissGroup*sizeof(double));
    memset(pCumFisUN,  0, nFissGroup*sizeof(double));
  }

  if (bPrintPotential) {
    puts("#############################################################");
    puts("###############                                ##############");
    puts("###############    POTENTIAL SCATTERING C/S    ##############");
    puts("###############                                ##############");
    puts("#############################################################");
    for (int i=0;i<nScatGroup;i++) {
      double e = (pScatGroup[i] + pScatGroup[i+1])/2.0;
      kernel.GetPotentialXS(e, f1, f2);
      printf("%3d %10.4lE - %10.4lE:  %11.5lE +/- %11.5lE\n", i+1, pScatGroup[i], pScatGroup[i+1], f1, f2);
    }
  }

  for (int i=0;i<nIteration;i++) {

    kernel.AssignJ();

    if (bScattering) {
      kernel.GetXSnUNC(i, SCAT, nScatGroup, pScatGroup, nFirstResScatGroup, pSctXS, pSctUN, bBound?BOUND:0);
      for (int n=0;n<nScatGroup;n++) {
        pCumSctXS[n]  += pSctXS[n];
        pCumSctXS2[n] += pSctXS[n]*pSctXS[n];
        pCumSctUN[n]  += pSctUN[n];
      }
    }

    if (bCapture) {
      kernel.GetXSnUNC(i, CAPT, nCaptGroup, pCaptGroup, nFirstResCaptGroup, pCapXS, pCapUN, bUseArea?USEAREA:0);
      for (int n=0;n<nCaptGroup;n++) {
        pCumCapXS[n]  += pCapXS[n];
        pCumCapXS2[n] += pCapXS[n]*pCapXS[n];
        pCumCapUN[n]  += pCapUN[n];
      }
    }

    if (bFission) {
      kernel.GetXSnUNC(i, FISS, nFissGroup, pFissGroup, nFirstResFissGroup, pFisXS, pFisUN, bUseArea?USEAREA:0);
      for (int n=0;n<nFissGroup;n++) {
        pCumFisXS[n]  += pFisXS[n];
        pCumFisXS2[n] += pFisXS[n]*pFisXS[n];
        pCumFisUN[n]  += pFisUN[n];
      }
    }

  }

  double xs2;

  if (bScattering) {
    for (int n=0;n<nScatGroup;n++) {
      if (n < nFirstResScatGroup) {
      // assign thermal scattering cross section
        if (pScatGroup[n] < 0.5) {
          pCumSctXS[n] = fScatteringXS;
          pCumSctUN[n] = fScatteringUN;
        } else {
          pCumSctXS[n] = fScatteringXS;
          pCumSctUN[n] = fScatteringUN2;
        }
      } else {
        pCumSctXS[n] /= nIteration;
        pCumSctUN[n] /= nIteration;
        xs2 = pCumSctXS[n]*pCumSctXS[n];
        if (xs2 == 0.0) pCumSctUN[n] = 0.0;
        else pCumSctUN[n] = sqrt(pCumSctUN[n]*pCumSctUN[n] + max(pCumSctXS2[n]/nIteration - xs2, 0.0)/xs2);
        pCumSctUN[n] = max(pCumSctUN[n],fScatlim);
      }
    }
    for (int n=0;n<nPreDefSctUN;n++)
      if (pPreDefSctUN[n].nBin >= 0 && pPreDefSctUN[n].nBin < nScatGroup)
        pCumSctUN[pPreDefSctUN[n].nBin] = pPreDefSctUN[n].fUnc;
  }

  if (bCapture) {
    for (int n=0;n<nCaptGroup;n++) {
      if (n < nFirstResCaptGroup) {
      // calculate thermal capture cross section
        pCumCapXS[n] = 2.0*fCaptureXS*sqrt(0.0253)*(sqrt(pCaptGroup[n+1])-sqrt(pCaptGroup[n]))/(pCaptGroup[n+1]-pCaptGroup[n]);
        if (pCaptGroup[n] < 0.5) {
          pCumCapUN[n] = fCaptureUN;
        } else {
          pCumCapUN[n] = fCaptureUN2;
        }
      } else {
        pCumCapXS[n] /= nIteration;
        pCumCapUN[n] /= nIteration;
        xs2 = pCumCapXS[n]*pCumCapXS[n];
        if (xs2 == 0.0) pCumCapUN[n] = 0.0;
        else pCumCapUN[n] = sqrt(pCumCapUN[n]*pCumCapUN[n] + max(pCumCapXS2[n]/nIteration - xs2, 0.0)/xs2);
        pCumCapUN[n] = max(pCumCapUN[n],fCaptlim);
      }
    }
    for (int n=0;n<nPreDefCapUN;n++)
      if (pPreDefCapUN[n].nBin >= 0 && pPreDefCapUN[n].nBin < nCaptGroup)
        pCumCapUN[pPreDefCapUN[n].nBin] = pPreDefCapUN[n].fUnc;
  }

  if (bFission) {
    for (int n=0;n<nFissGroup;n++) {
      pCumFisXS[n] /= nIteration;
      pCumFisUN[n] /= nIteration;
      xs2 = pCumFisXS[n]*pCumFisXS[n];
      if (xs2 == 0.0) pCumFisUN[n] = 0.0;
      else pCumFisUN[n] = sqrt(pCumFisUN[n]*pCumFisUN[n] + max(pCumFisXS2[n]/nIteration - xs2,0.0)/xs2);
      pCumFisUN[n] = max(pCumFisUN[n],fFisslim);
    }
  }


  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############             RESULTS            ##############");
  puts("###############                                ##############");
  puts("#############################################################");

// calculate resonance integral and its uncertainty for scattering

  if (bScattering) {

    double sri=0.0, dsri=0.0;
    double uri1, uri2;
    double e1, e2, de, xx, corx;

    for (int i=0;i<nScatGroup;i++) {

      if (pScatGroup[i+1] <= 0.5) continue;

      e1 = max(pScatGroup[i],0.5);
      e2 = pScatGroup[i+1];
      de = pScatGroup[i+1] - pScatGroup[i];
      xx = pCumSctXS[i]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;
      sri += xx;
      uri1 = pCumSctUN[i]*xx;
      // printf("%10.4lE - %10.4lE: ri for scat = %8.4lf\n",e1,e2,xx);

      for (int j=0;j<nScatGroup;j++) {

        if (pScatGroup[j+1] <= 0.5) continue;

        e1 = max(pScatGroup[j],0.5);
        e2 = min(pScatGroup[j+1],1.0E+05);
        de = pScatGroup[j+1] - pScatGroup[j];
        uri2 = pCumSctUN[j]*pCumSctXS[j]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;

        if (i == j) corx = 1.0;
        else {
          if (i < nFirstResScatGroup) {
            if (j < nFirstResScatGroup) corx = 1.0;
            else corx = fCorrNNRT;
          } else {
            if (j < nFirstResScatGroup) corx = fCorrNNRT;
            else corx = fCorrNNB;
          }
        }

        dsri += corx*uri1*uri2;

      }
    }

    dsri = sqrt(dsri);
    printf("Resonance integral for scattering     = %8.2lE +- %8.2lE (%6.2lf %%)\n", sri, dsri, dsri/sri*100);

  }

// calculate resonance integral and its uncertainty for capture

  if (bCapture) {

    double cri=0.0, dcri=0.0;
    double cmx=0.0, dcmx=0.0;
    double uri1, uri2, umx1, umx2;
    double ev, de, e1, e2, xx, corx;
    double a = fAwr/(1+fAwr);
    double b = 2.0/sqrt(PI)*a*a/9.0E+08;

    for (int i=0;i<nCaptGroup;i++) {

      ev = (pCaptGroup[i] + pCaptGroup[i+1])/2.0;
      ev = ev*exp(-a*ev/3.0E+04);
      de = pCaptGroup[i+1] - pCaptGroup[i];
      xx = b*pCumCapXS[i]*ev*de;
      cmx += xx;
      umx1 = pCumCapUN[i]*xx;
      // printf("group %d: cmx=%lf\n", i+1, xx);

      // only calculate the regular resonance integral from 0.5 - 100,000 eV.

      if (pCaptGroup[i+1] <= 0.5)   continue;
      if (pCaptGroup[i] >= 1.0E+05) continue;

      e1 = max(pCaptGroup[i],0.5);
      e2 = min(pCaptGroup[i+1],1.0E+05);
      xx = pCumCapXS[i]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;
      // xx = pCumCapXS[i]*log(e2/e1)*(e2-e1)/de;
      cri += xx;
      uri1 = pCumCapUN[i]*xx;

      for (int j=0;j<nCaptGroup;j++) {

        ev = (pCaptGroup[j] + pCaptGroup[j+1])/2.0;
        ev = ev*exp(-a*ev/3.0E+04);
        de = pCaptGroup[j+1] - pCaptGroup[j];
        umx2 = b*pCumCapUN[j]*pCumCapXS[j]*ev*de;

        if (pCaptGroup[j+1] <= 0.5)   continue;
        if (pCaptGroup[j] >= 1.0E+05) continue;

        e1 = max(pCaptGroup[j],0.5);
        e2 = min(pCaptGroup[j+1],1.0E+05);
        uri2 = pCumCapUN[j]*pCumCapXS[j]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;
        // uri2 = pCumCapUN[j]*pCumCapXS[j]*log(e2/e1)*(e2-e1)/de;

        if (i == j) corx = 1.0;
        else {
          if (i < nFirstResCaptGroup) {
            if (j < nFirstResCaptGroup) corx = 1.0;
            else corx = fCorrGGRT;
          } else {
            if (j < nFirstResCaptGroup) corx = fCorrGGRT;
            else corx = fCorrGGB;
          }
        }

        dcmx += corx*umx1*umx2;
        dcri += corx*uri1*uri2;

      }
    }

    dcri = sqrt(dcri);
    dcmx = sqrt(dcmx);
    printf("Resonance integral for capture        = %8.2lE +- %8.2lE (%6.2lf %%)\n", cri, dcri, dcri/cri*100);
    printf("30-keV Maxwellian average for capture = %8.2lE +- %8.2lE (%6.2lf %%)\n", cmx, dcmx, dcmx/cmx*100);

  }

//  printf("first res. group = %d, %d\n", nFirstResScatGroup, nFirstResCaptGroup);

// print the results

  if (bScattering) {
    puts("Average scattering cross sections");
    for (int i=0;i<nScatGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pScatGroup[i], pScatGroup[i+1], pCumSctXS[i], 100.0*pCumSctUN[i]);
    }
  }

  if (bCapture) {
    puts("Average capture cross sections");
    for (int i=0;i<nCaptGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pCaptGroup[i], pCaptGroup[i+1], pCumCapXS[i], 100.0*pCumCapUN[i]);
    }
  }

  if (bFission) {
    puts("Average fission cross sections");
    for (int i=0;i<nFissGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pFissGroup[i], pFissGroup[i+1], pCumFisXS[i], 100.0*pCumFisUN[i]);
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
  if (pCumSctXS2) delete[] pCumSctXS2;
  if (pCumCapXS) delete[] pCumCapXS;
  if (pCumCapXS2) delete[] pCumCapXS2;
  if (pCumFisXS) delete[] pCumFisXS;
  if (pCumFisXS2) delete[] pCumFisXS2;
  if (pCumSctUN) delete[] pCumSctUN;
  if (pCumCapUN) delete[] pCumCapUN;
  if (pCumFisUN) delete[] pCumFisUN;
  if (pPreDefSctUN) free(pPreDefSctUN);
  if (pPreDefCapUN) free(pPreDefCapUN);
}

void Plot()
{
  char tmp[20];
  FILE *fp;
  int i;

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
    fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[i], 100.0*pCumSctUN[i]);
  }
  fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[i], 100.0*pCumSctUN[i-1]);
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
    fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[i], 100.0*pCumCapUN[i]);
  }
  fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[i], 100.0*pCumCapUN[i-1]);
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

