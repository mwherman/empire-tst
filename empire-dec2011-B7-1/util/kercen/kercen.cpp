/***********************************************************************
 *
 * Filename: kercen.cpp
 * Purpose : Calculate average cross sections and their uncertainties
 *
 * Written by Youngsik Cho (Inspired by Pavel Oblozinsky)
 * Modified by Samuel Hoblit
 *
 ***********************************************************************/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <math.h>
#include "resxs.h"
#include "util.h"
#include "endf.h"

using namespace std;

#define  VERSION		1.0

typedef struct {
  int    nBin;
  double fUnc;
} BINUNC;

static CResXS resxs;
static bool bScattering = false;	// flag to calculate scattering cross sections and their uncertainties
static bool bCapture = false;		// flag to calculate capture cross sections and their uncertainties
static bool bFission = false;		// flag to calculate fission cross sections and their uncertainties
static bool bUseSubGroup = false;	// flag to consider a subgroup structure in the calculation of uncertainty
static int nZ=-1, nA=-1, nMat=0;	// Z, A, MAt number
static double fAwr = 0;			// AWR
static double fMin = .0, fMax = 1e38;	// lower and upper energy of resonance region
static double fR = -1, fdR = -1;	// scattering radius and its uncertainty
static int nScatGroup = 0;		// number of energy groups for scattering
static int nCaptGroup = 0;		// number of energy groups for capture
static int nFissGroup = 0;		// number of energy groups for fission
static int nScatSubGroup = 0;		// number of energy groups for scattering
static int nCaptSubGroup = 0;		// number of energy groups for capture
static int nFissSubGroup = 0;		// number of energy groups for fission
static int nFirstResScatGroup = -1;	// index of first energy group where resonance starts for scattering
static int nFirstResCaptGroup = -1;	// index of first energy group where resonance starts for capture
static int nFirstResFissGroup = 0;	// index of first energy group where resonance starts for fission
static double *pScatGroup = NULL;	// scattering group energy boundariies
static double *pCaptGroup = NULL;	// capture group energy boundariies
static double *pFissGroup = NULL;	// fission group energy boundariies
static double *pScatSubGroup = NULL;	// scattering subgroup energy boundariies
static double *pCaptSubGroup = NULL;	// capture subgroup energy boundariies
static double *pFissSubGroup = NULL;	// fission subgroup energy boundariies
static double *pScatXS = NULL;		// average scattering cross sections
static double *pCaptXS = NULL;		// average capture cross sections
static double *pFissXS = NULL;		// average fission cross sections
static double *pScatUN = NULL;		// scattering cross section uncertainties
static double *pCaptUN = NULL;		// capture cross section uncertainties
static double *pFissUN = NULL;		// fission cross section uncertainties
static int nIteration = 1;
static double fCaptlim = 0.0;		// lower limit on capture % uncertainties
static double fScatlim = 0.0;		// lower limit on scattering % uncertainties
static double fFisslim = 0.0;		// lower limit on fission % uncertainties
static double fScatteringXS = -1;	// thermal scattering cross section
static double fScatteringUN = -1;	// uncertainty of thermal scattering cross section (E < 0.5 eV)
static double fScatteringUN2 = -1;	// uncertainty of thermal scattering cross section (E >= 0.5 eV)
static double fCaptureXS = -1;		// thermal capture cross section
static double fCaptureUN = -1;		// uncertainty of thermal capture cross section (E < 0.5 eV)
static double fCaptureUN2 = -1;		// uncertainty of thermal capture cross section (E >= 0.5 eV)
static int    nPreDefScatUN = 0;	// number of predefined scattering uncertainties
static int    nPreDefCaptUN = 0;	// number of predefined capture uncertainties
static int    nPreDefFissUN = 0;	// number of predefined fission uncertainties
static BINUNC *pPreDefScatUN = NULL;	// predefined scattering uncertainties
static BINUNC *pPreDefCaptUN = NULL;	// predefined capture uncertainties
static BINUNC *pPreDefFissUN = NULL;	// predefined fission uncertainties

void PrintAtlas(double fDefaultScatUnc);
void CalcXSnUN(int nFlag);
void PrintPotential();
void CalcIntegral(double fCorrNNRT, double fCorrGGRT, double fCorrNNB, double fCorrGGB);
void Plot();
void WriteMatrix(double fCorrNNRT, double fCorrGGRT, double fCorrNNB, double fCorrGGB);
void WriteEndf(char *szEndfFiles, double fCorrNNRT, double fCorrGGRT, double fCorrNNB, double fCorrGGB);

int main(int argc, char **argv)
{
  FILE *fp;
  char s[1024];
  char szEndfFiles[1024] = "";
  char szAtlasDir[PATH_MAX];		// Atlas directory
  char szScatGroup[PATH_MAX];		// file containing energy group information for scattering
  char szCaptGroup[PATH_MAX];		// file containing energy group information for capture
  char szFissGroup[PATH_MAX];		// file containing energy group information for fission
  char szScatSubGroup[PATH_MAX];	// file containing energy group information for scattering
  char szCaptSubGroup[PATH_MAX];	// file containing energy group information for capture
  char szFissSubGroup[PATH_MAX];	// file containing energy group information for fission
  bool bReassignLJ = false;		// flag to reassign L and J
  bool bPrintAtlas = false;		// flag to print resonance parameters
  bool bPrintPotential = false;		// flag to print potential cross sections
  double fGammaFactor = 4;		// factor multiplied to the total width to estimate the resonance area
  double fDefaultScatUnc = .1;		// default scattering width uncertainty
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
  bool bUseArea = false;		// flag to use kernel from Atlas for capture cross section calculation
  bool bBound = false;			// flag to take bound resonances into account
  double f1, f2;

  if (argc < 2) {
    fputs("usage: kercen inputfile\n", stderr);
    exit(1);
  }
  if ((fp = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "cannot open '%s'\n", argv[1]);
    exit(1);
  }

  // read an input file

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
    else if (!strcasecmp(key, "scatsubgroup")) strcpy(szScatSubGroup, value);
    else if (!strcasecmp(key, "captsubgroup")) strcpy(szCaptSubGroup, value);
    else if (!strcasecmp(key, "fisssubgroup")) strcpy(szFissSubGroup, value);
    else if (!strcasecmp(key, "emin")) fMin = atof(value);
    else if (!strcasecmp(key, "emax")) fMax = atof(value);
    else if (!strcasecmp(key, "gammafactor")) fGammaFactor = atof(value);
    else if (!strcasecmp(key, "reassignlj")) bReassignLJ = atoi(value);
    else if (!strcasecmp(key, "usearea")) bUseArea = atoi(value);
    else if (!strcasecmp(key, "usesubgroup")) bUseSubGroup = atoi(value);
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
      pPreDefScatUN = (BINUNC*)realloc(pPreDefScatUN, (nPreDefScatUN+1)*sizeof(BINUNC));
      pPreDefScatUN[nPreDefScatUN].nBin = atoi(&key[7])-1;
      pPreDefScatUN[nPreDefScatUN].fUnc = atof(value)/100;
      ++nPreDefScatUN;
    } else if (!strncasecmp(key, "captunc", 7)) {
      pPreDefCaptUN = (BINUNC*)realloc(pPreDefCaptUN, (nPreDefCaptUN+1)*sizeof(BINUNC));
      pPreDefCaptUN[nPreDefCaptUN].nBin = atoi(&key[7])-1;
      pPreDefCaptUN[nPreDefCaptUN].fUnc = atof(value)/100;
      ++nPreDefCaptUN;
    }
  }
  fclose(fp);

  // check if the essential keywords are supplied

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

  // check what reactions shoule be calculated

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

  resxs.SetBaseDirectory(szAtlasDir);
  if (!resxs.ReadProperties(nZ, nA)) {
    fprintf(stderr, "ERROR: Error occured reading Atlas properties from the directory %s\n", szAtlasDir);
    exit(1);
  }

  if (bReassignLJ) resxs.ReassignLJ();
  if (fGg0 != -1) resxs.SetGg0(fGg0);
  if (fGg1 != -1) resxs.SetGg1(fGg1);
  if (fGg2 != -1) resxs.SetGg2(fGg2);
  if (fdGg0 != -1) resxs.SetdGg0(fdGg0);
  if (fdGg1 != -1) resxs.SetdGg1(fdGg1);
  if (fdGg2 != -1) resxs.SetdGg2(fdGg2);
  if (fAwr != -1) resxs.SetAwr(fAwr);
  else {
    fprintf(stderr, "ERROR: Atomic weight should be provided\n");
    exit(1);
  }

  if (!resxs.ReadParameters(nZ, nA)) {
    fprintf(stderr, "ERROR: Error occured reading Atlas paraemters from the directory %s\n", szAtlasDir);
    exit(1);
  }

  resxs.GetScatteringXS(f1, f2);
  if (fScatteringXS == -1) fScatteringXS = f1;
  if (fScatteringUN == -1) fScatteringUN = f2;

  resxs.GetCaptureXS(f1, f2);
  if (fCaptureXS == -1) fCaptureXS = f1;
  if (fCaptureUN == -1) fCaptureUN = f2;

  if (fR == -1) fR = resxs.GetR();
  if (fdR == -1) fdR = resxs.GetdR();

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

  // allocate the required memory

  if (bScattering) {
    if ((fp = fopen(szScatGroup, "r")) == NULL) {
      fprintf(stderr, "ERROR: cannot open a group file '%s'\n", szScatGroup);
      exit(1);
    }
    fgets(s, 1024, fp);
    sscanf(s, "%d", &nScatGroup);
    pScatGroup = new double[nScatGroup+1];
    for (int i=0;i<nScatGroup+1;i++) fscanf(fp, "%lf", pScatGroup+i);
    fclose(fp);
    nFirstResScatGroup = resxs.GetFirstResGroup(nScatGroup, pScatGroup);

    if (bUseSubGroup) {
      double pw = log10(pScatGroup[nScatGroup]-pScatGroup[nFirstResScatGroup])/100;
      CLink link;
      for (int i=0;i<=nScatGroup;i++) link.Add(pScatGroup[i]);
      for (int i=1;i<100;i++) link.Add(pScatGroup[nFirstResScatGroup]+pow(10.0, i*pw));
      double *p = link.GetArray(nScatSubGroup);
      --nScatSubGroup;
      pScatSubGroup = new double[nScatSubGroup+1];
      for (int i=0;i<=nScatSubGroup;i++) pScatSubGroup[i] = p[i];

      pScatXS = new double[nScatSubGroup];
    } else pScatXS = new double[nScatGroup];

    pScatUN = new double[nScatGroup];

    for (int i=0;i<nPreDefScatUN;i++)
      if (pPreDefScatUN[i].nBin+1 < 1 || pPreDefScatUN[i].nBin+1 > nScatGroup) {
        fprintf(stderr, "%d in scatunc%d exceeds number of bins\n", pPreDefScatUN[i].nBin+1, pPreDefScatUN[i].nBin+1);
        exit(1);
      }
  }

  if (bCapture) {
    if ((fp = fopen(szCaptGroup, "r")) == NULL) {
      fprintf(stderr, "ERROR: cannot open a group file '%s'\n", szCaptGroup);
      exit(1);
    }
    fgets(s, 1024, fp);
    sscanf(s, "%d", &nCaptGroup);
    pCaptGroup = new double[nCaptGroup+1];
    for (int i=0;i<nCaptGroup+1;i++) fscanf(fp, "%lf", pCaptGroup+i);
    fclose(fp);
    nFirstResCaptGroup = resxs.GetFirstResGroup(nCaptGroup, pCaptGroup);

    if (bUseSubGroup) {
      double pw = log10(pCaptGroup[nCaptGroup]-pCaptGroup[nFirstResCaptGroup])/100;
      CLink link;
      for (int i=0;i<=nCaptGroup;i++) link.Add(pCaptGroup[i]);
      for (int i=1;i<100;i++) link.Add(pCaptGroup[nFirstResCaptGroup]+pow(10.0, i*pw));
      double *p = link.GetArray(nCaptSubGroup);
      --nCaptSubGroup;
      pCaptSubGroup = new double[nCaptSubGroup+1];
      for (int i=0;i<=nCaptSubGroup;i++) pCaptSubGroup[i] = p[i];

      pCaptXS = new double[nCaptSubGroup];
    } else pCaptXS = new double[nCaptGroup];

    pCaptUN = new double[nCaptGroup];

    for (int i=0;i<nPreDefCaptUN;i++)
      if (pPreDefCaptUN[i].nBin+1 < 1 || pPreDefCaptUN[i].nBin+1 > nCaptGroup) {
        fprintf(stderr, "%d in captunc%d exceeds number of bins\n", pPreDefCaptUN[i].nBin+1, pPreDefCaptUN[i].nBin+1);
        exit(1);
      }
  }

  if (bFission) {
    if ((fp = fopen(szFissGroup, "r")) == NULL) {
      fprintf(stderr, "ERROR: cannot open a group file '%s'\n", szFissGroup);
      exit(1);
    }
    fgets(s, 1024, fp);
    sscanf(s, "%d", &nFissGroup);
    pFissGroup = new double[nFissGroup+1];
    for (int i=0;i<nFissGroup+1;i++) fscanf(fp, "%lf", pFissGroup+i);
    fclose(fp);
    nFirstResFissGroup = resxs.GetFirstResGroup(nFissGroup, pFissGroup);

    if (bUseSubGroup) {
      double pw = log10(pFissGroup[nFissGroup]-pFissGroup[nFirstResFissGroup])/100;
      CLink link;
      for (int i=0;i<=nFissGroup;i++) link.Add(pFissGroup[i]);
      for (int i=1;i<100;i++) link.Add(pFissGroup[nFirstResFissGroup]+pow(10.0, i*pw));
      double *p = link.GetArray(nFissSubGroup);
      --nFissSubGroup;
      pFissSubGroup = new double[nFissSubGroup+1];
      for (int i=0;i<=nFissSubGroup;i++) pFissSubGroup[i] = p[i];

      pFissXS = new double[nFissSubGroup];
    } else pFissXS = new double[nFissGroup];

    pFissUN = new double[nFissGroup];

    for (int i=0;i<nPreDefFissUN;i++)
      if (pPreDefFissUN[i].nBin+1 < 1 || pPreDefFissUN[i].nBin+1 > nFissGroup) {
        fprintf(stderr, "%d in fissunc%d exceeds number of bins\n", pPreDefFissUN[i].nBin+1, pPreDefFissUN[i].nBin+1);
        exit(1);
      }
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

  resxs.SetR(fR, fdR);
  resxs.SetRange(fMin, fMax);
  resxs.SetGammaFactor(fGammaFactor);
  resxs.SetDefaultScatUnc(fDefaultScatUnc);
  resxs.SetCorr(fCorrNN, fCorrGG, fCorrRP, fCorrNG, fCorrNGS);

  fScatteringUN /= fScatteringXS;
  if (fScatteringUN2 == -1) fScatteringUN2 = 1.5*fScatteringUN;
  else fScatteringUN2 /= fScatteringXS;

  fCaptureUN /= fCaptureXS;
  if (fCaptureUN2 == -1) {
    resxs.GetIg(f1, f2);
    fCaptureUN2 = f2/f1;
  } else fCaptureUN2 /= fCaptureXS;

  if (bPrintAtlas) PrintAtlas(fDefaultScatUnc);

  if (bPrintPotential) {
    puts("#############################################################");
    puts("###############                                ##############");
    puts("###############    POTENTIAL SCATTERING C/S    ##############");
    puts("###############                                ##############");
    puts("#############################################################");

    PrintPotential();
  }

  CalcXSnUN( (bUseArea?USEAREA:0) | (bBound?BOUND:0) );

  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############             RESULTS            ##############");
  puts("###############                                ##############");
  puts("#############################################################");

  CalcIntegral(fCorrNNRT, fCorrGGRT, fCorrNNB, fCorrGGB);

// print the results

  if (bScattering) {
    puts("Average scattering cross sections");
    for (int i=0;i<nScatGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pScatGroup[i], pScatGroup[i+1], pScatXS[i], 100.0*pScatUN[i]);
    }
  }

  if (bCapture) {
    puts("Average capture cross sections");
    for (int i=0;i<nCaptGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pCaptGroup[i], pCaptGroup[i+1], pCaptXS[i], 100.0*pCaptUN[i]);
    }
  }

  if (bFission) {
    puts("Average fission cross sections");
    for (int i=0;i<nFissGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pFissGroup[i], pFissGroup[i+1], pFissXS[i], 100.0*pFissUN[i]);
    }
  }

  WriteMatrix(fCorrNNRT, fCorrGGRT, fCorrNNB, fCorrGGB);
  WriteEndf(szEndfFiles, fCorrNNRT, fCorrGGRT, fCorrNNB, fCorrGGB);
  Plot();

  if (pScatGroup) delete[] pScatGroup;
  if (pCaptGroup) delete[] pCaptGroup;
  if (pFissGroup) delete[] pFissGroup;
  if (pScatXS) delete[] pScatXS;
  if (pCaptXS) delete[] pCaptXS;
  if (pFissXS) delete[] pFissXS;
  if (pScatUN) delete[] pScatUN;
  if (pCaptUN) delete[] pCaptUN;
  if (pFissUN) delete[] pFissUN;
  if (pPreDefScatUN) free(pPreDefScatUN);
  if (pPreDefCaptUN) free(pPreDefCaptUN);
}

void PrintAtlas(double fDefaultScatUnc)
{
  resxs.AssignJ();
  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  int l;
  for (int i=0;i<resxs.NoRes();i++) {
    resxs.GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
    l = resxs.GetL(i);
    if (dGn == 0) dGn = fDefaultScatUnc * Gn;
    printf("E=%10.2lf, l=%d, J=%3.1lf, g=%4.2lf, gGn=%9.3lE+-%9.3lE, Gg=%9.3lE+-%9.3lE, Gf=%9.3lE+-%9.3lE, A=%9.3lE+-%9.3lE, Af=%9.3lE+-%9.3lE\n",
            E, l, J, (2*J+1)/(2*(2*resxs.GetSpin()+1)),gGn,(2*J+1)/(2*(2*resxs.GetSpin()+1))*dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
  }
}

// calculate average cross sections and their uncertainties at the resonance energy region
void CalcXSnUN(int nFlag)
{
  double *pCumScatXS = NULL;
  double *pCumCaptXS = NULL;
  double *pCumFissXS = NULL;
  double *pCumScatUN = NULL;
  double *pCumCaptUN = NULL;
  double *pCumFissUN = NULL;
  double *pCumScatXS2 = NULL;		// sums for calc the error due to MC spin assignments
  double *pCumCaptXS2 = NULL;
  double *pCumFissXS2 = NULL;
  double *pScatGroupXS = NULL;
  double *pCaptGroupXS = NULL;
  double *pFissGroupXS = NULL;
  int    *pScatIndex = NULL;
  int    *pCaptIndex = NULL;
  int    *pFissIndex = NULL;
  int     nScatGroupXS;
  int     nCaptGroupXS;
  int     nFissGroupXS;

  resxs.SetupEnergyGrid();

  // allocate memory for iterative calculations

  if (bScattering) {
    nScatGroupXS = bUseSubGroup?nScatSubGroup:nScatGroup;
    pScatGroupXS = bUseSubGroup?pScatSubGroup:pScatGroup;

    pCumScatXS = new double[nScatGroupXS];
    pCumScatXS2 = new double[nScatGroupXS];
    pCumScatUN = new double[nScatGroup];
    memset(pCumScatXS,  0, nScatGroupXS*sizeof(double));
    memset(pCumScatXS2, 0, nScatGroupXS*sizeof(double));
    memset(pCumScatUN,  0, nScatGroup*sizeof(double));

    resxs.AddEnergyPoints(nScatGroupXS+1, pScatGroupXS);
  }
  if (bCapture) {
    nCaptGroupXS = bUseSubGroup?nCaptSubGroup:nCaptGroup;
    pCaptGroupXS = bUseSubGroup?pCaptSubGroup:pCaptGroup;

    pCumCaptXS = new double[nCaptGroupXS];
    pCumCaptXS2 = new double[nCaptGroupXS];
    pCumCaptUN = new double[nCaptGroup];
    memset(pCumCaptXS,  0, nCaptGroupXS*sizeof(double));
    memset(pCumCaptXS2, 0, nCaptGroupXS*sizeof(double));
    memset(pCumCaptUN,  0, nCaptGroup*sizeof(double));

    resxs.AddEnergyPoints(nCaptGroupXS+1, pCaptGroupXS);
  }
  if (bFission) {
    nFissGroupXS = bUseSubGroup?nFissSubGroup:nFissGroup;
    pFissGroupXS = bUseSubGroup?pFissSubGroup:pFissGroup;

    pCumFissXS = new double[nFissGroupXS];
    pCumFissXS2 = new double[nFissGroupXS];
    pCumFissUN = new double[nFissGroup];
    memset(pCumFissXS,  0, nFissGroupXS*sizeof(double));
    memset(pCumFissXS2, 0, nFissGroupXS*sizeof(double));
    memset(pCumFissUN,  0, nFissGroup*sizeof(double));

    resxs.AddEnergyPoints(nFissGroupXS+1, pFissGroupXS);
  }

  // repeat calculations of average c/s and their uncertainties for nIteration times
  // in order to take uncertainties from unknown Js into account

  for (int i=0;i<nIteration;i++) {
    resxs.AssignJ();

    fprintf(stderr, "Iteration %4d\n", i+1);
    if (bScattering) {
      if (bUseSubGroup) resxs.GetAvgXS(SCAT, nScatGroupXS, pScatGroupXS, pScatXS);
      else resxs.GetAvgResXS(SCAT, nScatGroupXS, pScatGroupXS, pScatXS, nFlag);
      resxs.GetAbsResUN(SCAT, nScatGroup, pScatGroup, pScatUN, nFlag);
      for (int n=0;n<nScatGroupXS;n++) {
        pCumScatXS[n]  += pScatXS[n];
        pCumScatXS2[n] += pScatXS[n]*pScatXS[n];
      }
      for (int n=0;n<nScatGroup;n++) {
        pCumScatUN[n]  += pScatUN[n];
      }
    }
    if (bCapture) {
      if (bUseSubGroup) resxs.GetAvgXS(CAPT, nCaptGroupXS, pCaptGroupXS, pCaptXS);
      else resxs.GetAvgResXS(CAPT, nCaptGroupXS, pCaptGroupXS, pCaptXS, nFlag);
      resxs.GetAbsResUN(CAPT, nCaptGroup, pCaptGroup, pCaptUN, nFlag);
      for (int n=0;n<nCaptGroupXS;n++) {
        pCumCaptXS[n]  += pCaptXS[n];
        pCumCaptXS2[n] += pCaptXS[n]*pCaptXS[n];
      }
      for (int n=0;n<nCaptGroup;n++) {
        pCumCaptUN[n]  += pCaptUN[n];
      }
    }
    if (bFission) {
      if (bUseSubGroup) resxs.GetAvgXS(FISS, nFissGroupXS, pFissGroupXS, pFissXS);
      else resxs.GetAvgResXS(FISS, nFissGroupXS, pFissGroupXS, pFissXS, nFlag);
      resxs.GetAbsResUN(FISS, nFissGroup, pFissGroup, pFissUN, nFlag);
      for (int n=0;n<nFissGroupXS;n++) {
        pCumFissXS[n]  += pFissXS[n];
        pCumFissXS2[n] += pFissXS[n]*pFissXS[n];
      }
      for (int n=0;n<nFissGroup;n++) {
        pCumFissUN[n]  += pFissUN[n];
      }
    }
  }

  // compute average cross section and their uncertainties

  double *pResXS = NULL;
  if (bScattering) {
    pResXS = new double[nScatGroupXS];
    for (int n=0;n<nScatGroupXS;n++) {
      pScatXS[n] = pCumScatXS[n]/nIteration;
      pCumScatXS2[n] /= nIteration;
      pResXS[n] = pScatXS[n];
    }
    for (int n=0;n<nScatGroup;n++) {
      pScatUN[n] = pCumScatUN[n]/nIteration;
    }
  }
  if (bCapture) {
    for (int n=0;n<nCaptGroupXS;n++) {
      pCaptXS[n] = pCumCaptXS[n]/nIteration;
      pCumCaptXS2[n] /= nIteration;
    }
    for (int n=0;n<nCaptGroup;n++) {
      pCaptUN[n] = pCumCaptUN[n]/nIteration;
    }
  }
  if (bFission) {
    for (int n=0;n<nFissGroupXS;n++) {
      pFissXS[n] = pCumFissXS[n]/nIteration;
      pCumFissXS2[n] /= nIteration;
    }
    for (int n=0;n<nFissGroup;n++) {
      pFissUN[n] = pCumFissUN[n]/nIteration;
    }
  }

  // distribute absolute uncertainties in the bins into finer subbins if UseSubGroup is defined

  if (bUseSubGroup) {
    double *pNewUN;
    if (bScattering) {
      pNewUN = new double[nScatSubGroup];
      pScatIndex = new int[nScatSubGroup];
      resxs.GetSubGroupXSnUN(nScatGroup, pScatGroup, nScatSubGroup, pScatSubGroup, pScatXS, pScatUN, pNewUN, pScatIndex);
      delete[] pScatUN;
      pScatUN = pNewUN;
      nScatGroup = nScatSubGroup;
      delete[] pScatGroup;
      pScatGroup = pScatSubGroup;
    }
    if (bCapture) {
      pNewUN = new double[nCaptSubGroup];
      pCaptIndex = new int[nCaptSubGroup];
      resxs.GetSubGroupXSnUN(nCaptGroup, pCaptGroup, nCaptSubGroup, pCaptSubGroup, pCaptXS, pCaptUN, pNewUN, pCaptIndex);
      delete[] pCaptUN;
      pCaptUN = pNewUN;
      nCaptGroup = nCaptSubGroup;
      delete[] pCaptGroup;
      pCaptGroup = pCaptSubGroup;
    }
    if (bFission) {
      pNewUN = new double[nFissSubGroup];
      pFissIndex = new int[nFissSubGroup];
      resxs.GetSubGroupXSnUN(nFissGroup, pFissGroup, nFissSubGroup, pFissSubGroup, pFissXS, pFissUN, pNewUN, pFissIndex);
      delete[] pFissUN;
      pFissUN = pNewUN;
      nFissGroup = nFissSubGroup;
      delete[] pFissGroup;
      pFissGroup = pFissSubGroup;
    }
  }

  // add potential c/s and their uncertainties

  if (bScattering) {
    if (!bUseSubGroup) resxs.AddPotentialXS(nFirstResScatGroup, nScatGroup, pScatGroup, pScatXS);
    resxs.AddPotentialUN(nFirstResScatGroup, nScatGroup, pScatGroup, pScatUN);
  }

  double xs2;

  // add uncertainties from unknown Js

  if (bScattering) {
    for (int n=0;n<nScatGroup;n++) {
      if (!bUseSubGroup) pCumScatXS2[n] += (pScatXS[n]-pResXS[n])*(pScatXS[n]-pResXS[n]) + 2*(pScatXS[n]-pResXS[n])*pResXS[n];
      xs2 = pScatXS[n]*pScatXS[n];
      if (xs2 == 0.0) pScatUN[n] = 0.0;
      else pScatUN[n] = sqrt(pScatUN[n]*pScatUN[n] + max(pCumScatXS2[n] - xs2, 0.0));
      pScatUN[n] = max(pScatUN[n],fScatlim*pScatXS[n]);
    }
    if (pResXS) delete[] pResXS;
  }
  if (bCapture) {
    for (int n=0;n<nCaptGroup;n++) {
      xs2 = pCaptXS[n]*pCaptXS[n];
      if (xs2 == 0.0) pCaptUN[n] = 0.0;
      else pCaptUN[n] = sqrt(pCaptUN[n]*pCaptUN[n] + max(pCumCaptXS2[n] - xs2, 0.0));
      pCaptUN[n] = max(pCaptUN[n],fCaptlim*pCaptXS[n]);
      // the following line is for Pb208 only:
      // if(pCaptGroup[n+1] < 1.0E+06) pCaptUN[n] = max(0.5,pCaptUN[n]);
    }
  }
  if (bFission) {
    for (int n=nFirstResFissGroup;n<nFissGroup;n++) {
      xs2 = pFissXS[n]*pFissXS[n];
      if (xs2 == 0.0) pFissUN[n] = 0.0;
      else pFissUN[n] = sqrt(pFissUN[n]*pFissUN[n] + max(pCumFissXS2[n] - xs2, 0.0));
      pFissUN[n] = max(pFissUN[n],fFisslim*pFissXS[n]);
    }
  }

  // convert absolute uncertainties to relative ones

  if (bScattering) {
    for (int n=0;n<nScatGroup;n++) {
      if (pScatXS[n] == 0) pScatUN[n] = 0;
      else pScatUN[n] /= pScatXS[n];
    }
  }
  if (bCapture) {
    for (int n=0;n<nCaptGroup;n++) {
      if (pCaptXS[n] == 0) pCaptUN[n] = 0;
      else pCaptUN[n] /= pCaptXS[n];
    }
  }
  if (bFission) {
    for (int n=0;n<nFissGroup;n++) {
      if (pFissXS[0] == 0) pFissUN[n] = 0;
      else pFissUN[n] /= pFissXS[n];
    }
  }

  // calculate thermal cross sections and their uncertainties and assign predefined uncertainties

  if (bScattering) {
    for (int n=0;n<nFirstResScatGroup;n++) {
      // assign thermal scattering cross section
      if (pScatGroup[n] < 0.5) {
        pScatXS[n] = fScatteringXS;
        pScatUN[n] = fScatteringUN;
      } else {
        pScatXS[n] = fScatteringXS;
        pScatUN[n] = fScatteringUN2;
      }
    }
    // assign predefined uncertainties
    for (int n=0;n<nPreDefScatUN;n++)
      if (bUseSubGroup) {
        for (int i=0;i<nScatGroup;i++) {
          if (pScatIndex[i] == pPreDefScatUN[n].nBin) pScatUN[i] = pPreDefScatUN[n].fUnc;
        }
      } else pScatUN[pPreDefScatUN[n].nBin] = pPreDefScatUN[n].fUnc;
  }

  if (bCapture) {
    for (int n=0;n<nFirstResCaptGroup;n++) {
      // calculate thermal capture cross section
      pCaptXS[n] = 2.0*fCaptureXS*sqrt(0.0253)*(sqrt(pCaptGroup[n+1])-sqrt(pCaptGroup[n]))/(pCaptGroup[n+1]-pCaptGroup[n]);
      if (pCaptGroup[n] < 0.5) pCaptUN[n] = fCaptureUN;
      else pCaptUN[n] = fCaptureUN2;
    }
    // assign predefined uncertainties
    for (int n=0;n<nPreDefCaptUN;n++)
      if (bUseSubGroup) {
        for (int i=0;i<nCaptGroup;i++) {
          if (pCaptIndex[i] == pPreDefCaptUN[n].nBin) pCaptUN[i] = pPreDefCaptUN[n].fUnc;
        }
      } else pCaptUN[pPreDefCaptUN[n].nBin] = pPreDefCaptUN[n].fUnc;
  }

  if (bFission) {
    // assign predefined uncertainties
    for (int n=0;n<nPreDefFissUN;n++)
      if (bUseSubGroup) {
        for (int i=0;i<nFissGroup;i++) {
          if (pFissIndex[i] == pPreDefFissUN[n].nBin) pFissUN[i] = pPreDefFissUN[n].fUnc;
        }
      } else pFissUN[pPreDefFissUN[n].nBin] = pPreDefFissUN[n].fUnc;
  }

  if (pCumScatXS) delete[] pCumScatXS;
  if (pCumScatXS2) delete[] pCumScatXS2;
  if (pCumCaptXS) delete[] pCumCaptXS;
  if (pCumCaptXS2) delete[] pCumCaptXS2;
  if (pCumFissXS) delete[] pCumFissXS;
  if (pCumFissXS2) delete[] pCumFissXS2;
  if (pCumScatUN) delete[] pCumScatUN;
  if (pCumCaptUN) delete[] pCumCaptUN;
  if (pCumFissUN) delete[] pCumFissUN;
  if (pScatIndex) delete[] pScatIndex;
  if (pCaptIndex) delete[] pCaptIndex;
  if (pFissIndex) delete[] pFissIndex;
}

void PrintPotential()
{
  int nScatGroupXS = bUseSubGroup?nScatSubGroup:nScatGroup;
  double *pScatGroupXS = bUseSubGroup?pScatSubGroup:pScatGroup;
  double f1, f2;
  for (int i=0;i<nScatGroupXS;i++) {
    double e = (pScatGroupXS[i] + pScatGroupXS[i+1])/2.0;
    resxs.GetPotentialXS(e, f1, f2);
    printf("%3d %10.4lE - %10.4lE:  %11.5lE +/- %11.5lE\n", i+1, pScatGroupXS[i], pScatGroupXS[i+1], f1, f2);
  }
}

// calculate resonance integral and its uncertainty for scattering
void CalcIntegral(double fCorrNNRT, double fCorrGGRT, double fCorrNNB, double fCorrGGB)
{
  if (bScattering) {

    double sri=0.0, dsri=0.0;
    double uri1, uri2;
    double e1, e2, de, xx, corx;

    for (int i=0;i<nScatGroup;i++) {

      if (pScatGroup[i+1] <= 0.5) continue;

      e1 = max(pScatGroup[i],0.5);
      e2 = pScatGroup[i+1];
      de = pScatGroup[i+1] - pScatGroup[i];
      xx = pScatXS[i]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;
      sri += xx;
      uri1 = pScatUN[i]*xx;
      // printf("%10.4lE - %10.4lE: ri for scat = %8.4lf\n",e1,e2,xx);

      for (int j=0;j<nScatGroup;j++) {

        if (pScatGroup[j+1] <= 0.5) continue;

        e1 = max(pScatGroup[j],0.5);
        e2 = min(pScatGroup[j+1],1.0E+05);
        de = pScatGroup[j+1] - pScatGroup[j];
        uri2 = pScatUN[j]*pScatXS[j]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;

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
      xx = b*pCaptXS[i]*ev*de;
      cmx += xx;
      umx1 = pCaptUN[i]*xx;
      // printf("group %d: cmx=%lf\n", i+1, xx);

      // calculate the regular resonance integral from 0.5 - 100,000 eV.

      if (pCaptGroup[i+1] <= 0.5)   continue;
      if (pCaptGroup[i] >= 1.0E+05) continue;

      e1 = max(pCaptGroup[i],0.5);
      e2 = min(pCaptGroup[i+1],1.0E+05);
      xx = pCaptXS[i]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;
      // xx = pCaptXS[i]*log(e2/e1)*(e2-e1)/de;
      cri += xx;
      uri1 = pCaptUN[i]*xx;

      for (int j=0;j<nCaptGroup;j++) {

        ev = (pCaptGroup[j] + pCaptGroup[j+1])/2.0;
        ev = ev*exp(-a*ev/3.0E+04);
        de = pCaptGroup[j+1] - pCaptGroup[j];
        umx2 = b*pCaptUN[j]*pCaptXS[j]*ev*de;

        if (pCaptGroup[j+1] <= 0.5)   continue;
        if (pCaptGroup[j] >= 1.0E+05) continue;

        e1 = max(pCaptGroup[j],0.5);
        e2 = min(pCaptGroup[j+1],1.0E+05);
        uri2 = pCaptUN[j]*pCaptXS[j]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;
        // uri2 = pCaptUN[j]*pCaptXS[j]*log(e2/e1)*(e2-e1)/de;

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
    fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[i], pScatXS[i]);
  fprintf(fp, "%10.4lE %11.5lE\n", pScatGroup[i], pScatXS[i-1]);
  fclose(fp);

  // create a data file including the uncertainties of scattering cross sections
  if ((fp = fopen("SCAT-UNC.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'SCAT-UNC.dat'\n", stderr);
    exit(1);
  }
  for (i=0;i<nScatGroup;i++) {
    fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[i], 100.0*pScatUN[i]);
  }
  fprintf(fp, "%10.4lE %5.2lf\n", pScatGroup[i], 100.0*pScatUN[i-1]);
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
    fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[i], pCaptXS[i]);
  fprintf(fp, "%10.4lE %11.5lE\n", pCaptGroup[i], pCaptXS[i-1]);
  fclose(fp);

  // create a data file including the uncertainties of capture cross sections
  if ((fp = fopen("CAPT-UNC.dat", "w")) == NULL) {
    fputs("ERROR: cannot creat a file 'CAPT-UNC.dat'\n", stderr);
    exit(1);
  }
  for (i=0;i<nCaptGroup;i++) {
    fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[i], 100.0*pCaptUN[i]);
  }
  fprintf(fp, "%10.4lE %5.2lf\n", pCaptGroup[i], 100.0*pCaptUN[i-1]);
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
          fprintf(fp, " %8.3lE", pScatUN[i]*pScatUN[j]);
        else if (i < nFirstResScatGroup && j < nFirstResScatGroup)
          fprintf(fp, " %8.3lE", pScatUN[i]*pScatUN[j]);
        else if ((i < nFirstResScatGroup && j >= nFirstResScatGroup) ||
                 (i >= nFirstResScatGroup && j < nFirstResScatGroup))
          fprintf(fp, " %8.3lE", fCorrNNRT*pScatUN[i]*pScatUN[j]);
        else fprintf(fp, " %8.3lE", fCorrNNB*pScatUN[i]*pScatUN[j]);
      }
      fputs("\n", fp);
    }
    fclose(fp);
  }
  if (bCapture && (fp=fopen("capture.matrix", "w")) != NULL) {
    for (int i=0;i<nCaptGroup;i++) {
      for (int j=0;j<nCaptGroup;j++) {
        if (i == j)
          fprintf(fp, " %8.3lE", pCaptUN[i]*pCaptUN[j]);
        else if (i < nFirstResCaptGroup && j < nFirstResCaptGroup)
          fprintf(fp, " %8.3lE", pCaptUN[i]*pCaptUN[j]);
        else if ((i < nFirstResCaptGroup && j >= nFirstResCaptGroup) ||
                 (i >= nFirstResCaptGroup && j < nFirstResCaptGroup))
          fprintf(fp, " %8.3lE", fCorrGGRT*pCaptUN[i]*pCaptUN[j]);
        else fprintf(fp, " %8.3lE", fCorrGGB*pCaptUN[i]*pCaptUN[j]);
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
    for (i=0;i<resxs.NoRes();i++) {
      resxs.GetParameter(i, E, l);
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
      endf.WriteCont(1e-5, fE, 1, 2, 0, 1, nMat, 2, 151);
    } else endf.WriteCont(1e-5, resxs.GetE(resxs.NoRes()-1), 1, 2, 0, 1, nMat, 2, 151);
    endf.WriteCont(resxs.GetSpin(), .1*fR, 0, 0, nL, 0, nMat, 2, 151);
    for (n=0;n<3;n++) {
      if (lcount[n] <= 0) continue;
      endf.WriteCont(fAwr, .1*fR, n, 0, 6*lcount[n], lcount[n], nMat, 2, 151);
      for (i=0;i<nRes;i++) {
        if (resxs.GetL(i) == n) {
          resxs.GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
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
    endf.WriteCont(1e-5, resxs.GetE(resxs.NoRes()-1), 1, 2, 0, 1, nMat, 32, 151);
    endf.WriteCont(resxs.GetSpin(), .1*fR, 0, 2, 0, 0, nMat, 32, 151);
    endf.WriteCont(fAwr, .0, 0, 0, 12*resxs.NoRes(), resxs.NoRes(), nMat, 32, 151);
    for (i=0;i<resxs.NoRes();i++) {
      resxs.GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
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
      pCov[n++] = pScatUN[i]*pScatUN[i];
      for (j=0;j<nScatGroup-i-1;j++) {
        if (i < nFirstResScatGroup && j+i+1 < nFirstResScatGroup)
          pCov[n++] = pScatUN[i]*pScatUN[j+i+1];
        else if (i < nFirstResScatGroup && j+i+1 >= nFirstResScatGroup)
           pCov[n++] = fCorrNNRT*pScatUN[i]*pScatUN[j+i+1];
        else pCov[n++] = fCorrNNB*pScatUN[i]*pScatUN[j+i+1];
      }
    }
    endf.WriteList(.0, .0, 1, 5, nScatGroup*(nScatGroup+1)/2+nScatGroup+1, nScatGroup+1, nMat, 33, 2, pCov, pScatGroup);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 33, 0);

    pCov = (double*)realloc(pCov, sizeof(double)*nCaptGroup*(nCaptGroup+1)/2);
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 0, 1, nMat, 33, 102);
    endf.WriteCont(.0, .0, 0, 102, 0, 1, nMat, 33, 102);
    for (i=0,n=0;i<nCaptGroup;i++) {
      pCov[n++] = pCaptUN[i]*pCaptUN[i];
      for (j=0;j<nCaptGroup-i-1;j++) {
        if (i < nFirstResScatGroup && j+i+1 < nFirstResScatGroup)
          pCov[n++] = pCaptUN[i]*pCaptUN[j+i+1];
        else if (i < nFirstResCaptGroup && j+i+1 >= nFirstResCaptGroup)
          pCov[n++] = fCorrGGRT*pCaptUN[i]*pCaptUN[j+i+1];
        else pCov[n++] = fCorrGGB*pCaptUN[i]*pCaptUN[j+i+1];
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

