/***********************************************************************
 *
 * Filename: regroup.cpp
 * Purpose : Determine the optimum energy group structure for resonance region
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int nGroup = 0, nStart = 0, nNewGroup = 0;
double *pGroup = NULL;				// original group structure
double *pNewGroup = NULL;			// new group structure
double *pKernelXS = NULL;			// integrated cross sections from Kernel (in original group)
double *pNjoyXS = NULL;				// integraed cross sections from NJOY (in original group)
double *pKernelSum = NULL;			// integrated cross sections from Kernel (in new group)
double *pNjoySum = NULL;			// integrated cross sections from NJOY (in new group)
double *pDiff = NULL;				// relative differences between Kernel and NJOY
double fScatMinXS = 20000;			// minimum integrated scattering cross section allowed in binning
double fScatMaxXS = 40000;			// maximum integrated scattering cross section allowed in binning
double fCaptMinXS = 200;			// minimum integrated capture cross section allowed in binning
double fCaptMaxXS = 400;			// maximum integrated capture cross section allowed in binning
double fScatDiffCut = .2;			// maximum scattering relative error allowed in binning
double fCaptDiffCut = .04;			// maximum capture relative error allowed in binning

void ReadData(const char *szGroup, const char *szXSFile)
{
  FILE *fp;
  if ((fp=fopen(szGroup, "r")) == NULL) {
    fprintf(stderr, "cannot open '%s'\n", szGroup);
    exit(1);
  }
  fscanf(fp, "%d %d", &nGroup, &nStart);
//  printf("Starging from group %d\n", nStart);
  nNewGroup = --nStart;
  if (pGroup) delete[] pGroup;
  if (pNewGroup) delete[] pNewGroup;
  pGroup = new double[nGroup+1];
  pNewGroup = new double[nGroup+1];
  for (int i=0;i<=nGroup;i++) fscanf(fp, "%lf", pGroup+i);
  fclose(fp);
  memset(pNewGroup, 0, (nGroup+1)*sizeof(double));

  if ((fp=fopen(szXSFile, "r")) == NULL) {
    fprintf(stderr, "cannot open '%s'\n", szXSFile);
    free(pGroup);
    exit(1);
  }
  if (pKernelXS) delete[] pKernelXS;
  if (pNjoyXS) delete[] pNjoyXS;
  if (pKernelSum) delete[] pKernelSum;
  if (pNjoySum) delete[] pNjoySum;
  if (pDiff) delete[] pDiff;
  pKernelXS = new double[nGroup];
  pNjoyXS = new double[nGroup];
  pKernelSum = new double[nGroup];
  pNjoySum = new double[nGroup];
  pDiff = new double[nGroup];
  double sum = 0;
  puts("<Original bin>");
  puts("        E1          E2          Kernel      NJOY    Diff");
  for (int i=0;i<nGroup;i++) {
    fscanf(fp, "%lf %lf", pKernelXS+i, pNjoyXS+i);
    pKernelXS[i] *= (pGroup[i+1]-pGroup[i]);
    pNjoyXS[i] *= (pGroup[i+1]-pGroup[i]);
    pDiff[i] = (pKernelXS[i]-pNjoyXS[i])/pNjoyXS[i];
    if (i >= nStart) sum += pKernelXS[i];
    printf("%2d: %10.5lE %10.5lE %10.1f %10.1f %6.2lf %%\n",
             i+1, pGroup[i], pGroup[i+1], pKernelXS[i], pNjoyXS[i], pDiff[i]*100);
//    printf("%2d: %lf - %lf = %lf, %lf\n", i+1, pGroup[i], pGroup[i+1], pKernelXS[i], pNjoyXS[i]);
  }
  fclose(fp);
}

void Calculate(double fMinXS, double fMaxXS, double fDiffCut)
{
  double xs1, xs2, diff, pxs1, pxs2;
  int n;
  for (int i=0;i<nStart;i++) {
    pNewGroup[i] = pGroup[i];
    pKernelSum[i] = pKernelXS[i];
    pNjoySum[i] = pNjoyXS[i];
  }
  pNewGroup[nStart] = pGroup[nStart];
  for (int i=nStart;i<nGroup;) {
    n = -1;
    xs1 = xs2 = 0;
    diff = 1e38;
//    printf("*** i=%d,E=%lf\n", i, pGroup[i]);
    for (int j=i;j<nGroup;j++) {
      xs1 += pKernelXS[j];
      xs2 += pNjoyXS[j];
//      printf("j=%d,xs=%lf\n",j,xs1);
      if (n != -1 && xs1 >= fMaxXS) break;
      else if (xs1 > fMinXS && fabs(xs1-xs2)/xs2 < fDiffCut && fabs(xs1-xs2)/xs2 < fabs(diff)) {
        n = j;
        diff = (xs1-xs2)/xs2;
        pxs1 = xs1;
        pxs2 = xs2;
//        printf("n = %d, diff = %lf %%\n", j, diff*100);
      }
    }
    if (n == -1) {
      n = nGroup - 1;
      diff = (xs1-xs2)/xs2;
      pxs1 = xs1;
      pxs2 = xs2;
    }
    pNewGroup[nNewGroup+1] = pGroup[n+1];
    pDiff[nNewGroup] = diff;
    pKernelSum[nNewGroup] = pxs1;
    pNjoySum[nNewGroup] = pxs2;
    ++nNewGroup;
    i = n+1;
//    printf("*** i = %d now\n", i);
  }
  if (nNewGroup - nStart >= 2 && pKernelSum[nNewGroup-1] < 50) {
    pNewGroup[nNewGroup-1] = pNewGroup[nNewGroup];
    pKernelSum[nNewGroup-2] += pKernelSum[nNewGroup-1];
    pNjoySum[nNewGroup-2] += pNjoySum[nNewGroup-1];
    pDiff[nNewGroup-2] = (pKernelSum[nNewGroup-2]-pNjoySum[nNewGroup-2])/pNjoySum[nNewGroup-2];
    --nNewGroup;
  }
}

void WriteResult()
{
  puts("<New bin>");
  puts("        E1          E2          Kernel      NJOY    Diff");
  for (int i=0;i<nNewGroup;i++) {
    //if (i >= nStart)
      printf("%2d: %10.5lE %10.5lE %10.1f %10.1f %6.2lf %%\n",
             i+1, pNewGroup[i], pNewGroup[i+1], pKernelSum[i], pNjoySum[i], pDiff[i]*100);
    //else
     // printf("%2d: %10.5lE %10.5lE\n", i+1, pNewGroup[i], pNewGroup[i+1]);
  }

  puts("-------------------------------------------------------------");
  printf("%d\n", nNewGroup);
  for (int i=0;i<=nNewGroup;i++) {
    printf("%10.5lE\n", pNewGroup[i]);
  }
  printf(" %d\n", nNewGroup);
  for (int i=0;i<=nNewGroup;i++) {
    if (i > 0 && i%6 == 0) puts("");
    printf("% 11.5lE", pNewGroup[i]);
  }
  puts("");
}

int main(int argc, char **argv)
{
  FILE *fp;
  char szScatGroup[PATH_MAX];
  char szCaptGroup[PATH_MAX];
  char szScatXS[PATH_MAX];
  char szCaptXS[PATH_MAX];
  char s[1024];
  bool bScat = false, bCapt = false;
  if (argc < 2) {
    fputs("usage: regroup inputfile\n", stderr);
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
    else if (!strcasecmp(key, "scatminxs")) fScatMinXS = atof(value);
    else if (!strcasecmp(key, "captminxs")) fCaptMinXS = atof(value);
    else if (!strcasecmp(key, "scatmaxxs")) fScatMaxXS = atof(value);
    else if (!strcasecmp(key, "captmaxxs")) fCaptMaxXS = atof(value);
    else if (!strcasecmp(key, "scatdiffcut")) fScatDiffCut = atof(value);
    else if (!strcasecmp(key, "captdiffcut")) fCaptDiffCut = atof(value);
    else if (!strcasecmp(key, "scatgroup")) strcpy(szScatGroup, value);
    else if (!strcasecmp(key, "captgroup")) strcpy(szCaptGroup, value);
    else if (!strcasecmp(key, "scatxs")) strcpy(szScatXS, value);
    else if (!strcasecmp(key, "captxs")) strcpy(szCaptXS, value);
  }
  fclose(fp);

  puts("#############################################################");
  puts("##                                                         ##");
  puts("##                       REGROUP v1.0                      ##");
  puts("##                                                         ##");
  puts("#############################################################");
  puts("");
  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############       INPUT DESCRIPTION        ##############");
  puts("###############                                ##############");
  puts("#############################################################");

  printf("ScatMinXS = %7.2lE, ScatMaxXS = %7.2lE, ScatDiffCut = %lg\n", fScatMinXS, fScatMaxXS, fScatDiffCut);
  printf("CaptMinXS = %7.2lE, CaptMaxXS = %7.2lE, CaptDiffCut = %lg\n", fCaptMinXS, fCaptMaxXS, fCaptDiffCut);
  printf("ScatXS file: %s\n", szScatXS);
  printf("CaptXS file: %s\n", szCaptXS);
 
  if (szScatGroup[0] != 0 && szScatXS[0] != 0) bScat = true;
  if (szCaptGroup[0] != 0 && szCaptXS[0] != 0) bCapt = true;
  if (!bScat && !bCapt) {
    fputs("You should enter the names of the files containig the group structure and average cross sections\n", stderr);
    exit(1);
  }

  if (fScatMaxXS == 0) fScatMaxXS = 2.5*fScatMinXS;
  if (fCaptMaxXS == 0) fCaptMaxXS = 2.5*fCaptMinXS;

  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############             RESULTS            ##############");
  puts("###############                                ##############");
  puts("#############################################################");

  if (bScat) {
    puts("\nFor scattering");
    ReadData(szScatGroup, szScatXS);
    Calculate(fScatMinXS, fScatMaxXS, fScatDiffCut);
    WriteResult();
  }

  if (bCapt) {
    puts("\nFor capture");
    ReadData(szCaptGroup, szCaptXS);
    Calculate(fCaptMinXS, fCaptMaxXS, fCaptDiffCut);
    WriteResult();
  }

  if (pGroup) delete[] pGroup;
  if (pNewGroup) delete[] pNewGroup;
  if (pKernelXS) delete[] pKernelXS;
  if (pNjoyXS) delete[] pNjoyXS;
  if (pKernelSum) delete[] pKernelSum;
  if (pNjoySum) delete[] pNjoySum;
  if (pDiff) delete[] pDiff;
}
