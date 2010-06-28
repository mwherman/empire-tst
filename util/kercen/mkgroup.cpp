/***********************************************************************
 *
 * Filename: mkgroup.cpp
 * Purpose : Determine the rough energy group structure for resonance region
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "atlas.h"

#define PI		3.14159

static CAtlas atlas;
static int nZ, nA;
static double fMin = .0, fMax = 1e38;
static double fGammaFactor = 4;

double *Calculate(int &nGroup)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  double E1,J1,gGn1,Gn1,Gg1,Gf1,area1,farea1;
  double E2,J2,gGn2,Gn2,Gg2,Gf2,area2,farea2;
  double *pGroup;
  double e;
  pGroup = (double*)malloc((atlas.NoRes()+1)*sizeof(double));
  if (pGroup == NULL) {
    fputs("out of memory\n", stderr);
    exit(1);
  }
  pGroup[0] = 1e-5;
  pGroup[1] = 20;
  nGroup = 1;
  for (int i=0;i<atlas.NoRes();i++) {
    atlas.GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
    if (E > 0) break;
  }
  e = E - 10*(Gn+Gg);
  for (int i=0;i<atlas.NoRes();i++) {
    atlas.GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
    if (E < fMin) continue;
    if (E > fMax) break;
    if (E - fGammaFactor*(Gn+Gg) < e)			// check again if e is lower than E - 4*(Gn+Gg)
      e = E - fGammaFactor*(Gn+Gg);
  }
  if (e > 20) pGroup[++nGroup] = e;
  double spin = atlas.GetSpin();
  double e1, e2;
  
  for (int i=0;i<atlas.NoRes();i++) {
    atlas.GetParameter(i, E1, J1, gGn1, Gn1, Gg1, Gf1, area1, farea1);
    if (i+1 == atlas.NoRes()) E2 = 0;
    else atlas.GetParameter(i+1, E2, J2, gGn2, Gn2, Gg2, Gf2, area2, farea2);
    if (E1 < fMin) continue;
    if (E1 > fMax) break;
    e1 = e2 = 0;
    e = floor(E1 + fGammaFactor*(Gn1+Gg1));
    while (1) {
      bool ok = true;
      for (int k=0;k<atlas.NoRes();k++) {
        atlas.GetParameter(k, E, J, gGn, Gn, Gg, Gf, area, farea);
        if (E > 0 &&
            ((k <= i && e <= E+fGammaFactor*(Gn+Gg)) ||
             (k > i && e >= E-fGammaFactor*(Gn+Gg)))) {
          ok = false;
//          printf("%lf(%lf): %lf,%lf,%lf,%lf,%lf FAILED\n", e, E+fGammaFactor*(Gn+Gg), E, J, Gn, Gg, area);
          break;
        }
//        printf("%lf: %lf,%lf,%lf,%lf,%lf PASSED\n", e, E, J, Gn, Gg, area);
      }
      if (ok) {
        e1 = e;
        break;
      }
      e += 1;
      if (E2 > 0 && e >=  E2 - fGammaFactor*(Gn2+Gg2)) break;
    }
    
    if (i+1 == atlas.NoRes()) e2 = e1;
    else {
      e = floor(E2 - fGammaFactor*(Gn2+Gg2));
      while (1) {
        bool ok = true;
        for (int k=0;k<atlas.NoRes();k++) {
          atlas.GetParameter(k, E, J, gGn, Gn, Gg, Gf, area, farea);
          if (E > 0 &&
              ((k <= i && e <= E+fGammaFactor*(Gn+Gg)) ||
               (k > i && e >= E-fGammaFactor*(Gn+Gg)))) {
            ok = false;
            break;
          }
        }
        if (ok) {
          e2 = e;
          break;
        }
        if ((e -= 1) <=  E1 + fGammaFactor*(Gn1+Gg1)) break;
      }
    }
    if (e1 != 0 && e2 != 0) {
      pGroup[++nGroup] = (e1+e2)/2;
    }
  }
  
  return pGroup;
}

int main(int argc, char **argv)
{
  FILE *fp;
  char szAtlasDir[PATH_MAX];
  char s[1024];
  if (argc < 2) {
    fputs("usage: mkgroup inputfile\n", stderr);
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
    else if (!strcasecmp(key, "atlasdir")) strcpy(szAtlasDir, value);
    else if (!strcasecmp(key, "emin")) fMin = atof(value);
    else if (!strcasecmp(key, "emax")) fMax = atof(value);
    else if (!strcasecmp(key, "gammafactor")) fGammaFactor = atof(value);
  }
  fclose(fp);

  puts("#############################################################");
  puts("##                                                         ##");
  puts("##                       MKGROUP v1.0                      ##");
  puts("##                                                         ##");
  puts("#############################################################");
  puts("");
  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############       INPUT DESCRIPTION        ##############");
  puts("###############                                ##############");
  puts("#############################################################");

  printf("Z = %d, A = %d\n", nZ, nA);
  printf("EMin = %lf, EMax = %lf\n", fMin, fMax);
  printf("atlas directory: %s\n", szAtlasDir);
  printf("gamma factor   : %lf\n", fGammaFactor);
 
  if (szAtlasDir[0] == 0) {
    fputs("You should enter the name of the directory containig the Atlas\n", stderr);
    exit(1);
  }

  atlas.SetBaseDirectory(szAtlasDir);
  atlas.Read(nZ, nA);
  atlas.AssignJ();

  int nGroup;
  double *pGroup = NULL;
  pGroup = Calculate(nGroup);

  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############             RESULTS            ##############");
  puts("###############                                ##############");
  puts("#############################################################");

  for (int i=0;i<nGroup;i++) {
    printf("%d: %lf - %lf\n", i+1, pGroup[i], pGroup[i+1]);
  }

  puts("-------------------------------------------------------------");
  printf("%d\n", nGroup);
  for (int i=0;i<=nGroup;i++) {
    printf("%11.5lE\n", pGroup[i]);
  }
  printf(" %d\n", nGroup);
  for (int i=0;i<=nGroup;i++) {
    if (i > 0 && i%6 == 0) puts("");
    printf("% 11.5lE", pGroup[i]);
  }
  puts("");
  free(pGroup);
}
