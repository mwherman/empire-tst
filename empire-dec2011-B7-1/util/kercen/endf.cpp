/***********************************************************************
 *
 * Filename: endf.cpp
 * Purpose : provides the extended functions for handling ENDF file
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "endf.h"
#include "util.h"

CEndf::CEndf()
{
}

CEndf::~CEndf()
{
}

// read resonance parameters from FILE 2
RESDATA *CEndf::ReadResParms(int isotope, int &nRes)
{
  int mat, mf, mt;
  char s[67];
  int lrp;
  double c1,c2;
  int l1,l2,n1,n2;
  int nis, ner, nls, lru, lrf, nro;
  double E, J, Gn, Gg;
  RESDATA *pRes = NULL;

  nRes = 0;

  if (m_fpEndf == NULL) return NULL;

  fseek(m_fpEndf, 0L, SEEK_SET);
  do {
    if (!ReadLine(mat, mf, mt)) break;
  } while (mf != 1 || mt != 451);
  if (mf != 1 || mt != 451) {			 // probably we're reading a wrong file
    fputs("could not find the lines with mf=1 and mt=451\n", stderr);
    return NULL;
  }
  Unread();

  ReadCont(c1, c2, lrp, l2, n1, n2, mat, mf, mt);
  printf("lrp = %d\n", lrp);
  if (lrp == -1 || lrp == 0) {
    fputs("no resonance parameters are given.\n", stderr);
    return NULL;
  }

  do {
    if (!ReadLine(mat, mf, mt)) break;
  } while (mf != 2 || mt != 151);
  if (mf != 2 || mt != 151) {			 // probably we're reading a wrong file
    fputs("could not find the lines with mf=2 and mt=451\n", stderr);
    return NULL;
  }
  Unread();

  puts("########## READING RESONANCE PARAMETERS ##########");

  double **pv;
  int *num;
  int *idx;
  int n = 0;
  ReadCont(c1, c2, l1, l2, nis, n2, mat, mf, mt);
  printf("nis = %d\n", nis);
  for (int i=0;i<nis;i++) {
    if (i != isotope) continue;
    ReadCont(c1, c2, l1, l2, ner, n2, mat, mf, mt);
    printf("ner = %d\n", ner);
    for (int j=0;j<ner;j++) {
      if (j>0) break;					// currently supports only first energy range
      ReadCont(c1, c2, lru, lrf, nro, n2, mat, mf, mt);
      if (nro != 0) {
        fputs("currently energy dependent scattering radius is not supported\n", stderr);
        exit(1);
      }
      ReadCont(c1, c2, l1, l2, nls, n2, mat, mf, mt);
      printf("nls = %d\n", nls);
      pv = new double*[nls];
      num = new int[nls];
      idx = new int[nls];
      // read resonance parameters
      for (int k=0;k<nls;k++) {
        pv[k] = ReadList(c1, c2, l1, l2, n1, n2, mat, mf, mt);
        num[k] = n1;
        idx[k] = 0;
        nRes += n2;
      }
      pRes = (RESDATA*)realloc(pRes, nRes*sizeof(RESDATA));
      while (1) {
        int in = -1;
        for (int k=0;k<nls;k++) {
          if (idx[k] < num[k]) {
            if (in == -1) in = k;
            else if (pv[k][idx[k]] < pv[in][idx[in]]) in = k;
          }
        }
        if (in == -1) break; 
        pRes[n].E = pv[in][idx[in]];
        pRes[n].J = pv[in][idx[in]+1];
        J = pv[in][idx[in]+1];
        if (lrf == 1 || lrf == 2) {
          pRes[n].Gn = pv[in][idx[in]+3];
          pRes[n].Gg = pv[in][idx[in]+4];
        } else if (lrf == 3) {
          pRes[n].Gn = pv[in][idx[in]+2];
          pRes[n].Gg = pv[in][idx[in]+3];
        } else {
          fprintf(stderr, "currently lrf %d not supported\n", lrf);
          exit(1);
        }
        pRes[n].dE = 0;
        pRes[n].dGn = 0;
        pRes[n].dGg = 0;
        pRes[n].nflag = 0;
        pRes[n].area = 0;
        if (++n == nRes) break;
        idx[in] += 6;
      }
      for (int k=0;k<nls;k++) delete[] pv[k];
      delete[] pv;
      delete[] num;
    }
  }

  if (nRes == 0 || n != nRes) {
    fprintf(stderr, "number of resonances read from ENDF (%d) does not match the expected number of resonances (%d)\n", n, nRes);
    for (int i=0;i<n;i++) {
      printf("E=%lf, Gn=%lf, Gg=%lf\n", pRes[i].E, pRes[i].Gn, pRes[i].Gg);
    }
    if (pRes) free(pRes);
    return NULL;
  }
  return pRes;
}
/*
// read resonance parameters and their uncertainties from FILE 32
bool CEndf::ReadResParmsB(int isotope, int &nRes, RESDATA **pRes)
{
  int mat, mf, mt;
  char s[67];
  int lrp, nis, ner, nls, lru, lrf, nro, lcomp;
  double c1,c2;
  int l1,l2,n1,n2;
  double E, J, Gn, Gg, area;

  nRes = 0;
  *pRes = NULL;

  if (m_fpEndf == NULL) return false;

  fseek(m_fpEndf, 0L, SEEK_SET);
  do {
    if (!ReadLine(mat, mf, mt)) break;
  } while (mf != 1 || mt != 451);
  if (mf != 1 || mt != 451) {			 // probably we're reading a wrong file
    fputs("could not find the lines with mf=1 and mt=451\n", stderr);
    return false;
  }
  Unread();

  ReadCont(c1, c2, lrp, l2, n1, n2, mat, mf, mt);
  printf("lrp = %d\n", lrp);
  if (lrp == -1 || lrp == 0) {
    fputs("no resonance parameters are given.\n", stderr);
    return false;
  }

  do {
    if (!ReadLine(mat, mf, mt)) break;
  } while (mf != 2 || mt != 151);
  if (mf != 2 || mt != 151) {			 // probably we're reading a wrong file
    fputs("could not find the lines with mf=1 and mt=451\n", stderr);
    return false;
  }
  Unread();

  puts("reading from ENDF ...");

  double **pv;
  int *num;
  int *idx;
  int n = 0;
  ReadCont(c1, c2, l1, l2, nis, n2, mat, mf, mt);
  printf("nis = %d\n", nis);
  for (int i=0;i<nis;i++) {
    if (i != isotope) continue;
    ReadCont(c1, c2, l1, l2, ner, n2, mat, mf, mt);
    printf("ner = %d\n", ner);
    for (int j=0;j<ner;j++) {
      ReadCont(c1, c2, lru, lrf, nro, n2, mat, mf, mt);
      if (nro != 0) {
        fputs("currently energy dependent scattering radius is not supported\n", stderr);
        exit(1);
      }
      ReadCont(c1, c2, l1, lcomp, nls, n2, mat, mf, mt);
      printf("lcomp = %d\n", lcomp);
      if (lcomp != 0) {
        fprintf(stderr, "currently lcomp %d not supported\n", lcomp);
        return false;
      }
      printf("nls = %d\n", nls);
      pv = new double*[nls];
      num = new int[nls];
      idx = new int[nls];
      // read resonance parameters
      for (int k=0;k<nls;k++) {
        pv[k] = ReadList(c1, c2, l1, l2, n1, n2, mat, mf, mt);
        num[k] = n1;
        idx[k] = 0;
        nRes += n2;
      }
      *pRes = (RESDATA*)realloc(*pRes, nRes*sizeof(RESDATA));
      while (1) {
        int in = -1;
        for (int k=0;k<nls;k++) {
          if (idx[k] < num[k]) {
            if (in == -1) in = k;
            else if (pv[k][idx[k]] < pv[in][idx[in]]) in = k;
          }
        }
        if (in == -1) break; 
        (*pRes)[n].dE = 0;
        (*pRes)[n].dGn = 0;
        (*pRes)[n].dGg = 0;
        (*pRes)[n].nflag = 0;
        (*pRes)[n].area = 0;
        (*pRes)[n].E = pv[in][idx[in]];
        (*pRes)[n].J = pv[in][idx[in]+1];
        J = pv[in][idx[in]+1];
        if (lrf == 1 || lrf == 2) {
          (*pRes)[n].Gn = pv[in][idx[in]+3];
          (*pRes)[n].Gg = pv[in][idx[in]+4];
          (*pRes)[n].dE = sqrt(pv[in][idx[in]+6]);
          (*pRes)[n].dGn = sqrt(pv[in][idx[in]+7]);
          (*pRes)[n].dGg = sqrt(pv[in][idx[in]+9]);
        } else if (lrf == 3) {
          (*pRes)[n].Gn = pv[in][idx[in]+2];
          (*pRes)[n].Gg = pv[in][idx[in]+3];
        } else {
          fprintf(stderr, "currently lrf %d not supported\n", lrf);
          exit(1);
        }
        n++;
        idx[in] += 18;
      }
      for (int k=0;k<nls;k++) delete[] pv[k];
      delete[] pv;
      delete[] num;
    }
  }

  if (nRes == 0 || n != nRes) {
    fputs("number of resonances read from ENDF does not match the expected number of resonances\n", stderr);
    return false;
  }
  return true;
}
*/

RESDATA *CEndf::ReadResParmsUncertainty(int isotope, int &nRes)
{
  int mat, mf, mt;
  char s[67];
  int lrp;
  double c1,c2;
  int l1,l2,n1,n2;
  int lcomp;
  int nis, ner, nls, lru, lrf, nro;
  RESDATA *pRes = NULL;

  nRes = 0;

  if (m_fpEndf == NULL) return NULL;

  fseek(m_fpEndf, 0L, SEEK_SET);
  do {
    if (!ReadLine(mat, mf, mt)) break;
  } while (mf != 1 || mt != 451);
  if (mf != 1 || mt != 451) {			 // probably we're reading a wrong file
    fputs("could not find the lines with mf=1 and mt=451\n", stderr);
    return NULL;
  }
  Unread();

  ReadCont(c1, c2, lrp, l2, n1, n2, mat, mf, mt);
  printf("lrp = %d\n", lrp);
  if (lrp == -1 || lrp == 0) {
    fputs("no resonance parameters are given.\n", stderr);
    return NULL;
  }

  do {
    if (!ReadLine(mat, mf, mt)) break;
  } while (mf != 32 || mt != 151);
  if (mf != 32 || mt != 151) {			 // probably we're reading a wrong file
    fputs("could not find the lines with mf=32 and mt=451\n", stderr);
    return NULL;
  }
  Unread();

  puts("########## READING RESONANCE PARAMETERS AND THEIR UNCERTAINTIES ##########");

  ReadCont(c1, c2, l1, l2, nis, n2, mat, mf, mt);
  printf("nis = %d\n", nis);
  for (int i=0;i<nis;i++) {
    if (i != isotope) continue;
    ReadCont(c1, c2, l1, l2, ner, n2, mat, mf, mt);
    printf("ner = %d\n", ner);
    for (int j=0;j<ner;j++) {
      if (j>0) break;					// currently supports only first energy range
      ReadCont(c1, c2, lru, lrf, nro, n2, mat, mf, mt);
      if (nro != 0) {
        fputs("currently energy dependent scattering radius is not supported\n", stderr);
        exit(1);
      }
      ReadCont(c1, c2, l1, lcomp, nls, n2, mat, mf, mt);
      printf("lcomp = %d\n", lcomp);
      if (lcomp == 0) {
        fputs("currently does not support LCOMP=0\n", stderr);
        exit(1);
      } else if (lcomp == 1) {
        fputs("currently does not support LCOMP=1\n", stderr);
        exit(1);
      } else if (lcomp == 2) {
        double *pv = ReadList(c1, c2, l1, l2, n1, n2, mat, mf, mt);
        if (pv == NULL) {
          fputs("Error occured while reading FILE 32\n", stderr);
          exit(1);
        }
        nRes = n2;
        pRes = (RESDATA*)malloc(nRes*sizeof(RESDATA));
        if (pRes == NULL) {
          fputs("Out of memory\n", stderr);
          exit(1);
        }
        for (int i=0;i<nRes;i++) {
          pRes[i].E = pv[12*i];
          pRes[i].J = pv[12*i+1];
          pRes[i].Gn = pv[12*i+3];
          pRes[i].Gg = pv[12*i+4];
          pRes[i].dGn = pv[12*i+9];
          pRes[i].dGg = pv[12*i+10];
          pRes[i].nflag = 0;
          pRes[i].area = 0;
        }
        delete[] pv;
      }
    }
  }
  return pRes;
}
