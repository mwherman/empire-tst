/***********************************************************************
 *
 * Filename: endfio.cpp
 * Purpose : provides the basic functions for handling ENDF file
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#include <stdio.h>
#include "endfio.h"
#include "util.h"

CEndfIO::CEndfIO()
{
  m_fpEndf = NULL;
  m_bCached = false;
}

CEndfIO::~CEndfIO()
{
  Close();
}

bool CEndfIO::Open(char *fname, char *mode)
{
  if (mode == NULL) return (m_fpEndf = fopen(fname, "r"));
  else return (m_fpEndf = fopen(fname, mode));
}

void CEndfIO::Close()
{
  if (m_fpEndf) {
    fclose(m_fpEndf);
    m_fpEndf = NULL;
  }
}

bool CEndfIO::Read()
{
  if (m_fpEndf == NULL) return false;
  if (m_bCached) m_bCached = false;
  else return fgets(m_szLine, 1024, m_fpEndf);
  return true;
}

void CEndfIO::Unread()
{
  m_bCached = true;
}

bool CEndfIO::ReadLine(int &mat, int &mf, int &mt)
{
  if (!Read()) return false;
  mat = ReadInt(m_szLine, 66, 4);
  mf = ReadInt(m_szLine, 70, 2);
  mt = ReadInt(m_szLine, 72, 3);
  return true;
}

bool CEndfIO::ReadText(char *s, int &mat, int &mf, int &mt)
{
  if (!Read()) return false;
  ReadString(m_szLine, 0, 66, s);
  mat = ReadInt(m_szLine, 66, 4);
  mf = ReadInt(m_szLine, 70, 2);
  mt = ReadInt(m_szLine, 72, 3);
  return true;
}

bool CEndfIO::WriteText(char *s, int mat, int mf, int mt)
{
  if (m_fpEndf == NULL) return false;
  return (fprintf(m_fpEndf, "%-66s%4d%2d%3d\n", s, mat, mf, mt) > 0);
}

bool CEndfIO::ReadCont(double &c1, double &c2, int &l1, int &l2, int &n1, int &n2,
                     int &mat, int &mf, int &mt)
{
  if (!Read()) return false;
  c1 = _ReadDouble(m_szLine, 0, 11);
  c2 = _ReadDouble(m_szLine, 11, 11);
  l1 = ReadInt(m_szLine, 22, 11);
  l2 = ReadInt(m_szLine, 33, 11);
  n1 = ReadInt(m_szLine, 44, 11);
  n2 = ReadInt(m_szLine, 55, 11);
  mat = ReadInt(m_szLine, 66, 4);
  mf = ReadInt(m_szLine, 70, 2);
  mt = ReadInt(m_szLine, 72, 3);
  return true;
}

bool CEndfIO::WriteCont(double c1, double c2, int l1, int l2, int n1, int n2, int mat, int mf, int mt)
{
  if (m_fpEndf == NULL) return false;
  char s1[15], s2[15];
  Floating2String(c1, 10, s1);
  Floating2String(c2, 10, s2);
  return (fprintf(m_fpEndf, "%11s%11s%11d%11d%11d%11d%4d%2d%3d\n", s1, s2, l1, l2, n1, n2, mat, mf, mt) > 0);
}

bool CEndfIO::WriteListItem(double b1, double b2, double b3, double b4, double b5, double b6, int mat, int mf, int mt)
{
  if (m_fpEndf == NULL) return false;
  char s1[15], s2[15], s3[15], s4[15], s5[15], s6[15];
  Floating2String(b1, 10, s1);
  Floating2String(b2, 10, s2);
  Floating2String(b3, 10, s3);
  Floating2String(b4, 10, s4);
  Floating2String(b5, 10, s5);
  Floating2String(b6, 10, s6);
  return (fprintf(m_fpEndf, "%11s%11s%11s%11s%11s%11s%4d%2d%3d\n", s1, s2, s3, s4, s5, s6, mat, mf, mt) > 0);
}

double *CEndfIO::ReadList(double &c1, double &c2, int &l1, int &l2, int &n1, int &n2,
                        int &mat, int &mf, int &mt)
{
  if (!Read()) return false;
  c1 = _ReadDouble(m_szLine, 0, 11);
  c2 = _ReadDouble(m_szLine, 11, 11);
  l1 = ReadInt(m_szLine, 22, 11);
  l2 = ReadInt(m_szLine, 33, 11);
  n1 = ReadInt(m_szLine, 44, 11);
  n2 = ReadInt(m_szLine, 55, 11);
  mat = ReadInt(m_szLine, 66, 4);
  mf = ReadInt(m_szLine, 70, 2);
  mt = ReadInt(m_szLine, 72, 3);
  int n = 0;
  double *p = new double[n1];
  if (!p) return NULL;
  for (int i=0;i<(n1+5)/6;i++) {
    if (!Read()) {
      delete[] p;
      return NULL;
    }
    for (int j=0;j<6;j++) {
      p[n++] = _ReadDouble(m_szLine, j*11, 11);
      if (n == n1) return p;
    }
  }
  return p;
}

bool CEndfIO::WriteList(double c1, double c2, int l1, int l2, int n1, int n2,
                        int mat, int mf, int mt, double *data1, double *data2)
{
  if (m_fpEndf == NULL) return false;
  if (data1 == NULL) return false;
  char s1[15], s2[15];
  Floating2String(c1, 10, s1);
  Floating2String(c2, 10, s2);
  if (fprintf(m_fpEndf, "%11s%11s%11d%11d%11d%11d%4d%2d%3d\n", s1, s2, l1, l2, n1, n2, mat, mf, mt) <= 0)
    return false;
  int i, n = 0;
  if (data2 != NULL) {
    for (i=0;i<n2;i++) {
      Floating2String(data2[i], 10, s1);
      fprintf(m_fpEndf, "%11s", s1);
      if (++n%6 == 0) fprintf(m_fpEndf, "%4d%2d%3d\n", mat, mf, mt);
    }
    n1 -= n2;
  }
  for (i=0;i<n1;i++) {
    Floating2String(data1[i], 10, s1);
    fprintf(m_fpEndf, "%11s", s1);
    if (++n%6 == 0) fprintf(m_fpEndf, "%4d%2d%3d\n", mat, mf, mt);
  }
  if (n%6 != 0) {
    s1[0] = 0;
    while ((n++)%6 != 0) fprintf(m_fpEndf, "%11s", s1);
    fprintf(m_fpEndf, "%4d%2d%3d\n", mat, mf, mt);
  }
}

bool CEndfIO::WriteTab1(double c1, double c2, int l1, int l2, int n1, int n2,
                        int mat, int mf, int mt, int *data1, double *data2)
{
  if (m_fpEndf == NULL) return false;
  if (data1 == NULL || data2 == NULL) return false;
  char s1[15], s2[15];
  Floating2String(c1, 10, s1);
  Floating2String(c2, 10, s2);
  if (fprintf(m_fpEndf, "%11s%11s%11d%11d%11d%11d%4d%2d%3d\n", s1, s2, l1, l2, n1, n2, mat, mf, mt) <= 0)
    return false;
  int i;
  for (i=0;i<n1;i++) {
    fprintf(m_fpEndf, "%11d%11d", data1[2*i], data1[2*i+1]);
    if ((i+1)%3 == 0) fprintf(m_fpEndf, "%4d%2d%3d\n", mat, mf, mt);
  }
  if (i%3 != 0) {
    for (i=3-i%3;i>0;i--) fprintf(m_fpEndf, "%11s%11s", "", "");
    fprintf(m_fpEndf, "%4d%2d%3d\n", mat, mf, mt);
  }
  for (i=0;i<n2;i++) {
    Floating2String(data2[2*i], 10, s1);
    Floating2String(data2[2*i+1], 10, s2);
    fprintf(m_fpEndf, "%11s%11s", s1, s2);
    if ((i+1)%3 == 0) fprintf(m_fpEndf, "%4d%2d%3d\n", mat, mf, mt);
  }
  if (i%3 != 0) {
    for (i=3-i%3;i>0;i--) fprintf(m_fpEndf, "%11s%11s", "", "");
    fprintf(m_fpEndf, "%4d%2d%3d\n", mat, mf, mt);
  }
}

