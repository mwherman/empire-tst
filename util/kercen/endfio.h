/***********************************************************************
 *
 * Filename: endfio.h
 * Purpose : declares the basic functions for handling ENDF file
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_ENDFIO_H_)
#define _ENDFIO_H_

#include <stdio.h>

class CEndfIO
{
public:
  CEndfIO();
  ~CEndfIO();
  bool Open(char *fname, char *mode = NULL);
  void Close();
  void Unread();
  bool Read();
  bool ReadLine(int &mat, int &mf, int &mt);
  bool ReadText(char *s, int &mat, int &mf, int &mt);
  bool WriteText(char *s, int mat, int mf, int mt);
  bool ReadCont(double &c1, double &c2, int &l1, int &l2, int &n1, int &n2, int &mat, int &mf, int &mt);
  bool WriteCont(double c1, double c2, int l1, int l2, int n1, int n2, int mat, int mf, int mt);
  bool WriteListItem(double b1, double b2, double b3, double b4, double b5, double b6, int mat, int mf, int mt);
  double *ReadList(double &c1, double &c2, int &l1, int &l2, int &n1, int &n2, int &mat, int &mf, int &mt);
  bool WriteList(double c1, double c2, int l1, int l2, int n1, int n2, int mat, int mf, int mt,
                 double *data1, double *data2 = NULL);
  bool WriteTab1(double c1, double c2, int l1, int l2, int n1, int n2, int mat, int mf, int mt, int *data1, double *data2);

protected:
  FILE *m_fpEndf;
  char m_szLine[1024];
  bool m_bCached;
};

#endif
