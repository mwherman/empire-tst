/***********************************************************************
 *
 * Filename: link.h
 * Purpose : declares functions for doubly-linked list
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_LINK_H_)
#define _LINK_H_

struct LINK
{
  double value;
  LINK *prev;
  LINK *next;
};

class CLink
{
public:
  CLink();
  ~CLink();
  void Add(double f);
  double *GetArray(int &n);
  double GoFirst();
  double GoNext();
  double GetCurrent();
  double LocateValue(double f);

protected:
  int m_nCount;
  LINK *m_pHead;
  LINK *m_pIndex;
  double *m_pArray;
};

#endif
