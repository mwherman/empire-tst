/***********************************************************************
 *
 * Filename: link.cpp
 * Purpose : construct and manipulate doubly-linked list
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#include <stdio.h>
#include "link.h"

CLink::CLink()
{
  m_nCount = 0;
  m_pHead = NULL;
  m_pIndex = NULL;
  m_pArray = NULL;
}

CLink::~CLink()
{
  LINK *pNext;
  LINK *p = m_pHead;
  while (p) {
    pNext = p->next;
    delete p;
    p = pNext;
  }
  if (m_pArray) delete[] m_pArray;
}

void CLink::Add(double f)
{
  LINK *pNew;
  LINK *p = m_pHead;
  if (p == NULL || p->value > f) {
    pNew = new LINK;
    pNew->value = f;
    pNew->prev = NULL;
    pNew->next = p;
    if (pNew->next) pNew->next->prev = pNew;
    m_pHead = pNew;
    ++m_nCount;
  } else {
    while (p) {
      if (p->value == f) break;
      if (p->next == NULL || p->next->value > f) {
        pNew = new LINK;
        pNew->value = f;
        pNew->prev = p;
        pNew->next = p->next;
        if (pNew->next) pNew->next->prev = pNew;
        p->next = pNew;
        ++m_nCount;
        break;
      }
      p = p->next;
    }
  }
}

double *CLink::GetArray(int &n)
{
  if (m_pHead == NULL) {
    n = 0;
    return NULL;
  }
  if (m_pArray == NULL) {
    m_pArray = new double[m_nCount];
    LINK *p = m_pHead;
    for (int i=0; i<m_nCount; i++) {
      m_pArray[i] = p->value;
      p = p->next;
    }
  }
  n = m_nCount;
  return m_pArray;
}

double CLink::GoFirst()
{
  m_pIndex = m_pHead;
  if (m_pIndex) return m_pIndex->value;
  else return 0;
}

// GetFirst should be called first.
// The null value of m_pIndex is not checked for speed-up
double CLink::GoNext()
{
  m_pIndex = m_pIndex->next;
  if (m_pIndex) return m_pIndex->value;
  else return 0;
}

double CLink::LocateValue(double f)
{
  if (m_pHead == NULL) return 0.0;
  if (m_pIndex == NULL) m_pIndex = m_pHead;
  if (f <= m_pIndex->value) {
     while (m_pIndex->prev != NULL && f <= m_pIndex->prev->value) m_pIndex = m_pIndex->prev;
  } else {
     while (m_pIndex->next != NULL && f > m_pIndex->value) m_pIndex = m_pIndex->next;
  }
  if (m_pIndex == NULL) return 0.0;
  else return m_pIndex->value;
}

double CLink::GetCurrent()
{
  if (m_pIndex) return m_pIndex->value;
  else return 0;
}
