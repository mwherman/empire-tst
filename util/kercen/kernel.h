/***********************************************************************
 *
 * Filename: kernel.cpp
 * Purpose : declares functions to calculate the average c/s based on Atlas
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_KERNEL_H_)
#define _KERNEL_H_

#include "atlas.h"

class CKernel : public CAtlas
{
public:
  CKernel();
  ~CKernel();
  void SetR(double R);
  double GetScatXS(int n, double g1, double g2);
  double GetCaptXS(int n, double g1, double g2, bool usearea = false);
  double GetFissXS(int n, double g1, double g2, bool usearea = false);

protected:
};

#endif
