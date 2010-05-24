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

#define PI              3.14159

#define SCAT            0
#define CAPT            1
#define FISS            2

class CKernel : public CAtlas
{
public:
  CKernel();
  ~CKernel();
  void SetR(double R, double dR);
  void SetCorr(double fNN, double fGG, double fRP, double fNG=.0, double fNGS=.0);
  void SetRange(double fMin, double fMax);
  void SetDefaultScatUnc(double f);
  void SetGammaFactor(double f);
  double GetPotentialXS(double g1, double g2);
  double GetPotentialUnc(double g1, double g2);
  bool GetXSnUNC(int nth, int nReaction, int nGroup, double *pGroup, int &nFirstResGroup, double *pXS, double *pUN, bool bUseArea=false);

protected:
  void InitPotential(int nGroup, double *pGroup);
  double GetScatXS(int n, double g1, double g2);
  double GetCaptXS(int n, double g1, double g2, bool usearea = false);
  double GetFissXS(int n, double g1, double g2, bool usearea = false);

  double *m_pPotXS;
  double *m_pPotUN;
  double m_fCorrNN;
  double m_fCorrGG;
  double m_fCorrRP;
  double m_fCorrNG;
  double m_fCorrNGS;
  double m_fR;
  double m_fdR;
  double m_fMin;
  double m_fMax;
  double m_fDefaultScatUnc;
  double m_fGammaFactor;
};

#endif
