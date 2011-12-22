/***********************************************************************
 *
 * Filename: resxs.h
 * Purpose : declares functions to calculate the average c/s based on Atlas
 *
 * Written by Youngsik Cho (Inspired by Pavel Oblozinsky)
 * Modified by Samuel Hoblit
 *
 ***********************************************************************/

#if !defined(_RESXS_H_)
#define _RESXS_H_

#include "atlas.h"
#include "link.h"

#define TOTL            0
#define SCAT            1
#define CAPT            2
#define FISS            3

#define USEAREA		1
#define BOUND		2

class CResXS : public CAtlas
{
public:
  CResXS();
  ~CResXS();
  void SetR(double R, double dR);
  void SetCorr(double fNN, double fGG, double fRP, double fNG=.0, double fNGS=.0);
  void SetRange(double fMin, double fMax);
  void SetDefaultScatUnc(double f);
  void SetGammaFactor(double f);
  void GetPotentialXS(double , double& , double& );
  double GetPotentialXS(double e);
  double GetPotentialUN(double e);
  int GetFirstResGroup(int nGroup, double *pGroup);
  bool GetAvgResXS(int nReaction, int nGroup, double *pGroup, double *pXS, int nFlag);
  bool GetAbsResUN(int nReaction, int nGroup, double *pGroup, double *pUN, int nFlag);
  bool AddPotentialXS(int nFirstResGroup, int nGroup, double *pGroup, double *pXS);
  bool AddPotentialUN(int nFirstResGroup, int nGroup, double *pGroup, double *pUN);
  void GetSubGroupXSnUN(int nGroup, double *pGroup, int nSubGroup, double *pSubGroup, double *pXS, double *pUN, double *pNewUN, int *pIndex);
  void GetXS(double e, double *xs);
  void SetupEnergyGrid();
  void AddEnergyPoints(int n, double *e);
  void GetAvgXS(double E1, double E2, double *xs);
  void GetAvgXS(int nReaction, int nGroup, double *pGroup, double *pXS);

protected:
  void InitScat(int nGroup, double *pGroup, bool bBound);
  double GetScatXS(int n, double g1, double g2);
  double GetCaptXS(int n, double g1, double g2, bool usearea = false);
  double GetFissXS(int n, double g1, double g2, bool usearea = false);
  double GetScatXSFrom(double e0, double l, double g, double gn,double gg, double g1, double g2);
  void CalcFactor();

  bool m_bInitScat;
  double *m_pExtXS;
  double *m_pTmpUN;
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
  CLink link;
  double *m_pPf;	// penetration factors for resonance energies
  double *m_pSf;	// shift factors for resonance energies
};

#endif
