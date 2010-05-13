/***********************************************************************
 *
 * Filename: basedata.h
 * Purpose : declares the basic data structures
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_BASEDATA_H_)
#define _BASEDATA_H_

struct RESDATA
{
  double E, dE;                 // resonance energy and its uncertainty
  double J;                     // spin
  double nJ;                    // newly assigned spin using pdf
  int l;                        // angular momentum
  double gGn;			// g times Gn
  double Gn, dGn;               // neutron width and its uncertainty multiplied by g factor
  double Gg, dGg;               // gamma width and its uncertainty
  double Gf, dGf;               // fission width and its uncertainty
  double area, darea;           // Gn*Gg/(Gn+Gg);
  double farea, dfarea;         // 
  char nflag;                   // neutron width flag
};

#endif
