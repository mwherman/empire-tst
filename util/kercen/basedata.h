/***********************************************************************
 *
 * Filename: basedata.h
 * Purpose : declares the basic data structures
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_BASEDATA_H_)
#define _BASEDATA_H_

struct RESDATA
{
  double E, dE;                 // resonance energy and its uncertainty
  double J;                     // spin
  double nJ;                    // newly assigned spin using pdf
  double g;			// statistical weight factor
  int l;                        // angular momentum
  double G, dG;			// total width = Gg + Gn + Gf, and unc
  double gGn, dgGn;		// g times Gn, uncert
  double Gn, dGn;               // neutron width and its uncertainty multiplied by g factor
  double Gg, dGg;               // gamma width and its uncertainty
  double Gf, dGf;               // fission width and its uncertainty
  double area, darea;           // gGn*Gg/(Gn+Gg)
  double farea, dfarea;         // fission area
  char nflag;                   // neutron width flag - 2nd char of Gn COD
};

#endif
