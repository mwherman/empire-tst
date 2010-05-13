/***********************************************************************
 *
 * Filename: endf.h
 * Purpose : declares the extended functions for handling ENDF file
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_ENDF_H_)
#define _ENDF_H_

#include <stdio.h>
#include "endfio.h"
#include "basedata.h"

class CEndf : public CEndfIO
{
public:
  CEndf();
  ~CEndf();
  RESDATA *ReadResParms(int isotope, int &n);
  RESDATA *ReadResParmsUncertainty(int isotope, int &nRes);

protected:
};

#endif
