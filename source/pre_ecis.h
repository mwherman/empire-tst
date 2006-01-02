Ccc   * $Author: herman $ 
Ccc   * $Date: 2006-01-02 06:19:01 $
Ccc   * $Id: pre_ecis.h,v 1.2 2006-01-02 06:19:01 herman Exp $
            
c      ---------------------------------------------------------
c      | File with common blocks and declarations for PRE_ECIS  |
c      ---------------------------------------------------------
c
c *** Common blocks for ECIS95 input subroutine ***
c
      common /ecischar/ becis1,becis2,ecis1,ecis2
      character*50 becis1,becis2,ecis1,ecis2      
      common /ecisreal/ astep
      real*8 astep
      common /optcalc/flgrel,flgdom  
      character*1 flgdom,flgrel
      common /znome/nuc,parname,outfile
      character*2 nuc(103)
      character*8 parname(9)
      character*8 outfile  
