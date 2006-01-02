Ccc   * $Author: herman $ 
Ccc   * $Date: 2006-01-02 06:19:34 $
Ccc   * $Id: ripl2empire.h,v 1.4 2006-01-02 06:19:34 herman Exp $
            

      INTEGER NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
C
C-----Common blocks and declarations from omretrieve.f (RIPL)
C
C-----Parameter statement
C
C-----Parameter statement
C     RCN, 08/2004, to handle new extension to the OMP RIPL-2 format
      PARAMETER(NDIM1 = 10, NDIM2 = 13, NDIM3 = 24, NDIM4 = 30,
     &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120)
C
      CHARACTER*1 AUThor, REFer, SUMmary
      INTEGER IREf, IZMin, IZMax, IAMin, IAMax, IMOdel, JRAnge, NCOll,
     &        NVIb, NISotop, IZ, IA, LMAx, IDEf, IZProj, IAProj, IREl,
     &        IDR, IPArv, NPH, IPAr, JCOul
C
      REAL*4 EMin, EMax, EPOt, RCO, ACO, POT, BANdk, DEF, EXV, DEFv,
     &       THEtm, BETa0, GAMma0, XMUbeta, EX, SPIn, SPInv, ECoul,
     &       RCoul, RCOul0, BETa, RCOul1, RCOul2, ACOul
C
         REAL*8 ETA,ATAR,ZTAR,TARMAS,PROJMAS,
     &       HBARC,AMU0C2,EFErmi,RC,ENCOUL,ACOu
C
         COMMON /RIPLXX/ETA,ATAR,ZTAR,TARMAS,PROJMAS,
     &       HBARC,AMU0C2,EFErmi,RC,ENCOUL,ACOu
C
      COMMON /LIB   / IREf, EMIn,
     &                EMAx, IZMin, IZMax, IAMin, IAMax, IMOdel,
     &                JRAnge(6), EPOt(6, NDIM1), RCO(6, NDIM1, NDIM2),
     &                ACO(6, NDIM1, NDIM2), POT(6, NDIM1, NDIM3),
     &                NCOll(NDIM4), NVIb(NDIM4), NISotop, IZ(NDIM4),
     &                IA(NDIM4), LMAx(NDIM4), BANdk(NDIM4),
     &                DEF(NDIM4, NDIM5), IDEf(NDIM4), IZProj, IAProj,
     &                EXV(NDIM7, NDIM4), IPArv(NDIM7, NDIM4), IREl, IDR,
     &                NPH(NDIM7, NDIM4), DEFv(NDIM7, NDIM4),
     &                THEtm(NDIM7, NDIM4), BETa0(NDIM4), GAMma0(NDIM4),
     &                XMUbeta(NDIM4), EX(NDIM6, NDIM4),
     &                SPIn(NDIM6, NDIM4), IPAr(NDIM6, NDIM4),
     &                SPInv(NDIM7, NDIM4), JCOul, ECOul(NDIM1),
     &                RCOul(NDIM1), RCOul0(NDIM1), BETa(NDIM1),
     &                RCOul1(NDIM1), RCOul2(NDIM1), ACOul(NDIM1),
     &                AUThor(80), REFer(80), SUMmary(320)

