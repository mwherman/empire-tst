Ccc   * $Author: Capote $ 
Ccc   * $Date: 2008-09-13 16:17:05 $
Ccc   * $Id: ripl2empire.h,v 1.5 2008-09-13 16:17:05 Capote Exp $
            

      INTEGER NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
C
C-----Common blocks and declarations from omretrieve.f (RIPL)
C
C-----Parameter statement
C
C-----Parameter statement
C     RCN, 08/2004, to handle new extension to the OMP RIPL-2 format
C     PARAMETER(NDIM1 = 10, NDIM2 = 13, NDIM3 = 24, NDIM4 = 30,
C    &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120)
C     RCN, 08/2008, to handle new extension to the OMP RIPL-3 format
C
      PARAMETER(NDIM1 = 10, NDIM2 = 13, NDIM3 = 25, NDIM4 = 40,
     &          NDIM5 = 10, NDIM6 = 10, NDIM7 = 120, NDIM8 = 50)

      CHARACTER*1 AUThor, REFer, SUMmary
      INTEGER IREf, IZMin, IZMax, IAMin, IAMax, IMOdel, JRAnge, NCOll,
     &        NVIb, NISotop, IZ, IA, LMAx, IDEf, IZProj, IAProj, IREl,
     &        IDR, IPArv, NPH, IPAr, JCOul
C
      REAL*8 EMin, EMax, EPOt, RCO, ACO, POT, BANdk, DEF, EXV, DEFv,
     &       THEtm, EX, SPIn, SPInv, ECoul,
     &       RCoul, RCOul0, BETa, RCOul1, RCOul2, ACOul, RCOul3
C
      REAL*8 SR_hw(ndim4),SR_amb0(ndim4),SR_amg0(ndim4),
     & SR_gam0(ndim4),SR_bet0(ndim4),SR_bet4(ndim4),
     & SR_bb42(ndim4),SR_gamg(ndim4),SR_delg(ndim4),
     & SR_bet3(ndim4),SR_et0(ndim4),SR_amu0(ndim4),
     & SR_hw0(ndim4),SR_bb32(ndim4),SR_gamde(ndim4),
     & SR_dpar(ndim4),SR_gshape(ndim4)

      INTEGER SR_ntu(ndim6,ndim4),SR_nnb(ndim6,ndim4),
     & SR_nng(ndim6,ndim4),SR_nno(ndim6,ndim4)
C
      REAL*8 ETA,ATAR,ZTAR,TARMAS,PROJMAS,
     &       HBARC,AMU0C2,EFErmi,RC,ENCOUL,ACOu
C
      COMMON /RIPLXX/ETA,ATAR,ZTAR,TARMAS,PROJMAS,
     &       HBARC,AMU0C2,EFErmi,RC,ENCOUL,ACOu
C
      COMMON /LIBDBL/ EMIn, EMAx, EPOt(6, NDIM1), RCO(6, NDIM1, NDIM2),
     &                ACO(6, NDIM1, NDIM2), POT(6, NDIM1, NDIM3),
     &                BANdk(NDIM4),	DEF(NDIM4, NDIM5), 
     &                EXV(NDIM7, NDIM4),  DEFv(NDIM7, NDIM4),
     &                THEtm(NDIM7, NDIM4), EX(NDIM6, NDIM4), 
     &                SPIn(NDIM6, NDIM4), SPInv(NDIM7, NDIM4),  
     &                RCOul(NDIM1), RCOul0(NDIM1), RCOul1(NDIM1),
     &                RCOul2(NDIM1), RCOul3(NDIM1),
     &                ACOul(NDIM1), ECOul(NDIM1), BETa(NDIM1)

	  COMMON /LIBSFT/ SR_hw, SR_amb0, SR_amg0, SR_gam0, SR_bet0,
     &                SR_bet4, SR_bb42, SR_gamg, SR_delg, SR_bet3,
     &                SR_et0, SR_amu0, SR_hw0, SR_bb32, SR_gamde,
     &                SR_dpar, SR_gshape

      COMMON /LIBCHA/AUThor(80), REFer(80), SUMmary(320)

	  COMMON /LIBINT/IREf,IZMin, IZMax, IAMin, IAMax, IMOdel, 
     &               JRAnge(6),NCOll(NDIM4), NVIb(NDIM4), NISotop,
     &               IZ(NDIM4), IA(NDIM4), LMAx(NDIM4), IDEf(NDIM4),
     &               IZProj, IAProj, IREl, IDR, IPArv(NDIM7, NDIM4),
     &               NPH(NDIM7, NDIM4), IPAr(NDIM6, NDIM4), JCOul,
     &               SR_ntu, SR_nnb, SR_nng, SR_nno


	 