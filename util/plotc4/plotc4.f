C     PROGRAM PLOTC4(INPUT,OUTPUT,TAPE4=INPUT,TAPE6=OUTPUT,             PLO00010
C    1 TAPE10,TAPE11,TAPE12,TAPE14,TAPE15,TAPE16,TAPE17)                PLO00020
C                                                                       PLO00030
C     PROGRAM PLOTC4                                                    PLO00040
C     VERSION 86-1 (AUGUST 1986)                                        PLO00050
C     VERSION 87-1 (JUNE 1987)   *SOFTWARE UPPER AND LOWER CASE         PLO00060
C                                 CHARACTERS                            PLO00070
C     VERSION 00-1 (NOV. 2000)   *UPGRADE TO COMPARE MEASURED
C                                 DIFFERENTIAL DATA WITH CURVES FROM
C                                 EVALUATED DATA FILES.
C                                *MODIFICATIONS FOR COMPATIBILITY WITH
C                                 CHARACTER PLOTTING PACKAGE OF THE
C                                 PRE-PRO CODES.
C                                                                       PLO00080
C     OWNED, MAINTAINED AND DISTRIBUTED BY
C     ------------------------------------
C     THE NUCLEAR DATA SECTION
C     INTERNATIONAL ATOMIC ENERGY AGENCY
C     P.O. BOX 100
C     A-1400, VIENNA, AUSTRIA
C     EUROPE
C
C     ORIGINALLY WRITTEN BY
C     ------------------------------------
C     DERMOTT E. CULLEN
C     CURRENT ADDRESS
C     UNIVERSITY OF CALIFORNIA
C     LAWRENCE LIVERMORE NATIONAL LABORATORY
C     L-86
C     P.O. BOX 808
C     LIVERMORE, CA 94550
C     U.S.A.
C     TELEPHONE  925-423-7359
C     E. MAIL    CULLEN1@LLNL.GOV
C     WEBSITE    HTTP://REDDOG1.LLNL.GOV
C
C     VERSION 00-1 IMPLEMENTED BY
C     ANDREJ TRKOV
C     THE NUCLEAR DATA SECTION
C     INTERNATIONAL ATOMIC ENERGY AGENCY
C     P.O. BOX 100
C     A-1400, VIENNA, AUSTRIA
C     EUROPE
C                                                                       PLO00150
C     PURPOSE                                                           PLO00160
C     =======                                                           PLO00170
C     PLOT EVALUATED DATA FROM THE ENDF/B FORMAT AND/OR EXPERIMENTAL    PLO00180
C     DATA WHICH IS IN A COMPUTATION FORMAT.                            PLO00190
C                                                                       PLO00200
C     THIS PROGRAM HAS BEEN DESIGNED TO USE THE COMPUTATION FORMAT      PLO00210
C     AND CONVENTIONS WHICH ARE USED BY PROGRAM X4TOC4 TO TRANSLATE     PLO00220
C     EXPERIMENTALLY MEASURED DATA FROM THE EXFOR TO A COMPUTATION      PLO00230
C     FORMAT.                                                           PLO00240
C                                                                       PLO00250
C     GRAPHICS INTERFACE                                                PLO00260
C     ==================================================================PLO00270
C     THIS PROGRAM USES A SIMPLE CALCOMP LIKE GRAPHICS INTERFACE WHICH  PLO00280
C     REQUIRES ONLY 3 SUBROUTINES...PLOTS, PLOT AND PEN (DESCRIBED IN   PLO00290
C     DETAIL BELOW). ALL CHARACTERS AND SYMBOLS ARE DRAWN USING TABLES  PLO00300
C     OF PEN STROKES (SUPPLIED WITH THIS PROGRAM). USING THIS METHOD    PLO00310
C     THE PROGRAM SHOULD BE SIMPLE TO INTERFACE TO VIRTUALLY ANY PLOTTERPLO00320
C     OR GRAPHICS TERMINAL AND THE APPEARANCE AND LAYOUT OF THE PLOTS   PLO00330
C     SHOULD BE INDEPENDENT OF WHICH PLOTTER IS USED.                   PLO00340
C                                                                       PLO00350
C     WHAT CAN BE PLOTTED                                               PLO00360
C     ===================                                               PLO00370
C     AT THE PRESENT TIME THE FOLLOWING TYPES OF DATA CAN BE PLOTTED,   PLO00380
C                                                                       PLO00390
C     (1) MF =   3 - CROSS SECTIONS. ENDF/B AND EXFOR DATA CAN BE       PLO00400
C                    COMPARED (ONLY MF = 3 ENDF/B DATA CAN BE COMPARED).PLO00410
C     (2) MF =   4 - ANGULAR DISTRIBUTIONS (ONLY EXFOR)                 PLO00420
C     (3) MF =   5 - ENERGY DISTRIBUTIONS (ONLY EXFOR)                  PLO00430
C     (4) MF =   6 - DOUBLE DIFFERENTIAL (ONLY EXFOR)                   PLO00440
C     (5) MF = 154 - LEGENDRE COEFFICENTS (ONLY EXFOR)                  PLO00450
C     (6) MF = 203 - CROSS SECTION RATIOS (ONLY EXFOR)                  PLO00460
C     (7) MF = 402 - RESONANCE PARAMETERS (ONLY EXFOR)                  PLO00470
C     (8) MF = 801 - FISSION YIELD DATA (ONLY EXFOR)                    PLO00480
C                                                                       PLO00490
C     ALL OTHER DATA WILL BE SKIPPED                                    PLO00500
C                                                                       PLO00510
C     WHAT DATA WILL BE PLOTTED                                         PLO00520
C     =========================                                         PLO00530
C     BASED ON INPUT OPTIONS THE USER MAY SPECIFY WHETHER THE PROGRAM   PLO00540
C     SHOULD PLOT ONLY EXFOR DATA OR EXFOR AND ENDF/B DATA. IN ADDITION PLO00550
C     THE USER MAY SPECIFY UP TO 100 ZA/MF/MT RANGES TO SELECT DATA.    PLO00560
C                                                                       PLO00570
C     HOW MUCH DATA CAN BE PLOTTED                                      PLO00580
C     ============================                                      PLO00590
C     ENDF/B DATA                                                       PLO00600
C     -----------                                                       PLO00610
C     EACH SECTION OF ENDF/B CROSS SECTIONS (MF =3, ANY MT) MAY CONTAIN PLO00620
C     ANY NUMBER OF DATA POINTS. IF A SECTION CONTAINS 9000 OR FEWER    PLO00630
C     POINTS ALL OF THE DATA WILL BE IN CORE. IF THE SECTION CONTAINS   PLO00640
C     MORE THAN 9000 POINTS THE DATA WILL BE WRITTEN TO A SCRATCH FILE  PLO00650
C     AND READ AS NEEDED.                                               PLO00660
C...  LIMIT INCREASED TO 90000 POINTS BY TRKOV.
C                                                                       PLO00670
C     EXFOR DATA                                                        PLO00680
C     ----------                                                        PLO00690
C     BASED ON INPUT PARAMETERS THE USER MAY CONTROL HOW MUCH EXFOR DATAPLO00700
C     WILL APPEAR ON EACH PLOT. THIS PROGRAM ALLOWS THE USER TO SPECIFY PLO00710
C     THAT UP TO 1000 DATA POINTS MAY APPEAR ON EACH PLOT. IF THERE ARE PLO00720
C     MORE PHYSICALLY COMPARABLE POINTS (E.G., SAME ZA, MF, MT) THAN    PLO00730
C     SPECIFIED BY THE USER THE PROGRAM WILL CREATE A SERIES OF PLOTS   PLO00740
C     EACH CONTAINING NOT MORE THAN THE MAXIMUM NUMBER OF POINTS PER    PLO00750
C     PLOT SPECIFIED BY THE USER.                                       PLO00760
C...  MAXIMUM NUMBER OF POINTS INCREASED TO 10000 BY TRKOV
C                                                                       PLO00770
C     WHAT COMPUTERS WILL THE PROGRAM RUN ON                            PLO00780
C     ======================================                            PLO00790
C     THE PROGRAM HAS BEEN IMPLEMENTED ON AN IBM MAINFRAME AND AN IBM   PLO00800
C     PERSONAL COMPUTER. THE PROGRAM IS SMALL ENOUGH TO RUN ON VIRTUALLYPLO00810
C     ANY COMPUTER. FOR SPECIAL CONSIDERATIONS SEE THE SECTIONS BELOW ONPLO00820
C     (1) COMPUTER DEPENDENT CODING                                     PLO00830
C     (2) PLOTTER INTERFACE                                             PLO00840
C                                                                       PLO00850
C     THE PROGRAM CAN DETERMINE WHETHER IT IS RUNNING ON A MAINFRAME OR PLO00860
C     AN IBM-PC. THE VARIABLE IBMPC IS SET TO -1 IN MAIN. IF RUNNING ON PLO00870
C     A MAINFRAME ITS VALUE WILL WILL NOT CHANGE. IF RUNNING ON AN      PLO00880
C     IBM-PC THE PLOTTER INTERFACE ROUTINE PLOTS WILL RESET IBMPC TO,   PLO00890
C                                                                       PLO00900
C     IBMPC = 0 - IBM-PC FULL SIZE PLOTS                                PLO00910
C     IBMPC = 1 - IBM-PC HALF SIZE PLOTS                                PLO00920
C                                                                       PLO00930
C     DUE TO SOME LIMITATIONS OF THE MINIMUM CHARACTER SIZE THAT CAN BE PLO00940
C     PLOTTED ON A HEWLETT-PACKARD PLOTTER IT IS IMPORTANT THAT THE     PLO00950
C     PROGRAM KNOW WHETHER IT IS RUNNING ON A MAINFRAME OR AN IBM-PC.   PLO00960
C                                                                       PLO00970
C     COMPUTATION FORMAT                                                PLO00980
C     ==================                                                PLO00990
C     THE COMPUTATION FORMAT USES A CLASSIFICATION SYSTEM AND UNITS     PLO01000
C     WHICH ARE COMPATIBLE WITH ENDF/B. DATA IS CLASSIFIED BY (1) ZA    PLO01010
C     OF PROJECTILE, (2) ZA OF TARGET, (3) METASTABLE STATE OF TARGET,  PLO01020
C     (4) MF - TYPE OF DATA, (5) MT - REACTION, (6) METASTABLE STATE    PLO01030
C     OF RESIDUAL NUCLEUS. TO IDENTIFY THE SOURCE OF THE DATA THE FIRST PLO01040
C     AUTHOR AND YEAR AND THE EXFOR ACCESSION AND SUB-ACCESSION NUMBER  PLO01050
C     ARE INCLUDED IN THE FORMAT. IN ADDITION FIELDS ARE ASSIGNED TO    PLO01060
C     DEFINE THE STATUS OF THE EXFOR DATA (E.G., S = SUPERCEDED),       PLO01070
C     WHETHER DATA IS IN THE LABORATORY OR CENTER-OF-MASS FRAME OF      PLO01080
C     REFERENCE AND THE PHYSICAL SIGNIFICANCE OF THE LAST 2 OUTPUT      PLO01090
C     FIELDS (LVL = LEVEL ENERGY, HL = HALF-LIFE). FINALLY THE FORMAT   PLO01100
C     INCLUDES 8 FIELDS IN WHICH THE OUTPUT DATA ARE CONTAINED (E.G.,   PLO01110
C     INCIDENT ENERGY, DATA, COSINE, UNCERTAINTIES, ETC.)               PLO01120
C                                                                       PLO01130
C     COLUMNS   DESCRIPTION                                             PLO01140
C     -------   -----------                                             PLO01150
C       1-  5   PROJECTILE ZA (E.G. NEUTRON =1, PROTON =1001)           PLO01160
C       6- 11   TARGET ZA (E.G. 26-FE-56 =  26056)                      PLO01170
C          12   TARGET METASTABLE STATE (E.G. 26-FE-56-M = M)           PLO01180
C      13- 15   MF (ENDF/B CONVENTIONS, PLUS ADDITIONS).                PLO01190
C      16- 19   MT (ENDF/B CONVENTIONS, PLUS ADDITIONS).                PLO01200
C          20   PRODUCT METASTABLE STATE (E.G. 26-FE-56-M = M)          PLO01210
C          21   EXFOR STATUS                                            PLO01220
C          22   CENTER-OF-MASS FLAG (C=CENTER-OF-MASS, BLANK=LAB)       PLO01230
C      23- 94   8 DATA FIELDS (EACH IN E9.3 FORMAT). DEFINED BELOW.     PLO01240
C      95- 97   IDENTIFICATION OF DATA FIELDS 7 AND 8                   PLO01250
C               (E.G., LVL=LEVEL, HL=HALF-LIFE.ETC.)                    PLO01260
C      98-122   REFERENCE (FIRST AUTHOR AND YEAR)                       PLO01270
C     123-127   EXFOR ACCESSION NUMBER                                  PLO01280
C     128-130   EXFOR SUB-ACCESSION NUMBER                              PLO01290
C         131   MULTI-DIMENSION TABLE FLAG                              PLO01300
C                                                                       PLO01310
C     DEFINITION OF 8 COMPUTATION FORMAT DATA FIELDS                    PLO01320
C     ==============================================                    PLO01330
C     IN ORDER TO PLOT DATA THIS PROGRAM ASSUMES THAT THE FOLLOWING     PLO01340
C     CONVENTIONS HAVE BEEN USED FOR DATA IN THE COMPUTATION FORMAT.    PLO01350
C                                                                       PLO01360
C     DATA FIELD   DEFINITION                                           PLO01370
C     ----------   ----------                                           PLO01380
C       1          PROJECTILE INCIDENT ENERGY                           PLO01390
C       2          PROJECTILE INCIDENT ENERGY UNCERTAINTY               PLO01400
C       3          DATA, E.G., CROSS SECTION, ANGULAR DISTRIBUTION, ETC.PLO01410
C       4          DATA UNCERTAINTY                                     PLO01420
C       5          COSINE OR LEGENDRE ORDER                             PLO01430
C       6          COSINE UNCERTAINTY                                   PLO01440
C       7          IDENTIFIED BY COLUMNS 95-97 (E.G.,LEVEL E, HALF-LIFE)PLO01450
C       8          IDENTIFIED BY COLUMNS 95-97 (E.G.,LEVEL E UNCERTANTY)PLO01460
C                                                                       PLO01470
C     THE PHYSICAL SIGNIFICANCE OF THE FIRST 6 DATA FIELDS IS DEFINED BYPLO01480
C     THE MF (DATA TYPE). THE PHYSICAL SIGNIFICANCE OF FIELDS 7 AND 8   PLO01490
C     ARE DEFINED BY COLUMNS 95 THROUGH 97 (E.G. LVL = LEVEL ENERGY AND PLO01500
C     ITS UNCERTAINTY).                                                 PLO01510
C                                                                       PLO01520
C     SPECIAL CONVENTIONS                                               PLO01530
C     ===================                                               PLO01540
C     THE ABOVE CONVENTIONS ARE APPROPRIATE FOR MOST TYPES OF DATA      PLO01550
C     IN THE ENDF/B SYSTEM. IN ORDER TO ALLOW THIS PROGRAM TO PLOT      PLO01560
C     ADDITIONAL TYPES OF DATA THE FOLLOWING SPECIAL CONVENTIONS HAVE   PLO01570
C     BEEN ADOPTED,                                                     PLO01580
C                                                                       PLO01590
C     CROSS SECTION RATIOS  - FIELD 5 = MT OF DENOMINATOR.              PLO01600
C     (MF = 203)              FIELD 6 = ZA OF DENOMINATOR.              PLO01610
C     FISSION YIELD DATA    - FIELD 5 = ZA OF FISSION FRAGMENT          PLO01620
C     (MF = 801)              FIELD 6 = MASS OF FISSION FRAGMENT        PLO01630
C     PRODUCTION            - FIELD 6 = ZA OF PRODUCT                   PLO01640
C     (MT = 9000-9999)                                                  PLO01650
C                                                                       PLO01660
C     SEE, REMARKS BELOW ON METASTABLE STATE FLAGS.                     PLO01670
C                                                                       PLO01680
C     BLANK VS. ZERO DATA FIELDS                                        PLO01690
C     ==========================                                        PLO01700
C     THE 8 DATA FIELDS ON EACH COMPUTATION FORMAT LINE ARE READ AS     PLO01710
C     CHARACTERS AND INTERNALLY CONVERTED TO FLOATING POINT NUMBERS     PLO01720
C     (SEE, SUBROUTINE FLOAT9). BY TESTING BEFORE CONVERTING THIS       PLO01730
C     PROGRAM CAN DETERMINE WHETHER ANY GIVEN FIELD IS BLANK (NO DATA   PLO01740
C     GIVEN) AS OPPOSED TO ZERO. IT IS OFTEN IMPORTANT TO MAKE THIS     PLO01750
C     DISTINCTION, PARTICULARLY FOR FIELDS 7 AND 8, E.G. THE DIFFERENCE PLO01760
C     BETWEEN 0.0 INDICATING GROUND STATE AS OPPOSED TO NO DATA GIVEN.  PLO01770
C     THE EXFOR TO COMPUTATION FORMAT CONVERSION PROGRAM X4TOC4 ALSO    PLO01780
C     MAKES THIS DISTINCTION AND LEAVES UNDEFINED FIELDS BLANK (NOT     PLO01790
C     ZERO). THEREFORE, ANY DATA CONVERTED TO THE COMPUTATION FORMAT    PLO01800
C     FORMAT USING PROGRAM X4TOC4 WILL FOLLOW THE CORRECT CONVENTIONS.  PLO01810
C                                                                       PLO01820
C     HOWEVER, IF THE USER OF THIS PROGRAM DIRECTLY CODES DATA IN THE   PLO01830
C     COMPUTATION FORMAT IT IS IMPORTANT TO MAINTAIN THIS CONVENTION.   PLO01840
C     REMEMBER---ANY UNDEFINED FIELDS SHOULD BE LEFT BLANK AND NOT SET  PLO01850
C     TO ZERO.                                                          PLO01860
C                                                                       PLO01870
C     COMPUTATION FORMAT UNITS                                          PLO01880
C     ========================                                          PLO01890
C     IN ORDER TO PLOT DATA THIS PROGRAM ASSUMES THAT THE FOLLOWING     PLO01900
C     UNITS HAVE BEEN USED FOR DATA IN THE COMPUTATION FORMAT.          PLO01910
C                                                                       PLO01920
C     EV         = ENERGY                                               PLO01930
C     BARNS      = CROSS SECTION                                        PLO01940
C     STERADIANS = SOLID ANGLE                                          PLO01950
C     SECONDS    = TIME                                                 PLO01960
C     KELVIN     = TEMPERATURE                                          PLO01970
C                                                                       PLO01980
C     FOR EXAMPLE DOUBLE DIFFERENTIAL DATA (MF=6) WILL BE IN,           PLO01990
C                                                                       PLO02000
C     BARNS/EV/STERADIAN                                                PLO02010
C                                                                       PLO02020
C     METASTABLE STATE                                                  PLO02030
C     ================                                                  PLO02040
C     THE COMPUTATION FORMAT ALLOWS THE METASTABLE STATE OF THE TARGET  PLO02050
C     AND RESIDUAL NUCLEUS TO BE IDENTIFIED. FOR RATIO DATA METASTABLE  PLO02060
C     STATE OF BOTH NUMERATOR AND DENOMINATOR OF THE RATIO SHOULD BE    PLO02070
C     DEFINED.                                                          PLO02080
C                                                                       PLO02090
C     THE METASTABLE STATE OF THE TARGET IS IDENTIFIED IN COLUMN 12 AND PLO02100
C     THE METASTABLE STATE OF THE RESIDUAL NUCLUES IN COLUMN 20. FOR    PLO02110
C     RATIO DATA THE METASTABLE STATE OF THE DENOMINATOR TARGET AND     PLO02120
C     RESIDUAL NUCLEUS ARE IDENTIFIED BY HAVING THE DENOMINATOR ZA AND  PLO02130
C     MT IN THE FORM ZA.M AND MT.M (E.G., 26056.9 AND 102.1). COLUMNS   PLO02140
C     12 AND 20 MAY COTAIN CHARACTERS SUCH AS M, BUT TO MAINTAIN THE    PLO02150
C     EIGHT OUTPUT FIELDS IN STRICTLY NUMERICAL FORM THE DENOMINATOR    PLO02160
C     ZA.M AND MT.M WILL BE IN NUMERICAL FORM. THE POSSIBLE CHARACTERS  PLO02170
C     THAT MAY APPEAR IN COLUMNS 12 OR 20 AND THEIR NUMERICAL           PLO02180
C     EQUIVALENTS USED WITH RATIO DENOMINATOR ZA AND MT INCLUDE,        PLO02190
C                                                                       PLO02200
C     DEFINITION    COLUMN 12 OR 20     EQUIVALENT   PLOTTED AS         PLO02210
C     ==========    ===============     ==========   ==========         PLO02220
C     GROUND              G                0           -G               PLO02230
C     M1                  1                1           -M1              PLO02240
C     M2                  2                2           -M2              PLO02250
C     M3                  3                3           -M3              PLO02260
C     M4                  4                4           -M4              PLO02270
C     M5                  5                5           -M5              PLO02280
C     UNKNOWN             ?                6           -M?              PLO02290
C     M                   M                7           -M               PLO02300
C     MORE THAN 1         +                8           -M+              PLO02310
C     ALL OR TOTAL        T                9           BLANK            PLO02320
C     ALL OR TOTAL      BLANK              9           BLANK            PLO02330
C                                                                       PLO02340
C     BY CONVENTION IF AN EXFOR REACTION DOES NOT SPECIFY A METASTABLE  PLO02350
C     STATE THE STATE IS DEFINED IN THE COMPUTATION FORMAT TO BE..ALL.. PLO02360
C     (I.E., BLANK IN COLUMN 12 OR 20, 9 IN RATIO ZA OR MT).            PLO02370
C                                                                       PLO02380
C     FOR EXAMPLE, FOR A RATIO IF THE ZA.M AND MT.M ARE OUTPUT AS       PLO02390
C     26056.9 AND 102.1, RESPECTIVELY THE RATIO DENOMINATOR TARGET IS   PLO02400
C     26-FE-56 (ALL) AND THE REACTION IS CAPTURE (MT=102) LEAVING THE   PLO02410
C     RESIDUAL NUCLUES IN THE M1 STATE.                                 PLO02420
C                                                                       PLO02430
C     EXFOR STATUS                                                      PLO02440
C     ============                                                      PLO02450
C     COLUMN 21 OF EACH COMPUTATION FORMAT RECORD MAY CONTAIN BLANK     PLO02460
C     (STATUS NOT SPECIFIED) OR ONE TO THE FOLLOWING CHARACTERS,        PLO02470
C                                                                       PLO02480
C     COLUMN 21   DEFINITION                                            PLO02490
C     ---------   ----------                                            PLO02500
C        U        UNNORMALIZED (HAS PRIORITY OVER EXFOR STATUS AND IS   PLO02510
C                 USED TO INDICATE THAT THE DATA IS NOT IN STANDARD     PLO02520
C                 OUTPUT UNITS. Y AXIS LABEL WILL SAY..UNNORMALIZED..). PLO02530
C        A        APPROVED BY AUTHOR                                    PLO02540
C        C        CORRELATED                                            PLO02550
C        D        DEPENDENT                                             PLO02560
C        O        OUTDATED                                              PLO02570
C        P        PRELIMINARY                                           PLO02580
C        R        RENORMALIZED                                          PLO02590
C        S        SUPERCEDED                                            PLO02600
C                                                                       PLO02610
C     IF DATA HAS ANY OTHER EXFOR STATUS (E.G., TRANSLATED FROM SCISRS) PLO02620
C     THE STATUS FIELD WILL BE BLANK.                                   PLO02630
C                                                                       PLO02640
C     CONTROL OF PLOTTING                                               PLO02650
C     ===================                                               PLO02660
C     THE USER HAS CONTROL OVER HOW DATA IN THE COMPUTATION FORMAT      PLO02670
C     IS INTERPRETED BY THIS PROGRAM.                                   PLO02680
C                                                                       PLO02690
C     DATA ON EACH PLOT IS IDENTIFIED BY PLOTTING A CHARACTER EQUIVALENTPLO02700
C     OF TARGET ZA AND METASTABLE STATE (ZA), DATA TYPE (MF), REACTION  PLO02710
C     (MT) AND RESIDUAL METASTABLE STATE. THE ZA, MF AND MT MAY BE      PLO02720
C     INTERPRETED IN ANY MANNER THAT THE USER CHOOSES.                  PLO02730
C                                                                       PLO02740
C     THIS IS ACCOMPLISHED BY USING THREE DICTIONARIES WHICH CONTROL    PLO02750
C     THE PLOTTING. ALL THREE OF THESE DICTIONARIES ARE DISTRIBUTED     PLO02760
C     WITH THIS PROGRAM. EACH DICTIONARY IS A SIMPLE CARD IMAGE FILE    PLO02770
C     WHICH MAY BE MODIFIED BY THE USER AT ANY TIME TO MEET SPECIFIC    PLO02780
C     NEEDS. THE THREE DICTIONARIES ARE,                                PLO02790
C                                                                       PLO02800
C     (1) INTERPRETATION OF SPECIAL ZA                                  PLO02810
C         ----------------------------                                  PLO02820
C         FOR ALL TARGET OR RESIDUAL NUCLEI THIS PROGRAM WILL USE THE   PLO02830
C         ENDF/B CONVENTION OF ASSUMING ZA = 1000*Z + A. FOR SPECIAL    PLO02840
C         MATERIALS WHICH DO NOT EASILY FIT INTO THIS SCHEME (E.G.,     PLO02850
C         WATER) THE ENDF/B CONVENTION IS TO DEFINE Z =0 AND TO ASSIGN  PLO02860
C         A NUMERICAL EQUIVALENT FOR EACH SPECIAL MATERIAL. FOR NORMAL  PLO02870
C         MATERIALS THIS PROGRAM WILL USE ZA TO DEFINE THE MATERIAL OR  PLO02880
C         ISOTOPE. FOR SPECIAL MATERIAL (Z=0) THIS PROGRAM WILL USE THISPLO02890
C         DICTIONARY TO DEFINE THE MATERIAL. AS DISTRIBUTED THIS        PLO02900
C         DICTIONARY CONTAINS ALL OF THE SPECIAL MATERIALS DEFINED IN   PLO02910
C         THE ENDF/B SYSTEM. THE USER MAY CODE DATA FOR ANY SPECIAL     PLO02920
C         MATERIAL IN THE COMPUTATION FORMAT AND ASSIGN IT A SPECIAL    PLO02930
C         ZA. BY ADDING THE DEFINITION TO THIS DICTIONARY THE USER MAY  PLO02940
C         OBTAIN PLOTS ON WHICH THE SPECIAL MATERIAL IS PROPERLY        PLO02950
C         IDENTIFIED.                                                   PLO02960
C                                                                       PLO02970
C     (2) INTERPRETATION OF MF                                          PLO02980
C         --------------------                                          PLO02990
C         THIS DICTIONARY DEFINES THE TITLES THAT WILL APEEAR FOR EACH  PLO03000
C         MF READ FROM THE COMPUTATION FORMAT. IN ADDITION THIS         PLO03010
C         DICTIONARY ALLOWS THE USER TO SPECIFY DIFFERENT TITLES FOR    PLO03020
C         THE SAME MF AND DIFFERENT MT RANGES, E.G.,                    PLO03030
C                                                                       PLO03040
C         MF =3, MT =  251 - 253 = PARAMETERS (USED FOR MU, XI, GAMMA)  PLO03050
C         MF =3, MT = 9000       = NEUTRON INDUCED (USED FOR PRODUCTION)PLO03060
C         MF =3, MT = OTHER      = CROSS SECTION                        PLO03070
C                                                                       PLO03080
C         IF THE USER DOES NOT LIKE THE TITLES NORMALLY OUTPUT BY THIS  PLO03090
C         PROGRAM IT IS MERELY NECESSARY TO MODIFY THIS DICTIONARY.     PLO03100
C                                                                       PLO03110
C     (3) INTERPRETATION OF MT                                          PLO03120
C         --------------------                                          PLO03130
C         THIS DICTIONARY DEFINES THE TITLES THAT WILL APPEAR FOR EACH  PLO03140
C         MT READ FROM THE COMPUTATION FORMAT, E.G.,                    PLO03150
C                                                                       PLO03160
C         MT  =    1 = TOTAL                                            PLO03170
C             = 9000 = PRODUCTION                                       PLO03180
C                                                                       PLO03190
C         IF THE USER DOES NOT LIKE THE TITLES NORMALLY OUTPUT BY THIS  PLO03200
C         PROGRAM IT IS MERELY NECESSARY TO MODIFY THIS DICTIONARY.     PLO03210
C                                                                       PLO03220
C     USED IN COMBINATION THE TRANSLATION OF THE ZA, MF AND MT SERVE TO PLO03230
C     IDENTIFY THE DATA BEING PLOTTED. BY USING THE DICTIONARIES        PLO03240
C     DESCRIBED ABOVE THE USER HAS COMPLETE CONTROL OVER HOW ZA, MF AND PLO03250
C     MT ARE INTERPRETED AND AS SUCH MAY SELECT ANY FORM TO IDENTIFY    PLO03260
C     DATA.                                                             PLO03270
C                                                                       PLO03280
C     PROGRAM OPERATION                                                 PLO03290
C     =================                                                 PLO03300
C     EXFOR DATA INDEX TABLE                                            PLO03310
C     ----------------------                                            PLO03320
C     THE ENTIRE COMPUTATION FORMAT FILE WILL FIRST BE READ AND COMPAREDPLO03330
C     TO THE REQUESTED ZA/MF/MT RANGES SPECIFIED BY THE USER. IF NO     PLO03340
C     COMPARABLE DATA IS FOUND THE PROGRAM WILL TERMINATE EXECUTION. IF PLO03350
C     COMPARABLE DATA IS FOUND THE PROGRAM WILL CREATE AN INDEX TABLE   PLO03360
C     SPECIFYING (1) ZA, (2) MF, (3) MT, (4) STARTING RECORD NUMBER,    PLO03370
C     (5) ENDING RECORD NUMBER, (6) NUMBER OF DATA POINTS WITH THIS ZA, PLO03380
C     MF AND MT. DURING EXECUTION THIS INDEX TABLE WILL BE USED TO,     PLO03390
C     (1) SELECT THE NEXT ZA, MF, MT TO BE PLOTTED IF ONLY PLOTTING     PLO03400
C     EXFOR DATA, OR (2) TO DETERMINE WHETHER OR NOT THERE IS COMPARABLEPLO03410
C     EXFOR DATA (WITHOUT AGAIN SEARCHING THE EXFOR DATA FILE) WHEN     PLO03420
C     COMPARING EXFOR AND ENDF/B DATA. ONCE IT HAS BEEN DECIDED TO PLOT PLO03430
C     EXFOR DATA WHICH HAS A GIVEN ZA, MF AND MT THE STARTING RECORD    PLO03440
C     INDEX IS USED TO QUICKLY POSITION TO THE FIRST RECORD TO READ AND PLO03450
C     THE ENDING RECORD INDEX IS USED TO DEFINE WHEN TO STOP READING    PLO03460
C     (INSTEAD OF READING THE ENTIRE COMPUTATION FORMAT DATA FILE).     PLO03470
C                                                                       PLO03480
C     ONLY PLOTTING EXFOR DATA                                          PLO03490
C     ------------------------                                          PLO03500
C     THE PROGRAM WILL USE THE INDEX TABLE TO DEFINE THE ZA, MF AND MT  PLO03510
C     OF THE NEXT SET OF DATA TO PLOT. BASED ON USER INPUT THE PROGRAM  PLO03520
C     WILL THEN PLOT EITHER ONE REFERENCE (REFERENCE = AUTHOR, YEAR,    PLO03530
C     EXFOR ACCESSION AND SUB-ACCESSION NUMBER) PER PLOT OR ALL         PLO03540
C     COMPARABLE REFERENCES ON THE SAME PLOT. THE CYCLE OF READING DATA PLO03550
C     AND PRODUCING PLOTS WILL BE CONTINUED UNTIL ALL DATA DEFINED IN   PLO03560
C     THE INDEX TABLE HAS BEEN PLOTTED.                                 PLO03570
C                                                                       PLO03580
C     COMPARING ENDF/B AND EXFOR DATA                                   PLO03590
C     -------------------------------                                   PLO03600
C     IN THE COMPARISON MODE THE PROGRAM WILL ONLY PLOT DATA IF THERE   PLO03610
C     IS COMPARABLE DATA (SAME ZA, MF, MT) ON BOTH THE ENDF/B FORMATTED PLO03620
C     AND COMPUTATION FORMATTED FILES.                                  PLO03630
C                                                                       PLO03640
C     BASED ON THE PLOTTING REQUESTS (SEE BELOW) THE PROGRAM WILL FIRST PLO03650
C     SEARCH THE ENDF/B DATA FILE TO FIND AN ACCEPTABLE SECTION OF CROSSPLO03660
C     SECTIONS (MF=3). THE PROGRAM WILL THEN USE THE EXFOR INDEX TO     PLO03670
C     DETERMINE IF THERE IS COMPARABLE EXFOR DATA (SAME ZA, MF, MT). IF PLO03680
C     THERE IS NO COMPARABLE DATA THE PROGRAM WILL IGNOR THE CURRENT    PLO03690
C     SECTION OF ENDF/B DATA AND SEARCH FOR THE NEXT REQUESTED SECTION  PLO03700
C     OF ENDF/B DATA. THE CYCLE OF READING ENDF/B DATA AND COMPARING TO PLO03710
C     THE EXFOR INDEX TABLE WILL BE CONTINUED UNTIL COMPARABLE ENDF/B   PLO03720
C     AND EXFOR DATA ARE FOUND. ONLY AFTER THE EXFOR INDEX TABLE SHOWS  PLO03730
C     THAT THE COMPUTATION FORMAT FILE CONTAINS COMPARABLE DATA WILL    PLO03740
C     THE FILE BE READ. AS DESCRIBED ABOVE WHILE READING EXFOR DATA THE PLO03750
C     PROGRAM WILL USE THE STARTING AND ENDING RECORD NUMBER TO QUICKLY PLO03760
C     POSITION TO THE DATA TO READ AND TO STOP READING WHEN ALL REQUIREDPLO03770
C     DATA HAS BEEN READ.                                               PLO03780
C                                                                       PLO03790
C     ONE REFERENCE PER PLOT                                            PLO03800
C     ----------------------                                            PLO03810
C     WHEN PLOTTING ONE REFERENCE PER PLOT THE PROGRAM WILL USE THE     PLO03820
C     EXFOR INDEX TABLE TO DETERMINE WHERE TO START READING. AFTER ONE  PLO03830
C     DATA POINT HAS BEEN READ THE PROGRAM WILL CONTINUE TO READ DATA   PLO03840
C     POINTS UNTIL (1) A POINT IS FOUND WITH A DIFFERENT ZA, MF, MT OR  PLO03850
C     REFERENCE, (2) THE INDEX TABLE LAST RECORD NUMBER INDICATES THAT  PLO03860
C     THE LAST POINT HAS BEEN READ, OR (3) THE MAXIMUM NUMBER OF POINTS PLO03870
C     PER PLOT HAVE BEEN READ.                                          PLO03880
C                                                                       PLO03890
C     WARNING...WHEN PLOTTING ONE REFERENCE PER PLOT IN ORDER TO PRODUCEPLO03900
C     A PLOT THE PROGRAM MUST FIND AT LEAST THE MINIMUM NUMBER OF POINTSPLO03910
C     REQUIRED (SEE, INPUT DESCRIPTION BELOW) ON SUCEESSIVE RECORDS.    PLO03920
C     THEREFORE THE COMPUTATION FORMAT SHOULD BE SORTED TO INSURE THAT  PLO03930
C     ALL DATA WITH THE SAME ZA, MF, MT, REFERENCE APPEAR ON SUCCESSIVE PLO03940
C     RECORDS.                                                          PLO03950
C                                                                       PLO03960
C     ALL COMPARABLE EXFOR DATA ON SAME PLOT                            PLO03970
C     --------------------------------------                            PLO03980
C     WHEN PLOTTING ALL COMPARABLE DATA ON THE SAME PLOT THE PROGRAM    PLO03990
C     WILL USE THE EXFOR INDEX TABLE TO DEFINE WHERE TO START READING.  PLO04000
C     THE PROGRAM WILL CONTINUE TO READ DATA UNTIL (1) THE INDEX TABLE  PLO04010
C     LAST RECORD NUMBER INDICATES THAT THE LAST POINT HAS BEEN READ, ORPLO04020
C     (2) THE MAXIMUM NUMBER OF POINTS PER PLOT HAVE BEEN READ.         PLO04030
C                                                                       PLO04040
C     IN THIS MODE THE EXFOR DATA NEED NOT BE SORTED BY ZA, MF, MT,     PLO04050
C     REFERENCE SINCE THE EXFOR INDEX TABLE WILL DEFINE WHERE ALL       PLO04060
C     COMPARABLE DATA ARE LOCATED ON THE COMPUTATION FORMAT DATA FILE.  PLO04070
C     HOWEVER, TO MINIMIZE THE TIME REQUIRED TO SEARCH THE COMPUTATION  PLO04080
C     FORMAT FILE THE USER SHOULD SORT THE DATA BY ZA, MF, MT.          PLO04090
C                                                                       PLO04100
C     OPTIMIZING PROGRAM OPERATION                                      PLO04110
C     ============================                                      PLO04120
C     PROGRAM OPERATION CAN BE OPTIMIZED BY MINIMIZING THE SIZE OF THE  PLO04130
C     ENDF/B AND COMPUTATION FORMATTED FILES. IF YOU WISH TO COMPARE    PLO04140
C     A LIMITED NUMBER OF REACTIONS IT IS SUGGESTED THAT YOU FIRST      PLO04150
C     PREPARE ENDF/B AND COMPUTATION FORMATTED DATA FILES THAT ONLY     PLO04160
C     CONTAIN THE DATA WHICH WILL BE PLOTTED, I.E., NEVER USE THIS      PLO04170
C     PROGRAM TO TRY TO COMPARE TWO ENORMOUS FILES OF ENDF/B AND EXFOR  PLO04180
C     DATA UNLESS YOU ARE WILLING TO SPEND A CORRESPENDINGLY ENORMOUS   PLO04190
C     AMOUNT OF MONEY ON COMPUTER TIME. IN ADDITION THE EXFOR DATA FILE PLO04200
C     SHOULD BE SORTED BY ZA, MF, MT, REFERENCE.                        PLO04210
C                                                                       PLO04220
C     SCALING OF PLOTS                                                  PLO04230
C     ================                                                  PLO04240
C     ENDF/B AND/OR EXFOR                                               PLO04250
C     -------------------                                               PLO04260
C     IF ONLY PLOTTING EXFOR DATA THIS PROGRAM WILL SCALE THE X AND Y   PLO04270
C     RANGE OF EACH PLOT TO INCLUDE ONLY EXFOR DATA. IF PLOTTING EXFOR  PLO04280
C     AND ENDF/B DATA THE USER MAY SPECIFY BY INPUT (INPUT DESCRIBED    PLO04290
C     BELOW) TO SCALE PLOTS TO INCLUDE ALL ENDF/B AND EXFOR DATA OR     PLO04300
C     ONLY ALL ENDF/B DATA OR ONLY ALL EXFOR DATA. ALTHOUGH THIS OPTION PLO04310
C     MAY BE USED FOR SPECIAL PURPOSES TO OBTAIN SPECIAL SCALING IT IS  PLO04320
C     RECOMMENDED THAT THE USER ALWAYS SCALE PLOTS TO INCLUDE ALL ENDF/BPLO04330
C     AND EXFOR DATA.                                                   PLO04340
C                                                                       PLO04350
C     ENERGY RANGE                                                      PLO04360
C     ------------                                                      PLO04370
C     REGARDLESS OF THE ENERGY RANGE SPECIFIED BY PLOTTING REQUESTS     PLO04380
C     (SEE DESCRIPTION OF REQUESTS BELOW) THIS PROGRAM WILL NEVER EXTENDPLO04390
C     THE ENERGY RANGE OF PLOTS BEYOND WHERE THERE ARE DATA. FOR EXAMPLEPLO04400
C     TO PLOT (N,2N) OVER THE ENTIRE ENERGY RANGE WHERE THERE ARE DATA  PLO04410
C     THE USER CAN SPECIFY 0.0 TO 100 MEV. THIS PROGRAM WILL PRODUCE    PLO04420
C     PLOTS FROM THRESHOLD UP TO THE HIGHEST ENERGY BELOW 100 MEV WHERE PLO04430
C     DATA ARE GIVEN.                                                   PLO04440
C                                                                       PLO04450
C     COSINE RANGE                                                      PLO04460
C     ------------                                                      PLO04470
C     FOR ANGULAR (MF=4) AND DOUBLE DIFFERENTIAL (MF=6) DISTRIBUTIONS   PLO04480
C     WHERE THE X VARIABLE IS COSINE PLOTS WILL ALWAYS BE PRODUCED OVER PLO04490
C     THE COSINE RANGE -1.0 TO 1.0.                                     PLO04500
C                                                                       PLO04510
C     INPUT UNITS                                                       PLO04520
C     ===========                                                       PLO04530
C     NOTE, INPUT PARAMETERS ARE ON UNIT 4, RATHER THAN THE NORMAL      PLO04540
C     FORTRAN UNIT 5. THIS IS TO ALLOW THE PROGRAM TO BE USED ON        PLO04550
C     AN IBM-PC WHERE UNIT 5 IS RESERVED FOR KEYBOARD INTERACTION.      PLO04560
C                                                                       PLO04570
C     UNIT   DESCRIPTION                                                PLO04580
C     ----   -----------                                                PLO04590
C       4    INPUT OPTIONS              (BCD - 80 COLUMNS/RECORD)       PLO04600
C      10    COMPUTATION FORMATTED DATA (BCD - 131 COLUMNS/RECORD)      PLO04610
C      11    ENDF/B FORMATTED DATA      (BCD - 80 COLUMNS/RECORD)       PLO04620
C      12    SPECIAL ZA DEFINITIONS     (BCD - 80 COLUMNS/RECORD)       PLO04630
C      14    MF DEFINITIONS             (BCD - 80 COLUMNS/RECORD)       PLO04640
C      15    MT DEFINITIONS             (BCD - 80 COLUMNS/RECORD)       PLO04650
C      17    SOFTWARE CHARACTERS        (BCD - 80 COLUMNS/RECORD)       PLO04660
C                                                                       PLO04670
C     OUTPUT UNITS                                                      PLO04680
C     ============                                                      PLO04690
C     UNIT   DESCRIPTION                                                PLO04700
C     ----   -----------                                                PLO04710
C       6    OUTPUT REPORT              (BCD - 120 COLUMNS/RECORD)      PLO04720
C                                                                       PLO04730
C     SCRATCH UNITS                                                     PLO04740
C     =============                                                     PLO04750
C     UNIT   DESCRIPTION                                                PLO04760
C     ----   -----------                                                PLO04770
C      16    ENDF/B DATA PAGING UNIT    (BINARY - 6000 WORDS/RECORD)    PLO04780
C                                                                       PLO04790
C     INPUT CARDS                                                       PLO04800
C     ===========                                                       PLO04810
C     THE USER MUST INPUT AT LEAST ONE CARD TO SPECIFY PLOTTING OPTIONS.PLO04820
C     IN THE SIMPLEST CASE THIS FIRST CARD CAN BE COMPLETELY BLANK (SEE PLO04830
C     EXAMPLE INPUT NO. 1 BELOW). TO SELECT DATA BY ZA/MF/MT/INCIDENT   PLO04840
C     ENERGY RANGE THE USER MAY INPUT UP TO 100 ADDITIONAL CARDS.       PLO04850
C                                                                       PLO04860
C     CARD  COLUMNS  FORMAT  DESCRIPTION                                PLO04870
C     ====  =======  ======  ===========                                PLO04880
C       1     1- 5     I5    COMPARE EXFOR DATA TO ENDF/B               PLO04890
C                            0 = NO                                     PLO04900
C                            1 = YES                                    PLO04910
C                            2 = YES (IDENTIFY ENDF/B POINTS BY PLOTTINGPLO04920
C                                     A SMALL DIAMOND ROUND EACH POINT).PLO04930
C                            NOTE 1, IF COMPARING DATA PLOTS WILL ONLY  PLO04940
C                            BE PRODUCED IF COMPARABLE DATA IS FOUND    PLO04950
C                            ON BOTH THE ENDF/B AND COMPUTATION FORMAT  PLO04960
C                            FILES.                                     PLO04970
C                            NOTE 2, IF COMPARING DATA ONLY MF =3 (CROSSPLO04980
C                            SECTIONS) WILL BE PLOTTED.                 PLO04990
C..... REDEFINED BY TRKOV
C             6-10     I5    ALL COMPARABLE EXFOR DATA ON SAME PLOT     PLO05000
C                            0 = NO, >0 = YES
C                            (0 = EACH REFERENCE ON A SEPERATE PLOT
C                            >O = MAX. NUMBER OF REFERENCES PER PLOT,
C                                 CURRENT UPPER LIMIT=48).
C            11-15     I5    PLOT SCALING                               PLO05030
C                            0 = ENDF AND EXFOR (RECOMMENDED)           PLO05040
C                            1 = ENDF                                   PLO05050
C                            2 = EXFOR                                  PLO05060
C                            (AUTOMATICALLY SET TO 2 IF NOT COMPARING)  PLO05070
C            16-20     I5    PLOT X ERROR BARS (ENERGY, COSINE, ETC.)   PLO05080
C                            0 = NO, 1 = YES                            PLO05090
C            21-25     I5    PLOT Y ERROR BARS (CROSS SECTION, ETC.)    PLO05100
C                            0 = NO, 1 = YES                            PLO05110
C            26-30     I5    IDENTIFY ALL REFERENCES BY SYMBOL          PLO05120
C                            0 = NO, 1 = YES                            PLO05130
C                            (0 = IF ONLY ONE REFERENCE ON PLOT DO NOT  PLO05140
C                            PLOT BOX AND REFERENCE SYMBOL AROUND EACH  PLO05150
C                            DATA POINT...RECOMMENDED).                 PLO05160
C            31-35     I5    ALLOW VARIABLE E2 ON SAME PLOT             PLO05170
C                            0 = NO, 1 = YES                            PLO05180
C                            (NORMALLY ONLY DATA WITH SAME ZA/MF/MT/E2  PLO05190
C                            WILL APPEAR ON SAME PLOT. 1 = COLLECT DATA PLO05200
C                            FROM 1 REFERENCE FOR SAME ZA/MF/MT AND     PLO05210
C                            A NUMBER OF VALUES OF E2. IDENTIFY DATA ON PLO05220
C                            PLOT BY EACH VALUE OF E2).                 PLO05230
C            36-40     I5    MINIMUM EXFOR POINTS PER PLOT              PLO05240
C                            (IF THERE ARE FEWER COMPARABLE EXFOR POINTSPLO05250
C                            THEY WILL BE SKIPPED...DEFAULT 8).         PLO05260
C...                         MINIMUM VALID ENTRY IS 2 (NOTE BY TRKOV).
C            41-45     I5    MAXIMUM EXFOR POINTS PER PLOT              PLO05270
C                            (MINIMUM DEFINED BY COLUMNS 36-40 UP TO    PLO05280
C                            1000...DEFAULT 1000).                      PLO05290
C...  MAXIMUM NUMBER OF POINTS INCREASED TO 10000 BY TRKOV
C                            HINT: THE LIMIT APPLIES TO THE TOTAL NUMBER
C                                  OF POINTS EXTRACTED FROM THE EXFOR
C                                  FILE. IF PLOTS ARE DEFINED BY
C                                  EXPLICITLY REQUESTED RANGES (SEE THE
C                                  NEXT INPUT LINE) AND ALL COMPARABLE
C                                  POINTS ARE TO BE DISPLAYED ON THE
C                                  SAME PLOT, USE THE DEFAULT VALUE.
C            46-50     I5    GRID TYPE                                  PLO05300
C                            = 0 - TICK MARKS ON EACH AXIS..RECOMMENDED.PLO05310
C                            = 1 - FULL GRID.                           PLO05320
C            51-55     I5    PLOT SIZE                                  PLO05330
C                            = 0 - FULL SIZE PLOTS.                     PLO05340
C                            = 1 - HALF SIZE (4 SUB-PLOTS PER PLOT).    PLO05350
C            56-70   3A4,A3  ENDF/B LIBRARY IDENTIFICATION.             PLO05360
C                            E.G., ENDF/B-V (ONLY USED IF COMPARING).   PLO05370
C     2-N     1- 7     I7    LOWER ZA LIMIT                             PLO05380
C             8-11     I4    LOWER MF LIMIT                             PLO05390
C            12-15     I4    LOWER MT LIMIT                             PLO05400
C            16-26   E11.4   LOWER INCIDENT ENERGY LIMIT (EV)           PLO05410
C            27-33     I7    UPPER ZA LIMIT                             PLO05420
C            34-37     I4    UPPER MF LIMIT                             PLO05430
C            38-41     I4    UPPER MT LIMIT                             PLO05440
C            42-52   E11.4   UPPER INCIDENT ENERGY LIMIT (EV)           PLO05450
C..... EXTENSION BY TRKOV:
C            53-55     I3    PLOT SCALES (ENDF CONVENTION):
C                             2  LINEAR ABSCISA AND ORDINATE
C                             3  LOGARITHMIC ABSCISA AND LINEAR ORDINATE
C                             4  LINEAR ABSCISA AND LOGARITHMIC ORDINATE
C                             5  LOGARITHMIC ABSCISA AND ORDINATE
C            56-66   E11.4   SMEARING PARAMETER FOR ELASTIC AND
C                            DISCRETE LEVEL INELASTIC SCATTERING ENERGY
C                            DISTRIBUTIONS GIVEN IN TERMS OF A
C                            FRACTIONAL ENERGY INCREMENT AT WHICH A
C                            GAUSSIAN CURVE IS HALF MAXIMUM. THIS IS
C                            ONLY USED WHEN DOUBLE DIFERENTIAL DATA IN
C                            AN ENDF FILE ARE COMPARED TO MEASUREMENTS.
C
C..... EXTENSION BY TRKOV:
C   * THE REQUEST LIST IS TERMINATED BY A BLANK LINE. THE REMAINDER
C     OF THE INPUT FILE WILL BE IGNORED.
C                                                                       PLO05460
C   * THERE MAY BE UP TO 100 ZA/MF/MT/ENERGY RANGE REQUESTS. IF THERE   PLO05470
C     ARE MORE THAN 100 REQUESTS ONLY THE FIRST 100 WILL BE USED.       PLO05480
C   * EACH REQUEST INDEPENDENTLY SPECIFIES A RANGE OF ZA/MF/MT/ENERGY   PLO05490
C     TO BE PLOTTED.                                                    PLO05500
C   * FOR EACH SET OF DATA, ZA MUST BE BETWEEN THE LOWER AND UPPER ZA   PLO05510
C     LIMIT, MF MUST BE BETWEEN THE LOWER AND UPPER MF LIMIT, MT MUST   PLO05520
C     BE BETWEEN THE LOWER AND UPPER MT LIMIT AND THE INCIDENT ENERGY   PLO05530
C     MUST BE BETWEEN THE LOWER AND UPPER ENERGY LIMIT.                 PLO05540
C   * E.G., Z=1 TO 90000, MF=3 TO 3, MT=1 TO 1, E=0.0 TO 2.0E+7 EV      PLO05550
C     WILL SELECT ALL ZA BETWEEN 1 TO 90000 WHICH HAVE MF=3 AND MT=1    PLO05560
C     AND DATA POINTS WITH INCIDENT ENERGY BETWEEN 0 AND 20 MEV.        PLO05570
C   * IF THERE ARE NO REQUEST CARDS ALL DATA WILL BE PLOTTED.           PLO05580
C                                                                       PLO05590
C     EXAMPLE INPUT NO. 1                                               PLO05600
C     -------------------                                               PLO05610
C     TO PLOT ALL EXFOR DATA WITHOUT COMPARISON TO ENDF/B AND WITHOUT   PLO05620
C     ERROR BARS THE USER NEED ONLY ENTER A SINGLE BLANK CARD, OR,      PLO05630
C                                                                       PLO05640
C         0    0    0    0    0    0    0    0    0    0    0           PLO05650
C                                                                       PLO05660
C     EXAMPLE INPUT NO. 2                                               PLO05670
C     -------------------                                               PLO05680
C     PLOT ALL EXFOR DATA 1 REFERENCE PER PLOT WITH X AND Y ERROR BARS. PLO05690
C     DO NOT PLOT DATA UNLESS THERE ARE 8 OR MORE POINTS. PLOT A FULL   PLO05700
C     GRID. INPUT THE FOLLOWING 1 CARD,                                 PLO05710
C                                                                       PLO05720
C         0    0    0    1    1    0    1    8    0    1    0           PLO05730
C                                                                       PLO05740
C     NOTE, THIS IS A GOOD SET OF INPUT PARAMETERS TO USE IN ORDER TO   PLO05750
C     PRODUCE ALL POSSIBLE PLOTS OF ALL EXFOR DATA TRANSLATED FROM A    PLO05760
C     GIVEN EXFOR TAPE. IT IS RECOMMENDED TO SPECIFY 8 AS THE MINIMUM   PLO05770
C     NUMBER OF POINTS PER PLOT IN ORDER TO AVOID OBTAINING A LARGE     PLO05780
C     NUMBER OF PLOTS EACH CONTSINING ONLY 1 OR 2 DATA POINTS.          PLO05790
C                                                                       PLO05800
C     EXAMPLE INPUT NO. 3                                               PLO05810
C     -------------------                                               PLO05820
C     PLOT CO-59 (N,2N) ENDF/B CROSS SECTIONS AND ALL COMPARABLE        PLO05830
C     EXFOR DATA ON THE SAME PLOT WITH CROSS SECTION ERROR BARS, ONE    PLO05840
C     PLOT FROM 0.0 EV (SCALED TO THRESHOLD) TO 20.0 MEV AND A SECOND   PLO05850
C     PLOT FROM 12.0 TO 15.0 MEV. DO NOT PLOT DATA UNLESS THERE ARE AT  PLO05860
C     LEAST 8 EXPERIMENTAL DATA POINTS. ONLY TICK MARKS ON AXIS. ENDL84 PLO05870
C     IS THE IDENTIFICATION FOR THE ENDF/B LIBRARY. INPUT THE FOLLOWING PLO05880
C     3 CARDS.                                                          PLO05890
C                                                                       PLO05900
C         1    1    0    0    1    0    0    8    0    0    0  ENDL84   PLO05910
C       27059   3  16 0.00000+ 0  27059   3  16 2.00000+ 7              PLO05920
C       27059   3  16 1.20000+ 7  27059   3  16 1.50000+ 7              PLO05930
C                                                                       PLO05940
C     REPORTING ERRORS                                                  PLO05950
C     ================                                                  PLO05960
C     IN ORDER TO IMPROVE THIS CODE AND MAKE FUTURE VERSIONS MORE       PLO05970
C     COMPATIBLE FOR USE ON AS MANY DIFFERENT TYPES OF COMPUTERS AS     PLO05980
C     POSSIBLE PLEASE REPORT ALL COMPILER DIAGNOSTICS AND/OR OPERATING  PLO05990
C     PROBLEMS TO THE AUTHOR AT THE ABOVE ADDRESS.                      PLO06000
C                                                                       PLO06010
C     PLEASE REMEMBER IF YOU SIMPLY REPORT 'I'VE GOT A PROBLEM' AND DO  PLO06020
C     NOT ADEQUATELY DESCRIBE EXACTLY HOW YOU WERE USING THE PROGRAM    PLO06030
C     IT WILL BE IMPOSSIBLE FOR THE AUTHOR TO HELP YOU. WHEN A PROBLEM  PLO06040
C     ARISES PLEASE WRITE TO THE AUTHOR, DESCRIBE THE PROBLEM IN AS MUCHPLO06050
C     DETAIL AS POSSIBLE, IDENTIFY THE VERSION OF THE PROGRAM THAT YOU  PLO06060
C     ARE USING (E.G. VERSION 87-1) AND SEND THE FOLLOWING INFORMATION  PLO06070
C     ON MAGNETIC TAPE TO THE AUTHOR,                                   PLO06080
C                                                                       PLO06090
C     (1) A COPY OF THE PROGRAM YOU ARE USING                           PLO06100
C     (2) A COPY OF COMPILER DIAGNOSTICS (IF ANY)                       PLO06110
C     (3) A COPY OF THE JCL DECK YOU USED TO EXECUTE THE PROGRAM        PLO06120
C     (4) A COPY OF THE 3 TRANSLATION DICTIONARIES YOU ARE USING        PLO06130
C     (5) A COPY OF THE COMPUTATION FORMAT DATA YOU USING               PLO06140
C     (6) A COPY OF THE OUTPUT REPORT FROM THE PROGRAM                  PLO06150
C                                                                       PLO06160
C     ALSO SEND COPIES OF ANY PLOTS WHICH YOU HAVE OBTAINED AS OUTPUT   PLO06170
C     FROM THIS PROGRAM.                                                PLO06180
C                                                                       PLO06190
C     WITHOUT ALL OF THIS INFORMATION IT IS IMPOSSIBLE TO EXACTLY       PLO06200
C     SIMULATE THE PROBLEM THAT YOU RAN AND TO DETERMINE THE SOURCE     PLO06210
C     OF YOUR PROBLEM.                                                  PLO06220
C                                                                       PLO06230
C***** COMPUTER DEPENDENT CODING ***************************************PLO06240
C                                                                       PLO06250
C   * THIS PROGRAM IS DESIGNED TO BE USED WITH A FORTRAN-77 COMPILER.   PLO06260
C                                                                       PLO06270
C   * THE ONLY COMPILER DEPENDENT FORMAT STATEMENTS INVOLVE,            PLO06280
C     (1) CHARACTER*1 AND CHARACTER*4                                   PLO06290
C     (2) TESTING FOR ERRORS AND END OF FILE DURING READS.              PLO06300
C                                                                       PLO06310
C   * IT IS ASSUMED THAT CHARACTERS ARE STORED IN SUCCESSIVE STORAGE    PLO06320
C     LOCATIONS AND THAT CHARACTERS MAY BE TREATED AS CONTINUOUS STRINGSPLO06330
C     OF CHARACTERS IN EITHER CHARACTER*4 OR CHARACTER*1 FORMAT.        PLO06340
C                                                                       PLO06350
C   * FOR EXAMPLE, IF ONE SUBROUTINE CONTAINS,                          PLO06360
C                                                                       PLO06370
C     CHARACTER*4 BCD                                                   PLO06380
C     DIMENSION BCD(10)                                                 PLO06390
C                                                                       PLO06400
C     THE ARRAY BCD IS ASSUMED TO BE AN ARRAY OF 40 CHARACTERS IN       PLO06410
C     SUCCESSIVE BYTE LOCATIONS.                                        PLO06420
C                                                                       PLO06430
C     IT IS ASSUMED THAT THIS ARRAY CAN BE PASSED AS AN ARGUMENT TO     PLO06440
C     ANOTHER SUBROUTINE AND USED AS CHARACTER*1,E.G.,                  PLO06450
C                                                                       PLO06460
C     CALL DUMMY(BCD)                                                   PLO06470
C                                                                       PLO06480
C     SUBROUTINE DUMMY(BCD)                                             PLO06490
C     CHARACTER*1 BCD                                                   PLO06500
C     DIMENSION BCD(40)                                                 PLO06510
C                                                                       PLO06520
C   * THIS CONVENTION WILL WORK ON ALL 32 BIT PER WORD COMPUTERS (E.G., PLO06530
C     IBM OR IBM COMPATIBLE COMPUTERS).                                 PLO06540
C                                                                       PLO06550
C   * FOR LONGER WORD LENGTH COMPUTERS (E.G., CDC OR CRAY) IT IS        PLO06560
C     SUGGESTED THAT BEFORE IMPLEMENTING AND USING THIS PROGRAM THE     PLO06570
C     USER FIRST VERIFY THAT CHARACTER STRINGS CAN BE TREATED AS        PLO06580
C     DESCRIBED ABOVE, E.G., WRITE A SIMPLE PROGRAM TO READ A CHARACTER PLO06590
C     STRING OF 40 CHARACTERS IN CHARACTER*4 FORMAT, PASS IT TO A       PLO06600
C     SUBROUTINE WHICH USES THE CHARACTER STRING IN CHARACTER*1 FORMAT  PLO06610
C     AND PRINT THE CHARACTER STRING IN THE SUBROUTINE. IF THE CHARACTERPLO06620
C     STRING IS PRINTED AS A CONTINUOUS STRING YOU WILL BE ABLE TO USE  PLO06630
C     THIS PROGRAM. IF THE CHARACTER STRING IS NOT PRINTED AS A         PLO06640
C     CONTINUOUS STRING IT IS NOT RECOMMENDED THAT YOU USE THIS PROGRAM.PLO06650
C                                                                       PLO06660
C   * THIS PROGRAM USING THE FORTRAN-77 CONVENTION FOR TESTING FOR      PLO06670
C     READING ERRORS AND END OF FILE DURING READS, E.G.,                PLO06680
C                                                                       PLO06690
C     READ(10,1000,END=100,ERR=200) A,B,C,D                             PLO06700
C                                                                       PLO06710
C***** COMPUTER DEPENDENT CODING ***************************************PLO06720
C***** PLOTTER/GRAPHICS TERMINAL INTERFACE *****************************PLO06730
C                                                                       PLO06740
C      THIS PROGRAM USES A SIMPLE CALCOMP LIKE INTERFACE INVOLVING      PLO06750
C      ONLY 3 SUBROUTINES,                                              PLO06760
C                                                                       PLO06770
C      PLOTS(BUF,IBUF,IPLOT) - INITIALIZE PLOTTER                       PLO06780
C            BUF    - PLOTTER BUFFER                                    PLO06790
C            IBUF   - SIZE OF PLOTTING BUFFER (5000 WORDS USED)         PLO06800
C            IPLOT  - PLOTTER UNIT (16)...USUALLY A DUMMY UNIT          PLO06810
C                                                                       PLO06820
C      PLOT(X,Y,IPEN)        - DRAW OR MOVE FROM LAST LOCATION TO (X,Y),PLO06830
C                              END OF CURRENT PLOT OR END OF PLOTTING.  PLO06840
C            IPEN =   2 - DRAW                                          PLO06850
C                 =   3 - MOVE                                          PLO06860
C                 =  -1 - END OF CURRENT PLOT...ADVANCE BY X,Y          PLO06870
C                 = 999 - END OF PLOTTING.                              PLO06880
C                                                                       PLO06890
C      PEN(IPEN)             - SELECT COLOR.                            PLO06900
C           IPEN- COLOR = 1 TO N (N = ANY POSITIVE INTEGER)             PLO06910
C                                                                       PLO06920
C     IN ORDER TO INTERFACE THIS PROGRAM FOR USE ON ANY PLOTTER WHICH   PLO06930
C     DOES NOT USE THE ABOVE CONVENTIONS IT IS MERELY NECESSARY FOR THE PLO06940
C     THE USER TO WRITE 3 SUBROUTINES WITH THE NAMES PLOTS, PLOT AND PENPLO06950
C     WITH THE SUBROUTINE ARGUMENTS DESCRIBED ABOVE AND TO THEN CALL THEPLO06960
C     LOCAL EQUIVALENT ROUTINES.                                        PLO06970
C                                                                       PLO06980
C     AVAILABLE PLOTTER INTERFACES                                      PLO06990
C     ==================================================================PLO07000
C     THIS PROGRAM HAS AVAILABLE PLOTTER INTERFACES TO OPERATE AS       PLO07010
C     FOLLOWS,                                                          PLO07020
C     (1) MAINFRAME - HARDCOPY PLOTS IN BLACK AND WHITE.                PLO07030
C     (2) MAINFRAME - SCREEN PLOTS IN 7 COLORS ON IBM GRAPHICS TERMINAL.PLO07040
C     (3) IBM-PC    - HARDCOPY PLOTS IN 6 COLORS ON A HEWLETT-PACKARD   PLO07050
C                     7475A PLOTTER.                                    PLO07060
C                                                                       PLO07070
C     CONTACT THE AUTHOR TO OBTAIN COPIES OF ANY OF THE ABOVE PLOTTER   PLO07080
C     INTERFACES.                                                       PLO07090
C                                                                       PLO07100
C     COLOR PLOTS                                                       PLO07110
C     ==================================================================PLO07120
C     TO SELECT PLOTTING COLORS SUBROUTINE PEN (DESCRIBED ABOVE) IS USEDPLO07130
C     TO SELECT ONE OF THE AVAILABLE COLORS. WHEN RUNNING ON A MAINFRAMEPLO07140
C     USING AN IBM GRAPHICS TERMINAL OR ON AN IBM-PC USING A HEWLETT-   PLO07150
C     PACKARD PLOTTER THE GRAPHICS INTERFACE (DESCRIBED ABOVE) WILL     PLO07160
C     PRODUCE COLOR PLOTS.                                              PLO07170
C                                                                       PLO07180
C     BLACK AND WHITE PLOTS                                             PLO07190
C     ==================================================================PLO07200
C     WHEN PRODUCING BLACK AND WHITE HARDCOPY ON A MAINFRAME THE USER   PLO07210
C     SHOULD ADD A DUMMY SUBROUTINE PEN TO THE END OF THE PROGRAM TO    PLO07220
C     IGNORE ATTEMPTS TO CHANGE COLOR. ADD THE FOLLOWING SUBROUTINE,    PLO07230
C                                                                       PLO07240
C     SUBROUTINE PEN(IPEN)                                              PLO07250
C     RETURN                                                            PLO07260
C     END                                                               PLO07270
C                                                                       PLO07280
C     CHARACTER SET                                                     PLO07290
C     ==================================================================PLO07300
C     THIS PROGRAM USES COMPUTER AND PLOTTER DEVICE INDEPENDENT SOFTWAREPLO07310
C     CHARACTERS. THIS PROGRAM COMES WITH A FILE THAT DEFINES THE PEN   PLO07320
C     STROKES REQUIRED TO DRAW ALL CHARACTERS ON AN IBM KEYBOARD (UPPER PLO07330
C     AND LOWER CASE CHARACTERS, NUMBERS, ETC.) PLUS AN ALTERNATE SET OFPLO07340
C     ALL UPPER AND LOWER CASE GREEK CHARACTERS AND ADDITIONAL SPECIAL  PLO07350
C     SYMBOLS.                                                          PLO07360
C                                                                       PLO07370
C     THE SOFTWARE CHARACTER TABLE CONTAINS X AND Y AND PEN POSITIONS TOPLO07380
C     DRAW EACH CHARACTER. IF YOU WISH TO DRAW ANY ADDITIONAL CHARACTERSPLO07390
C     OR TO MODIFY THE FONT OF THE EXISTING CHARACTERS YOU NEED ONLY    PLO07400
C     MODIFY THIS TABLE.                                                PLO07410
C                                                                       PLO07420
C     CONTROL CHARACTERS                                                PLO07430
C     ------------------------------------------------------------------PLO07440
C     IN THE SOFTWARE CHARACTER TABLE ALL CHARACTERS TO BE PLOTTED WILL PLO07450
C     HAVE PEN POSITION = 2 (DRAW) OR = 3 (MOVE). IN ADDITION THE TABLE PLO07460
C     CURRENTLY CONTAINS 4 CONTROL CHARACTERS,                          PLO07470
C                                                                       PLO07480
C     PEN POSITION = 0                                                  PLO07490
C     ----------------                                                  PLO07500
C     SHIFT THE NEXT PRINTED CHARACTER BY X AND Y. 3 CONTROL CHARACTERS PLO07510
C     ARE PRESENTLY INCLUDED IN THE SOFTWARE CHARACTER TABLE TO ALLOW   PLO07520
C     SHIFTING.                                                         PLO07530
C                                                                       PLO07540
C     {   = SHIFT UP (FOR SUPERSCRIPTS..............X= 0.0, Y= 0.5)     PLO07550
C     }   = SHIFT DOWN (FOR SUBSCRIPTS..............X= 0.0, Y=-0.5)     PLO07560
C     \   = SHIFT LEFT 1 CHARACTER (FOR BACKSPACE...X=-1.0, Y= 0.0)     PLO07570
C                                                                       PLO07580
C     PEN POSITION =-1                                                  PLO07590
C     ----------------                                                  PLO07600
C     SELECT THE NEXT PRINTED CHARACTER FROM THE ALTERNATE CHARACTER    PLO07610
C     SET. AT PRESENT THIS CONTROL CHARACTER IS,                        PLO07620
C                                                                       PLO07630
C     ]   = SWITCH TO ALTERNATE CHARACTER SET                           PLO07640
C                                                                       PLO07650
C     THESE 4 CONTROL CHARACTERS ARE ONLY DEFINED BY THE VALUE OF THE   PLO07660
C     PEN POSITION IN THE SOFTWARE CHARACTER TABLE (I.E., THEY ARE NOT  PLO07670
C     HARD WIRED INTO THIS PROGRAM). AS SUCH BY MODIFYING THE SOFTWARE  PLO07680
C     CHARACTER TABLE THE USER HAS THE OPTION OF DEFINING ANY CONTROL   PLO07690
C     CHARACTERS TO MEET SPECIFIC NEEDS.                                PLO07700
C                                                                       PLO07710
C     THESE CHARACTERS MAY BE USED IN CHARACTER STRINGS TO PRODUCE      PLO07720
C     SPECIAL EFFECTS. FOR EXAMPLE, TO PLOT SUBSCRIPT 5, B, SUPERSCRIPT PLO07730
C     10 USE THE STRING,                                                PLO07740
C                                                                       PLO07750
C     }5B{1{0                                                           PLO07760
C                                                                       PLO07770
C     TO PLOT B, SUBSCRIPT 5 AND SUPERSCRIPT 10 WITH THE 5 DIRECTLY     PLO07780
C     BELOW THE 1 OF THE 10 USE THE STRING,                             PLO07790
C                                                                       PLO07800
C     B}5\{1{0                                                          PLO07810
C                                                                       PLO07820
C     TO PLOT UPPER CASE GREEK GAMMA FOLLOWED BY THE WORDS TOTAL WIDTH  PLO07830
C     USE THE STRING,                                                   PLO07840
C                                                                       PLO07850
C     ]G TOTAL WIDTH                                                    PLO07860
C                                                                       PLO07870
C     NOTE, WHEN THESE CONTROL CHARACTERS ARE USED THEY ONLY EFFECT THE PLO07880
C     NEXT 1 PRINTED CHARACTER (SEE, ABOVE EXAMPLE OF PLOTTING SUPER-   PLO07890
C     SCRIPT 10 WHERE THE SHIFT UP CONTROL CHARACTER WAS USED BEFORE THEPLO07900
C     1 AND THEN AGAIN BEFORE THE 0).                                   PLO07910
C                                                                       PLO07920
C     IF THESE 4 CONTROL CHARACTERS ARE NOT AVAILABLE ON YOUR COMPUTER  PLO07930
C     YOU CAN MODIFY THE SOFTWARE CHARACTER TABLE TO USE ANY OTHER 4    PLO07940
C     CHARACTERS THAT YOU DO NOT NORMALLY USE IN CHARACTER STRINGS (FOR PLO07950
C     DETAILS SEE THE SOFTWARE CHARACTER TABLE).                        PLO07960
C                                                                       PLO07970
C     STANDARD/ALTERNATE CHARACTER SETS                                 PLO07980
C     ------------------------------------------------------------------PLO07990
C     THE SOFTWARE CHARACTER TABLE CONTAINS 2 SETS OF CHARACTERS WHICH  PLO08000
C     ARE A STANDARD SET (ALL CHARACTERS ON AN IBM KEYBOARD) AND AN     PLO08010
C     ALTERNATE SET (UPPER AND LOWER CASE GREEK CHARACTERS AND SPECIAL  PLO08020
C     CHARACTERS). TO DRAW A CHARACTER FROM THE ALTERNATE CHARACTER SET PLO08030
C     PUT A VERTICAL STROKE CHARACTER (]) BEFORE A CHARACTER (SEE THE   PLO08040
C     ABOVE EXAMPLE AND THE SOFTWARE CHARACTER TABLE FOR DETAILS). THIS PLO08050
C     CONTROL CHARACTER WILL ONLY EFFECT THE NEXT 1 PLOTTED CHARACTER.  PLO08060
C                                                                       PLO08070
C     SUB AND SUPER SCRIPTS                                             PLO08080
C     ------------------------------------------------------------------PLO08090
C     TO DRAW SUBSCRIPT PRECEED A CHARACTER BY }. TO DRAW SUPERSCRIPT   PLO08100
C     PRECEED A CHARACTER BY { (SEE THE ABOVE EXAMPLE AND THE SOFTWARE  PLO08110
C     CHARACTER TABLE FOR DETAILS). THESE CONTROL CHARACTER WILL ONLY   PLO08120
C     EFFECT THE NEXT 1 PLOTTED CHARACTER.                              PLO08130
C                                                                       PLO08140
C     BACKSPACING                                                       PLO08150
C     ------------------------------------------------------------------PLO08160
C     TO BACKSPACE ONE CHARACTER PRECEED A CHARACTER BY \ (SEE, THE     PLO08170
C     ABOVE EXAMPLE AND THE SOFTWARE CHARACTER TABLE FOR DETAILS). THIS PLO08180
C     CONTROL CHARACTER WILL PERFORM A TRUE BACKSPACE AND WILL EFFECT   PLO08190
C     ALL FOLLOWING CHARACTERS IN THE SAME CHARACTER STRING.            PLO08200
C                                                                       PLO08210
C***** PLOTTER/GRAPHICS TERMINAL INTERFACE *****************************PLO08220
      INTEGER OUTP                                                      PLO08230
C***** TRKOV - OPEN FILES EXPLICITLY
      CHARACTER*40 P4INP,P4LST,C4DAT,EDAT,ZADEF,MFDEF,MTDEF,CHTAB
      CHARACTER*4 VERSES,VERSEZ
C***** TRKOV - END
      CHARACTER*4 REFS,REF1,REFX,ZABCD,MSTAT1,MSTAT2,LIBNAM,BLANK       PLO08240
      CHARACTER*1 LABCM,LAB,STATUS,STAT,MSTAR1,MSTAR2,BLANK1            PLO08250
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO08260
C***** OLD
C***** TRKOV
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
C***** TRKOV
      COMMON/UNITT/NTAPE1,NTAPE2,NTAPE3                                 PLO08270
      COMMON/THICKY/ITHICK,THICK                                        PLO08280
      COMMON/INPARM/MINNIE,MAXIE                                        PLO08290
      COMMON/MODEMY/MYMODE                                              PLO08300
C***** OLD
C     COMMON/PAGEXY/XPAGE(9000),YPAGE(9000),N2,IBASE,ITOP,ISCR          PLO08310
C***** OLD
C***** TRKOV
      COMMON/PAGEXY/XPAGE(90000),YPAGE(90000),N2,IBASE,ITOP,ISCR
C***** TRKOV
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO08320
      COMMON/XYREAL/XREAL(2),YREAL(2)                                   PLO08330
      COMMON/RATZAC/MSTAR1,MSTAR2                                       PLO08340
      COMMON/GRIDDY/MYGRID                                              PLO08350
      COMMON/LIBI/ILIB                                                  PLO08360
      COMMON/LIBC/LIBNAM(4)                                             PLO08370
C***** OLD
C     COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX                           PLO08380
C***** OLD
C***** TRKOV
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX,AWR
C***** TRKOV
      COMMON/WHEREC/ZABCD(4),MSTAT1,MSTAT2                              PLO08390
      COMMON/WHERE2/IZABCD                                              PLO08400
      COMMON/WAYS/IWAY(2)                                               PLO08410
C***** OLD
C     COMMON/EXFOR/XEX(1000),DXEX(1000),YEX(1000),DYEX(1000),NREF(1000),PLO08420
C    1 E2(1000),IEX                                                     PLO08430
C     COMMON/REFERI/LREF(26),EXLOW(26),EXHIGH(26),E2T(26),IREF,MREF,    PLO08440
C    1 MAXREF,IGROUP,KGROUP                                             PLO08450
C***** OLD
C***** TRKOV
      COMMON/EXFOR/XEX(10000),DXEX(10000),YEX(10000),DYEX(10000)
     1,NREF(10000),E2(10000),IEX
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP                                             PLO08450
C***** TRKOV
      COMMON/RATZA/IZARAT,MTRAT,MFIN                                    PLO08460
C***** OLD
C     COMMON/REFERC/REFS(9,26),REF1(9)                                  PLO08470
C     COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),        PLO08480
C    1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),NGET,IGET              PLO08490
C***** OLD
C***** TRKOV
      COMMON/REFERC/REFS(9,48),REF1(9)
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
C***** TRKOV
      COMMON/SYSSTA/LABCM,STATUS                                        PLO08500
      COMMON/DOUBL/IDOUB,FIELD4(4)                                      PLO08510
      COMMON/XLIMIT/MPT                                                 PLO08520
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC                                 PLO08530
      COMMON/PLOTN/NUMPLT,ADVANC                                        PLO08540
C***** TRKOV
      COMMON/VERSC/VERSES(5,4)
      COMMON/VERSI/NVERSE(4)
      COMMON/EPSMF6/EP6
C***** TRKOV
      DIMENSION REFX(9),FIELDI(8)                                       PLO08550
C***** TRKOV
      DIMENSION VERSEZ(5,4)
      DIMENSION RWO(20000)
      DATA MXR/20000/
C***** TRKOV
      DATA REFX/'Othe','rs  ',' ',' ',' ',' ',' ',' ',' '/              PLO08560
      DATA BLANK/'    '/                                                PLO08570
      DATA BLANK1/' '/                                                  PLO08580
C-----DEFINE ALL I/O UNITS.                                             PLO08590
C***** TRKOV - OPEN FILES EXPLICITLY
      DATA P4INP /'PLOTC4.INP'/
     1     P4LST /'PLOTC4.LST'/
     1     C4DAT /'C4.DAT'/
     2     EDAT  /'ENDF.DAT'/
     3     ZADEF /'ENDFZA.DAT'/
     4     MFDEF /'ENDFMF.DAT'/
     5     MTDEF /'ENDFMT.DAT'/
     6     CHTAB /'PLOT.CHR'/
C***** TRKOV -END
C***** TRKOV
      DATA VERSEZ/'   P','rogr','am P','LOTC','4   ',
     1 '   (','Vers','ion ','2000','-1) ',
     2 'Nucl','ear ','Data',' Sec','tion',
     3 '    ','IAEA','  Vi','enna','    '/
      DO 14 I=1,4
      NVERSE(I)=20
      DO 12 J=1,5
      VERSES(J,I)=VERSEZ(J,I)
   12 CONTINUE
   14 CONTINUE
      IDX=0
C***** TRKOV
      INP=4                                                             PLO08600
C***** OLD
C     OUTP=6                                                            PLO08610
C***** OLD
C***** TRKOV - CHANGE UNIT NUMBER
      OUTP=7
C***** TRKOV - END
      ITAPE1=10                                                         PLO08620
      ITAPE2=11                                                         PLO08630
      NTAPE1=12                                                         PLO08640
      NTAPE2=14                                                         PLO08650
      NTAPE3=15                                                         PLO08660
C***** OLD
C     ISCR=16
C***** OLD END
C***** TRKOV - CHANGE FILE UNIT NUMBER
      ISCR=18
C***** TRKOV END
      ISYM=17                                                           PLO08680
C***** TRKOV - OPEN FILES EXPOLICITLY
      OPEN (UNIT=INP   ,FILE=P4INP,STATUS='OLD')
      OPEN (UNIT=ITAPE1,FILE=C4DAT,STATUS='OLD')
      OPEN (UNIT=ITAPE2,FILE=EDAT ,STATUS='OLD')
      OPEN (UNIT=NTAPE1,FILE=ZADEF,STATUS='OLD')
      OPEN (UNIT=NTAPE2,FILE=MFDEF,STATUS='OLD')
      OPEN (UNIT=NTAPE3,FILE=MTDEF,STATUS='OLD')
      OPEN (UNIT=ISYM  ,FILE=CHTAB,STATUS='OLD')
      OPEN (UNIT=ISCR  ,FILE='SCR.',FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN (UNIT=OUTP  ,FILE=P4LST,STATUS='UNKNOWN')
C***** TRKOV - END
C-----LOAD SOFTWARE CHARACTERS.                                         PLO08690
      CALL SYMIN                                                        PLO08700
C-----DEFINE LINE THICKNESS PARAMETERS.                                 PLO08710
      ITHICK=0                                                          PLO08720
      THICK=0.002                                                       PLO08730
C-----DEFINE MAXIMUM NUMBER OF REFERENCES WHICH CAN BE IDENTIFIED ON    PLO08740
C-----A PLOT.                                                           PLO08750
C***** OLD
C     MAXREF=20                                                         PLO08760
C***** OLD
C-----INITIALIZE FLAG TO SHOW PROGRAM IS RUNNING ON MAINFRAME (WILL BE  PLO08770
C-----RESET BY PLOTTER INTERFACE IF RUNNING ON IBM-PC).                 PLO08780
C***** TRKOV - SET TO ZERO
      IBMPC= 0
C***** TRKOV END
C***** OLD
C     IBMPC=-1                                                          PLO08790
C***** OLD END
C-----INITIALIZE NUMBER OF PLOTS GENERATED.                             PLO08800
      NUMPLT=0                                                          PLO08810
C-----IDENTIFY PROGRAM.                                                 PLO08820
C***** OLD
C     WRITE(OUTP,6000)                                                  PLO08830
C***** OLD
C***** TRKOV
      WRITE(OUTP,6000) (VERSES(I,2),I=2,5)
C***** TRKOV
C-----READ ZA/MF/MT TRANSLATION TABLES.                                 PLO08840
      CALL ZAMFMT                                                       PLO08850
C-----READ ALL INPUT PARAMETERS AND INITIALIZE PLOTTER.                 PLO08860
      CALL READIN                                                       PLO08870
C-----INITIALIZE ENDF/B AND EXFOR POINT COUNTS AND ENDF/B MAT NUMBER.   PLO08880
      N2=0                                                              PLO08890
      IEX=0                                                             PLO08900
      MAT=0                                                             PLO08910
C-----DEFINE LAST REFERENCE AS OTHERS.                                  PLO08920
      DO 10 J=1,9                                                       PLO08930
   10 REFS(J,MAXREF+1)=REFX(J)                                          PLO08940
C-----PRINT TITLE FOR OUTPUT REPORT.                                    PLO08950
      WRITE(OUTP,6040)                                                  PLO08960
C                                                                       PLO08970
C     READ ENDF/B AND/OR EXFOR DATA AND MAKE PLOTS.                     PLO08980
C                                                                       PLO08990
C-----IF COMPARISON MODE READ NEXT SECTION OF ENDF/B DATA.              PLO09000
   20 IF(MYMODE.LT.2) GO TO 30                                          PLO09010
      IEVEND=0                                                          PLO09020
      CALL GETEV(IEVEND)                                                PLO09030
C-----END OF RUN IF END OF ENDF/B DATA.                                 PLO09040
      IF(IEVEND.NE.0) GO TO 80                                          PLO09050
C-----READ COMPARABLE EXFOR DATA.                                       PLO09060
   30 IEXEND=0                                                          PLO09070
      CALL GETEX(IEXEND)                                                PLO09080
C-----NO PLOT IF LESS THAN MINIMUM NUMBER OF EXFOR POINTS.              PLO09090
      IF(IEX.LT.MINNIE) GO TO 60                                        PLO09100
C-----IF COMPARING TO ENDF/B SET ALL METASTABLE STATE FLAGS TO BLANK.   PLO09110
      IF(MYMODE.LT.2) GO TO 40                                          PLO09120
      MSTAT1=BLANK                                                      PLO09130
      MSTAT2=BLANK                                                      PLO09140
      MSTAR1=BLANK1                                                     PLO09150
      MSTAR2=BLANK1                                                     PLO09160
C-----PREPARE ENDF DOUBLE DIFFERENTIAL DATA FOR COMPARISON
C***** TRKOV
      IF(MF.LT.4 .OR. MF.GT.6) GO TO 40
      N2=0
      PAR=-2
      IF(IDOUB.EQ.2) THEN
        KEA=2
        IF(MFIN.EQ.6) THEN
          PAR=FIELD4(1)
          DEG=ACOS(PAR)*180/3.1415926
        END IF
      ELSE
        KEA=1
        IF(MFIN.EQ.6) PAR=FIELD4(3)
      END IF
C... Unit for messages from DXSEND (set zero to suppress)
      LTT=6
      ZAA=IZA
      ZAP=1
      EIN=ENEX
      MPT=90000
      MTX=MT
      IF(MT.EQ.9000) MTX=5
      CALL DXSEND(ITAPE2,ZAA,ZAP,MTX,KEA,EIN,PAR,EP6
     1           ,XPAGE,YPAGE,RWO,N2,MPT,MXR,LTT)
C... Temporary diagnostic printout
      PRINT *,'Zaa,Zap,MTX,Ein,Par,Kea,Np'
     1        ,ZAA,ZAP,MT ,EIN,PAR,IDOUB,N2
C...  IF(IDOUB.EQ.1) THEN
C...      PRINT *,(XPAGE(J),J=1,N2)
C...      PRINT *,(YPAGE(J),J=1,N2)
C...  END IF
C...
      IBASE=0
      ITOP=N2
C***** TRKOV
C-----DEFINE HOLLERITH EQUIVALENT OF ZA.                                PLO09170
   40 CALL ZAHOL(IZA,MSTAT1,ZABCD,IZABCD)                               PLO09180
C-----SAVE INPUT MF AND DEFINE INTERNAL MF. NO PLOT IF MF NOT IN LEGAL  PLO09190
C-----RANGE.                                                            PLO09200
C***** OLD
C     MFIN=MF                                                           PLO09210
C***** OLD
      IF(MF.EQ.203) MF=3                                                PLO09220
      IF(MF.EQ.402) MF=3                                                PLO09230
      IF(MF.EQ.154) MF=7                                                PLO09240
      IF(MF.EQ.801) MF=8                                                PLO09250
      IF(MF.LT.1.OR.MF.GT.8) GO TO 60                                   PLO09260
C-----PRINT DESCRIPTION OF DATA TO BE PLOTTED.                          PLO09270
C***** OLD
C     IF(MF.EQ.3) WRITE(OUTP,6050) (ZABCD(I),I=1,3),MAT,MFIN,MT,N2,     PLO09280
C    1 IEX,IREF                                                         PLO09290
C     IF(MF.NE.3) WRITE(OUTP,6050) (ZABCD(I),I=1,3),MAT,MFIN,MT,N2,     PLO09300
C    1 IEX,IREF,ENEX                                                    PLO09310
C***** OLD
C***** TRKOV
      IDX=IDX+1
      IF(MF.EQ.3)
     1 WRITE(OUTP,6050) (ZABCD(I),I=1,3),MAT,MFIN,MT,N2,IEX,IREF,IDX
      IF(MFIN.NE.3 .AND. MFIN.NE.6)
     1 WRITE(OUTP,6051) (ZABCD(I),I=1,3),MAT,MFIN,MT,N2,IEX,IREF,
     1                  ENEX,IDX
      IF(MFIN.EQ.6 .AND. IDOUB.EQ.2)
     1 WRITE(OUTP,6052) (ZABCD(I),I=1,3),MAT,MFIN,MT,N2,IEX,IREF,
     1                  ENEX,DEG,IDX
      IF(MFIN.EQ.6 .AND. IDOUB.EQ.1)
     1 WRITE(OUTP,6053) (ZABCD(I),I=1,3),MAT,MFIN,MT,N2,IEX,IREF,
     1                  ENEX,PAR,IDX
C***** TRKOV
C                                                                       PLO09320
C     SET UP LOOP OVER REQUESTS. IF NO REQUESTS SET UP ONCE THROUGH LOOPPLO09330
C                                                                       PLO09340
      DO 50 IGET=1,NGET                                                 PLO09350
C-----SELECT POINTS TO APPEAR ON NEXT PLOT.                             PLO09360
      CALL SCALER                                                       PLO09370
C-----NO PLOT IF LESS THAN MINIMUM NUMBER OF POINTS.                    PLO09380
      IF(MPT.LT.MINNIE) GO TO 50                                        PLO09390
C-----SELECT AXIS UNITS.                                                PLO09400
      CALL UNITED                                                       PLO09410
C-----PLOT BORDER AND ALL AXIS LABELS.                                  PLO09420
      CALL BORDER                                                       PLO09430
C-----PLOT X AND Y AXIS GRID                                            PLO09440
      IF(MYGRID.EQ.0) CALL GRID0                                        PLO09450
      IF(MYGRID.NE.0) CALL GRID1                                        PLO09460
C-----PLOT ENDF/B DATA (IF ANY).                                        PLO09470
      IF(MYMODE.GE.2) CALL EVALP                                        PLO09480
C-----PLOT EXPERIMENTAL DATA.                                           PLO09490
      CALL EXFORP                                                       PLO09500
C-----END OF PLOT. ADVANCE TO NEXT PLOTTING AREA.                       PLO09510
      CALL NXTPLT                                                       PLO09520
   50 CONTINUE                                                          PLO09530
C-----END OF EXFOR DATA = END OF PLOTTING.                              PLO09540
   60 IF(IEXEND.EQ.2) GO TO 80                                          PLO09550
C-----IF ALL EXFOR DATA HAS NOT YET BEEN READ CONTINUE READING EXFOR    PLO09560
C-----(IF NO COMPARISON...NO NEED TO READ ENDF/B)                       PLO09570
C-----(IF COMPARISON......THERE MAY BE MORE EXFOR DATA TO COMPARE TO    PLO09580
C-----CURRENT SECTION OF ENDF/B DATA).                                  PLO09590
      IF(IEXEND) 30,30,70                                               PLO09600
C-----IF COMPARISON MODE CONTINUE READING ENDF/B DATA.                  PLO09610
   70 IF(MYMODE.GE.2) GO TO 20                                          PLO09620
C-----NO COMPARISON. IF PLOTTING ALL REFERENCES TOGETHER CONTINUE       PLO09630
C-----READING UNTIL NO MORE DATA. OTHERWISE END OF RUN.                 PLO09640
      IF(MYMODE.EQ.1.AND.IEXEND.LT.2) GO TO 30                          PLO09650
C-----END OF RUN.                                                       PLO09660
C***** STARPLOT - ADAPT FOR NEW PLOTTER INTERFACE
   80 CALL ENDPLOTS
C***** STARPLOT END
C***** PLOTPACK
C  80 CALL PLOT(0.0,0.0,999)                                            PLO09670
C***** PLOTPACK END
      WRITE(OUTP,6060) NUMPLT                                           PLO09680
      STOP                                                              PLO09690
C***** OLD
C6000 FORMAT(' PLOT ENDF/B AND/OR EXFOR DATA (PLOTC4 VERSION 87-1)'/    PLO09700
C    1 1X,72('='))                                                      PLO09710
C6040 FORMAT(1X,72('=')/' PROCESSING'/1X,72('=')/                       PLO09720
C    4 ' MATERIAL      MAT  MF   MT EVALUATED EXPERIMENTAL',            PLO09730
C    4 ' EXPERIMENTAL    ENERGY'/                                       PLO09740
C    5 '                               POINTS       POINTS',            PLO09750
C    5 '   REFERENCES        EV'/                                       PLO09760
C    6 1X,72('='))                                                      PLO09770
C6050 FORMAT(1X,3A4,I5,I4,I5,I10,2I13,1PE10.3)                          PLO09780
C***** OLD
C***** TRKOV
 6000 FORMAT(' PLOT ENDF/B AND/OR EXFOR DATA (PLOTC4 ',4A4/
     1 1X,72('='))
 6040 FORMAT(1X,72('=')/' PROCESSING'/1X,75('=')/
     4 ' MATERIAL     MAT  MF   MT  EVAL. EXPR. ',
     4 'EXPR.   EN-INC ANG-OUT    EN-OUT IDX'/
     5 '                            PNTS. PNTS. ',
     5 ' REF.       EV     DEG        EV    '/
     6 1X,75('='))
 6050 FORMAT(1X,3A4,I4,I4,I5,3I6,                28X,I4)
 6051 FORMAT(1X,3A4,I4,I4,I5,3I6,1PE10.3,        18X,I4)
 6052 FORMAT(1X,3A4,I4,I4,I5,3I6,1PE10.3,0P,F8.2,10X,I4)
 6053 FORMAT(1X,3A4,I4,I4,I5,3I6,1PE10.3,8X,E10.3,   I4)
C***** TRKOV
 6060 FORMAT(1X,72('=')/' END OF RUN',20X,I6,' PLOTS GENERATED')        PLO09790
      END                                                               PLO09800
      SUBROUTINE SETUP                                                  PLO09810
C                                                                       PLO09820
C     DEFINE ALL PLOTTER PARAMETERS AND INITIALIZE PLOTTER.             PLO09830
C                                                                       PLO09840
      CHARACTER*4 VERSES                                                PLO09850
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO09860
      COMMON/HPINCH/XHP(2),YHP(2)                                       PLO09870
      COMMON/INCHTB/XPINCH(2,4),YPINCH(2,4)                             PLO09880
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO09890
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC                                 PLO09900
      COMMON/LOGTAB/TABLOG(10)                                          PLO09910
      COMMON/PLOTN/NUMPLT,ADVANC                                        PLO09920
C***** OLD
C     DIMENSION BUFF(5000),VERSES(5,4),NVERSE(4)                        PLO09930
C***** OLD
C***** TRKOV - Simplify version control
      COMMON/VERSC/VERSES(5,4)
      COMMON/VERSI/NVERSE(4)
      DIMENSION BUFF(5000)
C***** TRKOV
C-----DEFINE IDENTIFICATION FOR PROGRAM AND VERSION.                    PLO09940
C***** OLD
C     DATA NVERSE/20,20,20,20/                                          PLO09950
C     DATA VERSES/'   P','rogr','am P','LOTC','4   ',                   PLO09960
C    1 '   (','Vers','ion ','87-1',')   ',                              PLO09970
C    2 'Nucl','ear ','Data',' Sec','tion',                              PLO09980
C    3 '    ','IAEA','  Vi','enna','    '/                              PLO09990
C***** OLD
C-----DEFINE HEIGHT AND WIDTH OF CHARACTERS FOR PROGRAM I.D.            PLO10000
      DATA HTV/0.28/                                                    PLO10010
      DATA WTV/0.28/                                                    PLO10020
C***** STARPLOT DEFINE PAPER SIZE PLOT LIMITS (INTERFACE-DEPENDENT)
      DATA PAPERX0,PAPERX1,PAPERY0,PAPERY1/ 2., 22., 2., 15./
C***** STARPLOT
C-----SELECT FULL OR HALF SIZE PLOTS.                                   PLO10030
      IF(IPLOTZ.EQ.0) GO TO 30                                          PLO10040
C                                                                       PLO10050
C     HALF SIZE PLOTS.                                                  PLO10060
C                                                                       PLO10070
      SIZPLT=0.5                                                        PLO10080
      II=0                                                              PLO10090
      DO 20 I=1,2                                                       PLO10100
      DO 10 J=1,2                                                       PLO10110
      II=II+1                                                           PLO10120
C***** PLOTPACK
C     XPINCH(1,II)=0.0+6.5*FLOAT(J-1)                                   PLO10130
C     XPINCH(2,II)=XPINCH(1,II)+3.8                                     PLO10140
C     YPINCH(1,II)=0.50+5.1*FLOAT(2-I)                                  PLO10150
C  10 YPINCH(2,II)=YPINCH(1,II)+3.8                                     PLO10160
C***** PLOTPACK
C***** STARPLOT
      XPINCH(1,II)=PAPERX0+0.5*PAPERX1*FLOAT(J-1)                                   PLO10130
      XPINCH(2,II)=XPINCH(1,II)+0.45*(PAPERX1-PAPERX0)*0.65
      YPINCH(1,II)=PAPERY0+0.5*PAPERY1*FLOAT(2-I)
   10 YPINCH(2,II)=YPINCH(1,II)+0.45*(PAPERY1-PAPERY0)
C***** STARPLOT
   20 CONTINUE                                                          PLO10170
      XINCH(1)=XPINCH(1,1)                                              PLO10180
      XINCH(2)=XPINCH(2,1)                                              PLO10190
      YINCH(1)=YPINCH(1,1)                                              PLO10200
      YINCH(2)=YPINCH(2,1)                                              PLO10210
C***** PLOTPACK
C     BOX=0.05                                                          PLO10220
C     HT=0.06                                                           PLO10230
C***** PLOTPACK
C***** STARPLOT
      BOX=0.008*(PAPERX1-PAPERX0)*SIZPLT
      HT =0.012*(PAPERX1-PAPERX0)*SIZPLT
C***** STARPLOT
      GO TO 40                                                          PLO10240
C                                                                       PLO10250
C     FULL SIZE PLOTS.                                                  PLO10260
C                                                                       PLO10270
   30 SIZPLT=1.0                                                        PLO10280
C***** PLOTPACK
C     XINCH(1)=0.0                                                      PLO10290
C     XINCH(2)=8.0                                                      PLO10300
C     YINCH(1)=1.0                                                      PLO10310
C     YINCH(2)=9.0                                                      PLO10320
C     BOX=0.10                                                          PLO10330
C     HT=0.12                                                           PLO10340
C***** PLOTPACK
C***** STARPLOT
      XINCH(1)=PAPERX0
      XINCH(2)=PAPERX1*0.65
      YINCH(1)=PAPERY0
      YINCH(2)=PAPERY1
      BOX=0.008*(PAPERX1-PAPERX0)
      HT =0.012*(PAPERX1-PAPERX0)
C***** STARPLOT
C-----DEFINE X AND Y SIZE OF PLOTS (ONLY TO DEFINE PAPER PLOTTER        PLO10350
C-----ADVANCE AND MAXIMUM SIZE FOR HEWLETT-PACKARD PLOTTER).            PLO10360
   40 WT=HT                                                             PLO10370
C***** PLOTPACK
C     XHP(1)=0.0                                                        PLO10380
C     XHP(2)=8.0                                                        PLO10390
C     YHP(1)=1.0                                                        PLO10400
C     YHP(2)=9.0                                                        PLO10410
C***** PLOTPACK
C***** STARPLOT
      XHP(1)=PAPERX0
      XHP(2)=PAPERX1*0.6
      YHP(1)=PAPERY0
      YHP(2)=PAPERY1
C***** STARPLOT
C-----DEFINE DISTANCE TO ADVANCE IN X DIRECTION BETWEEN PLOTS.          PLO10420
      ADVANC=1.75*XHP(2)                                                PLO10430
C                                                                       PLO10440
C     INITIALIZE PLOTTER                                                PLO10450
C                                                                       PLO10460
C     IBMPC IS USED TO INDICATE WHETHER THE PROGRAM IS RUNNING ON THE   PLO10470
C     MAINFRAME OR ON AN IBM-PC.                                        PLO10480
C                                                                       PLO10490
C     IBMPC IS INITIALIZED TO -1 IN THE MAIN AND IF RUNNING ON THE      PLO10500
C     MAINFRAME IT WILL NOT CHANGE.                                     PLO10510
C                                                                       PLO10520
C     IF RUNNING ON AN IBM-PC THE PLOTTER INTERFACE ROUTINE PLOTS WILL  PLO10530
C     CHANGE THE VALUE OF IBM AS FOLLOWS,                               PLO10540
C                                                                       PLO10550
C     IBMPC = 0 - PC FULL SIZE PLOTS.                                   PLO10560
C           = 1 - PC HALF SIZE PLOTS.                                   PLO10570
C                                                                       PLO10580
C***** STARPLOT
      CALL STARPLOT
C***** STARPLOT END
C***** PLOTPACK
C     CALL PLOTS(BUFF,5000,16)                                          PLO10590
C***** PLOTPACK
C-----DEFINE ALL BOX AND CHARACTER HEIGHTS AND WIDTHS (ALL SCALED TO    PLO10600
C-----BOX AND HT DEFINED ABOVE).                                        PLO10610
      BOX2=BOX/2.0                                                      PLO10620
      BOX4=BOX/4.0                                                      PLO10630
      BOXWT2=4.0*BOX/7.0                                                PLO10640
      HT2=2.0*HT                                                        PLO10650
      HTH=HT/2.0                                                        PLO10660
      HT34=3.0*HT/4.0                                                   PLO10670
      WTH=WT/2.0                                                        PLO10680
      WT38=3.0*WT/8.0                                                   PLO10690
C-----DEFINE POSITION OF PLOT LABELS.                                   PLO10700
      CALL SPOTER                                                       PLO10710
C-----CONSTRUCT ONE DECADE OF TABLE OF LOGS (FOR AXIS LABELS).          PLO10720
      DO 50 I=1,10                                                      PLO10730
      XL=I                                                              PLO10740
   50 TABLOG(I)=ALOG10(XL)                                              PLO10750
C                                                                       PLO10760
C     IF RUNNING ON MAINFRAME IDENTIFY PROGRAM AND INSTALLATION.        PLO10770
C                                                                       PLO10780
      IF(IBMPC.GE.0) GO TO 70                                           PLO10790
      CALL PEN(2)                                                       PLO10800
      Y=0.5*(YHP(1)+YHP(2))+2.0*HTV                                     PLO10810
      XMID=0.5*(XHP(1)+XHP(2))                                          PLO10820
      DO 60 L=1,4                                                       PLO10830
      X=XMID-0.5*FLOAT(NVERSE(L))*WTV                                   PLO10840
      CALL SYMBLH(X,Y,HTV,VERSES(1,L),0.0,NVERSE(L))                    PLO10850
   60 Y=Y-1.75*HTV                                                      PLO10860
C-----ADVANCE TO NEXT PLOTTING AREA.                                    PLO10870
C***** STARPLOT
      CALL NEXTPLOT
C***** STARPLOT
C***** PLOTPACK
C     CALL PLOT(ADVANC,0.0,-3)                                          PLO10880
C***** PLOTPACK
   70 RETURN                                                            PLO10890
      END                                                               PLO10900
      SUBROUTINE NXTPLT                                                 PLO10910
C                                                                       PLO10920
C     ADVANCE TO NEXT PLOTTER AREA.                                     PLO10930
C                                                                       PLO10940
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO10950
      COMMON/HPINCH/XHP(2),YHP(2)                                       PLO10960
      COMMON/INCHTB/XPINCH(2,4),YPINCH(2,4)                             PLO10970
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO10980
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC                                 PLO10990
      COMMON/PLOTN/NUMPLT,ADVANC                                        PLO11000
C-----MOVE TO ORIGIN WITH PEN UP.                                       PLO11010
      CALL PLOT(0.0,0.0,3)                                              PLO11020
C-----IF FULL PLOT SIZE ADVANCE TO NEXT PLOT.                           PLO11030
      IF(IPLOTZ.EQ.0) GO TO 10                                          PLO11040
C-----DEFINE COORDINATES OF NEXT SUB-PLOT.                              PLO11050
      IPLOTZ=IPLOTZ+1                                                   PLO11060
      II=IPLOTZ                                                         PLO11070
      IF(IPLOTZ.GT.4) IPLOTZ=1                                          PLO11080
      XINCH(1)=XPINCH(1,IPLOTZ)                                         PLO11090
      XINCH(2)=XPINCH(2,IPLOTZ)                                         PLO11100
      YINCH(1)=YPINCH(1,IPLOTZ)                                         PLO11110
      YINCH(2)=YPINCH(2,IPLOTZ)                                         PLO11120
C-----DEFINE POSITION OF PLOT LABELS.                                   PLO11130
      CALL SPOTER                                                       PLO11140
C-----IF LAST PLOT IS FULL, ADVANCE TO NEXT PLOTTING AREA.              PLO11150
      IF(II.LE.4) GO TO 20                                              PLO11160
C-----END OF PLOT. ADVANCE TO NEXT PLOTTING AREA.                       PLO11170
C***** STARPLOT
   10 CALL NEXTPLOT
C***** STARPLOT
C***** PLOTPACK
C  10 CALL PLOT(ADVANC,0.0,-3)                                          PLO11180
C***** PLOTPACK
   20 RETURN                                                            PLO11190
      END                                                               PLO11200
      SUBROUTINE SPOTER                                                 PLO11210
C                                                                       PLO11220
C     DEFINE POISITION OF AXIS LABELS AND LEGEND BOX COORDINATES.       PLO11230
C                                                                       PLO11240
C     TOP1   = Y POSITION OF TOP TITLE LINE                             PLO11250
C     TOP2   = Y POSITION OF SECOND TITLE LINE                          PLO11260
C     TOP3   = Y POSITION OF THIRD TITLE LINE                           PLO11270
C     BOT1   = Y POSITION OF X AXIS NUMBERS (LINEAR)                    PLO11280
C            = Y POSITION OF X AXIS LOG EXPONENTS (LOG)                 PLO11290
C     BOT2   = Y POSITION OF X AXIS NUMBERS (LOG)                       PLO11300
C     BOT3   = Y POSITION OF X AXIS LABEL                               PLO11310
C     RIGHT1 = X POSITION OF Y AXIS NUMBERS                             PLO11320
C     RIGHT2 = X POSITION OF Y AXIS LABEL                               PLO11330
C     RIGHT3 = X POSITION OF START OF LEGEND BOX                        PLO11340
C     RIGHT4 = X POSITION OF END OF LEGEND BOX                          PLO11350
C     RIGHT5 = X POSITION OF REFERENCE/ENERGY RANGE TITLE               PLO11360
C     RIGHT6 = X POSITION OF POINTS TITLE                               PLO11370
C     RIGHT7 = X POSITION OF START OF BOX TO IDENTIFY REFERENCE         PLO11380
C     RIGHT8 = X POSITION OF END OF BOX TO IDENTIFY REFERENCE           PLO11390
C                                                                       PLO11400
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO11410
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO11420
      COMMON/SPOTS/TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGHT1,RIGHT2,RIGHT3,  PLO11430
     1 RIGHT4,RIGHT5,RIGHT6,RIGHT7,RIGHT8                               PLO11440
      TOP3=YINCH(2)+HT                                                  PLO11450
      TOP2=TOP3+1.75*HT                                                 PLO11460
      TOP1=TOP2+1.75*HT                                                 PLO11470
      BOT1=YINCH(1)-1.75*HT                                             PLO11480
      BOT2=YINCH(1)-2.75*HT                                             PLO11490
      BOT3=YINCH(1)-4.5*HT                                              PLO11500
      RIGHT1=XINCH(2)+WT                                                PLO11510
      RIGHT2=XINCH(2)+7.0*HT                                            PLO11520
      RIGHT3=XINCH(2)+8.0*WT                                            PLO11530
      RIGHT4=RIGHT3+4.0*BOX+30.0*WT                                     PLO11540
      RIGHT5=RIGHT3+4.0*BOX+WT                                          PLO11550
      RIGHT6=RIGHT5+22.0*WT                                             PLO11560
      RIGHT7=RIGHT3+2.0*BOX                                             PLO11570
      RIGHT8=RIGHT7+2.0*BOX                                             PLO11580
      RETURN                                                            PLO11590
      END                                                               PLO11600
      SUBROUTINE READIN                                                 PLO11610
C                                                                       PLO11620
C     READ ALL INPUT PARAMETERS AND INITIALIZE PLOTTER.                 PLO11630
C                                                                       PLO11640
      INTEGER OUTP                                                      PLO11650
      CHARACTER*4 LIBNAM,ENDF,EXFOR,ANSWER,BLANK,SCALEM,ENDFAN,GRIDTY,  PLO11660
     1 HSIZE                                                            PLO11670
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO11680
C     COMMON/REFERI/LREF(26),EXLOW(26),EXHIGH(26),E2T(26),IREF,MREF,    PLO11690
C    1 MAXREF,IGROUP,KGROUP                                             PLO11700
C***** OLD
C***** TRKOV
      CHARACTER*80 LINE
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP                                             PLO08450
C***** TRKOV
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC                                 PLO11710
      COMMON/LIBI/ILIB                                                  PLO11720
      COMMON/LIBC/LIBNAM(4)                                             PLO11730
C***** OLD
C     COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),        PLO11740
C    1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),NGET,IGET              PLO11750
C***** OLD
C***** TRKOV
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
C***** TRKOV
      COMMON/MODEMY/MYMODE                                              PLO11760
      COMMON/GRIDDY/MYGRID                                              PLO11770
      COMMON/INPARM/MINNIE,MAXIE                                        PLO11780
      COMMON/SYMBLM/MSYMBL                                              PLO11790
      COMMON/EXERRS/IXERR,IYERR                                         PLO11800
      COMMON/SCALEI/ISCALE                                              PLO11810
      COMMON/ENDFIM/IMENDF                                              PLO11820

      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO11410
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO11420

      DIMENSION EXFOR(2),ANSWER(2),SCALEM(3,3),ENDFAN(4,3),GRIDTY(3,2), PLO11830
     1 HSIZE(2)                                                         PLO11840
      DATA ENDFAN/                                                      PLO11850
     1 '  NO','    ','    ','    ',                                     PLO11860
     2 ' YES','    ','    ','    ',                                     PLO11870
     3 ' YES',' (ID','ENTI','FY) '/                                     PLO11880
      DATA ANSWER/' NO','YES'/                                          PLO11890
      DATA ENDF/'ENDF'/                                                 PLO11900
      DATA EXFOR/'  EX','FOR '/                                         PLO11910
      DATA BLANK/'    '/                                                PLO11920
      DATA SCALEM/                                                      PLO11930
     1 'ENDF',' + E','XFOR',                                            PLO11940
     2 '    ','    ','ENDF',                                            PLO11950
     3 '    ','   E','XFOR'/                                            PLO11960
      DATA GRIDTY/                                                      PLO11970
     1 'TICK',' MAR','KS  ',                                            PLO11980
     2 ' FUL','L GR','ID  '/                                            PLO11990
      DATA HSIZE/'FULL','HALF'/                                         PLO12000
C-----DEFINE UPPER DEFAULT ENERGY TO BE 100 MEV.                        PLO12010
      DATA EMAX/1.0E+10/                                                PLO12020
C-----READ INPUT PARAMTERS.                                             PLO12030
C***** OLD
C     READ(INP,4000) IMENDF,IMX4,ISCALE,IXERR,IYERR,MSYMBL,IGROUP,      PLO12040
C    1 MINNIE,MAXIE,MYGRID,IPLOTZ,LIBNAM                                PLO12050
C***** OLD
C***** TRKOV
      READ(INP,4000) IMENDF,MAXREF,ISCALE,IXERR,IYERR,MSYMBL,IGROUP,
     1 MINNIE,MAXIE,MYGRID,IPLOTZ,LIBNAM
      IMX4=0
      IF(MAXREF.LT.1) MAXREF=1
      IF(MAXREF.GT.1) IMX4=1
C***** TRKOV
      IF(IMENDF.GT.2) IMENDF=2                                          PLO12060
      IF(IMENDF.LE.0) IMENDF=0                                          PLO12070
C***** OLD
C     IF(IMX4.NE.0) IMX4=1                                              PLO12080
C***** OLD
      IF(ISCALE.LT.0.OR.ISCALE.GT.2) ISCALE=0                           PLO12090
      IF(IMENDF.EQ.0) ISCALE=2                                          PLO12100
      IF(IXERR.NE.0) IXERR=1                                            PLO12110
      IF(IYERR.NE.0) IYERR=1                                            PLO12120
      IF(MSYMBL.NE.0) MSYMBL=1                                          PLO12130
      IF(IGROUP.NE.0) IGROUP=1                                          PLO12140
      IF(MINNIE.LE.0) MINNIE=8                                          PLO12150
C***** OLD
C     IF(MAXIE.LE.MINNIE) MAXIE=1000                                    PLO12160
C***** OLD
C***** TRKOV
      IF(MAXIE.LE.MINNIE) MAXIE=10000
C***** TRKOV
      IF(MYGRID.NE.0) MYGRID=1                                          PLO12170
      IF(IPLOTZ.NE.0) IPLOTZ=1                                          PLO12180
      WRITE(OUTP,6000) (ENDFAN(I,IMENDF+1),I=1,4),ANSWER(IMX4+1),       PLO12190
     1 (SCALEM(I,ISCALE+1),I=1,3),ANSWER(IXERR+1),ANSWER(IYERR+1),      PLO12200
     2 ANSWER(MSYMBL+1),ANSWER(IGROUP+1),MINNIE,MAXIE,                  PLO12210
     3 (GRIDTY(I,MYGRID+1),I=1,3),HSIZE(IPLOTZ+1)                       PLO12220
C-----DEFINE INTERNAL RUN MODE.                                         PLO12230
      MYMODE=IMX4+2*IMENDF                                              PLO12240
      IF(IMENDF.EQ.2) MYMODE=IMX4+2                                     PLO12250
      IF(MYMODE.LT.2) GO TO 30                                          PLO12260
C-----LEFT ADJUST ENDF/B LIBRARY I.D....IF COMPLETELY BLANK DEFINE      PLO12270
C-----LIBRARY I.D. TO BE ENDF.                                          PLO12280
      CALL SHIFTY(LIBNAM,ILIB)                                          PLO12290
      IF(ILIB.GT.0) GO TO 20                                            PLO12300
      ILIB=4                                                            PLO12310
      LIBNAM(1)=ENDF                                                    PLO12320
      DO 10 I=2,4                                                       PLO12330
   10 LIBNAM(I)=BLANK                                                   PLO12340
   20 WRITE(OUTP,6010) LIBNAM                                           PLO12350
      GO TO 40                                                          PLO12360
C-----INITIALIZE LIBRARY I.D. TO EXFOR (ADD AN/SAN LATER).              PLO12370
   30 LIBNAM(1)=EXFOR(1)                                                PLO12380
      LIBNAM(2)=EXFOR(2)                                                PLO12390
C-----DEFINE ALL PLOTTER PARAMETERS AND INITIALIZE PLOTTER.             PLO12400
   40 CALL SETUP                                                        PLO12410
C-----LIMIT THE NUMBER OF DISPLAYED REFERENCES BY AVAILABLE SPACE
C***** TRKOV
       MXR=(YINCH(2)-YINCH(1))/(1.75*HT)-2
       MAXREF=MIN(MXR,MAXREF,48)
C***** TRKOV
C-----IDENTIFY COMPUTER THAT PROGRAM IS RUNNING ON.                     PLO12420
      IF(IBMPC.LT.0) WRITE(OUTP,6012)                                   PLO12430
      IF(IBMPC.GE.0) WRITE(OUTP,6014)                                   PLO12440
C-----PRINT TITLE BEFORE FIRST REQUEST.                                 PLO12450
      WRITE(OUTP,6020)                                                  PLO12460
C***** TRKOV - INTERPRET BLANK AS END OF INPUT
      NGET=0
      DO 70 JGET=1,100
      NGET=NGET+1
   50 READ(INP,4020,END=80) LINE
      IF(LINE( 1:26).EQ.'                          '.AND.
     1   LINE(27:52).EQ.'                          ') GO TO 80
      READ(LINE,4010,ERR=80) IZALOW(NGET),MFLOW(NGET),
     1 MTLOW(NGET),ELGET(NGET),IZAHI(NGET),MFHI(NGET),MTHI(NGET),
     2 EHGET(NGET),INTRN(NGET),EPGET(NGET)
C***** TRKOV
C***** OLD
C     DO 70 NGET=1,100                                                  PLO12470
C  50 READ(INP,4010,END=80,ERR=80) IZALOW(NGET),MFLOW(NGET),            PLO12480
C    1 MTLOW(NGET),ELGET(NGET),IZAHI(NGET),MFHI(NGET),MTHI(NGET),       PLO12490
C    2 EHGET(NGET)                                                      PLO12500
C***** OLD
C-----IF COMPARING TO ENDF/B SET MF = 3 (ONLY CROSS SECTIONS) AND       PLO12510
C----- MT = ONLY UP TO 999.                                             PLO12520
      IF(MYMODE.LT.2) GO TO 60                                          PLO12530
C***** TRKOV - ELIMINATE OUT-OF-RANGE ENTRIES
      IF(MFHI(NGET).LT.3 .OR. MFLOW(NGET).GT.6) GO TO 50
C***** TRKOV
      IF(MFLOW(NGET).LT.3) MFLOW(NGET)=3                                PLO12540
C***** OLD
C     IF(MFHI(NGET).GT.3) MFHI(NGET)=3                                  PLO12550
C     IF(MTLOW(NGET).GT.999) MTLOW(NGET)=999                            PLO12560
C     IF(MTHI(NGET).GT.999) MTHI(NGET)=999                              PLO12570
C***** OLD
C***** TRKOV
      IF(MFHI(NGET).GT.6) MFHI(NGET)=6
C***** TRKOV
C-----IF REQUIRED SET UPPER ZA/MF/MT LIMIT TO LOWER LIMIT.              PLO12580
   60 IF(IZAHI(NGET).LT.IZALOW(NGET)) IZAHI(NGET)=IZALOW(NGET)          PLO12590
      IF(MTHI(NGET).LT.MTLOW(NGET)) MTHI(NGET)=MTLOW(NGET)              PLO12600
      IF(MFHI(NGET).LT.MFLOW(NGET)) MFHI(NGET)=MFLOW(NGET)              PLO12610
C-----IF REQUIRED SET UPPER ENERGY LIMIT TO 100 MEV.                    PLO12620
      IF(EHGET(NGET).LE.0.0) EHGET(NGET)=EMAX                           PLO12630
C-----PRINT REQUEST.                                                    PLO12640
      WRITE(OUTP,6030) IZALOW(NGET),MFLOW(NGET),MTLOW(NGET),            PLO12650
     1 ELGET(NGET),IZAHI(NGET),MFHI(NGET),MTHI(NGET),EHGET(NGET)        PLO12660
      IF(EHGET(NGET).GT.ELGET(NGET)) GO TO 70                           PLO12670
      WRITE(OUTP,6040)                                                  PLO12680
      GO TO 50                                                          PLO12690
   70 CONTINUE                                                          PLO12700
      NGET=101                                                          PLO12710
   80 NGET=NGET-1                                                       PLO12720
      IF(NGET.GT.0) GO TO 100                                           PLO12730
C-----NO REQUESTS. DEFINE DEFAULT REQUEST TO PLOT AS MUCH DATA AS       PLO12740
C-----POSSIBLE.                                                         PLO12750
      IZALOW(1)=1                                                       PLO12760
      IZAHI(1)=999999                                                   PLO12770
      MFLOW(1)=1                                                        PLO12780
      MFHI(1)=999                                                       PLO12790
      MTLOW(1)=1                                                        PLO12800
      MTHI(1)=9999                                                      PLO12810
      ELGET(1)=0.0                                                      PLO12820
      EHGET(1)=EMAX                                                     PLO12830
      NGET=1                                                            PLO12840
C***** OLD
C     IF(MYMODE.LT.2) GO TO 90                                          PLO12850
C     MFLOW(1)=3                                                        PLO12860
C     MFHI(1)=3                                                         PLO12870
C     MTHI(1)=999                                                       PLO12880
C***** OLD
   90 WRITE(OUTP,6030) IZALOW(NGET),MFLOW(NGET),MTLOW(NGET),            PLO12890
     1 ELGET(NGET),IZAHI(NGET),MFHI(NGET),MTHI(NGET),EHGET(NGET)        PLO12900
  100 RETURN                                                            PLO12910
 4000 FORMAT(11I5,4A4)                                                  PLO12920
C***** OLD
C4010 FORMAT(I7,2I4,E11.4,I7,2I4,E11.4)                                 PLO12930
C***** OLD
C***** TRKOV
 4010 FORMAT(I7,2I4,E11.4,I7,2I4,E11.4,I3,E11.4)
C***** TRKOV
 4020 FORMAT(A80)
C***** TRKOV
 6000 FORMAT(1X,72('=')/' READING INPUT PARAMETERS'/1X,72('=')/         PLO12940
     1 ' COMPARE EXFOR DATA TO ENDF/B DATA----------',12X,4A4/          PLO12950
     2 ' ALL COMPARABLE EXFOR DATA ON SAME PLOT-----',13X,A3/           PLO12960
     3 ' SCALE PLOTS ACCORDING TO-------------------',4X,3A4/           PLO12970
     4 ' PLOT X ERROR BARS--------------------------',13X,A3/           PLO12980
     5 ' PLOT Y ERROR BARS--------------------------',13X,A3/           PLO12990
     6 ' IDENTIFY ALL REFERENCES BY SYMBOL----------',13X,A3/           PLO13000
     7 ' ALLOW VARIABLE E2 ON SAME PLOT-------------',13X,A3/           PLO13010
     8 ' MINIMUM EXFOR POINTS PER PLOT--------------',I16/              PLO13020
     9 ' MAXIMUM EXFOR POINTS PER PLOT--------------',I16/              PLO13030
     9 ' GRID TYPE----------------------------------',6X,2A4,A2/        PLO13040
     A ' PLOT SIZE----------------------------------',12X,A4)           PLO13050
 6010 FORMAT(' EVALUATED DATA I.D.------------------------',4A4)        PLO13060
 6012 FORMAT(' COMPUTER TYPE------------------------------',            PLO13070
     1 '       MAINFRAME')                                              PLO13080
 6014 FORMAT(' COMPUTER TYPE------------------------------',            PLO13090
     1 '          IBM-PC')                                              PLO13100
 6020 FORMAT(1X,72('=')/' PLOT THE FOLLOWING DATA'/1X,72('=')/          PLO13110
     1 '           LOWER LIMIT               UPPER LIMIT'/              PLO13120
     1 '      ZA  MF  MT  ENERGY-EV     ZA  MF  MT  ENERGY-EV'/         PLO13130
     3 1X,72('='))                                                      PLO13140
 6030 FORMAT(1X,I7,2I4,1PE11.4,I7,2I4,1PE11.4)                          PLO13150
 6040 FORMAT(' UPPER ENERGY LESS THAN LOWER ENERGY...REQUEST IGNORED')  PLO13160
      END                                                               PLO13170
      SUBROUTINE SHIFTY(LIBNAM,ILIB)                                    PLO13180
C                                                                       PLO13190
C     LEFT ADJUST LIBRARY I.D.                                          PLO13200
C                                                                       PLO13210
      CHARACTER*1 LIBNAM,BLANK                                          PLO13220
      DIMENSION LIBNAM(16)                                              PLO13230
      DATA BLANK/' '/                                                   PLO13240
C-----SKIP LEADING BLANKS.                                              PLO13250
      DO 10 I=1,16                                                      PLO13260
      IF(LIBNAM(I).NE.BLANK) GO TO 20                                   PLO13270
   10 CONTINUE                                                          PLO13280
C-----NAME IS COMPLETELY BLANK                                          PLO13290
      ILIB=0                                                            PLO13300
      RETURN                                                            PLO13310
C-----LEFT ADJUST NAME.                                                 PLO13320
   20 II=0                                                              PLO13330
      ILIB=0                                                            PLO13340
      DO 30 J=I,16                                                      PLO13350
      II=II+1                                                           PLO13360
      LIBNAM(II)=LIBNAM(J)                                              PLO13370
      IF(LIBNAM(II).NE.BLANK) ILIB=II                                   PLO13380
   30 CONTINUE                                                          PLO13390
C-----BLANK OUT REMAINDER (IF ANY).                                     PLO13400
      IF(ILIB.GE.16) GO TO 50                                           PLO13410
      II=ILIB+1                                                         PLO13420
      DO 40 I=II,16                                                     PLO13430
   40 LIBNAM(I)=BLANK                                                   PLO13440
   50 RETURN                                                            PLO13450
      END                                                               PLO13460
      SUBROUTINE GETEV(IEND)                                            PLO13470
C                                                                       PLO13480
C     FIND NEXT SECTION OF REQUESTED ENDF/B CROSS SECTIONS (MF=3).      PLO13490
C                                                                       PLO13500
      INTEGER OUTP                                                      PLO13510
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO13520
C     COMMON/PAGEXY/XPAGE(9000),YPAGE(9000),N2,IBASE,ITOP,ISCR          PLO08310
C     COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX                           PLO13540
C***** OLD
C***** TRKOV
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/PAGEXY/XPAGE(90000),YPAGE(90000),N2,IBASE,ITOP,ISCR          PLO08310
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX,AWR
C***** TRKOV
C***** OLD
C     COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),        PLO13550
C    1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),NGET,IGET              PLO13560
C***** OLD
C***** TRKOV
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
C***** TRKOV
C***** TRKOV
      SAVE IPASS,IMF6
C***** TRKOV
      DATA IPASS/0/                                                     PLO13570
      DATA MATLST/-99999/                                               PLO13580
C-----DURING FIRST PASS SKIP ENDF/B TAPE LABEL.                         PLO13590
      IF(IPASS.EQ.0) READ(ITAPE2,1000,ERR=100,END=100) HEADER           PLO13600
      IPASS=1                                                           PLO13610
C***** TRKOV
      IMF6=0
C***** TRKOV
      IEND=0                                                            PLO13620
C-----FIND BEGINNING OF NEXT SECTION.                                   PLO13630
   10 READ(ITAPE2,1010) C1,C2,L1,L2,N1,N2,MAT,MF,MT                     PLO13640
      IF(MT) 20,20,30                                                   PLO13650
   20 IF(MAT) 110,10,10                                                 PLO13660
C-----DEFINE EVALUATION MOD. FROM FIRST CARD OF HOLLERITH SECTION.      PLO13670
   30 IF(MAT.EQ.MATLST) GO TO 40                                        PLO13680
      MATLST=MAT                                                        PLO13690
      MODIZA=0                                                          PLO13700
      IF(MF.EQ.1.AND.MT.EQ.451) MODIZA=N2                               PLO13710
C-----SEARCH FOR SECTION OF FILE 3.                                     PLO13720
C***** OLD
C  40 IF(MF-3) 50,60,50                                                 PLO13730
C***** OLD
C***** TRKOV - Extension to process MF4/5/6
   40 AWR=C2
      IF(MF.LT.3 .OR. MF.GT.6) GO TO 50
      GO TO 60
C***** TRKOV
C                                                                       PLO13740
C     SKIP SECTION.                                                     PLO13750
C                                                                       PLO13760
   50 READ(ITAPE2,1040) MAT,MF,MT                                       PLO13770
      IF(MT) 10,10,50                                                   PLO13780
C                                                                       PLO13790
C     CHECK ZA/MF/MT AGAINST REQUESTS.                                  PLO13800
C                                                                       PLO13810
   60 IZA=C1                                                            PLO13820
      CALL RQUEST(IGET,IZA,MF,MT)                                       PLO13830
      IF(IGET) 50,50,70                                                 PLO13840
C... MAXIMUM NUMBER OF POINTS EXTENDED BY TRKOV
C                                                                       PLO13850
C     DATA IS REQUESTED. READ DATA UP TO 90000 POINTS AT A TIME. IF OVER PLO13860
C     90000 POINTS STORE ALL IN PAGING SYSTEM.                           PLO13870
C                                                                       PLO13880
C***** OLD
C  70 READ(ITAPE2,1010) C1,C2,L1,L2,N1,N2,MAT,MF,MT                     PLO13890
C***** OLD
C-----CHECK FOR MF3/4/5/6 FILE DATA
C-----IF MF4/5/6, SET THE IMF6 FLAG BUT LOAD THE DATA AFTER EXFOR
C***** TRKOV - Extension to process MF4/5/6
   70 IF(MF.EQ.3) GO TO 78
      IF(MF.LT.4 .OR. MF.GT.6) GO TO 50
      IF(MF.NE.4) MT=9000
   76 IMF6=1
      IEND=0
   77 READ(ITAPE2,1040) MAT,MFS,MTS
      IF(MFS.NE.0) GO TO 77
      RETURN
C***** TRKOV
C-----PROCESS FILE MF3 DATA
   78 READ(ITAPE2,1010) C1,C2,L1,L2,N1,N2,MAT,MF,MT                     PLO13890
C***** OLD
C     READ(ITAPE2,1020) (NBT,INT,I=1,N1)                                PLO13900
C***** OLD
C***** TRKOV
      READ(ITAPE2,1020) (NBT,INTI,I=1,N1)
C***** TRKOV
      IF(N1.GT.1 .OR. INTI.NE.2) GO TO 102
      IF(N2.GT.90000) REWIND ISCR
      DO 80 I=1,N2,90000
      II=I+90000-1
C***** TRKOV
C***** OLD
C     IF(N2.GT.9000) REWIND ISCR                                        PLO13910
C     DO 80 I=1,N2,9000                                                 PLO13920
C     II=I+9000-1                                                       PLO13930
C***** OLD
      IF(II.GT.N2) II=N2                                                PLO13940
C***** OLD
C     READ(ITAPE2,1030) (XPAGE(J),YPAGE(J),J=I,II)                      PLO13950
C***** OLD
C***** TRKOV - FIX INDEX ERROR
      READ(ITAPE2,1030) (XPAGE(J-I+1),YPAGE(J-I+1),J=I,II)
C***** TRKOV - END
C***** TRKOV
      IF(N2.GT.90000) WRITE(ISCR) XPAGE,YPAGE
C***** TRKOV
C***** OLD
C     IF(N2.GT.9000) WRITE(ISCR) XPAGE,YPAGE                            PLO13960
C***** OLD
   80 CONTINUE                                                          PLO13970
      IBASE=0                                                           PLO13980
C***** TRKOV
      IF(N2.GT.90000) GO TO 90
C***** TRKOV
C***** OLD
C     IF(N2.GT.9000) GO TO 90                                           PLO13990
C***** OLD
      ITOP=N2                                                           PLO14000
      RETURN                                                            PLO14010
C-----LOAD FIRST PAGE FROM SCRATCH AND DEFINE IN CORE PAGE LIMITS.      PLO14020
   90 END FILE ISCR                                                     PLO14030
      REWIND ISCR                                                       PLO14040
      READ(ISCR) XPAGE,YPAGE                                            PLO14050
C***** TRKOV
      ITOP=90000                                                         PLO14060
C***** TRKOV
C***** OLD
C     ITOP=9000                                                         PLO14060
C***** OLD
      RETURN                                                            PLO14070
C-----ERROR OR END OF FILE WHILE READING ENDF/B DATA.                   PLO14080
  100 WRITE(OUTP,6000)                                                  PLO14090
      STOP                                                              PLO14100
C***** TRKOV
  102 WRITE(OUTP,6010)
      STOP
C***** TRKOV
C-----END OF ENDF/B DATA (BASED ON MAT).                                PLO14110
  110 IEND=1                                                            PLO14120
      RETURN                                                            PLO14130
 1000 FORMAT(17A4,A4)                                                   PLO14140
 1010 FORMAT(2E11.4,4I11,I4,I2,I3)                                      PLO14150
 1020 FORMAT(2I11)                                                      PLO14160
 1030 FORMAT(6E11.4)                                                    PLO14170
 1040 FORMAT(66X,I4,I2,I3)                                              PLO14180
 6000 FORMAT(' ERROR READING ENDF/B DATA...EXECUTION TERMINATED')       PLO14190
C***** TRKOV
 6010 FORMAT(' ERROR - ENDF DATA NOT LIN.INTERPOLABLE'
     1      ,'...EXECUTION TERMINATED')
C***** TRKOV
      END                                                               PLO14200
      FUNCTION X(I)                                                     PLO14210
C                                                                       PLO14220
C     RETRIEVE ENDF/B ENERGY FROM PAGING SYSTEM.                        PLO14230
C                                                                       PLO14240
      INTEGER OUTP                                                      PLO14250
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO14260
C     COMMON/PAGEXY/XPAGE(9000),YPAGE(9000),N2,IBASE,ITOP,ISCR          PLO08310
C***** OLD
C***** TRKOV
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/PAGEXY/XPAGE(90000),YPAGE(90000),N2,IBASE,ITOP,ISCR
C***** TRKOV
      DATA IERR/0/                                                      PLO14280
C-----INSURE INDEX IS IN LEGAL RANGE.                                   PLO14290
      IF(I.GT.0.AND.I.LE.N2) GO TO 10                                   PLO14300
      WRITE(OUTP,6000) I,N2                                             PLO14310
      IERR=IERR+1                                                       PLO14320
      IF(IERR.LT.50) RETURN                                             PLO14330
      STOP                                                              PLO14340
C-----SEE IF REQUESTED POINT PRECEEDS POINTS IN CORE.                   PLO14350
   10 IF(I-IBASE) 20,20,40                                              PLO14360
C-----REWIND AND INITIALIZE UPPER PAGE INDEX.                           PLO14370
   20 REWIND ISCR                                                       PLO14380
C***** TRKOV
      ITOP=0
C***** TRKOV
C***** OLD
C     ITOP=9000                                                         PLO14390
C***** OLD
C-----LOAD NEXT PAGE INTO CORE.                                         PLO14400
   30 READ(ISCR) XPAGE,YPAGE                                            PLO14410
      IBASE=ITOP                                                        PLO14420
C***** TRKOV
      ITOP=ITOP+90000                                                    PLO14430
C***** TRKOV
C***** OLD
C     ITOP=ITOP+9000                                                    PLO14430
C***** OLD
C-----SEE IF REQUESTED POINT FOLLOWS POINTS IN CORE.                    PLO14440
   40 IF(I-ITOP) 50,50,30                                               PLO14450
C-----POINT IS IN CORE. DEFINE IT.                                      PLO14460
   50 INCORE=I-IBASE                                                    PLO14470
      X=XPAGE(INCORE)                                                   PLO14480
      RETURN                                                            PLO14490
 6000 FORMAT(' FUNCTION X...I=',I6,' (MUST BE 1 TO',I6,') DEFINED X=0') PLO14500
      END                                                               PLO14510
      FUNCTION Y(I)                                                     PLO14520
C                                                                       PLO14530
C     RETRIEVE ENDF/B CROSS SECTION FROM PAGING SYSTEM.                 PLO14540
C                                                                       PLO14550
      INTEGER OUTP                                                      PLO14560
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO14570
C     COMMON/PAGEXY/XPAGE(9000),YPAGE(9000),N2,IBASE,ITOP,ISCR          PLO08310
C***** OLD
C***** TRKOV
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/PAGEXY/XPAGE(90000),YPAGE(90000),N2,IBASE,ITOP,ISCR          PLO08310
C***** TRKOV
      DATA IERR/0/                                                      PLO14590
C-----INSURE INDEX IS IN LEGAL RANGE.                                   PLO14600
      IF(I.GT.0.AND.I.LE.N2) GO TO 10                                   PLO14610
      WRITE(OUTP,6000) I,N2                                             PLO14620
      IERR=IERR+1                                                       PLO14630
      IF(IERR.LT.50) RETURN                                             PLO14640
      STOP                                                              PLO14650
C-----SEE IF REQUESTED POINT PRECEEDS POINTS IN CORE.                   PLO14660
   10 IF(I-IBASE) 20,20,40                                              PLO14670
C-----REWIND AND INITIALIZE UPPER PAGE INDEX.                           PLO14680
   20 REWIND ISCR                                                       PLO14690
C***** TRKOV
      ITOP=0
C***** TRKOV
C***** OLD
C     ITOP=9000                                                         PLO14700
C***** OLD
C-----LOAD NEXT PAGE INTO CORE.                                         PLO14710
   30 READ(ISCR) XPAGE,YPAGE                                            PLO14720
      IBASE=ITOP                                                        PLO14730
C***** TRKOV
      ITOP=ITOP+90000                                                    PLO14740
C***** TRKOV
C***** OLD
C     ITOP=ITOP+9000                                                    PLO14740
C***** OLD
C-----SEE IF REQUESTED POINT FOLLOWS POINTS IN CORE.                    PLO14750
   40 IF(I-ITOP) 50,50,30                                               PLO14760
C-----POINT IS IN CORE. DEFINE IT.                                      PLO14770
   50 INCORE=I-IBASE                                                    PLO14780
      Y=YPAGE(INCORE)                                                   PLO14790
      RETURN                                                            PLO14800
 6000 FORMAT(' FUNCTION Y...I=',I6,' (MUST BE 1 TO',I6,') DEFINED X=0') PLO14810
      END                                                               PLO14820
      SUBROUTINE RQUEST(KGET,IZA,MF,MT)                                 PLO14830
C                                                                       PLO14840
C     COMPARE CURRENT ENDF/B ZA/MF/MT TO REQUESTS.                      PLO14850
C                                                                       PLO14860
C***** OLD
C     COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),        PLO14870
C    1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),NGET,IGET              PLO14880
C***** OLD
C***** TRKOV
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
C***** TRKOV
      DO 10 KGET=1,NGET                                                 PLO14890
      IF(IZA.LT.IZALOW(KGET).OR.IZA.GT.IZAHI(KGET)) GO TO 10            PLO14900
      IF(MF.LT.MFLOW(KGET).OR.MF.GT.MFHI(KGET)) GO TO 10                PLO14910
      IF(MT.GE.MTLOW(KGET).AND.MT.LE.MTHI(KGET)) GO TO 20               PLO14920
   10 CONTINUE                                                          PLO14930
      KGET=0                                                            PLO14940
   20 RETURN                                                            PLO14950
      END                                                               PLO14960
      SUBROUTINE SCALER                                                 PLO14970
C                                                                       PLO14980
C     DEFINE X AND Y LIMITS. SELECT LINEAR OR LOG SCALING.              PLO14990
C                                                                       PLO15000
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO15010
      COMMON/WAYS/IWAY(2)                                               PLO15020
C***** OLD
C     COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX                           PLO15030
C***** OLD
C***** TRKOV
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX,AWR
C***** TRKOV
      COMMON/RATZA/IZARAT,MTRAT,MFIN                                    PLO15040
C***** TRKOV
      COMMON/PAGEXY/XPAGE(90000),YPAGE(90000),N2,IBASE,ITOP,ISCR
C***** TRKOV
C***** OLD
C     COMMON/PAGEXY/XPAGE(9000),YPAGE(9000),N2,IBASE,ITOP,ISCR          PLO08310
C***** OLD
C***** TRKOV
      COMMON/EXFOR/XEX(10000),DXEX(10000),YEX(10000),DYEX(10000)
     1,NREF(10000),E2(10000),IEX
C***** TRKOV
C***** OLD
C     COMMON/EXFOR/XEX(1000),DXEX(1000),YEX(1000),DYEX(1000),NREF(1000),PLO15060
C    1 E2(1000),IEX                                                     PLO15070
C***** OLD
C***** OLD
C     COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),        PLO15080
C    1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),NGET,IGET              PLO15090
C***** OLD
C***** TRKOV
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
C***** TRKOV
      COMMON/DOUBL/IDOUB,FIELD4(4)                                      PLO15100
C***** OLD
C     COMMON/REFERI/LREF(26),EXLOW(26),EXHIGH(26),E2T(26),IREF,MREF,    PLO15110
C    1 MAXREF,IGROUP,KGROUP                                             PLO15120
C***** OLD
C***** TRKOV
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP                                             PLO08450
C***** TRKOV
      COMMON/XLIMIT/MPT                                                 PLO15130
      COMMON/SCALEI/ISCALE                                              PLO15140
      COMMON/X4LIMS/X4LIMX(2),X4LIMY(2)                                 PLO15150
C                                                                       PLO15160
C     COMPARE CURRENT ZA/MF/MT TO CURRENT REQUEST                       PLO15170
C     IF NOT CROSS SECTIONS ALSO COMPARE INCIDENT ENERGY.               PLO15180
C                                                                       PLO15190
      MPT=0                                                             PLO15200
      IF(IZA.LT.IZALOW(IGET).OR.IZA.GT.IZAHI(IGET)) GO TO 10            PLO15210
      IF(MFIN.LT.MFLOW(IGET).OR.MFIN.GT.MFHI(IGET)) GO TO 10            PLO15220
      IF(MT.LT.MTLOW(IGET).OR.MT.GT.MTHI(IGET)) GO TO 10                PLO15230
      IF(MF.EQ.3) GO TO 20                                              PLO15240
      IF(ENEX.LT.ELGET(IGET).OR.ENEX.GT.EHGET(IGET)) GO TO 10           PLO15250
      GO TO 20                                                          PLO15260
C-----REQUEST IS NOT FOR THIS ZA/MF/MT.                                 PLO15270
   10 RETURN                                                            PLO15280
C                                                                       PLO15290
C     DEFINE MAXIMUM AND MINIMUM X AND Y VALUES.                        PLO15300
C                                                                       PLO15310
   20 IPASS=0                                                           PLO15320
C-----IF NO ENDF/B DATA OR ONLY SCALING EXFOR DATA SKIP THIS SECTION.   PLO15330
      IF(N2.LE.0.OR.ISCALE.EQ.2) GO TO 120                              PLO15340
C                                                                       PLO15350
C     SCALE ENDF/B DATA.                                                PLO15360
C                                                                       PLO15370
      DO 110 I=1,N2                                                     PLO15380
C-----IF CROSS SECTIONS ONLY SELECT POINTS IN ENERGY RANGE OF NEXT PLOT.PLO15390
      XNOW=X(I)                                                         PLO15400
      YNOW=Y(I)                                                         PLO15410
      IF(MF.NE.3) GO TO 70                                              PLO15420
C-----IGNOR ALL POINTS BELOW LOWER ENERGY LIMIT OF PLOT.                PLO15430
      IF(XNOW-ELGET(IGET)) 100,70,30                                    PLO15440
C-----LOWER LIMIT OF PLOT REACHED. CHECK UPPER LIMIT.                   PLO15450
   30 IF(XNOW-EHGET(IGET)) 60,40,40                                     PLO15460
C-----UPPER ENERGY OF PLOT REACHED. NOTHING TO DO IF ALL POINTS ARE     PLO15470
C-----ABOVE THE ENERGY RANGE OF THE PLOT.                               PLO15480
   40 IF(I.LE.1) GO TO 120                                              PLO15490
C-----IF LAST POINT WAS BELOW LOWER LIMIT OF PLOT INTERPOLATE TO LOWER  PLO15500
C-----LIMIT AND INITIALIZE X AND Y LIMITS.                              PLO15510
      IF(IPASS.GT.0) GO TO 50                                           PLO15520
      YNOW=((XNOW-ELGET(IGET))*YLAST+(ELGET(IGET)-XLAST)*YNOW)/         PLO15530
     1 (XNOW-XLAST)                                                     PLO15540
      XNOW=ELGET(IGET)                                                  PLO15550
      XLIM(1)=XNOW                                                      PLO15560
      XLIM(2)=XNOW                                                      PLO15570
      YLIM(1)=YNOW                                                      PLO15580
      YLIM(2)=YNOW                                                      PLO15590
C-----INTERPOLATE TO UPPER ENERGY LIMIT (ABOVE TESTS INSURE AT LEAST    PLO15600
C-----ONE PRECEDING POINT).                                             PLO15610
   50 YNOW=((XNOW-EHGET(IGET))*YLAST+(EHGET(IGET)-XLAST)*YNOW)/         PLO15620
     1 (XNOW-XLAST)                                                     PLO15630
      XNOW=EHGET(IGET)                                                  PLO15640
C-----SET FLAG TO INDICATE END OF PLOT.                                 PLO15650
      IPASS=2                                                           PLO15660
      GO TO 90                                                          PLO15670
C-----POINT IS WITHIN ENERGY RANGE OF PLOT. IF LIMITS ARE ALREADY       PLO15680
C-----INITIALIZED USE CURRENT POINT TO UPDATE X AND Y LIMITS.           PLO15690
   60 IF(IPASS.GT.0) GO TO 90                                           PLO15700
C-----LIMITS ARE NOT YET INITIALIZED. IF THIS IS FIRST POINT USE        PLO15710
C-----IT TO INITIALIZE X AND Y LIMITS. OTHERWISE INTERPOLATE TO LOWER   PLO15720
C-----ENERGY LIMIT OF PLOT.                                             PLO15730
      IF(I.LE.1) GO TO 80                                               PLO15740
      YNOW=((XNOW-ELGET(IGET))*YLAST+(ELGET(IGET)-XLAST)*YNOW)/         PLO15750
     1 (XNOW-XLAST)                                                     PLO15760
      XNOW=ELGET(IGET)                                                  PLO15770
      GO TO 80                                                          PLO15780
C-----INITIALIZE LIMITS ON FIRST ACCEPTABLE POINT.                      PLO15790
   70 IF(IPASS.GT.0) GO TO 90                                           PLO15800
   80 IPASS=1                                                           PLO15810
      XLIM(1)=XNOW                                                      PLO15820
      XLIM(2)=XNOW                                                      PLO15830
      YLIM(1)=YNOW                                                      PLO15840
      YLIM(2)=YNOW                                                      PLO15850
      GO TO 100                                                         PLO15860
C-----UPDATE LIMIT.                                                     PLO15870
   90 IF(XNOW.LT.XLIM(1)) XLIM(1)=XNOW                                  PLO15880
      IF(XNOW.GT.XLIM(2)) XLIM(2)=XNOW                                  PLO15890
      IF(YNOW.LT.YLIM(1)) YLIM(1)=YNOW                                  PLO15900
      IF(YNOW.GT.YLIM(2)) YLIM(2)=YNOW                                  PLO15910
C-----END OF SCALING IF FLAG INDICATES UPPER ENERGY LIMIT REACHED.      PLO15920
  100 IF(IPASS.GE.2) GO TO 120                                          PLO15930
C-----SAVE X AND Y VALUES FOR INTERPOLATION.                            PLO15940
      XLAST=XNOW                                                        PLO15950
      YLAST=YNOW                                                        PLO15960
  110 CONTINUE                                                          PLO15970
C                                                                       PLO15980
C     DETERMINE WHICH REFERENCES WILL APEEAR ON NEXT PLOT, NUMBER OF    PLO15990
C     POINTS FROM EACH REFERENCE AND ENERGY RANGE OF EACH REFERENCE.    PLO16000
C     IF REQUESTED USE EXFOR DATA FOR SCALING.                          PLO16010
C                                                                       PLO16020
  120 MREF=0                                                            PLO16030
      IMLOW=0                                                           PLO16040
      IMHI=0                                                            PLO16050
C-----SCALE EACH REFERENCE SEPARATELY.                                  PLO16060
      DO 210 KREF=1,IREF                                                PLO16070
      LREF(KREF)=0                                                      PLO16080
C-----SELECT POINTS FROM CURRENT REFERENCE.                             PLO16090
      DO 200 I=1,IEX                                                    PLO16100
      IF(NREF(I).NE.KREF) GO TO 200                                     PLO16110
C-----IF CROSS SECTIONS ONLY SELECT POINTS IN ENERGY RANGE OF NEXT PLOT.PLO16120
C-----ENERGY RANGE OF NEXT PLOT.                                        PLO16130
      IF(MF.NE.3) GO TO 130                                             PLO16140
      IF(XEX(I).LT.ELGET(IGET)) GO TO 180                               PLO16150
      IF(XEX(I).GT.EHGET(IGET)) GO TO 190                               PLO16160
C-----IF ONLY SCALING FOR ENDF/B IGNOR POINTS OUTSIDE THE RANGE OF THE  PLO16170
C-----PLOT.                                                             PLO16180
  130 IF(ISCALE.NE.1) GO TO 140                                         PLO16190
      IF(XEX(I).LT.XLIM(1).OR.XEX(I).GT.XLIM(2)) GO TO 200              PLO16200
      IF(YEX(I).LT.YLIM(1).OR.YEX(I).GT.YLIM(2)) GO TO 200              PLO16210
C-----COUNT TOTAL POINTS, REFERENCES, POINTS FOR CURRENT REFERENCE AND  PLO16220
C-----SAVE ENERGY RANGE OF CURRENT REFERENCE.                           PLO16230
  140 MPT=MPT+1                                                         PLO16240
      IF(LREF(KREF).GT.0) GO TO 150                                     PLO16250
      MREF=MREF+1                                                       PLO16260
      EXLOW(KREF)=XEX(I)                                                PLO16270
      EXHIGH(KREF)=XEX(I)                                               PLO16280
  150 IF(XEX(I).LT.EXLOW(KREF)) EXLOW(KREF)=XEX(I)                      PLO16290
      IF(XEX(I).GT.EXHIGH(KREF)) EXHIGH(KREF)=XEX(I)                    PLO16300
      LREF(KREF)=LREF(KREF)+1                                           PLO16310
C-----IF REQUESTED DO NOT SCALE FOR EXFOR DATA.                         PLO16320
      IF(ISCALE.EQ.1) GO TO 200                                         PLO16330
C                                                                       PLO16340
C     SCALE DATA ALLOWING FOR X AND Y UNCERTAINTIES.                    PLO16350
C                                                                       PLO16360
C-----DEFINE X LIMITS TRUNCATED TO REQUESTED ENERGY RANGE AND DO NOT    PLO16370
C-----LET LOWER X LIMIT CROSS ZERO.                                     PLO16380
      XEXM=XEX(I)-DXEX(I)                                               PLO16390
      XEXP=XEX(I)+DXEX(I)                                               PLO16400
      IF(MF.NE.3) GO TO 160                                             PLO16410
      IF(XEXM.LT.ELGET(IGET)) XEXM=ELGET(IGET)                          PLO16420
      IF(XEXP.GT.EHGET(IGET)) XEXP=EHGET(IGET)                          PLO16430
      IF(MF.EQ.4) GO TO 160                                             PLO16440
C-----ALLOW COSINE ERROR TO CROSS ZERO.                                 PLO16450
      IF(MF.EQ.6.AND.IDOUB.EQ.1) GO TO 160                              PLO16460
      IF(XEX(I).GT.0.0.AND.XEXM.LE.0.0) XEXM=XEX(I)                     PLO16470
C-----DEFINE Y LIMITS TRUNCATED NOT TO LET LOWER Y LIMIT CROSS ZERO.    PLO16480
  160 YEXM=YEX(I)-DYEX(I)                                               PLO16490
      YEXP=YEX(I)+DYEX(I)                                               PLO16500
      IF(YEX(I).GT.0.0.AND.YEXM.LE.0.0) YEXM=YEX(I)                     PLO16510
      IF(IPASS.GT.0) GO TO 170                                          PLO16520
C-----SAVE LIMITS BASED ON FIRST POINT.                                 PLO16530
      IPASS=1                                                           PLO16540
      XLIM(1)=XEXM                                                      PLO16550
      XLIM(2)=XEXP                                                      PLO16560
      YLIM(1)=YEXM                                                      PLO16570
      YLIM(2)=YEXP                                                      PLO16580
      GO TO 200                                                         PLO16590
C-----UPDATE X AND Y LIMITS.                                            PLO16600
  170 IF(XEXM.LT.XLIM(1)) XLIM(1)=XEXM                                  PLO16610
      IF(XEXP.GT.XLIM(2)) XLIM(2)=XEXP                                  PLO16620
      IF(YEXM.LT.YLIM(1)) YLIM(1)=YEXM                                  PLO16630
      IF(YEXP.GT.YLIM(2)) YLIM(2)=YEXP                                  PLO16640
      GO TO 200                                                         PLO16650
C-----COUNT POINTS BELOW ENERGY RANGE OF PLOT.                          PLO16660
  180 IMLOW=IMLOW+1                                                     PLO16670
      GO TO 200                                                         PLO16680
C-----COUNT POINT ABOVE ENERGY RANGE OF PLOT.                           PLO16690
  190 IMHIGH=IMHIGH+1                                                   PLO16700
  200 CONTINUE                                                          PLO16710
  210 CONTINUE                                                          PLO16720
C-----IF THESE ARE CROSS SECTIONS IF THERE ARE POINTS BELOW OR ABOVE    PLO16730
C-----THE REQUESTED ENERGY LIMITS SET ENERGY LIMITS TO EXACTLY REQUESTEDPLO16740
C-----REQUESTED LIMITS.                                                 PLO16750
      IF(MF.NE.3.OR.ISCALE.NE.1) GO TO 220                              PLO16760
      IF(IMLOW.GT.0) XLIM(1)=ELGET(IGET)                                PLO16770
      IF(IMHIGH.GT.0) XLIM(2)=EHGET(IGET)                               PLO16780
C-----SAVE CURRENT LIMITS TO PREVENT ADDITIONAL EXFOR POINTS BEING      PLO16790
C-----PLOTTED AFTER ROUNDING PLOT LIMITS OUTWARD.                       PLO16800
  220 X4LIMX(1)=XLIM(1)                                                 PLO16810
      X4LIMX(2)=XLIM(2)                                                 PLO16820
      X4LIMY(1)=YLIM(1)                                                 PLO16830
      X4LIMY(2)=YLIM(2)                                                 PLO16840
C                                                                       PLO16850
C     DEFINE LINEAR OR LOG SCALING                                      PLO16860
C                                                                       PLO16870
      IWAY(1)=2                                                         PLO16880
      IF(MF.EQ.7) XLIM(1)=0.0                                           PLO16890
C***** OLD
C     IF(XLIM(1).LE.0.0.OR.MF.EQ.4.OR.MF.EQ.8) IWAY(1)=1                PLO16900
C     IF(MF.EQ.6.AND.IDOUB.EQ.1) IWAY(1)=1                              PLO16910
C     IF(XLIM(2).LT.10.0*XLIM(1)) IWAY(1)=1                             PLO16920
C***** OLD
C***** TRKOV
      JNTR=INTRN(IGET)
      IF(MF.EQ.6.AND.IDOUB.EQ.1) IWAY(1)=1
      IF(MF.EQ.4 .OR.MF.EQ.8) IWAY(1)=1
      IF(JNTR.EQ.0 .AND. XLIM(1).LE.0.0) IWAY(1)=1
      IF(JNTR.EQ.0 .AND. XLIM(2).LT.10.0*XLIM(1)) IWAY(1)=1
      IF(JNTR.EQ.2 .OR. JNTR.EQ.4) IWAY(1)=1
      IF(IWAY(1).EQ.2 .AND.XLIM(1).LE.0) XLIM(1)=1.E-5
C***** TRKOV
      IWAY(2)=2                                                         PLO16930
C***** OLD
C     IF(YLIM(1).LE.0.0) IWAY(2)=1                                      PLO16940
C     IF(YLIM(2).LT.10.0*YLIM(1)) IWAY(2)=1                             PLO16950
C***** OLD
C***** TRKOV
      IF(JNTR.EQ.0 .AND. YLIM(1).LE.0.0) IWAY(2)=1
      IF(JNTR.EQ.0 .AND. YLIM(2).LT.10.0*YLIM(1)) IWAY(2)=1
      IF(JNTR.EQ.2 .OR. JNTR.EQ.3) IWAY(2)=1
      IF(IWAY(2).EQ.2 .AND.YLIM(1).LE.0) YLIM(1)=1.E-5*YLIM(2)
C***** TRKOV
C                                                                       PLO16960
C     IF REQUIRED, CONVERT LIMITS TO LOG.                               PLO16970
C                                                                       PLO16980
      IF(IWAY(1).EQ.1) GO TO 230                                        PLO16990
      XLIM(1)=ALOG10(XLIM(1))                                           PLO17000
      XLIM(2)=ALOG10(XLIM(2))                                           PLO17010
  230 IF(IWAY(2).EQ.1) GO TO 240                                        PLO17020
      YLIM(1)=ALOG10(YLIM(1))                                           PLO17030
      YLIM(2)=ALOG10(YLIM(2))                                           PLO17040
C                                                                       PLO17050
C     PREVENT ZERO RANGES AND ROUND LIMITS OUTWARD.                     PLO17060
C                                                                       PLO17070
  240 IF(MF.EQ.4) GO TO 250                                             PLO17080
      IF(MF.EQ.6.AND.IDOUB.EQ.1) GO TO 250                              PLO17090
      IF(XLIM(2).GT.XLIM(1)) GO TO 260                                  PLO17100
      IF(XLIM(1).EQ.0.0) GO TO 250                                      PLO17110
      XLIM(1)=0.5*XLIM(1)                                               PLO17120
      XLIM(2)=1.5*XLIM(2)                                               PLO17130
      GO TO 280                                                         PLO17140
  250 XLIM(1)=-1.04                                                     PLO17150
      XLIM(2)=1.04                                                      PLO17160
      GO TO 280                                                         PLO17170
  260 IF(MF.EQ.7.OR.IWAY(1).EQ.2.OR.XLIM(1).GT.0.0) GO TO 270           PLO17180
      XLIM(2)=1.02*XLIM(2)                                              PLO17190
      GO TO 280                                                         PLO17200
  270 XMID=0.5*(XLIM(2)+XLIM(1))                                        PLO17210
      XRANGE=0.5*(XLIM(2)-XLIM(1))                                      PLO17220
      XLIM(1)=XMID-1.04*XRANGE                                          PLO17230
      XLIM(2)=XMID+1.04*XRANGE                                          PLO17240
  280 IF(YLIM(2).GT.YLIM(1)) GO TO 300                                  PLO17250
      IF(YLIM(1).EQ.0.0) GO TO 290                                      PLO17260
      YLIM(1)=0.5*YLIM(1)                                               PLO17270
      YLIM(2)=1.5*YLIM(2)                                               PLO17280
      GO TO 320                                                         PLO17290
  290 YLIM(1)=-1.0                                                      PLO17300
      YLIM(2)=1.0                                                       PLO17310
      GO TO 320                                                         PLO17320
  300 IF(IWAY(2).EQ.2.OR.YLIM(1).GT.0.0) GO TO 310                      PLO17330
      YLIM(2)=1.04*YLIM(2)                                              PLO17340
      GO TO 320                                                         PLO17350
  310 YMID=0.5*(YLIM(2)+YLIM(1))                                        PLO17360
      YRANGE=0.5*(YLIM(2)-YLIM(1))                                      PLO17370
      YLIM(1)=YMID-1.04*YRANGE                                          PLO17380
      YLIM(2)=YMID+1.04*YRANGE                                          PLO17390
  320 RETURN                                                            PLO17400
      END                                                               PLO17410
      SUBROUTINE UNITED                                                 PLO17420
C                                                                       PLO17430
C     SELECT X AND Y UNITS. DEFINE X AND Y SCALE FACTORS.               PLO17440
C     CONVERT X AND Y LIMITS TO AXIS UNITS.                             PLO17450
C                                                                       PLO17460
      CHARACTER*1 LABCM,STATUS,CMSYS                                    PLO17470
      CHARACTER*4 XLABEL,YLABEL,COSINE,COSCM,LORDER,COEFF,COEFCM,ATWT,  PLO17480
     1 YIELD,RATIO,IM78,DEF78,NOKNOW                                    PLO17490
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO17500
      COMMON/XYREAL/XREAL(2),YREAL(2)                                   PLO17510
C***** OLD
C     COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX                           PLO17520
C***** OLD
C***** TRKOV
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX,AWR
C***** TRKOV
      COMMON/RATZA/IZARAT,MTRAT,MFIN                                    PLO17530
      COMMON/WAYS/IWAY(2)                                               PLO17540
      COMMON/UNNORM/IMNORM                                              PLO17550
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,        PLO17560
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP                                PLO17570
      COMMON/XYLABC/XLABEL(10),YLABEL(10)                               PLO17580
      COMMON/SYSSTA/LABCM,STATUS                                        PLO17590
      COMMON/DOUBL/IDOUB,FIELD4(4)                                      PLO17600
      COMMON/WHO78C/IM78                                                PLO17610
      COMMON/SCALEI/ISCALE                                              PLO17620
      COMMON/X4LIMS/X4LIMX(2),X4LIMY(2)                                 PLO17630
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC                                 PLO17640
      DIMENSION COSINE(2),COSCM(3),LORDER(4),COEFF(3),COEFCM(4),ATWT(4),PLO17650
     1 YIELD(4),RATIO(2),DEF78(3),NOKNOW(3)                             PLO17660
      DATA COSINE/'Cosi','ne  '/                                        PLO17670
      DATA COSCM/'Cosi','ne-C','M   '/                                  PLO17680
      DATA LORDER/'Lege','ndre',' Ord','er'/                            PLO17690
      DATA ATWT/'Atom','ic W','eigh','t   '/                            PLO17700
      DATA YIELD/'Fiss','ion ','Yiel','d'/                              PLO17710
      DATA CMSYS/'C'/                                                   PLO17720
      DATA COEFF/'Coef','fice','nts '/                                  PLO17730
      DATA COEFCM/'Coef','fice','nts-','CM  '/                          PLO17740
      DATA RATIO/'Rati','o'/                                            PLO17750
      DATA NOKNOW/'Unno','rmal','ized'/                                 PLO17760
C                                                                       PLO17770
C     SELECT X AND Y UNITS. DEFINE AXIS LABELS AND MULTIPLIERS.         PLO17780
C                                                                       PLO17790
      IF(MF.EQ.3.OR.MF.EQ.5) GO TO 140                                  PLO17800
      IF(MF.EQ.6.AND.IDOUB.EQ.2) GO TO 140                              PLO17810
C-----USE X STANDARD UNITS.                                             PLO17820
      XMULT=1.0                                                         PLO17830
C-----DEFINE ANGULAR DISTRIBUTION X LABEL.                              PLO17840
      IF(MF.NE.4.AND.MF.NE.6) GO TO 50                                  PLO17850
C                                                                       PLO17860
C     ANGULAR DISTRIBUTION (SIMPLE OR DOUBLE DIFFERENTIAL).             PLO17870
C                                                                       PLO17880
C-----DEFINE EITHER COSINE OR COSINE-CM                                 PLO17890
      IF(LABCM.EQ.CMSYS) GO TO 10                                       PLO17900
      IXLAB=6                                                           PLO17910
      XLABEL(1)=COSINE(1)                                               PLO17920
      XLABEL(2)=COSINE(2)                                               PLO17930
      GO TO 20                                                          PLO17940
   10 IXLAB=9                                                           PLO17950
      XLABEL(1)=COSCM(1)                                                PLO17960
      XLABEL(2)=COSCM(2)                                                PLO17970
      XLABEL(3)=COSCM(3)                                                PLO17980
C-----DEFINE Y UNITS AND MULTIPLIER.                                    PLO17990
   20 CALL CSUNIT(YLIM,YMULT,YLABEL,IYLAB,IWAY(2))                      PLO18000
C-----ADD TO UNITS IF REQUIRED.                                         PLO18010
      CALL PERUN(YLABEL,IYLAB,MF,IDOUB)                                 PLO18020
C-----IF RUNNING ON IBM-PC LABEL X AXIS EVERY 0.5 AND DRAW LINE EVERY   PLO18030
C-----0.25                                                              PLO18040
      IXSTEP=1                                                          PLO18050
      IF(IBMPC.LE.0) GO TO 30                                           PLO18060
      XBASE=-1.5                                                        PLO18070
      XSTEP1=0.5                                                        PLO18080
      XSTEP2=0.25                                                       PLO18090
      GO TO 40                                                          PLO18100
C-----LABEL X AXIS EVERY 0.2 AND DRAW LINE EVERY 0.1.                   PLO18110
   30 XBASE=-1.2                                                        PLO18120
      XSTEP1=0.2                                                        PLO18130
      XSTEP2=0.1                                                        PLO18140
C-----DEFINE Y AXIS LABEL INCREMENTS.                                   PLO18150
   40 CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))              PLO18160
      GO TO 200                                                         PLO18170
C-----USE STANDARD Y UNITS.                                             PLO18180
   50 YMULT=1.0                                                         PLO18190
C-----DEFINE LEGENDRE ORDER X AND Y LABELS.                             PLO18200
      IF(MF.NE.7) GO TO 110                                             PLO18210
C                                                                       PLO18220
C     LEGENDRE COEFFICIENTS.                                            PLO18230
C                                                                       PLO18240
C-----DEFINE LEGENDRE ORDER.                                            PLO18250
      IXLAB=14                                                          PLO18260
      DO 60 I=1,4                                                       PLO18270
   60 XLABEL(I)=LORDER(I)                                               PLO18280
C-----DEFINE EITHER COEFFICIENTS OR COEFFICIENTS-CM                     PLO18290
      IF(LABCM.EQ.CMSYS) GO TO 80                                       PLO18300
      IYLAB=11                                                          PLO18310
      DO 70 J=1,3                                                       PLO18320
   70 YLABEL(J)=COEFF(J)                                                PLO18330
      GO TO 100                                                         PLO18340
   80 IYLAB=14                                                          PLO18350
      DO 90 J=1,4                                                       PLO18360
   90 YLABEL(J)=COEFCM(J)                                               PLO18370
C-----ONLY LABEL X AXIS AND DRAW LINES AT LEGENDRE ORDERS.              PLO18380
  100 IXSTEP=-1                                                         PLO18390
      XBASE=-1.0                                                        PLO18400
      XSTEP1=1.0                                                        PLO18410
      XSTEP2=1.0                                                        PLO18420
C-----DEFINE Y AXIS LABEL INCREMENTS.                                   PLO18430
      CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))              PLO18440
      GO TO 200                                                         PLO18450
C-----DEFINE FISSION YIELD X AND Y LABELS.                              PLO18460
  110 IF(MF.NE.8) GO TO 200                                             PLO18470
C                                                                       PLO18480
C     FISSION YIELD.                                                    PLO18490
C                                                                       PLO18500
C-----DEFINE ATOMIC WEIGHT                                              PLO18510
      IXLAB=13                                                          PLO18520
      DO 120 I=1,4                                                      PLO18530
  120 XLABEL(I)=ATWT(I)                                                 PLO18540
C-----DEFINE FISSION YIELD.                                             PLO18550
      IYLAB=13                                                          PLO18560
      DO 130 I=1,4                                                      PLO18570
  130 YLABEL(I)=YIELD(I)                                                PLO18580
C-----DEFINE X AXIS LABEL INCREMENTS.                                   PLO18590
      CALL SPACER(XLIM,XBASE,XSTEP1,XSTEP2,IXSTEP,IWAY(1))              PLO18600
C-----DEFINE Y AXIS LABEL INCREMENTS.                                   PLO18610
      CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))              PLO18620
      GO TO 200                                                         PLO18630
C                                                                       PLO18640
C     CROSS SECTION VS. ENERGY.                                         PLO18650
C                                                                       PLO18660
C-----DEFINE ENERGY UNITS AND MULTIPLIER.                               PLO18670
  140 CALL EUNIT(XLIM(2),XMULT,XLABEL,IXLAB,4,IPTZ,IWAY(1))             PLO18680
C-----IF SECONDARY ENERGY PRECEED X AXIS TITLE BY DEFINITION OF FIELD 7.PLO18690
      IF(MF.EQ.5) GO TO 150                                             PLO18700
      IF(MF.NE.3.AND.MF.NE.6) GO TO 160                                 PLO18710
      IF(IDOUB.NE.2) GO TO 160                                          PLO18720
C-----DEFINE FIELD 7.                                                   PLO18730
  150 CALL WHAT78(IM78,DEF78,IDEF78)                                    PLO18740
C-----COMBINE FIELD 7 DEFINITION AND X AXIS LABEL.                      PLO18750
      CALL PAKZAP(DEF78,IDEF78,XLABEL,IXLAB)                            PLO18760
  160 XLIM(1)=XLIM(1)*XMULT                                             PLO18770
      XLIM(2)=XLIM(2)*XMULT                                             PLO18780
C-----FOR RATIO OF RESONANCE PARAMETERS DEFINE Y LABEL AND USE STANDARD PLO18790
C-----MULTIPLIER 1.0                                                    PLO18800
      IF(MFIN.EQ.402.AND.MT.GE.6050) GO TO 170                          PLO18810
C-----FOR RATIO DEFINE Y LABEL AND USE STANDARD MULTIPLIER 1.0          PLO18820
      IF(MFIN.NE.203) GO TO 180                                         PLO18830
  170 IYLAB=5                                                           PLO18840
      YLABEL(1)=RATIO(1)                                                PLO18850
      YLABEL(2)=RATIO(2)                                                PLO18860
      YMULT=1.0                                                         PLO18870
      GO TO 190                                                         PLO18880
C-----DEFINE Y UNITS AND MULTIPLIER.                                    PLO18890
  180 CALL CSUNIT(YLIM,YMULT,YLABEL,IYLAB,IWAY(2))                      PLO18900
C-----ADD TO UNITS IF REQUIRED.                                         PLO18910
      CALL PERUN(YLABEL,IYLAB,MF,IDOUB)                                 PLO18920
C-----DEFINE X AXIS LABEL INCREMENTS.                                   PLO18930
  190 CALL SPACER(XLIM,XBASE,XSTEP1,XSTEP2,IXSTEP,IWAY(1))              PLO18940
C-----DEFINE Y AXIS LABEL INCREMENTS.                                   PLO18950
      CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))              PLO18960
C                                                                       PLO18970
C     SAVE LIMITS IN THE PLANE OF THE DATA.                             PLO18980
C                                                                       PLO18990
  200 IF(IWAY(1).EQ.1) GO TO 210                                        PLO19000
      XREAL(1)=10.0**XLIM(1)                                            PLO19010
      XREAL(2)=10.0**XLIM(2)                                            PLO19020
      GO TO 220                                                         PLO19030
  210 XREAL(1)=XLIM(1)/XMULT                                            PLO19040
      XREAL(2)=XLIM(2)/XMULT                                            PLO19050
  220 IF(IWAY(2).EQ.1) GO TO 230                                        PLO19060
      YREAL(1)=10.0**YLIM(1)                                            PLO19070
      YREAL(2)=10.0**YLIM(2)                                            PLO19080
      GO TO 240                                                         PLO19090
  230 YREAL(1)=YLIM(1)/YMULT                                            PLO19100
      YREAL(2)=YLIM(2)/YMULT                                            PLO19110
C-----IF USING EXFOR TO SCALE SAVE ROUNDED PLOT LIMITS.                 PLO19120
  240 IF(ISCALE.EQ.1) GO TO 250                                         PLO19130
      X4LIMX(1)=XREAL(1)                                                PLO19140
      X4LIMX(2)=XREAL(2)                                                PLO19150
      X4LIMY(1)=YREAL(1)                                                PLO19160
      X4LIMY(2)=YREAL(2)                                                PLO19170
C-----IF STATUS IS UNNORMALIZED CHANGE Y AXIS LABEL.                    PLO19180
  250 IF(IMNORM.NE.1) GO TO 270                                         PLO19190
      DO 260 I=1,3                                                      PLO19200
  260 YLABEL(I)=NOKNOW(I)                                               PLO19210
      IYLAB=12                                                          PLO19220
  270 RETURN                                                            PLO19230
      END                                                               PLO19240
      SUBROUTINE WHAT78(IM78,DEF78,IDEF78)                              PLO19250
C                                                                       PLO19260
C     DEFINE FIELDS 7-8.                                                PLO19270
C                                                                       PLO19280
      CHARACTER*4 TABA78,TABB78,IM78,DEF78                              PLO19290
      DIMENSION TABA78(8),TABB78(3,8),ITAB78(8),DEF78(3)                PLO19300
      DATA TABA78/' E2','LVL','EXC',' HL','DE2','DLV','MIN','MAX'/      PLO19310
      DATA ITAB78/2,5,10,9,2,5,6,6/                                     PLO19320
      DATA TABB78/                                                      PLO19330
     1 'E2  ','    ','    ',                                            PLO19340
     2 'Leve','l   ','    ',                                            PLO19350
     3 'Exci','tati','on  ',                                            PLO19360
     4 'Half','-Lif','e   ',                                            PLO19370
     5 'E2  ','    ','    ',                                            PLO19380
     6 'Leve','l   ','    ',                                            PLO19390
     7 'E2-M','in  ','    ',                                            PLO19400
     8 'E2-M','ax  ','    '/                                            PLO19410
      DO 10 I=1,8                                                       PLO19420
      IF(IM78.EQ.TABA78(I)) GO TO 20                                    PLO19430
   10 CONTINUE                                                          PLO19440
      I=1                                                               PLO19450
   20 IDEF78=ITAB78(I)                                                  PLO19460
      DO 30 K=1,3                                                       PLO19470
   30 DEF78(K)=TABB78(K,I)                                              PLO19480
      RETURN                                                            PLO19490
      END                                                               PLO19500
      SUBROUTINE SPACER(ZLIM,ZBASE,ZSTEP1,ZSTEP2,IZSTEP,IZWAY)          PLO19510
C                                                                       PLO19520
C     DEFINE LINEAR X OR Y AXIS LABEL INCREMENTS.                       PLO19530
C     USE FEWER AXIS INCREMENTS IF RUNNING ON IBM-PC.                   PLO19540
C                                                                       PLO19550
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC                                 PLO19560
      DIMENSION ZLIM(2),STEPS(13),ISTEPS(13)                            PLO19570
      DATA STEPS/100.,50.,20.,10.,5.,2.,1.,.5,.2,.1,.05,.02,.01/        PLO19580
      DATA ISTEPS/-1,-1,-1,-1,-1,-1,-1,1,1,1,2,2,2/                     PLO19590
      IF(IZWAY.EQ.2) RETURN                                             PLO19600
      DZLIM=ZLIM(2)-ZLIM(1)                                             PLO19610
      INEED=10                                                          PLO19620
      IF(ZLIM(2).GE.100.0) INEED=5                                      PLO19630
C-----FEWER GRID LINES IF HALF SIZE PLOTS ON IBM-PC.                    PLO19640
      IF(IBMPC.GT.0) INEED=5                                            PLO19650
      IF(IBMPC.GT.0.AND.ZLIM(2).GE.100.0) INEED=4                       PLO19660
      DO 10 I=1,13                                                      PLO19670
      IF(I.EQ.7) INEED=5                                                PLO19680
      IF(I.EQ.7.AND.IBMPC.GT.0) INEED=4                                 PLO19690
      IF(I.EQ.13) GO TO 20                                              PLO19700
      J=DZLIM/STEPS(I)                                                  PLO19710
      K=DZLIM/STEPS(I+1)                                                PLO19720
      IF(K.GE.2*INEED) GO TO 20                                         PLO19730
      IF(J.GE.INEED) GO TO 20                                           PLO19740
   10 CONTINUE                                                          PLO19750
      I=13                                                              PLO19760
   20 ZSTEP1=STEPS(I)                                                   PLO19770
      IZBASE=ZLIM(1)/ZSTEP1                                             PLO19780
      ZBASE=FLOAT(IZBASE)*ZSTEP1                                        PLO19790
      IZSTEP=ISTEPS(I)                                                  PLO19800
      TWICE=1.0                                                         PLO19810
      IF(J.LT.10) TWICE=2.0                                             PLO19820
      ZSTEP2=ZSTEP1/TWICE                                               PLO19830
      RETURN                                                            PLO19840
      END                                                               PLO19850
      SUBROUTINE BORDER                                                 PLO19860
C                                                                       PLO19870
C     PLOT BORDER AND ALL AXIS LABELS.                                  PLO19880
C                                                                       PLO19890
      CHARACTER*4 ZABCD,MSTAT1,MSTAT2,XLABEL,YLABEL,LIBNAM,MTBCD,MFBCD, PLO19900
     1 STATAB,COS1,EN2,UNITO,DATAO,REFS,REF1,ZUNIT,RATIO,ZAPBCD,MSTATP, PLO19910
     2 BLANK,UNDEFI,EN1,IM78,DEF78,HL                                   PLO19920
C***** TRKOV
       CHARACTER*4 COSA
C***** TRKOV
      CHARACTER*1 DIGITS,LABCM,STATUS,STAT1,MSTAR1,MSTAR2               PLO19930
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO19940
C***** OLD
C     COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX                           PLO19950
C***** OLD
C***** TRKOV
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX,AWR
C***** TRKOV
      COMMON/MODEMY/MYMODE                                              PLO19960
      COMMON/WHEREC/ZABCD(4),MSTAT1,MSTAT2                              PLO19970
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC                                 PLO19980
      COMMON/WHERE2/IZABCD                                              PLO19990
      COMMON/LIBI/ILIB                                                  PLO20000
      COMMON/LIBC/LIBNAM(4)                                             PLO20010
      COMMON/WAYS/IWAY(2)                                               PLO20020
      COMMON/WHO78C/IM78                                                PLO20030
      COMMON/WHO78I/IMAM78                                              PLO20040
C***** OLD
C     COMMON/REFERC/REFS(9,26),REF1(9)                                  PLO20050
C***** OLD
C***** TRKOV
      COMMON/REFERC/REFS(9,48),REF1(9)
C***** TRKOV
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,        PLO20060
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP                                PLO20070
      COMMON/XYLABC/XLABEL(10),YLABEL(10)                               PLO20080
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO20090
C***** OLD
C     COMMON/REFERI/LREF(26),EXLOW(26),EXHIGH(26),E2T(26),IREF,MREF,    PLO20100
C    1 MAXREF,IGROUP,KGROUP                                             PLO20110
C***** OLD
C***** TRKOV
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP                                             PLO08450
C***** TRKOV
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO20120
      COMMON/LOGTAB/TABLOG(10)                                          PLO20130
      COMMON/PLOTN/NUMPLT,ADVANC                                        PLO20140
      COMMON/SYSSTA/LABCM,STATUS                                        PLO20150
      COMMON/DOUBL/IDOUB,FIELD4(4)                                      PLO20160
      COMMON/RATZA/IZARAT,MTRAT,MFIN                                    PLO20170
      COMMON/RATZAC/MSTAR1,MSTAR2                                       PLO20180
      COMMON/SPOTS/TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGHT1,RIGHT2,RIGHT3,  PLO20190
     1 RIGHT4,RIGHT5,RIGHT6,RIGHT7,RIGHT8                               PLO20200
      DIMENSION MTBCD(20),MFBCD(8),DIGITS(9),STAT1(7),STATAB(3,7),      PLO20210
     1 COS1(2),ZUNIT(2),RATIO(2),ZAPBCD(20),UNDEFI(3),DEF78(3)          PLO20220
C***** TRKOV
      DIMENSION COSA(4)
C***** TRKOV
      DATA HL/' HL'/                                                    PLO20230
      DATA BLANK/'    '/                                                PLO20240
C***** OLD
C     DATA COS1/'Cos ','=   '/                                          PLO20250
C***** OLD
C***** TRKOV
      DATA COS1/'Degr','=   '/
      DATA COSA/'Angl','e int','egra','ted '/
C***** TRKOV
      DATA EN2/'E2 ='/                                                  PLO20260
      DATA EN1/'E ='/                                                   PLO20270
      DATA STAT1/'P','S','D','C','A','O','R'/                           PLO20280
      DATA STATAB/                                                      PLO20290
     1 ' Pre','limi','nary',                                            PLO20300
     2 '  Su','perc','eded',                                            PLO20310
     3 '   D','epen','dent',                                            PLO20320
     4 '  Co','rrel','ated',                                            PLO20330
     5 '    ','Appr','oved',                                            PLO20340
     6 '    ','Outd','ated',                                            PLO20350
     7 'Reno','rmal','ized'/                                            PLO20360
      DATA DATAO/'Data'/                                                PLO20370
      DATA UNDEFI/'Unde','fine','d'/                                    PLO20380
      DATA RATIO/'Rati','o'/                                            PLO20390
      DATA DIGITS/'1','2','3','4','5','6','7','8','9'/                  PLO20400
C-----SELECT BLACK PEN FOR ALL BORDER INFORMATON.                       PLO20410
      CALL PEN(2)                                                       PLO20420
C-----INCREMENT PLOT COUNT.                                             PLO20430
      NUMPLT=NUMPLT+1                                                   PLO20440
C-----DEFINE TRUE CENTER OF PLOT.                                       PLO20450
      XMID=0.5*(XINCH(1)+XINCH(2))                                      PLO20460
      YMID=0.5*(YINCH(1)+YINCH(2))                                      PLO20470
C-----DEFINE LOCATION OF X AXIS LABEL (DIFFERENT FOR LINEAR OR LOG).    PLO20480
      BOTX3=BOT3                                                        PLO20490
      IF(IWAY(1).EQ.1) BOTX3=BOTX3+HT                                   PLO20500
      BOTX4=BOTX3-1.75*HT                                               PLO20510
C                                                                       PLO20520
C     IF NOT IDENTIFYING E2 IN OUTSIDE LEGEND BOX, IDENTIFY E2 AT TOP   PLO20530
C     OF PLOT (IF CROSS SECTIONS, ANGULAR DISTRIBUTIONS OR LEGENDRE     PLO20540
C     COEFFICIENTS).                                                    PLO20550
C                                                                       PLO20560
      IF(KGROUP.GT.0) GO TO 40                                          PLO20570
      IF(MF.NE.3.AND.MF.NE.4.AND.MF.NE.7) GO TO 40                      PLO20580
C-----PRINT INCIDENT OR SECONDARY ENERGY IN NORMAL FORM.                PLO20590
      XZ=XINCH(1)                                                       PLO20600
      IF(MF.EQ.3.AND.IDOUB.EQ.2) GO TO 10                               PLO20610
C-----IDENTIFY E2 (UNLESS E2 FIELD IS BLANK).                           PLO20620
      IF(IMAM78.LE.0) GO TO 40                                          PLO20630
      CALL WHAT78(IM78,DEF78,IDEF78)                                    PLO20640
      CALL SYMBLH(XZ,BOTX4,HT,DEF78,0.0,IDEF78)                         PLO20650
      XZ=XZ+FLOAT(IDEF78+1)*WT                                          PLO20660
      CALL SYMBLH(XZ,BOTX4,HT,'=',0.0,1)                                PLO20670
      XZ=XZ+2.0*WT                                                      PLO20680
      IF(IM78.NE.HL) GO TO 20                                           PLO20690
C-----SELECT HALF-LIFE UNITS.                                           PLO20700
      CALL HLUNIT(E2T(1),ZMULT,ZUNIT,IZUNIT,4,IPTZ)                     PLO20710
      GO TO 30                                                          PLO20720
C-----FOR CROSS SECTIONS IDENTIFY E.                                    PLO20730
   10 CALL SYMBLH(XZ,BOTX4,HT,EN1,0.0,4)                                PLO20740
      XZ=XZ+4.0*WT                                                      PLO20750
C-----SELECT ENERGY UNITS.                                              PLO20760
   20 CALL EUNIT(E2T(1),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)                    PLO20770
C-----PRINT ENERGY OR HALF-LIFE IN NORMALIZED FORM.                     PLO20780
   30 ENOUT=E2T(1)*ZMULT                                                PLO20790
      CALL NUMBRH(XZ,BOTX4,HT,ENOUT,0.0,IPTZ)                           PLO20800
      XZ=XZ+7.0*WT                                                      PLO20810
      CALL SYMBLH(XZ,BOTX4,HT,ZUNIT,0.0,IZUNIT)                         PLO20820
C                                                                       PLO20830
C     SPECIAL TITLE FOR RATIO.                                          PLO20840
C                                                                       PLO20850
   40 IF(MFIN.NE.203) GO TO 70                                          PLO20860
C-----IDENTIFY AS RATIO.                                                PLO20870
      CALL SYMBLH(XINCH(1),TOP1,HT,RATIO,0.0,5)                         PLO20880
C-----DEFINE NUMERATOR ZA AND MT. PACK TOGETHER AND PLOT.               PLO20890
      CALL MTHOL(MT,MTBCD,IMTBCD,MSTAT2)                                PLO20900
      CALL ZAHOL(IZA,MSTAT1,ZAPBCD,KZAP)                                PLO20910
      CALL PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)                             PLO20920
      JMTBCD=IMTBCD                                                     PLO20930
      CALL LEFTY(MTBCD,IMTBCD,KMTBCD,JMTBCD)                            PLO20940
      XI1=XMID-WTH*FLOAT(KMTBCD)                                        PLO20950
      XIMIN=XI1                                                         PLO20960
      CALL SYMBLH(XI1,TOP2,HT,MTBCD,0.0,IMTBCD)                         PLO20970
C-----DEFINE DENOMINATOR ZA AND MT. PACK TOGETHER AND PLOT.             PLO20980
      IF(IZARAT.LE.0.OR.MTRAT.LE.0) GO TO 50                            PLO20990
      CALL MTHOL(MTRAT,MTBCD,IMTBCD,MSTAR2)                             PLO21000
      CALL ZAHOL(IZARAT,MSTAR1,ZAPBCD,KZAP)                             PLO21010
      CALL PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)                             PLO21020
      JMTBCD=IMTBCD                                                     PLO21030
      CALL LEFTY(MTBCD,IMTBCD,KMTBCD,JMTBCD)                            PLO21040
      XI=XMID-WTH*FLOAT(KMTBCD)                                         PLO21050
      IF(XI.LT.XIMIN) XIMIN=XI                                          PLO21060
      CALL SYMBLH(XI,TOP3,HT,MTBCD,0.0,IMTBCD)                          PLO21070
      GO TO 60                                                          PLO21080
C-----DENOMINATOR ZA AND/OR MT IF NOT DEFINED.                          PLO21090
   50 XI=XMID-WTH*9.0                                                   PLO21100
      IF(XI.LT.XIMIN) XIMIN=XI                                          PLO21110
      CALL SYMBLH(XI,TOP3,HT,UNDEFI,0.0,9)                              PLO21120
C-----PLOT LINE BETWEEN NUMERATOR AND DENOMINATOR.                      PLO21130
   60 XIMIN=XIMIN-WTH                                                   PLO21140
      XIMAX=XMID+(XMID-XIMIN)                                           PLO21150
      YI3=TOP2-0.375*HT                                                 PLO21160
      CALL PLOT(XIMIN,YI3,3)                                            PLO21170
      CALL PLOT(XIMAX,YI3,2)                                            PLO21180
      GO TO 110                                                         PLO21190
C-----IDENTIFY MATERIAL.                                                PLO21200
   70 XZ=XINCH(1)                                                       PLO21210
      CALL SYMBLH(XZ,TOP1,HT,ZABCD,0.0,IZABCD)                          PLO21220
C-----IF DOUBLE DIFFERENTAIL PRINT SECONDARY ENERGY OR COSINE.          PLO21230
      IF(MF.NE.6) GO TO 90                                              PLO21240
      XZ=XINCH(1)                                                       PLO21250
      IF(IDOUB.EQ.2) GO TO 80                                           PLO21260
C-----PRINT SECONDARY ENERGY IN NORMAL FORM.                            PLO21270
      CALL SYMBLH(XZ,BOTX4,HT,EN2,0.0,4)                                PLO21280
      XZ=XZ+5.0*WT                                                      PLO21290
      CALL EUNIT(FIELD4(3),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)                 PLO21300
      ENOUT=FIELD4(3)*ZMULT                                             PLO21310
      CALL NUMBRH(XZ,BOTX4,HT,ENOUT,0.0,IPTZ)                           PLO21320
      XZ=XZ+7.0*WT                                                      PLO21330
      CALL SYMBLH(XZ,BOTX4,HT,ZUNIT,0.0,IZUNIT)                         PLO21340
      GO TO 90                                                          PLO21350
C-----PRINT COSINE.                                                     PLO21360
C***** OLD
C  80 CALL SYMBLH(XZ,BOTX4,HT,COS1,0.0,5)                               PLO21370
C     XZ=XZ+6.0*WT                                                      PLO21380
C     CALL NUMBRH(XZ,BOTX4,HT,FIELD4(1),0.0,4)                          PLO21390
C***** OLD
C***** TRKOV
   80 IF(MFIN.EQ.5) GO TO 82
      CALL SYMBLH(XZ,BOTX4,HT,COS1,0.0,5)
      XZ=XZ+6.0*WT
      DEG=180.*ACOS(FIELD4(1))/3.14159654
      CALL NUMBRH(XZ,BOTX4,HT,DEG,0.0,0)
      GO TO 84
   82 CALL SYMBLH(XZ,BOTX4,HT,COSA,0.0,15)
   84 CONTINUE
C***** TRKOV
C                                                                       PLO21400
C     IDENTIFY DATA TYPE AND REACTION.                                  PLO21410
C                                                                       PLO21420
   90 CALL MTHOL(MT,MTBCD,IMTBCD,MSTAT2)                                PLO21430
      CALL MFHOL(MFIN,MT,MFBCD,IMFBCD)                                  PLO21440
C-----IDENTIFY REACTION.                                                PLO21450
      JMTBCD=IMTBCD                                                     PLO21460
      CALL LEFTY(MTBCD,IMTBCD,KMTBCD,JMTBCD)                            PLO21470
      XI=XMID-WTH*FLOAT(KMTBCD)                                         PLO21480
      CALL SYMBLH(XI,TOP2,HT,MTBCD,0.0,IMTBCD)                          PLO21490
C-----IDENTIFY DATA TYPE.                                               PLO21500
      IF(MF.NE.3) GO TO 100                                             PLO21510
      JMFBCD=IMFBCD                                                     PLO21520
      CALL LEFTY(MFBCD,IMFBCD,KMFBCD,JMFBCD)                            PLO21530
      XI1=XMID-WTH*FLOAT(KMFBCD)                                        PLO21540
      CALL SYMBLH(XI1,TOP3,HT,MFBCD,0.0,IMFBCD)                         PLO21550
      GO TO 110                                                         PLO21560
C-----IF NOT CROSS SECTION PRECEED DATA TYPE BY ENERGY.                 PLO21570
  100 CALL EUNIT(ENEX,ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)                      PLO21580
      JMFBCD=IMFBCD                                                     PLO21590
      CALL LEFTY(MFBCD,IMFBCD,KMFBCD,JMFBCD)                            PLO21600
      XI1=XMID-WTH*FLOAT(8+IZUNIT+KMFBCD)                               PLO21610
      ENOUT=ENEX*ZMULT                                                  PLO21620
      CALL NUMBRH(XI1,TOP3,HT,ENOUT,0.0,IPTZ)                           PLO21630
      XI1=XI1+7.0*WT                                                    PLO21640
      CALL SYMBLH(XI1,TOP3,HT,ZUNIT,0.0,IZUNIT)                         PLO21650
      XI1=XI1+WT*FLOAT(1+IZUNIT)                                        PLO21660
      CALL SYMBLH(XI1,TOP3,HT,MFBCD,0.0,IMFBCD)                         PLO21670
C-----IDENTIFY ENDF/B OR EXFOR DATA.                                    PLO21680
  110 IF(MYMODE.LT.2) GO TO 120                                         PLO21690
      XZ=XINCH(2)-FLOAT(11+ILIB)*WT                                     PLO21700
      CALL SYMBLH(XZ,TOP1,HT,LIBNAM,0.0,ILIB)                           PLO21710
      XZ=XZ+FLOAT(1+ILIB)*WT                                            PLO21720
      XMAT=MAT                                                          PLO21730
      CALL NUMBRH(XZ,TOP1,HT,XMAT,0.0,-1)                               PLO21740
      XZ=XZ+5.0*WT                                                      PLO21750
      CALL SYMBLH(XZ,TOP1,HT,'Mod',0.0,3)                               PLO21760
      XZ=XZ+4.0*WT                                                      PLO21770
      XMAT=MODIZA                                                       PLO21780
      CALL NUMBRH(XZ,TOP1,HT,XMAT,0.0,-1)                               PLO21790
      GO TO 160                                                         PLO21800
C-----IF ONLY ONE REFERENCE PRINT EXFOR ACCESSION/SUB-ACCESSION NUMBER. PLO21810
  120 IF(MREF.GT.1.AND.KGROUP.EQ.0) GO TO 150                           PLO21820
      XZ=XINCH(2)-WT*FLOAT(ILIB)                                        PLO21830
      CALL SYMBLH(XZ,TOP1,HT,LIBNAM,0.0,ILIB)                           PLO21840
C-----PRINT STATUS.                                                     PLO21850
      DO 130 I=1,7                                                      PLO21860
      IF(STATUS.EQ.STAT1(I)) GO TO 140                                  PLO21870
  130 CONTINUE                                                          PLO21880
      GO TO 160                                                         PLO21890
  140 XZ1=XINCH(2)-12.0*WT                                              PLO21900
      CALL SYMBLH(XZ1,BOTX4,HT,STATAB(1,I),0.0,12)                      PLO21910
      GO TO 160                                                         PLO21920
  150 XZ=XINCH(2)-10.0*WT                                               PLO21930
      CALL SYMBLH(XZ,TOP1,HT,'EXFOR DATA',0.0,10)                       PLO21940
C                                                                       PLO21950
C     PLOT BORDER FOR FIGURE                                            PLO21960
C                                                                       PLO21970
  160 CALL PLOTP(XLIM(2),YLIM(1),3)                                     PLO21980
      CALL PLOTP(XLIM(1),YLIM(1),2)                                     PLO21990
      CALL PLOTP(XLIM(1),YLIM(2),2)                                     PLO22000
      CALL PLOTP(XLIM(2),YLIM(2),2)                                     PLO22010
      CALL PLOTP(XLIM(2),YLIM(1),2)                                     PLO22020
C                                                                       PLO22030
C     PLOT X AXIS LABEL AND UNITS.                                      PLO22040
C                                                                       PLO22050
      JXLAB=IXLAB                                                       PLO22060
      CALL LEFTY(XLABEL,IXLAB,KXLAB,JXLAB)                              PLO22070
      XI=XMID-WTH*FLOAT(KXLAB)                                          PLO22080
      CALL SYMBLH(XI,BOTX3,HT,XLABEL,0.0,IXLAB)                         PLO22090
      IF(IWAY(1).EQ.1) GO TO 240                                        PLO22100
C                                                                       PLO22110
C     PLOT X SCALE LOG10 DECADES.                                       PLO22120
C                                                                       PLO22130
      IXMIN=XLIM(1)                                                     PLO22140
      IF(XLIM(1).LT.0.0) IXMIN=IXMIN-1                                  PLO22150
      IXMAX=XLIM(2)+1.0                                                 PLO22160
      XDEC=XLIM(2)-XLIM(1)                                              PLO22170
      IF(XDEC.LE.10.0) GO TO 170                                        PLO22180
      ILOG1=10                                                          PLO22190
      KLOG1=10                                                          PLO22200
      GO TO 200                                                         PLO22210
  170 IF(XDEC.LE.6.0) GO TO 180                                         PLO22220
      ILOG1=5                                                           PLO22230
      KLOG1=5                                                           PLO22240
      GO TO 200                                                         PLO22250
  180 IF(XDEC.LE.3.0) GO TO 190                                         PLO22260
      ILOG1=2                                                           PLO22270
      KLOG1=2                                                           PLO22280
      GO TO 200                                                         PLO22290
  190 ILOG1=2                                                           PLO22300
      KLOG1=1                                                           PLO22310
  200 DO 230 I=IXMIN,IXMAX                                              PLO22320
      XR=I                                                              PLO22330
      IF(XR.LT.XLIM(1).OR.XR.GT.XLIM(2)) GO TO 210                      PLO22340
      XI=((XR-XLIM(1))*XINCH(2)+(XLIM(2)-XR)*XINCH(1))/(XLIM(2)-XLIM(1))PLO22350
      CALL SYMBLH(XI-WT,BOT2,HT,'10',0.0,2)                             PLO22360
      CALL NUMBRH (XI+WT,BOT1,HT,XR,0.0,-1)                             PLO22370
  210 IF(ILOG1.GT.9) GO TO 230                                          PLO22380
      DO 220 J=ILOG1,9,KLOG1                                            PLO22390
      XZ=XR+TABLOG(J)                                                   PLO22400
      IF(XZ.LT.XLIM(1).OR.XZ.GT.XLIM(2)) GO TO 220                      PLO22410
      XI=((XZ-XLIM(1))*XINCH(2)+(XLIM(2)-XZ)*XINCH(1))/(XLIM(2)-XLIM(1))PLO22420
      CALL SYMBLH(XI-WT38,BOT1,HT34,DIGITS(J),0.0,1)                    PLO22430
  220 CONTINUE                                                          PLO22440
  230 CONTINUE                                                          PLO22450
      GO TO 270                                                         PLO22460
C                                                                       PLO22470
C     PLOT X SCALE LINEAR UNITS.                                        PLO22480
C                                                                       PLO22490
  240 XR=XBASE                                                          PLO22500
  250 IF(XR.LT.XLIM(1)) GO TO 260                                       PLO22510
      IF(XR.GT.XLIM(2)) GO TO 270                                       PLO22520
      XI=((XR-XLIM(1))*XINCH(2)+(XLIM(2)-XR)*XINCH(1))/(XLIM(2)-XLIM(1))PLO22530
      XI=XI-WTH                                                         PLO22540
      IF(MF.EQ.4.AND.ABS(XR).LT.0.01) XR=0.0                            PLO22550
      IF(MF.EQ.6.AND.IDOUB.EQ.1.AND.ABS(XR).LT.0.01) XR=0.0             PLO22560
      IF(XR.LT.0.0) XI=XI-WTH                                           PLO22570
      IF(XR.GE.10.0) XI=XI-WTH                                          PLO22580
      IF(XR.GE.100.0) XI=XI-WTH                                         PLO22590
      IF(IXSTEP.EQ.1) XI=XI-WT                                          PLO22600
      IF(IXSTEP.EQ.2) XI=XI-WTH                                         PLO22610
      IF(IXSTEP.EQ.3) XI=XI-WTH                                         PLO22620
      CALL NUMBRH (XI,BOT1,HT,XR,0.0,IXSTEP)                            PLO22630
  260 XR=XR+XSTEP1                                                      PLO22640
      GO TO 250                                                         PLO22650
C                                                                       PLO22660
C     PLOT Y AXIS LABEL AND UNITS.                                      PLO22670
C                                                                       PLO22680
  270 JYLAB=IYLAB                                                       PLO22690
      CALL LEFTY(YLABEL,IYLAB,KYLAB,JYLAB)                              PLO22700
      YI=YMID-WTH*FLOAT(IYLAB)                                          PLO22710
      CALL SYMBLH(RIGHT2,YI,HT,YLABEL,90.0,IYLAB)                       PLO22720
      IF(IWAY(2).EQ.1) GO TO 350                                        PLO22730
C                                                                       PLO22740
C     PLOT Y SCALE LOG10 DECADES.                                       PLO22750
C                                                                       PLO22760
      IYMIN=YLIM(1)                                                     PLO22770
      IF(YLIM(1).LT.0.0) IYMIN=IYMIN-1                                  PLO22780
      IYMAX=YLIM(2)+1.0                                                 PLO22790
      YDEC=YLIM(2)-YLIM(1)                                              PLO22800
      IF(YDEC.LE.10.0) GO TO 280                                        PLO22810
      ILOG1=10                                                          PLO22820
      KLOG1=10                                                          PLO22830
      GO TO 310                                                         PLO22840
  280 IF(YDEC.LE.6.0) GO TO 290                                         PLO22850
      ILOG1=5                                                           PLO22860
      KLOG1=5                                                           PLO22870
      GO TO 310                                                         PLO22880
  290 IF(YDEC.LE.3.0) GO TO 300                                         PLO22890
      ILOG1=2                                                           PLO22900
      KLOG1=2                                                           PLO22910
      GO TO 310                                                         PLO22920
  300 ILOG1=2                                                           PLO22930
      KLOG1=1                                                           PLO22940
  310 DO 340 I=IYMIN,IYMAX                                              PLO22950
      YR=I                                                              PLO22960
      IF(YR.LT.YLIM(1).OR.YR.GT.YLIM(2)) GO TO 320                      PLO22970
      YI=((YR-YLIM(1))*YINCH(2)+(YLIM(2)-YR)*YINCH(1))/(YLIM(2)-YLIM(1))PLO22980
      CALL SYMBLH(RIGHT1,YI,HT,'10',0.0,2)                              PLO22990
      CALL NUMBRH (RIGHT1+2.0*WT,YI+HT,HT,YR,0.0,-1)                    PLO23000
  320 IF(ILOG1.GT.9) GO TO 340                                          PLO23010
      DO 330 J=ILOG1,9,KLOG1                                            PLO23020
      YZ=YR+TABLOG(J)                                                   PLO23030
      IF(YZ.LT.YLIM(1).OR.YZ.GT.YLIM(2)) GO TO 330                      PLO23040
      YI=((YZ-YLIM(1))*YINCH(2)+(YLIM(2)-YZ)*YINCH(1))/(YLIM(2)-YLIM(1))PLO23050
      CALL SYMBLH(RIGHT1,YI,HT34,DIGITS(J),0.0,1)                       PLO23060
  330 CONTINUE                                                          PLO23070
  340 CONTINUE                                                          PLO23080
      GO TO 400                                                         PLO23090
C                                                                       PLO23100
C     PLOT Y SCALE LINEAR UNITS.                                        PLO23110
C                                                                       PLO23120
  350 YR=YBASE                                                          PLO23130
      YTOP=YR                                                           PLO23140
  360 IF(YR.GT.YLIM(2)) GO TO 370                                       PLO23150
      YTOP=YR                                                           PLO23160
      YR=YR+YSTEP1                                                      PLO23170
      GO TO 360                                                         PLO23180
  370 KT=1                                                              PLO23190
      IF(YTOP.GE.10.0) KT=2                                             PLO23200
      IF(YTOP.GE.100.0) KT=3                                            PLO23210
      YR=YBASE                                                          PLO23220
  380 IF(YR.LT.YLIM(1)) GO TO 390                                       PLO23230
      IF(YR.GT.YLIM(2)) GO TO 400                                       PLO23240
      YI=((YR-YLIM(1))*YINCH(2)+(YLIM(2)-YR)*YINCH(1))/(YLIM(2)-YLIM(1))PLO23250
      KN=1                                                              PLO23260
      IF(YR.GE.10.0) KN=2                                               PLO23270
      IF(YR.GE.100.0) KN=3                                              PLO23280
      KD=KT-KN+1                                                        PLO23290
      XI=XINCH(2)+KD*WT                                                 PLO23300
      CALL NUMBRH (XI,YI+HTH,HT,YR,0.0,IYSTEP)                          PLO23310
  390 YR=YR+YSTEP1                                                      PLO23320
      GO TO 380                                                         PLO23330
C                                                                       PLO23340
C     PLOT BORDERS FOR REFERENCES                                       PLO23350
C                                                                       PLO23360
  400 YR1=YINCH(2)                                                      PLO23370
      YR2=YINCH(2)-1.75*(MREF+2)*HT                                     PLO23380
      IF(KGROUP.NE.0) YR2=YR2-1.75*HT                                   PLO23390
      CALL SYMBLH(RIGHT5,YR1-1.75*HT,HT,'Reference',0.0,9)              PLO23400
      IF(KGROUP.EQ.0) GO TO 410                                         PLO23410
      CALL LEFTY(REFS(1,1),IRR,KRR,25)                                  PLO23420
      CALL SYMBLH(RIGHT5,YR1-3.5*HT,HT,REFS(1,1),0.0,IRR)               PLO23430
  410 CALL PLOT(RIGHT3,YR1,3)                                           PLO23440
      CALL PLOT(RIGHT4,YR1,2)                                           PLO23450
      CALL PLOT(RIGHT4,YR2,2)                                           PLO23460
      CALL PLOT(RIGHT3,YR2,2)                                           PLO23470
      CALL PLOT(RIGHT3,YR1,2)                                           PLO23480
C***** TRKOV Suppress Range plot if too many references
      IF(YINCH(2)-YINCH(1).LT. 1.75*(MREF+2)*HT*2) RETURN
C***** TRKOV
      YR1=YINCH(1)                                                      PLO23490
      YR2=YINCH(1)+1.75*(MREF+2)*HT                                     PLO23500
      YR3=YR2-1.75*HT                                                   PLO23510
      IF(MF.EQ.7.OR.MF.EQ.8) GO TO 420                                  PLO23520
      IF(MF.EQ.4) GO TO 430                                             PLO23530
      IF(MF.EQ.6.AND.IDOUB.EQ.1) GO TO 430                              PLO23540
      CALL SYMBLH(RIGHT5,YR3,HT,'Energy Range',0.0,12)                  PLO23550
      GO TO 440                                                         PLO23560
  420 CALL SYMBLH(RIGHT5,YR3,HT,'Range',0.0,5)                          PLO23570
      GO TO 440                                                         PLO23580
  430 CALL SYMBLH(RIGHT5,YR3,HT,'Cosine Range',0.0,12)                  PLO23590
  440 CALL SYMBLH(RIGHT6,YR3,HT,'Points',0.0,6)                         PLO23600
      CALL PLOT(RIGHT3,YR1,3)                                           PLO23610
      CALL PLOT(RIGHT4,YR1,2)                                           PLO23620
      CALL PLOT(RIGHT4,YR2,2)                                           PLO23630
      CALL PLOT(RIGHT3,YR2,2)                                           PLO23640
      CALL PLOT(RIGHT3,YR1,2)                                           PLO23650
      RETURN                                                            PLO23660
      END                                                               PLO23670
      SUBROUTINE EUNIT(Z,ZMULT,ZUNIT,IZUNIT,LPTZ,IPTZ,IZWAY)            PLO23680
C                                                                       PLO23690
C     DEFINE MULTIPLIER AND UNITS TO PUT ENERGY IN NORMAL FORM AND      PLO23700
C     NUMBER OF DECIMAL PLACES TO OUTPUT.                               PLO23710
C                                                                       PLO23720
      CHARACTER*4 ZUNIT,UNTAB                                           PLO23730
      DIMENSION ZUNIT(2),RANGER(6),IUNIT(6),UNTAB(2,6)                  PLO23740
C-----DEFINE MULTIPLIERS, LENGTH OF UNIT TITLE AND UNIT TITLES.         PLO23750
      DATA RANGER/1.0E+9,1.0E+6,1.0E+3,1.0,1.0E-3,1.0E-6/               PLO23760
      DATA IUNIT/3,3,3,2,8,8/                                           PLO23770
      DATA UNTAB/                                                       PLO23780
     1 'GeV ','    ',                                                   PLO23790
     2 'MeV ','    ',                                                   PLO23800
     3 'KeV ','    ',                                                   PLO23810
     4 'eV  ','    ',                                                   PLO23820
     5 'mill','i-eV',                                                   PLO23830
     6 'micr','o-eV'/                                                   PLO23840
C-----USE EV FOR LOG SCALING OR ZERO VALUES.                            PLO23850
      IF(IZWAY.EQ.1.AND.ABS(Z).NE.0.0) GO TO 10                         PLO23860
      I=4                                                               PLO23870
      GO TO 30                                                          PLO23880
C-----SELECT MULTIPLIER.                                                PLO23890
   10 DO 20 I=1,6                                                       PLO23900
      IF(Z.GE.RANGER(I)) GO TO 30                                       PLO23910
   20 CONTINUE                                                          PLO23920
      I=6                                                               PLO23930
C-----DEFINE UNITS AND MULTIPLIER.                                      PLO23940
   30 ZUNIT(1)=UNTAB(1,I)                                               PLO23950
      ZUNIT(2)=UNTAB(2,I)                                               PLO23960
      IZUNIT=IUNIT(I)                                                   PLO23970
      ZMULT=1.0/RANGER(I)                                               PLO23980
C-----DEFINE NUMBER OF DECIMAL PLACES TO OUTPUT.                        PLO23990
      ZX=Z*ZMULT                                                        PLO24000
      IPTZ=LPTZ                                                         PLO24010
      IF(ZX.GE.9.9999) IPTZ=IPTZ-1                                      PLO24020
      IF(ZX.GE.99.999) IPTZ=IPTZ-1                                      PLO24030
      IF(ZX.GE.999.99) IPTZ=IPTZ-1                                      PLO24040
      IF(IPTZ.LT.-1) IPTZ=-1                                            PLO24050
      RETURN                                                            PLO24060
      END                                                               PLO24070
      SUBROUTINE HLUNIT(HL,HLMULT,HLOUT,IHLOUT,LPTZ,IPTZ)               PLO24080
C                                                                       PLO24090
C     DEFINE CONVERSION FACTORS FROM SECONDS TO VARIABLE UNITS.         PLO24100
C                                                                       PLO24110
      CHARACTER*4 HLTAB,HLOUT                                           PLO24120
      DIMENSION HLTAB(2,9),HLTIME(9),IHLTAB(9),HLOUT(2)                 PLO24130
      DATA IHLTAB/4,4,5,4,3,3,4,3,4/                                    PLO24140
      DATA HLTAB/                                                       PLO24150
     1 'psec','    ',                                                   PLO24160
     2 'nsec','    ',                                                   PLO24170
     3 'muse','c   ',                                                   PLO24180
     4 'msec','    ',                                                   PLO24190
     5 'sec ','    ',                                                   PLO24200
     6 'min ','    ',                                                   PLO24210
     7 'hour','    ',                                                   PLO24220
     8 'day ','    ',                                                   PLO24230
     9 'year','    '/                                                   PLO24240
      DATA HLTIME/                                                      PLO24250
     1 1.00000E-12,                                                     PLO24260
     2 1.00000E- 9,                                                     PLO24270
     3 1.00000E- 6,                                                     PLO24280
     4 1.00000E- 3,                                                     PLO24290
     5 1.00000E+ 0,                                                     PLO24300
     6 6.00000E+ 1,                                                     PLO24310
     7 3.60000E+ 2,                                                     PLO24320
     8 8.64000E+ 3,                                                     PLO24330
     9 3.15576E+ 6/                                                     PLO24340
      HL=ABS(HL)                                                        PLO24350
C-----IF HALF-LIFE IS ZERO USE SECONDS.                                 PLO24360
      IF(HL.GT.0.0) GO TO 10                                            PLO24370
      I=5                                                               PLO24380
      GO TO 30                                                          PLO24390
C-----SELECT APPROPRIATE UNITS (WITH NON-ZERO LEADING DIGIT).           PLO24400
   10 I=9                                                               PLO24410
      DO 20 J=1,9                                                       PLO24420
      IF(HL.GT.HLTIME(I)) GO TO 30                                      PLO24430
   20 I=I-1                                                             PLO24440
      I=1                                                               PLO24450
C-----DEFINE UNITS AND MULTIPLIER.                                      PLO24460
   30 HLOUT(1)=HLTAB(1,I)                                               PLO24470
      HLOUT(2)=HLTAB(2,I)                                               PLO24480
      IHLOUT=IHLTAB(I)                                                  PLO24490
      HLMULT=1.0/HLTIME(I)                                              PLO24500
C-----DEFINE NUMBER OF DIGITS AFTER DECIMAL POINT.                      PLO24510
      ZHL=HL*HLMULT                                                     PLO24520
      IPTZ=LPTZ                                                         PLO24530
      ZMAX=9.9999                                                       PLO24540
   40 IF(IPTZ.LT.0) GO TO 50                                            PLO24550
      IF(ZHL.LT.ZMAX) GO TO 50                                          PLO24560
      IPTZ=IPTZ-1                                                       PLO24570
      ZMAX=10.0*ZMAX                                                    PLO24580
      GO TO 40                                                          PLO24590
   50 RETURN                                                            PLO24600
      END                                                               PLO24610
      SUBROUTINE CSUNIT(YLIM,YMULT,YLABEL,IYLAB,IYWAY)                  PLO24620
C                                                                       PLO24630
C     SELECT UNITS Y. DEFINE Y AXIS LABEL AND SCALE FACTOR.             PLO24640
C                                                                       PLO24650
      CHARACTER*4 YLABEL,YUNIT                                          PLO24660
      DIMENSION YLIM(2),YLABEL(10),YUNIT(3,6),RANGER(6),IYUNIT(6)       PLO24670
      DATA RANGER/1.0E+9,1.0E+6,1.0E+3,1.0,1.0E-3,1.0E-6/               PLO24680
      DATA IYUNIT/10,10,10,5,11,11/                                     PLO24690
      DATA YUNIT/                                                       PLO24700
     1 'Gega','-Bar','ns  ',                                            PLO24710
     2 'Mega','-Bar','ns  ',                                            PLO24720
     3 'Kilo','-Bar','ns  ',                                            PLO24730
     4 'Barn','s   ','    ',                                            PLO24740
     5 'mill','i-Ba','rns ',                                            PLO24750
     6 'micr','o-Ba','rns '/                                            PLO24760
C-----USE BARNS FOR LOG SCALING.                                        PLO24770
      IF(IYWAY.EQ.1) GO TO 10                                           PLO24780
      I=4                                                               PLO24790
      GO TO 30                                                          PLO24800
C-----LINEAR SCALING...USE VARIABLE UNITS.                              PLO24810
   10 DO 20 I=1,6                                                       PLO24820
      IF(YLIM(2).GE.RANGER(I)) GO TO 30                                 PLO24830
   20 CONTINUE                                                          PLO24840
      I=6                                                               PLO24850
   30 YMULT=1.0/RANGER(I)                                               PLO24860
      YLIM(1)=YLIM(1)*YMULT                                             PLO24870
      YLIM(2)=YLIM(2)*YMULT                                             PLO24880
      IYLAB=IYUNIT(I)                                                   PLO24890
      DO 40 J=1,3                                                       PLO24900
   40 YLABEL(J)=YUNIT(J,I)                                              PLO24910
      RETURN                                                            PLO24920
      END                                                               PLO24930
C***** OLD
C     SUBROUTINE PERUN(YLABEL,IYLAB,MF,ODOUB)                           PLO24940
C***** OLD
C***** TRKOV - Change to keep compilers happy (IDOUB is not used)
      SUBROUTINE PERUN(YLABEL,IYLAB,MF,IDOUB)
C***** TRKOV
C                                                                       PLO24950
C     IF NOT MF=3 ADD APPROPRIATE UNITS TO Y AXIS UNITS.                PLO24960
C                                                                       PLO24970
      CHARACTER*1 YLABEL,PEREV,PERST,PEREST                             PLO24980
      DIMENSION YLABEL(40),PEREV(3),PERST(10),PEREST(13)                PLO24990
      DATA PEREV/'/','e','V'/                                           PLO25000
      DATA PERST/'/','s','t','e','r','a','d','i','a','n'/               PLO25010
      DATA PEREST/'/','e','V','/','s','t','e','r','a','d','i','a','n'/  PLO25020
      IF(MF.EQ.4) GO TO 10                                              PLO25030
      IF(MF.EQ.5) GO TO 30                                              PLO25040
      IF(MF.EQ.6) GO TO 50                                              PLO25050
      RETURN                                                            PLO25060
C-----ADD /STERADIAN.                                                   PLO25070
   10 DO 20 I=1,10                                                      PLO25080
      IYLAB=IYLAB+1                                                     PLO25090
   20 YLABEL(IYLAB)=PERST(I)                                            PLO25100
      RETURN                                                            PLO25110
C-----ADD /EV.                                                          PLO25120
   30 DO 40 I=1,3                                                       PLO25130
      IYLAB=IYLAB+1                                                     PLO25140
   40 YLABEL(IYLAB)=PEREV(I)                                            PLO25150
      RETURN                                                            PLO25160
C-----ADD /EV/STERADIAN.                                                PLO25170
   50 DO 60 I=1,13                                                      PLO25180
      IYLAB=IYLAB+1                                                     PLO25190
   60 YLABEL(IYLAB)=PEREST(I)                                           PLO25200
      RETURN                                                            PLO25210
      END                                                               PLO25220
      SUBROUTINE GRID0                                                  PLO25230
C                                                                       PLO25240
C     PLOT X AND Y AXIS TICK MARKS.                                     PLO25250
C                                                                       PLO25260
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO25270
      COMMON/WAYS/IWAY(2)                                               PLO25280
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,        PLO25290
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP                                PLO25300
      COMMON/LOGTAB/TABLOG(10)                                          PLO25310
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO25320
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO25330
      CALL PEN(2)                                                       PLO25340
      YR1=YLIM(1)                                                       PLO25350
      BOX2P=BOX2*(YLIM(2)-YLIM(1))/(YINCH(2)-YINCH(1))                  PLO25360
      YR2=YR1+BOX2P                                                     PLO25370
      IF(IWAY(1).EQ.1) GO TO 90                                         PLO25380
C                                                                       PLO25390
C     PLOT X SCALE LOG10 DECADES.                                       PLO25400
C                                                                       PLO25410
      DO 80 LOOP=1,2                                                    PLO25420
      IXMIN=XLIM(1)                                                     PLO25430
      IF(XLIM(1).LT.0.0) IXMIN=IXMIN-1                                  PLO25440
      IXMAX=XLIM(2)+1.0                                                 PLO25450
      XDEC=XLIM(2)-XLIM(1)                                              PLO25460
      IF(XDEC.LE.10.0) GO TO 10                                         PLO25470
      ILOG1=10                                                          PLO25480
      KLOG1=10                                                          PLO25490
      GO TO 40                                                          PLO25500
   10 IF(XDEC.LE.6.0) GO TO 20                                          PLO25510
      ILOG1=5                                                           PLO25520
      KLOG1=5                                                           PLO25530
      GO TO 40                                                          PLO25540
   20 IF(XDEC.LE.3.0) GO TO 30                                          PLO25550
      ILOG1=2                                                           PLO25560
      KLOG1=2                                                           PLO25570
      GO TO 40                                                          PLO25580
   30 ILOG1=2                                                           PLO25590
      KLOG1=1                                                           PLO25600
   40 DO 70 I=IXMIN,IXMAX                                               PLO25610
      XR=I                                                              PLO25620
      IF(XR.LT.XLIM(1).OR.XR.GT.XLIM(2)) GO TO 50                       PLO25630
      CALL PLOTP(XR,YR1,3)                                              PLO25640
      CALL PLOTP(XR,YR2,2)                                              PLO25650
      IF(ILOG1.GT.9) GO TO 70                                           PLO25660
   50 DO 60 J=ILOG1,9,KLOG1                                             PLO25670
      XZ=XR+TABLOG(J)                                                   PLO25680
      IF(XZ.LT.XLIM(1).OR.XZ.GT.XLIM(2)) GO TO 60                       PLO25690
      CALL PLOTP(XZ,YR1,3)                                              PLO25700
      CALL PLOTP(XZ,YR2,2)                                              PLO25710
   60 CONTINUE                                                          PLO25720
   70 CONTINUE                                                          PLO25730
      YR1=YLIM(2)                                                       PLO25740
      YR2=YR1-BOX2P                                                     PLO25750
   80 CONTINUE                                                          PLO25760
      GO TO 140                                                         PLO25770
C                                                                       PLO25780
C     PLOT X SCALE LINEAR UNITS.                                        PLO25790
C                                                                       PLO25800
   90 DO 130 LOOP=1,2                                                   PLO25810
      XR=XBASE                                                          PLO25820
  100 IF(XR.LT.XLIM(1)) GO TO 110                                       PLO25830
      IF(XR.GT.XLIM(2)) GO TO 120                                       PLO25840
      CALL PLOTP(XR,YR1,3)                                              PLO25850
      CALL PLOTP(XR,YR2,2)                                              PLO25860
  110 XR=XR+XSTEP2                                                      PLO25870
      GO TO 100                                                         PLO25880
  120 YR1=YLIM(2)                                                       PLO25890
      YR2=YR1-BOX2P                                                     PLO25900
  130 CONTINUE                                                          PLO25910
  140 XR1=XLIM(1)                                                       PLO25920
      BOX2P=BOX2*(XLIM(2)-XLIM(1))/(XINCH(2)-XINCH(1))                  PLO25930
      XR2=XR1+BOX2P                                                     PLO25940
      IF(IWAY(2).EQ.1) GO TO 230                                        PLO25950
C                                                                       PLO25960
C     PLOT Y SCALE LOG10 DECADES.                                       PLO25970
C                                                                       PLO25980
      DO 220 LOOP=1,2                                                   PLO25990
      IYMIN=YLIM(1)                                                     PLO26000
      IF(YLIM(1).LT.0.0) IYMIN=IYMIN-1                                  PLO26010
      IYMAX=YLIM(2)+1.0                                                 PLO26020
      YDEC=YLIM(2)-YLIM(1)                                              PLO26030
      IF(YDEC.LE.10.0) GO TO 150                                        PLO26040
      ILOG1=10                                                          PLO26050
      KLOG1=10                                                          PLO26060
      GO TO 180                                                         PLO26070
  150 IF(YDEC.LE.6.0) GO TO 160                                         PLO26080
      ILOG1=5                                                           PLO26090
      KLOG1=5                                                           PLO26100
      GO TO 180                                                         PLO26110
  160 IF(YDEC.LE.3.0) GO TO 170                                         PLO26120
      ILOG1=2                                                           PLO26130
      KLOG1=2                                                           PLO26140
      GO TO 180                                                         PLO26150
  170 ILOG1=2                                                           PLO26160
      KLOG1=1                                                           PLO26170
  180 DO 210 I=IYMIN,IYMAX                                              PLO26180
      YR=I                                                              PLO26190
      IF(YR.LT.YLIM(1).OR.YR.GT.YLIM(2)) GO TO 190                      PLO26200
      CALL PLOTP(XR1,YR,3)                                              PLO26210
      CALL PLOTP(XR2,YR,2)                                              PLO26220
  190 IF(ILOG1.GT.9) GO TO 210                                          PLO26230
      DO 200 J=ILOG1,9,KLOG1                                            PLO26240
      YZ=YR+TABLOG(J)                                                   PLO26250
      IF(YZ.LT.YLIM(1).OR.YZ.GT.YLIM(2)) GO TO 200                      PLO26260
      CALL PLOTP(XR1,YZ,3)                                              PLO26270
      CALL PLOTP(XR2,YZ,2)                                              PLO26280
  200 CONTINUE                                                          PLO26290
  210 CONTINUE                                                          PLO26300
      XR1=XLIM(2)                                                       PLO26310
      XR2=XR1-BOX2P                                                     PLO26320
  220 CONTINUE                                                          PLO26330
      GO TO 280                                                         PLO26340
C                                                                       PLO26350
C     PLOT Y SCALE LINEAR UNITS.                                        PLO26360
C                                                                       PLO26370
  230 DO 270 LOOP=1,2                                                   PLO26380
      YR=YBASE                                                          PLO26390
  240 IF(YR.LT.YLIM(1)) GO TO 250                                       PLO26400
      IF(YR.GT.YLIM(2)) GO TO 260                                       PLO26410
      CALL PLOTP(XR1,YR,3)                                              PLO26420
      CALL PLOTP(XR2,YR,2)                                              PLO26430
  250 YR=YR+YSTEP2                                                      PLO26440
      GO TO 240                                                         PLO26450
  260 XR1=XLIM(2)                                                       PLO26460
      XR2=XR1-BOX2P                                                     PLO26470
  270 CONTINUE                                                          PLO26480
  280 RETURN                                                            PLO26490
      END                                                               PLO26500
      SUBROUTINE GRID1                                                  PLO26510
C                                                                       PLO26520
C     PLOT X AND Y AXIS FULL GRID                                       PLO26530
C                                                                       PLO26540
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO26550
      COMMON/WAYS/IWAY(2)                                               PLO26560
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,        PLO26570
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP                                PLO26580
      COMMON/LOGTAB/TABLOG(10)                                          PLO26590
      CALL PEN(1)                                                       PLO26600
      IDIR=1                                                            PLO26610
      IF(IWAY(1).EQ.1) GO TO 120                                        PLO26620
C                                                                       PLO26630
C     PLOT X SCALE LOG10 DECADES.                                       PLO26640
C                                                                       PLO26650
      IXMIN=XLIM(1)                                                     PLO26660
      IF(XLIM(1).LT.0.0) IXMIN=IXMIN-1                                  PLO26670
      IXMAX=XLIM(2)+1.0                                                 PLO26680
      XDEC=XLIM(2)-XLIM(1)                                              PLO26690
      IF(XDEC.LE.10.0) GO TO 10                                         PLO26700
      ILOG1=10                                                          PLO26710
      KLOG1=10                                                          PLO26720
      GO TO 40                                                          PLO26730
   10 IF(XDEC.LE.6.0) GO TO 20                                          PLO26740
      ILOG1=5                                                           PLO26750
      KLOG1=5                                                           PLO26760
      GO TO 40                                                          PLO26770
   20 IF(XDEC.LE.3.0) GO TO 30                                          PLO26780
      ILOG1=2                                                           PLO26790
      KLOG1=2                                                           PLO26800
      GO TO 40                                                          PLO26810
   30 ILOG1=2                                                           PLO26820
      KLOG1=1                                                           PLO26830
   40 DO 110 I=IXMIN,IXMAX                                              PLO26840
      XR=I                                                              PLO26850
      IF(XR.LT.XLIM(1).OR.XR.GT.XLIM(2)) GO TO 70                       PLO26860
      IF(IDIR.EQ.1) GO TO 50                                            PLO26870
      CALL PLOTP(XR,YLIM(2),3)                                          PLO26880
      CALL PLOTP(XR,YLIM(1),2)                                          PLO26890
      GO TO 60                                                          PLO26900
   50 CALL PLOTP(XR,YLIM(1),3)                                          PLO26910
      CALL PLOTP(XR,YLIM(2),2)                                          PLO26920
   60 IDIR=3-IDIR                                                       PLO26930
   70 IF(ILOG1.GT.9) GO TO 110                                          PLO26940
      DO 100 J=ILOG1,9,KLOG1                                            PLO26950
      XZ=XR+TABLOG(J)                                                   PLO26960
      IF(XZ.LT.XLIM(1).OR.XZ.GT.XLIM(2)) GO TO 100                      PLO26970
      IF(IDIR.EQ.1) GO TO 80                                            PLO26980
      CALL PLOTP(XZ,YLIM(2),3)                                          PLO26990
      CALL PLOTP(XZ,YLIM(1),2)                                          PLO27000
      GO TO 90                                                          PLO27010
   80 CALL PLOTP(XZ,YLIM(1),3)                                          PLO27020
      CALL PLOTP(XZ,YLIM(2),2)                                          PLO27030
   90 IDIR=3-IDIR                                                       PLO27040
  100 CONTINUE                                                          PLO27050
  110 CONTINUE                                                          PLO27060
      GO TO 160                                                         PLO27070
C                                                                       PLO27080
C     PLOT X SCALE LINEAR UNITS.                                        PLO27090
C                                                                       PLO27100
  120 XR=XBASE                                                          PLO27110
  130 IF(XR.LT.XLIM(1)) GO TO 150                                       PLO27120
      IF(XR.GT.XLIM(2)) GO TO 160                                       PLO27130
      IF(IDIR.EQ.1) GO TO 140                                           PLO27140
      CALL PLOTP(XR,YLIM(2),3)                                          PLO27150
      CALL PLOTP(XR,YLIM(1),2)                                          PLO27160
      GO TO 150                                                         PLO27170
  140 CALL PLOTP(XR,YLIM(1),3)                                          PLO27180
      CALL PLOTP(XR,YLIM(2),2)                                          PLO27190
  150 IDIR=3-IDIR                                                       PLO27200
      XR=XR+XSTEP2                                                      PLO27210
      GO TO 130                                                         PLO27220
  160 IDIR=1                                                            PLO27230
      IF(IWAY(2).EQ.1) GO TO 280                                        PLO27240
C                                                                       PLO27250
C     PLOT Y SCALE LOG10 DECADES.                                       PLO27260
C                                                                       PLO27270
      IYMIN=YLIM(1)                                                     PLO27280
      IF(YLIM(1).LT.0.0) IYMIN=IYMIN-1                                  PLO27290
      IYMAX=YLIM(2)+1.0                                                 PLO27300
      YDEC=YLIM(2)-YLIM(1)                                              PLO27310
      IF(YDEC.LE.10.0) GO TO 170                                        PLO27320
      ILOG1=10                                                          PLO27330
      KLOG1=10                                                          PLO27340
      GO TO 200                                                         PLO27350
  170 IF(YDEC.LE.6.0) GO TO 180                                         PLO27360
      ILOG1=5                                                           PLO27370
      KLOG1=5                                                           PLO27380
      GO TO 200                                                         PLO27390
  180 IF(YDEC.LE.3.0) GO TO 190                                         PLO27400
      ILOG1=2                                                           PLO27410
      KLOG1=2                                                           PLO27420
      GO TO 200                                                         PLO27430
  190 ILOG1=2                                                           PLO27440
      KLOG1=1                                                           PLO27450
  200 DO 270 I=IYMIN,IYMAX                                              PLO27460
      YR=I                                                              PLO27470
      IF(YR.LT.YLIM(1).OR.YR.GT.YLIM(2)) GO TO 230                      PLO27480
      IF(IDIR.EQ.1) GO TO 210                                           PLO27490
      CALL PLOTP(XLIM(2),YR,3)                                          PLO27500
      CALL PLOTP(XLIM(1),YR,2)                                          PLO27510
      GO TO 220                                                         PLO27520
  210 CALL PLOTP(XLIM(1),YR,3)                                          PLO27530
      CALL PLOTP(XLIM(2),YR,2)                                          PLO27540
  220 IDIR=3-IDIR                                                       PLO27550
  230 IF(ILOG1.GT.9) GO TO 270                                          PLO27560
      DO 260 J=ILOG1,9,KLOG1                                            PLO27570
      YZ=YR+TABLOG(J)                                                   PLO27580
      IF(YZ.LT.YLIM(1).OR.YZ.GT.YLIM(2)) GO TO 260                      PLO27590
      IF(IDIR.EQ.1) GO TO 240                                           PLO27600
      CALL PLOTP(XLIM(2),YZ,3)                                          PLO27610
      CALL PLOTP(XLIM(1),YZ,2)                                          PLO27620
      GO TO 250                                                         PLO27630
  240 CALL PLOTP(XLIM(1),YZ,3)                                          PLO27640
      CALL PLOTP(XLIM(2),YZ,2)                                          PLO27650
  250 IDIR=3-IDIR                                                       PLO27660
  260 CONTINUE                                                          PLO27670
  270 CONTINUE                                                          PLO27680
      GO TO 320                                                         PLO27690
C                                                                       PLO27700
C     PLOT Y SCALE LINEAR UNITS.                                        PLO27710
C                                                                       PLO27720
  280 YR=YBASE                                                          PLO27730
  290 IF(YR.LT.YLIM(1)) GO TO 310                                       PLO27740
      IF(YR.GT.YLIM(2)) GO TO 320                                       PLO27750
      IF(IDIR.EQ.1) GO TO 300                                           PLO27760
      CALL PLOTP(XLIM(2),YR,3)                                          PLO27770
      CALL PLOTP(XLIM(1),YR,2)                                          PLO27780
      GO TO 310                                                         PLO27790
  300 CALL PLOTP(XLIM(1),YR,3)                                          PLO27800
      CALL PLOTP(XLIM(2),YR,2)                                          PLO27810
  310 IDIR=3-IDIR                                                       PLO27820
      YR=YR+YSTEP2                                                      PLO27830
      GO TO 290                                                         PLO27840
  320 RETURN                                                            PLO27850
      END                                                               PLO27860
      SUBROUTINE EVALP                                                  PLO27870
C                                                                       PLO27880
C     PLOT EVALUATED DATA.                                              PLO27890
C                                                                       PLO27900
      COMMON/WAYS/IWAY(2)                                               PLO27910
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO27920
      COMMON/XYREAL/XREAL(2),YREAL(2)                                   PLO27930
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,        PLO27940
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP                                PLO27950
C***** TRKOV
      COMMON/PAGEXY/XPAGE(90000),YPAGE(90000),N2,IBASE,ITOP,ISCR          PLO08310
C***** TRKOV
C***** OLD
C     COMMON/PAGEXY/XPAGE(9000),YPAGE(9000),N2,IBASE,ITOP,ISCR          PLO08310
C***** OLD
      IF(N2.LE.0) RETURN                                                PLO27970
      CALL PEN(2)                                                       PLO27980
      IPASS=0                                                           PLO27990
      KTOP=0                                                            PLO28000
C                                                                       PLO28010
C     SET UP LOOP OVER POINTS.                                          PLO28020
C                                                                       PLO28030
      I=0                                                               PLO28040
   10 I=I+1                                                             PLO28050
C                                                                       PLO28060
C     SELECT NEXT POINT AND INITIALIZE FLAG TO INDICATE POINT IS NO PLOTPLO28070
C                                                                       PLO28080
   20 XNOW=X(I)                                                         PLO28090
      YNOW=Y(I)                                                         PLO28100
      IMON=1                                                            PLO28110
C                                                                       PLO28120
C     SELECT POINTS IN ENERGY RANGE OF PLOT.                            PLO28130
C                                                                       PLO28140
C***** OLD
C     IF(XNOW-XREAL(1)) 140,90,30                                       PLO28150
C  30 IF(XNOW-XREAL(2)) 70,40,40                                        PLO28160
C***** OLD
C***** TRKOV
      IF(XNOW.LT.XREAL(1)                 ) GO TO 140
      IF(YNOW.LT.YREAL(1) .AND. IPASS.EQ.0) GO TO 140
      IF(XNOW.LT.XREAL(2)) GO TO 70
C***** TRKOV
C                                                                       PLO28170
C     END OF PLOTTING RANGE REACHED. IF NO PRECEDING POINTS (ALL DATA   PLO28180
C     ABOVE RANGE OF PLOT) RETURN                                       PLO28190
C                                                                       PLO28200
   40 IF(I.LE.1) GO TO 150                                              PLO28210
C                                                                       PLO28220
C     IF NO POINTS YET PLOTTED (I.E., LAST POINT BELOW PLOT RANGE,      PLO28230
C     CURRENT POINT ABOVE RANGE) RESET POINT INDEX TO DO UPPER LIMIT    PLO28240
C     NEXT TIME THROUGH LOOP. THIS TIME INTERPOLATE TO LOWER ENERGY     PLO28250
C     LIMIT AND TOP/BOTTOM OF PLOT.                                     PLO28260
C                                                                       PLO28270
      IF(IPASS.GT.0) GO TO 50                                           PLO28280
      I=I-1                                                             PLO28290
      GO TO 80                                                          PLO28300
C                                                                       PLO28310
C     SET FLAG TO INDICATE END OF PLOTTING RANGE.                       PLO28320
C     INTERPOLATE TO UPPER ENERGY LIMIT AND TOP/BOTTOM OF PLOT.         PLO28330
C                                                                       PLO28340
   50 IF(XNOW.GT.XREAL(2)) IMON=0                                       PLO28350
      KTOP=1                                                            PLO28360
      YNOW=((XNOW-XREAL(2))*YLAST+(XREAL(2)-XLAST)*YNOW)/(XNOW-XLAST)   PLO28370
      XNOW=XREAL(2)                                                     PLO28380
      IF(YNOW.LE.YREAL(2)) GO TO 60                                     PLO28390
      IMON=0                                                            PLO28400
      XNOW=((YNOW-YREAL(2))*XLAST+(YREAL(2)-YLAST)*XNOW)/(YNOW-YLAST)   PLO28410
      YNOW=YREAL(2)                                                     PLO28420
      GO TO 110                                                         PLO28430
   60 IF(YNOW.GE.YREAL(1)) GO TO 110                                    PLO28440
      IMON=0                                                            PLO28450
      XNOW=((YNOW-YREAL(1))*XLAST+(YREAL(1)-YLAST)*XNOW)/(YNOW-YLAST)   PLO28460
      YNOW=YREAL(1)                                                     PLO28470
      GO TO 110                                                         PLO28480
C                                                                       PLO28490
C     INTERPOLATE TO LOWER ENERGY LIMIT AND TOP/BOTTOM OF PLOT          PLO28500
C     UNLESS FIRST POINT OR AT LEAST ONE POINT HAS ALREADY BEEN PLOTTED.PLO28510
C                                                                       PLO28520
   70 IF(I.LE.1.OR.IPASS.GT.0) GO TO 110                                PLO28530
   80 IMON=0                                                            PLO28540
C***** OLD
C     YNOW=((XNOW-XREAL(1))*YLAST+(XREAL(1)-XLAST)*YNOW)/(XNOW-XLAST)   PLO28550
C     XNOW=XREAL(1)                                                     PLO28560
C***** OLD
C***** TRKOV - GUARD AGAINST OVERFLOW
      IF(XNOW.GT.XLAST)
     1YNOW=((XNOW-XREAL(1))*YLAST+(XREAL(1)-XLAST)*YNOW)/(XNOW-XLAST)
      XNOW=XREAL(1)
C***** TRKOV
   90 IF(YNOW.LE.YREAL(2)) GO TO 100                                    PLO28570
      IMON=0                                                            PLO28580
C***** OLD
C     XNOW=((YNOW-YREAL(2))*XLAST+(YREAL(2)-YLAST)*XNOW)/(YNOW-YLAST)   PLO28590
C***** OLD
C***** TRKOV - GUARD AGAINST OVERFLOW
      IF(YNOW.LT.YLAST)
     1XN  =((YNOW-YREAL(2))*XLAST+(YREAL(2)-YLAST)*XNOW)/(YNOW-YLAST)
      XNOW=MAX(XN,XNOW)
C***** TRKOV
      YNOW=YREAL(2)
      GO TO 110                                                         PLO28610
  100 IF(YNOW.GE.YREAL(1)) GO TO 110                                    PLO28620
      IMON=0                                                            PLO28630
C***** OLD
C     XNOW=((YNOW-YREAL(1))*XLAST+(YREAL(1)-YLAST)*XNOW)/(YNOW-YLAST)   PLO28640
C***** OLD
C***** TRKOV - GUARD AGAINST OVERFLOW
      IF(YNOW.LT.YLAST)
     1XN  =((YNOW-YREAL(1))*XLAST+(YREAL(1)-YLAST)*XNOW)/(YNOW-YLAST)
      XNOW=MIN(XN,XNOW)
C***** TRKOV
      YNOW=YREAL(1)                                                     PLO28650
C                                                                       PLO28660
C     LIMIT X AND Y TO RANGE OF THE PLOT. CONVERT TO PLOT UNITS.        PLO28670
C                                                                       PLO28680
  110 IF(XNOW.LT.XREAL(1)) XNOW=XREAL(1)                                PLO28690
      IF(XNOW.GT.XREAL(2)) XNOW=XREAL(2)                                PLO28700
      IF(YNOW.LT.YREAL(1)) YNOW=YREAL(1)                                PLO28710
      IF(YNOW.GT.YREAL(2)) YNOW=YREAL(2)                                PLO28720
      XL=XNOW*XMULT                                                     PLO28730
      YL=YNOW*YMULT                                                     PLO28740
C                                                                       PLO28750
C     MOVE OR PLOT TO POINT.                                            PLO28760
C                                                                       PLO28770
      IF(IPASS.GT.0) GO TO 120                                          PLO28780
      CALL PLOTI(XL,YL,3,IMON)                                          PLO28790
      IPASS=1                                                           PLO28800
C-----IF FIRST POINT USED FOR INTERPOLATION BRANCH BACK TO NOW TREAT    PLO28810
C-----FIRST POINT.                                                      PLO28820
      IF(XNOW-X(I)) 20,130,130                                          PLO28830
C     IF(I-1) 35,35,3                                                   PLO28840
  120 CALL PLOTI(XL,YL,2,IMON)                                          PLO28850
  130 IF(KTOP.GT.0) GO TO 150                                           PLO28860
C                                                                       PLO28870
C     SAVE COORDINATES FOR INTERPOLATION.                               PLO28880
C                                                                       PLO28890
  140 XLAST=XNOW                                                        PLO28900
      YLAST=YNOW                                                        PLO28910
      IF(I.LT.N2) GO TO 10                                              PLO28920
  150 RETURN                                                            PLO28930
      END                                                               PLO28940
      SUBROUTINE EXFORP                                                 PLO28950
C                                                                       PLO28960
C     PLOT EXPERIMENTAL DATA...EACH REF SEPARATELY.                     PLO28970
C                                                                       PLO28980
      CHARACTER*4 HEADER,REFS,REF1,REFNUM,XLABEL,YLABEL,ZUNIT,IM78,HL,  PLO28990
     1 DEF78                                                            PLO29000
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO29010
      COMMON/XYREAL/XREAL(2),YREAL(2)                                   PLO29020
C***** OLD
C     COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,MNEX                           PLO29030
C     COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),        PLO29040
C    1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),NGET,IGET              PLO29050
C***** OLD
C***** TRKOV
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX,AWR
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
C***** TRKOV
      COMMON/XYLABC/XLABEL(10),YLABEL(10)                               PLO29060
      COMMON/WAYS/IWAY(2)                                               PLO29070
      COMMON/WHO78C/IM78                                                PLO29080
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,        PLO29090
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP                                PLO29100
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO29110
C***** OLD
C     COMMON/EXFOR/XEX(1000),DXEX(1000),YEX(1000),DYEX(1000),NREF(1000),PLO29120
C    1 E2(1000),IEX                                                     PLO29130
C     COMMON/REFERI/LREF(26),EXLOW(26),EXHIGH(26),E2T(26),IREF,MREF,    PLO29140
C    1 MAXREF,IGROUP,KGROUP                                             PLO29150
C     COMMON/REFERC/REFS(9,26),REF1(9)                                  PLO29160
C***** OLD
C***** TRKOV
      COMMON/EXFOR/XEX(10000),DXEX(10000),YEX(10000),DYEX(10000)
     1,NREF(10000),E2(10000),IEX
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP                                             PLO08450
      COMMON/REFERC/REFS(9,48),REF1(9)
C***** TRKOV
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO29170
      COMMON/DOUBL/IDOUB,FIELD4(4)                                      PLO29180
      COMMON/SYMBLM/MSYMBL                                              PLO29190
      COMMON/X4LIMS/X4LIMX(2),X4LIMY(2)                                 PLO29200
      COMMON/SPOTS/TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGHT1,RIGHT2,RIGHT3,  PLO29210
     1 RIGHT4,RIGHT5,RIGHT6,RIGHT7,RIGHT8                               PLO29220
      DIMENSION REFNUM(26),ZUNIT(2),DEF78(3)                            PLO29230
      DATA HL/' HL'/                                                    PLO29240
      DATA REFNUM/'1','2','3','4','5','6','7','8','9','A',              PLO29250
     1 'B','C','D','E','F','G','H','J','K','L',                         PLO29260
     2 'M','N','P','R','S','X'/                                         PLO29270
C-----NOTHING TO DO IF NO EXFOR DATA.                                   PLO29280
      IF(IEX.LE.0) RETURN                                               PLO29290
C-----INITIALIZE LEGEND BOX COORDINATES.                                PLO29300
      YNOW=YINCH(2)-3.5*HT                                              PLO29310
      IF(KGROUP.NE.0) YNOW=YNOW-1.75*HT                                 PLO29320
      YBOT=YINCH(1)+1.75*MREF*HT                                        PLO29330
C                                                                       PLO29340
C     SELECT REFERENCES IN ENERGY RANGE.                                PLO29350
C                                                                       PLO29360
      DO 190 KREF=1,IREF                                                PLO29370
      IF(LREF(KREF).LE.0) GO TO 190                                     PLO29380
C                                                                       PLO29390
C     IDENTIFY REFERENCE.                                               PLO29400
C                                                                       PLO29410
      IPEN=KREF+2                                                       PLO29420
      CALL PEN(IPEN)                                                    PLO29430
      IF(MSYMBL.EQ.0.AND.MREF.EQ.1.AND.KGROUP.EQ.0) GO TO 50            PLO29440
C-----IDENTIFY SYMBOL USED FOR EACH REFERENCE.                          PLO29450
      CALL PLOT (RIGHT7,YNOW-BOX,3)                                     PLO29460
      CALL PLOT (RIGHT8,YNOW-BOX,2)                                     PLO29470
      CALL PLOT (RIGHT8,YNOW+BOX,2)                                     PLO29480
      CALL PLOT (RIGHT7,YNOW+BOX,2)                                     PLO29490
      CALL PLOT (RIGHT7,YNOW-BOX,2)                                     PLO29500
      CALL SYMBLH(RIGHT7+BOX-BOXWT2,YNOW-BOX2,BOX,REFNUM(KREF),0.0,1)   PLO29510
      IF(KGROUP.EQ.0) GO TO 50                                          PLO29520
      IF(MF.NE.3) GO TO 10                                              PLO29530
      IF(IDOUB.EQ.2) GO TO 30                                           PLO29540
C-----IDENTIFY SECOND ENERGY.                                           PLO29550
   10 CALL WHAT78(IM78,DEF78,IDEF78)                                    PLO29560
      CALL SYMBLH(RIGHT5,YNOW-HTH,HT,DEF78,0.0,IDEF78)                  PLO29570
      XNOW2=RIGHT5+FLOAT(IDEF78+1)*WT                                   PLO29580
      CALL SYMBLH(XNOW2,YNOW-HTH,HT,'=',0.0,1)                          PLO29590
      XNOW2=XNOW2+2.0*WT                                                PLO29600
C-----SELECT HALF-LIFE OR ENERGY UNITS.                                 PLO29610
      IF(IM78.NE.HL) GO TO 20                                           PLO29620
      CALL HLUNIT(E2T(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ)                  PLO29630
      GO TO 40                                                          PLO29640
   20 CALL EUNIT(E2T(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)                 PLO29650
      GO TO 40                                                          PLO29660
C-----IDENTIFY INCIDENT ENERGY.                                         PLO29670
   30 CALL EUNIT(E2T(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)                 PLO29680
      CALL SYMBLH(RIGHT5,YNOW-HTH,HT,'E = ',0.0,4)                      PLO29690
      XNOW2=RIGHT5+4.0*WT                                               PLO29700
C-----PLOT E2 FIELD AND UNITS.                                          PLO29710
   40 E2OUT=E2T(KREF)*ZMULT                                             PLO29720
      CALL NUMBRH(XNOW2,YNOW-HTH,HT,E2OUT,0.0,IPTZ)                     PLO29730
      CALL SYMBLH(XNOW2+7.0*WT,YNOW-HTH,HT,ZUNIT,0.0,IZUNIT)            PLO29740
      GO TO 60                                                          PLO29750
C-----IDENTIFY REFERENCE.                                               PLO29760
   50 CALL LEFTY(REFS(1,KREF),IRR,KRR,25)                               PLO29770
      CALL SYMBLH(RIGHT5,YNOW-HTH,HT,REFS(1,KREF),0.0,IRR)              PLO29780
   60 IF(MSYMBL.EQ.0.AND.MREF.EQ.1.AND.KGROUP.EQ.0) GO TO 70            PLO29790
C***** TRKOV Suppress Range plot if too many references
      IF(YINCH(2)-YINCH(1).LT. 1.75*(MREF+2)*HT*2) GO TO 92
C***** TRKOV
C-----IDENTIFY REFERENCE IN RANGE BOX.                                  PLO29800
      CALL PLOT (RIGHT7,YBOT-BOX,3)                                     PLO29810
      CALL PLOT (RIGHT8,YBOT-BOX,2)                                     PLO29820
      CALL PLOT (RIGHT8,YBOT+BOX,2)                                     PLO29830
      CALL PLOT (RIGHT7,YBOT+BOX,2)                                     PLO29840
      CALL PLOT (RIGHT7,YBOT-BOX,2)                                     PLO29850
      CALL SYMBLH(RIGHT7+BOX-BOXWT2,YBOT-BOX2,BOX,REFNUM(KREF),0.0,1)   PLO29860
C-----PLOT RANGE.                                                       PLO29870
   70 IF(MF.EQ.3.OR.MF.EQ.5) GO TO 80                                   PLO29880
      IF(MF.EQ.6.AND.IDOUB.EQ.2) GO TO 80                               PLO29890
C-----PRINT COSINE, LEGENDRE ORDER OR ATOMIC WEIGHT RANGE.              PLO29900
      IPTZ=-1                                                           PLO29910
      IF(MF.EQ.4) IPTZ=4                                                PLO29920
      IF(MF.EQ.6.AND.IDOUB.EQ.1) IPTZ=4                                 PLO29930
      CALL NUMBRH(RIGHT5,YBOT-HTH,HT,EXLOW(KREF),0.0,IPTZ)              PLO29940
      XNOW2=RIGHT5+3.0*WT                                               PLO29950
      IF(IPTZ.EQ.4) XNOW2=XNOW2+5.0*WT                                  PLO29960
      IF(EXLOW(KREF).LE.0.0) XNOW2=XNOW2+WT                             PLO29970
      IF(EXLOW(KREF).GT.10.0) XNOW2=XNOW2+WT                            PLO29980
      IF(EXLOW(KREF).GT.100.0) XNOW2=XNOW2+WT                           PLO29990
      CALL NUMBRH(XNOW2,YBOT-HTH,HT,EXHIGH(KREF),0.0,IPTZ)              PLO30000
      GO TO 90                                                          PLO30010
C-----PRINT ENERGY IN NORMAL FORM.                                      PLO30020
   80 CALL EUNIT(EXHIGH(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)              PLO30030
      EXL=EXLOW(KREF)*ZMULT                                             PLO30040
      JPTX=4                                                            PLO30050
      IF(EXL.GE.10.0) JPTX=3                                            PLO30060
      IF(EXL.GE.100.0) JPTX=2                                           PLO30070
      CALL NUMBRH(RIGHT5,YBOT-HTH,HT,EXL,0.0,JPTX)                      PLO30080
      EXH=EXHIGH(KREF)*ZMULT                                            PLO30090
      CALL NUMBRH(RIGHT5+8.0*WT,YBOT-HTH,HT,EXH,0.0,IPTZ)               PLO30100
      CALL SYMBLH(RIGHT5+15.0*WT,YBOT-HTH,HT,ZUNIT,0.0,IZUNIT)          PLO30110
C-----PRINT POINT COUNT RIGHT ADJUSTED.                                 PLO30120
   90 XBOTX=RIGHT6                                                      PLO30130
      XREF=LREF(KREF)                                                   PLO30140
      IF(XREF.LT.100000.0) XBOTX=XBOTX+WT                               PLO30150
      IF(XREF.LT.10000.0) XBOTX=XBOTX+WT                                PLO30160
      IF(XREF.LT.1000.0) XBOTX=XBOTX+WT                                 PLO30170
      IF(XREF.LT.100.0) XBOTX=XBOTX+WT                                  PLO30180
      IF(XREF.LT.10.0) XBOTX=XBOTX+WT                                   PLO30190
      CALL NUMBRH(XBOTX,YBOT-HTH,HT,XREF,0.0,-1)                        PLO30200
C***** TRKOV
   92 CONTINUE
C***** TRKOV
      YNOW=YNOW-1.75*HT                                                 PLO30210
      YBOT=YBOT-1.75*HT                                                 PLO30220
C                                                                       PLO30230
C     SELECT POINTS IN ENERGY RANGE AND PLOT THEM.                      PLO30240
C                                                                       PLO30250
      DO 180 I=1,IEX                                                    PLO30260
C-----SELECT POINTS FROM CURRENT REFERENCE WHICH ARE WITHIN THE PLOTTINGPLO30270
C-----AREA.                                                             PLO30280
      IF(NREF(I).NE.KREF) GO TO 180                                     PLO30290
      IF(XEX(I).LT.X4LIMX(1).OR.XEX(I).GT.X4LIMX(2)) GO TO 180          PLO30300
      IF(YEX(I).LT.X4LIMY(1).OR.YEX(I).GT.X4LIMY(2)) GO TO 180          PLO30310
C-----DEFINE X COORDINATES AND TRUNCATE TO LIMITS OF PLOT.              PLO30320
      XL=XEX(I)                                                         PLO30330
      DXL=DXEX(I)                                                       PLO30340
      XM=XL-DXL                                                         PLO30350
      XP=XL+DXL                                                         PLO30360
      IF(XM.LT.XREAL(1)) XM=XREAL(1)                                    PLO30370
      IF(XP.GT.XREAL(2)) XP=XREAL(2)                                    PLO30380
      XM=XM*XMULT                                                       PLO30390
      XL=XL*XMULT                                                       PLO30400
      XP=XP*XMULT                                                       PLO30410
C-----IF REQUIRED CONVERT TO LOGS.                                      PLO30420
      IF(IWAY(1).EQ.1) GO TO 100                                        PLO30430
      XL=ALOG10(XL)                                                     PLO30440
      XM=ALOG10(XM)                                                     PLO30450
      XP=ALOG10(XP)                                                     PLO30460
C-----TRANSFORM TO COORDINATES OF THE PLOT.                             PLO30470
  100 XL=((XL-XLIM(1))*XINCH(2)+(XLIM(2)-XL)*XINCH(1))/(XLIM(2)-XLIM(1))PLO30480
      XM=((XM-XLIM(1))*XINCH(2)+(XLIM(2)-XM)*XINCH(1))/(XLIM(2)-XLIM(1))PLO30490
      XP=((XP-XLIM(1))*XINCH(2)+(XLIM(2)-XP)*XINCH(1))/(XLIM(2)-XLIM(1))PLO30500
C-----DEFINE Y COORDINATES AND TRUNCATE TO LIMITS OF PLOT.              PLO30510
      YL=YEX(I)                                                         PLO30520
      DYL=DYEX(I)                                                       PLO30530
      YM=YL-DYL                                                         PLO30540
      YP=YL+DYL                                                         PLO30550
      IF(YM.LT.YREAL(1)) YM=YREAL(1)                                    PLO30560
      IF(YP.GT.YREAL(2)) YP=YREAL(2)                                    PLO30570
      YM=YM*YMULT                                                       PLO30580
      YL=YL*YMULT                                                       PLO30590
      YP=YP*YMULT                                                       PLO30600
C-----IF REQUIRED CONVERT TO LOGS.                                      PLO30610
      IF(IWAY(2).EQ.1) GO TO 110                                        PLO30620
      YL=ALOG10(YL)                                                     PLO30630
      YM=ALOG10(YM)                                                     PLO30640
      YP=ALOG10(YP)                                                     PLO30650
C-----TRANSFORM TO COORDINATES OF THE PLOT.                             PLO30660
  110 YL=((YL-YLIM(1))*YINCH(2)+(YLIM(2)-YL)*YINCH(1))/(YLIM(2)-YLIM(1))PLO30670
      YM=((YM-YLIM(1))*YINCH(2)+(YLIM(2)-YM)*YINCH(1))/(YLIM(2)-YLIM(1))PLO30680
      YP=((YP-YLIM(1))*YINCH(2)+(YLIM(2)-YP)*YINCH(1))/(YLIM(2)-YLIM(1))PLO30690
C-----IF ONLY ONE REFERENCE AND NOT IDENTIFYING E2 DO NOT PLOT SYMBOL.  PLO30700
      IF(MSYMBL.NE.0.OR.MREF.GT.1.OR.KGROUP.NE.0) GO TO 140             PLO30710
C                                                                       PLO30720
C     ONLY PLOT ERROR BARS AND X AT MIDDLE.                             PLO30730
C                                                                       PLO30740
      XLBM=XL-BOX4                                                      PLO30750
      XLBP=XL+BOX4                                                      PLO30760
      YLBM=YL-BOX4                                                      PLO30770
      YLBP=YL+BOX4                                                      PLO30780
C-----PLOT X ERROR BARS.                                                PLO30790
      IF(DXL.LE.0) GO TO 120                                            PLO30800
      CALL PLOT(XM,YLBM,3)                                              PLO30810
      CALL PLOT(XM,YLBP,2)                                              PLO30820
      CALL PLOT(XM,YL,3)                                                PLO30830
      CALL PLOT(XP,YL,2)                                                PLO30840
      CALL PLOT(XP,YLBM,3)                                              PLO30850
      CALL PLOT(XP,YLBP,2)                                              PLO30860
C-----PLOT Y ERROR BARS.                                                PLO30870
  120 IF(DYL.LE.0.0) GO TO 130                                          PLO30880
      CALL PLOT(XLBM,YM,3)                                              PLO30890
      CALL PLOT(XLBP,YM,2)                                              PLO30900
      CALL PLOT(XL,YM,3)                                                PLO30910
      CALL PLOT(XL,YP,2)                                                PLO30920
      CALL PLOT(XLBM,YP,3)                                              PLO30930
      CALL PLOT(XLBP,YP,2)                                              PLO30940
C-----PLOT X AT DATA POINT.                                             PLO30950
  130 CALL PLOT(XLBM,YLBP,3)                                            PLO30960
      CALL PLOT(XLBP,YLBM,2)                                            PLO30970
      CALL PLOT(XLBM,YLBM,3)                                            PLO30980
      CALL PLOT(XLBP,YLBP,2)                                            PLO30990
      GO TO 180                                                         PLO31000
C                                                                       PLO31010
C     PLOT SYMBOL AND ERROR BARS.                                       PLO31020
C                                                                       PLO31030
  140 XLBM=XL-BOX                                                       PLO31040
      XLBP=XL+BOX                                                       PLO31050
      YLBM=YL-BOX                                                       PLO31060
      YLBP=YL+BOX                                                       PLO31070
C-----PLOT BOX AND SYMBOL.                                              PLO31080
      CALL PLOT(XLBM,YLBM,3)                                            PLO31090
      CALL PLOT(XLBP,YLBM,2)                                            PLO31100
      CALL PLOT(XLBP,YLBP,2)                                            PLO31110
      CALL PLOT(XLBM,YLBP,2)                                            PLO31120
      CALL PLOT(XLBM,YLBM,2)                                            PLO31130
      CALL SYMBLH(XL-BOXWT2,YL-BOX2,BOX,REFNUM(KREF),0.0,1)             PLO31140
C-----PLOT X ERROR BARS IF THEY EXTEND BEYOND BOX.                      PLO31150
      IF(XM.GE.XLBM) GO TO 150                                          PLO31160
      CALL PLOT(XM,YLBM,3)                                              PLO31170
      CALL PLOT(XM,YLBP,2)                                              PLO31180
      CALL PLOT(XM,YL,3)                                                PLO31190
      CALL PLOT(XLBM,YL,2)                                              PLO31200
  150 IF(XP.LE.XLBP) GO TO 160                                          PLO31210
      CALL PLOT(XP,YLBM,3)                                              PLO31220
      CALL PLOT(XP,YLBP,2)                                              PLO31230
      CALL PLOT(XP,YL,3)                                                PLO31240
      CALL PLOT(XLBP,YL,2)                                              PLO31250
C-----PLOT Y ERROR BARS IF THEY EXTEND BEYOND BOX.                      PLO31260
  160 IF(YM.GE.YLBM) GO TO 170                                          PLO31270
      CALL PLOT(XLBM,YM,3)                                              PLO31280
      CALL PLOT(XLBP,YM,2)                                              PLO31290
      CALL PLOT(XL,YM,3)                                                PLO31300
      CALL PLOT(XL,YLBM,2)                                              PLO31310
  170 IF(YP.LE.YLBP) GO TO 180                                          PLO31320
      CALL PLOT(XLBM,YP,3)                                              PLO31330
      CALL PLOT(XLBP,YP,2)                                              PLO31340
      CALL PLOT(XL,YP,3)                                                PLO31350
      CALL PLOT(XL,YLBP,2)                                              PLO31360
  180 CONTINUE                                                          PLO31370
  190 CONTINUE                                                          PLO31380
      RETURN                                                            PLO31390
      END                                                               PLO31400
      SUBROUTINE PLOTP(X,Y,IPEN)                                        PLO31410
C                                                                       PLO31420
C     TRANSLATE DATA FROM THE PLANE OF THE DATA TO THE PLANE OF THE     PLO31430
C     PLOT.                                                             PLO31440
C                                                                       PLO31450
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO31460
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO31470
      XI=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))  PLO31480
      YI=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))  PLO31490
      CALL PLOT(XI,YI,IPEN)                                             PLO31500
      RETURN                                                            PLO31510
      END                                                               PLO31520
      SUBROUTINE PLOTI(XP,YP,IPEN,IMON)                                 PLO31530
C                                                                       PLO31540
C     TRANSLATE DATA FROM THE PLANE OF THE DATA TO THE PLANE OF THE     PLO31550
C     PLOT. IF NECESSARY INTERPOLATE DATA TO NON-LINEAR PLOT.           PLO31560
C                                                                       PLO31570
      COMMON/XYLIM/XLIM(2),YLIM(2)                                      PLO31580
      COMMON/INCHES/XINCH(2),YINCH(2)                                   PLO31590
      COMMON/WAYS/IWAY(2)                                               PLO31600
      COMMON/ENDFIM/IMENDF                                              PLO31610
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO31620
      DATA XPLAST/0.0/                                                  PLO31630
      DATA YPLAST/0.0/                                                  PLO31640
      DATA XILAST/0.0/                                                  PLO31650
      DATA YILAST/0.0/                                                  PLO31660
C-----DEFINE COORDINATES FOR INTERNAL USE.                              PLO31670
      X=XP                                                              PLO31680
      Y=YP                                                              PLO31690
C-----IF NECESSARY CONVERT TO LOG.                                      PLO31700
      IF(IWAY(1).EQ.2) X=ALOG10(X)                                      PLO31710
      IF(IWAY(2).EQ.2) Y=ALOG10(Y)                                      PLO31720
C-----CONVERT TO THE PLANE OF THE PLOT.                                 PLO31730
      XI=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))  PLO31740
      YI=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))  PLO31750
C-----IF MOVING, NOT PLOTTING MOVE (NO INTERPOLATION NECESSARY).        PLO31760
      IF(IPEN.EQ.3) GO TO 120                                           PLO31770
C-----IF PLOT IS LINEAR X VS. LINEAR Y DRAW STRAIGHT LINE.              PLO31780
      IF(IWAY(1).EQ.1.AND.IWAY(2).EQ.1) GO TO 120                       PLO31790
C-----CHECK FOR INTERPOLATION.                                          PLO31800
      DXINCH=XI-XILAST                                                  PLO31810
      DYINCH=YI-YILAST                                                  PLO31820
      ABSDX=ABS(DXINCH)                                                 PLO31830
      ABSDY=ABS(DYINCH)                                                 PLO31840
C-----IF NO CHANGE IN ONE COORDINATE DRAW STRIAGHT LINE.                PLO31850
      IF(ABSDX.LE.0.0.OR.ABSDY.LE.0.0) GO TO 120                        PLO31860
C-----CHECK FOR LARGEST CHANGE IN INCHES. IF LESS THAN 0.1 DO NOT       PLO31870
C-----INTERPOLATE.                                                      PLO31880
      IF(ABSDX.LT.ABSDY) GO TO 60                                       PLO31890
      IF(ABSDX.LE.0.1) GO TO 120                                        PLO31900
C-----INTERPOLATE IN X DIRECTION. DEFINE LINEAR OR LOG STEP CHANGE IN X.PLO31910
      ISTEP=ABSDX/0.1+1.0                                               PLO31920
      XSTEP=ISTEP                                                       PLO31930
      XZ=XPLAST                                                         PLO31940
      IF(IWAY(1).NE.1) GO TO 10                                         PLO31950
      DX=(XP-XPLAST)/XSTEP                                              PLO31960
      GO TO 20                                                          PLO31970
   10 DX=LOG(XP/XPLAST)/XSTEP                                           PLO31980
      DX=EXP(DX)                                                        PLO31990
C-----SET UP LOOP OVER INTERPOLATION STEPS.                             PLO32000
   20 DO 50 I=1,ISTEP                                                   PLO32010
      IF(I.EQ.ISTEP) GO TO 120                                          PLO32020
      IF(IWAY(1).NE.1) GO TO 30                                         PLO32030
      XZ=XZ+DX                                                          PLO32040
      GO TO 40                                                          PLO32050
   30 XZ=XZ*DX                                                          PLO32060
   40 X=XZ                                                              PLO32070
      Y=((X-XPLAST)*YP+(XP-X)*YPLAST)/(XP-XPLAST)                       PLO32080
      IF(IWAY(1).EQ.2) X=ALOG10(X)                                      PLO32090
      IF(IWAY(2).EQ.2) Y=ALOG10(Y)                                      PLO32100
      XT=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))  PLO32110
      YT=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))  PLO32120
   50 CALL PLOT(XT,YT,2)                                                PLO32130
   60 IF(ABSDY.LE.0.1) GO TO 120                                        PLO32140
C-----INTERPOLATE IN Y DIRECTION. DEFINE LINEAR OR LOG STEP CHANGE IN Y.PLO32150
      ISTEP=ABSDY/0.1+1.0                                               PLO32160
      YSTEP=ISTEP                                                       PLO32170
      YZ=YPLAST                                                         PLO32180
      IF(IWAY(2).NE.1) GO TO 70                                         PLO32190
      DY=(YP-YPLAST)/YSTEP                                              PLO32200
      GO TO 80                                                          PLO32210
   70 DY=LOG(YP/YPLAST)/YSTEP                                           PLO32220
      DY=EXP(DY)                                                        PLO32230
C-----SET UP LOOP OVER INTERPOLATION STEPS.                             PLO32240
   80 DO 110 I=1,ISTEP                                                  PLO32250
      IF(I.EQ.ISTEP) GO TO 120                                          PLO32260
      IF(IWAY(2).NE.1) GO TO 90                                         PLO32270
      YZ=YZ+DY                                                          PLO32280
      GO TO 100                                                         PLO32290
   90 YZ=YZ*DY                                                          PLO32300
  100 Y=YZ                                                              PLO32310
      X=((Y-YPLAST)*XP+(YP-Y)*XPLAST)/(YP-YPLAST)                       PLO32320
      IF(IWAY(1).EQ.2) X=ALOG10(X)                                      PLO32330
      IF(IWAY(2).EQ.2) Y=ALOG10(Y)                                      PLO32340
      XT=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))  PLO32350
      YT=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))  PLO32360
  110 CALL PLOT(XT,YT,2)                                                PLO32370
C-----PLOT OR MOVE TO FINAL LOCATION.                                   PLO32380
  120 CALL PLOT(XI,YI,IPEN)                                             PLO32390
C-----IF REQUESTED IDENTIFY ENDF/B DATA POINT.                          PLO32400
      IF(IMON.LE.0.OR.IMENDF.NE.2) GO TO 130                            PLO32410
      CALL PLOT(XI-BOX4,YI,3)                                           PLO32420
      CALL PLOT(XI,YI+BOX4,2)                                           PLO32430
      CALL PLOT(XI+BOX4,YI,2)                                           PLO32440
      CALL PLOT(XI,YI-BOX4,2)                                           PLO32450
      CALL PLOT(XI-BOX4,YI,2)                                           PLO32460
      CALL PLOT(XI,YI,3)                                                PLO32470
C-----SAVE COORDINATES.                                                 PLO32480
  130 XPLAST=XP                                                         PLO32490
      YPLAST=YP                                                         PLO32500
      XILAST=XI                                                         PLO32510
      YILAST=YI                                                         PLO32520
      RETURN                                                            PLO32530
      END                                                               PLO32540
      SUBROUTINE GETEX(IEND)                                            PLO32550
C                                                                       PLO32560
C     READ COMPARABLE EXFOR DATA BASED ON EITHER,                       PLO32570
C     (1) COMPARISON TO ENDF/B IZA/MF/MT, OR,                           PLO32580
C     (2) COMPARISON TO REQUESTS.                                       PLO32590
C                                                                       PLO32600
      INTEGER OUTP                                                      PLO32610
      CHARACTER*4 REFS,REF1,LIBNAM,ZABCD,MSTAT1,MSTA1X,FIELDC,BLANK,    PLO32620
     1 IM78,IM78X,MSTAT2,MSTA2X,HL                                      PLO32630
      CHARACTER*1 LABCM,LABX,STATUS,STATX,DUMMY,MSTAR1,MSTAR2,MSTA1R,   PLO32640
     1 MSTA2R,BLANK1,USTAT                                              PLO32650
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO32660
C***** OLD
C***** TRKOV
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
C***** TRKOV
      COMMON/LIBI/ILIB                                                  PLO32670
      COMMON/LIBC/LIBNAM(4)                                             PLO32680
      COMMON/WHO78C/IM78                                                PLO32690
      COMMON/WHO78I/IMAM78                                              PLO32700
      COMMON/INPARM/MINNIE,MAXIE                                        PLO32710
C***** OLD
C     COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX                           PLO32720
C***** OLD
C***** TRKOV
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,ENEX,AWR
C***** TRKOV
      COMMON/WHEREC/ZABCD(4),MSTAT1,MSTAT2                              PLO32730
C***** OLD
C     COMMON/EXFOR/XEX(1000),DXEX(1000),YEX(1000),DYEX(1000),NREF(1000),PLO32740
C    1 E2(1000),IEX                                                     PLO32750
C     COMMON/REFERI/LREF(26),EXLOW(26),EXHIGH(26),E2T(26),IREF,MREF,    PLO32760
C    1 MAXREF,IGROUP,KGROUP                                             PLO32770
C     COMMON/REFERC/REFS(9,26),REF1(9)                                  PLO32780
C***** OLD
C***** TRKOV
      COMMON/EXFOR/XEX(10000),DXEX(10000),YEX(10000),DYEX(10000)
     1,NREF(10000),E2(10000),IEX
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP                                             PLO08450
      COMMON/REFERC/REFS(9,48),REF1(9)
C***** TRKOV
      COMMON/SYSSTA/LABCM,STATUS                                        PLO32790
      COMMON/UNNORM/IMNORM                                              PLO32800
      COMMON/DOUBL/IDOUB,FIELD4(4)                                      PLO32810
      COMMON/MODEMY/MYMODE                                              PLO32820
      COMMON/EXERRS/IXERR,IYERR                                         PLO32830
      COMMON/RATZA/IZARAT,MTRAT,MFIN                                    PLO32840
      COMMON/RATZAC/MSTAR1,MSTAR2                                       PLO32850
C***** OLD
C     DIMENSION FIELDI(8),FIELDC(3,8),MZATAB(9,400),EXTRA(400)          PLO32860
C***** OLD
C***** TRKOV
      DIMENSION FIELDI(8),FIELDC(3,8),MZATAB(9,800),EXTRA(800)
      DATA FIELDC/'    ','    ','    ',
     2            '    ','    ','    ',
     3            '    ','    ','    ',
     4            '    ','    ','    ',
     5            '    ','    ','    ',
     6            '    ','    ','    ',
     7            '    ','    ','    ',
     8            '    ','    ','    '/
C***** TRKOV
      DATA IPASS/0/                                                     PLO32870
      DATA BLANK/'    '/                                                PLO32880
      DATA BLANK1/' '/                                                  PLO32890
      DATA HL/' HL'/                                                    PLO32900
      DATA IZATRY/0/                                                    PLO32910
      DATA MTTRY/0/                                                     PLO32920
      DATA MSTA1R/' '/                                                  PLO32930
      DATA MSTA2R/' '/                                                  PLO32940
      DATA USTAT/'U'/                                                   PLO32950
C                                                                       PLO32960
C     WHEN THIS ROUTINE IS CALLED THE FIRST TIME READ ENTIRE EXFOR      PLO32970
C     FILE AND SAVE INDICES TO REQUESTED ZA/MF/MT/POINT COUNTS.         PLO32980
C                                                                       PLO32990
      IF(IPASS.GT.0) GO TO 100                                          PLO33000
      NZATAB=0                                                          PLO33010
      ILINE=0                                                           PLO33020
   10 ILINE=ILINE+1                                                     PLO33030
      READ(ITAPE1,2000,END=70,ERR=10) IPROJX,IZAX,MSTA1X,MFX,MTX,       PLO33040
     1 MSTA2X,STATX,LABX,FIELDC,IM78X,REF1                              PLO33050
C                                                                       PLO33060
C     SKIP ALL POINTS WHICH CANNOT BE PLOTTED OR HAVE NOT BEEN REQUESTEDPLO33070
C                                                                       PLO33080
C***** OLD
C     IF(MYMODE.GE.2.AND.MFX.NE.3) GO TO 10                             PLO33090
C***** OLD
C***** TRKOV - ALLOW MF4/5/6 
      IF(MYMODE.GE.2.AND.(MFX.LT.3 .OR. MFX.GT. 6)) GO TO 10
C***** TRKOV
      MFSAVX=MFX                                                        PLO33100
      IF(MFX.EQ.203) MFSAVX=3                                           PLO33110
      IF(MFX.EQ.402) MFSAVX=3                                           PLO33120
      IF(MFX.EQ.154) MFSAVX=7                                           PLO33130
      IF(MFX.EQ.801) MFSAVX=8                                           PLO33140
      IF(MFSAVX.LT.3.OR.MFSAVX.GT.8) GO TO 10                           PLO33150
      CALL FLOAT9(FIELDC(1,1),FIELDI(1))                                PLO33160
      CALL RQEX(KGET,IZAX,MFX,MTX,FIELDI(1))                            PLO33170
      IF(KGET.LE.0) GO TO 10                                            PLO33180
C                                                                       PLO33190
C     IF ONLY PLOTTING EXFOR DATA, IF NECESSARY TRANSLATE ADDITIONAL    PLO33200
C     ZA AND MT (PRODUCTION OR RATIO).                                  PLO33210
C                                                                       PLO33220
      MTTRY=0                                                           PLO33230
      IZATRY=0                                                          PLO33240
C***** OLD
C     IF(MYMODE.GE.2) GO TO 30                                          PLO33250
C***** OLD
C***** TRKOV
      IF(MYMODE.GE.2) GO TO 20
C***** TRKOV
C-----FOR CROSS SECTION RATIOS (MF=203) DEFINE DENOMINATOR ZA AND MT.   PLO33260
      IF(MFX.NE.203) GO TO 20                                           PLO33270
      CALL FLOAT9(FIELDC(1,5),FIELDI(5))                                PLO33280
      CALL FLOAT9(FIELDC(1,6),FIELDI(6))                                PLO33290
      MTTRY=FIELDI(5)                                                   PLO33300
      IZATRY=FIELDI(6)                                                  PLO33310
      GO TO 30                                                          PLO33320
C-----FOR PARTICLE/ISOTOPE PRODUCTION (MT=9000-9999) CROSS SECTIONS OR  PLO33330
C-----ANGULAR DISTRIBUTIONS (MF=3 OR 4) DEFINE PRODUCT ZA.              PLO33340
   20 IF(MTX.LT.9000) GO TO 30                                          PLO33350
      CALL FLOAT9(FIELDC(1,6),FIELDI(6))                                PLO33360
      IZATRY=FIELDI(6)                                                  PLO33370
C***** TRKOV
c...  IF(MFX.NE.5) GO TO 30
c...  MFX=6
C***** TRKOV
C                                                                       PLO33380
C     ACCEPTABLE POINT. SAVE ALL ZA/MF/MT COMBINATIONS.                 PLO33390
C                                                                       PLO33400
   30 IF(NZATAB.LE.0) GO TO 50                                          PLO33410
      DO 40 I=1,NZATAB                                                  PLO33420
      IF(IPROJX.NE.MZATAB(1,I).OR.                                      PLO33430
     1 IZAX.NE.MZATAB(2,I).OR.                                          PLO33440
     2 MFX.NE.MZATAB(3,I).OR.                                           PLO33450
     3 MTX.NE.MZATAB(4,I).OR.                                           PLO33460
     3 MTTRY.NE.MZATAB(8,I).OR.                                         PLO33470
     3 IZATRY.NE.MZATAB(9,I)) GO TO 40                                  PLO33480
      IF(MFSAVX.EQ.3) GO TO 60                                          PLO33490
      IF(FIELDI(1).EQ.EXTRA(I)) GO TO 60                                PLO33500
   40 CONTINUE                                                          PLO33510
C-----NEW ZA/MF/MT. IF POSSIBLE SAVE.                                   PLO33520
C***** OLD
C     IF(NZATAB.GE.400) GO TO 10                                        PLO33530
C  50 NZATAB=NZATAB+1                                                   PLO33540
C***** OLD
C-----SUPPRESS PROCESSING OF ANGLE-DEPENDENT ELASTIC CROSS SECTION
C***** TRKOV
      IF(MFSAVX.NE.4) GO TO 42
      IF(ILINE-MZATAB(5,NZATAB).GT.1) GO TO 42
      GO TO 52
   42 IF(NZATAB.LT.800) GO TO 50
      WRITE(OUTP,6000)
      GO TO 70
   50 NZATAB=NZATAB+1
   52 CONTINUE
C***** TRKOV
      MZATAB(1,NZATAB)=IPROJX                                           PLO33550
      MZATAB(2,NZATAB)=IZAX                                             PLO33560
      MZATAB(3,NZATAB)=MFX                                              PLO33570
      MZATAB(4,NZATAB)=MTX                                              PLO33580
      MZATAB(5,NZATAB)=ILINE                                            PLO33590
      MZATAB(7,NZATAB)=0                                                PLO33600
      MZATAB(8,NZATAB)=MTTRY                                            PLO33610
      MZATAB(9,NZATAB)=IZATRY                                           PLO33620
      EXTRA(NZATAB)=FIELDI(1)                                           PLO33630
      I=NZATAB                                                          PLO33640
C-----OLD ZA/MF/MT. UPDATE LAST RECORD COUNT AND POINT COUNT.           PLO33650
   60 MZATAB(6,I)=ILINE                                                 PLO33660
      MZATAB(7,I)=MZATAB(7,I)+1                                         PLO33670
      GO TO 10                                                          PLO33680
C                                                                       PLO33690
C     ENTIRE EXFOR FILE READ AND INDEXED. ELIMINATE SETS WITH LESS THAN PLO33700
C     MINIMUM NUMBER OF POINTS. INDICATE END OF PLOTTING IF NO EXFOR TO PLO33710
C     PLOT.                                                             PLO33720
C                                                                       PLO33730
   70 IF(NZATAB.LE.0) GO TO 630                                         PLO33740
      KZATAB=0                                                          PLO33750
      DO 90 I=1,NZATAB                                                  PLO33760
      IF(MZATAB(7,I).LT.MINNIE) GO TO 90                                PLO33770
      KZATAB=KZATAB+1                                                   PLO33780
      DO 80 K=1,9                                                       PLO33790
   80 MZATAB(K,KZATAB)=MZATAB(K,I)                                      PLO33800
      EXTRA(KZATAB)=EXTRA(I)                                            PLO33810
   90 CONTINUE                                                          PLO33820
      NZATAB=KZATAB                                                     PLO33830
      IF(NZATAB.LE.0) GO TO 630                                         PLO33840
C                                                                       PLO33850
C     POSITION EXFOR FILE TO READ.                                      PLO33860
C                                                                       PLO33870
      KZATAB=0                                                          PLO33880
      IPASS=1                                                           PLO33890
      ILINE=0                                                           PLO33900
      REWIND ITAPE1                                                     PLO33910
C                                                                       PLO33920
C     IF NO MORE EXFOR DATA TO PLOT INDICATE END OF PLOTTING.           PLO33930
C     DEFINE NEXT ZA/MF/MT BASED ON ENDF/B (ALREADY DEFINED) OR NEXT    PLO33940
C     ZA/MF/MT IN EXFOR INDEX. DETERMINE INDEX TO FIRST EXFOR RECORD    PLO33950
C     TO READ.                                                          PLO33960
C                                                                       PLO33970
  100 IF(NZATAB.LE.0) GO TO 630                                         PLO33980
      IF(MYMODE.GE.2) GO TO 110                                         PLO33990
C                                                                       PLO34000
C     SELECT NEXT ZA/MF/MT FROM EXFOR INDEX. END IF NO MORE EXFOR DATA  PLO34010
C     TO PLOT.                                                          PLO34020
C                                                                       PLO34030
      KZATAB=KZATAB+1                                                   PLO34040
      IF(KZATAB.GT.NZATAB) GO TO 630                                    PLO34050
      IPROJT=MZATAB(1,KZATAB)                                           PLO34060
      IZA=MZATAB(2,KZATAB)                                              PLO34070
      MF=MZATAB(3,KZATAB)                                               PLO34080
      MT=MZATAB(4,KZATAB)                                               PLO34090
      MTRAT=MZATAB(8,KZATAB)                                            PLO34100
      IZARAT=MZATAB(9,KZATAB)                                           PLO34110
      ENEX=EXTRA(KZATAB)                                                PLO34120
      MFSAVE=MF                                                         PLO34130
      IF(MF.EQ.203) MFSAVE=3                                            PLO34140
      IF(MF.EQ.402) MFSAVE=3                                            PLO34150
      IF(MF.EQ.154) MFSAVE=7                                            PLO34160
      IF(MF.EQ.801) MFSAVE=8                                            PLO34170
      KGROUP=IGROUP                                                     PLO34180
      IF(MF.NE.3) KGROUP=0                                              PLO34190
      GO TO 130                                                         PLO34200
C                                                                       PLO34210
C     COMPARE ENDF/B ZA/MF/MT TO EXFOR INDEX.                           PLO34220
C                                                                       PLO34230
  110 IPROJT=1                                                          PLO34240
      MTRAT=0                                                           PLO34250
      IZARAT=0                                                          PLO34260
      ENEX=0.0                                                          PLO34270
C***** OLD
C     MFSAVE=3                                                          PLO34280
C***** OLD
C***** TRKOV
      MFSAVE=MF
C***** TRKOV
      KGROUP=IGROUP                                                     PLO34290
C***** TRKOV
      IF(MF.LT.5 .OR. MF.GT.6) GO TO 112
      KGROUP=0
      IZARAT=1
  112 CONTINUE
C***** TRKOV
      IMNORM=0                                                          PLO34300
      DO 120 KZATAB=1,NZATAB                                            PLO34310
C***** OLD
C     IF(IPROJT.EQ.MZATAB(1,KZATAB).AND.                                PLO34320
C    1 IZA.EQ.MZATAB(2,KZATAB).AND.                                     PLO34330
C    2 MF.EQ.MZATAB(3,KZATAB).AND.                                      PLO34340
C    3 MT.EQ.MZATAB(4,KZATAB)) GO TO 130                                PLO34350
C***** OLD
C***** TRKOV
       MFJ=MF
       IF(MFJ.EQ.6 .AND. MZATAB(3,KZATAB).EQ.5) MFJ=5
      IF(IPROJT.EQ.MZATAB(1,KZATAB).AND.
     1 IZA.EQ.MZATAB(2,KZATAB).AND.
     2 MFJ.EQ.MZATAB(3,KZATAB).AND.
     3 MT.EQ.MZATAB(4,KZATAB)) GO TO 130
C***** TRKOV
  120 CONTINUE                                                          PLO34360
C-----NO COMPARABLE EXFOR DATA.                                         PLO34370
      IEX=0                                                             PLO34380
      IEND=1                                                            PLO34390
      RETURN                                                            PLO34400
C                                                                       PLO34410
C     REQUIRED DATA IS PRESENT. INITIALIZE PARAMETERS.                  PLO34420
C                                                                       PLO34430
C-----INITIALIZE DATA POINT, REFERENCE COUNT, GROUP BY E2 FLAG, FIELDS  PLO34440
C-----7 - 8 BLANK AND INTERNAL MF NUMBER.                               PLO34450
  130 IEX=0                                                             PLO34460
C***** TRKOV
      IF(MF.GE.4 .AND. MF.LE.6) ENEX=EXTRA(KZATAB)
C***** TRKOV
      IEXPAS=0                                                          PLO34470
      IREF=0                                                            PLO34480
C***** OLD
C     IMAM78=0                                                          PLO34490
C     IM78=BLANK                                                        PLO34500
C***** OLD
C-----INITIALIZE ALL METASTABLE STATE FLAGS.                            PLO34510
      MSTAT1=BLANK                                                      PLO34520
      MSTAT2=BLANK                                                      PLO34530
      MSTAR1=BLANK1                                                     PLO34540
      MSTAR2=BLANK1                                                     PLO34550
      MSTA1X=BLANK                                                      PLO34560
      MSTA2X=BLANK                                                      PLO34570
      MSTA1R=BLANK1                                                     PLO34580
      MSTA2R=BLANK1                                                     PLO34590
C-----INITIALIZE STATUS AND CENTER-OF-MASS FLAG.                        PLO34600
      STATUS=BLANK1                                                     PLO34610
      IMNORM=0                                                          PLO34620
      LABCM=BLANK1                                                      PLO34630
C                                                                       PLO34640
C     DECIDE WHETHER OR NOT TO REWIND COMPUTATION FORMAT FILE BASED     PLO34650
C     ON CURRENT POSITION OF EXFOR DATA FILE VS. INDEX TO FIRST DATA    PLO34660
C     POINT OF REQUIRED ZA/MF/MT.                                       PLO34670
C                                                                       PLO34680
      IF(ILINE-MZATAB(5,KZATAB)) 150,170,140                            PLO34690
C-----RE-INITIALIZE LINE COUNT AND REWIND.                              PLO34700
  140 ILINE=0                                                           PLO34710
      REWIND ITAPE1                                                     PLO34720
C-----SKIP FORWARD TO REQUESTED DATA.                                   PLO34730
  150 II=MZATAB(5,KZATAB)-1                                             PLO34740
      IF(II.EQ.ILINE) GO TO 180                                         PLO34750
      ILINE=ILINE+1                                                     PLO34760
      DO 160 I=ILINE,II                                                 PLO34770
      READ(ITAPE1,2010,ERR=160) DUMMY                                   PLO34780
  160 CONTINUE                                                          PLO34790
      ILINE=II                                                          PLO34800
      GO TO 180                                                         PLO34810
C                                                                       PLO34820
C     SET UP LOOP TO READ ALL COMPARABLE DATA POINTS.                   PLO34830
C                                                                       PLO34840
C-----SET FLAG TO SHOW THAT NEXT POINT IS IN CORE.                      PLO34850
  170 IEXPAS=1                                                          PLO34860
C***** OLD
C 180 DO 510 IEX=1,MAXIE                                                PLO34870
C***** OLD
C***** TRKOV
  180 DO 510 JEX=1,MAXIE
      IEX=JEX
C***** TRKOV
C-----SKIP READ IF NEXT POINT IS ALREADY IN CORE.                       PLO34880
      IF(IEXPAS.EQ.1) GO TO 200                                         PLO34890
C-----INCREMENT LINE COUNT AND READ NEXT POINT.                         PLO34900
  190 ILINE=ILINE+1                                                     PLO34910
      READ(ITAPE1,2000,END=520,ERR=190) IPROJX,IZAX,MSTA1X,MFX,MTX,     PLO34920
     1 MSTA2X,STATX,LABX,FIELDC,IM78X,REF1                              PLO34930
C                                                                       PLO34940
C     USE INDEX TO LAST POINT OF ZA/MF/MT TO DETERMINE WHEN TO STOP     PLO34950
C     READING.                                                          PLO34960
C                                                                       PLO34970
  200 IEXPAS=0                                                          PLO34980
      IF(ILINE.GT.MZATAB(6,KZATAB)) GO TO 520                           PLO34990
C                                                                       PLO35000
C     IMMEDIATELY SKIP DATA WHICH CANNOT BE PLOTTED.                    PLO35010
C                                                                       PLO35020
      MFSAVX=MFX                                                        PLO35040
      IF(MFX.EQ.203) MFSAVX=3                                           PLO35050
      IF(MFX.EQ.402) MFSAVX=3                                           PLO35060
      IF(MFX.EQ.154) MFSAVX=7                                           PLO35070
      IF(MFX.EQ.801) MFSAVX=8                                           PLO35080
      IF(MFSAVX.LT.1.OR.MFSAVX.GT.8) GO TO 190                          PLO35090
C                                                                       PLO35100
C     TRANSLATE 8 FIELDS FROM CHARACTER TO FLOATING POINT.              PLO35110
C                                                                       PLO35120
      DO 210 I=1,8                                                      PLO35130
  210 CALL FLOAT9(FIELDC(1,I),FIELDI(I))                                PLO35140
C                                                                       PLO35150
C     SKIP POINT IF IT IS NOT REQUESTED (EVEN THOUGH INDEX TO EXFOR HAS PLO35160
C     ALREADY BEEN MADE THIS TEST IS NECESSARY TO COLLECT TOGETHER      PLO35170
C     REQUESTED POINTS WHICH ARE MIXED IN WITH POINTS WHICH HAVE NOT    PLO35180
C     BEEN REQUESTED, E.G. MULTI-DIMENSIONAL TABLE WHERE SUCCESSIVE     PLO35190
C     POINTS CAN BE FOR DIFFERENT REACTIONS).                           PLO35200
C                                                                       PLO35210
      CALL RQEX(KGET,IZAX,MFX,MTX,FIELDI(1))                            PLO35220
      IF(KGET.LE.0) GO TO 190                                           PLO35230
C                                                                       PLO35240
C     IF ONLY PLOTTING EXFOR DATA, IF NECESSARY TRANSLATE ADDITIONAL    PLO35250
C     ZA AND MT (PRODUCTION OR RATIO).                                  PLO35260
C                                                                       PLO35270
      MTTRY=0                                                           PLO35280
      IZATRY=0                                                          PLO35290
C***** OLD
C     IF(MYMODE.GE.2) GO TO 230                                         PLO35300
C***** OLD
C-----FOR CROSS SECTION RATIOS (MF=203) DEFINE DENOMINATOR ZA AND MT.   PLO35310
      IF(MFX.NE.203) GO TO 220                                          PLO35320
      MTTRY=FIELDI(5)                                                   PLO35330
      IZATRY=FIELDI(6)                                                  PLO35340
C-----DEFINE DEMONINATOR METASTABLE STATE FLAG (CONVERT 9-TH CHARACTER  PLO35350
C-----OF 6-TH FIELD FROM INTEGER TO METASTABLE FLAG).                   PLO35360
      CALL META10(FIELDC(3,6),MSTA1R)                                   PLO35370
C-----DEFINE DEMONINATOR PRODUCT METASTABLE STATE FLAG (CONVERT 9-TH    PLO35380
C-----CHARACTER OF 5-TH FIELD FROM INTEGER TO METASTABLE FLAG).         PLO35390
      CALL META10(FIELDC(3,5),MSTA2R)                                   PLO35400
C-----SET DATA FIELDS TO ZERO TO AVOID THEIR BEING INTERPRETED AS DATA. PLO35410
      FIELDI(5)=0.0                                                     PLO35420
      FIELDI(6)=0.0                                                     PLO35430
      GO TO 230                                                         PLO35440
C-----FOR PARTICLE/ISOTOPE PRODUCTION (MT=9000-9999) CROSS SECTIONS OR  PLO35450
C-----ANGULAR DISTRIBUTIONS (MF=3 OR 4) DEFINE PRODUCT ZA.              PLO35460
  220 IF(MTX.LT.9000) GO TO 230                                         PLO35470
      IZATRY=FIELDI(6)                                                  PLO35480
C-----SET DATA FIELD TO ZERO TO AVOID ITS BEING INTERPRETED AS DATA.    PLO35490
      FIELDI(6)=0.0                                                     PLO35500
C                                                                       PLO35510
C     COMPARE,                                                          PLO35520
C     (1) CURRENT ZA/MF/MT TO REQUIRED ZA/MF/MT.                        PLO35530
C     (2) IF NOT CROSS SECTIONS INCIDENT ENERGY                         PLO35540
C                                                                       PLO35550
C***** OLD
C 230 IF(IPROJT.NE.IPROJX.OR.                                           PLO35560
C    1 IZA.NE.IZAX.OR.                                                  PLO35570
C    2 MF.NE.MFX.OR.                                                    PLO35580
C    3 MT.NE.MTX.OR.                                                    PLO35590
C    4 MTRAT.NE.MTTRY.OR.                                               PLO35600
C    5 IZARAT.NE.IZATRY) GO TO 240                                      PLO35610
C***** OLD
C***** TRKOV
  230 MFIN=MF
      IF(MFIN.EQ.6 .AND. MFX.EQ.5) MFIN=MFX
      IF(IPROJT.NE.IPROJX.OR.
     1 IZA.NE.IZAX.OR.
     2 MFIN.NE.MFX.OR.
     3 MT.NE.MTX.OR.
     4 MTRAT.NE.MTTRY.OR.
     5 IZARAT.NE.IZATRY) GO TO 240
C***** TRKOV
C-----SAME ZA/MF/MT. IF NOT MF=3 CHECK INCIDENT ENERGY.                 PLO35620
      IF(MFSAVE.EQ.3) GO TO 250                                         PLO35630
      IF(FIELDI(1).EQ.ENEX) GO TO 250                                   PLO35640
C                                                                       PLO35650
C     CURRENT POINT CANNOT BE INCLUDED ON CURRENT PLOT (E.G., NEW ZA/MF/PLO35660
C     MT OR OTHER PARAMETER CHANGED).                                   PLO35670
C                                                                       PLO35680
C-----IF PLOTTING ALL REFERENCES TOGETHER CONTINUE READING              PLO35690
  240 IF(MYMODE.EQ.1.OR.MYMODE.EQ.3) GO TO 190                          PLO35700
C-----IF NO POINTS FOUND YET CONTINUE READING.                          PLO35710
      IF(IEX.EQ.1) GO TO 190                                            PLO35720
C-----RETURN CURRENT POINTS READ.                                       PLO35730
      GO TO 530                                                         PLO35740
C                                                                       PLO35750
C     IF ONLY PLOTTING EXFOR DATA SAVE PARAMETERS WHEN FIRST POINT IS   PLO35760
C     READ.                                                             PLO35770
C                                                                       PLO35780
  250 IF(MYMODE.GE.2.OR.IEX.GT.1) GO TO 270                             PLO35790
      ILIB=16                                                           PLO35800
      LIBNAM(3)=REF1(8)                                                 PLO35810
      LIBNAM(4)=REF1(9)                                                 PLO35820
      STATUS=STATX                                                      PLO35830
      IMNORM=0                                                          PLO35840
      IF(STATUS.NE.USTAT) GO TO 260                                     PLO35850
      STATUS=BLANK1                                                     PLO35860
      IMNORM=1                                                          PLO35870
  260 LABCM=LABX                                                        PLO35880
      MSTAT1=MSTA1X                                                     PLO35890
      MSTAT2=MSTA2X                                                     PLO35900
      MSTAR1=MSTA1R                                                     PLO35910
      MSTAR2=MSTA2R                                                     PLO35920
      IM78=IM78X                                                        PLO35930
C                                                                       PLO35940
C     TEST FOR NON-BLANK E2 FIELD. DEFINE E2.                           PLO35950
C                                                                       PLO35960
C***** OLD
C 270 DO 280 I=1,3                                                      PLO35970
C     IF(FIELDC(I,7).NE.BLANK) IMAM78=1                                 PLO35980
C***** OLD
C***** TRKOV
  270 IMAM78=0
      DO 280 I=1,3
      IF(FIELDC(I,7).NE.BLANK) IMAM78=1
C***** TRKOV
  280 CONTINUE                                                          PLO35990
      E2(IEX)=FIELDI(7)                                                 PLO36000
C-----CONVERT ELASTIC CM DISTRIBUTION TO LAB
C***** TRKOV
      IF(MYMODE.LT.2 .OR.(MFX.NE.4 .AND.MTX.NE.2) ) GO TO 288
      IF(LABCM.EQ.BLANK1) GO TO 288
        EIN=FIELDI(1)
        AIN=FIELDI(5)
        QQ =0
        GAM=SQRT( EIN/ (EIN*AWR*AWR+QQ*AWR*(AWR+1) ) )
        DMC=2*GAM*AIN + SQRT(1-GAM*GAM*(1-AIN*AIN))
     1     +(GAM*AIN)**2 / SQRT(1-GAM*GAM*(1-AIN*AIN))
        FIELDI(3)=FIELDI(3)*DMC
        FIELDI(5)=(AIN+GAM)/SQRT(1+GAM*(GAM+2*AIN))
        LABCM=BLANK1
  288 CONTINUE
C***** TRKOV
C                                                                       PLO36010
C     SAVE ALL REFS AND DEFINE POINT INDEX TO REF.                      PLO36020
C                                                                       PLO36030
      IF(IREF.LE.0) GO TO 320                                           PLO36040
C***** OLD
C     DO 300 KREF=1,IREF                                                PLO36050
C***** OLD
C***** TRKOV
      DO 300 I=1,IREF
      KREF=I
C***** TRKOV
      IMSAME=0                                                          PLO36060
      DO 290 J=1,9                                                      PLO36070
      IF(REFS(J,KREF).NE.REF1(J)) GO TO 300                             PLO36080
  290 CONTINUE                                                          PLO36090
      IMSAME=1                                                          PLO36100
C-----OLD REFERENCE. FOR CROSS SECTIONS AND RATIO TEST FOR CHANGE IN E2.PLO36110
      IF(MFSAVE.NE.3) GO TO 340                                         PLO36120
      IF(E2T(KREF)-E2(IEX)) 300,340,300                                 PLO36130
  300 CONTINUE                                                          PLO36140
C-----NEW REFERENCE AND/OR E2. IF IDENTIFYING E2 CONTINUE TO READ AS    PLO36150
C-----LONG AS REFERENCE HAS NOT CHANGED. OTHERWISE RETURN.              PLO36160
      IF(KGROUP.EQ.0) GO TO 310                                         PLO36170
      IF(IMSAME) 530,530,320                                            PLO36180
C-----NOT IDENTIFYING E2. RETURN IF PLOTTING EACH REFERENCE SEPERATELY. PLO36190
  310 IF(MYMODE.EQ.0.OR.MYMODE.EQ.2) GO TO 530                          PLO36200
C-----COLLECTING REFERENCES. ONLY ALLOW ONE E2 VALUE.                   PLO36210
      IF(E2T(1).NE.E2(IEX)) GO TO 530                                   PLO36220
C***** OLD
C 320 IF(IREF.LE.MAXREF) IREF=IREF+1                                    PLO36230
C***** OLD
C***** TRKOV
  320 IF(IREF.GE.MAXREF) GO TO 540
      IREF=IREF+1
C***** TRKOV
C-----IF MORE THAN MAXIMUM NUMBER OF REFERENCES IDENTIFY ALL OTHER      PLO36240
C-----REFERENCES ONLY AS OTHERS.                                        PLO36250
C***** OLD
C     IF(IREF.GT.MAXREF) GO TO 340                                      PLO36260
C***** OLD
C-----SAVE E2 AND REFERENCE.                                            PLO36270
      E2T(IREF)=E2(IEX)                                                 PLO36280
      DO 330 J=1,9                                                      PLO36290
  330 REFS(J,IREF)=REF1(J)                                              PLO36300
      KREF=IREF                                                         PLO36310
C-----DEFINE DATA POINT INDEX TO REFERENCE.                             PLO36320
  340 NREF(IEX)=KREF                                                    PLO36330
C                                                                       PLO36340
C     SAVE DATA POINT ACCORDING TO TYPE OF DATA.                        PLO36350
C                                                                       PLO36360
      MFM2=MFSAVE-2                                                     PLO36370
      YEX(IEX)=FIELDI(3)                                                PLO36380
      DYEX(IEX)=ABS(FIELDI(4))                                          PLO36390
      GO TO (350,410,420,430,470,480),MFM2                              PLO36400
C-----BASED ON FIRST TWO POINT SELECT E VS. CROSS SECTION (CONSTANT E2) PLO36410
C-----OR E2 VS. CROSS SECTION (CONSTANT E).                             PLO36420
  350 IF(IEX.GT.1) GO TO 360                                            PLO36430
C-----INITIALIZE ASSUMING E VS. CROSS SECTION.                          PLO36440
      IDOUB=1                                                           PLO36450
      FIELD4(1)=FIELDI(7)                                               PLO36460
      FIELD4(2)=FIELDI(8)                                               PLO36470
      XEX(IEX)=FIELDI(1)                                                PLO36480
      DXEX(IEX)=FIELDI(2)                                               PLO36490
      GO TO 510                                                         PLO36500
  360 IF(IEX.GT.2) GO TO 390                                            PLO36510
C-----CANNOT USE E2 IF COMPARING TO ENDF/B DATA.                        PLO36520
C***** OLD - OVERRIDE TEST TO ENABLE MF4/5/6 COMPARISON WITH ENDF
C     IF(MYMODE.GE.2) GO TO 390                                         PLO36530
C***** OLD
C-----CANNOT USE E2 IF FIELD IS BLANK.                                  PLO36540
      IF(IMAM78.LE.0) GO TO 390                                         PLO36550
C-----CANNOT USE E2 IF FIELD IS HALF-LIFE                               PLO36560
      IF(IM78.EQ.HL) GO TO 390                                          PLO36570
C-----TEST FOR CONSTANT E FIELD.                                        PLO36580
      IF(XEX(1)-FIELDI(1)) 390,370,390                                  PLO36590
C-----CONSTANT E. TEST FOR CONSTANT E2 FIELD.                           PLO36600
  370 IF(FIELD4(1)-FIELDI(7)) 380,390,380                               PLO36610
C-----CONSTANT E. CHANGE TO E2 VS. CROSS SECTION.                       PLO36620
  380 IDOUB=2                                                           PLO36630
      E2(1)=XEX(1)                                                      PLO36640
      E2T(1)=XEX(1)                                                     PLO36650
      XEX(1)=FIELD4(1)                                                  PLO36660
      DXEX(1)=FIELD4(2)                                                 PLO36670
  390 IF(IDOUB.EQ.2) GO TO 400                                          PLO36680
C-----CROSS SECTION. E VS. CROSS SECTION.                               PLO36690
      XEX(IEX)=FIELDI(1)                                                PLO36700
      DXEX(IEX)=FIELDI(2)                                               PLO36710
      GO TO 510                                                         PLO36720
C-----CROSS SECTION...E2 VS. CROSS SECTION.                             PLO36730
  400 XEX(IEX)=FIELDI(7)                                                PLO36740
      DXEX(IEX)=FIELDI(8)                                               PLO36750
      E2(IEX)=FIELDI(1)                                                 PLO36760
      GO TO 510                                                         PLO36770
C-----ANGULAR DISTRIBUTION                                              PLO36780
  410 XEX(IEX)=FIELDI(5)                                                PLO36790
      DXEX(IEX)=FIELDI(6)                                               PLO36800
      GO TO 510                                                         PLO36810
C-----ENERGY DISTRIBUTION                                               PLO36820
  420 XEX(IEX)=FIELDI(7)                                                PLO36830
      DXEX(IEX)=FIELDI(8)                                               PLO36840
      GO TO 510                                                         PLO36850
C-----DOUBLE DIFFERENTIAL. ALLOW FOR EITHER CONSTANT COSINE OR E.       PLO36860
  430 IF(IEX.GT.1) GO TO 440                                            PLO36870
      IDOUB=1                                                           PLO36880
      FIELD4(1)=FIELDI(5)                                               PLO36890
      FIELD4(2)=FIELDI(6)                                               PLO36900
      FIELD4(3)=FIELDI(7)                                               PLO36910
      FIELD4(4)=FIELDI(8)                                               PLO36920
      XEX(IEX)=FIELD4(1)                                                PLO36930
      DXEX(IEX)=FIELD4(2)                                               PLO36940
      GO TO 510                                                         PLO36950
C-----FOR DOUBLE DIFFERENTAL DATA COLLECT EITHER CROSS SECTION VS.      PLO36960
C-----ANGLE (CONSTANT SECONDARY ENERGY) OR CROSS SECTION VS. SECONDARY  PLO36970
C-----ENERGY (COSTANT ANGLE) BASED ON WHICHEVER CHANGES BEWTEEEN FIRST  PLO36980
C-----TWO POINTS READ.                                                  PLO36990
  440 IF(IEX.GT.2) GO TO 450                                            PLO37000
      IF(FIELD4(1).NE.FIELDI(5).AND.FIELD4(3).NE.FIELDI(7)) GO TO 530   PLO37010
      IF(FIELD4(1).EQ.FIELDI(5)) IDOUB=2                                PLO37020
      IF(IDOUB.EQ.1) GO TO 450                                          PLO37030
      XEX(1)=FIELD4(3)                                                  PLO37040
      DXEX(1)=FIELD4(4)                                                 PLO37050
  450 IF(IDOUB.EQ.2) GO TO 460                                          PLO37060
      IF(FIELD4(3).NE.FIELDI(7)) GO TO 530                              PLO37070
      XEX(IEX)=FIELDI(5)                                                PLO37080
      DXEX(IEX)=FIELDI(6)                                               PLO37090
      GO TO 510                                                         PLO37100
  460 IF(FIELD4(1).NE.FIELDI(5)) GO TO 530                              PLO37110
      XEX(IEX)=FIELDI(7)                                                PLO37120
      DXEX(IEX)=FIELDI(8)                                               PLO37130
      GO TO 510                                                         PLO37140
C-----LEGENDRE COEFFICIENTS.                                            PLO37150
  470 XEX(IEX)=FIELDI(5)                                                PLO37160
      DXEX(IEX)=0.0                                                     PLO37170
      GO TO 510                                                         PLO37180
C-----FISSION YIELD. WHEN FIRST DATA POINT IS READ INITIALIZE           PLO37190
C-----CUMULATIVE YIELD TABLE AND STORE FIRST VALUE.                     PLO37200
  480 IF(IEX.GT.1) GO TO 500                                            PLO37210
      DO 490 M=1,400                                                    PLO37220
      YEX(M)=0.0                                                        PLO37230
  490 DYEX(M)=0.0                                                       PLO37240
      IATWT=FIELDI(6)                                                   PLO37250
      YEX(IATWT)=FIELDI(3)                                              PLO37260
      DYEX(IATWT)=FIELDI(4)**2                                          PLO37270
      MEX1=IATWT                                                        PLO37280
      MEX2=IATWT                                                        PLO37290
      GO TO 510                                                         PLO37300
C-----FISSION YIELD. ADD ATOMIC WEIGHT YIELD TO TABLE.                  PLO37310
  500 IATWT=FIELDI(6)                                                   PLO37320
      YEX(IATWT)=YEX(IATWT)+FIELDI(3)                                   PLO37330
      DYEX(IATWT)=DYEX(IATWT)+FIELDI(4)**2                              PLO37340
      IF(IATWT.LT.MEX1) MEX1=IATWT                                      PLO37350
      IF(IATWT.GT.MEX2) MEX2=IATWT                                      PLO37360
C                                                                       PLO37370
C     READ UP TO MAXIMUM POINTS.                                        PLO37380
C                                                                       PLO37390
  510 CONTINUE                                                          PLO37400
      GO TO 540                                                         PLO37410
C***** OLD
C 520 IF(MYMODE.NE.0) IEND=1                                            PLO37420
C***** OLD
C***** TRKOV
  520 CONTINUE
C***** TRKOV
      IPASS=IPASS+1                                                     PLO37430
C                                                                       PLO37440
C     ALL EXFOR DATA READ.                                              PLO37450
C                                                                       PLO37460
  530 IEX=IEX-1                                                         PLO37470
C-----SAVE COUNT OF ACTUAL POINTS READ (TO DECREMENT INDEX).            PLO37480
  540 KEX=IEX                                                           PLO37490
      IF(IEX.LE.0) GO TO 580                                            PLO37500
      IF(MF.NE.801) GO TO 560                                           PLO37510
C-----DEFINE ATOMIC WEIGHT RANGE FOR FISSION YIELD.                     PLO37520
      IEX=0                                                             PLO37530
      DO 550 M=MEX1,MEX2                                                PLO37540
      IF(YEX(M).LE.0.0) GO TO 550                                       PLO37550
      IEX=IEX+1                                                         PLO37560
      XEX(IEX)=M                                                        PLO37570
      DXEX(IEX)=0.0                                                     PLO37580
      YEX(IEX)=YEX(M)                                                   PLO37590
      DYEX(IEX)=SQRT(DYEX(M))                                           PLO37600
  550 CONTINUE                                                          PLO37610
      GO TO 580                                                         PLO37620
C                                                                       PLO37630
C     INSURE ERRORS ARE NON-NEGATIVE. SET TO ZERO IF ERROR WILL NOT BE  PLO37640
C     PLOTTED.                                                          PLO37650
C                                                                       PLO37660
  560 DO 570 I=1,IEX                                                    PLO37670
      DXEX(I)=ABS(DXEX(I))                                              PLO37680
      IF(IXERR.LE.0) DXEX(I)=0.0                                        PLO37690
      DYEX(I)=ABS(DYEX(I))                                              PLO37700
      IF(IYERR.LE.0) DYEX(I)=0.0                                        PLO37710
  570 CONTINUE                                                          PLO37720
C-----IF LESS THAN TWO REFERENCES TURN OFF IDENTIFY E2 FLAG.            PLO37730
  580 IF(IREF.LT.2) KGROUP=0                                            PLO37740
C                                                                       PLO37750
C     UPDATE EXFOR INDEX TABLE TO PREVENT POINTS BEING PLOTTED TWICE    PLO37760
C                                                                       PLO37770
C-----RESET LOWER LINE INDEX AND REMAINING POINT COUNT.                 PLO37780
      MZATAB(5,KZATAB)=ILINE                                            PLO37790
      MZATAB(7,KZATAB)=MZATAB(7,KZATAB)-KEX                             PLO37800
C-----IF MORE POINTS WITH SAME ZA/MF/MT TO PLOT RESET INDEX TO TABLE.   PLO37810
      IF(MZATAB(7,KZATAB).LT.MINNIE.OR.                                 PLO37820
     1 MZATAB(5,KZATAB).GT.MZATAB(6,KZATAB)) GO TO 590                  PLO37830
      KZATAB=KZATAB-1                                                   PLO37840
      RETURN                                                            PLO37850
C-----ELIMINATE INDEX FROM TABLE BY SHIFTING ALL FOLLOWING ENTRIES ONE  PLO37860
C-----LOCATION FORWARD IN TABLE.                                        PLO37870
  590 IF(NZATAB.LE.1.OR.KZATAB.EQ.NZATAB) GO TO 620                     PLO37880
      II=KZATAB+1                                                       PLO37890
      DO 610 I=II,NZATAB                                                PLO37900
      DO 600 K=1,9                                                      PLO37910
  600 MZATAB(K,I-1)=MZATAB(K,I)                                         PLO37920
  610 EXTRA(I-1)=EXTRA(I)                                               PLO37930
      KZATAB=KZATAB-1                                                   PLO37940
  620 NZATAB=NZATAB-1                                                   PLO37950
C-----SET END OF FILE FLAG IF NO EXFOR DATA LEFT TO PLOT.               PLO37960
      IF(NZATAB.LE.0) IEND=2                                            PLO37970
      RETURN                                                            PLO37980
C                                                                       PLO37990
C     END OF EXFOR PLOTTING.                                            PLO38000
C                                                                       PLO38010
  630 IEND=2                                                            PLO38020
      RETURN                                                            PLO38030
 2000 FORMAT(I5,I6,A1,I3,I4,3A1,8(2A4,A1),A3,6A4,A1,2A4)                PLO38040
 2010 FORMAT(A1)                                                        PLO38050
C***** TRKOV
 6000 FORMAT(' MAX.NO. OF REACTIONS REACHED...REMAINING SKIPPED')
C***** TRKOV
      END                                                               PLO38060
      SUBROUTINE FLOAT9(FIELD,X)                                        PLO38070
C                                                                       PLO38080
C     CONVERT FROM 9 HOLLERITH CHARACTERS TO FLOATING POINT.            PLO38090
C     MUST BE BETWEEN 1.0E-40 AND 1.0E+40.                              PLO38100
C                                                                       PLO38110
      INTEGER OUTP,OTAPE                                                PLO38120
      CHARACTER*1 BLANK,DOT,EXPD,EXPE,PLUS,MINUS,STAR,MESS,DIGIT,FIELD, PLO38130
     1 IFIELD                                                           PLO38140
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO38150
C     DIMENSION FIELD(9),TEN(40),DIGIT(10),MESS(9)                      PLO38160
C***** OLD
C***** TRKOV - FIX EXCESSIVE EXPONENTS ON SHORT-WORD MACHINES
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      DIMENSION FIELD(9),TEN(35),DIGIT(10),MESS(9)
C***** TRKOV END
      DATA BLANK/' '/                                                   PLO38170
      DATA DOT/'.'/                                                     PLO38180
      DATA EXPD/'D'/                                                    PLO38190
      DATA EXPE/'E'/                                                    PLO38200
      DATA PLUS/'+'/                                                    PLO38210
      DATA MINUS/'-'/                                                   PLO38220
      DATA STAR/'*'/                                                    PLO38230
      DATA MESS/' ',' ',' ',' ',' ',' ',' ',' ',' '/                    PLO38240
      DATA DIGIT/'0','1','2','3','4','5','6','7','8','9'/               PLO38250
      DATA ZERO/0.0E+00/                                                PLO38260
C***** TRKOV - FIX EXCESSIVE EXPONENTS ON SHORT-WORD MACHINES
      DATA TEN/                                                         PLO38270
     1 1.0E+01,1.0E+02,1.0E+03,1.0E+04,1.0E+05,                         PLO38280
     2 1.0E+06,1.0E+07,1.0E+08,1.0E+09,1.0E+10,                         PLO38290
     3 1.0E+11,1.0E+12,1.0E+13,1.0E+14,1.0E+15,                         PLO38300
     4 1.0E+16,1.0E+17,1.0E+18,1.0E+19,1.0E+20,                         PLO38310
     5 1.0E+21,1.0E+22,1.0E+23,1.0E+24,1.0E+25,                         PLO38320
     6 1.0E+26,1.0E+27,1.0E+28,1.0E+29,1.0E+30,                         PLO38330
     7 1.0E+31,1.0E+32,1.0E+33,1.0E+34,1.0E+35/
C***** TRKOV
C***** OLD
C     DATA TEN/                                                         PLO38270
C    1 1.0E+01,1.0E+02,1.0E+03,1.0E+04,1.0E+05,                         PLO38280
C    2 1.0E+06,1.0E+07,1.0E+08,1.0E+09,1.0E+10,                         PLO38290
C    3 1.0E+11,1.0E+12,1.0E+13,1.0E+14,1.0E+15,                         PLO38300
C    4 1.0E+16,1.0E+17,1.0E+18,1.0E+19,1.0E+20,                         PLO38310
C    5 1.0E+21,1.0E+22,1.0E+23,1.0E+24,1.0E+25,                         PLO38320
C    6 1.0E+26,1.0E+27,1.0E+28,1.0E+29,1.0E+30,                         PLO38330
C    7 1.0E+31,1.0E+32,1.0E+33,1.0E+34,1.0E+35,                         PLO38340
C    8 1.0E+36,1.0E+37,1.0E+38,1.0E+39,1.0E+40/                         PLO38350
C***** OLD
C                                                                       PLO38360
C     TRANSLATE MANTISSA.                                               PLO38370
C                                                                       PLO38380
C-----SKIP LEADING BLANK CHARACTERS.                                    PLO38390
      DO 10 I=1,9                                                       PLO38400
      IF(FIELD(I).NE.BLANK) GO TO 20                                    PLO38410
   10 CONTINUE                                                          PLO38420
C-----FIELD IS COMPLETELY BLANK. RETURN ZERO.                           PLO38430
      X=ZERO                                                            PLO38440
      GO TO 240                                                         PLO38450
C-----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT. PLO38460
   20 IN=0                                                              PLO38470
      IPT=-20                                                           PLO38480
C-----ALLOW LEADING SIGN.                                               PLO38490
      IF(FIELD(I).EQ.MINUS) GO TO 40                                    PLO38500
      IF(FIELD(I).NE.PLUS) GO TO 30                                     PLO38510
      I=I+1                                                             PLO38520
   30 XSIGN=1.0                                                         PLO38530
      GO TO 50                                                          PLO38540
   40 I=I+1                                                             PLO38550
      XSIGN=-1.0                                                        PLO38560
C-----SCAN REMAINDER OF MANTISSA.                                       PLO38570
   50 DO 90 J=I,9                                                       PLO38580
C***** TRKOV
      JC=J
C***** TRKOV
      IFIELD=FIELD(J)                                                   PLO38590
C-----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).     PLO38600
      DO 60 K=1,10                                                      PLO38610
      IF(IFIELD.EQ.DIGIT(K)) GO TO 80                                   PLO38620
   60 CONTINUE                                                          PLO38630
      IF(IFIELD.NE.DOT) GO TO 70                                        PLO38640
      IPT=0                                                             PLO38650
      GO TO 90                                                          PLO38660
C-----SCAN FOR BLANK (WHICH ENDS MANTISSA).                             PLO38670
   70 IF(IFIELD.EQ.BLANK) GO TO 100                                     PLO38680
C-----SCAN FOR E,D,- OR + (WHICH BEGINS EXPONENT).                      PLO38690
      IF(IFIELD.EQ.EXPE.OR.IFIELD.EQ.EXPD) GO TO 130                    PLO38700
      IF(IFIELD.EQ.MINUS) GO TO 160                                     PLO38710
      IF(IFIELD.EQ.PLUS) GO TO 140                                      PLO38720
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 PLO38730
      GO TO 250                                                         PLO38740
C-----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT   PLO38750
C-----OFFSET.                                                           PLO38760
   80 IN=10*IN+(K-1)                                                    PLO38770
      IPT=IPT+1                                                         PLO38780
   90 CONTINUE                                                          PLO38790
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT. PLO38800
      GO TO 120                                                         PLO38810
C-----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR        PLO38820
C-----EXPONENT.                                                         PLO38830
C***** OLD
C 100 I=J+1                                                             PLO38840
C***** OLD
C***** TRKOV
  100 I=JC+1
C***** TRKOV
      IF(I.GT.9) GO TO 120                                              PLO38850
      DO 110 J=I,9                                                      PLO38860
C***** TRKOV
      JC=J
C***** TRKOV
      IFIELD=FIELD(J)                                                   PLO38870
      IF(IFIELD.EQ.BLANK) GO TO 110                                     PLO38880
      IF(IFIELD.EQ.EXPE.OR.IFIELD.EQ.EXPD) GO TO 130                    PLO38890
      IF(IFIELD.EQ.MINUS) GO TO 160                                     PLO38900
      IF(IFIELD.EQ.PLUS) GO TO 140                                      PLO38910
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 PLO38920
      GO TO 250                                                         PLO38930
  110 CONTINUE                                                          PLO38940
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT. PLO38950
  120 X=IN                                                              PLO38960
      IF(IPT.GT.0) X=X/TEN(IPT)                                         PLO38970
      GO TO 230                                                         PLO38980
C                                                                       PLO38990
C     TRANSLATE EXPONENT.                                               PLO39000
C                                                                       PLO39010
C-----BEGINNING OF EXPONENT FOUND (X OR D). CHECK FOR FOLLOWING - OR +. PLO39020
  130 J=J+1                                                             PLO39030
      IFIELD=FIELD(J)                                                   PLO39040
      IF(IFIELD.EQ.MINUS) GO TO 160                                     PLO39050
      IF(IFIELD.NE.PLUS) GO TO 150                                      PLO39060
C----- + FOUND. INITIALIZE EXPONENT SIGN.                               PLO39070
  140 J=J+1                                                             PLO39080
  150 KSIGN=1                                                           PLO39090
      GO TO 170                                                         PLO39100
C----- - FOUND. INITIALIZE EXPONENT SIGN.                               PLO39110
  160 J=J+1                                                             PLO39120
      KSIGN=-1                                                          PLO39130
C-----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.   PLO39140
  170 KEXP=0                                                            PLO39150
      DO 200 I=J,9                                                      PLO39160
C***** TRKOV
      JC=I
C***** TRKOV
      IFIELD=FIELD(I)                                                   PLO39170
      IF(IFIELD.EQ.BLANK) GO TO 200                                     PLO39180
      DO 180 K=1,10                                                     PLO39190
      IF(IFIELD.EQ.DIGIT(K)) GO TO 190                                  PLO39200
  180 CONTINUE                                                          PLO39210
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 PLO39220
      GO TO 250                                                         PLO39230
C-----DIGIT FOUND. INCREMENT EXPONENT.                                  PLO39240
C-----OFFSET.                                                           PLO39250
  190 KEXP=10*KEXP+(K-1)                                                PLO39260
  200 CONTINUE                                                          PLO39270
C-----ENTIRE FIELD TRANSLATED (WITH EXPONENT). CONVERT TO FLOATING      PLO39280
C-----POINT.                                                            PLO39290
      X=IN                                                              PLO39300
      KEXP=KSIGN*KEXP                                                   PLO39310
      IF(IPT.GT.0) KEXP=KEXP-IPT                                        PLO39320
      IF(KEXP) 210,230,220                                              PLO39330
  210 KEXP=-KEXP                                                        PLO39340
      X=X/TEN(KEXP)                                                     PLO39350
      GO TO 230                                                         PLO39360
  220 X=X*TEN(KEXP)                                                     PLO39370
  230 X=XSIGN*X                                                         PLO39380
  240 RETURN                                                            PLO39390
C***** OLD
C 250 MESS(J)=STAR                                                      PLO39400
C***** OLD
C***** TRKOV
  250 MESS(JC)=STAR
C***** TRKOV
      WRITE(OUTP,6000) FIELD,MESS                                       PLO39410
      X=ZERO                                                            PLO39420
C***** OLD
C     MESS(J)=BLANK                                                     PLO39430
C***** OLD
C***** TRKOV
      MESS(JC)=BLANK
C***** TRKOV
      RETURN                                                            PLO39440
C***** OLD
C6000 FORMAT(1X,9A1/1X,9A1/                                             PLO39450
C    1 ' SUBROUTINE FLOAT9...ERROR IN INPUT DATA...TRANSLATED AS 0.0')  PLO39460
C***** OLD
C***** TRKOV
 6000 FORMAT(/1X,' STRING "',9A1,'"'/
     1        1X,' COLUMN  ',9A1/
     1 ' SUBROUTINE FLOAT9...ERROR IN INPUT DATA...TRANSLATED AS 0.0')
C***** TRKOV
      END                                                               PLO39470
      SUBROUTINE RQEX(KGET,IZA,MF,MT,EN)                                PLO39480
C                                                                       PLO39490
C     COMPARE CURRENT EXFOR ZA/MF/MT/INCIDENT ENERGY TO REQUESTS.       PLO39500
C                                                                       PLO39510
C***** OLD
C     COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),        PLO39520
C    1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),NGET,IGET              PLO39530
C***** OLD
C***** TRKOV
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
      COMMON/EPSMF6/EP6
C***** TRKOV
      DO 10 KGET=1,NGET                                                 PLO39540
      IF(IZA.LT.IZALOW(KGET).OR.IZA.GT.IZAHI(KGET)) GO TO 10            PLO39550
      IF(MF.LT.MFLOW(KGET).OR.MF.GT.MFHI(KGET)) GO TO 10                PLO39560
      IF(MT.LT.MTLOW(KGET).OR.MT.GT.MTHI(KGET)) GO TO 10                PLO39570
      IF(EN.GE.ELGET(KGET).AND.EN.LE.EHGET(KGET)) GO TO 20              PLO39580
   10 CONTINUE                                                          PLO39590
      KGET=0                                                            PLO39600
C***** OLD
C  20 RETURN
C***** OLD
C***** TRKOV
   20 IF(EPGET(KGET).GT.0 .AND. EPGET(KGET).LT.0.1) THEN
        EP6=EPGET(KGET)
      ELSE
        EP6=0.02
      END IF
      RETURN
C***** TRKOV
      END                                                               PLO39620
      SUBROUTINE ZAMFMT                                                 PLO39630
C                                                                       PLO39640
C     READ ALL,                                                         PLO39650
C     (1) SPECIAL ZA TITLES (IZA LESS THAN 1000)                        PLO39660
C     (2) MF TITLES                                                     PLO39670
C     (3) MT TITLES                                                     PLO39680
C                                                                       PLO39690
C     WARNING....THIS ROUTINE MUST BE CALLED BEFORE TRYING TO DEFINE    PLO39700
C     ANY ZA/MF/MT EQUIVALENCES.                                        PLO39710
C                                                                       PLO39720
      INTEGER OUTP                                                      PLO39730
      CHARACTER*4 ZATAB,MFTAB,MTTAB                                     PLO39740
C***** OLD
C     COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO39750
C***** OLD
C***** TRKOV
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
C***** TRKOV
      COMMON/UNITT/NTAPE1,NTAPE2,NTAPE3                                 PLO39760
      COMMON/TABZAC/ZATAB(3,200)                                        PLO39770
      COMMON/TABZAI/MZLONG,IZATAB(200)                                  PLO39780
      COMMON/TABMFC/MFTAB(8,100)                                        PLO39790
      COMMON/TABMFI/MFLONG,IMFTAB(4,100)                                PLO39800
      COMMON/TABMTC/MTTAB(10,300)                                       PLO39810
      COMMON/TABMTI/MTLONG,IMTTAB(3,300)                                PLO39820
C-----DEFINE TABLE SIZES.                                               PLO39830
      DATA MZMAX/200/                                                   PLO39840
      DATA MFMAX/100/                                                   PLO39850
      DATA MTMAX/300/                                                   PLO39860
C-----INDICATE PROGRAM IS READING TRANSLATION TABLES.                   PLO39870
      WRITE(OUTP,6000)                                                  PLO39880
C                                                                       PLO39890
C     READ SPECIAL ZA TITLES.                                           PLO39900
C                                                                       PLO39910
      DO 10 MZLONG=1,MZMAX                                              PLO39920
      READ(NTAPE1,1200,END=20,ERR=30) IZATAB(MZLONG),                   PLO39930
     1 (ZATAB(K,MZLONG),K=1,3)                                          PLO39940
   10 CONTINUE                                                          PLO39950
      MZLONG=MZMAX                                                      PLO39960
      GO TO 40                                                          PLO39970
C-----END OF REACTIONS.                                                 PLO39980
   20 MZLONG=MZLONG-1                                                   PLO39990
      IF(MZLONG.GT.0) GO TO 40                                          PLO40000
C-----ERROR READING.                                                    PLO40010
   30 WRITE(OUTP,6040)                                                  PLO40020
      GO TO 140                                                         PLO40030
   40 WRITE(OUTP,6010) MZLONG,MZMAX                                     PLO40040
C                                                                       PLO40050
C     READ MF TITLES                                                    PLO40060
C                                                                       PLO40070
      DO 60 MFLONG=1,MFMAX                                              PLO40080
   50 READ(NTAPE2,1400,END=70,ERR=80) IMFTAB(2,MFLONG),IMFTAB(3,MFLONG),PLO40090
     1 IMFTAB(4,MFLONG),(MFTAB(K,MFLONG),K=1,8)                         PLO40100
      IF(IMFTAB(2,MFLONG).LE.0) GO TO 50                                PLO40110
   60 CALL SIZER(MFTAB(1,MFLONG),IMFTAB(1,MFLONG),32)                   PLO40120
      MFLONG=MFMAX                                                      PLO40130
      GO TO 90                                                          PLO40140
C-----END OF REACTIONS.                                                 PLO40150
   70 MFLONG=MFLONG-1                                                   PLO40160
      IF(MFLONG.GT.0) GO TO 90                                          PLO40170
C-----ERROR READING.                                                    PLO40180
   80 WRITE(OUTP,6050)                                                  PLO40190
      GO TO 140                                                         PLO40200
   90 WRITE(OUTP,6020) MFLONG,MFMAX                                     PLO40210
C                                                                       PLO40220
C     READ MT TITLES                                                    PLO40230
C                                                                       PLO40240
      DO 100 MTLONG=1,MTMAX                                             PLO40250
      READ(NTAPE3,1500,END=110,ERR=120) IMTTAB(2,MTLONG),               PLO40260
     1 IMTTAB(3,MTLONG),(MTTAB(K,MTLONG),K=1,10)                        PLO40270
  100 CALL SIZER(MTTAB(1,MTLONG),IMTTAB(1,MTLONG),40)                   PLO40280
      MTLONG=MTMAX                                                      PLO40290
      GO TO 130                                                         PLO40300
C-----END OF REACTIONS.                                                 PLO40310
  110 MTLONG=MTLONG-1                                                   PLO40320
      IF(MTLONG.GT.0) GO TO 130                                         PLO40330
C-----ERROR READING.                                                    PLO40340
  120 WRITE(OUTP,6050)                                                  PLO40350
      GO TO 140                                                         PLO40360
  130 WRITE(OUTP,6030) MTLONG,MTMAX                                     PLO40370
      GO TO 150                                                         PLO40380
  140 STOP                                                              PLO40390
  150 RETURN                                                            PLO40400
 1200 FORMAT(I5,1X,3A4)                                                 PLO40410
 1400 FORMAT(3I5,1X,8A4)                                                PLO40420
 1500 FORMAT(2I5,1X,10A4)                                               PLO40430
 6000 FORMAT(' READING TRANSLATION TABLES'/1X,72('='))                  PLO40440
 6010 FORMAT(' SPECIAL ZA TITLES----------',I5,'(',I5,' ALLOWED)')      PLO40450
 6020 FORMAT(' MF TITLES------------------',I5,'(',I5,' ALLOWED)')      PLO40460
 6030 FORMAT(' MT TITLES------------------',I5,'(',I5,' ALLOWED)')      PLO40470
 6040 FORMAT(' ERROR READING SPECIAL ZA TITLES...EXECUTION TERMINATED') PLO40480
 6050 FORMAT(' ERROR READING MF TITLES...EXECUTION TERMINATED')         PLO40490
 6060 FORMAT(' ERROR READING MT TITLES...EXECUTION TERMINATED')         PLO40500
      END                                                               PLO40510
      SUBROUTINE ZAHOL(ZA,MSTAT1,ZABCD,IZABCD)                          PLO40520
C                                                                       PLO40530
C     GIVEN ANY ZA = (1000*Z+A) THIS ROUTINE WILL DEFINE A THREE WORD   PLO40540
C     EQUIVALENT IN ONE OF THE TWO FOLLOWING FORMS.                     PLO40550
C                                                                       PLO40560
C     IF Z IS GREATER THAN ZERO....ZZZ-SS-AAAM                          PLO40570
C                                  E.G., 26056-M = 26-FE-56M            PLO40580
C                                                                       PLO40590
C     IF Z IS EQUAL TO ZERO........3A4 CHARACTER EQUIVALENT             PLO40600
C                                  E.G., ZA=302 = ZIRCALLOY-2           PLO40610
C                                                                       PLO40620
      INTEGER ZA,Z,A                                                    PLO40630
      CHARACTER*1 MSTAT1                                                PLO40640
      CHARACTER*4 ZATAB,ZABCD,DUM1,DUM2,BLANK4                          PLO40650
      DIMENSION ZATAB(110),DUM1(54),DUM2(56),ZABCD(4)                   PLO40660
      EQUIVALENCE (ZATAB(1),DUM1(1)),(ZATAB(55),DUM2(1))                PLO40670
      DATA DUM1/                                                        PLO40680
     1 '-H -','-He-','-Li-','-Be-','-B -','-C -',                       PLO40690
     2 '-N -','-O -','-F -','-Ne-','-Na-','-Mg-',                       PLO40700
     3 '-Al-','-Si-','-P -','-S -','-Cl-','-Ar-',                       PLO40710
     4 '-K -','-Ca-','-Sc-','-Ti-','-V -','-Cr-',                       PLO40720
     5 '-Mm-','-Fe-','-Co-','-Ni-','-Cu-','-Zn-',                       PLO40730
     6 '-Ga-','-Ge-','-As-','-Se-','-Br-','-Kr-',                       PLO40740
     7 '-Rb-','-Sr-','-Y -','-Zr-','-Nb-','-Mo-',                       PLO40750
     8 '-Tc-','-Ru-','-Rh-','-Pd-','-Ag-','-Cd-',                       PLO40760
     9 '-In-','-Sn-','-Sb-','-Te-','-I -','-Xe-'/                       PLO40770
      DATA DUM2/                                                        PLO40780
     1 '-Cs-','-Ba-','-La-','-Ce-','-Pr-','-Nd-',                       PLO40790
     2 '-Pm-','-Sm-','-Eu-','-Gd-','-Tb-','-Dy-',                       PLO40800
     3 '-Ho-','-Er-','-Tm-','-Yb-','-Lu-','-Hf-',                       PLO40810
     4 '-Ta-','-W -','-Re-','-Os-','-Ir-','-Pt-',                       PLO40820
     5 '-Au-','-Hg-','-Tl-','-Pb-','-Bi-','-Po-',                       PLO40830
     6 '-At-','-Rn-','-Fr-','-Ra-','-Ac-','-Th-',                       PLO40840
     7 '-Pa-','-U -','-Np-','-Pu-','-Am-','-Cm-',                       PLO40850
     8 '-Bk-','-Cf-','-Es-','-Fm-','-Md-','-No-',                       PLO40860
     9 '-Lr-',' ',' ',' ',' ',' ',' ','-Err'/                           PLO40870
      DATA BLANK4/'    '/                                               PLO40880
      DO 10 I=1,4                                                       PLO40890
   10 ZABCD(I)=BLANK4                                                   PLO40900
C-----SPECIAL HANDLING FOR SPECIAL ENDF/B MATERIALS.                    PLO40910
      IF(ZA.GE.1000) GO TO 20                                           PLO40920
      CALL COMPND(ZA,ZABCD)                                             PLO40930
      GO TO 30                                                          PLO40940
C-----INSURE Z IS IN LEGAL RANGE.                                       PLO40950
   20 Z=ZA/1000                                                         PLO40960
      A=ZA-1000*Z                                                       PLO40970
      IF(Z.LT.1.OR.Z.GT.110) Z=110                                      PLO40980
C-----PACK ZZZ-SS-AAA INTO CHARACTER FORM.                              PLO40990
      CALL PACKZA(Z,A,MSTAT1,ZATAB(Z),ZABCD)                            PLO41000
C-----DEFINE LENGTH OF PACKED ZA.                                       PLO41010
   30 CALL SIZER(ZABCD,IZABCD,16)                                       PLO41020
      RETURN                                                            PLO41030
      END                                                               PLO41040
      SUBROUTINE PACKZA(Z,A,MSTAT1,ZATAB,ZABCD)                         PLO41050
C                                                                       PLO41060
C     PACK ZZZ-SS-AAAM INTO CHARACTER FORM.                             PLO41070
C                                                                       PLO41080
      INTEGER Z,A                                                       PLO41090
      CHARACTER*1 ZATAB,ZABCD,DIGITS,BLANK,MSTAT1,DASH                  PLO41100
C***** TRKOV 
      DIMENSION ZATAB(4),ZABCD(16),DIGITS(10),MULT(3)
C***** TRKOV END
C***** OLD
C     DIMENSION ZATAB(4),ZABCD(12),DIGITS(10),MULT(3)                   PLO41110
C***** OLD
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              PLO41120
      DATA BLANK/' '/                                                   PLO41130
      DATA DASH/'-'/                                                    PLO41140
      DATA MULT/1,10,100/                                               PLO41150
C-----INITIALIZE CHARACTERS AND INDEX.                                  PLO41160
      DO 10 I=1,16                                                      PLO41170
   10 ZABCD(I)=BLANK                                                    PLO41180
      I=0                                                               PLO41190
C-----PACK Z.                                                           PLO41200
      J=3                                                               PLO41210
      IF(Z.LT.100) J=2                                                  PLO41220
      IF(Z.LT.10) J=1                                                   PLO41230
      MULTZ=MULT(J)                                                     PLO41240
      DO 20 K=1,J                                                       PLO41250
      IZ=Z/MULTZ                                                        PLO41260
      I=I+1                                                             PLO41270
      ZABCD(I)=DIGITS(IZ+1)                                             PLO41280
      Z=Z-MULTZ*IZ                                                      PLO41290
   20 MULTZ=MULTZ/10                                                    PLO41300
C-----PACK CHEMICAL SYMBOL.                                             PLO41310
      DO 30 K=1,4                                                       PLO41320
      I=I+1                                                             PLO41330
   30 ZABCD(I)=ZATAB(K)                                                 PLO41340
C-----PACK A.                                                           PLO41350
      J=3                                                               PLO41360
      IF(A.LT.100) J=2                                                  PLO41370
      IF(A.LT.10) J=1                                                   PLO41380
      MULTA=MULT(J)                                                     PLO41390
      DO 40 K=1,J                                                       PLO41400
      IA=A/MULTA                                                        PLO41410
      I=I+1                                                             PLO41420
      ZABCD(I)=DIGITS(IA+1)                                             PLO41430
      A=A-MULTA*IA                                                      PLO41440
   40 MULTA=MULTA/10                                                    PLO41450
C-----IF METASTABLE STATE FLAG IS NOT BLANK ADD IT TO STRING.           PLO41460
      IF(MSTAT1.NE.BLANK) CALL METAST(ZABCD,MSTAT1,I)                   PLO41470
      RETURN                                                            PLO41480
      END                                                               PLO41490
      SUBROUTINE MTHOL(MT,MTBCD,IMTBCD,MSTAT2)                          PLO41500
C                                                                       PLO41510
C     DEFINE HOLLERITH EQUIVALENT OF REACTION (MT).                     PLO41520
C                                                                       PLO41530
C     DEFINITION OF TABLES                                              PLO41540
C                                                                       PLO41550
C     MTTAB = CHARACTER EQUIVALENT OF EACH MT NUMBER                    PLO41560
C              UP TO 40 CHARACTERS PER MT - LEFT ADJUSTED               PLO41570
C     IMTTAB = (1) NUMBER OF CHRACTERS IN EACH CHARACTER EQUIVALENT     PLO41580
C              (2) LOWER MT LIMIT                                       PLO41590
C              (3) UPPER MT LIMIT                                       PLO41600
C                                                                       PLO41610
C     THE CHARACTER EQUIVALENT IS RETURNED IN MTBCD                     PLO41620
C                                                                       PLO41630
C     MTBCD  = CHARACTER EQUIVALENT OF MT                               PLO41640
C              UP TO 40 CHARACTERS - LEFT ADJUSTED                      PLO41650
C     IMTBCD = NUMBER OF CHARACTERS IN CHARACTER EQUIVALENT OF MT       PLO41660
C                                                                       PLO41670
      INTEGER OUTP                                                      PLO41680
      CHARACTER*4 MTTAB,MTBCD,UNKNOW,ZAPBCD,BLANK,MSTATP,MSTAT2         PLO41690
      COMMON/RATZA/IZARAT,MTRAT,MFIN                                    PLO41700
      COMMON/TABMTC/MTTAB(10,300)                                       PLO41710
      COMMON/TABMTI/MTLONG,IMTTAB(3,300)                                PLO41720
      DIMENSION MTBCD(20),UNKNOW(10),ZAPBCD(20)                         PLO41730
      DATA BLANK/'    '/                                                PLO41740
      DATA UNKNOW/                                                      PLO41750
     1 '***U','ndef','ined','***',' ',' ',' ',' ',' ',' '/              PLO41760
C-----LOOK UP CHARACTER EQUIVALENT AND LOAD INTO CORE.                  PLO41770
      DO 20 M=1,MTLONG                                                  PLO41780
      IF(MT-IMTTAB(3,M)) 10,50,20                                       PLO41790
   10 IF(MT-IMTTAB(2,M)) 30,50,50                                       PLO41800
   20 CONTINUE                                                          PLO41810
C-----MT NUMBER NOT DEFINED. LOAD **UNDEFINED**                         PLO41820
   30 DO 40 L=1,10                                                      PLO41830
   40 MTBCD(L)=UNKNOW(L)                                                PLO41840
      IMTBCD=15                                                         PLO41850
      RETURN                                                            PLO41860
C-----MT NUMBER IS DEFINED.                                             PLO41870
   50 DO 60 L=1,10                                                      PLO41880
   60 MTBCD(L)=MTTAB(L,M)                                               PLO41890
      IMTBCD=IMTTAB(1,M)                                                PLO41900
C-----IF MT=9000-9999 (PARTICLE/ISOTOPE PRODUCTION) ADD PRODUCT ZA      PLO41910
C-----BEFORE MT I.D.                                                    PLO41920
      IF(MT.LT.9000) GO TO 70                                           PLO41930
      CALL ZAHOL(IZARAT,MSTAT2,ZAPBCD,KZAP)                             PLO41940
      CALL PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)                             PLO41950
      GO TO 80                                                          PLO41960
C-----IF FINAL METASTABLE STATE FLAG IS NOT BLANK ADD IT TO STRING.     PLO41970
   70 IF(MSTAT2.NE.BLANK) CALL METAST(MTBCD,MSTAT2,IMTBCD)              PLO41980
   80 RETURN                                                            PLO41990
      END                                                               PLO42000
      SUBROUTINE METAST(BCD,MSTATE,IBCD)                                PLO42010
C                                                                       PLO42020
C     ADD METASTABLE STATE FLAG TO ZA OR REACTION.                      PLO42030
C                                                                       PLO42040
C     ON ENTRY INTO THIS ROUTINE IBCD IS THE LENGTH OF THE CHARACTER    PLO42050
C     STRING DESCRIBING THE ZA AND/OR REACTION. UNLESS THE METASTABLE   PLO42060
C     STATE FLAG IS BLANK IT WILL BE ADDED TO THE STRING.               PLO42070
C                                                                       PLO42080
      CHARACTER*1 BCD,MSTATE,MSTAB1,MSTAB2,BLANK                        PLO42090
      DIMENSION BCD(40),MSTAB1(14),MSTAB2(3,15)                         PLO42100
      DATA BLANK/' '/                                                   PLO42110
      DATA MSTAB1/                                                      PLO42120
     1 ' ','T','G','M','1','2','3','4','5','6','7','8','9','+'/         PLO42130
      DATA MSTAB2/                                                      PLO42140
     1 ' ',' ',' ',  ' ',' ',' ',  '-','G',' ',  '-','M',' ',           PLO42150
     2 '-','M','1',  '-','M','2',  '-','M','3',  '-','M','4',           PLO42160
     3 '-','M','5',  '-','M','6',  '-','M','7',  '-','M','8',           PLO42170
     4 '-','M','9',  '-','M','+',  '-','M','?'/                         PLO42180
C-----LOOK UP ONE CHARACTER STATE FLAG.                                 PLO42190
      DO 10 I=1,14                                                      PLO42200
      IF(MSTATE.EQ.MSTAB1(I)) GO TO 20                                  PLO42210
   10 CONTINUE                                                          PLO42220
      I=15                                                              PLO42230
   20 DO 30 J=1,3                                                       PLO42240
      IF(MSTAB2(J,I).EQ.BLANK) GO TO 40                                 PLO42250
      IBCD=IBCD+1                                                       PLO42260
   30 BCD(IBCD)=MSTAB2(J,I)                                             PLO42270
   40 RETURN                                                            PLO42280
      END                                                               PLO42290
      SUBROUTINE SIZER(BCD,IBCD,MAXBCD)                                 PLO42300
C                                                                       PLO42310
C     DEFINE NON-BLANK LENGTH OF CHARACTER STRING.                      PLO42320
C                                                                       PLO42330
      CHARACTER*1 BCD,BLANK                                             PLO42340
      DIMENSION BCD(MAXBCD)                                             PLO42350
      DATA BLANK/' '/                                                   PLO42360
      IBCD=MAXBCD                                                       PLO42370
      DO 10 I=1,MAXBCD                                                  PLO42380
      IF(BCD(IBCD).NE.BLANK) GO TO 20                                   PLO42390
   10 IBCD=IBCD-1                                                       PLO42400
   20 RETURN                                                            PLO42410
      END                                                               PLO42420
      SUBROUTINE LEFTY(BCD,IBCD,NBCD,KBCD)                              PLO42430
C                                                                       PLO42440
C     LEFT ADJUST TITLES AND DEFINE NUMBER OF NON-BLANK CHARACTERS AND  PLO42450
C     NUMBER OF PLOTTED CHARACTERS (NON-BLANK MINUS CONTROL CHARACTERS).PLO42460
C                                                                       PLO42470
      CHARACTER*1 BCD,BLANK,CHRTAB,CHRTRL                               PLO42480
      COMMON/SYMTB1/CHRTAB(256),CHRTRL(256)                             PLO42490
C***** OLD
C     COMMON/SYMTB2/XCHAR(2000),YCHAR(2000),ICHPEN(2000),INDCHR(2,256), PLO42500
C    1 ICHAR,ICNTRL                                                     PLO42510
C***** OLD
C***** TRKOV
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
C***** TRKOV
      DIMENSION BCD(KBCD)                                               PLO42520
      DATA BLANK/' '/                                                   PLO42530
C-----FIND LAST NON-BLANK CHARACTER.                                    PLO42540
      I=KBCD                                                            PLO42550
      DO 10 II=1,KBCD                                                   PLO42560
      IF(BCD(I).NE.BLANK) GO TO 20                                      PLO42570
   10 I=I-1                                                             PLO42580
      IBCD=0                                                            PLO42590
      NBCD=0                                                            PLO42600
      GO TO 70                                                          PLO42610
C-----FIND FIRST NON-BLANK CHARACTER.                                   PLO42620
   20 DO 30 J=1,I                                                       PLO42630
      IF(BCD(J).NE.BLANK) GO TO 40                                      PLO42640
   30 CONTINUE                                                          PLO42650
      IBCD=0                                                            PLO42660
      NBCD=0                                                            PLO42670
      GO TO 70                                                          PLO42680
   40 IBCD=I                                                            PLO42690
C-----IF REQUIRED SHIFT CHARACTERS LEFT.                                PLO42700
      IF(J.EQ.1) GO TO 70                                               PLO42710
      K=0                                                               PLO42720
      DO 50 L=J,I                                                       PLO42730
      K=K+1                                                             PLO42740
   50 BCD(K)=BCD(L)                                                     PLO42750
      IBCD=K                                                            PLO42760
      K=K+1                                                             PLO42770
      DO 60 I=K,KBCD                                                    PLO42780
   60 BCD(I)=BLANK                                                      PLO42790
      CONTINUE                                                          PLO42800
C-----COUNT SPECIAL NON-PRINTING CHARACTERS.                            PLO42810
   70 NBCD=IBCD                                                         PLO42820
      IF(ICNTRL.LE.0) GO TO 100                                         PLO42830
      DO 90 I=1,IBCD                                                    PLO42840
      DO 80 J=1,ICNTRL                                                  PLO42850
      IF(BCD(I).NE.CHRTRL(J)) GO TO 80                                  PLO42860
      NBCD=NBCD-1                                                       PLO42870
      GO TO 90                                                          PLO42880
   80 CONTINUE                                                          PLO42890
   90 CONTINUE                                                          PLO42900
  100 RETURN                                                            PLO42910
      END                                                               PLO42920
      SUBROUTINE MFHOL(MF,MT,MFBCD,IMFBCD)                              PLO42930
C                                                                       PLO42940
C     DEFINE HOLLERITH EQUIVALENT OF DATA TYPE (MF).                    PLO42950
C                                                                       PLO42960
      CHARACTER*4 MFBCD,MFTAB,ERROR                                     PLO42970
      COMMON/TABMFC/MFTAB(8,100)                                        PLO42980
      COMMON/TABMFI/MFLONG,IMFTAB(4,100)                                PLO42990
      DIMENSION MFBCD(8),ERROR(8)                                       PLO43000
      DATA ERROR/'*** ','Erro','r **','*',' ',' ',' ',' '/              PLO43010
C-----LOOK UP MF AND MT RANGE IN TABLE.                                 PLO43020
      DO 10 I=1,MFLONG                                                  PLO43030
      IF(MF.NE.IMFTAB(2,I)) GO TO 10                                    PLO43040
      IF(MT.GE.IMFTAB(3,I).AND.MT.LE.IMFTAB(4,I)) GO TO 30              PLO43050
   10 CONTINUE                                                          PLO43060
C-----MF AND MT RANGE IS NOT IN TABLE.                                  PLO43070
      IMFBCD=13                                                         PLO43080
      DO 20 K=1,8                                                       PLO43090
   20 MFBCD(K)=ERROR(K)                                                 PLO43100
      RETURN                                                            PLO43110
C-----DEFINE MF AND MT RANGE EQUIVALENT.                                PLO43120
   30 IMFBCD=IMFTAB(1,I)                                                PLO43130
      DO 40 K=1,8                                                       PLO43140
   40 MFBCD(K)=MFTAB(K,I)                                               PLO43150
      RETURN                                                            PLO43160
      END                                                               PLO43170
      SUBROUTINE PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)                       PLO43180
C                                                                       PLO43190
C     COMBINE PRODUCT ZA WITH MT I.D. FOR PARTICLE/ISOTOPE PRODUCTION.  PLO43200
C                                                                       PLO43210
      CHARACTER*1 ZAPBCD,MTBCD,MTPBCD,BLANK                             PLO43220
      DIMENSION ZAPBCD(12),MTBCD(80),MTPBCD(80)                         PLO43230
      DATA BLANK/' '/                                                   PLO43240
C-----PACK PRODUCT ZA.                                                  PLO43250
      I=0                                                               PLO43260
      DO 10 J=1,KZAP                                                    PLO43270
      I=I+1                                                             PLO43280
   10 MTPBCD(I)=ZAPBCD(J)                                               PLO43290
C-----INSERT BLANK AND THEN MT I.D.                                     PLO43300
      I=I+1                                                             PLO43310
      MTPBCD(I)=BLANK                                                   PLO43320
      DO 20 J=1,IMTBCD                                                  PLO43330
      I=I+1                                                             PLO43340
   20 MTPBCD(I)=MTBCD(J)                                                PLO43350
C-----DEFINE NEW MT I.D. LENGTH AND COPY BACK TO MT I.D.                PLO43360
      IMTBCD=I                                                          PLO43370
      DO 30 J=1,IMTBCD                                                  PLO43380
   30 MTBCD(J)=MTPBCD(J)                                                PLO43390
      RETURN                                                            PLO43400
      END                                                               PLO43410
      SUBROUTINE COMPND(ZA,ZABCD)                                       PLO43420
C                                                                       PLO43430
C     DEFINE ZA EQUIVALENCE FOR ENDF/B SPECIAL MATERIALS AND COMPOUNDS. PLO43440
C                                                                       PLO43450
      INTEGER ZA                                                        PLO43460
      CHARACTER*4 ZABCD,ZATAB,BLANK4,ERROR                              PLO43470
      COMMON/TABZAC/ZATAB(3,200)                                        PLO43480
      COMMON/TABZAI/MZLONG,IZATAB(200)                                  PLO43490
      DIMENSION ZABCD(4),ERROR(3)                                       PLO43500
C-----DEFINE NUMBER OF SPECIAL MATERIALS AND COMPOUNDS IN TABLE.        PLO43510
      DATA BLANK4/'    '/                                               PLO43520
      DATA ERROR/'** E','rror',' ** '/                                  PLO43530
      ZABCD(4)=BLANK4                                                   PLO43540
C-----ERROR IF ZA IS NOT LESS THAN 1000.                                PLO43550
      IF(ZA.GE.1000) GO TO 20                                           PLO43560
C-----LOOK UP ZA IN TABLE.                                              PLO43570
      DO 10 N=1,MZLONG                                                  PLO43580
      IF(IZATAB(N)-ZA) 10,40,20                                         PLO43590
   10 CONTINUE                                                          PLO43600
C-----ZA IS NOT IN TABLE.                                               PLO43610
   20 DO 30 I=1,3                                                       PLO43620
   30 ZABCD(I)=ERROR(I)                                                 PLO43630
      RETURN                                                            PLO43640
C-----MATCH FOUND. DEFINE HOLLERITH EQUIVALENCE.                        PLO43650
   40 DO 50 I=1,3                                                       PLO43660
   50 ZABCD(I)=ZATAB(I,N)                                               PLO43670
      RETURN                                                            PLO43680
      END                                                               PLO43690
      SUBROUTINE META10(OUT,MSTATE)                                     PLO43700
C                                                                       PLO43710
C     DEFINE CHARACTER EQUIVALENT OF METASTABLE STATE FLAG              PLO43720
C                                                                       PLO43730
      CHARACTER*1 OUT,MSTATE,MTAB1,MTAB2                                PLO43740
      DIMENSION MTAB1(10),MTAB2(10)                                     PLO43750
      DATA MTAB1/                                                       PLO43760
     1 'G','1','2','3','4','5','?','M','+',' '/                         PLO43770
      DATA MTAB2/                                                       PLO43780
     1 '0','1','2','3','4','5','6','7','8','9'/                         PLO43790
C-----LOOK UP METASTABLE STATE CHARACTER.                               PLO43800
      DO 10 I=1,10                                                      PLO43810
      IF(OUT.EQ.MTAB2(I)) GO TO 20                                      PLO43820
   10 CONTINUE                                                          PLO43830
C-----SET INDEX TO UNKNOWN.                                             PLO43840
      I=7                                                               PLO43850
   20 MSTATE=MTAB1(I)                                                   PLO43860
      RETURN                                                            PLO43870
      END                                                               PLO43880
      SUBROUTINE NUMBRH(XI,YI,HT,Z,ANG,IZ)                              PLO43890
C                                                                       PLO43900
C     CONVERT FLOATING POINT NUMBER OF CHARACTER STRING AND PLOT WIDE.  PLO43910
C                                                                       PLO43920
      CHARACTER*1 DIGIT,MINUS,DOT,FIELD                                 PLO43930
      DIMENSION DIGIT(10),FIELD(16)                                     PLO43940
      DATA DIGIT/'1','2','3','4','5','6','7','8','9','0'/               PLO43950
      DATA MINUS/'-'/                                                   PLO43960
      DATA DOT/'.'/                                                     PLO43970
C                                                                       PLO43980
C     ROUND-OFF NUMBER TO REQUIRED DIGITS, E.G. IF WRITING 54.321 WITH  PLO43990
C     2 DIGITS AFTER DECIMAL POINT DEFINE MR=5432 (ENTIRE STRING) AND   PLO44000
C     MR1=54 (DIGITS PRECEDING DECIMAL POINT).                          PLO44010
C                                                                       PLO44020
      AZ=ABS(Z)                                                         PLO44030
      IIZ=IZ                                                            PLO44040
      IF(IIZ.LE.0) IIZ=0                                                PLO44050
      Z10=10.0**IIZ                                                     PLO44060
      MR=AZ*Z10+0.5                                                     PLO44070
      IZ10=Z10                                                          PLO44080
      MR1=MR/IZ10                                                       PLO44090
C                                                                       PLO44100
C     DETERMINE NUMBER OF DIGITS PRECEDING DECIMAL POINT.               PLO44110
C                                                                       PLO44120
      M10=1                                                             PLO44130
      DO 10 IDIG=1,12                                                   PLO44140
      NR=MR1/M10                                                        PLO44150
      IF(NR.LE.0) GO TO 20                                              PLO44160
   10 M10=10*M10                                                        PLO44170
C                                                                       PLO44180
C     NUMBER IS TOO BIG...NO PLOTTING.                                  PLO44190
C                                                                       PLO44200
      RETURN                                                            PLO44210
   20 IF(IDIG.EQ.1) GO TO 30                                            PLO44220
      IDIG=IDIG-1                                                       PLO44230
      M10=M10/10                                                        PLO44240
C                                                                       PLO44250
C     ADD DIGITS AFTER DECIMAL POINT.                                   PLO44260
C                                                                       PLO44270
   30 IDIG=IDIG+IIZ                                                     PLO44280
      M10=M10*IZ10                                                      PLO44290
C                                                                       PLO44300
C     IF NUMBER IS ZERO, PLOT ZERO AND RETURN.                          PLO44310
C                                                                       PLO44320
      IF(IDIG.GT.0) GO TO 40                                            PLO44330
      IFIELD=1                                                          PLO44340
      FIELD(1)=DIGIT(10)                                                PLO44350
      GO TO 80                                                          PLO44360
C                                                                       PLO44370
C     INITIALIZE CHARACTER COUNT.                                       PLO44380
C                                                                       PLO44390
   40 IFIELD=0                                                          PLO44400
C                                                                       PLO44410
C     IF NUMBER IS NEGATIVE INSERT MINUS SIGN.                          PLO44420
C                                                                       PLO44430
      IF(Z.GE.0.0) GO TO 50                                             PLO44440
      FIELD(1)=MINUS                                                    PLO44450
      IFIELD=1                                                          PLO44460
C                                                                       PLO44470
C     DEFINE POSITION OF DECIMAL POINT (IF ANY).                        PLO44480
C                                                                       PLO44490
   50 IDOT=IDIG-IZ                                                      PLO44500
      IF(IZ.LT.0) IDOT=IDIG+2                                           PLO44510
C                                                                       PLO44520
C     INSERT DIGITS AND DECIMAL POINT (IF ANY) IN STRING.               PLO44530
C                                                                       PLO44540
      DO 70 I=1,IDIG                                                    PLO44550
      NDIG=MR/M10                                                       PLO44560
      KDIG=NDIG                                                         PLO44570
      IF(KDIG.EQ.0) KDIG=10                                             PLO44580
      IFIELD=IFIELD+1                                                   PLO44590
      FIELD(IFIELD)=DIGIT(KDIG)                                         PLO44600
C                                                                       PLO44610
C     INSERT DECIMAL POINT AT APPROPRIATE POSITION.                     PLO44620
C                                                                       PLO44630
      IF(I.NE.IDOT) GO TO 60                                            PLO44640
      IFIELD=IFIELD+1                                                   PLO44650
      FIELD(IFIELD)=DOT                                                 PLO44660
   60 MR=MR-M10*NDIG                                                    PLO44670
   70 M10=M10/10                                                        PLO44680
C                                                                       PLO44690
C     ENTIRE FIELD FORMATTED. PLOT IT.                                  PLO44700
C                                                                       PLO44710
   80 CALL SYMBLH(XI,YI,HT,FIELD,ANG,IFIELD)                            PLO44720
      END                                                               PLO44740
      SUBROUTINE SYMBLH(X,Y,HTIN,MESS,ANG,NMESS)                        PLO44750
C                                                                       PLO44760
C     DRAW THICK CHARACTERS DEFINED BY STROKES IN SYMBOL TABLE          PLO44770
C                                                                       PLO44780
      CHARACTER*1 MESS,CHRTAB,CHRTRL                                    PLO44790
      DIMENSION MESS(NMESS)                                             PLO44800
C***** OLD
C     COMMON/SYMTB1/CHRTAB(256),CHRTRL                                  PLO44810
C     COMMON/SYMTB2/XCHAR(2000),YCHAR(2000),ICHPEN(2000),INDCHR(2,256), PLO44820
C    1 ICHAR,ICNTRL                                                     PLO44830
C***** OLD
C***** TRKOV
      COMMON/SYMTB1/CHRTAB(256),CHRTRL(256)
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
C***** TRKOV
      COMMON/THICKY/ITHICK,THICK                                        PLO44840
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38     PLO44850
C-----INITIALIZE FLAG TO USE STANDARD CHARACTER SET.                    PLO44860
      DATA IALTER/0/                                                    PLO44870
C-----INITIALIZE X AND Y OFFSET (USED FOR SUB AND SUPER SCRIPTS).       PLO44880
      DATA XOFF/0.0/                                                    PLO44890
      DATA YOFF/0.0/                                                    PLO44900
C-----IF NO SOFTWARE CHARACTERS RETURN.                                 PLO44910
C***** OLD
C     IF(ICHAR.LE.0) RETURN                                             PLO44920
C***** OLD
C***** TRKOV
      IF(ICHR.LE.0) RETURN
C***** TRKOV
C-----SAVE LINE WIDTH AND DEFINE SCALED LINE WIDTH.                     PLO44930
      NTHICK=ITHICK                                                     PLO44940
      ITHICK=FLOAT(NTHICK)*HTIN/HT                                      PLO44950
C-----INITIALIZE POSITION AND DEFINE INCREMENTS.                        PLO44960
      X1=X                                                              PLO44970
      Y1=Y                                                              PLO44980
      IWAY=0                                                            PLO44990
      IF(ABS(ANG).GT.1.0) IWAY=1                                        PLO45000
      IF(IWAY.NE.0) GO TO 10                                            PLO45010
      DX1=HTIN                                                          PLO45020
      DY1=0.0                                                           PLO45030
      GO TO 20                                                          PLO45040
   10 DX1=0.0                                                           PLO45050
      DY1=HTIN                                                          PLO45060
C-----SET UP LOOP TO PLOT CHARACTERS ONE AT A TIME.                     PLO45070
   20 DO 120 N=1,NMESS                                                  PLO45080
C-----INITIALIZE COUNT OF THE NUMBER OF TIMES CHARACTER HAS BEEN FOUND  PLO45090
C-----(TO SELECT STANDARD OR ALTERNATE CHARACTER SET).                  PLO45100
      NALTER=0                                                          PLO45110
C***** OLD
C     DO 40 I=1,ICHAR                                                   PLO45120
C***** OLD
C***** TRKOV
      DO 40 I=1,ICHR
C***** TRKOV
      IF(MESS(N).NE.CHRTAB(I)) GO TO 40                                 PLO45130
C-----ALWAYS USE CONTROL CHARACTERS REGARDLESS OF CHARACTER SET.        PLO45140
      I1=INDCHR(1,I)                                                    PLO45150
      IF(ICHPEN(I1).LE.0) GO TO 50                                      PLO45160
C-----SELECT STANDARD OR ALTERNATE CHARACTER SET.                       PLO45170
      IF(NALTER.NE.IALTER) GO TO 30                                     PLO45180
C-----CHARACTER FOUND.                                                  PLO45190
      I2=INDCHR(2,I)                                                    PLO45200
      GO TO 70                                                          PLO45210
   30 NALTER=NALTER+1                                                   PLO45220
   40 CONTINUE                                                          PLO45230
C-----NOT SPECIAL CHARACTER...IGNORE.                                   PLO45240
      GO TO 120                                                         PLO45250
C-----CONTROL CHARACTER...CHANGE CHARACTER SET OR RE-DEFINE OFFSET.     PLO45260
   50 IF(ICHPEN(I1).EQ.0) GO TO 60                                      PLO45270
C-----CHANGE CHARACTER SETS.                                            PLO45280
      IALTER=1-IALTER                                                   PLO45290
      GO TO 120                                                         PLO45300
C-----DEFINE OFFSET.                                                    PLO45310
   60 XOFF=XOFF+XCHAR(I1)                                               PLO45320
      YOFF=YOFF+YCHAR(I1)                                               PLO45330
      GO TO 120                                                         PLO45340
   70 IF(IWAY.NE.0) GO TO 90                                            PLO45350
C-----HORIZONTAL.                                                       PLO45360
      DO 80 I=I1,I2                                                     PLO45370
   80 CALL PLOTH(X1+HTIN*(XOFF+XCHAR(I)),Y1+HTIN*(YOFF+YCHAR(I)),       PLO45380
     1 ICHPEN(I))                                                       PLO45390
C-----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSETPLO45400
      X1=X1+XOFF                                                        PLO45410
      GO TO 110                                                         PLO45420
C-----VERTICAL.                                                         PLO45430
   90 DO 100 I=I1,I2                                                    PLO45440
  100 CALL PLOTH(X1-HTIN*(YOFF+YCHAR(I)),Y1+HTIN*(XOFF+XCHAR(I)),       PLO45450
     1 ICHPEN(I))                                                       PLO45460
C-----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSETPLO45470
      Y1=Y1+XOFF                                                        PLO45480
C-----MOVE TO NEXT CHARACTER POSITION.                                  PLO45490
  110 X1=X1+DX1                                                         PLO45500
      Y1=Y1+DY1                                                         PLO45510
C-----TURN OFF ALTERNATE CHARACTER SET FLAG AND SET OFFSET TO ZERO.     PLO45520
      IALTER=0                                                          PLO45530
      XOFF=0.0                                                          PLO45540
      YOFF=0.0                                                          PLO45550
  120 CONTINUE                                                          PLO45560
C-----RESTORE LINE WIDTH.                                               PLO45570
      ITHICK=NTHICK                                                     PLO45580
      RETURN                                                            PLO45590
      END                                                               PLO45600
C***** OLD - Entire routine deactivated - records shifted-->C in col.1
C      SUBROUTINE SYMIN                                                  PLO45610
CC                                                                       PLO45620
CC     LOAD SPECIAL SYMBOL TABLE.                                        PLO45630
CC                                                                       PLO45640
C      CHARACTER*1 CHRTAB,CHRTRL                                         PLO45650
C      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2,ISYM                          PLO45660
C      COMMON/SYMTB1/CHRTAB(256),CHRTRL(256)                             PLO45670
C      COMMON/SYMTB2/XCHAR(2000),YCHAR(2000),ICHPEN(2000),INDCHR(2,256), PLO45680
C     1 ICHAR,ICNTRL                                                     PLO45690
CC-----INITIALIZE CHARACTER INDEX AND COUNT OF CONTROL CHARACTERS.       PLO45700
C      IHIGH=0                                                           PLO45710
C      ICNTRL=0                                                          PLO45720
CC-----SET UP LOOP TO READ UP TO 256 SPECIAL CHARACTERS.                 PLO45730
C      DO 30 ICHAR=1,256                                                 PLO45740
CC-----READ FIRST LINE TO DEFINE CHARACTER AND NUMBER OF STROKES.        PLO45750
C      READ(ISYM,1000,ERR=50,END=50) CHRTAB(ICHAR),ICOUNT                PLO45760
C      LOWEST=IHIGH+1                                                    PLO45770
C      IHIGH=IHIGH+ICOUNT                                                PLO45780
CC-----READ STROKES (X, Y, PEN POSITION).                                PLO45790
C      DO 10 I=LOWEST,IHIGH                                              PLO45800
C   10 READ(ISYM,1010,ERR=40,END=40) XCHAR(I),YCHAR(I),ICHPEN(I)         PLO45810
CC-----SAVE CONTROL CHARACTERS.                                          PLO45820
C      IF(ICOUNT.NE.1) GO TO 20                                          PLO45830
C      IF(ICHPEN(LOWEST).GT.0) GO TO 20                                  PLO45840
C      ICNTRL=ICNTRL+1                                                   PLO45850
C      CHRTRL(ICNTRL)=CHRTAB(ICHAR)                                      PLO45860
CC-----DEFINE INDICES TO SPECIAL CHARACTER STROKE TABLE.                 PLO45870
C   20 INDCHR(1,ICHAR)=LOWEST                                            PLO45880
C   30 INDCHR(2,ICHAR)=IHIGH                                             PLO45890
C      ICHAR=256                                                         PLO45900
C      GO TO 60                                                          PLO45910
CC-----DEINE LAST CHARACTER.                                             PLO45920
C   40 IHIGH=LOWEST-1                                                    PLO45930
C   50 ICHAR=ICHAR-1                                                     PLO45940
C   60 RETURN                                                            PLO45950
C 1000 FORMAT(1X,A1,I5)                                                  PLO45960
C 1010 FORMAT(2F7.3,I5)                                                  PLO45970
C      END                                                               PLO45980
C***** OLD
C***** TRKOV - whole routine replaced but deactivated double precision
C***** TRKOV   WARNING: this update can not be restored
      SUBROUTINE SYMIN
C=======================================================================
C
C     LOAD SPECIAL SYMBOL TABLE.
C
C=======================================================================
C...  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 CHRTAB,CHRTRL
      COMMON/SYMTB1/CHRTAB(256),CHRTRL(256)
C***** OLD
C     COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
C    1 ICHAR,ICNTRL,ISYM
C***** OLD
C***** TRKOV
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
C***** TRKOV
C-----DEFINE AVAILABLE CORE SIZE.
      DATA ICORE/5000/
C-----INITIALIZE CHARACTER INDEX AND COUNT OF CONTROL CHARACTERS.
      IHIGH=0
      ICNTRL=0
C-----SET UP LOOP TO READ UP TO 256 SPECIAL CHARACTERS.
C***** OLD
C     DO 30 ICHAR=1,256
C***** OLD
C***** TRKOV
      DO 30 JCHR=1,256
      ICHR=JCHR
C***** TRKOV
C-----READ FIRST LINE TO DEFINE CHARACTER AND NUMBER OF STROKES.
C***** OLD
C     READ(ISYM,60,ERR=40,END=40) CHRTAB(ICHAR),ICOUNT
C***** OLD
C***** TRKOV
      READ(ISYM,60,ERR=40,END=40) CHRTAB(ICHR),ICOUNT
C***** TRKOV
      LOWEST=IHIGH+1
      IHIGH=IHIGH+ICOUNT
C-----INSURE AVAILABLE CORE WILL NOT BE EXCEEDED.
      IF(IHIGH.GT.ICORE) GO TO 40
C-----READ STROKES (X, Y, PEN POSITION).
      DO 10 I=LOWEST,IHIGH
   10 READ(ISYM,70,ERR=40,END=40) XCHAR(I),YCHAR(I),ICHPEN(I)
C-----SAVE CONTROL CHARACTERS.
      IF(ICOUNT.NE.1) GO TO 20
      IF(ICHPEN(LOWEST).GT.0) GO TO 20
      ICNTRL=ICNTRL+1
C***** OLD
C     CHRTRL(ICNTRL)=CHRTAB(ICHAR)
C***** OLD
C***** TRKOV
      CHRTRL(ICNTRL)=CHRTAB(ICHR)
C***** TRKOV
C-----DEFINE INDICES TO SPECIAL CHARACTER STROKE TABLE.
C***** OLD
C  20 INDCHR(1,ICHAR)=LOWEST
C  30 INDCHR(2,ICHAR)=IHIGH
C     ICHAR=256
C***** OLD
C***** TRKOV
   20 INDCHR(1,ICHR)=LOWEST
   30 INDCHR(2,ICHR)=IHIGH
      ICHR=256
C***** TRKOV
      GO TO 50
C-----END OF DATA OR ERROR.
C***** OLD
C  40 ICHAR=ICHAR-1
C***** OLD
C***** TRKOV
   40 ICHR=ICHR-1
      IF(ICHR.LT.1) STOP 'SYMIN ERROR - No symbols loaded'
C***** TRKOV
   50 RETURN
   60 FORMAT(1X,A1,I5)
   70 FORMAT(2F7.3,I5)
      END
***** TRKOV
      SUBROUTINE PLOTH(X,Y,IPEN)                                        PLO45990
C                                                                       PLO46000
C     PLOT FROM LAST (X,Y) TO CURRENT (X,Y) EITHER NORMAL OR THICK LINE.PLO46010
C                                                                       PLO46020
      COMMON/THICKY/ITHICK,THICK                                        PLO46030
C-----IF MOVE, NOT DRAW OR NORMAL LINE DO IT WITH 1 STROKE.             PLO46040
      IF(IPEN.NE.2.OR.ITHICK.LE.0) GO TO 30                             PLO46050
      DXY=SQRT((X-XL)**2+(Y-YL)**2)                                     PLO46060
      IF(DXY.LE.0.0) GO TO 40                                           PLO46070
C-----DEFINE DIRECTION COSINE AND SINE.                                 PLO46080
      COST=(Y-YL)/DXY                                                   PLO46090
      SINT=(X-XL)/DXY                                                   PLO46100
C-----DEFINE OFFSET FOR LINE THICKNESS.                                 PLO46110
      DXT=-THICK*COST                                                   PLO46120
      DYT=THICK*SINT                                                    PLO46130
C-----DRAW THICK LINE.                                                  PLO46140
      CALL PLOT(X,Y,2)                                                  PLO46150
      DXT1=DXT                                                          PLO46160
      DYT1=DYT                                                          PLO46170
      DO 10 I=1,ITHICK                                                  PLO46180
      CALL PLOT(XL+DXT,YL+DYT,3)                                        PLO46190
      CALL PLOT(X+DXT,Y+DYT,2)                                          PLO46200
      DXT=DXT+DXT1                                                      PLO46210
   10 DYT=DYT+DYT1                                                      PLO46220
      DXT=DXT1                                                          PLO46230
      DYT=DYT1                                                          PLO46240
      DO 20 I=1,ITHICK                                                  PLO46250
      CALL PLOT(XL-DXT,YL-DYT,3)                                        PLO46260
      CALL PLOT(X-DXT,Y-DYT,2)                                          PLO46270
      DXT=DXT+DXT1                                                      PLO46280
   20 DYT=DYT+DYT1                                                      PLO46290
      CALL PLOT(X,Y,3)                                                  PLO46300
      GO TO 40                                                          PLO46310
C-----DRAW NORMAL WIDTH LINE.                                           PLO46320
   30 CALL PLOT(X,Y,IPEN)                                               PLO46330
   40 XL=X                                                              PLO46340
      YL=Y                                                              PLO46350
      RETURN                                                            PLO46360
      END                                                               PLO46370
C***** TRKOV
C***** TRKOV

