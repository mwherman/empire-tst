C     PROGRAM X4TOC4(INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,             
C    1 TAPE10,TAPE11,TAPE12,TAPE14,TAPE15)                              
C                                                                       
C                                                                       
C     PROGRAM X4TOC4                                                    
C     VERSION 86-1 (AUGUST 1986)                                        
C     VERSION 87-1 (JUNE 1987)    *IMPROVED BASED ON USER COMMENTS      
C     VERSION 93-1 (JANUARY 1993) *UPDATED TO ALWAYS OUTPUT YEAR IN     
C                                  STANDARD COLUMNS = THE LAST 4 COLUMNS
C                                  OF THE AUTHOR FIELD AS 2 INTEGERS    
C                                  ENCLOSED IN PARENTHESIS - THEREFORE  
C                                  THE 2 DIGIT YEAR WILL ALWAYS BE IN   
C                                  COLUMNS 120-121 AND THESE CAN BE USED
C                                  TO EASILY SORT DATA BY YEAR.         
C                                                                       
C     WRITTEN BY DERMOTT E. CULLEN                                      
C                UNIVERSITY OF CALIFORNIA                               
C                LAWRENCE LIVERMORE NATIONAL LABORATORY                 
C                L-298                                                  
C                P.O. BOX 808                                           
C                LIVERMORE, CA 94550                                    
C                U.S.A                                                  
C     TELEPHONE  510-423-7359                                           
C                                                                       
C     PURPOSE                                                           
C     =======                                                           
C     THIS PROGRAM IS DESIGNED TO TRANSLATE EXPERIMENTAL DATA FROM THE  
C     EXFOR FORMAT TO A COMPUTATION FORMAT.                             
C                                                                       
C     WHAT COMPUTERS WILL THE PROGRAM RUN ON                            
C     ======================================                            
C     THE PROGRAM HAS BEEN IMPLEMENTED ON AN IBM MAINFRAME AND AN IBM   
C     PERSONAL COMPUTER. THE PROGRAM IS SMALL ENOUGH TO RUN ON VIRTUALLY
C     ANY COMPUTER. FOR SPECIAL CONSIDERATIONS SEE THE SECTION BELOW ON 
C     COMPUTER DEPENDENT CODING.                                        
C                                                                       
C     EXFOR FORMAT                                                      
C     ============                                                      
C     THE EXFOR FORMAT IS DESIGNED TO ALLOW EXPERIMENTALLY MEASURED DATA
C     TO BE CODED IN A COMPUTER READABLE FORMAT IN A VERY FLEXIBLE FORM.
C     IN PARTICULAR THE DATA CAN BE ENTERED IN ESSENTIALLY ANY SET OF   
C     UNITS (E.G., EV VS. BARNS OR KEV VS. MILLI-BARNS) AND IN ANY TABLE
C     FORMAT; ESSENTIALLY THE TABLE MAY BE ENTERED EXACTLY AS PUBLISHED 
C     BY AN AUTHOR (E.G., ENERGY FOLLOWED BY COLUMNS OF CROSS SECTIONS  
C     IN ANY ORDER).                                                    
C                                                                       
C     THE EXFOR FORMAT IS TABLE ORIENTED IN THE SENSE THAT DATA FROM A  
C     GIVEN MEASUREMENT ARE COLLECTED TOGETHER AND CAN BE PRESENTED IN  
C     A SINGLE, OR AS A SERIES OF TABLES.                               
C                                                                       
C     THE ADVANTAGE OF THE EXFOR FORMAT IS THAT SINCE DATA CAN BE CODED 
C     ESSENTIALLY AS PUBLISHED BY AN AUTHOR PROBLEMS OF UNIT CONVERSION 
C     AND RE-FORMATTING TABLES PRIOR TO CODING ARE AVOIDED AND THE      
C     AUTHOR CAN EASILY CHECK THE CODED DATA. THE RESULT IS A GREATLY   
C     IMPROVED RELIABILITY OF THE CODED DATA.                           
C                                                                       
C     THE DISADVANTAGE OF THE EXFOR FORMAT IS THAT SINCE PHYSICALLY     
C     COMPARABLE DATA FROM DIFFERENT MEASUREMENTS (E.G. FE-56 TOTAL     
C     CROSS SECTIONS) MAY BE GIVEN IN A VARIETY OF DIFFERENT UNITS AND  
C     FORMATS IT IS VERY DIFFICULT TO USE IN APPLICATIONS. IN ADDITION  
C     THE TABLE ORIENTED EXFOR SYSTEM MAKES IT DIFFICULT TO COLLECT     
C     TOGETHER PHYSICALLY COMPARABLE DATA FROM DIFFERENT MEASUREMENTS.  
C                                                                       
C     COMPUTATION FORMAT                                                
C     ==================                                                
C     THE COMPUTATION FORMAT USED BY THIS PROGRAM IS DESIGNED TO PRESENT
C     EXPERIMENTAL DATA IN A FIXED SET OF UNITS AND COLUMN ORDER. BY    
C     STARTING FROM DATA IN THE EXFOR FORMAT AND TRANSLATING DATA TO    
C     THE COMPUTATION FORMAT IT IS POSSIBLE TO COMBINE THE ADVANTAGES   
C     OF THE IMPROVED RELIABILITY OF THE DATA CODED IN THE EXFOR FORMAT 
C     WITH THE ADVANTAGES OF A FIXED UNIT AND COLUMN ORDER FORMAT FOR   
C     USE IN SUBSEQUENT APPLICATIONS.                                   
C                                                                       
C     IN ADDITION THE COMPUTATION FORMAT IS POINT ORIENTED (AS OPPOSED  
C     THE TABLE ORIENTED EXFOR FORMAT). EACH LINE OF THE COMPUTATION    
C     FORMAT REPRESENTS A SINGLE DATA POINT. THIS MAKES IT POSSIBLE TO  
C     SORT DATA IN THE COMPUTATION FORMAT INTO ANY DESIRED ORDER FOR USE
C     IN APPLICATION, E.G., SORT 26-FE-26 (N,2N) DATA FROM A NUMBER OF  
C     MEASUREMENTS TOGETHER INTO ENERGY ORDER TO SIMPLIFY COMPARISONS.  
C                                                                       
C     EXFOR VS. COMPUTATION FORMAT                                      
C     ============================                                      
C     THE COMPUTATION FORMAT IS NOT INTENTED AS A SUBSTITUTE FOR THE    
C     EXFOR FORMAT, RATHER THE TWO ARE COMPLEMENTARY. THE EXFOR FORMAT  
C     CONTAINS MUCH MORE INFORMATION THAN CAN BE INCLUDED IN COMPUTATION
C     FORMATS AND THIS INFORMATION SHOULD BE CONSULTED AND USED DURING  
C     EVALUATION. THE COMPUTATION FORMAT IS ONLY INTENDED TO SIMPLIFY   
C     USE OF THE DATA DURING EVALUATION, OR OTHER APPLICATIONS.         
C                                                                       
C     RELATIONSHIP TO ENDF/B                                            
C     ======================                                            
C     IT IS ASSUMED THAT ONE OF THE MAJOR USES OF THIS PROGRAM WILL BE  
C     TO PREPARE DATA FOR SUBSEQUENT USE IN EVALUATION AND/OR TO COMPARE
C     AVAILABLE EVALUATED AND EXPERIMENTAL DATA. AS SUCH THE COMPUTATION
C     FORMAT HAS BEEN DESIGNED TO ALLOW DATA TO BE REDUCED TO A FORM IN 
C     WHICH DATA ARE CLASSIFIED IN A MANNER SIMILAR TO ENDF/B DATA.     
C                                                                       
C     IN PARTICULAR THE EXFOR CLASSIFICATION OF DATA BY THE EXFOR       
C     KEYWORD REACTION (OR ISO-QUANT, ETC.) IS REPLACED BY CLASSIFYING  
C     THE DATA BY (1) PROJECTILE, (2) TARGET - ZA, (3) TYPE OF DATA -   
C     ENDF/B - MF, (4) REACTION - ENDF/B MT. IN ADDITION THE STANDARD   
C     UNITS USED BY THE TRANSLATION PROGRAM WERE SELECTED TO BE THE     
C     SAME AS THE UNITS USED BY ENDF/B (E.G., EV, BARNS, ETC.).         
C                                                                       
C     THE RESULT OF PUTTING DATA INTO THE COMPUTATION FORMAT IS THAT IT 
C     IS EASY TO DECIDE IF THE DATA IS COMPARABLE TO EVALUATED DATA     
C     (E.G. SAME ZA, MF, MT) AND ONCE IT IS DECIDED THAT DATA IS        
C     COMPARABLE, EVALUATION AND/OR COMPARISON IS SIMPLIFIED BECAUSE THE
C     DATA IS IN THE SAME UNITS AS ENDF/B (E.G., EV VS. BARNS).         
C                                                                       
C     EXTENSIONS OF ENDF/B CONVENTIONS                                  
C     ================================                                  
C     FOR ALL TYPES OF DATA WHICH ARE PHYSICALLY COMPARABLE TO DATA     
C     WHICH CAN BE INCLUDED IN THE ENDF/B DATA THIS PROGRAM USING THE   
C     ENDF/B DEFINITIONS OF (1) TYPE OF DATA - ENDF/B MF, (2) REACTION -
C     ENDF/B MT. FOR EXAMPLE ALL CROSS SECTIONS (MF=3), ANGULAR (MF=4)  
C     AND ENERGY (MF=5) AND DOUBLE DIFFERENTIAL (MF=6) DISTRIBUTIONS    
C     DATA ARE TRANSLATED TO MF =3 TO 6. SIMILARLY FOR SIMPLE REACTIONS 
C     SUCH AS TOTAL (MT=1), ELASTIC (MT=2), ETC. THE DATA ARE TRANSLATED
C     TO THE CORRESPONDING ENDF/B MT.                                   
C                                                                       
C     SINCE MANY TYPES OF DATA WHICH APPEAR IN EXFOR DO NOT HAVE A ONE  
C     TO ONE CORRESPONDENCE TO DATA WHICH APPEARS IN ENDF/B THE ENDF/B  
C     CLASSIFICATION OF TYPE OF DATA (MF) AND REACTION (MT) HAVE BEEN   
C     EXTENDED TO ALLOW ADDITIONAL TYPES OF DATA AND REACTIONS TO BE    
C     TRANSLATED (E.G., DEFINE MF NUMBERS FOR RATIOS, DEFINE MT NUMBERS 
C     FOR (N,NP)+(N,NA)).                                               
C                                                                       
C     THE ENDF/B MF IS A 2 DIGIT NUMBER AND THE MT IS A 3 DIGIT NUMBER. 
C     IN THE COMPUTATION FORMAT MF HAS BEEN EXTENDED TO 3 DIGITS AND THE
C     MT HAS BEEN EXPANDED TO 4 DIGITS. THESE EXTENSIONS ALLOW THE USER 
C     THE FLEXIBILITY TO TRANSLATE VIRTUALLY ANY EXFOR DATA TO A FIXED  
C     SET OF UNITS AND COLUMN ORDER FOR SUBSEQUENT USE IN APPLICATIONS. 
C                                                                       
C     SOME EXTENSIONS OF MF AND MT HAVE ALREADY BEEN ESTABLISHED (FOR,  
C     DETAILS SEE THE INPUT DICTIONARIES DESCRIBED BELOW) AND IF AT ALL 
C     POSSIBLE THESE CONVENTIONS SHOULD BE FOLLOWED BY THE USER. THE    
C     USER HAS THE FLEXIBILITY OF ESTABLISHING ANY CONVENTIONS THAT MAY 
C     BE REQUIRED TO MEET HIS OR HER NEEDS, BUT IN THIS CASE IT IS THE  
C     RESPONSIBILITY OF THE USER TO PROPERLY INTERPRET AND USE THE      
C     TRANSLATED DATA.                                                  
C                                                                       
C     DIRECT COMPARISON TO ENDF/B DATA                                  
C     ================================                                  
C     ALTHOUGH THE ENDF/B CLASSIFICATION SYSTEM OF MF AND MT IS USED FOR
C     TRANSLATION, GENERALLY VERY LITTLE OF THE EXFOR DATA IS DIRECTLY  
C     COMPARABLE TO ENDF/B DATA. GENERALLY CROSS SECTIONS (MF=3) ARE    
C     DIRECTLY COMPARABLE. HOWEVER, IT MUST BE REALIZED THAT ANGULAR    
C     (MF=4) AND ENERGY (MF=5) AND DOUBLE DIFFERENTIAL (MF=6) DATA ARE  
C     GIVEN IN ENDF/B IN A NORMALIZED (I.E., NORMALIZED TO UNITY WHEN   
C     INTERGRATED) FORM, WHEREAS DATA IN EXFOR ARE GENERALLY GIVEN IN AN
C     UNNORMALIZED FORM (E.G.,ANGULAR DISTRIBUTIONS IN BARNS/STERADIAN).
C                                                                       
C     AFTER THIS PROGRAM HAS BEEN USED TO TRANSLATE EXFOR DATA TO THE   
C     COMPUTATION FORMAT THE USER MAY MAKE ADDITIONAL DATA DIRECTLY     
C     COMPARABLE TO THE CORRESPONDING ENDF/B DATA BY EITHER,            
C     (1) NORMALIZING THE DATA IN THE COMPUTATION FORMAT, OR,           
C     (2) CONVERTING ENDF/B DATA TO UNNORMALIZED FORM.                  
C     THIS INVOLVES SELECTING AN INTGRATED CROSS SECTION AS A STANDARD  
C     TO USE FOR THE COMPARISON (E.G., FOR A 14.2 MEV ELASTIC ANGULAR   
C     DISTRIBUTION USE THE 14.2 MEV ENDF/B ELASTIC CROSS SECTION).      
C                                                                       
C     SINCE THE SELECTION OF A STANDARD TO USE FOR COMPARISON IN HIGHLY 
C     APPLICATION DEPENDENT IT HAS BEEN DECIDED THAT IT IS BETTER TO USE
C     THIS PROGRAM TO TRANSLATE DATA EXACTLY AS GIVEN IN EXFOR (EXCEPT  
C     FOR CONVERSION TO A STANDARD SET OF UNITS) AND TO ALLOW THE USER  
C     TO SUBSEQUENTLY SELECT A STANDARD FOR RENORMALIZATION.            
C                                                                       
C     CONTROL OF TRANSLATION                                            
C     ======================                                            
C     THE USER HAS COMPLETE CONTROL OVER WHAT DATA IS TRANSLATED, WHERE 
C     GIVEN TYPES OF DATA APPEAR IN THE COMPUTATION FORMAT AND THE UNITS
C     OF THE DATA IN THE COMPUTATION FORMAT.                            
C                                                                       
C     THIS IS ACCOMPLISHED BY USING THREE DICTIONARIES WHICH CONTROL    
C     THE TRANSLATION. ALL THREE OF THESE DICTIONARIES ARE DISTRIBUTED  
C     WITH THIS PROGRAM. EACH DICTIONARY IS A SIMPLE CARD IMAGE FILE    
C     WHICH MAY BE MODIFIED BY THE USER AT ANY TIME TO MEET SPECIFIC    
C     NEEDS. THE THREE DICTIONARIES ARE,                                
C                                                                       
C     (1) EXFOR REACTION - PROJECTILE, MF, MT EQUIVALENCE               
C         -----------------------------------------------               
C         THIS DICTIONARY TELLS THE PROGRAM FOR EACH EXFOR REACTION     
C         WHAT PROJECTILE, MF AND MT TO OUTPUT IN THE COMPUTATION FORMAT
C         (E.G.,(N,TOT) = NEUTRON, MF =3 (CROSS SECTION),MT =1 (TOTAL)).
C         IF A REACTION READ FROM THE EXFOR FORMAT IS NOT FOUND IN THIS 
C         DICTIONARY, OR THE ASSIGNED MF OR MT IS NOT POSITIVE THE EXFOR
C         DATA WILL SIMPLY BE SKIPPED AND NOT TRANSLATED. USING THIS    
C         DICTIONARY THE USER HAS CONTROL OVER WHICH DATA IS TRANSLATED 
C         AND WHAT MF AND MT ARE ASSIGNED TO EACH EXFOR REACTION.       
C                                                                       
C     (2) EXFOR COLUMN TITLE - COMPUTATION FORMAT OUTPUT FIELD          
C         ----------------------------------------------------          
C         ONCE THE EXFOR REACTION HAS BEEN TRANSLATED AND ASSIGNED AN   
C         EQUIVALENT MF AND MT THIS DICTIONARY TELLS THE PROGRAM WHERE  
C         TO PLACE EACH EXFOR COLUMN IN THE COMPUTATION FORMAT. THE     
C         ASSIGNED MF NUMBER CAN BE USED TO OUTPUT AN EXFOR COLUMN      
C         WITH THE SAME TITLE INTO DIFFERENT COLUMNS OF THE COMPUTATION 
C         FORMAT BASED ON DIFFERENT MF NUMBERS. FOR EXAMPLE, FOR CROSS  
C         SECTIONS (MF=3) THE USER MAY USE EN-MIN AND EN-MAX TO DEFINE  
C         AN AVERAGE INCIDENT ENERGY TO BE OUTPUT IN THE FIRST FIELD    
C         OF THE COMPUTATION FORMAT AND AN EQUIVALENT ENERGY UNCERTAINTY
C         IN THE SECOND FIELD OF THE COMPUTATION FORMAT. ALTERNATIVELY  
C         FOR RESONANCE INTEGRALS (MF=213) THE USER MAY DECIDE TO OUTPUT
C         EN-MIN AND EN-MAX IN THE FIRST TWO FIELDS OF THE COMPUTATION  
C         FORMAT TO DEFINE THE ENERGY RANGE OF THE RESONANCE INTEGRAL.  
C                                                                       
C         THERE ARE 8 OUTPUT FIELDS IN THE COMPUTATION FORMAT AND FOR   
C         ANY GIVEN MF NUMBER THE USER MAY OUTPUT ANY EXFOR COLUMN      
C         INTO ANY OF THESE FIELDS. ANY EXFOR TITLE WHICH IS NOT        
C         ASSIGNED TO AN OUTPUT FIELD 1 TO 8 WILL BE IGNORED AND NOT    
C         OUTPUT. THIS ALLOWS THE USER TO SELECTIVELY TRANSLATE PORTIONS
C         OF EXFOR DATA TABLES TO MEET ANY GIVEN NEED. FOR EXAMPLE, BY  
C         SIMPLY MODIFYING THIS DICTIONARY THE USER HAS CONTROL OVER    
C         WHETHER AN EXFOR COLUMN DATA-ERR3 IS TRANSLATED OR IGNORED,   
C         AND IF TRANSLATED THE USER HAS CONTROL OVER WHICH OF THE 8    
C         COMPUTATION FORMAT DATA FIELDS DATA-ERR3 WILL APPEAR IN.      
C                                                                       
C     (3) EXFOR COLUMN UNITS - COMPUTATION FORMAT UNITS                 
C         ---------------------------------------------                 
C         THIS DICTIONARY TELLS THE PROGRAM HOW TO CONVERT EACH EXFOR   
C         UNIT INTO STANDARD UNITS. AS DISTRIBUTED THIS DICTIONARY WILL 
C         CONVERT ALL EXFOR UNITS TO ENDF/B COMPATIBLE UNITS. HOWEVER,  
C         THE USER HAS THE OPTION TO CHANGE THIS DICTIONARY AT ANY TIME 
C         TO OBTAIN ANY OUTPUT UNITS TO MEET HIS OR HER NEEDS. FOR      
C         EXAMPLE IF THE USER WOULD LIKE OUTPUT IN MEV VS. MILLI-BARNS  
C         INSTEAD OF EV VS. BARNS IT IS MERELY NECESSARY TO MODIFY THIS 
C         DICTIONARY.                                                   
C                                                                       
C     OPERATIONS ON DATA                                                
C     ==================                                                
C     IN ADDITION TO THE INFORMATION DESCRIBED ABOVE EACH OF THE THREE  
C     DICTIONARIES ALLOWS THE USER TO SELECT FROM A MENU OF OPERATIONS  
C     WHICH MAY BE PERFORMED ON THE DATA (FOR A COMPLETE AND UP-TO-DATE 
C     LIST OF AVAILABLE OPERATIONS SEE THE DICTIONARIES). FOR EXAMPLE,  
C     THE REACTION DICTIONARY ALLOWS THE USER TO SPECIFY THAT LEGENDRE  
C     COEFFICENTS MAY BE RE-NORMALIZED, THE TITLE DICTIONARY ALLOWS THE 
C     USER TO SPECIFY THAT EN-MIN AND EN-MAX ARE TO BE CONVERTED TO AN  
C     AVERAGE ENERGY AND ASSOCIATED ENERGY UNCERTAINTY AND THE UNITS    
C     DICTIONARY ALLOWS THE USER TO SPECIFY THAT ANGLES SHOULD BE       
C     CONVERTED TO COSINES.                                             
C                                                                       
C     THESE OPERATIONS ARE COMPLETELY UNDER THE CONTROL OF THE USER AND 
C     BY SIMPLY MODIFYING THE DICTIONARIES THE USER CAN CONTROL WHETHER 
C     OR NOT EACH OPERATION IS PERFORMED (E.G., IF YOU WANT TO OUTPUT   
C     ANGLES INSTEAD OF COSINES MODIFY THE UNITS DICTIONARY BY REMOVING 
C     THE OPTION TO CONVERT FROM ANGLE TO COSINE FROM THE EXFOR UNITS   
C     ASEC, AMIN AND ADEG).                                             
C                                                                       
C     COMPUTATION FORMAT UNITS                                          
C     ========================                                          
C     AS DISTRIBUTED THE UNITS DICTIONARY WILL CONVERT ALL EXFOR UNITS  
C     TO ENDF/B UNITS,                                                  
C                                                                       
C     EV         = ENERGY                                               
C     BARNS      = CROSS SECTION                                        
C     STERADIANS = SOLID ANGLE                                          
C     SECONDS    = TIME                                                 
C     KELVIN     = TEMPERATURE                                          
C                                                                       
C     IF THE USER WOULD LIKE TO OBTAIN ANY OTHER OUTPUT UNITS IT IS     
C     MERELY NECESSARY TO MODIFY THE UNITS DICTIONARY (SEE UNITS        
C     DICTIONARY FOR DETAILS).                                          
C                                                                       
C     A LEARNING PROGRAM                                                
C     ==================                                                
C     AS DISTRIBUTED THE THREE TRANSLATION DICTIONARIES DO NOT CONTAIN  
C     DEFINITIONS OF HOW TO TRANSLATE ALL EXFOR REACTIONS, TITLES AND   
C     UNITS. AT THE PRESENT TIME THIS PROGRAM HAS ONLY BEEN USED TO     
C     TRANSLATE A SMALL PORTION OF THE DATA INCLUDED IN THE EXFOR SYSTEM
C     AND THE DICTIONARIES ONLY CONTAIN SUFFICIENT INFORMATION TO       
C     TRANSLATE THE EXFOR DATA WHICH HAS BEEN ENCOUNTERED TO DATE.      
C                                                                       
C     IT IS DIFFICULT AND DANGEROUS TO TRY TO DEFINE TRANSLATION RULES  
C     FOR ALL TYPES OF EXFOR DATA WITHOUT EXAMINING ACTUAL EXFOR DATA.  
C     THEREFORE ONLY WHEN A NEW REACTION, TITLE OR UNIT IS ENCOUNTERED  
C     DURING TRANSLATION WILL THE ACTUAL EXFOR DATA BE EXAMINED, A      
C     DECISION MADE AS TO HOW TO BEST TRANSLATE THE DATA AND THE        
C     DICTIONARIES UPDATED.                                             
C                                                                       
C     GENERALLY ONCE A GIVEN TYPE OF EXFOR DATA HAS BEEN ENCOUNTERED AND
C     THE DICTIONARIES UPDATED TO DEFINE HOW TO TRANSLATE THE DATA THE  
C     SAME RULES CAN BE USED TO TRANSLATE ALL SIMILAR DATA. THEREFORE   
C     OVER A PERIOD OF TIME USER EXPERIENCE WILL BE ACCUMULATED IN THE  
C     TRANSLATION DICTIONARIES AND THE PROGRAM WILL LEARN TO PROPERLY   
C     TRANSLATE MORE AND MORE TYPES OF EXFOR DATA.                      
C                                                                       
C     UNDEFINED EXFOR REACTIONS, TITLES AND UNITS                       
C     ===========================================                       
C     IN ORDER TO ASSIST THE USER TO DEFINE NEW TYPES OF EXFOR DATA AS  
C     THEY ARE ENCOUNTERED DURING TRANSLATION THE OUTPUT REPORT FROM    
C     THIS PROGRAM WILL INDICATE THE NUMBER OF EXFOR REACTIONS, TITLES  
C     AND UNITS WHICH HAVE BEEN ENCOUNTERED DURING TRANSLATION WHICH ARE
C     NOT DEFINED IN THE TRANSLATION DICTIONARIES. IN ADDITIONAL ALL    
C     UNDEFINED REACTIONS, TITLES AND UNITS WILL BE WRITTEN TO OUTPUT   
C     UNIT 4 (NEWX4).                                                   
C                                                                       
C     BASED ON COMPARISON TO THE REACTION, TITLE AND UNITS DICTIONARIES 
C     IF AN EXFOR REACTION, TITLE OR UNITS IS ENCOUNTERED DURING        
C     TRANSLATION THAT IS NOT DEFINED IN THE DICTIONARIES IT WILL BE    
C     WRITTEN TO UNIT 4 (NEWX4). THIS INFORMATION IS WRITTEN IN A FORM  
C     THAT CAN BE EASILY EDITED AND ADDED TO A TRANSLATION DICTIONARY.  
C     AFTER UPDATING THE DICTIONARIES IF THIS PROGRAM IS THEN RUN A     
C     SECOND TIME USING THE SAME EXFOR DATA ALL OF THE EXFOR DATA CAN   
C     BE TRANSLATED.                                                    
C                                                                       
C     COMPUTATION FORMAT                                                
C     ==================                                                
C     THE COMPUTATION FORMAT USES A CLASSIFICATION SYSTEM AND UNITS     
C     WHICH ARE COMPATIBLE WITH ENDF/B. DATA IS CLASSIFIED BY (1) ZA    
C     OF PROJECTILE, (2) ZA OF TARGET, (3) METASTABLE STATE OF TARGET,  
C     (4) MF - TYPE OF DATA, (5) MT - REACTION, (6) METASTABLE STATE    
C     OF RESIDUAL NUCLEUS. TO IDENTIFY THE SOURCE OF THE DATA THE FIRST 
C     AUTHOR AND YEAR AND THE EXFOR ACCESSION AND SUB-ACCESSION NUMBER  
C     ARE INCLUDED IN THE FORMAT. IN ADDITION FIELDS ARE ASSIGNED TO    
C     DEFINE THE STATUS OF THE EXFOR DATA (E.G., S = SUPERCEDED),       
C     WHETHER DATA IS IN THE LABORATORY OR CENTER-OF-MASS FRAME OF      
C     REFERENCE AND THE PHYSICAL SIGNIFICANCE OF THE LAST 2 OUTPUT      
C     FIELDS (LVL = LEVEL ENERGY, HL = HALF-LIFE). FINALLY THE FORMAT   
C     INCLUDES 8 FIELDS INTO WHICH THE USER MAY OUTPUT DATA (E.G., DATA,
C     INCIDENT ENERGY, COSINE, UNCERTAINTIES, ETC.).                    
C                                                                       
C     COLUMNS   DESCRIPTION                                             
C     -------   -----------                                             
C       1-  5   PROJECTILE ZA (E.G. NEUTRON =1, PROTON =1001)           
C               (DEFINED BY REACTION DICTIONARY)                        
C       6- 11   TARGET ZA (E.G. 26-FE-56 =  26056)                      
C               (DEFINED BY EXFOR REACTION)                             
C          12   TARGET METASTABLE STATE (E.G. 26-FE-56-M = M)           
C               (DEFINED BY EXFOR REACTION)                             
C      13- 15   MF (ENDF/B CONVENTIONS, PLUS ADDITIONS).                
C               (DEFINED BY REACTION DICTIONARY)                        
C      16- 19   MT (ENDF/B CONVENTIONS, PLUS ADDITIONS).                
C               (DEFINED BY REACTION DICTIONARY)                        
C          20   PRODUCT METASTABLE STATE (E.G. 26-FE-56-M = M)          
C               (DEFINED BY EXFOR REACTION)                             
C          21   EXFOR STATUS                                            
C               (DEFINED BY EXFOR KEYWORD STATUS)                       
C          22   CENTER-OF-MASS FLAG (C=CENTER-OF-MASS, BLANK=LAB)       
C               (DEFINED BY EXFOR TITLE DICTIONARY)                     
C      23- 94   8 DATA FIELDS (EACH IN E9.3 FORMAT). DEFINED BELOW.     
C               (DEFINED BY MF AND TITLE DICTIONARY)                    
C      95- 97   IDENTIFICATION OF DATA FIELDS 7 AND 8                   
C               (E.G., LVL=LEVEL, HL=HALF-LIFE.ETC.)                    
C               FOR A COMPLETE LIST OF CODES SEE TITLE DICTIONARY.      
C               (DEFINED BY MF AND TITLE DICTIONARY)                    
C      98-118   REFERENCE (FIRST AUTHOR)                                
C               (DEFINED BY EXFOR KEYWORDS TITLE AND REFERENCE)         
C     119-122   YEAR AS 2 DIGITS ENCLOSED IN PARENTHESIS, E.G.,         
C               1991 = (91)                                             
C     123-127   EXFOR ACCESSION NUMBER                                  
C               (DEFINED BY EXFOR FORMAT)                               
C     128-130   EXFOR SUB-ACCESSION NUMBER                              
C               (DEFINED BY EXFOR FORMAT)                               
C         131   MULTI-DIMENSION TABLE FLAG                              
C               (DEFINED BY EXFOR KEYWORD REACTION OR COMMON FIELDS)    
C                                                                       
C     PRECISION OF THE 8 DATA FIELDS                                    
C     ==============================                                    
C     IF WRITTEN IN NORMAL FORMAT E9.2 FORMAT THE OUTPUT FROM THIS      
C     THIS PROGRAM WOULD GIVE DATA TO ONLY 2 OR 3 DIGITS OF ACCURACY,   
C     DEPENDING ON THE COMPUTER USED (E.G., 0.23E+02 OR 2.34E+01), WHICH
C     IS NOT SUFFICIENT FOR MANY APPLICATIONS (E.G., ENERGY OF CROSS    
C     SECTION POINTS IN THE RESONANCE REGION).                          
C                                                                       
C     IN ORDER TO AVOID THIS PROBLEM THIS PROGRAM WILL OUTPUT DATA IN   
C     A SPECIAL, FORMAT COMPATIBLE FORMAT TO ALLOW UP TO 7 DIGITS OF    
C     ACCURACY (I.E.,MORE THAN THE FULL WORD ACCURACY OF IBM COMPUTERS).
C                                                                       
C     NUMBERS BETWEEN 0.01 AND LESS THAN 10 MILLION WILL BE OUTPUT IN F 
C     (RATHER THAN E FORMAT). FOR EXAMPLE, THE ENERGY 12.3456 KEV WILL  
C     BE OUTPUT AS 123456.0. NUMBERS LESS THAN 0.01 OR GREATER THAN     
C     10 MILLION WILL BE OUTPUT IN E FORMAT, BUT WITHOUT AS E AND AN    
C     EXPONENT OF 1 OR 2 DIGITS. FOR EXAMPLE 14.123 MEV WILL BE OUTPUT  
C     AS 1.4123+7                                                       
C                                                                       
C     THESE OUTPUT CONVENTIONS HAVE BEEN USED FOR MANY YEARS WITH ENDF/B
C     RELATED PROGRAMS AND HAVE BEEN PROVEN TO BE FORTRAN COMPATIBLE FOR
C     USE ON VIRTUALLY ANY COMPUTER. FOR EXAMPLE, ANY FORTRAN PROGRAM   
C     WHICH IS WRITTEN TO READ THIS DATA USING AN E9.2 FORMAT WILL READ 
C     THE DATA PROPERLY WHETHER THE DATA IS ACTUALLY IN E OR F FORMAT.  
C                                                                       
C     GENERALLY MAINTAINING HIGH PRECISION IN THE DATA IS MOST IMPORTANT
C     FOR THE INDEPENDENT VARIABLE, PARTICULARLY INCIDENT ENERGY. SINCE 
C     WE DO NOT EXPECT VERY NARROW RESONANCE STRUCTURE BELOW 0.01 EV OR 
C     ABOVE 10 MEV GENERALLY THESE OUTPUT CONVENTIONS WILL MAINTAIN THE 
C     ACCURACY OF THE EXFOR DATA TO MEET REQUIREMENTS.                  
C                                                                       
C     DEFINITION OF 8 COMPUTATION FORMAT DATA FIELDS                    
C     ==============================================                    
C     THE USER MAY USE THE TITLE DICTIONARY TO OUTPUT ANY EXFOR COLUMN  
C     INTO ANY COMPUTATION FORMAT DATA FIELD. AS DISTRIBUTED THE TITLE  
C     DICTIONARY CONTAINS A NUMBER OF CONVENTIONS WHICH IF AT ALL       
C     POSSIBLE SHOULD BE FOLLOWED BY THE USERS. THE GENERAL DEFINITIONS 
C     OF THE 8 COMPUTATION FORMAT DATA FIELDS ARE,                      
C                                                                       
C     DATA FIELD   DEFINITION                                           
C     ----------   ----------                                           
C       1          PROJECTILE INCIDENT ENERGY                           
C       2          PROJECTILE INCIDENT ENERGY UNCERTAINTY               
C       3          DATA (E.G., CROSS SECTION)                           
C       4          DATA UNCERTAINTY                                     
C       5          COSINE OR LEGENDRE ORDER                             
C       6          COSINE UNCERTAINTY                                   
C       7          IDENTIFIED BY COLUMNS 95-97 (E.G.,LEVEL E, HALF-LIFE)
C       8          IDENTIFIED BY COLUMNS 95-97 (E.G.,LEVEL E UNCERTANTY)
C                                                                       
C                                                                       
C     THE PHYSICAL SIGNIFICANCE OF EACH FIELD IS DEFINED BY THE ASSIGNED
C     MF NUMBER. FOR EXAMPLE, FOR MF =3 (CROSS SECTIONS), COLUMNS 1 AND 
C     2 CONTAIN THE INCIDENT PROJECTILE ENERGY AND ITS UNCERTAINTY IN   
C     EV, RESPECTIVELY AND COLUMNS 3 - 4 CONTAIN THE CROSS SECTION AND  
C     ITS UNCERTAINTY IN BARNS, RESPECTIVELY AND COLUMNS 7 AND 8 MAY    
C     CONTAIN A LEVEL ENERGY AND ITS UNCERTAINTY IN EV OR A HALF-LIFE   
C     AND ITS UNCERTAINTY IN SECONDS.                                   
C                                                                       
C     SPECIAL CONVENTIONS                                               
C     ===================                                               
C     THE ABOVE CONVENTIONS ARE APPROPRIATE FOR MOST TYPES OF DATA      
C     IN THE ENDF/B SYSTEM. IN ORDER TO ALLOW THIS PROGRAM TO TRANSLATE 
C     ADDITIONAL TYPES OF DATA THE FOLLOWING SPECIAL CONVENTIONS HAVE   
C     BEEN ADOPTED,                                                     
C                                                                       
C     CROSS SECTION RATIOS  - FIELD 5 = MT OF DENOMINATOR.              
C     (MF = 203)              FIELD 6 = ZA OF DENOMINATOR.              
C     RESONANCE INTEGRALS   - FIELD 1 = LOWER ENERGY LIMIT.             
C     (MF = 213)              FIELD 2 = UPPER ENERGY LIMIT.             
C     SPECTRUM AVERAGES     - FIELD 1 = LOWER ENERGY LIMIT.             
C     (MF = 223)              FIELD 2 = UPPER ENERGY LIMIT.             
C     FISSION YIELD DATA    - FIELD 5 = ZA OF FISSION FRAGMENT          
C     (MF = 801)              FIELD 6 = MASS OF FISSION FRAGMENT        
C     PRODUCTION            - FIELD 6 = ZA OF PRODUCT                   
C     (MT = 9000)                                                       
C                                                                       
C     REQUIRED DATA FIELDS                                              
C     ====================                                              
C     FOR VARIOUS TYPES OF DATA THE PROGRAM WILL CHECK IF ALL REQUIRED  
C     FIELDS ARE DEFINED AND NON-BLANK. IF THEY ARE NOT WARNING MESSAGES
C     WILL BE PRINTED. IF THE DATA FIELD (FIELD 3) IS NOT DEFINED OR    
C     BLANK THE DATA POINT WILL NOT BE OUTPUT. IF THE DATA FIELD IS NOT 
C     DEFINED THIS USUALLY INDICATES AN ERROR IN THE EXFOR DATA. BLANK  
C     DATA FIELDS ARE QUITE COMMON IN MULTI-DIMENSIONAL TABLES AND A    
C     WARNING MAY OR MAY NOT INDICATE AN ERROR (CHECK THE EXFOR DATA TO 
C     SEE IF IT IS CORRECT).                                            
C                                                                       
C     THE PROGRAM CONSIDERS THAT THE FOLLOWING FIELDS ARE REQUIRED,     
C                                                                       
C      MF (DATA TYPE)         DATA FIELD (X = REQUIRED)                 
C     =====================   =========================                 
C                              1  2  3  4  5  6  7  8                   
C     =====================   =========================                 
C       3 (CROSS SECTIONS)     X     X                                  
C       4 (ANGULAR DIST.)      X     X     X                            
C       5 (ENERGY DIST.)       X     X           X                      
C       6 (DOUBLE DIFF.)       X     X     X     X                      
C     154 (LEGENDRE COEFF.)    X     X     X                            
C     203 (RATIOS)             X     X     X  X                         
C     801 (YIELD DATA).        X     X     X  X                         
C                                                                       
C     (SEE, THE ABOVE DEFINITION OF THE 8 DATA FIELDS)                  
C                                                                       
C     MULTI-DIMENSIONAL TABLES                                          
C     ========================                                          
C     THE PROGRAM CAN TRANSLATE MULTI-DIMENSIONAL EXFOR TABLES FOR,     
C     (1) MULTIPLE REACTIONS FOLLOWING THE EXFOR KEYWORD REACTION       
C         (ISO-QUANT, ETC.) WITH EACH REACTION IDENTIFIED BY A CHARACTER
C         IN COLUMN 11.                                                 
C     (2) SINGLE REACTIONS WITH MULTIPLE COMMON FIELDS EACH IDENTIFIED  
C         BY A CHARACTER IN THE ELEVENTH COLUMN OF EACH FIELD.          
C     (3) THE OLD ISO-QUANT, ETC. CONVENTION OF REACTIONS SEPARATED BY  
C         COMMAS, E.G., ((90-TH-232,NG)/(29-CU-0,NG)),(29-CU-0,NG))     
C                                                                       
C     TRANSLATION OF EXFOR REACTIONS                                    
C     ==============================                                    
C     NOT ALL EXFOR REACTIONS (ISO-QUANT, ETC.) CAN BE TRANSLATED BY    
C     THIS PROGRAM. IN ORDER TO TRANSLATE EACH REACTION THE PROGRAM WILL
C     FIRST BREAK EACH REACTION INTO A SERIES OF SIMPLE REACTIONS AND   
C     REMOVE AND SAVE THE TARGET AND RESIDUAL ZA, E.G.,                 
C                                                                       
C     ((26-FE-56(N,G)26-FE-57-M1,,SIG)/(26-FE-56(N,G)26-FE-57-G,,SIG))  
C                                                                       
C     IS BROKEN DOWN TO DEFINE                                          
C                                                                       
C     ZA-TARGET = 26056 , ZA-RESIDUAL = 260571, REACTION = (N,G),SIG    
C     ZA-TARGET = 26056 , ZA-RESIDUAL = 260570, REACTION = (N,G),SIG    
C                                                                       
C     (NOTE, RESIDUAL METASTABLE STATE FLAGS. SEE EXPLANATION BELOW).   
C                                                                       
C     THE PROGRAM WILL THEN DEFINE AN EQUIVALENT MF, MT FOR EACH        
C     REACTION.                                                         
C                                                                       
C     THE PROGRAM WILL NEXT TRANSLATE THE FOLLOWING TYPES OF            
C     REACTIONS,                                                        
C     (1) SIMPLE REACTIONS                                              
C         (N,G),SIG                                                     
C     (2) EQUIVALENT REACTIONS                                          
C         ((N,G),SIG)=...ANYTHING ELSE....                              
C         AFTER DECODING THE FIRST SIMPLE REACTION THE PROGRAM ASSUMES  
C         THAT THE FIRST SIMPLE REACTION IS TRUELY EQUIVALENT TO THE    
C         REMAINDER OF THE REACTION AND DEFINES ZA, MF AND MT BASED ON  
C         THE FIRST SIMPLE REACTION.                                    
C     (3) SIMPLE RATIOS                                                 
C         ((N,G)M1/G,,SIG/RAT) OR ((N,G)M1,SIG)/((N,G)G,SIG)            
C     (4) COMPLEX REACTIONS - ALL WITH THE SAME EQUIVALENT ZA           
C         ((N,EL),WID,,G)*((N,G),WID)/((N,TOT),WID)                     
C     (5) OTHER REACTIONS                                               
C         (((N,G),SIG)/((N,G),SIG),(N,G),SIG))                          
C         IF THE REACTION IS NOT ONE OF THE ABOVE TYPES THE PROGRAM WILL
C         TRY TO USE THE ENTIRE EXFOR REACTION, INCLUDING TARGET AND    
C         RESIDUAL ZA AND SEE IF IT IS DEFINED IN REACTION EQUIVALENT   
C         DICTIONARY. IF AN MF, MT IS DEFINED FOR THE ENTIRE REACTION   
C         THE PROGRAM WILL USE THE TARGET AND RESIDUAL ZA FROM THE FIRST
C         SIMPLE REACTION TO TRANSLATE THE DATA. THIS LAST FORM MAY BE  
C         USED TO INSURE THAT ALMOST ALL EXFOR REACTION CAN BE          
C         TRANSLATED, REGARDLESS OF HOW COMPLICATED IT IS (FOR EXAMPLES 
C         SEE REACTION DICTIONARY) HOWEVER THE USER SHOULD CAREFULLY    
C         CHECK THE OUTPUT TO INSURE THAT THE DATA HAS BEEN TRANSLATED  
C         AS INTENDED.                                                  
C                                                                       
C     THE ONLY REACTIONS THAT HAVE SO FAR BEEN FOUND THAT CANNOT BE     
C     CORRECTLY TRANSLATED ARE RATIOS OF PRODUCTION CROSS SECTIONS,     
C     E.G., (29-CU-0(P,X)26-FE-56)/(28-NI-0(P,X)26-FE-58)               
C     BECAUSE RATIO DATA REQUIRES FIELDS 5 AND 6 FOR THE DENOMINATOR    
C     MT AND ZA AND RATIO DATA REQUIRES FIELD 5 FOR THE PRODUCT ZA.     
C     WHEN THIS CASE IS ENCOUNTERED THE PROGRAM WILL PRINT AN ERROR     
C     MESSAGE AND OUTPUT THE DENOMINATOR MT AND ZA IN FIELDS 5 AND 6.   
C     IN THIS CASE THE OUTPUT WILL IDENTIFY THE NUMERATOR AS ZA=29000,  
C     MT=9001 AND THE DENOMINATOR AS ZA=28000, MT=9001. ONE SOLUTION    
C     IS TO MODIFY THE OUTPUT OF THIS PROGRAM BY DEFINING TWO REACTIONS,
C     E.G., MT = 8001 = (P,X) 26-FE-56 AND MT = 8002 = (P,X) 26-FE-58,  
C     MODIFY THE NUMERATOR MT TO 8001 AND DENOMINATOR MT TO 8002 AND    
C     THEN PROPERLY INTERPRETING THE DATA USING THESE DEFINITION IN ALL 
C     APPLICATIONS (FOR EXAMPLES, SEE PROGRAM PLOTC4 INPUT DIRECTIONARY 
C     FOR PROTON INDUCED REACTIONS).                                    
C                                                                       
C     OUTPUT REPORT                                                     
C     =============                                                     
C     THIS PROGRAM WILL WRITE A REPORT ON UNIT 6 (OUTP) TO ALLOW THE    
C     USER TO MONITOR THE TRANSLATION OF THE EXFOR DATA. IT IS EXTREMELY
C     IMPORTANT THAT THE USER READ THIS REPORT AND NOT SIMPLY ASSUME    
C     THAT ALL OF THE DATA HAS BEEN PROPERLY TRANSLATED.                
C                                                                       
C     AFTER IDENTIFYING EACH EXFOR ACCESSION, SUB-ACCESSION NUMBER,     
C     ZA, MF, MT AND REACTION THE PROGRAM CAN PRINT TWO TYPES OF        
C     MESSAGES,                                                         
C                                                                       
C     WARNING    = SOMETHING UNUSUAL HAS OCCURRED. THE USER SHOULD      
C                  CAREFULLY CHECK TO INSURE THAT THE OUTPUT DATA HAS   
C                  BEEN PROPERLY TRANSLATED.                            
C     OPERATION  = ONE OF THE DEFINED REACTION, TITLE OR UNIT OPERATIONS
C                  HAS BEEN PERFORMED ON THE DATA. THE USER SHOULD      
C                  CAREFULLY CHECK TO INSURE THAT THE PROPER OPERATION  
C                  HAS BEEN PERFORMED.                                  
C                                                                       
C     IF THE USER DOES NOT AGREE WITH HOW THE DATA HAS BEEN TRANSLATED  
C     THE THREE DICTIONARIES MAY TO BE MODIFIED AND THE PROGRAM RE-RUN. 
C     FOR EXAMPLE, IF THE PROGRAM PRINTS A WARNING THAT THE TITLE       
C     DICTIONARY TELLS IT TO OUTPUT E-ERR1, E-ERR2, E-ERR3 ALL IN THE   
C     SAME COMPUTATION FORMAT FIELD, FOLLOWED BY AN OPERATION THAT SAYS 
C     THE PROGRAM WILL ONLY OUTPUT E-ERR1 AND IGNOR THE OTHER 2 EXFOR   
C     FIELDS, IF THE USER WOULD RATHER OUTPUT E-ERR2 AND IGNOR E-ERR1   
C     AND E-ERR3 IT IS MERELY NECESSARY TO MODIFY THE TITLE DICTIONARY  
C     TO IGNOR E-ERR1 AND E-ERR3 AND SELECT E-ERR2 AND THEN RE-RUN THE  
C     PROGRAM.                                                          
C                                                                       
C     METASTABLE STATE                                                  
C     ================                                                  
C     THE COMPUTATION FORMAT ALLOWS THE METASTABLE STATE OF THE TARGET  
C     AND RESIDUAL NUCLEUS TO BE IDENTIFIED. FOR RATIO DATA METASTABLE  
C     STATE OF BOTH NUMERATOR AND DENOMINATOR OF THE RATIO MAY BE       
C     DEFINED.                                                          
C                                                                       
C     THE METASTABLE STATE OF THE TARGET IS IDENTIFIED IN COLUMN 12 AND 
C     THE METASTABLE STATE OF THE RESIDUAL NUCLUES IN COLUMN 20. FOR    
C     RATIO DATA THE METASTABLE STATE OF THE DENOMINATOR TARGET AND     
C     RESIDUAL NUCLEUS ARE IDENTIFIED BY OUTPUT THE DENOMINATOR ZA AND  
C     MT IN THE FORM ZA.M AND MT.M (E.G., 26056.9 AND 102.1). COLUMNS   
C     12 AND 20 MAY CONTAIN CHARACTERS SUCH AS M, BUT TO MAINTAIN THE   
C     EIGHT OUTPUT FIELDS IN STRICTLY NUMERICAL FORM THE DENOMINATOR    
C     ZA.M AND MT.M WILL BE OUTPUT IN NUMERICAL FORM. THE POSSIBLE      
C     CHARACTERS THAT MAY APPEAR IN COLUMNS 12 OR 20 AND THEIR NUMERICAL
C     EQUIVALENTS USED WITH RATIO DENOMINATOR ZA AND MT INCLUDE,        
C                                                                       
C     DEFINITION    COLUMN 12 OR 20     EQUIVALENT                      
C     ==========    ===============     ==========                      
C     GROUND              G                0                            
C     M1                  1                1                            
C     M2                  2                2                            
C     M3                  3                3                            
C     M4                  4                4                            
C     M5                  5                5                            
C     UNKNOWN             ?                6                            
C     M                   M                7                            
C     MORE THAN 1         +                8                            
C     ALL OR TOTAL        T                9                            
C     ALL OR TOTAL      BLANK              9                            
C                                                                       
C     BY CONVENTION IF AN EXFOR REACTION DOES NOT SPECIFY A METASTABLE  
C     STATE THE STATE IS DEFINED IN THE COMPUTATION FORMAT TO BE..ALL.. 
C     (I.E., BLANK IN COLUMN 12 OR 20, 9 IN RATIO ZA OR MT).            
C                                                                       
C     FOR EXAMPLE, FOR A RATIO IF THE ZA.M AND MT.M ARE OUTPUT AS       
C     26056.9 AND 102.1, RESPECTIVELY THE RATIO DENOMINATOR TARGET WAS  
C     26-FE-56 (ALL) AND THE REACTION WAS CAPTURE (MT=102) LEAVING THE  
C     RESIDUAL NUCLEUS IN THE M1 STATE.                                 
C                                                                       
C     NOTE, SINCE MOST DATA WILL NOT CONTAIN A METASTABLE STATE FLAG    
C     THE ABOVE CONVENTION TO OUTPUT THE ZA AND MT OF THE DENOMINATOR   
C     OF RATIOS ALLOWS THE USER TO READ AND USE THE DENOMINATOR ZA AND  
C     MT AS INTEGERS (EFFECTIVELY IGNORING ANY METASTABLE STATE FLAG) OR
C     IF NECESSARY TO DETERMINE THE METASTABLE STATE.                   
C                                                                       
C     EXFOR STATUS                                                      
C     ============                                                      
C     COLUMN 21 OF EACH COMPUTATION FORMAT RECORD MAY CONTAIN BLANK     
C     (STATUS NOT SPECIFIED) OR ONE TO THE FOLLOWING CHARACTERS,        
C                                                                       
C     COLUMN 21   DEFINITION                                            
C     ---------   ----------                                            
C        U        UNNORMALIZED (INDICATED BY UNIT TRANSLATION DICTIONARY
C                 THIS CONDITION HAS PRIORITY OVER THE EXFOR STATUS AND 
C                 IS USED TO INDICATE THAT THE DATA IS NOT IN STANDARD  
C                 OUTPUT UNITS).                                        
C        A        APPROVED BY AUTHOR                                    
C        C        CORRELATED                                            
C        D        DEPENDENT                                             
C        O        OUTDATED                                              
C        P        PRELIMINARY                                           
C        R        RENORMALIZED                                          
C        S        SUPERCEDED                                            
C                                                                       
C     IF DATA HAS ANY OTHER EXFOR STATUS (E.G., TRANSLATED FROM SCISRS) 
C     IT WILL BE IGNORED AND THE STATUS FIELD WILL BE OUTPUT AS BLANK.  
C                                                                       
C     INPUT FILES                                                       
C     ===========                                                       
C     UNIT  DESCRIPTION                                                 
C     ====  ===========                                                 
C      10   EXFOR DATA (TO BE TRANSLATED) (BCD - 80 CHARACTERS/RECORD)  
C      12   EXFOR REACTION DICTIONARY     (BCD - 80 CHARACTERS/RECORD)  
C      14   EXFOR TITLE DICTIONARY        (BCD - 80 CHARACTERS/RECORD)  
C      15   EXFOR UNITS DICTIONARY        (BCD - 80 CHARACTERS/RECORD)  
C                                                                       
C     OUTPUT FILES                                                      
C     ============                                                      
C     UNIT  DESCRIPTION                                                 
C     ====  ===========                                                 
C       4   LIST OF ALL UNDEFINED EXFOR REACTIONS, TITLES AND UNITS     
C           FOUND DURING THE TRANSLATION (IF ANY).(BCD -  80 CHARACTERS)
C       6   OUTPUT REPORT                         (BCD - 132 CHARACTERS)
C      11   COMPUTATION FORMAT EXPERIMENTAL DATA  (BCD - 131 CHARACTERS)
C                                                                       
C     SCRATCH FILES                                                     
C     =============                                                     
C     NONE                                                              
C                                                                       
C     INPUT PARAMETERS                                                  
C     ================                                                  
C     NONE                                                              
C                                                                       
C     REPORTING ERRORS                                                  
C     ================                                                  
C     IN ORDER TO IMPROVE THIS CODE AND MAKE FUTURE VERSIONS MORE       
C     COMPATIBLE FOR USE ON AS MANY DIFFERENT TYPES OF COMPUTERS AS     
C     POSSIBLE PLEASE REPORT ALL COMPILER DIAGNOSTICS AND/OR OPERATING  
C     PROBLEMS TO THE AUTHOR AT THE ABOVE ADDRESS.                      
C                                                                       
C     PLEASE REMEMBER IF YOU SIMPLY REPORT 'I'VE GOT A PROBLEM' AND DO  
C     NOT ADEQUATELY DESCRIBE EXACTLY HOW YOU WERE USING THE PROGRAM    
C     IT WILL BE IMPOSSIBLE FOR THE AUTHOR TO HELP YOU. WHEN A PROBLEM  
C     ARISES PLEASE WRITE TO THE AUTHOR, DESCRIBE THE PROBLEM IN AS MUCH
C     DETAIL AS POSSIBLE, IDENTIFY THE VERSION OF THE PROGRAM THAT YOU  
C     ARE USING (E.G. VERSION 93-1) AND SEND THE FOLLOWING INFORMATION  
C     ON MAGNETIC TAPE TO THE AUTHOR,                                   
C                                                                       
C     (1) A COPY OF THE PROGRAM YOU ARE USING                           
C     (2) A COPY OF COMPILER DIAGNOSTICS (IF ANY)                       
C     (3) A COPY OF THE JCL DECK YOU USED TO EXECUTE THE PROGRAM        
C     (4) A COPY OF THE 3 TRANSLATION DICTIONARIES YOU ARE USING        
C     (5) A COPY OF THE EXFOR FORMAT DATA YOU USING                     
C     (6) A COPY OF THE COMPUTATION FORMAT DATA YOU USING               
C     (7) A COPY OF THE OUTPUT REPORT FROM THE PROGRAM                  
C                                                                       
C     WITHOUT ALL OF THIS INFORMATION IT IS IMPOSSIBLE TO EXACTLY       
C     SIMULATE THE PROBLEM THAT YOU RAN AND TO DETERMINE THE SOURCE     
C     OF YOUR PROBLEM.                                                  
C                                                                       
C***** COMPUTER DEPENDENT CODING ******                                 
C                                                                       
C   * CHARACTER/INTEGER                                                 
C     THIS PROGRAM CAN TREAT CHARACTERS EITHER AS CHARACTERS (FORTRAN-77
C     CONVENTION) OR INTEGERS (FORTRAN-H) CONVENTION.                   
C                                                                       
C     PROGRAM CONVERT CAN BE USED TO AUTOMATICALLY CONVERT THIS PROGRAM 
C     BACK AND FORTH BETWEEN THESE CONVENTIONS.                         
C                                                                       
C***** COMPUTER DEPENDENT CODING ******                                 
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 CARD1,CARD2,KEYTAB,ENDSUB                             
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER CARD1,CARD2,KEYTAB,ENDSUB                                 
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/CARDS/CARD1(11,6),CARD2(14)                                
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  
      COMMON/POINTR/MPOINT(9)                                           
      DIMENSION ENDSUB(11),KEYTAB(11,5)                                 
      DATA KEYTAB/                                                      
     1 'S','U','B','E','N','T',' ',' ',' ',' ',' ',                     
     1 'N','O','S','U','B','E','N','T',' ',' ',' ',                     
     1 'B','I','B',' ',' ',' ',' ',' ',' ',' ',' ',                     
     1 'C','O','M','M','O','N',' ',' ',' ',' ',' ',                     
     1 'D','A','T','A',' ',' ',' ',' ',' ',' ',' '/                     
      DATA ENDSUB/'E','N','D','S','U','B','E','N','T',' ',' '/          
C-----DEFINE ALL I/O UNITS.                                             
      NEWX4=4                                                           
      INP=5                                                             
      OUTP=6                                                            
      ITAPE=10                                                          
      OTAPE=11                                                          
      NTAPE1=12                                                         
      NTAPE2=14                                                         
      NTAPE3=15                                                         
C-----OPTIONALLY DEFINE FILENAMES AND REWIND TO START.                  
      CALL FILEIO                                                       
C-----INITIALIZE COUNTS.                                                
      DO 10 I=1,9                                                       
   10 MPOINT(I)=0                                                       
C-----PRINT TITLE FOR OUTPUT.                                           
      WRITE(OUTP,6000)                                                  
C-----READ REACTION VS. MF/MT TABLE                                     
      CALL MFMTIN                                                       
C-----READ COLUMN HEADINGS VS. MF/FIELDS.                               
      CALL TITLEI                                                       
C-----READ UNITS AND CONVERSION FACTORS TO STANDARD UNITS.              
      CALL UNITI                                                        
      WRITE(OUTP,6030)                                                  
C                                                                       
C     READ EXFOR CARDS AND PROCESS SUBENT, BIB, COMMON OR DATA.         
C                                                                       
   20 READ(ITAPE,1000,END=100,ERR=120) CARD1,CARD2                      
      DO 40 INKEY=1,5                                                   
      DO 30 J=1,11                                                      
      IF(CARD1(J,1).NE.KEYTAB(J,INKEY)) GO TO 40                        
   30 CONTINUE                                                          
      GO TO 50                                                          
   40 CONTINUE                                                          
      GO TO 20                                                          
C-----PROCESS SUBENT CARD.                                              
   50 IF(INKEY.GT.2) GO TO 60                                           
      CALL SUBIN                                                        
      GO TO 20                                                          
C-----TRANSLATE N1, N2 FOR BIB, COMMON OR DATA.                         
   60 CALL INTGER(CARD1(1,2),N1,11)                                     
      CALL INTGER(CARD1(1,3),N2,11)                                     
      IF(INKEY.NE.3) GO TO 90                                           
      CALL BIBIN                                                        
C-----IF SAN>1 AND NO REACTIONS TRANSLATED SKIP SUBENTRY.               
      IF(ISAN.LE.1) GO TO 20                                            
      IF(KSANR.GT.0) GO TO 20                                           
      MPOINT(2)=MPOINT(2)+1                                             
   70 READ(ITAPE,1000,END=100,ERR=120) CARD1,CARD2                      
      DO 80 I=1,11                                                      
      IF(CARD1(I,1).NE.ENDSUB(I)) GO TO 70                              
   80 CONTINUE                                                          
      GO TO 20                                                          
C-----PROCESS COMMON OR DATA.                                           
   90 IF(INKEY.EQ.4) CALL COMIN                                         
      IF(INKEY.EQ.5) CALL DATIN                                         
      GO TO 20                                                          
C-----END OF RUN. PRINT SUMMARY OF TRANSLATION.                         
  100 WRITE(OUTP,6010) MPOINT                                           
  110 END FILE OTAPE                                                    
      END FILE NEWX4                                                    
      CALL GOOUT                                                        
C-----ERROR READING EXFOR DATA.                                         
  120 WRITE(OUTP,6020)                                                  
      GO TO 110                                                         
 1000 FORMAT(80A1)                                                      
 6000 FORMAT(' TRANSLATE DATA FROM EXFOR TO COMPUTATION FORMAT',        
     1 ' (X4TOC4 VERSION 93-1)'/1X,70('=')/                             
     2 ' READING TRANSLATION TABLES'/1X,70('='))                        
 6010 FORMAT(1X,70('=')/' TRANSLATION SUMMARY'/1X,70('=')/              
     1 ' SUBENTRIES TRANSLATED--------',I7/                             
     2 ' SUBENTRIES SKIPPED-----------',I7,' (NO OUTPUT)'/              
     3 ' POINTS READ------------------',I7/                             
     4 ' POINTS TRANSLATED------------',I7/                             
     5 ' DATA FIELDS NOT DEFINED------',I7,' (NO OUTPUT)'/              
     6 ' DATA FIELDS BLANK------------',I7,' (NO OUTPUT)'/              
     7 ' UNDEFINED REACTIONS----------',I7/                             
     8 ' UNDEFINED TITLES-------------',I7/                             
     9 ' UNDEFINED UNITS--------------',I7/1X,70('='))                  
 6020 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     
 6030 FORMAT(1X,70('=')/'    AN SAN PROJECT  TARGET RESIDUAL',          
     1 '  MF   MT REACTION'/                                            
     1 1X,70('='))                                                      
      END                                                               
      SUBROUTINE SUBIN                                                  
C                                                                       
C     SUBENT OR NOSUBENT CARD. INITIALIZE COUNTERS AND ARRAYS.          
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 CARD1,CARD2,ENT,SUBENT,AUTH1,AUTHN,REFER1,REFERN,     
     1 STAT1,STATN,BLANK                                                
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER CARD1,CARD2,ENT,SUBENT,AUTH1,AUTHN,REFER1,REFERN,         
C    1 STAT1,STATN,BLANK                                                
C***** INTEGER *****                                                    
      COMMON/CARDS/CARD1(11,6),CARD2(14)                                
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/WHERE/ENT(5),SUBENT(3)                                     
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  
      COMMON/AUTHI/IAUTH,NAUTH                                          
      COMMON/AUTHC/AUTH1(25),AUTHN(25)                                  
      COMMON/REFERI/IREF,NREF                                           
      COMMON/REFERC/REFER1(4),REFERN(4)                                 
      COMMON/STATUC/STAT1,STATN                                         
      DATA BLANK/' '/                                                   
CGCP-ENEA-BO-930210                                                     
C-----STORE AN AND SAN INTO CARD2 FROM SUBENTRY OPERATOR FIELD          
C                                                                       
      DO I=1,8                                                          
      CARD2(I)=CARD1(I+3,2)                                             
      ENDDO                                                             
      DO I=9,14                                                         
      CARD2(I)=BLANK                                                    
      ENDDO                                                             
C                                                                       
C     CHECK FOR NEW ENTRY OR SUBENTRY 1.                                
C                                                                       
      IRESET=0                                                          
C-----SAVE ENTRY NUMBER.                                                
      DO 10 I=1,5                                                       
      IF(ENT(I).NE.CARD2(I)) IRESET=1                                   
   10 ENT(I)=CARD2(I)                                                   
C-----SAVE SUBENTRY.                                                    
      DO 20 I=1,3                                                       
   20 SUBENT(I)=CARD2(I+5)                                              
C-----CONVERT SUBENTRY NUMBER TO INTEGER.                               
      CALL INTGER(CARD2(6),ISAN,3)                                      
      IF(ISAN.EQ.1) IRESET=1                                            
C                                                                       
C     RESET COUNTS AND ARRAYS.                                          
C                                                                       
      IF(IRESET.EQ.0) GO TO 50                                          
C-----NEW ENTRY OR SUBENTRY 1. RESET COMMON SUBENT COUNTS AND ARRAYS.   
      ICOM1=0                                                           
      KSAN1=0                                                           
      STAT1=BLANK                                                       
      IAUTH=0                                                           
      DO 30 I=1,25                                                      
   30 AUTH1(I)=BLANK                                                    
      IREF=0                                                            
      DO 40 I=1,4                                                       
   40 REFER1(I)=BLANK                                                   
C-----RESET ORDINARY SUBENT COUNTS AND ARRAYS.                          
   50 ICOMN=ICOM1                                                       
      IDATN=0                                                           
      KSANR=KSAN1                                                       
      STATN=BLANK                                                       
      NAUTH=0                                                           
      DO 60 I=1,25                                                      
   60 AUTHN(I)=BLANK                                                    
      NREF=0                                                            
      DO 70 I=1,4                                                       
   70 REFERN(I)=BLANK                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE BIBIN                                                  
C                                                                       
C     BIB CARD READ. PROCESS ENTIRE BIB SECTION.                        
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 BIBKEY,KEYWD1                                         
      CHARACTER*1 BIB1,BIB2,KEYWD2                                      
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER BIBKEY,KEYWD1,BIB1,BIB2,KEYWD2                            
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/BIBCRD/KEYWD1(3),KEYWD2,BIB1(55),BIB2(14)                  
      DIMENSION BIBKEY(3,9)                                             
      DATA BIBKEY/                                                      
     1 'ENDB','IB  ','  ',                                              
     2 'REAC','TION','  ',                                              
     3 'ISO-','QUAN','T ',                                              
     4 'NUC-','QUAN','T ',                                              
     5 'CMPD','-QUA','NT',                                              
     6 'STAT','US  ','  ',                                              
     7 'REFE','RENC','E ',                                              
     8 'AUTH','OR  ','  ',                                              
     9 'TITL','E   ','  '/                                              
C-----READ ALL BIB CARDS AND LIST REQUIRED KEYWORDS AND CONTINUATIONS.  
   10 READ(ITAPE,1000,END=70,ERR=70) KEYWD1,KEYWD2,BIB1,BIB2            
   20 DO 40 K=1,9                                                       
      DO 30 I=1,3                                                       
      IF(KEYWD1(I).NE.BIBKEY(I,K)) GO TO 40                             
   30 CONTINUE                                                          
C-----REQUIRED KEYWORD FOUND.                                           
      GO TO 50                                                          
   40 CONTINUE                                                          
C-----REQUIRED KEYWORD NOT FOUND. CONTINUE READING.                     
      GO TO 10                                                          
C-----RETURN ON ENDBIB. OTHERWISE PROCESS.                              
   50 IF(K.EQ.1) GO TO 80                                               
C-----PROCESS ISO-QUNT, CMP-QUANT, NUC-QUANT OR REACTION.               
      IF(K.GT.5) GO TO 60                                               
      CALL REACTN(K-1)                                                  
      GO TO 20                                                          
C-----PROCESS STATUS.                                                   
   60 IF(K.EQ.6) CALL STATUS                                            
C-----PROCESS REFERENCE.                                                
      IF(K.EQ.7) CALL REFERS                                            
C-----PROCESS AUTHOR.                                                   
      IF(K.EQ.8) CALL AUTHOR                                            
      GO TO 10                                                          
C-----ERROR READING EXFOR DATA.                                         
   70 WRITE(OUTP,6000)                                                  
      CALL GOOUT                                                        
C-----END OF BIB SECTION.                                               
   80 RETURN                                                            
 1000 FORMAT(2A4,A2,A1,55A1,14A1)                                       
 6000 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     
      END                                                               
      SUBROUTINE REACTN(KTYPE)                                          
C                                                                       
C     TRANSLATE EACH REACTION (UP TO 30) SEPERATELY.                    
C                                                                       
C     ZAR1   = ENTIRE REACTION (BETWEEN BALANCED PARENTHESIS...OUTSIDE  
C              PARENTHESIS REMOVED...PRINTED IF REACTION CANNOT BE      
C              DECODED).                                                
C     ZARBAK = BAKCUP COPY OF ZAR1 (USED FOR COMPLEX REACTIONS).        
C     RN     = COMPLEX REACTION WITH ZA REMOVED.                        
C     R1     = SIMPLE REACTION WITH ZA REMOVED.                         
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 BLANK,PARENL,PARENR,ZAR1,ZAN,ZA1,R1,RN,FLAGR,BIB1,    
     1 BIB2,ENT,SUBENT,ZARBAK,KEYWD2,LABCM,ZARES,ZANRES,ZANRAT,SLASH,   
     2 EQUAL,MRAT,COMMA,ZASAVE                                          
      CHARACTER*4 KEYWD1,BLANK4                                         
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER BLANK,PARENL,PARENR,ZAR1,ZAN,ZA1,R1,RN,FLAGR,BIB1,        
C    1 BIB2,ENT,SUBENT,ZARBAK,KEYWD2,LABCM,ZARES,ZANRES,ZANRAT,SLASH,   
C    2 EQUAL,MRAT,COMMA,ZASAVE,                                         
C    3 KEYWD1,BLANK4                                                    
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/BIBCRD/KEYWD1(3),KEYWD2,BIB1(55),BIB2(14)                  
      COMMON/WHERE/ENT(5),SUBENT(3)                                     
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/ZART1I/KZAR1                                               
      COMMON/ZART1C/ZAR1(300)                                           
      COMMON/ZAT1I/KZA1,KR1                                             
      COMMON/ZAT1C/ZA1(7),R1(300)                                       
      COMMON/RATMET/MRAT                                                
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     
     1 LABCM(30)                                                        
      COMMON/ZATNC2/RN(300)                                             
      COMMON/POINTR/MPOINT(9)                                           
      COMMON/RESIDI/KZARES                                              
      COMMON/RESIDC/ZARES(7)                                            
      DIMENSION ZARBAK(300),ZASAVE(300,10),NSAVE(10)                    
      DATA BLANK4/'    '/                                               
      DATA BLANK/' '/                                                   
      DATA PARENL/'('/                                                  
      DATA PARENR/')'/                                                  
      DATA SLASH/'/'/                                                   
      DATA COMMA/','/                                                   
      DATA EQUAL/'='/                                                   
C-----INITIALIZE REACTION COUNT AND SAVED REACTION FLAG.                
      KSANR=KSAN1                                                       
      ISAVE=0                                                           
      KSAVE=1                                                           
C                                                                       
C     START OF NEW REACTION. COPY ENTIRE REACTION INTO ZAR1 (REACTION   
C     MAY BE CONTINUED ONTO MULTIPLE CARDS).                            
C                                                                       
C-----FIRST CARD HAS ALREADY BEEN READ. BRANCH TO TEST FOR MACHINE      
C-----READABLE REACTION.                                                
      GO TO 80                                                          
C                                                                       
C     IF OLD CONVENTION FOR MULTIPLE REACTIONS AND CHARACTERS ARE SAVED 
C     RESTORE THEM AND CONTINUE TRANSLATION.                            
C                                                                       
   10 IF(KSAVE.GT.ISAVE) GO TO 70                                       
C-----LOAD UP TO 55 CHARACTERS INTO INPUT CARD ARRAY.                   
      NSAVEK=NSAVE(KSAVE)                                               
      MSAVE=NSAVEK                                                      
      IF(MSAVE.GT.55) MSAVE=55                                          
      DO 20 I=1,MSAVE                                                   
   20 BIB1(I)=ZASAVE(I,KSAVE)                                           
      IF(MSAVE.GE.55) GO TO 40                                          
      NN=MSAVE+1                                                        
      DO 30 I=NN,55                                                     
   30 BIB1(I)=BLANK                                                     
C-----IF ANY CHARACTERS REMAIN SHIFT THEN FORWARD IN SAVED ARRAY.       
   40 IF(MSAVE.GE.NSAVEK) GO TO 60                                      
      II=0                                                              
      JJ=MSAVE+1                                                        
      DO 50 J=JJ,NSAVEK                                                 
      II=II+1                                                           
   50 ZASAVE(II,KSAVE)=ZASAVE(J,KSAVE)                                  
      NSAVE(KSAVE)=II                                                   
      GO TO 80                                                          
   60 KSAVE=KSAVE+1                                                     
      GO TO 80                                                          
C-----READ NEXT CARD.                                                   
   70 READ(ITAPE,1000,END=620,ERR=620) KEYWD1,KEYWD2,BIB1,BIB2          
      ISAVE=0                                                           
      KSAVE=1                                                           
C-----CONTINUE DECODING IF KEYWORD FIELD IS BLANK.                      
      IF(KEYWD1(1).NE.BLANK4) GO TO 610                                 
C-----TO BE MACHINE READABLE COLUMN 12 MUST CONTAIN (. IF NOT, ASSUME   
C-----COMMENT CARD AND SKIP IT.                                         
   80 IF(BIB1(1).NE.PARENL) GO TO 10                                    
C-----INITIALIZE CHARACTER COUNT, LEVEL AND INDEX TO NEXT CHARACTER.    
      KZAR1=0                                                           
      LEVEL=1                                                           
      LVLMAX=1                                                          
      II=2                                                              
C-----INITIALIZE CROSS SECTION RATIO FLAG OFF.                          
      IMRATS=0                                                          
C-----SAVE REACTION FLAG FROM COLUMN 11 AND INITIALIZE TARGET AND       
C-----RESIDUAL NUCLEUS, RATIO DENOMINATOR ZA AND MT, REACTION MF AND MT 
C-----AND CENTER-OF-MASS FLAG.                                          
      KSANP=KSANR+1                                                     
      IF(KSANP.GT.30) GO TO 630                                         
      FLAGR(KSANP)=KEYWD2                                               
      KZANRS(KSANP)=0                                                   
      DO 90 I=1,7                                                       
      ZAN(I,KSANP)=BLANK                                                
      ZANRAT(I,KSANP)=BLANK                                             
   90 ZANRES(I,KSANP)=BLANK                                             
      INPART(KSANP)=0                                                   
      MFR(KSANP)=0                                                      
      MTR(KSANP)=0                                                      
      MTRAT(KSANP)=0                                                    
      LABCM(KSANP)=BLANK                                                
C                                                                       
C     START OF NEW CARD (EITHER NEW REACTION OR CONTINUATION CARD).     
C                                                                       
C-----COPY UP TO BALANCED PARENTHESIS.                                  
  100 DO 130 I=II,55                                                    
      IF(BIB1(I).EQ.BLANK) GO TO 130                                    
      IF(BIB1(I).NE.PARENL) GO TO 110                                   
      LEVEL=LEVEL+1                                                     
      IF(LEVEL.GT.LVLMAX) LVLMAX=LEVEL                                  
      GO TO 120                                                         
  110 IF(BIB1(I).NE.PARENR) GO TO 120                                   
      LEVEL=LEVEL-1                                                     
      IF(LEVEL.EQ.0) GO TO 140                                          
  120 KZAR1=KZAR1+1                                                     
      IF(KZAR1.GT.300) GO TO 600                                        
      ZAR1(KZAR1)=BIB1(I)                                               
  130 CONTINUE                                                          
C-----PARENTHESIS NOT BALANCED YET. READ NEXT CARD.                     
      READ(ITAPE,1000,END=620,ERR=620) KEYWD1,KEYWD2,BIB1,BIB2          
C-----ERROR IF KEYWORD FIELD IS NOT BLANK.                              
      IF(KEYWD1(1).NE.BLANK4) GO TO 600                                 
C-----RESET TO BEGIN SCAN AT BEGINNING OF NEXT CARD.                    
      II=1                                                              
      GO TO 100                                                         
C                                                                       
C     ENTIRE REACTION COPIED. SAVE IT. DETERMINE IF THIS IS A SIMPLE    
C     OR COMPLEX REACTION                                               
C     FOR SIMPLE REACTION KEYWORD,                                      
C     REACTION IMPLIES ONLY 2 SETS OF PARENTHESIS.                      
C     OTHERS SUCH AS ISO-QUANT IMPLIES ONLY 1 SET OF PARENTHESIS.       
C                                                                       
  140 KZABAK=KZAR1                                                      
      DO 150 I=1,KZABAK                                                 
  150 ZARBAK(I)=ZAR1(I)                                                 
      IF(KZABAK-60) 160,180,180                                         
  160 J=KZABAK+1                                                        
      DO 170 I=J,60                                                     
  170 ZARBAK(I)=BLANK                                                   
  180 IF(KTYPE.EQ.1.AND.LVLMAX.LE.2) GO TO 540                          
      IF(LVLMAX.LE.1) GO TO 540                                         
C                                                                       
C     FOR OLD ISO-QUANT FORMALISM TEST FOR COMPLETE REACTIONS SEPERATED 
C     BY COMMAS. IF FOUND SAVE AND PROCESS EACH REACTION SEPERATELY.    
C                                                                       
      IF(KTYPE.EQ.1.OR.ISAVE.GT.0) GO TO 260                            
      J=1                                                               
      ISAVE=0                                                           
      KSAVE=1                                                           
      LVLOLD=0                                                          
  190 DO 200 I=J,KZABAK                                                 
      IF(ZARBAK(I).EQ.PARENL) LVLOLD=LVLOLD+1                           
      IF(ZARBAK(I).EQ.PARENR) LVLOLD=LVLOLD-1                           
      IF(LVLOLD.EQ.0) GO TO 210                                         
  200 CONTINUE                                                          
      GO TO 260                                                         
  210 J=I+1                                                             
      IF(J.GE.KZABAK) GO TO 260                                         
      IF(ZARBAK(J).NE.COMMA) GO TO 190                                  
C-----MULTIPLE REACTIONS FOUND. DEFINE FIRST REACTION AS BEGINNING UP TO
C-----CURRENT POINT.                                                    
      ISAVE=1                                                           
      DO 220 J=1,I                                                      
  220 ZASAVE(J,ISAVE)=ZARBAK(J)                                         
      NSAVE(ISAVE)=I                                                    
C-----COLLECT REMAINING REACTIONS.                                      
  230 ISAVE=ISAVE+1                                                     
      LSAVE=0                                                           
      LVLOLD=0                                                          
      J=I+2                                                             
      DO 240 I=J,KZABAK                                                 
      LSAVE=LSAVE+1                                                     
      ZASAVE(LSAVE,ISAVE)=ZARBAK(I)                                     
      IF(ZARBAK(I).EQ.PARENL) LVLOLD=LVLOLD+1                           
      IF(ZARBAK(I).EQ.PARENR) LVLOLD=LVLOLD-1                           
      IF(LVLOLD.NE.0) GO TO 240                                         
      IF(I.EQ.KZABAK) GO TO 250                                         
      IF(ZARBAK(I+1).EQ.COMMA) GO TO 250                                
  240 CONTINUE                                                          
      I=KZABAK                                                          
  250 NSAVE(ISAVE)=LSAVE                                                
      IF(I.LT.KZABAK) GO TO 230                                         
      GO TO 10                                                          
C                                                                       
C     COMPLEX REACTION. BREAK INTO PARTS AND DECODE EACH PART SEPERATELY
C     SAVE ALL ENCLOSING PARENTHESIS AND OTHER CHARACTERS TO DEFINE     
C     COMPLEX REACTION WITHOUT ZA.                                      
C                                                                       
C-----INCREMENT REACTION COUNT. ALLOW NO MORE THAN 30 REACTIONS.        
  260 LOOP=0                                                            
C-----COPY ALL LEADING LEFT PARENTHESIS INTO RN.                        
      DO 270 IBAK=1,KZABAK                                              
      IF(ZARBAK(IBAK).NE.PARENL) GO TO 320                              
      RN(IBAK)=ZARBAK(IBAK)                                             
  270 CONTINUE                                                          
C                                                                       
C     CANNOT TRANSLATE. ATEMPT TO TRANSLATE ENTIRE REACTION.            
C                                                                       
  280 DO 290 I=1,7                                                      
      ZA1(I)=ZAN(I,KSANP)                                               
  290 ZARES(I)=ZANRES(I,KSANP)                                          
      CALL MFMTX(ZARBAK,INPART(KSANP),MFR(KSANP),MTR(KSANP),            
     1 IRFLAG(KSANP),KNOWN)                                             
  300 WRITE(OUTP,6030) ENT,ISAN,INPART(KSANP),ZA1,ZARES,                
     1 MFR(KSANP),MTR(KSANP),FLAGR(KSANP),(ZARBAK(I),I=1,KZABAK)        
C-----INCREASE REACTION COUNT IF MF/MT ARE BOTH POSITIVE.               
      IF(MFR(KSANP).LE.0.OR.MTR(KSANP).LE.0) GO TO 310                  
      KSANR=KSANP                                                       
      GO TO 10                                                          
C                                                                       
C     WRITE REACTION TO NEWX4 FILE.                                     
C                                                                       
  310 IF(KNOWN.GT.0) GO TO 10                                           
      WRITE(NEWX4,4000) ENT,ISAN,(ZARBAK(I),I=1,KZABAK)                 
      MPOINT(7)=MPOINT(7)+1                                             
      GO TO 10                                                          
C-----DEFINE INITIALIZE NUMBER OF CHARACTERS COPIED.                    
  320 KRN=IBAK-1                                                        
C                                                                       
C     START OF SIMPLE REACTION FOUND. COPY TO BALANCED PARENTHESIS.     
C                                                                       
  330 LVL=1                                                             
      JBAK=IBAK                                                         
      KZAR1=0                                                           
      DO 340 IBAK=JBAK,KZABAK                                           
      IF(ZARBAK(IBAK).EQ.PARENL) LVL=LVL+1                              
      IF(ZARBAK(IBAK).EQ.PARENR) LVL=LVL-1                              
      IF(LVL.EQ.0) GO TO 350                                            
      KZAR1=KZAR1+1                                                     
      ZAR1(KZAR1)=ZARBAK(IBAK)                                          
  340 CONTINUE                                                          
      GO TO 280                                                         
C-----SIMPLE REACTION DEFINED (IN ZAR1). TRANSLATE IT.                  
  350 CALL REACT1                                                       
C-----SEE IF REACTION HAS BEEN TRANSLATED.                              
      IF(KR1.LE.0) GO TO 280                                            
C-----DEFINE MF/MT EQUIVALENT.                                          
      IF(KR1.GE.60) GO TO 370                                           
      JR1=KR1+1                                                         
      DO 360 I=JR1,60                                                   
  360 R1(I)=BLANK                                                       
  370 CALL MFMTX(R1,INPARX,MFRX,MTRX,IRFLGX,KNOWN)                      
C-----SEE IF REACTION CAN BE TRANSLATED. IF NOT, TRY TO TRANSLATE ENTIRE
C-----REACTION.                                                         
      IF(MFRX.LE.0.OR.MTRX.LE.0) GO TO 280                              
C-----ONLY ALLOW CROSS SECTION RATIOS FOR CROSS SECTIONS.               
      IF(MFRX.NE.3) IMRATS=0                                            
C-----AFTER FIRST SIMPLE REACTION INSURE ALL OTHERS HAVE SAME TARGET    
C-----AND RESIDUAL ZA (OTHERWISE CANNOT TRANSLATE).                     
      IF(LOOP.EQ.0) GO TO 420                                           
C-----IF THIS IS SECOND REACTION AND RATIO FLAG IS SET SAVE TARGET AND  
C-----PRODUCT ZA AND MT AND CONTINUE DECODING.                          
      IF(LOOP.NE.1.OR.IMRATS.LE.0) GO TO 390                            
      DO 380 I=1,7                                                      
      ZANRAT(I,KSANP)=ZA1(I)                                            
  380 ZANRAT(I+7,KSANP)=ZARES(I)                                        
      MTRAT(KSANP)=MTRX                                                 
      GO TO 440                                                         
C-----CHECK FOR SAME TARGET AND RESIDUAL ZA....IF NOT, CANNOT TRANSLATE.
  390 IF(KZA1.NE.KZAN(KSANP)) GO TO 280                                 
      DO 400 I=1,KZA1                                                   
      IF(ZAN(I,KSANP).NE.ZA1(I)) GO TO 280                              
  400 CONTINUE                                                          
      IF(KZARES.NE.KZANRS(KSANP)) GO TO 280                             
      IF(KZARES.LE.0) GO TO 440                                         
      DO 410 I=1,KZARES                                                 
      IF(ZANRES(I,KSANP).NE.ZARES(I)) GO TO 280                         
  410 CONTINUE                                                          
      GO TO 440                                                         
C-----SAVE FIRST TARGET AND RESIDUAL ZA (ONLY CROSS SECTIONS), INCIDENT 
C-----PARTICLE, MF, MT AND REACTION OPERATION FLAG.                     
  420 KZAN(KSANP)=KZA1                                                  
      KZANRS(KSANP)=KZARES                                              
      DO 430 I=1,7                                                      
      ZAN(I,KSANP)=ZA1(I)                                               
  430 ZANRES(I,KSANP)=ZARES(I)                                          
      INPART(KSANP)=INPARX                                              
      MFR(KSANP)=MFRX                                                   
      MTR(KSANP)=MTRX                                                   
      IRFLAG(KSANP)=IRFLGX                                              
C-----INCREMENT SIMPLE REACTION COUNT.                                  
  440 LOOP=LOOP+1                                                       
C-----ADD NEXT SIMPLE REACTION TO COMPLEX REACTION STRING.              
      DO 450 I=1,KR1                                                    
      KRN=KRN+1                                                         
  450 RN(KRN)=R1(I)                                                     
C                                                                       
C     COPY TO BEGINNING OF NEXT SIMPLE REACTION, OR END OF COMPLEX      
C     REACTION. SIMPLE REACTION STARTS AT FIRST CHARACTER AFTER NEXT    
C     SET OF LEFT PARENTHESIS.                                          
C                                                                       
C-----IF ONLY ONE REACTION READ SO FAR AND NEXT CHARACTER IS = ASSUME   
C-----REMAINDER OF REACTION EQUIVALENT TO FIRST SIMPLE REACTION.        
      IF(LOOP.EQ.1.AND.ZARBAK(IBAK+1).EQ.EQUAL) GO TO 470               
C-----IF ONLY ONE REACTION READ SO FAR AND NEXT CHARACTER IS / SET FLAG 
C-----TO INDICATE POSSIBLE SIMPLE RATIO.                                
      IF(LOOP.EQ.1.AND.ZARBAK(IBAK+1).EQ.SLASH) IMRATS=1                
C-----COPY UP TO NEXT LEFT PARENTHESIS OR END OF COMPLEX REACTION.      
      JBAK=IBAK                                                         
      DO 460 IBAK=JBAK,KZABAK                                           
      KRN=KRN+1                                                         
      RN(KRN)=ZARBAK(IBAK)                                              
      IF(ZARBAK(IBAK).EQ.PARENL) GO TO 490                              
  460 CONTINUE                                                          
C-----ENTIRE REACTION COPIED. IF SIMPLE RATIO INCREASE MF BY 200 AND    
C-----USE ZA/MF/MT FROM FIRST REACTION. OTHERWISE ATTEMPT TO TRANSLATE  
C-----COMPLEX REACTION.                                                 
      IF(LOOP.NE.2.OR.IMRATS.NE.1) GO TO 510                            
      MFR(KSANP)=MFR(KSANP)+200                                         
  470 DO 480 I=1,7                                                      
      ZA1(I)=ZAN(I,KSANP)                                               
  480 ZARES(I)=ZANRES(I,KSANP)                                          
      GO TO 300                                                         
C-----COPY ALL LEFT PARENTHESIS AND THEN BRANCH BACK TO DEFINE SIMPLE   
C-----REACTION.                                                         
  490 JBAK=IBAK+1                                                       
      IF(JBAK.GT.KZABAK) GO TO 280                                      
      DO 500 IBAK=JBAK,KZABAK                                           
      IF(ZAR1(IBAK).NE.PARENL) GO TO 330                                
      KRN=KRN+1                                                         
      RN(KRN)=ZARBAK(IBAK)                                              
  500 CONTINUE                                                          
      GO TO 280                                                         
C                                                                       
C     COMPLEX TRANSLATION COMPLETED. MOVE TO R1 AND ZA1 TO DEFINE       
C     PROJECTILE, MF AND MT.                                            
C                                                                       
  510 KR1=KRN                                                           
      DO 520 I=1,KRN                                                    
  520 R1(I)=RN(I)                                                       
      DO 530 I=1,7                                                      
      ZA1(I)=ZAN(I,KSANP)                                               
  530 ZARES(I)=ZANRES(I,KSANP)                                          
      GO TO 560                                                         
C                                                                       
C     SIMPLE REACTION. TRANSLATE IT.                                    
C                                                                       
  540 CALL REACT1                                                       
C-----SEE IF REACTION HAS BEEN TRANSLATED.                              
      IF(KR1.LE.0) GO TO 280                                            
C-----SAVE TARGET ZA.                                                   
      KZAN(KSANP)=KZA1                                                  
      KZANRS(KSANP)=KZARES                                              
      DO 550 I=1,7                                                      
      ZAN(I,KSANP)=ZA1(I)                                               
  550 ZANRES(I,KSANP)=ZARES(I)                                          
C                                                                       
C     DEFINE MF/MT EQUIVALENT.                                          
C                                                                       
  560 IF(KR1.GE.60) GO TO 580                                           
      JR1=KR1+1                                                         
      DO 570 I=JR1,60                                                   
  570 R1(I)=BLANK                                                       
  580 CALL MFMTX(R1,INPART(KSANP),MFR(KSANP),MTR(KSANP),                
     1 IRFLAG(KSANP),KNOWN)                                             
      WRITE(OUTP,6030) ENT,ISAN,INPART(KSANP),ZA1,ZARES,                
     1 MFR(KSANP),MTR(KSANP),FLAGR(KSANP),(R1(I),I=1,KR1)               
C-----INCREASE REACTION COUNT IF MF/MT ARE BOTH POSITIVE.               
      IF(MFR(KSANP).LE.0.OR.MTR(KSANP).LE.0) GO TO 310                  
      KSANR=KSANP                                                       
C-----IF SIMPLE RATIO DEFINE NUMERATOR AND DENOMINATOR TO BE THE SAME.  
      IF(MFR(KSANP).NE.203) GO TO 10                                    
      DO 590 I=1,7                                                      
      ZANRAT(I,KSANP)=ZA1(I)                                            
  590 ZANRAT(I+7,KSANP)=ZARES(I)                                        
C-----ASSUME RATIO OF METASTABLE STATES DEFINED.                        
      ZANRAT(14,KSANP)=MRAT                                             
      MTRAT(KSANP)=MTR(KSANP)                                           
      GO TO 10                                                          
C                                                                       
C     ERROR DECODING REACTIONS.                                         
C                                                                       
C-----PARENTHESIS DO NOT BALANCE.                                       
  600 WRITE(OUTP,6000) (ZAR1(I),I=1,KZAR1)                              
      WRITE(OUTP,6010)                                                  
  610 RETURN                                                            
C-----ERROR READING EXFOR DATA FILE.                                    
  620 WRITE(OUTP,6040)                                                  
      CALL GOOUT                                                        
C-----OVER 30 REACTIONS.                                                
  630 WRITE(OUTP,6020)                                                  
      RETURN                                                            
 1000 FORMAT(2A4,A2,A1,55A1,14A1)                                       
 4000 FORMAT(1X,5A1,I3,1X,60A1/(10X,60A1))                              
 6000 FORMAT(10X,80A1)                                                  
 6010 FORMAT(10X,'FORMAT ERROR...PARENTHESIS DO NOT BALANCE')           
 6020 FORMAT(10X,'OVER 30 REACTIONS...WILL ONLY TREAT FIRST 30')        
 6030 FORMAT(1X,5A1,I4,I8,1X,7A1,2X,7A1,I4,I5,1X,A1,1X,60A1/(46X,60A1)) 
 6040 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     
      END                                                               
      SUBROUTINE REACT1                                                 
C                                                                       
C     DECODE SIMPLE REACTION (ZAR1) INTO Z,A AND BASIC REACTION.        
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 BLANK,PARENL,PARENR,COMMA,ZERO,ZAR1,                  
     1 ZA1,R1,NX,ZARES                                                  
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER BLANK,PARENL,PARENR,COMMA,ZERO,ZAR1,                      
C    1 ZA1,R1,NX,ZARES                                                  
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/ZART1I/KZAR1                                               
      COMMON/ZART1C/ZAR1(300)                                           
      COMMON/ZAT1I/KZA1,KR1                                             
      COMMON/ZAT1C/ZA1(7),R1(300)                                       
      COMMON/RESIDI/KZARES                                              
      COMMON/RESIDC/ZARES(7)                                            
      DIMENSION NX(2)                                                   
      DATA BLANK/' '/                                                   
      DATA PARENL/'('/                                                  
      DATA PARENR/')'/                                                  
      DATA COMMA/','/                                                   
      DATA ZERO/'0'/                                                    
      DATA NX/',','X'/                                                  
C-----INITIALIZE RESIDUAL NUCLEUS ZA.                                   
      KZARES=0                                                          
      DO 10 I=1,7                                                       
   10 ZARES(I)=BLANK                                                    
C-----DECODE TARGET ZA AND CHECK FOR ERROR.                             
      CALL DECOZA(ZAR1,KZAR1,1,INOW,ZA1,KZA1)                           
      IF(KZA1.LE.0) GO TO 130                                           
C                                                                       
C     END OF TARGET Z, A. IF LEFT PARENTHESIS FOUND COPY REACTION UP    
C     TO RIGHT PARENTHESIS,E.G. (N,P). OTHERWISE OLD EXFOR FORMAT...    
C     COPY REMAINDER OF REACTION.                                       
C                                                                       
      KR1=0                                                             
      IF(ZAR1(INOW).NE.PARENL) GO TO 90                                 
      DO 20 JNOW=INOW,KZAR1                                             
      KR1=KR1+1                                                         
      R1(KR1)=ZAR1(JNOW)                                                
      IF(ZAR1(JNOW).EQ.PARENR) GO TO 30                                 
   20 CONTINUE                                                          
C-----ERROR. CANNOT LOCATE END OF REACTION.                             
      GO TO 120                                                         
C                                                                       
C     REACTION COPIED. TRANSLATE RESIDUAL NUCLEUS ZA. IF REACTION ENDS  
C     WITH ,X DEFINE LENGTH OF RESIDUAL NUCLEUS ZA.                     
C                                                                       
   30 JNOW=JNOW+1                                                       
      IF(JNOW.GT.KZAR1) GO TO 140                                       
C-----DECODE RSIDUAL NUCLEUS ZA AND CHECK FOR ERROR.                    
      CALL DECOZA(ZAR1,KZAR1,JNOW,INOW,ZARES,KZARES)                    
C-----IF LEGAL RESIDUAL FIELD NOT FOUND SKIP TO COPY REMAINDER OF       
C-----REACTION (WITHOUT ADVANCING CHARACTER INDEX). IF NEXT CHARACTER   
C-----IS COMMA SKIP IT (EMPTY RESIDUAL NUCLEUS FIELD).                  
      IF(INOW.GT.JNOW) GO TO 40                                         
      IF(ZAR1(JNOW).EQ.COMMA) GO TO 90                                  
      GO TO 100                                                         
   40 IF(KZARES.LE.0) GO TO 90                                          
C-----REMOVE LEADING ZEROES (EXCEPT THE LAST ONE) FROM RESIDUAL NUCLEUS.
      DO 50 I=1,5                                                       
      IF(ZARES(I).NE.BLANK) GO TO 60                                    
   50 CONTINUE                                                          
      GO TO 80                                                          
   60 DO 70 J=I,5                                                       
      IF(ZARES(J).NE.ZERO) GO TO 80                                     
   70 ZARES(J)=BLANK                                                    
C-----IF REACTION DOES NOT END WITH ,X) IS RESIDUAL CHARACTER COUNT=0   
   80 IF(R1(KR1-2).NE.NX(1).OR.R1(KR1-1).NE.NX(2)) KZARES=0             
C                                                                       
C     COPY REMAINDER OF REACTION.                                       
C                                                                       
   90 INOW=INOW+1                                                       
  100 IF(INOW.GT.KZAR1) GO TO 140                                       
      DO 110 JNOW=INOW,KZAR1                                            
      KR1=KR1+1                                                         
  110 R1(KR1)=ZAR1(JNOW)                                                
      GO TO 140                                                         
C-----ERROR. CANNOT LOCATE END OF REACTION.                             
  120 WRITE(OUTP,6000) (ZAR1(I),I=1,KZAR1)                              
      WRITE(OUTP,6020)                                                  
  130 KZA1=0                                                            
      KR1=0                                                             
  140 RETURN                                                            
 6000 FORMAT(10X,80A1)                                                  
 6020 FORMAT(10X,'FORMAT ERROR...CANNOT LOCATE END OF REACTION')        
      END                                                               
      SUBROUTINE DECOZA(ZARACT,KSIZE,KSTART,KNOW,ZAX,KZAX)              
C                                                                       
C     DECODE TARGET OR RESIDUAL NUCLEUS ZA.                             
C                                                                       
C     INPUT                                                             
C     =====                                                             
C     ZARACT = EXFOR SIMPLE REACTION.                                   
C     KSIZE  = LENGTH OF EXFOR SIMPLE REACTION STRING                   
C     KSTART = STARTING LOCATION TO BEGIN DECODING ZA.                  
C                                                                       
C     STARTING AT LOCATION KSTART AND NOT EXTENDING BEYOND LOCATION     
C     KSIZE OF THE ARRAY ZARACT THIS ROUTINE EXPECTS TO FIND A STRING   
C     OF THE FORM,                                                      
C                                                                       
C     ZZZ-SS-AAA-M = ZZZ = ATOMIC NUMBER                                
C                    SS  = CHEMICAL SYMBOL                              
C                    AAA = ATOMIC WEIGHT                                
C                    M   = METASTABLE STATE FLAG (MAY NOT BE PRESENT)   
C                                                                       
C     FOLLOWED BY , OR ( OR )                                           
C                                                                       
C     THIS ROUTINE WILL REMOVE THE CHEMICAL SYMBOL AND RIGHT ADJUST     
C     THE COMBINED ZZZ, AAA AND M TO THE FORM ZZZAAAM. IF ZZZ IS LESS   
C     THAN 3 CHARACTERS LONG IT WILL BE RIGHT ADJUSTED TO 3 CHARACTERS  
C     WITH LEADING BLANKS. IF AAA IS LESS THAN 3 CHARACTERS LONG IT WILL
C     BE RIGHT ADJUSTED WITH LEADING ZEROES. IF M IS NOT PRESENT IT WILL
C     BE SET TO BLANK.                                                  
C                                                                       
C     E.G., 26-FE-56-M = 26056M                                         
C                                                                       
C     THIS ROUTINE WILL RETURN,                                         
C                                                                       
C     ZAX  = 7 CHARACTER ZZZAAAM                                        
C     KZAX = COUNT OF CHARACTERS IN ZAX                                 
C          = 7 - NORMALLY                                               
C          = 0 - ERROR (CANNOT DECODE ZA)                               
C     KNOW = POINTER TO FIRST CHARACTER IN THE ARRAY ZARACT FOLLOWING   
C            THE ZA FIELD.                                              
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 ZARACT,ZAX,BLANK,ZERO,PARENL,PARENR,COMMA,DASH,MRAT,  
     1 METBCD                                                           
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER ZARACT,ZAX,BLANK,ZERO,PARENL,PARENR,COMMA,DASH,MRAT,      
C    1 METBCD                                                           
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/RATMET/MRAT                                                
      DIMENSION ZARACT(300),ZAX(7),METBCD(20)                           
      DATA BLANK/' '/                                                   
      DATA ZERO/'0'/                                                    
      DATA PARENL/'('/                                                  
      DATA PARENR/')'/                                                  
      DATA COMMA/','/                                                   
      DATA DASH/'-'/                                                    
C-----INITIALIZE ZA.                                                    
      KNOW=KSTART                                                       
      KZAX=0                                                            
      DO 10 I=1,7                                                       
   10 ZAX(I)=BLANK                                                      
C-----INITIALIZE METASTABLE FIELD CHARACTER COUNT.                      
      MRAT=BLANK                                                        
      METAF=0                                                           
C-----COPY FIELD IF NOT TWO DASHES BEFORE NEXT COMMA (ONLY RESIDUAL     
C-----NUCLEUS FIELD).                                                   
      IDASH=0                                                           
      DO 20 I=KSTART,KSIZE                                              
      IF(ZARACT(I).EQ.COMMA) GO TO 30                                   
      IF(ZARACT(I).EQ.DASH) IDASH=IDASH+1                               
   20 CONTINUE                                                          
      RETURN                                                            
   30 IF(IDASH.GE.2) GO TO 40                                           
      RETURN                                                            
C-----INITIALIZE DASH COUNT.                                            
   40 IDASH=-1                                                          
C-----SET UP LOOP OVER CHARACTERS.                                      
      DO 140 KNOW=KSTART,KSIZE                                          
C-----SEARCH FOR DASH (-).                                              
      IF(ZARACT(KNOW).EQ.DASH) GO TO 50                                 
C-----CHARACTER IS NOT A DASH. SKIP CHEMICAL SYMBOL BY SKIPPING ALL     
C-----CHARACTERS BETWEEN FIRST AND SECOND DASH.                         
      IF(IDASH) 60,140,120                                              
C-----CHARACTER IS A DASH.                                              
   50 IF(IDASH) 70,100,110                                              
C-----NO DASH FOUND YET. DEFINE Z TO BE UP TO 3 CHARACTERS PRECEDING    
C-----FIRST DASH (SKIP ALL CHARACTERS AFTER 3).                         
   60 IF(KZAX.GE.3) GO TO 140                                           
      KZAX=KZAX+1                                                       
      ZAX(KZAX)=ZARACT(KNOW)                                            
      GO TO 140                                                         
C-----FIRST DASH FOUND. IF NECESSARY RIGHT ADJUST Z WITH LEADING BLANKS.
   70 IDASH=0                                                           
      IF(KZAX.GE.3) GO TO 140                                           
      MM=3                                                              
      LL=KZAX                                                           
      DO 80 M=1,KZAX                                                    
      ZAX(MM)=ZAX(LL)                                                   
      MM=MM-1                                                           
   80 LL=LL-1                                                           
      DO 90 M=1,MM                                                      
   90 ZAX(M)=BLANK                                                      
      KZAX=3                                                            
      GO TO 140                                                         
C-----SECOND DASH FOUND. CHEMICAL SYMBOL HAS BEEN SKIPPED. SET DASH     
C-----COUNT TO COPY A FIELD.                                            
  100 IDASH=1                                                           
      GO TO 140                                                         
C-----MORE THAN 2 DASHES. SET DASH COUNT FOR METASTABLE STATE FIELD.    
  110 IDASH=2                                                           
      GO TO 140                                                         
C-----AFTER SECOND DASH SEARCH FOR END OF STRING...( OR ) OR ,          
  120 IF(ZARACT(KNOW).EQ.PARENL.OR.ZARACT(KNOW).EQ.PARENR.OR.           
     1 ZARACT(KNOW).EQ.COMMA) GO TO 150                                 
C-----NOT THE END OF STRING. IF MORE THAN 2 DASHES SAVE CHARACTERS FROM 
C-----METASTABLE STATE FIELD.                                           
      IF(IDASH.EQ.1) GO TO 130                                          
      IF(METAF.GT.20) GO TO 140                                         
      METAF=METAF+1                                                     
      METBCD(METAF)=ZARACT(KNOW)                                        
      GO TO 140                                                         
C-----IF 2 DASHES HAVE BEEN FOUND DEFINE A TO BE UP TO NEXT 3 CHARACTERS
C-----(SKIP ALL CHARACTERS AFTER 3).                                    
  130 IF(KZAX.GE.6) GO TO 140                                           
      KZAX=KZAX+1                                                       
      ZAX(KZAX)=ZARACT(KNOW)                                            
  140 CONTINUE                                                          
C-----ERROR. CANNOT LOCATE END OF ZA.                                   
      GO TO 190                                                         
C-----END OF A FOUND. IF NECESSARY RIGHT ADJUST A WITH LEADING ZEROES.  
  150 IF(KZAX.GE.6) GO TO 180                                           
      MM=6                                                              
      LL=KZAX                                                           
      DO 160 M=4,KZAX                                                   
      ZAX(MM)=ZAX(LL)                                                   
      MM=MM-1                                                           
  160 LL=LL-1                                                           
      DO 170 M=4,MM                                                     
  170 ZAX(M)=ZERO                                                       
  180 KZAX=7                                                            
C-----IF NECESSARY DECODE METASTABLE STATE FIELD.                       
      IF(METAF.GT.0) CALL DECODM(METBCD,METAF,ZAX,MRAT)                 
      RETURN                                                            
C-----ERROR. CANNOT LOCATE END OF ZA. PRINT ERROR AND SET ZA CHARACTER  
C-----COUNT TO ZERO.                                                    
  190 WRITE(OUTP,6000) (ZARACT(I),I=KSTART,KSIZE)                       
      WRITE(OUTP,6010)                                                  
      KZAX=0                                                            
      KNOW=KSIZE+1                                                      
      RETURN                                                            
 6000 FORMAT(10X,80A1)                                                  
 6010 FORMAT(10X,'FORMAT ERROR...CANNOT LOCATE END OF ZA')              
      END                                                               
      SUBROUTINE DECODM(METBCD,METAF,ZAX,MRAT)                          
C                                                                       
C     DECODE METASTABLE STATE FIELD.                                    
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 METBCD,ZAX,MRAT,SLASH,M,PLUS,WHAT,BLANK,TOTAL,MET1    
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER METBCD,ZAX,MRAT,SLASH,M,PLUS,WHAT,BLANK,TOTAL,MET1        
C***** INTEGER *****                                                    
      DIMENSION METBCD(20),ZAX(7)                                       
      DATA SLASH/'/'/                                                   
      DATA BLANK/' '/                                                   
      DATA M/'M'/                                                       
      DATA PLUS/'+'/                                                    
      DATA WHAT/'?'/                                                    
      DATA TOTAL/'T'/                                                   
C                                                                       
C     DETERMINE IF THIS IS RATIO OF METASTABLE STATES.                  
C                                                                       
      JFIELD=1                                                          
      DO 10 I=1,METAF                                                   
      IF(METBCD(I).EQ.SLASH) GO TO 20                                   
   10 CONTINUE                                                          
C-----NO. SET DENOMINATOR TO BLANK AND DECODE ONLY ONE FIELD.           
      MRAT=BLANK                                                        
      KFIELD=1                                                          
      NFIELD=METAF                                                      
      GO TO 30                                                          
C-----YES. DECODE 2 FIELDS SEPARATELY.                                  
   20 KFIELD=2                                                          
      NFIELD=I-1                                                        
C                                                                       
C     SET UP LOOP OVER FIELDS TO DECODE.                                
C                                                                       
   30 DO 110 K=1,KFIELD                                                 
      ICHAR=NFIELD-JFIELD+1                                             
C                                                                       
C     IF ONLY ONE CHARACTER USE AS DEFINITION.                          
C                                                                       
      IF(ICHAR-1) 80,40,50                                              
   40 MET1=METBCD(JFIELD)                                               
      IF(MET1.EQ.TOTAL) MET1=BLANK                                      
      IF(K.EQ.1) ZAX(7)=METBCD(JFIELD)                                  
      IF(K.EQ.2) MRAT=METBCD(JFIELD)                                    
      GO TO 100                                                         
C                                                                       
C     IF ONLY TWO CHARACTERS AND FIRST IS M USE SECOND CHARACTER.       
C                                                                       
   50 IF(METAF.GT.2) GO TO 60                                           
      IF(METBCD(JFIELD).NE.M) GO TO 80                                  
      IF(K.EQ.1) ZAX(7)=METBCD(JFIELD+1)                                
      IF(K.EQ.2) MRAT=METBCD(JFIELD+1)                                  
      GO TO 100                                                         
C                                                                       
C     MORE THAN TWO CHARACTERS. SEE IF SUM OF STATES.                   
C                                                                       
   60 DO 70 I=JFIELD,NFIELD                                             
      IF(METBCD(I).EQ.PLUS) GO TO 90                                    
   70 CONTINUE                                                          
C                                                                       
C     CANNOT DECODE. DEFINE ?                                           
C                                                                       
   80 IF(K.EQ.1) ZAX(7)=WHAT                                            
      IF(K.EQ.2) MRAT=WHAT                                              
      GO TO 100                                                         
C                                                                       
C     SUM OF STATES. DEFINE +                                           
C                                                                       
   90 IF(K.EQ.1) ZAX(7)=PLUS                                            
      IF(K.EQ.2) MRAT=PLUS                                              
  100 JFIELD=NFIELD+2                                                   
  110 NFIELD=METAF                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE STATUS                                                 
C                                                                       
C     DEFINE STATUS.                                                    
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*4 KEYWD1                                                
      CHARACTER*1 BIB1,BIB2,KEYWD2,BLANK,PARENL,STAT1,STATN,STATAB      
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER BIB1,BIB2,KEYWD2,BLANK,PARENL,STAT1,STATN,STATAB,         
C    1 KEYWD1                                                           
C***** INTEGER *****                                                    
      COMMON/BIBCRD/KEYWD1(3),KEYWD2,BIB1(55),BIB2(14)                  
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/STATUC/STAT1,STATN                                         
      DIMENSION STATAB(7,7)                                             
      DATA BLANK/' '/                                                   
      DATA PARENL/'('/                                                  
      DATA STATAB/                                                      
     1 'P','R','E','L','M',' ','P',                                     
     2 'S','P','S','D','D',' ','S',                                     
     3 'D','E','P',' ',' ',' ','D',                                     
     4 'C','O','R','E','L',' ','C',                                     
     5 'A','P','R','V','D',' ','A',                                     
     6 'O','U','T','D','T',' ','O',                                     
     7 'R','N','O','R','M',' ','R'/                                     
C-----INITIALIZE STATUS.                                                
      STATN=BLANK                                                       
      IF(ISAN.EQ.1) STAT1=BLANK                                         
      IF(BIB1(1).NE.PARENL) RETURN                                      
C-----FIND END OF STATUS.                                               
      DO 20 I=1,7                                                       
      DO 10 J=1,6                                                       
      IF(STATAB(J,I).EQ.BLANK) GO TO 30                                 
      IF(BIB1(J+1).NE.STATAB(J,I)) GO TO 20                             
   10 CONTINUE                                                          
      GO TO 30                                                          
   20 CONTINUE                                                          
C-----STATUS NOT DEFINED.                                               
      RETURN                                                            
C-----DEFINE STATUS.                                                    
   30 STATN=STATAB(7,I)                                                 
      IF(ISAN.EQ.1) STAT1=STATN                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE REFERS                                                 
C                                                                       
C     SAVE LATEST YEAR FROM REFERENCES.                                 
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*4 KEYWD1                                                
      CHARACTER*1 BIB1,BIB2,KEYWD2,REFER1,REFERN,BLANK,PARENL,PARENR,   
     1 COMMA,DIGITS                                                     
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER BIB1,BIB2,KEYWD2,REFER1,REFERN,BLANK,PARENL,PARENR,       
C    1 COMMA,DIGITS,KEYWD1                                              
C***** INTEGER *****                                                    
      COMMON/BIBCRD/KEYWD1(3),KEYWD2,BIB1(55),BIB2(14)                  
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/REFERI/IREF,NREF                                           
      COMMON/REFERC/REFER1(4),REFERN(4)                                 
      DIMENSION DIGITS(10)                                              
      DATA DIGITS/'1','2','3','4','5','6','7','8','9','0'/              
      DATA BLANK/' '/                                                   
      DATA PARENL/'('/                                                  
      DATA PARENR/')'/                                                  
      DATA COMMA/','/                                                   
C-----INITIALIZE REFERENCE.                                             
      NREF=0                                                            
      DO 10 I=1,4                                                       
   10 REFERN(I)=BLANK                                                   
      IF(BIB1(1).NE.PARENL) GO TO 70                                    
C-----FIND END OF REFERENCE AND LAST PRECEDING COMMA.                   
      ICOM=0                                                            
      LVL=1                                                             
      DO 20 IEND=2,55                                                   
      IF(BIB1(IEND).EQ.PARENL) LVL=LVL+1                                
      IF(BIB1(IEND).EQ.PARENR) LVL=LVL-1                                
      IF(BIB1(IEND).EQ.COMMA) ICOM=IEND                                 
      IF(LVL.LE.0) GO TO 30                                             
   20 CONTINUE                                                          
      GO TO 70                                                          
C-----SEARCH FOR YEAR BETWEEN LAST COMMA AND END OF REFERENCE.          
   30 IF(ICOM.LE.0) GO TO 70                                            
C-----SELECT LAST TWO DIGITS OF YEAR.                                   
      IEND=IEND-1                                                       
      IDIG=IEND-ICOM                                                    
      IF(IDIG.EQ.2) GO TO 60                                            
      IF(IDIG.EQ.4) GO TO 40                                            
C-----6 DIGIT DATE. SELECT MIDDLE TWO DIGITS AS MONTH.                  
      GO TO 50                                                          
C-----4 DIGIT DATE. IF FIRST DIGITS IS 0 OR 1 (START OF MONTH 1 TO 12   
C-----OR START OF  YEAR...1975) SELECT THIRD AND FOURTH DIGITS.         
   40 IF(BIB1(ICOM+1).EQ.DIGITS(1).OR.                                  
     1 BIB1(ICOM+1).EQ.DIGITS(10)) GO TO 50                             
C-----IF LAST TWO DIGITS ARE A MONTH (E.G. 00 TO 12...LEADING           
C-----DIGIT 0 OR 1) SELECT FIRST TWO DIGITS.                            
      IF(BIB1(ICOM+3).EQ.DIGITS(1).OR.                                  
     1 BIB1(ICOM+3).EQ.DIGITS(10)) GO TO 60                             
   50 ICOM=ICOM+2                                                       
   60 NREF=4                                                            
      REFERN(1)=PARENL                                                  
      REFERN(2)=BIB1(ICOM+1)                                            
      REFERN(3)=BIB1(ICOM+2)                                            
      REFERN(4)=PARENR                                                  
C-----IF THIS IS SUBENTRY 1 SAVE COMMON YEAR.                           
   70 IF(ISAN.NE.1) GO TO 90                                            
      IREF=NREF                                                         
      IF(IREF.LE.0) GO TO 90                                            
      DO 80 I=1,4                                                       
   80 REFER1(I)=REFERN(I)                                               
   90 RETURN                                                            
      END                                                               
      SUBROUTINE AUTHOR                                                 
C                                                                       
C     SAVE FIRST AUTHOR. IF MORE THAN ONE AUTHOR ADD ET.EL.             
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*4 KEYWD1                                                
      CHARACTER*1 BIB1,BIB2,KEYWD2,AUTH1,AUTHN,BLANK,PARENL,PARENR,     
     1 COMMA,ETAL                                                       
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER BIB1,BIB2,KEYWD2,AUTH1,AUTHN,BLANK,PARENL,PARENR,         
C    1 COMMA,ETAL,KEYWD1                                                
C***** INTEGER *****                                                    
      COMMON/BIBCRD/KEYWD1(3),KEYWD2,BIB1(55),BIB2(14)                  
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/AUTHI/IAUTH,NAUTH                                          
      COMMON/AUTHC/AUTH1(25),AUTHN(25)                                  
      DIMENSION ETAL(7)                                                 
      DATA BLANK/' '/                                                   
      DATA PARENL/'('/                                                  
      DATA PARENR/')'/                                                  
      DATA COMMA/','/                                                   
      DATA ETAL/',','E','T','.','A','L','.'/                            
C-----INITIALIZE AUTHOR.                                                
      NAUTH=0                                                           
      DO 10 I=1,25                                                      
   10 AUTHN(I)=BLANK                                                    
C-----TO BE MACHINE READABLE COLUMN 11 MUST CONTAIN (                   
      IF(BIB1(1).NE.PARENL) GO TO 60                                    
C-----DEFINE FIRST AUTHOR UP TO ) OR ,                                  
      DO 20 I=2,21                                                      
      IF(BIB1(I).EQ.PARENR) GO TO 60                                    
      IF(BIB1(I).EQ.COMMA) GO TO 30                                     
      NAUTH=NAUTH+1                                                     
      AUTHN(NAUTH)=BIB1(I)                                              
   20 CONTINUE                                                          
      GO TO 60                                                          
C-----MORE THAN 1 AUTHOR. INSTER ET.AL. IF THERE IS ROOM.               
C-----OTHERWISE JUST INSERT COMMA.                                      
   30 IF(NAUTH.LE.13) GO TO 40                                          
      NAUTH=NAUTH+1                                                     
      AUTHN(NAUTH)=COMMA                                                
      GO TO 60                                                          
   40 DO 50 I=1,7                                                       
      NAUTH=NAUTH+1                                                     
   50 AUTHN(NAUTH)=ETAL(I)                                              
C-----IF THIS IS SUBENTRY 1 SAVE COMMON AUTHOR.                         
   60 IF(ISAN.NE.1) GO TO 80                                            
      IAUTH=NAUTH                                                       
      IF(IAUTH.LE.0) GO TO 80                                           
      DO 70 I=1,IAUTH                                                   
   70 AUTH1(I)=AUTHN(I)                                                 
   80 RETURN                                                            
      END                                                               
      SUBROUTINE COMIN                                                  
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 UNIT,TIT78                                            
      CHARACTER*1 FLAGI,DATUM,TITLE                                     
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER FLAGI,DATUM,TITLE,UNIT,TIT78                              
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/HEADC1/TITLE(10,60),FLAGI(60),DATUM(11,60)                 
      COMMON/HEADC2/UNIT(3,60),TIT78(60)                                
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
C-----SAVE TITLES, UNITS AND DATA.                                      
      I1=ICOM1+1                                                        
      I2=ICOM1+N1                                                       
      READ(ITAPE,1010,END=10,ERR=10) ((TITLE(J,I),J=1,10),FLAGI(I),     
     1 I=I1,I2)                                                         
      READ(ITAPE,1020,END=10,ERR=10) ((UNIT(J,I),J=1,3),I=I1,I2)        
      READ(ITAPE,1030,END=10,ERR=10) ((DATUM(J,I),J=1,11),I=I1,I2)      
C-----DEFINE COLUMN COUNTS.                                             
      ICOMN=I2                                                          
      IF(ISAN.EQ.1) ICOM1=I2                                            
      GO TO 20                                                          
C-----ERROR READING EXFOR DATA.                                         
   10 WRITE(OUTP,6000)                                                  
      CALL GOOUT                                                        
   20 RETURN                                                            
 1010 FORMAT(66A1)                                                      
 1020 FORMAT(6(2A4,A3))                                                 
 1030 FORMAT(66A1)                                                      
 6000 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     
      END                                                               
      SUBROUTINE DATIN                                                  
C                                                                       
C     READ DATA POINTS, TRANSLATE AND OUTPUT IN COMPUTATION FORMAT.     
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 UNIT,BLANK4,TIT78,IM78                                
      CHARACTER*1 ENT,SUBENT,FLAGI,FLAGR,ZAN,AUTH1,AUTHN,AUTHK,DATUM,   
     1 REFER1,REFERN,LABCM,STAT1,STATN,ZANRES,ZANRAT,DIGITS,ZANOK,      
     2 BLANK1,OUTLIN,TITLE,REFERB                                       
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER ENT,SUBENT,FLAGI,FLAGR,ZAN,AUTH1,AUTHN,AUTHK,DATUM,       
C    1 REFER1,REFERN,LABCM,STAT1,STATN,ZANRES,ZANRAT,DIGITS,ZANOK,      
C    2 BLANK1,OUTLIN,TITLE,REFERB,                                      
C    3 UNIT,BLANK4,TIT78,IM78                                           
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/WHERE/ENT(5),SUBENT(3)                                     
      COMMON/HEADC1/TITLE(10,60),FLAGI(60),DATUM(11,60)                 
      COMMON/HEADC2/UNIT(3,60),TIT78(60)                                
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  
      COMMON/RNOW/ISANR                                                 
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     
     1 LABCM(30)                                                        
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
      COMMON/AUTHI/IAUTH,NAUTH                                          
      COMMON/AUTHC/AUTH1(25),AUTHN(25)                                  
      COMMON/REFERI/IREF,NREF                                           
      COMMON/REFERC/REFER1(4),REFERN(4)                                 
      COMMON/POINTR/MPOINT(9)                                           
      COMMON/STATUC/STAT1,STATN                                         
      DIMENSION AUTHK(25),OUTLIN(9,8),DIGITS(10),ZANOK(7),REFERB(4)     
      DATA BLANK1/' '/                                                  
      DATA BLANK4/'    '/                                               
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              
C-----USE (00) IF YEAR IS NOT DEFINED                                   
      DATA REFERB/'(','0','0',')'/                                      
C                                                                       
C     DEFINE STATUS, AUTHOR AND YEAR FROM CURRENT SUBENTRY OR COMMON    
C     SUBENTRY.                                                         
C                                                                       
      MPOINT(1)=MPOINT(1)+1                                             
      IF(STATN.EQ.BLANK1) STATN=STAT1                                   
      IF(NAUTH.GT.0.OR.IAUTH.LE.0) GO TO 20                             
      NAUTH=IAUTH                                                       
      DO 10 I=1,NAUTH                                                   
   10 AUTHN(I)=AUTH1(I)                                                 
   20 IF(NREF.GT.0.OR.IREF.LE.0) GO TO 40                               
      NREF=IREF                                                         
      DO 30 I=1,NREF                                                    
   30 REFERN(I)=REFER1(I)                                               
C-----COPY AUTHOR TO OUTPUT ARRAY.                                      
   40 KAUTH=NAUTH                                                       
      DO 50 I=1,25                                                      
   50 AUTHK(I)=AUTHN(I)                                                 
C                                                                       
C     ALWAYS ADD YEAR AS LAST 4 CHARACTERS OF AUTHOR FIELD.             
C                                                                       
C-----BLANK REMAINDER OF AUTHOR FIELD.                                  
      KAUTH=KAUTH+1                                                     
      IF(KAUTH.GT.25) GO TO 70                                          
      DO 60 I=KAUTH,25                                                  
   60 AUTHK(I)=BLANK1                                                   
   70 KAUTH=21                                                          
C-----ADD YEAR TO END OF AUTHOR.                                        
      IF(NREF.LE.0) GO TO 90                                            
C-----USED DEFINED YEAR.                                                
      DO 80 I=1,4                                                       
      KAUTH=KAUTH+1                                                     
   80 AUTHK(KAUTH)=REFERN(I)                                            
      GO TO 110                                                         
C-----YEAR IS NOT DEFINED - USE (00)                                    
   90 DO 100 I=1,4                                                      
      KAUTH=KAUTH+1                                                     
  100 AUTHK(KAUTH)=REFERB(I)                                            
C                                                                       
C     SAVE TITLES AND UNITS AND DEFINE INITIAL NUMBER OF COLUMNS.       
C                                                                       
  110 I1=ICOMN+1                                                        
      I2=ICOMN+N1                                                       
      IDATN=I2                                                          
      READ(ITAPE,1010,END=540,ERR=540) ((TITLE(J,I),J=1,10),FLAGI(I),   
     1 I=I1,I2)                                                         
      READ(ITAPE,1020,END=540,ERR=540) ((UNIT(J,I),J=1,3),I=I1,I2)      
C-----INITIALIZE FIELD 7-8 DEFINITION TO BLANK.                         
      DO 120 I=1,I2                                                     
  120 TIT78(I)=BLANK4                                                   
C-----INITIALIZE FLAGS TO INDICATE ALL INPUT FIELDS NOT YET REQUIRED OR 
C-----TRANSLATED.                                                       
      DO 130 I=1,IDATN                                                  
  130 IMUSED(I)=0                                                       
C                                                                       
C     DETERMINE THIS IS A MULTI-DIMENSIONAL TABLE = ONE REACTION BUT    
C     MULTIPLE FLAGS IN COMMON SECTION. FOR THIS CASE DEFINE MULTIPLE   
C     REACTIONS ALL OF WHICH ARE IDENTICAL AND DIFFER ONLY BY FLAG.     
C                                                                       
      IF(KSANR.GT.1.OR.ICOMN.LE.0) GO TO 180                            
      KSANR=0                                                           
      DO 170 I=1,ICOMN                                                  
C-----CHECK FOR NEW NON-BLANK FLAG IN COMMON.                           
      IF(FLAGI(I).EQ.BLANK1) GO TO 170                                  
      IF(KSANR.EQ.0) GO TO 150                                          
      DO 140 J=1,KSANR                                                  
      IF(FLAGI(I).EQ.FLAGR(I)) GO TO 170                                
  140 CONTINUE                                                          
C-----NEW FLAG. CREATE SAME REACTION WITH NEW FLAG BY INCREASING        
C-----REACTION COUNT AND DEFINING FLAG, ZA, MF, MT, REACTION OPERATION. 
  150 KSANR=KSANR+1                                                     
      FLAGR(KSANR)=FLAGI(I)                                             
      KZANRS(KSANR)=KZANRS(1)                                           
      DO 160 K=1,7                                                      
      ZAN(K,KSANR)=ZAN(K,1)                                             
  160 ZANRES(K,KSANR)=ZANRES(K,1)                                       
      INPART(KSANR)=INPART(1)                                           
      MFR(KSANR)=MFR(1)                                                 
      MTR(KSANR)=MTR(1)                                                 
      IRFLAG(KSANR)=IRFLAG(1)                                           
  170 CONTINUE                                                          
      IF(KSANR.EQ.0) KSANR=1                                            
C-----PRINT WARNING FOR MULTI-DIMENSIONAL TABLES.                       
  180 IF(KSANR.GT.1) WRITE(OUTP,6010)                                   
C                                                                       
C     CHECK FOR LEGAL TARGET ZA (ALL CHARACTERS BLANK OR DIGIT).        
C     IF ILLEGAL  CHARACTERS FOUND PRINT WARNING BUT DO NOT CHANGE.     
C                                                                       
      DO 210 ISANR=1,KSANR                                              
      IBADZA=0                                                          
      DO 200 M=1,6                                                      
      ZANOK(M)=ZAN(M,ISANR)                                             
      IF(ZAN(M,ISANR).EQ.BLANK1) GO TO 200                              
      DO 190 K=1,10                                                     
      IF(ZAN(M,ISANR).EQ.DIGITS(K)) GO TO 200                           
  190 CONTINUE                                                          
      IBADZA=1                                                          
      ZAN(M,ISANR)=DIGITS(1)                                            
  200 CONTINUE                                                          
      ZANOK(7)=ZAN(7,ISANR)                                             
      IF(IBADZA.NE.0) WRITE(OUTP,6055) ZANOK,(ZAN(M,ISANR),M=1,7)       
  210 CONTINUE                                                          
C                                                                       
C     PERFORM ALL OPERATIONS THAT APPLY TO ENTIRE DATA TABLE.           
C                                                                       
C-----DEFINE VECTORS TO PERMUTE DATA COLUMNS INTO OUTPUT ORDER FOR ALL  
C-----REACTIONS.                                                        
      CALL TITLEX                                                       
C-----PERFORM TITLE OPERATIONS THAT APPLY TO ALL POINTS IN TABLE.       
      CALL TOPS1                                                        
C-----DEFINE UNIT CONVERSION FACTORS AND OPERATIONS.                    
      CALL UNIT1                                                        
C-----SAVE NUMBER OF DATA COLUMNS (MAY BE MORE THAN THE NUMBER OF       
C-----COLUMN READ DUE TO CREATION OF COLUMNS).                          
      IDAT1=IDATN                                                       
C-----INCREMENT COUNT OF POINTS READ.                                   
      MPOINT(3)=MPOINT(3)+N2*KSANR                                      
C-----INITIALIZE BLANK DATA FIELD COUNT.                                
      NODATA=0                                                          
C-----INITIALIZE BLANK INCIDENT ENERGY FIELD COUNT.                     
      NOEIN=0                                                           
C-----INITIALIZE BLANK COSINE FIELD COUNT.                              
      NOMU=0                                                            
C-----INITIALIZE BLANK SECONDARY ENERGY FIELD COUNT.                    
      NOE2=0                                                            
C-----INITIALIZE L =0 LEGENDRE COEFFICIENT COUNT.                       
      LEGS=0                                                            
C-----SET UP LOOP TO READ DATA POINTS.                                  
      DO 500 NPT=1,N2                                                   
      READ(ITAPE,1030,END=540,ERR=540) ((DATUM(J,I),J=1,11),I=I1,I2)    
C-----RESET FLAGS TO INDICATE THAT DATA JUST READ HAS NOT YET BEEN      
C-----TRANSLATED.                                                       
      DO 220 I=I1,I2                                                    
      IF(IMUSED(I).EQ.2) IMUSED(I)=1                                    
  220 CONTINUE                                                          
C                                                                       
C     SET UP LOOP OVER REACTIONS.                                       
C                                                                       
      DO 490 ISANR=1,KSANR                                              
C-----RE-DEFINE NUMBER OF DATA COLUMNS                                  
      IDATN=IDAT1                                                       
C                                                                       
C     PERFORM ALL OPERATIONS THAT MAY BE DIFFERENT FOR EACH DATA POINT. 
C                                                                       
C-----CONVERT FIELDS REQUIRED FOR OUTPUT FROM HOLLERITH TO FLOATING     
C-----POINT AND FROM INPUT UNITS TO STANDARD UNITS.                     
      CALL UNIT2                                                        
C-----PERFORM UNIT OPERATIONS.                                          
      CALL UNOPS                                                        
C-----PERFORM TITLE OPERATIONS.                                         
      CALL TOPS2                                                        
C-----PERFORM REACTION OPERATIONS.                                      
      CALL REOPS                                                        
C                                                                       
C     CHECK DATA FIELD. IF NOT DEFINED NO OUTPUT. PRINT WARNING IF FIELD
C     IS NOT DEFINED OR BLANK.                                          
C                                                                       
      II=KMOUT(3,ISANR)                                                 
C-----PRINT WARNING IF DATA FIELD IS NOT DEFINED.                       
      IF(II.GT.0) GO TO 230                                             
      IF(NPT.EQ.1) WRITE(OUTP,6020)                                     
      MPOINT(5)=MPOINT(5)+1                                             
      GO TO 490                                                         
C-----DATA FIELD IS DEFINED. SEE IF DATA FIELD IS BLANK (SKIP TEST IF   
C-----DATA FIELD WAS CREATED).                                          
  230 IF(II.GT.IDAT1) GO TO 250                                         
      DO 240 I=1,11                                                     
      IF(DATUM(I,II).NE.BLANK1) GO TO 250                               
  240 CONTINUE                                                          
C-----PRINT WARNING WHEN FIRST BLANK DATA FIELD IS FOUND.               
      IF(NODATA.EQ.0) WRITE(OUTP,6025)                                  
      NODATA=1                                                          
      MPOINT(6)=MPOINT(6)+1                                             
      GO TO 490                                                         
C                                                                       
C     CHECK INCIDENT ENERGY FIELD. PRINT WARNING IF FIELD IS NOT DEFINED
C     OR BLANK.                                                         
C                                                                       
  250 IEIN=KMOUT(1,ISANR)                                               
C-----PRINT WARNING IF INCIDENT ENERGY FIELD IS NOT DEFINED.            
      IF(IEIN.GT.0) GO TO 260                                           
      IF(NPT.EQ.1) WRITE(OUTP,6140)                                     
      GO TO 280                                                         
C-----INCIDENT ENERGY FIELD IS DEFINED. SEE IF FIELD IS BLANK (SKIP     
C-----TEST IF FIELD WAS CREATED).                                       
  260 IF(IEIN.GT.IDAT1) GO TO 280                                       
      IF(NOEIN.GT.0) GO TO 280                                          
      DO 270 I=1,11                                                     
      IF(DATUM(I,IEIN).NE.BLANK1) GO TO 280                             
  270 CONTINUE                                                          
C-----PRINT WARNING WHEN FIRST BLANK INCIDENT ENERGY FIELD IS FOUND.    
      WRITE(OUTP,6150)                                                  
      NOEIN=1                                                           
C                                                                       
C     FOR ANGULAR AND DOUBLE DIFFERENTIAL DISTRIBUTION CHECK COSINE     
C     FIELD, FOR LEGENDRE COEFFICIENTS CHECK LEGENDRE ORDER FIELD.      
C     PRINT WARNING IF FIELD IS NOT DEFINED OR BLANK.                   
C                                                                       
  280 IF(MFR(ISANR).NE.4.AND.MFR(ISANR).NE.6.AND.MFR(ISANR).NE.154)     
     1 GO TO 310                                                        
      IMU=KMOUT(5,ISANR)                                                
C-----PRINT WARNING IF FIELD IS NOT DEFINED.                            
      IF(IMU.GT.0) GO TO 290                                            
      IF(NPT.EQ.1) WRITE(OUTP,6160)                                     
      GO TO 310                                                         
C-----FIELD IS DEFINED. SEE IF FIELD IS BLANK (SKIP TEST IF FIELD WAS   
C-----CREATED).                                                         
  290 IF(IMU.GT.IDAT1) GO TO 310                                        
      IF(NOMU.GT.0) GO TO 310                                           
      DO 300 I=1,11                                                     
      IF(DATUM(I,IMU).NE.BLANK1) GO TO 310                              
  300 CONTINUE                                                          
C-----PRINT WARNING WHEN FIRST BLANK FIELD IS FOUND.                    
      WRITE(OUTP,6170)                                                  
      NOMU=1                                                            
C                                                                       
C     FOR ENERGY AND DOUBLE DIFFERENTIAL DISTRIBUTION CHECK SECONDARY   
C     ENERGY FIELD. PRINT WARNING IF FIELD IS NOT DEFINED OR BLANK.     
C                                                                       
  310 IF(MFR(ISANR).NE.5.AND.MFR(ISANR).NE.6) GO TO 340                 
      IE2=KMOUT(7,ISANR)                                                
C-----PRINT WARNING IF FIELD IS NOT DEFINED.                            
      IF(IE2.GT.0) GO TO 320                                            
      IF(NPT.EQ.1) WRITE(OUTP,6180)                                     
      GO TO 340                                                         
C-----FIELD IS DEFINED. SEE IF FIELD IS BLANK (SKIP TEST IF FIELD WAS   
C-----CREATED).                                                         
  320 IF(IE2.GT.IDAT1) GO TO 340                                        
      IF(NOE2.GT.0) GO TO 340                                           
      DO 330 I=1,11                                                     
      IF(DATUM(I,IE2).NE.BLANK1) GO TO 340                              
  330 CONTINUE                                                          
C-----PRINT WARNING WHEN FIRST BLANK FIELD IS FOUND.                    
      WRITE(OUTP,6190)                                                  
      NOE2=1                                                            
C                                                                       
C     INITIALIZE OUTPUT FIELDS AND THEN FILL IN ALL REQUIRED OUTPUT.    
C                                                                       
  340 DO 350 J=1,8                                                      
      DO 350 I=1,9                                                      
  350 OUTLIN(I,J)=BLANK1                                                
C-----INITIALIZE DEFINITION OF FIELD 7-8.                               
      IM78=BLANK4                                                       
      DO 380 I=1,8                                                      
      II=KMOUT(I,ISANR)                                                 
C-----SKIP UNUSED FIELDS AND ZERO ERROR FIELDS.                         
      IF(II.LE.0) GO TO 380                                             
      OVALUE=VALUES(II)                                                 
C-----ON FIRST POINT PRINT WARNING MESSAGE IF L = 0 LEGENDRE COEFFICIENT
C-----IS NOT NORMALIZED TO UNITY.                                       
      IF(MFR(ISANR).NE.154.OR.LEGS.GT.0.OR.I.NE.5) GO TO 360            
      IF(ABS(OVALUE).GT.0.001) GO TO 360                                
      LEGS=1                                                            
      LEG1=KMOUT(3,ISANR)                                               
      IF(ABS(VALUES(LEG1)-1.0).GT.0.001) WRITE(OUTP,6120)               
  360 IF(I.NE.2.AND.I.NE.4.AND.I.NE.6.AND.I.NE.8) GO TO 370             
C-----INSURE THAT ALL ERRORS ARE NON-NEGATIVE.                          
      OVALUE=ABS(OVALUE)                                                
      IF(OVALUE.LE.0.0) GO TO 380                                       
C-----FORMAT DATA FOR OUTPUT.                                           
  370 CALL NORMF(OVALUE,OUTLIN(1,I))                                    
C-----IF REQUIRED SET DEFINITION OF FIELDS 7-8.                         
      IF(I.NE.7) GO TO 380                                              
      IF(TIT78(II).EQ.BLANK4) GO TO 380                                 
      IF(IM78.EQ.TIT78(II)) GO TO 380                                   
      IF(NPT.EQ.1.AND.IM78.NE.BLANK4) WRITE(OUTP,6040)                  
      IM78=TIT78(II)                                                    
      IF(NPT.EQ.1) WRITE(OUTP,6030) IM78                                
  380 CONTINUE                                                          
C                                                                       
C     IF REQUIRED INSERT RESIDUAL NUCLEUS ZA (PRODUCTION) OR RATIO      
C     DENOMINATOR MT AND MT. CHECK FOR ILLEGAL VALUES AND CONFLICTS.    
C                                                                       
C     PRODUCTION                                                        
C                                                                       
      IF(KZANRS(ISANR).LE.0) GO TO 420                                  
C-----ONLY PERFORM CHECKS AND PRINT MESSAGES FOR FIRST POINT.           
      IF(NPT.GT.1) GO TO 410                                            
C-----CHECK FOR LEGAL PRODUCT ZA (ALL CHARACTERS BLANK OR DIGIT).       
      IBADZA=0                                                          
      DO 400 M=1,6                                                      
      ZANOK(M)=ZANRES(M,ISANR)                                          
      IF(ZANRES(M,ISANR).EQ.BLANK1) GO TO 400                           
      DO 390 K=1,10                                                     
      IF(ZANRES(M,ISANR).EQ.DIGITS(K)) GO TO 400                        
  390 CONTINUE                                                          
      IBADZA=1                                                          
      ZANRES(M,ISANR)=DIGITS(1)                                         
  400 CONTINUE                                                          
      ZANOK(7)=ZANRES(7,ISANR)                                          
      IF(IBADZA.NE.0) WRITE(OUTP,6060) ZANOK,(ZANRAT(M,ISANR),M=1,7)    
C-----CHECK FOR PRODUCTION REACTIONS (MT = 9000 - 9999).                
      IF(MTR(ISANR).LT.9000) WRITE(OUTP,6080) (ZANRES(M,ISANR),M=1,7)   
C-----INSERT RESIDUAL NUCLEUS IN THE SIXTH OUTPUT FIELD (NORMALLY USED  
C-----FOR COSINE ERROR).                                                
  410 CALL RESZA(OUTLIN(1,6),ZANRES(1,ISANR))                           
      GO TO 430                                                         
C-----NO PRODUCT ZA. PRINT WARNING IF MT = 9000 - 9999.                 
  420 IF(NPT.EQ.1.AND.MTR(ISANR).GE.9000) WRITE(OUTP,6090)              
C                                                                       
C     RATIO                                                             
C                                                                       
  430 IF(MTRAT(ISANR).LE.0) GO TO 470                                   
C-----ONLY PERFORM CHECKS AND PRINT MESSAGES FOR FIRST POINT.           
      IF(NPT.GT.1) GO TO 460                                            
C-----CHECK FOR LEGAL RATIO ZA (ALL CHARACTERS BLANK OR DIGIT).         
      IBADZA=0                                                          
      DO 450 M=1,6                                                      
      ZANOK(M)=ZANRAT(M,ISANR)                                          
      IF(ZANRAT(M,ISANR).EQ.BLANK1) GO TO 450                           
      DO 440 K=1,10                                                     
      IF(ZANRAT(M,ISANR).EQ.DIGITS(K)) GO TO 450                        
  440 CONTINUE                                                          
      IBADZA=1                                                          
      ZANRAT(M,ISANR)=DIGITS(1)                                         
  450 CONTINUE                                                          
      ZANOK(7)=ZANRAT(7,ISANR)                                          
      IF(IBADZA.NE.0) WRITE(OUTP,6110) ZANOK,(ZANRAT(M,ISANR),M=1,7)    
C-----CHECK FOR RATIO (MF = 203).                                       
      IF(MFR(ISANR).NE.203) WRITE(OUTP,6050) (ZANRAT(M,ISANR),M=1,7),   
     1 MTRAT(ISANR)                                                     
C-----CHECK FOR CONFLICT (RATIO OF PRODUCTIONS).                        
      IF(MTR(ISANR).GE.9000.OR.MTRAT(ISANR).GE.9000) WRITE(OUTP,6070)   
C-----INSERT RATIO MT IN FIFTH FIELD AND ZA IN SIXTH OUTPUT FIELD       
C-----(NORMALLY USED FOR COSINE AND COSINE ERROR).                      
  460 CALL RATZA(OUTLIN(1,5),OUTLIN(1,6),MTRAT(ISANR),ZANRAT(1,ISANR))  
      GO TO 480                                                         
C-----NO RATIO ZA/MT. PRINT WARNING IF MF = 203.                        
  470 IF(NPT.EQ.1.AND.MFR(ISANR).EQ.203) WRITE(OUTP,6100)               
C                                                                       
C     PRINT DATA POINT.                                                 
C                                                                       
  480 WRITE(OTAPE,1100) INPART(ISANR),(ZAN(I,ISANR),I=1,7),MFR(ISANR),  
     1 MTR(ISANR),ZANRES(7,ISANR),STATN,LABCM(ISANR),OUTLIN,IM78,       
     1 AUTHK,ENT,ISAN,FLAGR(ISANR)                                      
      MPOINT(4)=MPOINT(4)+1                                             
  490 CONTINUE                                                          
  500 CONTINUE                                                          
C-----PRINT WARNING IF ANY LEGENDRE COEFFICIENTS AND L =0 COEFFICIENTS  
C-----ARE NOT GIVEN.                                                    
      IF(LEGS.GT.0) GO TO 530                                           
      DO 510 I=1,KSANR                                                  
      IF(MFR(I).EQ.154) GO TO 520                                       
  510 CONTINUE                                                          
      GO TO 530                                                         
  520 WRITE(OUTP,6130)                                                  
C                                                                       
C     TABLE COMPLETED. NORMAL RETURN.                                   
C                                                                       
  530 GO TO 550                                                         
C                                                                       
C     ERROR READING EXFOR DATA. PRINT ERROR MESSAGE AND TERMINATE.      
C                                                                       
  540 WRITE(OUTP,6000)                                                  
      CALL GOOUT                                                        
  550 RETURN                                                            
 1010 FORMAT(66A1)                                                      
 1020 FORMAT(6(2A4,A3))                                                 
 1030 FORMAT(66A1)                                                      
 1100 FORMAT(I5,7A1,I3,I4,3A1,8(9A1),A3,25A1,5A1,I3,A1)                 
 6000 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     
 6010 FORMAT(10X,'WARNING.....MULTI-DIMENSIONAL DATA TABLE')            
 6020 FORMAT(10X,'WARNING.....DATA IS NOT DEFINED. DATA POINT IGNORED') 
 6025 FORMAT(10X,'WARNING.....DATA FIELD IS BLANK. DATA POINT IGNORED') 
 6030 FORMAT(10X,'OPERATION...SET FIELD 7-8 DEFINITION=',A3)            
 6040 FORMAT(10X,'WARNING.....CONFLICTING DEFINITIONS OF FIELDS 7-8')   
 6050 FORMAT(10X,'WARNING.....RATIO DENOMINATOR ZA/MT=',7A1,'/',I5/     
     1       10X,'            (EXPECT MF = 203)')                       
 6055 FORMAT(10X,'WARNING.....NON-NUMERIC TARGET  ZA =',7A1/            
     1       10X,'                   CONVERTED TO ZA =',7A1/            
     2       10X,'            YOU MUST CORRECT TRANSLATED DATA TO',     
     3 ' DEFINE SPECIAL ZA.'/                                           
     4       10X,'            SEE ENDF/B SPECIAL ZA DICTIONARY')        
 6060 FORMAT(10X,'WARNING.....NON-NUMERIC PRODUCT ZA =',7A1/            
     1       10X,'                   CONVERTED TO ZA =',7A1/            
     2       10X,'IMPORTANT...YOU MUST CORRECT TRANSLATED DATA TO',     
     3 ' DEFINE SPECIAL ZA.'/                                           
     4       10X,'            SEE ENDF/B SPECIAL ZA DICTIONARY')        
 6070 FORMAT(10X,'WARNING.....RATIO OF PRODUCTION DATA. WILL DEFINE'/   
     1       10X,'            MT AND ZA OF DENOMINATOR IN FIELD 5 AND'/ 
     2       10X,'            6. CANNOT DEFINE ZA OF PRODUCTS.'/        
     3       10X,'            CHECK AND CORRECT OUTPUT.')               
 6080 FORMAT(10X,'WARNING.....PRODUCT ZA =',7A1,                        
     1 ' (EXPECT MT = 9000 - 9999)')                                    
 6090 FORMAT(10X,'WARNING.....EXPECT PRODUCT ZA FOR MT=9000-9999')      
 6100 FORMAT(10X,'WARNING.....EXPECT RATIO ZA/MT FOR MF = 203')         
 6110 FORMAT(10X,'WARNING.....RATIO ZA =',7A1,' CONVERTED TO'/          
     1       10X,'                  ZA =',7A1)                          
 6120 FORMAT(10X,'WARNING.....L = 0 LEGENDRE COEFFICIENTS ARE NOT',     
     1 ' NORMALIZED')                                                   
 6130 FORMAT(10X,'WARNING.....L = 0 LEGENDRE COEFFICIENTS ARE NOT',     
     1 ' PRESENT')                                                      
 6140 FORMAT(10X,'WARNING.....INCIDENT ENERGY IS NOT DEFINED')          
 6150 FORMAT(10X,'WARNING.....INCIDENT ENERGY FIELD IS BLANK')          
 6160 FORMAT(10X,'WARNING.....COSINE OR LEGENDRE ORDER IS NOT DEFINED') 
 6170 FORMAT(10X,'WARNING.....COSINE OR LEGENDRE ORDER FIELD IS BLANK') 
 6180 FORMAT(10X,'WARNING.....SECONDARY ENERGY IS NOT DEFINED')         
 6190 FORMAT(10X,'WARNING.....SECONDARY ENERGY FIELD IS BLANK')         
      END                                                               
      SUBROUTINE RESZA(OUT1,ZANRES)                                     
C                                                                       
C     DEFINE RESIDUAL NUCLEUS IN OUTPUT FIELD.                          
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 OUT1,ZANRES,BLANK,DOT                                 
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER OUT1,ZANRES,BLANK,DOT                                     
C***** INTEGER *****                                                    
      DIMENSION OUT1(9),ZANRES(7)                                       
      DATA BLANK/' '/                                                   
      DATA DOT/'.'/                                                     
      OUT1(1)=BLANK                                                     
      DO 10 I=1,6                                                       
   10 OUT1(I+1)=ZANRES(I)                                               
      OUT1(8)=DOT                                                       
C-----DEFINE PRODUCT METASTABLE STATE FLAG EQUIVALENT AND OUTPUT IN     
C-----COLUMN 9.                                                         
      CALL META10(OUT1(9),ZANRES(7))                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE RATZA(OUT1,OUT2,MTIN,ZANRAT)                           
C                                                                       
C     DEFINE RATIO MT IN FIELD 7 AND RATIO ZA IN FIELD 8.               
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 OUT1,OUT2,ZANRAT,BLANK,DOT,DIGITS                     
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER OUT1,OUT2,ZANRAT,BLANK,DOT,DIGITS                         
C***** INTEGER *****                                                    
      DIMENSION OUT1(9),OUT2(9),ZANRAT(14),MULT(4),DIGITS(10)           
      DATA BLANK/' '/                                                   
      DATA DOT/'.'/                                                     
      DATA MULT/1,10,100,1000/                                          
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              
      MT=MTIN                                                           
C-----INITIALIZE FIRST WORD TO BLANK FOLLOWED BY DECIMAL POINT.         
      DO 10 I=1,7                                                       
   10 OUT1(I)=BLANK                                                     
      OUT1(8)=DOT                                                       
C-----OUTPUT MT IN FIRST WORD (CHARACTERS 4 THROUGH 7).                 
      J=4                                                               
      IF(MT.LT.1000) J=3                                                
      IF(MT.LT.100) J=2                                                 
      IF(MT.LT.10) J=1                                                  
      MULTMT=MULT(J)                                                    
      II=8-J                                                            
      DO 20 I=1,J                                                       
      IDIG=MT/MULTMT                                                    
      OUT1(II)=DIGITS(IDIG+1)                                           
      MT=MT-IDIG*MULTMT                                                 
      MULTMT=MULTMT/10                                                  
   20 II=II+1                                                           
C-----DEFINE PRODUCT METASTABLE STATE FLAG EQUIVALENT AND OUTPUT IN     
C-----COLUMN 9.                                                         
      CALL META10(OUT1(9),ZANRAT(14))                                   
C-----OUTPUT ZA IN SECOND WORD.                                         
      OUT2(1)=BLANK                                                     
      DO 30 I=1,6                                                       
   30 OUT2(I+1)=ZANRAT(I)                                               
      OUT2(8)=DOT                                                       
C-----DEFINE ZA METASTABLE STATE FLAG EQUIVALENT AND OUTPUT IN          
C-----COLUMN 9.                                                         
      CALL META10(OUT2(9),ZANRAT(7))                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE META10(OUT,MSTATE)                                     
C                                                                       
C     DEFINE NUMERICAL EQUIVALENT OF METASTABLE STATE FLAG FOR OUTPUT   
C     WITH ZA OR MT.                                                    
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 OUT,MSTATE,MTAB1,MTAB2                                
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER OUT,MSTATE,MTAB1,MTAB2                                    
C***** INTEGER *****                                                    
      DIMENSION MTAB1(11),MTAB2(11)                                     
      DATA MTAB1/                                                       
     1 'G','1','2','3','4','5','?','M','+','T',' '/                     
      DATA MTAB2/                                                       
     1 '0','1','2','3','4','5','6','7','8','9','9'/                     
C-----LOOK UP METASTABLE STATE CHARACTER.                               
      DO 10 I=1,11                                                      
      IF(MSTATE.EQ.MTAB1(I)) GO TO 20                                   
   10 CONTINUE                                                          
C-----SET INDEX TO UNKNOWN.                                             
      I=7                                                               
   20 OUT=MTAB2(I)                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE NORMF(X,FIELD)                                         
C                                                                       
C     CONVERT FLOATING POINT NUMBER TO A STRING OF 9 CHARACTERS FOR     
C     OUTPUT. NUMBERS BETWEEN 0.01 AND 9999999. WILL BE FORMATTED IN    
C     F FORMAT. ALL OTHER NUMBERS WILL BE FORMATTED IN E FORMAT.        
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 MINUS,FIELD,DIGITS,BLANK,ZERO,DOT                     
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER MINUS,FIELD,DIGITS,BLANK,ZERO,DOT                         
C***** INTEGER *****                                                    
      DOUBLE PRECISION ZMULT,QMULT,XNORM,XIN,XABS,XZERO                 
      DIMENSION FIELD(9),DIGITS(10),ZERO(9),ZMULT(11),QMULT(11),IMULT(6)
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              
      DATA ZERO/' ','0','.','0','0','0','0','0','0'/                    
      DATA MINUS/'-'/                                                   
      DATA BLANK/' '/                                                   
      DATA DOT/'.'/                                                     
      DATA IMULT/100,1000,10000,100000,1000000,10000000/                
      DATA ZMULT/1.0D-3,1.0D-2,1.0D-1,1.0D+0,1.0D+1,1.0D+2,1.0D+3,      
     1 1.0D+4,1.0D+5,1.0D+6,1.0D+7/                                     
      DATA QMULT/1.0D-8,1.0D-7,1.0D-6,1.0D-5,1.0D-4,1.0D-3,1.0D-2,      
     1 1.0D-1,1.0D+0,1.0D+1,1.0D+2/                                     
      DATA XZERO/0.0D+0/                                                
C-----IF NUMBER IS ZERO RETURN STANDARD FORM.                           
      XIN=X                                                             
      XABS=DABS(XIN)                                                    
      IF(XABS.GT.XZERO) GO TO 20                                        
      DO 10 I=1,9                                                       
   10 FIELD(I)=ZERO(I)                                                  
      RETURN                                                            
C-----IF NUMBER OUT OF RANGE USE E FORMAT.                              
   20 IF(XABS.LT.ZMULT(2).OR.XABS.GE.ZMULT(11)) GO TO 110               
C-----DEFINE EXPONENT TO NORMALIZE MANTISSA.                            
      DO 30 IEXP=1,11                                                   
C-----IF CLOSE TO ONE DECADE SET EQUAL TO ONE DECADE.                   
      IF(DABS(XABS-ZMULT(IEXP)).LE.QMULT(IEXP)) XABS=ZMULT(IEXP)        
      IF(XABS.LT.ZMULT(IEXP)) GO TO 40                                  
   30 CONTINUE                                                          
      GO TO 110                                                         
C-----PRECEDING VALUE WILL NORMALIZE NUMBER.                            
   40 IEXP=IEXP-1                                                       
C-----DEFINE EXPONENT TO PUT NUMBER IN NORMAL FORM N.NNNN...            
      KEXP=IEXP-4                                                       
C-----INITIALIZE OUTPUT FIELD.                                          
      DO 50 I=1,9                                                       
   50 FIELD(I)=BLANK                                                    
C-----IF NUMBER IS NEGATIVE SET SIGN.                                   
      IF(X.LT.0.0) FIELD(1)=MINUS                                       
C-----DEFINE NORMALIZED INTEGER.                                        
      KEXPAB=IABS(KEXP)                                                 
      IF(KEXPAB.LE.0) KEXP=0                                            
      KMULT=5                                                           
      IF(KEXP.LT.0) KMULT=5+KEXP                                        
      XNORM=ZMULT(KMULT+6)*XABS/ZMULT(IEXP)                             
      INORM=XNORM                                                       
      INORM=(INORM+5)/10                                                
      KNORM=1000000                                                     
C-----TRY TO AVOID LAST DIGIT ROUND-OFF.                                
      IF(INORM.LT.KNORM) GO TO 70                                       
      LNORM=INORM-1000*(INORM/1000)                                     
C-----IF LAST 3 DIGITS ARE LESS THAN 005 ROUND DOWN.                    
      IF(LNORM.GT.5) GO TO 60                                           
      INORM=10*(INORM/10)                                               
      GO TO 70                                                          
C-----IF LAST 3 DIGITS ARE GREATER THAN 995 ROUND UP.                   
   60 IF(LNORM.LT.995) GO TO 70                                         
      INORM=1000*(INORM/1000)+1000                                      
C-----INSURE NUMBER IS IN NORMALIZED FORM (I.E., ALLOW FOR ROUND-OFF).  
   70 IF(INORM.GE.IMULT(KMULT)) GO TO 80                                
      INORM=IMULT(KMULT)                                                
      GO TO 90                                                          
   80 IF(INORM.LT.IMULT(KMULT+1)) GO TO 90                              
      INORM=IMULT(KMULT+1)                                              
      KEXP=KEXP+1                                                       
C-----DEFINE POSITION OF DECIMAL POINT AND INSERT IT.                   
   90 IPOINT=3                                                          
      IF(KEXP.GT.0) IPOINT=KEXP+3                                       
      IF(IPOINT.GT.9) GO TO 110                                         
      FIELD(IPOINT)=DOT                                                 
C-----CONVERT TO CHARACTERS AND INSERT INTO OUTPUT FIELD                
      DO 100 I=2,9                                                      
C-----SKIP DECIMAL POINT LOCATION.                                      
      IF(I.EQ.IPOINT) GO TO 100                                         
      IDIG=INORM/KNORM                                                  
      FIELD(I)=DIGITS(IDIG+1)                                           
      INORM=INORM-IDIG*KNORM                                            
      KNORM=KNORM/10                                                    
  100 CONTINUE                                                          
      RETURN                                                            
C-----NUMBER IS OUT OF RANGE. USE E FORMATTED OUTPUT.                   
  110 CALL NORME(X,FIELD)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE NORME(X,FIELD)                                         
C                                                                       
C     CONVERT FLOATING POINT NUMBER TO CHARACTER STRING FOR E9.3        
C     OUTPUT. OUTPUT WILL BE IN THE FORM X.XXX+/-NN OR X.XXXX+/-N       
C     (IF EXPONENT IS LESS THAN 10) WHICH GIVES 1 OR 2 MORE DIGITS      
C     OF ACCURACY COMPARED TO NORMAL FORTRAN OUTPUT.                    
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 PLUS,MINUS,FIELD,DIGITS,BLANK,ZERO,DOT                
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER PLUS,MINUS,FIELD,DIGITS,BLANK,ZERO,DOT                    
C***** INTEGER *****                                                    
      DIMENSION FIELD(9),DIGITS(10),ZERO(9)                             
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              
      DATA ZERO/' ','0','.','0','0','0','0','+','0'/                    
      DATA PLUS/'+'/                                                    
      DATA MINUS/'-'/                                                   
      DATA BLANK/' '/                                                   
      DATA DOT/'.'/                                                     
C-----IF NUMBER IS ZERO RETURN STANDARD FORM.                           
      XABS=ABS(X)                                                       
      IF(XABS.GT.0.0) GO TO 20                                          
      DO 10 I=1,9                                                       
   10 FIELD(I)=ZERO(I)                                                  
      RETURN                                                            
C-----INITIALIZE OUTPUT FIELD.                                          
   20 DO 30 I=1,9                                                       
   30 FIELD(I)=BLANK                                                    
C-----IF NUMBER IS NEGATIVE INSERT LEADING MINUS SIGN.                  
      IF(X.LT.0.0) FIELD(1)=MINUS                                       
C-----INSERT DECIMAL POINT.                                             
      FIELD(3)=DOT                                                      
C-----DEFINE EXPONENT TO NORMALIZE MANTISSA.                            
      KEXP=ALOG10(XABS)                                                 
      SHIFT=10.0**KEXP                                                  
      XNORM=XABS/SHIFT+0.000051                                         
C-----DEFINE NORMALIZAED MANTISSA.                                      
      IF(XNORM.GE.1.0) GO TO 40                                         
      KEXP=KEXP-1                                                       
      SHIFT=SHIFT/10.0                                                  
      XNORM=XABS/SHIFT+0.000051                                         
      GO TO 50                                                          
   40 IF(XNORM.LT.10.0) GO TO 50                                        
      KEXP=KEXP+1                                                       
      SHIFT=10.0*SHIFT                                                  
      XNORM=XABS/SHIFT+0.000051                                         
C-----SELECT X.XXX+/-NN OR X.XXXX+/-N FORMAT (DEPENDING ON SIZE OF      
C-----EXPONENT).                                                        
   50 KEXPAB=IABS(KEXP)                                                 
      IF(KEXPAB.LE.0) KEXP=0                                            
      IF(KEXPAB.LT.10) GO TO 60                                         
C-----X.XXX+/-NN FORMAT.                                                
      ITOP=6                                                            
      INORM=1000.0*XNORM                                                
      KNORM=1000                                                        
      IDIG=KEXPAB/10                                                    
      FIELD(8)=DIGITS(IDIG+1)                                           
      KEXPAB=KEXPAB-IDIG*10                                             
      GO TO 70                                                          
C-----X.XXXX+/-N FORMAT.                                                
   60 ITOP=7                                                            
      INORM=10000.0*XNORM                                               
      KNORM=10000                                                       
   70 FIELD(9)=DIGITS(KEXPAB+1)                                         
C-----DEFINE SIGN OF EXPONENT.                                          
      IF(KEXP.LT.0) FIELD(ITOP+1)=MINUS                                 
      IF(KEXP.GE.0) FIELD(ITOP+1)=PLUS                                  
C-----CONVERT TO CHARACTERS AND INSERT INTO OUTPUT FIELD                
      DO 80 I=2,ITOP                                                    
C-----SKIP DECIMAL POINT LOCATION.                                      
      IF(I.EQ.3) GO TO 80                                               
      IDIG=INORM/KNORM                                                  
      FIELD(I)=DIGITS(IDIG+1)                                           
      INORM=INORM-IDIG*KNORM                                            
      KNORM=KNORM/10                                                    
   80 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE MFMTIN                                                 
C                                                                       
C     READ EQUIVALENCE TABLE FOR REACTION VS. MF/MT                     
C                                                                       
C     COLUMNS                                                           
C     -------                                                           
C      1- 48    REACTION                                                
C     49- 53    INCIDENT PARTICLE ZA                                    
C     54- 56    MF                                                      
C     57- 60    MT                                                      
C     80- 80    NON-BLANK = COMMENT                                     
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 FLAG,BLANK,DUMMY,R2MFMT                               
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER FLAG,BLANK,DUMMY,R2MFMT                                   
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/MFMTI1/IMFMT                                               
      COMMON/MFMTI2/MFMTAB(7,900)                                       
      COMMON/MFMTC/R2MFMT(48,900)                                       
      DIMENSION DUMMY(24)                                               
      DATA MAXIE/900/                                                   
      DATA BLANK/' '/                                                   
C-----READ ENTIRE FILE. SKIP CARDS WITH NON-BLANK COLUMN 80.            
      DO 20 IMFMT=1,MAXIE                                               
   10 READ(NTAPE1,1000,END=60,ERR=50) (R2MFMT(J,IMFMT),J=1,48),         
     1 DUMMY,FLAG                                                       
      IF(FLAG.NE.BLANK) GO TO 10                                        
      CALL INTGER(DUMMY(1),MFMTAB(1,IMFMT),5)                           
      CALL INTGER(DUMMY(6),MFMTAB(2,IMFMT),3)                           
      CALL INTGER(DUMMY(9),MFMTAB(3,IMFMT),4)                           
      CALL INTGER(DUMMY(13),MFMTAB(4,IMFMT),3)                          
      CALL INTGER(DUMMY(16),MFMTAB(5,IMFMT),3)                          
      CALL INTGER(DUMMY(19),MFMTAB(6,IMFMT),3)                          
      CALL INTGER(DUMMY(22),MFMTAB(7,IMFMT),3)                          
   20 CONTINUE                                                          
      IMFMT=MAXIE                                                       
   30 IMFMT=IMFMT+1                                                     
   40 READ(NTAPE1,1010,END=60,ERR=50) FLAG                              
      IF(FLAG.NE.BLANK) GO TO 40                                        
      GO TO 30                                                          
   50 WRITE(OUTP,6000)                                                  
      CALL GOOUT                                                        
   60 IMFMT=IMFMT-1                                                     
      WRITE(OUTP,6010) IMFMT,MAXIE                                      
      IF(IMFMT.LE.MAXIE) RETURN                                         
      WRITE(OUTP,6020)                                                  
      IMFMT=MAXIE                                                       
      RETURN                                                            
 1000 FORMAT(48A1,24A1,7X,A1)                                           
 1010 FORMAT(79X,A1)                                                    
 6000 FORMAT(' ERROR READING REACTION TABLE...EXECUTION TERMINATED')    
 6010 FORMAT(' REACTIONS------------',I5,' (',I5,' ALLOWED)')           
 6020 FORMAT(' WARNING...ONLY FIRST ',I5,' REACTIONS USED')             
      END                                                               
      SUBROUTINE TITLEI                                                 
C                                                                       
C     READ COLUMN HEADING VS. MF/FIELDS.                                
C                                                                       
C     COLUMNS                                                           
C     -------                                                           
C      1- 48    REACTION                                                
C     49- 53    INCIDENT PARTICLE ZA                                    
C     54- 56    MF                                                      
C     57- 60    MT                                                      
C     80- 80    NON-BLANK = COMMENT                                     
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 TAB78                                                 
      CHARACTER*1 FLAG,BLANK,DUMMY,TITTAB                               
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER FLAG,BLANK,DUMMY,TITTAB,TAB78                             
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/TITTBI/ITITLE,MFTITL(900),MFTITH(900),IFIELD(900),         
     1 ITFLAG(900)                                                      
      COMMON/TITTBC/TAB78(900),TITTAB(11,900)                           
      DIMENSION DUMMY(19)                                               
      DATA BLANK/' '/                                                   
      DATA MAXIE/900/                                                   
C-----READ ENTIRE FILE. SKIP CARDS WITH NON-BLANK COLUMN 80.            
      DO 20 ITITLE=1,MAXIE                                              
   10 READ(NTAPE2,1000,END=60,ERR=50) (TITTAB(J,ITITLE),J=1,11),        
     1 DUMMY,TAB78(ITITLE),FLAG                                         
      IF(FLAG.NE.BLANK) GO TO 10                                        
      CALL INTGER(DUMMY(1),MFTITL(ITITLE),4)                            
      CALL INTGER(DUMMY(5),MFTITH(ITITLE),5)                            
      CALL INTGER(DUMMY(10),IFIELD(ITITLE),5)                           
      CALL INTGER(DUMMY(15),ITFLAG(ITITLE),5)                           
      IF(IFIELD(ITITLE).LT.0.OR.IFIELD(ITITLE).GT.8) IFIELD(ITITLE)=0   
   20 CONTINUE                                                          
      ITITLE=MAXIE                                                      
   30 ITITLE=ITITLE+1                                                   
   40 READ(NTAPE2,1010,END=60,ERR=50) FLAG                              
      IF(FLAG.NE.BLANK) GO TO 40                                        
      GO TO 30                                                          
   50 WRITE(OUTP,6000)                                                  
      CALL GOOUT                                                        
   60 ITITLE=ITITLE-1                                                   
      WRITE(OUTP,6010) ITITLE,MAXIE                                     
      IF(ITITLE.LE.MAXIE) RETURN                                        
      WRITE(OUTP,6020) MAXIE                                            
      ITITLE=MAXIE                                                      
      RETURN                                                            
 1000 FORMAT(11A1,19A1,2X,A3,44X,A1)                                    
 1010 FORMAT(79X,A1)                                                    
 6000 FORMAT(' ERROR READING TITLE TABLE...EXECUTION TERMINATED')       
 6010 FORMAT(' TITLES---------------',I5,' (',I5,' ALLOWED)')           
 6020 FORMAT(' WARNING...ONLY FIRST ',I5,' TITLES USED')                
      END                                                               
      SUBROUTINE UNITI                                                  
C                                                                       
C     READ COLUMN UNITS, STANDARD UNITS AND CONVERSION FACTORS          
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 UNITAB                                                
      CHARACTER*1 FLAG,BLANK,DUMMY                                      
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER FLAG,BLANK,DUMMY,UNITAB                                   
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/UNITBI/IUNIT,TIMES(900),ADD(900),IUFLAG(900)               
      COMMON/UNITBC/UNITAB(6,900)                                       
      DIMENSION DUMMY(33)                                               
      DATA BLANK/' '/                                                   
      DATA MAXIE/900/                                                   
C-----READ ENTIRE FILE. SKIP CARDS WITH NON-BLANK COLUMN 80.            
      DO 20 IUNIT=1,MAXIE                                               
   10 READ(NTAPE3,1000,END=60,ERR=50) (UNITAB(J,IUNIT),J=1,6),          
     1 DUMMY,FLAG                                                       
      IF(FLAG.NE.BLANK) GO TO 10                                        
      CALL FLOATF(DUMMY(1),TIMES(IUNIT))                                
      CALL FLOATF(DUMMY(12),ADD(IUNIT))                                 
      CALL INTGER(DUMMY(23),IUFLAG(IUNIT),11)                           
   20 CONTINUE                                                          
      IUNIT=MAXIE                                                       
   30 IUNIT=IUNIT+1                                                     
   40 READ(NTAPE3,1010,END=60,ERR=50) FLAG                              
      IF(FLAG.NE.BLANK) GO TO 40                                        
      GO TO 30                                                          
   50 WRITE(OUTP,6000)                                                  
      CALL GOOUT                                                        
   60 IUNIT=IUNIT-1                                                     
      WRITE(OUTP,6010) IUNIT,MAXIE                                      
      IF(IUNIT.LE.MAXIE) RETURN                                         
      WRITE(OUTP,6020) MAXIE                                            
      IUNIT=MAXIE                                                       
      RETURN                                                            
 1000 FORMAT(2A4,A3,2A4,A3,33A1,24X,A1)                                 
 1010 FORMAT(79X,A1)                                                    
 6000 FORMAT(' ERROR READING UNITS TABLE...EXECUTION TERMINATED')       
 6010 FORMAT(' UNITS----------------',I5,' (',I5,' ALLOWED)')           
 6020 FORMAT(' WARNING...ONLY FIRST ',I5,' UNITS USED')                 
      END                                                               
      SUBROUTINE MFMTX(REACT,INPART,MF,MT,IRFLAG,KNOWN)                 
C                                                                       
C     DEFINE MF/MT EQUIVALENT OF SIMPLE REACTION.                       
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 ENT,SUBENT,R2MFMT,REACT                               
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER ENT,SUBENT,R2MFMT,REACT                                   
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/WHERE/ENT(5),SUBENT(3)                                     
      COMMON/RESIDI/KZARES                                              
      COMMON/MFMTI1/IMFMT                                               
      COMMON/MFMTI2/MFMTAB(7,900)                                       
      COMMON/MFMTC/R2MFMT(48,900)                                       
      COMMON/POINTR/MPOINT(9)                                           
      DIMENSION REACT(48)                                               
      DO 20 I=1,IMFMT                                                   
      DO 10 J=1,48                                                      
      IF(REACT(J).NE.R2MFMT(J,I)) GO TO 20                              
   10 CONTINUE                                                          
      GO TO 30                                                          
   20 CONTINUE                                                          
C                                                                       
C     REACTION IS NOT DEFINED. WRITE REACTION TO NEWX4 FILE.            
C                                                                       
      INPART=0                                                          
      MF=0                                                              
      MT=0                                                              
      IRFLAG=0                                                          
      KZARES=0                                                          
      KNOWN=0                                                           
      WRITE(NEWX4,4000) ENT,ISAN,REACT                                  
      MPOINT(7)=MPOINT(7)+1                                             
      RETURN                                                            
C                                                                       
C     REACTION IS DEFINED.                                              
C                                                                       
   30 INPART=MFMTAB(1,I)                                                
      MF=MFMTAB(2,I)                                                    
      MT=MFMTAB(3,I)                                                    
      IRFLAG=MFMTAB(4,I)                                                
      KNOWN=1                                                           
C-----ONLY CONSIDER RESIDUAL NUCLEUS FOR PRODUCTION CROSS SECTIONS AND  
C-----ANGULAR DISTRIBUTIONS (MF=3 OR MF=4) AND MT=9000-9999.            
      IF(MF.NE.3.AND.MF.NE.4) KZARES=0                                  
      IF(MT.LT.9000) KZARES=0                                           
      RETURN                                                            
 4000 FORMAT(1X,5A1,I3,1X,15A4)                                         
      END                                                               
      SUBROUTINE TITLEX                                                 
C                                                                       
C     USE REACTION DEFINED MF TO PERMUTE DATA COLUMNS INTO OUTPUT ORDER 
C     FOR ALL REACTIONS.                                                
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 UNIT,TIT78,TAB78                                      
      CHARACTER*1 FLAGI,FLAGR,ZAN,BLANK ,ENT,SUBENT,LABCM,ZANRES,ZANRAT,
     1 DATUM,TITLE,TITTAB                                               
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER FLAGI,FLAGR,ZAN,BLANK ,ENT,SUBENT,LABCM,ZANRES,ZANRAT,    
C    1 DATUM,TITLE,TITTAB,                                              
C    2 UNIT,TIT78,TAB78                                                 
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/WHERE/ENT(5),SUBENT(3)                                     
      COMMON/TITTBI/ITITLE,MFTITL(900),MFTITH(900),IFIELD(900),         
     1 ITFLAG(900)                                                      
      COMMON/TITTBC/TAB78(900),TITTAB(11,900)                           
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  
      COMMON/HEADC1/TITLE(10,60),FLAGI(60),DATUM(11,60)                 
      COMMON/HEADC2/UNIT(3,60),TIT78(60)                                
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     
     1 LABCM(30)                                                        
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
      COMMON/POINTR/MPOINT(9)                                           
      DATA BLANK/' '/                                                   
C-----SET UP LOOP OVER REACTIONS.                                       
      IF(KSANR.LE.0) RETURN                                             
      DO 100 MSANR=1,KSANR                                              
C-----INITIALIZE OUTPUT FIELD VECTORS.                                  
      DO 20 K=1,10                                                      
      DO 10 I=1,8                                                       
   10 IMOUT(I,MSANR,K)=0                                                
   20 CONTINUE                                                          
C-----SELECT COLUMNS WITH COLUMN 11 BLANK OR SAME AS REACTION HAS IN    
C-----COLUMN 11.                                                        
      DO 90 KIN=1,IDATN                                                 
      IF(FLAGI(KIN).EQ.BLANK) GO TO 30                                  
      IF(FLAGI(KIN).NE.FLAGR(MSANR)) GO TO 90                           
C-----SELECT TITLES FROM TABLE FOR WHICH MF RANGE SPANS MF OF REACTION. 
   30 DO 50 KTAB=1,ITITLE                                               
      IF(MFR(MSANR).LT.MFTITL(KTAB).OR.                                 
     1 MFR(MSANR).GT.MFTITH(KTAB)) GO TO 50                             
      DO 40 L=1,10                                                      
      IF(TITLE(L,KIN).NE.TITTAB(L,KTAB)) GO TO 50                       
   40 CONTINUE                                                          
      GO TO 60                                                          
   50 CONTINUE                                                          
C                                                                       
C     TITLE IS NOT DEFINED. WRITE TITLE TO NEWX4 FILE.                  
C                                                                       
      WRITE(NEWX4,4000) ENT,ISAN,(TITLE(L,KIN),L=1,10)                  
      MPOINT(8)=MPOINT(8)+1                                             
      GO TO 90                                                          
C                                                                       
C     TITLE IS DEFINED. DEFINE POSITION OF OUTPUT FIELD (SKIP IF        
C     NOT USED IN OUTPUT).                                              
C                                                                       
   60 KOUT=IFIELD(KTAB)                                                 
      IF(KOUT.LE.0) GO TO 90                                            
C-----DEFINE FIELDS 7-8.                                                
      TIT78(KIN)=TAB78(KTAB)                                            
C-----SAVE INPUT FIELD INDEX IN NEXT AVAILABLE OUTPUT FIELD LOCATION.   
      DO 70 JMULT=1,10                                                  
      IF(IMOUT(KOUT,MSANR,JMULT).LE.0) GO TO 80                         
   70 CONTINUE                                                          
      JMULT=10                                                          
   80 IMOUT(KOUT,MSANR,JMULT)=KIN                                       
C-----DEFINE TITLE OPERATION FLAG FOR INPUT COLUMN.                     
      KTFLGX(KIN)=ITFLAG(KTAB)                                          
C-----INDICATE INPUT FIELD USED (TO FORCE HOLLERITH TO FLOATING POINT   
C-----TRANSLATION).                                                     
      IMUSED(KIN)=1                                                     
   90 CONTINUE                                                          
  100 CONTINUE                                                          
      RETURN                                                            
 4000 FORMAT(1X,5A1,I3,1X,10A1)                                         
      END                                                               
      SUBROUTINE TOPS1                                                  
C                                                                       
C     PERFORM TITLE OPERATIONS THAT APPLY TO ENTIRE TABLE,              
C     (1) SET CENTER-OF-MASS FLAG.                                      
C     (2) DEFINE -MIN OR -MAX TO CREATE A PAIR (CREATE -MIN= 0,         
C         OR -MAX = 15 MEV).                                            
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 UNIT,EV,TIT78                                         
      CHARACTER*1 FLAGI,FLAGR,ZAN,LABCM,CENTER,ZANRES,ZANRAT,DATUM,     
     1 BLANK,LIMITS,TITLE,TITLE2,TITLE3                                 
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER FLAGI,FLAGR,ZAN,LABCM,CENTER,ZANRES,ZANRAT,DATUM,         
C    1 BLANK,LIMITS,TITLE,TITLE2,TITLE3,                                
C    2 UNIT,EV,TIT78                                                    
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  
      COMMON/HEADC1/TITLE(10,60),FLAGI(60),DATUM(11,60)                 
      COMMON/HEADC2/UNIT(3,60),TIT78(60)                                
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     
     1 LABCM(30)                                                        
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
      DIMENSION TITLE2(10),TITLE3(10),EV(3),LIMITS(11,2)                
      DATA CENTER/'C'/                                                  
      DATA BLANK/' '/                                                   
      DATA EV/'EV  ','    ','   '/                                      
      DATA LIMITS/                                                      
     1 ' ','0','.','0',' ',' ',' ',' ',' ',' ',' ',                     
     1 ' ','1','.','5','0','0','0','0','+',' ','7'/                     
      KDATN=IDATN                                                       
      DO 130 MSANR=1,KSANR                                              
C-----INITIALIZE SYSTEM FLAG TO BLANK.                                  
      LABCM(MSANR)=BLANK                                                
      DO 120 KFIELD=1,8                                                 
      DO 110 KMULT=1,10                                                 
      KIN=IMOUT(KFIELD,MSANR,KMULT)                                     
      IF(KIN.LE.0) GO TO 120                                            
C-----ONLY CONSIDER FIELDS THAT ARE REQUIRED FOR OUTPUT.                
      IF(IMUSED(KIN).LE.0) GO TO 110                                    
C-----IF REQUESTED SET CENTER-OF-MASS SYSTEM FLAG.                      
      IF(KTFLGX(KIN).NE.6) GO TO 10                                     
      LABCM(MSANR)=CENTER                                               
      WRITE(OUTP,6030)                                                  
      GO TO 110                                                         
C-----SEE IF -MIN AND -MAX MUST APPEAR IN PAIR.                         
   10 IF(KTFLGX(KIN).NE.9) GO TO 110                                    
C-----DECODE TITLE TO DEFINE COMPLEMENTARY TITLE AND WHETHER TITLE      
C-----ENDS IN -MIN, -MAX OR OTHER (ERROR).                              
      CALL IPAIR(TITLE(1,KIN),TITLE2,IWAY)                              
      IF(IWAY.GT.0) GO TO 20                                            
C-----ERROR. TITLE DOES NOT END IN -MIN OR -MAX.                        
      WRITE(OUTP,6000) (TITLE(J,KIN),J=1,10),FLAGI(KIN)                 
      GO TO 110                                                         
C-----TITLE ENDS IN -MIN OR -MAX. SCAN REMAINING TITLES FOR OTHER       
C-----LIMIT AND SAME TITLE FLAG.                                        
   20 JIN=KIN+1                                                         
      IF(JIN.GT.KDATN) GO TO 70                                         
      DO 60 K=JIN,KDATN                                                 
      IF(IMUSED(K).LE.0.OR.KTFLGX(K).NE.9) GO TO 60                     
      IF(FLAGI(K).NE.FLAGI(KIN)) GO TO 60                               
      CALL IPAIR(TITLE(1,K),TITLE3,KWAY)                                
      IF(KWAY.EQ.0) GO TO 60                                            
      IF(IWAY.NE.KWAY) GO TO 40                                         
C-----POSSIBLE MULTIPLE SAME LIMIT. CHECK FOR SAME TITLE.               
      DO 30 J=1,10                                                      
      IF(TITLE(J,KIN).NE.TITLE3(J)) GO TO 110                           
   30 CONTINUE                                                          
      WRITE(OUTP,6010) (TITLE(J,KIN),J=1,10),FLAGI(KIN)                 
      GO TO 110                                                         
C-----SEE IF RECONSTRUCTED TITLE TITLE IS SAME AS ORIGINAL              
C-----(E.G., AVOID ASUMMING EN-MIN AND E-MAX ARE A PAIR).               
   40 DO 50 J=1,10                                                      
      IF(TITLE(J,KIN).NE.TITLE3(J)) GO TO 60                            
   50 CONTINUE                                                          
C-----LIMITS ARE PAIRED.                                                
      GO TO 110                                                         
   60 CONTINUE                                                          
C-----LIMITS ARE NOT PAIRED. CREATE DATA POINT.                         
   70 IDATN=IDATN+1                                                     
      KWAY=3-IWAY                                                       
      DO 80 J=1,10                                                      
   80 TITLE(J,IDATN)=TITLE2(J)                                          
      DO 90 J=1,3                                                       
   90 UNIT(J,IDATN)=EV(J)                                               
      DO 100 J=1,11                                                     
  100 DATUM(J,IDATN)=LIMITS(J,KWAY)                                     
      FLAGI(IDATN)=FLAGI(KIN)                                           
      IMUSED(IDATN)=1                                                   
      WRITE(OUTP,6020) (TITLE(J,IDATN),J=1,10),FLAGI(IDATN),            
     1 (DATUM(J,IDATN),J=1,11),(UNIT(J,IDATN),J=1,3)                    
C-----SET INDEX TO OUTPUT CREATED LIMIT NEXT TO EXISTING LIMIT.         
      IF(IWAY.EQ.1) IMOUT(KFIELD+1,MSANR,KMULT)=IDATN                   
      IF(IWAY.EQ.2) IMOUT(KFIELD-1,MSANR,KMULT)=IDATN                   
  110 CONTINUE                                                          
  120 CONTINUE                                                          
  130 CONTINUE                                                          
      RETURN                                                            
 6000 FORMAT(10X,'WARNING.....CHECK -MIN/-MAX FLAG FOR ',11A1)          
 6010 FORMAT(10X,'WARNING.....MULTIPLE -MIN/-MAX FIELDS ',11A1)         
 6020 FORMAT(10X,'OPERATION...CREATED ',11A1,1X,11A1,1X,2A4,A3)         
 6030 FORMAT(10X,'OPERATION...CENTER-OF-MASS SYSTEM FLAG SET')          
      END                                                               
      SUBROUTINE IPAIR(TITLE1,TITLE2,IWAY)                              
C                                                                       
C     SEARCH TITLE1 FOR ENDING OF -MIN OR -MAX.                         
C     IF FOUND, CREATE TITLE2 TO BE OTHER LIMIT.                        
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*1 TITLE1,TITLE2,MINMAX,BLANK                            
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER TITLE1,TITLE2,MINMAX,BLANK                                
C***** INTEGER *****                                                    
      DIMENSION TITLE1(10),TITLE2(10),MINMAX(4,2)                       
      DATA MINMAX/                                                      
     1 '-','M','I','N',                                                 
     2 '-','M','A','X'/                                                 
      DATA BLANK/' '/                                                   
C-----FIND LAST NON-BLANK CHARACTER.                                    
      II=11                                                             
      DO 10 I=1,10                                                      
      II=II-1                                                           
      IF(TITLE1(II).NE.BLANK) GO TO 20                                  
   10 CONTINUE                                                          
      GO TO 50                                                          
C-----SEARCH FOR -MIN OR -MAX.                                          
   20 DO 40 IWAY=1,2                                                    
      JJ=4                                                              
      KK=II                                                             
      DO 30 J=1,4                                                       
      IF(TITLE1(KK).NE.MINMAX(JJ,IWAY)) GO TO 40                        
      JJ=JJ-1                                                           
   30 KK=KK-1                                                           
      GO TO 60                                                          
   40 CONTINUE                                                          
C----- -MIN/-MAX NOT FOUND.                                             
   50 IWAY=0                                                            
      RETURN                                                            
C----- -MIN/-MAX FOUND. DEFINE COMPLEMENTARY TITLE.                     
   60 II=II-4                                                           
      KWAY=3-IWAY                                                       
      DO 70 I=1,II                                                      
   70 TITLE2(I)=TITLE1(I)                                               
      DO 80 I=1,4                                                       
      II=II+1                                                           
   80 TITLE2(II)=MINMAX(I,KWAY)                                         
      IF(II.GE.11) GO TO 100                                            
      II=II+1                                                           
      DO 90 I=II,10                                                     
   90 TITLE2(I)=BLANK                                                   
  100 RETURN                                                            
      END                                                               
      SUBROUTINE UNIT1                                                  
C                                                                       
C     DEFINE UNIT CONVERSION FACTORS AND OPERATIONS.                    
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 UNITAB,UNIT,TIT78                                     
      CHARACTER*1 ENT,SUBENT,STAT1,STATN,UNNORM,DATUM,TITLE,FLAGI       
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER ENT,SUBENT,STAT1,STATN,UNNORM,DATUM,TITLE,FLAGI,          
C    1 UNITAB,UNIT,TIT78                                                
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/WHERE/ENT(5),SUBENT(3)                                     
      COMMON/UNITBI/IUNIT,TIMES(900),ADD(900),IUFLAG(900)               
      COMMON/UNITBC/UNITAB(6,900)                                       
      COMMON/HEADC1/TITLE(10,60),FLAGI(60),DATUM(11,60)                 
      COMMON/HEADC2/UNIT(3,60),TIT78(60)                                
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
      COMMON/STATUC/STAT1,STATN                                         
      COMMON/POINTR/MPOINT(9)                                           
      DATA UNNORM/'U'/                                                  
C                                                                       
C     ONLY CONSIDER FIELDS THAT ARE REQUIRED FOR OUTPUT.                
C                                                                       
      DO 30 I=1,IDATN                                                   
      IF(IMUSED(I).LE.0) GO TO 30                                       
C-----DETERMINE CONVERSION FACTOR FOR UNITS.                            
      DO 20 J=1,IUNIT                                                   
      DO 10 K=1,3                                                       
      IF(UNIT(K,I).NE.UNITAB(K,J)) GO TO 20                             
   10 CONTINUE                                                          
C-----TITLE IS DEFINED. DEFINE MULTIPLIER, ADDER AND UNIT OPERATION.    
      TIMEX(I)=TIMES(J)                                                 
      ADDX(I)=ADD(J)                                                    
      KUFLGX(I)=IUFLAG(J)                                               
C-----IF REQUESTED PRINT WARNING MESSAGE.                               
      IF(IUFLAG(J).EQ.7) WRITE(OUTP,6000) (UNIT(K,I),K=1,3)             
      IF(IUFLAG(J).NE.8) GO TO 30                                       
      WRITE(OUTP,6010) (UNIT(K,I),K=1,3)                                
      STATN=UNNORM                                                      
      GO TO 30                                                          
   20 CONTINUE                                                          
C                                                                       
C     UNITS IS NOT DEFINED. WRITE TITLE TO NEWX4 FILE.                  
C                                                                       
      TIMEX(I)=0.0                                                      
      ADDX(I)=0.0                                                       
      KUFLGX(I)=0                                                       
      WRITE(NEWX4,4000) ENT,ISAN,(UNIT(K,I),K=1,3)                      
      MPOINT(9)=MPOINT(9)+1                                             
   30 CONTINUE                                                          
      RETURN                                                            
 4000 FORMAT(1X,5A1,I3,1X,2A4,A3)                                       
 6000 FORMAT(10X,'WARNING.....UNITS=',2A4,A3)                           
 6010 FORMAT(10X,'WARNING.....UNITS=',2A4,A3,' STATUS CHANGED TO',      
     1 ' UNNORMALIZED (U)')                                             
      END                                                               
      SUBROUTINE UNIT2                                                  
C                                                                       
C     TRANSLATE FIELDS TO STANDARD UNITS.                               
C                                                                       
C***** CHARACTER *****                                                  
      CHARACTER*4 UNIT,TIT78                                            
      CHARACTER*1 DATUM,TITLE,FLAGI                                     
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER UNIT,TIT78,DATUM,TITLE,FLAGI                              
C***** INTEGER *****                                                    
      COMMON/RNOW/ISANR                                                 
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          
      COMMON/HEADC1/TITLE(10,60),FLAGI(60),DATUM(11,60)                 
      COMMON/HEADC2/UNIT(3,60),TIT78(60)                                
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
      COMMON/INVAL/VALUEI(50)                                           
C                                                                       
C     ONLY TRANSLATE FIELDS THAT ARE REQUIRED FOR OUTPUT.               
C                                                                       
      DO 40 KFIELD=1,8                                                  
      DO 30 KMULT=1,10                                                  
      II=IMOUT(KFIELD,ISANR,KMULT)                                      
      IF(II.EQ.0) GO TO 40                                              
      IF(IMUSED(II)-1) 40,10,20                                         
C-----CONVERT DATA FROM HOLLERITH TO FLOATING POINT.                    
   10 CALL FLOATF(DATUM(1,II),VALUEI(II))                               
      IMUSED(II)=2                                                      
C-----APPLY CONVERSION FACTORS.                                         
   20 VALUES(II)=TIMEX(II)*VALUEI(II)+ADDX(II)                          
   30 CONTINUE                                                          
   40 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE UNOPS                                                  
C                                                                       
C     APPLY UNIT CONVERSION OPTIONS,                                    
C                                                                       
C     (1) PER-CENT TO ABSOLUTE                                          
C     (2) ANGLE TO COSINE (ANGLE OR ANGULAR ERROR)                      
C     (3) RESOLUTION (E.G. NSEC/M) TO ENERGY ERROR (EV).                
C     (4) ANGSTROM TO EV.                                               
C     (5) LENGTH (CM OR FERMI) TO AREA (BARNS)                          
C     (6) BARNS*SQRT(E) TO BARNS                                        
C                                                                       
      INTEGER OUTP,OTAPE                                                
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/RNOW/ISANR                                                 
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
      DIMENSION KUFLG1(50)                                              
      DATA PI/3.141597/                                                 
      DATA RES2EV/2.77E-5/                                              
C-----DEFINE UNIT OPERATIONS FOR INTERNAL USE.                          
      DO 10 I=1,IDATN                                                   
   10 KUFLG1(I)=KUFLGX(I)                                               
C-----SET UP LOOP OVER OUTPUT FIELDS.                                   
      DO 140 KFIELD=1,8                                                 
C-----SET UP LOOP OVER EXFOR FIELDS MAPPED INTO OUTPUT FIELD.           
      DO 130 JMULT=1,10                                                 
C-----DETERMINE IF OUTPUT FIELD IS USED, AND IF SO WHEATHER OR NOT      
C-----TO PERFORM AN OPERATION ON IT.                                    
      II=IMOUT(KFIELD,ISANR,JMULT)                                      
      IF(II.LE.0) GO TO 140                                             
      IF(KUFLG1(II).LE.0) GO TO 140                                     
C                                                                       
C     PERFORM PER-CENT TO ABSOLUTE CONVERSION.                          
C                                                                       
C     TO CONVERT FROM PER-CENT TO ABSOLUTE MULTIPLY THE OUTPUT FIELD    
C     BY 0.01 TIMES THE PRECEDING OUTPUT FIELD (THIS WILL WORK FOR      
C     ENERGY FOLLOWED BY ENERGY ERROR, DATA FOLLOWED BY DATA ERROR,ETC.)
C                                                                       
      IF(KUFLG1(II).NE.1) GO TO 30                                      
      JJ=IMOUT(KFIELD-1,ISANR,1)                                        
      IF(JJ.GT.0) GO TO 20                                              
      IF(NPT.EQ.1) WRITE(OUTP,6000)                                     
      VALUES(II)=0.0                                                    
      GO TO 120                                                         
   20 VALUES(II)=ABS(0.01*VALUES(II)*VALUES(JJ))                        
      IF(NPT.EQ.1) WRITE(OUTP,6040)                                     
      GO TO 120                                                         
C                                                                       
C     PERFORM ANGLE TO COSINE CONVERSION.                               
C                                                                       
   30 IF(KUFLG1(II).NE.2) GO TO 60                                      
      IF(KFIELD.EQ.5) GO TO 50                                          
C                                                                       
C     CONVERT ANGULAR RESOLUTION TO COSINE RESOLUTION.                  
C                                                                       
C     DEFINE COSINE RESOLUTION TO BE,                                   
C                                                                       
C     DMU = ABS(COS(ANGLE+DANGLE)-COS(ANGLE))+                          
C           ABS(COS(ANGLE-DANGLE)-COS(ANGLE)))/2.0                      
C                                                                       
      JJ=IMOUT(KFIELD-1,ISANR,1)                                        
      IF(JJ.GT.0) GO TO 40                                              
      IF(NPT.EQ.1) WRITE(OUTP,6010)                                     
      VALUES(II)=0.0                                                    
      GO TO 120                                                         
   40 XMU=VALUES(JJ)                                                    
      ANG=ACOS(XMU)                                                     
      DANG=PI*VALUES(II)/180.0                                          
      DMUP=COS(ANG+DANG)                                                
      DMUM=COS(ANG-DANG)                                                
      VALUES(II)=0.5*(ABS(DMUP-XMU)+ABS(DMUM-XMU))                      
      IF(NPT.EQ.1) WRITE(OUTP,6055)                                     
      GO TO 120                                                         
C-----CONVERT ANGLE TO COSINE.                                          
   50 VALUES(II)=COS(PI*VALUES(II)/180.0)                               
C-----ADJUST FOR EXACTLY 90 DEGREES (COSINE MAY DIFFER SLIGHTLY FROM    
C-----ZERO DUE TO APPROXIMATION OF PI TO ACCURACY OF COMPUTER).         
      IF(ABS(VALUES(II)).LT.0.00005) VALUES(II)=0.0                     
      IF(NPT.EQ.1) WRITE(OUTP,6050)                                     
      GO TO 120                                                         
C                                                                       
C     PERFORM RESOLUTION (E.G. NSEC/M) TO ENERGY ERROR (EV).            
C                                                                       
   60 IF(KUFLG1(II).NE.3) GO TO 80                                      
      JJ=IMOUT(1,ISANR,1)                                               
      IF(JJ.GT.0) GO TO 70                                              
      IF(NPT.EQ.1) WRITE(OUTP,6020)                                     
      VALUES(II)=0.0                                                    
      GO TO 120                                                         
   70 XE=VALUES(JJ)                                                     
      VALUES(II)=RES2EV*VALUES(II)*XE*SQRT(XE)                          
      IF(NPT.EQ.1) WRITE(OUTP,6060)                                     
      GO TO 120                                                         
C                                                                       
C     PERFORM ANGSTROM TO EV CONVERSION.                                
C                                                                       
   80 IF(KUFLG1(II).NE.4) GO TO 90                                      
      XE=VALUES(II)                                                     
      IF(XE.LE.0.0) GO TO 130                                           
      VALUES(II)=(8.18E+10)/(XE*XE)                                     
      IF(NPT.EQ.1) WRITE(OUTP,6070)                                     
      GO TO 120                                                         
C                                                                       
C     PERFORM LENGTH (CM OR FERMI) TO AREA (BARNS) CONVERSION.          
C                                                                       
   90 IF(KUFLG1(II).NE.5) GO TO 100                                     
      VALUES(II)=4.0*PI*VALUES(II)*VALUES(II)                           
      IF(NPT.EQ.1) WRITE(OUTP,6080)                                     
      GO TO 120                                                         
C                                                                       
C     PERFORM BARNS*SQRT(E) TO BARNS CONVERSION.                        
C                                                                       
  100 IF(KUFLG1(II).NE.6) GO TO 130                                     
      JJ=IMOUT(1,ISANR,1)                                               
      IF(JJ.GT.0) GO TO 110                                             
      IF(NPT.EQ.1) WRITE(OUTP,6030)                                     
      VALUES(II)=0.0                                                    
      GO TO 120                                                         
  110 XE=VALUES(JJ)                                                     
      IF(XE.LE.0.0) GO TO 130                                           
      VALUES(II)=VALUES(II)/SQRT(XE)                                    
      IF(NPT.EQ.1) WRITE(OUTP,6090)                                     
C                                                                       
C     TURN OFF FLAG TO INSURE OPERATION IS ONLY PERFORMED ONCE ON EACH  
C     INPUT VALUE.                                                      
C                                                                       
  120 KUFLG1(II)=0                                                      
  130 CONTINUE                                                          
  140 CONTINUE                                                          
      CONTINUE                                                          
      RETURN                                                            
 6000 FORMAT(10X,'WARNING.....CANNOT CONVERT PER-CENT TO ABSOLUTE'/     
     1 10X,'REQUIRED PRECEDING DATA FIELD NOT DEFINED')                 
 6010 FORMAT(10X,'WARNING.....CANNOT CONVERT ANGLE TO COSINE ERROR'/    
     1 10X,'REQUIRED COSINE FIELD NOT DEFINED')                         
 6020 FORMAT(10X,'WARNING.....CANNOT CONVERT RESOLUTION TO ERROR'/      
     1 10X,'REQUIRED ENERGY FIELD NOT DEFINED')                         
 6030 FORMAT(10X,'WARNING.....CANNOT CONVERT BARNS*SQRT(E) TO BARNS'/   
     1 10X,'REQUIRED ENERGY FIELD NOT DEFINED')                         
 6040 FORMAT(10X,'OPERATION...CONVERTED PER-CENT TO ABSOLUTE')          
 6050 FORMAT(10X,'OPERATION...CONVERTED ANGLES TO COSINES')             
 6055 FORMAT(10X,'OPERATION...CONVERTED ANGULAR ERROR TO COSINE ERROR') 
 6060 FORMAT(10X,'OPERATION...CONVERTED RESOLUTION TO ERROR')           
 6070 FORMAT(10X,'OPERATION...CONVERTED ANGSTROM TO ENERGY')            
 6080 FORMAT(10X,'OPERATION...CONVERTED LENGTH TO BARNS')               
 6090 FORMAT(10X,'OPERATION...CONVERTED BARNS*SQRT(E) TO BARNS')        
      END                                                               
      SUBROUTINE TOPS2                                                  
C                                                                       
C     RESOLVE MULTIPLE INPUT FIELDS MAPPED INTO A SINGLE OUTPUT FIELD.  
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*4 UNIT,TIT78                                            
      CHARACTER*1 FLAGI,DATUM,TITLE                                     
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER FLAGI,DATUM,TITLE,UNIT,TIT78                              
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/RNOW/ISANR                                                 
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/HEADC1/TITLE(10,60),FLAGI(60),DATUM(11,60)                 
      COMMON/HEADC2/UNIT(3,60),TIT78(60)                                
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
C-----INITIALIZE FIELD SKIP FLAG.                                       
      ISKIP=0                                                           
C-----SET UP LOOP OVER OUTPUT FIELDS.                                   
      DO 310 KFIELD=1,8                                                 
C-----CHECK FOR CURRENT FIELD DEFINED BY PRECEDING FIELD.               
      IF(ISKIP.LE.0) GO TO 10                                           
      ISKIP=0                                                           
      GO TO 310                                                         
C-----COUNT THE NUMBER OF INPUT FIELDS MAPPED INTO 1 OUTPUT FIELD.      
   10 DO 20 JMULT=1,10                                                  
      II=IMOUT(KFIELD,ISANR,JMULT)                                      
      IF(II.LE.0) GO TO 30                                              
   20 CONTINUE                                                          
      JMULT=10                                                          
C                                                                       
C     ATTEMPT TO RESOLVE MULTIPLE FIELD DEFINITION (IF ANY).            
C                                                                       
   30 IF(JMULT.LE.2) GO TO 290                                          
      JMULT=JMULT-1                                                     
C-----ONLY PRINT MULTIPLE FIELD WARNING MESSAGE FOR FIRST POINT.        
      IF(NPT.GT.1) GO TO 50                                             
      WRITE(OUTP,6000)                                                  
      DO 40 KMULT=1,JMULT                                               
      JJ=IMOUT(KFIELD,ISANR,KMULT)                                      
   40 WRITE(OUTP,6020) (TITLE(J,JJ),J=1,10),FLAGI(JJ)                   
C-----USE TITLE FLAG TO (1) ALWAYS CHOOSE.                              
   50 DO 60 KMULT=1,JMULT                                               
      JJ=IMOUT(KFIELD,ISANR,KMULT)                                      
      IF(KTFLGX(JJ).EQ.1) GO TO 90                                      
   60 CONTINUE                                                          
C-----USE TITLE FLAG TO (2) CHOOSE FIRST (3) NEVER CHOOSE.              
      KK=0                                                              
      DO 70 KMULT=1,JMULT                                               
      JJ=IMOUT(KFIELD,ISANR,KMULT)                                      
      IF(KTFLGX(JJ).EQ.2) GO TO 90                                      
      IF(KTFLGX(JJ).EQ.3) GO TO 70                                      
      KK=KK+1                                                           
      IMOUT(KFIELD,ISANR,KK)=IMOUT(KFIELD,ISANR,KMULT)                  
   70 CONTINUE                                                          
      JMULT=KK                                                          
C-----SET NEXT FIELD TO ZERO TO ELIMINATE ALL (3) NEVER CHOOSE FIELDS   
C-----FOR ALL POINTS IN TABLE.                                          
      IF(JMULT.LT.10) IMOUT(KFIELD,ISANR,JMULT+1)=0                     
      IF(JMULT-1) 80,90,100                                             
C-----NO FIELDS LEFT. SET INDEX FOR NO OUTPUT.                          
   80 JJ=0                                                              
      IF(NPT.EQ.1) WRITE(OUTP,6090)                                     
      GO TO 300                                                         
C-----IF (1) ALWAYS CHOOSE OR (2) CHOOSE FIRST FIELD OR ONLY ONE        
C-----FIELD LEFT NO MORE CONFLICT FOR ALL POINTS IN TABLE.              
   90 IMOUT(KFIELD,ISANR,1)=JJ                                          
      IMOUT(KFIELD,ISANR,2)=0                                           
      IF(NPT.EQ.1) WRITE(OUTP,6080) (TITLE(J,JJ),J=1,10),FLAGI(JJ)      
      GO TO 300                                                         
C-----SEE IF ALL REMAINING FIELDS HAVE THE SAME TITLE FLAG.             
  100 JOPS=0                                                            
      DO 110 KMULT=1,JMULT                                              
      JJ=IMOUT(KFIELD,ISANR,KMULT)                                      
C-----ERROR IF USER DOES NOT SPECIFY HOW TO RESOLVE.                    
      IF(KTFLGX(JJ).LE.0) GO TO 260                                     
      IF(JOPS.LE.0) JOPS=KTFLGX(JJ)                                     
      IF(KTFLGX(JJ).NE.JOPS) GO TO 260                                  
  110 CONTINUE                                                          
C-----SEE IF ALL REMAINING FIELDS SHOULD BE (4) USED TO SELECT LARGEST  
C-----(5) COMBINE QUADRATICALLY.                                        
      IF(JOPS.GT.5) GO TO 140                                           
      IF(NPT.EQ.1.AND.JOPS.EQ.4) WRITE(OUTP,6100)                       
      IF(NPT.EQ.1.AND.JOPS.EQ.5) WRITE(OUTP,6110)                       
C-----CHOOSE LARGEST OR COMBINE QUADRATICALLY.                          
      DXDX=0.0                                                          
      DO 130 KMULT=1,JMULT                                              
      II=IMOUT(KFIELD,ISANR,KMULT)                                      
      DX=ABS(VALUES(II))                                                
      DXDX=DXDX+DX*DX                                                   
      IF(KMULT.EQ.1) GO TO 120                                          
      IF(DX.LE.DXMAX) GO TO 130                                         
  120 JJ=II                                                             
      DXMAX=DX                                                          
  130 CONTINUE                                                          
C-----SELECT LARGEST OR QUADRATIC COMBINATION.                          
      IF(JOPS.EQ.4) GO TO 300                                           
      DX=SQRT(DXDX)                                                     
      IDATN=IDATN+1                                                     
      VALUES(IDATN)=DX                                                  
      JJ=IDATN                                                          
C-----SAVE DEFINITION OF FIELDS 7-8.                                    
      IF(KFIELD.NE.7) GO TO 300                                         
      II=IMOUT(KFIELD,ISANR,1)                                          
      TIT78(IDATN)=TIT78(II)                                            
      GO TO 300                                                         
C-----SEE IF DATA ARE TO BE COMBINED TO DEFINE AVERAGE AND SPREAD.      
  140 IF(JOPS.EQ.7) GO TO 150                                           
      IF(JOPS.GT.8) GO TO 190                                           
C-----CANNOT COMBINE FIELDS TO DEFINE AVERAGE AND ERROR IF CURRENT      
C-----OUTPUT FIELD IS 8 (I.E. NO OUTPUT FIELD 9) OR IF THE NEXT FIELD   
C-----IS ALREADY USED.                                                  
      IF(KFIELD.LT.8.AND.IMOUT(KFIELD+1,ISANR,1).LE.0) GO TO 150        
      IF(NPT.GT.1) GO TO 290                                            
      KFP1=KFIELD+1                                                     
      IF(KFIELD.EQ.8) WRITE(OUTP,6030) KFIELD                           
      IF(IMOUT(KFP1,ISANR,1).GT.1) WRITE(OUTP,6040) KFIELD,KFP1         
      GO TO 270                                                         
C-----COMBINE FIELDS TO DEFINE AVERAGE.                                 
  150 IF(NPT.EQ.1.AND.JOPS.EQ.7) WRITE(OUTP,6120)                       
      IF(NPT.EQ.1.AND.JOPS.EQ.8) WRITE(OUTP,6130)                       
      ZJMULT=JMULT                                                      
      AVER=0.0                                                          
      DO 160 KMULT=1,JMULT                                              
      II=IMOUT(KFIELD,ISANR,KMULT)                                      
  160 AVER=AVER+VALUES(II)                                              
      AVER=AVER/ZJMULT                                                  
      IDATN=IDATN+1                                                     
      VALUES(IDATN)=AVER                                                
      KMOUT(KFIELD,ISANR)=IDATN                                         
C-----SAVE DEFINITION OF FIELDS 7-8.                                    
      IF(KFIELD.NE.7) GO TO 170                                         
      II=IMOUT(KFIELD,ISANR,1)                                          
      TIT78(IDATN)=TIT78(II)                                            
  170 IF(JOPS.EQ.7) GO TO 310                                           
C-----DEFINE COMBINED ERROR.                                            
      ERRAV=0.0                                                         
      DO 180 KMULT=1,JMULT                                              
      II=IMOUT(KFIELD,ISANR,KMULT)                                      
  180 ERRAV=ERRAV+ABS(VALUES(II)-AVER)                                  
      ERRAV=ERRAV/ZJMULT                                                
      IDATN=IDATN+1                                                     
      VALUES(IDATN)=ERRAV                                               
      KMOUT(KFIELD+1,ISANR)=IDATN                                       
C-----SET FLAG TO SKIP NEXT FIELD (ERROR FIELD ALREADY DEFINED).        
      ISKIP=1                                                           
      GO TO 310                                                         
C-----SELECT SMALLEST AND LARGEST IN 2 SUCCESSIVE FIELDS.               
  190 IF(JOPS.NE.10) GO TO 260                                          
C-----CANNOT SELECT SMALLEST AND LARGEST IF CURRENT OUTPUT FIELD IS     
C-----8 (I.E. NO OUTPUT FIELD 9) OR IF THE NEXT FIELD IS ALREADY USED.  
      IF(KFIELD.LT.8.AND.IMOUT(KFIELD+1,ISANR,1).LE.0) GO TO 200        
      IF(NPT.GT.1) GO TO 290                                            
      KFP1=KFIELD+1                                                     
      IF(KFIELD.EQ.8) WRITE(OUTP,6060) KFIELD                           
      IF(IMOUT(KFP1,ISANR,1).GT.1) WRITE(OUTP,6070) KFIELD,KFP1         
      GO TO 270                                                         
C-----SELECT SMALLEST AND LARGEST IN 2 SUCCESSIVE FIELDS.               
  200 IF(NPT.EQ.1) WRITE(OUTP,6140)                                     
      DO 250 KMULT=1,JMULT                                              
      II=IMOUT(KFIELD,ISANR,KMULT)                                      
      DX=ABS(VALUES(II))                                                
      IF(KMULT.EQ.1) GO TO 210                                          
      IF(DX-DXMIN) 220,230,230                                          
  210 JH=II                                                             
      DXMAX=DX                                                          
  220 JL=II                                                             
      DXMIN=DX                                                          
  230 IF(DX-DXMAX) 250,250,240                                          
  240 JH=II                                                             
      DXMAX=DX                                                          
  250 CONTINUE                                                          
C-----DEFINE INDICES TO SMALLEST AND LARGEST VALUES.                    
      KMOUT(KFIELD,ISANR)=JL                                            
      KMOUT(KFIELD+1,ISANR)=JH                                          
C-----SET FLAG TO SKIP NEXT FIELD (ERROR FIELD ALREADY DEFINED).        
      ISKIP=1                                                           
      GO TO 310                                                         
C                                                                       
C     CANNOT RESOLVE MULTIPLE FIELD DEFINITION. PRINT ERROR MESSAGE     
C     WHEN PROCESSING FIRST POINT OF TABLE.                             
C                                                                       
  260 IF(NPT.GT.1) GO TO 290                                            
      WRITE(OUTP,6010) KFIELD                                           
  270 WRITE(OUTP,6050)                                                  
      DO 280 KMULT=1,JMULT                                              
      JJ=IMOUT(KFIELD,ISANR,KMULT)                                      
  280 WRITE(OUTP,6020) (TITLE(J,JJ),J=1,10),FLAGI(JJ)                   
C-----USE FIRST INPUT FIELD FOR OUTPUT.                                 
  290 JJ=IMOUT(KFIELD,ISANR,1)                                          
C-----DEFINE UNIQUE INPUT FIELD INDEX TO MAP INTO OUTPUT FIELD.         
  300 KMOUT(KFIELD,ISANR)=JJ                                            
  310 CONTINUE                                                          
      RETURN                                                            
 6000 FORMAT(10X,'WARNING.....CHECK MULTIPLE FIELD DEFINITION')         
 6010 FORMAT(10X,'WARNING.....FIELD=',I2,' UNRESOLVED MULTIPLE FIELDS') 
 6020 FORMAT(10X,11A1)                                                  
 6030 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT COMBINE FIELDS TO',   
     1       10X,'DEFINE AVERAGE FOLLOWED BY ERROR (NO FIELD 9)')       
 6040 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT COMBINE FIELDS TO',   
     1       10X,'DEFINE AVERAGE FOLLOWED BY ERROR (FIELD=',I2,' USED)')
 6050 FORMAT(10X,'WILL USE THE FIRST OF THE FOLLOWING COLUMN TITLES')   
 6060 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT SELECT LARGEST AND'/  
     1 10X,'SMALLEST VALUES (NO FIELD 9)')                              
 6070 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT SELECT LARGEST AND'/  
     1 10X,'SMALLEST VALUES (FIELD=',I2,' USED)')                       
 6080 FORMAT(10X,'OPERATION...SELECTED ',11A1)                          
 6090 FORMAT(10X,'OPERATION...NO FIELD SELECTED (ALL NEVER OUTPUT)')    
 6100 FORMAT(10X,'OPERATION...SELECTED LARGEST')                        
 6110 FORMAT(10X,'OPERATION...COMBINED FIELDS QUADRATICALLY')           
 6120 FORMAT(10X,'OPERATION...DEFINED AVERAGE VALUE')                   
 6130 FORMAT(10X,'OPERATION...DEFINED AVERAGE VALUE AND ERROR')         
 6140 FORMAT(10X,'OPERATION...SELECTED SMALLEST AND LARGEST VALUES')    
      END                                                               
      SUBROUTINE REOPS                                                  
C                                                                       
C     PERFORM REACTION DEFINED OPERATIONS.                              
C     (1) IF EN IS NOT DEFINED, DEFINE EN = 0.0253 EV                   
C     (2) IF EN IS NOT DEFINED, DEFINE EN = 2.0 MEV                     
C     (3) DATA = DATA/2 (DATA AND DATA ERROR)                           
C     (4) DATA = DATA/(2*L+1) (DATA AND DATA ERROR)                     
C     (5) DATA = DATA/F(0) (F(0)=ZEROTH ORDER LEGENDRE COEFFICIENT,     
C                           DATA AND DATA ERROR).                       
C     (6) DATA = DATA/(F(0)*(2*L+1)) (DATA AND DATA ERROR)              
C                                                                       
      INTEGER OUTP,OTAPE                                                
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  
      COMMON/RNOW/ISANR                                                 
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          
      COMMON/OUTVAL/IMUSED(60),VALUES(100),TIMEX(60),ADDX(60),          
     1 KTFLGX(60),KUFLGX(60)                                            
      DIMENSION EF(500),F(500)                                          
      DATA ETHERM/2.53E-02/                                             
      DATA EFISS/2.0E+06/                                               
C-----NOTHING TO DO IF NO OPERATION DEFINED.                            
      IF(IRFLAG(ISANR).LE.0) GO TO 110                                  
C-----RESET ENERGY COUNT ON FIRST POINT.                                
      IF(ISANR.EQ.1.AND.NPT.EQ.1) IEF=0                                 
C                                                                       
C     CHECK FOR CREATION OF AVERAGE ENERGY (THERMAL OR FISSION).        
C                                                                       
      IF(IRFLAG(ISANR).GT.2) GO TO 10                                   
C-----DEFINE LOCATION OF ENERGY FIELD. NOTHING TO DO IF ENERGY IS       
C-----DEFINED.                                                          
      II=KMOUT(1,ISANR)                                                 
      IF(II.GT.0) GO TO 110                                             
C-----CREATE AVERAGE ENERGY (THERMAL OR FISSION).                       
      IF(IRFLAG(ISANR).EQ.1) EX=ETHERM                                  
      IF(IRFLAG(ISANR).EQ.2) EX=EFISS                                   
      IDATN=IDATN+1                                                     
      KMOUT(1,ISANR)=IDATN                                              
      VALUES(IDATN)=EX                                                  
      IF(NPT.EQ.1) WRITE(OUTP,6000) EX                                  
      GO TO 110                                                         
C                                                                       
C     CHECK FOR RENORMALIZATION OF DATA AND DATA ERROR.                 
C                                                                       
C-----DEFINE LOCATION OF DATA FIELD. NOTHING TO DO IF DATA FIELD IS     
C-----NOT DEFINED.                                                      
   10 II=KMOUT(3,ISANR)                                                 
      IF(II.LE.0) GO TO 110                                             
      IF(IRFLAG(ISANR).NE.3) GO TO 20                                   
C-----DEFINE DATA = DATA/2                                              
      IF(NPT.EQ.1) WRITE(OUTP,6020)                                     
      VALUES(II)=VALUES(II)/2.0                                         
      II=KMOUT(4,ISANR)                                                 
      IF(II.LE.0) GO TO 110                                             
      VALUES(II)=VALUES(II)/2.0                                         
      GO TO 110                                                         
   20 IF(IRFLAG(ISANR).GT.6) GO TO 110                                  
C                                                                       
C     RENORMALIZE LEGENDRE COEFFICIENTS.                                
C                                                                       
C-----DEFINE INDEX TO LEGENDRE ORDER. IF NOT DEFINED TURN OFF FLAG.     
      JJ=KMOUT(5,ISANR)                                                 
      IF(JJ.GT.0) GO TO 30                                              
      IRFLAG(ISANR)=0                                                   
      WRITE(OUTP,6010)                                                  
      GO TO 110                                                         
C-----DEFINE LEGENDRE ORDER.                                            
   30 ORDERL=VALUES(JJ)                                                 
C-----IF NOT NORMALIZED TO F(0) PERFORM NORMALIZATION.                  
      IF(IRFLAG(ISANR).EQ.4) GO TO 90                                   
C-----SAVE F(0) AT ALL INCIDENT ENERGIES.                               
C-----DEFINE INDEX TO ENERGY. IF NOT DEFINED TURN OFF FLAG.             
      KK=KMOUT(1,ISANR)                                                 
      IF(KK.GT.0) GO TO 40                                              
      IRFLAG(ISANR)=0                                                   
      WRITE(OUTP,6060)                                                  
      GO TO 110                                                         
C-----SAVE ZEROTH ORDER ENERGY AND COEFFICIENT.                         
   40 ENOW=VALUES(KK)                                                   
      LORDER=ORDERL                                                     
      IF(LORDER.NE.0) GO TO 50                                          
      IF(IEF.LT.500) IEF=IEF+1                                          
      EF(IEF)=ENOW                                                      
      F(IEF)=VALUES(II)                                                 
C-----LOOK UP ZEROTH ORDER COEFFICIENT IN ENERGY TABLE.                 
   50 IF(IEF.LE.0) GO TO 70                                             
      DO 60 M=1,IEF                                                     
      IF(ABS(ENOW-EF(IEF)).LE.0.00001*EF(IEF)) GO TO 80                 
   60 CONTINUE                                                          
   70 WRITE(OUTP,6070) ENOW                                             
      GO TO 110                                                         
C-----DEFINE NORMALIZATION.                                             
   80 IF(IRFLAG(ISANR).EQ.5) ZNORM=F(M)                                 
      IF(IRFLAG(ISANR).EQ.6) ZNORM=F(M)*(2.0*ORDERL+1.0)                
      IF(NPT.EQ.1.AND.IRFLAG(ISANR).EQ.5) WRITE(OUTP,6040)              
      IF(NPT.EQ.1.AND.IRFLAG(ISANR).EQ.6) WRITE(OUTP,6050)              
      GO TO 100                                                         
C-----DEFINE NORMALIZATION.                                             
   90 ZNORM=2.0*ORDERL+1.0                                              
      IF(NPT.EQ.1) WRITE(OUTP,6030)                                     
C-----RE-NORMALIZE DATA.                                                
  100 VALUES(II)=VALUES(II)/ZNORM                                       
      II=KMOUT(4,ISANR)                                                 
      IF(II.LE.0) GO TO 110                                             
      VALUES(II)=VALUES(II)/ZNORM                                       
  110 RETURN                                                            
 6000 FORMAT(10X,'OPERATION...CREATED EN          ',1PE11.4,' EV')      
 6010 FORMAT(10X,'WARNING...LEGENDRE ORDER (COLUMN 5) NOT DEFINED'/     
     1 10X,'LEGENDRE COEFFICIENTS CANNOT BE RENORMALIZED')              
 6020 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/2')                   
 6030 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/(2*L+1)')             
 6040 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/F(0)')                
 6050 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/(F(0)*(2*L+1))')      
 6060 FORMAT(10X,'WARNING...ENERGY (COLUMN 1) NOT DEFINED'/             
     1 10X,'LEGENDRE COEFFICIENTS CANNOT BE RENORMALIZED')              
 6070 FORMAT(10X,'WARNING...NO F(0) AT ENERGY = ',1PE11.4,' EV'/        
     1 10X,'LEGENDRE COEFFICIENTS CANNOT BE RENORMALIZED')              
      END                                                               
      SUBROUTINE INTGER(CARD,N,I)                                       
C                                                                       
C     TRANSLATE FROM CHARACTERS TO INTEGER                              
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 CARD,DIGITS,PLUS,MINUS,BLANK,STAR,STARS               
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER CARD,DIGITS,PLUS,MINUS,BLANK,STAR,STARS                   
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      DIMENSION CARD(I),DIGITS(10),STARS(11)                            
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              
      DATA STARS/' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/           
      DATA PLUS/'+'/                                                    
      DATA MINUS/'-'/                                                   
      DATA BLANK/' '/                                                   
      DATA STAR/'*'/                                                    
C-----INITIALIZE NUMBER AND SKIP LEADING BLANKS.                        
      N=0                                                               
      NS=1                                                              
      DO 10 K=1,I                                                       
      IF(CARD(K).NE.BLANK) GO TO 20                                     
   10 CONTINUE                                                          
C-----FIELD IS BLANK. RETURN ZERO.                                      
      RETURN                                                            
C-----ALLOW LEADING + OR -.                                             
   20 IF(CARD(K).EQ.PLUS) GO TO 30                                      
      IF(CARD(K).NE.MINUS) GO TO 40                                     
      NS=-1                                                             
   30 K=K+1                                                             
C-----ERROR IF NUMBER ENDS WITH + OR -                                  
      IF(K.LE.I) GO TO 40                                               
      J=I                                                               
      GO TO 80                                                          
C-----TRANSLATE DIGITS.                                                 
   40 DO 70 J=K,I                                                       
      DO 50 M=1,10                                                      
      IF(CARD(J).EQ.DIGITS(M)) GO TO 60                                 
   50 CONTINUE                                                          
C-----ERROR. CANNOT TRANSLATE CHARACTER.                                
      GO TO 80                                                          
   60 N=10*N+(M-1)                                                      
   70 CONTINUE                                                          
C-----ALL CHARACTERS TRANSLATED. DEFINE SIGNED NUMBER                   
      N=NS*N                                                            
      RETURN                                                            
   80 STARS(J)=STAR                                                     
      WRITE(OUTP,6000) CARD                                             
      WRITE(OUTP,6010) STARS                                            
      N=0                                                               
      STARS(J)=BLANK                                                    
      RETURN                                                            
 6000 FORMAT(' SUBROUTINE INTGER....CANNOT TRANSLATE BELOW FIELD'/      
     1 1X,11A1)                                                         
 6010 FORMAT(1X,11A1)                                                   
      END                                                               
      SUBROUTINE FLOATF(FIELD,X)                                        
C                                                                       
C     CONVERT FROM HOLLERITH TO FLOATING POINT.                         
C     MUST BE BETWEEN 1.0E-35 AND 1.0E+35..OTHERWISE 0.0 IS RETURNED.   
C                                                                       
      INTEGER OUTP,OTAPE                                                
C***** CHARACTER *****                                                  
      CHARACTER*1 BLANK,DOT,EXPD,EXPE,PLUS,MINUS,STAR,MESS,DIGIT,FIELD, 
     1 MFIELD                                                           
C***** CHARACTER *****                                                  
C***** INTEGER *****                                                    
C     INTEGER BLANK,DOT,EXPD,EXPE,PLUS,MINUS,STAR,MESS,DIGIT,FIELD,     
C    1 MFIELD                                                           
C***** INTEGER *****                                                    
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
      DIMENSION FIELD(11),TEN(35),DIGIT(10),MESS(11)                    
      DATA BLANK/' '/                                                   
      DATA DOT/'.'/                                                     
      DATA EXPD/'D'/                                                    
      DATA EXPE/'E'/                                                    
      DATA PLUS/'+'/                                                    
      DATA MINUS/'-'/                                                   
      DATA STAR/'*'/                                                    
      DATA MESS/' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/            
      DATA DIGIT/'0','1','2','3','4','5','6','7','8','9'/               
      DATA ZERO/0.0E+00/                                                
      DATA TEN/                                                         
     1 1.0E+01,1.0E+02,1.0E+03,1.0E+04,1.0E+05,                         
     2 1.0E+06,1.0E+07,1.0E+08,1.0E+09,1.0E+10,                         
     3 1.0E+11,1.0E+12,1.0E+13,1.0E+14,1.0E+15,                         
     4 1.0E+16,1.0E+17,1.0E+18,1.0E+19,1.0E+20,                         
     5 1.0E+21,1.0E+22,1.0E+23,1.0E+24,1.0E+25,                         
     6 1.0E+26,1.0E+27,1.0E+28,1.0E+29,1.0E+30,                         
     7 1.0E+31,1.0E+32,1.0E+33,1.0E+34,1.0E+35/                         
C                                                                       
C     TRANSLATE MANTISSA.                                               
C                                                                       
C-----SKIP LEADING BLANK CHARACTERS.                                    
      DO 10 I=1,11                                                      
      IF(FIELD(I).NE.BLANK) GO TO 20                                    
   10 CONTINUE                                                          
C-----FIELD IS COMPLETELY BLANK. RETURN ZERO.                           
      X=ZERO                                                            
      GO TO 240                                                         
C-----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT. 
   20 IN=0                                                              
      IPT=-20                                                           
C-----ALLOW LEADING SIGN.                                               
      IF(FIELD(I).EQ.MINUS) GO TO 40                                    
      IF(FIELD(I).NE.PLUS) GO TO 30                                     
      I=I+1                                                             
   30 XSIGN=1.0                                                         
      GO TO 50                                                          
   40 I=I+1                                                             
      XSIGN=-1.0                                                        
C-----SCAN REMAINDER OF MANTISSA.                                       
   50 DO 90 J=I,11                                                      
      MFIELD=FIELD(J)                                                   
C-----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).     
      DO 60 K=1,10                                                      
      IF(MFIELD.EQ.DIGIT(K)) GO TO 80                                   
   60 CONTINUE                                                          
      IF(MFIELD.NE.DOT) GO TO 70                                        
      IPT=0                                                             
      GO TO 90                                                          
C-----SCAN FOR BLANK (WHICH ENDS MANTISSA).                             
   70 IF(MFIELD.EQ.BLANK) GO TO 100                                     
C-----SCAN FOR E,D,- OR + (WHICH BEGINS EXPONENT).                      
      IF(MFIELD.EQ.EXPE.OR.MFIELD.EQ.EXPD) GO TO 130                    
      IF(MFIELD.EQ.MINUS) GO TO 160                                     
      IF(MFIELD.EQ.PLUS) GO TO 140                                      
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 
      GO TO 250                                                         
C-----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT   
C-----OFFSET.                                                           
   80 IN=10*IN+(K-1)                                                    
      IPT=IPT+1                                                         
   90 CONTINUE                                                          
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT. 
      GO TO 120                                                         
C-----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR        
C-----EXPONENT.                                                         
  100 I=J+1                                                             
      IF(I.GT.11) GO TO 120                                             
      DO 110 J=I,11                                                     
      MFIELD=FIELD(J)                                                   
      IF(MFIELD.EQ.BLANK) GO TO 110                                     
      IF(MFIELD.EQ.EXPE.OR.MFIELD.EQ.EXPD) GO TO 130                    
      IF(MFIELD.EQ.MINUS) GO TO 160                                     
      IF(MFIELD.EQ.PLUS) GO TO 140                                      
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 
      GO TO 250                                                         
  110 CONTINUE                                                          
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT. 
  120 X=IN                                                              
      IF(IPT.GT.0) X=X/TEN(IPT)                                         
      GO TO 230                                                         
C                                                                       
C     TRANSLATE EXPONENT.                                               
C                                                                       
C-----BEGINNING OF EXPONENT FOUND (X OR D). CHECK FOR FOLLOWING - OR +. 
  130 J=J+1                                                             
      MFIELD=FIELD(J)                                                   
      IF(MFIELD.EQ.MINUS) GO TO 160                                     
      IF(MFIELD.NE.PLUS) GO TO 150                                      
C----- + FOUND. INITIALIZE EXPONENT SIGN.                               
  140 J=J+1                                                             
  150 KSIGN=1                                                           
      GO TO 170                                                         
C----- - FOUND. INITIALIZE EXPONENT SIGN.                               
  160 J=J+1                                                             
      KSIGN=-1                                                          
C-----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.   
  170 KEXP=0                                                            
      DO 200 I=J,11                                                     
      MFIELD=FIELD(I)                                                   
      IF(MFIELD.EQ.BLANK) GO TO 200                                     
      DO 180 K=1,10                                                     
      IF(MFIELD.EQ.DIGIT(K)) GO TO 190                                  
  180 CONTINUE                                                          
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 
      GO TO 250                                                         
C-----DIGIT FOUND. INCREMENT EXPONENT.                                  
C-----OFFSET.                                                           
  190 KEXP=10*KEXP+(K-1)                                                
  200 CONTINUE                                                          
C-----ENTIRE FIELD TRANSLATED (WITH EXPONENT). CONVERT TO FLOATING      
C-----POINT.                                                            
      X=IN                                                              
      KEXP=KSIGN*KEXP                                                   
      IF(IPT.GT.0) KEXP=KEXP-IPT                                        
      IF(IABS(KEXP).GT.35) GO TO 260                                    
      IF(KEXP) 210,230,220                                              
  210 KEXP=-KEXP                                                        
      X=X/TEN(KEXP)                                                     
      GO TO 230                                                         
  220 X=X*TEN(KEXP)                                                     
  230 X=XSIGN*X                                                         
  240 RETURN                                                            
  250 MESS(J)=STAR                                                      
      WRITE(OUTP,6000) FIELD,MESS                                       
      X=ZERO                                                            
      MESS(J)=BLANK                                                     
      RETURN                                                            
  260 WRITE(OUTP,6010) FIELD,MESS                                       
      X=ZERO                                                            
      RETURN                                                            
 6000 FORMAT(1X,11A1/1X,11A1/                                           
     1 ' SUBROUTINE FLOATF...ERROR IN INPUT DATA...TRANSLATED AS 0')    
 6010 FORMAT(1X,11A1/1X,11A1/                                           
     1 ' SUBROUTINE FLOATF...EXPONENT NOT +/-35....TRANSLATED AS 0')    
      END                                                               
      SUBROUTINE FILEIO                                                 
C                                                                       
C     OPTIONALLY DEFINE FILENAMES AND REWIND TO START                   
C                                                                       
      INTEGER OUTP,OTAPE                                                
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NTAPE1,NTAPE2,NTAPE3      
C***** NAMES *****                                                      
C     OPEN(INP,FILE='x4toc4.inp')                                       
      OPEN(NEWX4,FILE='errors')                                         
      OPEN(OUTP,FILE='x4toc4.lst')                                      
      OPEN(ITAPE,FILE='exfor.in')                                       
      OPEN(OTAPE,FILE='c4.out')                                         
      OPEN(NTAPE1,FILE='reaction')                                      
      OPEN(NTAPE2,FILE='titles')                                        
      OPEN(NTAPE3,FILE='units')                                         
C***** NAMES *****                                                      
C***** REWIND *****                                                     
C     REWIND INP                                                        
      REWIND NEWX4                                                      
      REWIND ITAPE                                                      
      REWIND OTAPE                                                      
      REWIND NTAPE1                                                     
      REWIND NTAPE2                                                     
      REWIND NTAPE3                                                     
C***** REWIND *****                                                     
      RETURN                                                            
C***** STOP *****                                                       
      END                                                               
      SUBROUTINE GOOUT                                                  
      STOP                                                              
C***** STOP *****                                                       
      END                                                               
