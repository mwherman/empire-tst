      PROGRAM X4TOC4                                                    X4T00050
C-Title  : Program X4TOC4
C-Version: 86-1 (August 1986)
C-V      2001/03 (March 2001)  *Minor corrections
C-V      2002/10 Read sample thickness, convert transmission to x-sect.
C-V      2004/01 Define all input filenames from input.
C-V      2004/10 Redefine MT>=9000 to define incident particle.
C-V      2005/07 Introduce F90 features for characters and do-loops
C-V      2005/12 If E-level is zero, redefine MT 51 to MT 4
C-V      2006/02 Fix Y2K date in references
C-V      2006/04 Deleted SF9, V.Zerkin@iaea.org
C-V      2006/04 Extended dimensions 400->11111 (large EXFOR14A.DAT) Z.V.
C-V      2006/12 Ratio-to-rutherfors scattering for charged particles
C-V      2007/04 Trivial syntax correction (V. Zerkin)
C-V              Interpret Q-value as -LevelEnergy for inelastic (A.Trkov)
C-Purpose: Translate Data from EXFOR to Computational Format
C-Author :
C-A  OWNED, MAINTAINED AND DISTRIBUTED BY:
C-A  -------------------------------------
C-A  The Nuclear Data Section
C-A  International Atomic Energy Agency
C-A  P.O. Box 100
C-A  A-1400, VIENNA, AUSTRIA
C-A  EUROPE
C-A
C-A  ORIGINALLY WRITTEN BY:
C-A  ----------------------
C-A  Dermott E. Cullen
C-A  University of California
C-A  Lawrence Livermore National Laboratory
C-A  L-86
C-A  P.O. Box 808
C-A  Livermore, CA 94550
C-A  U.S.A.
C-A  Telephone  925-423-7359
C-A  e-mail     CULLEN1@LLNL.GOV
C-A  Website    HTTP://REDDOG1.LLNL.GOV
C-A
C-A  VERSION 2001/03 AND LATER IMPLEMENTED BY:
C-A  ----------------------------------------
C-A  Andrej Trkov
C-A  The Nuclear Data Section
C-A  International Atomic Energy Agency
C-A  P.O. Box 100
C-A  A-1400, VIENNA, AUSTRIA
C-A  EUROPE
C-A  e-mail     A.Trkov@iaea.org
C-
C-M
C-M  USERS' GUIDE FOR PROGRAM X4TOC4
C-M  ===============================
C-M
C-M  PURPOSE
C-M  -------
C-M  This program is designed to translate experimental data from the
C-M  EXFOR format to a computation format.
C-M
C-M  WHAT COMPUTERS WILL THE PROGRAM RUN ON
C-M  --------------------------------------
C-M  The program has been implemented on a variety of computers from
C-M  Cray and IBM mainframe to Sun workstations to a PC. The
C-M  program is small enough to run on virtually any computer.
C-M
C-M  EXFOR FORMAT
C-M  ------------
C-M  The EXFOR format is designed to allow experimentally measured data
C-M  to be coded in a computer readable format in a very flexible form.
C-M  In particular the data can be entered in essentially any set of
C-M  units (e.g., eV vs. barns or KeV vs. milli-barns) and in any table
C-M  format; essentially the table may be entered exactly as published
C-M  by an author (e.g., energy followed by columns of cross sections
C-M  in any order).
C-M
C-M  The EXFOR format is table oriented in the sense that data from a
C-M  given measurement are collected together and can be presented in
C-M  a single, or as a series of tables.
C-M
C-M  The advantage of the EXFOR format is that since data can be coded
C-M  essentially as published by an author problems of unit conversion
C-M  and re-formatting tables prior to coding are avoided and the
C-M  author can easily check the coded data. The result is a greatly
C-M  improved reliability of the coded data.
C-M
C-M  The disadvantage of the EXFOR format is that since physically
C-M  comparable data from different measurements (e.g. Fe-56 total
C-M  cross sections) may be given in a variety of different units and
C-M  formats it is very difficult to use in applications. In addition
C-M  the table oriented EXFOR system makes it difficult to collect
C-M  together physically comparable data from different measurements.
C-M
C-M  COMPUTATION FORMAT
C-M  ------------------
C-M  The computation format used by this program is designed to present
C-M  experimental data in a fixed set of units and column order. By
C-M  starting from data in the EXFOR format and translating data to
C-M  the computation format it is possible to combine the advantages
C-M  of the improved reliability of the data coded in the EXFOR format
C-M  with the advantages of a fixed unit and column order format for
C-M  use in subsequent applications.
C-M
C-M  In addition the computation format is point oriented (as opposed
C-M  the table oriented EXFOR format). Each line of the computation
C-M  format represents a single data point. This makes it possible to
C-M  sort data in the computation format into any desired order for use
C-M  in application, e.g., sort 26-Fe-26 (n,2n) data from a number of
C-M  measurements together into energy order to simplify comparisons.
C-M
C-M  EXFOR VS. COMPUTATION FORMAT
C-M  ----------------------------
C-M  The computation format is not intented as a substitute for the
C-M  EXFOR format, rather the two are complementary. The EXFOR format
C-M  contains much more information than can be included in computation
C-M  formats and this information should be consulted and used during
C-M  evaluation. The computation format is only intended to simplify
C-M  use of the data during evaluation, or other applications.
C-M
C-M  RELATIONSHIP TO ENDF
C-M  --------------------
C-M  It is assumed that one of the major uses of this program will be
C-M  to prepare data for subsequent use in evaluation and/or to compare
C-M  available evaluated and experimental data. As such the computation
C-M  format has been designed to allow data to be reduced to a form in
C-M  which data are classified in a manner similar to ENDF data.
C-M
C-M  In particular the EXFOR classification of data by the EXFOR
C-M  keyword reaction (or ISO-QUANT, etc.) is replaced by classifying
C-M  the data by (1) projectile, (2) target - ZA, (3) type of data
C-M  (ENDF MF number), (4) reaction (ENDF MT number). In addition the
C-M  standard units used by the translation program were selected to
C-M  be the same as the units used by ENDF (e.g., eV, barns, etc.).
C-M
C-M  The result of putting data into the computation format is that it
C-M  is easy to decide if the data is comparable to evaluated data
C-M  (e.g. same ZA, MF, MT) and once it is decided that data is
C-M  comparable, evaluation and/or comparison is simplified because the
C-M  data is in the same units as ENDF (e.g., eV vs. barns).
C-M
C-M  EXTENSIONS OF ENDF CONVENTIONS
C-M  ------------------------------
C-M  For all types of data which are physically comparable to data,
C-M  which can be included in the ENDF data, this program uses
C-M  the ENDF definitions of (1) type of data (ENDF MF number),
C-M  (2) reaction (ENDF MT number). For example all cross sections
C-M  are represented by MF=3, angular distributions by MF=4, energy
C-M  distributions by MF=5 and double differential distributions
C-M  by MF=6. Similarly for simple reactions such as total, elastic
C-M  etc., the data are translated into corresponding MT=1,2, etc.,
C-M  respectively.
C-M
C-M  Since many types of data which appear in EXFOR do not have a one
C-M  to one correspondence to data which appears in ENDF the ENDF
C-M  classification of type of data (MF) and reaction (MT) have been
C-M  extended to allow additional types of data and reactions to be
C-M  translated (e.g., define MF numbers for ratios, define MT numbers
C-M  for (n,np)+(n,na) reactions).
C-M
C-M  The ENDF MF is a 2 digit number and the MT is a 3 digit number.
C-M  In the computation format MF has been extended to 3 digits and the
C-M  MT has been expanded to 4 digits. These extensions allow the user
C-M  the flexibility to translate virtually any EXFOR data to a fixed
C-M  set of units and column order for subsequent use in applications.
C-M
C-M  Some extensions of MF and MT have already been established (for,
C-M  details see the input dictionaries described below) and if at all
C-M  possible these conventions should be followed by the user. The
C-M  user has the flexibility of establishing any conventions that may
C-M  be required to meet his or her needs, but in this case it is the
C-M  responsibility of the user to properly interpret and use the
C-M  translated data.
C-M
C-M  DIRECT COMPARISON TO ENDF DATA
C-M  ------------------------------
C-M  Although the ENDF classification system of MF and MT is used for
C-M  translation, generally very little of the EXFOR data is directly
C-M  comparable to ENDF data. Generally cross sections (MF=3) are
C-M  directly comparable. However, it must be realized that angular
C-M  (MF=4) and energy (MF=5) and double differential (MF=6) data are
C-M  given in ENDF in a normalized (i.e., normalized to unity when
C-M  intergrated) form, whereas data in EXFOR are generally given in an
C-M  unnormalized form (e.g.,angular distributions in barns/steradian).
C-M
C-M  After this program has been used to translate EXFOR data to the
C-M  computation format the user may make additional data directly
C-M  comparable to the corresponding ENDF data by either,
C-M  (1) Normalizing the data in the computation format, or,
C-M  (2) Converting ENDF data to unnormalized form.
C-M  This involves selecting an intgrated cross section as a standard
C-M  to use for the comparison (e.g., for a 14.2 MeV elastic angular
C-M  distribution use the 14.2 MeV ENDF elastic cross section).
C-M
C-M  Since the selection of a standard to use for comparison in highly
C-M  application dependent it has been decided that it is better to use
C-M  this program to translate data exactly as given in EXFOR (except
C-M  for conversion to a standard set of units) and to allow the user
C-M  to subsequently select a standard for renormalization.
C-M
C-M  CONTROL OF TRANSLATION
C-M  ----------------------
C-M  The user has complete control over what data is translated, where
C-M  given types of data appear in the computation format and the units
C-M  of the data in the computation format.
C-M
C-M  This is accomplished by using three dictionaries which control
C-M  the translation. All three of these dictionaries are distributed
C-M  with this program. Each dictionary is a simple card image file
C-M  which may be modified by the user at any time to meet specific
C-M  needs. The three dictionaries are:
C-M
C-M  (1) EXFOR REACTION - PROJECTILE, MF, MT EQUIVALENCE
C-M      This dictionary tells the program for each EXFOR reaction
C-M      what projectile, MF and MT to output in the computation format
C-M      (e.g.,(n,tot) = neutron, MF =3 (cross section),MT =1 (total)).
C-M      If a reaction read from the EXFOR format is not found in this
C-M      dictionary, or the assigned MF or MT is not positive the EXFOR
C-M      data will simply be skipped and not translated. Using this
C-M      dictionary the user has control over which data is translated
C-M      and what MF and MT are assigned to each EXFOR reaction.
C-M
C-M  (2) EXFOR COLUMN TITLE - COMPUTATION FORMAT OUTPUT FIELD
C-M      Once the EXFOR reaction has been translated and assigned an
C-M      equivalent MF and MT this dictionary tells the program where
C-M      to place each EXFOR column in the computation format. The
C-M      assigned MF number can be used to output an EXFOR column
C-M      with the same title into different columns of the computation
C-M      format based on different mf numbers. For example, for cross
C-M      sections (MF=3) the user may use EN-MIN and EN-MAX to define
C-M      an average incident energy to be output in the first field
C-M      of the computation format and an equivalent energy uncertainty
C-M      in the second field of the computation format. Alternatively,
C-M      for resonance integrals (MF=213) the user may decide to output
C-M      EN-MIN and EN-MAX in the first two fields of the computation
C-M      format to define the energy range of the resonance integral.
C-M
C-M      There are 8 output fields in the computation format and for
C-M      any given MF number the user may output any EXFOR column
C-M      into any of these fields. Any EXFOR title which is not
C-M      assigned to an output field 1 to 8 will be ignored and not
C-M      output. This allows the user to selectively translate portions
C-M      of EXFOR data tables to meet any given need. For example, by
C-M      simply modifying this dictionary the user has control over
C-M      whether an EXFOR column DATA-ERR3 is translated or ignored,
C-M      and if translated the user has control over which of the 8
C-M      computation format data fields DATA-ERR3 will appear in.
C-M
C-M  (3) EXFOR COLUMN UNITS - COMPUTATION FORMAT UNITS
C-M      This dictionary tells the program how to convert each EXFOR
C-M      unit into standard units. As distributed this dictionary will
C-M      convert all EXFOR units to ENDF compatible units. However,
C-M      the user has the option to change this dictionary at any time
C-M      to obtain any output units to meet his or her needs. For
C-M      example if the user would like output in MeV vs. milli-barns
C-M      instead of eV vs. barns it is merely necessary to modify this
C-M      dictionary.
C-M
C-M  OPERATIONS ON DATA
C-M  ------------------
C-M  In addition to the information described above each of the three
C-M  dictionaries allows the user to select from a menu of operations
C-M  which may be performed on the data (for a complete and up-to-date
C-M  list of available operations see the dictionaries). For example,
C-M  the reaction dictionary allows the user to specify that legendre
C-M  coefficents may be re-normalized, the title dictionary allows the
C-M  user to specify that EN-MIN and EN-MAX are to be converted to an
C-M  average energy and associated energy uncertainty and the units
C-M  dictionary allows the user to specify that angles should be
C-M  converted to cosines.
C-M
C-M  These operations are completely under the control of the user and
C-M  by simply modifying the dictionaries the user can control whether
C-M  or not each operation is performed (e.g., if you want to output
C-M  angles instead of cosines modify the units dictionary by removing
C-M  the option to convert from angle to cosine from the EXFOR units
C-M  ASEC, AMIN and ADEG).
C-M
C-M  COMPUTATION FORMAT UNITS
C-M  ------------------------
C-M  As distributed the Units dictionary will convert all EXFOR units
C-M  to ENDF units:
C-M
C-M  eV         = energy
C-M  barns      = cross section
C-M  steradians = solid angle
C-M  seconds    = time
C-M  kelvin     = temperature
C-M
C-M  If the user would like to obtain any other output units it is
C-M  merely necessary to modify the units dictionary (see units
C-M  dictionary for details).
C-M
C-M  A LEARNING PROGRAM
C-M  ------------------
C-M  As distributed the three translation dictionaries do not contain
C-M  definitions of how to translate all EXFOR reactions, titles and
C-M  units. At the present time this program has only been used to
C-M  translate a small portion of the data included in the EXFOR system
C-M  and the dictionaries only contain sufficient information to
C-M  translate the EXFOR data which has been encountered to date.
C-M
C-M  It is difficult and dangerous to try to define translation rules
C-M  for all types of EXFOR data without examining actual EXFOR data.
C-M  therefore only when a new reaction, title or unit is encountered
C-M  during translation will the actual EXFOR data be examined, a
C-M  decision made as to how to best translate the data and the
C-M  dictionaries updated.
C-M
C-M  Generally once a given type of EXFOR data has been encountered and
C-M  the dictionaries updated to define how to translate the data the
C-M  same rules can be used to translate all similar data. Therefore
C-M  over a period of time user experience will be accumulated in the
C-M  translation dictionaries and the program will learn to properly
C-M  translate more and more types of EXFOR data.
C-M
C-M  UNDEFINED EXFOR REACTIONS, TITLES AND UNITS
C-M  -------------------------------------------
C-M  In order to assist the user to define new types of EXFOR data as
C-M  they are encountered during translation the output report from
C-M  this program will indicate the number of EXFOR reactions, titles
C-M  and units which have been encountered during translation which are
C-M  not defined in the translation dictionaries. In additional all
C-M  undefined reactions, titles and units will be written to output
C-M  Unit 4 (NEWX4).
C-M
C-M  Based on comparison to the reaction, title and units dictionaries
C-M  if an EXFOR reaction, title or units is encountered during
C-M  translation that is not defined in the dictionaries it will be
C-M  written to Unit 4 (NEWX4). This information is written in a form
C-M  that can be easily edited and added to a translation dictionary.
C-M  After updating the dictionaries if this program is then run a
C-M  second time using the same EXFOR data all of the EXFOR data can
C-M  be translated.
C-M
C-M  COMPUTATION FORMAT
C-M  ------------------
C-M  The computation format uses a classification system and units
C-M  which are compatible with ENDF. Data is classified by (1) ZA
C-M  of projectile, (2) ZA of target, (3) metastable state of target,
C-M  (4) MF - type of data, (5) MT - reaction, (6) metastable state
C-M  of residual nucleus. To identify the source of the data the first
C-M  author and year and the EXFOR accession and sub-accession number
C-M  are included in the format. In addition, fields are assigned to
C-M  define the status of the EXFOR data (e.g., S = superceded),
C-M  whether data is in the laboratory or center-of-mass frame of
C-M  reference and the physical significance of the last 2 output
C-M  fields (LVL = level energy, HL = half-life). Finally the format
C-M  includes 8 fields in which the output data are contained (e.g.,
C-M  incident energy, data, cosine, uncertainties, etc.)
C-M
C-M  Columns   Description
C-M  -------   -----------
C-M    1-  5   Projectile ZA (e.g. neutron =1, proton =1001)
C-M            (defined by reaction dictionary).
C-M    6- 11   Target ZA (e.g. 26-Fe-56 =  26056)
C-M            (defined by EXFOR reaction).
C-M       12   Target metastable state (e.g. 26-FE-56m = M)
C-M            (defined by EXFOR reaction).
C-M   13- 15   MF (ENDF conventions, plus additions)
C-M            (defined by reaction dictionary).
C-M   16- 19   MT (ENDF conventions, plus additions)
C-M            (defined by reaction dictionary).
C-M       20   Product metastable state (e.g. 26-FE-56M = M)
C-M            (defined by EXFOR reaction).
C-M       21   EXFOR status
C-M            (defined by EXFOR keyword status).
C-M       22   Center-of-mass flag (C=center-of-mass, blank=lab)
C-M            (defined by EXFOR title dictionary).
C-M   23- 94   8 data fields (each in E9.3 format defined below)
C-M            (defined by MF and title dictionary).
C-M   95- 97   Identification of data fields 7 and 8
C-M            (e.g., LVL=level, HL=half-life, etc.).
C-M            For a complete list of codes see title dictionary
C-M            (defined by MF and title dictionary).
C-M   98-122   Reference (first author and year)
C-M            (defined by EXFOR keywords title and reference).
C-M  123-127   EXFOR accession number
C-M            (defined by EXFOR format).
C-M  128-130   EXFOR sub-accession number
C-M            (defined by EXFOR format).
C-M      131   Multi-dimension table flag
C-M            (defined by EXFOR keyword reaction or common fields).
C-M
C-M  PRECISION OF THE 8 DATA FIELDS
C-M  ------------------------------
C-M  If written in normal format E9.2 format the output from this
C-M  this program would give data to only 2 or 3 digits of accuracy,
C-M  depending on the computer used (e.g., 0.23E+02 or 2.34E+01), which
C-M  is not sufficient for many applications (e.g., energy of cross
C-M  section points in the resonance region).
C-M
C-M  In order to avoid this problem this program will output data in
C-M  a special compatible format to allow up to 7 digits of accuracy
C-M  (i.e.,more than the full word accuracy of IBM computers).
C-M
C-M  Numbers between 0.01 and less than 10 million will be output in F
C-M  (rather than E format). For example, the energy 12.3456 KeV will
C-M  be output as 123456.0. Numbers less than 0.01 or greater than
C-M  10 million will be output in E format, but without as E and an
C-M  exponent of 1 or 2 digits. For example 14.123 MeV will be output
C-M  as "1.4123+7".
C-M
C-M  These output conventions have been used for many years with ENDF
C-M  related programs and have been proven to be FORTRAN compatible forx
C-M  use on virtually any computer. For example, any fortran program
C-M  which is written to read this data using an E9.2 format will read
C-M  the data properly whether the data is actually in E or F format.
C-M
C-M  Generally maintaining high precision in the data is most important
C-M  for the independent variable, particularly incident energy. Since
C-M  we do not expect very narrow resonance structure below 0.01 eV or
C-M  above 10 MeV generally these output conventions will maintain the
C-M  accuracy of the EXFOR data to meet requirements.
C-M
C-M  DEFINITION OF 8 COMPUTATION FORMAT DATA FIELDS
C-M  ----------------------------------------------
C-M  The user may use the title dictionary to output any EXFOR column
C-M  into any computation format data field. As distributed the title
C-M  dictionary contains a number of conventions which if at all
C-M  possible should be followed by the users. The general definitions
C-M  of the 8 computation format data fields are:
C-M
C-M  Data field   Definition
C-M  ----------   ----------
C-M    1          Projectile incident energy
C-M    2          Projectile incident energy uncertainty
C-M    3          Data, e.g., cross section, angular distribution, etc.
C-M    4          Data uncertainty
C-M    5          Cosine or legendre order
C-M    6          Cosine uncertainty
C-M    7          Identified by columns 95-97 (e.g.,level E, half-life)
C-M    8          Identified by columns 95-97 (e.g.,level E, uncertainty)
C-M
C-M  The physical significance of each field is defined by the assigned
C-M  MF number. For example, for MF =3 (cross sections), columns 1 and
C-M  2 contain the incident projectile energy and its uncertainty in
C-M  eV, respectively and columns 3 - 4 contain the cross section and
C-M  its uncertainty in barns, respectively and columns 7 and 8 may
C-M  contain a level energy and its uncertainty in eV or a half-life
C-M  and its uncertainty in seconds.
C-M
C-M  SPECIAL CONVENTIONS
C-M  The above conventions are appropriate for most types of data
C-M  in the ENDF system. In order to allow this program to plot
C-M  additional types of data the following special conventions have
C-M  been adopted,
C-M
C-M  Cross section ratios  - Field 5 = MT of denominator.
C-M  (MF = 203)              Field 6 = ZA of denominator.
C-M  Resonance integrals   - Field 1 = lower energy limit.
C-M  (MF = 213)              Field 2 = upper energy limit.
C-M  Spectrum averages     - Field 1 = lower energy limit.
C-M  (MF = 223)              Field 2 = upper energy limit.
C-M  Fission yield data    - Field 5 = ZA of fission fragment.
C-M  (MF = 801)              Field 6 = mass of fission fragment.
C-M  Production            - Field 6 = ZA of product.
C-M  (MT = 9000-9999)
C-M
C-M  See, remarks below on metastable state flags.
C-M
C-M  REQUIRED DATA FIELDS
C-M  --------------------
C-M  For various types of data the program will check if all required
C-M  fields are defined and non-blank. If they are not warning messages
C-M  will be printed. If the data field (Field 3) is not defined or
C-M  blank the data point will not be output. If the data field is not
C-M  defined this usually indicates an error in the EXFOR data. Blank
C-M  data fields are quite common in multi-dimensional tables and a
C-M  warning may or may not indicate an error (check the EXFOR data to
C-M  see if it is correct).
C-M
C-M  The program considers that the following fields are required:
C-M
C-M   MF (Data type)         Data field (X = Required)
C-M  ---------------------   -------------------------
C-M                           1  2  3  4  5  6  7  8
C-M  ---------------------   -------------------------
C-M    3 (Cross sections)     X     X
C-M    4 (Angular dist.)      X     X     X
C-M    5 (Energy dist.)       X     X           X
C-M    6 (Double diff.)       X     X     X     X
C-M  154 (Legendre coeff.)    X     X     X
C-M  203 (Ratios)             X     X     X  X
C-M  801 (Yield data).        X     X     X  X
C-M
C-M  (See the above definition of the 8 data fields).
C-M
C-M  MULTI-DIMENSIONAL TABLES
C-M  ------------------------
C-M  The program can translate multi-dimensional EXFOR tables for:
C-M  (1) Multiple reactions following the EXFOR keyword reaction
C-M      (ISO-QUANT, etc.) with each reaction identified by a character
C-M      in column 11.
C-M  (2) Single reactions with multiple common fields each identified
C-M      by a character in the eleventh column of each field.
C-M  (3) The old ISO-QUANT, etc. convention of reactions separated by
C-M      commas, e.g., ((90-TH-232,NG)/(29-CU-0,NG)),(29-CU-0,NG)).
C-M
C-M  TRANSLATION OF EXFOR REACTIONS
C-M  ------------------------------
C-M  Not all EXFOR reactions (ISO-QUANT, etc.) can be translated by
C-M  this program. In order to translate each reaction the program will
C-M  first break each reaction into a series of simple reactions and
C-M  remove and save the target and residual ZA, E.G.:
C-M
C-M  ((26-FE-56(N,G)26-FE-57-M1,,SIG)/(26-FE-56(N,G)26-FE-57-G,,SIG))
C-M
C-M  is broken down to define
C-M
C-M  ZA-target = 26056 , ZA-residual = 260571, reaction = (N,G),SIG
C-M
C-M  Note residual metastable state flags. See explanation below.
C-M
C-M  The program will then define an equivalent MF, MT for each
C-M  reaction.
C-M
C-M  The program will next translate the following types of
C-M  reactions:
C-M  (1) Simple reactions
C-M      (N,G),SIG
C-M  (2) Equivalent reactions
C-M      ((N,G),SIG)=...anything else....
C-M      After decoding the first simple reaction the program assumes
C-M      that the first simple reaction is truely equivalent to the
C-M      remainder of the reaction and defines ZA, MF and MT based on
C-M      the first simple reaction.
C-M  (3) Simple ratios
C-M      ((N,G)M1/G,,SIG/RAT) or ((N,G)M1,SIG)/((N,G)G,SIG)
C-M  (4) Complex reactions - all with the same equivalent ZA
C-M      ((N,EL),WID,,G)*((N,G),WID)/((N,TOT),WID)
C-M  (5) Other reactions
C-M      (((N,G),SIG)/((N,G),SIG),(N,G),SIG))
C-M
C-M      If the reaction is not one of the above types the program will
C-M      try to use the entire EXFOR reaction, including target and
C-M      residual ZA and see if it is defined in reaction equivalent
C-M      dictionary. If an MF, MT is defined for the entire reaction
C-M      the program will use the target and residual ZA from the first
C-M      simple reaction to translate the data. This last form may be
C-M      used to insure that almost all EXFOR reaction can be
C-M      translated, regardless of how complicated it is (for examples
C-M      see reaction dictionary) however the user should carefully
C-M      check the output to insure that the data has been translated
C-M      as intended.
C-M
C-M  The only reactions that have so far been found that cannot be
C-M  correctly translated are ratios of production cross sections,
C-M  e.g., (29-CU-0(P,X)26-FE-56)/(28-NI-0(P,X)26-FE-58)
C-M  because ratio data requires fields 5 and 6 for the denominator
C-M  MT and ZA and ratio data requires field 5 for the product ZA.
C-M  When this case is encountered the program will print an error
C-M  message and output the denominator MT and ZA in fields 5 and 6.
C-M  In this case the output will identify the numerator as ZA=29000,
C-M  MT=9001 and the denominator as ZA=28000, MT=9001. One solution
C-M  is to modify the output of this program by defining two reactions,
C-M  e.g., MT = 8001 = (p,x) 26-Fe-56 and MT = 8002 = (p,x) 26-Fe-58,
C-M  modify the numerator MT to 8001 and denominator MT to 8002 and
C-M  then properly interpreting the data using these definition in all
C-M  applications (for examples, see program PLOTC4 input directionary
C-M  for proton induced reactions).
C-M
C-M  Sometimes elastic scattering reactions may be represented in the
C-M  form of the ratio to Coulomb scattering cross section (sometimes
C-M  referred to as "Ratio-to-Rutherford), identified by reaction
C-M  modifier flag RTH. Masses of the target and the projectile are
C-M  required, which are read from the Audi-Wapstra mass tables
C-M  in the file specified on input. The file is available from the
C-M  IAEA web site. The expression for Coulomb scattering corresponds
C-M  to the equations 6.11 and 6.12 in the ENDF-102 formats manual.
C-M
C-M  OUTPUT REPORT
C-M  -------------
C-M  This program will write a report on Unit 6 (OUTP) to allow the
C-M  user to monitor the translation of the EXFOR data. It is extremely
C-M  important that the user read this report and not simply assume
C-M  that all of the data has been properly translated.
C-M
C-M  After identifying each EXFOR accession, sub-accession number,
C-M  ZA, MF, MT and reaction the program can print two types of
C-M  messages:
C-M
C-M  WARNING    = Something unusual has occurred. The user should
C-M               carefully check to insure that the output data has
C-M               been properly translated.
C-M  OPERATION  = One of the defined reaction, title or unit operations
C-M               has been performed on the data. The user should
C-M               carefully check to insure that the proper operation
C-M               has been performed.
C-M
C-M  If the user does not agree with how the data has been translated
C-M  the three dictionaries may to be modified and the program re-run.
C-M  For example, if the program prints a warning that the title
C-M  dictionary tells it to output E-ERR1, E-ERR2, E-ERR3 all in the
C-M  same computation format field, followed by an operation that says
C-M  the program will only output E-ERR1 and ignore the other 2 EXFOR
C-M  fields, if the user would rather output E-ERR2 and ignore E-ERR1
C-M  and E-ERR3 it is merely necessary to modify the title dictionary
C-M  to ignore E-ERR1 and E-ERR3 and select E-ERR2 and then re-run the
C-M  program.
C-M
C-M  METASTABLE STATE
C-M  ----------------
C-M  The computation format allows the metastable state of the target
C-M  and residual nucleus to be identified. For ratio data metastable
C-M  state of both numerator and denominator of the ratio may be
C-M  defined.
C-M
C-M  The metastable state of the target is identified in column 12 and
C-M  the metastable state of the residual nuclues in column 20. For
C-M  ratio data the metastable state of the denominator target and
C-M  residual nucleus are identified by output the denominator ZA and
C-M  MT in the form ZA.M and MT.M (e.g., 26056.9 and 102.1). Columns
C-M  12 and 20 could contain characters such as M, but to maintain the
C-M  eight output fields in strictly numerical form the denominator
C-M  ZA.M and MT.M will be output in numerical form. The possible
C-M  characters that may appear in columns 12 or 20 and their numerical
C-M  equivalents used with ratio denominator ZA and MT include:
C-M
C-M  Definition    Column 12 or 20     Equivalent
C-M  ----------    ---------------     ----------
C-M  ground              G                0
C-M  m1                  1                1
C-M  m2                  2                2
C-M  m3                  3                3
C-M  m4                  4                4
C-M  m5                  5                5
C-M  unknown             ?                6
C-M  m                   M                7
C-M  more than 1         +                8
C-M  all or total        T                9
C-M  all or total      blank              9
C-M
C-M  By convention if an EXFOR reaction does not specify a metastable
C-M  state the state is defined in the computation format to be..ALL..
C-M  (i.e., blank in column 12 or 20, 9 in ratio ZA or MT).
C-M
C-M  For example, for a ratio if the ZA.m and MT.m are output as
C-M  26056.9 and 102.1, respectively the ratio denominator target is
C-M  26-Fe-56 (all) and the reaction is capture (MT=102) leaving the
C-M  residual nucleus in the m1 state.
C-M
C-M  NOTE: Since most data will not contain a metastable state flag
C-M  the above convention to output the ZA and MT of the denominator
C-M  of ratios allows the user to read and use the denominator ZA and
C-M  MT as integers (effectively ignoring any metastable state flag) or
C-M  if necessary to determine the metastable state.
C-M
C-M  EXFOR STATUS
C-M  ------------
C-M  Column 21 of each computation format record may contain blank
C-M  (status not specified) or one to the following characters:
C-M
C-M  Column 21   Definition
C-M  ---------   ----------
C-M     U        Unnormalized (indicated by unit translation dictionary)
C-M              This condition has priority over the EXFOR status and
C-M              is used to indicate that the data is not in standard
C-M              output units).
C-M     A        Approved by author
C-M     C        Correlated
C-M     D        Dependent
C-M     O        Outdated
C-M     P        Preliminary
C-M     R        Renormalized
C-M     S        Superceded
C-M
C-M  If data has any other EXFOR status (e.g., translated from SCISRS)
C-M  it will be ignored and the status field will be output as blank.
C-M
C-M  INPUT FILES
C-M  -----------
C-M  Unit Name     Description
C-M  ---- -------  -----------
C-M    5  X4INP    X4TOC4.INP  Input defining filenames (fixed filename)
C-M   10  X4       EXFOR data to be translated (default 'X4.DAT')
C-M   12  EXFOR14A EXFOR reaction dictionary (default 'EXFOR14A.DAT')
C-M   14  EXFOR24A EXFOR title dictionary (default 'EXFOR24A.DAT')
C-M   15  EXFOR25A EXFOR units dictionary (default 'EXFOR25A.DAT')
C-M   16  ATMASS   Audi-Wapstra mass table (default 'mass.mas03')
C-M
C-M  OUTPUT FILES
C-M  ------------
C-M  Unit Name     Description
C-M  ---- -------  -----------
C-M    4  X4NEW    List of all undefined EXFOR reactions, titles
C-M                and units found during the translation, if any
C-M                (default 'NEWX4.DAT')
C-M    6  X4LST    X4TOC4.LST output report (default 'X4TOC4.LST')
C-M   11  C4       Output data in computation format (default 'C4.DAT')
C-M
C-M  SCRATCH FILES
C-M  -------------
C-M  NONE
C-M
C-M  INPUT PARAMETERS
C-M  ----------------
C-M  The input file contains the list of files in the following order:
C-M    X4       EXFOR data (to be translated)
C-M    C4       Output data in computation format
C-M    EXFOR14A EXFOR reaction dictionary
C-M    EXFOR24A EXFOR title dictionary
C-M    EXFOR25A EXFOR units dictionary
C-M    ATMASS   Audi-Wapstra mass table
C-M  If any of the filenames is blank or if an end-of-file mark is
C-M  encountered, the remaining filenames assume their default
C-M  values.
C-M
C-M  REPORTING ERRORS
C-M  ----------------
C-M  In order to improve this code and make future versions more
C-M  compatible for use on as many different types of computers as
C-M  possible please report all compiler diagnostics and/or operating
C-M  problems to the author at the above address.
C-M
C-M  Please remember if you simply report "I'VE GOT A PROBLEM" and do
C-M  not adequately describe exactly how you were using the program
C-M  it will be impossible for the author to help you. When a problem
C-M  arises please write to the author, describe the problem in as much
C-M  detail as possible, identify the version of the program that you
C-M  are using (e.g. Version 2001-3) and send the following information
C-M  in computer-readable form (e-mail, floppy disc, etc.) to the author:
C-M
C-M  (1) A copy of the program you are using
C-M  (2) A copy of compiler diagnostics (if any)
C-M  (3) A copy of the JCL deck you used to execute the program
C-M  (4) A copy of the 3 translation dictionaries you are using
C-M  (5) A copy of the EXFOR format data you using
C-M  (6) A copy of the computation format data you produce
C-M  (7) A copy of the output report from the program.
C-M
C-M  Without all of this information it is impossible to exactly
C-M  simulate the problem that you ran and to determine the source
C-M  of your problem.
C-M
C***** COMPUTER DEPENDENT CODING ******
C-M
C-M COMPUTER DEPENDENT CODING
C-M -------------------------
C-M
C-M * This program is designed to be used with a Fortran-77 or
C-M  Fortran-90 compiler.
C-M
C-M * The only compiler dependent format statements involve,
C-M  (1) CHARACTER*1 and CHARACTER*4
C-M  (2) Testing for errors and end of file during reads.
C-M
C-M * It is assumed that characters are stored in successive storage
C-M   locations and that characters may be treated as continuous strings
C-M   of characters in either CHARACTER*4 or CHARACTER*1 format.
C-M
C-M * For example, if one subroutine contains,
C-M
C-M   CHARACTER*4 BCD
C-M   DIMENSION BCD(10)
C-M
C-M  the array BCD is assumed to be an array of 40 characters in
C-M  successive byte locations.
C-M
C-M  It is assumed that this array can be passed as an argument to
C-M  another subroutine and used as CHARACTER*1, e.g.,
C-M
C-M   CALL DUMMY(BCD)
C-M
C-M   SUBROUTINE DUMMY(BCD)
C-M   CHARACTER*1 BCD
C-M   DIMENSION BCD(40)
C-M
C-M * This convention will work on all 32 bit per word computers (e.g.,
C-M   IBM or IBM compatible computers).
C-M
C-M * For longer word length computers (e.g., CDC or CRAY) it is
C-M   suggested that before implementing and using this program the
C-M   user first verify that character strings can be treated as
C-M   described above, e.g., write a simple program to read a character
C-M   string of 40 characters in CHARACTER*4 format, pass it to a
C-M   subroutine which uses the character string in CHARACTER*1 format
C-M   and print the character string in the subroutine. If the character
C-M   string is printed as a continuous string you will be able to use
C-M   this program. If the character string is not printed as a
C-M   continuous string it is not recommended that you use this program.
C-M
C-M * This program using the Fortran-77 convention for testing for
C-M   reading errors and end of file during reads, e.g.,
C-M
C-M   READ(10,1000,END=100,ERR=200) A,B,C,D
C-M
C***** COMPUTER DEPENDENT CODING ******
      INTEGER      OUTP,OTAPE
      CHARACTER*40 X4,X4NEW,C4,X4INP,X4LST,FLNM,BLNK
      CHARACTER*60 EXFOR14A,EXFOR24A,EXFOR25A,FLNM60,ATMASS
      CHARACTER*11 CARD1,KEYTAB,ENDSUB
      CHARACTER*1  CARD2,ENT,SUBENT
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDS/CARD1(6),CARD2(14)
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T07300
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T07310
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T07320
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T07330
      COMMON/POINTR/MPOINT(9)                                           X4T07340
      DIMENSION KEYTAB(5)
      DATA KEYTAB/
     & 'SUBENT     ','NOSUBENT   ','BIB        ','COMMON     ',                                             X4T07400
     & 'DATA       '/
      DATA ENDSUB/'ENDSUBENT  '/
C-----DEFINE ALL I/O UNITS.
      DATA BLNK     /'                                        '/
     1     X4INP    /'X4TOC4.INP'/
     1     X4LST    /'X4TOC4.LST'/
     2     X4       /'X4.DAT'/
     3     C4       /'C4.DAT'/
     4     X4NEW    /'NEWX4.DAT'/
     5     EXFOR14A /'EXFOR14A.DAT'/
     6     EXFOR24A /'EXFOR24A.DAT'/
     7     EXFOR25A /'EXFOR25A.DAT'/
     8     ATMASS   /'mass.mas03'/
      NEWX4=4                                                           X4T07440
      INP=5                                                             X4T07450
      OUTP=8
      ITAPE=10                                                          X4T07470
      OTAPE=11                                                          X4T07480
      NTAPE1=12                                                         X4T07490
      NTAPE2=14                                                         X4T07500
      NTAPE3=15                                                         X4T07510
      NMASS=16
      OPEN (UNIT=INP,FILE=X4INP,STATUS='OLD',ERR=9)
      READ (INP,'(A40)',END=9) FLNM
      IF(FLNM.NE.BLNK) X4=FLNM
      READ (INP,'(A40)',END=9) FLNM
      IF(FLNM.NE.BLNK) C4=FLNM
      READ (INP,'(A60)',END=9) FLNM60
      IF(FLNM60(1:40).NE.BLNK) EXFOR14A=FLNM60
      READ (INP,'(A60)',END=9) FLNM60
      IF(FLNM60(1:40).NE.BLNK) EXFOR24A=FLNM60
      READ (INP,'(A60)',END=9) FLNM60
      IF(FLNM60(1:40).NE.BLNK) EXFOR25A=FLNM60
      READ (INP,'(A60)',END=9) FLNM60
      IF(FLNM60(1:40).NE.BLNK) ATMASS=FLNM60
    9 CLOSE(UNIT=INP)
C*
      WRITE(*,*)'INPUT  FILE : ',X4INP
      WRITE(*,*)'SOURCE FILE : ',X4
      WRITE(*,*)'OUTPUT FILE : ',C4
      OPEN (UNIT=ITAPE ,FILE=X4      ,STATUS='OLD')
      OPEN (UNIT=NTAPE1,FILE=EXFOR14A,STATUS='OLD')
      OPEN (UNIT=NTAPE2,FILE=EXFOR24A,STATUS='OLD')
      OPEN (UNIT=NTAPE3,FILE=EXFOR25A,STATUS='OLD')
      OPEN (UNIT=OUTP  ,FILE=X4LST   ,STATUS='UNKNOWN')
      OPEN (UNIT=OTAPE ,FILE=C4      ,STATUS='UNKNOWN')
      OPEN (UNIT=NEWX4 ,FILE=X4NEW   ,STATUS='UNKNOWN')
      OPEN (UNIT=NMASS ,FILE=ATMASS,STATUS='OLD',ERR=22)
      GO TO 24
   22   NMASS=-NMASS
   24 CONTINUE
C-----INITIALIZE COUNTS.                                                X4T07520
      DO I=1,9
        MPOINT(I)=0
      END DO
C-----PRINT TITLE FOR OUTPUT.                                           X4T07550
      WRITE(OUTP,6000)                                                  X4T07560
C-----READ REACTION VS. MF/MT TABLE                                     X4T07570
      CALL MFMTIN(NTAPE1)                                               X4T07580
C-----READ COLUMN HEADINGS VS. MF/FIELDS.                               X4T07590
      CALL TITLEI(NTAPE2)                                               X4T07600
C-----READ UNITS AND CONVERSION FACTORS TO STANDARD UNITS.              X4T07610
      CALL UNITI(NTAPE3)                                                X4T07620
      WRITE(OUTP,6030)                                                  X4T07630
C                                                                       X4T07640
C     READ EXFOR CARDS AND PROCESS SUBENT, BIB, COMMON OR DATA.         X4T07650
C                                                                       X4T07660
   20 READ(ITAPE,1000,END=110,ERR=100) CARD1,CARD2                      X4T07670
      DO I=1,5
        INKEY=I
        IF(CARD1(1).EQ.KEYTAB(INKEY)) GO TO 50
      END DO
      GO TO 20                                                          X4T07740
C-----PROCESS SUBENT CARD.                                              X4T07750
   50 IF(INKEY.GT.2) GO TO 60                                           X4T07760
      CALL SUBIN                                                        X4T07770
      GO TO 20                                                          X4T07780
C-----TRANSLATE N1, N2 FOR BIB, COMMON OR DATA.                         X4T07790
   60 CALL INTGER(CARD1(2),N1,11)
      CALL INTGER(CARD1(3),N2,11)
      IF(INKEY.NE.3) GO TO 90                                           X4T07820
      CALL BIBIN                                                        X4T07830
C-----IF SAN>1 AND NO REACTIONS TRANSLATED SKIP SUBENTRY.               X4T07840
      IF(ISAN.LE.1) GO TO 20                                            X4T07850
      IF(KSANR.GT.0) GO TO 20                                           X4T07860
      MPOINT(2)=MPOINT(2)+1                                             X4T07870
   70 READ(ITAPE,1000,END=110,ERR=100) CARD1,CARD2                      X4T07880
      IF(CARD1(1).NE.ENDSUB) GO TO 70
      GO TO 20                                                          X4T07920
C-----PROCESS COMMON OR DATA.                                           X4T07930
   90 IF(INKEY.EQ.4) CALL COMIN                                         X4T07940
      IF(INKEY.EQ.5) CALL DATIN                                         X4T07950
      GO TO 20                                                          X4T07960
C-----ERROR READING EXFOR DATA.                                         X4T07970
  100 WRITE(OUTP,6020)                                                  X4T07980
      GO TO 120                                                         X4T07990
C-----END OF RUN. PRINT SUMMARY OF TRANSLATION.                         X4T08000
  110 WRITE(OUTP,6010) MPOINT                                           X4T08010
  120 END FILE OTAPE                                                    X4T08020
      END FILE NEWX4                                                    X4T08030
      STOP                                                              X4T08040
 1000 FORMAT(6A11,14A1)
 6000 FORMAT(' TRANSLATE DATA FROM EXFOR TO COMPUTATION FORMAT',
     1 ' (X4TOC4 VERSION 05/07)'/1X,70('=')/
     2 ' READING TRANSLATION TABLES'/1X,70('='))
 6010 FORMAT(1X,70('=')/' TRANSLATION SUMMARY'/1X,70('=')/              X4T08090
     1 ' SUBENTRIES TRANSLATED--------',I7/                             X4T08100
     2 ' SUBENTRIES SKIPPED-----------',I7,' (NO OUTPUT)'/              X4T08110
     3 ' POINTS READ------------------',I7/                             X4T08120
     4 ' POINTS TRANSLATED------------',I7/                             X4T08130
     5 ' DATA FIELDS NOT DEFINED------',I7,' (NO OUTPUT)'/              X4T08140
     6 ' DATA FIELDS BLANK------------',I7,' (NO OUTPUT)'/              X4T08150
     7 ' UNDEFINED REACTIONS----------',I7/                             X4T08160
     8 ' UNDEFINED TITLES-------------',I7/                             X4T08170
     9 ' UNDEFINED UNITS--------------',I7/1X,70('='))                  X4T08180
 6020 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     X4T08190
 6030 FORMAT(1X,70('=')/'    AN SAN PROJECT  TARGET RESIDUAL',          X4T08200
     1 '  MF   MT REACTION'/                                            X4T08210
     1 1X,70('='))                                                      X4T08220
      END                                                               X4T08230
      SUBROUTINE SUBIN                                                  X4T08240
C                                                                       X4T08250
C     SUBENT OR NOSUBENT CARD. INITIALIZE COUNTERS AND ARRAYS.          X4T08260
C                                                                       X4T08270
      INTEGER      OUTP,OTAPE
      CHARACTER*11 CARD1
      CHARACTER*1  CARD2,ENT,SUBENT,AUTH1,AUTHN,REFER1,REFERN,STAT1
     1            ,STATN,BLANK
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDS/CARD1(6),CARD2(14)
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T08340
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T08350
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T08360
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T08370
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T08380
      COMMON/AUTHI/IAUTH,NAUTH                                          X4T08390
      COMMON/AUTHC/AUTH1(25),AUTHN(25)                                  X4T08400
      COMMON/REFERI/IREF,NREF                                           X4T08410
      COMMON/REFERC/REFER1(4),REFERN(4)                                 X4T08420
      COMMON/STATUC/STAT1,STATN                                         X4T08430
      DATA BLANK/' '/                                                   X4T08440
C                                                                       X4T08450
C     CHECK FOR NEW ENTRY OR SUBENTRY 1.                                X4T08460
C                                                                       X4T08470
C-----ALLOW FOR EXFOR FILES WITHOUT IDENTS IN COLUMNS 67-80
C-----READ ENT/SUBENT FROM COLUMNS 12-22 IF COLUMNS 67-80 ARE BLANK
C
      DO I=1,14
        IF(CARD2(I).NE.BLANK) GO TO 8
      END DO
      DO I=1,8
        CARD2(I)=CARD1(2)(I+3:I+3)
      END DO
    8 CONTINUE
      IRESET=0                                                          X4T08480
C-----SAVE ENTRY NUMBER.                                                X4T08490
      DO I=1,5
        IF(ENT(I).NE.CARD2(I)) IRESET=1
        ENT(I)=CARD2(I)
      END DO
C-----SAVE SUBENTRY.                                                    X4T08530
      DO I=1,3
        SUBENT(I)=CARD2(I+5)
      END DO
C-----CONVERT SUBENTRY NUMBER TO INTEGER.                               X4T08560
      CALL INTGER(CARD2(6),ISAN,3)                                      X4T08570
      IF(ISAN.EQ.1) IRESET=1                                            X4T08580
C                                                                       X4T08590
C     RESET COUNTS AND ARRAYS.                                          X4T08600
C                                                                       X4T08610
      IF(IRESET.EQ.0) GO TO 50                                          X4T08620
C-----NEW ENTRY OR SUBENTRY 1. RESET COMMON SUBENT COUNTS AND ARRAYS.   X4T08630
      ICOM1=0                                                           X4T08640
      KSAN1=0                                                           X4T08650
      STAT1=BLANK                                                       X4T08660
      IAUTH=0                                                           X4T08670
      DO I=1,25
        AUTH1(I)=BLANK
      END DO
      IREF=0                                                            X4T08700
      DO I=1,4
        REFER1(I)=BLANK
      END DO
C-----RESET ORDINARY SUBENT COUNTS AND ARRAYS.                          X4T08730
   50 ICOMN=ICOM1                                                       X4T08740
      IDATN=0                                                           X4T08750
      KSANR=KSAN1                                                       X4T08760
      STATN=BLANK                                                       X4T08770
      NAUTH=0                                                           X4T08780
      DO I=1,25
        AUTHN(I)=BLANK
      END DO
      NREF=0                                                            X4T08810
      DO I=1,4
        REFERN(I)=BLANK
      END DO
      RETURN                                                            X4T08840
      END                                                               X4T08850
      SUBROUTINE BIBIN                                                  X4T08860
C                                                                       X4T08870
C     BIB CARD READ. PROCESS ENTIRE BIB SECTION.                        X4T08880
C                                                                       X4T08890
      INTEGER      OUTP,OTAPE
      CHARACTER*10 BIBKEY,KEYWD1
      CHARACTER*1  CARD1,CARD2,KEYWD2,STAT1,STATN,AUTH1,AUTHN,REFER1
     1            ,REFERN,BLANK
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T08950
      COMMON/BIBCRD/KEYWD1,KEYWD2,CARD1(55),CARD2(14)
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T08970
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T08980
      COMMON/AUTHI/IAUTH,NAUTH                                          X4T08990
      COMMON/AUTHC/AUTH1(25),AUTHN(25)                                  X4T09000
      COMMON/REFERI/IREF,NREF                                           X4T09010
      COMMON/REFERC/REFER1(4),REFERN(4)                                 X4T09020
      COMMON/STATUC/STAT1,STATN                                         X4T09030
      DIMENSION BIBKEY(9)
      DATA BLANK/' '/                                                   X4T09050
      DATA BIBKEY/                                                      X4T09060
     1 'ENDBIB    ','REACTION  ','ISO-QUANT ','NUC-QUANT ',
     5 'CMPD-QUANT','STATUS    ','REFERENCE ','AUTHOR    ',                                              X4T09140
     9 'TITLE     '/
C-----READ ALL BIB CARDS AND LIST REQUIRED KEYWORDS AND CONTINUATIONS.  X4T09160
   10 READ(ITAPE,1000,END=80,ERR=80) KEYWD1,KEYWD2,CARD1,CARD2          X4T09170
   20 DO K=1,9
        IF(KEYWD1.EQ.BIBKEY(K)) GO TO 50
      END DO
C-----REQUIRED KEYWORD NOT FOUND. CONTINUE READING.                     X4T09250
      GO TO 10                                                          X4T09260
C-----RETURN ON ENDBIB. OTHERWISE PROCESS.                              X4T09270
   50 IF(K.EQ.1) GO TO 70                                               X4T09280
C-----PROCESS ISO-QUNT, CMP-QUANT, NUC-QUANT OR REACTION.               X4T09290
      IF(K.GT.5) GO TO 60                                               X4T09300
      CALL REACTN(K-1)                                                  X4T09310
      GO TO 20                                                          X4T09320
C-----PROCESS STATUS.                                                   X4T09330
   60 IF(K.EQ.6) CALL STATUS                                            X4T09340
C-----PROCESS REFERENCE.                                                X4T09350
      IF(K.EQ.7) CALL REFERS                                            X4T09360
C-----PROCESS AUTHOR.                                                   X4T09370
      IF(K.EQ.8) CALL AUTHOR                                            X4T09380
      GO TO 10                                                          X4T09390
C-----END OF BIB SECTION.                                               X4T09400
   70 RETURN                                                            X4T09410
C-----ERROR READING EXFOR DATA.                                         X4T09420
   80 WRITE(OUTP,6000)                                                  X4T09430
      STOP                                                              X4T09440
 1000 FORMAT(A10,A1,55A1,14A1)
 6000 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     X4T09460
      END                                                               X4T09470
c---zvv+++
c delete SF9 and end-commas from reaction-string
c by V.Zerkin, IAEA-NDS, 2006-04-11
      function deleteSF9(ZAR1,KZAR1)
      CHARACTER*300  ZAR1,tmp,tmp2
      tmp=' '
      tmp(1:1)='('
      tmp(2:KZAR1+1)=ZAR1(1:KZAR1)
      tmp(KZAR1+2:KZAR1+2)=')'
c     call outCharArray(tmp,KZAR1+3)
      deleteSF9=0
      i=replaceStr(tmp,300,',CALC)',')')      !--- Calculated data
      i=replaceStr(tmp,300,',DERIV)',')')     !--- Derived data
      i=replaceStr(tmp,300,',EVAL)',')')      !--- Evaluated data
      i=replaceStr(tmp,300,',EXP)',')')       !--- Evaluated data
      i=replaceStr(tmp,300,',RECOM)',')')     !--- Recommended data
      i=replaceStr(tmp,300,',)',')')          !--- delete comas at the end
c     call outCharArray(tmp,KZAR1+3)
      ll=mylen(tmp)
c     write (*,*) ' LL=',ll
      tmp2=' '
      tmp2(1:ll-2)=tmp(2:ll-1)
      ll=mylen(tmp2)
c     call outCharArray(tmp2,ll+1)
      deleteSF9=ll
      return
      end
      subroutine outCharArray(str,lstr)
      CHARACTER*1 str(lstr)
      WRITE(*,4000) '"',(str(I),I=1,lstr),'"'
 4000 FORMAT(1X,60A1/(10X,60A1))
      end

      function mylen(str)
      CHARACTER*1 str(1)
      mylen=0
      do i=1,300
c       call outCharArray(str(i),1)
        if (str(i).eq.' ') return
        mylen=mylen+1
      end do
      return
      end
      function replaceStr(str0,lstr0,str1,str2)
      CHARACTER(LEN=*) str0,str1,str2
      lstr1=len(str1)
      lstr2=len(str2)
      replaceStr=0
      do i=1,300
        ind=INDEX(str0,str1)
c       write (*,*) ' ind=',ind,' L1=',lstr1,' L2=',lstr2
        if (ind.le.0) return
        lshift=lstr0-(ind+lstr1)
        str0(ind+lstr2:ind+lstr2+lshift)
     & =str0(ind+lstr1:ind+lstr1+lshift)
        str0(ind:ind+lstr2)=str2(1:lstr2)
        replaceStr=replaceStr+1
      end do
      return
      end
c---zvv---
      SUBROUTINE REACTN(KTYPE)                                          X4T09480
C                                                                       X4T09490
C     TRANSLATE EACH REACTION (UP TO 30) SEPERATELY.                    X4T09500
C                                                                       X4T09510
C     ZAR1   = ENTIRE REACTION (BETWEEN BALANCED PARENTHESIS...OUTSIDE  X4T09520
C              PARENTHESIS REMOVED...PRINTED IF REACTION CANNOT BE      X4T09530
C              DECODED).                                                X4T09540
C     ZARBAK = BAKCUP COPY OF ZAR1 (USED FOR COMPLEX REACTIONS).        X4T09550
C     RN     = COMPLEX REACTION WITH ZA REMOVED.                        X4T09560
C     R1     = SIMPLE REACTION WITH ZA REMOVED.                         X4T09570
C                                                                       X4T09580
      INTEGER      OUTP,OTAPE
      CHARACTER*1  BLANK,PARENL,PARENR,ZAR1,ZAN,ZA1,R1,RN,FLAGR,CARD1
     &            ,CARD2,ENT,SUBENT,ZARBAK,KEYWD2,LABCM,ZARES,ZANRES
     &            ,ZANRAT,SLASH,EQUAL,MRAT,COMMA,ZASAVE                                          X4T09620
      CHARACTER*10 KEYWD1,BLANK10
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/BIBCRD/KEYWD1,KEYWD2,CARD1(55),CARD2(14)
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T09660
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T09670
      COMMON/ZART1I/KZAR1                                               X4T09680
      COMMON/ZART1C/ZAR1(300)                                           X4T09690
      COMMON/ZAT1I/KZA1,KR1                                             X4T09700
      COMMON/ZAT1C/ZA1(7),R1(300)                                       X4T09710
      COMMON/RATMET/MRAT                                                X4T09720
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T09730
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T09740
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     X4T09750
     1 LABCM(30)                                                        X4T09760
      COMMON/ZATNC2/RN(300)                                             X4T09770
      COMMON/POINTR/MPOINT(9)                                           X4T09780
      COMMON/RESIDI/KZARES                                              X4T09790
      COMMON/RESIDC/ZARES(7)                                            X4T09800
      DIMENSION ZARBAK(300),ZASAVE(300,10),NSAVE(10)                    X4T09810
      DATA BLANK10/'          '/
      DATA BLANK/' '/                                                   X4T09830
      DATA PARENL/'('/                                                  X4T09840
      DATA PARENR/')'/                                                  X4T09850
      DATA SLASH/'/'/                                                   X4T09860
      DATA COMMA/','/                                                   X4T09870
      DATA EQUAL/'='/                                                   X4T09880
      DATA IZERO/0/                                                     X4T09890
C-----INITIALIZE REACTION COUNT AND SAVED REACTION FLAG.                X4T09900
      KSANR=KSAN1                                                       X4T09910
      ISAVE=0                                                           X4T09920
      KSAVE=1                                                           X4T09930
C                                                                       X4T09940
C     START OF NEW REACTION. COPY ENTIRE REACTION INTO ZAR1 (REACTION   X4T09950
C     MAY BE CONTINUED ONTO MULTIPLE CARDS).                            X4T09960
C                                                                       X4T09970
C-----FIRST CARD HAS ALREADY BEEN READ. BRANCH TO TEST FOR MACHINE      X4T09980
C-----READABLE REACTION.                                                X4T09990
      GO TO 80                                                          X4T10000
C                                                                       X4T10010
C     IF OLD CONVENTION FOR MULTIPLE REACTIONS AND CHARACTERS ARE SAVED X4T10020
C     RESTORE THEM AND CONTINUE TRANSLATION.                            X4T10030
C                                                                       X4T10040
   10 IF(KSAVE.GT.ISAVE) GO TO 70                                       X4T10050
C-----LOAD UP TO 55 CHARACTERS INTO INPUT CARD ARRAY.                   X4T10060
      NSAVEK=NSAVE(KSAVE)                                               X4T10070
      MSAVE=NSAVEK                                                      X4T10080
      IF(MSAVE.GT.55) MSAVE=55                                          X4T10090
      DO I=1,MSAVE
        CARD1(I)=ZASAVE(I,KSAVE)
      END DO
      IF(MSAVE.GE.55) GO TO 40                                          X4T10120
      NN=MSAVE+1                                                        X4T10130
      DO I=NN,55
        CARD1(I)=BLANK
      END DO
C-----IF ANY CHARACTERS REMAIN SHIFT THEN FORWARD IN SAVED ARRAY.       X4T10160
   40 IF(MSAVE.GE.NSAVEK) GO TO 60                                      X4T10170
      II=0                                                              X4T10180
      JJ=MSAVE+1                                                        X4T10190
      DO J=JJ,NSAVEK
        II=II+1
        ZASAVE(II,KSAVE)=ZASAVE(J,KSAVE)
      END DO
      NSAVE(KSAVE)=II                                                   X4T10230
      GO TO 80                                                          X4T10240
   60 KSAVE=KSAVE+1                                                     X4T10250
      GO TO 80                                                          X4T10260
C-----READ NEXT CARD.                                                   X4T10270
   70 READ(ITAPE,1000,END=630,ERR=630) KEYWD1,KEYWD2,CARD1,CARD2        X4T10280
      ISAVE=0                                                           X4T10290
      KSAVE=1                                                           X4T10300
C-----CONTINUE DECODING IF KEYWORD FIELD IS BLANK.                      X4T10310
      IF(KEYWD1.NE.BLANK10) GO TO 610
C-----TO BE MACHINE READABLE COLUMN 12 MUST CONTAIN (. IF NOT, ASSUME   X4T10330
C-----COMMENT CARD AND SKIP IT.                                         X4T10340
   80 IF(CARD1(1).NE.PARENL) GO TO 10                                   X4T10350
C-----INITIALIZE CHARACTER COUNT, LEVEL AND INDEX TO NEXT CHARACTER.    X4T10360
      KZAR1=0                                                           X4T10370
      LEVEL=1                                                           X4T10380
      LVLMAX=1                                                          X4T10390
      II=2                                                              X4T10400
C-----INITIALIZE CROSS SECTION RATIO FLAG OFF.                          X4T10410
      IMRATS=0                                                          X4T10420
C-----SAVE REACTION FLAG FROM COLUMN 11 AND INITIALIZE TARGET AND       X4T10430
C-----RESIDUAL NUCLEUS, RATIO DENOMINATOR ZA AND MT, REACTION MF AND MT X4T10440
C-----AND CENTER-OF-MASS FLAG.                                          X4T10450
      KSANP=KSANR+1                                                     X4T10460
      IF(KSANP.GT.30) GO TO 620                                         X4T10470
      FLAGR(KSANP)=KEYWD2                                               X4T10480
      KZANRS(KSANP)=0                                                   X4T10490
      DO I=1,7
        ZAN(I,KSANP)=BLANK
        ZANRAT(I,KSANP)=BLANK
        ZANRES(I,KSANP)=BLANK
      END DO
      INPART(KSANP)=0                                                   X4T10540
      MFR(KSANP)=0                                                      X4T10550
      MTR(KSANP)=0                                                      X4T10560
      MTRAT(KSANP)=0                                                    X4T10570
      LABCM(KSANP)=BLANK                                                X4T10580
C                                                                       X4T10590
C     START OF NEW CARD (EITHER NEW REACTION OR CONTINUATION CARD).     X4T10600
C                                                                       X4T10610
C-----COPY UP TO BALANCED PARENTHESIS.                                  X4T10620
  100 DO 130 I=II,55                                                    X4T10630
      IF(CARD1(I).EQ.BLANK) GO TO 130                                   X4T10640
      IF(CARD1(I).NE.PARENL) GO TO 110                                  X4T10650
      LEVEL=LEVEL+1                                                     X4T10660
      IF(LEVEL.GT.LVLMAX) LVLMAX=LEVEL                                  X4T10670
      GO TO 120                                                         X4T10680
  110 IF(CARD1(I).NE.PARENR) GO TO 120                                  X4T10690
      LEVEL=LEVEL-1                                                     X4T10700
      IF(LEVEL.EQ.0) GO TO 140                                          X4T10710
  120 KZAR1=KZAR1+1                                                     X4T10720
      IF(KZAR1.GT.300) GO TO 600                                        X4T10730
      ZAR1(KZAR1)=CARD1(I)                                              X4T10740
  130 CONTINUE                                                          X4T10750
C-----PARENTHESIS NOT BALANCED YET. READ NEXT CARD.                     X4T10760
      READ(ITAPE,1000,END=630,ERR=630) KEYWD1,KEYWD2,CARD1,CARD2        X4T10770
C-----ERROR IF KEYWORD FIELD IS NOT BLANK.                              X4T10780
      IF(KEYWD1.NE.BLANK10) GO TO 600
C-----RESET TO BEGIN SCAN AT BEGINNING OF NEXT CARD.                    X4T10800
      II=1                                                              X4T10810
      GO TO 100                                                         X4T10820
C                                                                       X4T10830
C     ENTIRE REACTION COPIED. SAVE IT. DETERMINE IF THIS IS A SIMPLE    X4T10840
C     OR COMPLEX REACTION                                               X4T10850
C     FOR SIMPLE REACTION KEYWORD,                                      X4T10860
C     REACTION IMPLIES ONLY 2 SETS OF PARENTHESIS.                      X4T10870
C     OTHERS SUCH AS ISO-QUANT IMPLIES ONLY 1 SET OF PARENTHESIS.       X4T10880
C                                                                       X4T10890
  140 KZABAK=KZAR1                                                      X4T10900
c---zvv+++
c     WRITE(*,4000) ENT,ISAN,'<',(ZAR1(I),I=1,KZAR1),'>'     !---zvv-tst
      KZAR1=deleteSF9(ZAR1,KZAR1)
      KZABAK=KZAR1
c     WRITE(*,4000) ENT,ISAN,'<',(ZAR1(I),I=1,KZAR1),'>'     !---zvv-tst
c---zvv---
      DO I=1,KZABAK
        ZARBAK(I)=ZAR1(I)
      END DO
      IF(KZABAK.GE.60) GO TO 180
      J=KZABAK+1
      DO I=J,60
        ZARBAK(I)=BLANK
      END DO
  180 IF(KTYPE.EQ.1.AND.LVLMAX.LE.2) GO TO 540                          X4T10970
      IF(LVLMAX.LE.1) GO TO 540                                         X4T10980
C                                                                       X4T10990
C     FOR OLD ISO-QUANT FORMALISM TEST FOR COMPLETE REACTIONS SEPERATED X4T11000
C     BY COMMAS. IF FOUND SAVE AND PROCESS EACH REACTION SEPERATELY.    X4T11010
C                                                                       X4T11020
      IF(KTYPE.EQ.1.OR.ISAVE.GT.0) GO TO 260                            X4T11030
      J=1                                                               X4T11040
      ISAVE=0                                                           X4T11050
      KSAVE=1                                                           X4T11060
      LVLOLD=0                                                          X4T11070
  190 DO I=J,KZABAK
        IF(ZARBAK(I).EQ.PARENL) LVLOLD=LVLOLD+1
        IF(ZARBAK(I).EQ.PARENR) LVLOLD=LVLOLD-1
        IF(LVLOLD.EQ.0) GO TO 210
      END DO
      GO TO 260                                                         X4T11130
  210 J=I+1                                                             X4T11140
      IF(J.GE.KZABAK) GO TO 260                                         X4T11150
      IF(ZARBAK(J).NE.COMMA) GO TO 190                                  X4T11160
C-----MULTIPLE REACTIONS FOUND. DEFINE FIRST REACTION AS BEGINNING UP TOX4T11170
C-----CURRENT POINT.                                                    X4T11180
      ISAVE=1                                                           X4T11190
      DO J=1,I
        ZASAVE(J,ISAVE)=ZARBAK(J)
      END DO
      NSAVE(ISAVE)=I                                                    X4T11220
C-----COLLECT REMAINING REACTIONS.                                      X4T11230
  230 ISAVE=ISAVE+1                                                     X4T11240
      LSAVE=0                                                           X4T11250
      LVLOLD=0                                                          X4T11260
      J=I+2                                                             X4T11270
      DO 240 I=J,KZABAK                                                 X4T11280
      LSAVE=LSAVE+1                                                     X4T11290
      ZASAVE(LSAVE,ISAVE)=ZARBAK(I)                                     X4T11300
      IF(ZARBAK(I).EQ.PARENL) LVLOLD=LVLOLD+1                           X4T11310
      IF(ZARBAK(I).EQ.PARENR) LVLOLD=LVLOLD-1                           X4T11320
      IF(LVLOLD.NE.0) GO TO 240                                         X4T11330
      IF(I.EQ.KZABAK) GO TO 250                                         X4T11340
      IF(ZARBAK(I+1).EQ.COMMA) GO TO 250                                X4T11350
  240 CONTINUE                                                          X4T11360
      I=KZABAK                                                          X4T11370
  250 NSAVE(ISAVE)=LSAVE                                                X4T11380
      IF(I.LT.KZABAK) GO TO 230                                         X4T11390
      GO TO 10                                                          X4T11400
C                                                                       X4T11410
C     COMPLEX REACTION. BREAK INTO PARTS AND DECODE EACH PART SEPERATELYX4T11420
C     SAVE ALL ENCLOSING PARENTHESIS AND OTHER CHARACTERS TO DEFINE     X4T11430
C     COMPLEX REACTION WITHOUT ZA.                                      X4T11440
C                                                                       X4T11450
C-----INCREMENT REACTION COUNT. ALLOW NO MORE THAN 30 REACTIONS.        X4T11460
  260 LOOP=0                                                            X4T11470
C-----COPY ALL LEADING LEFT PARENTHESIS INTO RN.                        X4T11480
      DO IBAK=1,KZABAK
        IF(ZARBAK(IBAK).NE.PARENL) GO TO 320
        RN(IBAK)=ZARBAK(IBAK)
      END DO
C                                                                       X4T11530
C     CANNOT TRANSLATE. ATEMPT TO TRANSLATE ENTIRE REACTION.            X4T11540
C                                                                       X4T11550
  280 DO I=1,7
        ZA1(I)=ZAN(I,KSANP)
        ZARES(I)=ZANRES(I,KSANP)
      END DO
      CALL MFMTX(ZARBAK,INPART(KSANP),MFR(KSANP),MTR(KSANP),            X4T11590
     1 IRFLAG(KSANP),KNOWN)                                             X4T11600
  300 WRITE(OUTP,6030) ENT,ISAN,INPART(KSANP),ZA1,ZARES,                X4T11610
     1 MFR(KSANP),MTR(KSANP),FLAGR(KSANP),(ZARBAK(I),I=1,KZABAK)        X4T11620
C-----INCREASE REACTION COUNT IF MF/MT ARE BOTH POSITIVE.               X4T11630
      IF(MFR(KSANP).LE.0.OR.MTR(KSANP).LE.0) GO TO 310                  X4T11640
      KSANR=KSANP                                                       X4T11650
      GO TO 10                                                          X4T11660
C                                                                       X4T11670
C     WRITE REACTION TO NEWX4 FILE.                                     X4T11680
C                                                                       X4T11690
  310 IF(KNOWN.GT.0) GO TO 10                                           X4T11700
      WRITE(NEWX4,4000) ENT,ISAN,(ZARBAK(I),I=1,KZABAK)                 X4T11710
      MPOINT(7)=MPOINT(7)+1                                             X4T11720
      GO TO 10                                                          X4T11730
C-----DEFINE INITIALIZE NUMBER OF CHARACTERS COPIED.                    X4T11740
  320 KRN=IBAK-1                                                        X4T11750
C                                                                       X4T11760
C     START OF SIMPLE REACTION FOUND. COPY TO BALANCED PARENTHESIS.     X4T11770
C                                                                       X4T11780
  330 LVL=1                                                             X4T11790
      JBAK=IBAK                                                         X4T11800
      KZAR1=0                                                           X4T11810
      DO IBAK=JBAK,KZABAK
        IF(ZARBAK(IBAK).EQ.PARENL) LVL=LVL+1
        IF(ZARBAK(IBAK).EQ.PARENR) LVL=LVL-1
        IF(LVL.EQ.0) GO TO 350
        KZAR1=KZAR1+1
        ZAR1(KZAR1)=ZARBAK(IBAK)
      END DO
      GO TO 280                                                         X4T11890
C-----SIMPLE REACTION DEFINED (IN ZAR1). TRANSLATE IT.                  X4T11900
  350 CALL REACT1                                                       X4T11910
C-----SEE IF REACTION HAS BEEN TRANSLATED.                              X4T11920
      IF(KR1.LE.0) GO TO 280                                            X4T11930
C-----DEFINE MF/MT EQUIVALENT.                                          X4T11940
      IF(KR1.GE.60) GO TO 370                                           X4T11950
      JR1=KR1+1                                                         X4T11960
      DO I=JR1,60
        R1(I)=BLANK
      END DO
  370 CALL MFMTX(R1,INPARX,MFRX,MTRX,IRFLGX,KNOWN)                      X4T11990
C-----SEE IF REACTION CAN BE TRANSLATED. IF NOT, TRY TO TRANSLATE ENTIREX4T12000
C-----REACTION.                                                         X4T12010
      IF(MFRX.LE.0.OR.MTRX.LE.0) GO TO 280                              X4T12020
C-----ONLY ALLOW CROSS SECTION RATIOS FOR CROSS SECTIONS.               X4T12030
      IF(MFRX.NE.3) IMRATS=0                                            X4T12040
C-----AFTER FIRST SIMPLE REACTION INSURE ALL OTHERS HAVE SAME TARGET    X4T12050
C-----AND RESIDUAL ZA (OTHERWISE CANNOT TRANSLATE).                     X4T12060
      IF(LOOP.EQ.0) GO TO 420                                           X4T12070
C-----IF THIS IS SECOND REACTION AND RATIO FLAG IS SET SAVE TARGET AND  X4T12080
C-----PRODUCT ZA AND MT AND CONTINUE DECODING.                          X4T12090
      IF(LOOP.NE.1.OR.IMRATS.LE.0) GO TO 390                            X4T12100
      DO I=1,7
        ZANRAT(I,KSANP)=ZA1(I)
        ZANRAT(I+7,KSANP)=ZARES(I)
      END DO
      MTRAT(KSANP)=MTRX                                                 X4T12140
      GO TO 440                                                         X4T12150
C-----CHECK FOR SAME TARGET AND RESIDUAL ZA....IF NOT, CANNOT TRANSLATE.X4T12160
  390 IF(KZA1.NE.KZAN(KSANP)) GO TO 280                                 X4T12170
      DO I=1,KZA1
        IF(ZAN(I,KSANP).NE.ZA1(I)) GO TO 280
      END DO
      IF(KZARES.NE.KZANRS(KSANP)) GO TO 280                             X4T12210
      IF(KZARES.LE.0) GO TO 440                                         X4T12220
      DO I=1,KZARES
        IF(ZANRES(I,KSANP).NE.ZARES(I)) GO TO 280
      END DO
      GO TO 440                                                         X4T12260
C-----SAVE FIRST TARGET AND RESIDUAL ZA (ONLY CROSS SECTIONS), INCIDENT X4T12270
C-----PARTICLE, MF, MT AND REACTION OPERATION FLAG.                     X4T12280
  420 KZAN(KSANP)=KZA1                                                  X4T12290
      KZANRS(KSANP)=KZARES                                              X4T12300
      DO I=1,7
        ZAN(I,KSANP)=ZA1(I)
        ZANRES(I,KSANP)=ZARES(I)
      END DO
      INPART(KSANP)=INPARX                                              X4T12340
      MFR(KSANP)=MFRX                                                   X4T12350
      MTR(KSANP)=MTRX                                                   X4T12360
      IRFLAG(KSANP)=IRFLGX                                              X4T12370
C-----INCREMENT SIMPLE REACTION COUNT.                                  X4T12380
  440 LOOP=LOOP+1                                                       X4T12390
C-----ADD NEXT SIMPLE REACTION TO COMPLEX REACTION STRING.              X4T12400
      DO I=1,KR1
        KRN=KRN+1
        RN(KRN)=R1(I)
      END DO
C                                                                       X4T12440
C     COPY TO BEGINNING OF NEXT SIMPLE REACTION, OR END OF COMPLEX      X4T12450
C     REACTION. SIMPLE REACTION STARTS AT FIRST CHARACTER AFTER NEXT    X4T12460
C     SET OF LEFT PARENTHESIS.                                          X4T12470
C                                                                       X4T12480
C-----IF ONLY ONE REACTION READ SO FAR AND NEXT CHARACTER IS = ASSUME   X4T12490
C-----REMAINDER OF REACTION EQUIVALENT TO FIRST SIMPLE REACTION.        X4T12500
      IF(LOOP.EQ.1.AND.ZARBAK(IBAK+1).EQ.EQUAL) GO TO 470               X4T12510
C-----IF ONLY ONE REACTION READ SO FAR AND NEXT CHARACTER IS / SET FLAG X4T12520
C-----TO INDICATE POSSIBLE SIMPLE RATIO.                                X4T12530
      IF(LOOP.EQ.1.AND.ZARBAK(IBAK+1).EQ.SLASH) IMRATS=1                X4T12540
C-----COPY UP TO NEXT LEFT PARENTHESIS OR END OF COMPLEX REACTION.      X4T12550
      JBAK=IBAK                                                         X4T12560
      DO IBAK=JBAK,KZABAK
        KRN=KRN+1
        RN(KRN)=ZARBAK(IBAK)
        IF(ZARBAK(IBAK).EQ.PARENL) GO TO 490
      END DO
C-----ENTIRE REACTION COPIED. IF SIMPLE RATIO INCREASE MF BY 200 AND    X4T12620
C-----USE ZA/MF/MT FROM FIRST REACTION. OTHERWISE ATTEMPT TO TRANSLATE  X4T12630
C-----COMPLEX REACTION.                                                 X4T12640
      IF(LOOP.NE.2.OR.IMRATS.NE.1) GO TO 510                            X4T12650
      MFR(KSANP)=MFR(KSANP)+200                                         X4T12660
  470 DO I=1,7
        ZA1(I)=ZAN(I,KSANP)
        ZARES(I)=ZANRES(I,KSANP)
      END DO
      GO TO 300                                                         X4T12700
C-----COPY ALL LEFT PARENTHESIS AND THEN BRANCH BACK TO DEFINE SIMPLE   X4T12710
C-----REACTION.                                                         X4T12720
  490 JBAK=IBAK+1                                                       X4T12730
      IF(JBAK.GT.KZABAK) GO TO 280                                      X4T12740
      DO IBAK=JBAK,KZABAK
        IF(ZAR1(IBAK).NE.PARENL) GO TO 330
        KRN=KRN+1
        RN(KRN)=ZARBAK(IBAK)
      END DO
      GO TO 280                                                         X4T12800
C                                                                       X4T12810
C     COMPLEX TRANSLATION COMPLETED. MOVE TO R1 AND ZA1 TO DEFINE       X4T12820
C     PROJECTILE, MF AND MT.                                            X4T12830
C                                                                       X4T12840
  510 KR1=KRN                                                           X4T12850
      DO I=1,KRN
        R1(I)=RN(I)
      END DO
      DO I=1,7
        ZA1(I)=ZAN(I,KSANP)
        ZARES(I)=ZANRES(I,KSANP)
      END DO
      GO TO 560                                                         X4T12910
C                                                                       X4T12920
C     SIMPLE REACTION. TRANSLATE IT.                                    X4T12930
C                                                                       X4T12940
  540 CALL REACT1                                                       X4T12950
C-----SEE IF REACTION HAS BEEN TRANSLATED.                              X4T12960
      IF(KR1.LE.0) GO TO 280                                            X4T12970
C-----SAVE TARGET ZA.                                                   X4T12980
      KZAN(KSANP)=KZA1                                                  X4T12990
      KZANRS(KSANP)=KZARES                                              X4T13000
      DO I=1,7
        ZAN(I,KSANP)=ZA1(I)
        ZANRES(I,KSANP)=ZARES(I)
      END DO
C                                                                       X4T13040
C     DEFINE MF/MT EQUIVALENT.                                          X4T13050
C                                                                       X4T13060
  560 IF(KR1.GE.60) GO TO 580                                           X4T13070
      JR1=KR1+1                                                         X4T13080
      DO I=JR1,60
        R1(I)=BLANK
      END DO
  580 CALL MFMTX(R1,INPART(KSANP),MFR(KSANP),MTR(KSANP),                X4T13110
     1 IRFLAG(KSANP),KNOWN)                                             X4T13120
      WRITE(OUTP,6030) ENT,ISAN,INPART(KSANP),ZA1,ZARES,                X4T13130
     1 MFR(KSANP),MTR(KSANP),FLAGR(KSANP),(R1(I),I=1,KR1)               X4T13140
C-----INCREASE REACTION COUNT IF MF/MT ARE BOTH POSITIVE.               X4T13150
      IF(MFR(KSANP).LE.0.OR.MTR(KSANP).LE.0) GO TO 310                  X4T13160
      KSANR=KSANP                                                       X4T13170
C-----IF SIMPLE RATIO DEFINE NUMERATOR AND DENOMINATOR TO BE THE SAME.  X4T13180
      IF(MFR(KSANP).NE.203) GO TO 10                                    X4T13190
      DO I=1,7
        ZANRAT(I,KSANP)=ZA1(I)
        ZANRAT(I+7,KSANP)=ZARES(I)
      END DO
C-----ASSUME RATIO OF METASTABLE STATES DEFINED.                        X4T13230
      ZANRAT(14,KSANP)=MRAT                                             X4T13240
      MTRAT(KSANP)=MTR(KSANP)                                           X4T13250
      GO TO 10                                                          X4T13260
C                                                                       X4T13270
C     ERROR DECODING REACTIONS.                                         X4T13280
C                                                                       X4T13290
C-----PARENTHESIS DO NOT BALANCE.                                       X4T13300
  600 WRITE(OUTP,6000) (ZAR1(I),I=1,KZAR1)                              X4T13310
      WRITE(OUTP,6010)                                                  X4T13320
  610 RETURN                                                            X4T13330
C-----OVER 30 REACTIONS.                                                X4T13340
  620 WRITE(OUTP,6020)                                                  X4T13350
      RETURN                                                            X4T13360
C-----ERROR READING EXFOR DATA FILE.                                    X4T13370
  630 WRITE(OUTP,6040)                                                  X4T13380
      STOP                                                              X4T13390
 1000 FORMAT(A10,A1,55A1,14A1)
 4000 FORMAT(1X,5A1,I3,1X,60A1/(10X,60A1))                              X4T13410
 6000 FORMAT(10X,80A1)                                                  X4T13420
 6010 FORMAT(10X,'FORMAT ERROR...PARENTHESIS DO NOT BALANCE')           X4T13430
 6020 FORMAT(10X,'OVER 30 REACTIONS...WILL ONLY TREAT FIRST 30')        X4T13440
 6030 FORMAT(1X,5A1,I4,I8,1X,7A1,2X,7A1,I4,I5,1X,A1,1X,60A1/(46X,60A1)) X4T13450
 6040 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     X4T13460
      END                                                               X4T13470
      SUBROUTINE REACT1                                                 X4T13480
C                                                                       X4T13490
C     DECODE SIMPLE REACTION (ZAR1) INTO Z,A AND BASIC REACTION.        X4T13500
C                                                                       X4T13510
      INTEGER      OUTP,OTAPE
      CHARACTER*1  BLANK,PARENL,PARENR,DASH,COMMA,ZERO,DIGIT,ZAR1
     &            ,ZA1,R1,NX,FLAGR,ZAN,LABCM,ZARES,ZANRES,ZANRAT
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/ZART1I/KZAR1                                               X4T13560
      COMMON/ZART1C/ZAR1(300)                                           X4T13570
      COMMON/ZAT1I/KZA1,KR1                                             X4T13580
      COMMON/ZAT1C/ZA1(7),R1(300)                                       X4T13590
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T13600
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T13610
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     X4T13620
     1 LABCM(30)                                                        X4T13630
      COMMON/RESIDI/KZARES                                              X4T13650
      COMMON/RESIDC/ZARES(7)                                            X4T13660
      DIMENSION DIGIT(10),NX(2)                                         X4T13670
      DATA BLANK/' '/                                                   X4T13680
      DATA PARENL/'('/                                                  X4T13690
      DATA PARENR/')'/                                                  X4T13700
      DATA DASH/'-'/                                                    X4T13710
      DATA COMMA/','/                                                   X4T13720
      DATA ZERO/'0'/                                                    X4T13730
      DATA DIGIT/'0','1','2','3','4','5','6','7','8','9'/               X4T13740
      DATA NX/',','X'/                                                  X4T13750
C-----INITIALIZE RESIDUAL NUCLEUS ZA.                                   X4T13760
      KZARES=0                                                          X4T13770
      DO I=1,7
        ZARES(I)=BLANK
      END DO
C-----DECODE TARGET ZA AND CHECK FOR ERROR.                             X4T13800
      CALL DECOZA(ZAR1,KZAR1,1,INOW,ZA1,KZA1)                           X4T13810
      IF(KZA1.LE.0) GO TO 130                                           X4T13820
C                                                                       X4T13830
C     END OF TARGET Z, A. IF LEFT PARENTHESIS FOUND COPY REACTION UP    X4T13840
C     TO RIGHT PARENTHESIS,E.G. (N,P). OTHERWISE OLD EXFOR FORMAT...    X4T13850
C     COPY REMAINDER OF REACTION.                                       X4T13860
C                                                                       X4T13870
      KR1=0                                                             X4T13880
      IF(ZAR1(INOW).NE.PARENL) GO TO 90                                 X4T13890
      DO 20 JNOW=INOW,KZAR1                                             X4T13900
      KR1=KR1+1                                                         X4T13910
      R1(KR1)=ZAR1(JNOW)                                                X4T13920
      IF(ZAR1(JNOW).EQ.PARENR) GO TO 30                                 X4T13930
   20 CONTINUE                                                          X4T13940
C-----ERROR. CANNOT LOCATE END OF REACTION.                             X4T13950
      GO TO 120                                                         X4T13960
C                                                                       X4T13970
C     REACTION COPIED. TRANSLATE RESIDUAL NUCLEUS ZA. IF REACTION ENDS  X4T13980
C     WITH ,X DEFINE LENGTH OF RESIDUAL NUCLEUS ZA.                     X4T13990
C                                                                       X4T14000
   30 JNOW=JNOW+1                                                       X4T14010
      IF(JNOW.GT.KZAR1) GO TO 140                                       X4T14020
C-----DECODE RSIDUAL NUCLEUS ZA AND CHECK FOR ERROR.                    X4T14030
      CALL DECOZA(ZAR1,KZAR1,JNOW,INOW,ZARES,KZARES)                    X4T14040
C-----IF LEGAL RESIDUAL FIELD NOT FOUND SKIP TO COPY REMAINDER OF       X4T14050
C-----REACTION (WITHOUT ADVANCING CHARACTER INDEX). IF NEXT CHARACTER   X4T14060
C-----IS COMMA SKIP IT (EMPTY RESIDUAL NUCLEUS FIELD).                  X4T14070
      IF(INOW.GT.JNOW) GO TO 40                                         X4T14080
      IF(ZAR1(JNOW).EQ.COMMA) GO TO 90                                  X4T14090
      GO TO 100                                                         X4T14100
   40 IF(KZARES.LE.0) GO TO 90                                          X4T14110
C-----REMOVE LEADING ZEROES (EXCEPT THE LAST ONE) FROM RESIDUAL NUCLEUS.X4T14120
      DO I=1,5
        IF(ZARES(I).NE.BLANK) GO TO 60
      END DO
      GO TO 80                                                          X4T14160
   60 DO J=I,5
        IF(ZARES(J).NE.ZERO) GO TO 80
        ZARES(J)=BLANK
      END DO
C-----IF REACTION DOES NOT END WITH ,X) IS RESIDUAL CHARACTER COUNT=0   X4T14200
   80 IF(R1(KR1-2).NE.NX(1).OR.R1(KR1-1).NE.NX(2)) KZARES=0             X4T14210
C                                                                       X4T14220
C     COPY REMAINDER OF REACTION.                                       X4T14230
C                                                                       X4T14240
   90 INOW=INOW+1                                                       X4T14250
  100 IF(INOW.GT.KZAR1) GO TO 140                                       X4T14260
      DO JNOW=INOW,KZAR1
        KR1=KR1+1
        R1(KR1)=ZAR1(JNOW)
      END DO
      GO TO 140                                                         X4T14300
C-----ERROR. CANNOT LOCATE END OF REACTION.                             X4T14310
  120 WRITE(OUTP,6000) (ZAR1(I),I=1,KZAR1)                              X4T14320
      WRITE(OUTP,6020)                                                  X4T14330
  130 KZA1=0                                                            X4T14340
      KR1=0                                                             X4T14350
  140 RETURN                                                            X4T14360
 6000 FORMAT(10X,80A1)                                                  X4T14370
 6020 FORMAT(10X,'FORMAT ERROR...CANNOT LOCATE END OF REACTION')        X4T14380
      END                                                               X4T14390
      SUBROUTINE DECOZA(ZARACT,KSIZE,KSTART,KNOW,ZAX,KZAX)              X4T14400
C                                                                       X4T14410
C     DECODE TARGET OR RESIDUAL NUCLEUS ZA.                             X4T14420
C                                                                       X4T14430
C     INPUT                                                             X4T14440
C     =====                                                             X4T14450
C     ZARACT = EXFOR SIMPLE REACTION.                                   X4T14460
C     KSIZE  = LENGTH OF EXFOR SIMPLE REACTION STRING                   X4T14470
C     KSTART = STARTING LOCATION TO BEGIN DECODING ZA.                  X4T14480
C                                                                       X4T14490
C     STARTING AT LOCATION KSTART AND NOT EXTENDING BEYOND LOCATION     X4T14500
C     KSIZE OF THE ARRAY ZARACT THIS ROUTINE EXPECTS TO FIND A STRING   X4T14510
C     OF THE FORM,                                                      X4T14520
C                                                                       X4T14530
C     ZZZ-SS-AAA-M = ZZZ = ATOMIC NUMBER                                X4T14540
C                    SS  = CHEMICAL SYMBOL                              X4T14550
C                    AAA = ATOMIC WEIGHT                                X4T14560
C                    M   = METASTABLE STATE FLAG (MAY NOT BE PRESENT)   X4T14570
C                                                                       X4T14580
C     FOLLOWED BY , OR ( OR )                                           X4T14590
C                                                                       X4T14600
C     THIS ROUTINE WILL REMOVE THE CHEMICAL SYMBOL AND RIGHT ADJUST     X4T14610
C     THE COMBINED ZZZ, AAA AND M TO THE FORM ZZZAAAM. IF ZZZ IS LESS   X4T14620
C     THAN 3 CHARACTERS LONG IT WILL BE RIGHT ADJUSTED TO 3 CHARACTERS  X4T14630
C     WITH LEADING BLANKS. IF AAA IS LESS THAN 3 CHARACTERS LONG IT WILLX4T14640
C     BE RIGHT ADJUSTED WITH LEADING ZEROES. IF M IS NOT PRESENT IT WILLX4T14650
C     BE SET TO BLANK.                                                  X4T14660
C                                                                       X4T14670
C     E.G., 26-FE-56-M = 26056M                                         X4T14680
C                                                                       X4T14690
C     THIS ROUTINE WILL RETURN,                                         X4T14700
C                                                                       X4T14710
C     ZAX  = 7 CHARACTER ZZZAAAM                                        X4T14720
C     KZAX = COUNT OF CHARACTERS IN ZAX                                 X4T14730
C          = 7 - NORMALLY                                               X4T14740
C          = 0 - ERROR (CANNOT DECODE ZA)                               X4T14750
C     KNOW = POINTER TO FIRST CHARACTER IN THE ARRAY ZARACT FOLLOWING   X4T14760
C            THE ZA FIELD.                                              X4T14770
C                                                                       X4T14780
      INTEGER OUTP,OTAPE                                                X4T14790
      CHARACTER*1 ZARACT,ZAX,BLANK,ZERO,PARENL,PARENR,COMMA,DASH,MRAT,  X4T14800
     1 METBCD                                                           X4T14810
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/RATMET/MRAT                                                X4T14830
      DIMENSION ZARACT(300),ZAX(7),METBCD(20)                           X4T14840
      DATA BLANK/' '/                                                   X4T14850
      DATA ZERO/'0'/                                                    X4T14860
      DATA PARENL/'('/                                                  X4T14870
      DATA PARENR/')'/                                                  X4T14880
      DATA COMMA/','/                                                   X4T14890
      DATA DASH/'-'/                                                    X4T14900
C-----INITIALIZE ZA.                                                    X4T14910
      KNOW=KSTART                                                       X4T14920
      KZAX=0                                                            X4T14930
      DO I=1,7
        ZAX(I)=BLANK
      END DO
C-----INITIALIZE METASTABLE FIELD CHARACTER COUNT.                      X4T14960
      MRAT=BLANK                                                        X4T14970
      METAF=0                                                           X4T14980
C-----COPY FIELD IF NOT TWO DASHES BEFORE NEXT COMMA (ONLY RESIDUAL     X4T14990
C-----NUCLEUS FIELD).                                                   X4T15000
      IDASH=0                                                           X4T15010
      DO I=KSTART,KSIZE
        IF(ZARACT(I).EQ.COMMA) GO TO 30
        IF(ZARACT(I).EQ.DASH) IDASH=IDASH+1
      END DO
      RETURN                                                            X4T15060
   30 IF(IDASH.GE.2) GO TO 40                                           X4T15070
      RETURN                                                            X4T15080
C-----INITIALIZE DASH COUNT.                                            X4T15090
   40 IDASH=-1                                                          X4T15100
C-----SET UP LOOP OVER CHARACTERS.                                      X4T15110
      DO 140 KNOW=KSTART,KSIZE                                          X4T15120
C-----SEARCH FOR DASH (-).                                              X4T15130
      IF(ZARACT(KNOW).EQ.DASH) GO TO 50                                 X4T15140
C-----CHARACTER IS NOT A DASH. SKIP CHEMICAL SYMBOL BY SKIPPING ALL     X4T15150
C-----CHARACTERS BETWEEN FIRST AND SECOND DASH.                         X4T15160
      IF(IDASH) 60,140,120                                              X4T15170
C-----CHARACTER IS A DASH.                                              X4T15180
   50 IF(IDASH) 70,100,110                                              X4T15190
C-----NO DASH FOUND YET. DEFINE Z TO BE UP TO 3 CHARACTERS PRECEDING    X4T15200
C-----FIRST DASH (SKIP ALL CHARACTERS AFTER 3).                         X4T15210
   60 IF(KZAX.GE.3) GO TO 140                                           X4T15220
      KZAX=KZAX+1                                                       X4T15230
      ZAX(KZAX)=ZARACT(KNOW)                                            X4T15240
      GO TO 140                                                         X4T15250
C-----FIRST DASH FOUND. IF NECESSARY RIGHT ADJUST Z WITH LEADING BLANKS.X4T15260
   70 IDASH=0                                                           X4T15270
      IF(KZAX.GE.3) GO TO 140                                           X4T15280
      MM=3                                                              X4T15290
      LL=KZAX                                                           X4T15300
      DO 80 M=1,KZAX                                                    X4T15310
      ZAX(MM)=ZAX(LL)                                                   X4T15320
      MM=MM-1                                                           X4T15330
   80 LL=LL-1                                                           X4T15340
      DO M=1,MM
        ZAX(M)=BLANK
      END DO
      KZAX=3                                                            X4T15370
      GO TO 140                                                         X4T15380
C-----SECOND DASH FOUND. CHEMICAL SYMBOL HAS BEEN SKIPPED. SET DASH     X4T15390
C-----COUNT TO COPY A FIELD.                                            X4T15400
  100 IDASH=1                                                           X4T15410
      GO TO 140                                                         X4T15420
C-----MORE THAN 2 DASHES. SET DASH COUNT FOR METASTABLE STATE FIELD.    X4T15430
  110 IDASH=2                                                           X4T15440
      GO TO 140                                                         X4T15450
C-----AFTER SECOND DASH SEARCH FOR END OF STRING...( OR ) OR ,          X4T15460
  120 IF(ZARACT(KNOW).EQ.PARENL.OR.ZARACT(KNOW).EQ.PARENR.OR.           X4T15470
     1 ZARACT(KNOW).EQ.COMMA) GO TO 150                                 X4T15480
C-----NOT THE END OF STRING. IF MORE THAN 2 DASHES SAVE CHARACTERS FROM X4T15490
C-----METASTABLE STATE FIELD.                                           X4T15500
      IF(IDASH.EQ.1) GO TO 130                                          X4T15510
      IF(METAF.GT.20) GO TO 140                                         X4T15520
      METAF=METAF+1                                                     X4T15530
      METBCD(METAF)=ZARACT(KNOW)                                        X4T15540
      GO TO 140                                                         X4T15550
C-----IF 2 DASHES HAVE BEEN FOUND DEFINE A TO BE UP TO NEXT 3 CHARACTERSX4T15560
C-----(SKIP ALL CHARACTERS AFTER 3).                                    X4T15570
  130 IF(KZAX.GE.6) GO TO 140                                           X4T15580
      KZAX=KZAX+1                                                       X4T15590
      ZAX(KZAX)=ZARACT(KNOW)                                            X4T15600
  140 CONTINUE                                                          X4T15610
C-----ERROR. CANNOT LOCATE END OF ZA.                                   X4T15620
      GO TO 190                                                         X4T15630
C-----END OF A FOUND. IF NECESSARY RIGHT ADJUST A WITH LEADING ZEROES.  X4T15640
  150 IF(KZAX.GE.6) GO TO 180                                           X4T15650
      MM=6                                                              X4T15660
      LL=KZAX                                                           X4T15670
      DO M=4,KZAX
        ZAX(MM)=ZAX(LL)
        MM=MM-1
        LL=LL-1
      END DO
      DO M=4,MM
        ZAX(M)=ZERO
      END DO
  180 KZAX=7                                                            X4T15740
C-----IF NECESSARY DECODE METASTABLE STATE FIELD.                       X4T15750
      IF(METAF.GT.0) CALL DECODM(METBCD,METAF,ZAX,MRAT)                 X4T15760
      RETURN                                                            X4T15770
C-----ERROR. CANNOT LOCATE END OF ZA. PRINT ERROR AND SET ZA CHARACTER  X4T15780
C-----COUNT TO ZERO.                                                    X4T15790
  190 WRITE(OUTP,6000) (ZARACT(I),I=KSTART,KSIZE)                       X4T15800
      WRITE(OUTP,6010)                                                  X4T15810
      KZAX=0                                                            X4T15820
      KNOW=KSIZE+1                                                      X4T15830
      RETURN                                                            X4T15840
 6000 FORMAT(10X,80A1)                                                  X4T15850
 6010 FORMAT(10X,'FORMAT ERROR...CANNOT LOCATE END OF ZA')              X4T15860
      END                                                               X4T15870
      SUBROUTINE DECODM(METBCD,METAF,ZAX,MRAT)                          X4T15880
C                                                                       X4T15890
C     DECODE METASTABLE STATE FIELD.                                    X4T15900
C                                                                       X4T15910
      INTEGER OUTP,OTAPE                                                X4T15920
      CHARACTER*1 METBCD,ZAX,MRAT,SLASH,M,PLUS,WHAT,BLANK,TOTAL,MET1    X4T15930
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      DIMENSION METBCD(20),ZAX(7)                                       X4T15950
      DATA SLASH/'/'/                                                   X4T15960
      DATA BLANK/' '/                                                   X4T15970
      DATA M/'M'/                                                       X4T15980
      DATA PLUS/'+'/                                                    X4T15990
      DATA WHAT/'?'/                                                    X4T16000
      DATA TOTAL/'T'/                                                   X4T16010
C                                                                       X4T16020
C     DETERMINE IF THIS IS RATIO OF METASTABLE STATES.                  X4T16030
C                                                                       X4T16040
      JFIELD=1                                                          X4T16050
      DO I=1,METAF
        IF(METBCD(I).EQ.SLASH) GO TO 20
      END DO
C-----NO. SET DENOMINATOR TO BLANK AND DECODE ONLY ONE FIELD.           X4T16090
      MRAT=BLANK                                                        X4T16100
      KFIELD=1                                                          X4T16110
      NFIELD=METAF                                                      X4T16120
      GO TO 30                                                          X4T16130
C-----YES. DECODE 2 FIELDS SEPARATELY.                                  X4T16140
   20 KFIELD=2                                                          X4T16150
      NFIELD=I-1                                                        X4T16160
C                                                                       X4T16170
C     SET UP LOOP OVER FIELDS TO DECODE.                                X4T16180
C                                                                       X4T16190
   30 DO 110 K=1,KFIELD                                                 X4T16200
      ICHAR=NFIELD-JFIELD+1                                             X4T16210
C                                                                       X4T16220
C     IF ONLY ONE CHARACTER USE AS DEFINITION.                          X4T16230
C                                                                       X4T16240
      IF(ICHAR-1) 80,40,50                                              X4T16250
   40 MET1=METBCD(JFIELD)                                               X4T16260
      IF(MET1.EQ.TOTAL) MET1=BLANK                                      X4T16270
      IF(K.EQ.1) ZAX(7)=METBCD(JFIELD)                                  X4T16280
      IF(K.EQ.2) MRAT=METBCD(JFIELD)                                    X4T16290
      GO TO 100                                                         X4T16300
C                                                                       X4T16310
C     IF ONLY TWO CHARACTERS AND FIRST IS M USE SECOND CHARACTER.       X4T16320
C                                                                       X4T16330
   50 IF(METAF.GT.2) GO TO 60                                           X4T16340
      IF(METBCD(JFIELD).NE.M) GO TO 80                                  X4T16350
      IF(K.EQ.1) ZAX(7)=METBCD(JFIELD+1)                                X4T16360
      IF(K.EQ.2) MRAT=METBCD(JFIELD+1)                                  X4T16370
      GO TO 100                                                         X4T16380
C                                                                       X4T16390
C     MORE THAN TWO CHARACTERS. SEE IF SUM OF STATES.                   X4T16400
C                                                                       X4T16410
   60 DO I=JFIELD,NFIELD
        IF(METBCD(I).EQ.PLUS) GO TO 90
      END DO
C                                                                       X4T16450
C     CANNOT DECODE. DEFINE ?                                           X4T16460
C                                                                       X4T16470
   80 IF(K.EQ.1) ZAX(7)=WHAT                                            X4T16480
      IF(K.EQ.2) MRAT=WHAT                                              X4T16490
      GO TO 100                                                         X4T16500
C                                                                       X4T16510
C     SUM OF STATES. DEFINE +                                           X4T16520
C                                                                       X4T16530
   90 IF(K.EQ.1) ZAX(7)=PLUS                                            X4T16540
      IF(K.EQ.2) MRAT=PLUS                                              X4T16550
  100 JFIELD=NFIELD+2                                                   X4T16560
  110 NFIELD=METAF                                                      X4T16570
      RETURN                                                            X4T16580
      END                                                               X4T16590
      SUBROUTINE STATUS                                                 X4T16600
C                                                                       X4T16610
C     DEFINE STATUS.                                                    X4T16620
C                                                                       X4T16630
      CHARACTER*10 KEYWD1
      CHARACTER*1 CARD1,CARD2,KEYWD2,BLANK,PARENL,STAT1,STATN,STATAB    X4T16650
      COMMON/BIBCRD/KEYWD1,KEYWD2,CARD1(55),CARD2(14)
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T16670
      COMMON/STATUC/STAT1,STATN                                         X4T16680
      DIMENSION STATAB(7,7)                                             X4T16690
      DATA BLANK/' '/                                                   X4T16700
      DATA PARENL/'('/                                                  X4T16710
      DATA STATAB/                                                      X4T16720
     1 'P','R','E','L','M',' ','P',                                     X4T16730
     2 'S','P','S','D','D',' ','S',                                     X4T16740
     3 'D','E','P',' ',' ',' ','D',                                     X4T16750
     4 'C','O','R','E','L',' ','C',                                     X4T16760
     5 'A','P','R','V','D',' ','A',                                     X4T16770
     6 'O','U','T','D','T',' ','O',                                     X4T16780
     7 'R','N','O','R','M',' ','R'/                                     X4T16790
C-----INITIALIZE STATUS.                                                X4T16800
      STATN=BLANK                                                       X4T16810
      IF(ISAN.EQ.1) STAT1=BLANK                                         X4T16820
      IF(CARD1(1).NE.PARENL) RETURN                                     X4T16830
C-----FIND END OF STATUS.                                               X4T16840
      DO 20 I=1,7                                                       X4T16850
      DO 10 J=1,6                                                       X4T16860
      IF(STATAB(J,I).EQ.BLANK) GO TO 30                                 X4T16870
      IF(CARD1(J+1).NE.STATAB(J,I)) GO TO 20                            X4T16880
   10 CONTINUE                                                          X4T16890
      GO TO 30                                                          X4T16900
   20 CONTINUE                                                          X4T16910
C-----STATUS NOT DEFINED.                                               X4T16920
      RETURN                                                            X4T16930
C-----DEFINE STATUS.                                                    X4T16940
   30 STATN=STATAB(7,I)                                                 X4T16950
      IF(ISAN.EQ.1) STAT1=STATN                                         X4T16960
      RETURN                                                            X4T16970
      END                                                               X4T16980
      SUBROUTINE REFERS                                                 X4T16990
C                                                                       X4T17000
C     SAVE LATEST YEAR FROM REFERENCES.                                 X4T17010
C                                                                       X4T17020
      INTEGER OUTP,OTAPE                                                X4T17030
      CHARACTER*10 KEYWD1
      CHARACTER*1 CARD1,CARD2,KEYWD2,REFER1,REFERN,BLANK,PARENL,PARENR, X4T17050
     1 COMMA,DIGITS                                                     X4T17060
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/BIBCRD/KEYWD1,KEYWD2,CARD1(55),CARD2(14)
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T17090
      COMMON/REFERI/IREF,NREF                                           X4T17100
      COMMON/REFERC/REFER1(4),REFERN(4)                                 X4T17110
      DIMENSION DIGITS(10)                                              X4T17120
      DATA DIGITS/'1','2','3','4','5','6','7','8','9','0'/              X4T17130
      DATA BLANK/' '/                                                   X4T17140
      DATA PARENL/'('/                                                  X4T17150
      DATA PARENR/')'/                                                  X4T17160
      DATA COMMA/','/                                                   X4T17170
C-----INITIALIZE REFERENCE.                                             X4T17180
      NREF=0                                                            X4T17190
      DO I=1,4
        REFERN(I)=BLANK
      END DO
      IF(CARD1(1).NE.PARENL) GO TO 70                                   X4T17220
C-----FIND END OF REFERENCE AND LAST PRECEDING COMMA.                   X4T17230
      ICOM=0                                                            X4T17240
      LVL=1                                                             X4T17250
      DO IEND=2,55
        IF(CARD1(IEND).EQ.PARENL) LVL=LVL+1
        IF(CARD1(IEND).EQ.PARENR) LVL=LVL-1
        IF(CARD1(IEND).EQ.COMMA) ICOM=IEND
        IF(LVL.LE.0) GO TO 30
      END DO
      GO TO 70                                                          X4T17320
C-----SEARCH FOR YEAR BETWEEN LAST COMMA AND END OF REFERENCE.          X4T17330
   30 IF(ICOM.LE.0) GO TO 70                                            X4T17340
C-----SELECT LAST TWO DIGITS OF YEAR.                                   X4T17350
      IEND=IEND-1                                                       X4T17360
      IDIG=IEND-ICOM                                                    X4T17370
      IF(IDIG.EQ.2) GO TO 60                                            X4T17380
      IF(IDIG.EQ.4) GO TO 40                                            X4T17390
C-----6 DIGIT DATE. SELECT MIDDLE TWO DIGITS AS MONTH.                  X4T17400
      IF(IDIG.GT.6) GO TO 50
C-----(START OF  YEAR...19YY, 20YY) SELECT THIRD AND FOURTH DIGITS.
      IF( (CARD1(ICOM+1).EQ.DIGITS(1).AND.CARD1(ICOM+2).EQ.DIGITS(9))
     &.OR.(CARD1(ICOM+1).EQ.DIGITS(2).AND.CARD1(ICOM+2).LT.DIGITS(4))
     &.OR. CARD1(ICOM+1).EQ.DIGITS(10)) GO TO 50
      GO TO 60
C-----4 DIGIT DATE. IF FIRST DIGITS IS 0 OR 1 (START OF MONTH 1 TO 12   X4T17420
C-----OR START OF  YEAR...1975) SELECT THIRD AND FOURTH DIGITS.         X4T17430
   40 IF( (CARD1(ICOM+1).EQ.DIGITS(1).AND.CARD1(ICOM+2).EQ.DIGITS(9))
     &.OR.(CARD1(ICOM+1).EQ.DIGITS(2).AND.CARD1(ICOM+2).LT.DIGITS(4))
     &.OR. CARD1(ICOM+1).EQ.DIGITS(10)) GO TO 50
C-----IF LAST TWO DIGITS ARE A MONTH (E.G. 00 TO 12...LEADING           X4T17460
C-----DIGIT 0 OR 1) SELECT FIRST TWO DIGITS.                            X4T17470
      IF(CARD1(ICOM+3).EQ.DIGITS(1).OR.                                 X4T17480
     1 CARD1(ICOM+3).EQ.DIGITS(10)) GO TO 60                            X4T17490
   50 ICOM=ICOM+2                                                       X4T17500
   60 NREF=4                                                            X4T17510
      REFERN(1)=PARENL                                                  X4T17520
      REFERN(2)=CARD1(ICOM+1)                                           X4T17530
      REFERN(3)=CARD1(ICOM+2)                                           X4T17540
      REFERN(4)=PARENR                                                  X4T17550
C-----IF THIS IS SUBENTRY 1 SAVE COMMON YEAR.                           X4T17560
   70 IF(ISAN.NE.1) GO TO 90                                            X4T17570
      IREF=NREF                                                         X4T17580
      IF(IREF.LE.0) GO TO 90                                            X4T17590
      DO I=1,4
        REFER1(I)=REFERN(I)
      END DO
   90 RETURN                                                            X4T17620
      END                                                               X4T17630
      SUBROUTINE AUTHOR                                                 X4T17640
C                                                                       X4T17650
C     SAVE FIRST AUTHOR. IF MORE THAN ONE AUTHOR ADD ET.EL.             X4T17660
C                                                                       X4T17670
      INTEGER OUTP,OTAPE                                                X4T17680
      CHARACTER*10 KEYWD1
      CHARACTER*1 CARD1,CARD2,KEYWD2,AUTH1,AUTHN,BLANK,PARENL,PARENR,   X4T17700
     1 COMMA,ETAL                                                       X4T17710
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/BIBCRD/KEYWD1,KEYWD2,CARD1(55),CARD2(14)
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T17740
      COMMON/AUTHI/IAUTH,NAUTH                                          X4T17750
      COMMON/AUTHC/AUTH1(25),AUTHN(25)                                  X4T17760
      DIMENSION ETAL(7)                                                 X4T17770
      DATA BLANK/' '/                                                   X4T17780
      DATA PARENL/'('/                                                  X4T17790
      DATA PARENR/')'/                                                  X4T17800
      DATA COMMA/','/                                                   X4T17810
      DATA ETAL/',','E','T','.','A','L','.'/                            X4T17820
C-----INITIALIZE AUTHOR.                                                X4T17830
      NAUTH=0                                                           X4T17840
      DO I=1,25
        AUTHN(I)=BLANK
      END DO
C-----TO BE MACHINE READABLE COLUMN 11 MUST CONTAIN (                   X4T17870
      IF(CARD1(1).NE.PARENL) GO TO 60                                   X4T17880
C-----DEFINE FIRST AUTHOR UP TO ) OR ,                                  X4T17890
      DO 20 I=2,21                                                      X4T17900
      IF(CARD1(I).EQ.PARENR) GO TO 60                                   X4T17910
      IF(CARD1(I).EQ.COMMA) GO TO 30                                    X4T17920
      NAUTH=NAUTH+1                                                     X4T17930
      AUTHN(NAUTH)=CARD1(I)                                             X4T17940
   20 CONTINUE                                                          X4T17950
      GO TO 60                                                          X4T17960
C-----MORE THAN 1 AUTHOR. INSTER ET.AL. IF THERE IS ROOM.               X4T17970
C-----OTHERWISE JUST INSERT COMMA.                                      X4T17980
   30 IF(NAUTH.LE.13) GO TO 40                                          X4T17990
      NAUTH=NAUTH+1                                                     X4T18000
      AUTHN(NAUTH)=COMMA                                                X4T18010
      GO TO 60                                                          X4T18020
   40 DO I=1,7
        NAUTH=NAUTH+1
        AUTHN(NAUTH)=ETAL(I)
      END DO
C-----IF THIS IS SUBENTRY 1 SAVE COMMON AUTHOR.                         X4T18060
   60 IF(ISAN.NE.1) GO TO 80                                            X4T18070
      IAUTH=NAUTH                                                       X4T18080
      IF(IAUTH.LE.0) GO TO 80                                           X4T18090
      DO I=1,IAUTH
        AUTH1(I)=AUTHN(I)
      END DO
   80 RETURN                                                            X4T18120
      END                                                               X4T18130
      SUBROUTINE COMIN                                                  X4T18140
      INTEGER OUTP,OTAPE                                                X4T18150
      CHARACTER*11 CARD1,TITLE,UNIT,DATUM
      CHARACTER*4  TITLE4
      CHARACTER*1 CARD2,ENT,SUBENT,FLAGI                                X4T18170
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDS/CARD1(6),CARD2(14)
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T18200
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T18210
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADC2/FLAGI(50)                                           X4T18230
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T18240
C-----SAVE TITLES, UNITS AND DATA.                                      X4T18250
      I1=ICOM1+1                                                        X4T18260
      I2=ICOM1+N1                                                       X4T18270
      READ(ITAPE,1010,END=10,ERR=10) (TITLE(I),FLAGI(I),I=I1,I2)                                                         X4T18290
      READ(ITAPE,1020,END=10,ERR=10) ( UNIT(I),I=I1,I2)
      READ(ITAPE,1020,END=10,ERR=10) (DATUM(I),I=I1,I2)
C-----DEFINE COLUMN COUNTS.                                             X4T18320
      ICOMN=I2                                                          X4T18330
      IF(ISAN.EQ.1) ICOM1=I2                                            X4T18340
      RETURN                                                            X4T18350
C-----ERROR READING EXFOR DATA.                                         X4T18360
   10 WRITE(OUTP,6000)                                                  X4T18370
      STOP                                                              X4T18380
 1000 FORMAT(6(2A4,A3),14A1)                                            X4T18390
 1010 FORMAT(6(A10,A1))
 1020 FORMAT(6A11)
 6000 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     X4T18420
      END                                                               X4T18430
      SUBROUTINE DATIN                                                  X4T18440
C                                                                       X4T18450
C     READ DATA POINTS, TRANSLATE AND OUTPUT IN COMPUTATION FORMAT.     X4T18460
C                                                                       X4T18470
      INTEGER OUTP,OTAPE                                                X4T18480
      CHARACTER*11 CARD1,TITLE,UNIT,DATUM,BLANK11
      CHARACTER*9  OUTLIN,BLANK9
      CHARACTER*4  TITLE4,IM78,BLANK4
      CHARACTER*1  CARD2,ENT,SUBENT,FLAGI,FLAGR,ZAN,AUTH1,AUTHN,AUTHK,
     1 REFER1,REFERN,LABCM,STAT1,STATN,BLANK,ZANRES,ZANRAT,DIGITS,ZANOK X4T18510
     1,ZAPO(6,8)                                                        TRKOV
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDS/CARD1(6),CARD2(14)
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T18540
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T18550
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADC2/FLAGI(50)                                           X4T18570
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T18590
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T18600
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T18610
      COMMON/RNOW/ISANR                                                 X4T18620
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     X4T18630
     1 LABCM(30)                                                        X4T18640
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          X4T18650
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T18660
     1 KTFLGX(50),KUFLGX(50)                                            X4T18670
      COMMON/AUTHI/IAUTH,NAUTH                                          X4T18680
      COMMON/AUTHC/AUTH1(25),AUTHN(25)                                  X4T18690
      COMMON/REFERI/IREF,NREF                                           X4T18700
      COMMON/REFERC/REFER1(4),REFERN(4)                                 X4T18710
      COMMON/POINTR/MPOINT(9)                                           X4T18720
      COMMON/STATUC/STAT1,STATN                                         X4T18730
      DIMENSION AUTHK(25),OUTLIN(8),DIGITS(10),ZANOK(7)
      DATA BLANK/' '/                                                   X4T18750
      DATA BLANK4/'    '/                                               X4T18760
      DATA BLANK9/'         '/
      DATA BLANK11/'           '/
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              X4T18770
      DATA ZAPO /' ',' ',' ',' ',' ','0',                               TRKOV
     2           ' ',' ',' ',' ',' ','1',                               TRKOV
     3           ' ',' ','1','0','0','1',                               TRKOV
     4           ' ',' ','1','0','0','2',                               TRKOV
     5           ' ',' ','1','0','0','3',                               TRKOV
     6           ' ',' ','2','0','0','3',                               TRKOV
     7           ' ',' ','2','0','0','4',                               TRKOV
     8           ' ','9','9','9','9','9'/                               TRKOV
C                                                                       X4T18780
C     DEFINE STATUS, AUTHOR AND YEAR FROM CURRENT SUBENTRY OR COMMON    X4T18790
C     SUBENTRY.                                                         X4T18800
C                                                                       X4T18810
      MPOINT(1)=MPOINT(1)+1                                             X4T18820
      IF(STATN.EQ.BLANK) STATN=STAT1                                    X4T18830
      IF(NAUTH.GT.0.OR.IAUTH.LE.0) GO TO 20                             X4T18840
      NAUTH=IAUTH                                                       X4T18850
      DO I=1,NAUTH
        AUTHN(I)=AUTH1(I)
      END DO
   20 IF(NREF.GT.0.OR.IREF.LE.0) GO TO 40                               X4T18880
      NREF=IREF                                                         X4T18890
      DO I=1,NREF
        REFERN(I)=REFER1(I)
      END DO
C-----COPY AUTHOR TO OUTPUT ARRAY.                                      X4T18920
   40 KAUTH=NAUTH                                                       X4T18930
      DO I=1,25
        AUTHK(I)=AUTHN(I)
      END DO
C-----ADD YEAR TO END OF AUTHOR.                                        X4T18960
      IF(NREF.LE.0) GO TO 70                                            X4T18970
      KAUTH=KAUTH+1                                                     X4T18980
      DO I=1,4
        KAUTH=KAUTH+1
        AUTHK(KAUTH)=REFERN(I)
      END DO
C                                                                       X4T19020
C     SAVE TITLES AND UNITS AND DEFINE INITIAL NUMBER OF COLUMNS.       X4T19030
C                                                                       X4T19040
   70 I1=ICOMN+1                                                        X4T19050
      I2=ICOMN+N1                                                       X4T19060
      IDATN=I2                                                          X4T19070
      READ(ITAPE,1010,END=500,ERR=500) (TITLE(I),FLAGI(I),I=I1,I2)                                                         X4T19090
      READ(ITAPE,1020,END=500,ERR=500) (UNIT(I),I=I1,I2)
C-----INITIALIZE FIELD 7-8 DEFINITION TO BLANK.                         X4T19110
      DO I=1,I2
        TITLE4(I)=BLANK4
      END DO
C-----INITIALIZE FLAGS TO INDICATE ALL INPUT FIELDS NOT YET REQUIRED OR X4T19140
C-----TRANSLATED.                                                       X4T19150
      DO I=1,IDATN
        IMUSED(I)=0
      END DO
C                                                                       X4T19180
C     DETERMINE THIS IS A MULTI-DIMENSIONAL TABLE = ONE REACTION BUT    X4T19190
C     MULTIPLE FLAGS IN COMMON SECTION. FOR THIS CASE DEFINE MULTIPLE   X4T19200
C     REACTIONS ALL OF WHICH ARE IDENTICAL AND DIFFER ONLY BY FLAG.     X4T19210
C                                                                       X4T19220
      IF(KSANR.GT.1.OR.ICOMN.LE.0) GO TO 140                            X4T19230
      KSANR=0                                                           X4T19240
      DO 130 I=1,ICOMN                                                  X4T19250
C-----CHECK FOR NEW NON-BLANK FLAG IN COMMON.                           X4T19260
      IF(FLAGI(I).EQ.BLANK) GO TO 130                                   X4T19270
      IF(KSANR.EQ.0) GO TO 110                                          X4T19280
      DO 100 J=1,KSANR                                                  X4T19290
      IF(FLAGI(I).EQ.FLAGR(J)) GO TO 130
  100 CONTINUE                                                          X4T19310
C-----NEW FLAG. CREATE SAME REACTION WITH NEW FLAG BY INCREASING        X4T19320
C-----REACTION COUNT AND DEFINING FLAG, ZA, MF, MT, REACTION OPERATION. X4T19330
  110 KSANR=KSANR+1                                                     X4T19340
      FLAGR(KSANR)=FLAGI(I)                                             X4T19350
      KZANRS(KSANR)=KZANRS(1)                                           X4T19360
      DO K=1,7
        ZAN(K,KSANR)=ZAN(K,1)
        ZANRES(K,KSANR)=ZANRES(K,1)
      END DO
      INPART(KSANR)=INPART(1)                                           X4T19400
      MFR(KSANR)=MFR(1)                                                 X4T19410
      MTR(KSANR)=MTR(1)                                                 X4T19420
      IRFLAG(KSANR)=IRFLAG(1)                                           X4T19430
  130 CONTINUE                                                          X4T19440
      IF(KSANR.EQ.0) KSANR=1                                            X4T19450
C-----PRINT WARNING FOR MULTI-DIMENSIONAL TABLES.                       X4T19460
  140 IF(KSANR.GT.1) WRITE(OUTP,6010)                                   X4T19470
C                                                                       X4T19480
C     CHECK FOR LEGAL TARGET ZA (ALL CHARACTERS BLANK OR DIGIT).        X4T19490
C     IF ILLEGAL  CHARACTERS FOUND PRINT WARNING BUT DO NOT CHANGE.     X4T19500
C                                                                       X4T19510
      DO 170 ISANR=1,KSANR                                              X4T19520
      IBADZA=0                                                          X4T19530
      DO 160 M=1,6                                                      X4T19540
      ZANOK(M)=ZAN(M,ISANR)                                             X4T19550
      IF(ZAN(M,ISANR).EQ.BLANK) GO TO 160                               X4T19560
      DO 150 K=1,10                                                     X4T19570
      IF(ZAN(M,ISANR).EQ.DIGITS(K)) GO TO 160                           X4T19580
  150 CONTINUE                                                          X4T19590
      IBADZA=1                                                          X4T19600
      ZAN(M,ISANR)=DIGITS(1)                                            X4T19610
  160 CONTINUE                                                          X4T19620
      ZANOK(7)=ZAN(7,ISANR)                                             X4T19630
      IF(IBADZA.NE.0) WRITE(OUTP,6055) ZANOK,(ZAN(M,ISANR),M=1,7)       X4T19640
  170 CONTINUE                                                          X4T19650
C                                                                       X4T19660
C     PERFORM ALL OPERATIONS THAT APPLY TO ENTIRE DATA TABLE.           X4T19670
C                                                                       X4T19680
C-----DEFINE VECTORS TO PERMUTE DATA COLUMNS INTO OUTPUT ORDER FOR ALL  X4T19690
C-----REACTIONS.                                                        X4T19700
      CALL TITLEX                                                       X4T19710
C-----PERFORM TITLE OPERATIONS THAT APPLY TO ALL POINTS IN TABLE.       X4T19720
      CALL TOPS1                                                        X4T19730
C-----DEFINE UNIT CONVERSION FACTORS AND OPERATIONS.                    X4T19740
      CALL UNIT1                                                        X4T19750
C-----SAVE NUMBER OF DATA COLUMNS (MAY BE MORE THAN THE NUMBER OF       X4T19760
C-----COLUMN READ DUE TO CREATION OF COLUMNS).                          X4T19770
      IDAT1=IDATN                                                       X4T19780
C-----INCREMENT COUNT OF POINTS READ.                                   X4T19790
      MPOINT(3)=MPOINT(3)+N2*KSANR                                      X4T19800
C-----INITIALIZE BLANK DATA FIELD COUNT.                                X4T19810
      NODATA=0                                                          X4T19820
C-----INITIALIZE BLANK INCIDENT ENERGY FIELD COUNT.                     X4T19830
      NOEIN=0                                                           X4T19840
C-----INITIALIZE BLANK COSINE FIELD COUNT.                              X4T19850
      NOMU=0                                                            X4T19860
C-----INITIALIZE BLANK SECONDARY ENERGY FIELD COUNT.                    X4T19870
      NOE2=0                                                            X4T19880
C-----INITIALIZE L =0 LEGENDRE COEFFICIENT COUNT.                       X4T19890
      LEGS=0                                                            X4T19900
C-----SET UP LOOP TO READ DATA POINTS.                                  X4T19910
      DO 460 NPT=1,N2                                                   X4T19920
      READ(ITAPE,1020,END=500,ERR=500) (DATUM(I),I=I1,I2)
C-----RESET FLAGS TO INDICATE THAT DATA JUST READ HAS NOT YET BEEN      X4T19940
C-----TRANSLATED.                                                       X4T19950
      DO I=I1,I2
        IF(IMUSED(I).EQ.2) IMUSED(I)=1
      END DO
C                                                                       X4T19990
C     SET UP LOOP OVER REACTIONS.                                       X4T20000
C                                                                       X4T20010
      DO 450 ISANR=1,KSANR                                              X4T20020
C-----RE-DEFINE NUMBER OF DATA COLUMNS                                  X4T20030
      IDATN=IDAT1                                                       X4T20040
C                                                                       X4T20050
C     PERFORM ALL OPERATIONS THAT MAY BE DIFFERENT FOR EACH DATA POINT. X4T20060
C                                                                       X4T20070
C-----CONVERT FIELDS REQUIRED FOR OUTPUT FROM HOLLERITH TO FLOATING     X4T20080
C-----POINT AND FROM INPUT UNITS TO STANDARD UNITS.                     X4T20090
      CALL UNIT2                                                        X4T20100
C-----PERFORM UNIT OPERATIONS.                                          X4T20110
      CALL UNOPS                                                        X4T20120
C-----PERFORM TITLE OPERATIONS.                                         X4T20130
      CALL TOPS2                                                        X4T20140
C-----PERFORM REACTION OPERATIONS.                                      X4T20150
      CALL REOPS                                                        X4T20160
C                                                                       X4T20170
C     CHECK DATA FIELD. IF NOT DEFINED NO OUTPUT. PRINT WARNING IF FIELDX4T20180
C     IS NOT DEFINED OR BLANK.                                          X4T20190
C                                                                       X4T20200
      II=KMOUT(3,ISANR)                                                 X4T20210
C-----PRINT WARNING IF DATA FIELD IS NOT DEFINED.                       X4T20220
      IF(II.GT.0) GO TO 190                                             X4T20230
      IF(NPT.EQ.1) WRITE(OUTP,6020)                                     X4T20240
      MPOINT(5)=MPOINT(5)+1                                             X4T20250
      GO TO 450                                                         X4T20260
C-----DATA FIELD IS DEFINED. SEE IF DATA FIELD IS BLANK (SKIP TEST IF   X4T20270
C-----DATA FIELD WAS CREATED).                                          X4T20280
  190 IF(II.GT.IDAT1) GO TO 210                                         X4T20290
      IF(DATUM(II).NE.BLANK11) GO TO 210
C-----PRINT WARNING WHEN FIRST BLANK DATA FIELD IS FOUND.               X4T20330
      IF(NODATA.EQ.0) WRITE(OUTP,6025)                                  X4T20340
      NODATA=1                                                          X4T20350
      MPOINT(6)=MPOINT(6)+1                                             X4T20360
      GO TO 450                                                         X4T20370
C                                                                       X4T20380
C     CHECK INCIDENT ENERGY FIELD. PRINT WARNING IF FIELD IS NOT DEFINEDX4T20390
C     OR BLANK.                                                         X4T20400
C                                                                       X4T20410
  210 IEIN=KMOUT(1,ISANR)                                               X4T20420
C-----PRINT WARNING IF INCIDENT ENERGY FIELD IS NOT DEFINED.            X4T20430
      IF(IEIN.GT.0) GO TO 220                                           X4T20440
      IF(NPT.EQ.1) WRITE(OUTP,6140)                                     X4T20450
      GO TO 240                                                         X4T20460
C-----INCIDENT ENERGY FIELD IS DEFINED. SEE IF FIELD IS BLANK (SKIP     X4T20470
C-----TEST IF FIELD WAS CREATED).                                       X4T20480
  220 IF(IEIN.GT.IDAT1) GO TO 240                                       X4T20490
      IF(NOEIN.GT.0) GO TO 240                                          X4T20500
      IF(DATUM(IEIN).NE.BLANK11) GO TO 240
C-----PRINT WARNING WHEN FIRST BLANK INCIDENT ENERGY FIELD IS FOUND.    X4T20540
      WRITE(OUTP,6150)                                                  X4T20550
      NOEIN=1                                                           X4T20560
C                                                                       X4T20570
C     FOR ANGULAR AND DOUBLE DIFFERENTIAL DISTRIBUTION CHECK COSINE     X4T20580
C     FIELD, FOR LEGENDRE COEFFICIENTS CHECK LEGENDRE ORDER FIELD.      X4T20590
C     PRINT WARNING IF FIELD IS NOT DEFINED OR BLANK.                   X4T20600
C                                                                       X4T20610
  240 IF(MFR(ISANR).NE.4.AND.MFR(ISANR).NE.6.AND.MFR(ISANR).NE.154)     X4T20620
     1 GO TO 270                                                        X4T20630
      IMU=KMOUT(5,ISANR)                                                X4T20640
C-----PRINT WARNING IF FIELD IS NOT DEFINED.                            X4T20650
      IF(IMU.GT.0) GO TO 250                                            X4T20660
      IF(NPT.EQ.1) WRITE(OUTP,6160)                                     X4T20670
      GO TO 270                                                         X4T20680
C-----FIELD IS DEFINED. SEE IF FIELD IS BLANK (SKIP TEST IF FIELD WAS   X4T20690
C-----CREATED).                                                         X4T20700
  250 IF(IMU.GT.IDAT1) GO TO 270                                        X4T20710
      IF(NOMU.GT.0) GO TO 270                                           X4T20720
      IF(DATUM(IMU).NE.BLANK11) GO TO 270
C-----PRINT WARNING WHEN FIRST BLANK FIELD IS FOUND.                    X4T20760
      WRITE(OUTP,6170)                                                  X4T20770
      NOMU=1                                                            X4T20780
C                                                                       X4T20790
C     FOR ENERGY AND DOUBLE DIFFERENTIAL DISTRIBUTION CHECK SECONDARY   X4T20800
C     ENERGY FIELD. PRINT WARNING IF FIELD IS NOT DEFINED OR BLANK.     X4T20810
C                                                                       X4T20820
  270 IF(MFR(ISANR).NE.5.AND.MFR(ISANR).NE.6) GO TO 300                 X4T20830
      IE2=KMOUT(7,ISANR)                                                X4T20840
C-----PRINT WARNING IF FIELD IS NOT DEFINED.                            X4T20850
      IF(IE2.GT.0) GO TO 280                                            X4T20860
      IF(NPT.EQ.1) WRITE(OUTP,6180)                                     X4T20870
      GO TO 300                                                         X4T20880
C-----FIELD IS DEFINED. SEE IF FIELD IS BLANK (SKIP TEST IF FIELD WAS   X4T20890
C-----CREATED).                                                         X4T20900
  280 IF(IE2.GT.IDAT1) GO TO 300                                        X4T20910
      IF(NOE2.GT.0) GO TO 300                                           X4T20920
      IF(DATUM(IE2).NE.BLANK11) GO TO 300
C-----PRINT WARNING WHEN FIRST BLANK FIELD IS FOUND.                    X4T20960
      WRITE(OUTP,6190)                                                  X4T20970
      NOE2=1                                                            X4T20980
C                                                                       X4T20990
C     INITIALIZE OUTPUT FIELDS AND THEN FILL IN ALL REQUIRED OUTPUT.    X4T21000
C                                                                       X4T21010
  300 DO J=1,8
        OUTLIN(J)=BLANK9
      END DO
C-----INITIALIZE DEFINITION OF FIELD 7-8.                               X4T21050
      IM78=BLANK4                                                       X4T21060
      DO 340 I=1,8                                                      X4T21070
      II=KMOUT(I,ISANR)                                                 X4T21080
C-----SKIP UNUSED FIELDS AND ZERO ERROR FIELDS.                         X4T21090
      IF(II.LE.0) GO TO 340                                             X4T21100
      OVALUE=VALUES(II)                                                 X4T21110
C-----ON FIRST POINT PRINT WARNING MESSAGE IF L = 0 LEGENDRE COEFFICIENTX4T21120
C-----IS NOT NORMALIZED TO UNITY.                                       X4T21130
      IF(MFR(ISANR).NE.154.OR.LEGS.GT.0.OR.I.NE.5) GO TO 320            X4T21140
      IF(ABS(OVALUE).GT.0.001) GO TO 320                                X4T21150
      LEGS=1                                                            X4T21160
      LEG1=KMOUT(3,ISANR)                                               X4T21170
      IF(ABS(VALUES(LEG1)-1.0).GT.0.001) WRITE(OUTP,6120)               X4T21180
  320 IF(I.NE.2.AND.I.NE.4.AND.I.NE.6.AND.I.NE.8) GO TO 330             X4T21190
C-----INSURE THAT ALL ERRORS ARE NON-NEGATIVE.                          X4T21200
      OVALUE=ABS(OVALUE)                                                X4T21210
      IF(OVALUE.LE.0.0) GO TO 340                                       X4T21220
C-----FORMAT DATA FOR OUTPUT.                                           X4T21230
  330 CALL NORMF(OVALUE,OUTLIN(I))
C-----IF REQUIRED SET DEFINITION OF FIELDS 7-8.                         X4T21250
      IF(I.NE.7) GO TO 340                                              X4T21260
      IF(TITLE4(II).EQ.BLANK4) GO TO 340
      IF(IM78.EQ.TITLE4(II)) GO TO 340
      IF(NPT.EQ.1.AND.IM78.NE.BLANK4) WRITE(OUTP,6040)                  X4T21290
      IM78=TITLE4(II)
      IF(NPT.EQ.1) WRITE(OUTP,6030) IM78                                X4T21310
c...
      write(outp,*) values(ii)
c...  OVALUE=ABS(VALUES(II))
c...  CALL NORMF(OVALUE,OUTLIN(I))
c...
  340 CONTINUE                                                          X4T21320
C                                                                       X4T21330
C     IF REQUIRED INSERT RESIDUAL NUCLEUS ZA (PRODUCTION) OR RATIO      X4T21340
C     DENOMINATOR MT AND MT. CHECK FOR ILLEGAL VALUES AND CONFLICTS.    X4T21350
C                                                                       X4T21360
C     PRODUCTION                                                        X4T21370
C                                                                       X4T21380
      IF(KZANRS(ISANR).LE.0) GO TO 380                                  X4T21390
C-----ONLY PERFORM CHECKS AND PRINT MESSAGES FOR FIRST POINT.           X4T21400
      IF(NPT.GT.1) GO TO 370                                            X4T21410
C-----CHECK FOR LEGAL PRODUCT ZA (ALL CHARACTERS BLANK OR DIGIT).       X4T21420
      IBADZA=0                                                          X4T21430
      DO 360 M=1,6                                                      X4T21440
      ZANOK(M)=ZANRES(M,ISANR)                                          X4T21450
      IF(ZANRES(M,ISANR).EQ.BLANK) GO TO 360                            X4T21460
      DO 350 K=1,10                                                     X4T21470
      IF(ZANRES(M,ISANR).EQ.DIGITS(K)) GO TO 360                        X4T21480
  350 CONTINUE                                                          X4T21490
      IBADZA=1                                                          X4T21500
      ZANRES(M,ISANR)=DIGITS(1)                                         X4T21510
  360 CONTINUE                                                          X4T21520
      ZANOK(7)=ZANRES(7,ISANR)                                          X4T21530
      IF(IBADZA.NE.0) WRITE(OUTP,6060) ZANOK,(ZANRAT(M,ISANR),M=1,7)    X4T21540
C-----CHECK FOR PRODUCTION REACTIONS (MT = 9000 - 9999).                X4T21550
      IF(MTR(ISANR).LT.9000) WRITE(OUTP,6080) (ZANRES(M,ISANR),M=1,7)   X4T21560
C-----INSERT RESIDUAL NUCLEUS IN THE SIXTH OUTPUT FIELD (NORMALLY USED  X4T21570
C-----FOR COSINE ERROR).                                                X4T21580
  370 CALL RESZA(OUTLIN(6),ZANRES(1,ISANR))
      GO TO 390                                                         X4T21600
C-----NO PRODUCT ZA. PRINT WARNING IF MT = 9000 - 9999.                 X4T21610
C 380 IF(NPT.EQ.1.AND.MTR(ISANR).GE.9000) WRITE(OUTP,6090)              X4T21620
  380 IF(MTR(ISANR).LT.9000) GO TO 390                                  TRKOV
      IF(NPT.GT.1) GO TO 382                                            TRKOV
      IPO=MTR(ISANR)                                                    TRKOV
      IPO=IPO-1000*(IPO/1000)                                           TRKOV
      MTR(ISANR)=MTR(ISANR)-IPO                                         TRKOV
      IPO=IPO+1                                                         TRKOV
      IF(IPO.GT.8) IPO=8                                                TRKOV
      WRITE(OUTP,6090) (ZAPO(J,IPO),J=1,6)                              TRKOV
  382 CALL RESZA(OUTLIN(6),ZAPO(1,IPO))
C                                                                       X4T21630
C     RATIO                                                             X4T21640
C                                                                       X4T21650
  390 IF(MTRAT(ISANR).LE.0) GO TO 430                                   X4T21660
C-----ONLY PERFORM CHECKS AND PRINT MESSAGES FOR FIRST POINT.           X4T21670
      IF(NPT.GT.1) GO TO 420                                            X4T21680
C-----CHECK FOR LEGAL RATIO ZA (ALL CHARACTERS BLANK OR DIGIT).         X4T21690
      IBADZA=0                                                          X4T21700
      DO 410 M=1,6                                                      X4T21710
      ZANOK(M)=ZANRAT(M,ISANR)                                          X4T21720
      IF(ZANRAT(M,ISANR).EQ.BLANK) GO TO 410                            X4T21730
      DO 400 K=1,10                                                     X4T21740
      IF(ZANRAT(M,ISANR).EQ.DIGITS(K)) GO TO 410                        X4T21750
  400 CONTINUE                                                          X4T21760
      IBADZA=1                                                          X4T21770
      ZANRAT(M,ISANR)=DIGITS(1)                                         X4T21780
  410 CONTINUE                                                          X4T21790
      ZANOK(7)=ZANRAT(7,ISANR)                                          X4T21800
      IF(IBADZA.NE.0) WRITE(OUTP,6110) ZANOK,(ZANRAT(M,ISANR),M=1,7)    X4T21810
C-----CHECK FOR RATIO (MF = 203).                                       X4T21820
      IF(MFR(ISANR).NE.203) WRITE(OUTP,6050) (ZANRAT(M,ISANR),M=1,7),   X4T21830
     1 MTRAT(ISANR)                                                     X4T21840
C-----CHECK FOR CONFLICT (RATIO OF PRODUCTIONS).                        X4T21850
      IF(MTR(ISANR).GE.9000.OR.MTRAT(ISANR).GE.9000) WRITE(OUTP,6070)   X4T21860
C-----INSERT RATIO MT IN FIFTH FIELD AND ZA IN SIXTH OUTPUT FIELD       X4T21870
C-----(NORMALLY USED FOR COSINE AND COSINE ERROR).                      X4T21880
  420 CALL RATZA(OUTLIN(5),OUTLIN(6),MTRAT(ISANR),ZANRAT(1,ISANR))
      GO TO 440                                                         X4T21900
C-----NO RATIO ZA/MT. PRINT WARNING IF MF = 203.                        X4T21910
  430 IF(NPT.EQ.1.AND.MFR(ISANR).EQ.203) WRITE(OUTP,6100)               X4T21920
C-----IF LEVEL ENERGY UNDEFINED FOR MT 51 CHANGE TO MT 4
  440 IF(MTR(ISANR).EQ.51 .AND. OUTLIN(7).EQ.BLANK9) MTR(ISANR)=4
C                                                                       X4T21930
C     PRINT DATA POINT.                                                 X4T21940
C                                                                       X4T21950
      WRITE(OTAPE,1100) INPART(ISANR),(ZAN(I,ISANR),I=1,7),MFR(ISANR),  X4T21960
     1 MTR(ISANR),ZANRES(7,ISANR),STATN,LABCM(ISANR),OUTLIN,IM78,       X4T21970
     1 AUTHK,ENT,ISAN,FLAGR(ISANR)                                      X4T21980
      MPOINT(4)=MPOINT(4)+1                                             X4T21990
  450 CONTINUE                                                          X4T22000
  460 CONTINUE                                                          X4T22010
C-----PRINT WARNING IF ANY LEGENDRE COEFFICIENTS AND L =0 COEFFICIENTS  X4T22020
C-----ARE NOT GIVEN.                                                    X4T22030
      IF(LEGS.GT.0) GO TO 490                                           X4T22040
      DO 470 I=1,KSANR                                                  X4T22050
      IF(MFR(I).EQ.154) GO TO 480                                       X4T22060
  470 CONTINUE                                                          X4T22070
      GO TO 490                                                         X4T22080
  480 WRITE(OUTP,6130)                                                  X4T22090
C                                                                       X4T22100
C     TABLE COMPLETED. NORMAL RETURN.                                   X4T22110
C                                                                       X4T22120
  490 RETURN                                                            X4T22130
C                                                                       X4T22140
C     ERROR READING EXFOR DATA. PRINT ERROR MESSAGE AND TERMINATE.      X4T22150
C                                                                       X4T22160
  500 WRITE(OUTP,6000)                                                  X4T22170
      STOP                                                              X4T22180
 1000 FORMAT(6(2A4,A3),14A1)                                            X4T22190
 1010 FORMAT(6(A10,A1))
 1020 FORMAT(6A11)
 1100 FORMAT(I5,7A1,I3,I4,3A1,8A9,A3,25A1,5A1,I3,A1)
 6000 FORMAT(10X,'ERROR READING EXFOR DATA...EXECUTION TERMINATED')     X4T22230
 6010 FORMAT(10X,'WARNING.....MULTI-DIMENSIONAL DATA TABLE')            X4T22240
 6020 FORMAT(10X,'WARNING.....DATA IS NOT DEFINED. DATA POINT IGNORED') X4T22250
 6025 FORMAT(10X,'WARNING.....DATA FIELD IS BLANK. DATA POINT IGNORED') X4T22260
 6030 FORMAT(10X,'OPERATION...SET FIELD 7-8 DEFINITION=',A3)            X4T22270
 6040 FORMAT(10X,'WARNING.....CONFLICTING DEFINITIONS OF FIELDS 7-8')   X4T22280
 6050 FORMAT(10X,'WARNING.....RATIO DENOMINATOR ZA/MT=',7A1,'/',I5/     X4T22290
     1       10X,'            (EXPECT MF = 203)')                       X4T22300
 6055 FORMAT(10X,'WARNING.....NON-NUMERIC TARGET  ZA =',7A1/            X4T22310
     1       10X,'                   CONVERTED TO ZA =',7A1/            X4T22320
     2       10X,'            YOU MUST CORRECT TRANSLATED DATA TO',     X4T22330
     3 ' DEFINE SPECIAL ZA.'/                                           X4T22340
     4       10X,'            SEE ENDF/B SPECIAL ZA DICTIONARY')        X4T22350
 6060 FORMAT(10X,'WARNING.....NON-NUMERIC PRODUCT ZA =',7A1/            X4T22360
     1       10X,'                   CONVERTED TO ZA =',7A1/            X4T22370
     2       10X,'IMPORTANT...YOU MUST CORRECT TRANSLATED DATA TO',     X4T22380
     3 ' DEFINE SPECIAL ZA.'/                                           X4T22390
     4       10X,'            SEE ENDF/B SPECIAL ZA DICTIONARY')        X4T22400
 6070 FORMAT(10X,'WARNING.....RATIO OF PRODUCTION DATA. WILL DEFINE'/   X4T22410
     1       10X,'            MT AND ZA OF DENOMINATOR IN FIELD 5 AND'/ X4T22420
     2       10X,'            6. CANNOT DEFINE ZA OF PRODUCTS.'/        X4T22430
     3       10X,'            CHECK AND CORRECT OUTPUT.')               X4T22440
 6080 FORMAT(10X,'WARNING.....PRODUCT ZA =',7A1,                        X4T22450
     1 ' (EXPECT MT = 9000 - 9999)')                                    X4T22460
C6090 FORMAT(10X,'WARNING.....EXPECT PRODUCT ZA FOR MT=9000-9999')      X4T22470
 6090 FORMAT(10X,'WARNING.....EXPECT PRODUCT ZA FOR MT=9000-9999'/      TRKOV
     1       10X,'            SET DEFAULT ',6A1,'.9')                   TRKOV
 6100 FORMAT(10X,'WARNING.....EXPECT RATIO ZA/MT FOR MF = 203')         X4T22480
 6110 FORMAT(10X,'WARNING.....RATIO ZA =',7A1,' CONVERTED TO'/          X4T22490
     1       10X,'                  ZA =',7A1)                          X4T22500
 6120 FORMAT(10X,'WARNING.....L = 0 LEGENDRE COEFFICIENTS ARE NOT',     X4T22510
     1 ' NORMALIZED')                                                   X4T22520
 6130 FORMAT(10X,'WARNING.....L = 0 LEGENDRE COEFFICIENTS ARE NOT',     X4T22530
     1 ' PRESENT')                                                      X4T22540
 6140 FORMAT(10X,'WARNING.....INCIDENT ENERGY IS NOT DEFINED')          X4T22550
 6150 FORMAT(10X,'WARNING.....INCIDENT ENERGY FIELD IS BLANK')          X4T22560
 6160 FORMAT(10X,'WARNING.....COSINE OR LEGENDRE ORDER IS NOT DEFINED') X4T22570
 6170 FORMAT(10X,'WARNING.....COSINE OR LEGENDRE ORDER FIELD IS BLANK') X4T22580
 6180 FORMAT(10X,'WARNING.....SECONDARY ENERGY IS NOT DEFINED')         X4T22590
 6190 FORMAT(10X,'WARNING.....SECONDARY ENERGY FIELD IS BLANK')         X4T22600
      END                                                               X4T22610
      SUBROUTINE RESZA(OUT1,ZANRES)                                     X4T22620
C                                                                       X4T22630
C     DEFINE RESIDUAL NUCLEUS IN OUTPUT FIELD.                          X4T22640
C                                                                       X4T22650
      CHARACTER*1 OUT1,ZANRES,BLANK,DOT                                 X4T22660
      DIMENSION OUT1(9),ZANRES(7),METAB1(10),METAB2(10)                 X4T22670
      DATA BLANK/' '/                                                   X4T22680
      DATA DOT/'.'/                                                     X4T22690
      OUT1(1)=BLANK                                                     X4T22700
      DO I=1,6
        OUT1(I+1)=ZANRES(I)
      END DO
      OUT1(8)=DOT                                                       X4T22730
C-----DEFINE PRODUCT METASTABLE STATE FLAG EQUIVALENT AND OUTPUT IN     X4T22740
C-----COLUMN 9.                                                         X4T22750
      CALL META10(OUT1(9),ZANRES(7))                                    X4T22760
      RETURN                                                            X4T22770
      END                                                               X4T22780
      SUBROUTINE RATZA(OUT1,OUT2,MTIN,ZANRAT)                           X4T22790
C                                                                       X4T22800
C     DEFINE RATIO MT IN FIELD 7 AND RATIO ZA IN FIELD 8.               X4T22810
C                                                                       X4T22820
      CHARACTER*1 OUT1,OUT2,ZANRAT,BLANK,DOT,DIGITS                     X4T22830
      DIMENSION OUT1(9),OUT2(9),ZANRAT(14),MULT(4),DIGITS(10)           X4T22840
      DATA BLANK/' '/                                                   X4T22850
      DATA DOT/'.'/                                                     X4T22860
      DATA MULT/1,10,100,1000/                                          X4T22870
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              X4T22880
      MT=MTIN                                                           X4T22890
C-----INITIALIZE FIRST WORD TO BLANK FOLLOWED BY DECIMAL POINT.         X4T22900
      DO I=1,7
        OUT1(I)=BLANK
      END DO
      OUT1(8)=DOT                                                       X4T22930
C-----OUTPUT MT IN FIRST WORD (CHARACTERS 4 THROUGH 7).                 X4T22940
      J=4                                                               X4T22950
      IF(MT.LT.1000) J=3                                                X4T22960
      IF(MT.LT.100) J=2                                                 X4T22970
      IF(MT.LT.10) J=1                                                  X4T22980
      MULTMT=MULT(J)                                                    X4T22990
      II=8-J                                                            X4T23000
      DO I=1,J
        IDIG=MT/MULTMT
        OUT1(II)=DIGITS(IDIG+1)
        MT=MT-IDIG*MULTMT
        MULTMT=MULTMT/10
        II=II+1
      END DO
C-----DEFINE PRODUCT METASTABLE STATE FLAG EQUIVALENT AND OUTPUT IN     X4T23070
C-----COLUMN 9.                                                         X4T23080
      CALL META10(OUT1(9),ZANRAT(14))                                   X4T23090
C-----OUTPUT ZA IN SECOND WORD.                                         X4T23100
      OUT2(1)=BLANK                                                     X4T23110
      DO I=1,6
        OUT2(I+1)=ZANRAT(I)
      END DO
      OUT2(8)=DOT                                                       X4T23140
C-----DEFINE ZA METASTABLE STATE FLAG EQUIVALENT AND OUTPUT IN          X4T23150
C-----COLUMN 9.                                                         X4T23160
      CALL META10(OUT2(9),ZANRAT(7))                                    X4T23170
      RETURN                                                            X4T23180
      END                                                               X4T23190
      SUBROUTINE META10(OUT,MSTATE)                                     X4T23200
C                                                                       X4T23210
C     DEFINE NUMERICAL EQUIVALENT OF METASTABLE STATE FLAG FOR OUTPUT   X4T23220
C     WITH ZA OR MT.                                                    X4T23230
C                                                                       X4T23240
      CHARACTER*1 OUT,MSTATE,MTAB1,MTAB2                                X4T23250
      DIMENSION MTAB1(11),MTAB2(11)                                     X4T23260
      DATA MTAB1/                                                       X4T23270
     1 'G','1','2','3','4','5','?','M','+','T',' '/                     X4T23280
      DATA MTAB2/                                                       X4T23290
     1 '0','1','2','3','4','5','6','7','8','9','9'/                     X4T23300
C-----LOOK UP METASTABLE STATE CHARACTER.                               X4T23310
      DO I=1,11
        IF(MSTATE.EQ.MTAB1(I)) GO TO 20
      END DO
C-----SET INDEX TO UNKNOWN.                                             X4T23350
      I=7                                                               X4T23360
   20 OUT=MTAB2(I)                                                      X4T23370
      RETURN                                                            X4T23380
      END                                                               X4T23390
      SUBROUTINE NORMF(X,FIELD)                                         X4T23400
C                                                                       X4T23410
C     CONVERT FLOATING POINT NUMBER TO A STRING OF 9 CHARACTERS FOR     X4T23420
C     OUTPUT. NUMBERS BETWEEN 0.01 AND 9999999. WILL BE FORMATTED IN    X4T23430
C     F FORMAT. ALL OTHER NUMBERS WILL BE FORMATTED IN E FORMAT.        X4T23440
C                                                                       X4T23450
      CHARACTER*1 MINUS,FIELD,DIGITS,BLANK,ZERO,DOT                     X4T23460
      DOUBLE PRECISION ZMULT,QMULT,XNORM,XIN,XABS,XZERO                 X4T23470
      DIMENSION FIELD(9),DIGITS(10),ZERO(9),ZMULT(11),QMULT(11),IMULT(6)X4T23480
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              X4T23490
      DATA ZERO/' ','0','.','0','0','0','0','0','0'/                    X4T23500
      DATA MINUS/'-'/                                                   X4T23510
      DATA BLANK/' '/                                                   X4T23520
      DATA DOT/'.'/                                                     X4T23530
      DATA IMULT/100,1000,10000,100000,1000000,10000000/                X4T23540
      DATA ZMULT/1.0D-3,1.0D-2,1.0D-1,1.0D+0,1.0D+1,1.0D+2,1.0D+3,      X4T23550
     1 1.0D+4,1.0D+5,1.0D+6,1.0D+7/                                     X4T23560
      DATA QMULT/1.0D-8,1.0D-7,1.0D-6,1.0D-5,1.0D-4,1.0D-3,1.0D-2,      X4T23570
     1 1.0D-1,1.0D+0,1.0D+1,1.0D+2/                                     X4T23580
      DATA XZERO/0.0D+0/                                                X4T23590
C-----IF NUMBER IS ZERO RETURN STANDARD FORM.                           X4T23600
      XIN=X                                                             X4T23610
      XABS=DABS(XIN)                                                    X4T23620
      IF(XABS.GT.XZERO) GO TO 20                                        X4T23630
      DO I=1,9
        FIELD(I)=ZERO(I)
      END DO
      RETURN                                                            X4T23660
C-----IF NUMBER OUT OF RANGE USE E FORMAT.                              X4T23670
   20 IF(XABS.LT.ZMULT(2).OR.XABS.GE.ZMULT(11)) GO TO 110               X4T23680
C-----DEFINE EXPONENT TO NORMALIZE MANTISSA.                            X4T23690
      DO 30 IEXP=1,11                                                   X4T23700
C-----IF CLOSE TO ONE DECADE SET EQUAL TO ONE DECADE.                   X4T23710
      IF(DABS(XABS-ZMULT(IEXP)).LE.QMULT(IEXP)) XABS=ZMULT(IEXP)        X4T23720
      IF(XABS.LT.ZMULT(IEXP)) GO TO 40                                  X4T23730
   30 CONTINUE                                                          X4T23740
      GO TO 110                                                         X4T23750
C-----PRECEDING VALUE WILL NORMALIZE NUMBER.                            X4T23760
   40 IEXP=IEXP-1                                                       X4T23770
C-----DEFINE EXPONENT TO PUT NUMBER IN NORMAL FORM N.NNNN...            X4T23780
      KEXP=IEXP-4                                                       X4T23790
C-----INITIALIZE OUTPUT FIELD.                                          X4T23800
      DO I=1,9
        FIELD(I)=BLANK
      END DO
C-----IF NUMBER IS NEGATIVE SET SIGN.                                   X4T23830
      IF(X.LT.0.0) FIELD(1)=MINUS                                       X4T23840
C-----DEFINE NORMALIZED INTEGER.                                        X4T23850
      KEXPAB=IABS(KEXP)                                                 X4T23860
      IF(KEXPAB.LE.0) KEXP=0                                            X4T23870
      KMULT=5                                                           X4T23880
      IF(KEXP.LT.0) KMULT=5+KEXP                                        X4T23890
      XNORM=ZMULT(KMULT+6)*XABS/ZMULT(IEXP)                             X4T23900
      INORM=XNORM                                                       X4T23910
      INORM=(INORM+5)/10                                                X4T23920
      KNORM=1000000                                                     X4T23930
C-----TRY TO AVOID LAST DIGIT ROUND-OFF.                                X4T23940
      IF(INORM.LT.KNORM) GO TO 70                                       X4T23950
      LNORM=INORM-1000*(INORM/1000)                                     X4T23960
C-----IF LAST 3 DIGITS ARE LESS THAN 005 ROUND DOWN.                    X4T23970
      IF(LNORM.GT.5) GO TO 60                                           X4T23980
      INORM=10*(INORM/10)                                               X4T23990
      GO TO 70                                                          X4T24000
C-----IF LAST 3 DIGITS ARE GREATER THAN 995 ROUND UP.                   X4T24010
   60 IF(LNORM.LT.995) GO TO 70                                         X4T24020
      INORM=1000*(INORM/1000)+1000                                      X4T24030
C-----INSURE NUMBER IS IN NORMALIZED FORM (I.E., ALLOW FOR ROUND-OFF).  X4T24040
   70 IF(INORM.GE.IMULT(KMULT)) GO TO 80                                X4T24050
      INORM=IMULT(KMULT)                                                X4T24060
      GO TO 90                                                          X4T24070
   80 IF(INORM.LT.IMULT(KMULT+1)) GO TO 90                              X4T24080
      INORM=IMULT(KMULT+1)                                              X4T24090
      KEXP=KEXP+1                                                       X4T24100
C-----DEFINE POSITION OF DECIMAL POINT AND INSERT IT.                   X4T24110
   90 IPOINT=3                                                          X4T24120
      IF(KEXP.GT.0) IPOINT=KEXP+3                                       X4T24130
      IF(IPOINT.GT.9) GO TO 110                                         X4T24140
      FIELD(IPOINT)=DOT                                                 X4T24150
C-----CONVERT TO CHARACTERS AND INSERT INTO OUTPUT FIELD                X4T24160
      DO 100 I=2,9                                                      X4T24170
C-----SKIP DECIMAL POINT LOCATION.                                      X4T24180
      IF(I.EQ.IPOINT) GO TO 100                                         X4T24190
      IDIG=INORM/KNORM                                                  X4T24200
      FIELD(I)=DIGITS(IDIG+1)                                           X4T24210
      INORM=INORM-IDIG*KNORM                                            X4T24220
      KNORM=KNORM/10                                                    X4T24230
  100 CONTINUE                                                          X4T24240
      RETURN                                                            X4T24250
C-----NUMBER IS OUT OF RANGE. USE E FORMATTED OUTPUT.                   X4T24260
  110 CALL NORME(X,FIELD)                                               X4T24270
      RETURN                                                            X4T24280
      END                                                               X4T24290
      SUBROUTINE NORME(X,FIELD)                                         X4T24300
C                                                                       X4T24310
C     CONVERT FLOATING POINT NUMBER TO CHARACTER STRING FOR E9.3        X4T24320
C     OUTPUT. OUTPUT WILL BE IN THE FORM X.XXX+/-NN OR X.XXXX+/-N       X4T24330
C     (IF EXPONENT IS LESS THAN 10) WHICH GIVES 1 OR 2 MORE DIGITS      X4T24340
C     OF ACCURACY COMPARED TO NORMAL FORTRAN OUTPUT.                    X4T24350
C                                                                       X4T24360
      CHARACTER*1 PLUS,MINUS,FIELD,DIGITS,BLANK,ZERO,DOT                X4T24370
      DIMENSION FIELD(9),DIGITS(10),ZERO(9)                             X4T24380
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              X4T24390
      DATA ZERO/' ','0','.','0','0','0','0','+','0'/                    X4T24400
      DATA PLUS/'+'/                                                    X4T24410
      DATA MINUS/'-'/                                                   X4T24420
      DATA BLANK/' '/                                                   X4T24430
      DATA DOT/'.'/                                                     X4T24440
C-----IF NUMBER IS ZERO RETURN STANDARD FORM.                           X4T24450
      XABS=ABS(X)                                                       X4T24460
      IF(XABS.GT.0.0) GO TO 20                                          X4T24470
      DO I=1,9
        FIELD(I)=ZERO(I)
      END DO
      RETURN                                                            X4T24500
C-----INITIALIZE OUTPUT FIELD.                                          X4T24510
   20 DO I=1,9
        FIELD(I)=BLANK
      END DO
C-----IF NUMBER IS NEGATIVE INSERT LEADING MINUS SIGN.                  X4T24540
      IF(X.LT.0.0) FIELD(1)=MINUS                                       X4T24550
C-----INSERT DECIMAL POINT.                                             X4T24560
      FIELD(3)=DOT                                                      X4T24570
C-----DEFINE EXPONENT TO NORMALIZE MANTISSA.                            X4T24580
      KEXP=ALOG10(XABS)                                                 X4T24590
      SHIFT=10.0**KEXP                                                  X4T24600
      XNORM=XABS/SHIFT+0.000051                                         X4T24610
C-----DEFINE NORMALIZAED MANTISSA.                                      X4T24620
      IF(XNORM.GE.1.0) GO TO 40                                         X4T24630
      KEXP=KEXP-1                                                       X4T24640
      SHIFT=SHIFT/10.0                                                  X4T24650
      XNORM=XABS/SHIFT+0.000051                                         X4T24660
      GO TO 50                                                          X4T24670
   40 IF(XNORM.LT.10.0) GO TO 50                                        X4T24680
      KEXP=KEXP+1                                                       X4T24690
      SHIFT=10.0*SHIFT                                                  X4T24700
      XNORM=XABS/SHIFT+0.000051                                         X4T24710
C-----SELECT X.XXX+/-NN OR X.XXXX+/-N FORMAT (DEPENDING ON SIZE OF      X4T24720
C-----EXPONENT).                                                        X4T24730
   50 KEXPAB=IABS(KEXP)                                                 X4T24740
      IF(KEXPAB.LE.0) KEXP=0                                            X4T24750
      IF(KEXPAB.LT.10) GO TO 60                                         X4T24760
C-----X.XXX+/-NN FORMAT.                                                X4T24770
      ITOP=6                                                            X4T24780
      INORM=1000.0*XNORM                                                X4T24790
      KNORM=1000                                                        X4T24800
      IDIG=KEXPAB/10                                                    X4T24810
      FIELD(8)=DIGITS(IDIG+1)                                           X4T24820
      KEXPAB=KEXPAB-IDIG*10                                             X4T24830
      GO TO 70                                                          X4T24840
C-----X.XXXX+/-N FORMAT.                                                X4T24850
   60 ITOP=7                                                            X4T24860
      INORM=10000.0*XNORM                                               X4T24870
      KNORM=10000                                                       X4T24880
   70 FIELD(9)=DIGITS(KEXPAB+1)                                         X4T24890
C-----DEFINE SIGN OF EXPONENT.                                          X4T24900
      IF(KEXP.LT.0) FIELD(ITOP+1)=MINUS                                 X4T24910
      IF(KEXP.GE.0) FIELD(ITOP+1)=PLUS                                  X4T24920
C-----CONVERT TO CHARACTERS AND INSERT INTO OUTPUT FIELD                X4T24930
      DO 80 I=2,ITOP                                                    X4T24940
C-----SKIP DECIMAL POINT LOCATION.                                      X4T24950
      IF(I.EQ.3) GO TO 80                                               X4T24960
      IDIG=INORM/KNORM                                                  X4T24970
      FIELD(I)=DIGITS(IDIG+1)                                           X4T24980
      INORM=INORM-IDIG*KNORM                                            X4T24990
      KNORM=KNORM/10                                                    X4T25000
   80 CONTINUE                                                          X4T25010
      RETURN                                                            X4T25020
      END                                                               X4T25030
      SUBROUTINE MFMTIN(NTAPE1)                                         X4T25040
C                                                                       X4T25050
C     READ EQUIVALENCE TABLE FOR REACTION VS. MF/MT                     X4T25060
C                                                                       X4T25070
C     COLUMNS                                                           X4T25080
C     -------                                                           X4T25090
C      1- 48    REACTION                                                X4T25100
C     49- 53    INCIDENT PARTICLE ZA                                    X4T25110
C     54- 56    MF                                                      X4T25120
C     57- 60    MT                                                      X4T25130
C                                                                       X4T25140
      INTEGER OUTP,OTAPE                                                X4T25150
      CHARACTER*4 R2MFMT,DUMMY,DUMMY2                                   X4T25160
      CHARACTER*1 FLAG,BLANK                                            X4T25170
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/MFMTI1/IMFMT                                               X4T25190
      COMMON/MFMTI2/MFMTAB(7,11111)
      COMMON/MFMTC/R2MFMT(12,11111)
      DIMENSION DUMMY(8)                                                X4T25220
      DATA MAXIE/11111/
      DATA BLANK/' '/                                                   X4T25240
C-----READ ENTIRE FILE. SKIP CARDS WITH NON-BLANK COLUMN 80.            X4T25250
      DO 20 IMFMT=1,MAXIE                                               X4T25260
   10 READ(NTAPE1,1000,END=60,ERR=50) (R2MFMT(J,IMFMT),J=1,12),         X4T25270
     1 DUMMY,FLAG                                                       X4T25280
      IF(FLAG.NE.BLANK) GO TO 10                                        X4T25290
      CALL INTGER(DUMMY(1),MFMTAB(1,IMFMT),5)                           X4T25300
      CALL INTGER(DUMMY(3),MFMTAB(2,IMFMT),3)                           X4T25310
      CALL INTGER(DUMMY(4),MFMTAB(3,IMFMT),4)                           X4T25320
      CALL INTGER(DUMMY(5),MFMTAB(4,IMFMT),3)                           X4T25330
      CALL INTGER(DUMMY(6),MFMTAB(5,IMFMT),3)                           X4T25340
      CALL INTGER(DUMMY(7),MFMTAB(6,IMFMT),3)                           X4T25350
      CALL INTGER(DUMMY(8),MFMTAB(7,IMFMT),3)                           X4T25360
   20 CONTINUE                                                          X4T25370
      IMFMT=MAXIE                                                       X4T25380
   30 IMFMT=IMFMT+1                                                     X4T25390
   40 READ(NTAPE1,1000,END=60,ERR=50) (DUMMY2,J=1,12),                  X4T25400
     1 DUMMY,FLAG                                                       X4T25410
      IF(FLAG.NE.BLANK) GO TO 40                                        X4T25420
      GO TO 30                                                          X4T25430
   50 WRITE(OUTP,6000)                                                  X4T25440
      STOP                                                              X4T25450
   60 IMFMT=IMFMT-1                                                     X4T25460
      WRITE(OUTP,6010) IMFMT,MAXIE                                      X4T25470
      IF(IMFMT.LE.MAXIE) RETURN                                         X4T25480
      WRITE(OUTP,6020)                                                  X4T25490
      IMFMT=MAXIE                                                       X4T25500
      RETURN                                                            X4T25510
 1000 FORMAT(12A4,A4,A1,A3,A4,4A3,7X,A1)                                X4T25520
 6000 FORMAT(' ERROR READING REACTION TABLE...EXECUTION TERMINATED')    X4T25530
 6010 FORMAT(' REACTIONS------------',I5,' (',I5,' ALLOWED)')           X4T25540
 6020 FORMAT(' WARNING...ONLY FIRST ',I5,' REACTIONS USED')             X4T25550
      END                                                               X4T25560
      SUBROUTINE TITLEI(NTAPE2)                                         X4T25570
C                                                                       X4T25580
C     READ COLUMN HEADING VS. MF/FIELDS.                                X4T25590
C                                                                       X4T25600
      INTEGER OUTP,OTAPE                                                X4T25610
      CHARACTER*11 TITTAB
      CHARACTER*4  TITTAB4,DUMMY,DUMMY2
      CHARACTER*1 FLAG,BLANK                                            X4T25630
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/TITTBI/ITITLE,MFTITL(11111),MFTITH(11111),IFIELD(11111),
     1 ITFLAG(11111)
      COMMON/TITTBC/TITTAB(11111),TITTAB4(11111)
      DIMENSION DUMMY(7)                                                X4T25680
      DATA BLANK/' '/                                                   X4T25690
      DATA MAXIE/11111/
C-----READ ENTIRE FILE. SKIP CARDS WITH NON-BLANK COLUMN 80.            X4T25710
      DO 20 ITITLE=1,MAXIE                                              X4T25720
   10 READ(NTAPE2,1000,END=60,ERR=50) TITTAB(ITITLE),
     1 DUMMY,TITTAB4(ITITLE),FLAG
      IF(FLAG.NE.BLANK) GO TO 10                                        X4T25750
      CALL INTGER(DUMMY(1),MFTITL(ITITLE),4)                            X4T25760
      CALL INTGER(DUMMY(2),MFTITH(ITITLE),5)                            X4T25770
      CALL INTGER(DUMMY(4),IFIELD(ITITLE),5)                            X4T25780
      IF(IFIELD(ITITLE).LT.0.OR.IFIELD(ITITLE).GT.8) IFIELD(ITITLE)=0   X4T25790
      CALL INTGER(DUMMY(6),ITFLAG(ITITLE),5)                            X4T25800
   20 CONTINUE                                                          X4T25810
      ITITLE=MAXIE                                                      X4T25820
   30 ITITLE=ITITLE+1                                                   X4T25830
   40 READ(NTAPE2,1000,END=60,ERR=50) (DUMMY2,J=1,3),                   X4T25840
     1 DUMMY,DUMMY2,FLAG                                                X4T25850
      IF(FLAG.NE.BLANK) GO TO 40                                        X4T25860
      GO TO 30                                                          X4T25870
   50 WRITE(OUTP,6000)                                                  X4T25880
      STOP                                                              X4T25890
   60 ITITLE=ITITLE-1                                                   X4T25900
      WRITE(OUTP,6010) ITITLE,MAXIE                                     X4T25910
      IF(ITITLE.LE.MAXIE) RETURN                                        X4T25920
      WRITE(OUTP,6020) MAXIE                                            X4T25930
      ITITLE=MAXIE                                                      X4T25940
      RETURN                                                            X4T25950
 1000 FORMAT(A11,A4,A4,A1,A4,A1,A4,A1,2X,A3,44X,A1)
 6000 FORMAT(' ERROR READING TITLE TABLE...EXECUTION TERMINATED')       X4T25970
 6010 FORMAT(' TITLES---------------',I5,' (',I5,' ALLOWED)')           X4T25980
 6020 FORMAT(' WARNING...ONLY FIRST ',I5,' TITLES USED')                X4T25990
      END                                                               X4T26000
      SUBROUTINE UNITI(NTAPE3)                                          X4T26010
C                                                                       X4T26020
C     READ COLUMN UNITS, STANDARD UNITS AND CONVERSION FACTORS          X4T26030
C                                                                       X4T26040
      INTEGER OUTP,OTAPE                                                X4T26050
      CHARACTER*11 UNITAB,DUMMY,DUMMY2
      CHARACTER*1 FLAG,BLANK                                            X4T26070
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/UNITBI/IUNIT,TIMES(11111),ADD(11111),IUFLAG(11111)
      COMMON/UNITBC/UNITAB(2,11111)
      DIMENSION DUMMY(3)
      DATA BLANK/' '/                                                   X4T26120
      DATA MAXIE/11111/
C-----READ ENTIRE FILE. SKIP CARDS WITH NON-BLANK COLUMN 80.            X4T26140
      DO 20 IUNIT=1,MAXIE                                               X4T26150
   10 READ(NTAPE3,1000,END=60,ERR=50) (UNITAB(J,IUNIT),J=1,2),
     1 DUMMY,FLAG                                                       X4T26170
      IF(FLAG.NE.BLANK) GO TO 10                                        X4T26180
      CALL FLOATF(DUMMY(1),TIMES(IUNIT))
      CALL FLOATF(DUMMY(2),ADD(IUNIT))
      CALL INTGER(DUMMY(3),IUFLAG(IUNIT),11)
   20 CONTINUE                                                          X4T26220
      IUNIT=MAXIE                                                       X4T26230
   30 IUNIT=IUNIT+1                                                     X4T26240
   40 READ(NTAPE3,1000,END=60,ERR=50) (DUMMY2,J=1,2),
     1 DUMMY,FLAG                                                       X4T26260
      IF(FLAG.NE.BLANK) GO TO 40                                        X4T26270
      GO TO 30                                                          X4T26280
   50 WRITE(OUTP,6000)                                                  X4T26290
      STOP                                                              X4T26300
   60 IUNIT=IUNIT-1                                                     X4T26310
      WRITE(OUTP,6010) IUNIT,MAXIE                                      X4T26320
      IF(IUNIT.LE.MAXIE) RETURN                                         X4T26330
      WRITE(OUTP,6020) MAXIE                                            X4T26340
      IUNIT=MAXIE                                                       X4T26350
      RETURN                                                            X4T26360
 1000 FORMAT(5(A11),24X,A1)
 6000 FORMAT(' ERROR READING UNITS TABLE...EXECUTION TERMINATED')       X4T26380
 6010 FORMAT(' UNITS----------------',I5,' (',I5,' ALLOWED)')           X4T26390
 6020 FORMAT(' WARNING...ONLY FIRST ',I5,' UNITS USED')                 X4T26400
      END                                                               X4T26410
      SUBROUTINE MFMTX(REACT,INPART,MF,MT,IRFLAG,KNOWN)                 X4T26420
C                                                                       X4T26430
C     DEFINE MF/MT EQUIVALENT OF SIMPLE REACTION.                       X4T26440
C                                                                       X4T26450
      INTEGER OUTP,OTAPE                                                X4T26460
      CHARACTER*4 R2MFMT,REACT                                          X4T26470
      CHARACTER*1 FLAG,BLANK,ENT,SUBENT                                 X4T26480
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T26500
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T26510
      COMMON/RESIDI/KZARES                                              X4T26520
      COMMON/MFMTI1/IMFMT                                               X4T26530
      COMMON/MFMTI2/MFMTAB(7,11111)
      COMMON/MFMTC/R2MFMT(12,11111)
      COMMON/POINTR/MPOINT(9)                                           X4T26560
      DIMENSION REACT(15)                                               X4T26570
      DO 20 I=1,IMFMT                                                   X4T26580
      DO 10 J=1,12                                                      X4T26590
      IF(REACT(J).NE.R2MFMT(J,I)) GO TO 20                              X4T26600
   10 CONTINUE                                                          X4T26610
      GO TO 30                                                          X4T26620
   20 CONTINUE                                                          X4T26630
C                                                                       X4T26640
C     REACTION IS NOT DEFINED. WRITE REACTION TO NEWX4 FILE.            X4T26650
C                                                                       X4T26660
      INPART=0                                                          X4T26670
      MF=0                                                              X4T26680
      MT=0                                                              X4T26690
      IRFLAG=0                                                          X4T26700
      KZARES=0                                                          X4T26710
      KNOWN=0                                                           X4T26720
      WRITE(NEWX4,4000) ENT,ISAN,REACT                                  X4T26730
      MPOINT(7)=MPOINT(7)+1                                             X4T26740
      RETURN                                                            X4T26750
C                                                                       X4T26760
C     REACTION IS DEFINED.                                              X4T26770
C                                                                       X4T26780
   30 INPART=MFMTAB(1,I)                                                X4T26790
      MF=MFMTAB(2,I)                                                    X4T26800
      MT=MFMTAB(3,I)                                                    X4T26810
      IRFLAG=MFMTAB(4,I)                                                X4T26820
      KNOWN=1                                                           X4T26830
C-----ONLY CONSIDER RESIDUAL NUCLEUS FOR PRODUCTION CROSS SECTIONS AND  X4T26840
C-----ANGULAR DISTRIBUTIONS (MF=3 OR MF=4) AND MT=9000-9999.            X4T26850
      IF(MF.NE.3.AND.MF.NE.4) KZARES=0                                  X4T26860
      IF(MT.LT.9000) KZARES=0                                           X4T26870
      RETURN                                                            X4T26880
 4000 FORMAT(1X,5A1,I3,1X,15A4)                                         X4T26890
      END                                                               X4T26900
      SUBROUTINE TITLEX                                                 X4T26910
C                                                                       X4T26920
C     USE REACTION DEFINED MF TO PERMUTE DATA COLUMNS INTO OUTPUT ORDER X4T26930
C     FOR ALL REACTIONS.                                                X4T26940
C                                                                       X4T26950
      INTEGER OUTP,OTAPE                                                X4T26960
      CHARACTER*11 TITLE,UNIT,DATUM,TITTAB
      CHARACTER*4  TITLE4,TITTAB4
      CHARACTER*1 FLAGI,FLAGR,ZAN,BLANK ,ENT,SUBENT,LABCM,ZANRES,ZANRAT X4T26980
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T27000
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T27010
      COMMON/TITTBI/ITITLE,MFTITL(11111),MFTITH(11111),IFIELD(11111),
     1 ITFLAG(11111)
      COMMON/TITTBC/TITTAB(11111),TITTAB4(11111)
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T27050
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T27060
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADC2/FLAGI(50)                                           X4T27080
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T27090
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     X4T27100
     1 LABCM(30)                                                        X4T27110
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          X4T27120
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T27130
     1 KTFLGX(50),KUFLGX(50)                                            X4T27140
      COMMON/POINTR/MPOINT(9)                                           X4T27150
      DATA BLANK/' '/                                                   X4T27160
C-----SET UP LOOP OVER REACTIONS.                                       X4T27170
      IF(KSANR.LE.0) RETURN                                             X4T27180
      DO 100 ISANR=1,KSANR                                              X4T27190
C-----INITIALIZE OUTPUT FIELD VECTORS.                                  X4T27200
      DO K=1,10
        DO I=1,8
          IMOUT(I,ISANR,K)=0
        END DO
      END DO
C-----SELECT COLUMNS WITH COLUMN 11 BLANK OR SAME AS REACTION HAS IN    X4T27250
C-----COLUMN 11.                                                        X4T27260
      DO 90 KIN=1,IDATN                                                 X4T27270
      IF(FLAGI(KIN).EQ.BLANK) GO TO 30                                  X4T27280
      IF(FLAGI(KIN).NE.FLAGR(ISANR)) GO TO 90                           X4T27290
C-----SELECT TITLES FROM TABLE FOR WHICH MF RANGE SPANS MF OF REACTION. X4T27300
   30 DO 50 KTAB=1,ITITLE                                               X4T27310
      IF(MFR(ISANR).LT.MFTITL(KTAB).OR.                                 X4T27320
     1 MFR(ISANR).GT.MFTITH(KTAB)) GO TO 50                             X4T27330
      IF(TITLE(KIN).NE.TITTAB(KTAB)) GO TO 50
      GO TO 60                                                          X4T27370
   50 CONTINUE                                                          X4T27380
C                                                                       X4T27390
C     TITLE IS NOT DEFINED. WRITE TITLE TO NEWX4 FILE.                  X4T27400
C                                                                       X4T27410
      WRITE(NEWX4,4000) ENT,ISAN,TITLE(KIN)
      MPOINT(8)=MPOINT(8)+1                                             X4T27430
      GO TO 90                                                          X4T27440
C                                                                       X4T27450
C     TITLE IS DEFINED. DEFINE POSITION OF OUTPUT FIELD (SKIP IF        X4T27460
C     NOT USED IN OUTPUT).                                              X4T27470
C                                                                       X4T27480
   60 KOUT=IFIELD(KTAB)                                                 X4T27490
      IF(KOUT.LE.0) GO TO 90                                            X4T27500
C-----DEFINE FIELDS 7-8.                                                X4T27510
      TITLE4(KIN)=TITTAB4(KTAB)
C-----SAVE INPUT FIELD INDEX IN NEXT AVAILABLE OUTPUT FIELD LOCATION.   X4T27530
      DO 70 JMULT=1,10                                                  X4T27540
      IF(IMOUT(KOUT,ISANR,JMULT).LE.0) GO TO 80                         X4T27550
   70 CONTINUE                                                          X4T27560
      JMULT=10                                                          X4T27570
   80 IMOUT(KOUT,ISANR,JMULT)=KIN                                       X4T27580
C-----DEFINE TITLE OPERATION FLAG FOR INPUT COLUMN.                     X4T27590
      KTFLGX(KIN)=ITFLAG(KTAB)                                          X4T27600
C-----INDICATE INPUT FIELD USED (TO FORCE HOLLERITH TO FLOATING POINT   X4T27610
C-----TRANSLATION).                                                     X4T27620
      IMUSED(KIN)=1                                                     X4T27630
   90 CONTINUE                                                          X4T27640
  100 CONTINUE                                                          X4T27650
      RETURN                                                            X4T27660
 4000 FORMAT(1X,5A1,I3,1X,A10)
      END                                                               X4T27680
      SUBROUTINE TOPS1                                                  X4T27690
C                                                                       X4T27700
C     PERFORM TITLE OPERATIONS THAT APPLY TO ENTIRE TABLE,              X4T27710
C     (1) SET CENTER-OF-MASS FLAG.                                      X4T27720
C     (2) DEFINE -MIN OR -MAX TO CREATE A PAIR (CREATE -MIN= 0,         X4T27730
C         OR -MAX = 15 MEV).                                            X4T27740
C                                                                       X4T27750
      INTEGER OUTP,OTAPE                                                X4T27760
      CHARACTER*11 TITLE,UNIT,DATUM,EV,LIMITS,TITLE2,TITLE3
      CHARACTER*4  TITLE4
      CHARACTER*1  FLAGI,FLAGR,ZAN,ENT,SUBENT,LABCM,CENTER
     &            ,ZANRES,ZANRAT,BLANK                                                            X4T27790
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T27810
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T27820
      COMMON/TITTBI/ITITLE,MFTITL(11111),MFTITH(11111),IFIELD(11111),
     1 ITFLAG(11111)
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T27860
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T27870
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADC2/FLAGI(50)                                           X4T27890
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T27900
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     X4T27910
     1 LABCM(30)                                                        X4T27920
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          X4T27930
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T27940
     1 KTFLGX(50),KUFLGX(50)                                            X4T27950
      COMMON/POINTR/MPOINT(9)                                           X4T27960
      DIMENSION LIMITS(2)
      DATA CENTER/'C'/                                                  X4T27980
      DATA BLANK/' '/                                                   X4T27990
      DATA EV/'EV         '/
      DATA LIMITS/                                                      X4T28010
     1 ' 0.0       ',' 1.50000+ 7'/
      KDATN=IDATN                                                       X4T28040
      DO 110 ISANR=1,KSANR                                              X4T28050
C-----INITIALIZE SYSTEM FLAG TO BLANK.                                  X4T28060
      LABCM(ISANR)=BLANK                                                X4T28070
      DO 100 KFIELD=1,8                                                 X4T28080
      DO 90 KMULT=1,10                                                  X4T28090
      KIN=IMOUT(KFIELD,ISANR,KMULT)                                     X4T28100
      IF(KIN.LE.0) GO TO 100                                            X4T28110
C-----ONLY CONSIDER FIELDS THAT ARE REQUIRED FOR OUTPUT.                X4T28120
      IF(IMUSED(KIN).LE.0) GO TO 90                                     X4T28130
C-----IF REQUESTED SET CENTER-OF-MASS SYSTEM FLAG.                      X4T28140
      IF(KTFLGX(KIN).NE.6) GO TO 10                                     X4T28150
      LABCM(ISANR)=CENTER                                               X4T28160
      WRITE(OUTP,6030)                                                  X4T28170
      GO TO 90                                                          X4T28180
C-----SEE IF -MIN AND -MAX MUST APPEAR IN PAIR.                         X4T28190
   10 IF(KTFLGX(KIN).NE.9) GO TO 90                                     X4T28200
C-----DECODE TITLE TO DEFINE COMPLEMENTARY TITLE AND WHETHER TITLE      X4T28210
C-----ENDS IN -MIN, -MAX OR OTHER (ERROR).                              X4T28220
      CALL IPAIR(TITLE(KIN),TITLE2,IWAY)
      IF(IWAY.GT.0) GO TO 20                                            X4T28240
C-----ERROR. TITLE DOES NOT END IN -MIN OR -MAX.                        X4T28250
      WRITE(OUTP,6000) TITLE(KIN),FLAGI(KIN)
      GO TO 90                                                          X4T28270
C-----TITLE ENDS IN -MIN OR -MAX. SCAN REMAINING TITLES FOR OTHER       X4T28280
C-----LIMIT AND SAME TITLE FLAG.                                        X4T28290
   20 JIN=KIN+1                                                         X4T28300
      IF(JIN.GT.KDATN) GO TO 70                                         X4T28310
      DO 60 K=JIN,KDATN                                                 X4T28320
      IF(IMUSED(K).LE.0.OR.KTFLGX(K).NE.9) GO TO 60                     X4T28330
      IF(FLAGI(K).NE.FLAGI(KIN)) GO TO 60                               X4T28340
      CALL IPAIR(TITLE(K),TITLE3,KWAY)
      IF(KWAY.EQ.0) GO TO 60                                            X4T28360
      IF(IWAY.NE.KWAY) GO TO 40                                         X4T28370
C-----POSSIBLE MULTIPLE SAME LIMIT. CHECK FOR SAME TITLE.               X4T28380
      IF(TITLE(KIN).NE.TITLE3) GO TO 90
      WRITE(OUTP,6010) TITLE(KIN),FLAGI(KIN)
      GO TO 90                                                          X4T28430
C-----SEE IF RECONSTRUCTED TITLE TITLE IS SAME AS ORIGINAL              X4T28440
C-----(E.G., AVOID ASUMMING EN-MIN AND E-MAX ARE A PAIR).               X4T28450
   40 IF(TITLE(KIN).NE.TITLE3) GO TO 60
C-----LIMITS ARE PAIRED.                                                X4T28490
      GO TO 90                                                          X4T28500
   60 CONTINUE                                                          X4T28510
C-----LIMITS ARE NOT PAIRED. CREATE DATA POINT.                         X4T28520
   70 IDATN=IDATN+1                                                     X4T28530
      KWAY=3-IWAY                                                       X4T28540
      TITLE(IDATN)=TITLE2
      DATUM(IDATN)=LIMITS(KWAY)
      UNIT(IDATN)=EV
      FLAGI(IDATN)=FLAGI(KIN)                                           X4T28590
      IMUSED(IDATN)=1                                                   X4T28600
      WRITE(OUTP,6020) TITLE(IDATN),FLAGI(IDATN),
     1 DATUM(IDATN),UNIT(IDATN)
C-----SET INDEX TO OUTPUT CREATED LIMIT NEXT TO EXISTING LIMIT.         X4T28630
      IF(IWAY.EQ.1) IMOUT(KFIELD+1,ISANR,KMULT)=IDATN                   X4T28640
      IF(IWAY.EQ.2) IMOUT(KFIELD-1,ISANR,KMULT)=IDATN                   X4T28650
   90 CONTINUE                                                          X4T28660
  100 CONTINUE                                                          X4T28670
  110 CONTINUE                                                          X4T28680
      RETURN                                                            X4T28690
 6000 FORMAT(10X,'WARNING.....CHECK -MIN/-MAX FLAG FOR ',A10,A1)
 6010 FORMAT(10X,'WARNING.....MULTIPLE -MIN/-MAX FIELDS ',A10,A1)
 6020 FORMAT(10X,'OPERATION...CREATED ',A10,A1,1X,A11,1X,A11)
 6030 FORMAT(10X,'OPERATION...CENTER-OF-MASS SYSTEM FLAG SET')          X4T28730
 6040 FORMAT(10X,'OPERATION...ABSOLUTE',A11)
      END                                                               X4T28740
      SUBROUTINE IPAIR(TITLE1,TITLE2,IWAY)                              X4T28750
C                                                                       X4T28760
C     SEARCH TITLE1 FOR ENDING OF -MIN OR -MAX.                         X4T28770
C     IF FOUND, CREATE TITLE2 TO BE OTHER LIMIT.                        X4T28780
C                                                                       X4T28790
      CHARACTER*1 TITLE1,TITLE2,MINMAX,BLANK                            X4T28800
      DIMENSION TITLE1(10),TITLE2(10),MINMAX(4,2)                       X4T28810
      DATA MINMAX/                                                      X4T28820
     1 '-','M','I','N',                                                 X4T28830
     2 '-','M','A','X'/                                                 X4T28840
      DATA BLANK/' '/                                                   X4T28850
C-----FIND LAST NON-BLANK CHARACTER.                                    X4T28860
      II=11                                                             X4T28870
      DO I=1,10
        II=II-1
        IF(TITLE1(II).NE.BLANK) GO TO 20
      END DO
      GO TO 50                                                          X4T28920
C-----SEARCH FOR -MIN OR -MAX.                                          X4T28930
   20 DO 40 IWAY=1,2                                                    X4T28940
      JJ=4                                                              X4T28950
      KK=II                                                             X4T28960
      DO 30 J=1,4                                                       X4T28970
      IF(TITLE1(KK).NE.MINMAX(JJ,IWAY)) GO TO 40                        X4T28980
      JJ=JJ-1                                                           X4T28990
   30 KK=KK-1                                                           X4T29000
      GO TO 60                                                          X4T29010
   40 CONTINUE                                                          X4T29020
C----- -MIN/-MAX NOT FOUND.                                             X4T29030
   50 IWAY=0                                                            X4T29040
      RETURN                                                            X4T29050
C----- -MIN/-MAX FOUND. DEFINE COMPLEMENTARY TITLE.                     X4T29060
   60 II=II-4                                                           X4T29070
      KWAY=3-IWAY                                                       X4T29080
      DO I=1,II
        TITLE2(I)=TITLE1(I)
      END DO
      DO I=1,4
        II=II+1
        TITLE2(II)=MINMAX(I,KWAY)
      END DO
      IF(II.GE.11) GO TO 100                                            X4T29140
      II=II+1                                                           X4T29150
      DO I=II,11
        TITLE2(I)=BLANK
      END DO
  100 RETURN                                                            X4T29180
      END                                                               X4T29190
      SUBROUTINE UNIT1                                                  X4T29200
C                                                                       X4T29210
C     DEFINE UNIT CONVERSION FACTORS AND OPERATIONS.                    X4T29220
C                                                                       X4T29230
      INTEGER      OUTP,OTAPE
      CHARACTER*11 UNITAB,TITLE,UNIT,DATUM
      CHARACTER*4  TITLE4
      CHARACTER*1  ENT,SUBENT,STAT1,STATN,UNNORM
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T29280
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T29290
      COMMON/UNITBI/IUNIT,TIMES(11111),ADD(11111),IUFLAG(11111)
      COMMON/UNITBC/UNITAB(2,11111)
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T29330
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T29340
     1 KTFLGX(50),KUFLGX(50)                                            X4T29350
      COMMON/STATUC/STAT1,STATN                                         X4T29360
      COMMON/POINTR/MPOINT(9)                                           X4T29370
      DATA UNNORM/'U'/                                                  X4T29380
C                                                                       X4T29390
C     ONLY CONSIDER FIELDS THAT ARE REQUIRED FOR OUTPUT.                X4T29400
C                                                                       X4T29410
      DO 30 I=1,IDATN                                                   X4T29420
      IF(IMUSED(I).LE.0) GO TO 30                                       X4T29430
C-----DETERMINE CONVERSION FACTOR FOR UNITS.                            X4T29440
      DO 20 J=1,IUNIT                                                   X4T29450
      IF(UNIT(I).NE.UNITAB(1,J)) GO TO 20
C-----TITLE IS DEFINED. DEFINE MULTIPLIER, ADDER AND UNIT OPERATION.    X4T29490
      TIMEX(I)=TIMES(J)                                                 X4T29500
      ADDX(I)=ADD(J)                                                    X4T29510
      KUFLGX(I)=IUFLAG(J)                                               X4T29520
C-----IF REQUESTED PRINT WARNING MESSAGE.                               X4T29530
      IF(IUFLAG(J).EQ.7) WRITE(OUTP,6000) UNIT(I)
      IF(IUFLAG(J).NE.8) GO TO 30                                       X4T29550
      WRITE(OUTP,6010) UNIT(I)
      STATN=UNNORM                                                      X4T29570
      GO TO 30                                                          X4T29580
   20 CONTINUE                                                          X4T29590
C                                                                       X4T29600
C     UNITS IS NOT DEFINED. WRITE TITLE TO NEWX4 FILE.                  X4T29610
C                                                                       X4T29620
      TIMEX(I)=0.0                                                      X4T29630
      ADDX(I)=0.0                                                       X4T29640
      KUFLGX(I)=0                                                       X4T29650
      WRITE(NEWX4,4000) ENT,ISAN,UNIT(I)
      MPOINT(9)=MPOINT(9)+1                                             X4T29670
   30 CONTINUE                                                          X4T29680
      RETURN                                                            X4T29690
 4000 FORMAT(1X,5A1,I3,1X,A10)
 6000 FORMAT(10X,'WARNING.....UNITS=',A11)
 6010 FORMAT(10X,'WARNING.....UNITS=',A11,' STATUS CHANGED TO',
     1 ' UNNORMALIZED (U)')                                             X4T29730
      END                                                               X4T29740
      SUBROUTINE UNIT2                                                  X4T29750
C                                                                       X4T29760
C     TRANSLATE FIELDS TO STANDARD UNITS.                               X4T29770
C                                                                       X4T29780
      CHARACTER*11 TITLE,UNITAB,UNIT,DATUM
      CHARACTER*4  TITLE4
      CHARACTER*1  ENT,SUBENT
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T29810
      COMMON/WHERE/ENT(5),SUBENT(3)                                     X4T29820
      COMMON/UNITBI/IUNIT,TIMES(11111),ADD(11111),IUFLAG(11111)
      COMMON/UNITBC/UNITAB(2,11111)
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T29850
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T29860
      COMMON/RNOW/ISANR                                                 X4T29870
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          X4T29880
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T29900
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T29910
     1 KTFLGX(50),KUFLGX(50)                                            X4T29920
      COMMON/INVAL/VALUEI(50)                                           X4T29930
C                                                                       X4T29940
C     ONLY TRANSLATE FIELDS THAT ARE REQUIRED FOR OUTPUT.               X4T29950
C                                                                       X4T29960
      DO 40 KFIELD=1,8                                                  X4T29970
      DO 30 KMULT=1,10                                                  X4T29980
      II=IMOUT(KFIELD,ISANR,KMULT)                                      X4T29990
      IF(II.EQ.0) GO TO 40                                              X4T30000
      IF(IMUSED(II)-1) 40,10,20                                         X4T30010
C-----CONVERT DATA FROM HOLLERITH TO FLOATING POINT.                    X4T30020
   10 CALL FLOATF(DATUM(II),VALUEI(II))
      IMUSED(II)=2                                                      X4T30040
C-----APPLY CONVERSION FACTORS.                                         X4T30050
   20 VALUES(II)=TIMEX(II)*VALUEI(II)+ADDX(II)                          X4T30060
   30 CONTINUE                                                          X4T30070
   40 CONTINUE                                                          X4T30080
      RETURN                                                            X4T30090
      END                                                               X4T30100
      SUBROUTINE UNOPS                                                  X4T30110
C                                                                       X4T30120
C     APPLY UNIT CONVERSION OPTIONS,                                    X4T30130
C                                                                       X4T30140
C     (1) PER-CENT TO ABSOLUTE                                          X4T30150
C     (2) ANGLE TO COSINE (ANGLE OR ANGULAR ERROR)                      X4T30160
C     (3) RESOLUTION (E.G. NSEC/M) TO ENERGY ERROR (EV).                X4T30170
C     (4) ANGSTROM TO EV.                                               X4T30180
C     (5) LENGTH (CM OR FERMI) TO AREA (BARNS)                          X4T30190
C     (6) BARNS*SQRT(E) TO BARNS                                        X4T30200
C                                                                       X4T30210
      INTEGER OUTP,OTAPE                                                X4T30220
      CHARACTER*11 UNITAB,TITLE,UNIT,DATUM
      CHARACTER*4  TITLE4
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/UNITBI/IUNIT,TIMES(11111),ADD(11111),IUFLAG(11111)
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T30260
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T30270
      COMMON/RNOW/ISANR                                                 X4T30280
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T30290
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T30300
      COMMON/UNITBC/UNITAB(2,11111)
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          X4T30330
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T30340
     1 KTFLGX(50),KUFLGX(50)                                            X4T30350
      DIMENSION KUFLG1(50)                                              X4T30360
      DATA PI/3.141597/                                                 X4T30370
      DATA RES2EV/2.77E-5/                                              X4T30380
C-----DEFINE UNIT OPERATIONS FOR INTERNAL USE.                          X4T30390
      DO I=1,IDATN
        KUFLG1(I)=KUFLGX(I)
      END DO
C-----SET UP LOOP OVER OUTPUT FIELDS.                                   X4T30420
      DO 140 KFIELD=1,8                                                 X4T30430
C-----SET UP LOOP OVER EXFOR FIELDS MAPPED INTO OUTPUT FIELD.           X4T30440
      DO 130 JMULT=1,10                                                 X4T30450
C-----DETERMINE IF OUTPUT FIELD IS USED, AND IF SO WHEATHER OR NOT      X4T30460
C-----TO PERFORM AN OPERATION ON IT.                                    X4T30470
      II=IMOUT(KFIELD,ISANR,JMULT)                                      X4T30480
      IF(II.LE.0) GO TO 140                                             X4T30490
      IF(KUFLG1(II).LE.0) GO TO 140                                     X4T30500
C                                                                       X4T30510
C     PERFORM PER-CENT TO ABSOLUTE CONVERSION.                          X4T30520
C                                                                       X4T30530
C     TO CONVERT FROM PER-CENT TO ABSOLUTE MULTIPLY THE OUTPUT FIELD    X4T30540
C     BY 0.01 TIMES THE PRECEDING OUTPUT FIELD (THIS WILL WORK FOR      X4T30550
C     ENERGY FOLLOWED BY ENERGY ERROR, DATA FOLLOWED BY DATA ERROR,ETC.)X4T30560
C                                                                       X4T30570
      IF(KUFLG1(II).NE.1) GO TO 30                                      X4T30580
      JJ=IMOUT(KFIELD-1,ISANR,1)                                        X4T30590
      IF(JJ.GT.0) GO TO 20                                              X4T30600
      IF(NPT.EQ.1) WRITE(OUTP,6000)                                     X4T30610
      VALUES(II)=0.0                                                    X4T30620
      GO TO 120                                                         X4T30630
   20 VALUES(II)=ABS(0.01*VALUES(II)*VALUES(JJ))                        X4T30640
      IF(NPT.EQ.1) WRITE(OUTP,6040)                                     X4T30650
      GO TO 120                                                         X4T30660
C                                                                       X4T30670
C     PERFORM ANGLE TO COSINE CONVERSION.                               X4T30680
C                                                                       X4T30690
   30 IF(KUFLG1(II).NE.2) GO TO 60                                      X4T30700
      IF(KFIELD.EQ.5) GO TO 50                                          X4T30710
C                                                                       X4T30720
C     CONVERT ANGULAR RESOLUTION TO COSINE RESOLUTION.                  X4T30730
C                                                                       X4T30740
C     DEFINE COSINE RESOLUTION TO BE,                                   X4T30750
C                                                                       X4T30760
C     DMU = ABS(COS(ANGLE+DANGLE)-COS(ANGLE))+                          X4T30770
C           ABS(COS(ANGLE-DANGLE)-COS(ANGLE)))/2.0                      X4T30780
C                                                                       X4T30790
      JJ=IMOUT(KFIELD-1,ISANR,1)                                        X4T30800
      IF(JJ.GT.0) GO TO 40                                              X4T30810
      IF(NPT.EQ.1) WRITE(OUTP,6010)                                     X4T30820
      VALUES(II)=0.0                                                    X4T30830
      GO TO 120                                                         X4T30840
   40 XMU=VALUES(JJ)                                                    X4T30850
      IF(ABS(XMU).GT.1) THEN
        ANG=XMU*PI/180
        WRITE(OUTP,6051)
      ELSE
        ANG=ACOS(XMU)
      END IF
      DANG=PI*VALUES(II)/180.0                                          X4T30870
      DMUP=COS(ANG+DANG)                                                X4T30880
      DMUM=COS(ANG-DANG)                                                X4T30890
      VALUES(II)=0.5*(ABS(DMUP-XMU)+ABS(DMUM-XMU))                      X4T30900
      IF(NPT.EQ.1) WRITE(OUTP,6055)                                     X4T30910
      GO TO 120                                                         X4T30920
C-----CONVERT ANGLE TO COSINE.                                          X4T30930
   50 VALUES(II)=COS(PI*VALUES(II)/180.0)                               X4T30940
C-----ADJUST FOR EXACTLY 90 DEGREES (COSINE MAY DIFFER SLIGHTLY FROM    X4T30950
C-----ZERO DUE TO APPROXIMATION OF PI TO ACCURACY OF COMPUTER).         X4T30960
      IF(ABS(VALUES(II)).LT.0.00005) VALUES(II)=0.0                     X4T30970
      IF(NPT.EQ.1) WRITE(OUTP,6050)                                     X4T30980
      GO TO 120                                                         X4T30990
C                                                                       X4T31000
C     PERFORM RESOLUTION (E.G. NSEC/M) TO ENERGY ERROR (EV).            X4T31010
C                                                                       X4T31020
   60 IF(KUFLG1(II).NE.3) GO TO 80                                      X4T31030
      JJ=IMOUT(1,ISANR,1)                                               X4T31040
      IF(JJ.GT.0) GO TO 70                                              X4T31050
      IF(NPT.EQ.1) WRITE(OUTP,6020)                                     X4T31060
      VALUES(II)=0.0                                                    X4T31070
      GO TO 120                                                         X4T31080
   70 XE=VALUES(JJ)                                                     X4T31090
      VALUES(II)=RES2EV*VALUES(II)*XE*SQRT(XE)                          X4T31100
      IF(NPT.EQ.1) WRITE(OUTP,6060)                                     X4T31110
      GO TO 120                                                         X4T31120
C                                                                       X4T31130
C     PERFORM ANGSTROM TO EV CONVERSION.                                X4T31140
C                                                                       X4T31150
   80 IF(KUFLG1(II).NE.4) GO TO 90                                      X4T31160
      XE=VALUES(II)                                                     X4T31170
      IF(XE.LE.0.0) GO TO 130                                           X4T31180
      VALUES(II)=(8.18E+10)/(XE*XE)                                     X4T31190
      IF(NPT.EQ.1) WRITE(OUTP,6070)                                     X4T31200
      GO TO 120                                                         X4T31210
C                                                                       X4T31220
C     PERFORM LENGTH (CM OR FERMI) TO AREA (BARNS) CONVERSION.          X4T31230
C                                                                       X4T31240
   90 IF(KUFLG1(II).NE.5) GO TO 100                                     X4T31250
      VALUES(II)=4.0*PI*VALUES(II)*VALUES(II)                           X4T31260
      IF(NPT.EQ.1) WRITE(OUTP,6080)                                     X4T31270
      GO TO 120                                                         X4T31280
C                                                                       X4T31290
C     PERFORM BARNS*SQRT(E) TO BARNS CONVERSION.                        X4T31300
C                                                                       X4T31310
  100 IF(KUFLG1(II).NE.6) GO TO 130                                     X4T31320
      JJ=IMOUT(1,ISANR,1)                                               X4T31330
      IF(JJ.GT.0) GO TO 110                                             X4T31340
      IF(NPT.EQ.1) WRITE(OUTP,6030)                                     X4T31350
      VALUES(II)=0.0                                                    X4T31360
      GO TO 120                                                         X4T31370
  110 XE=VALUES(JJ)                                                     X4T31380
      IF(XE.LE.0.0) GO TO 130                                           X4T31390
      VALUES(II)=VALUES(II)/SQRT(XE)                                    X4T31400
      IF(NPT.EQ.1) WRITE(OUTP,6090)                                     X4T31410
C                                                                       X4T31420
C     TURN OFF FLAG TO INSURE OPERATION IS ONLY PERFORMED ONCE ON EACH  X4T31430
C     INPUT VALUE.                                                      X4T31440
C                                                                       X4T31450
  120 KUFLG1(II)=0                                                      X4T31460
  130 CONTINUE                                                          X4T31470
  140 CONTINUE                                                          X4T31480
      CONTINUE                                                          X4T31490
      RETURN                                                            X4T31500
 6000 FORMAT(10X,'WARNING.....CANNOT CONVERT PER-CENT TO ABSOLUTE'/     X4T31510
     1 10X,'REQUIRED PRECEDING DATA FIELD NOT DEFINED')                 X4T31520
 6010 FORMAT(10X,'WARNING.....CANNOT CONVERT ANGLE TO COSINE ERROR'/    X4T31530
     1 10X,'REQUIRED COSINE FIELD NOT DEFINED')                         X4T31540
 6020 FORMAT(10X,'WARNING.....CANNOT CONVERT RESOLUTION TO ERROR'/      X4T31550
     1 10X,'REQUIRED ENERGY FIELD NOT DEFINED')                         X4T31560
 6030 FORMAT(10X,'WARNING.....CANNOT CONVERT BARNS*SQRT(E) TO BARNS'/   X4T31570
     1 10X,'REQUIRED ENERGY FIELD NOT DEFINED')                         X4T31580
 6040 FORMAT(10X,'OPERATION...CONVERTED PER-CENT TO ABSOLUTE')          X4T31590
 6050 FORMAT(10X,'OPERATION...CONVERTED ANGLES TO COSINES')             X4T31600
 6051 FORMAT(10X,'WARNING...CANNOT CONVERT COSINE TO ANGLE'/
     1 10X,'|COSINE| > 1; ASSUMED ANGLE IS GIVEN')
 6055 FORMAT(10X,'OPERATION...CONVERTED ANGULAR ERROR TO COSINE ERROR') X4T31610
 6060 FORMAT(10X,'OPERATION...CONVERTED RESOLUTION TO ERROR')           X4T31620
 6070 FORMAT(10X,'OPERATION...CONVERTED ANGSTROM TO ENERGY')            X4T31630
 6080 FORMAT(10X,'OPERATION...CONVERTED LENGTH TO BARNS')               X4T31640
 6090 FORMAT(10X,'OPERATION...CONVERTED BARNS*SQRT(E) TO BARNS')        X4T31650
      END                                                               X4T31660
      SUBROUTINE TOPS2                                                  X4T31670
C                                                                       X4T31680
C     RESOLVE MULTIPLE INPUT FIELDS MAPPED INTO A SINGLE OUTPUT FIELD.  X4T31690
C                                                                       X4T31700
      INTEGER OUTP,OTAPE                                                X4T31710
      CHARACTER*11 TITLE,UNIT,DATUM
      CHARACTER*4  TITLE4
      CHARACTER*1  FLAGI
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T31750
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T31760
      COMMON/RNOW/ISANR                                                 X4T31770
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T31780
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T31790
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADC2/FLAGI(50)                                           X4T31810
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          X4T31820
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T31830
     1 KTFLGX(50),KUFLGX(50)                                            X4T31840
      DIMENSION KUFLG1(50)                                              X4T31850
      DATA PI/3.141597/                                                 X4T31860
C-----INITIALIZE FIELD SKIP FLAG.                                       X4T31870
      ISKIP=0                                                           X4T31880
C-----SET UP LOOP OVER OUTPUT FIELDS.                                   X4T31890
      DO 310 KFIELD=1,8                                                 X4T31900
C-----CHECK FOR CURRENT FIELD DEFINED BY PRECEDING FIELD.               X4T31910
      IF(ISKIP.LE.0) GO TO 10                                           X4T31920
      ISKIP=0                                                           X4T31930
      GO TO 310                                                         X4T31940
C-----COUNT THE NUMBER OF INPUT FIELDS MAPPED INTO 1 OUTPUT FIELD.      X4T31950
   10 DO JMULT=1,10
        II=IMOUT(KFIELD,ISANR,JMULT)
        IF(II.LE.0) GO TO 30
C-----  OPERATION ABSOLUTE ON THE VALUES (IF REQUESTED)
        IF(KTFLGX(II).EQ.11) VALUES(II)=ABS(VALUES(II))
      END DO
      JMULT=10                                                          X4T32000
C                                                                       X4T32010
C     ATTEMPT TO RESOLVE MULTIPLE FIELD DEFINITION (IF ANY).            X4T32020
C                                                                       X4T32030
   30 IF(JMULT.LE.2) GO TO 290                                          X4T32040
      JMULT=JMULT-1                                                     X4T32050
C-----ONLY PRINT MULTIPLE FIELD WARNING MESSAGE FOR FIRST POINT.        X4T32060
      IF(NPT.GT.1) GO TO 50                                             X4T32070
      WRITE(OUTP,6000)                                                  X4T32080
      DO KMULT=1,JMULT
        JJ=IMOUT(KFIELD,ISANR,KMULT)
        WRITE(OUTP,6020) TITLE(JJ),FLAGI(JJ)
      END DO
C-----USE TITLE FLAG TO (1) ALWAYS CHOOSE.                              X4T32120
   50 DO KMULT=1,JMULT
        JJ=IMOUT(KFIELD,ISANR,KMULT)
        IF(KTFLGX(JJ).EQ.1) GO TO 90
      END DO
C-----USE TITLE FLAG TO (2) CHOOSE FIRST (3) NEVER CHOOSE.              X4T32170
      KK=0                                                              X4T32180
      DO 70 KMULT=1,JMULT
        JJ=IMOUT(KFIELD,ISANR,KMULT)
        IF(KTFLGX(JJ).EQ.2) GO TO 90
        IF(KTFLGX(JJ).EQ.3) GO TO 70
        KK=KK+1
        IMOUT(KFIELD,ISANR,KK)=IMOUT(KFIELD,ISANR,KMULT)
   70 CONTINUE
      JMULT=KK                                                          X4T32260
C-----SET NEXT FIELD TO ZERO TO ELIMINATE ALL (3) NEVER CHOOSE FIELDS   X4T32270
C-----FOR ALL POINTS IN TABLE.                                          X4T32280
      IF(JMULT.LT.10) IMOUT(KFIELD,ISANR,JMULT+1)=0                     X4T32290
      IF(JMULT-1) 80,90,100                                             X4T32300
C-----NO FIELDS LEFT. SET INDEX FOR NO OUTPUT.                          X4T32310
   80 JJ=0                                                              X4T32320
      IF(NPT.EQ.1) WRITE(OUTP,6090)                                     X4T32330
      GO TO 300                                                         X4T32340
C-----IF (1) ALWAYS CHOOSE OR (2) CHOOSE FIRST FIELD OR ONLY ONE        X4T32350
C-----FIELD LEFT NO MORE CONFLICT FOR ALL POINTS IN TABLE.              X4T32360
   90 IMOUT(KFIELD,ISANR,1)=JJ                                          X4T32370
      IMOUT(KFIELD,ISANR,2)=0                                           X4T32380
      IF(NPT.EQ.1) WRITE(OUTP,6080) TITLE(JJ),FLAGI(JJ)
      GO TO 300                                                         X4T32400
C-----SEE IF ALL REMAINING FIELDS HAVE THE SAME TITLE FLAG.             X4T32410
  100 JOPS=0                                                            X4T32420
      DO 110 KMULT=1,JMULT                                              X4T32430
      JJ=IMOUT(KFIELD,ISANR,KMULT)                                      X4T32440
C-----ERROR IF USER DOES NOT SPECIFY HOW TO RESOLVE.                    X4T32450
      IF(KTFLGX(JJ).LE.0) GO TO 260                                     X4T32460
      IF(JOPS.LE.0) JOPS=KTFLGX(JJ)                                     X4T32470
      IF(KTFLGX(JJ).NE.JOPS) GO TO 260                                  X4T32480
  110 CONTINUE                                                          X4T32490
C-----SEE IF ALL REMAINING FIELDS SHOULD BE (4) USED TO SELECT LARGEST  X4T32500
C-----(5) COMBINE QUADRATICALLY.                                        X4T32510
      IF(JOPS.GT.5) GO TO 140                                           X4T32520
      IF(NPT.EQ.1.AND.JOPS.EQ.4) WRITE(OUTP,6100)                       X4T32530
      IF(NPT.EQ.1.AND.JOPS.EQ.5) WRITE(OUTP,6110)                       X4T32540
C-----CHOOSE LARGEST OR COMBINE QUADRATICALLY.                          X4T32550
      DXDX=0.0                                                          X4T32560
      DO 130 KMULT=1,JMULT                                              X4T32570
      II=IMOUT(KFIELD,ISANR,KMULT)                                      X4T32580
      DX=ABS(VALUES(II))                                                X4T32590
      DXDX=DXDX+DX*DX                                                   X4T32600
      IF(KMULT.EQ.1) GO TO 120                                          X4T32610
      IF(DX.LE.DXMAX) GO TO 130                                         X4T32620
  120 JJ=II                                                             X4T32630
      DXMAX=DX                                                          X4T32640
  130 CONTINUE                                                          X4T32650
C-----SELECT LARGEST OR QUADRATIC COMBINATION.                          X4T32660
      IF(JOPS.EQ.4) GO TO 300                                           X4T32670
      DX=SQRT(DXDX)                                                     X4T32680
      IDATN=IDATN+1                                                     X4T32690
      VALUES(IDATN)=DX                                                  X4T32700
      JJ=IDATN                                                          X4T32710
C-----SAVE DEFINITION OF FIELDS 7-8.                                    X4T32720
      IF(KFIELD.NE.7) GO TO 300                                         X4T32730
      II=IMOUT(KFIELD,ISANR,1)                                          X4T32740
      TITLE4(IDATN)=TITLE4(II)
      GO TO 300                                                         X4T32760
C-----SEE IF DATA ARE TO BE COMBINED TO DEFINE AVERAGE AND SPREAD.      X4T32770
  140 IF(JOPS.EQ.7) GO TO 150                                           X4T32780
      IF(JOPS.GT.8) GO TO 190                                           X4T32790
C-----CANNOT COMBINE FIELDS TO DEFINE AVERAGE AND ERROR IF CURRENT      X4T32800
C-----OUTPUT FIELD IS 8 (I.E. NO OUTPUT FIELD 9) OR IF THE NEXT FIELD   X4T32810
C-----IS ALREADY USED.                                                  X4T32820
      IF(KFIELD.LT.8.AND.IMOUT(KFIELD+1,ISANR,1).LE.0) GO TO 150        X4T32830
      IF(NPT.GT.1) GO TO 290                                            X4T32840
      KFP1=KFIELD+1                                                     X4T32850
      IF(KFIELD.EQ.8) WRITE(OUTP,6030) KFIELD                           X4T32860
      IF(IMOUT(KFP1,ISANR,1).GT.1) WRITE(OUTP,6040) KFIELD,KFP1         X4T32870
      GO TO 270                                                         X4T32880
C-----COMBINE FIELDS TO DEFINE AVERAGE.                                 X4T32890
  150 IF(NPT.EQ.1.AND.JOPS.EQ.7) WRITE(OUTP,6120)                       X4T32900
      IF(NPT.EQ.1.AND.JOPS.EQ.8) WRITE(OUTP,6130)                       X4T32910
      ZJMULT=JMULT                                                      X4T32920
      AVER=0.0                                                          X4T32930
      DO 160 KMULT=1,JMULT                                              X4T32940
      II=IMOUT(KFIELD,ISANR,KMULT)                                      X4T32950
  160 AVER=AVER+VALUES(II)                                              X4T32960
      AVER=AVER/ZJMULT                                                  X4T32970
      IDATN=IDATN+1                                                     X4T32980
      VALUES(IDATN)=AVER                                                X4T32990
      KMOUT(KFIELD,ISANR)=IDATN                                         X4T33000
C-----SAVE DEFINITION OF FIELDS 7-8.                                    X4T33010
      IF(KFIELD.NE.7) GO TO 170                                         X4T33020
      II=IMOUT(KFIELD,ISANR,1)                                          X4T33030
      TITLE4(IDATN)=TITLE4(II)
  170 IF(JOPS.EQ.7) GO TO 310                                           X4T33050
C-----DEFINE COMBINED ERROR.                                            X4T33060
      ERRAV=0.0                                                         X4T33070
      DO 180 KMULT=1,JMULT                                              X4T33080
      II=IMOUT(KFIELD,ISANR,KMULT)                                      X4T33090
  180 ERRAV=ERRAV+ABS(VALUES(II)-AVER)                                  X4T33100
      ERRAV=ERRAV/ZJMULT                                                X4T33110
      IDATN=IDATN+1                                                     X4T33120
      VALUES(IDATN)=ERRAV                                               X4T33130
      KMOUT(KFIELD+1,ISANR)=IDATN                                       X4T33140
C-----SET FLAG TO SKIP NEXT FIELD (ERROR FIELD ALREADY DEFINED).        X4T33150
      ISKIP=1                                                           X4T33160
      GO TO 310                                                         X4T33170
C-----SELECT SMALLEST AND LARGEST IN 2 SUCCESSIVE FIELDS.               X4T33180
  190 IF(JOPS.NE.10) GO TO 260                                          X4T33190
C-----CANNOT SELECT SMALLEST AND LARGEST IF CURRENT OUTPUT FIELD IS     X4T33200
C-----8 (I.E. NO OUTPUT FIELD 9) OR IF THE NEXT FIELD IS ALREADY USED.  X4T33210
      IF(KFIELD.LT.8.AND.IMOUT(KFIELD+1,ISANR,1).LE.0) GO TO 200        X4T33220
      IF(NPT.GT.1) GO TO 290                                            X4T33230
      KFP1=KFIELD+1                                                     X4T33240
      IF(KFIELD.EQ.8) WRITE(OUTP,6060) KFIELD                           X4T33250
      IF(IMOUT(KFP1,ISANR,1).GT.1) WRITE(OUTP,6070) KFIELD,KFP1         X4T33260
      GO TO 270                                                         X4T33270
C-----SELECT SMALLEST AND LARGEST IN 2 SUCCESSIVE FIELDS.               X4T33280
  200 IF(NPT.EQ.1) WRITE(OUTP,6140)                                     X4T33290
      JL=IMOUT(KFIELD,ISANR,1)                                          TRKOV
      JH=JL                                                             TRKOV
      DO 250 KMULT=1,JMULT                                              X4T33300
      II=IMOUT(KFIELD,ISANR,KMULT)                                      X4T33310
      DX=ABS(VALUES(II))                                                X4T33320
C-----SKIP ZERO ENTRIES                                                 TRKOV
      IF(DX.EQ.0) GO TO 250                                             TRKOV
      IF(KMULT.EQ.1) GO TO 210                                          X4T33330
      IF(DX-DXMIN) 220,230,230                                          X4T33340
  210 JH=II                                                             X4T33350
      DXMAX=DX                                                          X4T33360
  220 JL=II                                                             X4T33370
      DXMIN=DX                                                          X4T33380
  230 IF(DX-DXMAX) 250,250,240                                          X4T33390
  240 JH=II                                                             X4T33400
      DXMAX=DX                                                          X4T33410
  250 CONTINUE                                                          X4T33420
C-----DEFINE INDICES TO SMALLEST AND LARGEST VALUES.                    X4T33430
      KMOUT(KFIELD,ISANR)=JL                                            X4T33440
      KMOUT(KFIELD+1,ISANR)=JH                                          X4T33450
C-----SET FLAG TO SKIP NEXT FIELD (ERROR FIELD ALREADY DEFINED).        X4T33460
      ISKIP=1                                                           X4T33470
      GO TO 310                                                         X4T33480
C                                                                       X4T33490
C     CANNOT RESOLVE MULTIPLE FIELD DEFINITION. PRINT ERROR MESSAGE     X4T33500
C     WHEN PROCESSING FIRST POINT OF TABLE.                             X4T33510
C                                                                       X4T33520
  260 IF(NPT.GT.1) GO TO 290                                            X4T33530
      WRITE(OUTP,6010) KFIELD                                           X4T33540
  270 WRITE(OUTP,6050)                                                  X4T33550
      DO KMULT=1,JMULT
        JJ=IMOUT(KFIELD,ISANR,KMULT)
        WRITE(OUTP,6020) TITLE(JJ),FLAGI(JJ)
      END DO
C-----USE FIRST INPUT FIELD FOR OUTPUT.                                 X4T33590
  290 JJ=IMOUT(KFIELD,ISANR,1)                                          X4T33600
C-----DEFINE UNIQUE INPUT FIELD INDEX TO MAP INTO OUTPUT FIELD.         X4T33610
  300 KMOUT(KFIELD,ISANR)=JJ                                            X4T33620
  310 CONTINUE                                                          X4T33630
      RETURN                                                            X4T33640
 6000 FORMAT(10X,'WARNING.....CHECK MULTIPLE FIELD DEFINITION')         X4T33650
 6010 FORMAT(10X,'WARNING.....FIELD=',I2,' UNRESOLVED MULTIPLE FIELDS') X4T33660
 6020 FORMAT(10X,A10,A1)
 6030 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT COMBINE FIELDS TO',   X4T33680
     1       10X,'DEFINE AVERAGE FOLLOWED BY ERROR (NO FIELD 9)')       X4T33690
 6040 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT COMBINE FIELDS TO',   X4T33700
     1       10X,'DEFINE AVERAGE FOLLOWED BY ERROR (FIELD=',I2,' USED)')X4T33710
 6050 FORMAT(10X,'WILL USE THE FIRST OF THE FOLLOWING COLUMN TITLES')   X4T33720
 6060 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT SELECT LARGEST AND'/  X4T33730
     1 10X,'SMALLEST VALUES (NO FIELD 9)')                              X4T33740
 6070 FORMAT(10X,'WARNING.....FIELD=',I2,' CANNOT SELECT LARGEST AND'/  X4T33750
     1 10X,'SMALLEST VALUES (FIELD=',I2,' USED)')                       X4T33760
 6080 FORMAT(10X,'OPERATION...SELECTED ',A10,A1)
 6090 FORMAT(10X,'OPERATION...NO FIELD SELECTED (ALL NEVER OUTPUT)')    X4T33780
 6100 FORMAT(10X,'OPERATION...SELECTED LARGEST')                        X4T33790
 6110 FORMAT(10X,'OPERATION...COMBINED FIELDS QUADRATICALLY')           X4T33800
 6120 FORMAT(10X,'OPERATION...DEFINED AVERAGE VALUE')                   X4T33810
 6130 FORMAT(10X,'OPERATION...DEFINED AVERAGE VALUE AND ERROR')         X4T33820
 6140 FORMAT(10X,'OPERATION...SELECTED SMALLEST AND LARGEST VALUES')    X4T33830
      END                                                               X4T33840
      SUBROUTINE REOPS                                                  X4T33850
C                                                                       X4T33860
C     PERFORM REACTION DEFINED OPERATIONS.                              X4T33870
C     (1) IF EN IS NOT DEFINED, DEFINE EN = 0.0253 EV                   X4T33880
C     (2) IF EN IS NOT DEFINED, DEFINE EN = 2.0 MEV                     X4T33890
C     (3) DATA = DATA/2 (DATA AND DATA ERROR)                           X4T33900
C     (4) DATA = DATA/(2*L+1) (DATA AND DATA ERROR)                     X4T33910
C     (5) DATA = DATA/F(0) (F(0)=ZEROTH ORDER LEGENDRE COEFFICIENT,     X4T33920
C                           DATA AND DATA ERROR).                       X4T33930
C     (6) DATA = DATA/(F(0)*(2*L+1)) (DATA AND DATA ERROR)              X4T33940
C     (8) DATA =-LOG(DATA)/THICKNESS (CONVERT TRANSMISSION TO BARNS)    ***** TRKOV
C     (9) DATA = DATA*RUTHERFORD (COULOMB) CROSS SECTION                ***** TRKOV
C                                                                       X4T33950
      INTEGER      OUTP,OTAPE
      CHARACTER*11 TITLE,UNIT,DATUM
      CHARACTER*7  ZACH7
      CHARACTER*4  TITLE4
      CHARACTER*1  FLAGI,FLAGR,ZAN,LABCM,BLANK,ZANRES,ZANRAT
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      COMMON/ZATNI/KSAN1,KSANR,KZAN(30),INPART(30),MFR(30),MTR(30),     X4T34000
     1 IRFLAG(30),KZANRS(30),MTRAT(30)                                  X4T34010
      COMMON/RNOW/ISANR                                                 X4T34020
      COMMON/HEADI/ICOM1,ICOMN,IDATN                                    X4T34030
      COMMON/CARDI/INKEY,N1,N2,ISAN,NPT                                 X4T34040
      COMMON/HEADC1/TITLE(50),TITLE4(50),UNIT(50),DATUM(50)
      COMMON/HEADC2/FLAGI(50)                                           X4T34060
      COMMON/OUTVEC/IMOUT(8,30,10),KMOUT(8,30)                          X4T34070
      COMMON/ZATNC1/FLAGR(30),ZAN(7,30),ZANRES(7,30),ZANRAT(14,30),     X4T34080
     1 LABCM(30)                                                        X4T34090
      COMMON/OUTVAL/IMUSED(50),VALUES(100),TIMEX(50),ADDX(50),          X4T34100
     1 KTFLGX(50),KUFLGX(50)                                            X4T34110
      DIMENSION EF(500),F(500)                                          X4T34120
      DATA BLANK/' '/                                                   X4T34130
      DATA ETHERM/2.53E-02/                                             X4T34140
      DATA EFISS/2.0E+06/                                               X4T34150
C-----NOTHING TO DO IF NO OPERATION DEFINED.                            X4T34160
      IF(IRFLAG(ISANR).LE.0) GO TO 110                                  X4T34170
C-----RESET ENERGY COUNT ON FIRST POINT.                                X4T34180
      IF(ISANR.EQ.1.AND.NPT.EQ.1) IEF=0                                 X4T34190
C                                                                       X4T34200
C     CHECK FOR CREATION OF AVERAGE ENERGY (THERMAL OR FISSION).        X4T34210
C                                                                       X4T34220
      IF(IRFLAG(ISANR).GT.2) GO TO 10                                   X4T34230
C-----DEFINE LOCATION OF ENERGY FIELD. NOTHING TO DO IF ENERGY IS       X4T34240
C-----DEFINED.                                                          X4T34250
      II=KMOUT(1,ISANR)                                                 X4T34260
      IF(II.GT.0) GO TO 110                                             X4T34270
C-----CREATE AVERAGE ENERGY (THERMAL OR FISSION).                       X4T34280
      IF(IRFLAG(ISANR).EQ.1) EX=ETHERM                                  X4T34290
      IF(IRFLAG(ISANR).EQ.2) EX=EFISS                                   X4T34300
      IDATN=IDATN+1                                                     X4T34310
      KMOUT(1,ISANR)=IDATN                                              X4T34320
      VALUES(IDATN)=EX                                                  X4T34330
      IF(NPT.EQ.1) WRITE(OUTP,6000) EX                                  X4T34340
      GO TO 110                                                         X4T34350
C                                                                       X4T34360
C     CHECK FOR RENORMALIZATION OF DATA AND DATA ERROR.                 X4T34370
C                                                                       X4T34380
C-----DEFINE LOCATION OF DATA FIELD. NOTHING TO DO IF DATA FIELD IS     X4T34390
C-----NOT DEFINED.                                                      X4T34400
   10 II=KMOUT(3,ISANR)                                                 X4T34410
      IF(II.LE.0) GO TO 110                                             X4T34420
      IF(IRFLAG(ISANR).NE.3) GO TO 20                                   X4T34430
C-----DEFINE DATA = DATA/2                                              X4T34440
      IF(NPT.EQ.1) WRITE(OUTP,6020)                                     X4T34450
      VALUES(II)=VALUES(II)/2.0                                         X4T34460
      II=KMOUT(4,ISANR)                                                 X4T34470
      IF(II.LE.0) GO TO 110                                             X4T34480
      VALUES(II)=VALUES(II)/2.0                                         X4T34490
      GO TO 110                                                         X4T34500
C***** CULLEN
C  20 IF(IRFLAG(ISANR).GT.6) GO TO 110                                  X4T34510
C***** CULLEN
C***** TRKOV
   20 IF(IRFLAG(ISANR).GT.6) GO TO 120
C***** TRKOV
C                                                                       X4T34520
C     RENORMALIZE LEGENDRE COEFFICIENTS.                                X4T34530
C                                                                       X4T34540
C-----DEFINE INDEX TO LEGENDRE ORDER. IF NOT DEFINED TURN OFF FLAG.     X4T34550
      JJ=KMOUT(5,ISANR)                                                 X4T34560
      IF(JJ.GT.0) GO TO 30                                              X4T34570
      IRFLAG(ISANR)=0                                                   X4T34580
      WRITE(OUTP,6010)                                                  X4T34590
      GO TO 110                                                         X4T34600
C-----DEFINE LEGENDRE ORDER.                                            X4T34610
   30 ORDERL=VALUES(JJ)                                                 X4T34620
C-----IF NOT NORMALIZED TO F(0) PERFORM NORMALIZATION.                  X4T34630
      IF(IRFLAG(ISANR).EQ.4) GO TO 90                                   X4T34640
C-----SAVE F(0) AT ALL INCIDENT ENERGIES.                               X4T34650
C-----DEFINE INDEX TO ENERGY. IF NOT DEFINED TURN OFF FLAG.             X4T34660
      KK=KMOUT(1,ISANR)                                                 X4T34670
      IF(KK.GT.0) GO TO 40                                              X4T34680
      IRFLAG(ISANR)=0                                                   X4T34690
      WRITE(OUTP,6060)                                                  X4T34700
      GO TO 110                                                         X4T34710
C-----SAVE ZEROTH ORDER ENERGY AND COEFFICIENT.                         X4T34720
   40 ENOW=VALUES(KK)                                                   X4T34730
      LORDER=ORDERL                                                     X4T34740
      IF(LORDER.NE.0) GO TO 50                                          X4T34750
      IF(IEF.LT.500) IEF=IEF+1                                          X4T34760
      EF(IEF)=ENOW                                                      X4T34770
      F(IEF)=VALUES(II)                                                 X4T34780
C-----LOOK UP ZEROTH ORDER COEFFICIENT IN ENERGY TABLE.                 X4T34790
   50 IF(IEF.LE.0) GO TO 70                                             X4T34800
      DO 60 M=1,IEF                                                     X4T34810
      IF(ABS(ENOW-EF(IEF)).LE.0.00001*EF(IEF)) GO TO 80                 X4T34820
   60 CONTINUE                                                          X4T34830
   70 WRITE(OUTP,6070) ENOW                                             X4T34840
      GO TO 110                                                         X4T34850
C-----DEFINE NORMALIZATION.                                             X4T34860
   80 IF(IRFLAG(ISANR).EQ.5) ZNORM=F(M)                                 X4T34870
      IF(IRFLAG(ISANR).EQ.6) ZNORM=F(M)*(2.0*ORDERL+1.0)                X4T34880
      IF(NPT.EQ.1.AND.IRFLAG(ISANR).EQ.5) WRITE(OUTP,6040)              X4T34890
      IF(NPT.EQ.1.AND.IRFLAG(ISANR).EQ.6) WRITE(OUTP,6050)              X4T34900
      GO TO 100                                                         X4T34910
C-----DEFINE NORMALIZATION.                                             X4T34920
   90 ZNORM=2.0*ORDERL+1.0                                              X4T34930
      IF(NPT.EQ.1) WRITE(OUTP,6030)                                     X4T34940
C-----RE-NORMALIZE DATA.                                                X4T34950
  100 VALUES(II)=VALUES(II)/ZNORM                                       X4T34960
      II=KMOUT(4,ISANR)                                                 X4T34970
      IF(II.LE.0) GO TO 110                                             X4T34980
      VALUES(II)=VALUES(II)/ZNORM                                       X4T34990
  110 GO TO 800
  120 IF(IRFLAG(ISANR).GT.8) GO TO 130
C-----CONVERT TRANSMISSION DATA TO CROSS SECTION (USING SAMPLE THICKNESS)
      IF(ICOMN .LT. 1) GO TO 126
      IF(IMUSED(ICOMN).NE.0) GO TO 124
        IMUSED(ICOMN)=1
        VALUES(ICOMN)=1
        IF(UNIT(ICOMN).NE.'ATOMS/B   ') GO TO 126
        IF(NPT.EQ.1.AND.IRFLAG(ISANR).EQ.9) WRITE(OUTP,6081)
          CALL FLOATF(DATUM(ICOMN),VALUES(ICOMN))
          GO TO 124
  123   WRITE(OUTP,6080) DATUM(ICOMN)
  124 CONTINUE
      VALUES(II)=-LOG(VALUES(II))/VALUES(ICOMN)
      GO TO 800
  126 WRITE(OUTP,6082)
      IRFLAG(ISANR)=0
C-----CONVERT RATIO-TO-RUTHERFORD INTO CROSS SECTIONS
  130 IF(IRFLAG(ISANR).GT.9) GO TO 800
C*    -- define constants (Table 1, Appendix H, ENDF-102 manual)
      IF(NPT.EQ.1.AND.IRFLAG(ISANR).EQ.9) WRITE(OUTP,6090)
      PCP=6.58211889E-16
      RAL=137.03599976
      AMUEV=9.31494013E8
      CC=299792458
C*    -- Energy and angle
      IE=KMOUT(1,ISANR)
      IA=KMOUT(5,ISANR)
      EE=VALUES(IE)
      AIN=VALUES(IA)
C*    -- Target and projectile ZA
      DO K=1,7
        ZACH7(K:K)=ZAN(K,ISANR)
      END DO
      READ (ZACH7,'(I6)') IZA
      IZAP=INPART(ISANR)
C*    -- Target and projectile mass (equal to ejectile)
      CALL ZAMASS(NMASS,IZA, AWT)
      CALL ZAMASS(NMASS,IZAP,AM1)
      AWP=AM1
C*    -- Target and projectile charge
      ZZT=IZA /1000
      ZZI=IZAP/1000
C*    -- Target spin (only relevant when equal to projectile)
      SPI=0
C*    - Calculate Rutherford (Coulomb) scattering term
      AA =AWT/AWP
      AK =2*AMUEV/(PCP*CC*1E14)**2
      AE =AMUEV/(2*RAL*RAL)
      AKA=SQRT(AK*AM1*EE)*AA/(AA+1)
      AET=ZZT*ZZI*SQRT(AE*AM1/EE)
        IF(IZAP.NE.IZA) THEN
          SIGC=AET*AET/(AKA*AKA*(1-AIN)**2)
        ELSE
          A0 =LOG((1+AIN)/(1-AIN))
          A1 =COS(AET*A0)
          A2 =A1* (-1)**NINT(2*SPI) /(2*SPI+1)
          A3 =A2+ (1+AIN*AIN)/(1-AIN*AIN)
          SIGC=A3* 2 * (AET*AET/(AKA*AKA*(1-AIN*AIN)))
        END IF
      VALUES(II)=VALUES(II)*SIGC
C-----
  800 RETURN
 6000 FORMAT(10X,'OPERATION...CREATED EN          ',1PE11.4,' EV')      X4T35010
 6010 FORMAT(10X,'WARNING...LEGENDRE ORDER (COLUMN 5) NOT DEFINED'/     X4T35020
     1 10X,'LEGENDRE COEFFICIENTS CANNOT BE RENORMALIZED')              X4T35030
 6020 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/2')                   X4T35040
 6030 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/(2*L+1)')             X4T35050
 6040 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/F(0)')                X4T35060
 6050 FORMAT(10X,'OPERATION...DEFINED DATA = DATA/(F(0)*(2*L+1))')      X4T35070
 6060 FORMAT(10X,'WARNING...ENERGY (COLUMN 1) NOT DEFINED'/             X4T35080
     1 10X,'LEGENDRE COEFFICIENTS CANNOT BE RENORMALIZED')              X4T35090
 6070 FORMAT(10X,'WARNING...NO F(0) AT ENERGY = ',1PE11.4,' EV'/        X4T35100
     1 10X,'LEGENDRE COEFFICIENTS CANNOT BE RENORMALIZED')              X4T35110
 6080 FORMAT(10X,'WARNING...UNITS IN COMMON FIELD ',A11,/
     1      ,10X,'                     EXPECTED ATOMS/B')
 6081 FORMAT(10X,'OPERATION...CONVERTED TRANSMISSION TO X.S.')
 6082 FORMAT(10X,'WARNING...SAMPLE THICKNESS FOR TRANSMISSION DATA'
     1      ,' NOT SPECIFIED')
 6090 FORMAT(10X,'OPERATION...CONVERTED RUTHERFORD RATIO TO X.S.')
      END                                                               X4T35120
      SUBROUTINE INTGER(CARD,N,I)                                       X4T35130
C                                                                       X4T35140
C     TRANSLATE FROM CHARACTERS TO INTEGER                              X4T35150
C                                                                       X4T35160
      INTEGER OUTP,OTAPE                                                X4T35170
      CHARACTER*1 CARD,DIGITS,PLUS,MINUS,BLANK,STAR,STARS               X4T35180
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      DIMENSION CARD(I),DIGITS(10),STARS(11)                            X4T35200
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/              X4T35210
      DATA STARS/' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/           X4T35220
      DATA PLUS/'+'/                                                    X4T35230
      DATA MINUS/'-'/                                                   X4T35240
      DATA BLANK/' '/                                                   X4T35250
      DATA STAR/'*'/                                                    X4T35260
C-----INITIALIZE NUMBER AND SKIP LEADING BLANKS.                        X4T35270
      N=0                                                               X4T35280
      NS=1                                                              X4T35290
      DO 10 K=1,I                                                       X4T35300
      IF(CARD(K).NE.BLANK) GO TO 20                                     X4T35310
   10 CONTINUE                                                          X4T35320
C-----FIELD IS BLANK. RETURN ZERO.                                      X4T35330
      RETURN                                                            X4T35340
C-----ALLOW LEADING + OR -.                                             X4T35350
   20 IF(CARD(K).EQ.PLUS) GO TO 30                                      X4T35360
      IF(CARD(K).NE.MINUS) GO TO 40                                     X4T35370
      NS=-1                                                             X4T35380
   30 K=K+1                                                             X4T35390
C-----ERROR IF NUMBER ENDS WITH + OR -                                  X4T35400
      IF(K.LE.I) GO TO 40                                               X4T35410
      J=I                                                               X4T35420
      GO TO 80                                                          X4T35430
C-----TRANSLATE DIGITS.                                                 X4T35440
   40 IPASS=0                                                           X4T35450
      DO 70 J=K,I                                                       X4T35460
      DO 50 M=1,10                                                      X4T35470
      IF(CARD(J).EQ.DIGITS(M)) GO TO 60                                 X4T35480
   50 CONTINUE                                                          X4T35490
C-----ERROR. CANNOT TRANSLATE CHARACTER.                                X4T35500
      GO TO 80                                                          X4T35510
   60 N=10*N+(M-1)                                                      X4T35520
   70 CONTINUE                                                          X4T35530
C-----ALL CHARACTERS TRANSLATED. DEFINE SIGNED NUMBER                   X4T35540
      N=NS*N                                                            X4T35550
      RETURN                                                            X4T35560
   80 STARS(J)=STAR                                                     X4T35570
      WRITE(OUTP,6000) CARD                                             X4T35580
      WRITE(OUTP,6010) STARS                                            X4T35590
      N=0                                                               X4T35600
      STARS(J)=BLANK                                                    X4T35610
      RETURN                                                            X4T35620
 6000 FORMAT(' SUBROUTINE INTGER....CANNOT TRANSLATE BELOW FIELD'/      X4T35630
     1 1X,11A1)                                                         X4T35640
 6010 FORMAT(1X,11A1)                                                   X4T35650
      END                                                               X4T35660
      SUBROUTINE FLOATF(FIELD,X)                                        X4T35670
C                                                                       X4T35680
C     CONVERT FROM HOLLERITH TO FLOATING POINT.                         X4T35690
C     MUST BE BETWEEN 1.0E-40 AND 1.0E+40.                              X4T35700
C                                                                       X4T35710
      INTEGER OUTP,OTAPE                                                X4T35720
      CHARACTER*1 BLANK,DOT,EXPD,EXPE,PLUS,MINUS,STAR,MESS,DIGIT,FIELD, X4T35730
     1 IFIELD,EXPDL,EXPEL
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE,NEWX4,NMASS
      DIMENSION FIELD(11),TEN(35),DIGIT(10),MESS(11)
      DATA BLANK/' '/                                                   X4T35770
      DATA DOT/'.'/                                                     X4T35780
      DATA EXPD/'D'/                                                    X4T35790
      DATA EXPE/'E'/                                                    X4T35800
      DATA EXPDL/'d'/
      DATA EXPEL/'e'/
      DATA PLUS/'+'/                                                    X4T35810
      DATA MINUS/'-'/                                                   X4T35820
      DATA STAR/'*'/                                                    X4T35830
      DATA MESS/' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/            X4T35840
      DATA DIGIT/'0','1','2','3','4','5','6','7','8','9'/               X4T35850
      DATA ZERO/0.0E+00/                                                X4T35860
      DATA TEN/                                                         X4T35870
     1 1.0E+01,1.0E+02,1.0E+03,1.0E+04,1.0E+05,                         X4T35880
     2 1.0E+06,1.0E+07,1.0E+08,1.0E+09,1.0E+10,                         X4T35890
     3 1.0E+11,1.0E+12,1.0E+13,1.0E+14,1.0E+15,                         X4T35900
     4 1.0E+16,1.0E+17,1.0E+18,1.0E+19,1.0E+20,                         X4T35910
     5 1.0E+21,1.0E+22,1.0E+23,1.0E+24,1.0E+25,                         X4T35920
     6 1.0E+26,1.0E+27,1.0E+28,1.0E+29,1.0E+30,                         X4T35930
     7 1.0E+31,1.0E+32,1.0E+33,1.0E+34,1.0E+35/
C                                                                       X4T35960
C     TRANSLATE MANTISSA.                                               X4T35970
C                                                                       X4T35980
C-----SKIP LEADING BLANK CHARACTERS.                                    X4T35990
      DO 10 I=1,11                                                      X4T36000
      IF(FIELD(I).NE.BLANK) GO TO 20                                    X4T36010
   10 CONTINUE                                                          X4T36020
C-----FIELD IS COMPLETELY BLANK. RETURN ZERO.                           X4T36030
      X=ZERO                                                            X4T36040
      GO TO 240                                                         X4T36050
C-----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT. X4T36060
   20 IN=0                                                              X4T36070
      IPT=-20                                                           X4T36080
C-----ALLOW LEADING SIGN.                                               X4T36090
      IF(FIELD(I).EQ.MINUS) GO TO 40                                    X4T36100
      IF(FIELD(I).NE.PLUS) GO TO 30                                     X4T36110
      I=I+1                                                             X4T36120
   30 XSIGN=1.0                                                         X4T36130
      GO TO 50                                                          X4T36140
   40 I=I+1                                                             X4T36150
      XSIGN=-1.0                                                        X4T36160
C-----SCAN REMAINDER OF MANTISSA.                                       X4T36170
   50 DO 90 J=I,11                                                      X4T36180
      IFIELD=FIELD(J)                                                   X4T36190
C-----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).     X4T36200
      DO 60 K=1,10                                                      X4T36210
      IF(IFIELD.EQ.DIGIT(K)) GO TO 80                                   X4T36220
   60 CONTINUE                                                          X4T36230
      IF(IFIELD.NE.DOT) GO TO 70                                        X4T36240
      IPT=0                                                             X4T36250
      GO TO 90                                                          X4T36260
C-----SCAN FOR BLANK (WHICH ENDS MANTISSA).                             X4T36270
   70 IF(IFIELD.EQ.BLANK) GO TO 100                                     X4T36280
C-----SCAN FOR E,D,- OR + (WHICH BEGINS EXPONENT).                      X4T36290
      IF(IFIELD.EQ.EXPE .OR.IFIELD.EQ.EXPD .OR.
     &   IFIELD.EQ.EXPEL.OR.IFIELD.EQ.EXPDL) GO TO 130
      IF(IFIELD.EQ.MINUS) GO TO 160                                     X4T36310
      IF(IFIELD.EQ.PLUS) GO TO 140                                      X4T36320
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 X4T36330
      GO TO 250                                                         X4T36340
C-----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT   X4T36350
C-----OFFSET.                                                           X4T36360
   80 IN=10*IN+(K-1)                                                    X4T36370
      IPT=IPT+1                                                         X4T36380
   90 CONTINUE                                                          X4T36390
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT. X4T36400
      GO TO 120                                                         X4T36410
C-----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR        X4T36420
C-----EXPONENT.                                                         X4T36430
  100 I=J+1                                                             X4T36440
      IF(I.GT.11) GO TO 120                                             X4T36450
      DO 110 J=I,11                                                     X4T36460
      IFIELD=FIELD(J)                                                   X4T36470
      IF(IFIELD.EQ.BLANK) GO TO 110                                     X4T36480
      IF(IFIELD.EQ.EXPE.OR.IFIELD.EQ.EXPD) GO TO 130                    X4T36490
      IF(IFIELD.EQ.MINUS) GO TO 160                                     X4T36500
      IF(IFIELD.EQ.PLUS) GO TO 140                                      X4T36510
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 X4T36520
      GO TO 250                                                         X4T36530
  110 CONTINUE                                                          X4T36540
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT. X4T36550
  120 X=IN                                                              X4T36560
      IF(IPT.GT.0) X=X/TEN(IPT)                                         X4T36570
      GO TO 230                                                         X4T36580
C                                                                       X4T36590
C     TRANSLATE EXPONENT.                                               X4T36600
C                                                                       X4T36610
C-----BEGINNING OF EXPONENT FOUND (X OR D). CHECK FOR FOLLOWING - OR +. X4T36620
  130 J=J+1                                                             X4T36630
      IFIELD=FIELD(J)                                                   X4T36640
      IF(IFIELD.EQ.MINUS) GO TO 160                                     X4T36650
      IF(IFIELD.NE.PLUS) GO TO 150                                      X4T36660
C----- + FOUND. INITIALIZE EXPONENT SIGN.                               X4T36670
  140 J=J+1                                                             X4T36680
  150 KSIGN=1                                                           X4T36690
      GO TO 170                                                         X4T36700
C----- - FOUND. INITIALIZE EXPONENT SIGN.                               X4T36710
  160 J=J+1                                                             X4T36720
      KSIGN=-1                                                          X4T36730
C-----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.   X4T36740
  170 KEXP=0                                                            X4T36750
      DO 200 I=J,11                                                     X4T36760
      IFIELD=FIELD(I)                                                   X4T36770
      IF(IFIELD.EQ.BLANK) GO TO 200                                     X4T36780
      DO 180 K=1,10                                                     X4T36790
      IF(IFIELD.EQ.DIGIT(K)) GO TO 190                                  X4T36800
  180 CONTINUE                                                          X4T36810
C-----ERROR. CANNOT IDENTIFY CHARACTER.                                 X4T36820
      GO TO 250                                                         X4T36830
C-----DIGIT FOUND. INCREMENT EXPONENT.                                  X4T36840
C-----OFFSET.                                                           X4T36850
  190 KEXP=10*KEXP+(K-1)                                                X4T36860
  200 CONTINUE                                                          X4T36870
C-----ENTIRE FIELD TRANSLATED (WITH EXPONENT). CONVERT TO FLOATING      X4T36880
C-----POINT.                                                            X4T36890
      X=IN                                                              X4T36900
      KEXP=KSIGN*KEXP                                                   X4T36910
      IF(IPT.GT.0) KEXP=KEXP-IPT                                        X4T36920
      IF(KEXP) 210,230,220                                              X4T36930
  210 KEXP=-KEXP                                                        X4T36940
      X=X/TEN(KEXP)                                                     X4T36950
      GO TO 230                                                         X4T36960
  220 X=X*TEN(KEXP)                                                     X4T36970
  230 X=XSIGN*X                                                         X4T36980
  240 RETURN                                                            X4T36990
  250 MESS(J)=STAR                                                      X4T37000
      WRITE(OUTP,6000) FIELD,MESS                                       X4T37010
      X=ZERO                                                            X4T37020
      MESS(J)=BLANK                                                     X4T37030
      RETURN                                                            X4T37040
 6000 FORMAT(1X,11A1/1X,11A1/                                           X4T37050
     1 ' SUBROUTINE FLOATF...ERROR IN INPUT DATA...TRANSLATED AS 0')    X4T37060
      END                                                               X4T37070
      SUBROUTINE ZAMASS(NMASS,IZA,AWT)
C-Title  : Subroutine ZAMASS
C-Purpose: Read Audi-Wapstra tables for atomic weight of nuclide IZA
C-
      CHARACTER*132 REC
      IF(NMASS.LE.0) GO TO 80
C* Try reading Audi-Wapstra file
      REWIND NMASS
C* Skip header records
   20 READ (NMASS,90) REC
      IF(REC(1:4).NE.'1N-Z') GO TO 20
      READ (NMASS,90) REC
C* Search the file for matching ZA
   40 READ (NMASS,90,END=80) REC
      READ (REC,91,ERR=40) IZ,IA,AM1,AM2
      JZA=IZ*1000+IA
      IF(JZA.NE.IZA) GO TO 40
C* Matching nuclide found
      AWT=AM1+AM2/1000000
      RETURN
C...
C... CRUDE APPROXIMATION FOR NUCLIDES NOT LISTED
C...
   80 CONTINUE
      PRINT *,'ZAMASS WARNING - Mass approximated by A for',IZA
C...
      IZ=IZA/1000
      IA=IZA-1000*IZ
      AWT=IA
      RETURN
   90 FORMAT(A132)
   91 FORMAT(9X,I5,I5,77X,F3.0,F13.0)
      END
