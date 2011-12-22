      PROGRAM PLOTC4
C-Title  : Program PLOTC4
C-Version: 86-1 (August 1986)
C-V        87-1 (June 1987)   *Software upper and lower case
C-V                            characters.
C-V      2001-3 (March 2001)  *Upgrade to compare measured
C-V                            differential data with curves from
C-V                            evaluated data files.
C-V                           *Modifications for compatibility with
C-V                            character plotting package of the
C-V                            Pre-Pro codes.
C-V      2002-5 (Jul 2002)    *Enable plotting of emitted charged
C-V                            particle spectra and gamma.
C-V                           *Enhance plotting of discrete level
C-V                            inelastic angular distributions.
C-V                           *Differentiate between isomeric states.
C-V                           *Fix bugs for compilers with dynamic 
C-V                            memory allocation
C-V      2004-1               *Fix bug plotting EXFOR only.
C-V      2006-2               *Update for consistency with DXSEND
C-V                            upgrade to provide uncertainties,
C-V                            if available.
C-Purpose: Plot experimental and evaluated nuclear data
C-Author:
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
C-A  VERSION 2001-3 IMPLEMENTED BY:
C-A  ------------------------------
C-A  Andrej Trkov
C-A  The Nuclear Data Section
C-A  International Atomic Energy Agency
C-A  P.O. Box 100
C-A  A-1400, VIENNA, AUSTRIA
C-A  EUROPE
C-
C-M
C-M  USERS' GUIDE FOR PROGRAM PLOTC4
C-M  ===============================
C-M
C-M  PURPOSE
C-M  -------
C-M  The function of the PLOTC4 program is to plot and compare
C-M  experimental data, which are in a computation format with
C-M  evaluated data in ENDF format.
C-M
C-M  This program has been designed to use the computation format
C-M  and conventions which are used by program X4TOC4 to translate
C-M  experimentally measured data from the EXFOR to a computation
C-M  format.
C-M
C-M  The evaluated data in ENDF format need some pre-processing to
C-M  reduce the complexity of format representations, which are
C-M  allowed in the general ENDF format. The codes of the Pre-Pro
C-M  sequence are used for this purpose. It is recommended that
C-M  the following are executed:
C-M    LINEAR   to convert all cross sections to linearly interpolable
C-M             form.
C-M    RECENT   to reconstruct the resonances from resonance parameters.
C-M    SIGMA1   to Doppler broaden the cross sections to room
C-M             temperature (optional but recommended when comparing
C-M             to experimental data since measurements are usually
C-M             done at room temperature).
C-M    ACTIVATE to generate cross sections for excitation into discrete
C-M             excited state (file MF10) from the data in files MF3
C-M             and MF9.
C-M    LEGEND   to convert angular distributions in arbitrary ENDF
C-M             representation into tabular ENDF representation.
C-M    SIXTAB   to convert double differential data in arbitrary ENDF
C-M             representation into tabular (law-7) ENDF representation.
C-M
C-M  GRAPHICS INTERFACE
C-M  ------------------
C-M  This program uses a simple CALCOMP like graphics interface which
C-M  requires only 3 subroutines...PLOTS, PLOT and PEN (described in
C-M  detail below). All characters and symbols are drawn using tables
C-M  of pen strokes (supplied with this program). Using this method
C-M  the program should be simple to interface to virtually any plotter
C-M  or graphics terminal and the appearance and layout of the plots
C-M  should be independent of which plotter is used.
C-M
C-M  WHAT CAN BE PLOTTED
C-M  -------------------
C-M  At the present time the following types of data can be plotted:
C-M
C-M  (1) MF =   3 - Cross sections. ENDF and EXFOR data can be
C-M                 compared (only MF = 3 ENDF data can be compared).
C-M  (2) MF =   4 - Angular distributions. ENDF and EXFOR data can be
C-M                 compared.
C-M  (3) MF =   5 - Energy distributions. ENDF and EXFOR data can be
C-M                 compared.
C-M  (4) MF =   6 - Double differential cross sections. ENDF and EXFOR
C-M                 data can be compared.
C-M  (5) MF = 154 - Legendre coefficients (only EXFOR).
C-M  (6) MF = 203 - Cross section ratios (only EXFOR).
C-M  (7) MF = 402 - Resonance parameters (only EXFOR).
C-M  (8) MF = 801 - Fission yield data (only EXFOR).
C-M
C-M  All other data will be skipped.
C-M
C-M  WHAT DATA WILL BE PLOTTED
C-M  -------------------------
C-M  Based on input options the user may specify whether the program
C-M  should plot only EXFOR data or EXFOR and ENDF data. In addition
C-M  the user may specify up to 100 ZA/MF/MT ranges to select data.
C-M
C-M  HOW MUCH DATA CAN BE PLOTTED
C-M  ----------------------------
C-M  ENDF DATA
C-M  Each section of ENDF cross sections (MF =3, any MT) may contain
C-M  any number of data points. If a section contains MXPNT or fewer
C-M  points all of the data will be in core. If the section contains
C-M  more than MXPNT points the data will be written to a scratch file
C-M  and read as needed.
C-M  Parameter MXPNT is currently set at 90000.
C-M
C-M  EXFOR DATA
C-M  Based on input parameters the user may control how much EXFOR data
C-M  will appear on each plot. This program allows the user to specify
C-M  that up to MXPGP data points may appear on each plot. If there are
C-M  more physically comparable points (e.g., same ZA, MF, MT) than
C-M  specified by the user the program will create a series of plots
C-M  each containing not more than the maximum number of points per
C-M  plot specified by the user.
C-M  Parameter MXPGP is currently set at 10000.
C-M
C-M  WHAT COMPUTERS WILL THE PROGRAM RUN ON
C-M  --------------------------------------
C-M  The program has been implemented on a variety of computers from
C-M  Cray and IBM mainframe to Sun workstations to a PC. The
C-M  program is small enough to run on virtually any computer.
C-M
C-M  The program uses a simple Calcomp-like graphics interface
C-M  (described below) and allows the user specify the physical size
C-M  of the plotter being used, by input parameters. Using these
C-M  conventions this program can be easily interfaced to virtually
C-M  any plotter.
C-M
C-M  For special considerations see the sections below on,
C-M  (1) Computer dependent coding.
C-M  (2) Plotter/graphics terminal interface.
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
C-M    6- 11   Target ZA (e.g. 26-Fe-56 =  26056)
C-M       12   Target metastable state (e.g. 26-FE-56m = M)
C-M   13- 15   MF (ENDF conventions, plus additions).
C-M   16- 19   MT (ENDF conventions, plus additions).
C-M       20   Product metastable state (e.g. 26-FE-56M = M)
C-M       21   EXFOR status
C-M       22   Center-of-mass flag (C=center-of-mass, blank=lab)
C-M   23- 94   8 data fields (each in E9.3 format). defined below.
C-M   95- 97   Identification of data fields 7 and 8
C-M            (e.g., LVL=level, HL=half-life, etc.)
C-M   98-122   Reference (first author and year)
C-M  123-127   EXFOR accession number
C-M  128-130   EXFOR sub-accession number
C-M      131   Multi-dimension table flag
C-M
C-M  DEFINITION OF 8 COMPUTATION FORMAT DATA FIELDS
C-M  In order to plot data this program assumes that the following
C-M  conventions have been used for data in the computation format.
C-M
C-M  Data Field   Definition
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
C-M  The physical significance of the first 6 data fields is defined by
C-M  the MF (data type). The physical significance of fields 7 and 8
C-M  are defined by columns 95 through 97 (e.g. LVL = level energy and
C-M  its uncertainty).
C-M
C-M  SPECIAL CONVENTIONS
C-M  The above conventions are appropriate for most types of data
C-M  in the ENDF system. In order to allow this program to plot
C-M  additional types of data the following special conventions have
C-M  been adopted:
C-M
C-M  Cross section ratios  - Field 5 = MT of denominator.
C-M  (MF = 203)              Field 6 = ZA of denominator.
C-M  Fission yield data    - Field 5 = ZA of fission fragment.
C-M  (MF = 801)              Field 6 = mass of fission fragment.
C-M  Production            - Field 6 = ZA of product.
C-M  (MT = 9000-9999)
C-M
C-M  See, remarks below on metastable state flags.
C-M
C-M  BLANK VS. ZERO DATA FIELDS
C-M  The 8 data fields on each computation format line are read as
C-M  characters and internally converted to floating point numbers
C-M  (see, subroutine FLOAT9). By testing before converting this
C-M  program can determine whether any given field is blank (no data
C-M  given) as opposed to zero. It is often important to make this
C-M  distinction, particularly for fields 7 and 8, e.g. the difference
C-M  between 0.0 indicating ground state as opposed to no data given.
C-M  The EXFOR to computation format conversion program X4TOC4 also
C-M  makes this distinction and leaves undefined fields blank (not
C-M  zero). Therefore, any data converted to the computation format
C-M  format using program X4TOC4 will follow the correct conventions.
C-M
C-M  However, if the user of this program directly codes data in the
C-M  computation format it is important to maintain this convention.
C-M  Remember---any undefined fields should be left blank and not set
C-M  to zero.
C-M
C-M  COMPUTATION FORMAT UNITS
C-M  In order to plot data this program assumes that the following
C-M  units have been used for data in the computation format.
C-M
C-M  eV         = energy
C-M  barns      = cross section
C-M  steradians = solid angle
C-M  seconds    = time
C-M  kelvin     = temperature
C-M
C-M  For example double differential data (MF=6) will be in,
C-M
C-M  barns/eV/steradian
C-M
C-M  METASTABLE STATE
C-M  The computation format allows the metastable state of the target
C-M  and residual nucleus to be identified. For ratio data metastable
C-M  state of both numerator and denominator of the ratio should be
C-M  defined.
C-M
C-M  The metastable state of the target is identified in column 12 and
C-M  the metastable state of the residual nucleus in column 20. For
C-M  ratio data the metastable state of the denominator target and
C-M  residual nucleus are identified by having the denominator ZA and
C-M  MT in the form ZA.m AND MT.m (e.g., 26056.9 and 102.1). Columns
C-M  12 and 20 may contain characters such as M, but to maintain the
C-M  eight output fields in strictly numerical form the denominator
C-M  ZA.m and MT.m will be in numerical form. The possible characters
C-M  that may appear in columns 12 or 20 and their numerical
C-M  equivalents used with ratio denominator za and mt include:
C-M
C-M  Definition    Column 12 or 20     Equivalent   Plotted as
C-M  ----------    ---------------     ----------   ----------
C-M  ground              G                0           -G
C-M  m1                  1                1           -M1
C-M  m2                  2                2           -M2
C-M  m3                  3                3           -M3
C-M  m4                  4                4           -M4
C-M  m5                  5                5           -M5
C-M  unknown             ?                6           -M?
C-M  m                   M                7           -M
C-M  more than 1         +                8           -M+
C-M  all or total        T                9           blank
C-M  all or total      blank              9           blank
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
C-M  EXFOR STATUS
C-M  Column 21 of each computation format record may contain blank
C-M  (status not specified) or one to the following characters:
C-M
C-M  Column 21   Definition
C-M  ---------   ----------
C-M     U        Unnormalized (has priority over EXFOR status and is
C-M              used to indicate that the data is not in standard
C-M              output units. y axis label will say..UNNORMALIZED..).
C-M     A        Approved by author
C-M     C        Correlated
C-M     D        Dependent
C-M     O        Outdated
C-M     P        Preliminary
C-M     R        Renormalized
C-M     S        Superceded
C-M
C-M  If data has any other EXFOR status (e.g., translated from SCISRS)
C-M  the status field will be blank.
C-M
C-M  CONTROL OF PLOTTING
C-M  -------------------
C-M  The user has control over how data in the computation format
C-M  is interpreted by this program.
C-M
C-M  Data on each plot is identified by plotting a character equivalent
C-M  of target za and metastable state (ZA), data type (MF), reaction
C-M  (MT) and residual metastable state. The ZA, MF and MT may be
C-M  interpreted in any manner that the user chooses.
C-M
C-M  This is accomplished by using three dictionaries which control
C-M  the plotting. All three of these dictionaries are distributed
C-M  with this program. Each dictionary is a simple card image file
C-M  which may be modified by the user at any time to meet specific
C-M  needs. The three dictionaries are:
C-M
C-M  (1) INTERPRETATION OF SPECIAL ZA
C-M      For all target or residual nuclei this program will use the
C-M      ENDF convention of assuming ZA = 1000*Z + A. For special
C-M      materials which do not easily fit into this scheme (e.g.,
C-M      water) the ENDF convention is to define Z =0 and to assign
C-M      a numerical equivalent for each special material. For normal
C-M      materials this program will use ZA to define the material or
C-M      isotope. For special material (Z=0) this program will use this
C-M      dictionary to define the material. As distributed this
C-M      dictionary contains all of the special materials defined in
C-M      the ENDF system. The user may code data for any special
C-M      material in the computation format and assign it a special
C-M      ZA. By adding the definition to this dictionary the user may
C-M      obtain plots on which the special material is properly
C-M      identified.
C-M
C-M  (2) INTERPRETATION OF MF
C-M      This dictionary defines the titles that will appear for each
C-M      MF read from the computation format. In addition this
C-M      dictionary allows the user to specify different titles for
C-M      the same MF and different MT ranges, e.g.:
C-M
C-M      MF =3, MT =  251 - 253 = parameters (used for MU, XI, GAMMA)
C-M      MF =3, MT = 9000       = neutron induced (used for production)
C-M      MF =3, MT = other      = cross section
C-M
C-M      If the user does not like the titles normally output by this
C-M      program it is merely necessary to modify this dictionary.
C-M
C-M  (3) INTERPRETATION OF MT
C-M      This dictionary defines the titles that will appear for each
C-M      MT read from the computation format, e.g.:
C-M
C-M      MT  =    1 = total
C-M          = 9000 = production
C-M
C-M      If the user does not like the titles normally output by this
C-M      program it is merely necessary to modify this dictionary.
C-M
C-M  Used in combination with the translation of the ZA, MF and MT serve
C-M  to identify the data being plotted. By using the dictionaries
C-M  described above the user has complete control over how ZA, MF and
C-M  MT are interpreted and as such may select any form to identify
C-M  data.
C-M
C-M  PROGRAM OPERATION
C-M  -----------------
C-M  EXFOR DATA INDEX TABLE
C-M  The entire computation format file will first be read and compared
C-M  to the requested ZA/MF/MT ranges specified by the user. If no
C-M  comparable data is found the program will terminate execution. If
C-M  comparable data is found the program will create an index table
C-M  specifying (1) ZA, (2) MF, (3) MT, (4) starting record number,
C-M  (5) ending record number, (6) number of data points with this ZA,
C-M  MF and MT. During execution this index table will be used to,
C-M  (1) select the next ZA, MF, MT to be plotted if only plotting
C-M  EXFOR data, or (2) to determine whether or not there is comparable
C-M  EXFOR data (without again searching the EXFOR data file) when
C-M  comparing EXFOR and ENDF data. Once it has been decided to plot
C-M  EXFOR data which has a given ZA, MF and MT the starting record
C-M  index is used to quickly position to the first record to read and
C-M  the ending record index is used to define when to stop reading
C-M  (instead of reading the entire computation format data file).
C-M
C-M  ONLY PLOTTING EXFOR DATA
C-M  The program will use the index table to define the ZA, MF and MT
C-M  of the next set of data to plot. Based on user input the program
C-M  will then plot either one reference (reference = author, year,
C-M  EXFOR accession and sub-accession number) per plot or all
C-M  comparable references on the same plot. The cycle of reading data
C-M  and producing plots will be continued until all data defined in
C-M  the index table have been plotted.
C-M
C-M  COMPARING ENDF/B AND EXFOR DATA
C-M  In the comparison mode the program will only plot data if there
C-M  is comparable data (same ZA, MF, MT) on both the ENDF formatted
C-M  and computation formatted files.
C-M
C-M  Based on the plotting requests (see below) the program will first
C-M  search the ENDF data file to find an acceptable section of cross
C-M  sections (MF=3,4,5,6). The program will then use the EXFOR index
C-M  to determine if there is comparable EXFOR data (same ZA, MF, MT).
C-M  If there is no comparable data the program will ignore the current
C-M  section of ENDF data and search for the next requested section
C-M  of ENDF data. The cycle of reading ENDF data and comparing to
C-M  the EXFOR index table will be continued until comparable ENDF
C-M  and EXFOR data are found. Only after the EXFOR index table shows
C-M  that the computation format file contains comparable data will
C-M  the file be read. As described above while reading EXFOR data the
C-M  program will use the starting and ending record number to quickly
C-M  position to the data to read and to stop reading when all required
C-M  data has been read.
C-M
C-M  Experimentally measured differential cross sections are seldom
C-M  measured for a particular reaction as defined in ENDF. Usually
C-M  they are "particle production cross sections", therefore
C-M  summation over several ENDF reactions is necessary. Differential
C-M  data in an ENDF file are marked available for comparison if file
C-M  MF4, 5 or 6 are found, but no action is taken until the
C-M  differential data are also found in the EXFOR file. At this
C-M  point the emitted particle, the incident particle energy, the
C-M  outgoing particle anergy and/or angle are known. At this point
C-M  the differential data retrieval routine is activated, extracting
C-M  the normalised distributions, multiplying by the appropriate
C-M  cross sections from file MF3 and summing contributions from all
C-M  reactions that produce that outgoing particle.
C-M
C-M  To simplify coding a restriction on the formats of the
C-M  differential data exists. The retrieval routine only accepts
C-M  tabular representation. ENDF files in other format representations
C-M  can be converted using codes LEGEND for the angular distributions
C-M  and SIXTAB for the double differential data.
C-M
C-M  At present the assembly of the differential data is limited to
C-M  the neutron production reactions.
C-M
C-M  ONE REFERENCE PER PLOT
C-M  When plotting one reference per plot the program will use the
C-M  EXFOR index table to determine where to start reading. After one
C-M  data point has been read the program will continue to read data
C-M  points until (1) a point is found with a different ZA, MF, MT or
C-M  reference, (2) the index table last record number indicates that
C-M  the last point has been read, or (3) the maximum number of points
C-M  per plot have been read.
C-M
C-M  WARNING...When plotting one reference per plot in order to produce
C-M  a plot the program must find at least the minimum number of points
C-M  required (see, input description below) on suceessive records.
C-M  therefore the computation format should be sorted to insure that
C-M  all data with the same ZA, MF, MT, reference appear on successive
C-M  records.
C-M
C-M  ALL COMPARABLE EXFOR DATA ON SAME PLOT
C-M  When plotting all comparable data on the same plot the program
C-M  will use the EXFOR index table to define where to start reading.
C-M  The program will continue to read data until (1) the index table
C-M  last record number indicates that the last point has been read, or
C-M  (2) the maximum number of points per plot have been read.
C-M
C-M  In this mode the EXFOR data need not be sorted by ZA, MF, MT,
C-M  reference since the EXFOR index table will define where all
C-M  comparable data are located on the computation format data file.
C-M  However, to minimize the time required to search the computation
C-M  format file the user should sort the data by ZA, MF, MT.
C-M
C-M  OPTIMIZING PROGRAM OPERATION
C-M  ----------------------------
C-M  Program operation can be optimized by minimizing the size of the
C-M  ENDF and computation formatted files. If you wish to compare
C-M  a limited number of reactions it is suggested that you first
C-M  prepare ENDF and computation formatted data files that only
C-M  contain the data which will be plotted, i.e., never use this
C-M  program to try to compare two enormous files of ENDF and EXFOR
C-M  data unless you are willing to spend a correspendingly enormous
C-M  amount of computer time. In addition the EXFOR data file
C-M  should be sorted by ZA, MF, MT, reference.
C-M
C-M  SCALING OF PLOTS
C-M  ----------------
C-M  ENDF/B AND/OR EXFOR
C-M  If only plotting EXFOR data this program will scale the X and Y
C-M  range of each plot to include only EXFOR data. If plotting EXFOR
C-M  and ENDF data the user may specify by input (input described
C-M  below) to scale plots to include all ENDF and EXFOR data or
C-M  only all ENDF data or only all EXFOR data. Although this option
C-M  may be used for special purposes to obtain special scaling it is
C-M  recommended that the user always scale plots to include all ENDF
C-M  and EXFOR data.
C-M
C-M  ENERGY RANGE
C-M  Regardless of the energy range specified by plotting requests
C-M  (see description of requests below) this program will never extend
C-M  the energy range of plots beyond where there are data. For example
C-M  to plot (n,2n) over the entire energy range where there are data
C-M  the user can specify 0.0 to 100 MeV. This program will produce
C-M  plots from threshold up to the highest energy below 100 MeV where
C-M  data are given.
C-M
C-M  COSINE RANGE
C-M  For angular (MF=4) and double differential (MF=6) distributions
C-M  where the X variable is cosine, plots will always be produced over
C-M  the cosine range -1.0 to 1.0.
C-M
C-M  INPUT LOGICAL FILE UNITS
C-M  ------------------------
C-M  Note that input instructions for PLOTC4 are on unit 4, rather than
C-M  the usual Fortran unit 5. This is so for historic reasons to allow
C-M  the program to be used on an IBM-PC where unit 5 was reserved for
C-M  keyboard interaction.
C-M
C-M  Unit   Description
C-M  ----   -----------
C-M    4    Input options              (BCD - 80 columns/record)
C-M   10    Computation formatted data (BCD - 131 columns/record)
C-M   11    ENDF formatted data        (BCD - 80 columns/record)
C-M   12    Special ZA definitions     (BCD - 80 columns/record)
C-M   14    MF definitions             (BCD - 80 columns/record)
C-M   15    MT definitions             (BCD - 80 columns/record)
C-M   17    Software characters        (BCD - 80 columns/record)
C-M
C-M  OUTPUT UNITS
C-M  ------------
C-M  Unit   Description
C-M  ----   -----------
C-M    6    Output report              (BCD - 120 columns/record)
C-M
C-M  SCRATCH UNITS
C-M  -------------
C-M  UNIT   DESCRIPTION
C-M  ----   -----------
C-M   18    ENDF data paging unit      (Binary - 6000 words/record)
C-M
C-M  INPUT CARDS
C-M  -----------
C-M  The user must input at least teo cards to specify plotting options.
C-M  In the simplest case this first card can be completely blank (see
C-M  Example input No. 1 below). To select data by ZA/MF/MT/Incident
C-M  energy range the user may input up to 100 additional cards.
C-M
C-M  Card  Columns  Format  Description
C-M  ----  -------  ------  -----------
C-M    1     1-10     F10   Paper offset along paper height [inch].
C-M         11-20     F10   Total paper height (default 12.5) [inch]
C-M         21-30     F10   Paper offset along paper width.
C-M         31-40     F10   Total paper width (default 10) [inch]
C-M    2     1- 5     I5    Compare EXFOR data to ENDF
C-M                         0 = No
C-M                         1 = Yes
C-M                         2 = Yes (identify ENDF points by plotting
C-M                                  a small diamond round each point).
C-M                         NOTE: If comparing data plots will only
C-M                         be produced if comparable data is found
C-M                         on both the ENDF and computation format
C-M                         files.
C-M          6-10     I5    All comparable exfor data on same plot
C-M                          0 = No
C-M                         <0 = Yes, each reference on a seperate plot
C-M                         >O = Yes, the value entered is the maximum
C-M                              number of references per plot (current
C-M                              upper limit=48).
C-M         11-15     I5    Plot scaling
C-M                         0 = ENDF and EXFOR (recommended)
C-M                         1 = ENDF
C-M                         2 = EXFOR
C-M                         (automatically set to 2 if not comparing)
C-M         16-20     I5    Plot X error bars (energy, cosine, etc.)
C-M                         0 = No, 1 = Yes
C-M         21-25     I5    Plot Y error bars (cross section, etc.)
C-M                         0 = No, 1 = Yes
C-M         26-30     I5    Identify all references by symbol
C-M                         0 = No, 1 = Yes
C-M                         (0 = If only one reference on plot do not
C-M                         plot box and reference symbol around each
C-M                         data point...recommended).
C-M         31-35     I5    Allow variable E2 on same plot
C-M                         0 = No, 1 = Yes
C-M                         (Normally only data with same ZA/MF/MT/E2
C-M                         will appear on same plot. 1 = collect data
C-M                         from 1 reference for same ZA/MF/MT and
C-M                         a number of values of E2. Identify data on
C-M                         plot by each value of E2).
C-M         36-40     I5    Minimum EXFOR points per plot
C-M                         (If there are fewer comparable EXFOR points
C-M                         they will be skipped...default 8, minimum
C-M                         valid entry 2).
C-M         41-45     I5    Maximum number of EXFOR points per plot
C-M                         (Minimum defined by columns 36-40 up to
C-M                         MXPGP...DEFAULT MXPGP1000).
C-M                         Currently parameter MXPGP is set to 10000.
C-M                         HINT: The limit applies to the total number
C-M                               of points extracted from the EXFOR
C-M                               file. If plots are defined by
C-M                               explicitly requested ranges (see the
C-M                               next input line) and all comparable
C-M                               points are to be displayed on the
C-M                               same plot, use the default value.
C-M         46-50     I5    Grid type
C-M                         = 0 - Tick marks on each axis..recommended.
C-M                         = 1 - Full grid.
C-M         51-55     I5    Plot size
C-M                         = 0 - Full size plots.
C-M                         = 1 - Half size (4 sub-plots per plot).
C-M         56-70   3A4,A3  ENDF library identification.
C-M                         e.g., ENDF/B-V (only used if comparing).
C-M  3-N     1- 7     I7    Lower ZA limit
C-M          8-11     I4    Lower MF limit
C-M         12-15     I4    Lower MT limit
C-M         16-26   E11.4   Lower incident energy limit (eV)
C-M         27-33     I7    Upper ZA limit
C-M         34-37     I4    Upper MF limit
C-M         38-41     I4    Upper MT limit
C-M         42-52   E11.4   Upper incident energy limit (eV)
C-M         53-55     I3    Plot scales (ENDF convention):
C-M                          2  Linear abscisa and ordinate
C-M                          3  Logarithmic abscisa and linear ordinate
C-M                          4  Linear abscisa and logarithmic ordinate
C-M                          5  Logarithmic abscisa and ordinate
C-M         56-66   E11.4   Smearing parameter for elastic and
C-M                         discrete level inelastic scattering energy
C-M                         distributions given in terms of a
C-M                         fractional energy increment at which a
C-M                         gaussian curve is half maximum. This is
C-M                         only used to simulate resolution broadening
C-M                         of the distributions, which are ideally
C-M                         delta-functions (e.g. elastic and discrete
C-M                         inelastic scattering reactions when
C-M                         assembling total neutron emission
C-M                         energy distribution).
C-M
C-M * The request list is terminated by a blank line. The remainder
C-M   of the input file will be ignored.
C-M
C-M * There may be up to 100 ZA/MF/MT/Energy range requests. If there
C-M   are more than 100 requests only the first 100 will be used.
C-M * Each request independently specifies a range of ZA/MF/MT/Energy
C-M   to be plotted.
C-M * For each set of data, ZA must be between the lower and upper ZA
C-M   limit, MF must be between the lower and upper MF limit, MT must
C-M   be between the lower and upper mt limit and the incident energy
C-M   must be between the lower and upper energy limit.
C-M * e.g., Z=1 to 90000, MF=3 to 3, MT=1 TO 1, E=0.0 to 2.0E+7 eV
C-M   will select all ZA between 1 to 90000 which have MF=3 and MT=1
C-M   and data points with incident energy between 0 and 20 MeV.
C-M * If there are no request cards all data will be plotted.
C-M
C-M  EXAMPLE INPUT NO. 1
C-M  ...................
C-M  To plot all EXFOR data without comparison to ENDF and without
C-M  error bars the user need only enter a single blank card, or,
C-M
C-M      0   13.9947    0      10.3
C-M      0    0    0    0    0    0    0    0    0    0    0
C-M
C-M  EXAMPLE INPUT NO. 2
C-M  ...................
C-M  Plot all EXFOR data 1 reference per plot with X and Y error bars.
C-M  Do not plot data unless there are 8 or more points. Plot a full
C-M  grid. Input the following 1 card,
C-M
C-M      0    0    0    1    1    0    1    8    0    1    0
C-M
C-M  NOTE: This is a good set of input parameters to use in order to
C-M  produce all possible plots of all EXFOR data translated from a
C-M  given EXFOR tape. It is recommended to specify 8 as the minimum
C-M  number of points per plot in order to avoid obtaining a large
C-M  number of plots each containing only 1 or 2 data points.
C-M
C-M  EXAMPLE INPUT NO. 3
C-M  ...................
C-M  Plot Co-59 (n,2n) ENDF cross sections and all comparable
C-M  EXFOR data on the same plot with cross section error bars, one
C-M  plot from 0.0 eV (scaled to threshold) to 20.0 MeV and a second
C-M  plot from 12.0 to 15.0 MeV. Do not plot data unless there are at
C-M  least 8 experimental data points. Only tick marks on axis. ENDL84
C-M  is the identification for the ENDF library. Input the following
C-M  3 cards.
C-M
C-M      1    1    0    0    1    0    0    8    0    0    0  ENDL84
C-M    27059   3  16 0.00000+ 0  27059   3  16 2.00000+ 7
C-M    27059   3  16 1.20000+ 7  27059   3  16 1.50000+ 7
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
C-M  (3) A copy of the jcl deck you used to execute the program
C-M  (4) A copy of the 3 translation dictionaries you are using
C-M  (5) A copy of the computation format data you using
C-M  (6) A copy of the output report from the program.
C-M
C-M  Also send copies of any plots which you have obtained as output
C-M  from this program, if possible and/or applicable.
C-M
C-M  Without all of this information it is impossible to exactly
C-M  simulate the problem that you ran and to determine the source
C-M  of your problem.
C-M
C***** RETRIEVAL OF DIFFERENTIAL AND DOUBLE DIFFERENTIAL DATA **********
C-M
C-M  RETRIEVAL OF DIFFERENTIAL AND DOUBLE DIFFERENTIAL DATA
C-M  ------------------------------------------------------
C-M
C-M  Retrieval of differential and double differential data is done
C-M  through the DXSEND package, which is provided as a separate
C-M  module. It is called by:
C-M
C-M     CALL DXSELM(LEF,NUC,ZEL,FRC,ZAP,MF0,MT0,KEA,EINC,PAR,EPS
C-M    1           ,ENR,DXS,RWO,NEN,MEN,MRW,LTT,ELV)
C-M
C-M  The DXSELM module reads an ENDF file and extract cross sections
C-M  (KEA=0), differential cross section (angular distributions KEA=1
C-M  or energy spectra KEA=2, parameter PAR < -2) and double
C-M  differential cross sections (correlated energy/angle distributions
C-M  with the same conventions for KEA. Parameter PAR is the requested
C-M  outgoing particle energy when the correlated angular distribution
C-M  are requested. Similarly, PAR is the cosine of the scattering
C-M  angle when the spectrum at a particular scattering angle is
C-M  requested. Differential cross sections are output in the Lab
C-M  co-ordinate system.
C-M    If a special MT number is requested (for example, MT=5 for
C-M  particle emission where particle is defined by its ZA designation
C-M  in ZAP), the retrieval is done recursively for all neutron emission
C-M  reactions and all contributions are summed.
C-M    Although the DXSELM routine allows reconstruction of elemental
C-M  data from the isotopic components, this feature cannot be
C-M  implemented in PLOTC4.
C-M
C-M  Formal parameters are:
C-M  LEF  - File unit number from which the ENDF file is read.
C-M  ZA0  - Requested nuclide identification number. If ZA>0 it is
C-M         given in terms of Z*1000+A+LIS0/10 where Z is the atomic
C-M         number, A the mass number and LIS0 the metastable state
C-M         number. When ZA<0 it implies the ENDF material MAT number.
C-M  ZAP0 - Outgoing particle ZA designation (ZAP0=1 for neutrons).
C-M  MF0  - Requested MF number according to the ENDF conventions.
C-M  MT0  - Requested reaction MT number. Broadly this follows the
C-M         ENDF conventions.
C-M  KEA  - Control flag to select retrieval of cross section (KEA=0)
C-M         angular distributions (KEA=1) of energy spectra (KEA=2).
C-M  EINC - Incident particle energy (eV).
C-M  PAR  - Fixed parameter when requesting differential data:
C-M         KEA=1, PAR is the requested outgoing particle energy.
C-M                A value PAR <= -2 implies integrated distribution
C-M                over all angles.
C-M         KEA=2, PAR is the requested scattering angle (cosine).
C-M                A value PAR <= -2 implies angle integrated energy
C-M                distribution.
C-M  EPS  - Resolution broadening parameter is used for the two-body
C-M         scattering reactions like the elastic and discrete inelastic
C-M         cross sections  where in principle the energy distribution
C-M         is a delta function. For such reactions the energy
C-M         distribution is displayed as a Gaussian distribution where
C-M         EPS the fractional half-width at half-maximum. Such
C-M         representation is convenient for comparison with measured
C-M         data.
C-M  ENR  - Argument vector of the assembled output cross section.
C-M  DXS  - Function vector of the assembled output cross section.
C-M  RWO  - Work array of length MRW.
C-M  NEN  - Number of points in the assembled output cross section
C-M         vector.
C-M  MEN  - Available size of ENR and DXS arrays.
C-M  MRW  - Available size of the RWO work array.
C-M  ELV  - Energy of the requested discrete inelastic level when
C-M         requesting angular distributions (if applicable).
C-M         completed successfully.
C-M
C-M  External routines called:
C-M   DXSELM,DXSEND,DXSEN1,SKIPSC,FINDMT,RDTAB1,RDTAB2,RDLIST,
C-M   FINT2D,YTGEOU,FNGAUS,FYTG2D,UNIGRD,FITGRD
C-M
C***** RETRIEVAL OF DIFFERENTIAL AND DOUBLE DIFFERENTIAL DATA **********
C***** COMPUTER DEPENDENT CODING ***************************************
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
C***** COMPUTER DEPENDENT CODING ***************************************
C***** PLOTTER/GRAPHICS TERMINAL INTERFACE *****************************
C-M
C-M   PLOTTER/GRAPHICS TERMINAL INTERFACE
C-M   -----------------------------------
C-M
C-M   This program uses a simple Calcomp-like interface involving
C-M   only 3 subroutines,
C-M
C-M   PLOTS(BUF,IBUF,IPLOT) - Initialize plotter
C-M         BUF    - Plotter buffer
C-M         IBUF   - Size of plotting buffer (5000 words used)
C-M         IPLOT  - Plotter unit (16)...usually a dummy unit
C-M
C-M   PLOT(X,Y,IPEN)        - Draw or move from last location to (X,Y),
C-M                           end of current plot or end of plotting.
C-M         IPEN =   2 - Draw
C-M              =   3 - Move
C-M              =  -1 - End of current plot...advance by X,Y
C-M              = 999 - End of plotting.
C-M
C-M   PEN(ICOL)             - Select color.
C-M        ICOL  - Color = 1 to N (N = Any positive integer)
C-M
C-M  In order to interface this program for use on any plotter which
C-M  does not use the above conventions it is merely necessary for the
C-M  the user to write 3 subroutines with the names PLOTS, PLOT and PEN
C-M  with the subroutine arguments described above and to then call the
C-M  local equivalent routines.
C-M
C-M  AVAILABLE PLOTTER INTERFACES
C-M  ----------------------------
C-M  This program has available plotter interfaces to operate as
C-M  follows,
C-M  (1) Mainframe - hardcopy plots in black and white.
C-M  (2) Mainframe - screen plots in 7 colors on IBM graphics terminal.
C-M  (3) PC        - hardcopy plots in 6 colors on a Hewlett-Packard
C-M                  7475a plotter.
C-M
C-M  Contact the author to obtain copies of any of the above plotter
C-M  interfaces.
C-M
C-M  COLOR PLOTS
C-M  -----------
C-M  To select plotting colors subroutine PEN (described above) is used
C-M  to select one of the available colors. When running on a mainframe
C-M  using an IBM graphics terminal or on a PC using a Hewlett-
C-M  Packard plotter the graphics interface (described above) will
C-M  produce color plots.
C-M
C-M  BLACK AND WHITE PLOTS
C-M  ---------------------
C-M  When producing black and white hardcopy on a mainframe the user
C-M  should add a dummy subroutine PEN to the end of the program to
C-M  ignore attempts to change color. Add the following subroutine:
C-M
C-M  SUBROUTINE PEN(IPEN)
C-M  RETURN
C-M  END
C-M
C-M  CHARACTER SET
C-M  -------------
C-M  This program uses computer and plotter device independent software
C-M  characters. This program comes with a file that defines the pen
C-M  strokes required to draw all characters on the keyboard (upper
C-M  and lower case characters, numbers, etc.) plus an alternate set of
C-M  all upper and lower case greek characters and additional special
C-M  symbols.
C-M
C-M  The software character table contains X and Y and pen positions to
C-M  draw each character. If you wish to draw any additional characters
C-M  or to modify the font of the existing characters you need only
C-M  modify this table.
C-M
C-M  CONTROL CHARACTERS
C-M  In the software character table all characters to be plotted will
C-M  have pen position = 2 (draw) or = 3 (move). In addition the table
C-M  currently contains 4 control characters:
C-M
C-M  PEN POSITION = 0
C-M  Shift the next printed character by X and Y. 3 control characters
C-M  are presently included in the software character table to allow
C-M  shifting.
C-M
C-M  {   = Shift up (for superscripts..............X= 0.0, Y= 0.5)
C-M  }   = Shift down (for subscripts..............X= 0.0, Y=-0.5)
C-M  \   = Shift left 1 character (for backspace...X=-1.0, Y= 0.0)
C-M
C-M  PEN POSITION =-1
C-M  Select the next printed character from the alternate character
C-M  set. At present this control character is,
C-M
C-M  ]   = Switch to alternate character set
C-M
C-M  These 4 control characters are only defined by the value of the
C-M  pen position in the software character table (i.e., they are not
C-M  hard wired into this program). As such by modifying the software
C-M  character table the user has the option of defining any control
C-M  characters to meet specific needs.
C-M
C-M  These characters may be used in character strings to produce
C-M  special effects. For example, to plot subscript 5, B, superscript
C-M  10 use the string,
C-M
C-M  }5B{1{0
C-M
C-M  To plot B, subscript 5 and superscript 10 with the 5 directly
C-M  below the 1 of the 10 use the string,
C-M
C-M  B}5\{1{0
C-M
C-M  To plot upper case greek gamma followed by the words "Total Width"
C-M  use the string,
C-M
C-M  ]G Total Width
C-M
C-M  NOTE: When these control characters are used they only effect the
C-M  next 1 printed character (see, above example of plotting super-
C-M  script 10 where the shift up control character was used before the
C-M  1 and then again before the 0).
C-M
C-M  If these 4 control characters are not available on your computer
C-M  you can modify the software character table to use any other 4
C-M  characters that you do not normally use in character strings (for
C-M  details see the software character table).
C-M
C-M  STANDARD/ALTERNATE CHARACTER SETS
C-M  The software character table contains 2 sets of characters, which
C-M  are a standard set (all characters on an IBM keyboard) and an
C-M  alternate set (upper and lower case greek characters and special
C-M  characters). To draw a character from the alternate character set
C-M  put a vertical stroke character (]) before a character (see the
C-M  above example and the software character table for details). This
C-M  control character will only effect the next 1 plotted character.
C-M
C-M  SUB AND SUPER SCRIPTS
C-M  To draw subscript preceed a character by }. To draw superscript
C-M  preceed a character by { (see the above example and the software
C-M  character table for details). These control character will only
C-M  effect the next 1 plotted character.
C-M
C-M  BACKSPACING
C-M  To backspace one character preceed a character by \ (see, the
C-M  above example and the software character table for details). This
C-M  control character will perform a true backspace and will effect
C-M  all following characters in the same character string.
C-M
C***** PLOTTER/GRAPHICS TERMINAL INTERFACE *****************************
      PARAMETER (MXPNT=90000)
      PARAMETER (MXPGP=10000)
      PARAMETER (MXRW=200000,MXIS=10)
      INTEGER OUTP
      CHARACTER*40 P4INP,P4LST,C4DAT,EDAT,ZADEF,MFDEF,MTDEF,CHTAB
      CHARACTER*4 VERSES,VERSEZ
      CHARACTER*4 REFS,REF1,REFX,ZABCD,MSTAT1,MSTAT2,LIBNAM,BLANK
      CHARACTER*1 LABCM,LAB,STATUS,STAT,MSTAR1,MSTAR2,BLANK1
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
      COMMON/UNITT/NTAPE1,NTAPE2,NTAPE3
      COMMON/THICKY/ITHICK,THICK
      COMMON/INPARM/MINNIE,MAXIE
      COMMON/MODEMY/MYMODE
      COMMON/PAGEXY/XPAGE(MXPNT),YPAGE(MXPNT),N2,IBASE,ITOP,ISCR
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/XYREAL/XREAL(2),YREAL(2)
      COMMON/RATZAC/MSTAR1,MSTAR2
      COMMON/GRIDDY/MYGRID
      COMMON/LIBI/ILIB
      COMMON/LIBC/LIBNAM(4)
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,EINC,AWR
      COMMON/WHEREC/ZABCD(4),MSTAT1,MSTAT2
      COMMON/WHERE2/IZABCD
      COMMON/WAYS/IWAY(2)
      COMMON/EXFOR/XEX(MXPGP),DXEX(MXPGP),YEX(MXPGP),DYEX(MXPGP)
     1,NREF(MXPGP),E2(MXPGP),IEX
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP
      COMMON/RATZA/IZARAT,MTRAT,MFIN
      COMMON/REFERC/REFS(9,48),REF1(9)
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
      COMMON/SYSSTA/LABCM,STATUS
      COMMON/DOUBL/IDOUB,FIELD4(4)
      COMMON/XLIMIT/MPT
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC
      COMMON/PLOTN/NUMPLT,ADVANC
      COMMON/VERSC/VERSES(5,4)
      COMMON/VERSI/NVERSE(4)
      COMMON/EPSMF6/EP6
      DIMENSION REFX(9),FIELDI(8)
      DIMENSION VERSEZ(5,4)
      DIMENSION RWO(MXRW),ZEL(MXIS),FRC(MXIS),ZPAGE(MXPNT)
      DATA REFX/'Othe','rs  ',' ',' ',' ',' ',' ',' ',' '/
      DATA BLANK/'    '/
      DATA BLANK1/' '/
C ----DEFINE ALL I/O UNITS.
      DATA P4INP /'PLOTC4.INP'/
     1     P4LST /'PLOTC4.LST'/
     1     C4DAT /'C4.DAT'/
     2     EDAT  /'ENDF.DAT'/
     3     ZADEF /'ENDFZA.DAT'/
     4     MFDEF /'ENDFMF.DAT'/
     5     MTDEF /'ENDFMT.DAT'/
     6     CHTAB /'PLOT.CHR'/
C ----CHANGE THIS DATA STATEMENT TO IDENTIFY UPDATED VERSIONS
      DATA VERSEZ/'   P','rogr','am P','LOTC','4   ',
     1 '   (','Vers','ion ','2001','-3) ',
     2 'Nucl','ear ','Data',' Sec','tion',
     3 '    ','IAEA','  Vi','enna','    '/
      DO 14 I=1,4
      NVERSE(I)=20
      DO 12 J=1,5
      VERSES(J,I)=VERSEZ(J,I)
   12 CONTINUE
   14 CONTINUE
      IDX=0
      INP=4
      OUTP=7
      ITAPE1=10
      ITAPE2=11
      NTAPE1=12
      NTAPE2=14
      NTAPE3=15
      ISCR=18
      ISYM=17
      OPEN (UNIT=INP   ,FILE=P4INP,STATUS='OLD')
      OPEN (UNIT=ITAPE1,FILE=C4DAT,STATUS='OLD')
      OPEN (UNIT=ITAPE2,FILE=EDAT ,STATUS='UNKNOWN')
      OPEN (UNIT=NTAPE1,FILE=ZADEF,STATUS='OLD')
      OPEN (UNIT=NTAPE2,FILE=MFDEF,STATUS='OLD')
      OPEN (UNIT=NTAPE3,FILE=MTDEF,STATUS='OLD')
      OPEN (UNIT=ISYM  ,FILE=CHTAB,STATUS='OLD')
      OPEN (UNIT=ISCR  ,FILE='SCR.',FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN (UNIT=OUTP  ,FILE=P4LST,STATUS='UNKNOWN')
C ----LOAD SOFTWARE CHARACTERS.
      CALL SYMIN
C ----DEFINE LINE THICKNESS PARAMETERS.
      ITHICK=0
      THICK=0.002
C ----INITIALIZE FLAG TO SHOW PROGRAM IS RUNNING ON MAINFRAME (WILL BE
      IBMPC= 0
C ----INITIALIZE NUMBER OF PLOTS GENERATED.
      NUMPLT=0
C ----IDENTIFY PROGRAM.
      WRITE(OUTP,6000) (VERSES(I,2),I=2,5)
C ----READ ZA/MF/MT TRANSLATION TABLES.
      CALL ZAMFMT
C ----READ ALL INPUT PARAMETERS AND INITIALIZE PLOTTER.
      CALL READIN
C ----INITIALIZE ENDF/B AND EXFOR POINT COUNTS AND ENDF/B MAT NUMBER.
      N2=0
      IEX=0
      MAT=0
C ----INITIALISE LAST REFERENCE AS "OTHERS".
      DO 10 J=1,9
   10 REFS(J,MAXREF+1)=REFX(J)
C ----PRINT TITLE FOR OUTPUT REPORT.
      WRITE(OUTP,6040)
C
C     READ ENDF/B AND/OR EXFOR DATA AND MAKE PLOTS.
C
C ----IF COMPARISON MODE READ NEXT SECTION OF ENDF/B DATA.
c...
c...      print *,'mymode',mymode
c...
   20 IF(MYMODE.LT.2) GO TO 30
      IEVEND=0
      CALL GETEV(IEVEND)
C...
C...  PRINT *,'MF,MT,IEVEND',MF,MT,IEVEND
C...
C ----END OF RUN IF END OF ENDF/B DATA.
      IF(IEVEND.NE.0) GO TO 80
C ----READ COMPARABLE EXFOR DATA.
   30 IEXEND=0
      CALL GETEX(IEXEND)
C...
C...      PRINT *,'MF,MT,IEXEND,IEX',MF,MT,IEXEND,IEX
C...
C ----NO PLOT IF LESS THAN MINIMUM NUMBER OF EXFOR POINTS.
      IF(IEX.LT.MINNIE) GO TO 60
C ----IF COMPARING TO ENDF/B SET ALL METASTABLE STATE FLAGS TO BLANK.
      IF(MYMODE.LT.2) GO TO 40
C...  MSTAT1=BLANK
C...  MSTAT2=BLANK
      MSTAR1=BLANK1
      MSTAR2=BLANK1
C ----PREPARE ENDF DOUBLE DIFFERENTIAL DATA FOR COMPARISON
      IF(MF.LT.4 .OR. MF.GT.6) GO TO 40
      N2=0
      PAR=-2
C* Outgoing particle ZA designation
      IF(MFIN.EQ.4) THEN
        IF(MT.EQ.  2                ) IZARAT=   1
        IF(MT.GT. 50 .AND. MT.LT. 91) IZARAT=   1
        IF(MT.GE.600 .AND. MT.LT.649) IZARAT=1001
        IF(MT.GE.800 .AND. MT.LT.849) IZARAT=2004
      END IF
      ZAP=IZARAT
C* Select plot as a function of outgoing particle angle or energy
      IF     (MFIN.EQ.4) THEN
        KEA=1
      ELSE IF(MFIN.EQ.5) THEN
        KEA=2
      ELSE IF(MFIN.EQ.6) THEN
        IF(IDOUB.EQ.2) THEN
          KEA=2
          DEG=ACOS(FIELD4(1))*180/3.1415926
          PAR=DEG
        ELSE
          KEA=1
          PAR=FIELD4(3)
        END IF
      END IF
C* Energy level (if applicable)
      ELV=E2T(1)
C* Unit LTT is for messages from DXSEND (set zero to suppress)
      LTT=6
      ZAA=IZA
      MPT=MXPNT
      MTX=MT
      IF(MT.EQ.9000) MTX=5
C... Temporary diagnostic printout
      PRINT *,'mfin,mf,mt,zap,kea',MFIN,MF,MTX,NINT(ZAP),KEA
      NUC=0
      ZEL(1)=ZAA
      CALL DXSELM(ITAPE2,NUC,ZEL,FRC,ZAP,MFIN,MTX,KEA,EINC,PAR,EP6
     1           ,XPAGE,YPAGE,ZPAGE,RWO,N2,MPT,MXRW,LTT,ELV)
C... Temporary diagnostic printout
      PRINT *,'Zaa,Zap,MTX,Ein,Par,Kea,Np'
     1        ,IFIX(ZAA),NINT(ZAP),MTX,EINC,PAR,IDOUB,N2
C...  IF(IDOUB.EQ.1) THEN
C...      PRINT *,(XPAGE(J),J=1,N2)
C...      PRINT *,(YPAGE(J),J=1,N2)
C...  END IF
C...
C* Skip the plot if no comparable data to plot
      IF(N2.EQ.0) GO TO 60
      IBASE=0
      ITOP=N2
C ----DEFINE HOLLERITH EQUIVALENT OF ZA.
   40 CALL ZAHOL(IZA,MSTAT1,ZABCD,IZABCD)
C ----SAVE INPUT MF AND DEFINE INTERNAL MF. NO PLOT IF MF NOT IN LEGAL
C ----RANGE.
      IF(MF.EQ. 10) MF=3
      IF(MF.EQ.203) MF=3
      IF(MF.EQ.402) MF=3
      IF(MF.EQ.154) MF=7
      IF(MF.EQ.801) MF=8
      IF(MF.LT.1.OR.MF.GT.10) GO TO 60
C ----PRINT DESCRIPTION OF DATA TO BE PLOTTED.
      IDX=IDX+1
      IZAP=NINT(ZAP)
      IF     (MF.EQ.3  .OR. MF.EQ.10) THEN
        WRITE(OUTP,6050) (ZABCD(I),I=1,3),IZAP,MFIN,MT,N2,IEX,IREF,
     1                    IDX
      ELSE IF(MFIN.EQ.4             ) THEN
        IF((MT.GT. 50 .AND. MT.LT .91)  .OR.
     1     (MT.GE.600 .AND. MT.LT.649)  .OR.
     1     (MT.GE.800 .AND. MT.LT.849)) THEN
        WRITE(OUTP,6053) (ZABCD(I),I=1,3),IZAP,MFIN,MT,N2,IEX,IREF,
     1                    EINC,ELV,IDX
        ELSE
        WRITE(OUTP,6051) (ZABCD(I),I=1,3),IZAP,MFIN,MT,N2,IEX,IREF,
     1                    EINC,IDX
        END IF
      ELSE IF(MFIN.EQ.6             ) THEN
        IF(IDOUB.EQ.2) THEN
        WRITE(OUTP,6052) (ZABCD(I),I=1,3),IZAP,MFIN,MT,N2,IEX,IREF,
     1                    EINC,DEG,IDX
        ELSE
        WRITE(OUTP,6053) (ZABCD(I),I=1,3),IZAP,MFIN,MT,N2,IEX,IREF,
     1                    EINC,PAR,IDX
        END IF
      ELSE
        WRITE(OUTP,6051) (ZABCD(I),I=1,3),IZAP,MFIN,MT,N2,IEX,IREF,
     1                    EINC,IDX
      END IF
C
C     SET UP LOOP OVER REQUESTS. IF NO REQUESTS SET UP ONCE THROUGH LOOP
C
      DO 50 IGET=1,NGET
C ----SELECT POINTS TO APPEAR ON NEXT PLOT.
      CALL SCALER
C ----NO PLOT IF LESS THAN MINIMUM NUMBER OF POINTS.
      IF(MPT.LT.MINNIE) GO TO 50
C ----SELECT AXIS UNITS.
      CALL UNITED
C ----PLOT BORDER AND ALL AXIS LABELS.
      CALL BORDER
C ----PLOT X AND Y AXIS GRID
      IF(MYGRID.EQ.0) CALL GRID0
      IF(MYGRID.NE.0) CALL GRID1
C ----PLOT ENDF/B DATA (IF ANY).
      IF(MYMODE.GE.2) CALL EVALP
C ----PLOT EXPERIMENTAL DATA.
      CALL EXFORP
C ----END OF PLOT. ADVANCE TO NEXT PLOTTING AREA.
      CALL NXTPLT
   50 CONTINUE
C ----END OF EXFOR DATA = END OF PLOTTING.
   60 IF(IEXEND.EQ.2) GO TO 80
C ----IF ALL EXFOR DATA HAS NOT YET BEEN READ CONTINUE READING EXFOR
C ----(IF NO COMPARISON...NO NEED TO READ ENDF/B)
C ----(IF COMPARISON......THERE MAY BE MORE EXFOR DATA TO COMPARE TO
C ----CURRENT SECTION OF ENDF/B DATA).
      IF(IEXEND) 30,30,70
C ----IF COMPARISON MODE CONTINUE READING ENDF/B DATA.
   70 IF(MYMODE.GE.2) GO TO 20
C ----NO COMPARISON. IF PLOTTING ALL REFERENCES TOGETHER CONTINUE
C ----READING UNTIL NO MORE DATA. OTHERWISE END OF RUN.
      IF(MYMODE.EQ.1.AND.IEXEND.LT.2) GO TO 30
C ----END OF RUN.
   80 CALL ENDPLOTS
      WRITE(OUTP,6060) NUMPLT
      STOP
 6000 FORMAT(' PLOT ENDF/B AND/OR EXFOR DATA (PLOTC4 ',4A4/
     1 1X,72('='))
 6040 FORMAT(1X,72('=')/' PROCESSING'/1X,75('=')/
     4 ' MATERIAL     ZAP  MF   MT  EVAL. EXPR. ',
     4 'EXPR.    E-INC ANG-OUT ELV/E-OUT IDX'/
     5 '                            PNTS. PNTS. ',
     5 ' REF.       EV     DEG        EV    '/
     6 1X,75('='))
 6050 FORMAT(1X,3A4,I4,I4,I5,3I6,                28X,I4)
 6051 FORMAT(1X,3A4,I4,I4,I5,3I6,1PE10.3,        18X,I4)
 6052 FORMAT(1X,3A4,I4,I4,I5,3I6,1PE10.3,0P,F8.2,10X,I4)
 6053 FORMAT(1X,3A4,I4,I4,I5,3I6,1PE10.3,8X,E10.3,   I4)
 6060 FORMAT(1X,72('=')/' END OF RUN',20X,I6,' PLOTS GENERATED')
      END
      SUBROUTINE SETUP
C
C     DEFINE ALL PLOTTER PARAMETERS AND INITIALIZE PLOTTER.
C
      CHARACTER*4 VERSES
      COMMON/INCHES/XINCH(2),YINCH(2)
      COMMON/HPINCH/XHP(2),YHP(2)
      COMMON/INCHTB/XPINCH(2,4),YPINCH(2,4)
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC
      COMMON/LOGTAB/TABLOG(10)
      COMMON/PLOTN/NUMPLT,ADVANC
      COMMON/VERSC/VERSES(5,4)
      COMMON/VERSI/NVERSE(4)
      COMMON/PAPER/PAPERX0,PAPERX1,PAPERY0,PAPERY1
      DIMENSION BUFF(5000)
C ----DEFINE HEIGHT AND WIDTH OF CHARACTERS FOR PROGRAM I.D.
      DATA HTV/0.28/
      DATA WTV/0.28/
C ----SELECT FULL OR HALF SIZE PLOTS.
      IF(IPLOTZ.EQ.0) GO TO 30
C
C     HALF SIZE PLOTS.
C
      SIZPLT=0.5
      II=0
      DO 20 I=1,2
      DO 10 J=1,2
      II=II+1
      XPINCH(1,II)=PAPERX0+0.5*PAPERX1*FLOAT(J-1)
      XPINCH(2,II)=XPINCH(1,II)+0.45*(PAPERX1-PAPERX0)*0.65
      YPINCH(1,II)=PAPERY0+0.5*PAPERY1*FLOAT(2-I)
   10 YPINCH(2,II)=YPINCH(1,II)+0.45*(PAPERY1-PAPERY0)
   20 CONTINUE
      XINCH(1)=XPINCH(1,1)
      XINCH(2)=XPINCH(2,1)
      YINCH(1)=YPINCH(1,1)
      YINCH(2)=YPINCH(2,1)
      BOX=0.008*(PAPERX1-PAPERX0)*SIZPLT
      HT =0.012*(PAPERX1-PAPERX0)*SIZPLT
      GO TO 40
C
C     FULL SIZE PLOTS.
C
   30 SIZPLT=1.0
      XINCH(1)=PAPERX0
      XINCH(2)=PAPERX1*0.65
      YINCH(1)=PAPERY0
      YINCH(2)=PAPERY1
      BOX=0.008*(PAPERX1-PAPERX0)
      HT =0.012*(PAPERX1-PAPERX0)
C ----DEFINE X AND Y SIZE OF PLOTS (ONLY TO DEFINE PAPER PLOTTER
C ----ADVANCE AND MAXIMUM SIZE FOR HEWLETT-PACKARD PLOTTER).
   40 WT=HT
      XHP(1)=PAPERX0
      XHP(2)=PAPERX1*0.6
      YHP(1)=PAPERY0
      YHP(2)=PAPERY1
C ----DEFINE DISTANCE TO ADVANCE IN X DIRECTION BETWEEN PLOTS.
      ADVANC=1.75*XHP(2)
C
C     INITIALIZE PLOTTER
C
C     IBMPC IS USED TO INDICATE WHETHER THE PROGRAM IS RUNNING ON THE
C     MAINFRAME OR ON AN IBM-PC.
C
C     IBMPC IS INITIALIZED TO -1 IN THE MAIN AND IF RUNNING ON THE
C     MAINFRAME IT WILL NOT CHANGE.
C
C     IF RUNNING ON AN IBM-PC THE PLOTTER INTERFACE ROUTINE PLOTS WILL
C     CHANGE THE VALUE OF IBM AS FOLLOWS,
C
C     IBMPC = 0 - PC FULL SIZE PLOTS.
C           = 1 - PC HALF SIZE PLOTS.
C
      CALL STARPLOT
C ----DEFINE ALL BOX AND CHARACTER HEIGHTS AND WIDTHS (ALL SCALED TO
C ----BOX AND HT DEFINED ABOVE).
      BOX2=BOX/2.0
      BOX4=BOX/4.0
      BOXWT2=4.0*BOX/7.0
      HT2=2.0*HT
      HTH=HT/2.0
      HT34=3.0*HT/4.0
      WTH=WT/2.0
      WT38=3.0*WT/8.0
C ----DEFINE POSITION OF PLOT LABELS.
      CALL SPOTER
C ----CONSTRUCT ONE DECADE OF TABLE OF LOGS (FOR AXIS LABELS).
      DO 50 I=1,10
      XL=I
   50 TABLOG(I)=ALOG10(XL)
C
C     IF RUNNING ON MAINFRAME IDENTIFY PROGRAM AND INSTALLATION.
C
      IF(IBMPC.GE.0) GO TO 70
      CALL PEN(2)
      Y=0.5*(YHP(1)+YHP(2))+2.0*HTV
      XMID=0.5*(XHP(1)+XHP(2))
      DO 60 L=1,4
      X=XMID-0.5*FLOAT(NVERSE(L))*WTV
      CALL SYMBLH(X,Y,HTV,VERSES(1,L),0.0,NVERSE(L))
   60 Y=Y-1.75*HTV
C ----ADVANCE TO NEXT PLOTTING AREA.
      CALL NEXTPLOT
   70 RETURN
      END
      SUBROUTINE NXTPLT
C
C     ADVANCE TO NEXT PLOTTER AREA.
C
      COMMON/INCHES/XINCH(2),YINCH(2)
      COMMON/HPINCH/XHP(2),YHP(2)
      COMMON/INCHTB/XPINCH(2,4),YPINCH(2,4)
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC
      COMMON/PLOTN/NUMPLT,ADVANC
C ----MOVE TO ORIGIN WITH PEN UP.
      CALL PLOT(0.0,0.0,3)
C ----IF FULL PLOT SIZE ADVANCE TO NEXT PLOT.
      IF(IPLOTZ.EQ.0) GO TO 10
C ----DEFINE COORDINATES OF NEXT SUB-PLOT.
      IPLOTZ=IPLOTZ+1
      II=IPLOTZ
      IF(IPLOTZ.GT.4) IPLOTZ=1
      XINCH(1)=XPINCH(1,IPLOTZ)
      XINCH(2)=XPINCH(2,IPLOTZ)
      YINCH(1)=YPINCH(1,IPLOTZ)
      YINCH(2)=YPINCH(2,IPLOTZ)
C ----DEFINE POSITION OF PLOT LABELS.
      CALL SPOTER
C ----IF LAST PLOT IS FULL, ADVANCE TO NEXT PLOTTING AREA.
      IF(II.LE.4) GO TO 20
C ----END OF PLOT. ADVANCE TO NEXT PLOTTING AREA.
   10 CALL NEXTPLOT
   20 RETURN
      END
      SUBROUTINE SPOTER
C
C     DEFINE POISITION OF AXIS LABELS AND LEGEND BOX COORDINATES.
C
C     TOP1   = Y POSITION OF TOP TITLE LINE
C     TOP2   = Y POSITION OF SECOND TITLE LINE
C     TOP3   = Y POSITION OF THIRD TITLE LINE
C     BOT1   = Y POSITION OF X AXIS NUMBERS (LINEAR)
C            = Y POSITION OF X AXIS LOG EXPONENTS (LOG)
C     BOT2   = Y POSITION OF X AXIS NUMBERS (LOG)
C     BOT3   = Y POSITION OF X AXIS LABEL
C     RIGHT1 = X POSITION OF Y AXIS NUMBERS
C     RIGHT2 = X POSITION OF Y AXIS LABEL
C     RIGHT3 = X POSITION OF START OF LEGEND BOX
C     RIGHT4 = X POSITION OF END OF LEGEND BOX
C     RIGHT5 = X POSITION OF REFERENCE/ENERGY RANGE TITLE
C     RIGHT6 = X POSITION OF POINTS TITLE
C     RIGHT7 = X POSITION OF START OF BOX TO IDENTIFY REFERENCE
C     RIGHT8 = X POSITION OF END OF BOX TO IDENTIFY REFERENCE
C
      COMMON/INCHES/XINCH(2),YINCH(2)
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
      COMMON/SPOTS/TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGHT1,RIGHT2,RIGHT3,
     1 RIGHT4,RIGHT5,RIGHT6,RIGHT7,RIGHT8
      TOP3=YINCH(2)+HT
      TOP2=TOP3+1.75*HT
      TOP1=TOP2+1.75*HT
      BOT1=YINCH(1)-1.75*HT
      BOT2=YINCH(1)-2.75*HT
      BOT3=YINCH(1)-4.5*HT
      RIGHT1=XINCH(2)+WT
      RIGHT2=XINCH(2)+7.0*HT
      RIGHT3=XINCH(2)+8.0*WT
      RIGHT4=RIGHT3+4.0*BOX+30.0*WT
      RIGHT5=RIGHT3+4.0*BOX+WT
      RIGHT6=RIGHT5+22.0*WT
      RIGHT7=RIGHT3+2.0*BOX
      RIGHT8=RIGHT7+2.0*BOX
      RETURN
      END
      SUBROUTINE READIN
C
C     READ ALL INPUT PARAMETERS AND INITIALIZE PLOTTER.
C
      PARAMETER (MXPGP=10000)
      INTEGER OUTP
      CHARACTER*4 LIBNAM,ENDF,EXFOR,ANSWER,BLANK,SCALEM,ENDFAN,GRIDTY,
     1 HSIZE
      CHARACTER*80 LINE
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC
      COMMON/LIBI/ILIB
      COMMON/LIBC/LIBNAM(4)
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
      COMMON/MODEMY/MYMODE
      COMMON/GRIDDY/MYGRID
      COMMON/INPARM/MINNIE,MAXIE
      COMMON/SYMBLM/MSYMBL
      COMMON/EXERRS/IXERR,IYERR
      COMMON/SCALEI/ISCALE
      COMMON/ENDFIM/IMENDF
      COMMON/INCHES/XINCH(2),YINCH(2)
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
      COMMON/PAPER/PAPERX0,PAPERX1,PAPERY0,PAPERY1
      DIMENSION EXFOR(2),ANSWER(2),SCALEM(3,3),ENDFAN(4,3),GRIDTY(3,2),
     1 HSIZE(2)
      DATA ENDFAN/
     1 '  NO','    ','    ','    ',
     2 ' YES','    ','    ','    ',
     3 ' YES',' (ID','ENTI','FY) '/
      DATA ANSWER/' NO','YES'/
      DATA ENDF/'ENDF'/
      DATA EXFOR/'  EX','FOR '/
      DATA BLANK/'    '/
      DATA SCALEM/
     1 'ENDF',' + E','XFOR',
     2 '    ','    ','ENDF',
     3 '    ','   E','XFOR'/
      DATA GRIDTY/
     1 'TICK',' MAR','KS  ',
     2 ' FUL','L GR','ID  '/
      DATA HSIZE/'FULL','HALF'/
C ----DEFINE UPPER DEFAULT ENERGY TO BE 100 MEV.
      DATA EMAX/1.0E+10/
C ----DEFAULT PAPER SIZE
      PAPERX0= 0
      PAPERX1=12.5
      PAPERY0= 0.5
      PAPERY1=10
C ----READ INPUT PARAMTERS.
      READ(INP,4030) PAPX0,PAPX1,PAPY0,PAPY1
      IF(PAPX0.NE.0) PAPERX0=PAPX0
      IF(PAPX1.NE.0) PAPERX1=PAPX1
      IF(PAPY0.NE.0) PAPERY0=PAPY0
      IF(PAPY1.NE.0) PAPERY1=PAPY1
      READ(INP,4000) IMENDF,MAXREF,ISCALE,IXERR,IYERR,MSYMBL,IGROUP,
     1 MINNIE,MAXIE,MYGRID,IPLOTZ,LIBNAM
      IMX4=0
      IF(MAXREF.LT.1) MAXREF=1
      IF(MAXREF.GT.1) IMX4=1
      IF(IMENDF.GT.2) IMENDF=2
      IF(IMENDF.LE.0) IMENDF=0
      IF(ISCALE.LT.0.OR.ISCALE.GT.2) ISCALE=0
      IF(IMENDF.EQ.0) ISCALE=2
      IF(IXERR.NE.0) IXERR=1
      IF(IYERR.NE.0) IYERR=1
      IF(MSYMBL.NE.0) MSYMBL=1
      IF(IGROUP.NE.0) IGROUP=1
      IF(MINNIE.LE.0) MINNIE=8
      IF(MAXIE.LE.MINNIE) MAXIE=MXPGP
      IF(MAXIE.GT.MXPGP ) MAXIE=MXPGP
      IF(MYGRID.NE.0) MYGRID=1
      IF(IPLOTZ.NE.0) IPLOTZ=1
      WRITE(OUTP,6000) (ENDFAN(I,IMENDF+1),I=1,4),ANSWER(IMX4+1),
     1 (SCALEM(I,ISCALE+1),I=1,3),ANSWER(IXERR+1),ANSWER(IYERR+1),
     2 ANSWER(MSYMBL+1),ANSWER(IGROUP+1),MINNIE,MAXIE,
     3 (GRIDTY(I,MYGRID+1),I=1,3),HSIZE(IPLOTZ+1)
C ----DEFINE INTERNAL RUN MODE.
      MYMODE=IMX4+2*IMENDF
      IF(IMENDF.EQ.2) MYMODE=IMX4+2
      IF(MYMODE.LT.2) GO TO 30
C ----LEFT ADJUST ENDF/B LIBRARY I.D....IF COMPLETELY BLANK DEFINE
C ----LIBRARY I.D. TO BE ENDF.
      CALL SHIFTY(LIBNAM,ILIB)
      IF(ILIB.GT.0) GO TO 20
      ILIB=4
      LIBNAM(1)=ENDF
      DO 10 I=2,4
   10 LIBNAM(I)=BLANK
   20 WRITE(OUTP,6010) LIBNAM
      GO TO 40
C ----INITIALIZE LIBRARY I.D. TO EXFOR (ADD AN/SAN LATER).
   30 LIBNAM(1)=EXFOR(1)
      LIBNAM(2)=EXFOR(2)
C ----DEFINE ALL PLOTTER PARAMETERS AND INITIALIZE PLOTTER.
   40 CALL SETUP
C ----LIMIT THE NUMBER OF DISPLAYED REFERENCES BY AVAILABLE SPACE
       MXR=(YINCH(2)-YINCH(1))/(1.75*HT)-2
       MAXREF=MIN(MXR,MAXREF,48)
C ----IDENTIFY COMPUTER THAT PROGRAM IS RUNNING ON.
      IF(IBMPC.LT.0) WRITE(OUTP,6012)
      IF(IBMPC.GE.0) WRITE(OUTP,6014)
C ----PRINT TITLE BEFORE FIRST REQUEST.
      WRITE(OUTP,6020)
      NGET=0
      DO 70 JGET=1,100
      NGET=NGET+1
   50 READ(INP,4020,END=80) LINE
      IF(LINE( 1:26).EQ.'                          '.AND.
     1   LINE(27:52).EQ.'                          ') GO TO 80
      READ(LINE,4010,ERR=80) IZALOW(NGET),MFLOW(NGET),
     1 MTLOW(NGET),ELGET(NGET),IZAHI(NGET),MFHI(NGET),MTHI(NGET),
     2 EHGET(NGET),INTRN(NGET),EPGET(NGET)
C ----IF COMPARING TO ENDF/B SET MF = 3 (ONLY CROSS SECTIONS) AND
C ---- MT = ONLY UP TO 999.
      IF(MYMODE.LT.2) GO TO 60
      IF(MFHI(NGET).LT.3 .OR. MFLOW(NGET).GT.6) GO TO 50
      IF(MFLOW(NGET).LT.3) MFLOW(NGET)=3
      IF(MFHI(NGET).GT.10) MFHI(NGET)=10
C ----IF REQUIRED SET UPPER ZA/MF/MT LIMIT TO LOWER LIMIT.
   60 IF(IZAHI(NGET).LT.IZALOW(NGET)) IZAHI(NGET)=IZALOW(NGET)
      IF(MTHI(NGET).LT.MTLOW(NGET)) MTHI(NGET)=MTLOW(NGET)
      IF(MFHI(NGET).LT.MFLOW(NGET)) MFHI(NGET)=MFLOW(NGET)
C ----IF REQUIRED SET UPPER ENERGY LIMIT TO 100 MEV.
      IF(EHGET(NGET).LE.0.0) EHGET(NGET)=EMAX
C ----PRINT REQUEST.
      WRITE(OUTP,6030) IZALOW(NGET),MFLOW(NGET),MTLOW(NGET),
     1 ELGET(NGET),IZAHI(NGET),MFHI(NGET),MTHI(NGET),EHGET(NGET)
      IF(EHGET(NGET).GT.ELGET(NGET)) GO TO 70
      WRITE(OUTP,6040)
      GO TO 50
   70 CONTINUE
      NGET=101
   80 NGET=NGET-1
      IF(NGET.GT.0) GO TO 100
C ----NO REQUESTS. DEFINE DEFAULT REQUEST TO PLOT AS MUCH DATA AS
C ----POSSIBLE.
      IZALOW(1)=1
      IZAHI(1)=999999
      MFLOW(1)=1
      MFHI(1)=999
      MTLOW(1)=1
      MTHI(1)=9999
      ELGET(1)=0.0
      EHGET(1)=EMAX
      NGET=1
   90 WRITE(OUTP,6030) IZALOW(NGET),MFLOW(NGET),MTLOW(NGET),
     1 ELGET(NGET),IZAHI(NGET),MFHI(NGET),MTHI(NGET),EHGET(NGET)
  100 RETURN
 4000 FORMAT(11I5,4A4)
 4010 FORMAT(I7,2I4,E11.4,I7,2I4,E11.4,I3,E11.4)
 4020 FORMAT(A80)
 4030 FORMAT(4F10.0)
 6000 FORMAT(1X,72('=')/' READING INPUT PARAMETERS'/1X,72('=')/
     1 ' COMPARE EXFOR DATA TO ENDF/B DATA----------',12X,4A4/
     2 ' ALL COMPARABLE EXFOR DATA ON SAME PLOT-----',13X,A3/
     3 ' SCALE PLOTS ACCORDING TO-------------------',4X,3A4/
     4 ' PLOT X ERROR BARS--------------------------',13X,A3/
     5 ' PLOT Y ERROR BARS--------------------------',13X,A3/
     6 ' IDENTIFY ALL REFERENCES BY SYMBOL----------',13X,A3/
     7 ' ALLOW VARIABLE E2 ON SAME PLOT-------------',13X,A3/
     8 ' MINIMUM EXFOR POINTS PER PLOT--------------',I16/
     9 ' MAXIMUM EXFOR POINTS PER PLOT--------------',I16/
     9 ' GRID TYPE----------------------------------',6X,2A4,A2/
     A ' PLOT SIZE----------------------------------',12X,A4)
 6010 FORMAT(' EVALUATED DATA I.D.------------------------',4A4)
 6012 FORMAT(' COMPUTER TYPE------------------------------',
     1 '       MAINFRAME')
 6014 FORMAT(' COMPUTER TYPE------------------------------',
     1 '          IBM-PC')
 6020 FORMAT(1X,72('=')/' PLOT THE FOLLOWING DATA'/1X,72('=')/
     1 '           LOWER LIMIT               UPPER LIMIT'/
     1 '      ZA  MF  MT  ENERGY-EV     ZA  MF  MT  ENERGY-EV'/
     3 1X,72('='))
 6030 FORMAT(1X,I7,2I4,1PE11.4,I7,2I4,1PE11.4)
 6040 FORMAT(' UPPER ENERGY LESS THAN LOWER ENERGY...REQUEST IGNORED')
      END
      SUBROUTINE SHIFTY(LIBNAM,ILIB)
C
C     LEFT ADJUST LIBRARY I.D.
C
      CHARACTER*1 LIBNAM,BLANK
      DIMENSION LIBNAM(16)
      DATA BLANK/' '/
C ----SKIP LEADING BLANKS.
      DO 10 I=1,16
      IF(LIBNAM(I).NE.BLANK) GO TO 20
   10 CONTINUE
C ----NAME IS COMPLETELY BLANK
      ILIB=0
      RETURN
C ----LEFT ADJUST NAME.
   20 II=0
      ILIB=0
      DO 30 J=I,16
      II=II+1
      LIBNAM(II)=LIBNAM(J)
      IF(LIBNAM(II).NE.BLANK) ILIB=II
   30 CONTINUE
C ----BLANK OUT REMAINDER (IF ANY).
      IF(ILIB.GE.16) GO TO 50
      II=ILIB+1
      DO 40 I=II,16
   40 LIBNAM(I)=BLANK
   50 RETURN
      END
      SUBROUTINE GETEV(IEND)
C
C     FIND NEXT SECTION OF REQUESTED ENDF DATA
C
      PARAMETER (MXPNT=90000)
      INTEGER OUTP
C...
      CHARACTER*4 MSTAT1,MSTAT2,CHST
C...
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/PAGEXY/XPAGE(MXPNT),YPAGE(MXPNT),N2,IBASE,ITOP,ISCR
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,EINC,AWR
C...
      COMMON/WHEREC/ZABCD(4),MSTAT1,MSTAT2
C...
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
C...
      DIMENSION CHST(9)
C...
      SAVE
C...
      DATA IPASS/0/
      DATA MATLST/-99999/
C...
      DATA MF10MT,MF10ST/ 0, 0/
      DATA CHST/'G   ','M   ','N   ','4   ','5   '
     1         ,'6   ','7   ','8   ','*   '/
C...
C ----DURING FIRST PASS SKIP ENDF/B TAPE LABEL.
      IF(IPASS.EQ.0) READ(ITAPE2,1000,ERR=100,END=100) HEADER
      IPASS=1
      IEND=0
C ----FIND BEGINNING OF NEXT SECTION.
   10 READ(ITAPE2,1010) C1,C2,L1,L2,NS,N2,MAT,MF,MT
      IF(MT) 20,20,30
   20 IF(MAT) 110,10,10
C ----DEFINE EVALUATION MOD. FROM FIRST CARD OF HOLLERITH SECTION.
   30 IF(MAT.EQ.MATLST) GO TO 40
      MATLST=MAT
      MODIZA=0
      IF(MF.NE.1. OR .MT.NE.451) GO TO 40
        MODIZA=N2
        READ(ITAPE2,1010) C1,C2,LIS,LIS0,N1,N2,MAT,MF,MT
        MSTAT1='    '
        IF(LIS0.GT.0) MSTAT1=CHST(LIS0+1)
C ----SEARCH FOR SECTION OF FILE 3 OR 10.
   40 AWR=C2
      IF(MF.LT.3 .OR. MF.GT.10) GO TO 50
      GO TO 60
C
C     SKIP SECTION.
C
   50 READ(ITAPE2,1040) MAT,MF,MT
      IF(MT) 10,10,50
C
C     CHECK ZA/MF/MT AGAINST REQUESTS.
C
   60 IZA=C1
      CALL RQUEST(IGET,IZA,MF,MT)
      IF(IGET) 50,50,70
C
C     DATA IS REQUESTED. READ DATA UP TO MXPNT POINTS AT A TIME. IF OVER
C     MXPNT POINTS STORE ALL IN PAGING SYSTEM.
C
C ----CHECK FOR MF3/4/5/6 FILE DATA
C ----IF MF4/5/6, LOAD THE DATA AFTER EXFOR
   70 IF(MF.EQ.3 .OR. MF.EQ.10) GO TO 78
      IF(MF.LT.4 .OR. (MF.GT.6 .AND. MF.LT.12) .OR. MF.GT.12) GO TO 50
      IF(MF.NE. 4) MF=6
      IF(MF.NE. 4) MT=9000
      IEND=0
   77 READ(ITAPE2,1040) MAT,MFS,MTS
      IF(MTS.NE.0) GO TO 77
      RETURN
C ----PROCESS FILE MF3 AND MF10 DATA
C...
   78 NS=MAX(NS,1)
      MSTAT2='    '
      IF(MF.NE.10) GO TO 79
        IF(MT.NE.MF10MT) MF10ST=0
        MF10ST=MF10ST+1
        IF(MF10ST.GT.NS) GO TO 50
        J=MIN(MF10ST,9)
        MSTAT2=CHST(J)
   79 DO 82 K=1,NS
C...
      READ(ITAPE2,1010) C1,C2,L1,L2,N1,N2,MAT,MF,MT
      READ(ITAPE2,1020) (NBT,INTI,I=1,N1)
      IF(N1.GT.1 .OR. INTI.NE.2) GO TO 102
      IF(N2.GT.MXPNT) REWIND ISCR
      DO 80 I=1,N2,MXPNT
      II=I+MXPNT-1
      IF(II.GT.N2) II=N2
      READ(ITAPE2,1030) (XPAGE(J-I+1),YPAGE(J-I+1),J=I,II)
      IF(N2.GT.MXPNT) WRITE(ISCR) XPAGE,YPAGE
   80 CONTINUE
C...
   82 CONTINUE
C...
      IBASE=0
      IF(N2.GT.MXPNT) GO TO 90
      ITOP=N2
      RETURN
C ----LOAD FIRST PAGE FROM SCRATCH AND DEFINE IN CORE PAGE LIMITS.
   90 END FILE ISCR
      REWIND ISCR
      READ(ISCR) XPAGE,YPAGE
      ITOP=MXPNT
      RETURN
C ----ERROR OR END OF FILE WHILE READING ENDF/B DATA.
  100 WRITE(OUTP,6000)
      STOP
  102 WRITE(OUTP,6010)
      STOP
C ----END OF ENDF/B DATA (BASED ON MAT).
  110 IEND=1
      RETURN
 1000 FORMAT(17A4,A4)
 1010 FORMAT(2E11.4,4I11,I4,I2,I3)
 1020 FORMAT(2I11)
 1030 FORMAT(6E11.4)
 1040 FORMAT(66X,I4,I2,I3)
 6000 FORMAT(' ERROR READING ENDF/B DATA...EXECUTION TERMINATED')
 6010 FORMAT(' ERROR - ENDF DATA NOT LIN.INTERPOLABLE'
     1      ,'...EXECUTION TERMINATED')
      END
      FUNCTION X(I)
C
C     RETRIEVE ENDF/B ENERGY FROM PAGING SYSTEM.
C
      PARAMETER (MXPNT=90000)
      INTEGER OUTP
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/PAGEXY/XPAGE(MXPNT),YPAGE(MXPNT),N2,IBASE,ITOP,ISCR
      SAVE IERR
      DATA IERR/0/
C ----INSURE INDEX IS IN LEGAL RANGE.
      IF(I.GT.0.AND.I.LE.N2) GO TO 10
      WRITE(OUTP,6000) I,N2
      IERR=IERR+1
      IF(IERR.LT.50) RETURN
      STOP
C ----SEE IF REQUESTED POINT PRECEEDS POINTS IN CORE.
   10 IF(I-IBASE) 20,20,40
C ----REWIND AND INITIALIZE UPPER PAGE INDEX.
   20 REWIND ISCR
      ITOP=0
C ----LOAD NEXT PAGE INTO CORE.
   30 READ(ISCR) XPAGE,YPAGE
      IBASE=ITOP
      ITOP=ITOP+MXPNT
C ----SEE IF REQUESTED POINT FOLLOWS POINTS IN CORE.
   40 IF(I-ITOP) 50,50,30
C ----POINT IS IN CORE. DEFINE IT.
   50 INCORE=I-IBASE
      X=XPAGE(INCORE)
      RETURN
 6000 FORMAT(' FUNCTION X...I=',I6,' (MUST BE 1 TO',I6,') DEFINED X=0')
      END
      FUNCTION Y(I)
C
C     RETRIEVE ENDF/B CROSS SECTION FROM PAGING SYSTEM.
C
      PARAMETER (MXPNT=90000)
      INTEGER OUTP
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/PAGEXY/XPAGE(MXPNT),YPAGE(MXPNT),N2,IBASE,ITOP,ISCR
      SAVE IERR
      DATA IERR/0/
C ----INSURE INDEX IS IN LEGAL RANGE.
      IF(I.GT.0.AND.I.LE.N2) GO TO 10
      WRITE(OUTP,6000) I,N2
      IERR=IERR+1
      IF(IERR.LT.50) RETURN
      STOP
C ----SEE IF REQUESTED POINT PRECEEDS POINTS IN CORE.
   10 IF(I-IBASE) 20,20,40
C ----REWIND AND INITIALIZE UPPER PAGE INDEX.
   20 REWIND ISCR
      ITOP=0
C ----LOAD NEXT PAGE INTO CORE.
   30 READ(ISCR) XPAGE,YPAGE
      IBASE=ITOP
      ITOP=ITOP+MXPNT
C ----SEE IF REQUESTED POINT FOLLOWS POINTS IN CORE.
   40 IF(I-ITOP) 50,50,30
C ----POINT IS IN CORE. DEFINE IT.
   50 INCORE=I-IBASE
      Y=YPAGE(INCORE)
      RETURN
 6000 FORMAT(' FUNCTION Y...I=',I6,' (MUST BE 1 TO',I6,') DEFINED X=0')
      END
      SUBROUTINE RQUEST(KGET,IZA,MF,MT)
C
C     COMPARE CURRENT ENDF/B ZA/MF/MT TO REQUESTS.
C
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
      DO 10 KGET=1,NGET
      IF(IZA.LT.IZALOW(KGET).OR.IZA.GT.IZAHI(KGET)) GO TO 10
      IF(MF.LT.MFLOW(KGET).OR.MF.GT.MFHI(KGET)) GO TO 10
      IF(MT.GE.MTLOW(KGET).AND.MT.LE.MTHI(KGET)) GO TO 20
   10 CONTINUE
      KGET=0
   20 RETURN
      END
      SUBROUTINE SCALER
C
C     DEFINE X AND Y LIMITS. SELECT LINEAR OR LOG SCALING.
C
      PARAMETER (MXPNT=90000)
      PARAMETER (MXPGP=10000)
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/WAYS/IWAY(2)
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,EINC,AWR
      COMMON/RATZA/IZARAT,MTRAT,MFIN
      COMMON/PAGEXY/XPAGE(MXPNT),YPAGE(MXPNT),N2,IBASE,ITOP,ISCR
      COMMON/EXFOR/XEX(MXPGP),DXEX(MXPGP),YEX(MXPGP),DYEX(MXPGP)
     1,NREF(MXPGP),E2(MXPGP),IEX
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
      COMMON/DOUBL/IDOUB,FIELD4(4)
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP
      COMMON/XLIMIT/MPT
      COMMON/SCALEI/ISCALE
      COMMON/X4LIMS/X4LIMX(2),X4LIMY(2)
C
C     COMPARE CURRENT ZA/MF/MT TO CURRENT REQUEST
C     IF NOT CROSS SECTIONS ALSO COMPARE INCIDENT ENERGY.
C
      MPT=0
      IF(IZA.LT.IZALOW(IGET).OR.IZA.GT.IZAHI(IGET)) GO TO 10
      IF(MFIN.LT.MFLOW(IGET).OR.MFIN.GT.MFHI(IGET)) GO TO 10
      IF(MT.LT.MTLOW(IGET).OR.MT.GT.MTHI(IGET)) GO TO 10
      IF(MF.EQ.3) GO TO 20
      IF(EINC.LT.ELGET(IGET).OR.EINC.GT.EHGET(IGET)) GO TO 10
      GO TO 20
C ----REQUEST IS NOT FOR THIS ZA/MF/MT.
   10 RETURN
C
C     DEFINE MAXIMUM AND MINIMUM X AND Y VALUES.
C
   20 IPASS=0
C ----IF NO ENDF/B DATA OR ONLY SCALING EXFOR DATA SKIP THIS SECTION.
      IF(N2.LE.0.OR.ISCALE.EQ.2) GO TO 120
C
C     SCALE ENDF/B DATA.
C
      DO 110 I=1,N2
C ----IF CROSS SECTIONS ONLY SELECT POINTS IN ENERGY RANGE OF NEXT PLOT.
      XNOW=X(I)
      YNOW=Y(I)
      IF(MF.NE.3) GO TO 70
C ----IGNOR ALL POINTS BELOW LOWER ENERGY LIMIT OF PLOT.
      IF(XNOW-ELGET(IGET)) 100,70,30
C ----LOWER LIMIT OF PLOT REACHED. CHECK UPPER LIMIT.
   30 IF(XNOW-EHGET(IGET)) 60,40,40
C ----UPPER ENERGY OF PLOT REACHED. NOTHING TO DO IF ALL POINTS ARE
C ----ABOVE THE ENERGY RANGE OF THE PLOT.
   40 IF(I.LE.1) GO TO 120
C ----IF LAST POINT WAS BELOW LOWER LIMIT OF PLOT INTERPOLATE TO LOWER
C ----LIMIT AND INITIALIZE X AND Y LIMITS.
      IF(IPASS.GT.0) GO TO 50
      YNOW=((XNOW-ELGET(IGET))*YLAST+(ELGET(IGET)-XLAST)*YNOW)/
     1 (XNOW-XLAST)
      XNOW=ELGET(IGET)
      XLIM(1)=XNOW
      XLIM(2)=XNOW
      YLIM(1)=YNOW
      YLIM(2)=YNOW
C ----INTERPOLATE TO UPPER ENERGY LIMIT (ABOVE TESTS INSURE AT LEAST
C ----ONE PRECEDING POINT).
   50 YNOW=((XNOW-EHGET(IGET))*YLAST+(EHGET(IGET)-XLAST)*YNOW)/
     1 (XNOW-XLAST)
      XNOW=EHGET(IGET)
C ----SET FLAG TO INDICATE END OF PLOT.
      IPASS=2
      GO TO 90
C ----POINT IS WITHIN ENERGY RANGE OF PLOT. IF LIMITS ARE ALREADY
C ----INITIALIZED USE CURRENT POINT TO UPDATE X AND Y LIMITS.
   60 IF(IPASS.GT.0) GO TO 90
C ----LIMITS ARE NOT YET INITIALIZED. IF THIS IS FIRST POINT USE
C ----IT TO INITIALIZE X AND Y LIMITS. OTHERWISE INTERPOLATE TO LOWER
C ----ENERGY LIMIT OF PLOT.
      IF(I.LE.1) GO TO 80
      YNOW=((XNOW-ELGET(IGET))*YLAST+(ELGET(IGET)-XLAST)*YNOW)/
     1 (XNOW-XLAST)
      XNOW=ELGET(IGET)
      GO TO 80
C ----INITIALIZE LIMITS ON FIRST ACCEPTABLE POINT.
   70 IF(IPASS.GT.0) GO TO 90
   80 IPASS=1
      XLIM(1)=XNOW
      XLIM(2)=XNOW
      YLIM(1)=YNOW
      YLIM(2)=YNOW
      GO TO 100
C ----UPDATE LIMIT.
   90 IF(XNOW.LT.XLIM(1)) XLIM(1)=XNOW
      IF(XNOW.GT.XLIM(2)) XLIM(2)=XNOW
      IF(YNOW.LT.YLIM(1)) YLIM(1)=YNOW
      IF(YNOW.GT.YLIM(2)) YLIM(2)=YNOW
C ----END OF SCALING IF FLAG INDICATES UPPER ENERGY LIMIT REACHED.
  100 IF(IPASS.GE.2) GO TO 120
C ----SAVE X AND Y VALUES FOR INTERPOLATION.
      XLAST=XNOW
      YLAST=YNOW
  110 CONTINUE
C
C     DETERMINE WHICH REFERENCES WILL APEEAR ON NEXT PLOT, NUMBER OF
C     POINTS FROM EACH REFERENCE AND ENERGY RANGE OF EACH REFERENCE.
C     IF REQUESTED USE EXFOR DATA FOR SCALING.
C
  120 MREF=0
      IMLOW=0
      IMHI=0
C ----SCALE EACH REFERENCE SEPARATELY.
      DO 210 KREF=1,IREF
      LREF(KREF)=0
C ----SELECT POINTS FROM CURRENT REFERENCE.
      DO 200 I=1,IEX
      IF(NREF(I).NE.KREF) GO TO 200
C ----IF CROSS SECTIONS ONLY SELECT POINTS IN ENERGY RANGE OF NEXT PLOT.
C ----ENERGY RANGE OF NEXT PLOT.
      IF(MF.NE.3) GO TO 130
      IF(XEX(I).LT.ELGET(IGET)) GO TO 180
      IF(XEX(I).GT.EHGET(IGET)) GO TO 190
C ----IF ONLY SCALING FOR ENDF/B IGNOR POINTS OUTSIDE THE RANGE OF THE
C ----PLOT.
  130 IF(ISCALE.NE.1) GO TO 140
      IF(XEX(I).LT.XLIM(1).OR.XEX(I).GT.XLIM(2)) GO TO 200
      IF(YEX(I).LT.YLIM(1).OR.YEX(I).GT.YLIM(2)) GO TO 200
C ----COUNT TOTAL POINTS, REFERENCES, POINTS FOR CURRENT REFERENCE AND
C ----SAVE ENERGY RANGE OF CURRENT REFERENCE.
  140 MPT=MPT+1
      IF(LREF(KREF).GT.0) GO TO 150
      MREF=MREF+1
      EXLOW(KREF)=XEX(I)
      EXHIGH(KREF)=XEX(I)
  150 IF(XEX(I).LT.EXLOW(KREF)) EXLOW(KREF)=XEX(I)
      IF(XEX(I).GT.EXHIGH(KREF)) EXHIGH(KREF)=XEX(I)
      LREF(KREF)=LREF(KREF)+1
C ----IF REQUESTED DO NOT SCALE FOR EXFOR DATA.
      IF(ISCALE.EQ.1) GO TO 200
C
C     SCALE DATA ALLOWING FOR X AND Y UNCERTAINTIES.
C
C ----DEFINE X LIMITS TRUNCATED TO REQUESTED ENERGY RANGE AND DO NOT
C ----LET LOWER X LIMIT CROSS ZERO.
      XEXM=XEX(I)-DXEX(I)
      XEXP=XEX(I)+DXEX(I)
      IF(MF.NE.3) GO TO 160
      IF(XEXM.LT.ELGET(IGET)) XEXM=ELGET(IGET)
      IF(XEXP.GT.EHGET(IGET)) XEXP=EHGET(IGET)
      IF(MF.EQ.4) GO TO 160
C ----ALLOW COSINE ERROR TO CROSS ZERO.
      IF(MF.EQ.6.AND.IDOUB.EQ.1) GO TO 160
      IF(XEX(I).GT.0.0.AND.XEXM.LE.0.0) XEXM=XEX(I)
C ----DEFINE Y LIMITS TRUNCATED NOT TO LET LOWER Y LIMIT CROSS ZERO.
  160 YEXM=YEX(I)-DYEX(I)
      YEXP=YEX(I)+DYEX(I)
      IF(YEX(I).GT.0.0.AND.YEXM.LE.0.0) YEXM=YEX(I)
      IF(IPASS.GT.0) GO TO 170
C ----SAVE LIMITS BASED ON FIRST POINT.
      IPASS=1
      XLIM(1)=XEXM
      XLIM(2)=XEXP
      YLIM(1)=YEXM
      YLIM(2)=YEXP
      GO TO 200
C ----UPDATE X AND Y LIMITS.
  170 IF(XEXM.LT.XLIM(1)) XLIM(1)=XEXM
      IF(XEXP.GT.XLIM(2)) XLIM(2)=XEXP
      IF(YEXM.LT.YLIM(1)) YLIM(1)=YEXM
      IF(YEXP.GT.YLIM(2)) YLIM(2)=YEXP
      GO TO 200
C ----COUNT POINTS BELOW ENERGY RANGE OF PLOT.
  180 IMLOW=IMLOW+1
      GO TO 200
C ----COUNT POINT ABOVE ENERGY RANGE OF PLOT.
  190 IMHIGH=IMHIGH+1
  200 CONTINUE
  210 CONTINUE
C ----IF THESE ARE CROSS SECTIONS IF THERE ARE POINTS BELOW OR ABOVE
C ----THE REQUESTED ENERGY LIMITS SET ENERGY LIMITS TO EXACTLY REQUESTED
C ----REQUESTED LIMITS.
      IF(MF.NE.3.OR.ISCALE.NE.1) GO TO 220
      IF(IMLOW.GT.0) XLIM(1)=ELGET(IGET)
      IF(IMHIGH.GT.0) XLIM(2)=EHGET(IGET)
C ----SAVE CURRENT LIMITS TO PREVENT ADDITIONAL EXFOR POINTS BEING
C ----PLOTTED AFTER ROUNDING PLOT LIMITS OUTWARD.
  220 X4LIMX(1)=XLIM(1)
      X4LIMX(2)=XLIM(2)
      X4LIMY(1)=YLIM(1)
      X4LIMY(2)=YLIM(2)
C
C     DEFINE LINEAR OR LOG SCALING
C
      IWAY(1)=2
      IF(MF.EQ.7) XLIM(1)=0.0
      JNTR=INTRN(IGET)
      IF(MF.EQ.6.AND.IDOUB.EQ.1) IWAY(1)=1
      IF(MF.EQ.4 .OR.MF.EQ.8) IWAY(1)=1
      IF(JNTR.EQ.0 .AND. XLIM(1).LE.0.0) IWAY(1)=1
      IF(JNTR.EQ.0 .AND. XLIM(2).LT.10.0*XLIM(1)) IWAY(1)=1
      IF(JNTR.EQ.2 .OR. JNTR.EQ.4) IWAY(1)=1
      IF(IWAY(1).EQ.2 .AND.XLIM(1).LE.0) XLIM(1)=1.E-5
      IWAY(2)=2
      IF(JNTR.EQ.0 .AND. YLIM(1).LE.0.0) IWAY(2)=1
      IF(JNTR.EQ.0 .AND. YLIM(2).LT.10.0*YLIM(1)) IWAY(2)=1
      IF(JNTR.EQ.2 .OR. JNTR.EQ.3) IWAY(2)=1
      IF(IWAY(2).EQ.2 .AND.YLIM(1).LE.0) YLIM(1)=1.E-5*YLIM(2)
C
C     IF REQUIRED, CONVERT LIMITS TO LOG.
C
      IF(IWAY(1).EQ.1) GO TO 230
      XLIM(1)=ALOG10(XLIM(1))
      XLIM(2)=ALOG10(XLIM(2))
  230 IF(IWAY(2).EQ.1) GO TO 240
      YLIM(1)=ALOG10(YLIM(1))
      YLIM(2)=ALOG10(YLIM(2))
C
C     PREVENT ZERO RANGES AND ROUND LIMITS OUTWARD.
C
  240 IF(MF.EQ.4) GO TO 250
      IF(MF.EQ.6.AND.IDOUB.EQ.1) GO TO 250
      IF(XLIM(2).GT.XLIM(1)) GO TO 260
      IF(XLIM(1).EQ.0.0) GO TO 250
      XLIM(1)=0.5*XLIM(1)
      XLIM(2)=1.5*XLIM(2)
      GO TO 280
  250 XLIM(1)=-1.04
      XLIM(2)=1.04
      GO TO 280
  260 IF(MF.EQ.7.OR.IWAY(1).EQ.2.OR.XLIM(1).GT.0.0) GO TO 270
      XLIM(2)=1.02*XLIM(2)
      GO TO 280
  270 XMID=0.5*(XLIM(2)+XLIM(1))
      XRANGE=0.5*(XLIM(2)-XLIM(1))
      XLIM(1)=XMID-1.04*XRANGE
      XLIM(2)=XMID+1.04*XRANGE
  280 IF(YLIM(2).GT.YLIM(1)) GO TO 300
      IF(YLIM(1).EQ.0.0) GO TO 290
      YLIM(1)=0.5*YLIM(1)
      YLIM(2)=1.5*YLIM(2)
      GO TO 320
  290 YLIM(1)=-1.0
      YLIM(2)=1.0
      GO TO 320
  300 IF(IWAY(2).EQ.2.OR.YLIM(1).GT.0.0) GO TO 310
      YLIM(2)=1.04*YLIM(2)
      GO TO 320
  310 YMID=0.5*(YLIM(2)+YLIM(1))
      YRANGE=0.5*(YLIM(2)-YLIM(1))
      YLIM(1)=YMID-1.04*YRANGE
      YLIM(2)=YMID+1.04*YRANGE
  320 RETURN
      END
      SUBROUTINE UNITED
C
C     SELECT X AND Y UNITS. DEFINE X AND Y SCALE FACTORS.
C     CONVERT X AND Y LIMITS TO AXIS UNITS.
C
      CHARACTER*1 LABCM,STATUS,CMSYS
      CHARACTER*4 XLABEL,YLABEL,COSINE,COSCM,LORDER,COEFF,COEFCM,ATWT,
     1 YIELD,RATIO,IM78,DEF78,NOKNOW
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/XYREAL/XREAL(2),YREAL(2)
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,EINC,AWR
      COMMON/RATZA/IZARAT,MTRAT,MFIN
      COMMON/WAYS/IWAY(2)
      COMMON/UNNORM/IMNORM
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP
      COMMON/XYLABC/XLABEL(10),YLABEL(10)
      COMMON/SYSSTA/LABCM,STATUS
      COMMON/DOUBL/IDOUB,FIELD4(4)
      COMMON/WHO78C/IM78
      COMMON/SCALEI/ISCALE
      COMMON/X4LIMS/X4LIMX(2),X4LIMY(2)
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC
      DIMENSION COSINE(2),COSCM(3),LORDER(4),COEFF(3),COEFCM(4),ATWT(4),
     1 YIELD(4),RATIO(2),DEF78(3),NOKNOW(3)
      DATA COSINE/'Cosi','ne  '/
      DATA COSCM/'Cosi','ne-C','M   '/
      DATA LORDER/'Lege','ndre',' Ord','er'/
      DATA ATWT/'Atom','ic W','eigh','t   '/
      DATA YIELD/'Fiss','ion ','Yiel','d'/
      DATA CMSYS/'C'/
      DATA COEFF/'Coef','fice','nts '/
      DATA COEFCM/'Coef','fice','nts-','CM  '/
      DATA RATIO/'Rati','o'/
      DATA NOKNOW/'Unno','rmal','ized'/
C
C     SELECT X AND Y UNITS. DEFINE AXIS LABELS AND MULTIPLIERS.
C
      IF(MF.EQ.3.OR.MF.EQ.5) GO TO 140
      IF(MF.EQ.6.AND.IDOUB.EQ.2) GO TO 140
C ----USE X STANDARD UNITS.
      XMULT=1.0
C ----DEFINE ANGULAR DISTRIBUTION X LABEL.
      IF(MF.NE.4.AND.MF.NE.6) GO TO 50
C
C     ANGULAR DISTRIBUTION (SIMPLE OR DOUBLE DIFFERENTIAL).
C
C ----DEFINE EITHER COSINE OR COSINE-CM
      IF(LABCM.EQ.CMSYS) GO TO 10
      IXLAB=6
      XLABEL(1)=COSINE(1)
      XLABEL(2)=COSINE(2)
      GO TO 20
   10 IXLAB=9
      XLABEL(1)=COSCM(1)
      XLABEL(2)=COSCM(2)
      XLABEL(3)=COSCM(3)
C ----DEFINE Y UNITS AND MULTIPLIER.
   20 CALL CSUNIT(YLIM,YMULT,YLABEL,IYLAB,IWAY(2))
C ----ADD TO UNITS IF REQUIRED.
      CALL PERUN(YLABEL,IYLAB,MF,IDOUB)
C ----IF RUNNING ON IBM-PC LABEL X AXIS EVERY 0.5 AND DRAW LINE EVERY
C ----0.25
      IXSTEP=1
      IF(IBMPC.LE.0) GO TO 30
      XBASE=-1.5
      XSTEP1=0.5
      XSTEP2=0.25
      GO TO 40
C ----LABEL X AXIS EVERY 0.2 AND DRAW LINE EVERY 0.1.
   30 XBASE=-1.2
      XSTEP1=0.2
      XSTEP2=0.1
C ----DEFINE Y AXIS LABEL INCREMENTS.
   40 CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))
      GO TO 200
C ----USE STANDARD Y UNITS.
   50 YMULT=1.0
C ----DEFINE LEGENDRE ORDER X AND Y LABELS.
      IF(MF.NE.7) GO TO 110
C
C     LEGENDRE COEFFICIENTS.
C
C ----DEFINE LEGENDRE ORDER.
      IXLAB=14
      DO 60 I=1,4
   60 XLABEL(I)=LORDER(I)
C ----DEFINE EITHER COEFFICIENTS OR COEFFICIENTS-CM
      IF(LABCM.EQ.CMSYS) GO TO 80
      IYLAB=11
      DO 70 J=1,3
   70 YLABEL(J)=COEFF(J)
      GO TO 100
   80 IYLAB=14
      DO 90 J=1,4
   90 YLABEL(J)=COEFCM(J)
C ----ONLY LABEL X AXIS AND DRAW LINES AT LEGENDRE ORDERS.
  100 IXSTEP=-1
      XBASE=-1.0
      XSTEP1=1.0
      XSTEP2=1.0
C ----DEFINE Y AXIS LABEL INCREMENTS.
      CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))
      GO TO 200
C ----DEFINE FISSION YIELD X AND Y LABELS.
  110 IF(MF.NE.8) GO TO 200
C
C     FISSION YIELD.
C
C ----DEFINE ATOMIC WEIGHT
      IXLAB=13
      DO 120 I=1,4
  120 XLABEL(I)=ATWT(I)
C ----DEFINE FISSION YIELD.
      IYLAB=13
      DO 130 I=1,4
  130 YLABEL(I)=YIELD(I)
C ----DEFINE X AXIS LABEL INCREMENTS.
      CALL SPACER(XLIM,XBASE,XSTEP1,XSTEP2,IXSTEP,IWAY(1))
C ----DEFINE Y AXIS LABEL INCREMENTS.
      CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))
      GO TO 200
C
C     CROSS SECTION VS. ENERGY.
C
C ----DEFINE ENERGY UNITS AND MULTIPLIER.
  140 CALL EUNIT(XLIM(2),XMULT,XLABEL,IXLAB,4,IPTZ,IWAY(1))
C ----IF SECONDARY ENERGY PRECEED X AXIS TITLE BY DEFINITION OF FIELD 7.
      IF(MF.EQ.5) GO TO 150
      IF(MF.NE.3.AND.MF.NE.6) GO TO 160
      IF(IDOUB.NE.2) GO TO 160
C ----DEFINE FIELD 7.
  150 CALL WHAT78(IM78,DEF78,IDEF78)
C ----COMBINE FIELD 7 DEFINITION AND X AXIS LABEL.
      CALL PAKZAP(DEF78,IDEF78,XLABEL,IXLAB)
  160 XLIM(1)=XLIM(1)*XMULT
      XLIM(2)=XLIM(2)*XMULT
C ----FOR RATIO OF RESONANCE PARAMETERS DEFINE Y LABEL AND USE STANDARD
C ----MULTIPLIER 1.0
      IF(MFIN.EQ.402.AND.MT.GE.6050) GO TO 170
C ----FOR RATIO DEFINE Y LABEL AND USE STANDARD MULTIPLIER 1.0
      IF(MFIN.NE.203) GO TO 180
  170 IYLAB=5
      YLABEL(1)=RATIO(1)
      YLABEL(2)=RATIO(2)
      YMULT=1.0
      GO TO 190
C ----DEFINE Y UNITS AND MULTIPLIER.
  180 CALL CSUNIT(YLIM,YMULT,YLABEL,IYLAB,IWAY(2))
C ----ADD TO UNITS IF REQUIRED.
      CALL PERUN(YLABEL,IYLAB,MF,IDOUB)
C ----DEFINE X AXIS LABEL INCREMENTS.
  190 CALL SPACER(XLIM,XBASE,XSTEP1,XSTEP2,IXSTEP,IWAY(1))
C ----DEFINE Y AXIS LABEL INCREMENTS.
      CALL SPACER(YLIM,YBASE,YSTEP1,YSTEP2,IYSTEP,IWAY(2))
C
C     SAVE LIMITS IN THE PLANE OF THE DATA.
C
  200 IF(IWAY(1).EQ.1) GO TO 210
      XREAL(1)=10.0**XLIM(1)
      XREAL(2)=10.0**XLIM(2)
      GO TO 220
  210 XREAL(1)=XLIM(1)/XMULT
      XREAL(2)=XLIM(2)/XMULT
  220 IF(IWAY(2).EQ.1) GO TO 230
      YREAL(1)=10.0**YLIM(1)
      YREAL(2)=10.0**YLIM(2)
      GO TO 240
  230 YREAL(1)=YLIM(1)/YMULT
      YREAL(2)=YLIM(2)/YMULT
C ----IF USING EXFOR TO SCALE SAVE ROUNDED PLOT LIMITS.
  240 IF(ISCALE.EQ.1) GO TO 250
      X4LIMX(1)=XREAL(1)
      X4LIMX(2)=XREAL(2)
      X4LIMY(1)=YREAL(1)
      X4LIMY(2)=YREAL(2)
C ----IF STATUS IS UNNORMALIZED CHANGE Y AXIS LABEL.
  250 IF(IMNORM.NE.1) GO TO 270
      DO 260 I=1,3
  260 YLABEL(I)=NOKNOW(I)
      IYLAB=12
  270 RETURN
      END
      SUBROUTINE WHAT78(IM78,DEF78,IDEF78)
C
C     DEFINE FIELDS 7-8.
C
      CHARACTER*4 TABA78,TABB78,IM78,DEF78
      DIMENSION TABA78(8),TABB78(3,8),ITAB78(8),DEF78(3)
      DATA TABA78/' E2','LVL','EXC',' HL','DE2','DLV','MIN','MAX'/
      DATA ITAB78/2,5,10,9,2,5,6,6/
      DATA TABB78/
     1 'E2  ','    ','    ',
     2 'Leve','l   ','    ',
     3 'Exci','tati','on  ',
     4 'Half','-Lif','e   ',
     5 'E2  ','    ','    ',
     6 'Leve','l   ','    ',
     7 'E2-M','in  ','    ',
     8 'E2-M','ax  ','    '/
      DO 10 I=1,8
      IF(IM78.EQ.TABA78(I)) GO TO 20
   10 CONTINUE
      I=1
   20 IDEF78=ITAB78(I)
      DO 30 K=1,3
   30 DEF78(K)=TABB78(K,I)
      RETURN
      END
      SUBROUTINE SPACER(ZLIM,ZBASE,ZSTEP1,ZSTEP2,IZSTEP,IZWAY)
C
C     DEFINE LINEAR X OR Y AXIS LABEL INCREMENTS.
C     USE FEWER AXIS INCREMENTS IF RUNNING ON IBM-PC.
C
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC
      DIMENSION ZLIM(2),STEPS(13),ISTEPS(13)
      DATA STEPS/100.,50.,20.,10.,5.,2.,1.,.5,.2,.1,.05,.02,.01/
      DATA ISTEPS/-1,-1,-1,-1,-1,-1,-1,1,1,1,2,2,2/
      IF(IZWAY.EQ.2) RETURN
      DZLIM=ZLIM(2)-ZLIM(1)
      INEED=10
      IF(ZLIM(2).GE.100.0) INEED=5
C ----FEWER GRID LINES IF HALF SIZE PLOTS ON IBM-PC.
      IF(IBMPC.GT.0) INEED=5
      IF(IBMPC.GT.0.AND.ZLIM(2).GE.100.0) INEED=4
      DO 10 I=1,13
      IF(I.EQ.7) INEED=5
      IF(I.EQ.7.AND.IBMPC.GT.0) INEED=4
      IF(I.EQ.13) GO TO 20
      J=DZLIM/STEPS(I)
      K=DZLIM/STEPS(I+1)
      IF(K.GE.2*INEED) GO TO 20
      IF(J.GE.INEED) GO TO 20
   10 CONTINUE
      I=13
   20 ZSTEP1=STEPS(I)
      IZBASE=ZLIM(1)/ZSTEP1
      ZBASE=FLOAT(IZBASE)*ZSTEP1
      IZSTEP=ISTEPS(I)
      TWICE=1.0
      IF(J.LT.10) TWICE=2.0
      ZSTEP2=ZSTEP1/TWICE
      RETURN
      END
      SUBROUTINE BORDER
C
C     PLOT BORDER AND ALL AXIS LABELS.
C
      CHARACTER*4 ZABCD,MSTAT1,MSTAT2,XLABEL,YLABEL,LIBNAM,MTBCD,MFBCD,
     1 STATAB,COS1,EN2,UNITO,DATAO,REFS,REF1,ZUNIT,RATIO,ZAPBCD,MSTATP,
     2 BLANK,UNDEFI,EN1,IM78,DEF78,HL
       CHARACTER*4 COSA
      CHARACTER*1 DIGITS,LABCM,STATUS,STAT1,MSTAR1,MSTAR2
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,EINC,AWR
      COMMON/MODEMY/MYMODE
      COMMON/WHEREC/ZABCD(4),MSTAT1,MSTAT2
      COMMON/PLTSIZ/SIZPLT,IPLOTZ,IBMPC
      COMMON/WHERE2/IZABCD
      COMMON/LIBI/ILIB
      COMMON/LIBC/LIBNAM(4)
      COMMON/WAYS/IWAY(2)
      COMMON/WHO78C/IM78
      COMMON/WHO78I/IMAM78
      COMMON/REFERC/REFS(9,48),REF1(9)
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP
      COMMON/XYLABC/XLABEL(10),YLABEL(10)
      COMMON/INCHES/XINCH(2),YINCH(2)
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
      COMMON/LOGTAB/TABLOG(10)
      COMMON/PLOTN/NUMPLT,ADVANC
      COMMON/SYSSTA/LABCM,STATUS
      COMMON/DOUBL/IDOUB,FIELD4(4)
      COMMON/RATZA/IZARAT,MTRAT,MFIN
      COMMON/RATZAC/MSTAR1,MSTAR2
      COMMON/SPOTS/TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGHT1,RIGHT2,RIGHT3,
     1 RIGHT4,RIGHT5,RIGHT6,RIGHT7,RIGHT8
      DIMENSION MTBCD(20),MFBCD(8),DIGITS(9),STAT1(7),STATAB(3,7),
     1 COS1(2),ZUNIT(2),RATIO(2),ZAPBCD(20),UNDEFI(3),DEF78(3)
      DIMENSION COSA(4)
      DATA HL/' HL'/
      DATA BLANK/'    '/
      DATA COS1/'Degr','=   '/
      DATA COSA/'Angl','e in','tegr','ated'/
      DATA EN2/'E2 ='/
      DATA EN1/'E ='/
      DATA STAT1/'P','S','D','C','A','O','R'/
      DATA STATAB/
     1 ' Pre','limi','nary',
     2 '  Su','perc','eded',
     3 '   D','epen','dent',
     4 '  Co','rrel','ated',
     5 '    ','Appr','oved',
     6 '    ','Outd','ated',
     7 'Reno','rmal','ized'/
      DATA DATAO/'Data'/
      DATA UNDEFI/'Unde','fine','d'/
      DATA RATIO/'Rati','o'/
      DATA DIGITS/'1','2','3','4','5','6','7','8','9'/
C ----SELECT BLACK PEN FOR ALL BORDER INFORMATON.
      CALL PEN(2)
C ----INCREMENT PLOT COUNT.
      NUMPLT=NUMPLT+1
C ----DEFINE TRUE CENTER OF PLOT.
      XMID=0.5*(XINCH(1)+XINCH(2))
      YMID=0.5*(YINCH(1)+YINCH(2))
C ----DEFINE LOCATION OF X AXIS LABEL (DIFFERENT FOR LINEAR OR LOG).
      BOTX3=BOT3
      IF(IWAY(1).EQ.1) BOTX3=BOTX3+HT
      BOTX4=BOTX3-1.75*HT
C
C     IF NOT IDENTIFYING E2 IN OUTSIDE LEGEND BOX, IDENTIFY E2 AT TOP
C     OF PLOT (IF CROSS SECTIONS, ANGULAR DISTRIBUTIONS OR LEGENDRE
C     COEFFICIENTS).
C
      IF(KGROUP.GT.0) GO TO 40
      IF(MF.NE.3.AND.MF.NE.4.AND.MF.NE.7) GO TO 40
C ----PRINT INCIDENT OR SECONDARY ENERGY IN NORMAL FORM.
      XZ=XINCH(1)
      IF(MF.EQ.3.AND.IDOUB.EQ.2) GO TO 10
C ----IDENTIFY E2 (UNLESS E2 FIELD IS BLANK).
      IF(IMAM78.LE.0) GO TO 40
      CALL WHAT78(IM78,DEF78,IDEF78)
      CALL SYMBLH(XZ,BOTX4,HT,DEF78,0.0,IDEF78)
      XZ=XZ+FLOAT(IDEF78+1)*WT
      CALL SYMBLH(XZ,BOTX4,HT,'=',0.0,1)
      XZ=XZ+2.0*WT
      IF(IM78.NE.HL) GO TO 20
C ----SELECT HALF-LIFE UNITS.
      CALL HLUNIT(E2T(1),ZMULT,ZUNIT,IZUNIT,4,IPTZ)
      GO TO 30
C ----FOR CROSS SECTIONS IDENTIFY E.
   10 CALL SYMBLH(XZ,BOTX4,HT,EN1,0.0,4)
      XZ=XZ+4.0*WT
C ----SELECT ENERGY UNITS.
   20 CALL EUNIT(E2T(1),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)
C ----PRINT ENERGY OR HALF-LIFE IN NORMALIZED FORM.
   30 ENOUT=E2T(1)*ZMULT
      CALL NUMBRH(XZ,BOTX4,HT,ENOUT,0.0,IPTZ)
      XZ=XZ+7.0*WT
      CALL SYMBLH(XZ,BOTX4,HT,ZUNIT,0.0,IZUNIT)
C
C     SPECIAL TITLE FOR RATIO.
C
   40 IF(MFIN.NE.203) GO TO 70
C ----IDENTIFY AS RATIO.
      CALL SYMBLH(XINCH(1),TOP1,HT,RATIO,0.0,5)
C ----DEFINE NUMERATOR ZA AND MT. PACK TOGETHER AND PLOT.
      CALL MTHOL(MT,MTBCD,IMTBCD,MSTAT2)
      CALL ZAHOL(IZA,MSTAT1,ZAPBCD,KZAP)
      CALL PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)
      JMTBCD=IMTBCD
      CALL LEFTY(MTBCD,IMTBCD,KMTBCD,JMTBCD)
      XI1=XMID-WTH*FLOAT(KMTBCD)
      XIMIN=XI1
      CALL SYMBLH(XI1,TOP2,HT,MTBCD,0.0,IMTBCD)
C ----DEFINE DENOMINATOR ZA AND MT. PACK TOGETHER AND PLOT.
      IF(IZARAT.LE.0.OR.MTRAT.LE.0) GO TO 50
      CALL MTHOL(MTRAT,MTBCD,IMTBCD,MSTAR2)
      CALL ZAHOL(IZARAT,MSTAR1,ZAPBCD,KZAP)
      CALL PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)
      JMTBCD=IMTBCD
      CALL LEFTY(MTBCD,IMTBCD,KMTBCD,JMTBCD)
      XI=XMID-WTH*FLOAT(KMTBCD)
      IF(XI.LT.XIMIN) XIMIN=XI
      CALL SYMBLH(XI,TOP3,HT,MTBCD,0.0,IMTBCD)
      GO TO 60
C ----DENOMINATOR ZA AND/OR MT IF NOT DEFINED.
   50 XI=XMID-WTH*9.0
      IF(XI.LT.XIMIN) XIMIN=XI
      CALL SYMBLH(XI,TOP3,HT,UNDEFI,0.0,9)
C ----PLOT LINE BETWEEN NUMERATOR AND DENOMINATOR.
   60 XIMIN=XIMIN-WTH
      XIMAX=XMID+(XMID-XIMIN)
      YI3=TOP2-0.375*HT
      CALL PLOT(XIMIN,YI3,3)
      CALL PLOT(XIMAX,YI3,2)
      GO TO 110
C ----IDENTIFY MATERIAL.
   70 XZ=XINCH(1)
      CALL SYMBLH(XZ,TOP1,HT,ZABCD,0.0,IZABCD)
C ----IF DOUBLE DIFFERENTAIL PRINT SECONDARY ENERGY OR COSINE.
      IF(MF.NE.6) GO TO 90
      XZ=XINCH(1)
      IF(IDOUB.EQ.2) GO TO 80
C ----PRINT SECONDARY ENERGY IN NORMAL FORM.
      CALL SYMBLH(XZ,BOTX4,HT,EN2,0.0,4)
      XZ=XZ+5.0*WT
      CALL EUNIT(FIELD4(3),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)
      ENOUT=FIELD4(3)*ZMULT
      CALL NUMBRH(XZ,BOTX4,HT,ENOUT,0.0,IPTZ)
      XZ=XZ+7.0*WT
      CALL SYMBLH(XZ,BOTX4,HT,ZUNIT,0.0,IZUNIT)
      GO TO 90
C ----PRINT COSINE.
   80 IF(MFIN.EQ.5) GO TO 82
      CALL SYMBLH(XZ,BOTX4,HT,COS1,0.0,5)
      XZ=XZ+6.0*WT
      DEG=180.*ACOS(FIELD4(1))/3.14159654
      CALL NUMBRH(XZ,BOTX4,HT,DEG,0.0,0)
      GO TO 84
   82 CALL SYMBLH(XZ,BOTX4,HT,COSA,0.0,15)
   84 CONTINUE
C
C     IDENTIFY DATA TYPE AND REACTION.
C
   90 CALL MTHOL(MT,MTBCD,IMTBCD,MSTAT2)
      CALL MFHOL(MFIN,MT,MFBCD,IMFBCD)
C ----IDENTIFY REACTION.
      JMTBCD=IMTBCD
      CALL LEFTY(MTBCD,IMTBCD,KMTBCD,JMTBCD)
      XI=XMID-WTH*FLOAT(KMTBCD)
      CALL SYMBLH(XI,TOP2,HT,MTBCD,0.0,IMTBCD)
C ----IDENTIFY DATA TYPE.
      IF(MF.NE.3) GO TO 100
      JMFBCD=IMFBCD
      CALL LEFTY(MFBCD,IMFBCD,KMFBCD,JMFBCD)
      XI1=XMID-WTH*FLOAT(KMFBCD)
      CALL SYMBLH(XI1,TOP3,HT,MFBCD,0.0,IMFBCD)
      GO TO 110
C ----IF NOT CROSS SECTION PRECEED DATA TYPE BY ENERGY.
  100 CALL EUNIT(EINC,ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)
      JMFBCD=IMFBCD
      CALL LEFTY(MFBCD,IMFBCD,KMFBCD,JMFBCD)
      XI1=XMID-WTH*FLOAT(8+IZUNIT+KMFBCD)
      ENOUT=EINC*ZMULT
      CALL NUMBRH(XI1,TOP3,HT,ENOUT,0.0,IPTZ)
      XI1=XI1+7.0*WT
      CALL SYMBLH(XI1,TOP3,HT,ZUNIT,0.0,IZUNIT)
      XI1=XI1+WT*FLOAT(1+IZUNIT)
      CALL SYMBLH(XI1,TOP3,HT,MFBCD,0.0,IMFBCD)
C ----IDENTIFY ENDF/B OR EXFOR DATA.
  110 IF(MYMODE.LT.2) GO TO 120
      XZ=XINCH(2)-FLOAT(11+ILIB)*WT
      CALL SYMBLH(XZ,TOP1,HT,LIBNAM,0.0,ILIB)
      XZ=XZ+FLOAT(1+ILIB)*WT
      XMAT=MAT
      CALL NUMBRH(XZ,TOP1,HT,XMAT,0.0,-1)
      XZ=XZ+5.0*WT
      CALL SYMBLH(XZ,TOP1,HT,'Mod',0.0,3)
      XZ=XZ+4.0*WT
      XMAT=MODIZA
      CALL NUMBRH(XZ,TOP1,HT,XMAT,0.0,-1)
      GO TO 160
C ----IF ONLY ONE REFERENCE PRINT EXFOR ACCESSION/SUB-ACCESSION NUMBER.
  120 IF(MREF.GT.1.AND.KGROUP.EQ.0) GO TO 150
      XZ=XINCH(2)-WT*FLOAT(ILIB)
      CALL SYMBLH(XZ,TOP1,HT,LIBNAM,0.0,ILIB)
C ----PRINT STATUS.
      DO 130 I=1,7
      IF(STATUS.EQ.STAT1(I)) GO TO 140
  130 CONTINUE
      GO TO 160
  140 XZ1=XINCH(2)-12.0*WT
      CALL SYMBLH(XZ1,BOTX4,HT,STATAB(1,I),0.0,12)
      GO TO 160
  150 XZ=XINCH(2)-10.0*WT
      CALL SYMBLH(XZ,TOP1,HT,'EXFOR DATA',0.0,10)
C
C     PLOT BORDER FOR FIGURE
C
  160 CALL PLOTP(XLIM(2),YLIM(1),3)
      CALL PLOTP(XLIM(1),YLIM(1),2)
      CALL PLOTP(XLIM(1),YLIM(2),2)
      CALL PLOTP(XLIM(2),YLIM(2),2)
      CALL PLOTP(XLIM(2),YLIM(1),2)
C
C     PLOT X AXIS LABEL AND UNITS.
C
      JXLAB=IXLAB
      CALL LEFTY(XLABEL,IXLAB,KXLAB,JXLAB)
      XI=XMID-WTH*FLOAT(KXLAB)
      CALL SYMBLH(XI,BOTX3,HT,XLABEL,0.0,IXLAB)
      IF(IWAY(1).EQ.1) GO TO 240
C
C     PLOT X SCALE LOG10 DECADES.
C
      IXMIN=XLIM(1)
      IF(XLIM(1).LT.0.0) IXMIN=IXMIN-1
      IXMAX=XLIM(2)+1.0
      XDEC=XLIM(2)-XLIM(1)
      IF(XDEC.LE.10.0) GO TO 170
      ILOG1=10
      KLOG1=10
      GO TO 200
  170 IF(XDEC.LE.6.0) GO TO 180
      ILOG1=5
      KLOG1=5
      GO TO 200
  180 IF(XDEC.LE.3.0) GO TO 190
      ILOG1=2
      KLOG1=2
      GO TO 200
  190 ILOG1=2
      KLOG1=1
  200 DO 230 I=IXMIN,IXMAX
      XR=I
      IF(XR.LT.XLIM(1).OR.XR.GT.XLIM(2)) GO TO 210
      XI=((XR-XLIM(1))*XINCH(2)+(XLIM(2)-XR)*XINCH(1))/(XLIM(2)-XLIM(1))
      CALL SYMBLH(XI-WT,BOT2,HT,'10',0.0,2)
      CALL NUMBRH (XI+WT,BOT1,HT,XR,0.0,-1)
  210 IF(ILOG1.GT.9) GO TO 230
      DO 220 J=ILOG1,9,KLOG1
      XZ=XR+TABLOG(J)
      IF(XZ.LT.XLIM(1).OR.XZ.GT.XLIM(2)) GO TO 220
      XI=((XZ-XLIM(1))*XINCH(2)+(XLIM(2)-XZ)*XINCH(1))/(XLIM(2)-XLIM(1))
      CALL SYMBLH(XI-WT38,BOT1,HT34,DIGITS(J),0.0,1)
  220 CONTINUE
  230 CONTINUE
      GO TO 270
C
C     PLOT X SCALE LINEAR UNITS.
C
  240 XR=XBASE
  250 IF(XR.LT.XLIM(1)) GO TO 260
      IF(XR.GT.XLIM(2)) GO TO 270
      XI=((XR-XLIM(1))*XINCH(2)+(XLIM(2)-XR)*XINCH(1))/(XLIM(2)-XLIM(1))
      XI=XI-WTH
      IF(MF.EQ.4.AND.ABS(XR).LT.0.01) XR=0.0
      IF(MF.EQ.6.AND.IDOUB.EQ.1.AND.ABS(XR).LT.0.01) XR=0.0
      IF(XR.LT.0.0) XI=XI-WTH
      IF(XR.GE.10.0) XI=XI-WTH
      IF(XR.GE.100.0) XI=XI-WTH
      IF(IXSTEP.EQ.1) XI=XI-WT
      IF(IXSTEP.EQ.2) XI=XI-WTH
      IF(IXSTEP.EQ.3) XI=XI-WTH
      CALL NUMBRH (XI,BOT1,HT,XR,0.0,IXSTEP)
  260 XR=XR+XSTEP1
      GO TO 250
C
C     PLOT Y AXIS LABEL AND UNITS.
C
  270 JYLAB=IYLAB
      CALL LEFTY(YLABEL,IYLAB,KYLAB,JYLAB)
      YI=YMID-WTH*FLOAT(IYLAB)
      CALL SYMBLH(RIGHT2,YI,HT,YLABEL,90.0,IYLAB)
      IF(IWAY(2).EQ.1) GO TO 350
C
C     PLOT Y SCALE LOG10 DECADES.
C
      IYMIN=YLIM(1)
      IF(YLIM(1).LT.0.0) IYMIN=IYMIN-1
      IYMAX=YLIM(2)+1.0
      YDEC=YLIM(2)-YLIM(1)
      IF(YDEC.LE.10.0) GO TO 280
      ILOG1=10
      KLOG1=10
      GO TO 310
  280 IF(YDEC.LE.6.0) GO TO 290
      ILOG1=5
      KLOG1=5
      GO TO 310
  290 IF(YDEC.LE.3.0) GO TO 300
      ILOG1=2
      KLOG1=2
      GO TO 310
  300 ILOG1=2
      KLOG1=1
  310 DO 340 I=IYMIN,IYMAX
      YR=I
      IF(YR.LT.YLIM(1).OR.YR.GT.YLIM(2)) GO TO 320
      YI=((YR-YLIM(1))*YINCH(2)+(YLIM(2)-YR)*YINCH(1))/(YLIM(2)-YLIM(1))
      CALL SYMBLH(RIGHT1,YI,HT,'10',0.0,2)
      CALL NUMBRH (RIGHT1+2.0*WT,YI+HT,HT,YR,0.0,-1)
  320 IF(ILOG1.GT.9) GO TO 340
      DO 330 J=ILOG1,9,KLOG1
      YZ=YR+TABLOG(J)
      IF(YZ.LT.YLIM(1).OR.YZ.GT.YLIM(2)) GO TO 330
      YI=((YZ-YLIM(1))*YINCH(2)+(YLIM(2)-YZ)*YINCH(1))/(YLIM(2)-YLIM(1))
      CALL SYMBLH(RIGHT1,YI,HT34,DIGITS(J),0.0,1)
  330 CONTINUE
  340 CONTINUE
      GO TO 400
C
C     PLOT Y SCALE LINEAR UNITS.
C
  350 YR=YBASE
      YTOP=YR
  360 IF(YR.GT.YLIM(2)) GO TO 370
      YTOP=YR
      YR=YR+YSTEP1
      GO TO 360
  370 KT=1
      IF(YTOP.GE.10.0) KT=2
      IF(YTOP.GE.100.0) KT=3
      YR=YBASE
  380 IF(YR.LT.YLIM(1)) GO TO 390
      IF(YR.GT.YLIM(2)) GO TO 400
      YI=((YR-YLIM(1))*YINCH(2)+(YLIM(2)-YR)*YINCH(1))/(YLIM(2)-YLIM(1))
      KN=1
      IF(YR.GE.10.0) KN=2
      IF(YR.GE.100.0) KN=3
      KD=KT-KN+1
      XI=XINCH(2)+KD*WT
      CALL NUMBRH (XI,YI+HTH,HT,YR,0.0,IYSTEP)
  390 YR=YR+YSTEP1
      GO TO 380
C
C     PLOT BORDERS FOR REFERENCES
C
  400 YR1=YINCH(2)
      YR2=YINCH(2)-1.75*(MREF+2)*HT
      IF(KGROUP.NE.0) YR2=YR2-1.75*HT
      CALL SYMBLH(RIGHT5,YR1-1.75*HT,HT,'Reference',0.0,9)
      IF(KGROUP.EQ.0) GO TO 410
      CALL LEFTY(REFS(1,1),IRR,KRR,25)
      CALL SYMBLH(RIGHT5,YR1-3.5*HT,HT,REFS(1,1),0.0,IRR)
  410 CALL PLOT(RIGHT3,YR1,3)
      CALL PLOT(RIGHT4,YR1,2)
      CALL PLOT(RIGHT4,YR2,2)
      CALL PLOT(RIGHT3,YR2,2)
      CALL PLOT(RIGHT3,YR1,2)
C ----SUPPRESS RANGE BOX PLOT IF TOO MANY REFERENCES
      IF(YINCH(2)-YINCH(1).LT. 1.75*(MREF+2)*HT*2) RETURN
      YR1=YINCH(1)
      YR2=YINCH(1)+1.75*(MREF+2)*HT
      YR3=YR2-1.75*HT
      IF(MF.EQ.7.OR.MF.EQ.8) GO TO 420
      IF(MF.EQ.4) GO TO 430
      IF(MF.EQ.6.AND.IDOUB.EQ.1) GO TO 430
      CALL SYMBLH(RIGHT5,YR3,HT,'Energy Range',0.0,12)
      GO TO 440
  420 CALL SYMBLH(RIGHT5,YR3,HT,'Range',0.0,5)
      GO TO 440
  430 CALL SYMBLH(RIGHT5,YR3,HT,'Cosine Range',0.0,12)
  440 CALL SYMBLH(RIGHT6,YR3,HT,'Points',0.0,6)
      CALL PLOT(RIGHT3,YR1,3)
      CALL PLOT(RIGHT4,YR1,2)
      CALL PLOT(RIGHT4,YR2,2)
      CALL PLOT(RIGHT3,YR2,2)
      CALL PLOT(RIGHT3,YR1,2)
      RETURN
      END
      SUBROUTINE EUNIT(Z,ZMULT,ZUNIT,IZUNIT,LPTZ,IPTZ,IZWAY)
C
C     DEFINE MULTIPLIER AND UNITS TO PUT ENERGY IN NORMAL FORM AND
C     NUMBER OF DECIMAL PLACES TO OUTPUT.
C
      CHARACTER*4 ZUNIT,UNTAB
      DIMENSION ZUNIT(2),RANGER(6),IUNIT(6),UNTAB(2,6)
C ----DEFINE MULTIPLIERS, LENGTH OF UNIT TITLE AND UNIT TITLES.
      DATA RANGER/1.0E+9,1.0E+6,1.0E+3,1.0,1.0E-3,1.0E-6/
      DATA IUNIT/3,3,3,2,8,8/
      DATA UNTAB/
     1 'GeV ','    ',
     2 'MeV ','    ',
     3 'KeV ','    ',
     4 'eV  ','    ',
     5 'mill','i-eV',
     6 'micr','o-eV'/
C ----USE EV FOR LOG SCALING OR ZERO VALUES.
      IF(IZWAY.EQ.1.AND.ABS(Z).NE.0.0) GO TO 10
      I=4
      GO TO 30
C ----SELECT MULTIPLIER.
   10 DO 20 I=1,6
      IF(Z.GE.RANGER(I)) GO TO 30
   20 CONTINUE
      I=6
C ----DEFINE UNITS AND MULTIPLIER.
   30 ZUNIT(1)=UNTAB(1,I)
      ZUNIT(2)=UNTAB(2,I)
      IZUNIT=IUNIT(I)
      ZMULT=1.0/RANGER(I)
C ----DEFINE NUMBER OF DECIMAL PLACES TO OUTPUT.
      ZX=Z*ZMULT
      IPTZ=LPTZ
      IF(ZX.GE.9.9999) IPTZ=IPTZ-1
      IF(ZX.GE.99.999) IPTZ=IPTZ-1
      IF(ZX.GE.999.99) IPTZ=IPTZ-1
      IF(IPTZ.LT.-1) IPTZ=-1
      RETURN
      END
      SUBROUTINE HLUNIT(HL,HLMULT,HLOUT,IHLOUT,LPTZ,IPTZ)
C
C     DEFINE CONVERSION FACTORS FROM SECONDS TO VARIABLE UNITS.
C
      CHARACTER*4 HLTAB,HLOUT
      DIMENSION HLTAB(2,9),HLTIME(9),IHLTAB(9),HLOUT(2)
      DATA IHLTAB/4,4,5,4,3,3,4,3,4/
      DATA HLTAB/
     1 'psec','    ',
     2 'nsec','    ',
     3 'muse','c   ',
     4 'msec','    ',
     5 'sec ','    ',
     6 'min ','    ',
     7 'hour','    ',
     8 'day ','    ',
     9 'year','    '/
      DATA HLTIME/
     1 1.00000E-12,
     2 1.00000E- 9,
     3 1.00000E- 6,
     4 1.00000E- 3,
     5 1.00000E+ 0,
     6 6.00000E+ 1,
     7 3.60000E+ 2,
     8 8.64000E+ 3,
     9 3.15576E+ 6/
      HL=ABS(HL)
C ----IF HALF-LIFE IS ZERO USE SECONDS.
      IF(HL.GT.0.0) GO TO 10
      I=5
      GO TO 30
C ----SELECT APPROPRIATE UNITS (WITH NON-ZERO LEADING DIGIT).
   10 I=9
      DO 20 J=1,9
      IF(HL.GT.HLTIME(I)) GO TO 30
   20 I=I-1
      I=1
C ----DEFINE UNITS AND MULTIPLIER.
   30 HLOUT(1)=HLTAB(1,I)
      HLOUT(2)=HLTAB(2,I)
      IHLOUT=IHLTAB(I)
      HLMULT=1.0/HLTIME(I)
C ----DEFINE NUMBER OF DIGITS AFTER DECIMAL POINT.
      ZHL=HL*HLMULT
      IPTZ=LPTZ
      ZMAX=9.9999
   40 IF(IPTZ.LT.0) GO TO 50
      IF(ZHL.LT.ZMAX) GO TO 50
      IPTZ=IPTZ-1
      ZMAX=10.0*ZMAX
      GO TO 40
   50 RETURN
      END
      SUBROUTINE CSUNIT(YLIM,YMULT,YLABEL,IYLAB,IYWAY)
C
C     SELECT UNITS Y. DEFINE Y AXIS LABEL AND SCALE FACTOR.
C
      CHARACTER*4 YLABEL,YUNIT
      DIMENSION YLIM(2),YLABEL(10),YUNIT(3,6),RANGER(6),IYUNIT(6)
      DATA RANGER/1.0E+9,1.0E+6,1.0E+3,1.0,1.0E-3,1.0E-6/
      DATA IYUNIT/10,10,10,5,11,11/
      DATA YUNIT/
     1 'Gega','-Bar','ns  ',
     2 'Mega','-Bar','ns  ',
     3 'Kilo','-Bar','ns  ',
     4 'Barn','s   ','    ',
     5 'mill','i-Ba','rns ',
     6 'micr','o-Ba','rns '/
C ----USE BARNS FOR LOG SCALING.
      IF(IYWAY.EQ.1) GO TO 10
      I=4
      GO TO 30
C ----LINEAR SCALING...USE VARIABLE UNITS.
   10 DO 20 I=1,6
      IF(YLIM(2).GE.RANGER(I)) GO TO 30
   20 CONTINUE
      I=6
   30 YMULT=1.0/RANGER(I)
      YLIM(1)=YLIM(1)*YMULT
      YLIM(2)=YLIM(2)*YMULT
      IYLAB=IYUNIT(I)
      DO 40 J=1,3
   40 YLABEL(J)=YUNIT(J,I)
      RETURN
      END
      SUBROUTINE PERUN(YLABEL,IYLAB,MF,IDOUB)
C
C     IF NOT MF=3 ADD APPROPRIATE UNITS TO Y AXIS UNITS.
C
      CHARACTER*1 YLABEL,PEREV,PERST,PEREST
      DIMENSION YLABEL(40),PEREV(3),PERST(10),PEREST(13)
      DATA PEREV/'/','e','V'/
      DATA PERST/'/','s','t','e','r','a','d','i','a','n'/
      DATA PEREST/'/','e','V','/','s','t','e','r','a','d','i','a','n'/
      IF(MF.EQ.4) GO TO 10
      IF(MF.EQ.5) GO TO 30
      IF(MF.EQ.6) GO TO 50
      RETURN
C ----ADD /STERADIAN.
   10 DO 20 I=1,10
      IYLAB=IYLAB+1
   20 YLABEL(IYLAB)=PERST(I)
      RETURN
C ----ADD /EV.
   30 DO 40 I=1,3
      IYLAB=IYLAB+1
   40 YLABEL(IYLAB)=PEREV(I)
      RETURN
C ----ADD /EV/STERADIAN.
   50 DO 60 I=1,13
      IYLAB=IYLAB+1
   60 YLABEL(IYLAB)=PEREST(I)
      RETURN
      END
      SUBROUTINE GRID0
C
C     PLOT X AND Y AXIS TICK MARKS.
C
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/WAYS/IWAY(2)
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP
      COMMON/LOGTAB/TABLOG(10)
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
      COMMON/INCHES/XINCH(2),YINCH(2)
      CALL PEN(2)
      YR1=YLIM(1)
      BOX2P=BOX2*(YLIM(2)-YLIM(1))/(YINCH(2)-YINCH(1))
      YR2=YR1+BOX2P
      IF(IWAY(1).EQ.1) GO TO 90
C
C     PLOT X SCALE LOG10 DECADES.
C
      DO 80 LOOP=1,2
      IXMIN=XLIM(1)
      IF(XLIM(1).LT.0.0) IXMIN=IXMIN-1
      IXMAX=XLIM(2)+1.0
      XDEC=XLIM(2)-XLIM(1)
      IF(XDEC.LE.10.0) GO TO 10
      ILOG1=10
      KLOG1=10
      GO TO 40
   10 IF(XDEC.LE.6.0) GO TO 20
      ILOG1=5
      KLOG1=5
      GO TO 40
   20 IF(XDEC.LE.3.0) GO TO 30
      ILOG1=2
      KLOG1=2
      GO TO 40
   30 ILOG1=2
      KLOG1=1
   40 DO 70 I=IXMIN,IXMAX
      XR=I
      IF(XR.LT.XLIM(1).OR.XR.GT.XLIM(2)) GO TO 50
      CALL PLOTP(XR,YR1,3)
      CALL PLOTP(XR,YR2,2)
      IF(ILOG1.GT.9) GO TO 70
   50 DO 60 J=ILOG1,9,KLOG1
      XZ=XR+TABLOG(J)
      IF(XZ.LT.XLIM(1).OR.XZ.GT.XLIM(2)) GO TO 60
      CALL PLOTP(XZ,YR1,3)
      CALL PLOTP(XZ,YR2,2)
   60 CONTINUE
   70 CONTINUE
      YR1=YLIM(2)
      YR2=YR1-BOX2P
   80 CONTINUE
      GO TO 140
C
C     PLOT X SCALE LINEAR UNITS.
C
   90 DO 130 LOOP=1,2
      XR=XBASE
  100 IF(XR.LT.XLIM(1)) GO TO 110
      IF(XR.GT.XLIM(2)) GO TO 120
      CALL PLOTP(XR,YR1,3)
      CALL PLOTP(XR,YR2,2)
  110 XR=XR+XSTEP2
      GO TO 100
  120 YR1=YLIM(2)
      YR2=YR1-BOX2P
  130 CONTINUE
  140 XR1=XLIM(1)
      BOX2P=BOX2*(XLIM(2)-XLIM(1))/(XINCH(2)-XINCH(1))
      XR2=XR1+BOX2P
      IF(IWAY(2).EQ.1) GO TO 230
C
C     PLOT Y SCALE LOG10 DECADES.
C
      DO 220 LOOP=1,2
      IYMIN=YLIM(1)
      IF(YLIM(1).LT.0.0) IYMIN=IYMIN-1
      IYMAX=YLIM(2)+1.0
      YDEC=YLIM(2)-YLIM(1)
      IF(YDEC.LE.10.0) GO TO 150
      ILOG1=10
      KLOG1=10
      GO TO 180
  150 IF(YDEC.LE.6.0) GO TO 160
      ILOG1=5
      KLOG1=5
      GO TO 180
  160 IF(YDEC.LE.3.0) GO TO 170
      ILOG1=2
      KLOG1=2
      GO TO 180
  170 ILOG1=2
      KLOG1=1
  180 DO 210 I=IYMIN,IYMAX
      YR=I
      IF(YR.LT.YLIM(1).OR.YR.GT.YLIM(2)) GO TO 190
      CALL PLOTP(XR1,YR,3)
      CALL PLOTP(XR2,YR,2)
  190 IF(ILOG1.GT.9) GO TO 210
      DO 200 J=ILOG1,9,KLOG1
      YZ=YR+TABLOG(J)
      IF(YZ.LT.YLIM(1).OR.YZ.GT.YLIM(2)) GO TO 200
      CALL PLOTP(XR1,YZ,3)
      CALL PLOTP(XR2,YZ,2)
  200 CONTINUE
  210 CONTINUE
      XR1=XLIM(2)
      XR2=XR1-BOX2P
  220 CONTINUE
      GO TO 280
C
C     PLOT Y SCALE LINEAR UNITS.
C
  230 DO 270 LOOP=1,2
      YR=YBASE
  240 IF(YR.LT.YLIM(1)) GO TO 250
      IF(YR.GT.YLIM(2)) GO TO 260
      CALL PLOTP(XR1,YR,3)
      CALL PLOTP(XR2,YR,2)
  250 YR=YR+YSTEP2
      GO TO 240
  260 XR1=XLIM(2)
      XR2=XR1-BOX2P
  270 CONTINUE
  280 RETURN
      END
      SUBROUTINE GRID1
C
C     PLOT X AND Y AXIS FULL GRID
C
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/WAYS/IWAY(2)
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP
      COMMON/LOGTAB/TABLOG(10)
      CALL PEN(1)
      IDIR=1
      IF(IWAY(1).EQ.1) GO TO 120
C
C     PLOT X SCALE LOG10 DECADES.
C
      IXMIN=XLIM(1)
      IF(XLIM(1).LT.0.0) IXMIN=IXMIN-1
      IXMAX=XLIM(2)+1.0
      XDEC=XLIM(2)-XLIM(1)
      IF(XDEC.LE.10.0) GO TO 10
      ILOG1=10
      KLOG1=10
      GO TO 40
   10 IF(XDEC.LE.6.0) GO TO 20
      ILOG1=5
      KLOG1=5
      GO TO 40
   20 IF(XDEC.LE.3.0) GO TO 30
      ILOG1=2
      KLOG1=2
      GO TO 40
   30 ILOG1=2
      KLOG1=1
   40 DO 110 I=IXMIN,IXMAX
      XR=I
      IF(XR.LT.XLIM(1).OR.XR.GT.XLIM(2)) GO TO 70
      IF(IDIR.EQ.1) GO TO 50
      CALL PLOTP(XR,YLIM(2),3)
      CALL PLOTP(XR,YLIM(1),2)
      GO TO 60
   50 CALL PLOTP(XR,YLIM(1),3)
      CALL PLOTP(XR,YLIM(2),2)
   60 IDIR=3-IDIR
   70 IF(ILOG1.GT.9) GO TO 110
      DO 100 J=ILOG1,9,KLOG1
      XZ=XR+TABLOG(J)
      IF(XZ.LT.XLIM(1).OR.XZ.GT.XLIM(2)) GO TO 100
      IF(IDIR.EQ.1) GO TO 80
      CALL PLOTP(XZ,YLIM(2),3)
      CALL PLOTP(XZ,YLIM(1),2)
      GO TO 90
   80 CALL PLOTP(XZ,YLIM(1),3)
      CALL PLOTP(XZ,YLIM(2),2)
   90 IDIR=3-IDIR
  100 CONTINUE
  110 CONTINUE
      GO TO 160
C
C     PLOT X SCALE LINEAR UNITS.
C
  120 XR=XBASE
  130 IF(XR.LT.XLIM(1)) GO TO 150
      IF(XR.GT.XLIM(2)) GO TO 160
      IF(IDIR.EQ.1) GO TO 140
      CALL PLOTP(XR,YLIM(2),3)
      CALL PLOTP(XR,YLIM(1),2)
      GO TO 150
  140 CALL PLOTP(XR,YLIM(1),3)
      CALL PLOTP(XR,YLIM(2),2)
  150 IDIR=3-IDIR
      XR=XR+XSTEP2
      GO TO 130
  160 IDIR=1
      IF(IWAY(2).EQ.1) GO TO 280
C
C     PLOT Y SCALE LOG10 DECADES.
C
      IYMIN=YLIM(1)
      IF(YLIM(1).LT.0.0) IYMIN=IYMIN-1
      IYMAX=YLIM(2)+1.0
      YDEC=YLIM(2)-YLIM(1)
      IF(YDEC.LE.10.0) GO TO 170
      ILOG1=10
      KLOG1=10
      GO TO 200
  170 IF(YDEC.LE.6.0) GO TO 180
      ILOG1=5
      KLOG1=5
      GO TO 200
  180 IF(YDEC.LE.3.0) GO TO 190
      ILOG1=2
      KLOG1=2
      GO TO 200
  190 ILOG1=2
      KLOG1=1
  200 DO 270 I=IYMIN,IYMAX
      YR=I
      IF(YR.LT.YLIM(1).OR.YR.GT.YLIM(2)) GO TO 230
      IF(IDIR.EQ.1) GO TO 210
      CALL PLOTP(XLIM(2),YR,3)
      CALL PLOTP(XLIM(1),YR,2)
      GO TO 220
  210 CALL PLOTP(XLIM(1),YR,3)
      CALL PLOTP(XLIM(2),YR,2)
  220 IDIR=3-IDIR
  230 IF(ILOG1.GT.9) GO TO 270
      DO 260 J=ILOG1,9,KLOG1
      YZ=YR+TABLOG(J)
      IF(YZ.LT.YLIM(1).OR.YZ.GT.YLIM(2)) GO TO 260
      IF(IDIR.EQ.1) GO TO 240
      CALL PLOTP(XLIM(2),YZ,3)
      CALL PLOTP(XLIM(1),YZ,2)
      GO TO 250
  240 CALL PLOTP(XLIM(1),YZ,3)
      CALL PLOTP(XLIM(2),YZ,2)
  250 IDIR=3-IDIR
  260 CONTINUE
  270 CONTINUE
      GO TO 320
C
C     PLOT Y SCALE LINEAR UNITS.
C
  280 YR=YBASE
  290 IF(YR.LT.YLIM(1)) GO TO 310
      IF(YR.GT.YLIM(2)) GO TO 320
      IF(IDIR.EQ.1) GO TO 300
      CALL PLOTP(XLIM(2),YR,3)
      CALL PLOTP(XLIM(1),YR,2)
      GO TO 310
  300 CALL PLOTP(XLIM(1),YR,3)
      CALL PLOTP(XLIM(2),YR,2)
  310 IDIR=3-IDIR
      YR=YR+YSTEP2
      GO TO 290
  320 RETURN
      END
      SUBROUTINE EVALP
C
C     PLOT EVALUATED DATA.
C
      PARAMETER (MXPNT=90000)
      COMMON/WAYS/IWAY(2)
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/XYREAL/XREAL(2),YREAL(2)
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP
      COMMON/PAGEXY/XPAGE(MXPNT),YPAGE(MXPNT),N2,IBASE,ITOP,ISCR
C
      IF(N2.LE.0) RETURN
      CALL PEN(2)
      IPASS=0
      KTOP=0
C
C     SET UP LOOP OVER POINTS.
C
      I=0
   10 I=I+1
C
C     SELECT NEXT POINT AND INITIALIZE FLAG TO INDICATE POINT IS NO PLOT
C
   20 XNOW=X(I)
      YNOW=Y(I)
      IMON=1
C
C     SELECT POINTS IN ENERGY RANGE OF PLOT.
C
      IF(XNOW.LT.XREAL(1)                 ) GO TO 140
      IF(YNOW.LT.YREAL(1) .AND. IPASS.EQ.0) GO TO 140
      IF(XNOW.LT.XREAL(2)) GO TO 70
C
C     END OF PLOTTING RANGE REACHED. IF NO PRECEDING POINTS (ALL DATA
C     ABOVE RANGE OF PLOT) RETURN
C
   40 IF(I.LE.1) GO TO 150
C
C     IF NO POINTS YET PLOTTED (I.E., LAST POINT BELOW PLOT RANGE,
C     CURRENT POINT ABOVE RANGE) RESET POINT INDEX TO DO UPPER LIMIT
C     NEXT TIME THROUGH LOOP. THIS TIME INTERPOLATE TO LOWER ENERGY
C     LIMIT AND TOP/BOTTOM OF PLOT.
C
      IF(IPASS.GT.0) GO TO 50
      I=I-1
      GO TO 80
C
C     SET FLAG TO INDICATE END OF PLOTTING RANGE.
C     INTERPOLATE TO UPPER ENERGY LIMIT AND TOP/BOTTOM OF PLOT.
C
   50 IF(XNOW.GT.XREAL(2)) IMON=0
      KTOP=1
      IF(XNOW.GT.XLAST)
     1YNOW=((XNOW-XREAL(2))*YLAST+(XREAL(2)-XLAST)*YNOW)/(XNOW-XLAST)
      XNOW=XREAL(2)
      IF(YNOW.LE.YREAL(2)) GO TO 60
      IMON=0
      IF(YNOW.LT.YLAST)
     1XNOW=((YNOW-YREAL(2))*XLAST+(YREAL(2)-YLAST)*XNOW)/(YNOW-YLAST)
      YNOW=YREAL(2)
      GO TO 110
   60 IF(YNOW.GE.YREAL(1)) GO TO 110
      IMON=0
      IF(YNOW.GT.YLAST)
     1XNOW=((YNOW-YREAL(1))*XLAST+(YREAL(1)-YLAST)*XNOW)/(YNOW-YLAST)
      YNOW=YREAL(1)
      GO TO 110
C
C     INTERPOLATE TO LOWER ENERGY LIMIT AND TOP/BOTTOM OF PLOT
C     UNLESS FIRST POINT OR AT LEAST ONE POINT HAS ALREADY BEEN PLOTTED.
C
   70 IF(I.LE.1.OR.IPASS.GT.0) GO TO 110
   80 IMON=0
      IF(XNOW.GT.XLAST)
     1YNOW=((XNOW-XREAL(1))*YLAST+(XREAL(1)-XLAST)*YNOW)/(XNOW-XLAST)
      XNOW=XREAL(1)
   90 IF(YNOW.LE.YREAL(2)) GO TO 100
      IMON=0
      IF(YNOW.LT.YLAST)
     1XN  =((YNOW-YREAL(2))*XLAST+(YREAL(2)-YLAST)*XNOW)/(YNOW-YLAST)
      XNOW=MAX(XN,XNOW)
      YNOW=YREAL(2)
      GO TO 110
  100 IF(YNOW.GE.YREAL(1)) GO TO 110
      IMON=0
      IF(YNOW.LT.YLAST)
     1XN  =((YNOW-YREAL(1))*XLAST+(YREAL(1)-YLAST)*XNOW)/(YNOW-YLAST)
      XNOW=MIN(XN,XNOW)
      YNOW=YREAL(1)
C
C     LIMIT X AND Y TO RANGE OF THE PLOT. CONVERT TO PLOT UNITS.
C
  110 IF(XNOW.LT.XREAL(1)) XNOW=XREAL(1)
      IF(XNOW.GT.XREAL(2)) XNOW=XREAL(2)
      IF(YNOW.LT.YREAL(1)) YNOW=YREAL(1)
      IF(YNOW.GT.YREAL(2)) YNOW=YREAL(2)
      XL=XNOW*XMULT
      YL=YNOW*YMULT
C
C     MOVE OR PLOT TO POINT.
C
      IF(IPASS.GT.0) GO TO 120
      CALL PLOTI(XL,YL,3,IMON)
      IPASS=1
C ----IF FIRST POINT USED FOR INTERPOLATION BRANCH BACK TO NOW TREAT
C ----FIRST POINT.
      IF(XNOW-X(I)) 20,130,130
C     IF(I-1) 35,35,3
  120 CALL PLOTI(XL,YL,2,IMON)
  130 IF(KTOP.GT.0) GO TO 150
C
C     SAVE COORDINATES FOR INTERPOLATION.
C
  140 XLAST=XNOW
      YLAST=YNOW
      IF(I.LT.N2) GO TO 10
  150 RETURN
      END
      SUBROUTINE EXFORP
C
C     PLOT EXPERIMENTAL DATA...EACH REF SEPARATELY.
C
      PARAMETER (MXPGP=10000)
      CHARACTER*4 HEADER,REFS,REF1,REFNUM,XLABEL,YLABEL,ZUNIT,IM78,HL,
     1 DEF78
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/XYREAL/XREAL(2),YREAL(2)
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,EINC,AWR
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
      COMMON/XYLABC/XLABEL(10),YLABEL(10)
      COMMON/WAYS/IWAY(2)
      COMMON/WHO78C/IM78
      COMMON/XYLABI/IXLAB,IYLAB,XMULT,YMULT,XBASE,XSTEP1,XSTEP2,
     1 YBASE,YSTEP1,YSTEP2,IXSTEP,IYSTEP
      COMMON/INCHES/XINCH(2),YINCH(2)
      COMMON/EXFOR/XEX(MXPGP),DXEX(MXPGP),YEX(MXPGP),DYEX(MXPGP)
     1,NREF(MXPGP),E2(MXPGP),IEX
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP
      COMMON/REFERC/REFS(9,48),REF1(9)
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
      COMMON/DOUBL/IDOUB,FIELD4(4)
      COMMON/SYMBLM/MSYMBL
      COMMON/X4LIMS/X4LIMX(2),X4LIMY(2)
      COMMON/SPOTS/TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGHT1,RIGHT2,RIGHT3,
     1 RIGHT4,RIGHT5,RIGHT6,RIGHT7,RIGHT8
      DIMENSION REFNUM(26),ZUNIT(2),DEF78(3)
      DATA HL/' HL'/
      DATA REFNUM/'1','2','3','4','5','6','7','8','9','A',
     1 'B','C','D','E','F','G','H','J','K','L',
     2 'M','N','P','R','S','X'/
C ----NOTHING TO DO IF NO EXFOR DATA.
      IF(IEX.LE.0) RETURN
C ----INITIALIZE LEGEND BOX COORDINATES.
      YNOW=YINCH(2)-3.5*HT
      IF(KGROUP.NE.0) YNOW=YNOW-1.75*HT
      YBOT=YINCH(1)+1.75*MREF*HT
C
C     SELECT REFERENCES IN ENERGY RANGE.
C
      DO 190 KREF=1,IREF
      IF(LREF(KREF).LE.0) GO TO 190
C
C     IDENTIFY REFERENCE.
C
      IPEN=KREF+2
      CALL PEN(IPEN)
      IF(MSYMBL.EQ.0.AND.MREF.EQ.1.AND.KGROUP.EQ.0) GO TO 50
C ----IDENTIFY SYMBOL USED FOR EACH REFERENCE.
      CALL PLOT (RIGHT7,YNOW-BOX,3)
      CALL PLOT (RIGHT8,YNOW-BOX,2)
      CALL PLOT (RIGHT8,YNOW+BOX,2)
      CALL PLOT (RIGHT7,YNOW+BOX,2)
      CALL PLOT (RIGHT7,YNOW-BOX,2)
      CALL SYMBLH(RIGHT7+BOX-BOXWT2,YNOW-BOX2,BOX,REFNUM(KREF),0.0,1)
      IF(KGROUP.EQ.0) GO TO 50
      IF(MF.NE.3) GO TO 10
      IF(IDOUB.EQ.2) GO TO 30
C ----IDENTIFY SECOND ENERGY.
   10 CALL WHAT78(IM78,DEF78,IDEF78)
      CALL SYMBLH(RIGHT5,YNOW-HTH,HT,DEF78,0.0,IDEF78)
      XNOW2=RIGHT5+FLOAT(IDEF78+1)*WT
      CALL SYMBLH(XNOW2,YNOW-HTH,HT,'=',0.0,1)
      XNOW2=XNOW2+2.0*WT
C ----SELECT HALF-LIFE OR ENERGY UNITS.
      IF(IM78.NE.HL) GO TO 20
      CALL HLUNIT(E2T(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ)
      GO TO 40
   20 CALL EUNIT(E2T(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)
      GO TO 40
C ----IDENTIFY INCIDENT ENERGY.
   30 CALL EUNIT(E2T(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)
      CALL SYMBLH(RIGHT5,YNOW-HTH,HT,'E = ',0.0,4)
      XNOW2=RIGHT5+4.0*WT
C ----PLOT E2 FIELD AND UNITS.
   40 E2OUT=E2T(KREF)*ZMULT
      CALL NUMBRH(XNOW2,YNOW-HTH,HT,E2OUT,0.0,IPTZ)
      CALL SYMBLH(XNOW2+7.0*WT,YNOW-HTH,HT,ZUNIT,0.0,IZUNIT)
      GO TO 60
C ----IDENTIFY REFERENCE.
   50 CALL LEFTY(REFS(1,KREF),IRR,KRR,25)
      CALL SYMBLH(RIGHT5,YNOW-HTH,HT,REFS(1,KREF),0.0,IRR)
c...
c...       print *,iex,refs(1,kref),refs(2,kref)
c...
   60 IF(MSYMBL.EQ.0.AND.MREF.EQ.1.AND.KGROUP.EQ.0) GO TO 70
C ----SUPPRESS RANGE BOX PLOT IF TOO MANY REFERENCES
      IF(YINCH(2)-YINCH(1).LT. 1.75*(MREF+2)*HT*2) GO TO 92
C ----IDENTIFY REFERENCE IN RANGE BOX.
      CALL PLOT (RIGHT7,YBOT-BOX,3)
      CALL PLOT (RIGHT8,YBOT-BOX,2)
      CALL PLOT (RIGHT8,YBOT+BOX,2)
      CALL PLOT (RIGHT7,YBOT+BOX,2)
      CALL PLOT (RIGHT7,YBOT-BOX,2)
      CALL SYMBLH(RIGHT7+BOX-BOXWT2,YBOT-BOX2,BOX,REFNUM(KREF),0.0,1)
C ----PLOT RANGE.
   70 IF(MF.EQ.3.OR.MF.EQ.5) GO TO 80
      IF(MF.EQ.6.AND.IDOUB.EQ.2) GO TO 80
C ----PRINT COSINE, LEGENDRE ORDER OR ATOMIC WEIGHT RANGE.
      IPTZ=-1
      IF(MF.EQ.4) IPTZ=4
      IF(MF.EQ.6.AND.IDOUB.EQ.1) IPTZ=4
      CALL NUMBRH(RIGHT5,YBOT-HTH,HT,EXLOW(KREF),0.0,IPTZ)
      XNOW2=RIGHT5+3.0*WT
      IF(IPTZ.EQ.4) XNOW2=XNOW2+5.0*WT
      IF(EXLOW(KREF).LE.0.0) XNOW2=XNOW2+WT
      IF(EXLOW(KREF).GT.10.0) XNOW2=XNOW2+WT
      IF(EXLOW(KREF).GT.100.0) XNOW2=XNOW2+WT
      CALL NUMBRH(XNOW2,YBOT-HTH,HT,EXHIGH(KREF),0.0,IPTZ)
      GO TO 90
C ----PRINT ENERGY IN NORMAL FORM.
   80 CALL EUNIT(EXHIGH(KREF),ZMULT,ZUNIT,IZUNIT,4,IPTZ,1)
      EXL=EXLOW(KREF)*ZMULT
      JPTX=4
      IF(EXL.GE.10.0) JPTX=3
      IF(EXL.GE.100.0) JPTX=2
      CALL NUMBRH(RIGHT5,YBOT-HTH,HT,EXL,0.0,JPTX)
      EXH=EXHIGH(KREF)*ZMULT
      CALL NUMBRH(RIGHT5+8.0*WT,YBOT-HTH,HT,EXH,0.0,IPTZ)
      CALL SYMBLH(RIGHT5+15.0*WT,YBOT-HTH,HT,ZUNIT,0.0,IZUNIT)
C ----PRINT POINT COUNT RIGHT ADJUSTED.
   90 XBOTX=RIGHT6
      XREF=LREF(KREF)
      IF(XREF.LT.100000.0) XBOTX=XBOTX+WT
      IF(XREF.LT.10000.0) XBOTX=XBOTX+WT
      IF(XREF.LT.1000.0) XBOTX=XBOTX+WT
      IF(XREF.LT.100.0) XBOTX=XBOTX+WT
      IF(XREF.LT.10.0) XBOTX=XBOTX+WT
      CALL NUMBRH(XBOTX,YBOT-HTH,HT,XREF,0.0,-1)
   92 CONTINUE
      YNOW=YNOW-1.75*HT
      YBOT=YBOT-1.75*HT
C
C     SELECT POINTS IN ENERGY RANGE AND PLOT THEM.
C
      DO 180 I=1,IEX
c...
c...       print *,i,xex(i),yex(i)
c...
C ----SELECT POINTS FROM CURRENT REFERENCE WHICH ARE WITHIN THE PLOTTING
C ----AREA.
      IF(NREF(I).NE.KREF) GO TO 180
      IF(XEX(I).LT.X4LIMX(1).OR.XEX(I).GT.X4LIMX(2)) GO TO 180
      IF(YEX(I).LT.X4LIMY(1).OR.YEX(I).GT.X4LIMY(2)) GO TO 180
C ----DEFINE X COORDINATES AND TRUNCATE TO LIMITS OF PLOT.
      XL=XEX(I)
      DXL=DXEX(I)
      XM=XL-DXL
      XP=XL+DXL
      IF(XM.LT.XREAL(1)) XM=XREAL(1)
      IF(XP.GT.XREAL(2)) XP=XREAL(2)
      XM=XM*XMULT
      XL=XL*XMULT
      XP=XP*XMULT
C ----IF REQUIRED CONVERT TO LOGS.
      IF(IWAY(1).EQ.1) GO TO 100
      XL=ALOG10(XL)
      XM=ALOG10(XM)
      XP=ALOG10(XP)
C ----TRANSFORM TO COORDINATES OF THE PLOT.
  100 XL=((XL-XLIM(1))*XINCH(2)+(XLIM(2)-XL)*XINCH(1))/(XLIM(2)-XLIM(1))
      XM=((XM-XLIM(1))*XINCH(2)+(XLIM(2)-XM)*XINCH(1))/(XLIM(2)-XLIM(1))
      XP=((XP-XLIM(1))*XINCH(2)+(XLIM(2)-XP)*XINCH(1))/(XLIM(2)-XLIM(1))
C ----DEFINE Y COORDINATES AND TRUNCATE TO LIMITS OF PLOT.
      YL=YEX(I)
      DYL=DYEX(I)
      YM=YL-DYL
      YP=YL+DYL
      IF(YM.LT.YREAL(1)) YM=YREAL(1)
      IF(YP.GT.YREAL(2)) YP=YREAL(2)
      YM=YM*YMULT
      YL=YL*YMULT
      YP=YP*YMULT
C ----IF REQUIRED CONVERT TO LOGS.
      IF(IWAY(2).EQ.1) GO TO 110
      YL=ALOG10(YL)
      YM=ALOG10(YM)
      YP=ALOG10(YP)
C ----TRANSFORM TO COORDINATES OF THE PLOT.
  110 YL=((YL-YLIM(1))*YINCH(2)+(YLIM(2)-YL)*YINCH(1))/(YLIM(2)-YLIM(1))
      YM=((YM-YLIM(1))*YINCH(2)+(YLIM(2)-YM)*YINCH(1))/(YLIM(2)-YLIM(1))
      YP=((YP-YLIM(1))*YINCH(2)+(YLIM(2)-YP)*YINCH(1))/(YLIM(2)-YLIM(1))
C ----IF ONLY ONE REFERENCE AND NOT IDENTIFYING E2 DO NOT PLOT SYMBOL.
      IF(MSYMBL.NE.0.OR.MREF.GT.1.OR.KGROUP.NE.0) GO TO 140
C
C     ONLY PLOT ERROR BARS AND X AT MIDDLE.
C
      XLBM=XL-BOX4
      XLBP=XL+BOX4
      YLBM=YL-BOX4
      YLBP=YL+BOX4
C ----PLOT X ERROR BARS.
      IF(DXL.LE.0) GO TO 120
      CALL PLOT(XM,YLBM,3)
      CALL PLOT(XM,YLBP,2)
      CALL PLOT(XM,YL,3)
      CALL PLOT(XP,YL,2)
      CALL PLOT(XP,YLBM,3)
      CALL PLOT(XP,YLBP,2)
C ----PLOT Y ERROR BARS.
  120 IF(DYL.LE.0.0) GO TO 130
      CALL PLOT(XLBM,YM,3)
      CALL PLOT(XLBP,YM,2)
      CALL PLOT(XL,YM,3)
      CALL PLOT(XL,YP,2)
      CALL PLOT(XLBM,YP,3)
      CALL PLOT(XLBP,YP,2)
C ----PLOT X AT DATA POINT.
  130 CALL PLOT(XLBM,YLBP,3)
      CALL PLOT(XLBP,YLBM,2)
      CALL PLOT(XLBM,YLBM,3)
      CALL PLOT(XLBP,YLBP,2)
      GO TO 180
C
C     PLOT SYMBOL AND ERROR BARS.
C
  140 XLBM=XL-BOX
      XLBP=XL+BOX
      YLBM=YL-BOX
      YLBP=YL+BOX
C ----PLOT BOX AND SYMBOL.
      CALL PLOT(XLBM,YLBM,3)
      CALL PLOT(XLBP,YLBM,2)
      CALL PLOT(XLBP,YLBP,2)
      CALL PLOT(XLBM,YLBP,2)
      CALL PLOT(XLBM,YLBM,2)
      CALL SYMBLH(XL-BOXWT2,YL-BOX2,BOX,REFNUM(KREF),0.0,1)
C ----PLOT X ERROR BARS IF THEY EXTEND BEYOND BOX.
      IF(XM.GE.XLBM) GO TO 150
      CALL PLOT(XM,YLBM,3)
      CALL PLOT(XM,YLBP,2)
      CALL PLOT(XM,YL,3)
      CALL PLOT(XLBM,YL,2)
  150 IF(XP.LE.XLBP) GO TO 160
      CALL PLOT(XP,YLBM,3)
      CALL PLOT(XP,YLBP,2)
      CALL PLOT(XP,YL,3)
      CALL PLOT(XLBP,YL,2)
C ----PLOT Y ERROR BARS IF THEY EXTEND BEYOND BOX.
  160 IF(YM.GE.YLBM) GO TO 170
      CALL PLOT(XLBM,YM,3)
      CALL PLOT(XLBP,YM,2)
      CALL PLOT(XL,YM,3)
      CALL PLOT(XL,YLBM,2)
  170 IF(YP.LE.YLBP) GO TO 180
      CALL PLOT(XLBM,YP,3)
      CALL PLOT(XLBP,YP,2)
      CALL PLOT(XL,YP,3)
      CALL PLOT(XL,YLBP,2)
  180 CONTINUE
  190 CONTINUE
      RETURN
      END
      SUBROUTINE PLOTP(X,Y,IPEN)
C
C     TRANSLATE DATA FROM THE PLANE OF THE DATA TO THE PLANE OF THE
C     PLOT.
C
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/INCHES/XINCH(2),YINCH(2)
      XI=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))
      YI=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))
      CALL PLOT(XI,YI,IPEN)
      RETURN
      END
      SUBROUTINE PLOTI(XP,YP,IPEN,IMON)
C
C     TRANSLATE DATA FROM THE PLANE OF THE DATA TO THE PLANE OF THE
C     PLOT. IF NECESSARY INTERPOLATE DATA TO NON-LINEAR PLOT.
C
      COMMON/XYLIM/XLIM(2),YLIM(2)
      COMMON/INCHES/XINCH(2),YINCH(2)
      COMMON/WAYS/IWAY(2)
      COMMON/ENDFIM/IMENDF
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
C...
      SAVE XPLAST,YPLAST,XILAST,YILAST
C...
      DATA XPLAST/0.0/
      DATA YPLAST/0.0/
      DATA XILAST/0.0/
      DATA YILAST/0.0/
C ----DEFINE COORDINATES FOR INTERNAL USE.
      X=XP
      Y=YP
C ----IF NECESSARY CONVERT TO LOG.
      IF(IWAY(1).EQ.2) X=ALOG10(X)
      IF(IWAY(2).EQ.2) Y=ALOG10(Y)
C ----CONVERT TO THE PLANE OF THE PLOT.
      XI=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))
      YI=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))
C ----IF MOVING, NOT PLOTTING MOVE (NO INTERPOLATION NECESSARY).
      IF(IPEN.EQ.3) GO TO 120
C ----IF PLOT IS LINEAR X VS. LINEAR Y DRAW STRAIGHT LINE.
      IF(IWAY(1).EQ.1.AND.IWAY(2).EQ.1) GO TO 120
C ----CHECK FOR INTERPOLATION.
      DXINCH=XI-XILAST
      DYINCH=YI-YILAST
      ABSDX=ABS(DXINCH)
      ABSDY=ABS(DYINCH)
C ----IF NO CHANGE IN ONE COORDINATE DRAW STRIAGHT LINE.
      IF(ABSDX.LE.0.0.OR.ABSDY.LE.0.0) GO TO 120
C ----CHECK FOR LARGEST CHANGE IN INCHES. IF LESS THAN 0.1 DO NOT
C ----INTERPOLATE.
      IF(ABSDX.LT.ABSDY) GO TO 60
      IF(ABSDX.LE.0.1) GO TO 120
C ----INTERPOLATE IN X DIRECTION. DEFINE LINEAR OR LOG STEP CHANGE IN X.
      ISTEP=ABSDX/0.1+1.0
      XSTEP=ISTEP
      XZ=XPLAST
      IF(IWAY(1).NE.1) GO TO 10
      DX=(XP-XPLAST)/XSTEP
      GO TO 20
   10 DX=LOG(XP/XPLAST)/XSTEP
      DX=EXP(DX)
C ----SET UP LOOP OVER INTERPOLATION STEPS.
   20 DO 50 I=1,ISTEP
      IF(I.EQ.ISTEP) GO TO 120
      IF(IWAY(1).NE.1) GO TO 30
      XZ=XZ+DX
      GO TO 40
   30 XZ=XZ*DX
   40 X=XZ
      Y=((X-XPLAST)*YP+(XP-X)*YPLAST)/(XP-XPLAST)
      IF(IWAY(1).EQ.2) X=ALOG10(X)
      IF(IWAY(2).EQ.2) Y=ALOG10(Y)
      XT=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))
      YT=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))
   50 CALL PLOT(XT,YT,2)
   60 IF(ABSDY.LE.0.1) GO TO 120
C ----INTERPOLATE IN Y DIRECTION. DEFINE LINEAR OR LOG STEP CHANGE IN Y.
      ISTEP=ABSDY/0.1+1.0
      YSTEP=ISTEP
      YZ=YPLAST
      IF(IWAY(2).NE.1) GO TO 70
      DY=(YP-YPLAST)/YSTEP
      GO TO 80
   70 DY=LOG(YP/YPLAST)/YSTEP
      DY=EXP(DY)
C ----SET UP LOOP OVER INTERPOLATION STEPS.
   80 DO 110 I=1,ISTEP
      IF(I.EQ.ISTEP) GO TO 120
      IF(IWAY(2).NE.1) GO TO 90
      YZ=YZ+DY
      GO TO 100
   90 YZ=YZ*DY
  100 Y=YZ
      X=((Y-YPLAST)*XP+(YP-Y)*XPLAST)/(YP-YPLAST)
      IF(IWAY(1).EQ.2) X=ALOG10(X)
      IF(IWAY(2).EQ.2) Y=ALOG10(Y)
      XT=((X-XLIM(1))*XINCH(2)+(XLIM(2)-X)*XINCH(1))/(XLIM(2)-XLIM(1))
      YT=((Y-YLIM(1))*YINCH(2)+(YLIM(2)-Y)*YINCH(1))/(YLIM(2)-YLIM(1))
  110 CALL PLOT(XT,YT,2)
C ----PLOT OR MOVE TO FINAL LOCATION.
  120 CALL PLOT(XI,YI,IPEN)
C ----IF REQUESTED IDENTIFY ENDF/B DATA POINT.
      IF(IMON.LE.0.OR.IMENDF.NE.2) GO TO 130
      CALL PLOT(XI-BOX4,YI,3)
      CALL PLOT(XI,YI+BOX4,2)
      CALL PLOT(XI+BOX4,YI,2)
      CALL PLOT(XI,YI-BOX4,2)
      CALL PLOT(XI-BOX4,YI,2)
      CALL PLOT(XI,YI,3)
C ----SAVE COORDINATES.
  130 XPLAST=XP
      YPLAST=YP
      XILAST=XI
      YILAST=YI
      RETURN
      END
      SUBROUTINE GETEX(IEND)
C
C     READ COMPARABLE EXFOR DATA BASED ON EITHER,
C     (1) COMPARISON TO ENDF/B IZA/MF/MT, OR,
C     (2) COMPARISON TO REQUESTS.
C
      PARAMETER (MXPGP=10000)
      PARAMETER (MXZAT= 1800)
      INTEGER OUTP
      CHARACTER*4 REFS,REF1,LIBNAM,ZABCD,MSTAT1,MSTA1X,FIELDC,BLANK,
     1 IM78,IM78X,MSTAT2,MSTA2X,HL
      CHARACTER*1 LABCM,LABX,STATUS,STATX,DUMMY,MSTAR1,MSTAR2,MSTA1R,
     1 MSTA2R,BLANK1,USTAT
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/LIBI/ILIB
      COMMON/LIBC/LIBNAM(4)
      COMMON/WHO78C/IM78
      COMMON/WHO78I/IMAM78
      COMMON/INPARM/MINNIE,MAXIE
      COMMON/WHEREI/IZA,MAT,MF,MT,MODIZA,EINC,AWR
      COMMON/WHEREC/ZABCD(4),MSTAT1,MSTAT2
      COMMON/EXFOR/XEX(MXPGP),DXEX(MXPGP),YEX(MXPGP),DYEX(MXPGP)
     1,NREF(MXPGP),E2(MXPGP),IEX
      COMMON/REFERI/LREF(48),EXLOW(48),EXHIGH(48),E2T(48),IREF,MREF,
     1 MAXREF,IGROUP,KGROUP
      COMMON/REFERC/REFS(9,48),REF1(9)
      COMMON/SYSSTA/LABCM,STATUS
      COMMON/UNNORM/IMNORM
      COMMON/DOUBL/IDOUB,FIELD4(4)
      COMMON/MODEMY/MYMODE
      COMMON/EXERRS/IXERR,IYERR
      COMMON/RATZA/IZARAT,MTRAT,MFIN
      COMMON/RATZAC/MSTAR1,MSTAR2
      DIMENSION FIELDI(8),FIELDC(3,8)
      DIMENSION MZATAB(9,MXZAT),ENINC(MXZAT),ENEXT(MXZAT)
C...
      SAVE
C...
      DATA FIELDC/'    ','    ','    ',
     2            '    ','    ','    ',
     3            '    ','    ','    ',
     4            '    ','    ','    ',
     5            '    ','    ','    ',
     6            '    ','    ','    ',
     7            '    ','    ','    ',
     8            '    ','    ','    '/
      DATA IPASS/0/
      DATA BLANK/'    '/
      DATA BLANK1/' '/
      DATA HL/' HL'/
      DATA IZATRY/0/
      DATA MTTRY/0/
      DATA MSTA1R/' '/
      DATA MSTA2R/' '/
      DATA USTAT/'U'/
C* Fraction tolerance for incident and secondary energies, respectively
      DATA ETOL,E2TOL/0.015, 0.003/
C* Tolerance fraction for numbers to be considered the same
      DATA SMALL/1.E-5/
C
C     WHEN THIS ROUTINE IS CALLED THE FIRST TIME READ ENTIRE EXFOR
C     FILE AND SAVE INDICES TO REQUESTED ZA/MF/MT/POINT COUNTS.
C
      MOR10=0
      IF(IPASS.GT.0) GO TO 100
c...
      open(unit=29,file='exfor.lst',status='unknown')
c...
      NZATAB=0
      ILINE=0
   10 ILINE=ILINE+1

      READ(ITAPE1,2000,END=70,ERR=10) IPROJX,IZAX,MSTA1X,MFX,MTX,
     1 MSTA2X,STATX,LABX,FIELDC,IM78X,REF1
C
C     SKIP ALL POINTS WHICH CANNOT BE PLOTTED OR HAVE NOT BEEN REQUESTED
C
      IF(MYMODE.GE.2.AND.(MFX.LT.3 .OR. MFX.GT. 10)) GO TO 10
      MFSAVX=MFX
      IF(MFX.EQ. 10) MFSAVX=3
      IF(MFX.EQ.203) MFSAVX=3
      IF(MFX.EQ.402) MFSAVX=3
      IF(MFX.EQ.154) MFSAVX=7
      IF(MFX.EQ.801) MFSAVX=8
      IF(MFSAVX.LT.3.OR.MFSAVX.GT.10) GO TO 10
      CALL FLOAT9(FIELDC(1,1),FIELDI(1))
      CALL FLOAT9(FIELDC(1,5),FIELDI(5))
      CALL FLOAT9(FIELDC(1,7),FIELDI(7))
      CALL RQEX(KGET,IZAX,MFX,MTX,FIELDI(1))
      IF(KGET.LE.0) GO TO 10
C
C     IF ONLY PLOTTING EXFOR DATA, IF NECESSARY TRANSLATE ADDITIONAL
C     ZA AND MT (PRODUCTION OR RATIO).
C
      MTTRY=0
      IZATRY=0
      IF(MYMODE.GE.2) GO TO 20
C ----FOR CROSS SECTION RATIOS (MF=203) DEFINE DENOMINATOR ZA AND MT.
      IF(MFX.NE.203) GO TO 20
      CALL FLOAT9(FIELDC(1,5),FIELDI(5))
      CALL FLOAT9(FIELDC(1,6),FIELDI(6))
      MTTRY=FIELDI(5)
      IZATRY=FIELDI(6)
      GO TO 30
C ----FOR PARTICLE/ISOTOPE PRODUCTION (MT=9000-9999) CROSS SECTIONS OR
C ----ANGULAR DISTRIBUTIONS (MF=3 OR 4) DEFINE PRODUCT ZA.
   20 IF(MTX.LT.9000) GO TO 30
      CALL FLOAT9(FIELDC(1,6),FIELDI(6))
C     BY DEFAULT OUTGOING PARTICLE IS THE SAME AS THE PROJECTILE
      IF(MFX.EQ.6 .AND. FIELDI(6).EQ.0) FIELDI(6)=IPROJX+0.9
      IZATRY=FIELDI(6)
C
C     ACCEPTABLE POINT. SAVE ALL ZA/MF/MT COMBINATIONS.
C
   30 IF(NZATAB.LE.0) GO TO 50
      DO 40 I=1,NZATAB
      IF(IPROJX.NE.MZATAB(1,I).OR.
     1 IZAX.NE.MZATAB(2,I).OR.
     2 MFX.NE.MZATAB(3,I).OR.
     3 MTX.NE.MZATAB(4,I).OR.
     3 MTTRY.NE.MZATAB(8,I).OR.
     3 IZATRY.NE.MZATAB(9,I)) GO TO 40
      IF(MFSAVX.EQ.3) GO TO 60
C     DIFFERENTIAL DATA ARE DIFFERENT IF INCIDENT OR SECONDARY
C     ENERGIES DIFFER BY MORE THAN A FRACTION ETOL
C...
      EINC=FIELDI(1)
      DE1 =ABS(EINC-ENINC(I))
      TS1 =ETOL*EINC
      IF     (MFX.EQ.4) THEN
        EEXT=FIELDI(7)
        DE2 =ABS(EEXT-ENEXT(I))
      ELSE IF(MFX.EQ.5) THEN
        EEXT=FIELDI(5)
        DE2 =ABS(EEXT-ENEXT(I))
      ELSE
        EEXT=0
        DE2 =0
      END IF
      TS2 =E2TOL*EEXT
C...      IF((ABS(EINC-ENINC(I)).LE. ETOL*EINC) .AND.
C...     &   (ABS(EEXT-ENEXT(I)).LE.E2TOL*EEXT)) GO TO 60
      IF(DE1.LE.TS1 .AND. DE2.LE.TS2) GO TO 60
C...  IF( ABS(FIELDI(1)-ENINC(I)).LT.ETOL*ENINC(I)) GO TO 60
C...  IF(FIELDI(1).EQ.ENINC(I)) GO TO 60
c...
c...      print *,EINC,DE1,TS1,EEXT,DE2,TS2
c...
   40 CONTINUE
C ----NEW ZA/MF/MT. IF POSSIBLE SAVE.
C ----SUPPRESS PROCESSING OF ANGLE-DEPENDENT ELASTIC CROSS SECTION
      IF(MFSAVX.NE.4) GO TO 42
      IF(ILINE-MZATAB(5,NZATAB).GT.1) GO TO 42
      GO TO 52
   42 IF(NZATAB.LT.MXZAT) GO TO 50
      WRITE(OUTP,6000)
      GO TO 70
   50 NZATAB=NZATAB+1
   52 CONTINUE
      MZATAB(1,NZATAB)=IPROJX
      MZATAB(2,NZATAB)=IZAX
      MZATAB(3,NZATAB)=MFX
      MZATAB(4,NZATAB)=MTX
      MZATAB(5,NZATAB)=ILINE
      MZATAB(7,NZATAB)=0
      MZATAB(8,NZATAB)=MTTRY
      MZATAB(9,NZATAB)=IZATRY
      ENINC(NZATAB)=EINC
      ENEXT(NZATAB)=EEXT
      I=NZATAB
c...
c...  print *,' NEW mf,mt,Ei,Ex',mfx,mtx,einc,eext
c... 1       ,ref1(1),ref1(2)
c...
C ----OLD ZA/MF/MT. UPDATE LAST RECORD COUNT AND POINT COUNT.
   60 MZATAB(6,I)=ILINE
      MZATAB(7,I)=MZATAB(7,I)+1
c...
c...      print *,'same mf,mt,Ei,Ex',mfx,mtx,einc,eext
c...     1       ,ref1(1),ref1(2)
c...
      GO TO 10
C
C     ENTIRE EXFOR FILE READ AND INDEXED. ELIMINATE SETS WITH LESS THAN
C     MINIMUM NUMBER OF POINTS. INDICATE END OF PLOTTING IF NO EXFOR TO
C     PLOT.
C
   70 IF(NZATAB.LE.0) GO TO 630
      KZATAB=0
      DO 90 I=1,NZATAB
c...
      WRITE( 29,'(I4,i6,i4,i6,2i8,3i6,1p,2e10.3)')
     1     (MZATAB(J,I),J=1,9),ENINC(I),ENEXT(I)
c...
      IF(MZATAB(7,I).LT.MINNIE) GO TO 90
      KZATAB=KZATAB+1
      DO 80 K=1,9
      MZATAB(K,KZATAB)=MZATAB(K,I)
   80 CONTINUE
      ENINC(KZATAB)=ENINC(I)
      ENEXT(KZATAB)=ENEXT(I)
   90 CONTINUE
c...
      CLOSE(UNIT=29)
C...
      NZATAB=KZATAB
      IF(NZATAB.LE.0) GO TO 630
C
C     POSITION EXFOR FILE TO READ.
C
      KZATAB=0
      IPASS=1
      ILINE=0
      REWIND ITAPE1
C
C     IF NO MORE EXFOR DATA TO PLOT INDICATE END OF PLOTTING.
C     DEFINE NEXT ZA/MF/MT BASED ON ENDF/B (ALREADY DEFINED) OR NEXT
C     ZA/MF/MT IN EXFOR INDEX. DETERMINE INDEX TO FIRST EXFOR RECORD
C     TO READ.
C
  100 IF(NZATAB.LE.0) GO TO 630
      IF(MYMODE.GE.2) GO TO 110
C
C     SELECT NEXT ZA/MF/MT FROM EXFOR INDEX. END IF NO MORE EXFOR DATA
C     TO PLOT.
C
      MSTAT1=BLANK
      MSTAT2=BLANK
      KZATAB=KZATAB+1
      IF(KZATAB.GT.NZATAB) GO TO 630
      IPROJT=MZATAB(1,KZATAB)
      IZA=MZATAB(2,KZATAB)
      MF=MZATAB(3,KZATAB)
      MT=MZATAB(4,KZATAB)
      MTRAT=MZATAB(8,KZATAB)
      IZARAT=MZATAB(9,KZATAB)
      EINC=ENINC(KZATAB)
      EEXT=ENEXT(KZATAB)
      MFSAVE=MF
      IF(MF.EQ. 10) MFSAVX=3
      IF(MF.EQ.203) MFSAVE=3
      IF(MF.EQ.402) MFSAVE=3
      IF(MF.EQ.154) MFSAVE=7
      IF(MF.EQ.801) MFSAVE=8
      KGROUP=IGROUP
      IF(MF.NE.3 .AND. MF.NE.10) KGROUP=0
      GO TO 130
C
C     COMPARE ENDF/B ZA/MF/MT TO EXFOR INDEX.
C
  110 IPROJT=1
      MTRAT=0
      IZARAT=0
      EINC=0.0
      MFSAVE=MF
      IF(MF.EQ.10) MFSAVE=3
      KGROUP=IGROUP
      IF(MF.LT.5 .OR. MF.GT.6) KGROUP=0
      IMNORM=0
      DO 120 K=1,NZATAB
        KZATAB=K
        MFJ=MF
        IF(MFJ.EQ.6 .AND. MZATAB(3,KZATAB).EQ.5) MFJ=5
C...
C       IF(MFJ.EQ.10) MFJ=3
C...
        IF(IPROJT.EQ.MZATAB(1,KZATAB).AND.
     1     IZA.EQ.MZATAB(2,KZATAB).AND.
     2     MFJ.EQ.MZATAB(3,KZATAB).AND.
     3     MT.EQ.MZATAB(4,KZATAB)) GO TO 130
  120 CONTINUE
C ----NO COMPARABLE EXFOR DATA.
      IEX=0
      IEND=1
      RETURN
C
C     REQUIRED DATA IS PRESENT. INITIALIZE PARAMETERS.
C
C ----INITIALIZE DATA POINT, REFERENCE COUNT, GROUP BY E2 FLAG, FIELDS
C ----7 - 8 BLANK AND INTERNAL MF NUMBER.
  130 IEX=0
      IEXPAS=0
      IREF=0
C ----SET THE INCIDENT ENERGY AND OUTGOING PARTICLE FOR DIFFERENTIAL DATA
      IZARAT=MZATAB(9,KZATAB)
      IF(MF.GE.4 .AND. MF.LE.6) THEN
        EINC=ENINC(KZATAB)
        EEXT=ENEXT(KZATAB)
C...
C...        DO K1=1,NZATAB
C...          PRINT *,(MZATAB(K2,K1),K2=1,4),ENINC(K1),ENEXT(K1)
C...        END DO
C...
C...        PRINT *,'MF,MT,KZATAB,EINC,EEXT',MF,MT,KZATAB,EINC,EEXT
C...
      END IF
C ----INITIALIZE ALL METASTABLE STATE FLAGS.
      MSTAR1=BLANK1
      MSTAR2=BLANK1
      MSTA1X=MSTAT1
      MSTA2X=MSTAT2
      MSTA1R=BLANK1
      MSTA2R=BLANK1
C ----INITIALIZE STATUS AND CENTER-OF-MASS FLAG.
      STATUS=BLANK1
      IMNORM=0
      LABCM=BLANK1
C
C     DECIDE WHETHER OR NOT TO REWIND COMPUTATION FORMAT FILE BASED
C     ON CURRENT POSITION OF EXFOR DATA FILE VS. INDEX TO FIRST DATA
C     POINT OF REQUIRED ZA/MF/MT.
C
      IF(ILINE-MZATAB(5,KZATAB)) 150,170,140
C ----RE-INITIALIZE LINE COUNT AND REWIND.
  140 ILINE=0
      REWIND ITAPE1
C ----SKIP FORWARD TO REQUESTED DATA.
  150 II=MZATAB(5,KZATAB)-1
      IF(II.EQ.ILINE) GO TO 180
      ILINE=ILINE+1
      DO 160 I=ILINE,II
      READ(ITAPE1,2010,ERR=160) DUMMY
  160 CONTINUE
      ILINE=II
      GO TO 180
C
C     SET UP LOOP TO READ ALL COMPARABLE DATA POINTS.
C
C ----SET FLAG TO SHOW THAT NEXT POINT IS IN CORE.
  170 IEXPAS=1
  180 DO 510 JEX=1,MAXIE
      IEX=JEX
C...
C...     print *,'JEX,IEX,IEXPAS',JEX,IEX,IEXPAS
C...
C ----SKIP READ IF NEXT POINT IS ALREADY IN CORE.
      IF(IEXPAS.EQ.1) GO TO 200
C ----INCREMENT LINE COUNT AND READ NEXT POINT.
  190 ILINE=ILINE+1
      READ(ITAPE1,2000,END=520,ERR=190) IPROJX,IZAX,MSTA1X,MFX,MTX,
     1 MSTA2X,STATX,LABX,FIELDC,IM78X,REF1
C
C     USE INDEX TO LAST POINT OF ZA/MF/MT TO DETERMINE WHEN TO STOP
C     READING.
C
  200 IEXPAS=0
      IF(ILINE.GT.MZATAB(6,KZATAB)) GO TO 520
C
C     IMMEDIATELY SKIP DATA WHICH CANNOT BE PLOTTED.
C
C     NON-MATCHING DISCRETE STATES (GROUND AND METASTABLE)
C...
C...  print *,'MSTA2X,MSTAT2"',MSTA2X(1:1),'"',MSTAT2(1:1),'"',IEX
C...

      IF(MSTA2X(1:1).EQ.MSTAT2(1:1)) GO TO 202
        IF(MSTA2X(1:1).GT.MSTAT2(1:1)) MOR10=1
        GO TO 190
C     SPECIAL DATA TYPES AND RATIOS
  202 MFSAVX=MFX
      IF(MFX.EQ. 10) MFSAVX=3
      IF(MFX.EQ.203) MFSAVX=3
      IF(MFX.EQ.402) MFSAVX=3
      IF(MFX.EQ.154) MFSAVX=7
      IF(MFX.EQ.801) MFSAVX=8
      IF(MFSAVX.LT.1.OR.MFSAVX.GT.10) GO TO 190
C
C     TRANSLATE 8 FIELDS FROM CHARACTER TO FLOATING POINT.
C
      DO 210 I=1,8
  210 CALL FLOAT9(FIELDC(1,I),FIELDI(I))
C     BY DEFAULT OUTGOING PARTICLE IS THE SAME AS THE PROJECTILE
      IF(MFX.EQ.6 .AND. FIELDI(6).EQ.0) FIELDI(6)=IPROJX+0.9
C
C     SKIP POINT IF IT IS NOT REQUESTED (EVEN THOUGH INDEX TO EXFOR HAS
C     ALREADY BEEN MADE THIS TEST IS NECESSARY TO COLLECT TOGETHER
C     REQUESTED POINTS WHICH ARE MIXED IN WITH POINTS WHICH HAVE NOT
C     BEEN REQUESTED, E.G. MULTI-DIMENSIONAL TABLE WHERE SUCCESSIVE
C     POINTS CAN BE FOR DIFFERENT REACTIONS).
C
      CALL RQEX(KGET,IZAX,MFX,MTX,FIELDI(1))
      IF(KGET.LE.0) GO TO 190
C
C     IF ONLY PLOTTING EXFOR DATA, IF NECESSARY TRANSLATE ADDITIONAL
C     ZA AND MT (PRODUCTION OR RATIO).
C
      MTTRY=0
      IZATRY=0
C ----FOR CROSS SECTION RATIOS (MF=203) DEFINE DENOMINATOR ZA AND MT.
      IF(MFX.NE.203) GO TO 220
      MTTRY=FIELDI(5)
      IZATRY=FIELDI(6)
C ----DEFINE DEMONINATOR METASTABLE STATE FLAG (CONVERT 9-TH CHARACTER
C ----OF 6-TH FIELD FROM INTEGER TO METASTABLE FLAG).
      CALL META10(FIELDC(3,6),MSTA1R)
C ----DEFINE DEMONINATOR PRODUCT METASTABLE STATE FLAG (CONVERT 9-TH
C ----CHARACTER OF 5-TH FIELD FROM INTEGER TO METASTABLE FLAG).
      CALL META10(FIELDC(3,5),MSTA2R)
C ----SET DATA FIELDS TO ZERO TO AVOID THEIR BEING INTERPRETED AS DATA.
      FIELDI(5)=0.0
      FIELDI(6)=0.0
      GO TO 230
C ----FOR PARTICLE/ISOTOPE PRODUCTION (MT=9000-9999) CROSS SECTIONS OR
C ----ANGULAR DISTRIBUTIONS (MF=3 OR 4) DEFINE PRODUCT ZA.
  220 IF(MTX.LT.9000) GO TO 230
      IZATRY=FIELDI(6)
c...      IZARAT=IZATRY
C ----SET DATA FIELD TO ZERO TO AVOID ITS BEING INTERPRETED AS DATA.
      FIELDI(6)=0.0
C
C     COMPARE,
C     (1) CURRENT ZA/MF/MT TO REQUIRED ZA/MF/MT.
C     (2) IF NOT CROSS SECTIONS INCIDENT ENERGY
C
  230 MFIN=MF
      IF(MFIN.EQ.6 .AND. MFX.EQ.5) MFIN=MFX
C...
C...      PRINT *,'IPROJT,IPROJX',IPROJT,IPROJX
C...      PRINT *,'IZA,IZAX',IZA,IZAX
C...      PRINT *,'MFIN,MFX,MFSAVE',MFIN,MFX,MFSAVE
C...      PRINT *,'MTRAT,MTTRY',MTRAT,MTTRY
C...      PRINT *,'IZARAT,IZATRY',IZARAT,IZATRY,' iex',IEX
C...
      IF(IPROJT.NE.IPROJX.OR.
     1 IZA.NE.IZAX.OR.
     2 MFIN.NE.MFX.OR.
     3 MT.NE.MTX.OR.
     4 MTRAT.NE.MTTRY.OR.
     5 IZARAT.NE.IZATRY) GO TO 240
C ----SAME ZA/MF/MT. IF NOT MF=3 CHECK INCIDENT ENERGY.
      IF(MFSAVE.EQ.3) GO TO 250
C...
      DE1=ABS(FIELDI(1)-EINC)
      TS1=ETOL*EINC

C...         PRINT *,'        DE1,TS1',FIELDI(1),EINC,DE1,TS1

      IF(DE1.LE.TS1) GO TO 250
C...  IF(ABS(FIELDI(1)-EINC).LT.ETOL*EINC) GO TO 250
C...  IF(FIELDI(1).EQ.EINC) GO TO 250
C
C     CURRENT POINT CANNOT BE INCLUDED ON CURRENT PLOT (E.G., NEW ZA/MF/
C     MT OR OTHER PARAMETER CHANGED).
C
C ----IF PLOTTING ALL REFERENCES TOGETHER CONTINUE READING
  240 IF(MYMODE.EQ.1.OR.MYMODE.EQ.3) GO TO 190
C ----IF NO POINTS FOUND YET CONTINUE READING.
      IF(IEX.EQ.1) GO TO 190
C ----RETURN CURRENT POINTS READ.
      GO TO 530
C
C     IF ONLY PLOTTING EXFOR DATA SAVE PARAMETERS WHEN FIRST POINT IS
C     READ.
C
  250 IF(MYMODE.GE.2.OR.IEX.GT.1) GO TO 270
      ILIB=16
      LIBNAM(3)=REF1(8)
      LIBNAM(4)=REF1(9)
      STATUS=STATX
      IMNORM=0
      IF(STATUS.NE.USTAT) GO TO 260
      STATUS=BLANK1
      IMNORM=1
  260 LABCM=LABX
      MSTAT1=MSTA1X
      MSTAT2=MSTA2X
      MSTAR1=MSTA1R
      MSTAR2=MSTA2R
      IM78=IM78X
C
C     TEST FOR NON-BLANK E2 FIELD. DEFINE E2.
C
  270 IMAM78=0
      DO 280 I=1,3
      IF(FIELDC(I,7).NE.BLANK) IMAM78=1
  280 CONTINUE
      E2(IEX)=FIELDI(7)
C ----CONVERT ELASTIC CM DISTRIBUTION TO LAB
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
C
C     SAVE ALL REFS AND DEFINE POINT INDEX TO REF.
C
      IF(IREF.LE.0) GO TO 320
      DO 300 I=1,IREF
      KREF=I
      IMSAME=0
      DO 290 J=1,9
      IF(REFS(J,KREF).NE.REF1(J)) GO TO 300
  290 CONTINUE
      IMSAME=1
C ----OLD REFERENCE. FOR CROSS SECTIONS AND RATIO TEST FOR CHANGE IN E2.
c...
c...  PRINT *,'MF,MT,MSAME,E2,E2T',MF,MT,IMSAME,E2(IEX),E2T(KREF)
c... 1       ,KGROUP,MFSAVE
c...
      IF(MFSAVE.NE.3) GO TO 340
C ----TEST STATE DESIGNATION IF PLOTTING SIMPLE TABULATED DATA
C...
      DE2=ABS(E2(IEX)-E2T(KREF))
      TS2=E2TOL*E2T(KREF)
      IF(DE2.LE.TS2) GO TO 340
C...  IF(ABS(E2(IEX)-E2T(KREF)).LE.E2TOL*E2T(KREF)) GO TO 340
C...  IF(E2T(KREF)-E2(IEX)) 300,340,300
  300 CONTINUE
c...
c...    print *,'New ref, kg,ms,mod',kgroup,imsame,mymode
c...
C ----NEW REFERENCE AND/OR E2. IF IDENTIFYING E2 CONTINUE TO READ AS
C ----LONG AS REFERENCE HAS NOT CHANGED. OTHERWISE RETURN.
      IF(KGROUP.EQ.0) GO TO 310
      IF(IMSAME.EQ.1) GO TO 320
C ----NOT IDENTIFYING E2. RETURN IF PLOTTING EACH REFERENCE SEPERATELY.
  310 IF(MYMODE.EQ.0.OR.MYMODE.EQ.2) GO TO 530
C ----COLLECTING REFERENCES. ONLY ALLOW ONE E2 VALUE FOR MF 3.
      IF(MFSAVE.NE.3) GO TO 320
C...
c...  print *,'e2,e2t,dif,crit',E2(IEX),E2T(1)
c... 1       ,(E2(IEX)-E2T(1)),E2TOL*E2T(1)

      DE2=ABS(E2(IEX)-E2T(1))
      TS2=E2TOL*E2T(1)
      IF(DE2.GT.TS2) GO TO 530
C...  IF(ABS(E2(IEX)-E2T(1)).GT.E2TOL*E2T(1)) GO TO 530
C...  IF(E2T(1).NE.E2(IEX)) GO TO 530
  320 IF(IREF.GE.MAXREF) GO TO 530
      IREF=IREF+1
C ----IF MORE THAN MAXIMUM NUMBER OF REFERENCES IDENTIFY ALL OTHER
C ----REFERENCES ONLY AS OTHERS.
C ----SAVE E2 AND REFERENCE.
      E2T(IREF)=E2(IEX)
      DO 330 J=1,9
  330 REFS(J,IREF)=REF1(J)
      KREF=IREF
C ----DEFINE DATA POINT INDEX TO REFERENCE.
  340 NREF(IEX)=KREF
C
C     SAVE DATA POINT ACCORDING TO TYPE OF DATA.
C
      MFM2=MFSAVE-2
      YEX(IEX)=FIELDI(3)
      DYEX(IEX)=ABS(FIELDI(4))
      GO TO (350,410,420,430,470,480),MFM2
C ----BASED ON FIRST TWO POINT SELECT E VS. CROSS SECTION (CONSTANT E2)
C ----OR E2 VS. CROSS SECTION (CONSTANT E).
  350 IF(IEX.GT.1) GO TO 360
C ----INITIALIZE ASSUMING E VS. CROSS SECTION.
      IDOUB=1
      FIELD4(1)=FIELDI(7)
      FIELD4(2)=FIELDI(8)
      XEX(IEX)=FIELDI(1)
      DXEX(IEX)=FIELDI(2)
      GO TO 510
  360 IF(IEX.GT.2) GO TO 390
C ----CANNOT USE E2 IF FIELD IS BLANK.
      IF(IMAM78.LE.0) GO TO 390
C ----CANNOT USE E2 IF FIELD IS HALF-LIFE
      IF(IM78.EQ.HL) GO TO 390
C ----TEST FOR CONSTANT E FIELD.
      IF(XEX(1)-FIELDI(1)) 390,370,390
C ----CONSTANT E. TEST FOR CONSTANT E2 FIELD.
  370 IF(FIELD4(1)-FIELDI(7)) 380,390,380
C ----CONSTANT E. CHANGE TO E2 VS. CROSS SECTION.
  380 IDOUB=2
      E2(1)=XEX(1)
      E2T(1)=XEX(1)
      XEX(1)=FIELD4(1)
      DXEX(1)=FIELD4(2)
  390 IF(IDOUB.EQ.2) GO TO 400
C ----CROSS SECTION. E VS. CROSS SECTION.
      XEX(IEX)=FIELDI(1)
      DXEX(IEX)=FIELDI(2)
      GO TO 510
C ----CROSS SECTION...E2 VS. CROSS SECTION.
  400 XEX(IEX)=FIELDI(7)
      DXEX(IEX)=FIELDI(8)
      E2(IEX)=FIELDI(1)
      GO TO 510
C ----ANGULAR DISTRIBUTION - SEPARATE OUT DIFFERENT LEVELS
C...
C...  410 print *,'e2,e2t,dif,test'
C...     1       ,E2(IEX),E2T(1),(E2(IEX)-E2T(1)),E2TOL*E2T(1)
C...
C...  IF(ABS(E2(IEX)-E2T(1)).GT.E2TOL*E2T(1)) GO TO 530
  410 DE2=ABS(E2(IEX)-E2T(1))
      TS2=E2TOL*E2T(1)
      IF(DE2.GT.TS2) GO TO 530
C...  410 IF(ABS(E2(IEX)-E2T(1)).GT.E2TOL*E2T(1)) GO TO 530
C...
      XEX(IEX)=FIELDI(5)
      DXEX(IEX)=FIELDI(6)
      GO TO 510
C ----ENERGY DISTRIBUTION
  420 XEX(IEX)=FIELDI(7)
      DXEX(IEX)=FIELDI(8)
      GO TO 510
C ----DOUBLE DIFFERENTIAL. ALLOW FOR EITHER CONSTANT COSINE OR E.
  430 IF(IEX.GT.1) GO TO 440
      IDOUB=1
      FIELD4(1)=FIELDI(5)
      FIELD4(2)=FIELDI(6)
      FIELD4(3)=FIELDI(7)
      FIELD4(4)=FIELDI(8)
      XEX(IEX)=FIELD4(1)
      DXEX(IEX)=FIELD4(2)
      GO TO 510
C ----FOR DOUBLE DIFFERENTAL DATA COLLECT EITHER CROSS SECTION VS.
C ----ANGLE (CONSTANT SECONDARY ENERGY) OR CROSS SECTION VS. SECONDARY
C ----ENERGY (COSTANT ANGLE) BASED ON WHICHEVER CHANGES BEWTEEEN FIRST
C ----TWO POINTS READ.
  440 IF(IEX.GT.2) GO TO 450
      IF(FIELD4(1).NE.FIELDI(5).AND.FIELD4(3).NE.FIELDI(7)) GO TO 530
      IF(FIELD4(1).EQ.FIELDI(5)) IDOUB=2
      IF(IDOUB.EQ.1) GO TO 450
      XEX(1)=FIELD4(3)
      DXEX(1)=FIELD4(4)
  450 IF(IDOUB.EQ.2) GO TO 460
      IF(FIELD4(3).NE.FIELDI(7)) GO TO 530
      XEX(IEX)=FIELDI(5)
      DXEX(IEX)=FIELDI(6)
      GO TO 510
  460 IF(FIELD4(1).NE.FIELDI(5)) GO TO 530
      XEX(IEX)=FIELDI(7)
      DXEX(IEX)=FIELDI(8)

c...  print *,'   iex,e,x',iex,xex(iex),dxex(iex),' going to 510'

      GO TO 510
C ----LEGENDRE COEFFICIENTS.
  470 XEX(IEX)=FIELDI(5)
      DXEX(IEX)=0.0
      GO TO 510
C ----FISSION YIELD. WHEN FIRST DATA POINT IS READ INITIALIZE
C ----CUMULATIVE YIELD TABLE AND STORE FIRST VALUE.
  480 IF(IEX.GT.1) GO TO 500
      DO 490 M=1,400
      YEX(M)=0.0
  490 DYEX(M)=0.0
      IATWT=FIELDI(6)
      YEX(IATWT)=FIELDI(3)
      DYEX(IATWT)=FIELDI(4)**2
      MEX1=IATWT
      MEX2=IATWT
      GO TO 510
C ----FISSION YIELD. ADD ATOMIC WEIGHT YIELD TO TABLE.
  500 IATWT=FIELDI(6)
      YEX(IATWT)=YEX(IATWT)+FIELDI(3)
      DYEX(IATWT)=DYEX(IATWT)+FIELDI(4)**2
      IF(IATWT.LT.MEX1) MEX1=IATWT
      IF(IATWT.GT.MEX2) MEX2=IATWT
C
C     READ UP TO MAXIMUM POINTS.
C
  510 CONTINUE
      GO TO 540
  520 CONTINUE
      IPASS=IPASS+1
C
C     ALL EXFOR DATA READ.
C
  530 IEX=IEX-1
C ----SAVE COUNT OF ACTUAL POINTS READ (TO DECREMENT INDEX).
  540 KEX=IEX
      IF(IEX.LE.0) GO TO 580
      IF(MF.NE.801) GO TO 560
C ----DEFINE ATOMIC WEIGHT RANGE FOR FISSION YIELD.
      IEX=0
      DO 550 M=MEX1,MEX2
      IF(YEX(M).LE.0.0) GO TO 550
      IEX=IEX+1
      XEX(IEX)=M
      DXEX(IEX)=0.0
      YEX(IEX)=YEX(M)
      DYEX(IEX)=SQRT(DYEX(M))
  550 CONTINUE
      GO TO 580
C
C     INSURE ERRORS ARE NON-NEGATIVE. SET TO ZERO IF ERROR WILL NOT BE
C     PLOTTED.
C
  560 DO 570 I=1,IEX
      DXEX(I)=ABS(DXEX(I))
      IF(IXERR.LE.0) DXEX(I)=0.0
      DYEX(I)=ABS(DYEX(I))
      IF(IYERR.LE.0) DYEX(I)=0.0
  570 CONTINUE
C...
C ----DO NOTHING IF MF=10 AND MORE STATES EXIST
C...  IF(MF.EQ.10 .AND. MOR10.NE.0) GO TO 582
C...
C ----IF LESS THAN TWO REFERENCES TURN OFF IDENTIFY E2 FLAG.
  580 IF(IREF.LT.2) KGROUP=0
C
C     UPDATE EXFOR INDEX TABLE TO PREVENT POINTS BEING PLOTTED TWICE
C
C ----RESET LOWER LINE INDEX AND REMAINING POINT COUNT.
      MZATAB(5,KZATAB)=ILINE
      MZATAB(7,KZATAB)=MZATAB(7,KZATAB)-KEX
C ----IF MORE POINTS WITH SAME ZA/MF/MT TO PLOT RESET INDEX TO TABLE.
      IF(MZATAB(7,KZATAB).LT.MINNIE.OR.
     1 MZATAB(5,KZATAB).GT.MZATAB(6,KZATAB)) GO TO 590
  582 KZATAB=KZATAB-1
      RETURN
C ----ELIMINATE INDEX FROM TABLE BY SHIFTING ALL FOLLOWING ENTRIES ONE
C ----LOCATION FORWARD IN TABLE.
  590 IF(NZATAB.LE.1.OR.KZATAB.EQ.NZATAB) GO TO 620
      II=KZATAB+1
      DO 610 I=II,NZATAB
        DO 600 K=1,9
          MZATAB(K,I-1)=MZATAB(K,I)
  600   CONTINUE
        ENINC(I-1)=ENINC(I)
        ENEXT(I-1)=ENEXT(I)
  610 CONTINUE
      KZATAB=KZATAB-1
  620 NZATAB=NZATAB-1
C ----SET END OF FILE FLAG IF NO EXFOR DATA LEFT TO PLOT.
      IF(NZATAB.LE.0) IEND=2
      RETURN
C
C     END OF EXFOR PLOTTING.
C
  630 IEND=2
      RETURN
 2000 FORMAT(I5,I6,A1,I3,I4,3A1,8(2A4,A1),A3,6A4,A1,2A4)
 2010 FORMAT(A1)
 6000 FORMAT(' MAX.NO. OF REACTIONS REACHED...REMAINING SKIPPED')
      END
      SUBROUTINE FLOAT9(FIELD,X)
C
C     CONVERT FROM 9 HOLLERITH CHARACTERS TO FLOATING POINT.
C     MUST BE BETWEEN 1.0E-40 AND 1.0E+40.
C
      INTEGER OUTP,OTAPE
      CHARACTER*1 BLANK,DOT,EXPD,EXPE,PLUS,MINUS,STAR,MESS,DIGIT,FIELD,
     1 IFIELD
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      DIMENSION FIELD(9),TEN(35),DIGIT(10),MESS(9)
      DATA BLANK/' '/
      DATA DOT/'.'/
      DATA EXPD/'D'/
      DATA EXPE/'E'/
      DATA PLUS/'+'/
      DATA MINUS/'-'/
      DATA STAR/'*'/
      DATA MESS/' ',' ',' ',' ',' ',' ',' ',' ',' '/
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
C ----SKIP LEADING BLANK CHARACTERS.
      DO 10 I=1,9
      IF(FIELD(I).NE.BLANK) GO TO 20
   10 CONTINUE
C ----FIELD IS COMPLETELY BLANK. RETURN ZERO.
      X=ZERO
      GO TO 240
C ----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT.
   20 IN=0
      IPT=-20
C ----ALLOW LEADING SIGN.
      IF(FIELD(I).EQ.MINUS) GO TO 40
      IF(FIELD(I).NE.PLUS) GO TO 30
      I=I+1
   30 XSIGN=1.0
      GO TO 50
   40 I=I+1
      XSIGN=-1.0
C ----SCAN REMAINDER OF MANTISSA.
   50 DO 90 J=I,9
      JC=J
      IFIELD=FIELD(J)
C ----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).
      DO 60 K=1,10
      IF(IFIELD.EQ.DIGIT(K)) GO TO 80
   60 CONTINUE
      IF(IFIELD.NE.DOT) GO TO 70
      IPT=0
      GO TO 90
C ----SCAN FOR BLANK (WHICH ENDS MANTISSA).
   70 IF(IFIELD.EQ.BLANK) GO TO 100
C ----SCAN FOR E,D,- OR + (WHICH BEGINS EXPONENT).
      IF(IFIELD.EQ.EXPE.OR.IFIELD.EQ.EXPD) GO TO 130
      IF(IFIELD.EQ.MINUS) GO TO 160
      IF(IFIELD.EQ.PLUS) GO TO 140
C ----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 250
C ----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT
C ----OFFSET.
   80 IN=10*IN+(K-1)
      IPT=IPT+1
   90 CONTINUE
C ----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
      GO TO 120
C ----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR
C ----EXPONENT.
  100 I=JC+1
      IF(I.GT.9) GO TO 120
      DO 110 J=I,9
      JC=J
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.BLANK) GO TO 110
      IF(IFIELD.EQ.EXPE.OR.IFIELD.EQ.EXPD) GO TO 130
      IF(IFIELD.EQ.MINUS) GO TO 160
      IF(IFIELD.EQ.PLUS) GO TO 140
C ----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 250
  110 CONTINUE
C ----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
  120 X=IN
      IF(IPT.GT.0) X=X/TEN(IPT)
      GO TO 230
C
C     TRANSLATE EXPONENT.
C
C ----BEGINNING OF EXPONENT FOUND (X OR D). CHECK FOR FOLLOWING - OR +.
  130 J=J+1
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.MINUS) GO TO 160
      IF(IFIELD.NE.PLUS) GO TO 150
C ---- + FOUND. INITIALIZE EXPONENT SIGN.
  140 J=J+1
  150 KSIGN=1
      GO TO 170
C ---- - FOUND. INITIALIZE EXPONENT SIGN.
  160 J=J+1
      KSIGN=-1
C ----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.
  170 KEXP=0
      DO 200 I=J,9
      JC=I
      IFIELD=FIELD(I)
      IF(IFIELD.EQ.BLANK) GO TO 200
      DO 180 K=1,10
      IF(IFIELD.EQ.DIGIT(K)) GO TO 190
  180 CONTINUE
C ----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 250
C ----DIGIT FOUND. INCREMENT EXPONENT.
C ----OFFSET.
  190 KEXP=10*KEXP+(K-1)
  200 CONTINUE
C ----ENTIRE FIELD TRANSLATED (WITH EXPONENT). CONVERT TO FLOATING
C ----POINT.
      X=IN
      KEXP=KSIGN*KEXP
      IF(IPT.GT.0) KEXP=KEXP-IPT
      IF(KEXP) 210,230,220
  210 KEXP=-KEXP
      X=X/TEN(KEXP)
      GO TO 230
  220 X=X*TEN(KEXP)
  230 X=XSIGN*X
  240 RETURN
  250 MESS(JC)=STAR
      WRITE(OUTP,6000) FIELD,MESS
      X=ZERO
      MESS(JC)=BLANK
      RETURN
 6000 FORMAT(/1X,' STRING "',9A1,'"'/
     1        1X,' COLUMN  ',9A1/
     1 ' SUBROUTINE FLOAT9...ERROR IN INPUT DATA...TRANSLATED AS 0.0')
      END
      SUBROUTINE RQEX(KGET,IZA,MF,MT,EN)
C
C     COMPARE CURRENT EXFOR ZA/MF/MT/INCIDENT ENERGY TO REQUESTS.
C
      COMMON/GETEM/IZALOW(100),MFLOW(100),MTLOW(100),ELGET(100),
     1 IZAHI(100),MFHI(100),MTHI(100),EHGET(100),EPGET(100),INTRN(100),
     2 NGET,IGET
      COMMON/EPSMF6/EP6
      DO 10 KGET=1,NGET
      IF(IZA.LT.IZALOW(KGET).OR.IZA.GT.IZAHI(KGET)) GO TO 10
      IF(MF.LT.MFLOW(KGET).OR.MF.GT.MFHI(KGET)) GO TO 10
      IF(MT.LT.MTLOW(KGET).OR.MT.GT.MTHI(KGET)) GO TO 10
      IF(EN.GE.ELGET(KGET).AND.EN.LE.EHGET(KGET)) GO TO 20
   10 CONTINUE
      KGET=0
C* RESOLUTION BROADENING PARAMETER
   20 IF(EPGET(KGET).GT.0 .AND. EPGET(KGET).LT.0.1) THEN
        EP6=EPGET(KGET)
      ELSE
        EP6=0.02
      END IF
      RETURN
      END
      SUBROUTINE ZAMFMT
C
C     READ ALL,
C     (1) SPECIAL ZA TITLES (IZA LESS THAN 1000)
C     (2) MF TITLES
C     (3) MT TITLES
C
C     WARNING....THIS ROUTINE MUST BE CALLED BEFORE TRYING TO DEFINE
C     ANY ZA/MF/MT EQUIVALENCES.
C
      INTEGER OUTP
      CHARACTER*4 ZATAB,MFTAB,MTTAB
      COMMON/UNITS/INP,OUTP,ITAPE1,ITAPE2
      COMMON/UNITT/NTAPE1,NTAPE2,NTAPE3
      COMMON/TABZAC/ZATAB(3,200)
      COMMON/TABZAI/MZLONG,IZATAB(200)
      COMMON/TABMFC/MFTAB(8,100)
      COMMON/TABMFI/MFLONG,IMFTAB(4,100)
      COMMON/TABMTC/MTTAB(10,300)
      COMMON/TABMTI/MTLONG,IMTTAB(3,300)
C ----DEFINE TABLE SIZES.
      DATA MZMAX/200/
      DATA MFMAX/100/
      DATA MTMAX/300/
C ----INDICATE PROGRAM IS READING TRANSLATION TABLES.
      WRITE(OUTP,6000)
C
C     READ SPECIAL ZA TITLES.
C
      DO 10 MZLONG=1,MZMAX
      READ(NTAPE1,1200,END=20,ERR=30) IZATAB(MZLONG),
     1 (ZATAB(K,MZLONG),K=1,3)
   10 CONTINUE
      MZLONG=MZMAX
      GO TO 40
C ----END OF REACTIONS.
   20 MZLONG=MZLONG-1
      IF(MZLONG.GT.0) GO TO 40
C ----ERROR READING.
   30 WRITE(OUTP,6040)
      GO TO 140
   40 WRITE(OUTP,6010) MZLONG,MZMAX
C
C     READ MF TITLES
C
      DO 60 MFLONG=1,MFMAX
   50 READ(NTAPE2,1400,END=70,ERR=80) IMFTAB(2,MFLONG),IMFTAB(3,MFLONG),
     1 IMFTAB(4,MFLONG),(MFTAB(K,MFLONG),K=1,8)
      IF(IMFTAB(2,MFLONG).LE.0) GO TO 50
   60 CALL SIZER(MFTAB(1,MFLONG),IMFTAB(1,MFLONG),32)
      MFLONG=MFMAX
      GO TO 90
C ----END OF REACTIONS.
   70 MFLONG=MFLONG-1
      IF(MFLONG.GT.0) GO TO 90
C ----ERROR READING.
   80 WRITE(OUTP,6050)
      GO TO 140
   90 WRITE(OUTP,6020) MFLONG,MFMAX
C
C     READ MT TITLES
C
      DO 100 MTLONG=1,MTMAX
      READ(NTAPE3,1500,END=110,ERR=120) IMTTAB(2,MTLONG),
     1 IMTTAB(3,MTLONG),(MTTAB(K,MTLONG),K=1,10)
  100 CALL SIZER(MTTAB(1,MTLONG),IMTTAB(1,MTLONG),40)
      MTLONG=MTMAX
      GO TO 130
C ----END OF REACTIONS.
  110 MTLONG=MTLONG-1
      IF(MTLONG.GT.0) GO TO 130
C ----ERROR READING.
  120 WRITE(OUTP,6050)
      GO TO 140
  130 WRITE(OUTP,6030) MTLONG,MTMAX
      GO TO 150
  140 STOP
  150 RETURN
 1200 FORMAT(I5,1X,3A4)
 1400 FORMAT(3I5,1X,8A4)
 1500 FORMAT(2I5,1X,10A4)
 6000 FORMAT(' READING TRANSLATION TABLES'/1X,72('='))
 6010 FORMAT(' SPECIAL ZA TITLES----------',I5,'(',I5,' ALLOWED)')
 6020 FORMAT(' MF TITLES------------------',I5,'(',I5,' ALLOWED)')
 6030 FORMAT(' MT TITLES------------------',I5,'(',I5,' ALLOWED)')
 6040 FORMAT(' ERROR READING SPECIAL ZA TITLES...EXECUTION TERMINATED')
 6050 FORMAT(' ERROR READING MF TITLES...EXECUTION TERMINATED')
 6060 FORMAT(' ERROR READING MT TITLES...EXECUTION TERMINATED')
      END
      SUBROUTINE ZAHOL(ZA,MSTAT1,ZABCD,IZABCD)
C
C     GIVEN ANY ZA = (1000*Z+A) THIS ROUTINE WILL DEFINE A THREE WORD
C     EQUIVALENT IN ONE OF THE TWO FOLLOWING FORMS.
C
C     IF Z IS GREATER THAN ZERO....ZZZ-SS-AAAM
C                                  E.G., 26056-M = 26-FE-56M
C
C     IF Z IS EQUAL TO ZERO........3A4 CHARACTER EQUIVALENT
C                                  E.G., ZA=302 = ZIRCALLOY-2
C
      INTEGER ZA,Z,A
      CHARACTER*1 MSTAT1
      CHARACTER*4 ZATAB,ZABCD,DUM1,DUM2,BLANK4
      DIMENSION ZATAB(110),DUM1(54),DUM2(56),ZABCD(4)
      EQUIVALENCE (ZATAB(1),DUM1(1)),(ZATAB(55),DUM2(1))
      DATA DUM1/
     1 '-H -','-He-','-Li-','-Be-','-B -','-C -',
     2 '-N -','-O -','-F -','-Ne-','-Na-','-Mg-',
     3 '-Al-','-Si-','-P -','-S -','-Cl-','-Ar-',
     4 '-K -','-Ca-','-Sc-','-Ti-','-V -','-Cr-',
     5 '-Mm-','-Fe-','-Co-','-Ni-','-Cu-','-Zn-',
     6 '-Ga-','-Ge-','-As-','-Se-','-Br-','-Kr-',
     7 '-Rb-','-Sr-','-Y -','-Zr-','-Nb-','-Mo-',
     8 '-Tc-','-Ru-','-Rh-','-Pd-','-Ag-','-Cd-',
     9 '-In-','-Sn-','-Sb-','-Te-','-I -','-Xe-'/
      DATA DUM2/
     1 '-Cs-','-Ba-','-La-','-Ce-','-Pr-','-Nd-',
     2 '-Pm-','-Sm-','-Eu-','-Gd-','-Tb-','-Dy-',
     3 '-Ho-','-Er-','-Tm-','-Yb-','-Lu-','-Hf-',
     4 '-Ta-','-W -','-Re-','-Os-','-Ir-','-Pt-',
     5 '-Au-','-Hg-','-Tl-','-Pb-','-Bi-','-Po-',
     6 '-At-','-Rn-','-Fr-','-Ra-','-Ac-','-Th-',
     7 '-Pa-','-U -','-Np-','-Pu-','-Am-','-Cm-',
     8 '-Bk-','-Cf-','-Es-','-Fm-','-Md-','-No-',
     9 '-Lr-',' ',' ',' ',' ',' ',' ','-Err'/
      DATA BLANK4/'    '/
      DO 10 I=1,4
   10 ZABCD(I)=BLANK4
C ----SPECIAL HANDLING FOR SPECIAL ENDF/B MATERIALS.
      IF(ZA.GE.1000) GO TO 20
      CALL COMPND(ZA,ZABCD)
      GO TO 30
C ----INSURE Z IS IN LEGAL RANGE.
   20 Z=ZA/1000
      A=ZA-1000*Z
      IF(Z.LT.1.OR.Z.GT.110) Z=110
C ----PACK ZZZ-SS-AAA INTO CHARACTER FORM.
      CALL PACKZA(Z,A,MSTAT1,ZATAB(Z),ZABCD)
C ----DEFINE LENGTH OF PACKED ZA.
   30 CALL SIZER(ZABCD,IZABCD,16)
      RETURN
      END
      SUBROUTINE PACKZA(Z,A,MSTAT1,ZATAB,ZABCD)
C
C     PACK ZZZ-SS-AAAM INTO CHARACTER FORM.
C
      INTEGER Z,A
      CHARACTER*1 ZATAB,ZABCD,DIGITS,BLANK,MSTAT1,DASH
      DIMENSION ZATAB(4),ZABCD(16),DIGITS(10),MULT(3)
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
      DATA BLANK/' '/
      DATA DASH/'-'/
      DATA MULT/1,10,100/
C ----INITIALIZE CHARACTERS AND INDEX.
      DO 10 I=1,16
   10 ZABCD(I)=BLANK
      I=0
C ----PACK Z.
      J=3
      IF(Z.LT.100) J=2
      IF(Z.LT.10) J=1
      MULTZ=MULT(J)
      DO 20 K=1,J
      IZ=Z/MULTZ
      I=I+1
      ZABCD(I)=DIGITS(IZ+1)
      Z=Z-MULTZ*IZ
   20 MULTZ=MULTZ/10
C ----PACK CHEMICAL SYMBOL.
      DO 30 K=1,4
      I=I+1
   30 ZABCD(I)=ZATAB(K)
C ----PACK A.
      J=3
      IF(A.LT.100) J=2
      IF(A.LT.10) J=1
      MULTA=MULT(J)
      DO 40 K=1,J
      IA=A/MULTA
      I=I+1
      ZABCD(I)=DIGITS(IA+1)
      A=A-MULTA*IA
   40 MULTA=MULTA/10
C ----IF METASTABLE STATE FLAG IS NOT BLANK ADD IT TO STRING.
      IF(MSTAT1.NE.BLANK) CALL METAST(ZABCD,MSTAT1,I)
      RETURN
      END
      SUBROUTINE MTHOL(MT,MTBCD,IMTBCD,MSTAT2)
C
C     DEFINE HOLLERITH EQUIVALENT OF REACTION (MT).
C
C     DEFINITION OF TABLES
C
C     MTTAB = CHARACTER EQUIVALENT OF EACH MT NUMBER
C              UP TO 40 CHARACTERS PER MT - LEFT ADJUSTED
C     IMTTAB = (1) NUMBER OF CHRACTERS IN EACH CHARACTER EQUIVALENT
C              (2) LOWER MT LIMIT
C              (3) UPPER MT LIMIT
C
C     THE CHARACTER EQUIVALENT IS RETURNED IN MTBCD
C
C     MTBCD  = CHARACTER EQUIVALENT OF MT
C              UP TO 40 CHARACTERS - LEFT ADJUSTED
C     IMTBCD = NUMBER OF CHARACTERS IN CHARACTER EQUIVALENT OF MT
C
      INTEGER OUTP
      CHARACTER*4 MTTAB,MTBCD,UNKNOW,ZAPBCD,BLANK,MSTATP,MSTAT2
      COMMON/RATZA/IZARAT,MTRAT,MFIN
      COMMON/TABMTC/MTTAB(10,300)
      COMMON/TABMTI/MTLONG,IMTTAB(3,300)
      DIMENSION MTBCD(20),UNKNOW(10),ZAPBCD(20)
      DATA BLANK/'    '/
      DATA UNKNOW/
     1 '***U','ndef','ined','***',' ',' ',' ',' ',' ',' '/
C ----LOOK UP CHARACTER EQUIVALENT AND LOAD INTO CORE.
      DO 20 M=1,MTLONG
      IF(MT-IMTTAB(3,M)) 10,50,20
   10 IF(MT-IMTTAB(2,M)) 30,50,50
   20 CONTINUE
C ----MT NUMBER NOT DEFINED. LOAD **UNDEFINED**
   30 DO 40 L=1,10
   40 MTBCD(L)=UNKNOW(L)
      IMTBCD=15
      RETURN
C ----MT NUMBER IS DEFINED.
   50 DO 60 L=1,10
   60 MTBCD(L)=MTTAB(L,M)
      IMTBCD=IMTTAB(1,M)
C ----IF MT=9000-9999 (PARTICLE/ISOTOPE PRODUCTION) ADD PRODUCT ZA
C ----BEFORE MT I.D.
      IF(MT.LT.9000) GO TO 70
      CALL ZAHOL(IZARAT,MSTAT2,ZAPBCD,KZAP)
      CALL PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)
      GO TO 80
C ----IF FINAL METASTABLE STATE FLAG IS NOT BLANK ADD IT TO STRING.
   70 IF(MSTAT2.NE.BLANK) CALL METAST(MTBCD,MSTAT2,IMTBCD)
   80 RETURN
      END
      SUBROUTINE METAST(BCD,MSTATE,IBCD)
C
C     ADD METASTABLE STATE FLAG TO ZA OR REACTION.
C
C     ON ENTRY INTO THIS ROUTINE IBCD IS THE LENGTH OF THE CHARACTER
C     STRING DESCRIBING THE ZA AND/OR REACTION. UNLESS THE METASTABLE
C     STATE FLAG IS BLANK IT WILL BE ADDED TO THE STRING.
C
      CHARACTER*1 BCD,MSTATE,MSTAB1,MSTAB2,BLANK
      DIMENSION BCD(40),MSTAB1(14),MSTAB2(3,15)
      DATA BLANK/' '/
      DATA MSTAB1/
     1 ' ','T','G','M','1','2','3','4','5','6','7','8','9','+'/
      DATA MSTAB2/
     1 ' ',' ',' ',  ' ',' ',' ',  '-','G',' ',  '-','M',' ',
     2 '-','M','1',  '-','M','2',  '-','M','3',  '-','M','4',
     3 '-','M','5',  '-','M','6',  '-','M','7',  '-','M','8',
     4 '-','M','9',  '-','M','+',  '-','M','?'/
C ----LOOK UP ONE CHARACTER STATE FLAG.
      DO 10 I=1,14
      IF(MSTATE.EQ.MSTAB1(I)) GO TO 20
   10 CONTINUE
      I=15
   20 DO 30 J=1,3
      IF(MSTAB2(J,I).EQ.BLANK) GO TO 40
      IBCD=IBCD+1
   30 BCD(IBCD)=MSTAB2(J,I)
   40 RETURN
      END
      SUBROUTINE SIZER(BCD,IBCD,MAXBCD)
C
C     DEFINE NON-BLANK LENGTH OF CHARACTER STRING.
C
      CHARACTER*1 BCD,BLANK
      DIMENSION BCD(MAXBCD)
      DATA BLANK/' '/
      IBCD=MAXBCD
      DO 10 I=1,MAXBCD
      IF(BCD(IBCD).NE.BLANK) GO TO 20
   10 IBCD=IBCD-1
   20 RETURN
      END
      SUBROUTINE LEFTY(BCD,IBCD,NBCD,KBCD)
C
C     LEFT ADJUST TITLES AND DEFINE NUMBER OF NON-BLANK CHARACTERS AND
C     NUMBER OF PLOTTED CHARACTERS (NON-BLANK MINUS CONTROL CHARACTERS).
C
      CHARACTER*1 BCD,BLANK,CHRTAB,CHRTRL
      COMMON/SYMTB1/CHRTAB(256),CHRTRL(256)
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
      DIMENSION BCD(KBCD)
      DATA BLANK/' '/
C ----FIND LAST NON-BLANK CHARACTER.
      I=KBCD
      DO 10 II=1,KBCD
      IF(BCD(I).NE.BLANK) GO TO 20
   10 I=I-1
      IBCD=0
      NBCD=0
      GO TO 70
C ----FIND FIRST NON-BLANK CHARACTER.
   20 DO 30 J=1,I
      IF(BCD(J).NE.BLANK) GO TO 40
   30 CONTINUE
      IBCD=0
      NBCD=0
      GO TO 70
   40 IBCD=I
C ----IF REQUIRED SHIFT CHARACTERS LEFT.
      IF(J.EQ.1) GO TO 70
      K=0
      DO 50 L=J,I
      K=K+1
   50 BCD(K)=BCD(L)
      IBCD=K
      K=K+1
      DO 60 I=K,KBCD
   60 BCD(I)=BLANK
      CONTINUE
C ----COUNT SPECIAL NON-PRINTING CHARACTERS.
   70 NBCD=IBCD
      IF(ICNTRL.LE.0) GO TO 100
      DO 90 I=1,IBCD
      DO 80 J=1,ICNTRL
      IF(BCD(I).NE.CHRTRL(J)) GO TO 80
      NBCD=NBCD-1
      GO TO 90
   80 CONTINUE
   90 CONTINUE
  100 RETURN
      END
      SUBROUTINE MFHOL(MF,MT,MFBCD,IMFBCD)
C
C     DEFINE HOLLERITH EQUIVALENT OF DATA TYPE (MF).
C
      CHARACTER*4 MFBCD,MFTAB,ERROR
      COMMON/TABMFC/MFTAB(8,100)
      COMMON/TABMFI/MFLONG,IMFTAB(4,100)
      DIMENSION MFBCD(8),ERROR(8)
      DATA ERROR/'*** ','Erro','r **','*',' ',' ',' ',' '/
C ----LOOK UP MF AND MT RANGE IN TABLE.
      DO 10 I=1,MFLONG
      IF(MF.NE.IMFTAB(2,I)) GO TO 10
      IF(MT.GE.IMFTAB(3,I).AND.MT.LE.IMFTAB(4,I)) GO TO 30
   10 CONTINUE
C ----MF AND MT RANGE IS NOT IN TABLE.
      IMFBCD=13
      DO 20 K=1,8
   20 MFBCD(K)=ERROR(K)
      RETURN
C ----DEFINE MF AND MT RANGE EQUIVALENT.
   30 IMFBCD=IMFTAB(1,I)
      DO 40 K=1,8
   40 MFBCD(K)=MFTAB(K,I)
      RETURN
      END
      SUBROUTINE PAKZAP(ZAPBCD,KZAP,MTBCD,IMTBCD)
C
C     COMBINE PRODUCT ZA WITH MT I.D. FOR PARTICLE/ISOTOPE PRODUCTION.
C
      CHARACTER*1 ZAPBCD,MTBCD,MTPBCD,BLANK
      DIMENSION ZAPBCD(12),MTBCD(80),MTPBCD(80)
      DATA BLANK/' '/
C ----PACK PRODUCT ZA.
      I=0
      DO 10 J=1,KZAP
      I=I+1
   10 MTPBCD(I)=ZAPBCD(J)
C ----INSERT BLANK AND THEN MT I.D.
      I=I+1
      MTPBCD(I)=BLANK
      DO 20 J=1,IMTBCD
      I=I+1
   20 MTPBCD(I)=MTBCD(J)
C ----DEFINE NEW MT I.D. LENGTH AND COPY BACK TO MT I.D.
      IMTBCD=I
      DO 30 J=1,IMTBCD
   30 MTBCD(J)=MTPBCD(J)
      RETURN
      END
      SUBROUTINE COMPND(ZA,ZABCD)
C
C     DEFINE ZA EQUIVALENCE FOR ENDF/B SPECIAL MATERIALS AND COMPOUNDS.
C
      INTEGER ZA
      CHARACTER*4 ZABCD,ZATAB,BLANK4,ERROR
      COMMON/TABZAC/ZATAB(3,200)
      COMMON/TABZAI/MZLONG,IZATAB(200)
      DIMENSION ZABCD(4),ERROR(3)
C ----DEFINE NUMBER OF SPECIAL MATERIALS AND COMPOUNDS IN TABLE.
      DATA BLANK4/'    '/
      DATA ERROR/'** E','rror',' ** '/
      ZABCD(4)=BLANK4
C ----ERROR IF ZA IS NOT LESS THAN 1000.
      IF(ZA.GE.1000) GO TO 20
C ----LOOK UP ZA IN TABLE.
      DO 10 N=1,MZLONG
      IF(IZATAB(N)-ZA) 10,40,20
   10 CONTINUE
C ----ZA IS NOT IN TABLE.
   20 DO 30 I=1,3
   30 ZABCD(I)=ERROR(I)
      RETURN
C ----MATCH FOUND. DEFINE HOLLERITH EQUIVALENCE.
   40 DO 50 I=1,3
   50 ZABCD(I)=ZATAB(I,N)
      RETURN
      END
      SUBROUTINE META10(OUT,MSTATE)
C
C     DEFINE CHARACTER EQUIVALENT OF METASTABLE STATE FLAG
C
      CHARACTER*1 OUT,MSTATE,MTAB1,MTAB2
      DIMENSION MTAB1(10),MTAB2(10)
      DATA MTAB1/
     1 'G','1','2','3','4','5','?','M','+',' '/
      DATA MTAB2/
     1 '0','1','2','3','4','5','6','7','8','9'/
C ----LOOK UP METASTABLE STATE CHARACTER.
      DO 10 I=1,10
      IF(OUT.EQ.MTAB2(I)) GO TO 20
   10 CONTINUE
C ----SET INDEX TO UNKNOWN.
      I=7
   20 MSTATE=MTAB1(I)
      RETURN
      END
      SUBROUTINE NUMBRH(XI,YI,HT,Z,ANG,IZ)
C
C     CONVERT FLOATING POINT NUMBER OF CHARACTER STRING AND PLOT WIDE.
C
      CHARACTER*1 DIGIT,MINUS,DOT,FIELD
      DIMENSION DIGIT(10),FIELD(16)
      DATA DIGIT/'1','2','3','4','5','6','7','8','9','0'/
      DATA MINUS/'-'/
      DATA DOT/'.'/
C
C     ROUND-OFF NUMBER TO REQUIRED DIGITS, E.G. IF WRITING 54.321 WITH
C     2 DIGITS AFTER DECIMAL POINT DEFINE MR=5432 (ENTIRE STRING) AND
C     MR1=54 (DIGITS PRECEDING DECIMAL POINT).
C
      AZ=ABS(Z)
      IIZ=IZ
      IF(IIZ.LE.0) IIZ=0
      Z10=10.0**IIZ
      MR=AZ*Z10+0.5
      IZ10=Z10
      MR1=MR/IZ10
C
C     DETERMINE NUMBER OF DIGITS PRECEDING DECIMAL POINT.
C
      M10=1
      DO 10 IDIG=1,12
      NR=MR1/M10
      IF(NR.LE.0) GO TO 20
   10 M10=10*M10
C
C     NUMBER IS TOO BIG...NO PLOTTING.
C
      RETURN
   20 IF(IDIG.EQ.1) GO TO 30
      IDIG=IDIG-1
      M10=M10/10
C
C     ADD DIGITS AFTER DECIMAL POINT.
C
   30 IDIG=IDIG+IIZ
      M10=M10*IZ10
C
C     IF NUMBER IS ZERO, PLOT ZERO AND RETURN.
C
      IF(IDIG.GT.0) GO TO 40
      IFIELD=1
      FIELD(1)=DIGIT(10)
      GO TO 80
C
C     INITIALIZE CHARACTER COUNT.
C
   40 IFIELD=0
C
C     IF NUMBER IS NEGATIVE INSERT MINUS SIGN.
C
      IF(Z.GE.0.0) GO TO 50
      FIELD(1)=MINUS
      IFIELD=1
C
C     DEFINE POSITION OF DECIMAL POINT (IF ANY).
C
   50 IDOT=IDIG-IZ
      IF(IZ.LT.0) IDOT=IDIG+2
C
C     INSERT DIGITS AND DECIMAL POINT (IF ANY) IN STRING.
C
      DO 70 I=1,IDIG
      NDIG=MR/M10
      KDIG=NDIG
      IF(KDIG.EQ.0) KDIG=10
      IFIELD=IFIELD+1
      FIELD(IFIELD)=DIGIT(KDIG)
C
C     INSERT DECIMAL POINT AT APPROPRIATE POSITION.
C
      IF(I.NE.IDOT) GO TO 60
      IFIELD=IFIELD+1
      FIELD(IFIELD)=DOT
   60 MR=MR-M10*NDIG
   70 M10=M10/10
C
C     ENTIRE FIELD FORMATTED. PLOT IT.
C
   80 CALL SYMBLH(XI,YI,HT,FIELD,ANG,IFIELD)
      END
      SUBROUTINE SYMBLH(X,Y,HTIN,MESS,ANG,NMESS)
C
C     DRAW THICK CHARACTERS DEFINED BY STROKES IN SYMBOL TABLE
C
      CHARACTER*1 MESS,CHRTAB,CHRTRL
      DIMENSION MESS(NMESS)
      COMMON/SYMTB1/CHRTAB(256),CHRTRL(256)
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
      COMMON/THICKY/ITHICK,THICK
      COMMON/PAINT/BOX,BOX2,BOX4,BOXWT2,HT,HT2,HTH,HT34,WT,WTH,WT38
C ----INITIALIZE FLAG TO USE STANDARD CHARACTER SET.
      DATA IALTER/0/
C ----INITIALIZE X AND Y OFFSET (USED FOR SUB AND SUPER SCRIPTS).
      DATA XOFF/0.0/
      DATA YOFF/0.0/
C ----IF NO SOFTWARE CHARACTERS RETURN.
      IF(ICHR.LE.0) RETURN
C ----SAVE LINE WIDTH AND DEFINE SCALED LINE WIDTH.
      NTHICK=ITHICK
      ITHICK=FLOAT(NTHICK)*HTIN/HT
C ----INITIALIZE POSITION AND DEFINE INCREMENTS.
      X1=X
      Y1=Y
      IWAY=0
      IF(ABS(ANG).GT.1.0) IWAY=1
      IF(IWAY.NE.0) GO TO 10
      DX1=HTIN
      DY1=0.0
      GO TO 20
   10 DX1=0.0
      DY1=HTIN
C ----SET UP LOOP TO PLOT CHARACTERS ONE AT A TIME.
   20 DO 120 N=1,NMESS
C ----INITIALIZE COUNT OF THE NUMBER OF TIMES CHARACTER HAS BEEN FOUND
C ----(TO SELECT STANDARD OR ALTERNATE CHARACTER SET).
      NALTER=0
      DO 40 I=1,ICHR
      IF(MESS(N).NE.CHRTAB(I)) GO TO 40
C ----ALWAYS USE CONTROL CHARACTERS REGARDLESS OF CHARACTER SET.
      I1=INDCHR(1,I)
      IF(ICHPEN(I1).LE.0) GO TO 50
C ----SELECT STANDARD OR ALTERNATE CHARACTER SET.
      IF(NALTER.NE.IALTER) GO TO 30
C ----CHARACTER FOUND.
      I2=INDCHR(2,I)
      GO TO 70
   30 NALTER=NALTER+1
   40 CONTINUE
C ----NOT SPECIAL CHARACTER...IGNORE.
      GO TO 120
C ----CONTROL CHARACTER...CHANGE CHARACTER SET OR RE-DEFINE OFFSET.
   50 IF(ICHPEN(I1).EQ.0) GO TO 60
C ----CHANGE CHARACTER SETS.
      IALTER=1-IALTER
      GO TO 120
C ----DEFINE OFFSET.
   60 XOFF=XOFF+XCHAR(I1)
      YOFF=YOFF+YCHAR(I1)
      GO TO 120
   70 IF(IWAY.NE.0) GO TO 90
C ----HORIZONTAL.
      DO 80 I=I1,I2
   80 CALL PLOTH(X1+HTIN*(XOFF+XCHAR(I)),Y1+HTIN*(YOFF+YCHAR(I)),
     1 ICHPEN(I))
C ----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSET
      X1=X1+XOFF
      GO TO 110
C ----VERTICAL.
   90 DO 100 I=I1,I2
  100 CALL PLOTH(X1-HTIN*(YOFF+YCHAR(I)),Y1+HTIN*(XOFF+XCHAR(I)),
     1 ICHPEN(I))
C ----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSET
      Y1=Y1+XOFF
C ----MOVE TO NEXT CHARACTER POSITION.
  110 X1=X1+DX1
      Y1=Y1+DY1
C ----TURN OFF ALTERNATE CHARACTER SET FLAG AND SET OFFSET TO ZERO.
      IALTER=0
      XOFF=0.0
      YOFF=0.0
  120 CONTINUE
C ----RESTORE LINE WIDTH.
      ITHICK=NTHICK
      RETURN
      END
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
      COMMON/SYMTB2/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHR,ICNTRL,ISYM
C ----DEFINE AVAILABLE CORE SIZE.
      DATA ICORE/5000/
C ----INITIALIZE CHARACTER INDEX AND COUNT OF CONTROL CHARACTERS.
      IHIGH=0
      ICNTRL=0
C ----SET UP LOOP TO READ UP TO 256 SPECIAL CHARACTERS.
      DO 30 JCHR=1,256
      ICHR=JCHR
C ----READ FIRST LINE TO DEFINE CHARACTER AND NUMBER OF STROKES.
      READ(ISYM,60,ERR=40,END=40) CHRTAB(ICHR),ICOUNT
      LOWEST=IHIGH+1
      IHIGH=IHIGH+ICOUNT
C ----INSURE AVAILABLE CORE WILL NOT BE EXCEEDED.
      IF(IHIGH.GT.ICORE) GO TO 40
C ----READ STROKES (X, Y, PEN POSITION).
      DO 10 I=LOWEST,IHIGH
   10 READ(ISYM,70,ERR=40,END=40) XCHAR(I),YCHAR(I),ICHPEN(I)
C ----SAVE CONTROL CHARACTERS.
      IF(ICOUNT.NE.1) GO TO 20
      IF(ICHPEN(LOWEST).GT.0) GO TO 20
      ICNTRL=ICNTRL+1
      CHRTRL(ICNTRL)=CHRTAB(ICHR)
C ----DEFINE INDICES TO SPECIAL CHARACTER STROKE TABLE.
   20 INDCHR(1,ICHR)=LOWEST
   30 INDCHR(2,ICHR)=IHIGH
      ICHR=256
      GO TO 50
C ----END OF DATA OR ERROR.
   40 ICHR=ICHR-1
      IF(ICHR.LT.1) STOP 'SYMIN ERROR - No symbols loaded'
   50 RETURN
   60 FORMAT(1X,A1,I5)
   70 FORMAT(2F7.3,I5)
      END
      SUBROUTINE PLOTH(X,Y,IPEN)
C
C     PLOT FROM LAST (X,Y) TO CURRENT (X,Y) EITHER NORMAL OR THICK LINE.
C
      COMMON/THICKY/ITHICK,THICK
C ----IF MOVE, NOT DRAW OR NORMAL LINE DO IT WITH 1 STROKE.
      IF(IPEN.NE.2.OR.ITHICK.LE.0) GO TO 30
      DXY=SQRT((X-XL)**2+(Y-YL)**2)
      IF(DXY.LE.0.0) GO TO 40
C ----DEFINE DIRECTION COSINE AND SINE.
      COST=(Y-YL)/DXY
      SINT=(X-XL)/DXY
C ----DEFINE OFFSET FOR LINE THICKNESS.
      DXT=-THICK*COST
      DYT=THICK*SINT
C ----DRAW THICK LINE.
      CALL PLOT(X,Y,2)
      DXT1=DXT
      DYT1=DYT
      DO 10 I=1,ITHICK
      CALL PLOT(XL+DXT,YL+DYT,3)
      CALL PLOT(X+DXT,Y+DYT,2)
      DXT=DXT+DXT1
   10 DYT=DYT+DYT1
      DXT=DXT1
      DYT=DYT1
      DO 20 I=1,ITHICK
      CALL PLOT(XL-DXT,YL-DYT,3)
      CALL PLOT(X-DXT,Y-DYT,2)
      DXT=DXT+DXT1
   20 DYT=DYT+DYT1
      CALL PLOT(X,Y,3)
      GO TO 40
C ----DRAW NORMAL WIDTH LINE.
   30 CALL PLOT(X,Y,IPEN)
   40 XL=X
      YL=Y
      RETURN
      END

