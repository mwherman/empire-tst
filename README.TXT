                 E M P I R E  3.1 (Rivoli)
               Sao Jose dos Campos, Brazil   
                       June 2011
                    Upton, NY, USA   
                      January 2012

EMPIRE is a modular system of nuclear reaction codes for advanced modeling
of nuclear reactions using various theoretical models. It consists of a
number of FORTRAN codes, input parameter libraries, and experimental data
library (EXFOR/CSISRS) -  operated through the Graphic User Interface (GUI).

EMPIRE 3.1 has been tested successfully on Linux (Ubuntu, Redhat) and Mac OS X
(10.5 and higher).  It can also be run on Windows with either the MinGW and
Cygwin environments.  EMPIRE is provided as source code that must be compiled to 
function.  See the file INSTALL.TXT for installation instructions.

EMPIRE is intended to be a general, flexible, and easy to use tool 
for basic research and evaluation of nuclear data. It offers a  possibility
of combining several theoretical approaches, choosing among alternative 
input parameters and calculating extended set of observables in a single run.
Nuclear data evaluation is facilitated by the ENDF-6 formatting, file 
verification and graphical comparison with experimental data.

Major new features in 3.1
-------------------------
* Covariance capabilities using Kalman filter and Monte Carlo
* Resonance module to produce MF2 and MF32 from the Atlas of Neutron Resonances
* RIPL-3 library of input parameters
* New version of Coupled Channel code ECIS-2006
* Coupled Channel code OPTMAN for soft-rotor calculations
* Improved treatment of fission
* Parity dependent level densities
* New parametrization of EGSM level densities
* Three additional ejectiles (d, t,3He)
* Platform independent retrieval of EXFOR data
* Fully operational on Mac OS X

Installation
------------
See the INSTALL.TXT file included in the EMPIRE distribution.

To run EMPIRE
-------------
To run empire you may create a working subdirectory of your empire root 
[e.g. /home/empire-3.1-rivoli/Fe56new], change to it and launch the EMPIRE GUI (Xrun.tcl):

    $ mkdir Fe56new
    $ cd Fe56new
    $ ../scripts/Xrun.tcl & 


Good calculations !
The EMPIRE team
_______________________________________________________________________
Report problems to: r.capotenoy@iaea.org (IAEA); mwherman@bnl.gov (BNL)