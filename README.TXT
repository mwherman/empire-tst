                 E M P I R E  3.1 (Rivoli)
               Sao Jose dos Campos, Brazil   
                       June 2011
                    Upton, NY, USA   
                      January 2012

RECOMMENDED SETUP 
_________________

1) You may create your new EMPIRE-3.1 directory [e.g. /home/empire-3.1-rivoli] 
and change to it by typing:

cd /home/empire-3.1-rivoli
 
2) Untar the distribution file EMPIRE-3.1-RIVOLI.tgz by typing 

tar xvfz EMPIRE-3.1-RIVOLI.tgz

3) Run install.sh script and follow instructions: 

install.sh

--- setup finished 

4) To run empire you may create a working subdirectory of your empire root 
[e.g. /home/empire-3.1-rivoli/Fe56new], change to it and launch the EMPIRE GUI (Xrun.tcl):

mkdir Fe56new
cd Fe56new
../scripts/Xrun.tcl & 

Good calculations !
The EMPIRE team



MANUAL SETUP 
____________

1) You may create your new EMPIRE-3.1 directory [e.g. /home/empire-3.1-rivoli]
2) Untar the distribution file EMPIRE-3.1-RIVOLI.tgz
3) Untar the HFB level-density distribution file level-densities-hfb.tgz to the 
       corresponding subdirectory RIPL/densities/total/level-densities-hfb 
       (if you plan to use RIPL HFB level densities)
4) Unzip the distribution file C4-latest.zip to the corresponding subdirectory 
       EXFOR/ to get the automatic retrieval and plotting of EXFOR experiemntal 
       data.  One is available on the EMPIRE download site and from the IAEA at
       http://www-nds.iaea.org/x4toc4-master/.   Once you unzip it, run the 
       parseC4.py script.

--- setup finished 

4) To run empire you may create a working subdirectory of your empire root 
[e.g. /home/empire-3.1-rivoli/Fe56new], change to it and launch the EMPIRE GUI (Xrun.tcl):

mkdir Fe56new
cd Fe56new
../scripts/Xrun.tcl & 

Good calculations !
The EMPIRE team
_______________________________________________________________________
Report problems to: r.capotenoy@iaea.org (IAEA); mwherman@bnl.gov (BNL)