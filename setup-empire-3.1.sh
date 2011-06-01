#! /bin/sh -
#script to install and compile EMPIRE-3.1 package
#
#directory in which EMPIRE is to be installed
instdir=$HOME
#directory with EMPIRE distribution
sourcedir=`pwd`
#EMPIRE version number
empver='3.1'
echo '                                                           '
echo '    EEEEE  M    M  PPPP   I  RRRR   EEEEE      33333       '
echo '    E      MM  MM  P   P  I  R   R  E             33       '
echo '    EEE    M MM M  PPPP   I  RRRR   EEE    =   33333       '
echo '    E      M    M  P      I  R  R   E              3       '
echo '    EEEEE  M    M  P      I  R   R  EEEEE      33333       '
echo '                                                           '
echo '        A R C O L E (release '$empver'), June 2011         '
echo '_______________________________________________________________________________________'
echo '                                                                                       '
echo ' Please refer to:                                                                      '
echo ' 1)M.Herman, R.Capote, B.Carlson, P.Oblozinsky, M.Sin, A.Trkov, H.Wienke, and V.Zerkin '
echo '  "EMPIRE: Nuclear Reaction Model Code System for data evaluation",                    '
echo '   Nuclear Data Sheets 108 (2007) 2655-2715                                            '
echo '                                                                                       '
echo ' 2)R.Capote, M.Herman, P.Oblozinsky, P.G.Young, S.Goriely, T.Belgya, A.V.Ignatyuk,     '
echo '   A.J.Koning, S.Hilaire, V.A.Plujko, M.Avrigeanu, Zhigang Ge, Yinlu Han, S.Kailas,    '
echo '   J. Kopecky, V.M. Maslov, G.Reffo, M.Sin, E.Sh.Soukhovitskii, and P.Talou,           '
echo '  "RIPL - Reference Input Parameter Library for Calculation of Nuclear Reactions and   '
echo '   Nuclear Data Evaluations", Nuclear Data Sheets 110 (2009) 3107-3214                 '
echo '   RIPL available online at http://www-nds.iaea.org/RIPL-3/                            '
echo '_______________________________________________________________________________________'
echo ' '
echo 'This is the EMPIRE-3 (release '$empver') installation script. It will lead you through '
echo 'the set-up procedure and eventually compile the whole the package. '
echo ' '
echo '  Press ENTER to start the EMPIRE setup, CTRL-C to cancel'
read dir
#if [ ! "$dir" = "" ]; then
#   sourcedir=$dir
#fi
if [ -f $sourcedir/EMPIRE-$empver.tgz ]; then
   echo ' '
   echo 'Setup file EMPIRE-'$empver'.tgz found'
   echo ' '
else
   echo ' '
   echo 'EMPIRE-'$empver'.tgz not found in '$sourcedir', please restart the setup script in '
   echo ' the directory containing the EMPIRE-'$empver'.tgz file'
   exit
fi
echo 'Specify a directory in which EMPIRE-3 should be installed.'
echo ' A subdirectory of your '$HOME' directory will be created or the existing directory will be used.'
echo ' Hit return to accept default directory: ['$HOME/empire']:        '
read dir
instdir=$HOME/empire
if [ ! "$dir" = "" ]; then
   instdir=$HOME/$dir
fi
echo ' '
echo 'EMPIRE will be installed into the directory ['$instdir']:'
echo ' '
echo '  Press any key to confirm this setup, CTRL-C to cancel '
read dir
mkdir $instdir  
EMPIREDIR=$instdir
export EMPIREDIR
cd $instdir
echo ' '
echo 'Exploding EMPIRE-3 release $empver source'
tar xzvf $sourcedir/EMPIRE-$empver.tgz
echo ' '
echo 'EMPIRE-3 system decompressed in the directory '$instdir
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir
echo ' '
if [ -f $sourcedir/level-densities-hfb.tgz ]; then
   echo ' '
   echo 'HFB level density setup file found'
   echo ' '
else
   echo ' '
   echo 'level-densities-hfb.tgz not found in '$sourcedir', please restart the '
   echo ' setup script in the directory containing the EMPIRE-'$empver'.tgz file'
   echo ' '
   exit
fi
echo 'Exploding microscopic HFB level densities in the directory '$instdir'/RIPL-2/densities/total/level-densities-hfb'
cd $instdir/RIPL-2/densities/total/level-densities-hfb
tar xzvf $sourcedir/level-densities-hfb.tgz
echo ' '
echo 'RIPL-3 HFB level densities decompressed in directory '$instdir/RIPL-2/densities/total/level-densities-hfb
cd $instdir
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir
echo ' '
if [ -f $sourcedir/C4-IAEA-Jan2011.tgz ]; then
   echo ' '
   echo 'EXFOR(C4) setup file found'
   echo ' '
else
   echo ' '
   echo 'C4-IAEA-Jan2011.tgz not found in '$sourcedir', please restart the '
   echo ' setup script in the directory containing the EMPIRE-'$empver'.tgz file'
   exit
fi
echo 'Exploding EXFOR(C4) experimental data retrieved from IAEA/NDS'
tar xzvf $sourcedir/C4-IAEA-Jan2011.tgz
echo ' '
echo 'EXFOR(C4) data decompressed in the directory '$instdir
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir
echo ' '
# Installing Tcl/Tk packages
echo ' '
echo 'We will need Tcl/Tk and Itcl packages to run EMPIRE GUI. Although your system    '
echo 'might have these packages installed it often happens that they are not compatible'
echo 'with EMPIRE. To be on a safe side we would install industry standard ActiveTcl   '
echo 'package that runs on most Linuxes.'
echo ' '
echo 'The ActiveTcl graphical setup will request the Tcl installation directory:       '
echo 'Please specify installation directory (default:/usr/local/username               '
echo ' '
echo 'You should overwrite the default selection by writing '$instdir/ActiveTcl        '
echo ' '
echo 'Please note that this package will be installed locally in the empire directory  '
echo 'and will not mess up with your native Tcl/Tk installation (if any).              '
echo ' '
echo 'Do you want to proceed with installation of the ActiveTcl package y/n [y]:'
read yesno
if [ ! "$yesno" = "" ]; then
        activetcl=$yesno
        else
        activetcl=y
fi
echo ' '
if [ "$activetcl" = "y" ]; then
  if [ -f $sourcedir/ActiveTcl8.4.7.0-linux-ix86-108887.tar.gz ]; then
     echo 'exploding ActiveTcl'
#    gunzip $sourcedir/ActiveTcl8.4.7.0-linux-ix86-108887.tar.gz
#    tar xzf $sourcedir/ActiveTcl8.4.7.0-linux-ix86-108887.tar
     tar xvzf $sourcedir/ActiveTcl8.4.7.0-linux-ix86-108887.tar.gz
     cd ActiveTcl8.4.7.0-linux-ix86
     sh install.sh
     cd ../
     echo 'DO NOT FORGET TO ADD THE LINE AS ADVISED, THEN LOGOFF AND LOGIN! '
  else
     echo 'ActiveTcl installation failed, '
     echo '$sourcedir/ActiveTcl8.4.7.0-linux-ix86-108887.tar.gz file not found'
  fi
     echo 'If there were errors during installation of the TCL/Tk packages'
     echo 'you will have to pick up CD-ROM(s) with your system and install'
     echo 'Tcl, Tk, and itcl packages to be able to use GUIs'
fi
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir
echo ' '
echo '_____________________________________________________________________'
cd $instdir
echo ' '
echo 'Now we will compile the whole EMPIRE package.'
#echo 'By default Intel FORTRAN compiler will be used.'
echo ' '
echo 'By default gfortran GNU FORTRAN compiler will be used.'
echo 'It is freely available for download and distributed in all LINUX packages.'
echo 'Please note that gfortran version > 4.2 is required !!'
echo ' '
echo 'Compiling EMPIRE and preprocessing codes'
echo ' '
echo 'The warning in lev-dens.f:'
echo '   100 READ (34,99010,ERR = 100,END = 300) car2, izr, iar, paritate '
echo 'is expected.'
echo ' '
make
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir
echo ' '
cd $HOME
#
# Modifying .bashrc
#
echo 'Adding EMPIREDIR to .bashrc file'
#
newpath=./:$instdir/scripts:$instdir/ActiveTcl/bin:$PATH
if [ "$activetcl" = "n" ]; then
newpath=./:$instdir/scripts:$PATH
fi
#
cat >>.bashrc <<EOF
# ___________________________________________________________________
# Lines added by EMPIRE-3.1 setup
#
# It is assumed that ActiveTcl was installed in $HOME/ActiveTcl
#
# EMPIREDIR is pointing to the active EMPIRE installation
#
EMPIREDIR=$instdir
echo '
echo 'EMPIRE 3.1 directory set to '$instdir 
echo '
PATH=$newpath
export EMPIREDIR PATH 
EOF
source .bashrc 
EMPIREDIR=$instdir
PATH=$newpath
export EMPIREDIR PATH 
echo 'EMPIREDIR environment variable set to '$EMPIREDIR    
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir
echo ' '
echo '______________________________________________________________________'
echo ' '
echo ' We extended your PATH variables to get access to ActiveTcl and       '
echo ' empire binaries for a shell sh enviroment (.bashrc)                  '
echo ' '
echo ' For a sh or similar perform                                          '
echo '     EMPIREDIR="'$instdir'"                                           '
echo '     PATH="'$instdir/ActiveTcl/bin:./:$instdir/scripts':$PATH"        '
echo '     export EMPIREDIR PATH                                            '
echo ' '
echo ' For a csh or compatible perform                                      '
echo '     setenv EMPIREDIR "'$instdir'"                                    '
echo '     setenv PATH "'$instdir/ActiveTcl/bin:./:$instdir/scripts':$PATH" '
echo '     export EMPIREDIR PATH                                            '
echo ' '
echo ' Some shells (bash for example) allow                                 '
echo '     export PATH="'$instdir/ActiveTcl/bin:./:$instdir/scripts':$PATH" '
echo ' '
echo ' Once you modify your path variables close your console and reopen it '
echo '______________________________________________________________________'
echo '                                                                      '
echo ' Additional information and previous versions available at            '
echo ' EMPIRE homepages:                                                    '
echo ' International Atomic Energy Agency, Vienna, Austria                  '
echo '       @ http://www-nds.iaea.org/empire/                              '                      
echo ' Brookhaven National Laboratory, NNDC, New York, USA                  '             
echo '       @ http://www.nndc.bnl.gov/empire-3.1/                          '
echo ' '
echo '______________________________________________________________________'
echo ' '
echo 'Assuming your working directory is '$instdir/work', you should type:  '
echo ' '
echo ' cd '$instdir/work
echo ' $instdir/scripts/Xrun.tcl &'
echo ' '
echo ' to start EMPIRE GUI.'
echo ' '
echo ' Bons calculos !'
echo ' The EMPIRE team, Sao Jose do Campos, Brazil, May 2011.'
echo ' '
echo '  Press ENTER to launch EMPIRE GUI, CTRL-C to cancel'
read dir
echo ' '
cd $instdir/work
$instdir/scripts/Xrun.tcl &
exit

