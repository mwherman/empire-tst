#! /bin/sh -
#script to install and compile EMPIRE-3.1 package
#

#directory in which EMPIRE is to be installed
instdir=$HOME
#directory with EMPIRE distribution
sourcedir=`pwd`
#EMPIRE version number
empver=`\grep VERSIONNUMBER version | sed -e 's/VERSIONNUMBER = //g'`
emprelname=`\grep VERSIONNAME version | sed -e 's/VERSIONNAME   = //g'`
XWIN=`uname`


echo '                                                           '
echo '    EEEEE  M    M  PPPP   I  RRRR   EEEEE      33333       '
echo '    E      MM  MM  P   P  I  R   R  E             33       '
echo '    EEE    M MM M  PPPP   I  RRRR   EEE    =   33333       '
echo '    E      M    M  P      I  R  R   E              3       '
echo '    EEEEE  M    M  P      I  R   R  EEEEE      33333       '
echo '                                                           '
echo '              '$emprelname' (release '$empver')            '
echo '                                                           '
echo '         Sao Jose dos Campos, Brazil, May 2011             '
echo '           Upton, New York, USA, January 2012              '
echo '_______________________________________________________________________________'
echo '                                                                               '
echo ' Please refer to:                                                              '
echo ' 1)M.Herman, R.Capote, B.Carlson, P.Oblozinsky, M.Sin, A.Trkov, H.Wienke, and  '
echo '   V.Zerkin, "EMPIRE: Nuclear Reaction Model Code System for data evaluation", '
echo '   Nuclear Data Sheets 108 (2007) 2655-2715                                    '
echo '                                                                               '
echo ' 2)R.Capote, M.Herman, P.Oblozinsky, P.G.Young, S.Goriely, T.Belgya,           '
echo '   A.V.Ignatyuk, A.J.Koning, S.Hilaire, V.A.Plujko, M.Avrigeanu, Zhigang Ge,   '
echo '   S.Kailas, J. Kopecky, V.M. Maslov, G.Reffo, M.Sin, E.Sh.Soukhovitskii, and  '
echo '   P.Talou, "RIPL - Reference Input Parameter Library for Calculation of       '
echo '   Nuclear Reactions and Nuclear Data Evaluations", Nuclear Data Sheets 110    '
echo '   (2009) 3107-3214. RIPL available online at http://www-nds.iaea.org/RIPL-3/  '
echo '_______________________________________________________________________________'
echo ' '
echo 'This is the EMPIRE-3 (release '$empver') installation script. It will lead you '
echo 'through the set-up procedure and eventually compile the whole package.         '
echo ' '


echo '  Press ENTER to start the EMPIRE setup, CTRL-C to cancel'
read dir
#if [ ! "$dir" = "" ]; then
#   sourcedir=$dir
#fi
if [ -f $sourcedir/EMPIRE-$empver-$emprelname.tgz ]; then
   echo ' '
   echo 'Setup file EMPIRE-'$empver'-'$emprelname'.tgz found'
   echo ' '
else
   echo ' '
   echo 'EMPIRE-'$empver'-'$emprelname'.tgz not found in '$sourcedir', please restart the setup'
   echo '  script in the directory containing the EMPIRE-'$empver'-'$emprelname'.tgz file'
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
tar xzf $sourcedir/EMPIRE-$empver-$emprelname.tgz
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
   echo ' setup script in the directory containing the EMPIRE-'$empver-$emprelname'.tgz file'
   echo ' '
   exit
fi
echo 'Exploding HFB level densities in the directory '$instdir'/RIPL/densities/total/level-densities-hfb'
cd $instdir/RIPL/densities/total/
tar xzf $sourcedir/level-densities-hfb.tgz
echo ' '
echo 'RIPL HFB level densities decompressed ('$instdir/RIPL/densities/total/level-densities-hfb')'
cd $instdir
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir

exit

echo ' '
if [ -f $sourcedir/C4-latest.zip ]; then
   echo ' '
   echo 'EXFOR(C4) setup file found'
   echo ' '
else
   echo ' '
   echo 'C4-latest.tgz not found in '$sourcedir', please restart the      '
   echo ' setup script in the directory containing the EMPIRE-'$empver'-'$emprelname'.tgz file'
   exit
fi
echo 'Exploding EXFOR(C4) experimental data retrieved from IAEA/NDS'
tar xzvf $sourcedir/C4-latest.zip
echo ' '
echo 'EXFOR(C4) data decompressed in the directory '$instdir
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir


echo ' '
# Installing Tcl/Tk packages
echo ' '
echo 'We will need Tcl/Tk and itcl packages to run EMPIRE GUI. Although your system    '
echo 'might have these packages installed it often happens that they are not compatible'
echo 'with EMPIRE. To be on a safe side we would install industry standard ActiveTcl   '
echo 'package that runs on most operating systems (Linux,Mac, Windows).'
echo ' '
echo 'Please note that this package will be copied to your '$instdir' directory        '
echo 'and will not mess up with your native Tcl/Tk installation (if any).              '
echo ' '
echo 'Do you want to copy the ActiveTcl package to your '$instdir' directory  y/n [y]:'
read yesno
if [ ! "$yesno" = "" ]; then
        activetcl=$yesno
        else
        activetcl=y
fi
echo ' '
if [ "$activetcl" = "y" ]; then
  if [ "$XWIN" = "Darwin" ]; then
    echo ' '
    if [ -f $sourcedir/ActiveTcl8.4.19.5.294317-macosx-universal-threaded.dmg ]; then
      echo ' '
      echo '********* '
      echo 'Apple Mac users need to install ActiveTcl manually'
      echo ' '
      echo 'Once EMPIRE setup finished please double click on the icon'
      echo ' ActiveTcl8.4.19.5.294317-macosx-universal-threaded.dmg      '
      echo ' in your '$sourcedir' to install the ActiveTcl package in your Mac computer'
      echo '********* '
      echo ' '
    else
      echo 'ActiveTcl copying for Apple Mac failed, '
      echo 'ActiveTcl8.4.19.5.294317-macosx-universal-threaded.dmg file not found in '$sourcedir
      echo 'please find the file and restart the EMPIRE setup script'
      exit
    fi
  else
    echo ' '
    echo 'ActiveTcl for Linux users'
    echo ' '
    echo '******** '
    echo 'The ActiveTcl GUI setup will request the Tcl installation directory as follows:  '
    echo 'Please specify installation directory:/usr/local/username                        '
    echo 'You should overwrite the default selection by writing '$instdir/ActiveTcl'       '
    echo 'Please copy '$instdir/ActiveTcl' to have it available to paste when requested    '
    echo '******** '
    echo ' '
    echo '  Press ENTER to continue the setup, CTRL-C to cancel'
    read dir
    if [ -f $sourcedir/ActiveTcl8.4.7.0-linux-ix86-108887.tar.gz ]; then
      echo 'exploding ActiveTcl'
      tar xvzf $sourcedir/ActiveTcl8.4.7.0-linux-ix86-108887.tar.gz
      cd ActiveTcl8.4.7.0-linux-ix86
      sh install.sh
      cd ../
    else
      echo 'ActiveTcl installation for Linux failed, '
      echo 'ActiveTcl8.4.7.0-linux-ix86-108887.tar.gz file not found in '$sourcedir
      echo 'please find the file and restart the setup script'
      exit
    fi
  fi  
fi


if [ "$XWIN" = "Darwin" ]; then
export DISPLAY=:0
echo '********* '
cp -f $instdir/util/c4zvd/zvv2-1.005-mac.exe $instdir/util/c4zvd/zvview.exe
echo 'Mac ZVView installed'
echo '********* '
fi


echo ' '
cd $HOME
#
# Modifying/Creating .bashrc
#
echo 'Adding EMPIREDIR to .bashrc file'
#
if [ "$XWIN" = "Darwin" ]; then
newpath=./:$instdir/scripts:$PATH
else
newpath=./:$instdir/scripts:$instdir/ActiveTcl/bin:$PATH
fi
if [ "$activetcl" = "n" ]; then
newpath=./:$instdir/scripts:$PATH
fi
#
cat >>.bashrc <<EOF
# ___________________________________________________________________
# Lines added by EMPIRE-3.1 setup
#
# It is assumed that ActiveTcl was installed in $instdir/ActiveTcl
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
EMPIREDIR=$instdir
PATH=$newpath
export EMPIREDIR PATH 
echo ' '
echo 'EMPIREDIR environment variable set to '$EMPIREDIR    
echo ' '
echo '  Press ENTER to continue the setup, CTRL-C to cancel'
read dir
echo '_____________________________________________________________________'
cd $instdir
echo ' '
echo 'Now we will compile the whole EMPIRE package.'
echo ' '
echo 'By default gfortran GNU FORTRAN compiler will be used.'
echo 'It is freely available for download and distributed in all LINUX packages.'
echo 'You may use a different compiler (e.g. ifort) once the setup is finished  '
echo '   by going to the '$instdir' (cd '$instdir') and type make ifort'
echo 'If gfortran is available then you only need to type make to recompile     '   
echo ' '
echo 'The gfortran compiler installed in your system is:'
gfortran -dumpversion
echo ' '
echo 'Please note that gfortran version > 4.2 is required !!'
echo ' '
echo '  Press ENTER to start compilation, CTRL-C to cancel the setup'
read dir
echo ' '
echo '_____________________________________________________________________'
echo ' '
echo 'Compiling EMPIRE and preprocessing codes (it takes 2-5 minutes) '
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
echo '______________________________________________________________________'
echo ' '
echo ' We extended your PATH variables to get access to ActiveTcl and       '
echo ' empire binaries for a shell sh enviroment (.bashrc). You have        '
echo ' to close your console and reopen it ********                         '
echo ' '
echo ' If you are using a different shell (not bash), then read below       '
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
echo ' '$instdir'/scripts/Xrun.tcl &'
echo ' '
echo ' to start EMPIRE GUI.'
echo ' '
echo ' Bons calculos !'
echo ' The EMPIRE team, Sao Jose dos Campos, Brazil, June 2011.'
if [ "$XWIN" = "Darwin" ]; then
  echo ' '
  echo '********* '
  echo 'Apple Mac users please remember to install ActiveTcl manually'
  echo 'Please go to the '$sourcedir' and double click on the icon ActiveTcl8.4.*'
  echo 'to install the ActiveTcl package in your Mac computer'
echo '______________________________________________________________________'
echo ' For further information contact R.Capote at r.capotenoy@iaea.org     '
exit

