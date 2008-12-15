#! /bin/sh -
#script to install and compile EMPIRE-3 package
#directory in which EMPIRE is to be installed
instdir=$HOME
#directory with EMPIRE distribution
sourcedir=`pwd`
#EMPIRE version number
empver=3_01
echo '                                                           '
echo '    EEEEE  M    M  PPPP   I  RRRR   EEEEE      33333       '
echo '    E      MM  MM  P   P  I  R   R  E             33       '
echo '    EEE    M MM M  PPPP   I  RRRR   EEE    =   33333       '
echo '    E      M    M  P      I  R  R   E              3       '
echo '    EEEEE  M    M  P      I  R   R  EEEEE      33333       '
echo '                                                           '
echo '            A R C O L E (beta 2) Dec 2008                  '
echo '                                                           '
echo ' Please refer to:                                          '
echo ' 1) M. Herman, R. Capote , B.V. Carlson , P. Oblozinsky,   '
echo '    M. Sin , A. Trkov , H. Wienke  and V. Zerkin.          '
echo '    Nuclear Data Sheets 108 (2007) 2655                    '
echo ' Input data taken from the IAEA database :                 '
echo ' 2) T. Belgya, O. Bersillon, R. Capote, T. Fukahori,       '
echo '    G. Zhigang, S. Goriely, M. Herman, A.V. Ignatyuk,      '
echo '    S. Kailas, A. Koning, P. Oblozinsky, V. Plujko and     '
echo '    P. Young.                                              '
echo '    Handbook for calculations of nuclear reaction          '
echo '    data: Reference Input Parameter Library II.            '
echo '    IAEA-TECDOC-1506, Vienna 2006. Available               '
echo '    online at http://www-nds.iaea.org/RIPL-2/              '
echo '                                                           '
echo ' Other collaborators: A. Ventura, E. Betak, V. Plujko      '
echo '                                                           '
echo ' Additional information and previous versions available at '
echo ' EMPIRE homepages:                                         '
echo ' . International Atomic Energy Agency, Vienna, Austria     '
echo '   @ http://www-nds.iaea.org/empire/                       '
echo ' . Brookhaven National Laboratory, National Nuclear Data   '
echo '     Center, New York, USA                                 '
echo '   @ http://www.nndc.bnl.gov/nndcscr/model-codes/empire-ii/'
echo '***********************************************************'
echo '  Press any key to start the setup of the EMPIRE-3 beta 1  '
read
echo ' '
echo 'This is the EMPIRE-3 installations script. It will lead you'
echo 'through the set-up procedure and eventually compile whole  '
echo 'the package. Hit return to accept defaults within [].      '
echo ' '
echo ' '
echo 'Specify directory with EMPIRE-3 distribution ['$sourcedir']:'
read dir
if [ ! "$dir" = "" ]; then
   sourcedir=$dir
fi
if [ -f $sourcedir/empire-$empver.tgz ]; then
   echo ' '
   echo 'OK empire-'$empver'.tgz found'
   echo ' '
else
   echo ' '
   echo 'empire-'$empver'.tgz not found in '$sourcedir', restart setup script '
   echo 'and specify the correct path to the empire-'$empver'.tgz file'
   exit
fi
echo 'Specify existing directory in which EMPIRE-3 should be installed.'
echo 'The directory "empire" will be created inside so default should be'
echo 'a good choice (hit return to accept default) ['$instdir']:        '
read dir
if [ ! "$dir" = "" ]; then
   instdir=$dir
fi
exfor=n
echo 'The relational (MySQL) version of the EXFOR library.         '
echo 'is available on a separate CD.                               '
echo 'Lack of EXFOR disables comparison with experimental data     '
echo 'but does not compromise EMPIRE calculations.                 '
exforel=n

cd $instdir
echo 'exploding EMPIRE-3 source'
#  gunzip $sourcedir/empire-$empver.tgz
#  tar xvf $sourcedir/empire-$empver.tar
tar xzvf $sourcedir/empire-$empver.tgz
echo 'Have decompressed EMPIRE-3 in the directory '$instdir

echo ' '
echo ' '
echo 'Now I will compile the whole package.'
echo 'By default Intel FORTRAN compiler will be used.'
#echo 'By default gfortran GNU FORTRAN compiler will be used.'
#echo 'It is freely available for download and distributed in all LINUX packages.'
#echo 'Please note that a version > 4.2 is highly desirable.'
echo '.'
#echo 'Note that three processing codes still need g77: FIXUP,PLTLST and SIGMA1'
#echo 'They are needed for ENDF formatting and validation.'
#echo ' '
echo 'g77 fortran compiler rpm packages are available in the EMPIRE-3 CD '
#echo 'in the directory g77.'
#echo '.'
echo 'Compiling EMPIRE and preprocessing codes'
echo ' '
echo 'The warning in lev-dens.f:2554 '
echo '   100 READ (34,99010,ERR = 100,END = 300) car2, izr, iar, paritate '
echo 'is expected.'
echo ' '
cd empire
# chmod +x Compile
# ./Compile
make
cd ../
#Installing Tcl/Tk packages
echo ' '
echo 'We will need Tcl/Tk and itcl packages to run EMPIRE GUI. '
echo 'Although your system might have these packages installed  '
echo 'it often happens that they are not compatible with EMPIRE. '
echo 'To be on a safe side We would install industry standard  '
echo 'ActiveTcl package that runs on most Linuxes.'
echo ' '
echo 'We strongly recommend to install this package. It will be '
echo 'installed locally in the empire directory and will not mess up '
echo 'with your native Tcl/Tk installation. To make it active you will'
echo 'have to add a line to your shell initialization file as indicated '
echo 'by the ActiveTcl installation script. To return to your native Tcl '
echo 'it is enough to remove this line, then logoff and login.'
echo 'If you decide to install ActiveTcl say yes below and follow the'
echo 'instructions (you may choose '$instdir/ActiveTcl' as an'
echo 'installation directory) '
echo ' '
echo 'DO NOT FORGET TO ADD THE LINE AS ADVISED, THEN CLOSE THE CONSOLE '
echo ' AND REOPEN IT! '
echo ' '
echo 'Do you want to install ActiveTcl package y/n [y]:'
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
     echo 'Press any key to continue'
     read
fi

echo ' '
echo '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! '
echo ' '
echo ' Please do not forget to extend your PATH variables to       '
echo ' get access to ActiveTcl and empire binaries (zvview,empire3)'
echo ' '
echo ' For a shell sh enviroment,you can include these lines in the '
echo ' file .bashrc in your home directory '
echo ' '
echo ' For a sh or similar perform                               '
echo '     PATH="'$instdir/ActiveTcl/bin:./:$instdir/empire/scripts':$PATH"        '
echo '     export PATH                                           '
echo ' '
echo ' For a csh or compatible perform'
echo '     setenv PATH "'$instdir/ActiveTcl/bin:./:$instdir/empire/scripts':$PATH" '
echo '     export PATH '
echo ' '
echo ' Some shells (bash for example) allow                      '
echo '     export PATH="'$instdir/ActiveTcl/bin:./:$instdir/empire/scripts':$PATH" '
echo ' '
echo ' Once you modify your path variables close your console and reopen it'
echo '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! '
echo '      Press any key to end the EMPIRE-3 beta 2 setup'
read

echo 'If you are lucky EMPIRE-3 beta 2 is successfully installed! '
echo ' '
echo 'Please modify your PATH varaibles as described above. After that, '
echo 'if all went fine choose anyone of your working directories '
echo 'or create a new one inside the empire dicretory. '
echo 'Assuming your working directory is "work"'
echo 'change to the '$instdir/empire/work' directory and type:'
echo ' '
echo ' ../scripts/Xrun.tcl &'
echo '     or '
echo ' empire3 '
echo ' '
echo ' to start EMPIRE GUI.'
echo ' '
echo ' '
echo ' Good calculations !'
echo '                   '
echo ' The EMPIRE team.  '
exit

