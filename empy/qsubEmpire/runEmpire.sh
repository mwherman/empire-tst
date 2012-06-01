#PBS -m a
# #PBS -N empire    #use qsub -N to set name
#PBS -S /bin/bash
#PBS -j oe
# PBS -o empire${file}_${energy}.log
#
# could use -m e above to mail me when script finishes
# right now only mails on abort
#
#SCRIPT TO RUN FULL EMPIRE PACKAGE

origDir=`pwd`

# do a couple of sanity checks to make sure this 
# was run from qsubEmpire. If not, abort.

if [ -z $dir ]; then
  echo " Working directory not specified!"
  echo " Was this run from qsubEmpire?"
  exit
fi

if [ -z $PBS_JOBID ]; then
  echo " PBS JOB_ID not defined!"
  echo " Was this run from qsubEmpire?"
  exit
fi

# setup working directory on local scratch disk
work=/dev/shm/${PBS_JOBID%%.*}
if [ -e $work ]; then
        rm -r $work
fi
mkdir $work
cd $work
cp -r $dir/* ./

# RUN X-SEC CALCULATIONS & EXFOR PROCESSING

~/empire/scripts/runE $file

# check if everything completed properly:
if [ ! `tail -n 2 $file.out | grep -c "SUCCESSFULLY"` = 1 ]; then
    echo crash: $dir >> $origDir/crashlog.txt
fi

# all done - copy local files back to orig dir
if [ -z $clean ]; then
   clean="N"
fi
if [ $clean = Y ]; then
    # only copy .xsc & pfns.out files
   if [ -e $file.xsc ]
   then
      cp $file.xsc $dir
   fi
   if [ -e $file-pfns.out ]
   then
      cp $file-pfns.out $dir
   fi
else
   # copy everything
   cp -r ./* $dir
fi

# delete working dir
rm -r $work

exit
