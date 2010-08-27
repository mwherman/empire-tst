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
cd $dir

# RUN X-SEC CALCULATIONS & EXFOR PROCESSING
~/empire/scripts/runE $file

# check if everything completed properly:
if [ ! `tail -n 2 $file.out | grep -c "SUCCESSFULLY"` = 1 ]; then
    echo crash: $dir >> $origDir/crashlog.txt
fi

if [ $clean = Y ]; then
    # keep only .xsc file:
    #find . ! -name $file.xsc -exec rm -rf {} \;
    ls | grep -v $file.xsc | xargs rm -rf
fi

exit
