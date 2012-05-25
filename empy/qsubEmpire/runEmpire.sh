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

# for local testing
# dir=/home/herman/temp
# file=u235
# clean=Y

origDir=`pwd`

# create directory on local /dev/shm using local
# directory and one level up.

strt=${dir%/*/*}
end=${dir#$strt}
work=/dev/shm$end

# make local working directory if not already there

if [ ! -e $work ]; then
   mkdir -p $work
fi

# copy files over to local directory

rm -r $work/* 2>/dev/null
cd $work
cp -r $dir/* ./

# RUN X-SEC CALCULATIONS & EXFOR PROCESSING

~/empire/scripts/runE $file

# check if everything completed properly:

if [ ! `tail -n 2 $file.out | grep -c "SUCCESSFULLY"` = 1 ]; then
    echo crash: $dir >> $origDir/crashlog.txt
fi

if [ $clean = Y ]; then
    # keep only .xsc file:
    #find . ! -name $file.xsc -exec rm -rf {} \;
    #ls | grep -v '$file.xsc' | xargs rm -rf
    grep_pattern="$file.xsc\|$file-pfns.out"
    ls | grep -v $grep_pattern | xargs rm -rf
fi

# all done. Copy local directory back to original

cp -r ./* $dir

# clean up - remove working directory and parent (if empty)

rm -r $work 2>/dev/null
rmdir ${work%/*} 2>/dev/null

exit
