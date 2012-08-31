#PBS -m a
#PBS -S /bin/bash
#PBS -j oe
#PBS -o ${workdir##/*/}/${proj}_endf.log
echo 'Create local working directory'

# setup working directory on local scratch disk
locdir=/dev/shm/${PBS_JOBID%%.*}
if [ -e $locdir ]
then
	rm -r $locdir
fi
mkdir $locdir

echo ' Entering working dir: '$workdir

cd $workdir

echo 'Copying empire output & ENDF files to local dir'

if [ -e ${proj}.out ]
then
   cp ${proj}.out $locdir
else
   echo ' Empire output file not found: '${proj}.out
   exit
fi

if [ -e ${proj}.res ]
then
   cp ${proj}.res $locdir
elif [ -e ../${proj}.res ]
   then cp ../${proj}.res $locdir
else
   echo ' Resonance file (MF2/32) not found!'
   echo ' Please create file '${proj}.res
   exit
fi

if [ -e ${proj}_orig.endf ]
then
   cp ${proj}_orig.endf %locdir
elif [ -e ../${proj}_orig.endf ]
   then cp ../${proj}_orig.endf $locdir
else
   echo ' Donor ENDF file not found!'
   echo ' Please create file '${proj}_orig.endf
   exit
fi

# make ENDF file on local working directory
# and move it back to the original working dir

echo ' Making ENDF file'

cd $locdir
$EMPIREDIR/util/mkendf/mkendf.sh $proj
mv ${proj}.endf $workdir
cd $workdir
rm -r $locdir

exit
