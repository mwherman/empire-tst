#PBS -m a
#PBS -S /bin/bash
#PBS -j oe
#PBS -o ${workdir##/*/}/${proj}_mcnp.log
echo 'Running MCNP'

# setup working directory on local scratch disk
locdir=/dev/shm/${PBS_JOBID%%.*}
if [ -e $locdir ]
then
	rm -r $locdir
fi
mkdir $locdir
cd $locdir

# copy over script & cross sections
$EMPIREDIR/util/mkendf/mk_xsdir ${workdir%/*}/xsdir ${locdir}/xsec
cp -r ${workdir%/*}/xsec ./
$EMPIREDIR/util/mkendf/mk_ace ${workdir} ${locdir}/xsec
cp ${workdir%/*}/mcnp.i ./

/home/arcilla/bin/mcnp5 inp=mcnp.i xsdir=xsdir eol > out.dat

# move output to original directory & clean up
mv out.dat ${workdir}/${proj}_mcnp.out
mv outp ${workdir}/${proj}_mcnp.outp
rm -r $locdir

exit
