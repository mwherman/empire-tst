runE a-ni58-pcross 
runE d-au197 
runE fe54-blind 
runE fe54-vib-CC-direct-2 
runE HI-pcross-ccfus 
runE n_eu153-dir-msc-msd 
runE ni62-th-EGSM 
runE n-zr92-soft--dccomp 
runE p-th232
runE test-HRTW-fis
runE test-LD-mo100 
runE test-PE-models 
runE th232-rot-vib 
runE u235-blind 
runE u235-pf 
runE u238-blind 
runE u238-HFB-num-fis 
runE u238-HFB-par-fis 
runE u238-rot-CC-direct-2 
mv th232-rot-vib-lev.col temporaryfile
rm -r *-tl *.out *.lev *.ripl *.rng *.war *.zvd file* *.c4 *fiss.xsc *.sys *.col *omp.dir *optman* spec *omp.ripl *ecis* *-inp.fis >& /dev/null
mv temporaryfile th232-rot-vib-lev.col
