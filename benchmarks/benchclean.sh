clean a-ni58-pcross 
clean d-au197 
clean fe54-blind 
clean fe54-vib-CC-direct-2 
clean HI-pcross-ccfus 
clean n_eu153-dir-msc-msd 
clean ni62-th-EGSM 
clean n-zr92-soft--dccomp 
clean test-LD-mo100 
clean test-PE-models 
mv th232-rot-vib-lev.col temporaryfile
clean th232-rot-vib 
mv temporaryfile th232-rot-vib-lev.col
clean u235-blind 
clean u235-pf 
clean u238-blind 
clean u238-HFB-num-fis 
clean u238-HFB-par-fis 
clean u238-rot-CC-direct-2 
rm *.zvd file* spec ecis* >& /dev/null
