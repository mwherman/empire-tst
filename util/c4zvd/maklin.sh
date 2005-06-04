    set -x
echo "Begin compilation of C programs"
    make -f curzvd3l.mak
    make -f pntdatl.mak
    make -f datzvdl.mak
    make -f c4dat.mak    
    make -f endzvd.mak        
