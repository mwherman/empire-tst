"""
"empy" python package, containing python tools for EMPIRE:

depends:    python (v 2.x), numpy, pylab
==================

empy:
    endf:   functions for interacting with the 80-column endf file in
    an 'overview' mode: search for sections, delete, insert, etc
    
    bash:   rm(), mv(), cp(), etc. functions 
    using python os and shutil modules

    MF_base:    base class for single MF file.
    permits reading/writing single lines of ENDF files
    
    **classes inheriting MF_base** 
    
        MF31:   class for reading/writing nubar covariances.
        very simple right now
    
        MF32:   class for resonance parameters. Can read Atlas (Mughabghab) 
        or ENDF file in certain formats. Writing of ENDF files 2/32 not yet
        implemented
        
        MF33:   class for cross-section covariances. Read an MT section from 
        the file, can then manipulate and write new MT file. Limited to 1
        subsection (2 in special 'low-fidelity' case) right now
    
    
    
    **visualization tools**
    
    formatGP:   put matrix or list into gnuplot-friendly format
    
    
    **testing**
    
    empyTests:  some automated tests

"""
