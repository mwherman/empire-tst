"""
"empy" python package, containing python tools for EMPIRE:

requires:   python (v 2.x)
optional, recommended:  numpy (v 1.1 or later)
==================

This document provides an overview of empy. More information is available
in each sub-module

empy:
    endf.py:   functions for searching, deleting, inserting from ENDF files
    
    bash.py:   has python rm(), mv(), cp(), ln() equivalents
    using python os and shutil modules
    
    MF_base.py:
        base class for reading/writing MF files in ENDF-6 format.
    
    **classes inheriting MF_base. Each reads data from ENDF into a
    class structure:** 
    
        MF31.py:   class for reading/writing nubar covariances.
        
        MF32.py:   class for resonance parameters. Can read Atlas 
        (Mughabghab) or ENDF file in certain formats. Writing of files 2/32
        in compact and full format (LCOMP=1 or 2) is also supported
        
        MF33.py:   class for cross-section covariances. Read an MT section
        from the file, can then manipulate and write new MT file. 
        If multiple subsections are present, all will be read in to the 
        'rawData' member array. Goal: also reconstruct full covariance 
        matrix from all subsections
        
        MF35.py:   class for energy spectra covariances (PFNS).
    
    mgBase.py:
        base class for multi-group covariances produced by processing with
        PUFF/NJOY. Contains x-sections, correlations, thresholds etc for 
        each reaction in multigroup format. Also functions for writing class
        back to ascii or ENDF format
    
    **classes inheriting mgBase**
    
        readPUFF.mgCovars:  read 'puff.output' into class inheriting mgBase
         
        readNJOY.mgCovars:  read 'corr.matrix' into class inheriting mgBase
        
        boxr.mgCovars: read ascii or binary 'BOXR' format 
            (produced by njoy) into mgBase
       
    
    **other stuff**
    
    la.py: linear algebra useful tricks. Includes method for fixing non-
        positive-definite matrix.
    
    formatGP.py:   put matrix or list into gnuplot-friendly format.
                recent gnuplot features make this redundant
    
    
    **testing**
    
    empyTests.py:  some automated tests

"""
