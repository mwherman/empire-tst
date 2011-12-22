TEST directory
Contents:
    n-009_F_019.endf
    n-064_Gd_155.endf
    (not checked in to repository to save space. They come from ENDF-B/VII.0)

Right now, my MF33 class can handle the sections in the 155Gd file but CANNOT handle everything in the 19F file (too many subsections). Also, my code wimps out when it encounters an LTY0 format.

For the MF33.py class, I was able to read MF33 MT1 from Gd155, write it to a new file, and diff reports that the new file is identical to the original, so for now MF33 passes.
