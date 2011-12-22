#
# python module of useful code for the nndc.
# so far just contains 'endf', 
# with functions for searching, removing and inserting in endf files
#

# don't import numpy-dependent stuff here, in case someone without numpy
# wants to use it:

from info import __doc__

# for 'from foo import *:
__all__ = ['endf','MF_base','bash']

# or just import foo:
import endf, MF_base, bash

