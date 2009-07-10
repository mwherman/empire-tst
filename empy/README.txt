INSTALL information for empy -- cmattoon, 10/13/2008

This is 'empy', a set of python classes and functions for use with the 
nuclear modeling code EMPIRE.

A recent version of python (2.*) is needed, along with NumPy 
(both available online or from a package manager.

Python must be made aware of these functions. Currently this must be 
done manually:

in your .bashrc or equivalent, place these lines:

PYTHONPATH=$PYTHONPATH:<path_to_empire>
export PYTHONPATH

Note this should point to the directory *containing* empy, not to empy itself.
Thus,

PYTHONPATH=$PYTHONPATH:$EMPIREDIR
export PYTHONPATH

will work if EMPIREDIR is defined properly.

Alternate installation via distutils may become available soon

