INSTALL information for empyre -- cmattoon, 10/13/2008

This is 'empyre', a set of python classes and functions for use with the 
nuclear modeling code EMPIRE.

A recent version of python (2.*) is needed, along with NumPy 
(both available online or from a package manager.

Python must be made aware of these functions. Currently this must be 
done manually:

in your .bashrc or equivalent, place these lines:

PYTHONPATH=$PYTHONPATH:<path_to_empire>/empyre
export PYTHONPATH


Alternate installation via distutils should be available soon

