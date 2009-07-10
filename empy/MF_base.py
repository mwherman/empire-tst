#!/usr/bin/env python
# encoding: utf-8
"""
MF_base.py

Created by Caleb Mattoon on 2008-10-11.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.


Base class for other MF classes: provides common low-lying functions.
Please be careful when considering changes!

Some functions for dealing with section headers may be fairly general, 
those might end up here...
"""

__metaclass__ = type

import sys
import os
import math


class UnknownMTFormat(NotImplementedError):
    """
    I'm making a new exception just to make the source of the error clear
    """
    def __init__(self):
        pass
    def __str__(self):
        return repr(self.value)



class MF_base:
    def __init__(self):
        # make a 'threshold' value: any floats closer to 
        # zero will just become zero
        self._nzro = 1.e-12
        
        # for reading/writing INTG format: max value = 10**self.ndigit
        self.ndigit = 3
    
    
    def treat(self,string):
        """
        change a string from 1.000-5 to float(1.000e-5) format
        """
        try:
            sign = ''
            if string[0] == '-':
                # remove the sign to make processing easier
                sign = '-'
                string = string[1:]
            if len( string.split('+') ) > 1:
                tmp = string.split('+')
                return float ( sign + tmp[0] + 'e+' + tmp[1] )
            else:
                tmp = string.split('-')
                return float ( sign + tmp[0] + 'e-' + tmp[1] )
        except:
            raise ValueError
            #print "Errors encountered converting to floats (treat function)!"
    

    def readENDFline(self,string):
        """
        change a whole line of six dense ENDF floats to python floats
        This won't work for INTG format
        """
        arr = (string[0:11], string[11:22], string[22:33], 
                string[33:44], string[44:55], string[55:66])
        retval = []
        
        for val in arr:
            try:
                retval.append( self.treat(val) )
            except ValueError:
                try:
                    retval.append( int(val) )
                except ValueError:
                    try:
                        retval.append( float(val) )
                    except ValueError:
                        if val.strip()=='':
                            retval.append(None)
                        else:
                            # real format problem
                            raise
        return retval
        #return [self.treat(a) for a in arr if a.strip() != '']
    

    def untreat(self,val):
        """
        put int, string, or float into endf format
        in case of float must use 1.000000-5 style
        """
        if type(val) is int:
            # integers are right-adjusted:
            return ("%11i" % val)
        
        if type(val) is str:
            # in case we want '0.0', '2.5', etc in the file
            # for Jpi for example. Pass these as strings, and they
            # will be printed as-is, right-adjusted
            try:
                dum = float(val)
            except ValueError:
                print "Numeric values only, please!"
                raise
            if len(val) > 11:
                raise FormatError, "String too long for endf format"
            return ("%11s" % val)
        
        if val is None:
            # blank spot in file
            return ("%11c" % ' ')
        
        # everything else is converted to Sci. notation
        if math.fabs(val) < self._nzro:
            # round small values down to zero
            val = 0.
        
        # exponents from E-09 to E+09:
        if (val==0) or (-9 <= math.log10( math.fabs(val) ) < 10):
            str_rep = ("% E" % val)
            if str_rep.find('+') > -1:
                return str_rep.replace('E+0','+')
            else:
                return str_rep.replace('E-0','-')
        # need extra space for exponents from E-99 to E+99:
        else:
            str_rep = ("% .5E" % val)
            if str_rep.find('+') > -1:
                return str_rep.replace('E+','+')
            else:
                return str_rep.replace('E-','-')
    

    def writeENDFline(self, tmplist, MAT,MF,MT,lineNo=None):
        """
        turn a list of 6 reals into an ENDF-formatted 80-column string
        with identifiers in right column

        line number is added if lineNo specified 
        (lineNo=0 is for FEND record only)
        """
        if len(tmplist) > 6:
            raise OverflowError, "only six values can fit onto an ENDF line"
        
        if len(tmplist) < 6:
            # 'ragged end' at the end of a section:
            data = ''
            for a in tmplist:
                data += self.untreat(a)
            blank = "%11s"%''
            nblank = 6 - len(tmplist)
            for i in range(nblank):
                data += blank
            if lineNo is not None:
                rightColumn = ("%4i%2i%3i%5i") % (MAT, MF, MT, lineNo)
            else:
                rightColumn = ("%4i%2i%3i%5c") % (MAT, MF, MT, ' ')
            return data + rightColumn + "\n"
        
        else:   # len(list) == 6
            data = ''
            for a in tmplist:
                data += self.untreat(a)
            if lineNo is not None:
                rightColumn = ("%4i%2i%3i%5i") % (MAT, MF, MT, lineNo)
            else:
                rightColumn = ("%4i%2i%3i%5c") % (MAT, MF, MT, ' ')
            return data + rightColumn + '\n'
    
    
    
    def readINTG(self, string):
        """
        read the 'INTG' format from ENDF: integers only, can be very compact.
        self.ndigit determines precision: max value=10**self.ndigit
        
        self.ndigit=2 line holds 18 ints
        self.ndigit=3 line holds 13 ints
        self.ndigit=4 line holds 11 ints
        self.ndigit=5 line holds 9 ints
        self.ndigit=6 line holds 8 ints
        otherwise ValueError is raised
        
        returns (x,y, array)
        where x,y give row,column of first element of array
        
        !! x and y use same index as in the file, so subtract 1 for c/python
        compatibility
        """
        def parseInt(str):
            """ helper function: read int OR blank space """
            if str.strip()=='':
                return None
            return int(str)
        
        if self.ndigit==2:
            sub = string[11:65]
            vals = [sub[3*i:3*i+3] for i in range(len(sub)//3)]
        elif self.ndigit==3:
            sub = string[11:63]
            vals = [sub[4*i:4*i+4] for i in range(len(sub)//4)]
        elif self.ndigit==4:
            sub = string[11:66]
            vals = [sub[5*i:5*i+5] for i in range(len(sub)//5)]
        elif self.ndigit==5:
            sub = string[11:65]
            vals = [sub[6*i:6*i+6] for i in range(len(sub)//6)]
        elif self.ndigit==6:
            sub = string[10:66]
            vals = [sub[7*i:7*i+7] for i in range(len(sub)//7)]
        else:
            raise ValueError, ("Illegal self.ndigit (%i) for INTG format" 
                    % self.ndigit)
            
        # x and y give row and column for the left-most array element:
        x = int(string[:5]) # - 1
        y = int(string[5:10]) # - 1
        arr = [parseInt(a) for a in vals]
        
        return x,y,arr


    def writeINTG(self, row, col, tmplist, MAT, MF, MT, lineNo=None):
        """
        turn row,col, list into INTG-formatted 80 column string
        number of values for the list depends on self.ndigit
        (see readINTG)
        
        line number is added only if specified
        
        just like with readINTG, it's the user's responsibility to change 
        row/col back from python 0-based to fortran 1-based index
        """
        # check if tmplist is too long?
        linelength = 56 # space available for integers
        nints = linelength // (self.ndigit + 1) # how many ints fit on line
        if self.ndigit == 3:
            nints = 13  # special case
        if len(tmplist) > nints:
            raise ValueError, ("List is too long (len=%i) for INTG format"
                    % len(tmplist) )
        
        while len(tmplist) < nints:
            # fill up with blank values
            tmplist.append(None)
        
        leftColumn = "%5i%5i" % (row,col)
        
        def toStr(val):
            """ helper function: put in format with checking """
            
            # create two template strings:
            dat = "%" + "%ii" % (self.ndigit + 1)     # "%4i" for ndigit=3
            blank = "%" + "%is" % (self.ndigit + 1)   # "%4s" for ndigit=3
            
            if val is None:
                return blank % ' '
            if abs(val) > 10**self.ndigit:
                raise ValueError, "value %i too large for format" % val
            return dat % val
            
        # for each value of self.ndigit the format is different
        # the left and right side must be padded with spaces in some cases:
        if self.ndigit==2:
            data = ' '  # pad left side with 1 space
            padr = ' '  # pad right side with 1 space
        elif self.ndigit==3:
            data = ' '
            padr = '   '
        elif self.ndigit==4:
            data = ' '
            padr = ''
        elif self.ndigit==5:
            data = ' '
            padr = ' '
        elif self.ndigit==6:
            data = ''
            padr = ''
        
        for a in tmplist:
            data += toStr(a)
        data += padr
        
        if lineNo is not None:
            rightColumn = ("%4i%2i%3i%5i") % (MAT, MF, MT, lineNo)
        else:
            rightColumn = ("%4i%2i%3i%5c") % (MAT, MF, MT, ' ')
        
        return leftColumn + data + rightColumn + '\n'
    
    
    def writeSEND(self, MAT, MF):
        """
        return a SEND line to finish of the MT section
        """
        str_val = " 0.000000+0 0.000000+0          0          0          0          0%4i%2i  099999\n" % (MAT,MF)
        return str_val
    
    
    def writeFEND(self, MAT):
        """
        return FEND line to finish MF section
        """
        str_val = ("%66c%4i%2i%3i%5i\n") % (' ',MAT,0,0,0)
        return str_val


if __name__ == '__main__':
    pass
