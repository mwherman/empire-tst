#!/usr/bin/env python
# encoding: utf-8
"""
bash.py

Created by Caleb Mattoon on 2009-02-14.
Copyright (c) 2009 __nndc.bnl.gov__. All rights reserved.

emulate some useful shell commands:

>from empy.bash import *
>rm(foo, bar) equivalent to rm foo bar
    can also use rm on lists:
>rm(foo, [bar1,bar2,bar3])

>mv(foo, bar) equals mv foo bar

also cp and ln -s
"""

import sys
import os
import shutil

def rm(*args, **kwargs):
    """
    remove file or list of files, silent on error unless verbose=True
    example:
    rm( 'foo.txt', 'bar.txt', verbose=True ) prints any errors
    rm( 'foo.txt', 'bar.txt' ) is silent on error
    """
    for f in args:
        # so we can use lists returned by glob:
        if type(f) is list:
            for el in f:
                try:
                    os.remove(el)
                except OSError, e:
                    if 'verbose' in kwargs.keys() and kwargs['verbose']:
                        print (e)
        else:
            try:
                os.remove(f)
            except OSError, e:
                if 'verbose' in kwargs.keys() and kwargs['verbose']:
                    print (e)


def mv(source, target, verbose=True):
    """
    emulate bash mv command (one file at a time)
    silent on error unless verbose=True
    """
    try:
        os.rename(source, target)
    except OSError, e:
        if verbose:
            print (e)
    """
    try:
        shutil.move(source, target) # not present before v2.3
    except IOError, e:
        if verbose:
            print (e)
    """


def cp(source, target, verbose=True):
    """
    emulate bash cp command
    """
    try:
        shutil.copy(source, target)
    except IOError, e:
        if verbose:
            print (e)


def ln(source, target, verbose=True):
    """
    emulate bash ln command
    """
    try:
        os.symlink(source, target)
    except OSError, e:
        if verbose:
            print (e)


def isEmpty(filename):
    """
    emulate the [ ! -s filename ] test in bash (doesn't exist or is empty)
    also 'not isEmpty()' should be equivalent to [ -s filename ]
    """
    return (not os.path.exists(filename)) or (os.path.getsize(filename)==0)


def main():
    pass


if __name__ == '__main__':
    main()

