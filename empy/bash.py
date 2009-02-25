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

also cp and ln
"""

import sys
import os

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


def mv(source, target, verbose=False):
    """
    emulate bash mv command (one file at a time)
    silent on error unless verbose=True
    """
    try:
        shutil.move(source, target)
    except IOError, e:
        if verbose:
            print (e)


def cp(source, target, verbose=False):
    """
    emulate bash cp command
    """
    try:
        shutil.copy(source, target)
    except IOError, e:
        if verbose:
            print (e)


def ln(source, target, verbose=False):
    """
    emulate bash ln command
    """
    try:
        os.symlink(source, target)
    except OSError, e:
        if verbose:
            print (e)


def main():
    pass


if __name__ == '__main__':
    main()

