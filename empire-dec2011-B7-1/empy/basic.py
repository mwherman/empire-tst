#!/usr/bin/env python
"""
basic python example
"""
import os
import sys

while True:
    print "Please input a word (q to exit)"
    w = raw_input()
    if w.lower() == 'q':
        break
    
    print "You entered: %s" % w

print "Good-bye!"
