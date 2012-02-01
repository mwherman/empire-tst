# --------------------------------------------------------------
# Makefile for c4zvd.  Caleb Mattoon, 08/22/2008.
# 
# simple, just call each .mak file
# --------------------------------------------------------------


makList = *.mak


all: zvview.exe
	@for makfile in $(makList); do (echo $$makfile; $(MAKE) -f $$makfile); done

clean:
	rm *.o

zvview.exe:
	python pickZVView.py