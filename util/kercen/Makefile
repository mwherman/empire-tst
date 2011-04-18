#----GNU C++ compiler
CC     =  g++
#----flags for production compilation with g++
CFLAGS = -O

OBJA =	mkgroup.o util.o atlas.o endfio.o endf.o
OBJB =	regroup.o
OBJC =	kercen.o util.o link.o atlas.o resxs.o endfio.o endf.o

all:	mkgroup regroup kercen

mkgroup:   $(OBJA)
	$(CC) $(CFLAGS) -o $@ $(OBJA)

regroup: $(OBJB)
	$(CC) $(CFLAGS) -o $@ $(OBJB)

kercen: $(OBJC)
	$(CC) $(CFLAGS) -o $@ $(OBJC)

clean:
	rm *.o

.cpp.o:
	$(CC) $(CFLAGS) -c $*.cpp
