#----GNU C++ compiler
CC     =  g++
#----flags for production compilation with g++
CFLAGS = -O3

OBJA =	mkgroup.o util.o atlas.o endfio.o endf.o
OBJB =	regroup.o
OBJC =	kercen.o util.o atlas.o kernel.o endfio.o endf.o

all:	mkgroup regroup kercen

mkgroup:   $(OBJA)
	$(CC) -o $@ $(OBJA)

regroup: $(OBJB)
	$(CC) -o $@ $(OBJB)

kercen: $(OBJC)
	$(CC) -o $@ $(OBJC)

clean:
	rm *.o
