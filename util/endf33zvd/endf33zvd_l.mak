NAME   = endf33zvd
O      = o
NAMEOBJ= $(NAME).$(O)
NAMEEXE= $(NAME)_l.exe
NAMEMAK= $(NAME)_l.mak
LH     = ../

OBJ0 = $(NAME).$(O) my_mf33.$(O)
OBJ1 = 
OBJ2 = zv1_str.$(O) d_alloc.$(O)
# dir_nt.$(O)

OBJS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3)

CC     = gcc
CFLAGS = -c

LINK   = gcc
OPT    =
LIBS   =
LFLAGS = -o $(NAMEEXE)

L3=-lm
LIBS = $(L1) $(L2) $(L3)

LFLAGS = $(LIBS) -o $(NAMEEXE)

$(NAMEEXE) :  $(NAMEMAK) $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS)

#.c.obj:
#   $(CC) $(CFLAGS) { $< }
.c.$(O):
	$(CC) $(CFLAGS) $< 

clean:
	rm -f *.o
