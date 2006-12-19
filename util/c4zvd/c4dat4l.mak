NAME   = c4dat4
O      = obj
O      = o
NAMEOBJ= $(NAME).$(O)
NAMEEXE= $(NAME)l.exe
NAMEMAK= $(NAME)l.mak
LH     = ../

OBJ0 = $(NAME).$(O)
OBJ1 = zv1_str.$(O)

OBJS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3)

CC     = cl
CC     = cc
CFLAGS = -c

LINK   = link
LINK   = cc
OPT    =
LIBS   =
LFLAGS = /out:$(NAMEEXE)
LFLAGS = -o $(NAMEEXE) -static -lm

$(NAMEEXE) :  $(NAMEMAK) $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS)

#.c.obj:
#   $(CC) $(CFLAGS) { $< }
.c.$(O):
	$(CC) $(CFLAGS) $< 
