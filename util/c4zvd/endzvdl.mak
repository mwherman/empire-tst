NAME   = endzvd
O      = obj
O      = o
NAMEOBJ= $(NAME).$(O)
NAMEEXE= $(NAME)l.exe
LH     = ../

OBJ0 = $(NAME).$(O)
OBJ1 = zv1_str.$(O)

OBJS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3)

#CC     = cl
CC     = gcc
CFLAGS = -c

#LINK   = link
#LINK   = cc
LINK   = gcc
OPT    =
LIBS   =
LFLAGS = /out:$(NAMEEXE)
LFLAGS = -o $(NAMEEXE)

$(NAMEEXE) :  $(NAME)l.mak $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS)

#.c.obj:
#   $(CC) $(CFLAGS) { $< }
.c.$(O):
	$(CC) $(CFLAGS) $<
