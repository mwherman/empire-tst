NAME   = pntdat
O      = obj
NAMEOBJ= $(NAME).$(O)
NAMEEXE= $(NAME).exe
LH     = ../

OBJ0 = $(NAME).$(O)
OBJ1 = zv1_str.$(O)

OBJS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3)

CC     = cl
CFLAGS = -c

LINK   = link
OPT    =
LIBS   =
LFLAGS = /out:$(NAMEEXE)

$(NAMEEXE) :  $(NAME).mak $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS)

#.c.obj:
#   $(CC) $(CFLAGS) { $< }
.c.$(O):
	$(CC) $(CFLAGS) $< 
