NAME   = datzvd
O      = o
NAMEOBJ= $(NAME).$(O)
NAMEEXE= $(NAME).exe
LH     = ../

OBJ0 = $(NAME).$(O)
OBJ1 = 
OBJ2 = zv1_str.$(O) d_alloc.$(O)

OBJS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3)

CC     = cc
CFLAGS = -c

LINK   = cc
OPT    =
LIBS   =
LFLAGS = -o$(NAMEEXE)

$(NAMEEXE) :  $(NAME).mak $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS)

#.c.obj:
#   $(CC) $(CFLAGS) { $< }
.c.$(O):
	$(CC) $(CFLAGS) $< 
