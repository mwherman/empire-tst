//#include <malloc.h>

static int nalloc=0;

char *my_calloc(kmem,lmem)
unsigned  int  kmem,lmem;
{
    char *mem;
    void *calloc();
    mem = calloc(kmem,lmem);
nalloc++;
//printf("-----ALLOC %d-----%p---L=%ld-----\n",nalloc,mem,(long)lmem*(long)kmem);
    return(mem);
}

int my_free(mem)
char    *mem;
{
//printf("-----FREE %d-----%p-----\n",nalloc,mem);
nalloc--;
    free(mem);
    return(0);
}
