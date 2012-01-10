#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

/*
  author: Sam Hoblit, NNDC, BNL
  These jacket routines make block-mode I/O easier from
  fortran. On linux fortran compilers tend to decorate
  external reference names with a underscore, and the
  flags for file open and the fstat structure need to
  be put in by hand. Here just make a few C routines to
  open file and do basic I/O & status. These routines are
  used by endf_blklin.f90 only.
*/

static struct stat file_stat;

int open_endf_blkfile_(char *file, int *flg, int len)
{
   int fhn;
   char lfil[len+1];
   char *s, *t;

   /* make local copy of filename so we can safely
   terminate the fortran string with null character. */

   s = file + len - 1;
   t = lfil + len - 1;
   while(*t-- = *s--);
   lfil[len] = '\0';

   /*now open the file. if flg=0, open new file for output.
    Otherwise open existing file for read. When creating
    an output file also set privs for new file. */

   if(*flg) {
      fhn = open(lfil, O_WRONLY|O_CREAT|O_EXCL);
      if(fhn>0) fchmod(fhn, 436);
      }
   else
      fhn = open(lfil, O_RDONLY);
   return fhn; 
}

int get_endf_buffer_(int *fhandl, int *numrec, void *buf)
{
   return read(*fhandl, buf, 81*(*numrec));
}

int put_endf_buffer_(int *fhandl, int *numrec, void *buf)
{
   return write(*fhandl, buf, 81*(*numrec));
}

int close_endf_blkfile_(int *fhandl)
{
   return close(*fhandl);
}

int endf_file_status_(int *fhandl, struct stat *sblk)
{
   return fstat(*fhandl, sblk);
}

int endf_file_size_(int *fhandl)
{
   if(fstat(*fhandl, &file_stat))
      return 0;
   else
      return file_stat.st_size;
}
