      subroutine strlen(st, ls1, ls2)

      implicit none

      ! search the input string (st) and return indicies of the first (ls1)
      ! and last (ls2) non-blank characters. If a string contains all blanks,
      ! print a warning message on unit 0 and return ls1=ls2=0.

      character*(*), intent(in) :: st         ! string to search
      integer*4, intent(out)    :: ls1        ! first non-blank character in st
      integer*4, intent(out)    :: ls2        ! last  non-blank character in st

      integer*4 i

      i = len(st)

      do while(i > 0)
          if(st(i:i) .ne. ' ') exit
          i = i - 1
      end do
      ls2 = i

      if(i == 0) then
          write(0,*) 'EMPTY STRING'
          ls1 = 0
          return
      endif

      i = 1
      do while(i < ls2)
          if(st(i:i) .ne. ' ') exit
          i = i + 1
      end do
      ls1 = i

      return
      end subroutine strlen
