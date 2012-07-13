      integer*4 function rctn(name)

      implicit none

      ! look for the supplied 9-character reaction name and
      ! return the corresponding MT value for this reaction.
      ! if reaction string is not found, return -1.

      character*12, intent(in) :: name         ! reaction name to find

      integer*4, parameter :: nrmax = 47       ! max # of reactions tested

      character*9, parameter :: LIST(nrmax) = (/ &
       'Total    ', 'Elastic  ', '(z,n)    ', '(z,2nd)  ', &
       '(z,2n)   ', '(z,3n)   ', 'Fission  ', '(z,f)    ', &
       '(z,nf)   ', '(z,2nf)  ', '(z,na)   ', '(z,n3a)  ', &
       '(z,2na)  ', '(z,3na)  ', '(z,np)   ', '(z,n2a)  ', &
       '(z,2n2a) ', '(z,nd)   ', '(z,nt)   ', '(z,nHe3) ', &
       '(z,nd2a) ', '(z,nt2a) ', '(z,4n)   ', '(z,3nf)  ', &
       '(z,2np)  ', '(z,3np)  ', '(z,n2p)  ', '(z,npa)  ', &
       '(z,gamma)', '(z,p)    ', '(z,d)    ', '(z,t)    ', &
       '(z,He3)  ', '(z,a)    ', '(z,2a)   ', '(z,3a)   ', &
       '(z,2p)   ', '(z,pa)   ', '(z,t2a)  ', '(z,d2a)  ', &
       '(z,pd)   ', '(z,pt)   ', '(z,da)   ', 'Nonelast ', &
       '(z,2npa) ', 'Mu-bar   ', 'Nu-bar   '/)

      integer*4, parameter :: MT(nrmax) = (/ &
       1, 2, 4, 11, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 28, 29, 30, &
       32, 33, 34, 35, 36, 37, 38, 41, 42, 44, 45, 102, 103, 104, 105, &
       106, 107, 108, 109, 111, 112, 113, 114, 115, 116, 117, 3, 999, 251, 456/)

      integer*4 ln1,ln2,ll1,ll2,i

      i = 1
      CALL STRLEN(NAME,LN1,LN2)

      do

         CALL STRLEN(LIST(I),LL1,LL2)

         if(name(ln1:ln2) == list(i)(ll1:ll2)) then
             rctn = mt(i)
             return
         endif

         i = i + 1

         if(i > nrmax) then
             WRITE(0,*) 'MT NOT FOUND FOR ',name(ln1:ln2)
             rctn = -1
             return
         endif

      end do

      end function rctn
