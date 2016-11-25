!<CPS_v1 type="PRIMITIVE"/>

!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>

!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E        
!
! Name       : sort 
! Category   : sorts
! Written    : 1989-01-11   by: Dave Hale   Colorado School of Mines
! Revised    : 2002-07-02   by: Charles C Burch
! Maturity   : production   2002-07-29
! Purpose    : Functions to sort arrays of data or arrays of indices
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! SORT - Functions to sort arrays of data or arrays of indices
!
! Data sorts rearrange data points a() such that
!        a(1) <= a(2) <= ... <= a(n)
! Index sorts rearrange indices i() such that
!        a(i(1)) <= a(i(2)) <= ... <= a(i(n))
!
! sort_hpsort   Sort an array of floats by the heap sort method.
! sort_hpisort  Sort an array of indices by the heap sort method.
! sort_qksort   Sort an array of floats by the quick sort method.
! sort_qkisort  Sort an array of indices by the quick sort method.
! sort_shsort   Sort an subarray of floats by the Shell sort method.
! sort_shisort  Sort an subarray of indices by the Shell sort method.
! sort_inssort  Sort an subarray of floats by the insertion sort method.
! sort_insisort Sort an subarray of indices by the insertion sort method.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!****************************************************************************
! Function Prototypes:
!                      i        b
!    sort_hpsort  (int n, float a())
!
!                      i        i        b
!    sort_hpisort (int n, float a(), int i())
!
!                      i        b
!    sort_qksort  (int n, float a())
!
!                      i        i        b
!    sort_qkisort (int n, float a(), int i())
!
!                      i      i        b
!    sort_shsort  (float a(), int p, int q)
!
!                        i        b        i      i
!    sort_shisort (float a(), int i(), int p, int q)
!
!                        b        i      i
!    sort_inssort (float a(), int p, int q)
!
!                        i        b        i      i
!    sort_insisort(float a(), int i(), int p, int q)
!
!****************************************************************************
! sort_hpsort:
! Input:
! n            number of elements in a
! a            array(n) to be sorted
!
! Output:
! a            array(n) sorted
!
!****************************************************************************
! sort_hpisort:
! Input:
! n            number of elements in a
! a            array(n) elements
! i            array(n) indices to be sorted
!
! Output:
! i            array(n) indices sorted
!
!****************************************************************************
! sort_qksort:
! Input:
! n            number of elements in array a
! a            array(n) containing elements to be sorted
!
! Output:
! a            array(n) containing sorted elements
!
!****************************************************************************
! sort_qkisort:
! Input:
! n            number of elements in array a
! a            array(n) elements
! i            array(n) indices to be sorted
!
! Output:
! i            array(n) indices sorted
!
!****************************************************************************
!****************************************************************************
! sort_shsort:
! Input:
! a            subarray(p:q) containing elements to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! a            subarray(p:q) sorted
!****************************************************************************
! sort_shisort:
! Input:
! a            subarray(p:q) containing elements
! i            subarray(p:q) containing indices to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! i            subarray(p:q) of indices sorted
!****************************************************************************
! sort_inssort:
! Input:
! a            subarray(p:q) containing elements to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! a            subarray(p:q) sorted
!****************************************************************************
! sort_insisort:
! Input:
! a            subarray(p:q) containing elements
! i            subarray(p:q) containing indices to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! i            subarray(p:q) of indices sorted
!****************************************************************************
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  9. 2002-07-29  C C Burch    Removed qkfind/qkifind-had errors and not used
!                              Replaced hpsort routines as they did not work
!                              Replaced qksort/qkisort with simplier version
!                              Simplified shsort/shisort and inssort/inisort
!  8. 2000-03-15  O'Brien      Documentation updates
!  7. 1999-12-29  O'Brien      Brought xml tags up to date
!  6. 1999-12-15  O'Brien      Adjusted the index decrement in sort_shisort
!                              Added RCS identifier variable.
!  5. 1999-11-10  O'Brien      Altered interface to sort_hpisort
!  4. 1999-10-28  O'Brien      Revised private names to prefix standard
!  3. 1999-10-18  O'Brien      Added Shell sorts, made insertion sorts
!                                public, made names more consistent, and
!                                added heap sort for indices.
!  2. 1999-09-29  O'Brien      Converted from C to Fortran 90 in CPS format
!  1. 1989-01-13  Dave Hale    Initial C version written for Seismic Unix.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
! sort_hpsort and sort_hpisort:
! The heap sort algorithm is, at worst, N log_2 N, and in most cases
! is 20% faster. The version now used was orginally written in 1973 by CC Burch
!
! sort_qksort, sort_qkisort:
! n must be less than 2^NSTACK, where NSTACK is defined above.
! The version now sused is a variation of Kernigan/Ritchie versions using
! stacks rather than recursion.
!
! sort_shsort and sort_shisort:
! The Shell sort algorithm is an N^(3/2) algorithm for the
! worst possible ordering of data, and approximately N^1.27 for
! randomly ordered data. Shell sorting is a variant of the insertion sort.
!
! sort_inssort and sort_insisort:
! The insertion sort algorithm is an N^2 algorithm. It's best
! used for small N, say N<50. Insertion sorts may be required
! when multiple arrays are to be sorted on more than one key,
! where ordering on the first key must be preserved through
! remaining sorts. 
!
!****************************************************************************
! Reference:
! heap sort functions sort_hpsort, sort_hpisort:
! Standish, T. A., Data Structure Techniques, p. 91.
! See also Press, W. A., et al., Numerical Recipes in C.
!
! quick sort functions sort_qkisort, sort_qksort:
! Hoare, C.A.R., 1961, Communications of the ACM, v. 4, p. 321.
! K&R C book
!
! Shell and insertion sort functions
! sort_shsort, sort_shisort, sort_inssort, and sort_insisort:
! See Press, W. A., et al., Numerical Recipes.
!     Sedgewick, R., 1983, Algorithms, Addison Wesley, pg96.
!
! This code was originally converted from C Seismic Unix routines by Mike Obien
!
! Most of the code by Mike Obrien was essentialy rewritten June 2002 by 
! Charles C Burch. There is little dependency on the original code.
!****************************************************************************
! Below are some actual timings(in seconds) done on a PC-
!    similar relative results are expected on other machines
!          n=10000
!  inssort     1.53 insisort     1.88 
!  shsort       .02 shisort       .03
!  qksort       .01 qkisort       .02
!  hpsort       .02 hpisort       .03
!
!          n=100000
!  inssort   167.28 insisort   331.93 
!  shsort       .44 shisort       .72
!  qksort       .18 qkisort       .25
!  hpsort       .22 hpisort       .39
!
!          n=1000000
!  inssort too long insisort too long        
!  shsort      6.41 shisort     13.13
!  qksort      2.32 qkisort      3.79
!  hpsort      3.32 hpisort      6.41
!
!          n=2000000
!  inssort too long insisort too long        
!  shsort     14.98 shisort     31.67
!  qksort      4.86 qkisort      8.45
!  hpsort      7.42 hpisort     14.40
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module sort_module
      implicit none

      private
      public :: sort_hpsort, sort_hpisort
      public :: sort_qksort, sort_qkisort
      public :: sort_shsort, sort_shisort
      public :: sort_inssort, sort_insisort

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      integer,parameter :: NSTACK=50   ! maximum sort length is 2^NSTACK

      character(len=100),public,save :: SORT_IDENT = &
             '$Id: sort.f90,v 1.9 2002/07/26 20:03:01 CCBurch prod sps $'

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!


!****************************************************************************
! hpsort (int n, float(a))
! sort an array so that a(1) <= a(2) <= ... <= a(n)
!****************************************************************************
! Input:
! n            number of elements in a
! a            array(n) to be sorted
!
! Output:
! a            array(n) sorted
!****************************************************************************
      subroutine sort_hpsort (n,a)
      implicit none
      integer  :: n                             ! arguments
      real     :: a(:)                          ! arguments

      integer  :: i, j_father, j_son            ! local variables
      real     :: ai                            ! local variables
!------------------------------------------------

      if(n.le.1) return

      ! Make a(1:n) into a heap which is a binary tree where father  a node
      ! has a value >= its two sons
      ! we use the convention if a node has index i, 
      !   its left son has index 2i and right son has index 2i+1
      ! hence father of node i has index floor(i/2)
      ! note in a heap, the largest value has index 1
      
      do i=2,n
        !make a(1:i) a heap assuming a(1:i-1) is a heap
      
        ai=a(i)               !value to be inserted
        j_son=i               !where to eventually insert
        ! swap father and sons until find where new value goes
        do while(j_son.ge.2)
          j_father=j_son/2
          if(ai<=a(j_father)) exit

          a(j_son)=a(j_father)
          j_son=j_father
        enddo

        a(j_son)=ai
      enddo

      ! heap formed, now sort it
      ! vary i from n to 2 by -1
      ! extract a(1) as the max element and replace a(i) with it
      ! then reconfigure the heap inserting the old a(i) to preserve a heap

      do i=n,2,-1
        ai=a(i)      !value to be inserted into heap
        a(i)=a(1)    !put largest value in correct sorted position
        j_father=1   !where new value eventually gets inserted
      
        ! now make a(1:i-1) a heap again when extracting a(1) and inserting a(i)
        j_son=2          
        do while(j_son<i)
          if(j_son.lt.(i-1)) then   ! make j_son the larger son
            if(a(j_son).lt.a(j_son+1)) j_son=j_son+1
          endif
        
          if(ai.ge.a(j_son)) exit

          a(j_father)=a(j_son)
          j_father=j_son
          j_son=j_father+j_father
        enddo

        a(j_father)=ai
      enddo

      return
      end subroutine sort_hpsort

!****************************************************************************
! hpisort (int n, float(a), int i())
! sort an array o indices so that a(i(1)) <= a(i(2)) <= ... <= a(i(n))
!****************************************************************************
! Input:
! n            number of elements in a
! a            array(n) elements
! i            array(n) indices to be sorted
!
! Output:
! i            array(n) indices sorted
!****************************************************************************
      subroutine sort_hpisort (n,a,i)
      implicit none
      integer :: n, i(:)                       ! arguments
      real    :: a(:)                          ! arguments

      integer  :: i1, j_son, j_father, ix      ! local variables
      real     :: ai                           ! local variables
!------------------------------------------------

      if(n.le.1) return

      ! First make a(i(1:n)) into a heap
      ! for more detailed comments, see sort_hpsort
      do i1=2,n
        !make a(i(1:i1)) a heap assuming a(i(1:i1-1)) is a heap
        ix=i(i1)
        ai=a(ix)
        j_son=i1

        do while(j_son.ge.2)
          j_father=j_son/2
          if(ai<=a(i(j_father))) exit

          i(j_son)=i(j_father)
          j_son=j_father
        enddo

        i(j_son)=ix
      enddo

      ! heap formed, now sort it
      ! vary i1 from n to 2 by -1
      ! extract a(i(1)) as the max element and replace a(i(i1)) with it
      ! then reconfigure the heap inserting the old a(i(i1)) to preserve a heap

      do i1=n,2,-1
        ix=i(i1)
        ai=a(ix)
        i(i1)=i(1)
        j_father=1

        j_son=2
        do while(j_son<i1)
          if(j_son.lt.(i1-1)) then
            if(a(i(j_son)).lt.a(i(j_son+1))) j_son=j_son+1
          endif
          if(ai.ge.a(i(j_son))) exit

          i(j_father)=i(j_son)
          j_father=j_son
          j_son=j_father+j_father
        enddo

        i(j_father)=ix
      enddo

      return
      end subroutine sort_hpisort

!****************************************************************************
! qksort (int n, float a())
! Sort an array such that a(1) <= a(2) <= ... <= a(n)
!****************************************************************************
! Input:
! n            number of elements in array a
! a            array(n) containing elements to be sorted
!
! Output:
! a            array(n) containing sorted elements
!****************************************************************************
! Notes:
! n must be less than 2^NSTACK, where NSTACK is defined above.
!****************************************************************************
      subroutine sort_qksort (n,a)
      implicit none

      integer     :: n      ! arguments
      real        :: a(:)   ! arguments

      integer     :: stack_lefts(NSTACK), stack_rights(NSTACK)  ! local
      integer     :: i, last, left, right, tos                  ! local
      logical     :: pop_sw
      real        :: a_val, temp
!------------------------------------------------

      ! initialize subarray lower and upper bounds to entire array
      tos = 1
      stack_lefts(tos)  = 1
      stack_rights(tos) = n
      pop_sw=.true.
!     print *,"a=",a

      do while(tos.gt.0)              ! while subarrays remain to be sorted

        if(pop_sw) then
          left  = stack_lefts(tos)    ! pop a subarray off the stack
          right = stack_rights(tos)
          tos = tos-1 
        endif
!       print *,"left=",left," right=",right

        if(right<=left) then
          pop_sw=.true.               !No data or 1 element that is sorted
        else 
          pop_sw=.false.
          i=(left+right)/2
          a_val=a(i)                  !get midpoint a-value
          last=left                   !a(left:last) will be < a_val 
          a(i)=a(left)                !leave hole at a(left)
          
          do i=left+1,right           !make a(last+1:right) >= a_val
            if(a(i).lt.a_val) then
              last=last+1             !current a(i) belongs in a(left:last)
              temp=a(i)               !swap a(i) and a(last)
              a(i)=a(last)
              a(last)=temp
            endif
          enddo

          a(left)=a(last)             !fill in a(left)
          a(last)=a_val               !put a_val in correct location in a
!         print *,"last=",last, a(last)
!         print *,"left ",a(left:last-1)
!         print *,"right=",a(last+1:right)

          if((right-last).gt.(last-left)) then
            tos = tos+1               !push larger subarray a(last+1:right)
            stack_lefts(tos) = last+1      
            stack_rights(tos) = right
            right=last-1              !process smaller subarray a(left:last-1)
          else
            tos = tos+1               !push larger subarray a(left:last-1)
            stack_lefts(tos) = left
            stack_rights(tos) = last-1
            left=last+1               !process smaller subarray a(last+1:right)
          endif
        endif
      enddo

      return
      end subroutine sort_qksort

!****************************************************************************
! qkisort (int n, float a(), int i())
! Sort an array of indices i() so that 
! a(i(1)) <= a(i(2)) <= ... <= a(i(n))
!****************************************************************************
! Input:
! n            number of elements in array a
! a            array(n) elements
! i            array(n) indices to be sorted
!
! Output:
! i            array(n) indices sorted
!****************************************************************************
! Notes:
! n must be less than 2^NSTACK, where NSTACK is defined above.
!****************************************************************************
      subroutine sort_qkisort (n,a,i)
      implicit none

      integer   :: n, i(:)            ! arguments
      real      :: a(:)               ! arguments

      integer     :: stack_lefts(NSTACK), stack_rights(NSTACK)  ! local
      integer     :: j, last, left, right, tos                  ! local
      logical     :: pop_sw
      real        :: a_val
      integer     :: temp, ij
!------------------------------------------------

      ! initialize subarray lower and upper bounds to entire array
      tos = 1
      stack_lefts(tos)  = 1
      stack_rights(tos) = n
      pop_sw=.true.
!     print *,"a=",a

      do while(tos.gt.0)              ! while subarrays remain to be sorted

        if(pop_sw) then
          left  = stack_lefts(tos)    ! pop a subarray off the stack
          right = stack_rights(tos)
          tos = tos-1 
        endif
!       print *,"left=",left," right=",right

        if(right<=left) then
          pop_sw=.true.             !no data or one element that is sorted
        else 
          pop_sw=.false.
          j=(left+right)/2
          ij=i(j)
          a_val=a(ij)                 !get midpoint a value
          last=left                   !a(i(left:last)) will be < a_val 
          i(j)=i(left)                !leave hole at i(left)
          
          do j=left+1,right           !make a(i(last+1:right)) >= a_val
            if(a(i(j)).lt.a_val) then
              last=last+1             !current a(i(j)) belongs in a(i(left:last)
              temp=i(j)               !swap a(i) and a(last)
              i(j)=i(last)
              i(last)=temp
            endif
          enddo

          i(left)=i(last)             !fill in a(left)
          i(last)=ij                  !put a_val in correct location in a
!         print *,"last=",last, a(last)
!         print *,"left ",a(left:last-1)
!         print *,"right=",a(last+1:right)

          if((right-last).gt.(last-left)) then
            tos = tos+1               !push larger subarray a(last+1:right)
            stack_lefts(tos) = last+1      
            stack_rights(tos) = right
            right=last-1              !process smaller subarray a(left:last-1)
          else
            tos = tos+1               !push larger subarray a(left:last-1)
            stack_lefts(tos) = left
            stack_rights(tos) = last-1
            left=last+1               !process smaller subarray a(last+1:right)
          endif
        endif
      enddo

      return
      end subroutine sort_qkisort

!****************************************************************************
! shsort (float a(), int p, int q)
! Shell sort:
! Sort a subarray bounded by p and q so that
! a(p) <= a(p+1) <= ... <= a(q)
!****************************************************************************
! Input:
! a            subarray(p:q) containing elements to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! a            subarray(p:q) sorted
!****************************************************************************
      subroutine sort_shsort (a,p,q)
      implicit none

      integer    :: p, q          ! arguments
      real       :: a(:)          ! arguments

      integer    :: i,j, istride  ! local
      real       :: ai            ! local
!------------------------------------------------

      istride = (q-p+1)/2
      do while(istride.gt.0)
        i=p+istride
        do while(i.le.q)
          ai=a(i)
          j=i
          do while((j-istride).ge.p)
            if(a(j-istride)<=ai) exit
            a(j) = a(j-istride)
            j = j-istride
          enddo
          a(j) = ai
          i = i+1
        enddo
        istride = istride/2
      enddo

      return
      end subroutine sort_shsort

!****************************************************************************
! shisort (float a(), int i(), int p, int q)
! Shell sort:
! Sort a subarray of indices bounded by p and q so that
! a(i(p)) <= a(i(p+1)) <= ... <= a(i(q))
!****************************************************************************
! Input:
! a            subarray(p:q) containing elements
! i            subarray(p:q) containing indices to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! i            subarray(p:q) of indices sorted
!****************************************************************************
      subroutine sort_shisort (a,i,p,q)
      implicit none

      integer    :: p, q, i(:)           ! arguments
      real       :: a(:)                 ! arguments

      integer    :: j,k,ij, istride      ! local
      real       :: aij                  ! local
!------------------------------------------------

      istride = (q-p+1)/2
      do while(istride.gt.0)
        j=p+istride
        do while(j.le.q)
          ij=i(j)
          aij=a(ij)
          k=j
          do while((k-istride).ge.p)
            if(a(i(k-istride))<=aij ) exit
            i(k) = i(k-istride)
            k=k-istride
          enddo
          i(k) = ij
          j=j+1
        enddo
        istride = istride/2
      enddo

      return
      end subroutine sort_shisort

!****************************************************************************
! inssort (float a(), int p, int q)
! insertion sort:
! Sort a subarray bounded by p and q so that
! a(p) <= a(p+1) <= ... <= a(q)
!****************************************************************************
! Input:
! a            subarray(p:q) containing elements to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! a            subarray(p:q) sorted
!****************************************************************************
      subroutine sort_inssort (a,p,q)
      implicit none

      integer    :: p, q       ! arguments
      real       :: a(:)       ! arguments

      integer    :: i,j        ! local
      real       :: ai         ! local
!------------------------------------------------

      do i=p+1, q
        ai=a(i)
        j=i
        do while(j.gt.p)
          if(a(j-1)<=ai ) exit
          a(j) = a(j-1)
          j = j-1
        enddo
        a(j) = ai
      enddo

      return
      end subroutine sort_inssort

!****************************************************************************
! insisort (float a(), int i(), int p, int q)
! insertion sort:
! Sort a subarray of indices bounded by p and q so that
! a(i(p)) <= a(i(p+1)) <= ... <= a(i(q))
!****************************************************************************
! Input:
! a            subarray(p:q) containing elements
! i            subarray(p:q) containing indices to be sorted
! p            lower bound of subarray; must be less than q
! q            upper bound of subarray; must be greater then p
!
! Output:
! i            subarray(p:q) of indices sorted
!****************************************************************************
      subroutine sort_insisort (a,i,p,q)
      implicit none

      integer    :: p, q, i(:)           ! arguments
      real       :: a(:)                 ! arguments

      integer    :: j,k,ij               ! local
      real       :: aij                  ! local
!------------------------------------------------

      do j=p+1, q
        ij=i(j)
        aij=a(ij)
        k=j
        do while(k.gt.p)
          if(a(i(k-1))<=aij ) exit
          i(k) = i(k-1)
          k=k-1
        enddo
        i(k) = ij
      enddo

      return
      end subroutine sort_insisort

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module sort_module

!!----------------------------- test driver -----------------------------!!
!!----------------------------- test driver -----------------------------!!
!!----------------------------- test driver -----------------------------!!

!      subroutine time_stamp(title, isw)
!        character(len=*), intent(in) :: title
!        integer, intent(in)!          :: isw
!  
!        character(len=10)!      !      :: t_str
!        integer!      !      !          :: hr, min
!        double precision!      !       :: t_now
!        double precision, save!       :: t_beg=-1
!
!        call date_and_time(time=t_str)
!        read(t_str,"(i2,i2,f6.3)") hr,min,t_now
!        t_now=t_now+hr*3600.d0+min*60.d0
!        if(isw.eq.0) t_beg=t_now
!        t_now=t_now-t_beg  
!        write(*,'(1x,a,a,f7.3)') title, ", time=", t_now
!        return
!      end subroutine time_stamp
!
!      subroutine data_check(a,ix,n)
!        real, intent(in)    :: a(:)
!        integer, intent(in) :: ix(:)
!        integer, intent(in) :: n
!
!        integer!      !       :: i
!
!        do i=2,n
!          if(a(ix(i-1)).gt.a(ix(i))) print *,"error", i, a(ix(i-1)), a(ix(i))
!        enddo    
!        return
!      end subroutine data_check
!
!      program main
!        use sort_module
!        implicit none
!
!        interface
!          subroutine data_check(a,ix,n)
!            real, intent(in)    :: a(:)
!            integer, intent(in) :: ix(:)
!            integer, intent(in) :: n
!          end subroutine data_check
!    
!          subroutine time_stamp(title, isw)
!            character(len=*), intent(in) :: title
!            integer, intent(in)!          :: isw
!          end subroutine time_stamp
!        end interface  
!
!        integer, parameter :: n=1000000
!        real,pointer!       :: a(:), a1(:)
!        integer!      !      :: i, istat
!        integer, pointer   :: ix(:), ix1(:)
!
!        allocate(a(n),stat=istat)
!        if(istat.ne.0) stop "allocation error"
!        allocate(a1(n),stat=istat)
!        if(istat.ne.0) stop "allocation error"
!        allocate(ix(n),stat=istat)
!        if(istat.ne.0) stop "allocation error"
!        allocate(ix1(n),stat=istat)
!        if(istat.ne.0) stop "allocation error"
!
!        do i=1,n
!          ix1(i)=i
!          call random_number(a1(i))
!        enddo
!
!        if(n.le.20000) then
!          a=a1
!          ix=ix1
!          call time_stamp("begin inssort",0)
!          call sort_inssort(a,1,n)
!          call time_stamp("end inssort",1)
!          call data_check(a,ix,n)
!  
!          a=a1
!          ix=ix1
!          call time_stamp("begin insisort",0)
!          call sort_insisort(a,ix,1,n)
!          call time_stamp("end insisort",1)
!          call data_check(a,ix,n)
!          if(maxval(abs(a-a1)).gt.0) print *,"sort index problem"
!          print *,""
!        endif
!  
!        a=a1
!        ix=ix1
!        call time_stamp("begin shsort",0)
!        call sort_shsort(a,1,n)
!        call time_stamp("end shsort",1)
!        call data_check(a,ix,n)
!  
!        a=a1
!        ix=ix1
!        call time_stamp("begin shisort",0)
!        call sort_shisort(a,ix,1,n)
!        call time_stamp("end shisort",1)
!        call data_check(a,ix,n)
!        if(maxval(abs(a-a1)).gt.0) print *,"sort index problem"
!        print *,""
!  
!        a=-a
!        ix=ix1
!        call time_stamp("begin back shisort",0)
!        call sort_shisort(a,ix,1,n)
!        call time_stamp("end back shisort",1)
!        call data_check(a,ix,n)
!        if(maxval(abs(a+a1)).gt.0) print *,"sort index problem"
!        print *,""
!  
!        a=a1
!        ix=ix1
!        call time_stamp("begin qksort",0)
!        call sort_qksort(n,a)
!        call time_stamp("end qksort",1)
!        call data_check(a,ix,n)
!  
!        a=a1
!        ix=ix1
!        call time_stamp("begin qkisort",0)
!        call sort_qkisort(n,a,ix)
!        call time_stamp("end qkisort",1)
!        call data_check(a,ix,n)
!        if(maxval(abs(a-a1)).gt.0) print *,"sort index problem"
!        print *,""
!  
!        a=-a
!        ix=ix1
!        call time_stamp("begin back qkisort",0)
!        call sort_qkisort(n,a,ix)
!        call time_stamp("end back qkisort",1)
!        call data_check(a,ix,n)
!        if(maxval(abs(a+a1)).gt.0) print *,"sort index problem"
!        print *,""
!  
!        a=a1
!        ix=ix1
!        call time_stamp("begin hpsort",0)
!        call sort_hpsort(n,a)
!        call time_stamp("end hpsort",1)
!        call data_check(a,ix,n)
!  
!        a=a1
!        ix=ix1
!        call time_stamp("begin hpisort",0)
!        call sort_hpisort(n,a,ix)
!        call time_stamp("end hpisort",1)
!        call data_check(a,ix,n)
!        if(maxval(abs(a-a1)).gt.0) print *,"sort index problem"
!        print *,""
!  
!        a=-a
!        ix=ix1
!        call time_stamp("begin back hpisort",0)
!        call sort_hpisort(n,a,ix)
!        call time_stamp("end back hpisort",1)
!        call data_check(a,ix,n)
!        if(maxval(abs(a+a1)).gt.0) print *,"sort index problem"
!        print *,""
!  
!        stop
!      end program main  
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
