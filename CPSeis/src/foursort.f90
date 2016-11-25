!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- foursort.f90 --------------------------------!!
!!--------------------------- foursort.f90 --------------------------------!!
!!--------------------------- foursort.f90 --------------------------------!!


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
! Name       : FOURSORT 
! Category   : sorts
! Written    : 2001-09-21   by: Tom Stoeckley
! Revised    : 2001-09-21   by: Tom Stoeckley
! Maturity   : production   2001-10-18
! Purpose    : Sort an array of structures containing four integers.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive sorts an array of structures containing four integers.
!
! The structure is defined as follows:
!
!     type,public :: foursort_ints
!       integer :: one               ! primary sort key.
!       integer :: two               ! secondary sort key.
!       integer :: three             ! tertiary sort key.
!       integer :: four              ! fourth sort key.
!     end type foursort_ints
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
!                          CALLING SEQUENCE               
!
! To sort an array of keys into ascending order:
!
!      call foursort_sort (keys,nkeys)
!                           b     i
!
! To compare two keys:
!
!      compare = foursort_compare (key1,key2)
!         o                         i    i
!
! type(foursort_ints)  keys(nkeys) = array of keys to sort.
! integer              nkeys       = number of keys in the array.
! type(foursort_ints)  key1        = first  key to compare.
! type(foursort_ints)  key2        = second key to compare.
! integer              compare     = result of comparison of KEY1 and KEY2.
!
! FOURSORT_SORT:
!
!  Does an n-log-n sort in place.
!  Sorts the array of keys using the primary, secondary, tertiary, and fourth
!    keys in that order of priority.
!
! FOURSORT_COMPARE:
!
!  Returns COMPARE = -1  if  KEY1 <  KEY2.
!  Returns COMPARE =  0  if  KEY1 == KEY2.
!  Returns COMPARE =  1  if  KEY1 >  KEY2.
!  Uses all four components in the same priority order as FOURSORT_SORT.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  2.
!  1. 2001-10-18  Stoeckley  Initial version.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module foursort_module
      implicit none
      public

      character(len=100),public,save :: FOURSORT_IDENT = &
       '$Id: foursort.f90,v 1.1 2001/10/17 20:05:35 Stoeckley prod sps $'


      type,public :: foursort_ints
        integer :: one               ! primary sort key.
        integer :: two               ! secondary sort key.
        integer :: three             ! tertiary sort key.
        integer :: four              ! fourth sort key.
      end type foursort_ints


      contains


!!------------------------- foursort sort --------------------------------!!
!!------------------------- foursort sort --------------------------------!!
!!------------------------- foursort sort --------------------------------!!

 
      subroutine foursort_sort (keys,nkeys)
      implicit none
      integer            ,intent(in)    :: nkeys           ! arguments
      type(foursort_ints),intent(inout) :: keys(nkeys)     ! arguments
      type(foursort_ints)               :: key             ! local
      integer                           :: istride,k,j     ! local
      integer                           :: indx1,indx2     ! local

      istride = nkeys
      do
           istride = istride/2
           if (istride < 1) exit
           k = nkeys - istride
           do j = 1, k
                indx1 = j
                do
                     indx2 = indx1 + istride
                     if (foursort_compare(keys(indx1),keys(indx2)) <= 0) exit

                     key         = keys(indx1)        ! switch
                     keys(indx1) = keys(indx2)        ! switch
                     keys(indx2) = key                ! switch

                     if (indx1 <= istride) exit
                     indx1 = indx1 - istride
                end do
           end do
      end do
      return
      end subroutine foursort_sort


!!----------------------- foursort compare ----------------------------!!
!!----------------------- foursort compare ----------------------------!!
!!----------------------- foursort compare ----------------------------!!


      function foursort_compare (key1,key2) result (compare)
      implicit none
      type(foursort_ints),intent(in) :: key1,key2         ! arguments
      integer                        :: compare           ! result

      if      (key1%one   < key2%one  ) then ; compare = -1
      else if (key1%one   > key2%one  ) then ; compare =  1
      else if (key1%two   < key2%two  ) then ; compare = -1
      else if (key1%two   > key2%two  ) then ; compare =  1
      else if (key1%three < key2%three) then ; compare = -1
      else if (key1%three > key2%three) then ; compare =  1
      else if (key1%four  < key2%four ) then ; compare = -1
      else if (key1%four  > key2%four ) then ; compare =  1
      else                                   ; compare =  0
      end if
      return
      end function foursort_compare


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module foursort_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

