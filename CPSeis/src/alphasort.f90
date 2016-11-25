!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- alphasort.f90 --------------------------------!!
!!--------------------------- alphasort.f90 --------------------------------!!
!!--------------------------- alphasort.f90 --------------------------------!!


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
! Name       : ALPHASORT 
! Category   : sorts
! Written    : 2001-05-17   by: Tom Stoeckley
! Revised    : 2006-10-31   by: B. Menger
! Maturity   : production
! Purpose    : Sort an array of character strings into alphabetical order.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!             This primitive sorts an array of character
!             strings into alphabetical order.
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
!                                          b     i
!           call alphasort_sort (        array,narray)
!           call alphasort_sort (indices,array,narray)
!                                  b       i     i
!
! integer            indices(narray) = indices into array of strings to sort.
! character(len=*)     array(narray) = array of strings to sort.
! integer                    narray  = number of strings in the array.
!
!  These two routines do an n-log-n sort of the strings in place.
!
!  The routine without the INDICES argument sorts ARRAY.  The sorted
!  order of ARRAY then is in the order of ARRAY(INDX) where INDX=1,NARRAY.
!
!  The routine with the INDICES argument rearranges the INDICES instead of
!  sorting the ARRAY.  The sorted order of ARRAY then is in the order of
!  ARRAY(INDICES(INDX)) where INDX=1,NARRAY.  INDICES must be initialized
!  to the values 1,...,NARRAY.
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
!  2. 2006-10-31  B. Menger  Removed Unused Variables.
!  1. 2001-12-17  Stoeckley  Initial version.
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


      module alphasort_module
      implicit none
      public

      character(len=100),public,save :: ALPHASORT_IDENT = &
'$Id: alphasort.f90,v 1.2 2006/10/30 14:01:43 Menger prod sps $'

      interface alphasort_sort
        module procedure alphasort_sort_strings
        module procedure alphasort_sort_indices
      end interface

      contains


!!---------------------- alphasort sort strings ---------------------------!!
!!---------------------- alphasort sort strings ---------------------------!!
!!---------------------- alphasort sort strings ---------------------------!!

 
      subroutine alphasort_sort_strings (array,narray)
      implicit none
      character(len=*),intent(inout) :: array(:)        ! arguments
      integer         ,intent(in)    :: narray          ! arguments
      character(len=222)             :: value           ! local
      integer                        :: istride,k,j     ! local
      integer                        :: indx1,indx2     ! local

      istride = narray
      do
           istride = istride/2
           if (istride == 0) return
           k = narray - istride
           do j = 1, k
                indx1 = j
                do
                     indx2 = indx1 + istride
                     if (array(indx1) <= array(indx2)) exit

                     value        = array(indx1)        ! switch
                     array(indx1) = array(indx2)        ! switch
                     array(indx2) = value               ! switch

                     if (indx1 <= istride) exit
                     indx1 = indx1 - istride
                end do
           end do
      end do
      return
      end subroutine alphasort_sort_strings


!!------------------------- alphasort sort indices ------------------------!!
!!------------------------- alphasort sort indices ------------------------!!
!!------------------------- alphasort sort indices ------------------------!!


      subroutine alphasort_sort_indices (indices,array,narray)
      implicit none
      integer         ,intent(in)    :: narray               ! arguments
      integer         ,intent(inout) :: indices(narray)      ! arguments
      character(len=*),intent(in)    :: array  (narray)      ! arguments

      integer                        :: istride,k,j          ! local
      integer                        :: indx1,indx2          ! local

      istride = narray
      do
           istride = istride/2
           if (istride < 1) exit
           do j = istride+1,narray
                indx2 = indices(j)
                k     = j
                do
                     if (k-istride < 1) exit
                     indx1 = indices(k-istride)
                     if (array(indx1) <= array(indx2)) exit
                     indices(k) = indices(k-istride)
                     k          = k - istride
                end do
                indices(k) = indx2
           end do
      end do
      return
      end subroutine alphasort_sort_indices


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module alphasort_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

