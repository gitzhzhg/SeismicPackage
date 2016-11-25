!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- onesort.f90 --------------------------------!!
!!--------------------------- onesort.f90 --------------------------------!!
!!--------------------------- onesort.f90 --------------------------------!!


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
! Name       : ONESORT 
! Category   : sorts
! Written    : 2000-06-16   by: Tom Stoeckley
! Revised    : 2001-05-17   by: Tom Stoeckley
! Maturity   : production   2001-12-10
! Purpose    : Sort an array of integers or floating point numbers.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!             This primitive sorts an array of integers or
!             floating point numbers into ascending order.
!
!             Two different algorithms are provided, each
!             doing an N-log-N sort.  One of the algorithms
!             sorts in place, and the other uses an automatic
!             array for scratch space.
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
!      call onesort_sort1 (array,narray)
!      call onesort_sort2 (array,narray)
!                            b     i
!
! integer or real     array(narray) = array to sort.
! integer                   narray  = number of elements in the array.
!
! ONESORT_SORT1 does an n-log-n sort in place.
! ONESORT_SORT2 does an n-log-n sort using a scratch array.
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
!  3. 2001-12-10  Stoeckley  Add ability to sort integers.
!  2. 2000-10-19  Stoeckley  Add required missing documentation section.
!  1. 2000-08-22  Stoeckley  Initial version.
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


      module onesort_module
      implicit none
      public

      character(len=100),public,save :: ONESORT_IDENT = &
       '$Id: onesort.f90,v 1.3 2001/12/06 19:37:25 Stoeckley prod sps $'

      interface onesort_sort1
        module procedure onesort_sort1_real
        module procedure onesort_sort1_integer
      end interface

      interface onesort_sort2
        module procedure onesort_sort2_real
        module procedure onesort_sort2_integer
      end interface

      contains


!!------------------------- onesort sort1 real ---------------------------!!
!!------------------------- onesort sort1 real ---------------------------!!
!!------------------------- onesort sort1 real ---------------------------!!

 
      subroutine onesort_sort1_real (array,narray)
      implicit none
      real   ,intent(inout) :: array(:)        ! arguments
      integer,intent(in)    :: narray          ! arguments
      real                  :: value           ! local
      integer               :: istride,k,j     ! local
      integer               :: indx1,indx2     ! local

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
      end subroutine onesort_sort1_real


!!------------------------- onesort sort2 real ---------------------------!!
!!------------------------- onesort sort2 real ---------------------------!!
!!------------------------- onesort sort2 real ---------------------------!!

 
      subroutine onesort_sort2_real (array,narray)
      implicit none
      real   ,intent(inout) :: array(:)                   ! arguments
      integer,intent(in)    :: narray                     ! arguments
      real                  :: scratch(narray)            ! local
      integer               :: kk,nn,L1,L11               ! local
      integer               :: ii,i,i1,i2,i11,i22,j,j2    ! local

!----------GET STARTED.

      KK = 0
      NN = 1
1     NN = 2*NN
      KK = KK+1
      IF (narray > NN) GO TO 1
      L1 = 1

!----------DO THE SORT (TIME PROPORTIONAL TO N LOG N).

      DO II = 1,KK
           L11 = L1
           L1  = 2*L1
           DO I = 1,narray,L1
                I1  = I
                I2  = I+L11
                I11 = MIN(I1+L11-1,narray)
                I22 = MIN(I2+L11-1,narray)
                J2  = MIN(I +L1 -1,narray)
                DO J = I,J2
                     IF (I1 > I11) GO TO 19
                     IF (I2 > I22) GO TO 20
                     IF (array(I1) < array(I2)) GO TO 20
19                   scratch(J) = array(I2)
                     I2   = I2 + 1
                     cycle
20                   scratch(J) = array(I1)
                     I1   = I1 + 1
                end do
           end do
           array(:) = scratch(:)
      end do
      return
      end subroutine onesort_sort2_real


!!------------------------- onesort sort1 integer ---------------------------!!
!!------------------------- onesort sort1 integer ---------------------------!!
!!------------------------- onesort sort1 integer ---------------------------!!

 
      subroutine onesort_sort1_integer (array,narray)
      implicit none
      integer,intent(inout) :: array(:)        ! arguments
      integer,intent(in)    :: narray          ! arguments
      integer               :: value           ! local
      integer               :: istride,k,j     ! local
      integer               :: indx1,indx2     ! local

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
      end subroutine onesort_sort1_integer


!!------------------------- onesort sort2 integer ---------------------------!!
!!------------------------- onesort sort2 integer ---------------------------!!
!!------------------------- onesort sort2 integer ---------------------------!!

 
      subroutine onesort_sort2_integer (array,narray)
      implicit none
      integer,intent(inout) :: array(:)                   ! arguments
      integer,intent(in)    :: narray                     ! arguments
      integer               :: scratch(narray)            ! local
      integer               :: kk,nn,L1,L11               ! local
      integer               :: ii,i,i1,i2,i11,i22,j,j2    ! local

!----------GET STARTED.

      KK = 0
      NN = 1
1     NN = 2*NN
      KK = KK+1
      IF (narray > NN) GO TO 1
      L1 = 1

!----------DO THE SORT (TIME PROPORTIONAL TO N LOG N).

      DO II = 1,KK
           L11 = L1
           L1  = 2*L1
           DO I = 1,narray,L1
                I1  = I
                I2  = I+L11
                I11 = MIN(I1+L11-1,narray)
                I22 = MIN(I2+L11-1,narray)
                J2  = MIN(I +L1 -1,narray)
                DO J = I,J2
                     IF (I1 > I11) GO TO 19
                     IF (I2 > I22) GO TO 20
                     IF (array(I1) < array(I2)) GO TO 20
19                   scratch(J) = array(I2)
                     I2   = I2 + 1
                     cycle
20                   scratch(J) = array(I1)
                     I1   = I1 + 1
                end do
           end do
           array(:) = scratch(:)
      end do
      return
      end subroutine onesort_sort2_integer


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module onesort_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

