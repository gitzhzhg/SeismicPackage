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
! Name       : gridcheck
! Category   : miscellaneous
! Written    : 1999-10-28   by: Randy L. Selzler
! Revised    : 2005-01-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Check grid coordinates and count unique X, Y points.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! The input X and Y coordinates (already sorted) are examined to verify
! that they form a rectangular grid and the number of unique points on
! the X and Y axis are counted.
!
! Example:
!   pnt_cnt = 12
!   x_coord = 11.  13.  17.  19.    11.  13.  17.  19.   11.  13.  17.  19.
!   y_coord =  3.   3.   3.   3.     5.   5.   5.   5.    8.   8.   8.   8.
!
!   In this case the number of unique points in X and Y are 4 and 3.
!   The pnt_cnt must be equal to the product of the unique point counts.
!   The unique X values are the first 4 with a stride of 1.
!   The unique Y values are the first 3 with a stride of 4.
!
!   Note how the coordinates must be sorted (ascending, X most rapid).
!
!     If there is a need to support other sort orders (descending and
!     or Y most rapidly), the functionality can be enhanced.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!-------------------------------------------------------------------------------
!</header_word_doc>

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
!                    i        i        i       o      o     o
! call gridcheck(pnt_cnt, x_coord, y_coord, x_cnt, y_cnt, msg)
!
! integer                    pnt_cnt = total number of X and Y points.
! real,dimension(pnt_cnt)    x_coord = array of X coordinates.
! real,dimension(pnt_cnt)    y_coord = array of Y coordinates.
! real                       x_cnt   = number of unique X axis points.
! real                       y_cnt   = number of unique Y axis points.
! character(len=*)           msg     = msg(1:1) is non-blank (start of error
!                                      message) if a problem is detected.
!                                      X and Y counts are zeroed in this case.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2005-01-31  Stoeckley    Fix wrong name of this primitive in printouts.
!  4. 2000-07-20  Selzler      Fixed problems found by CPS Fortran Code Review.
!  3. 2000-01-25  Selzler      Clean up trailing blanks and block labels
!  2. 1999-11-19  Selzler      Added RCS "Id" strings to tag executeable
!  1. 1999-10-28  Selzler      Initial version.
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
!                     SPECIAL COMPILING REQUIREMENTS
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
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module gridcheck_module
      use mth_module
      implicit none

      private
      public :: gridcheck

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      character(len=100),public :: gridcheck_ident = &
        "$Id: gridcheck.f90,v 1.5 2005/01/31 14:06:02 Stoeckley prod sps $"

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

subroutine gridcheck(pnt_cnt, x_coord, y_coord, x_cnt2, y_cnt2, msg)
  implicit none
  integer,intent(in)                 :: pnt_cnt      ! argument
  real,dimension(pnt_cnt),intent(in) :: x_coord      ! argument
  real,dimension(pnt_cnt),intent(in) :: y_coord      ! argument
  integer,intent(out)                :: x_cnt2       ! argument
  integer,intent(out)                :: y_cnt2       ! argument
  character(len=*),intent(out)       :: msg          ! argument

  integer :: pnt_cnt_do, x_idx, y_compare, x_cnt, y_cnt   ! local

  ! assume there is an error, until refuted
  x_cnt2 = 0
  y_cnt2 = 0

  ! initialize first X, Y coordinate
  x_cnt = 1
  y_cnt = 1
  x_idx = 1

  pnt_cnt_loop: do pnt_cnt_do = 2, pnt_cnt
    ! count X and Y grid coordinates and verify correctness
    ! validate subsequent coordinates
    if(y_cnt == 1) then
      ! accumulate first row
      if(mth_compare(y_coord(pnt_cnt_do), &
                     y_coord(pnt_cnt_do - 1)) == 0) then
        ! Y coordinate has NOT changed
        if(mth_compare(x_coord(pnt_cnt_do), &
                       x_coord(pnt_cnt_do - 1)) > 0) then
          ! X coordinate HAS increased... all is well
          x_cnt = x_cnt + 1
        else
          ! X coordinate has NOT increased
          msg = 'gridcheck: X coordinate not ascending'
          return
        end if
      end if
    end if

    x_idx = 1 + mod(pnt_cnt_do - 1, x_cnt)

    y_compare = mth_compare(y_coord(pnt_cnt_do), &
      y_coord(pnt_cnt_do - 1))

    if(y_compare > 0) then
      ! Y coordinate HAS increased...

      if(x_idx == 1) then
        ! The previous row was complete
        y_cnt = y_cnt + 1
      else
        msg = 'gridcheck: X coordinate not complete'
        return
      end if
    else if(y_compare < 0 .or. x_idx == 1) then
      msg = 'gridcheck: Y coordinates not ascending'
      return
    end if

    if(mth_compare(x_coord(pnt_cnt_do), &
                   x_coord(x_idx)) /= 0) then
      msg = 'gridcheck: X coordinate not uniform'
      return
    end if

  end do pnt_cnt_loop

  if(x_idx == x_cnt) then
    ! everything appears to be OK
    msg = ' '
    x_cnt2 = x_cnt
    y_cnt2 = y_cnt
  else
    msg = 'gridcheck: X coordinate not complete'
  end if

  return
end subroutine gridcheck

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module gridcheck_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
