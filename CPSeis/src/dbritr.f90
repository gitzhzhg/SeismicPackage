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
! Name       : dbritr   (DeBRIghten TRace)
! Category   : filters
! Written    : 1989-06-26   by: John Reed
! Revised    : 2000-07-19   by: Randy Selzler, Data-Warp, Inc.
! Maturity   : production   2000-07-19
! Purpose    : Apply debrightening to individual events that exceed
!                debrightening threshold.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!   When called, DBRITR will find the largest event over debri_max, apply
!   a scaling factor to bring this event back to debri_max and apply a ramp
!   of length debri_tpr_cnt to each side of this event.  This process will be
!   iterated until there are no events over debri_max.
!
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
!</header_word_doc

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
!                    b             i             i          i
!  call  dbritr (debri_vector, debri_pt_cnt, debri_max, debri_tpr_cnt)
!
! real(*)                 debri_vector  = samples to be debrightened
! integer                 debri_pt_cnt  = number of points in debri_vector
! real                    debri_max     = debrightening threshold
! integer                 debri_tpr_cnt = point count in debrighten taper
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
!  6. 2000-07-19  Selzler      Fixed problems found by CPS Fortran Code Review.
!  5. 2000-01-25  Selzler      Clean up trailing blanks and block labels
!  4. 1999-11-19  Selzler      Added RCS "Id" strings to tag executeable
!  3. 1999-09-20  Selzler      Conversion to f90
!  2. 1999-01-11  Goodger      Begin using the fortran90 compiler.
!  1. 1998-06-26  John Reed    New debrighten algorithm written as a primitive

!
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


      module dbritr_module
      implicit none

      private
      public :: dbritr

      character(len=100),public :: dbritr_ident = &
        "$Id: dbritr.f90,v 1.6 2000/07/18 22:41:32 sps prod sps $"

      contains

!!----------------------------- DBRITR -------------------------------!!
!!----------------------------- DBRITR -------------------------------!!
!!----------------------------- DBRITR -------------------------------!!

      SUBROUTINE DBRITR(debri_vector, debri_pt_cnt, debri_max, debri_tpr_cnt)
      USE mth_module
      IMPLICIT NONE

      INTEGER,                      INTENT(IN)     :: debri_pt_cnt  ! argument
      INTEGER,                      INTENT(IN)     :: debri_tpr_cnt ! argument
      REAL,                         INTENT(IN)     :: debri_max     ! argument
      REAL,dimension(debri_pt_cnt),INTENT(INOUT)   :: debri_vector  ! argument

      INTEGER :: big_abs_idx, J                ! local variable
      REAL :: big_abs_value, RFACT, XJ, FACT   ! local variable

      ! Find largest point
      big_abs_idx = mth_isamax(debri_pt_cnt,debri_vector,1)
      big_abs_value = ABS(debri_vector(big_abs_idx))

      DO WHILE(big_abs_value > debri_max)
        ! Quit if none above threshold
        RFACT = debri_max/big_abs_value*0.95

        DO J = MAX(big_abs_idx - debri_tpr_cnt + 1,1), &
          MIN(big_abs_idx + debri_tpr_cnt - 1,debri_pt_cnt)
          XJ = ABS(FLOAT(J - big_abs_idx))/FLOAT(debri_tpr_cnt)
          FACT = RFACT + (1.0 - RFACT)*XJ

          ! Apply ramp function to event
          debri_vector(J) = debri_vector(J)*FACT
        END DO

        ! Find largest point
        big_abs_idx = mth_isamax(debri_pt_cnt,debri_vector(1),1)
        big_abs_value = ABS(debri_vector(big_abs_idx))
      END DO

      RETURN
      END SUBROUTINE DBRITR

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module dbritr_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
