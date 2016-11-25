!<CPS_v1 type="PROCESS"/>
!!------------------------------- SETMUTE.f90 --------------------------------!!
!!------------------------------- SETMUTE.f90 --------------------------------!!
!!------------------------------- SETMUTE.f90 --------------------------------!!

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
!                         C P S   P R O C E S S
!
! Name       : SETMUTE
! Category   : amplitude_mod
! Written    : 1998-01-09   by: Chuck I. Burch
! Revised    : 2000-12-14   by: Bob Baumel
! Maturity   : production   2001-07-13
! Purpose    : Set the head mute header word based on an amplitude threshold.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! SETMUTE sets the head mute header word (number 2) to the index of the first
! trace sample whose absolute value meets or exceeds a specified amplitude
! threshold, possibly adjusted by the MUTE_ADD parameter.
!
! If MODE = RELATIVE, the amplitude threshold is relative to the LAV of each
! trace; in particular, the head mute index is set to the position of the
! first sample whose absolute value >= FACTOR * trace LAV (possibly adjusted
! by MUTE_ADD).
!
! If MODE = FIXED, the amplitude threshold is fixed; in particular, the
! head mute index is set to the position of the first sample whose absolute
! value >= AMPL_SET (possibly adjusted by MUTE_ADD).
!
! In either mode (RELATIVE or FIXED), if no trace sample meets the condition
! for setting the head mute index, then the trace is assumed to be dead, in
! which case SETMUTE actually kills the trace.
!
! This process is intended primarily to establish mutes on contractor processed
! post-stack data.
!
! SETMUTE only sets the (head) mute header word, but DOES NOT APPLY this mute.
! If you wish to apply the mute, you must follow SETMUTE by the MUTE process
! using OPT_MUTE = REST_HEAD.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process outputs the same traces as it receives, altering header word 2
! (and possibly header words 25 and 64). Trace values are usually identical to
! those in the input traces, except when no trace sample meets the condition
! for setting the head mute index, in which case SETMUTE kills the trace.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                       Action taken
! ----     -----------                       ------------
! NDPT     number of samples in trace        used but not changed
! DT       sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 2       Head mute index            Set
! 25      Largest Abs Value          Possibly set *
! 64      Tail mute index            Possibly set **
!
!  * Header word 25 is reset to zero when SETMUTE kills a trace; otherwise it
!    remains unchanged.
!
! ** Header word 64 may be reset to make sure it is >= header word 2;
!    otherwise it is kept unchanged.
!
! All DEAD traces output by SETMUTE (whether killed by this process or already
! dead upon input) will have header word 2 = header word 64 = NDPT.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  9. 2001-07-13  Bob Baumel   Changed wrapped_up flag to skip_wrapup. PROD.
!  8. 2000-11-15  Bob Baumel   Added note to doc and CFE screen about following
!                              SETMUTE with MUTE process if wish to apply mute.
!  7. 2000-10-30  Bob Baumel   Added new MUTE_ADD capability.
!  6. 2000-03-28  Bob Baumel   Added gui_def to doc (no changes in code).
!  5. 2000-03-22  Bob Baumel   Clean up code; implement new header conventions.
!  4. 1999-11-17  David Sharp  Converted from old system.
!  3. 1998-12-18  Vunderink    Begin using the f90 compiler.
!  2. 1998-01-12  Vunderink    Changed some do loop ranges and kill trace
!                              if no value meets or exceeds the threshold.
!  1. 1998-01-09  C I Burch    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
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

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS SETMUTE Process/NC=80>
!
!                        SETMUTE Process
!  Set the head mute header word based on an amplitude threshold.
!
!                        MODE=`CCCCCCC
!
!        FACTOR=`FFFFFFFFFFF       AMPL_SET=`FFFFFFFFFFF
!
!                    MUTE_ADD=`FFFFFFFFFFF
!
!
! Note: SETMUTE only sets the (head) mute header word but does not
!  apply this mute. If you wish to apply the mute, follow SETMUTE
!          by the MUTE process using OPT_MUTE = REST_HEAD.
!
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Whether amplitude threshold is RELATIVE (to LAV) or FIXED. </Tip>
! Default = RELATIVE
! Allowed = RELATIVE
! Allowed = FIXED
!
! If MODE = RELATIVE, the amplitude threshold is relative to the LAV of each
! trace; in particular, the head mute index is set to the position of the
! first sample whose absolute value >= FACTOR * trace LAV (possibly adjusted
! by MUTE_ADD).
!
! If MODE = FIXED, the amplitude threshold is fixed; in particular, the
! head mute index is set to the position of the first trace sample whose
! absolute value >= AMPL_SET (possibly adjusted by MUTE_ADD).
!</Help>
!
!<Help KEYWORD="FACTOR">
!<Tip> Factor for the RELATIVE operation method. </Tip>
! Default = 0.01
! Allowed: 1.0 > real > 0.0
! If MODE = RELATIVE, then the head mute index is set to the position of the
! first sample whose absolute value >= FACTOR * trace LAV (possibly adjusted
! by MUTE_ADD).
!</Help>
!
!<Help KEYWORD="AMPL_SET">
!<Tip> Absolute threshold for the FIXED operation method. </Tip>
! Default = 0.01
! Allowed = real > 0.0
! If MODE = FIXED, then the head mute index is set to the position of the
! first sample whose absolute value >= AMPL_SET (possibly adjusted by
! MUTE_ADD).
!</Help>
!
!<Help KEYWORD="MUTE_ADD">
!<Tip> Time, in seconds, to add to mute position defined by threshold. </Tip>
! Default = 0.0
! Allowed = real
! In setting the head mute index, this amount (in seconds) is added to the
! position of the first trace sample whose absolute value meets or exceeds
! the specified amplitude threshold.
!
! Positive MUTE_ADD shifts the head mute index downwards.
! Negative MUTE_ADD shifts the head mute index upwards.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!

      module setmute_module
      use pc_module
      use named_constants_module
      use mutehw_module
      implicit none
      private
      public :: setmute_create
      public :: setmute_initialize
      public :: setmute_update
      public :: setmute_delete
!<execute_only>
      public :: setmute
      public :: setmute_wrapup
!</execute_only>

      character(len=100),public,save :: SETMUTE_IDENT = &
'$Id: setmute.f90,v 1.9 2001/07/12 19:47:59 sps prod sps $'

!!---------------------- parameter structure ------------------------------!!
!!---------------------- parameter structure ------------------------------!!
!!---------------------- parameter structure ------------------------------!!

      type,public :: setmute_struct

        private

        logical          :: skip_wrapup    ! wrapup flag.

        character(len=8) :: mode           ! process parameters.
        real             :: ampl_set       ! process parameters.
        real             :: factor         ! process parameters.
        real             :: mute_add       ! process parameters.

        integer          :: ndpt           ! globals.
        real             :: dt             ! globals.

        integer          :: mute_add_samp  ! dependent variables.
        integer          :: ktestmax       ! dependent variables.

      end type setmute_struct

!!--------------------------------- data ----------------------------------!!
!!--------------------------------- data ----------------------------------!!
!!--------------------------------- data ----------------------------------!!

      type(setmute_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!

      subroutine setmute_create (obj)
      implicit none
      type(setmute_struct),pointer :: obj       ! arguments

      allocate (obj)

      call setmute_initialize (obj)
      return
      end subroutine setmute_create

!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!

      subroutine setmute_delete (obj)
      implicit none
      type(setmute_struct),pointer :: obj       ! arguments

!<execute_only>
      call setmute_wrapup (obj)
!</execute_only>

      deallocate(obj)
      return
      end subroutine setmute_delete

!!----------------------------- initialize --------------------------------!!
!!----------------------------- initialize --------------------------------!!
!!----------------------------- initialize --------------------------------!!

   subroutine setmute_initialize (obj)
   implicit none
   type(setmute_struct),intent(inout) :: obj       ! arguments

   obj%mode     = 'RELATIVE'
   obj%factor   = 0.01
   obj%ampl_set = 0.01
   obj%mute_add = 0.0

   call setmute_update (obj)
   return
   end subroutine setmute_initialize

!!------------------------- start of update -------------------------------!!
!!------------------------- start of update -------------------------------!!
!!------------------------- start of update -------------------------------!!

  subroutine setmute_update (obj)
   implicit none
   type(setmute_struct),intent(inout),target :: obj     ! arguments

   object => obj              ! needed for traps.
   obj%skip_wrapup = .true.   ! needed for the wrapup routine.

!!------------------------- read parameters -------------------------------!!
!!------------------------- read parameters -------------------------------!!
!!------------------------- read parameters -------------------------------!!

   call pc_get_global ('NDPT', obj%ndpt)
   call pc_get_global ('DT',   obj%dt)

   call pc_get ('MODE'     , obj%mode)
   call pc_get ('FACTOR'   , obj%factor)
   call pc_get ('AMPL_SET' , obj%ampl_set)
   call pc_get ('MUTE_ADD' , obj%mute_add)

!!-------------------------- verify parameters ----------------------------!!
!!-------------------------- verify parameters ----------------------------!!
!!-------------------------- verify parameters ----------------------------!!

   call pc_put_sensitive_field_flag ('FACTOR'  , .false.)
   call pc_put_sensitive_field_flag ('AMPL_SET', .false.)

   if (obj%mode(1:1)=='R' .or. obj%mode(1:1)=='r') then
     obj%mode = 'RELATIVE'
     call pc_put_sensitive_field_flag ('FACTOR', .true.)
   else if (obj%mode(1:1)=='F' .or. obj%mode(1:1)=='f') then
     obj%mode = 'FIXED'
     call pc_put_sensitive_field_flag ('AMPL_SET', .true.)
   else
     call pc_error ('MODE must be either RELATIVE or FIXED.')
   end if

   obj%factor = abs(obj%factor)
   if (obj%mode == 'RELATIVE') then
     if (obj%factor == 0.0) then
       call pc_error ('FACTOR must be > 0.')
     else if (obj%factor >= 1.0) then
       call pc_error ('FACTOR must be < 1.')
     end if
   end if

   obj%ampl_set = abs(obj%ampl_set)
   if (obj%mode == 'FIXED') then
     if (obj%ampl_set == 0.) then
       call pc_error ('AMPL_SET must be > 0.')
     end if
   end if

   obj%mute_add_samp = nint (obj%mute_add / obj%dt)
   obj%mute_add  = obj%mute_add_samp * obj%dt

!!----------------------- write parameters --------------------------------!!
!!----------------------- write parameters --------------------------------!!
!!----------------------- write parameters --------------------------------!!

   call pc_put_options_field ('MODE', (/ 'RELATIVE', 'FIXED   ' /), 2)

   call pc_put ('MODE'     , obj%mode)
   call pc_put ('FACTOR'   , obj%factor)
   call pc_put ('AMPL_SET' , obj%ampl_set)
   call pc_put ('MUTE_ADD' , obj%mute_add)

!!----------------------- prepare for execution ---------------------------!!
!!----------------------- prepare for execution ---------------------------!!
!!----------------------- prepare for execution ---------------------------!!

!<execute_only>

   if (pc_do_not_process_traces()) return
   obj%skip_wrapup = .false.        ! needed for the wrapup routine.
   obj%ktestmax = min (obj%ndpt - obj%mute_add_samp, obj%ndpt)

!</execute_only>

!!------------------------- finish update ---------------------------------!!
!!------------------------- finish update ---------------------------------!!
!!------------------------- finish update ---------------------------------!!
   return
  end subroutine setmute_update

!!--------------------------- main execution ------------------------------!!
!!--------------------------- main execution ------------------------------!!
!!--------------------------- main execution ------------------------------!!

!<execute_only>

      subroutine setmute (obj, ntr, hd, tr)
      implicit none
      type(setmute_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(inout) :: ntr           ! arguments
      double precision    ,intent(inout) :: hd(:,:)       ! arguments
      real                ,intent(inout) :: tr(:,:)       ! arguments

      real       :: big, testamp           ! local
      integer    :: i, k                   ! local

      if (ntr == NO_MORE_TRACES) then
        call setmute_wrapup (obj)
        return
      end if

      do i = 1, ntr

        big = hd(HDR_LAV,i)
        if (big == 0.0) goto 10           ! trace is already dead

        if (obj%mode == 'RELATIVE') then
          testamp = obj%factor * big
        else
          testamp = obj%ampl_set
        end if

        do k = 1, obj%ktestmax
          if (abs(tr(k,i)) >= testamp) then
            hd(HDR_TOP_MUTE,i) = k + obj%mute_add_samp
            goto 20
          end if
        end do

        tr(:obj%ndpt,i) = 0.0     ! kill trace if get here
        hd(HDR_LAV,i)   = 0.0
  10    hd(HDR_TOP_MUTE,i) = obj%ndpt

  20    call mutehw (hd(:,i), tr(:,i), obj%ndpt, 0.0, MUTEHW_SET)

      end do

      return
      end subroutine setmute

!</execute_only>

!!------------------------------- wrapup ----------------------------------!!
!!------------------------------- wrapup ----------------------------------!!
!!------------------------------- wrapup ----------------------------------!!

!<execute_only>

      subroutine setmute_wrapup (obj)
      implicit none
      type(setmute_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine setmute_wrapup

!</execute_only>

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module SETMUTE_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
