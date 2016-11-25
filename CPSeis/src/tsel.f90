!<CPS_v1 type="PROCESS"/>
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
! Name       : TSEL  (Time SELect)
! Category   : miscellaneous
! Written    : 1988-10-06  by: Bob Baumel
! Revised    : 2002-04-26  by: Donna K. Vunderink
! Maturity   : production  2002-05-06
! Purpose    : Change the time range of traces.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! TSEL will reset the beginning and/or ending time for traces.  The TIM_BEG
! parameter will reset the TSTRT global (time of the first trace sample).
!
! Trace samples will be lost at the beginning of traces if TIM_BEG is greater
! than the previous TSTRT time.  Trace samples will be lost at the end of the
! traces if TIM_END is less than the previous end of trace time.
!
! Zeros will be padded at the beginning of traces if TIM_BEG is less
! than the previous TSTRT time.  Zeros will be padded at the end of the
! traces if TIM_END is greater than the previous end of trace time.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (usually altered).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      may be changed
! TSTRT    starting time on trace                may be changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 2       Head mute index            may be changed
! 64      Tail mute index            may be changed
! 25      Trace Largest Abs Value    may be changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
! 19. 2002-05-06  Vunderink   Added parallel control parameters
! 18. 2001-04-19  Bob Baumel  Fix global problems; general code cleanup.
! 17. 2000-12-12  Stoeckley   Add missing help for display-only parameters.
! 16. 2000-12-11  Stoeckley   Add missing advice_doc section.
! 15. 2000-12-08  Stoeckley   Change wrapup flag.
! 14. 2000-04-10  M. O'Brien  Impemented EzGUI layout
! 13. 2000-03-08  M. O'Brien  Fixed registration of global variables for
!                               gui updates
! 12. 2000-02-25  M. O'Brien  Removed blocks of unused code.
!                             Brought up to date with latest mutehw_module.
!                             Polished the code slightly.
! 11. 1999-12-29  M. O'Brien  Added RCS character ID variable
!                             Brought xml tags up to date
! 10. 1999-09-13  M. O'Brien  Updates for conformance with new pc_module
!  9. 1999-08-23  M. O'Brien  Changed header array type to double precision
!  8. 1999-08-16  M. O'Brien  Changed state variable use per changes
!                              in pc_module
!  7. 1999-08-10  M. O'Brien  replaced pc_reset call with pc_error
!  6. 1999-08-06  M. O'Brien  Full f90 conversion
!  5. 1998-11-20  K. Goodger  Begin using the fortran90 compiler.
!  4. 1993-03-25  K. Goodger  Fix bad dimension on HD array.
!  3. 1992-02-18  Troutt      Add handling for header word 64 (tail mute).
!                              Added call to MUTEHW.
!  2. 1990-01-19  Troutt      Minor change to force tim_beg to a multiple of DT
!                              if round-off error is indicated.
!  1. 1988-10-06  Howard      NWIH and LTR conversion
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

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.

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
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
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

!-------------------------------------------------------------------------------
!<gui_def>
!<NS TSEL Process/NC=80>
!
!    Change the time range of traces.
!
!
!     Input data:
!           start time`FFFFFFF      end time`FFFFFFF
!     <PARMS STARTTIME[TBEG/EN]>
!     <PARMS ENDTIME[TEND/EN]>
!
!
!     Output data:
!           TIM_BEG=~~`FFFFFFF      TIM_END=`FFFFFFF
!</gui_def>
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="TBEG" TYPE="DISPLAY_ONLY">
!<Tip> Starting time in seconds of your input traces (display only). </Tip>
!</Help>
!
!<Help KEYWORD="TEND" TYPE="DISPLAY_ONLY">
!<Tip> Ending time in seconds of your input traces (display only). </Tip>
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> New starting time in seconds for your output traces. </Tip>
! Default = TSTRT
! Allowed = real
!
! The TIM_BEG parameter will reset the TSTRT global (time of the first trace
! sample).
!
! Trace samples will be lost at the beginning of traces if TIM_BEG is greater
! than the previous TSTRT time.
!
! Zeros will be padded at the beginning of traces if TIM_BEG is less than the
! previous TSTRT time.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> New ending time in seconds for your output traces. </Tip>
! Default = End_Trace
! Allowed = real>TIM_BEG
!
! Trace samples will be lost at the end of the traces if TIM_END is less than
! the previous end of trace time.
!
! Zeros will be padded at the end of the traces if TIM_END is greater than the
! previous end of trace time.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!------------------------------ start of module -----------------------------!!
!!------------------------------ start of module -----------------------------!!
!!------------------------------ start of module -----------------------------!!


      module tsel_module
      use pc_module
      use named_constants_module
      use mutehw_module
      use lav_module

      implicit none

      private
      public :: tsel_create     ! uses the parameter cache.
      public :: tsel_initialize ! uses the parameter cache.
      public :: tsel_update     ! uses the parameter cashe.
      public :: tsel_delete
!<execute_only>
      public :: tsel
      public :: tsel_wrapup
!</execute_only>

      character(len=100),public,save :: TSEL_IDENT = &
'$Id: tsel.f90,v 1.19 2002/05/02 16:01:49 Vunderink prod sps $'

!!--------------------------- parameter structure ----------------------------!!
!!--------------------------- parameter structure ----------------------------!!
!!--------------------------- parameter structure ----------------------------!!

      type,public :: tsel_struct

        logical         :: skip_wrapup      ! wrapup flag

        real            :: tim_beg          ! parms - new start time (new tstrt)
        real            :: tim_end          ! parms - new end time

        integer         :: ndpt             ! input globals
        real            :: tstrt, dt        ! input globals

        integer         :: ndptnew          ! new ndpt global
        integer         :: ishift           ! number of samples to shift
        integer         :: istrt, istop     ! range of output trace copied from
                                            ! input trace

      end type tsel_struct


!!----------------------------------- data -----------------------------------!!
!!----------------------------------- data -----------------------------------!!
!!----------------------------------- data -----------------------------------!!

      type (tsel_struct),pointer,save :: object           ! needed for traps

      contains


!!---------------------------------- create ----------------------------------!!
!!---------------------------------- create ----------------------------------!!
!!---------------------------------- create ----------------------------------!!

      subroutine tsel_create (obj)
      implicit none
      type (tsel_struct),pointer :: obj           ! arguments

      allocate (obj)

      call tsel_initialize (obj)

      return
      end subroutine tsel_create


!!---------------------------------- delete ----------------------------------!!
!!---------------------------------- delete ----------------------------------!!
!!---------------------------------- delete ----------------------------------!!

      subroutine tsel_delete (obj)
      implicit none
      type (tsel_struct),pointer :: obj     ! arguments

      deallocate (obj)

      return
      end subroutine tsel_delete


!!-------------------------------- initialize --------------------------------!!
!!-------------------------------- initialize --------------------------------!!
!!-------------------------------- initialize --------------------------------!!

      subroutine tsel_initialize (obj)
      implicit none
      type (tsel_struct),pointer :: obj      ! arguments

      call pc_get_global ('NDPT' , obj%ndpt)
      call pc_get_global ('TSTRT', obj%tstrt)
      call pc_get_global ('DT'   , obj%dt)

      obj%tim_beg  = obj%tstrt
      obj%tim_end  = obj%tstrt + obj%dt*(obj%ndpt-1)

      call tsel_update (obj)

      return
      end subroutine tsel_initialize

!!------------------------------ start of update -----------------------------!!
!!------------------------------ start of update -----------------------------!!
!!------------------------------ start of update -----------------------------!!

      subroutine tsel_update (obj)
      implicit none
      type (tsel_struct),intent(inout),target :: obj             ! arguments

      object => obj                  ! needed for traps
      obj%skip_wrapup = .true.       ! needed for wrapup routine

!!------------------------------ read data cards -----------------------------!!
!!------------------------------ read data cards -----------------------------!!
!!------------------------------ read data cards -----------------------------!!

      call pc_get_global ('NDPT' , obj%ndpt)
      call pc_get_global ('TSTRT', obj%tstrt)
      call pc_get_global ('DT'   , obj%dt)

      call pc_get ('TIM_BEG', obj%tim_beg)
      call pc_get ('TIM_END', obj%tim_end)

!!----------------------------- verify parameters ----------------------------!!
!!----------------------------- verify parameters ----------------------------!!
!!----------------------------- verify parameters ----------------------------!!

      obj%tim_beg = obj%tstrt  +  nint((obj%tim_beg-obj%tstrt)/obj%dt) * obj%dt
      obj%tim_end = obj%tstrt  +  nint((obj%tim_end-obj%tstrt)/obj%dt) * obj%dt

      obj%ndptnew = nint((obj%tim_end-obj%tim_beg)/obj%dt) + 1

      if (abs((obj%tim_beg/obj%dt) - nint(obj%tim_beg/obj%dt)) < 0.01) then
        obj%tim_beg = obj%dt * nint(obj%tim_beg/obj%dt)
        obj%tim_end = obj%tim_beg + (obj%ndptnew-1)*obj%dt
      endif

      obj%ishift = nint ((obj%tim_beg-obj%tstrt)/obj%dt)

      obj%istrt = max (1, 1-obj%ishift)
      obj%istop = min (obj%ndptnew, obj%ndpt-obj%ishift)

      if (obj%tim_end < obj%tim_beg) then
        call pc_error ('TIM_END must be greater than or equal to TIM_BEG.')
      else if (obj%istop < obj%istrt) then
        call pc_error ('You must have some overlap between old and new &
                       &time ranges.')
      endif

!!------------------------------ write data cards ----------------------------!!
!!------------------------------ write data cards ----------------------------!!
!!------------------------------ write data cards ----------------------------!!

      call pc_put ('TBEG', obj%tstrt)
      call pc_put ('TEND', obj%tstrt + obj%dt*(obj%ndpt-1))

      call pc_put ('TIM_BEG', obj%tim_beg)
      call pc_put ('TIM_END', obj%tim_end)

      call pc_put_global ('TSTRT', obj%tim_beg)
      call pc_put_global ('NDPT',  obj%ndptnew)

      call pc_put_control ('PARALLEL_SAFE'        ,.true.)
      call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')

!!--------------------------- prepare for execution --------------------------!!
!!--------------------------- prepare for execution --------------------------!!
!!--------------------------- prepare for execution --------------------------!!

!<execute_only>
      if ( pc_do_not_process_traces() ) return
      obj%skip_wrapup = .false.
!</execute_only>

      return
      end subroutine tsel_update

!!------------------------------ main execution ------------------------------!!
!!------------------------------ main execution ------------------------------!!
!!------------------------------ main execution ------------------------------!!

!<execute_only>

      subroutine tsel (obj, ntr, hd, tr)
      implicit none
      type (tsel_struct),intent(inout) :: obj               ! arguments
      integer           ,intent(inout) :: ntr               ! arguments
      double precision  ,intent(inout) :: hd(:,:)           ! arguments
      real              ,intent(inout) :: tr(:,:)           ! arguments

      integer                          :: itrc              ! local

! Loop over traces passed in
      do itrc = 1,ntr

! Shift samples in overlap range
        tr(obj%istrt:obj%istop, itrc) &
                 = tr(obj%istrt+obj%ishift:obj%istop+obj%ishift, itrc)

! Zero the top of the trace
        tr(1:obj%istrt-1, itrc) = 0.0

! Zero the bottom of the trace
        tr(obj%istop+1:obj%ndptnew, itrc) = 0.0

! Adjust the bottom mute if it wasn't properly set before now.
        if (nint(hd(HDR_BOTTOM_MUTE,itrc)) < 1) &
          hd(HDR_BOTTOM_MUTE,itrc) = obj%ndpt

! Deal with traces whose mute/time selection combo makes them dead.
        call mutehw (hd(:,itrc), tr(:,itrc), obj%ndptnew, &
                     real(-obj%ishift), MUTEHW_SET)
        if (hd(HDR_TOP_MUTE,itrc) >= hd(HDR_BOTTOM_MUTE,itrc)) &
                     tr(1:obj%ndptnew, itrc) = 0.0

      end do

! Finish up by recomputing trace LAV values
      call lav_set_hdr (hd, tr, obj%ndptnew, ntr)

      if (ntr == NO_MORE_TRACES) call tsel_wrapup(obj)
      return
      end subroutine tsel

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine tsel_wrapup (obj)
      implicit none
      type(tsel_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print ('TSEL completed successfully')

      return
      end subroutine tsel_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module tsel_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
