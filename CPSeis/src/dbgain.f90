!<CPS_v1 type="PROCESS"/>
!!------------------------------- dbgain.f90 ---------------------------------!!
!!------------------------------- dbgain.f90 ---------------------------------!!
!!------------------------------- dbgain.f90 ---------------------------------!!

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
! Name       : DBGAIN
! Category   : amplitude_mod
! Written    : 1997-10-27   by: Donna Vunderink
! Revised    : 2000-12-07   by: Bob Baumel
! Maturity   : production   2001-06-11
! Purpose    : Apply an exponential (dB/s) gain to trace data.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! DBGAIN applies an exponential gain to traces.  The actual gain applied is
! given by:
!                  gain(t) = 10.0 ** (GAIN_RATE * t / 20.0),
!
! where the exponential gain parameter GAIN_RATE is expressed in dB/s and the
! time is referenced either to 0.0 (REF_TIM = ZERO) or the mute time
! (REF_TIM = MUTE).
!
! A positive value of GAIN_RATE results in an increasing gain as a function of
! time while a negative value of GAIN_RATE results in a decreasing gain as a
! function of time.
!
! The gain is applied to the trace over a window beginning at a time
! REF_TIM + TIM_ADD.  The length of the time window is WIN_LEN.  Unity gain is
! applied above the window and a constant gain is applied from the bottom of
! the window to the end of the trace.
!
! A plot of an increasing gain function follows.
!
!  gain
!      |                xxxxxxxxxxxxx
!      |               x
!      |              x
!      |             x
!      |            x
!      |           x
!      |          x
!      |         x
!      |_xxxxxxxx____________________ time
!        |<---->|<----->|
!        |   |      |
!        |   |     WIN_LEN
!        | TIM_ADD
!        |
!      REF_TIM
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! The definition of decibel measure, referenced to amplitude is:
!      difference (in dB)  =  20.0 * LOG10 (amplitude/reference amplitude);
! thus, an amplitude ratio of 2.0 corresponds to 6.0 dB (or 6.020599...).
!
! Typical values for GAIN_RATE in seismic processing applications range from
! 2.0 to 6.0 dB/s.
!
! Handling of TOP_MUTE and BOTTOM_MUTE headers:
! When REF_TIM = MUTE, the gain window is defined relative to the TOP_MUTE
! header word. For either REF_TIM = MUTE or REF_TIM = ZERO, the gain window
! will not extend below the BOTTOM_MUTE time; i.e., applied gain is always
! constant below the BOTTOM_MUTE time.
!
! Relationship to TPOW process:
! When REF_TIM = ZERO, the effect of DBGAIN is nearly identical to that of
! TPOW if, in the TPOW process, one chooses PWR = 0.0 and BETA is selected as:
!               BETA  =  GAIN_RATE * LN(10.0) / 20.0
! where "LN" denotes natural (base e) logarithm. The only differences are in
! handling of mute header words (which TPOW ignores), and an overall scale
! factor when the top of the gain window is not at time zero.
!
! Reversibility:
! The effect of DBGAIN can be undone by running another DBGAIN with reversed
! sign of GAIN_RATE, assuming that nothing has been done in the meantime to
! alter the values of the mute header words.
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 2       Head mute                  Used but not changed
! 25      LAV                        Set
! 64      Tail mute                  Used but not changed
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                       REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 8.  2001-06-11  Baumel       Change wrapped_up flag to skip_wrapup.
!                              PRODUCTION.
! 7.  2000-07-25  Baumel       Add gui_def to doc (no changes in code).
! 6.  2000-03-21  Baumel       Fix initialization of WIN_LEN parameter.
! 5.  2000-03-20  Baumel       Clean up code, increase efficiency, add
!                              option field for CFE.
! 4.  1999-10-27  Sharp        Removed calls to pc_get and pc_put from main
!                              processing routine
! 3.  1999-10-11  Sharp        Convert to new system using F90
! 2.  1998-11-13  Vunderink    Begin using the f90 compiler.
! 1.  1997-10-27  Vunderink    Original version.
!
!------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>

!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS DBGAIN Process/NC=80>
!
! Apply an exponential (dB/s) gain to trace data.
!
! GAIN_RATE=`FFFFFFFFFFF    REF_TIM=`CCCC    
!
! TIM_ADD=~~`FFFFFFFFFFF    WIN_LEN=`FFFFFFFFFFFF
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="GAIN_RATE">
!<Tip> Exponential gain parameter, in dB/s. </Tip>
! Default = 6.0
! Allowed = real
! Exponential gain will be applied to the trace, where GAIN_RATE is the rate of
! change of trace gain, in dB/s.
!</Help>
!
!<Help KEYWORD="REF_TIM">
!<Tip> Whether the reference time is zero time or mute time. </Tip>
! Default = MUTE
! Allowed = MUTE  (Reference time is mute time.)
! Allowed = ZERO  (Reference time is zero time.)
! REF_TIM has two functions:  1. The reference time for the gain calculation
! and 2. The reference time for the top of the window for applying the gain.
!
! The exponential gain is applied starting at REF_TIM + TIM_ADD.  Note that if
! OPT_TIM = ZERO, REF_TIM is zero time which will not be the top of the trace
! if TSTRT /= zero.
!</Help>
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Exponential gain starts at REF_TIM + TIM_ADD. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the time window over which exponential gain is applied.</Tip>
!Default = len tr
!Allowed = real > 0.0
!Length of the time window, in seconds, over which exponential gain is applied.
!A constant gain is applied from the bottom of the window to the end of the
! trace.
!</Help>
!
!</HelpSection>

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

    module dbgain_module
      use pc_module
      use named_constants_module
      use mem_module
      use mutehw_module
      use lav_module
      implicit none
      private
      public :: dbgain_create     ! uses the parameter cache.
      public :: dbgain_initialize
      public :: dbgain_update     ! uses the parameter cache.
      public :: dbgain_delete
!<execute_only>
      public :: dbgain         ! main execution (trace processing) routine.
      public :: dbgain_wrapup
!</execute_only>

      character(len=100),public,save :: DBGAIN_IDENT = &
'$Id: dbgain.f90,v 1.8 2001/06/07 14:30:42 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    type,public :: dbgain_struct
       private

       logical          :: skip_wrapup    ! wrapup flag.

       real             :: gain_rate      ! process parameter
       character(len=4) :: ref_tim        ! process parameter
       real             :: tim_add        ! process parameter
       real             :: win_len        ! process parameter
!
       integer          :: ndpt           ! globals
       real             :: dt             ! globals
       real             :: tstrt          ! globals
!
       integer          :: i_tim_add
       integer          :: i_win_len
       real, pointer    :: gainfunc(:)

     end type dbgain_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(dbgain_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine dbgain_create (obj)
       implicit none
       type(dbgain_struct),pointer :: obj       ! arguments
!
       allocate (obj)
!
       nullify (obj%gainfunc)
!
       call dbgain_initialize (obj)
       return
      end subroutine dbgain_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine dbgain_delete (obj)
      implicit none
      type(dbgain_struct),pointer :: obj

!<execute_only>
      call dbgain_wrapup (obj)
!</execute_only>

      call mem_free (obj%gainfunc)
!
      deallocate(obj)
      return
      end subroutine dbgain_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine dbgain_initialize (obj)
      implicit none
      type(dbgain_struct),intent(inout) :: obj       ! arguments
!
       obj%gain_rate =  6.
       obj%ref_tim   = 'MUTE'
       obj%tim_add   =  0.
       obj%ndpt      = -1
       obj%dt        = -1.
       call pc_get_global ('NDPT', obj%ndpt)
       call pc_get_global ('DT', obj%dt)
       if (obj%ndpt>0 .and. obj%dt>0.) then
         obj%win_len = (obj%ndpt-1) * obj%dt
       else
         obj%win_len = fnil
       end if
!
      call dbgain_update (obj)
      return
      end subroutine dbgain_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

   subroutine dbgain_update (obj)
   implicit none
   type(dbgain_struct),intent(inout),target :: obj             ! arguments

   integer :: nstore, nscratch, status, i
   real    :: factor

   object => obj              ! needed for traps.
   obj%skip_wrapup = .true.   ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

   obj%ndpt  = -1
   obj%dt    = -1.
   obj%tstrt = fnil
   call pc_get_global ('NDPT' , obj%ndpt)  ! number of trace samples.
   call pc_get_global ('DT'   , obj%dt)    ! trace sample interval (s).
   call pc_get_global ('TSTRT', obj%tstrt) ! time of 1st trace sample (s).
   if (obj%ndpt <= 0) then
     call pc_error ("NDPT global hasn't been set.")
     obj%ndpt = 1
   end if
   if (obj%dt <= 0.) then
     call pc_error ("DT global hasn't been set.")
     obj%dt = 1.
   end if
   if (obj%tstrt == fnil) then
     call pc_error ("TSTRT global hasn't been set.")
     obj%tstrt = 0.
   end if

   call pc_get ('GAIN_RATE', obj%gain_rate)
   call pc_get ('REF_TIM'  , obj%ref_tim)
   call pc_get ('TIM_ADD'  , obj%tim_add)
   call pc_get ('WIN_LEN'  , obj%win_len)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

   if (obj%ref_tim(1:1)=='Z' .or. obj%ref_tim(1:1)=='z') then
     obj%ref_tim = 'ZERO'
   else if (obj%ref_tim(1:1)=='M' .or. obj%ref_tim(1:1)=='m') then
     obj%ref_tim = 'MUTE'
   else
     call pc_error ('REF_TIM must be either MUTE or ZERO.')
   end if

   if (obj%ref_tim == 'ZERO') then
     obj%i_tim_add = nint((obj%tim_add - obj%tstrt)/obj%dt)  +  1
     obj%i_tim_add = min ( max(obj%i_tim_add, 1) , obj%ndpt )
     obj%tim_add   = obj%tstrt + (obj%i_tim_add - 1)*obj%dt
   else
     obj%i_tim_add = max ( nint(obj%tim_add/obj%dt) , 0 )
     obj%tim_add   = obj%i_tim_add * obj%dt
   end if

   obj%i_win_len = max ( nint(obj%win_len/obj%dt) , 0 )
   obj%win_len   = obj%i_win_len * obj%dt
   if (obj%i_win_len == 0) then
     call pc_error ('WIN_LEN must be positive.')
   end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

   call pc_put_options_field ('REF_TIM', (/ 'MUTE', 'ZERO' /), 2)

   call pc_put ('GAIN_RATE', obj%gain_rate)
   call pc_put ('REF_TIM'  , obj%ref_tim)
   call pc_put ('TIM_ADD'  , obj%tim_add)
   call pc_put ('WIN_LEN'  , obj%win_len)

   nscratch = 0
   nstore   = obj%i_win_len + 1
   call pc_put_control ('NSCRATCH', nscratch)
   call pc_put_control ('NSTORE',   nstore)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

   call mem_free (obj%gainfunc)

!<execute_only>
   if (pc_do_not_process_traces()) return

     obj%skip_wrapup = .false.    ! needed for the wrapup routine.

     call mem_alloc (obj%gainfunc, obj%i_win_len + 1, status=status)
     if (status /= 0) then
       obj%skip_wrapup = .true.
       return
     end if

     factor = log(10.) * obj%gain_rate * obj%dt / 20.
     do i = 1, obj%i_win_len + 1
       obj%gainfunc(i) = exp (factor*(i-1))
     end do

!</execute_only>

     return
   end subroutine dbgain_update

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

    subroutine dbgain (obj,ntr,hd,tr)
      implicit none
      type(dbgain_struct),intent(inout) :: obj                  ! arguments
      integer            ,intent(inout) :: ntr                  ! arguments
      double precision   ,intent(inout) :: hd(:,:)              ! arguments
      real               ,intent(inout) :: tr(:,:)              ! arguments

      integer j, imute, i1, i2

      if (ntr == NO_MORE_TRACES) then
        call dbgain_wrapup(obj)
        return
      end if

      do j = 1, ntr

        if (hd(HDR_LAV,j) == 0.) cycle             ! Dead trace

!       Make sure mute headers in proper range:
        call mutehw (hd(:,j), tr(:,j), obj%ndpt, 0.0, MUTEHW_SET)

        if (obj%ref_tim == 'MUTE') then
          imute = nint(hd(HDR_TOP_MUTE,j))
          i1 = imute + obj%i_tim_add
          if (i1 > obj%ndpt) cycle
        else
          i1 = obj%i_tim_add
        end if

        i2 = min ( i1 + obj%i_win_len , obj%ndpt ,  &
                                          nint(hd(HDR_BOTTOM_MUTE,j)) )
        if (i2 < i1) cycle

        tr(i1:i2, j) = tr(i1:i2, j) * obj%gainfunc(1:i2-i1+1)

        if (i2 < obj%ndpt) then
          tr(i2+1:obj%ndpt, j) = tr(i2+1:obj%ndpt, j)  &
                                    * obj%gainfunc(i2-i1+1)
        end if

      end do

      call lav_set_hdr (hd, tr, obj%ndpt, ntr)

      return
    end subroutine dbgain

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine dbgain_wrapup (obj)
       implicit none
       type(dbgain_struct),intent(inout) :: obj       ! arguments
!
       if (obj%skip_wrapup) return
       obj%skip_wrapup = .true.
!
       return
      end subroutine dbgain_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module dbgain_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
