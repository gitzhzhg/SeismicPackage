!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- xputil.f90 --------------------------------!!
!!------------------------------- xputil.f90 --------------------------------!!
!!------------------------------- xputil.f90 --------------------------------!!

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
! Name       : XPUTIL                     (XP utilities)
! Category   : amplitude_mod
! Written    : 2003-07-21   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Utilities used by both XP and MVXP.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive contains the XP and MVXP trace gain algorithm, including
! the required process parameters, gui_def, and HelpSection which are common
! to both XP and MVXP.
!
! See the XP and MVXP documentation, and the HelpSection below, for additional
! information.
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
!                                      o      i
!             call xputil_create     (obj, procname)
!
!                                      b
!             call xputil_initialize (obj)
!             call xputil_update     (obj)
!             call xputil_delete     (obj)
!
!                                      b   b   b     o
!             call xputil_execute    (obj, hd, tr, whoops)
!
! type(xputil_struct)      obj = pointer to the XPUTIL structure.
! integer             procname = process identification.
! double precision       hd(:) = trace header words.
! real                   tr(:) = trace values.
! logical               whoops = true if an error occurred.
!
! The above routines should be called from the similarly-named routines
! in processes XP or MVXP.
!
! PROCNAME must be set to one of the following named constants:
!             XPUTIL_XP_PROCESS    XPUTIL_MVXP_PROCESS
!
! PROCNAME is used to set different parameter defaults for XP and MVXP.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!007. 2006-06-20  B. Menger   Removed Unused Variables.
!  6. 2005-04-11  Stoeckley  Change so will print message about number of
!                             traces with bad headers when removing previous
!                             gain, rather than to abort in such cases.
!  5. 2003-08-11  Stoeckley  Fixed a bug with custom windows, introduced
!                             in the initial version of this primitive.
!  4. 2003-07-24  Stoeckley  Fixed a remove-gain bug found by Bob Olson; allow
!                             debrightening when applying a removable gain.
!  3. 2003-07-22  Stoeckley  Fixed two bugs (one found by Bob Olsen).
!  2. 2003-07-23  Goodger    Change category to amplitude_mod
!  1. 2003-07-21  Stoeckley  Initial version, made from redundancies in XP
!                             and MVXP, with additional parameters for removing
!                             a previously-applied gain.
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


!-------------------------------------------------------------------------------
!<gui_def>
! GAIN_MODE=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCC   HDR_FIRST=`II   NWIH_INPUT =`XX
!                                            HDR_LAST =`II   NWIH_OUTPUT=`XX
!
! START_VAL=`FFFFFFFFFFF   TIM_LAST=`FFFFFFFFFFF   GAIN_MAX=`FFFFFFFFFFF
!
!      `--------------------------------------------------------
!                          WINDOWS=`CCCCCC
!
!          WIN_LEN=`FFFFFFFFFFF    TIM_WIN     TIM_LEN
!          WIN_INC=`FFFFFFFFFFF    `FFFFFFFFFFF`FFFFFFFFFFF
!                                  `FFFFFFFFFFF`FFFFFFFFFFF
!                                  `FFFFFFFFFFF`FFFFFFFFFFF
!                                  `FFFFFFFFFFF`FFFFFFFFFFF
!                                  `FFFFFFFFFFF`FFFFFFFFFFF
!      `--------------------------------------------------------
!
!      `--------------------------------------------------------
!                       Debrighten Parameters
!
!                            DEBRI=`CC
!
!        DEBRI_MAX=~~~~`FFFFFFFFFFF  DEBRI_TPR=~~~~`FFFFFFFFFF
!        DEBRI_TIM_BEG=`FFFFFFFFFFF  DEBRI_TIM_END=`FFFFFFFFFF
!      `--------------------------------------------------------
!<PARMS TIM_WIN_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="GAIN_MODE">
!<tip> Whether to apply or remove the gain. </tip>
!  Default = "APPLY PERMANENT GAIN"
!  Allowed = "APPLY PERMANENT GAIN"
!  Allowed = "APPLY REMOVABLE GAIN"
!  Allowed = "REMOVE PREVIOUS GAIN"
!
!  When applying a gain which is removable, the gain parameters are stored
!  in user-defined header words tacked onto the end of the trace headers.
!  These user-defined header words are then deleted when the gain is removed.
!  When the gain is removed, all process parameters except GAIN_MODE, HDR_FIRST,
!  and HDR_LAST are irrelevant.
!
!  If DEBRI is YES when applying a removable gain, the debrightening operation
!  is permanent (i.e. when removing the gain later, the debrightened portions
!  of the trace will remain debrightened).
!
!  If traces with bad gain headers are encountered while removing previous
!  gain, the trace is killed.  A report is printed telling how many such
!  traces were killed.  Usually this means that this trace did not exist
!  yet when the original gain was applied.  This might also mean that the
!  trace was dead when the previous gain was applied, but was live when
!  removing previous gain.
!</Help>
!
!<Help KEYWORD="HDR_FIRST">
!<tip> First user-defined header word for storing gain information. </tip>
!  Default = NWIH+1  (set automatically when applying a removable gain)
!  Allowed = NWIH+1  (set automatically when applying a removable gain)
!
!  Additional header words from HDR_FIRST to HDR_LAST are created when
!  applying a removable gain.  These additional headers are then deleted
!  when removing the gain (unless other additional headers were created
!  for other purposes in the interim).
!
!  The number of additional header words required is 1 + 2*NUMWIN, where
!  NUMWIN is the number of trace expansion windows used.
!
!  HDR_FIRST and HDR_LAST are set automatically when applying a removable
!  gain, and must be set by the user to the same values when removing the
!  previously-applied gain.
!
!  HDR_FIRST and HDR_LAST are set automatically to zero (and ignored) when
!  applying a permanent gain.
!</Help>
!
!<Help KEYWORD="HDR_LAST">
!<tip> Last user-defined header word for storing gain information. </tip>
!  Default = NWIH+1+2*NUMWIN (set automatically when applying a removable gain)
!  Allowed = NWIH+1+2*NUMWIN (set automatically when applying a removable gain)
!
!  Additional header words from HDR_FIRST to HDR_LAST are created when
!  applying a removable gain.  These additional headers are then deleted
!  when removing the gain (unless other additional headers were created
!  for other purposes in the interim).
!
!  The number of additional header words required is 1 + 2*NUMWIN, where
!  NUMWIN is the number of trace expansion windows used.
!
!  HDR_FIRST and HDR_LAST are set automatically when applying a removable
!  gain, and must be set by the user to the same values when removing the
!  previously-applied gain.
!
!  HDR_FIRST and HDR_LAST are set automatically to zero (and ignored) when
!  applying a permanent gain.
!</Help>
!
!
!<Help KEYWORD="NWIH_INPUT">
!<tip> Number of trace header words entering this process. </tip>
!</Help>
!
!<Help KEYWORD="NWIH_OUTPUT">
!<tip> Number of trace header words exiting this process. </tip>
!</Help>
!
!
!<Help KEYWORD="WINDOWS">
!<tip> Selects one constant window or multiple time varying windows. </tip>
!  Default = "REGULAR"
!  Allowed = "REGULAR" one constant window specified by WIN_LEN and WIN_INC.
!  Allowed = "CUSTOM" multiple, time varying windows via TIM_WIN and TIM_LEN.
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<tip> Window length, in seconds. </tip>
!  Default = 0.5             (for XP)
!  Default = 1.0             (for MVXP)
!  Allowed = real>10*DT
!
!  Window length in seconds.
!  This parameter is used only when WINDOWS is "REGULAR".
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<tip> Time increment for window locations, in seconds. </tip>
!  Default = 0.5             (for XP)
!  Default = 0.25            (for MVXP)
!  Allowed = real>=DT
!
!  Time increment for window locations in seconds.
!  This parameter is used only when WINDOWS is "REGULAR".
!
!  This parameter is ignored when removing a previous gain.
!</Help>

!<Help KEYWORD="START_VAL">
!<tip> Amplitude threshold parameter for start of first window. </tip>
!  Default = 0.0
!  Allowed = 0<=real<1
!
!  Start window at the first sample whose absolute amplitude exceeds
!  START_VAL * trace LAV.  If START_VAL = 0.0 then start at mute time.
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="TIM_LAST">
!<tip> Last time to use in calculating gain function. </tip>
!  Default = tr_end
!  Allowed = real>TSTRT
!
!  The entire trace is balanced, although the gain calculation ignores trace
!  values beyond TIM_LAST.
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="GAIN_MAX">
!<tip> Maximum gain to allow. </tip>
!  Default = 1.0e30
!  Allowed = real>0.0
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="DEBRI">
!<tip> Option whether to perform debrighten. </tip>
!  Default = YES             (for XP)
!  Default = NO              (for MVXP)
!  Allowed = YES/NO
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="DEBRI_MAX">
!<tip> Debrighten threshold. </tip>
!  Default = 4.4186          (for XP)
!  Default = 10.0            (for MVXP)
!  Allowed = real>1.0
!
!  Samples exceeding this absolute amplitude after amplitude balance are
!  debrightened.
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="DEBRI_TPR">
!<tip> Width in seconds for each side of the debrighten taper. </tip>
!  Default = 0.032
!  Allowed = real>0.0
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="DEBRI_TIM_BEG">
!<tip> Time in seconds to begin debrighten operation. </tip>
!  Default = TSTRT
!  Allowed = real >= TSTRT
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="DEBRI_TIM_END">
!<tip> Time in seconds to end debrighten operation. </tip>
!  Default = trace end time
!  Allowed = DEBRI_TIM_BEG < real <= trace end time
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="TIM_WIN">
!<tip> Array of window start times (may be time varying). </tip>
!  Default = 0.5
!  Allowed = real>0.0
!
!  Linked array of window start times and window lengths, in seconds.
!  TIM_WIN values are measured from the mute time.
!  This parameter is used only when WINDOWS is "CUSTOM".
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!<Help KEYWORD="TIM_LEN">
!<tip> Array of window lengths (may be time varying). </tip>
!  Default = 0.5
!  Allowed = real>DT
!
!  Linked array of window start times and window lengths in seconds.
!  TIM_WIN values are measured from the mute time.
!  This parameter is used only when WINDOWS is "CUSTOM".
!
!  This parameter is ignored when removing a previous gain.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module xputil_module
      use pc_module
      use named_constants_module
      use string_module
      use lav_module
      use dbritr_module
      use median_module
      use mth_module
      use mutehw_module
      implicit none
      private
      public :: xputil_create
      public :: xputil_delete
      public :: xputil_initialize
      public :: xputil_update
      public :: xputil_execute

      character(len=100),public,save :: XPUTIL_IDENT = &
'$Id: xputil.f90,v 1.7 2006/06/20 13:12:14 Menger prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: xputil_struct              
        private

        integer            :: nwih_input             ! global parameter
        integer            :: nwih_output            ! global parameter
        integer            :: ndpt                   ! global parameter
        real               :: dt                     ! global parameter
        real               :: tstrt                  ! global parameter

        character(len=30)  :: gain_mode              ! process parameter
        integer            :: hdr_first              ! process parameter
        integer            :: hdr_last               ! process parameter
        character(len=7)   :: windows                ! process parameter
        real               :: win_len                ! process parameter
        real               :: win_inc                ! process parameter
        real               :: start_val              ! process parameter
        real               :: tim_last               ! process parameter
        real               :: gain_max               ! process parameter
        logical            :: debri                  ! process parameter
        real               :: debri_max              ! process parameter
        real               :: debri_tpr              ! process parameter
        real               :: debri_tim_beg          ! process parameter
        real               :: debri_tim_end          ! process parameter
        real,pointer       :: tim_win(:)             ! process parameter
        real,pointer       :: tim_len(:)             ! process parameter
        integer            :: tim_win_cnt            ! process parameter

        integer            :: procname               ! passed argument

        integer            :: pwr_mode_flag          ! internal variable
        integer            :: gain_mode_flag         ! internal variable
        integer            :: windows_flag           ! internal variable
        integer            :: buf_samp_max           ! internal variable
        integer            :: max_win_cnt            ! internal variable
        integer            :: win_len_cnt            ! internal variable
        integer            :: win_inc_cnt            ! internal variable
        integer            :: tim_last_idx           ! internal variable
        integer            :: debri_tpr_cnt          ! internal variable
        integer            :: half_win_cnt           ! internal variable
        integer            :: last_win_start_idx     ! internal variable
        integer            :: debri_beg_idx          ! internal variable
        integer            :: debri_pt_cnt           ! internal variable
        real   ,pointer    :: median_buf  (:)        ! internal variable
        integer,pointer    :: win_beg_idx (:)        ! internal variable
        integer,pointer    :: win_samp_cnt(:)        ! internal variable
        real   ,pointer    :: agc_factor  (:)        ! internal variable
        integer            :: ecount                 ! internal variable
        character(len=30)  :: prefix                 ! internal variable

      end type xputil_struct


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      integer,parameter,public  :: XPUTIL_XP_PROCESS    = 1   ! procname
      integer,parameter,public  :: XPUTIL_MVXP_PROCESS  = 2   ! procname

      integer,parameter,private :: AVERAGE_POWER        = 1   ! pwr_mode_flag
      integer,parameter,private :: MEDIAN_POWER         = 2   ! pwr_mode_flag

      integer,parameter,private :: APPLY_PERMANENT_GAIN = 1   ! gain_mode_flag
      integer,parameter,private :: APPLY_REMOVABLE_GAIN = 2   ! gain_mode_flag
      integer,parameter,private :: REMOVE_PREVIOUS_GAIN = 3   ! gain_mode_flag

      integer,parameter,private :: REGULAR_WINDOWS      = 1   ! windows_flag
      integer,parameter,private :: CUSTOM_WINDOWS       = 2   ! windows_flag

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine xputil_create (obj,procname)
      type(xputil_struct),pointer    :: obj                 ! arguments
      integer            ,intent(in) :: procname            ! arguments

      allocate (obj)

      obj%procname = procname
      obj%ecount   = 0

      if (obj%procname == XPUTIL_XP_PROCESS) then
           obj%prefix = 'XP:'
      else
           obj%prefix = 'MVXP:'
      end if

      nullify (obj%tim_win)                           ! process parameter
      nullify (obj%tim_len)                           ! process parameter
      nullify (obj%median_buf)                        ! dependent parameter
      nullify (obj%win_beg_idx)                       ! dependent parameter
      nullify (obj%win_samp_cnt)                      ! dependent parameter
      nullify (obj%agc_factor)                        ! dependent parameter

      end subroutine xputil_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine xputil_delete (obj)
      type(xputil_struct),pointer :: obj                       ! arguments

      if (associated(obj%tim_win     )) deallocate (obj%tim_win)
      if (associated(obj%tim_len     )) deallocate (obj%tim_len)
      if (associated(obj%median_buf  )) deallocate (obj%median_buf)
      if (associated(obj%win_beg_idx )) deallocate (obj%win_beg_idx)
      if (associated(obj%win_samp_cnt)) deallocate (obj%win_samp_cnt)
      if (associated(obj%agc_factor  )) deallocate (obj%agc_factor)

      if (obj%ecount > 0) then
           call pc_print (' ')
           call pc_print (obj%prefix,obj%ecount,'traces were killed because &
                          &of bad headers when removing previous gain.')
           call pc_print (obj%prefix,'This might mean that this trace did not &
                          &exist yet when the original gain was applied.')
           call pc_print (obj%prefix,'This might also mean that the trace was &
                          &dead when the previous gain was applied, but became')
           call pc_print (obj%prefix,'live before removing previous gain.')
           call pc_print (' ')
      end if

      deallocate(obj)
      end subroutine xputil_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine xputil_initialize (obj)
      type(xputil_struct),intent(inout) :: obj                 ! arguments

      call pc_get_global ('ndpt' , obj%ndpt)
      call pc_get_global ('dt'   , obj%dt)
      call pc_get_global ('tstrt', obj%tstrt)

      obj%gain_mode     = 'APPLY PERMANENT GAIN'
      obj%hdr_first     = 0
      obj%hdr_last      = 0
      obj%windows       = 'REGULAR'
      obj%start_val     = 0.0
      obj%tim_last      = obj%tstrt + obj%dt * (obj%ndpt - 1)
      obj%gain_max      = 1.0e30
      obj%debri_tpr     = 0.032
      obj%debri_tim_beg = obj%tstrt
      obj%debri_tim_end = obj%tim_last
      obj%tim_win_cnt   = 0

      if (obj%procname == XPUTIL_XP_PROCESS) then
           obj%pwr_mode_flag = AVERAGE_POWER
           obj%win_len       = 0.5
           obj%win_inc       = 0.5
           obj%debri         = .true.
           obj%debri_max     = 4.4186
      else
           obj%pwr_mode_flag = MEDIAN_POWER
           obj%win_len       = 1.0
           obj%win_inc       = 0.25
           obj%debri         = .false.
           obj%debri_max     = 10.0
      end if

      end subroutine xputil_initialize


!!------------------------ start of update -------------------------------!!
!!------------------------ start of update -------------------------------!!
!!------------------------ start of update -------------------------------!!


      subroutine xputil_update (obj)
      type(xputil_struct),intent(inout) :: obj                 ! arguments
      integer                           :: temp_hdr_first      ! local
      integer                           :: temp_hdr_last       ! local
      integer                           :: ier1,ier2,ier3,ier4 ! local
      logical                           :: removing            ! local
      logical                           :: debrighten          ! local
      logical                           :: regular             ! local
      logical                           :: custom              ! local
      integer                           :: win_cnt_do,idx      ! local
      integer                           :: nstore,nheaders     ! local


!!----------------------- read parameters ---------------------------------!!
!!----------------------- read parameters ---------------------------------!!
!!----------------------- read parameters ---------------------------------!!


  call pc_register_array_names ('tim_win_arrayset', (/'tim_win', 'tim_len'/))

      call pc_get_global ('nwih' , obj%nwih_input)
      call pc_get_global ('ndpt' , obj%ndpt)
      call pc_get_global ('dt'   , obj%dt)
      call pc_get_global ('tstrt', obj%tstrt)

      call pc_get   ('gain_mode'    , obj%gain_mode)
      call pc_get   ('hdr_first'    , obj%hdr_first)
      call pc_get   ('hdr_last'     , obj%hdr_last)
      call pc_get   ('windows'      , obj%windows)
      call pc_get   ('win_len'      , obj%win_len)
      call pc_get   ('win_inc'      , obj%win_inc)
      call pc_get   ('start_val'    , obj%start_val)
      call pc_get   ('tim_last'     , obj%tim_last)
      call pc_get   ('gain_max'     , obj%gain_max)
      call pc_get   ('debri'        , obj%debri)
      call pc_get   ('debri_max'    , obj%debri_max)
      call pc_get   ('debri_tpr'    , obj%debri_tpr)
      call pc_get   ('debri_tim_beg', obj%debri_tim_beg)
      call pc_get   ('debri_tim_end', obj%debri_tim_end)
      call pc_alloc ('tim_win'      , obj%tim_win, obj%tim_win_cnt)
      call pc_alloc ('tim_len'      , obj%tim_len, obj%tim_win_cnt)

      call string_to_upper (obj%gain_mode)
      call string_to_upper (obj%windows)


!!----------------------- verify parameters ---------------------------------!!
!!----------------------- verify parameters ---------------------------------!!
!!----------------------- verify parameters ---------------------------------!!


      select case (obj%windows)
        case ("REGULAR") ; obj%windows_flag = REGULAR_WINDOWS
        case ("CUSTOM" ) ; obj%windows_flag = CUSTOM_WINDOWS
        case default     ; obj%windows_flag = REGULAR_WINDOWS
                           call pc_error ('invalid WINDOWS parameter')
      end select

      obj%tim_last_idx = MIN(NINT((obj%tim_last - obj%tstrt)/obj%dt), &
        obj%ndpt - 1)

      IF (obj%tim_last_idx < 11) then
        call pc_warning('TIM_LAST constraint implies WIN_LEN < 10*DT')
        obj%tim_last_idx = obj%ndpt - 1
      end if

      if(0 /= mth_compare(obj%tim_last, &
        obj%tstrt + obj%tim_last_idx * obj%dt)) then
        call pc_warning('Rounding TIM_LAST to nearest DT')
        obj%tim_last = obj%tstrt + obj%tim_last_idx * obj%dt
      end if

      win_choice: if(obj%windows_flag == REGULAR_WINDOWS) then
        ! use WIN_LEN and WIN_INC (not TIM_WIN and TIM_LEN arrays)

        obj%win_len_cnt = NINT(obj%win_len/obj%dt)

        if(obj%win_len_cnt < 11) then
          call pc_error('WIN_LEN must be greater than 10*DT')
          obj%win_len_cnt = 11
        end if

        if(0 /= mth_compare(obj%win_len, obj%win_len_cnt * obj%dt)) then
          call pc_warning('Rounding WIN_LEN to nearest DT')
          obj%win_len = obj%win_len_cnt * obj%dt
        end if

        obj%win_len_cnt = MIN(obj%win_len_cnt, obj%tim_last_idx)

        obj%half_win_cnt = (obj%win_len_cnt - 1)/2
        obj%last_win_start_idx = obj%tim_last_idx - obj%win_len_cnt + 1

        if(obj%win_inc < obj%dt) then
          call pc_error('WIN_INC must be greater than or equal to DT')
          obj%win_inc = max(0.5, obj%dt)
        end if

        obj%win_inc_cnt = MAX(NINT(obj%win_inc/obj%dt),1)

        if(0 /= mth_compare(obj%win_inc, obj%win_inc_cnt * obj%dt)) then
          call pc_warning('Rounding WIN_INC to nearest DT')
          obj%win_inc = obj%win_inc_cnt * obj%dt
        end if

        obj%max_win_cnt = (obj%last_win_start_idx + obj%win_inc_cnt - 2) / &
          obj%win_inc_cnt + 1

        obj%buf_samp_max = obj%win_len_cnt
      else ! win_choice
        ! use TIM_WIN and TIM_LEN arrays (not WIN_LEN and WIN_INC)

        if (pc_verify_end()) then

          if(obj%tim_win_cnt == 0) then
            call pc_error('one or more TIM_WIN/TIM_LEN pairs required')
          end if

          do win_cnt_do = 1, obj%tim_win_cnt
            if(obj%tim_win(win_cnt_do) < 0.0 .or. &
              obj%tim_win(win_cnt_do) == FNIL) then
              call pc_error( &
                'TIM_WIN one or more values undefined or less than 0.0')
              exit
            else
              idx = nint(obj%tim_win(win_cnt_do)/obj%dt)

              if(0 /= mth_compare(obj%tim_win(win_cnt_do), idx*obj%dt)) then
                call pc_warning('Rounding TIM_WIN(', win_cnt_do, &
                  ') to nearest DT')
                obj%tim_win(win_cnt_do) = idx*obj%dt
              end if
            end if

            if(obj%tim_len(win_cnt_do) < obj%dt .or. &
              obj%tim_len(win_cnt_do) == FNIL) then
              call pc_error( &
                'TIM_LEN one or more values undefined or less than DT')
              exit
            else
              idx = nint(obj%tim_len(win_cnt_do)/obj%dt)

              if(0 /= mth_compare(obj%tim_len(win_cnt_do), idx*obj%dt)) then
                call pc_warning('Rounding TIM_LEN(', win_cnt_do, &
                  ') to nearest DT')
                obj%tim_len(win_cnt_do) = idx*obj%dt
              end if
            end if
          end do
        end if

        obj%max_win_cnt = obj%tim_win_cnt

        obj%buf_samp_max = obj%ndpt
      end if win_choice

      nstore = obj%buf_samp_max + 3*obj%max_win_cnt

      if(obj%start_val < 0.0 .or. obj%start_val >= 1.0) then
        call pc_error('START_VAL must be >= 0.0 and < 1.0 - reset to 0')
        obj%start_val = 0.0
      end if

      if(obj%gain_max <= 0.0) then
        call pc_error('GAIN_MAX must be greater than 0.0 - reset to default')
        obj%gain_max = 1.0e30
      end if

      if(obj%debri_max <= 0.0) then
        call pc_error('DEBRI_MAX must be greater than 1.0 - reset to default')
        if (obj%procname == XPUTIL_XP_PROCESS) then
          obj%debri_max = 4.4186
        else
          obj%debri_max = 10.0
        end if
      end if

      if(obj%debri_tpr <= 0.0) then
        call pc_error('DEBRI_TPR must be greater than 0.0 - reset to default')
        obj%debri_tpr = 0.032
      end if

      IF (obj%debri) THEN
        obj%debri_tpr_cnt = MAX(NINT(obj%debri_tpr/obj%dt),1)

        if(0 /= mth_compare(obj%debri_tpr, obj%debri_tpr_cnt*obj%dt)) then
          call pc_warning('Rounding DEBRI_TPR to nearest DT')
          obj%debri_tpr = obj%debri_tpr_cnt*obj%dt
        end if

        if(obj%debri_tim_beg < obj%tstrt) then
          call pc_error('DEBRI_TIM_BEG must be greater than or equal to TSTRT')
          obj%debri_tim_beg = obj%tstrt
        end if

        ! 1999-09-17 debri_beg_idx latent bug fixed (SELZLER)
        obj%debri_beg_idx = NINT((obj%debri_tim_beg - obj%tstrt)/obj%dt + 1)

        if(0 /= mth_compare(obj%debri_tim_beg, &
          obj%tstrt + (obj%debri_beg_idx-1)*obj%dt)) then
          call pc_warning('Rounding DEBRI_TIM_BEG to nearest DT')
          obj%debri_tim_beg = obj%tstrt + (obj%debri_beg_idx-1)*obj%dt
        end if

        obj%debri_pt_cnt = NINT((obj%debri_tim_end - obj%debri_tim_beg) / &
          obj%dt) + 1

        if(obj%debri_pt_cnt < 1 .or. obj%debri_pt_cnt > obj%ndpt) then
          call pc_error('DEBRI_TIM_END must be > TSTRT and <= trace end time')
          obj%debri_tim_end = obj%tstrt + obj%dt * (obj%ndpt - 1)
        else if(0 /= mth_compare(obj%debri_tim_end, &
          obj%debri_tim_beg + (obj%debri_pt_cnt-1)*obj%dt)) then
          call pc_warning('Rounding DEBRI_TIM_END to nearest DT')
          obj%debri_tim_end = obj%debri_tim_beg + (obj%debri_pt_cnt-1)*obj%dt
        end if
      ENDIF

      select case (obj%gain_mode)
        case ("APPLY PERMANENT GAIN")
               obj%gain_mode_flag = APPLY_PERMANENT_GAIN
               temp_hdr_first     = 0
               temp_hdr_last      = 0
               obj%nwih_output    = obj%nwih_input
        case ("APPLY REMOVABLE GAIN")
               obj%gain_mode_flag = APPLY_REMOVABLE_GAIN
               nheaders           = xputil_gain_headers_needed(obj%max_win_cnt)
               temp_hdr_first     = obj%nwih_input + 1
               temp_hdr_last      = obj%nwih_input + nheaders
               obj%nwih_output    = obj%nwih_input + nheaders
        case ("REMOVE PREVIOUS GAIN")
               obj%gain_mode_flag = REMOVE_PREVIOUS_GAIN
               temp_hdr_first     = obj%hdr_first
               temp_hdr_last      = obj%hdr_last
               if (obj%hdr_last == obj%nwih_input) then
                    obj%nwih_output = obj%hdr_first - 1
               else
                    obj%nwih_output = obj%nwih_input
               end if
               nheaders        = obj%hdr_last - obj%hdr_first + 1
               obj%max_win_cnt = xputil_gain_windows_available(nheaders)
        case default
               obj%gain_mode_flag = APPLY_PERMANENT_GAIN
               temp_hdr_first     = 0
               temp_hdr_last      = 0
               obj%nwih_output    = obj%nwih_input
               call pc_error ('invalid GAIN_MODE parameter')
      end select

      if (pc_verify_end()) then
  !     if (obj%debri .and. obj%gain_mode_flag == APPLY_REMOVABLE_GAIN) then
  !       call pc_error ('DEBRI cannot be YES when applying a removable gain')
  !     end if
        if (obj%gain_mode_flag == REMOVE_PREVIOUS_GAIN) then
          if (obj%hdr_first <= HDR_NOMINAL_SIZE .or. &
              obj%hdr_first > obj%nwih_input) then
            call pc_error ('HDR_FIRST must be set when removing previous gain')
            call pc_error ('HDR_FIRST must be >',HDR_NOMINAL_SIZE)
            call pc_error ('HDR_FIRST must be <=',obj%nwih_input)
          end if
          if (obj%hdr_last <= obj%hdr_first) then
            call pc_error ('HDR_LAST must exceed HDR_FIRST &
                           &when removing previous gain')
          end if
          if (obj%max_win_cnt <= 0) then
            call pc_error ('HDR_LAST minus HDR_FIRST is too small to store &
                           &gain parameters when removing previous gain')
          end if
        end if
      end if


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('gain_mode', (/ "APPLY PERMANENT GAIN", &
                                                 "APPLY REMOVABLE GAIN", &
                                                 "REMOVE PREVIOUS GAIN" /))

      call pc_put_options_field ('windows'  , (/ "REGULAR", "CUSTOM " /))

      call pc_put_global   ('nwih', obj%nwih_output)

      call pc_put_gui_only ('nwih_input' , obj%nwih_input)
      call pc_put_gui_only ('nwih_output', obj%nwih_output)

      call pc_put ('gain_mode'    , obj%gain_mode)
      call pc_put ('hdr_first'    , temp_hdr_first)
      call pc_put ('hdr_last'     , temp_hdr_last)
      call pc_put ('windows'      , obj%windows)
      call pc_put ('win_len'      , obj%win_len)
      call pc_put ('win_inc'      , obj%win_inc)
      call pc_put ('start_val'    , obj%start_val)
      call pc_put ('tim_last'     , obj%tim_last)
      call pc_put ('gain_max'     , obj%gain_max)
      call pc_put ('debri'        , obj%debri)
      call pc_put ('debri_max'    , obj%debri_max)
      call pc_put ('debri_tpr'    , obj%debri_tpr)
      call pc_put ('debri_tim_beg', obj%debri_tim_beg)
      call pc_put ('debri_tim_end', obj%debri_tim_end)
      call pc_put ('tim_win'      , obj%tim_win, obj%tim_win_cnt)
      call pc_put ('tim_len'      , obj%tim_len, obj%tim_win_cnt)

      removing   = (obj%gain_mode_flag == REMOVE_PREVIOUS_GAIN)
      debrighten = (obj%debri                           .and. .not.removing)
      regular    = (obj%windows_flag == REGULAR_WINDOWS .and. .not.removing)
      custom     = (obj%windows_flag == CUSTOM_WINDOWS  .and. .not.removing)

      call pc_put_sensitive_field_flag    ('hdr_first'       , removing)
      call pc_put_sensitive_field_flag    ('hdr_last'        , removing)
      call pc_put_sensitive_field_flag    ('start_val'       , .not.removing)
      call pc_put_sensitive_field_flag    ('tim_last'        , .not.removing)
      call pc_put_sensitive_field_flag    ('gain_max'        , .not.removing)
      call pc_put_sensitive_field_flag    ('debri'           , .not.removing)
      call pc_put_sensitive_field_flag    ('windows'         , .not.removing)
      call pc_put_sensitive_field_flag    ('debri_max'       , debrighten)
      call pc_put_sensitive_field_flag    ('debri_tpr'       , debrighten)
      call pc_put_sensitive_field_flag    ('debri_tim_beg'   , debrighten)
      call pc_put_sensitive_field_flag    ('debri_tim_end'   , debrighten)
      call pc_put_sensitive_field_flag    ('win_len'         , regular)
      call pc_put_sensitive_field_flag    ('win_inc'         , regular)
      call pc_put_sensitive_arrayset_flag ('tim_win_arrayset', custom)
      call pc_put_sensitive_array_flag    ('tim_win'         , custom)
      call pc_put_sensitive_array_flag    ('tim_len'         , custom)

      call pc_put_control ('PARALLEL_SAFE'        , .true.)
      call pc_put_control ('PCPS_SEND_MODE'       , 'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    , 'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      , 'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   , 'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   , 'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE', 'PCPS_RECEIVE_ALL_EOF')


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%median_buf  )) deallocate (obj%median_buf)
      if (associated(obj%win_beg_idx )) deallocate (obj%win_beg_idx)
      if (associated(obj%win_samp_cnt)) deallocate (obj%win_samp_cnt)
      if (associated(obj%agc_factor  )) deallocate (obj%agc_factor)

      if (pc_do_not_process_traces()) return

      obj%hdr_first = temp_hdr_first
      obj%hdr_last  = temp_hdr_last
      obj%ecount    = 0

        ! NOTE: The following four arrays could have been (and preferably
        ! would have been) automatic arrays in the trace processing routine,
        ! but they are placed into the data structure and allocated here
        ! for efficiency.

      allocate (obj%median_buf  (obj%buf_samp_max), stat=ier1)
      allocate (obj%win_beg_idx (obj%max_win_cnt) , stat=ier2)
      allocate (obj%win_samp_cnt(obj%max_win_cnt) , stat=ier3)
      allocate (obj%agc_factor  (obj%max_win_cnt) , stat=ier4)

      if(ier1 /= 0 .or. ier2 /= 0 .or. ier3 /= 0 .or. ier4 /= 0) then
        call pc_error('memory allocation failed')
      end if


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine xputil_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine xputil_execute (obj,hd,tr,whoops)
      implicit none
      type(xputil_struct),intent(inout)  :: obj             ! arguments
      double precision   ,intent(inout)  :: hd(:)           ! arguments
      real               ,intent(inout)  :: tr(:)           ! arguments
      logical            ,intent(out)    :: whoops          ! arguments

      INTEGER :: J1, win_half_cnt, agc_start_idx
      INTEGER :: agc_end_idx, last_win_stop_idx, big_abs_idx
      integer :: win_cnt, win_cnt_do, win_len_tmp, win_idx, win_size, win_end
      integer :: win_nonzero_cnt, win_samp_do, samp_cnt_tmp
      REAL    :: samp_tst_value, guess, median_samp_value


!!!!!!!!!!!!!! initialize error flag:

      whoops = .false.

!!!!!!!!!!!!!! initialize new trace headers:

      if (obj%gain_mode_flag == APPLY_REMOVABLE_GAIN) then
           hd(obj%hdr_first:obj%hdr_last) = 0.0
      end if

!!!!!!!!!!!!!! return if trace is dead:

      if (hd(25) == 0.0) return

!!!!!!!!!!!!!! remove previous gain:

      if (obj%gain_mode_flag == REMOVE_PREVIOUS_GAIN) then

           call xputil_fetch_gain (obj, hd, win_cnt, win_half_cnt, whoops)

           if (whoops) then
                obj%ecount = obj%ecount + 1
                tr(:)      = 0.0
                hd(25)     = 0.0
                whoops     = .false.
                return
           end if

           call xputil_gain (obj, tr, win_cnt, win_half_cnt, .false.)

           call lav_set_hdr (hd, tr, obj%ndpt)
           return
      end if

!!!!!!!!!!!!!! verify mute headers:

        CALL mutehw (HD, tr, obj%ndpt, 0.0, MUTEHW_SET)

!!!!!!!!!!!!!! last sample to use:

        agc_end_idx = MIN(obj%tim_last_idx,NINT(HD(64)))

!!!!!!!!!!!!!! Find starting point for this trace:

        IF (obj%start_val == 0.0) THEN
          agc_start_idx = MAX(NINT(HD(2)),1)
        ELSE
          big_abs_idx = mth_isamax(agc_end_idx,tr(1:obj%ndpt),1)
          samp_tst_value = obj%start_val*ABS(tr(big_abs_idx))

          IF (samp_tst_value == 0.0) GO TO 80  ! kill

          DO agc_start_idx = 1, big_abs_idx
            IF (ABS(tr(agc_start_idx)) >= samp_tst_value) exit
          END DO
        ENDIF

        agc_start_idx = MIN(obj%ndpt + 1,agc_start_idx)

!!!!!!!!!!!!!! select CONSTANT SIZE WINDOWS:

        win_choice: if(obj%windows_flag == REGULAR_WINDOWS) then

          ! replaces obj%last_win_start_idx calc at setup
          last_win_stop_idx = agc_end_idx - obj%win_len_cnt + 1

          agc_choice: IF (agc_start_idx < last_win_stop_idx) THEN
            win_cnt = (last_win_stop_idx - agc_start_idx + &
              obj%win_inc_cnt - 1)/obj%win_inc_cnt + 1
            obj%agc_factor(:win_cnt-1) = 0.0

            ! One or more windows fits in bounds, then:
            ! Set window tops starting at agc_start_idx, and always
            ! put last window BOTTOM at agc_end_idx.
            obj%win_beg_idx(:win_cnt-1) = agc_start_idx + &
              (/(J1,J1=0,win_cnt - 2)/)*obj%win_inc_cnt
            obj%agc_factor(win_cnt) = 0.0
            obj%win_beg_idx(win_cnt) = last_win_stop_idx
            win_len_tmp = obj%win_len_cnt
            win_half_cnt = obj%half_win_cnt
          ELSE IF (agc_start_idx < agc_end_idx - 8) THEN ! agc_choice
            ! If less than one window fits in bounds, then:
            ! If have at least 10  samples, then make one SHORT window.
            win_cnt = 1
            obj%agc_factor(1) = 0.0
            obj%win_beg_idx(1) = agc_start_idx
            win_len_tmp = agc_end_idx - agc_start_idx + 1
            win_half_cnt = (win_len_tmp - 1)/2
          ELSE ! agc_choice
            ! Otherwise, kill trace.
            GO TO 80  ! kill
          ENDIF agc_choice

!!!!!!!!!!!!!! select TIME VARIENT WINDOWS:

        ELSE ! win_choice

          win_cnt = obj%tim_win_cnt  ! Set number of windows

          ! Calculate window indices:
          obj%agc_factor(:win_cnt) = 0.0 !   Clear expansion array

          !   Top  + (mute index/obj%start_val)
          obj%win_beg_idx(:win_cnt) = obj%tim_win(:win_cnt)/obj%dt + &
            agc_start_idx

          obj%win_samp_cnt(:win_cnt) = obj%tim_len(:win_cnt)/obj%dt ! Size
          win_idx = 0

          DO win_cnt_do = 1, win_cnt
            ! Check to see how many of these adjusted windows
            ! are in trace length.
            IF (obj%win_beg_idx(win_cnt_do) > agc_end_idx - 8) cycle

            win_idx = win_cnt_do

            IF (obj%win_beg_idx(win_cnt_do) + obj%win_samp_cnt(win_cnt_do) &
              <= agc_end_idx - 8) cycle

            ! Adjust last window to fit
            obj%win_samp_cnt(win_cnt_do) = agc_end_idx - &
              obj%win_beg_idx(win_cnt_do)
          END DO

          win_cnt = win_idx  ! Reset number of windows.

          IF (win_cnt == 0) GO TO 80  ! If zero, kill trace.
        ENDIF win_choice

!!!!!!!!!!!!!! Start Compute Expansion Rates (obj%agc_factor):

        scan_loop: DO win_cnt_do = 1, win_cnt
          IF (obj%win_beg_idx(win_cnt_do) >= agc_end_idx) return

          win_size = win_len_tmp

          if(obj%windows_flag == CUSTOM_WINDOWS) &
                   win_size = obj%win_samp_cnt(win_cnt_do)

          win_end = obj%win_beg_idx(win_cnt_do) + win_size - 1
          win_end = MIN(agc_end_idx,win_end)
          win_nonzero_cnt = 0

!!!!!!!!!!!!!! Compute Average Power Expansion Rates (AVERAGE_POWER):

          mode_choice: IF (obj%pwr_mode_flag == AVERAGE_POWER) THEN

            win_nonzero_cnt = win_nonzero_cnt + &
              COUNT(tr(obj%win_beg_idx(win_cnt_do):win_end)/=0.0)

            ! **** MAKE SURE WINDOW IS AT LEAST 50% FULL
            samp_cnt_tmp = win_end - obj%win_beg_idx(win_cnt_do) + 1

            IF (win_nonzero_cnt > NINT(0.5*samp_cnt_tmp)) THEN
              obj%agc_factor(win_cnt_do) = MIN(obj%gain_max,win_nonzero_cnt / &
                sum(abs(tr(obj%win_beg_idx(win_cnt_do):win_end))))
            ELSE
              obj%agc_factor(win_cnt_do) = 0.0
            ENDIF

!!!!!!!!!!!!!! Compute Median Power Expansion Rates (MEDIAN_POWER):

          ELSE ! mode_choice

            guess = 0.0

            DO win_samp_do = obj%win_beg_idx(win_cnt_do), win_end
              IF (tr(win_samp_do) == 0.0) cycle

              win_nonzero_cnt = win_nonzero_cnt + 1
              obj%median_buf(win_nonzero_cnt) = ABS(tr(win_samp_do))
              guess = guess + ABS(tr(win_samp_do))
            END DO

            ! **** MAKE SURE WINDOW IS AT LEAST 50% FULL
            samp_cnt_tmp = win_end - obj%win_beg_idx(win_cnt_do) + 1

            IF (win_nonzero_cnt > NINT(0.5*samp_cnt_tmp)) THEN
              CALL median (obj%median_buf, win_nonzero_cnt, &
                median_samp_value, 0.845*guess/win_nonzero_cnt)

              obj%agc_factor(win_cnt_do) = MIN(obj%gain_max, &
                1./median_samp_value)
            ELSE
              obj%agc_factor(win_cnt_do) = 0.0
            ENDIF
          ENDIF mode_choice

!!!!!!!!!!!!!! Finish Compute Expansion Rates (obj%agc_factor):

        END DO scan_loop

!!!!!!!!!!!!!! Apply expansion to trace:

        if (obj%gain_mode_flag == APPLY_REMOVABLE_GAIN) then
             call xputil_save_gain (obj, hd, win_cnt, win_half_cnt)
        end if

        call xputil_gain (obj, tr, win_cnt, win_half_cnt, .true.)

!!!!!!!!!!!!!! Debrighten trace if desired.

        IF (obj%debri) CALL DBRITR (tr(obj%debri_beg_idx:), &
          obj%debri_pt_cnt, obj%debri_max, obj%debri_tpr_cnt)

        call lav_set_hdr (hd, tr, obj%ndpt)
        return

!!!!!!!!!!!!!! Kill trace if requested.

   80   CONTINUE
        tr(1:obj%ndpt) = 0.0
        hd(HDR_LAV) = 0.0

      end subroutine xputil_execute


!!-------------------------------- gain ----------------------------------!!
!!-------------------------------- gain ----------------------------------!!
!!-------------------------------- gain ----------------------------------!!

          ! needs  obj%ndpt  obj%windows_flag  obj%agc_factor(:)
          ! needs  obj%win_samp_cnt(:)  obj%win_beg_idx(:)

      subroutine xputil_gain (obj,tr,win_cnt,win_half_cnt,forward)
      type(xputil_struct),intent(inout) :: obj                  ! arguments
      real               ,intent(inout) :: tr(:)                ! arguments
      integer            ,intent(in)    :: win_cnt              ! arguments
      integer            ,intent(inout) :: win_half_cnt         ! arguments
      logical            ,intent(in)    :: forward              ! arguments
      integer                           :: first_nonzero_samp_idx
      integer                           :: win_center_idx,win_cnt_do
      integer                           :: prev_win_center_idx,J2
      real                              :: agc_value,agc_rate

!!!!!!!!!!!!!! Kill trace totally if there are no windows:

        if (win_cnt == 0) then
             tr(1:obj%ndpt) = 0.0
             return
        end if

!!!!!!!!!!!!!! Find first non-zero expansion factor:

        DO first_nonzero_samp_idx = 1, win_cnt
          IF (obj%agc_factor(first_nonzero_samp_idx) /= 0.0) exit
        END DO

!!!!!!!!!!!!!! Kill trace totally if all expansion factors are zero:

        IF (first_nonzero_samp_idx > win_cnt) then
             tr(1:obj%ndpt) = 0.0
             return
        end if

!!!!!!!!!!!!!! Get ready to apply expansions:

        if(obj%windows_flag == CUSTOM_WINDOWS) &
                   win_half_cnt = obj%win_samp_cnt(first_nonzero_samp_idx)/2

        prev_win_center_idx = obj%win_beg_idx(first_nonzero_samp_idx) + &
          win_half_cnt

        prev_win_center_idx = MIN(obj%ndpt,prev_win_center_idx)
        agc_value = obj%agc_factor(first_nonzero_samp_idx)

!!!!!!!!!!!!!! Apply expansion from start of trace to center of first window:

        if (forward) then
             tr(:prev_win_center_idx) = tr(:prev_win_center_idx) * agc_value
        else
             tr(:prev_win_center_idx) = tr(:prev_win_center_idx) / agc_value
        end if

!!!!!!!!!!!!!! Apply expansion from center first window to center last window:

        apply_loop: DO win_cnt_do = first_nonzero_samp_idx + 1, win_cnt
          ! Apply expansions to intervals between window centers.
          IF (obj%agc_factor(win_cnt_do) == 0.0) return

          if(obj%windows_flag == CUSTOM_WINDOWS) &
                     win_half_cnt = obj%win_samp_cnt(win_cnt_do)/2

          win_center_idx = obj%win_beg_idx(win_cnt_do) + win_half_cnt
          win_center_idx = MIN(obj%ndpt,win_center_idx)

          agc_rate = (obj%agc_factor(win_cnt_do)-agc_value) / &
            (win_center_idx - prev_win_center_idx)

          if (forward) then
            tr(prev_win_center_idx+1:win_center_idx) = &
              tr(prev_win_center_idx+1:win_center_idx) * &
              (agc_value + (/(J2,J2=prev_win_center_idx + 1 - &
                prev_win_center_idx,win_center_idx - prev_win_center_idx)/) * &
                agc_rate)
          else
            tr(prev_win_center_idx+1:win_center_idx) = &
              tr(prev_win_center_idx+1:win_center_idx) / &
              (agc_value + (/(J2,J2=prev_win_center_idx + 1 - &
                prev_win_center_idx,win_center_idx - prev_win_center_idx)/) * &
                agc_rate)
          end if

          prev_win_center_idx = win_center_idx
          agc_value = obj%agc_factor(win_cnt_do)
        END DO apply_loop

!!!!!!!!!!!!!! Apply expansion from center of last window to end of trace.

        if (forward) then
             tr(prev_win_center_idx+1:obj%ndpt) = &
                       tr(prev_win_center_idx+1:obj%ndpt) * agc_value
        else
             tr(prev_win_center_idx+1:obj%ndpt) = &
                       tr(prev_win_center_idx+1:obj%ndpt) / agc_value
        end if

      end subroutine xputil_gain

      
!!------------------------- gain headers needed ---------------------------!!
!!------------------------- gain headers needed ---------------------------!!
!!------------------------- gain headers needed ---------------------------!!


      function xputil_gain_headers_needed (nwindows) result (nheaders)
      integer            ,intent(in) :: nwindows             ! arguments
      integer                        :: nheaders             ! result

      nheaders = 6 + 3 * nwindows
      end function xputil_gain_headers_needed


!!------------------------- gain windows available --------------------------!!
!!------------------------- gain windows available --------------------------!!
!!------------------------- gain windows available --------------------------!!


      function xputil_gain_windows_available (nheaders) result (nwindows)
      integer            ,intent(in) :: nheaders             ! arguments
      integer                        :: nwindows             ! result

      nwindows = (nheaders - 6) / 3
      end function xputil_gain_windows_available


!!----------------------------- save gain --------------------------------!!
!!----------------------------- save gain --------------------------------!!
!!----------------------------- save gain --------------------------------!!

          ! uses   obj%hdr_first
          ! saves  obj%ndpt  obj%tstrt  obj%dt
          ! saves  obj%windows_flag     obj%agc_factor(:)
          ! saves  obj%win_samp_cnt(:)  obj%win_beg_idx(:)

      subroutine xputil_save_gain (obj,hd,win_cnt,win_half_cnt)
      type(xputil_struct),intent(in)    :: obj                  ! arguments
      double precision   ,intent(inout) :: hd(:)                ! arguments
      integer            ,intent(in)    :: win_cnt              ! arguments
      integer            ,intent(in)    :: win_half_cnt         ! arguments
      integer                           :: win_cnt_do           ! local

      hd(obj%hdr_first    ) = win_cnt
      hd(obj%hdr_first + 1) = win_half_cnt   
      hd(obj%hdr_first + 2) = obj%windows_flag
      hd(obj%hdr_first + 3) = obj%ndpt  
      hd(obj%hdr_first + 4) = obj%tstrt
      hd(obj%hdr_first + 5) = obj%dt  

      do win_cnt_do = 1,win_cnt
        hd(obj%hdr_first + 6 + 3*(win_cnt_do-1)) = obj%win_beg_idx (win_cnt_do)
        hd(obj%hdr_first + 7 + 3*(win_cnt_do-1)) = obj%win_samp_cnt(win_cnt_do)
        hd(obj%hdr_first + 8 + 3*(win_cnt_do-1)) = obj%agc_factor  (win_cnt_do)
      end do


      end subroutine xputil_save_gain


!!---------------------------- fetch gain ---------------------------------!!
!!---------------------------- fetch gain ---------------------------------!!
!!---------------------------- fetch gain ---------------------------------!!

          ! uses      obj%hdr_first        obj%max_win_cnt
          ! verifies  obj%ndpt  obj%tstrt  obj%dt
          ! fetches   obj%windows_flag     obj%agc_factor(:)
          ! fetches   obj%win_samp_cnt(:)  obj%win_beg_idx(:)

      subroutine xputil_fetch_gain (obj,hd,win_cnt,win_half_cnt,whoops)
      type(xputil_struct),intent(inout) :: obj                  ! arguments
      double precision   ,intent(in)    :: hd(:)                ! arguments
      integer            ,intent(out)   :: win_cnt              ! arguments
      integer            ,intent(out)   :: win_half_cnt         ! arguments
      logical            ,intent(out)   :: whoops               ! arguments
      integer                           :: ndpt,win_cnt_do      ! local
      real                              :: tstrt,dt             ! local
      real,parameter                    :: EPSILON = 1.0e-5     ! local

      win_cnt          = nint(hd(obj%hdr_first    ))
      win_half_cnt     = nint(hd(obj%hdr_first + 1))
      obj%windows_flag = nint(hd(obj%hdr_first + 2))
      ndpt             = nint(hd(obj%hdr_first + 3))
      tstrt            =      hd(obj%hdr_first + 4)
      dt               =      hd(obj%hdr_first + 5)

      if (win_cnt < 0) then
           if (obj%ecount < 10) call pc_print (trim(obj%prefix)// &
                     ' negative window count',win_cnt,'found in trace header')
           whoops = .true.
           return
      end if

      if (win_cnt > obj%max_win_cnt) then
           if (obj%ecount < 10) call pc_print (trim(obj%prefix)// &
                    ' excessive window count',win_cnt,'found in trace header')
           whoops = .true.
           return
      end if

      if (obj%windows_flag /= REGULAR_WINDOWS .and. &
          obj%windows_flag /= CUSTOM_WINDOWS) then
           if (obj%ecount < 10) call pc_print (trim(obj%prefix)// &
             ' invalid windows flag',obj%windows_flag,'found in trace header')
           whoops = .true.
           return
      end if

      if (ndpt /= obj%ndpt) then
           if (obj%ecount < 10) call pc_print (trim(obj%prefix)// &
                                   ' wrong NDPT',ndpt,'found in trace header')
           whoops = .true.
           return
      end if

      if (.not.mth_ameq(tstrt,obj%tstrt,EPSILON)) then
           if (obj%ecount < 10) call pc_print (trim(obj%prefix)// &
                                 ' wrong TSTRT',tstrt,'found in trace header')
           whoops = .true.
           return
      end if

      if (.not.mth_ameq(dt,obj%dt,EPSILON)) then
           if (obj%ecount < 10) call pc_print (trim(obj%prefix)// &
                                       ' wrong DT',dt,'found in trace header')
           whoops = .true.
           return
      end if

      do win_cnt_do = 1,win_cnt
        obj%win_beg_idx (win_cnt_do) = hd(obj%hdr_first + 6 + 3*(win_cnt_do-1))
        obj%win_samp_cnt(win_cnt_do) = hd(obj%hdr_first + 7 + 3*(win_cnt_do-1))
        obj%agc_factor  (win_cnt_do) = hd(obj%hdr_first + 8 + 3*(win_cnt_do-1))
  !     if (obj%agc_factor(win_cnt_do) > 0.0) &
  !         obj%agc_factor(win_cnt_do) = 1.0 / obj%agc_factor(win_cnt_do)
        if (obj%agc_factor(win_cnt_do) == 0.0) &
            obj%agc_factor(win_cnt_do) = 1.0
      end do

      whoops = .false.
      end subroutine xputil_fetch_gain


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module xputil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

