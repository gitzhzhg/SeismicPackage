!<CPS_v1 type="PROCESS"/>
!!----------------------------- trstats.f90 ---------------------------------!!
!!----------------------------- trstats.f90 ---------------------------------!!
!!----------------------------- trstats.f90 ---------------------------------!!


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
! Name       : TRSTATS              (Trace Statistics)
! Category   : miscellaneous
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2004-12-06   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Derive statistics from seismic traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Statistics are gathered and printed for all input traces which have specified
! identification in the HDR_IDENT header word.
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
! This process is a single-trace process.
! Traces are not changed by this process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs the same traces it receives.
! Traces are not changed by this process.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         used but not changed.
! NDPT      number of sample values in trace        used but not changed.
! TSTRT     starting time on trace                  used but not changed.
! DT        trace sample interval                   used but not changed.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#           Description                Action taken
! ----           -----------                ------------
! HDR_IDENT      trace identification       used to identify traces.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2004-12-06  Stoeckley  Initial version.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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
!
!<NS TRSTATS/NC=80>
!
!                          Trace Statistics
!
!
! HDR_IDENT~~~~~~=`II           [/L]Header word containing trace identification.
!
! IDENT~~~~~~~~~~=`IIIIIII      [/L]Trace identification.
!
! MIN_ESTIMATE~~~=`FFFFFFFFFFF  [/L]Estimate for smallest expected value.
!
! MAX_ESTIMATE~~~=`FFFFFFFFFFF  [/L]Estimate for largest expected value.
!
! USE_ABS_VALUES =`KK           [/L]Whether to use absolute values.
!
! DESCRIPTION~~~~=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="HDR_IDENT">
!<Tip> Header word containing trace identification. </Tip>
! Default = 48
! Allowed = 0-NWIH    (normally 0 or 48-55 or 65-NHIH)
!
! If HDR_IDENT is zero, statistics are gathered from all traces.
!
! If HDR_IDENT is not zero, statistics are gathered only from traces with
! the specified header word value specified by the IDENT parameter.
! Traces with other values in the HDR_IDENT header word are ignored.
!</Help>
!
!
!<Help KEYWORD="IDENT">
!<Tip> Trace identification. </Tip>
! Default = 0
! Allowed = any integer value
!
! This is the identification to be found in the HDR_IDENT header word
! of the input traces.  Statistics are gathered only from traces with
! the specified header word value specified by this IDENT parameter.
! Traces with other values in the HDR_IDENT header word are ignored.
!
! This parameter is not used if HDR_IDENT is zero.
!</Help>
!
!
!<Help KEYWORD="DESCRIPTION">
!<Tip> Descriptions for the traces being analyzed. </Tip>
! Default = blank
! Allowed = any character string
!
! This description can be used as a convenience for describing the traces
! being analysed.  It is particularly useful if several instances of TRSTATS
! are being run in the same job using different IDENT parameters.
!</Help>
!
!
!<Help KEYWORD="MIN_ESTIMATE">
!<Tip> Estimate for the smallest expected trace value. </Tip>
! Default = 0.0
! Allowed = any floating point value
!
! Statistics are gathered for trace values between MIN_ESTIMATE and
! MAX_ESTIMATE.
!</Help>
!
!
!<Help KEYWORD="MAX_ESTIMATE">
!<Tip> Estimate for the largest expected trace value. </Tip>
! Default = 1.0
! Allowed = any floating point value
!
! Statistics are gathered for trace values between MIN_ESTIMATE and
! MAX_ESTIMATE.
!</Help>
!
!
!<Help KEYWORD="USE_ABS_VALUES">
!<Tip> Whether to use absolute values. </Tip>
! Default = YES
! Allowed = YES or NO
!
! If this parameter is YES, the absolute values on the traces will be
! used for gathering statistics.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module trstats_module
      use pc_module
      use named_constants_module
      use statistics_module

      implicit none
      private
      public :: trstats_create
      public :: trstats_initialize
      public :: trstats_update
      public :: trstats_delete
      public :: trstats            ! main trace processing routine.
      public :: trstats_wrapup

      character(len=100),public,save :: TRSTATS_IDENT = &
'$Id: trstats.f90,v 1.1 2004/12/06 13:41:59 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: trstats_struct

        private
        logical               :: skip_wrapup           ! wrapup flag.

        integer               :: hdr_ident             ! process parameter
        integer               :: ident                 ! process parameter
        character(len=30)     :: description           ! process parameter
        real                  :: min_estimate          ! process parameter
        real                  :: max_estimate          ! process parameter
        logical               :: use_abs_values        ! process parameter

        type(statistics_struct),pointer :: statistics

      end type trstats_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                     ,save :: lunprint  ! unit number for printing.
      type(trstats_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine trstats_create (obj)
      type(trstats_struct),pointer :: obj       ! arguments
      integer                      :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
           call pc_error ("Unable to allocate obj in trstats_create")
           return
      end if

      nullify (obj%statistics)

      call trstats_initialize (obj)
      end subroutine trstats_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine trstats_delete (obj)
      type(trstats_struct),pointer :: obj            ! arguments
      integer                      :: ierr           ! for error checking

      call trstats_wrapup (obj)

      call statistics_delete (obj%statistics)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) &
             call pc_warning ("error deallocating obj in trstats_delete")
      end subroutine trstats_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine trstats_initialize (obj)
      type(trstats_struct),intent(inout) :: obj            ! arguments

      obj%hdr_ident       = 48
      obj%ident           = 0
      obj%description     = ' '
      obj%min_estimate    = 0.0
      obj%max_estimate    = 1.0
      obj%use_abs_values  = .true.

      call trstats_update (obj)
      end subroutine trstats_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine trstats_update (obj)
      type(trstats_struct),intent(inout),target :: obj         ! arguments
      integer                                   :: nwih,ndpt   ! local
      real                                      :: tstrt,dt    ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global ('nwih'    , nwih)
      call pc_get_global ('ndpt'    , ndpt)
      call pc_get_global ('tstrt'   , tstrt)
      call pc_get_global ('dt'      , dt)

      call pc_get ('hdr_ident         ', obj%hdr_ident)
      call pc_get ('ident             ', obj%ident)
      call pc_get ('description       ', obj%description)
      call pc_get ('min_estimate      ', obj%min_estimate)
      call pc_get ('max_estimate      ', obj%max_estimate)
      call pc_get ('use_abs_values    ', obj%use_abs_values)


!!------------------------ verify parameters --------------------------------!!
!!------------------------ verify parameters --------------------------------!!
!!------------------------ verify parameters --------------------------------!!


      if (obj%hdr_ident < 0 .or. obj%hdr_ident > nwih) then
           call pc_warning ('bad HDR_IDENT number - reset to 48')
           obj%hdr_ident = 48
      end if


!!------------------------ write parameters --------------------------------!!
!!------------------------ write parameters --------------------------------!!
!!------------------------ write parameters --------------------------------!!


      call pc_put ('hdr_ident         ', obj%hdr_ident)
      call pc_put ('ident             ', obj%ident)
      call pc_put ('description       ', obj%description)
      call pc_put ('min_estimate      ', obj%min_estimate)
      call pc_put ('max_estimate      ', obj%max_estimate)
      call pc_put ('use_abs_values    ', obj%use_abs_values)

      call pc_put_sensitive_field_flag ('ident'  , obj%hdr_ident > 0)


!!------------------- prepare for execution --------------------------------!!
!!------------------- prepare for execution --------------------------------!!
!!------------------- prepare for execution --------------------------------!!


      call statistics_delete (obj%statistics)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call statistics_create (obj%statistics, ndpt, tstrt, dt, &
                              obj%min_estimate, obj%max_estimate)


!!------------------------- end of update --------------------------------!!
!!------------------------- end of update --------------------------------!!
!!------------------------- end of update --------------------------------!!


      end subroutine trstats_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine trstats (obj,ntr,hd,tr)
      type(trstats_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(in)    :: ntr                   ! arguments
      double precision    ,intent(in)    :: hd(:,:)               ! arguments
      real                ,intent(in)    :: tr(:,:)               ! arguments
      integer                            :: itr,ident             ! local

      do itr = 1,ntr
           if (obj%hdr_ident > 0) then
                ident = nint(hd(obj%hdr_ident,itr))
                if (ident /= obj%ident) cycle
           end if

           call statistics_gather &
                            (obj%statistics, tr(:,itr), obj%use_abs_values)
      end do

      end subroutine trstats


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine trstats_wrapup (obj)
      type(trstats_struct),intent(inout) :: obj              ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call statistics_print (obj%statistics, lunprint, obj%description)

      end subroutine trstats_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module trstats_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

