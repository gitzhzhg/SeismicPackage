!<CPS_v1 type="PROCESS"/>
!!----------------------------- kastats.f90 ---------------------------------!!
!!----------------------------- kastats.f90 ---------------------------------!!
!!----------------------------- kastats.f90 ---------------------------------!!


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
! Name       : KASTATS              (Trace Statistics for KA)
! Category   : miscellaneous
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2005-01-10   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Derive statistics from seismic traces for the KA process.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Statistics are gathered and printed for all input traces which have the
! following identifications in the HDR_IDENT header word:
!
!         ABRAKADABRA_RMS_AMPL    =  82   RMS amplitudes.
!         ABRAKADABRA_MAX_AMPL    =  83   max absolute amplitudes.
!         ABRAKADABRA_GATHER_SEMB =  71   gather semblance.
!         ABRAKADABRA_STRUCT_SEMB =  90   structural semblance.
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
!  2. 2005-01-10  Stoeckley  Use abrakadabra to return identification string.
!  1. 2003-06-19  Stoeckley  Initial version.
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
!<NS KASTATS/NC=80>
!
!                          Trace Statistics for KA
!
!
! HDR_IDENT~~~~~~~~=`II           [/L]Header word containing trace identification.
!
! MAX_RMS_ESTIMATE =`FFFFFFFFFFF  [/L]Estimate for largest expected RMS amplitude trace value.
!
! MAX_ABS_ESTIMATE =`FFFFFFFFFFF  [/L]Estimate for largest expected max absolute amplitude trace value.
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
! Allowed = 1-NWIH    (normally 48-55 or 65-NHIH)
!</Help>
!
!
!<Help KEYWORD="MAX_RMS_ESTIMATE">
!<Tip> Estimate for the largest expected RMS amplitude trace value. </Tip>
! Default = 1.0
! Allowed = any floating point value
!
! Statistics are gathered for RMS amplitude trace values between zero and
! MAX_RMS_ESTIMATE.  Negative RMS amplitudes are ignored.
!</Help>
!
!
!<Help KEYWORD="MAX_ABS_ESTIMATE">
!<Tip> Estimate for the largest expected absolute amplitude trace value. </Tip>
! Default = 1.0
! Allowed = any floating point value
!
! Statistics are gathered for max absolute amplitude trace values between
! zero and MAX_ABS_ESTIMATE.  Negative absolute amplitudes are treated as if
! they were positive.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module kastats_module
      use pc_module
      use named_constants_module
      use statistics_module
      use abrakadabra_module
      use string_module

      implicit none
      private
      public :: kastats_create
      public :: kastats_initialize
      public :: kastats_update
      public :: kastats_delete
      public :: kastats            ! main trace processing routine.
      public :: kastats_wrapup

      character(len=100),public,save :: KASTATS_IDENT = &
'$Id: kastats.f90,v 1.2 2005/01/10 14:12:10 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: kastats_struct

        private
        logical               :: skip_wrapup           ! wrapup flag.

        integer               :: hdr_ident             ! process parameter
        real                  :: max_rms_estimate      ! process parameter
        real                  :: max_abs_estimate      ! process parameter

        type(statistics_struct),pointer :: statistics_rms
        type(statistics_struct),pointer :: statistics_abs
        type(statistics_struct),pointer :: statistics_gat
        type(statistics_struct),pointer :: statistics_str

      end type kastats_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                     ,save :: lunprint  ! unit number for printing.
      type(kastats_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine kastats_create (obj)
      type(kastats_struct),pointer :: obj       ! arguments
      integer                      :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
           call pc_error ("Unable to allocate obj in kastats_create")
           return
      end if

      nullify (obj%statistics_rms)
      nullify (obj%statistics_abs)
      nullify (obj%statistics_gat)
      nullify (obj%statistics_str)

      call kastats_initialize (obj)
      end subroutine kastats_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine kastats_delete (obj)
      type(kastats_struct),pointer :: obj            ! arguments
      integer                      :: ierr           ! for error checking

      call kastats_wrapup (obj)

      call statistics_delete (obj%statistics_rms)
      call statistics_delete (obj%statistics_abs)
      call statistics_delete (obj%statistics_gat)
      call statistics_delete (obj%statistics_str)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) &
             call pc_warning ("error deallocating obj in kastats_delete")
      end subroutine kastats_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine kastats_initialize (obj)
      type(kastats_struct),intent(inout) :: obj            ! arguments

      obj%hdr_ident        = 48
      obj%max_rms_estimate = 1.0
      obj%max_abs_estimate = 1.0

      call kastats_update (obj)
      end subroutine kastats_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine kastats_update (obj)
      type(kastats_struct),intent(inout),target :: obj         ! arguments
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
      call pc_get ('max_rms_estimate  ', obj%max_rms_estimate)
      call pc_get ('max_abs_estimate  ', obj%max_abs_estimate)


!!------------------------ verify parameters --------------------------------!!
!!------------------------ verify parameters --------------------------------!!
!!------------------------ verify parameters --------------------------------!!


      if (obj%hdr_ident < 1 .or. obj%hdr_ident > nwih) then
           call pc_warning ('bad HDR_IDENT number - reset to 48')
           obj%hdr_ident = 48
      end if


!!------------------------ write parameters --------------------------------!!
!!------------------------ write parameters --------------------------------!!
!!------------------------ write parameters --------------------------------!!


      call pc_put ('hdr_ident         ', obj%hdr_ident)
      call pc_put ('max_rms_estimate  ', obj%max_rms_estimate)
      call pc_put ('max_abs_estimate  ', obj%max_abs_estimate)


!!------------------- prepare for execution --------------------------------!!
!!------------------- prepare for execution --------------------------------!!
!!------------------- prepare for execution --------------------------------!!


      call statistics_delete (obj%statistics_rms)
      call statistics_delete (obj%statistics_abs)
      call statistics_delete (obj%statistics_gat)
      call statistics_delete (obj%statistics_str)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call statistics_create (obj%statistics_rms, ndpt, tstrt, dt, &
                              0.0, obj%max_rms_estimate)

      call statistics_create (obj%statistics_abs, ndpt, tstrt, dt, &
                              0.0, obj%max_abs_estimate)

      call statistics_create (obj%statistics_gat, ndpt, tstrt, dt, &
                              0.0, 1.0)

      call statistics_create (obj%statistics_str, ndpt, tstrt, dt, &
                              0.0, 1.0)


!!------------------------- end of update --------------------------------!!
!!------------------------- end of update --------------------------------!!
!!------------------------- end of update --------------------------------!!


      end subroutine kastats_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine kastats (obj,ntr,hd,tr)
      type(kastats_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(in)    :: ntr                   ! arguments
      double precision    ,intent(in)    :: hd(:,:)               ! arguments
      real                ,intent(in)    :: tr(:,:)               ! arguments
      integer                            :: itr,ident             ! local

      do itr = 1,ntr
        ident = nint(hd(obj%hdr_ident,itr))
        select case (ident)
          case (ABRAKADABRA_RMS_AMPL) 
                 call statistics_gather (obj%statistics_rms, tr(:,itr))
          case (ABRAKADABRA_MAX_AMPL)  
                 call statistics_gather (obj%statistics_abs, tr(:,itr), .true.)
          case (ABRAKADABRA_GATHER_SEMB)
                 call statistics_gather (obj%statistics_gat, tr(:,itr))
          case (ABRAKADABRA_STRUCT_SEMB)
                 call statistics_gather (obj%statistics_str, tr(:,itr))
        end select
      end do

      end subroutine kastats


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine kastats_wrapup (obj)
      type(kastats_struct),intent(inout) :: obj                  ! arguments
      character(len=50)                  :: msg1,msg2,msg3,msg4  ! local

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      msg1 = abrakadabra_message(ABRAKADABRA_RMS_AMPL)
      msg2 = abrakadabra_message(ABRAKADABRA_MAX_AMPL)
      msg3 = abrakadabra_message(ABRAKADABRA_GATHER_SEMB)
      msg4 = abrakadabra_message(ABRAKADABRA_STRUCT_SEMB)

      call statistics_print (obj%statistics_rms, lunprint, msg1)
      call statistics_print (obj%statistics_abs, lunprint, msg2)
      call statistics_print (obj%statistics_gat, lunprint, msg3)
      call statistics_print (obj%statistics_str, lunprint, msg4)

      end subroutine kastats_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module kastats_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

