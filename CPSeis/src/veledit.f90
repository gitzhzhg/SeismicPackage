!<CPS_v1 type="PROCESS"/>
!!------------------------------ veledit.f90 --------------------------------!!
!!------------------------------ veledit.f90 --------------------------------!!
!!------------------------------ veledit.f90 --------------------------------!!

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
! Name       : VELEDIT
! Category   : velocity_analysis
! Written    : 2003-09-24   by: Tom Stoeckley
! Revised    : 2004-12-15   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Edit velocity function traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process can be used to edit velocity functions which are received
! as traces.
!
! Currently this process can convert one velocity type to another.
!
! This process supports the following velocity function types:
!
!             VTRM            RMS velocity versus 2-way time
!             VTAV        average velocity versus 2-way time
!             VTIN       interval velocity versus 2-way time
!             VZRM            RMS velocity versus depth
!             VZAV        average velocity versus depth
!             VZIN       interval velocity versus depth
! 
! If an error occurs upon conversion, some of the output velocity values
! will be set to zero.  This process will print a warning message if this
! occurs.
!
! A conversion error will occur if:
!    (a) Velocity is zero or negative.
!    (b) Conversion from an RMS velocity causes a negative
!            argument for the SQRT function.
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
! Input traces must contain velocities versus time or depth.
! The time or depth of the first sample on the trace must be zero.
! The value of the TSTRT global must be zero.
!
! If the velocities are versus time, the trace sample interval must be
! constant in time.
!
! If the velocities are versus depth, the trace sample interval must be
! constant in depth.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Output traces will contain edited velocities versus time or depth.
!
! If the input velocities are versus time, the output velocities must also
! be versus time.
!
! If the input velocities are versus depth, the output velocities must also
! be versus depth.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
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
! Hwd#    Description                Action taken
! ----    -----------                ------------
!  25     largest absolute value     changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2004-12-15  Stoeckley  Initial version.
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
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
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
!<NS VELEDIT/NC=80>
!
!                     Edit Velocity Function Traces
!
! NDPT=`XXXXXX   TSTRT=`XXXXXXXXXX   DT=`XXXXXXXXX
!
! VELTYPE_INPUT~~~~~=`CCCC
!
! VELTYPE_OUTPUT~~~~=`CCCC
!
! TIMEDEPTH_INTERVAL=`FFFFFFFFFFFFFFF
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="NDPT">
!<Tip> Number of trace samples (global).</Tip>
!
! This is the number of velocity values on a trace.
!</Help>
!
!
!<Help KEYWORD="TSTRT">
!<Tip> Time or depth of first trace sample (global).</Tip>
!
! This must be zero.
!</Help>
!
!
!<Help KEYWORD="DT">
!<Tip> Trace sample interval (global).</Tip>
!
! TIMEDEPTH_INTERVAL does not necessarily have to match this DT global.
!</Help>
!
!
!<Help KEYWORD="TIMEDEPTH_INTERVAL">
!<Tip> Velocity sample interval on the traces in time or depth.</Tip>
! Default = none
! Allowed = real > 0
!
! TIMEDEPTH_INTERVAL must be in seconds if VELTYPE_INPUT is versus time.
! TIMEDEPTH_INTERVAL must be in depth units if VELTYPE_INPUT is versus depth.
! The depth units must match the units in the velocity values (depth/second).
!
! TIMEDEPTH_INTERVAL does not necessarily have to match the DT global.
!</Help>
!
!
!<Help KEYWORD="VELTYPE_INPUT">
!<Tip> Input velocity function type.</Tip>
! Default = VTRM
! Allowed = VTRM                RMS velocity versus 2-way time
! Allowed = VTAV            average velocity versus 2-way time
! Allowed = VTIN           interval velocity versus 2-way time
! Allowed = VZRM                RMS velocity versus depth
! Allowed = VZAV            average velocity versus depth
! Allowed = VZIN           interval velocity versus depth
!
! TIMEDEPTH_INTERVAL must be in seconds if VELTYPE_INPUT is versus time.
! TIMEDEPTH_INTERVAL must be in depth units if VELTYPE_INPUT is versus depth.
! The depth units must match the units in the velocity values (depth/second).
!</Help>
!
!
!<Help KEYWORD="VELTYPE_OUTPUT">
!<Tip> Output velocity function type.</Tip>
! Default = VTRM
! Allowed = VTRM                RMS velocity versus 2-way time
! Allowed = VTAV            average velocity versus 2-way time
! Allowed = VTIN           interval velocity versus 2-way time
! Allowed = VZRM                RMS velocity versus depth
! Allowed = VZAV            average velocity versus depth
! Allowed = VZIN           interval velocity versus depth
!
! If VELTYPE_INPUT is versus time, VELTYPE_OUTPUT must also be versus time.
! If VELTYPE_INPUT is versus depth, VELTYPE_OUTPUT must also be versus depth.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module veledit_module
      use pc_module
      use named_constants_module
      use velutil_module

      implicit none
      private
      public :: veledit_create
      public :: veledit_initialize
      public :: veledit_update
      public :: veledit_delete
      public :: veledit            ! main trace processing routine.
      public :: veledit_wrapup

      character(len=100),public,save :: VELEDIT_IDENT = &
'$Id: veledit.f90,v 1.1 2004/12/15 14:00:42 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: veledit_struct

        private
        logical                    :: skip_wrapup         ! wrapup flag.

        integer                    :: ndpt                ! global
        real                       :: tstrt               ! global
        real                       :: dt                  ! global

        real                       :: timedepth_interval  ! process parameter
        character(len=5)           :: veltype_input       ! process parameter
        character(len=5)           :: veltype_output      ! process parameter

        integer                    :: kount               ! dependent
        integer                    :: nerr                ! dependent

      end type veledit_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                     ,save :: lunprint  ! unit number for printing.
      type(veledit_struct),pointer,save :: object    ! needed for traps.

      integer,parameter,private :: NTYPES = 6

      character(len=8),parameter,private :: veltypes(NTYPES) =   &
                                                  (/'VTRM    ',  &
                                                    'VTAV    ',  &
                                                    'VTIN    ',  &
                                                    'VZRM    ',  &
                                                    'VZAV    ',  &
                                                    'VZIN    '/)
      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine veledit_create (obj)
      type(veledit_struct),pointer :: obj       ! arguments
      integer                      :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in veledit_create")

      call veledit_initialize (obj)
      end subroutine veledit_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine veledit_delete (obj)
      type(veledit_struct),pointer :: obj       ! arguments
      integer                      :: ierr      ! for error checking

      call veledit_wrapup (obj)

      deallocate(obj, stat=ierr)
    if (ierr /= 0) call pc_warning ("error deallocating obj in veledit_delete")
      end subroutine veledit_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine veledit_initialize (obj)
      type(veledit_struct),intent(inout) :: obj       ! arguments

      obj%timedepth_interval = FNIL
      obj%veltype_input      = 'VTRM'
      obj%veltype_output     = 'VTRM'

      call veledit_update (obj)
      end subroutine veledit_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine veledit_update (obj)
      type(veledit_struct),intent(inout),target :: obj             ! arguments
      integer                                   :: itype           ! local
      logical                                   :: good1,good2     ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)

      call pc_get ('TIMEDEPTH_INTERVAL', obj%timedepth_interval)
      call pc_get ('VELTYPE_INPUT     ', obj%veltype_input)
      call pc_get ('VELTYPE_OUTPUT    ', obj%veltype_output)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%tstrt /= 0.0) then
           call pc_error ('the TSTRT global must be zero.')
      end if

      if (obj%timedepth_interval == FNIL) then
           call pc_error ('TIMEDEPTH_INTERVAL is not set.')
      else if (obj%timedepth_interval <= 0.0) then
           call pc_error ('TIMEDEPTH_INTERVAL must be greater than zero.')
      end if

      good1 = .false.
      good2 = .false.
      do itype = 1,NTYPES
           if (obj%veltype_input  == veltypes(itype)) good1 = .true.
           if (obj%veltype_output == veltypes(itype)) good2 = .true.
      end do

      if (.not.good1) then
           call pc_error ('invalid VELTYPE_INPUT choice.')
      end if

      if (.not.good2) then
           call pc_error ('invalid VELTYPE_OUTPUT choice.')
      end if

      if (obj%veltype_input(1:2) /= obj%veltype_output(1:2)) then
           call pc_error ('VELTYPE_INPUT and VELTYPE_OUTPUT must be &
                            &both versus time or both versus depth.')
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('VELTYPE_INPUT ', veltypes)
      call pc_put_options_field ('VELTYPE_OUTPUT', veltypes)

      call pc_put ('TIMEDEPTH_INTERVAL', obj%timedepth_interval)
      call pc_put ('VELTYPE_INPUT     ', obj%veltype_input)
      call pc_put ('VELTYPE_OUTPUT    ', obj%veltype_output)

      call pc_put_gui_only ('NDPT    ', obj%ndpt)
      call pc_put_gui_only ('TSTRT   ', obj%tstrt)
      call pc_put_gui_only ('DT      ', obj%dt)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%kount = 0
      obj%nerr  = 0

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine veledit_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine veledit (obj,ntr,hd,tr)
      type(veledit_struct),intent(inout) :: obj                    ! arguments
      integer             ,intent(inout) :: ntr                    ! arguments
      double precision    ,intent(inout) :: hd(:,:)                ! arguments
      real                ,intent(inout) :: tr(:,:)                ! arguments
      integer                            :: itr,ierr,indx          ! local
      real                               :: xin (obj%ndpt)         ! local
      real                               :: xout(obj%ndpt)         ! local
      real                               :: vout(obj%ndpt)         ! local

      do indx = 1,obj%ndpt
           xin(indx) = (indx-1) * obj%timedepth_interval
      end do

      do itr = 1,ntr
           call velutil_convert (obj%veltype_input, obj%ndpt,   &
                                 xin, tr(:,itr),                &
                                 obj%veltype_output,xout,vout,ierr)
           tr(1:obj%ndpt,itr) = vout
           obj%kount = obj%kount + 1
           if (ierr == VELUTIL_ERROR) obj%nerr = obj%nerr + 1
      end do

      end subroutine veledit


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine veledit_wrapup (obj)
      type(veledit_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print (' ')
      call pc_print &
        ('VELEDIT:',obj%kount,'velocity function traces processed.')
      call pc_print &
        ('VELEDIT:',obj%nerr,'velocity function traces had conversion errors.')
      call pc_print (' ')

      end subroutine veledit_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module veledit_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

