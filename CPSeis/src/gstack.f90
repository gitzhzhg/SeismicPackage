!<CPS_v1 type="PROCESS"/>
!!------------------------------- gstack.f90 ---------------------------------!!
!!------------------------------- gstack.f90 ---------------------------------!!
!!------------------------------- gstack.f90 ---------------------------------!!

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
! Name       : GSTACK                     (generalized stack)
! Category   : stacks
! Written    : 2004-11-30   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Generalized stack.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process stacks traces into a pre-defined 3D volume.  The traces need not
! be sorted into any particular order.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! The traces should be pre-stack traces.
! Traces can be ungathered, or gathered in any manner.
! There is no significance to the gathers if the traces are gathered.
! The traces can be unsorted or sorted in any order.
! Dead traces are ignored.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! The output traces will be stacked and scaled traces with the primary sort in
! the Y direction and the secondary sort in the X direction.
!
! There will be one stacked trace per bin.
! Traces are output one at a time.
! Empty bins will not be output.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         --> specify action taken.
! NDPT      number of sample values in trace        --> specify action taken.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!   1     sequential trace number    reset to sequential trace number
!   2     top mute                   reset to the smallest top mute
!   3     current gather number      reset to sequential trace number
!   4     current channel number     reset to 1
!   5     fold                       reset to nominal fold of stacked trace
!   6     offset                     used but not reset
!  25     largest absolute value     used and reset
!  64     bottom mute                reset to the largest bottom mute
! HDR_X   X grid coordinate          used but not reset
! HDR_Y   Y grid coordinate          used but not reset
!
! All header words except 1,2,3,4,5,25,64 will be taken from the
! smallest-offset trace contributing to the stack.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!003. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  2. 2005-01-31  stoectr    Modified to speed up by factor of two by
!                             replacing temptbuffer with temptcache, which
!                             does less trace array copying; added test to
!                             insure trace buffer memory usage is not too large.
!  1. 2004-11-30  stoectr    Initial version.
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
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
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
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
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS GSTACK Process/NC=80>
!                              Generalized Stack
!
!          HDR_FLAG=`II
!
!          FSE=`FFFFFFFFFFF    MSCL=`FFFFFFFFFFF    TVFSE=`FFFFFFFFFFF
!
!
!   HDR_X=`II  X_INIT=`FFFFFFF  X_INC=`FFFFFFF  X_LAST=`FFFFFFF  X_TOT=`IIIII
!
!   HDR_Y=`II  Y_INIT=`FFFFFFF  Y_INC=`FFFFFFF  Y_LAST=`FFFFFFF  Y_TOT=`IIIII
!
!
!              TRACE_BUFFER=`IIIIIII           megabytes=`XXXXXXXX
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing the X grid coordinate.</Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing the Y grid coordinate.</Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="X_INIT">
!<Tip> Minimum CMP X grid location (header word HDR_X). </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="X_INC">
!<Tip> Increment between header word HDR_X values for CMP locations. </Tip>
! Default =  1.0
! Allowed = real > 0.
!</Help>
!
!
!<Help KEYWORD="X_LAST">
!<Tip> Maximum CMP X grid location (header word HDR_X). </Tip>
! Default =  1.0
! Allowed = real
!
! If both X_LAST and X_TOT are specified, X_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="X_TOT">
!<Tip> Total number of CMP X grid locations (header word HDR_X). </Tip>
! Default = 1
! Allowed = int
!
! If both X_LAST and X_TOT are specified, X_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="Y_INIT">
!<Tip> Minimum CMP Y grid location (header word HDR_Y). </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment between header word HDR_Y values for CMP locations. </Tip>
! Default =  1.0
! Allowed = real > 0.
!</Help>
!
!
!<Help KEYWORD="Y_LAST">
!<Tip> Maximum CMP Y grid location (header word HDR_Y). </Tip>
! Default =  1.0
! Allowed = real
!
! If both Y_LAST and Y_TOT are specified, Y_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="Y_TOT">
!<Tip> Total number of CMP Y grid locations (header word HDR_Y). </Tip>
! Default = 1
! Allowed = int
!
! If both Y_LAST and Y_TOT are specified, Y_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! If HDR_FLAG = 0 then all traces are processed.  Otherwise only traces with
! a flag set in header word HDR_FLAG are processed.
!</Help>
!
!
!<Help KEYWORD="FSE">
!<Tip> Fold of stack exponential method for scaling stacked traces. </Tip>
! Default = 0.5
! Allowed = 0.0-1.0
!
! The Fold of Stack exponential method scales stacked traces by dividing trace
! samples by the number of live traces stacked together raised to the FSE
! power.  The FSE method is time-independent.  FSE = 0.0 does no scaling.
!</Help>
!
!
!<Help KEYWORD="MSCL">
!<Tip> Mute Scaling method for scaling stacked traces. </Tip>
! Default = 0.25
! Allowed = 0.0-1.0
!
! The Mute Scaling method scales stacked traces by multiplying by a complicated
! empirical formula (with no obvious justification) based on maximum fold and
! local time-dependent fold.  Mute scaling is intended to compensate for loss
! of fold due to muting.  MSCL = 0.0 does no scaling.
!
!        Scale(MSCL) = 1.0 + MSCL*(((maximum fold)/(actual fold)) - 1.0)
!</Help>
!
!
!<Help KEYWORD="TVFSE">
!<Tip> Time Varying Fold of Stack Exponential method. </Tip>
! Default = 0.0
! Allowed = 0.0-1.0
!
! The Time Varying Fold of Stack Exponential method scales stacked traces by
! dividing trace samples by the time varying number of live samples stacked
! together raised to the TVFSE power.  TVFSE = 0.0 does no scaling.  This is an
! industry-standard method.
!
! If TVFSE /= 0.0, then FSE and MSCL methods are disabled.
!</Help>
!
!
!<Help KEYWORD="TRACE_BUFFER">
!<Tip> Number of traces to keep in a trace buffer. </Tip>
! Default = 100
! Allowed >= 1
!
! A work buffer will be created to store stacked traces as they are being
! composited.  Additional stacked traces will be stored on disk.  Traces that
! are being actively stacked will have preference in the work buffer, with
! traces not being actively stacked moved to a temporary disk file.
!</Help>
!
!
!<Help KEYWORD="megabytes">
!<Tip> Amount of central memory required in megabytes.</Tip>
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module gstack_module
      use pc_module
      use named_constants_module
      use lav_module
      use mth_module
      use stkscale_module
      use temptcache_module

      implicit none
      private
      public :: gstack_create
      public :: gstack_initialize
      public :: gstack_update
      public :: gstack_delete
      public :: gstack            ! main trace processing routine.
      public :: gstack_wrapup

      character(len=100),public,save :: GSTACK_IDENT = &
'$Id: gstack.f90,v 1.3 2006/10/17 13:45:44 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: gstack_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.

        integer                    :: hdr_x         ! process parameter.
        integer                    :: hdr_y         ! process parameter.
        real                       :: x_inc         ! process parameter.
        real                       :: x_init        ! process parameter.
        real                       :: x_last        ! process parameter.
        integer                    :: x_tot         ! process parameter.
        real                       :: y_inc         ! process parameter.
        real                       :: y_init        ! process parameter.
        real                       :: y_last        ! process parameter.
        integer                    :: y_tot         ! process parameter.
        real                       :: fse           ! process parameter.
        integer                    :: hdr_flag      ! process parameter.
        real                       :: mscl          ! process parameter.
        real                       :: tvfse         ! process parameter.
        integer                    :: trace_buffer  ! process parameter.

        integer                         :: maxrecords     ! x_tot * y_tot
        integer                         :: maxfold
        integer                         :: kount_input
        integer                         :: kount_dead
        integer                         :: kount_unflagged
        integer                         :: kount_outside
        integer                         :: kount_used
        integer                         :: kount_output
        integer                         :: irec
        type(temptcache_struct),pointer :: temptcache
        integer                ,pointer :: ifold(:)
        real                            :: xmin,xmax
        real                            :: ymin,ymax

      end type gstack_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                    ,save :: lunprint  ! unit number for printing.
      type(gstack_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine gstack_create (obj)
      type(gstack_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()
      allocate (obj)

      nullify  (obj%temptcache)
      nullify  (obj%ifold)

      call gstack_initialize (obj)
      end subroutine gstack_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine gstack_delete (obj)
      type(gstack_struct),pointer :: obj       ! arguments

      call gstack_wrapup (obj)

      deallocate(obj)
      end subroutine gstack_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine gstack_initialize (obj)
      type(gstack_struct),intent(inout) :: obj       ! arguments

      obj%hdr_x        = 7
      obj%hdr_y        = 8
      obj%x_inc        = 1.0
      obj%x_init       = 1.0
      obj%x_last       = 1.0
      obj%x_tot        = 1
      obj%y_inc        = 1.0
      obj%y_init       = 1.0
      obj%y_last       = 1.0
      obj%y_tot        = 1
      obj%fse          = 0.5
      obj%hdr_flag     = 0
      obj%mscl         = 0.25
      obj%tvfse        = 0.0
      obj%trace_buffer = 100

      call gstack_update (obj)
      end subroutine gstack_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine gstack_update (obj)
      type(gstack_struct),intent(inout),target :: obj             ! arguments
      real                                     :: x_last_keep     ! local
      real                                     :: y_last_keep     ! local
      integer                                  :: x_tot_keep      ! local
      integer                                  :: y_tot_keep      ! local
      logical                                  :: fse_mscl_sense  ! local
      logical                                  :: whoops          ! local
      integer                                  :: ier,nstore      ! local
      real                                     :: megabytes       ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      x_last_keep = obj%x_last
      y_last_keep = obj%y_last

      x_tot_keep = obj%x_tot
      y_tot_keep = obj%y_tot


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)

      call pc_get ('HDR_X       ', obj%hdr_x)
      call pc_get ('HDR_Y       ', obj%hdr_y)
      call pc_get ('X_INC       ', obj%x_inc)
      call pc_get ('X_INIT      ', obj%x_init)
      call pc_get ('X_LAST      ', obj%x_last)
      call pc_get ('X_TOT       ', obj%x_tot)
      call pc_get ('Y_INC       ', obj%y_inc)
      call pc_get ('Y_INIT      ', obj%y_init)
      call pc_get ('Y_LAST      ', obj%y_last)
      call pc_get ('Y_TOT       ', obj%y_tot)
      call pc_get ('FSE         ', obj%fse)
      call pc_get ('HDR_FLAG    ', obj%hdr_flag)
      call pc_get ('MSCL        ', obj%mscl)
      call pc_get ('TVFSE       ', obj%tvfse)
      call pc_get ('TRACE_BUFFER', obj%trace_buffer)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%hdr_x    < 1 .or. obj%hdr_x    > obj%nwih) obj%hdr_x    = 7
      if (obj%hdr_y    < 1 .or. obj%hdr_y    > obj%nwih) obj%hdr_y    = 8
      if (obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih) obj%hdr_flag = 0

      call mth_fix_pattern (obj%x_init, obj%x_inc, obj%x_last,  &
                            obj%x_tot, x_last_keep, x_tot_keep)

      call mth_fix_pattern (obj%y_init, obj%y_inc, obj%y_last,  &
                            obj%y_tot, y_last_keep, y_tot_keep)

      call stkscale_check (obj%fse, obj%mscl, obj%tvfse, fse_mscl_sense)

      obj%trace_buffer = max(obj%trace_buffer, 1)
      obj%maxrecords   = obj%x_tot * obj%y_tot

      nstore = obj%maxrecords + temptcache_nstore(obj%nwih,obj%ndpt,  &
                                     obj%maxrecords,obj%trace_buffer,'DRR')
      megabytes = 4.0 * nstore / 1000000.0

      if (pc_verify_end()) then
           if (megabytes > 500) then
             call pc_error ('GSTACK: too much memory requested.')
             call pc_error ('GSTACK: you must reduce TRACE_BUFFER.')
           endif
      endif


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put ('HDR_X       ', obj%hdr_x)
      call pc_put ('HDR_Y       ', obj%hdr_y)
      call pc_put ('X_INC       ', obj%x_inc)
      call pc_put ('X_INIT      ', obj%x_init)
      call pc_put ('X_LAST      ', obj%x_last)
      call pc_put ('X_TOT       ', obj%x_tot)
      call pc_put ('Y_INC       ', obj%y_inc)
      call pc_put ('Y_INIT      ', obj%y_init)
      call pc_put ('Y_LAST      ', obj%y_last)
      call pc_put ('Y_TOT       ', obj%y_tot)
      call pc_put ('FSE         ', obj%fse)
      call pc_put ('HDR_FLAG    ', obj%hdr_flag)
      call pc_put ('MSCL        ', obj%mscl)
      call pc_put ('TVFSE       ', obj%tvfse)
      call pc_put ('TRACE_BUFFER', obj%trace_buffer)

      call pc_put_control ('need_request' , .true.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('nstore'       , nstore)

      call pc_put_gui_only ('megabytes', megabytes, ndec=3)

      call pc_put_sensitive_field_flag ('FSE' , fse_mscl_sense)
      call pc_put_sensitive_field_flag ('MSCL', fse_mscl_sense)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call temptcache_close (obj%temptcache)

      if (associated(obj%ifold)) deallocate (obj%ifold)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%maxfold         = 0
      obj%kount_input     = 0
      obj%kount_dead      = 0
      obj%kount_unflagged = 0
      obj%kount_outside   = 0
      obj%kount_used      = 0
      obj%kount_output    = 0
      obj%irec            = 0
      obj%xmin            = 0.0
      obj%xmax            = 0.0
      obj%ymin            = 0.0
      obj%ymax            = 0.0

      call temptcache_open (obj%temptcache, 'GSTACK',             &
                            obj%nwih, obj%ndpt, lunprint, whoops, &
                            obj%maxrecords,obj%trace_buffer,'DRR')
      if (whoops) then
        call pc_error ('GSTACK: error opening temporary trace cache')
        return
      endif

      allocate (obj%ifold (obj%maxrecords),stat=ier)
      if (ier /= 0) then
        call pc_error ('GSTACK: error allocating',obj%maxrecords,'for IFOLD')
        return
      endif

      obj%ifold(:) = 0            ! nominal fold of each bin.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine gstack_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine gstack (obj,ntr,hd,tr)
      type(gstack_struct),intent(inout) :: obj                    ! arguments
      integer            ,intent(inout) :: ntr                    ! arguments
      double precision   ,intent(inout) :: hd(:,:)                ! arguments
      real               ,intent(inout) :: tr(:,:)                ! arguments
      integer                           :: itr                    ! local
      logical                           :: whoops,finished        ! local

      if (ntr >= 1) then
        do itr = 1,ntr
          call gstack_trace_input (obj,hd(:,itr),tr(:,itr),whoops)
          if (whoops) then
            call gstack_wrapup (obj)
            ntr = FATAL_ERROR
            return
          endif
        enddo
        ntr = NEED_TRACES
        return
      endif

      if (ntr == NO_MORE_TRACES) then
        call gstack_print_statistics (obj)
        ntr = NEED_TRACES
        ! do not return here - fall through to next IF block.
      endif

      if (ntr == NEED_TRACES) then
          call gstack_trace_output (obj,hd(:,1),tr(:,1),whoops,finished)
          if (whoops) then
            call gstack_wrapup (obj)
            ntr = FATAL_ERROR
          elseif (finished) then
            call gstack_wrapup (obj)
            ntr = NO_MORE_TRACES
          else
            ntr = 1
          endif
          return
      endif

      call pc_error ('GSTACK: illegal input value',ntr,'for NTR')
      call gstack_wrapup (obj)
      ntr = FATAL_ERROR

      end subroutine gstack


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine gstack_wrapup (obj)
      type(gstack_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print (' ')
      call pc_print ('GSTACK: all stacked traces have been output.')
      call pc_print ('GSTACK: requested number of bins to output = ', &
                       obj%maxrecords)
      call pc_print ('GSTACK: actual number of bins output = ', &
                       obj%kount_output)
      call pc_print ('GSTACK: number of empty bins not output = ', &
                       obj%maxrecords - obj%kount_output)
      call pc_print ('GSTACK: wrapup completed.')
      call pc_print (' ')
      call temptcache_close (obj%temptcache)
      call pc_print (' ')

      if (associated(obj%ifold)) deallocate (obj%ifold)

      end subroutine gstack_wrapup


!!------------------------ print statistics -------------------------------!!
!!------------------------ print statistics -------------------------------!!
!!------------------------ print statistics -------------------------------!!


      subroutine gstack_print_statistics (obj)
      type(gstack_struct),intent(in) :: obj                    ! arguments
      integer                        :: kount,irec             ! local
      real                           :: average                ! local

      kount   = 0
      average = 0.0
      do irec = 1,obj%maxrecords
           if (obj%ifold(irec) > 0) then
                kount   = kount   + 1
                average = average + obj%ifold(irec)
           endif
      enddo
      if (kount > 0) average = average / kount
      call pc_print (' ')
      call pc_print ('GSTACK: all input traces have been received.')
      call pc_print ('GSTACK: total number of input traces = ', &
                       obj%kount_input)
      call pc_print ('GSTACK: ', obj%kount_dead, 'dead traces skipped')
      call pc_print ('GSTACK: ', obj%kount_unflagged, &
                       'unflagged traces skipped')
      call pc_print ('GSTACK: ', obj%kount_outside, &
                       'traces outside of range skipped')
      call pc_print ('GSTACK: number of input traces used =', &
                       obj%kount_used)
      call pc_print ('GSTACK: maximum fold to output =', obj%maxfold)
      call pc_print ('GSTACK: average live fold to output =', average)
      call pc_print ('GSTACK: minimum X coordinate encountered =',obj%xmin, &
                                '    requested X_INIT =',obj%x_init)
      call pc_print ('GSTACK: maximum X coordinate encountered =',obj%xmax, &
                                '    requested X_LAST =',obj%x_last)
      call pc_print ('GSTACK: minimum Y coordinate encountered =',obj%ymin, &
                                '    requested Y_INIT =',obj%y_init)
      call pc_print ('GSTACK: maximum Y coordinate encountered =',obj%ymax, &
                                '    requested Y_LAST =',obj%y_last)
      call pc_print ('GSTACK: requested number of bins to output = ', &
                       obj%maxrecords)
      call pc_print ('GSTACK: actual number of bins to output = ', &
                       kount)
      call pc_print ('GSTACK: number of empty bins not to output = ', &
                       obj%maxrecords - kount)
      call pc_print ('GSTACK: now starting to output stacked traces.')
      call pc_print (' ')

      end subroutine gstack_print_statistics


!!--------------------------- trace input ----------------------------------!!
!!--------------------------- trace input ----------------------------------!!
!!--------------------------- trace input ----------------------------------!!


      subroutine gstack_trace_input (obj,hd,tr,whoops)
      type(gstack_struct),intent(inout) :: obj                    ! arguments
      double precision   ,intent(in)    :: hd(:)                  ! arguments
      real               ,intent(in)    :: tr(:)                  ! arguments
      logical            ,intent(out)   :: whoops                 ! arguments
      integer                           :: ixbin,iybin,irec       ! local
      real                              :: xcoord,ycoord          ! local
      double precision   ,pointer       :: hdstack(:)             ! local
      real               ,pointer       :: trstack(:)             ! local
      real               ,pointer       :: fold   (:)             ! local

      nullify (hdstack) ! jpa
      nullify (trstack) ! jpa
      nullify (fold) ! jpa

      obj%kount_input = obj%kount_input + 1

      if (hd(HDR_LAV) == 0.0) then
           obj%kount_dead = obj%kount_dead + 1
           whoops = .false.
           return
      endif

      if (obj%hdr_flag > 0) then
        if (hd(obj%hdr_flag) == 0.0) then
             obj%kount_unflagged = obj%kount_unflagged + 1
             whoops = .false.
             return
        endif
      endif

      xcoord = hd(obj%hdr_x)
      ycoord = hd(obj%hdr_y)

      if (obj%kount_input == 1) then
           obj%xmin = xcoord
           obj%xmax = xcoord
           obj%ymin = ycoord
           obj%ymax = ycoord
      else
           obj%xmin = min(xcoord,obj%xmin)
           obj%xmax = max(xcoord,obj%xmax)
           obj%ymin = min(ycoord,obj%ymin)
           obj%ymax = max(ycoord,obj%ymax)
      endif

      call mth_get_indices (xcoord, obj%x_init, obj%x_inc, obj%x_tot, &
                            ycoord, obj%y_init, obj%y_inc, obj%y_tot, &
                            ixbin, iybin, irec)
      if (irec == 0) then
           obj%kount_outside = obj%kount_outside + 1
           whoops = .false.
           return
      endif

      obj%kount_used = obj%kount_used + 1

      call temptcache_fetch (obj%temptcache,irec,hdstack,trstack,fold,whoops)
      if (whoops) then
            call pc_error ('GSTACK: error fetching trace from trace cache')
            return
      endif

      call gstack_augment_stack (obj,irec,hd,tr,hdstack,trstack,fold)
          
      end subroutine gstack_trace_input


!!--------------------------- trace output ---------------------------------!!
!!--------------------------- trace output ---------------------------------!!
!!--------------------------- trace output ---------------------------------!!


      subroutine gstack_trace_output (obj,hd,tr,whoops,finished)
      type(gstack_struct),intent(inout) :: obj                    ! arguments
      double precision   ,intent(out)   :: hd(:)                  ! arguments
      real               ,intent(out)   :: tr(:)                  ! arguments
      logical            ,intent(out)   :: whoops,finished        ! arguments
      double precision   ,pointer       :: hdstack(:)             ! local
      real               ,pointer       :: trstack(:)             ! local
      real               ,pointer       :: fold   (:)             ! local

      nullify (hdstack) ! jpa
      nullify (trstack) ! jpa
      nullify (fold) ! jpa

      whoops = .false.
      finished = .false.

      do
           obj%irec = obj%irec + 1
           if (obj%irec > obj%maxrecords) then
                finished = .true.
                return
           endif

           if (obj%ifold(obj%irec) == 0) cycle

           call temptcache_fetch (obj%temptcache,obj%irec, &
                                  hdstack,trstack,fold,whoops)
           if (whoops) then
                call pc_error ('GSTACK: error fetching output trace from cache')
                return
           endif


           call gstack_finish_stack (obj,obj%irec,hdstack,trstack,fold)
           hd(1:obj%nwih) = hdstack(:)
           tr(1:obj%ndpt) = trstack(:)
           exit
      enddo

      end subroutine gstack_trace_output


!!--------------------------- augment stack ------------------------------!!
!!--------------------------- augment stack ------------------------------!!
!!--------------------------- augment stack ------------------------------!!

! This routine is called to add a trace to the stacked trace.

      subroutine gstack_augment_stack (obj,irec,hd,tr,hdstack,trstack,fold)
      type(gstack_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: irec                  ! arguments
      double precision   ,intent(in)    :: hd     (:)            ! arguments
      real               ,intent(in)    :: tr     (:)            ! arguments
      double precision   ,intent(inout) :: hdstack(:)            ! arguments
      real               ,intent(inout) :: trstack(:)            ! arguments
      real               ,intent(inout) :: fold   (:)            ! arguments
      real                              :: offset,offset_stack   ! arguments
      real                              :: tmute,tmute_stack     ! arguments
      real                              :: bmute,bmute_stack     ! arguments

      if (obj%ifold(irec) == 0) then
           hdstack(:)        = hd(1:obj%nwih)
           trstack(:)        = tr(1:obj%ndpt)
           fold   (:)        = 0.0
           obj%ifold(irec)   = 1
           where (tr(1:obj%ndpt) /= 0.0) fold(:) = 1.0
           return
      endif

      offset       = hd      (HDR_OFFSET)
      tmute        = hd      (HDR_TOP_MUTE)
      bmute        = hd      (HDR_BOTTOM_MUTE)

      offset_stack = hdstack (HDR_OFFSET)
      tmute_stack  = hdstack (HDR_TOP_MUTE)
      bmute_stack  = hdstack (HDR_BOTTOM_MUTE)

      if (offset < offset_stack) hdstack(:) = hd(1:obj%nwih)

      obj%ifold(irec)          = obj%ifold(irec) + 1
      obj%maxfold              = max(obj%maxfold,obj%ifold(irec))
      hdstack(HDR_TOP_MUTE)    = min(tmute, tmute_stack)
      hdstack(HDR_BOTTOM_MUTE) = min(bmute, bmute_stack)

      trstack(:) = trstack(:) + tr(1:obj%ndpt)
      where (tr(1:obj%ndpt) /= 0.0) fold(:) = fold(:) + 1.0

      end subroutine gstack_augment_stack


!!---------------------------- finish stack -------------------------------!!
!!---------------------------- finish stack -------------------------------!!
!!---------------------------- finish stack -------------------------------!!

! This routine is called when a stacked trace has been accumulated
! and is ready to be scaled for output.

      subroutine gstack_finish_stack (obj,irec,hd,tr,fold)
      type(gstack_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: irec                  ! arguments
      double precision   ,intent(inout) :: hd(:)                 ! arguments
      real               ,intent(inout) :: tr(:)                 ! arguments
      real               ,intent(inout) :: fold(:)               ! arguments

      call stkscale (hd, tr, fold, obj%ndpt, &
                     obj%fse, obj%mscl, obj%tvfse, obj%ifold(irec))

      call lav_set_hdr (hd, tr, obj%ndpt)

      obj%kount_output        = obj%kount_output + 1
      hd(HDR_SEQUENCE)        = obj%kount_output
      hd(HDR_CURRENT_GROUP)   = obj%kount_output
      hd(HDR_CURRENT_CHANNEL) = 1
      hd(HDR_FOLD)            = obj%ifold(irec)

      end subroutine gstack_finish_stack


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module gstack_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

