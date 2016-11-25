!<CPS_v1 type="PROCESS"/>
!!------------------------------- mix.f90 ---------------------------------!!
!!------------------------------- mix.f90 ---------------------------------!!
!!------------------------------- mix.f90 ---------------------------------!!


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
! Name       : MIX             (Simple 3D Mix)
! Category   : filters
! Written    : 2004-04-05   by: Tom Stoeckley
! Revised    : 2005-01-10   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Mix traces in X and Y directions and in time.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! MIX mixes traces by replacing each trace value by an average of values in
! a cube centered on the trace value.  The cube contains several traces in
! the X and Y directions and several trace samples in the time/depth direction.
!
! Hard zero values on a trace are not used in calculating the average value
! in a mixing cube.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  1. The bin centers and bin widths are used to identify the locations
!     of traces to use for mixing.  This insures that the correct traces
!     are used in both the X and Y directions, even if the starting X
!     coordinates of different lines are not the same or there are missing
!     traces.  The following binning parameters:
!                        X_INIT       Y_INIT
!                        X_INC        Y_INC
!     should match the actual inline and crossline spacing of the input
!     traces.  If two or more traces fall into the same bin, only one
!     trace in the bin will be used for dip searching and mixing.
!
!  2. The input traces should be sorted to either inline or crossline
!     order.  The inline direction corresponds to traces in the same
!     Y bin, and with X bins incrementing by one bin per trace.  The
!     crossline direction corresponds to traces in the same X bin, and
!     with Y bins incrementing by one bin per trace.  The trace spacing
!     should be such that there is generally one trace in each bin,
!     although some bins can be empty.
!
!  3. Each output trace is a mixed version of each input trace.  Traces
!     are output in the same order as they are input.
!
!  4. The parameter WIN_LEN is intended primarily for smoothing attributes
!     (such as velocity) which are in seismic trace format.  This parameter
!     should normally be set to zero when mixing actual seismic traces.
!     Seismic traces should usually be mixed only laterally but not vertically.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
! Process requires traces to be input with the primary sort in the Y
! direction and the secondary sort in the X direction.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to 1
! GATHERED  whether traces are a legitimate gather  set to false
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#          Description                Action taken
! ----          -----------                ------------
! 25            LAV                        reset
! HDR_X         X coordinate               used but not changed
! HDR_Y         Y coordinate               used but not changed
! 58,59,60,61   Scratch                    used
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2005-01-10  Stoeckley  Original Version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY ISSUES
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting traces.
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
!<NS MIX Process/NC=80>
!                           Simple 3D Mix
!
!  WIN_LEN =`FFFFFFFFFF  [/L]Window length in seconds for smoothing.
!  nt~~~~~~=`XXXXXXXXXX  [/L]#Samples in window for smoothing.
!
! `------------------- `-------------------
!  HDR_X~~~~~~=`II      HDR_Y~~~~~~=`II     [/L]Header word.
!  X_INIT~~~~~=`FFFFF   Y_INIT~~~~~=`FFFFF  [/L]First (or any) bin center.
!  X_INC~~~~~~=`FFFFF   Y_INC~~~~~~=`FFFFF  [/L]Bin increment.
!  MAX_X_BINS =`IIIII                       [/L]Maximum approximate number of bins (traces) in X direction.
!
!  NUM_TR_X~~~=`II      NUM_TR_Y~~~=`II     [/L]#Traces each side to mix.
!  nx~~~~~~~~~=`XX      ny~~~~~~~~~=`XX     [/L]Total #traces to mix.
! `------------------- `-------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="nx" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in X direction to mix together. </Tip>
!
! This value is equal to 2 * NUM_TR_X + 1.
!</Help>
!
!<Help KEYWORD="ny" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in Y direction to mix together. </Tip>
!
! This value is equal to 2 * NUM_TR_X + 1.
!</Help>
!
!
!<Help KEYWORD="nt" TYPE="DISPLAY_ONLY">
!<Tip> Total number of trace sample points (in time) to mix together. </Tip>
!
! This value is equal to the nearest integer of (WIN_LEN / DT) + 1.
!</Help>
!
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the time window, in sec, for mixing. </Tip>
! Default = 0.1
! Allowed = real >= 0.0
!
! The number of trace sample points mixed together is equal to the nearest
! integer of (WIN_LEN / DT) + 1.
!
! WIN_LEN is always adjusted to be an even multiple of the trace sample rate.
!
! The parameter WIN_LEN is intended primarily for smoothing attributes
! (such as velocity) which are in seismic trace format.  This parameter
! should normally be set to zero when mixing actual seismic traces.
! Seismic traces should usually be mixed only laterally but not vertically.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_X">
!<Tip> Number of traces on each side in X direction to mix together. </Tip>
! Default = 1
! Allowed = integer >= 0
!
! This parameter is the number of traces on each side of the center trace.
! This parameter should be zero for 2D data.
!
! A total of (2*NUM_TR_X + 1) times (2*NUM_TR_Y + 1) traces are mixed.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_Y">
!<Tip> Number of traces on each side in Y direction to mix together. </Tip>
! Default = 1
! Allowed = integer >= 0
!
! This parameter is the number of traces on each side of the center trace.
! This parameter should be zero for 2D data.
!
! A total of (2*NUM_TR_X + 1) times (2*NUM_TR_Y + 1) traces are mixed.
!</Help>
!
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing the X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing the Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="X_INIT">
!<Tip> X coordinate of the first (or any) trace in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin measured in the X direction.
! This value does not have to correspond to the first actual trace in the
! X direction, since this could in fact vary from line to line.
!</Help>
!
!
!<Help KEYWORD="Y_INIT">
!<Tip> Y coordinate of the first (or any) trace in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin measured in the Y direction.
! This value does not have to correspond to the first actual trace (or
! first actual line) in the Y direction.
!</Help>
!
!
!<Help KEYWORD="X_INC">
!<Tip> Increment of HDR_X between bin centers in the X direction. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This value must be the bin increment (or width) in the X direction.
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment of HDR_Y between bin centers in the Y direction. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This value must be the bin increment (or width) in the Y direction.
!</Help>
!
!
!<Help KEYWORD="MAX_X_BINS">
!<Tip> Maximum number of X bins in any one Y-line. </Tip>
! Default = blank
! Allowed = integer > 0
!
! This value must be specified.  The maximum number of traces which will
! be stored on disk at any one time will be slightly greater than MAX_X_BINS
! times (2*NUM_TR_Y + 1).
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module mix_module
      use pc_module
      use named_constants_module
      use mgather3d_module
      use lav_module
      use mth_module
      implicit none
      private
      public :: mix_create
      public :: mix_initialize
      public :: mix_update
      public :: mix_delete
      public :: mix            ! main execution (trace processing) routine.
      public :: mix_wrapup

      character(len=100),public,save :: MIX_IDENT = &
'$Id: mix.f90,v 1.1 2005/01/10 14:12:14 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: mix_struct

        private
        logical             :: skip_wrapup               ! wrapup flag
        integer             :: nwih,ndpt                 ! globals

        real                :: win_len                   ! process parameters
        integer             :: num_tr_x                  ! process parameters
        integer             :: num_tr_y                  ! process parameters
        integer             :: hdr_x                     ! process parameters
        integer             :: hdr_y                     ! process parameters
        real                :: x_init                    ! process parameters
        real                :: y_init                    ! process parameters
        real                :: x_inc                     ! process parameters
        real                :: y_inc                     ! process parameters
        integer             :: max_x_bins                ! process parameters

        integer                        :: ngather        ! dependent
        integer                        :: nhalf          ! dependent
        type(mgather3d_struct),pointer :: mgather3d      ! dependent

      end type mix_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(mix_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: SCRATCH_NSX = HDR_SCRATCH_58
      integer,parameter     :: SCRATCH_NSY = HDR_SCRATCH_59
      integer,parameter     :: SCRATCH_NSW = HDR_SCRATCH_60
      integer,parameter     :: SCRATCH_NSF = HDR_SCRATCH_61

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine mix_create (obj)

      type(mix_struct),pointer :: obj                      ! arguments

      allocate (obj)

      nullify (obj%mgather3d)

      call mix_initialize (obj)

      end subroutine mix_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine mix_delete (obj)

      type(mix_struct),pointer :: obj                      ! arguments

      call mix_wrapup (obj)

      deallocate(obj)

      end subroutine mix_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine mix_initialize (obj)

      type(mix_struct),intent(inout) :: obj                   ! arguments

      obj%win_len            = 0.100
      obj%num_tr_x           = 1
      obj%num_tr_y           = 1
      obj%hdr_x              = 7
      obj%hdr_y              = 8
      obj%x_init             = 1.0
      obj%y_init             = 1.0
      obj%x_inc              = 1.0
      obj%y_inc              = 1.0
      obj%max_x_bins         = INIL

      call mix_update (obj)

      end subroutine mix_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine mix_update (obj)

      type(mix_struct),intent(inout),target :: obj                ! arguments
      integer                               :: lun                ! local
      integer                               :: nstore,nscratch    ! local
      integer                               :: nx,ny,nt           ! local
      real                                  :: dt,tlength         ! local
      real,allocatable                      :: xweights(:)        ! local
      real,allocatable                      :: yweights(:)        ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      lun = pc_get_lun()

      call pc_get_global ('nwih'              , obj%nwih)
      call pc_get_global ('ndpt'              , obj%ndpt)
      call pc_get_global ('dt'                , dt)

      call pc_get        ('win_len'           , obj%win_len           )
      call pc_get        ('num_tr_x'          , obj%num_tr_x          )
      call pc_get        ('num_tr_y'          , obj%num_tr_y          )
      call pc_get        ('hdr_x'             , obj%hdr_x             )
      call pc_get        ('hdr_y'             , obj%hdr_y             )
      call pc_get        ('x_init'            , obj%x_init            )
      call pc_get        ('y_init'            , obj%y_init            )
      call pc_get        ('x_inc'             , obj%x_inc             )
      call pc_get        ('y_inc'             , obj%y_inc             )
      call pc_get        ('max_x_bins'        , obj%max_x_bins        )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      tlength = (obj%ndpt - 1) * dt

      call mth_constrain (obj%win_len  , 0.0 , tlength, dt)
      call mth_constrain (obj%num_tr_x ,  0  , 999)
      call mth_constrain (obj%num_tr_y ,  0  , 999)

      if (obj%hdr_x <= 0 .or. obj%hdr_x > obj%nwih) then
           call pc_warning ('BAD X HEADER WORD NUMBER - reset to 7')
           obj%hdr_x = 7
      end if

      if (obj%hdr_y <= 0 .or. obj%hdr_y > obj%nwih) then
           call pc_warning ('BAD Y HEADER WORD NUMBER - reset to 8')
           obj%hdr_x = 8
      end if

      if (obj%x_inc <= 0.0) then
           call pc_warning ('X BIN INCREMENT MUST BE > ZERO - reset to 1.0')
           obj%x_inc = 1.0
      end if

      if (obj%y_inc <= 0.0) then
           call pc_warning ('Y BIN INCREMENT MUST BE > ZERO - reset to 1.0')
           obj%y_inc = 1.0
      end if

      if (obj%max_x_bins == INIL .or. obj%max_x_bins <= 0) then
           call pc_error ('MAX_X_BINS MUST BE SPECIFIED')
           obj%max_x_bins = INIL
      end if

      nx          = 2 * obj%num_tr_x  + 1
      ny          = 2 * obj%num_tr_y  + 1
      nt          = 1 + nint(obj%win_len / dt)
      obj%ngather = nx * ny
      obj%nhalf   = nt / 2
      obj%win_len = 2 * obj%nhalf * dt
      nt          = 1 + 2 * obj%nhalf


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put        ('win_len'           , obj%win_len           ,7)
      call pc_put        ('num_tr_x'          , obj%num_tr_x            )
      call pc_put        ('num_tr_y'          , obj%num_tr_y            )
      call pc_put        ('hdr_x'             , obj%hdr_x               )
      call pc_put        ('hdr_y'             , obj%hdr_y               )
      call pc_put        ('x_init'            , obj%x_init            ,6)
      call pc_put        ('y_init'            , obj%y_init            ,6)
      call pc_put        ('x_inc'             , obj%x_inc             ,6)
      call pc_put        ('y_inc'             , obj%y_inc             ,6)
      call pc_put        ('max_x_bins'        , obj%max_x_bins          )

      call pc_put_gui_only ('nx'  , nx   )
      call pc_put_gui_only ('ny'  , ny   )
      call pc_put_gui_only ('nt'  , nt   )

      nscratch = mgather3d_scratch(obj%nwih,obj%ndpt,nx,ny) &
                   + obj%ngather * (obj%nwih + obj%ndpt)
      nstore   = mgather3d_store &
                      (obj%nwih,obj%ndpt,nx,ny,obj%max_x_bins)

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call mgather3d_delete (obj%mgather3d)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      allocate (xweights(nx))
      allocate (yweights(ny))

      xweights(:) = 1.0
      yweights(:) = 1.0

      call mgather3d_create (obj%mgather3d, lun, obj%nwih, obj%ndpt,     &
                             obj%hdr_x, obj%hdr_y,                       &
                             obj%x_init, obj%y_init,                     &
                             obj%x_inc, obj%y_inc,                       &
                             nx, ny,                                     &
                             xweights, yweights,                         &
                             SCRATCH_NSX, SCRATCH_NSY,                   &
                             SCRATCH_NSW, SCRATCH_NSF, obj%max_x_bins,   &
                             'MIX')

      deallocate (xweights)
      deallocate (yweights)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine mix_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine mix (obj,ntr,hdi,tri,hdo,tro)

      type(mix_struct),intent(inout) :: obj                          ! arguments
      integer         ,intent(inout) :: ntr                          ! arguments
      double precision,intent(inout) :: hdi(:,:)                     ! arguments
      real            ,intent(in)    :: tri(:,:)                     ! arguments
      double precision,intent(out)   :: hdo(:,:)                     ! arguments
      real            ,intent(out)   :: tro(:,:)                     ! arguments
      double precision               :: hdmove(obj%nwih,obj%ngather) ! local
      real                           :: trmove(obj%ndpt,obj%ngather) ! local
      integer                        :: mid,igather,indx             ! local
      integer                        :: indx2,istart,istop,kount2    ! local
      real                           :: value,total2                 ! local
      integer                        :: kount(obj%ndpt)              ! local
      real                           :: total(obj%ndpt)              ! local

!----------get the mix gather.

      call mgather3d (obj%mgather3d,ntr,hdi,tri,hdmove,trmove)

      if (ntr == NEED_TRACES) return

      if (ntr == FATAL_ERROR) then
           call pc_error ('MIX: FATAL ERROR IN MGATHER3D')
           call mix_wrapup (obj)
           return
      end if

      if (ntr == NO_MORE_TRACES) then
           call mix_wrapup (obj)
           return
      end if

!----------get output trace.

      mid = (ntr+1)/2
      hdo(1:obj%nwih,1) = hdmove(1:obj%nwih,mid)
      tro(1:obj%ndpt,1) = 0.0

      do indx = 1,obj%ndpt
           kount(indx) = 0
           total(indx) = 0.0
           do igather = 1,obj%ngather
                value = trmove(indx,igather)
                if (value == 0.0) cycle
                kount(indx) = kount(indx) + 1
                total(indx) = total(indx) + value
           end do
      end do

      do indx = 1,obj%ndpt
           istart = max(indx - obj%nhalf, 1)
           istop  = min(indx + obj%nhalf, obj%ndpt)
           kount2 = 0
           total2 = 0.0
           do indx2 = istart,istop
                kount2 = kount2 + kount(indx2)
                total2 = total2 + total(indx2)
           end do
           if (kount2 == 0) then
                tro(indx,1) = 0.0
           else
                tro(indx,1) = total2 / kount2
           end if
      end do

      ntr = 1
      call lav_set_hdr (hdo,tro,obj%ndpt,ntr)

      end subroutine mix


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine mix_wrapup (obj)

      type(mix_struct),intent(inout) :: obj                  ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print         (' ')
      call pc_print         ('MIX: WRAPUP')
      call mgather3d_delete (obj%mgather3d)
      call pc_print         ('MIX: FINISHED')
      call pc_print         (' ')

      end subroutine mix_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module mix_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

