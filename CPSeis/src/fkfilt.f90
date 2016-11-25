!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2001-01-29. />

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
!------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : FKFILT    (F-K Filter)
! Category   : filters
! Written    : 1989-05-05   by: John Reed
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Perform 2D F-K filtering of trace gathers or sections.
! Portability: No known limitations.
! Parallel   : No.
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! FKFILT provides 2D F-K filtering for gathers and for stacked or migrated
! sections.  Three types of filter are available:
!
!     1. Regular dip (pie-slice) filtering,
!
!     2. Wavenumber filtering (the filter can change in the wavenumber
!     direction but is constant over frequency), and
!
!     3. A special option to set defaults for F-K demultiple filtering.
!     (F-K demultiple filtering is a special filter used with NMO.  See NMO for
!     further documentation.)
!
! FKFILT also has an option to filter only a convex quadrilateral subset within
! the input trace group.  This option should be used when noise is localized
! and it is undesirable to filter the entire trace group.
!------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Dip Convention for FKFILT
!
! A positive dip in F-K space corresponds to an event in X-T space whose time
! increases with increasing value of header word 1.
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! Process requires traces to be input in order for each functional group being
! filtered.
!
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs one trace at a time.
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       changed to 1
! GATHERED  whether traces are a legitimate gather  used
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 2       Head mute index            used
! 3       Current gather             Used to group traces into gathers.
! 25      LAV                        reset
! 60      Scratch Hdr 60             Used to save Current gather HDR(3)
! 64      Tail mute index            used
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 23. 2006-09-11 Stoeckley  Add call to pc_register_array_names for SeisSpace.
!022. 2006-01-10  B. Menger   Removed Unused Variables.
! 21. 2001-10-11 SMCook     Minor gui_def section changes due to CFE update.
! 20. 2001-06-11 Selzler    Corrected dip_factor computation.
! 19. 2001-04-27 Selzler    Reconverted to new CPS.
! 18. 2001-03-30 Brad Kruse Correct filter inversion error.  DEMULT mode now
!                           seems to be correct.
! 17. 2001-03-27 Brad Kruse Correct error in handling traces when the
!                           panel is disregarded (< PANEL_INIT).
! 16. 2001-03-21 Brad Kruse Convert to new CPS.  Remove PART or SUBSET mode.
! 15. 1998-11-10 Vunderink  Begin using the f90 compiler.
! 14. 1997-03-19 Vunderink  Increased memory used by X2KCC and T2FRC
!                           primitives
! 13. 1997-01-16 Vunderink  Changed SUBSET from a data region bounded by 3
!                           lines to a data region bounded by a convex
!                           quadrilateral.
! 12. 1992-03-19 Troutt     Add tail mute restore (call MUTEHW).  The mute
!                           restoration here reapplies a 60-mil taper_tim.
!                           This code has been changed to match MUTE's
!                           algorithm.
! 11. 1990-04-25 Howard     Make compatible with STRIN change.
! 10. 1990-04-03 Troutt     Correct process name in controlled abort at 1999
!                           (previously said "FKAP").
! 9.  1989-12-13 Troutt     Add report of number of groups processed at
!                           NTR=0 time. Also fix formatted writes of TMAX
!                           and FMAX for DCODE.
! 8.  1989-10-05 Troutt     Set linked array counter NFLT to 0 before call
!                           to DCODE so "left over" values won't get used.
! 7.  1989-09-13 Troutt     Changed logic to use STORAGE for working trace
!                           (and header)arrays instead of using the 2ndary
!                           arrays in the FKF call.  The 2ndary arrays
!                           were renamed to HDR2 and TR2 and are used to
!                           output 1 trace at a time via an internal call
!                           to GATHR.  All of the main logic for the old
!                           2ndary arrays (HDR1, TR1) remains intact,
!                           except that they now reside in storage via
!                           pointers.
! 6.  1989-08-18 Troutt     Fixed calculations to include TSTRT.
! 5.  1989-06-01 Reed       Fixed second bug in merge logic
! 4.  1980-05-31 Reed       Fixed a bug in merging logic
! 3.  1989-05-26 Reed       Modified for far-near subset filters
! 2.  1989-05-23 Reed       Running correctly on test profiles.
! 1.  1989-05-05 Reed       Started modifying FKAP code.
!
!------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!
!
!
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!
!------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!------------------------------------------------------------------------------
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
! NSTORE           0       amount of permanent memory needed.
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
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS
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
!
!------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!  MODE = SUBSET is not implemented.  bk
!  FREQ_MAX was removed.  This was a limit to the frequencies processed,
!  and a performance tuning issue when the 2D FFT was accomplished as
!  two 1D FFT's separated with a transpose (memory reversal of columns and
!  rows).  Since there is no performance advantage to reducing the number
!  of FFT's, this limit was removed.
!
!------------------------------------------------------------------------------
!</programming_doc>

!------------------------------------------------------------------------------
!<gui_def>
!<NS FKFILT Process/NC=78/NR=20>
!                               F-K Filter Process
!             Perform 2D F-K filtering of trace gathers or sections.
!
!        TYPE_FILT=`CCCCCC        HDR_PANEL~= `III
!                                 PANEL_INIT= `FFFFFFFFFF
!        NUM_TR_IN=`IIII          PANEL_INC~= `FFFFFFFFFF
!
!    `-----------------------------------
!       COORDINATES  FACTORS
!       `FFFFFFFFFFFF`FFFFFFFFFFFF           XFORM_VERT_SIZE=`IIIII
!       `FFFFFFFFFFFF`FFFFFFFFFFFF           XFORM_HOR_SIZE~=`IIIII
!       `FFFFFFFFFFFF`FFFFFFFFFFFF
!       `FFFFFFFFFFFF`FFFFFFFFFFFF
!       `FFFFFFFFFFFF`FFFFFFFFFFFF
!    `-----------------------------------
!
!    COORD_UNITS=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS COORDINATES_ARRAYSET[/XST/YST]>
!<PARMS COORD_UNITS[/ML=64/XST]>
!</gui_def>
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="NUM_TR_IN">
!<Tip> Maximum number of traces in any group to be filtered at once. </Tip>
! Default = -
! Allowed = int > 0
! NUM_TR is for memory management only.  It is not used to define trace groups.
! If greater than the actual maximum, fft computations may be wasted.
!</Help>
!
!<Help KEYWORD="TYPE_FILT">
!<Tip> Whether to use dip, wavenumber or demultiple F-K filtering. </Tip>
! Default = DIP
! Allowed = DIP    (F-K dip filtering)
! Allowed = WAVE   (F-K wavenumber filtering)
! Allowed = DEMULT (F-K demultiple filtering)
! F-K dip filtering is a filter that varies with dip only.
!
! F-K wavenumber filtering is a filter that varies with wavenumber only and is
! constant over frequency.
!
! F-K demultiple filtering is a special filter used with NMO to perform F-K
! demultiple filtering.  (See NMO for further documentation.)
!</Help>
!
!<Help KEYWORD="HDR_PANEL">
!<Tip> Header word used to define input trace groups to be filtered. </Tip>
! Default = 3
! Allowed = 1 - NWIH
! Normally header word 3 should be used for trace gathers and header word 8 for
! stacked or migrated sections.
!
! The value in header word HDR_PANEL must translate to a different bin
! for each group of traces that are filtered together.
! This normally means the header word will be constant within such a group.
!</Help>
!
!<Help KEYWORD="PANEL_INIT">
!<Tip> First value of HDR_PANEL for input trace groups. </Tip>
! Default = 1.0
! Allowed = real
!
! Values of the HDR_PANEL header word will not be processed if less than
! PANEL_INIT.
!</Help>
!
!<Help KEYWORD="PANEL_INC">
!<Tip> Increment between HDR_PANEL values for input trace groups. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="COORDINATES">
!<Tip> Array of coordinates for specifying filter (linked with FACTORS). </Tip>
! Default = -
! Allowed = real array (linked)
!
! If TYPE_FILT = DIP, then units of COORDINATES are seconds/trace.
!
! If TYPE_FILT = WAVE, then COORDINATES entries are wavenumbers as labeled by
! trace count, ie., -num_fft/2 to +num_fft/2, with zero indicating the center
! of the F-K plane (where num_fft == XFORM_HOR_SIZE).
!
! If TYPE_FILT = DEMULT, then no COORDINATES entries are required or allowed.
!</Help>
!
!<Help KEYWORD="FACTORS">
!<Tip> Array of factors for specifying filter (linked with COORDINATES). </Tip>
! Default = -
! Allowed = real array (linked)
! Entries in FACTORS are multiplicative filter factors applied at the
! associated COORDINATES value.  Factor values are linearly interpolated
! between adjacent coordinate-factor pairs.  Factor values are extrapolated as
! a constant from the first and last FACTORS entries to the beginning and
! ending of the natural range of COORDINATES, respectively.
!</Help>
!
!<Help KEYWORD="XFORM_VERT_SIZE" TYPE="DISPLAY_ONLY">
!<Tip> Vertical extent of the transform region. </Tip>
! Default = -
! Allowed = integer
!
! FFT size of the time dimension, along the trace.
!</Help>
!
!<Help KEYWORD="XFORM_HOR_SIZE" TYPE="DISPLAY_ONLY">
!<Tip> Horizontal extent of the transform region. </Tip>
! Default = -
! Allowed = integer
!
! FFT Size of the X dimension, across traces in a panel
!</Help>
!
!<Help KEYWORD="COORD_UNITS" TYPE="DISPLAY_ONLY">
!<Tip> Units associated with coordinate values. </Tip>
! Default = -
! Allowed = seconds/trace
! Allowed = fft bin number
!
! Units associated with coordinate values.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module fkfilt_module
      use pc_module
      use named_constants_module
      use mem_module            ! to use the memory allocation module.
      use mutehw_module
      use gather_module
      use fft_module
      use interp_module
      use lav_module
      implicit none
      private
      public :: fkfilt_create
      public :: fkfilt_initialize
      public :: fkfilt_update
      public :: fkfilt_delete
!<execute_only>
      public :: fkfilt            ! main execution (trace processing) routine.
      public :: fkfilt_wrapup
!</execute_only>


      character(len=100),public,save :: fkfilt_IDENT = &
'$Id: fkfilt.f90,v 1.23 2006/09/11 13:15:46 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: fkfilt_struct
        private
        logical                    :: skip_wrapup      ! wrapup flag.
        integer                    :: num_tr_in        ! process parameter
        character(len=6)           :: type_filt        ! process parameter
        integer                    :: hdr_panel        ! process parameter
        integer                    :: panel_init       ! process parameter
        integer                    :: panel_inc        ! process parameter
        real,dimension(:),pointer  :: coordinates(:)   ! process parameter
        real,dimension(:),pointer  :: factors(:)       ! process parameter

        character(len=64)          :: coord_units      ! dependent display
        integer                    :: xform_vert_size  ! dependent display
                                                       ! fft size in time
                                                       ! derived from ndpt.
        integer                    :: xform_hor_size   ! dependent display
                                                       ! fft size in space
                                                       ! derived from num_tr_in

        integer                    :: ndpt             ! global parameter
        integer                    :: nwih             ! global parameter
        real                       :: dt               ! global parameter
        real                       :: tstrt            ! global parameter

        integer                    :: coordinates_cnt  ! dependent variable
                                                       ! number of elements
                                                       ! in coordinates and
                                                       ! factors array.
        integer                    :: panel_ntr        ! dependent variable
                                                       ! number of traces
                                                       ! in current gather
        integer                    :: panel_next       ! dependent variable
                                                       ! next output trace num

        integer                    :: nt_fft_cpx       ! dependent variable
                                                       ! = 1+xform_vert_size/2
        integer                    :: nt_fft_real      ! dependent variable
                                                       ! = 2*nt_fft_cpx
        real                       :: dip_factor       ! dependent variable
        double precision  ,pointer :: hd2(:,:)         ! dependent variable
                                                       ! D1 = NWIH
                                                       ! D2 = num_tr_in
        real              ,pointer :: tr2(:,:)         ! dependent variable
                                                       ! D1 = nt_fft_real
                                                       !   xform_vert_size
                                                       !     real elements
                                                       !     are valid, which
                                                       !     is one or two less
                                                       !     than nt_fft_real.
                                                       !   nt_fft_cpx
                                                       !     complex elements
                                                       !     are valid.
                                                       ! D2 = xform_hor_size
        real, dimension(:),pointer :: filt             ! dependent variable
                                                       ! D1 = xform_hor_size
        real, dimension(:),pointer :: xscr             ! dependent variable
                                                       ! D1 = coordinates_cnt
        real, dimension(:),pointer :: scr              ! dependent variable
                                                       ! D1 = 2*xform_hor_size
        real, dimension(:),pointer :: scr1             ! dependent variable
                                                       ! D1 = xform_hor_size =
                                                       ! D1 = ndpt
        integer                    :: ltap             ! dependent variable
                                                       ! taper length
        real, dimension(:),pointer :: tap              ! dependent variable
                                                       ! D1 = ltap
        type(gather_struct),pointer :: gather_proc
        type(fft_2d_struct),pointer :: fft_2d_forward
        type(fft_2d_struct),pointer :: fft_2d_inverse
      end type fkfilt_struct

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(fkfilt_struct),pointer,save :: object      ! needed for traps.

      character(len=6),dimension(3),parameter :: type_filt_options = &
        (/'DIP   ','WAVE  ','DEMULT'/)

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine fkfilt_create (obj)
      implicit none
      type(fkfilt_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify  (obj%coordinates)
      nullify  (obj%factors)
      nullify  (obj%hd2)
      nullify  (obj%tr2)
      nullify  (obj%filt)
      nullify  (obj%xscr)
      nullify  (obj%scr)
      nullify  (obj%scr1)
      nullify  (obj%tap)

      nullify  (obj%gather_proc)
      nullify  (obj%fft_2d_forward)
      nullify  (obj%fft_2d_inverse)

      call fkfilt_initialize (obj)
      return
      end subroutine fkfilt_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine fkfilt_delete (obj)
      implicit none
      type(fkfilt_struct),pointer :: obj       ! arguments

!<execute_only>
      call fkfilt_wrapup (obj)
!</execute_only>

      call mem_free (obj%coordinates)
      call mem_free (obj%factors)
      call mem_free (obj%hd2)
      call mem_free (obj%tr2)

      call mem_free (obj%filt)
      call mem_free (obj%xscr)
      call mem_free (obj%scr)
      call mem_free (obj%scr1)
      call mem_free (obj%tap)

      if (associated(obj%gather_proc)) &
        call gather_delete (obj%gather_proc)
      if (associated(obj%fft_2d_forward)) &
        call fft_delete_2d (obj%fft_2d_forward)
      if (associated(obj%fft_2d_inverse)) &
        call fft_delete_2d (obj%fft_2d_inverse)

      deallocate(obj)
      return
      end subroutine fkfilt_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine fkfilt_initialize (obj)
      implicit none
      type(fkfilt_struct),intent(inout) :: obj       ! arguments

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      obj%num_tr_in = 100
      obj%type_filt = "DIP"
      obj%hdr_panel = 3
      obj%panel_init = 1.0
      obj%panel_inc = 1.0

      obj%coordinates_cnt = 0

      obj%coord_units= "millisconds/trace"
      obj%xform_vert_size=fft_nfctr(obj%ndpt)
      obj%xform_hor_size=fft_nfctr(obj%num_tr_in)

      obj%panel_ntr= 0
      obj%panel_next= 1
      obj%nt_fft_cpx= 1 + obj%xform_vert_size / 2
      obj%nt_fft_real= 2 * obj%nt_fft_cpx
      obj%dip_factor= 0.0
      obj%ltap= 0

      call fkfilt_update (obj)
      return
      end subroutine fkfilt_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine fkfilt_update (obj)
      implicit none
      type(fkfilt_struct),intent(inout),target :: obj             ! arguments

      integer :: state
      logical :: verify
      integer :: coordinates_cnt2
      integer :: ier1, j, coord_do, fft_size_2d(2)
      real :: tpl, fac1, coord, coord_lower, coord_upper

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("coordinates_arrayset", (/  &
                                    "coordinates",              &
                                    "factors    " /))

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      call pc_get ('num_tr_in', obj%num_tr_in)

      call pc_get ('type_filt', obj%type_filt)
      call string_to_upper (obj%type_filt)

      call pc_get ('hdr_panel', obj%hdr_panel)
      call pc_get ('panel_init', obj%panel_init)
      call pc_get ('panel_inc', obj%panel_inc)

      coordinates_cnt2 = obj%coordinates_cnt
      call pc_alloc ('coordinates', obj%coordinates, obj%coordinates_cnt)
      call pc_alloc ('factors', obj%factors, coordinates_cnt2)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      obj%xform_vert_size=fft_nfctr(obj%ndpt)
      obj%xform_hor_size=fft_nfctr(obj%num_tr_in)

      if(obj%num_tr_in < 1) then
        call pc_error('Invalid NUM_TR_IN.  Must be > 0')
        obj%num_tr_in = 100
      end if

      if(all(type_filt_options /= obj%type_filt)) then
        call pc_error( &
          'Invalid TYPE_FILT value. Valid values are DIP, WAVE, DEMULT')
        obj%type_filt = 'DIP'
      end if

      if(obj%hdr_panel < 1 .or. obj%hdr_panel > obj%nwih) then
        call pc_error('Invalid HDR_PANEL.  Must be > 0 and < NWIH')
        obj%hdr_panel = 3
      end if

      if(obj%panel_inc == 0 .or. obj%panel_inc == FNIL) then
        call pc_error('Invalid PANEL_INC.  Must be non-zero')
        obj%panel_inc = 1.0
      end if

      if(obj%coordinates_cnt /= coordinates_cnt2) then
        call pc_error('COORDINATES and FACTORS arrays are not linked')
        obj%coordinates_cnt = min(obj%coordinates_cnt, coordinates_cnt2)
      end if

      if(obj%type_filt == 'DEMULT') then
        if(obj%coordinates_cnt /= 2) then
          call mem_realloc(obj%coordinates,2)
          call mem_realloc(obj%factors,2)
          obj%coordinates_cnt = 2
        end if

        obj%coordinates(1) = 0.0
        obj%coordinates(2) = 1.0
        obj%factors(1) = 1.0
        obj%factors(2) = 0.0
      end if

      if(obj%type_filt == 'DIP') then
        ! Old CPS defined a dip FACT=FLOAT(NFR)/1000.0*DT*FLOAT(NXFT).
        ! NFR is defined by the loop where FACT is scales COORD.
        ! Old coordinates were supposedly in milliseconds/trace.
        ! I believe the old FACT or documentation for it is wrong.

        ! New CPS coordinates use seconds/trace and defines dip_factor.
        ! NFR is defined by a loop and NFR * dip_factor scale coordinates.
        obj%dip_factor = obj%xform_hor_size / (obj%xform_vert_size * obj%dt)

        coord_upper = (0.001 + obj%xform_hor_size / 2.0) / obj%dip_factor
        coord_lower = - coord_upper

        write(obj%coord_units,*) 'seconds/trace, max= +/- ', coord_upper
      else
        ! assume type_filt is 'WAVE' or 'DEMULT'
        obj%dip_factor = 0.0

        coord_upper = 0.001 + obj%xform_hor_size / 2.0
        coord_lower = - coord_upper

        write(obj%coord_units,*) 'fft bin number, max= +/- ', coord_upper
      end if

      if(verify .or. pc_verify_arrayset('coordinates_arrayset')) then
        if(obj%coordinates_cnt <= 0) then
          call pc_error('one or more COORDINATES and FACTORS required.')
          obj%coordinates_cnt = 0
        end if

        do coord_do = 1, obj%coordinates_cnt
          coord = obj%coordinates(coord_do)

          if(coord == FNIL .or. &
             obj%factors(coord_do) == FNIL) then
             call pc_error('COORDINATES or FACTORS array element ', coord_do, &
               ', needs a value')
          else if(coord < coord_lower .or. coord > coord_upper) then
            call pc_error('COORDINATES must be between +/- ', coord_upper)
          else if(coord_do > 1) then

            if(coord <= obj%coordinates(coord_do - 1)) then
              call pc_error('COORDINATES at element ', coord_do, &
                ', is not increasing')
            end if
          end if
        end do
      end if

      obj%xform_vert_size = fft_nfctr(obj%ndpt)
      obj%xform_hor_size = fft_nfctr(obj%num_tr_in)

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

      call pc_clear()

      call pc_put_process ('num_tr_max', obj%num_tr_in)

      if (associated(obj%gather_proc)) then
           call gather_update (obj%gather_proc)
      else
           call gather_create (obj%gather_proc)
      end if

!     gather_nstore   = 0     ! just in case gather_update does not set this.
!     gather_nscratch = 0     ! just in case gather_update does not set this.
!     call pc_get_control ("nstore"  , gather_nstore)    ! to add to yours.
!     call pc_get_control ("nscratch", gather_nscratch)  ! to add to yours.

      call pc_restore

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_global  ('gathered', .false.)
      call pc_put_global  ('numtr', 1)

      call pc_put ('num_tr_in', obj%num_tr_in)

      call pc_put_options_field('type_filt', type_filt_options, 3)
      call pc_put ('type_filt', obj%type_filt)

      call pc_put ('hdr_panel', obj%hdr_panel)
      call pc_put ('panel_init', obj%panel_init)
      call pc_put ('panel_inc', obj%panel_inc)
      call pc_put ('coordinates', obj%coordinates, obj%coordinates_cnt)
      call pc_put ('factors', obj%factors, obj%coordinates_cnt)

      call pc_put ('coord_units', obj%coord_units)
      call pc_put ('xform_vert_size', obj%xform_vert_size)
      call pc_put ('xform_hor_size', obj%xform_hor_size)

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'  , .true.)

      call pc_put_sensitive_field_flag('coord_units', .false.)
      call pc_put_sensitive_field_flag('xform_vert_size', .false.)
      call pc_put_sensitive_field_flag('xform_hor_size', .false.)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      if(obj%type_filt /= 'DIP') then
        ! Adjust wave number filter values
        obj%coordinates = obj%coordinates + obj%xform_hor_size / 2
      end if

      obj%nt_fft_cpx = 1 + obj%xform_vert_size / 2
      obj%nt_fft_real = 2 * obj%nt_fft_cpx

      tpl = 0.06
      obj%ltap = tpl / obj%dt - 0.000001

      call mem_alloc (obj%hd2, obj%nwih, obj%num_tr_in)
      call mem_alloc (obj%tr2, obj%nt_fft_real, obj%xform_hor_size)

      call mem_alloc (obj%filt,   obj%xform_hor_size)
      call mem_alloc (obj%xscr,   obj%coordinates_cnt)
      call mem_alloc (obj%scr,  2*obj%xform_hor_size)
      call mem_alloc (obj%scr1,   obj%xform_hor_size)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      fft_size_2d(1) = obj%xform_vert_size
      fft_size_2d(2) = obj%xform_hor_size

      ier1 = fft_create_2d(obj%fft_2d_forward, fft_size_2d, +1, 'rtoc', .true.)

      ier1 = fft_create_2d(obj%fft_2d_inverse, fft_size_2d, -1, 'ctor', .true.)

      call mem_alloc (obj%tap,   obj%ltap)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      fac1 = PI * obj%dt / (2.0 * tpl)

      do j = 1, obj%ltap
        obj%tap(j) = sin(fac1 * j) ** 2
      end do

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine fkfilt_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine fkfilt (obj,ntr,hd,tr,hdo,tro)
      implicit none
      type(fkfilt_struct),intent(inout) :: obj                  ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
      double precision ,intent(out)   :: hdo(:,:)               ! arguments
      real             ,intent(out)   :: tro(:,:)               ! arguments

      integer :: imut, ntr_do
      if(ntr >= 0 .or. &
        (ntr == NEED_TRACES .and. obj%panel_next > obj%panel_ntr)) then
        if(ntr > 0) then
          do ntr_do = 1, ntr
            ! save the original gather value in a scratch header word
            hd(HDR_SCRATCH_60,ntr_do) = hd(3,ntr_do)

            ! compute a gather value based upon desired paneling
            hd(3,ntr_do) = nint((hd(obj%hdr_panel,ntr_do) - obj%panel_init) / &
              obj%panel_inc)
          end do
        end if

        call gather(obj%gather_proc, ntr, hd, tr, obj%hd2, obj%tr2)

        if(ntr == NEED_TRACES) then
          ! gather needs more traces before proceeding
          return
        else if(ntr > 0) then
          ! zero pad the traces provided by gather
          obj%tr2(obj%ndpt+1:obj%xform_vert_size,:ntr) = 0.0

          ! zero the traces not provided by gather
          obj%tr2(:obj%xform_vert_size,ntr+1:obj%xform_hor_size) = 0.0

          ! forward transform, filter and inverse transform
          call fkfilt_xfx(obj)

          obj%panel_ntr = ntr
          obj%panel_next = 1
          ntr = NEED_TRACES
        else
          call fkfilt_wrapup(obj)
        end if
      end if

      if(ntr == NEED_TRACES) then
        ! return the next output trace to the caller
        hdo(1:obj%nwih,1) = obj%hd2(1:obj%nwih,obj%panel_next)

        ! restore the original gather value
        hdo(3,1) = obj%hd2(HDR_SCRATCH_60,obj%panel_next)

        tro(1:obj%ndpt,1) = obj%tr2(1:obj%ndpt,obj%panel_next)

        ! honor head and tail mutes
        call mutehw(hdo(:,1), tro(:,1), obj%ndpt, 0.0, MUTEHW_BOTH)

        ! set header LAV (dead trace flag)
        call lav_set_hdr(hdo(:,1), tro(:,1), obj%ndpt)

        ! head mute taper
        IMUT = NINT(hdo(2,1)) - 1 

        tro(IMUT+MAX(1,1-IMUT):MIN(obj%ltap,obj%NDPT-IMUT)+IMUT,1) = &
          obj%tap(MAX(1,1-IMUT):MIN(obj%ltap,obj%NDPT-IMUT))* &
          tro(IMUT+MAX(1,1-IMUT):MIN(obj%ltap,obj%NDPT-IMUT)+IMUT,1) 

        ! tail mute taper
        IMUT = NINT(hdo(64,1)) + 1 

        tro(IMUT-MAX(1,IMUT-obj%NDPT):IMUT-MIN(obj%ltap,IMUT-1):(-1),1) = &
          obj%tap(MAX(1,IMUT-obj%NDPT):MIN(obj%ltap,IMUT-1))* &
          tro(IMUT-MAX(1,IMUT-obj%NDPT):IMUT-MIN(obj%ltap,IMUT-1):(-1),1) 

        obj%panel_next = obj%panel_next + 1
        ntr = 1
      end if

      return
      end subroutine fkfilt

!</execute_only>

!!---------------------------- fkfilt_xfx ---------------------------------!!
!!---------------------------- fkfilt_xfx ---------------------------------!!
!!---------------------------- fkfilt_xfx ---------------------------------!!

!<execute_only>

      subroutine fkfilt_xfx(obj)
      implicit none

      ! forward transform, filter and inverse transform
      ! Note: fkfilt_xfx is NOT a Fortran 90 module because the tr2
      ! array must be manipulated as a real and complex array.

      type(fkfilt_struct) :: obj

      integer :: ier, nfr
      real :: scale



      scale = 1.0 / (obj%nt_fft_real * obj%xform_hor_size)

      ! forward transform
      ier = fft_rc_2d(obj%fft_2d_forward, obj%tr2, opt_scale = scale)

      do NFR = 1, obj%nt_fft_cpx
        if(obj%type_filt /= 'DIP' .and. NFR == 1) then
          ! build the wave number filter once and once only.
          CALL interp_1d_var_lin_real (obj%coordinates, obj%factors, &
            obj%coordinates_cnt, obj%scr, obj%scr1, &
            obj%xform_hor_size, 1.0, FLOAT(obj%xform_hor_size))

          ! move filter to match FFT output format
          obj%filt(:obj%xform_hor_size/2) = &
            obj%scr1(obj%xform_hor_size/2:1:(-1))

          obj%filt(obj%xform_hor_size/2+1:obj%xform_hor_size) = &
            obj%scr1(obj%xform_hor_size: &
              obj%xform_hor_size+1-obj%xform_hor_size/2:(-1))
        end if

        if(obj%type_filt == 'DIP') then
          !rebuild the dip filter for each frequency
          ! convert dips to fft bin
          obj%xscr(:obj%coordinates_cnt) = &
            obj%coordinates(:obj%coordinates_cnt)*NFR*obj%dip_factor + &
              obj%xform_hor_size/2

          CALL interp_1d_var_lin_real (obj%xscr, obj%factors, &
            obj%coordinates_cnt, obj%scr, obj%scr1, &
            obj%xform_hor_size, 1.0, FLOAT(obj%xform_hor_size))

          ! move filter to match FFT output format
          obj%filt(:obj%xform_hor_size/2) = &
            obj%scr1(obj%xform_hor_size/2:1:(-1))

          obj%filt(obj%xform_hor_size/2+1:obj%xform_hor_size) = &
            obj%scr1(obj%xform_hor_size: &
              obj%xform_hor_size+1-obj%xform_hor_size/2:(-1))
        end if

        ! apply the filter
        call fkfilt_apply(NFR, obj%nt_fft_cpx, obj%xform_hor_size, &
         obj%tr2, obj%filt)
      end do

      ! inverse transform
      ! alternative to...  ier = fft_cr_2d(obj%fft_2d_inverse, obj%tr2)
      call fkfilt_fft_kludge(obj%fft_2d_inverse, &
        obj%nt_fft_cpx, obj%xform_hor_size, obj%tr2)

      end subroutine fkfilt_xfx

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine fkfilt_wrapup (obj)
      implicit none
      type(fkfilt_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (associated(obj%gather_proc)) call gather_wrapup (obj%gather_proc)

      return
      end subroutine fkfilt_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module fkfilt_module

      ! The following subroutines MUST be outside the Fortran module.

      ! Fortran 90 does not allow pointer equivalencing unless the
      ! the array shape AND TYPE are the same.
      ! Fortran 90 does not allow the array shape OR TYPE to change
      ! between the actual arguments and dummy arguments when calling
      ! another MODULE, but does allow it for non-module subroutines.

!!----------------------------- fkfilt_apply -------------------------------!!
!!----------------------------- fkfilt_apply -------------------------------!!
!!----------------------------- fkfilt_apply -------------------------------!!

      ! Note: the goal is to apply a real filter to complex traces.
      subroutine fkfilt_apply(NFR, nt_fft_cpx, xform_hor_size, &
        tr2_cpx, filt)
      implicit none
      integer :: NFR, nt_fft_cpx, xform_hor_size
      ! Note: the calling argument type changes from real to complex.
      complex, dimension(nt_fft_cpx,xform_hor_size) :: tr2_cpx
      real, dimension(xform_hor_size) :: filt

      integer :: i

      do i = 1, xform_hor_size
        tr2_cpx(NFR,i) = filt(i) * tr2_cpx(NFR,i)
      end do

      end subroutine fkfilt_apply

!!--------------------------- fkfilt_fft_kludge ----------------------------!!
!!--------------------------- fkfilt_fft_kludge ----------------------------!!
!!--------------------------- fkfilt_fft_kludge ----------------------------!!

      ! Note: The goal is to do a 2D fft in-place.

      ! The real-to-complex fft subroutine will accepts a real array.
      ! The complex-to-real fft subroutine wants a complex array.

      ! The following subroutine kludge changes the 2d array type from
      ! real to complex, where the actual fft call is made.

      subroutine fkfilt_fft_kludge(fft_2d_inverse, &
        nt_fft_cpx, xform_hor_size, tr2_cpx)
      use fft_module

      implicit none

      type(fft_2d_struct) :: fft_2d_inverse
      integer nt_fft_cpx, xform_hor_size
      ! Note: the calling argument type changes from real to complex.
      complex tr2_cpx(nt_fft_cpx,xform_hor_size)

      integer ier

      ier = fft_cr_2d(fft_2d_inverse, tr2_cpx)

      end subroutine fkfilt_fft_kludge

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

