!<CPS_v1 type="PROCESS"/>
!!------------------------------- elev.f90 ---------------------------------!!
!!------------------------------- elev.f90 ---------------------------------!!
!!------------------------------- elev.f90 ---------------------------------!!

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
! Name       : ELEV  (Elevation information entry process)
! Category   : headers
! Written    : 1989-06-01   by: George Harney
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Enter elevation information in selected header word.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! ELEV writes elevation information in header word HDR_ELEV (typically 19).  
! Elevation values are entered in the ELEVATIONS array and values of header word
! HDR_TR (used to identify trace location) are entered in the TR_VALUES array.
! These arrays are linked and associate particular elevations with particular
! trace locations.
!
! Elevation values written in HDR_ELEV are interpolated between values entered
! in ELEVATIONS and are extrapolated outside the range of entries.
!
!
! Elevation or Water Depth
!
! If TYPE_ENTRY = WD, then ELEVATIONS are considered to be water depths entered
! as positive numbers; ELEV changes the sign internally to conform to the CPS 
! convention for water depth.
!
! If TYPE_ENTRY = ELEV, then ELEV does not change the sign of ELEVATIONS
! entries.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! ELEV is normally used when elevation information is not available when the 
! geometry description is written.
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!

!
! This process does not alter input traces (only headers).
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed
! NWIH      number of words in trace header         used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
!         HDR_TR    header word used for interpolation
!         HDR_ELEV  header word used for elevation entry
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author      Description
!     ----        ------      -----------
! 10. 2006-09-11  Stoeckley   Add call to pc_register_array_names for SeisSpace.
!  9. 2001-06-04  Brad Kruse  Change name for wrapup flag to SKIP_WRAPUP for
!                             clarity, and slightly change how it is used.
!  8. 2000-06-13  Brad Kruse  Force elevations to be negative, if TYPE_ENTRY
!                              equals "WD"
!  7. 2000-05-02  Brad Kruse  Initial CPS conversion.
!  6. 1998-12-15  Vunderink   Begin using the f90 compiler.
!  5. 1990-04-12  Goodger     Use next sequential trace header if TRNM is  
!                              not found.
!  4. 1990-02-08  Peterson    Remove the + or - .5 on elevation calculation
!                              because elevations are flt. pt. and are no
!                              longer interpolated to the nearest integer.
!  3. 1989-10-02  Peterson    Fix bug in parameter allocation. ISPACE
!  2. 1989-06-08  Howard      Fix bug in PUTP-GETP-REPP call.
!  1. 1989-06-01  Harney      Original Version.  
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
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
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS ELEV Process/NC=80/NR=22>
!
!                  Elevation information entry process Process
!              Enter elevation information in selected header word.
!
!        Trace location header word: [HDR_TR]`IIIIII
!
!        Header word to record elev: [HDR_ELEV]`IIIIII
!          Recorded elevation units: [UNITS_HDR_ELEV]`CCCCCC
!
!        Trace mapping table
!
!          Elevation table units:       Elevation type:
!          [UE]`CCCCCC                  [TE]`CCCCCCCCCCCCCCCCCCCCCC
!
!          Trace Values   Elevations      
!          `FFFFFFFFFFFF- `FFFFFFFFFFFF--------
!          `FFFFFFFFFFFF- `FFFFFFFFFFFF--------
!          `FFFFFFFFFFFF- `FFFFFFFFFFFF--------
!          `FFFFFFFFFFFF- `FFFFFFFFFFFF--------
!          `FFFFFFFFFFFF- `FFFFFFFFFFFF--------
!
!<PARMS TraceValues_Arrayset[/xsf/yst]>
!<PARMS Trace Values[TR_VALUES/r]>
!<PARMS UE[UNITS_ELEVATIONS]>
!<PARMS TE[TYPE_ENTRY]>
!</gui_def>
!
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="TYPE_ENTRY">
!<Tip> Whether entries are elevations or water depths. </Tip>
! Default = WD
! Allowed = WD    (Entries are water depths.)
! Allowed = ELEV  (Entries are elevations.)
! Water depths are entered as positive numbers; ELEV changes the sign to 
! conform to the CPS standard for water depth.  ELEV does not change the sign 
! of elevation entries.
!</Help>
!
!<Help KEYWORD="HDR_TR">
!<Tip> Header word to use for identifying traces. </Tip>
! Default = 7
! Allowed = 1 - NWIH
! Header words 1, 7 or 37 are typically used.
!</Help>
!
!<Help KEYWORD="HDR_ELEV">
!<Tip> Header word to use for elevation entries. </Tip>
! Default = 19
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="UNITS_HDR_ELEV">
!<Tip> Data units for elevation header word entries. </Tip>
! Default = METERS
! Allowed = FEET
! Allowed = METERS
! The values from the ELEVATIONS list will be converted to UNITS_HDR_ELEV
! if needed.
!</Help>
!
!<Help KEYWORD="TR_VALUES">
!<Tip> Array of values of header word HDR_TR linked to elevation entries. </Tip>
! Default = -
! Allowed = real (linked array)
! TR_VALUES and ELEVATIONS are linked arrays associating elevation entries with 
! trace location.  Elevation values written in HDR_ELEV are interpolated 
! between values entered in ELEVATIONS and are extrapolated outside the range 
! of entries.
!</Help>
!
!<Help KEYWORD="ELEVATIONS">
!<Tip> Array of elevation entries linked to values of HDR_TR. </Tip>
! Default = -
! Allowed = real (linked array)
! TR_VALUES and ELEVATIONS are linked arrays associating elevation entries with 
! trace location.  Elevation values written in HDR_ELEV are interpolated 
! between values entered in ELEVATIONS and are extrapolated outside the range 
! of entries. 
!</Help>
!
!<Help KEYWORD="UNITS_ELEVATIONS">
!<Tip> Data units for array of elevation entries. </Tip>
! Default = METERS
! Allowed = FEET
! Allowed = METERS
! The values entered in the ELEVATION list will be converted from 
! UNITS_ELEVATIONS to UNITS_HDR_ELEV if needed.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module elev_module
  !
  ! - Module reference
  !
  use pc_module
  use named_constants_module
  !
  use sort_module, only:    &
      sort_insisort
  !
  implicit none
  !
  private
  !
  public :: elev_create
  public :: elev_initialize
  public :: elev_update
  public :: elev_delete
!<execute_only>
  public :: elev            ! main execution (trace processing) routine.
  public :: elev_wrapup
!</execute_only>

  character(len=100),public,save :: ELEV_IDENT = &
    '$Id: elev.f90,v 1.10 2006/09/11 13:15:44 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

  integer, parameter :: max_points = 100
  !
  type,public :: elev_struct              
    !
    private
    !
    logical                    :: skip_wrapup       ! wrapup flag.
    !
    ! - Process parameters
    !
    double precision     :: elevations (max_points)
    double precision     :: tr_values  (max_points)
    character (len = 20) :: type_entry
    character (len =  6) :: units_elevations
    character (len =  6) :: units_hdr_elev
    integer              :: hdr_elev           ! pc - 1 - NWIH
    integer              :: hdr_tr             ! pc - 1 - NWIH
    !
    ! - Processing values
    !
    integer              :: num_elev
    real                 :: sign
    real                 :: conversion_factor
    !
    ! - Globals
    !
    integer              :: nwih        ! globals.  
    !
  end type elev_struct


  !!---------------------------- interfaces -------------------------------!!
  !!---------------------------- interfaces -------------------------------!!
  !!---------------------------- interfaces -------------------------------!!


  !!------------------------------- data -----------------------------------!!
  !!------------------------------- data -----------------------------------!!
  !!------------------------------- data -----------------------------------!!

  character (len=4)     :: type_entry         ! pc - WD, ELEV

  integer,           parameter :: num_entry_options = 2
  character (len=*), parameter :: entry_wd   = 'Table is Water Depth'
  character (len=*), parameter :: entry_elev = 'Table is Elevation  '
  
  character (len=*), parameter :: entry_options (num_entry_options)    &
    = (/entry_wd, entry_elev/)

  integer,           parameter :: num_units_options = 2
  character (len=6), parameter :: units_feet   = 'Feet  '
  character (len=6), parameter :: units_meters = 'Meters'
  
  character (len=6), parameter :: units_options (num_entry_options)    &
    = (/units_feet, units_meters/)

  real, parameter, public :: FEET_TO_METERS = 0.30480  ! Meters in one foot
  real, parameter, public :: METERS_TO_FEET = 3.28084  ! Feet in one meter

contains


  !!----------------------------- create -----------------------------------!!
  !!----------------------------- create -----------------------------------!!
  !!----------------------------- create -----------------------------------!!

  subroutine elev_create (obj)
    !
    ! - Arguments
    !
    type(elev_struct),pointer :: obj
    !
    ! - Begin elev_create
    !
    allocate (obj)
    !
    call elev_initialize (obj)
    !
  end subroutine elev_create


  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!

  subroutine elev_delete (obj)
    !
    ! - Arguments
    !
    type (elev_struct), pointer :: obj       ! arguments
    !
    ! - Begin elev_delete
    !

!<execute_only>
      call elev_wrapup (obj)
!</execute_only>

    deallocate(obj)
    !
  end subroutine elev_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


  subroutine elev_initialize (obj)
    !
    ! - Arguments
    !
    type(elev_struct),intent(inout) :: obj       ! arguments
    !
    ! - Begin elev_initialize
    !
    obj%type_entry        = entry_wd
    obj%units_elevations  = units_meters
    obj%units_hdr_elev    = units_meters
    obj%hdr_tr            = 7
    obj%hdr_elev          = 19
    !
    obj%nwih              = HDR_BOTTOM_MUTE
    !
    obj%sign              = 1.0
    obj%conversion_factor = 1.0
    obj%skip_wrapup       = .true.
    obj%num_elev          = 0
    obj%elevations        = 0.0d0
    obj%tr_values         = 0.0d0
    !
    call elev_update (obj)
    !
  end subroutine elev_initialize


  !!------------------------- start of update ------------------------------!!
  !!------------------------- start of update ------------------------------!!
  !!------------------------- start of update ------------------------------!!

  subroutine elev_update (obj)
    !
    ! - Arguments
    !
    type (elev_struct), intent (inout) :: obj             ! arguments
    !
    ! - Local variables
    !
    integer :: i
    integer :: indices (max_points)
    integer :: temp    (max_points)
    !
    ! - Begin elev_update
    !
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


    !!------------------------- read parameters ----------------------------!!
    !!------------------------- read parameters ----------------------------!!
    !!------------------------- read parameters ----------------------------!!


      call pc_register_array_names ("tracevalues_arrayset", (/  &
                                    "tr_values ",               &
                                    "elevations" /))

    call pc_get_global ('NWIH', obj%nwih)  ! number of header words.
    !
    ! - Process parameters
    !
    call pc_get ('units_elevations', obj%units_elevations)
    !
    call pc_get ('units_hdr_elev', obj%units_hdr_elev)
    !
    call pc_get ('type_entry', obj%type_entry)
    !
    call pc_get ('hdr_elev',   obj%hdr_elev)
    call pc_get ('hdr_tr',     obj%hdr_tr)
    call pc_get ('elevations', obj%elevations, obj%num_elev)
    call pc_get ('tr_values',  obj%tr_values,  obj%num_elev)


    !!------------------------ verify parameters ---------------------------!!
    !!------------------------ verify parameters ---------------------------!!
    !!------------------------ verify parameters ---------------------------!!

    !
    select case (obj%type_entry)
      case (entry_wd, 'WD')
        obj%sign = -1
        obj%elevations = -1.0d0 * abs (obj%elevations)
      case (entry_elev, 'ELEV')
        obj%sign = +1
      case default
        obj%sign = 0
        call pc_error ("ELEV:  Invalid TYPE_ENTRY " // obj%type_entry)
        call pc_error ("TYPE_ENTRY must be 'WD' or 'ELEV'");
    end select
    !
    select case (obj%units_elevations // obj%units_hdr_elev)
      case (units_feet // units_feet,    &
            units_meters // units_meters)
        obj%conversion_factor = 1.0
      case (units_meters // units_feet)
        obj%conversion_factor = METERS_TO_FEET
      case (units_feet // units_meters)
        obj%conversion_factor = FEET_TO_METERS
      case default
        obj%conversion_factor = 0
        call pc_error ("ELEV:  Invalid unit conversion units_hdr_elev: "    &
                        // obj%units_hdr_elev)
        call pc_error ("ELEV:  units_elevations:" // obj%units_elevations)
        call pc_error ("ELEV:  Both must be "     &
                       // units_meters // " or " // units_feet)
    end select


    !!-------------------- call processes internally -----------------------!!
    !!-------------------- call processes internally -----------------------!!
    !!-------------------- call processes internally -----------------------!!


    !!----------------------- write parameters -----------------------------!!
    !!----------------------- write parameters -----------------------------!!
    !!----------------------- write parameters -----------------------------!!

    !
    ! - Global parameters
    !
    call pc_put_global ('NWIH', obj%nwih)  ! number of header words.
    !
    ! - Control parameters
    !
    call pc_put_control ('ntapes'      , 0)
    call pc_put_control ('need_request', .false.)
    call pc_put_control ('need_label'  , .false.)
    call pc_put_control ('twosets'     , .false.)
    call pc_put_control ('nscratch'    , 0)
    call pc_put_control ('nstore'      , 0) 
    call pc_put_control ('iftd'        , .false.)
    call pc_put_control ('ndisk'       , 0)
    call pc_put_control ('setup_only'  , .false.)
    !
    ! - Process parameters
    !
    call pc_put_options_field ('TYPE_ENTRY',          &
                               entry_options,    &
                               num_entry_options)
    call pc_put  ('TYPE_ENTRY', obj%type_entry)
    !
    call pc_put_options_field ('UNITS_HDR_ELEV',          &
                               units_options,    &
                               num_units_options)
    call pc_put  ('UNITS_HDR_ELEV', obj%units_hdr_elev)
    !
    call pc_put_options_field ('UNITS_ELEVATIONS',          &
                               units_options,    &
                               num_units_options)
    call pc_put  ('UNITS_ELEVATIONS', obj%units_elevations)
    !
    call pc_put ('hdr_elev',   obj%hdr_elev)
    call pc_put ('hdr_tr',     obj%hdr_tr)
    call pc_put ('elevations', obj%elevations, obj%num_elev)
    call pc_put ('tr_values',  obj%tr_values,  obj%num_elev)

    !!---------------------- prepare for execution -------------------------!!
    !!---------------------- prepare for execution -------------------------!!
    !!---------------------- prepare for execution -------------------------!!


!<execute_only>

    if (pc_do_not_process_traces()) return
    !
    obj%skip_wrapup = .false.
    !
    if (obj%num_elev > 1) then
      !
      ! - order the trace-values lookup table
      !
      do i = 1, obj%num_elev
        indices (i) = i
      end do
      !
      call sort_insisort (a = real (obj%tr_values),    &
                          i = indices,          & 
                          p = 1,                &
                          q = obj%num_elev)
      !
      ! - 'unwrap' the indexes of the sort
      !
      temp (1 : obj%num_elev) = obj%tr_values (indices (1 : obj%num_elev))
      obj%tr_values (1 : obj%num_elev) = temp (1 : obj%num_elev)
      !
      temp (1 : obj%num_elev) = obj%elevations (indices (1 : obj%num_elev))
      obj%elevations (1 : obj%num_elev) = temp (1 : obj%num_elev)
      !
    end if

!</execute_only>


    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!

    !
  end subroutine elev_update


  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!


  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!

!<execute_only>

  subroutine elev (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type (elev_struct), intent (inout) :: obj
    integer,            intent (in)    :: ntr
    double precision,   intent (inout) :: hd(:,:)
    real,               intent (in)    :: tr(:,:)
    !
    ! - Local variables
    !
    double precision :: elevation
    double precision :: e_span
    double precision :: factor
    double precision :: tr_val
    integer          :: bottom
    integer          :: elev_pos
    integer          :: mid
    integer          :: n
    integer          :: top
    !
    ! - Begin elev
    !
    if (ntr >= 1) then
      !
    loop_thru_traces:    &
      do n = 1, ntr
        !
        tr_val = hd (obj%hdr_tr, n)
        !
        if (obj%num_elev < 1) then
          !
          elevation = 0.0
          !
        else if (tr_val >= obj%tr_values (obj%num_elev)) then
          !
          elevation = obj%elevations (obj%num_elev)
          !
          !
        else if ((obj%num_elev == 1)    &
                 .or. (tr_val <= obj%tr_values (1))) then
          !
          elevation = obj%elevations (1)
          !
        else
          !
          ! - Binary search to locate the trace value
          !
          top    = obj%num_elev
          bottom = 1
          !
        loop_binary_search:    &
          do
            !
            mid = (top + bottom) / 2

            !
            if (tr_val < obj%tr_values (mid)) then
              top = mid
            else if (tr_val > obj%tr_values (mid)) then
              bottom = mid
            else
              exit loop_binary_search
            end if
            !
            if ((top - bottom) <= 1) then
              mid = bottom
              exit loop_binary_search
            end if
            !
          end do loop_binary_search
          !
          elev_pos = mid
          !
          ! - Interpolate desired value from trace value location
          !
          if (tr_val < obj%tr_values (1)) then 
            !
            elevation = obj%elevations (1)
            !
          else if (tr_val >= obj%tr_values (obj%num_elev)) then
            !
            elevation = obj%elevations (obj%num_elev)
            !
          else
            !
            factor    = (tr_val - obj%tr_values (elev_pos))   &
                        / (obj%tr_values (elev_pos + 1)       &
                           - obj%tr_values (elev_pos))
            !
            e_span    = obj%elevations (elev_pos + 1)     &
                        - obj%elevations (elev_pos)
            !
            elevation = obj%elevations (elev_pos)     &
                        + e_span * factor
            !
          end if
          !
        end if 
        !
        ! - Set the elevation header word
        !
        hd (obj%hdr_elev, n) = elevation
        !
      end do loop_thru_traces
      !
    end if
    !
   if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
     call elev_wrapup (obj)
    end if
    !
  end subroutine elev

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

  subroutine elev_wrapup (obj)
    !
    ! - Arguments
    !
    type (elev_struct), intent (inout) :: obj       ! arguments
    !
    ! - Begin elev_wrapup
    !
    if (obj%skip_wrapup) return
    !
    obj%skip_wrapup = .true.
    !
  end subroutine elev_wrapup

!</execute_only>

  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!

end module elev_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

