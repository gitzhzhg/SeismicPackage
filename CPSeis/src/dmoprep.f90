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
! Name       : DMOPREP   (Prepare 3D Data for 2D Style DMO)
! Category   : sorts
! Written    : 1998-07-02   by: Vunderink, CI Burch, M Howard
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : DMOPREP is designed to prepare 3D data for a 2D style 3D DMO.
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
!
!
! DMOPREP rotates source and receiver coordinates so that the new azimuth is
! inline.  Neither offset nor midpoint coordinates are changed by this 
! rotation.  Surveyed and grid coordinates are changed for both sources and 
! receivers.
!
!
! DMOPREP is designed to prepare 3D data for a 2D style 3D DMO.  Advantages of
! using DMOPREP prior to DMO3D are:
!
!     1.  Sampling of broadcast traces in output bins is improved,
!
!     2.  Noise and artifacts are reduced, and
!
!     3.  Job run-time is reduced.
! 
! However, the accuracy of the DMO is reduced if DMOPREP is used.
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Grid coordinates should not be rounded prior to application of DMOPREP.
!
! DMOPREP should only be used to reduce artifacts in DMO3D output that would
! otherwise be unacceptable.
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
!
! Traces may be input in gathers or as single traces.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input trace headers.
! This process outputs the same traces as it receives.
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
! GATHERED  whether traces are a legitimate gather  used but not changed
! NDPT      number of sample values in trace        used but not changed
! GRID      grid transformation structure           used but not changed
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
! 
! 
! Source and receiver surveyed (easting and northing) coordinates and grid 
! coordinates (header words 11, 12, 14, 15, 33, 34, 35, 36) are changed.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author     Description
!     ----        ------     -----------
!013. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
! 12. 2006-01-10  B. Menger  Removed Unused Variables.
! 11. 2002-03-13  CI Burch   Documentation change only.
!                 Goodger    Disable all options except opt_rotate.
! 10. 2001-10-02  Stoeckley  Move trap subroutine to different location in code
!                             to make the intel compiler happy.
!  9. 2001-08-24  Stoeckley  Add file selection box and file status message.
!  8. 2000-12-08  Stoeckley  Change wrapup flag.
!  7. 2000-06-09  O'Brien    Full f90 conversion.
!  6. 1999-01-18  Vunderink  Begin using the f90 compiler.
!                            Moved from newlib to conlib.
!  5. 1998-07-20  Vunderink  Added parameters for partial NMO.
!  4. 1998-07-13  Vunderink  Changed method of regularizing traces
!  3. 1998-07-09  Vunderink  Fixed D_X calculation
!  2. 1998-07-08  Vunderink  Set Headers 1 and 4 on output traces
!  1. 1998-07-02  Vunderink  Original version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
!
! 
! 
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
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
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
!
!<gui_def>
!<NS DMOPREP Process/NC=80>
!
!  Prepare 3D Data for 2D Style DMO
!
!  This process has no parameters.
!
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!

!
!</HelpSection>

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module dmoprep_module

      use pc_module
      use nmo_module
      use sizeof_module
      use pathcheck_module
      use pathchoose_module

      implicit none

      private
      public :: dmoprep_create     ! uses the parameter cache.
      public :: dmoprep_initialize
      public :: dmoprep_update     ! uses the parameter cache.
      public :: dmoprep_delete

!<execute_only>

      public :: dmoprep            ! main execution (trace processing) routine.
      public :: dmoprep_wrapup

!</execute_only>

      character(len=100),public,save :: DMOPREP_IDENT = &
             '$Id: dmoprep.f90,v 1.13 2006/09/18 13:32:45 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: dmoprep_struct              
 
        private
        logical              :: skip_wrapup       ! Wrappup flag

        character(len=3)     :: opt_reg           ! process parameters
        character(len=3)     :: opt_pnmo          ! process parameters
        character(len=3)     :: opt_rotate        ! process parameters
        integer              :: hdr_off           ! process parameters
        real                 :: off_init          ! process parameters
        real                 :: off_inc           ! process parameters
        real                 :: off_last          ! process parameters
        integer              :: off_tot           ! process parameters
        real                 :: max_shift         ! process parameters
        real                 :: nmo_doppler       ! process parameters
        character(len=4)     :: nmo_opt_nmo_res   ! process parameters
        character(len=FILENAME_LENGTH):: nmo_pathname ! ess parameters

        integer              :: nwih,ndpt         ! globals.

        integer                  :: prtlu         ! logical unit for printing
        integer                  :: nout          ! total traces passed out
        real                     :: dx(2,2)       ! used in rotation
        real                     :: xorg          ! used in rotation
        real                     :: yorg          ! used in rotation
        real,pointer             :: offsets(:)    ! Array of offsets
        type(nmo_struct),pointer :: nmo           ! Structure for NMO
        type(pathchoose_struct),pointer :: dialog

      end type dmoprep_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(dmoprep_struct),pointer,save :: object      ! needed for traps.

      contains


!!--------------------------------- traps ------------------------------------!!
!!--------------------------------- traps ------------------------------------!!
!!--------------------------------- traps ------------------------------------!!
   
      subroutine dmoprep_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! Arguments

!------------------------------------------------------------

   
      select case (keyword)

        case('OPT_REG')
          if (object%opt_reg/='YES' .and. object%opt_reg/='NO') then
            call pc_error ('DMOPREP: OPT_REG is being set to default: YES')
            object%opt_reg = 'YES'
          endif

        case('OPT_PNMO')
          if (object%opt_pnmo/='YES' .and. object%opt_pnmo/='NO') then
            call pc_error ('DMOPREP: OPT_PNMO is being set to default: NO')
            object%opt_pnmo = 'NO'
          endif

        case('OPT_ROTATE')
          if (object%opt_pnmo/='YES' .and. object%opt_pnmo/='NO') then
            call pc_error ('DMOPREP: OPT_ROTATE is being set to default: NO')
            object%opt_pnmo = 'NO'
          endif

        case('HDR_OFF')
          if (object%hdr_off<1 .or. object%hdr_off>object%nwih)  then
            call pc_error ('DMOPREP: HDR_OFF is out of range 1 to ', &
                           object%nwih,' Resetting to default.')
            object%hdr_off = HDR_OFFSET
          endif

        case('OFF_INIT')
          if (object%off_init <= 0.0) then
            call pc_error ('DMOPREP: OFF_INIT must be greater than 0.0')
            object%off_init = 1.0
          endif

        case('OFF_INC')
          if (object%off_inc <= 0.0) then
            call pc_error ('DMOPREP: OFF_INC must be greater than 0.0')
            object%off_inc = 1.0
          endif

        case('OFF_LAST')
          if (object%off_last <= 0.0) then
            call pc_error ('DMOPREP: OFF_LAST must be greater than 0.0')
            object%off_last = 1.0
          else
            object%off_tot = &
                nint((object%off_last-object%off_init)/object%off_inc) + 1
            object%off_last = &
                object%off_init + object%off_inc*(object%off_tot-1)
          endif

        case('OFF_TOT')
          if (object%off_tot <= 0) then
            call pc_error ('DMOPREP: OFF_TOT must be greater than 0')
            object%off_tot = 1.0
          else
            object%off_last = &
                object%off_init + object%off_inc*(object%off_tot-1)
          endif

        case('MAX_SHIFT')
          if (object%max_shift <= 0.0) &
            call pc_error ('DMOPREP: MAX_SHIFT must be greater than 0.0')

        case('DOPPLER')
          if (object%nmo_doppler < 1.0) then
            call pc_error ('DMOPREP: DOPPLER must be greater than 1.0, &
                           &Resetting to default: 1.7')
            object%nmo_doppler = 1.7
          endif

        case('OPT_PNMO_RES')
          if (object%nmo_opt_nmo_res /= 'NONE' .and. &
              object%nmo_opt_nmo_res /= 'FFT2' .and. &
              object%nmo_opt_nmo_res /= 'FFT4' .and. &
              object%nmo_opt_nmo_res /= 'FFT8' .and. &
              object%nmo_opt_nmo_res /= 'CUB4' ) then
            call pc_error &
                 ('DMOPREP: OPT_PNMO_RES is being set to default: FFT4')
            object%nmo_opt_nmo_res = 'FFT4'
          endif

      end select

      return
      end subroutine dmoprep_trap
   

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine dmoprep_create (obj)
      implicit none
      type(dmoprep_struct),pointer :: obj       ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers
      nullify (obj%offsets)
      nullify (obj%nmo)
      nullify (obj%dialog) ! jpa
   
      call pathchoose_create  (obj%dialog, 'pathname_vel', 'vel')
      call dmoprep_initialize (obj)
      return
      end subroutine dmoprep_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine dmoprep_delete (obj)
      implicit none
      type(dmoprep_struct),pointer :: obj       ! arguments

!<execute_only>
      call dmoprep_wrapup (obj)
!</execute_only>

      if (associated(obj%offsets)) deallocate      (obj%offsets)
      if (associated(obj%nmo    )) call nmo_delete (obj%nmo)

      call pathchoose_delete (obj%dialog)

      deallocate(obj)

      return
      end subroutine dmoprep_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine dmoprep_initialize (obj)
      implicit none
      type(dmoprep_struct),pointer :: obj       ! arguments

! Initialize global values
      call pc_get_global ('NWIH',obj%nwih)
      call pc_get_global ('NDPT',obj%ndpt)

! Initialize process parameters
      obj%opt_reg    = 'NO'
      obj%opt_pnmo   = 'NO'
      obj%opt_rotate = 'YES'
      obj%hdr_off    = 6
      obj%off_init   = 1.0
      obj%off_inc    = 1.0
      obj%off_last   = 1.0
      obj%off_tot    = 1
      obj%max_shift  = 1.0

      obj%nmo_doppler     = 1.7
      obj%nmo_opt_nmo_res = 'FFT4'
      obj%nmo_pathname    = PATHCHECK_EMPTY

! Initialize all the rest
      obj%prtlu   = 0
      obj%nout    = 0

      obj%dx(:,:) = 0.0
      obj%xorg    = 0.0
      obj%yorg    = 0.0

      call dmoprep_update (obj)

      return
      end subroutine dmoprep_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine dmoprep_update (obj)
      implicit none
      type(dmoprep_struct),target :: obj               ! Arguments

      integer     :: j                              ! Local
      integer     :: nstore  ! Local
      integer     :: ier1           ! Local
      integer     :: update_state                   ! Local

      integer     :: ngrid_parms             ! pc return size for grid_parms
      real,pointer:: grid_parms(:)           ! 1D array of grid parameters
                                             ! that has origin and DX info
      logical     :: gathered = .false.          ! Used for test on globals
      integer     :: numtr                       ! Used for test on globals

      integer     :: SIZEOF_INT                     ! Convenience variables
      integer     :: SIZEOF_REAL                    ! Convenience variables
      integer     :: SIZEOF_DOUBLE                  ! Convenience variables
!-------------------------------------------------------

      nullify (grid_parms) ! jpa

      SIZEOF_INT    = sizeof(1)
      SIZEOF_REAL   = sizeof(1.0)
      SIZEOF_DOUBLE = sizeof(1.0d0)

      object => obj                 ! needed for traps.
      obj%skip_wrapup  = .true.

      update_state = pc_get_update_state()

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%dialog, obj%nmo_pathname)) return

!      call pc_get_global ('GATHERED', gathered)
!      if (.not.gathered) then
!        call pc_error('DMOPREP: Input data are not gathered. You should &
!                      &gather the data in CMPs prior to this process.')
!      endif

!      call pc_get ('OPT_REG'      , obj%opt_reg         , dmoprep_trap)
!      call pc_get ('OPT_PNMO'     , obj%opt_pnmo        , dmoprep_trap)
!      call pc_get ('OPT_ROTATE'   , obj%opt_rotate      , dmoprep_trap)
!      call pc_get ('HDR_OFF'      , obj%hdr_off         , dmoprep_trap)
!      call pc_get ('OFF_INIT'     , obj%off_init        , dmoprep_trap)
!      call pc_get ('OFF_INC'      , obj%off_inc         , dmoprep_trap)
!      call pc_get ('OFF_LAST'     , obj%off_last        , dmoprep_trap)
!      call pc_get ('OFF_TOT'      , obj%off_tot         , dmoprep_trap)
!      call pc_get ('MAX_SHIFT'    , obj%max_shift       , dmoprep_trap)
!      call pc_get ('DOPPLER'      , obj%nmo_doppler     , dmoprep_trap)
!      call pc_get ('OPT_PNMO_RES' , obj%nmo_opt_nmo_res , dmoprep_trap)
!      call pc_get ('PATHNAME_VEL' , obj%nmo_pathname    , dmoprep_trap)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

! Run through parameter tests because pc_get isn't trapping some bad values
!      call dmoprep_trap('HDR_OFF' )
!      call dmoprep_trap('OFF_INIT')
!      call dmoprep_trap('OFF_INC' )
!      call dmoprep_trap('OFF_LAST')
!      call dmoprep_trap('OFF_TOT' )

!      call pathcheck &
!          ('pathname_vel', obj%nmo_pathname, 'vel', show=PATHCHECK_INFO_INPUT)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

! Create NMO object
      if ( obj%opt_pnmo == 'YES' ) then
        call pc_clear
        call pc_put_process ('OPT_NMO'    , 'PARTIAL'          )
        call pc_put_process ('DOPPLER'    , obj%nmo_doppler    )
        call pc_put_process ('OPT_NMO_RES', obj%nmo_opt_nmo_res)
        call pc_put_process ('PATHNAME'   , obj%nmo_pathname   )
        if (associated(obj%nmo)) then
          call nmo_update (obj%nmo)
        else
          call nmo_create (obj%nmo)
        endif
        call pc_restore
      endif

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

!      call pc_put_options_field ('OPT_REG'     ,(/'YES','NO '/), 2)
!      call pc_put_options_field ('OPT_PNMO'    ,(/'YES','NO '/), 2)
!      call pc_put_options_field ('OPT_ROTATE'  ,(/'YES','NO '/), 2)
!      call pc_put_options_field &
!             ('OPT_PNMO_RES',(/'NONE','FFT2','FFT4','FFT8','CUB4'/), 5)

!      call pc_put ('OPT_REG'      , obj%opt_reg        )
!      call pc_put ('OPT_PNMO'     , obj%opt_pnmo       )
!      call pc_put ('OPT_ROTATE'   , obj%opt_rotate     )
!      call pc_put ('HDR_OFF'      , obj%hdr_off        )
!      call pc_put ('OFF_INIT'     , obj%off_init       )
!      call pc_put ('OFF_INC'      , obj%off_inc        )
!      call pc_put ('OFF_LAST'     , obj%off_last       )
!      call pc_put ('OFF_TOT'      , obj%off_tot        )
!      call pc_put ('MAX_SHIFT'    , obj%max_shift      )
!      call pc_put ('DOPPLER'      , obj%nmo_doppler    )
!      call pc_put ('OPT_PNMO_RES' , obj%nmo_opt_nmo_res)
!      call pc_put ('PATHNAME_VEL' , obj%nmo_pathname   )

!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!

! Update control parameters
      nstore = 0
      if ( obj%opt_reg == 'YES' ) nstore = nstore + obj%off_tot*SIZEOF_REAL
      call pc_put_control ('NSTORE' , nstore)
      call pc_put_control ('TWOSETS', 'YES')

! Offset parameters might require more than NUMTR output traces. Warn of this.
      if ( obj%opt_reg == 'YES' ) then
        call pc_get_global ( 'NUMTR', numtr )
        if ( numtr < obj%off_tot ) then
          call pc_error('DMOPREP: OFF_TOT cannot be greater than NUMTR. &
                        &The offset binning should be adjusted.')
          call pc_error('DMOPREP: OFF_TOT =',obj%off_tot,', NUMTR =',numtr)
        else
          call pc_put_global ( 'NUMTR', obj%off_tot )
        endif
      endif

!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!

      if ( update_state == PC_GUI .or. update_state == PC_FRONTEND ) then
        call dmoprep_set_sensitivities(obj)
      endif

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! Get a logical unit for printing
      obj%prtlu = pc_get_lun()

! Set up offsets as needed
      if ( obj%opt_reg == 'YES' ) then
        allocate (obj%offsets(obj%off_tot), stat=ier1)
        obj%offsets(1:obj%off_tot) = &
             obj%off_init + (/(j,j=0,obj%off_tot-1)/) * obj%off_inc
      endif

! Set up dx, xorg, and yorg as needed
      if ( obj%opt_rotate == 'YES' ) then
        call pc_alloc_global ('GRID', grid_parms, ngrid_parms)
        obj%xorg    = grid_parms(1)
        obj%yorg    = grid_parms(2)
        obj%dx = reshape( grid_parms(3:6),(/2,2/) )
      endif

!</execute_only>

!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!

      return
      end subroutine dmoprep_update

   

!*******************************************************************************
! Subroutine to handle all the GUI sensitivity settings during update.
!*******************************************************************************
      subroutine dmoprep_set_sensitivities(obj)
      implicit none
      type(dmoprep_struct) :: obj       ! arguments

!----------------------------------------------------------------

      ! These variables are always sensitive
!      call pc_put_sensitive_field_flag ('OPT_REG'      , .true. )
!      call pc_put_sensitive_field_flag ('OPT_ROTATE'   , .true. )

      ! These are the other sensitivity settings at initializaion
!      call pc_put_sensitive_field_flag ('HDR_OFF'      , .true. )
!      call pc_put_sensitive_field_flag ('OFF_INIT'     , .true. )
!      call pc_put_sensitive_field_flag ('OFF_INC'      , .true. )
!      call pc_put_sensitive_field_flag ('OFF_LAST'     , .true. )
!      call pc_put_sensitive_field_flag ('OFF_TOT'      , .true. )
!      call pc_put_sensitive_field_flag ('MAX_SHIFT'    , .true. )
!      call pc_put_sensitive_field_flag ('OPT_PNMO'     , .true. )
!      call pc_put_sensitive_field_flag ('DOPPLER'      , .false.)
!      call pc_put_sensitive_field_flag ('OPT_PNMO_RES' , .false.)
!      call pc_put_sensitive_field_flag ('PATHNAME_VEL' , .false.)
!      call pc_put_sensitive_field_flag ('SELECT_PATHNAME_VEL', .false.)
!      call pc_put_sensitive_field_flag ('PATHNAME_VEL_INFO'  , .false.)

      ! Handle OPT_REG variations
!      select case (obj%opt_reg)
!        case ('YES')
!
!         ! Handle OPT_PNMO variations
!          select case (obj%opt_pnmo)
!            case ('NO')
!              !This is the default as set above.
!            case ('YES')
!              call pc_put_sensitive_field_flag ('DOPPLER'      , .true. )
!              call pc_put_sensitive_field_flag ('OPT_PNMO_RES' , .true. )
!              call pc_put_sensitive_field_flag ('PATHNAME_VEL' , .true. )
!              call pc_put_sensitive_field_flag ('SELECT_PATHNAME_VEL', .true.)
!              call pc_put_sensitive_field_flag ('PATHNAME_VEL_INFO'  , .true.)
!          end select
!
!        case ('NO')
!
!          call pc_put_sensitive_field_flag ('HDR_OFF'   , .false.)
!          call pc_put_sensitive_field_flag ('OFF_INIT'  , .false.)
!          call pc_put_sensitive_field_flag ('OFF_INC'   , .false.)
!          call pc_put_sensitive_field_flag ('OFF_LAST'  , .false.)
!          call pc_put_sensitive_field_flag ('OFF_TOT'   , .false.)
!          call pc_put_sensitive_field_flag ('MAX_SHIFT' , .false.)
!          call pc_put_sensitive_field_flag ('OPT_PNMO'  , .false.)
!
!      end select

      ! Handle OPT_ROTATE variations
        ! Nothing to do in current configuration

      return
      end subroutine dmoprep_set_sensitivities


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine dmoprep (obj,ntr,hdi,tri,hdo,tro)

      implicit none
!
      type(dmoprep_struct)          :: obj                  ! Arguments
      integer                       :: ntr                  ! Arguments
      double precision,intent(inout):: hdi(:,:)             ! Arguments
      real,            intent(in)   :: tri(:,:)             ! Arguments
      double precision,intent(out)  :: hdo(:,:)             ! Arguments
      real,            intent(out)  :: tro(:,:)             ! Arguments
!
      integer           :: i, j, jstrt, ilive, inear
      real              :: dist, dtest, bint
      real, allocatable :: d_x(:)
      double precision,allocatable  :: hdtmp(:)
!----------------------------------------------------------------
      if ( ntr == 0 ) return

      IF ( obj%opt_reg == 'YES' ) THEN

! Recalculate Offset
        hdi(obj%hdr_off,1:ntr)= &
          sqrt( (hdi(HDR_RECEIVER_XLOC,1:ntr)-hdi(HDR_SOURCE_XLOC,1:ntr))**2 &
               +(hdi(HDR_RECEIVER_YLOC,1:ntr)-hdi(HDR_SOURCE_YLOC,1:ntr))**2 )

! Initialize Output Trace Buffer
        hdo(1:obj%nwih,1:obj%off_tot) = 0.0
        tro(1:obj%ndpt,1:obj%off_tot) = 0.0

! Use input scratch header 31 to Flag Used Traces
        hdi(HDR_SCRATCH_31,1:ntr) = 0.0

! Identify Nearest Trace Within One Offset Bin Increment
        do i=1,obj%off_tot
          jstrt = 1
          ilive = 0
          dist  = obj%off_inc
          do while ( jstrt <= ntr .and. ilive == 0 )
            if ( hdi(HDR_SCRATCH_31,jstrt) == 0.0 .and. &
                 hdi(HDR_LAV       ,jstrt) /= 0.0 ) then
              ilive = 1
              dist  = abs( hdi(obj%hdr_off,jstrt) - obj%offsets(i) ) 
              inear = jstrt
            else
              jstrt = jstrt + 1
            endif
          enddo
          if ( jstrt < ntr ) then
            do j=jstrt+1,ntr
              if ( hdi(HDR_SCRATCH_31,j) == 0.0 .and. &
                   hdi(HDR_LAV       ,j) < 0.0 ) then
                dtest = abs( hdi(obj%hdr_off,j) - obj%offsets(i) )
                if ( dtest < dist ) then
                  dist  = dtest
                  inear = j
                endif
              endif
            enddo
          else
            inear = jstrt
          endif

! Load Ouput Trace Buffer With Nearest Trace
          if ( dist < obj%off_inc ) then
            hdo(1:obj%nwih,i)     = hdi(1:obj%nwih,inear)
            hdo(HDR_SCRATCH_31,i) = inear
            tro(1:obj%ndpt,i)     = tri(1:obj%ndpt,inear)
            hdi(HDR_SCRATCH_31,inear) = 1.0      !Flag input trace as used
          endif
        enddo

! Identify Nearest Trace Within MAX_SHIFT Distance
        do i=1,obj%off_tot
          if ( hdo(HDR_SCRATCH_31,i) == 0.0 ) then
            jstrt = 1
            ilive = 0
            dist  = obj%max_shift + 1.0
            do while ( jstrt <= ntr .and. ilive == 0 )
              if ( hdi(HDR_SCRATCH_31,jstrt) == 0.0 .and. &
                   hdi(HDR_LAV       ,jstrt) < 0.0 ) then
                ilive = 1
                dist  = abs( hdi(obj%hdr_off,jstrt) - obj%offsets(i) )
                inear = jstrt
              else
                jstrt = jstrt + 1
              endif
            enddo
            if ( jstrt < ntr ) then
              do j=jstrt+1,ntr
                if ( hdi(HDR_SCRATCH_31,j) == 0.0 .and. &
                     hdi(HDR_LAV       ,j) < 0.0 ) then
                  dtest = abs( hdi(obj%hdr_off,j) - obj%offsets(i) )
                  if ( dtest < dist ) then
                    dist  = dtest
                    inear = j
                  endif
                endif
              enddo
            else
              inear = jstrt
            endif

! Load Ouput Trace Buffer With Nearest Trace
            if ( dist < obj%max_shift ) then
              hdo(1:obj%nwih,i)     = hdi(1:obj%nwih,inear)
              hdo(HDR_SCRATCH_31,i) = inear
              tro(1:obj%ndpt,i) = tri(1:obj%ndpt,inear)
              hdi(HDR_SCRATCH_31,inear) = 1.0     !Flag input trace as used
            endif
          endif
        enddo
        ntr = obj%off_tot

! Fix up remaining headers in regularized output traces
        hdo(HDR_MIDPOINT_XGRID    ,1:obj%off_tot)= hdi(HDR_MIDPOINT_XGRID,1)
        hdo(HDR_MIDPOINT_YGRID    ,1:obj%off_tot)= hdi(HDR_MIDPOINT_YGRID,1)
        hdo(HDR_SCRATCH_32        ,1:obj%off_tot)= obj%offsets(1:obj%off_tot)
        hdo(HDR_MIDPOINT_SHOTPOINT,1:obj%off_tot)= hdi(HDR_MIDPOINT_SHOTPOINT,1)
        hdo(HDR_MIDPOINT_LINE     ,1:obj%off_tot)= hdi(HDR_MIDPOINT_LINE,1)

        hdo(HDR_CURRENT_CHANNEL,1:obj%off_tot)= (/(j,j=1,obj%off_tot)/)
        hdo(HDR_SEQUENCE       ,1:obj%off_tot)= obj%nout+(/(j,j=1,obj%off_tot)/)
        obj%nout = obj%nout + obj%off_tot

! Adjust traces to their new offsets
        if ( obj%opt_pnmo == 'YES' ) then
          call nmo(obj%nmo,ntr,hdo,tro)
        else
          allocate ( hdtmp(ntr) )
          hdtmp(1:ntr)           = hdo(obj%hdr_off,1:ntr)
          hdo(obj%hdr_off,1:ntr) = hdo( 32,1:ntr)
          hdo( 32,1:ntr)         = hdtmp(1:ntr)
          deallocate ( hdtmp )
        endif

      ELSE

        ! If opt_reg==NO we still must copy input to output before rotation
        hdo(1:obj%nwih,1:ntr) = hdi(1:obj%nwih,1:ntr)
        tro(1:obj%ndpt,1:ntr) = tri(1:obj%ndpt,1:ntr)

      ENDIF

      if ( obj%opt_rotate == 'YES' ) then

        allocate ( d_x(ntr) )

        ! Rotate Source and Receiver Corrdinates to In-Line
        bint = sqrt( obj%dx(1,1)**2 + obj%dx(2,1)**2 )
        d_x(1:ntr) = hdo(obj%hdr_off,1:ntr) / (2 * bint)

        hdo(HDR_MIDPOINT_XGRID,1:ntr) = hdo(HDR_MIDPOINT_SHOTPOINT,1:ntr)
        hdo(HDR_MIDPOINT_YGRID,1:ntr) = hdo(HDR_MIDPOINT_LINE     ,1:ntr)
        hdo(HDR_SOURCE_XGRID  ,1:ntr) = hdo(HDR_MIDPOINT_SHOTPOINT,1:ntr) &
                                       +d_x(1:ntr)
        hdo(HDR_SOURCE_YGRID  ,1:ntr) = hdo(HDR_MIDPOINT_LINE     ,1:ntr)
        hdo(HDR_RECEIVER_XGRID,1:ntr) = hdo(HDR_MIDPOINT_SHOTPOINT,1:ntr) &
                                       -d_x(1:ntr)
        hdo(HDR_RECEIVER_YGRID,1:ntr) = hdo(HDR_MIDPOINT_LINE     ,1:ntr)

        hdo(HDR_SOURCE_XLOC  ,1:ntr) = &
                        obj%dx(1,1)*hdo(HDR_SOURCE_XGRID  ,1:ntr) &
                       +obj%dx(1,2)*hdo(HDR_SOURCE_YGRID  ,1:ntr) + obj%xorg
        hdo(HDR_SOURCE_YLOC  ,1:ntr) = &
                        obj%dx(2,1)*hdo(HDR_SOURCE_XGRID  ,1:ntr) &
                       +obj%dx(2,2)*hdo(HDR_SOURCE_YGRID  ,1:ntr) + obj%yorg
        hdo(HDR_RECEIVER_XLOC,1:ntr) = &
                        obj%dx(1,1)*hdo(HDR_RECEIVER_XGRID,1:ntr) &
                       +obj%dx(1,2)*hdo(HDR_RECEIVER_YGRID,1:ntr) + obj%xorg
        hdo(HDR_RECEIVER_YLOC,1:ntr) = &
                        obj%dx(2,1)*hdo(HDR_RECEIVER_XGRID,1:ntr) &
                       +obj%dx(2,2)*hdo(HDR_RECEIVER_YGRID,1:ntr) + obj%yorg

        deallocate ( d_x )

      endif

      return
      end subroutine dmoprep

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine dmoprep_wrapup (obj)
      implicit none
      type(dmoprep_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine dmoprep_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module dmoprep_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
