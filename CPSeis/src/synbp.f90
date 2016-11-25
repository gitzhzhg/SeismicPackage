!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-10-31. />

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
! Name       : SYNBP   (Synthetics for Broadcast Pattern testing)
! Category   : synthetics
! Written    : 2001-02-14   by: Stephen Chiu
! Revised    : 2007-12-11   by: Karen Goodger
! Maturity   : beta      
! Purpose    : Generate synthetic traces for broadcast pattern testing.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! SYNBP creates synthetic traces to use as input to Kirchhoff migration 
! routines for broadcast pattern testing.  For each output trace, the user must
! specify the midpoint grid coordinates (header words 7 and 8), the offset and 
! the azimuth.  From the user specified parameter values and the grid global, 
! the process calculates grid and surveyed coordinate values for source, 
! midpoint and receiver and sets the header words appropriately.
! 
! Sample values for output traces are zero except for unit amplitude spikes that
! alternate in polarity.  Spikes start at time TIM_BEG seconds, end at time 
! TIM_END seconds and are spaced TIM_INC seconds apart.  
!
! The PEAK_FREQ is used to calculate a Ricker wavelet which is applied to the
! output traces.  If PEAK_FREQ = 0.0, then no wavelet is applied to the
! output traces.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
!
! Process is a trace supplying process; there are no input traces.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This is a trace-supplying process.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set
! GATHERED  whether traces are a legitimate gather  set
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
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
! 1       Sequential Trace Count     Set
! 2       Head mute index            Set
! 3       Current gather number      Set
! 4       Number in current gather   Set
! 5       Fold                       Set
! 6       Offset                     Set
! 7       Midpoint x-grid            Set
! 8       Midpoint y-grid            Set
! 11      Source surveyed easting    Set
! 12      Source surveyed northing   Set
! 14      Receiver surveyed easting  Set
! 15      Receiver surveyed northing Set
! 17      Midpoint surveyed easting  Set
! 18      Midpoint surveyed northing Set
! 25      LAV                        Set
! 33      Source x-grid              Set
! 34      Source y-grid              Set
! 35      Receiver x-grid            Set
! 36      Receiver y-grid            Set
! 37      Midpoint x-annotation      Same as hdr 7
! 38      Midpoint y-annotation      Same as hdr 8
! 64      Tail mute index            Set
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author      Description
!     ----        ------      -----------
! 8.  2007-12-11  Goodger     Remove initialization of 4 arrays in the
!                             initialize routine.  These arrays were not
!                             yet allocated.
! 7.  2007-01-03  B. Menger   Correct center of spike by one sample down, fix the
!                             error correction for front end using traps, correctly
!                             handle traces that start before time = 0.0.
! 6.  2006-09-11  Stoeckley   Add call to pc_register_array_names for SeisSpace.
! 5.  2006-01-10  B. Menger   Removed Unused Variables.
! 4.  2002-07-10  S.Chiu      Remove EZGUI warning
! 3.  2002-07-01  S.Chiu      Set unused headers to zero
! 2.  2002-03-04  S.Chiu      Set Need_label = .true.
! 1.  2001-02-14  S.Chiu      Original Version
!     
!  
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
! NEED_LABEL      yes      whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR is ignored because this is a trace supplying process.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting traces.
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
!<--  XML code for the GUI goes in this section. />
!<gui_def>
!<NS SYNBP Process/NC=80>
!
!          Synthetics for Broadcast Pattern testing
!
!   TIM_BEG=~~~~`FFFFFFFFFFF
!   TIM_INC=~~~~`FFFFFFFFFFF   
!   TIM_END=~~~~`FFFFFFFFFFF
!   PEAK_FREQ=~~`FFFFFFFFFFF
!
!   MP_X_GRID    MP_Y_GRID    OFFSET       AZIMUTH
!   `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF
!   `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF
!   `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF
!   `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF `FFFFFFFFFFF
!<PARMS MP_X_GRID_ARRAYSET[/XST/YST]>

!</gui_def>
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!----------------------Parameters for output trace headers----------------------
!
!<Help KEYWORD="MP_X_GRID">
!<Tip> Midpoint x-grid value for output traces (header word 7 value). </Tip>
! Default = -
! Allowed = real (linked array, 50)
! Linked array of midpoint x-grid values for output traces (header word 7 
! values).
!</Help>
!
!<Help KEYWORD="MP_Y_GRID">
!<Tip> Midpoint y-grid value for output traces (header word 8 value). </Tip>
! Default = -
! Allowed = real (linked array, 50)
! Linked array of midpoint y-grid values for output traces (header word 8 
! values).
!</Help>
!
!<Help KEYWORD="OFFSET">
!<Tip> Linked array of offset values for output traces, in feet or meters.</Tip>
! Default = -
! Allowed = real > 0.0 (linked array, 50)
!</Help>
!
!<Help KEYWORD="AZIMUTH">
!<Tip> Linked array of azimuth values for output traces, in degrees. </Tip>
! Default = -
! Allowed = real (linked array, 50)
! Azimuth value is the angle measured clockwise (looking down) from north to the
! direction from source to receiver, in degrees. 
!</Help>
!
!----------------------Parameters for trace sample values-----------------------
!
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Time, in seconds, for the first unit amplitude spike on traces. </Tip>
! Default = TSTRT
! Allowed = real 
! Time, in seconds, for the first unit amplitude spike on output traces.
! Spikes on output traces are of unit amplitude and have alternating polarity. 
!</Help>
!
!<Help KEYWORD="TIM_INC">
!<Tip> Spacing, in seconds, between unit amplitude spikes on traces. </Tip>
! Default = 0.1
! Allowed = real > 0.0
! Spacing, in seconds, between unit amplitude spikes on output traces.  
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Time, in seconds, for the last unit amplitude spike on traces. </Tip>
! Default = end of trace
! Allowed = real > TSTRT
! Time, in seconds, for the last unit amplitude spike on output traces.
!</Help>
!
!<Help KEYWORD="PEAK_FREQ">
!<Tip> Peak frequency in Hz of for the Ricker wavelet. </Tip>
! Default = 45
! Allowed = real >= 0.0
! If PEAK_FREQ > 0.0, then it is used as a parameter in calculating a Ricker 
! wavelet that will be applied to the output traces.  [More documentation on 
! the Ricker wavelet is found in the GENFILT process.]
!
! If PEAK_FREQ = 0.0, then no wavelet will be applied to the output traces.  
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module synbp_module

      use fft_module
      use genfilt_module
      use pc_module
      use named_constants_module
      use mutehw_module
      use lav_module
      use mem_module
      use grid_module

      implicit none

      private
      public :: synbp_create
      public :: synbp_initialize
      public :: synbp_update
      public :: synbp_delete

!<execute_only>

      public :: synbp
      public :: synbp_wrapup

!</execute_only>

      character(len=100),public,save :: SYNBP_IDENT = &
'$Id: synbp.f90,v 1.8 2007/12/12 15:06:39 Goodger beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

 
      real,parameter    :: DEG_TO_RAD = 0.0174533

      type,public :: synbp_struct

      private
      logical              :: skip_wrapup         ! for wrapup routine
      real,pointer         :: mp_x_grid(:)        ! process parameters.
      real,pointer         :: mp_y_grid(:)        ! process parameters.
      real,pointer         :: offset(:)           ! process parameters.
      real,pointer         :: azimuth(:)          ! process parameters.

      real                 :: tim_beg             ! process parameters
      real                 :: tim_inc             ! process parameters
      real                 :: tim_end             ! process parameters
      real                 :: peak_freq           ! process parameters

      real                 :: tstrt               ! global 
      real                 :: dt                  ! global
      integer              :: ndpt                ! global
      integer              :: nwih                ! global
      integer              :: size_n1             ! dependent variables
      integer              :: size_n2             ! dependent variables
      integer              :: size_n3             ! dependent variables
      integer              :: size_n4             ! dependent variables

      integer              :: ntrace              ! dependent variables.
      integer              :: ntim_beg            ! dependent variables.
      integer              :: ntim_end            ! dependent variables.
      integer              :: ntim_inc            ! dependent variables.
      integer              :: print_lun           ! dependent variables.
      real                 :: xwidth              ! dependent variables.
      real                 :: ywidth              ! dependent variables.

      logical              :: done  

      type(genfilt_struct),pointer    :: genfilt  ! dependent variables
      type(grid_struct)               :: grid     ! dependent variables
      end type synbp_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(synbp_struct),pointer,save :: trp ! need this for traps

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine synbp_create (obj)
      implicit none
      type(synbp_struct),pointer :: obj              ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers

      nullify  (obj%genfilt)
      nullify  (obj%mp_x_grid)  
      nullify  (obj%mp_y_grid)  
      nullify  (obj%offset)     
      nullify  (obj%azimuth)  

      call synbp_initialize (obj)

      return
      end subroutine synbp_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine synbp_delete (obj)
      implicit none
      type(synbp_struct),pointer :: obj       ! arguments

!<execute_only>
      call synbp_wrapup (obj)
!</execute_only>

      if (associated(obj%genfilt))     call genfilt_delete (obj%genfilt)
      if (associated(obj%mp_x_grid))   deallocate(obj%mp_x_grid)
      if (associated(obj%mp_y_grid))   deallocate(obj%mp_y_grid)
      if (associated(obj%offset))      deallocate(obj%offset)     
      if (associated(obj%azimuth))     deallocate(obj%azimuth)
  
      deallocate(obj)

      return
      end subroutine synbp_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine synbp_initialize (obj)
      implicit none
      type(synbp_struct),intent(inout) :: obj       ! arguments

      ! First retrieve globals
      call pc_get_global ('TSTRT', obj%tstrt)
      call pc_get_global ('NDPT', obj%ndpt)
      call pc_get_global ('DT'  , obj%dt)
      call pc_get_global ('NWIH', obj%nwih)

!           These arrays have not been allocated, and should not be set
!              here- Caught by the intel compiler - kpg
!              Commenting out the next 4 lines
!!    obj%mp_x_grid = 0.0  
!!    obj%mp_y_grid = 0.0 
!!    obj%offset    = 0.0     
!!    obj%azimuth   = 0.0    
      obj%tim_beg   = obj%tstrt          
      obj%tim_inc   = 0.1           
      obj%tim_end   = (obj%ndpt - 1)*obj%dt            
      obj%peak_freq = 45

      obj%size_n1 = 0
      obj%size_n2 = 0
      obj%size_n3 = 0
      obj%size_n4 = 0

      obj%print_lun = pc_get_lun()
      call grid_initialize(obj%grid)

      call synbp_update (obj)

      return
      end subroutine synbp_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine synbp_update (obj)
      implicit none
      type(synbp_struct),intent(inout),target :: obj      ! Arguments

! Local variables
      integer            :: nstore, nscratch              ! local


      integer            :: nmin, nmax                    ! local
      logical            :: gathered                      ! local

      real               :: fnyq, sigma, power            ! local

      trp => obj                                          ! needed for traps.

      obj%skip_wrapup = .true.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("mp_x_grid_arrayset", (/  &
                                    "mp_x_grid",              &
                                    "mp_y_grid",              &
                                    "offset   ",              &
                                    "azimuth  " /))

! Now retrieve user paramerters

      call pc_get_global ('grid', obj%grid)

      call pc_alloc('MP_X_GRID'  , obj%mp_x_grid, obj%size_n1)
      call pc_alloc('MP_Y_GRID'  , obj%mp_y_grid, obj%size_n2)
      call pc_alloc('OFFSET'     , obj%offset,    obj%size_n3)
      call pc_alloc('AZIMUTH'    , obj%azimuth,   obj%size_n4)

      call pc_get ('TIM_BEG'     , obj%tim_beg, synbp_tim_beg_trap)
      call pc_get ('TIM_INC'     , obj%tim_inc, synbp_tim_inc_trap)
      call pc_get ('TIM_END'     , obj%tim_end, synbp_tim_end_trap)
      call pc_get ('PEAK_FREQ'   , obj%peak_freq)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      nmin = min(obj%size_n1,obj%size_n2,obj%size_n3,obj%size_n4)
      nmax = max(obj%size_n1,obj%size_n2,obj%size_n3,obj%size_n4)
      obj%ntrace = nmax

      if ( nmin /= nmax) then
           call pc_error ('A1,A2,A3,A4 arrays have different lengths')
           obj%ntrace = min(obj%size_n1,obj%size_n2,obj%size_n3,obj%size_n4)
      else
           obj%ntrace = obj%size_n1
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

     if (obj%peak_freq > 0.0) then 

!  ricker wavelet parameters

        power = 2.0
        fnyq = 0.5 / obj%dt
        obj%peak_freq = min( obj%peak_freq, fnyq)
        sigma = 1000.0 * sqrt(power) / (PI*2.0*obj%peak_freq)

! genfilt
        call pc_clear
        call pc_put_process ('AMP_PWR',  power)
        call pc_put_process ('SIGMA',    sigma)
        
        if (associated(obj%genfilt)) then
            call genfilt_update (obj%genfilt)
        else
            call genfilt_create (obj%genfilt)
        end if
        call pc_restore
! end genfilt

     end if


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      gathered = .true.

      call pc_put_global  ('numtr'       , obj%ntrace)       ! if changed.
      call pc_put_global  ('gathered'    , gathered)         ! if changed.

      call pc_put('MP_X_GRID'  , obj%mp_x_grid, obj%size_n1)
      call pc_put('MP_Y_GRID'  , obj%mp_y_grid, obj%size_n2)
      call pc_put('OFFSET'     , obj%offset,    obj%size_n3)
      call pc_put('AZIMUTH'    , obj%azimuth,   obj%size_n4)

      call pc_put ('TIM_BEG'     , obj%tim_beg)
      call pc_put ('TIM_INC'     , obj%tim_inc)
      call pc_put ('TIM_END'     , obj%tim_end)
      call pc_put ('PEAK_FREQ'   , obj%peak_freq)

! Determine memory usage

      nstore = 4*obj%ntrace
      nscratch  = 100

      call pc_put_control ('NSTORE', nstore)
      call pc_put_control ('NSCRATCH', nscratch)
      call pc_put_control ('NEED_LABEL' , .true.)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      obj%done   = .false.
      obj%xwidth = grid_get_xgrid_width (obj%grid)
      obj%ywidth = grid_get_ygrid_width (obj%grid)
      obj%xwidth = 1./(obj%xwidth + 1.0e-25)
      obj%ywidth = 1./(obj%ywidth + 1.0e-25)

      obj%ntim_beg = max(1,1+nint((obj%tim_beg-obj%tstrt)/obj%dt))
      obj%ntim_end = min(obj%ndpt,1+nint((obj%tim_end-obj%tstrt)/obj%dt))
      obj%ntim_inc = max(1,nint(obj%tim_inc/obj%dt))

      if (pc_do_not_process_traces()) return ! In case of allocation errors



!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine synbp_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

      subroutine synbp_tim_beg_trap(tim_beg)
        character (len=*), intent(in) :: tim_beg
        real                          :: trlen
        trlen = (trp%ndpt - 1)*trp%dt + trp%tstrt
        if(trp%tim_beg < trp%tstrt ) then 
          trp%tim_beg = trp%tstrt
          call pc_error('TIM_BEG MUST BE >= TRACE START TIME.')  
          call pc_jump_field('tim_beg')
        end if
        if((trp%tim_inc+trp%tim_beg) > trlen ) then 
          call pc_error('TIM_INC+TIM_BEG MUST BE <= TRACE LENGTH')  
          call pc_jump_field('tim_beg')
        end if

      end subroutine synbp_tim_beg_trap

      subroutine synbp_tim_inc_trap(tim_inc)
        character (len=*), intent(in) :: tim_inc
        real                          :: trlen
        trlen = (trp%ndpt - 1)*trp%dt + trp%tstrt
        if(trp%tim_inc < 0.0 ) then 
          trp%tim_inc = 0.0
          call pc_error('TIM_INC MUST BE > zero')  
          call pc_jump_field('tim_inc')
        end if

        if((trp%tim_inc+trp%tim_beg) > trlen ) then
          call pc_error('TIM_INC+TIM_BEG MUST BE <= TRACE LENGTH')  
          call pc_jump_field('tim_inc')
        end if

      end subroutine synbp_tim_inc_trap

      subroutine synbp_tim_end_trap(tim_end)
        character (len=*), intent(in) :: tim_end
        real                          :: trlen
        trlen = (trp%ndpt - 1)*trp%dt + trp%tstrt
        if(trp%tim_end < (trp%tim_beg+trp%tim_inc) ) then                    
          trp%tim_end=trp%tim_beg+trp%tim_inc
          call pc_error('TIM_END MUST BE >= TIM_BEG')  
          call pc_jump_field('tim_end')
        end if
        if(trp%tim_end > trlen ) then 
          trp%tim_end = trlen
          call pc_error('TIM_END MUST BE <= TRACE LENGTH')  
          call pc_jump_field('tim_end')
        end if
      end subroutine synbp_tim_end_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine synbp (obj,ntr,hd,tr)
      implicit none
      type(synbp_struct),intent(inout) :: obj              ! arguments
      integer             ,intent(inout) :: ntr            ! arguments
      double precision    ,intent(inout) :: hd(:,:)        ! arguments
      real                ,intent(inout) :: tr(:,:)        ! arguments

      integer             :: itr, it, isign, i             ! local
      real                :: sx,sy,rx,ry,angle,half_off    ! local
      real                :: trmax                         ! local

!----------------------------------------------------------------
! Loop over traces
 
      if (ntr == FATAL_ERROR ) then
        call pc_error('FATAL_ERROR in routine SYNBP ') 
        return
      end if  

      if (ntr == NO_MORE_TRACES .or. obj%done) then
        ntr = NO_MORE_TRACES
        call synbp_wrapup (obj)
        return
      end if

      isign = -1
      do itr = 1, obj%ntrace

        tr(1:obj%ndpt, itr) = 0.0
        hd(1:obj%nwih,itr)  = 0.0

        do it = obj%ntim_beg, obj%ntim_end, obj%ntim_inc 
          isign = isign * (-1)
          tr(it, itr) = isign
        end do

!  compute headers
        
        if (obj%mp_x_grid(itr) == FNIL) obj%mp_x_grid(itr) = 0.0 
        if (obj%mp_y_grid(itr) == FNIL) obj%mp_y_grid(itr) = 0.0
        if (obj%azimuth(itr) == FNIL) obj%azimuth(itr) = 0.0
        if (obj%offset(itr) == FNIL) obj%offset(itr) = 0.0

        angle = (90. - obj%azimuth(itr))*DEG_TO_RAD
        half_off = 0.5 * obj%offset(itr)
        sx = half_off * obj%xwidth * cos(angle)
        sy = half_off * obj%ywidth * sin(angle)
        rx = -sx
        ry = -sy
 
        hd(33,itr) = obj%mp_x_grid(itr) + sx
        hd(34,itr) = obj%mp_y_grid(itr) + sy
        hd(35,itr) = obj%mp_x_grid(itr) + rx
        hd(36,itr) = obj%mp_y_grid(itr) + ry

        hd(1,itr) = itr
        hd(2,itr) = min(0, obj%ntim_beg-1) 
        hd(3,itr) = 1
        hd(4,itr) = itr
        hd(5,itr) = 1
        hd(6,itr) = obj%offset(itr)
        hd(7,itr) = obj%mp_x_grid(itr)
        hd(8,itr) = obj%mp_y_grid(itr)
        hd(25,itr) = 1.0
        hd(37,itr) = hd(7,itr)
        hd(38,itr) = hd(8,itr)
        hd(64,itr) = max(obj%ndpt, obj%ntim_end+1)
      
        call grid_get_survey_coords (obj%grid, hd(7,itr), hd(8,itr), &
                 hd(17,itr) , hd(18,itr) )

        call grid_get_survey_coords (obj%grid, hd(33,itr), hd(34,itr), &
                 hd(11,itr) , hd(12,itr) )

        call grid_get_survey_coords (obj%grid, hd(35,itr), hd(36,itr), &
                 hd(14,itr) , hd(15,itr) )

        if (obj%peak_freq > 0.0) then

!  generate Ricker wavelet

           ntr = 1
           call genfilt (obj%genfilt, ntr, hd(:,itr:itr), tr(:,itr:itr))
           trmax = maxval(tr(:,itr))
           tr(:,itr) = tr(:,itr)/trmax
           do i=1,obj%ndpt
             if(tr(i,itr) .ne. 0.0 ) then
               hd(2,itr) = i-1
               exit
             endif
           end do
        end if

      enddo

      ntr = obj%ntrace
      call lav_set_hdr (hd, tr, obj%ndpt, ntr)
      obj%done = .true. 

      return
      end subroutine synbp

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine synbp_wrapup (obj)
      implicit none
      type(synbp_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine synbp_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module synbp_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
