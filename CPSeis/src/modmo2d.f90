!<CPS_v1 type="PROCESS"/>
!!------------------------------- modmo2d.f90 --------------------------------!!
!!------------------------------- modmo2d.f90 --------------------------------!!
!!------------------------------- modmo2d.f90 --------------------------------!!


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
! Name       : MODMO2D
! Category   : Transforms
! Written    : 2004-07-27   by: bkmacy
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Time to depth 2-D model-based moveout.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! MODMO2D converts time-domain common image gathers with moveout to the
! depth domain using model-based moveout.  Alternatively, depth-domain
! gathers without moveout can be converted to the time domain with
! moveout.  The model-based approach uses a 2-D velocity model with the
! velocity model profile parallel to the dominant acquisition azimuth.
! The input velocity model is either isotropic or transversely isotropic
! with a vertical symmetry axis (VTI).  The following velocity parameters
! are required:
!
! ISOTROPIC:     vertical P-wave velocity
! VTI:           vertical P-wave velocity, vertical S-wave velocity, delta,
!                and epsilon.
!
! This process is based on the FreeUSP-style programs GATHERT2Z_ANI and
! GATHERZ2T_ANI written by Dan Whitmore.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! The grid transformation must be set up in the project data if the velocity
! model is specified in grid coordinates rather than survey coordinates.
! Otherwise, the velocity model will not be scaled correctly laterally.
! Alternatively, the user can specify the real-world distance between adjacent
! grid numbers for the dominant acquisition azimuth.  See the GRID_WIDTH_TYPE
! and GRID_WIDTH_USER parameters for more details.
!
! To scale the depth axis appropriately, use the DEPTH_SCALE option.  For input
! depth traces, the sampling interval (DT) and starting depth (TSTRT) are
! multiplied by DEPTH_SCALE to determine DZ and Z0; i.e.,
!
! DZ = DT    * DEPTH_SCALE
! Z0 = TSTRT * DEPTH_SCALE
!
! For output depth traces, the sampling interval (DZ) and starting depth (Z0)
! are divided by DEPTH_SCALE to determine DT and TSTRT; i.e.,
!
! DT    = DZ / DEPTH_SCALE
! TSTRT = Z0 / DEPTH_SCALE
!
! If the true depth scale should be in feet or meters, but the input or
! output depth traces should have a sample interval of kilofeet or
! kilometers, set DEPTH_SCALE=1000.  For example, if the true depth sample
! interval is 30 feet, but the input depth sample interval is 0.03 (as
! specified by DT in JOB DATA), then set DEPTH_SCALE=1000.
!
! The DEPTH_SCALE option does not affect the "model" depth parameters (DZM and
! Z0M).  Set the model depth parameters to the same scale as DZ and Z0, not to
! DT and TSTRT.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process is a multiple-trace process.
! This process accepts more than one trace at a time.
! The input data must be gathered into common image gathers prior to MODMO2D.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process is a multiple-trace process.
! This process outputs more than one trace at a time.
! The output data is gathered into common image gathers upon output.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       Used, but not changed.
! GATHERED  whether traces are a legitimate gather  Used, but not changed.
! NWIH      number of words in trace header         Used, but not changed.
! NDPT      number of sample values in trace        Used and changed.
! TSTRT     starting time on trace                  Used and changed.
! DT        trace sample interval                   Used and changed.
! GRID      grid transformation structure           Used, but not changed.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                   Action taken
! ----    -----------                   ------------
! 6       Offset                        Used, but not changed.
! 7       Midpoint in-line    grid val. Used, but not changed.
! 8       Midpoint cross-line grid val. Used, but not changed.
! 17      Midpoint surveyed X coord.    Used, but not changed.
! 18      Midpoint surveyed Y coord.    Used, but not changed.
! 25      Largest absolute value        Changed.
!
! Either hwd's 7 and 8 or 17 and 18 are used, depending on user input.  By
! default, hwd's 7 and 8 are used.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!004. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!003. 2006-01-10  B. Menger  Removed Unused Variables.
!  2. 2005-01-31  bkmacy     Added ability for user to explicitly specify the
!                              grid width for the dominant acquisition azimuth
!                              (see GRID_WIDTH_TYPE and GRID_WIDTH_USER
!                              parameters.)
!                            Changed DUMP_VEL flag to be YES or NO instead of 1
!                              or 0, and to allow the user to specify the name
!                              of the output file (VP0_DUMPFILE).
!                            Expanded the use of timer routines and reporting.
!  1. 2004-07-27  bkmacy     Initial version.  Based on CPS revision 13 of
!                            modmo.f90.
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
! PARALLEL_SAFE  true      whether this process can be in a parallelized loop.
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
!<NS MODMO2D>
!                [/C]MODMO2D (MODel-based Move-Out for 2-D velocity variations)
!
! ani_type=~~~~~`CCCCCCCCCC
! Select VP0_FILE [VP0_FILE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [VP0_FILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! constant_vs_vp= `CCC
! vs_vp_ratio=~~~~`FFFF
! Select VS0_FILE [VS0_FILE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [VS0_FILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! Select EPS_FILE [EPS_FILE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [EPS_FILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! Select DEL_FILE [DEL_FILE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [DEL_FILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! time2depth=~~~`CCCCCCCCCC
! single_vz=~~~~`CCC
! nz=~~~~~~~~~~~`IIII nt=~~~~`IIII nzm=~~`IIII
! dz=~~~~~~~~~~~`FFFF delt=~~`FFFF dzm=~~`FFFF
! z0=~~~~~~~~~~~`FFFF t0=~~~~`FFFF z0m=~~`FFFF
! depth_scale=~~`FFFF
! offmax=~~~~~~~`FFFF
! dxmap=~~~~~~~~`FFFF
! ismoo=~~~~~~~~`IIII
! ang=~~~~~~~~~~`FFFF
! dang=~~~~~~~~~`FFFF
! ilinehdr=~~~~~`IIII acq_azimuth=~~~~~~`CCCCC
! xlinehdr=~~~~~`IIII grid_width_type=~~`CCCCCCCCCCCCCCC
! iverbos=~~~~~~`IIII grid_width_user=~~`FFFF
! dump_vel=~~~~~`CCC
! Select VP0_DUMPFILE [VP0_DUMPFILE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [VP0_DUMPFILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! <PARMS VP0_FILE[/ML=128/XST]>
! <PARMS VS0_FILE[/ML=128/XST]>
! <PARMS EPS_FILE[/ML=128/XST]>
! <PARMS DEL_FILE[/ML=128/XST]>
! <PARMS VP0_DUMPFILE[/ML=128/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="ANG">
!<Tip> Maximum emergence angle.</Tip>
! Default = 89.9
! Allowed = real scalar
! Specified in degrees.
!</Help>
!
!<Help KEYWORD="DANG">
!<Tip> Angle increment.</Tip>
! Default = 0.5
! Allowed = real scalar
! Specified in degrees.
!</Help>
!
!<Help KEYWORD="DXMAP">
!<Tip> Transverse sampling in a ttmap.</Tip>
! Default = 5.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="NZ">
!<Tip> Number of z samples.</Tip>
! Default = 1000
! Allowed = int>=1
! If input is depth data, NZ is taken from the input trace data.
!</Help>
!
!<Help KEYWORD="DZ">
!<Tip> Depth increment.</Tip>
! Default = 5.0
! Allowed = real scalar
! If input is depth data, DZ is taken from the input trace data.
!</Help>
!
!<Help KEYWORD="Z0">
!<Tip> Starting depth.</Tip>
! Default = 0.0
! Allowed = real scalar
! If input is depth data, Z0 is taken from the input trace data.
!</Help>
!
!<Help KEYWORD="DEPTH_SCALE">
!<Tip> Scale factor for depth axis.</Tip>
! Default = 1.0
! Allowed = real scalar > 0.0
! For input depth data,
!   DZ = DT    * DEPTH_SCALE
!   Z0 = TSTRT * DEPTH_SCALE
! For output depth data,
!   DT    = DZ / DEPTH_SCALE
!   TSTRT = Z0 / DEPTH_SCALE
! DZM and Z0M are not affected by this parameter.
!</Help>
!
!<Help KEYWORD="NZM">
!<Tip> Number of z samples for model.</Tip>
! Default = NZ
! Allowed = int>=1
! Does not need to match input velocity model, but does need to fall within
! the input velocity model.
!</Help>
!
!<Help KEYWORD="DZM">
!<Tip> Depth increment for model.</Tip>
! Default = DZ
! Allowed = real scalar
! Does not need to match input velocity model, but does need to fall within
! the input velocity model.
!</Help>
!
!<Help KEYWORD="Z0M">
!<Tip> Starting depth for model.</Tip>
! Default = Z0
! Allowed = real scalar
! Does not need to match input velocity model, but does need to fall within
! the input velocity model.
!</Help>
!
!<Help KEYWORD="NT">
!<Tip> Number of time samples.</Tip>
! Default = 1000
! Allowed = int>=1
! If input is time data, NT is taken from the input trace data.
!</Help>
!
!<Help KEYWORD="DELT">
!<Tip> Time increment.</Tip>
! Default = 0.004
! Allowed = real scalar
! If input is time data, DELT is taken from the input trace data.
!</Help>
!
!<Help KEYWORD="T0">
!<Tip> Starting time.</Tip>
! Default = 0.0
! Allowed = real scalar
! If input is time data, T0 is taken from the input trace data.
!</Help>
!
!<Help KEYWORD="ISMOO">
!<Tip> Number of times to smooth the velocity model.</Tip>
! Default = 5
! Allowed = int>=0
!</Help>
!
!<Help KEYWORD="ANI_TYPE">
!<Tip> Type of anisotropy.</Tip>
! Default = ISOTROPIC
! Allowed = VTI, ISOTROPIC
!</Help>
!
!<Help KEYWORD="VP0_FILE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of VP0_FILE.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_VP0_FILE">
!<Tip> Choose VP0_FILE using a file selection dialog box.</Tip>
!</Help>
!
!<Help KEYWORD="VP0_FILE">
!<Tip> Pathname for input vertical P-wave velocity dataset.</Tip>
! Default = NONE
! Allowed = char
!</Help>
!
!<Help KEYWORD="CONSTANT_VS_VP">
!<Tip> Constant Vs/Vp ratio.</Tip>
! Default = NO
! Allowed = YES, NO
!</Help>
!
!<Help KEYWORD="VS_VP_RATIO">
!<Tip> Constant Vs/Vp ratio value.</Tip>
! Default = 0.5
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="VS0_FILE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of VS0_FILE.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_VS0_FILE">
!<Tip> Choose VS0_FILE using a file selection dialog box.</Tip>
!</Help>
!
!<Help KEYWORD="VS0_FILE">
!<Tip> Pathname for input vertical S-wave velocity dataset.</Tip>
! Default = NONE
! Allowed = char
!</Help>
!
!<Help KEYWORD="EPS_FILE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of EPS_FILE.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_EPS_FILE">
!<Tip> Choose EPS_FILE using a file selection dialog box.</Tip>
!</Help>
!
!<Help KEYWORD="EPS_FILE">
!<Tip> Pathname for input epsilon parameter dataset.</Tip>
! Default = NONE
! Allowed = char
!</Help>
!
!<Help KEYWORD="DEL_FILE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of DEL_FILE.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_DEL_FILE">
!<Tip> Choose DEL_FILE using a file selection dialog box.</Tip>
!</Help>
!
!<Help KEYWORD="DEL_FILE">
!<Tip> Pathname for input delta parameter dataset.</Tip>
! Default = NONE
! Allowed = char
!</Help>
!
!<Help KEYWORD="OFFMAX">
!<Tip> Maximum offset.</Tip>
! Default = 4000.
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="TIME2DEPTH">
!<Tip> Direction of transform.</Tip>
! Default = TIME2DEPTH
! Allowed = DEPTH2TIME, TIME2DEPTH
! TIME2DEPTH converts from time to depth.
! DEPTH2TIME converts from depth to time.
!</Help>
!
!<Help KEYWORD="SINGLE_VZ">
!<Tip> Single velocity trace.</Tip>
! Default = NO
! Allowed = YES, NO
!</Help>
!
!<Help KEYWORD="IVERBOS">
!<Tip> Verbose logging.</Tip>
! Default = 0
! Allowed = 0,1
!</Help>
!
!<Help KEYWORD="DUMP_VEL">
!<Tip> Dump velocities.</Tip>
! Default = NO
! Allowed = YES, NO
! Dump velocities used for each gather location to disk.  See VP0_DUMPFILE for
! more information.
!</Help>
!
!<Help KEYWORD="VP0_DUMPFILE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of VP0_DUMPFILE.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_VP0_DUMPFILE">
!<Tip> Choose VP0_DUMPFILE using a file selection dialog box.</Tip>
!</Help>
!
!<Help KEYWORD="VP0_DUMPFILE">
!<Tip> Pathname for output vertical P-wave velocities used for each gather
!      location.</Tip>
! Default = NONE
! Allowed = char
! Pathname for output vertical P-wave velocities used for each gather location.
! For parallel jobs, the filename is modified by appending ".peXXXX", where
! XXXX is the worker number.  The output format is CPS TRCIO.  The header words
! for each output velocity trace are identical to the first trace in each
! gather.  For parallel jobs, this should allow the velocity traces to be
! sorted to the correct order.
!</Help>
!
!<Help KEYWORD="ILINEHDR">
!<Tip> Header word to use for in-line coordinate or grid value.</Tip>
! Default = 7
! Allowed = valid CPS header word
! This needs to match input data.
!</Help>
!
!<Help KEYWORD="XLINEHDR">
!<Tip> Header word to use for cross-line coordinate or grid value.</Tip>
! Default = 8
! Allowed = valid CPS header word
! This needs to match input data.
!</Help>
!
!<Help KEYWORD="ACQ_AZIMUTH">
!<Tip> Dominant acquisition azimuth.</Tip>
! Default = ILINE
! Allowed = ILINE or XLINE
! 2-D raytracing will be performed in velocity profiles parallel to the
! dominant acquisition azimuth.
!</Help>
!
!<Help KEYWORD="GRID_WIDTH_TYPE">
!<Tip> Grid width type.</Tip>
! Default = USER_DEFINED
! Allowed = X_GRID_DISTANCE, Y_GRID_DISTANCE, USER_DEFINED
! The grid width specifies the real-world distance between adjacent grid
! numbers for the dominant acquisition azimuth.  If the geometry is set
! correctly in PROJECT_DATA, grid width should be equal to either
! X_GRID_DISTANCE or Y_GRID_DISTANCE.  Otherwise, USER_DEFINED should be
! selected and the correct value entered for GRID_WIDTH_USER.
!</Help>
!
!<Help KEYWORD="GRID_WIDTH_USER">
!<Tip> User-defined grid width.</Tip>
! Default = 1.0
! Allowed = real scalar > 0.
! Specify the real-world distance between adjacent grid numbers for the
! dominant acquisition azimuth; i.e. grid width = real-world distance between
! grid point i and grid point i+1.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module modmo2d_module
   use pc_module
   use named_constants_module
   use memman_module
   use grid_module            ! if you need the grid transformation.
   use pathchoose_module      ! if you use file name parameters.
   use pathcheck_module       ! if you use file name parameters.
   use spline_module
   use lav_module
   use modgrid_module
   use mth_module
   use interpolate_module
   use pcps_module
   use cio_module
   use trcio_module
   use sizeof_module
   use timer_module
   use clean_module

   implicit none

   private

   ! Public subroutines
   public :: modmo2d_create
   public :: modmo2d_initialize
   public :: modmo2d_update
   public :: modmo2d_delete
   public :: modmo2d            ! main trace processing routine.
   public :: modmo2d_wrapup

   ! Private subroutines
   private :: modmo2d_do_gather
   private :: modmo2d_vzmapr_vti
   private :: modmo2d_vzmapr_vti2d
   private :: modmo2d_vzmapr_iso2d
   private :: modmo2d_rkstep
   private :: modmo2d_vtiderivs
   private :: modmo2d_vtiderivs2d
   private :: modmo2d_isoderivs2d
   private :: modmo2d_t2dzmo
   private :: modmo2d_d2tzmo
   private :: modmo2d_set_parms
   private :: modmo2d_alloc
   private :: modmo2d_dealloc
   private :: modmo2d_vel_init
   private :: modmo2d_vel_read_slice
   private :: modmo2d_vel_read_pencil
   private :: modmo2d_vel_paint_pencil
   private :: modmo2d_vel_paint_slice

   ! Private subroutines
   private :: modmo2d_vpphase
   private :: modmo2d_dvpdtheta
   private :: modmo2d_vpgroup
   private :: modmo2d_phigroup

   ! Traps
   private :: modmo2d_ani_type
   private :: modmo2d_time2depth
   private :: modmo2d_ang
   private :: modmo2d_dang
   private :: modmo2d_dxmap
   private :: modmo2d_depth_scale
   private :: modmo2d_nz
   private :: modmo2d_dz
   private :: modmo2d_z0
   private :: modmo2d_nzm
   private :: modmo2d_dzm
   private :: modmo2d_z0m
   private :: modmo2d_nt
   private :: modmo2d_delt
   private :: modmo2d_t0
   private :: modmo2d_ismoo
   private :: modmo2d_vp0_file
   private :: modmo2d_constant_vs_vp
   private :: modmo2d_vs_vp_ratio
   private :: modmo2d_vs0_file
   private :: modmo2d_eps_file
   private :: modmo2d_del_file
   private :: modmo2d_offmax
   private :: modmo2d_single_vz
   private :: modmo2d_ilinehdr
   private :: modmo2d_xlinehdr
   private :: modmo2d_acq_azimuth
   private :: modmo2d_grid_width_type
   private :: modmo2d_grid_width_user
   private :: modmo2d_iverbos
   private :: modmo2d_dump_vel
   private :: modmo2d_vp0_dumpfile
   private :: modmo2d_modmo2d
   private :: modmo2d_end


   character(len=100),public,save :: MODMO2D_IDENT = &
'$Id: modmo2d.f90,v 1.4 2006/10/17 13:45:45 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


   integer, parameter                 :: modmo2d_mgdim = 3
   integer, parameter, private        :: MODMO2D_DERIV_TYPE_VTI1D = 1
   integer, parameter, private        :: MODMO2D_DERIV_TYPE_VTI2D = 2
   integer, parameter, private        :: MODMO2D_DERIV_TYPE_ISO2D = 3
   integer, parameter, private        :: MODMO2D_TIMER_MAIN      = 1
   integer, parameter, private        :: MODMO2D_TIMER_DO_GATHER = 2
   integer, parameter, private        :: MODMO2D_TIMER_RT        = 3
   integer, parameter, private        :: MODMO2D_TIMER_TD        = 4
   integer, parameter, private        :: MODMO2D_TIMER_DT        = 5
   integer,            private        :: modmo2d_timers(5)

   type,public :: modmo2d_struct

     private
     logical                    :: skip_wrapup      ! wrapup flag.

     integer                    :: ipn      ! process number.
     integer                    :: numtr    ! max number of input traces.
     logical                    :: gathered ! whether properly gathered.
     integer                    :: nwih     ! number of header words.
     integer                    :: ndpt     ! number of trace samples.
     real                       :: tstrt    ! time of 1st trace sample (sec).
     real                       :: dt       ! trace sample interval (sec).
     type(grid_struct)          :: grid     ! grid transform.

     integer                          :: ndpt_out     ! output ndpt
     real                             :: tstrt_out    ! output tstrt
     real                             :: dt_out       ! output dt
     integer                          :: nt           ! ndpt  for time data
     real                             :: t0           ! tstrt for time data
     real                             :: delt         ! dt    for time data
     integer                          :: nz           ! ndpt  for depth data
     real                             :: z0           ! tstrt for depth data
     real                             :: dz           ! dt    for depth data
     integer                          :: nzm          ! velocity model nz
     real                             :: z0m          ! velocity model z0
     real                             :: dzm          ! velocity model dz
     real                             :: depth_scale  ! depth scaling factor
     real                             :: ang
     real                             :: dang
     real                             :: dxmap
     integer                          :: ismoo
     character(len=FILENAME_LENGTH)   :: vp0_file
     type(pathchoose_struct), pointer :: vp0_file_choose
     real                             :: vs_vp_ratio
     character(len=FILENAME_LENGTH)   :: vs0_file
     type(pathchoose_struct), pointer :: vs0_file_choose
     character(len=FILENAME_LENGTH)   :: eps_file
     type(pathchoose_struct), pointer :: eps_file_choose
     character(len=FILENAME_LENGTH)   :: del_file
     type(pathchoose_struct), pointer :: del_file_choose
     real                             :: offmax
     character(len=10)                :: ani_type
     character(len=10)                :: time2depth
     character(len=3)                 :: single_vz
     character(len=3)                 :: constant_vs_vp
     integer                          :: ilinehdr
     integer                          :: xlinehdr
     integer                          :: iverbos
     character(len=3)                 :: dump_vel
     character(len=FILENAME_LENGTH)   :: vp0_dumpfile
     type(pathchoose_struct), pointer :: vp0_dumpfile_choose
     character(len=5)                 :: acq_azimuth
     character(len=15)                :: grid_width_type
     real                             :: grid_width_user

     real                             :: theta_min
     real                             :: theta_max
     real                             :: dtheta
     real                             :: tmax
     real                             :: hoffmax
     real                             :: grid_width
     logical                          :: anisotropic
     logical                          :: constant_vs_vp_l
     logical                          :: dump_vel_l
     logical                          :: t2d
     logical                          :: vzflag
     logical                          :: verbos
     real, pointer                    :: tmap(:,:)
     real, pointer                    :: xr  (:,:)
     real, pointer                    :: tr  (:,:)
     real                             :: dtmap
     integer                          :: nzd
     integer                          :: np
     integer                          :: nxmap
     integer                          :: nzmap
     real                             :: dzmap
     real                             :: zsrc
     integer                          :: noint
     real, pointer                    :: cube(:,:,:)
     real, pointer                    :: vp0(:,:)
     !type(modmo2d_pprop_struct)         :: vp0obj
     type(modgrid_struct),pointer     :: vp0_obj
     character(len=8)                 :: vp0_desc
     character(len=FILENAME_LENGTH)   :: vp0_dfile
     character(len=8)                 :: vp0_wtype
     character(len=8)                 :: vp0_ftype
     double precision                 :: vp0_fsize
     integer                          :: vp0_xhdr
     integer                          :: vp0_yhdr
     real                             :: vp0_xgridwidth
     real                             :: vp0_ygridwidth
     integer                          :: vp0_ix
     integer                          :: vp0_iy
     integer                          :: vp0_iz
     type(trcio_struct),pointer       :: vp0_trcio
     integer, dimension(modmo2d_mgdim)  :: vp0_hdwd
     integer, dimension(modmo2d_mgdim)  :: vp0_ng
     real,    dimension(modmo2d_mgdim)  :: vp0_og
     real,    dimension(modmo2d_mgdim)  :: vp0_dg
     real, pointer                    :: vp0_data(:)
     real, pointer                    :: vs0(:,:)
     !type(modmo2d_pprop_struct)         :: vs0obj
     type(modgrid_struct),pointer     :: vs0_obj
     character(len=8)                 :: vs0_desc
     character(len=FILENAME_LENGTH)   :: vs0_dfile
     character(len=8)                 :: vs0_wtype
     character(len=8)                 :: vs0_ftype
     double precision                 :: vs0_fsize
     integer                          :: vs0_xhdr
     integer                          :: vs0_yhdr
     real                             :: vs0_xgridwidth
     real                             :: vs0_ygridwidth
     integer                          :: vs0_ix
     integer                          :: vs0_iy
     integer                          :: vs0_iz
     integer, dimension(modmo2d_mgdim)  :: vs0_hdwd
     integer, dimension(modmo2d_mgdim)  :: vs0_ng
     real,    dimension(modmo2d_mgdim)  :: vs0_og
     real,    dimension(modmo2d_mgdim)  :: vs0_dg
     real, pointer                    :: vs0_data(:)
     real, pointer                    :: eps(:,:)
     !type(modmo2d_pprop_struct)         :: epsobj
     type(modgrid_struct),pointer     :: eps_obj
     character(len=8)                 :: eps_desc
     character(len=FILENAME_LENGTH)   :: eps_dfile
     character(len=8)                 :: eps_wtype
     character(len=8)                 :: eps_ftype
     double precision                 :: eps_fsize
     integer                          :: eps_xhdr
     integer                          :: eps_yhdr
     real                             :: eps_xgridwidth
     real                             :: eps_ygridwidth
     integer                          :: eps_ix
     integer                          :: eps_iy
     integer                          :: eps_iz
     integer, dimension(modmo2d_mgdim)  :: eps_hdwd
     integer, dimension(modmo2d_mgdim)  :: eps_ng
     real,    dimension(modmo2d_mgdim)  :: eps_og
     real,    dimension(modmo2d_mgdim)  :: eps_dg
     real, pointer                    :: eps_data(:)
     real, pointer                    :: del(:,:)
     !type(modmo2d_pprop_struct)         :: delobj
     type(modgrid_struct),pointer     :: del_obj
     character(len=8)                 :: del_desc
     character(len=FILENAME_LENGTH)   :: del_dfile
     character(len=8)                 :: del_wtype
     character(len=8)                 :: del_ftype
     double precision                 :: del_fsize
     integer                          :: del_xhdr
     integer                          :: del_yhdr
     real                             :: del_xgridwidth
     real                             :: del_ygridwidth
     integer                          :: del_ix
     integer                          :: del_iy
     integer                          :: del_iz
     integer, dimension(modmo2d_mgdim)  :: del_hdwd
     integer, dimension(modmo2d_mgdim)  :: del_ng
     real,    dimension(modmo2d_mgdim)  :: del_og
     real,    dimension(modmo2d_mgdim)  :: del_dg
     real, pointer                    :: del_data(:)
   end type modmo2d_struct

   ! Data object for a physical property (velocity, anisotropy parameter, etc.)
   ! This type is not used due to memory problems -- needs further
   ! investigation.
   type,public :: modmo2d_pprop_struct
     private
     type(modgrid_struct),pointer     :: mgobj
     character(len=8)                 :: desc
     character(len=FILENAME_LENGTH)   :: dfile
     character(len=8)                 :: wtype
     character(len=8)                 :: ftype
     integer                          :: fsize
     integer                          :: xhdr
     integer                          :: yhdr
     integer, dimension(modmo2d_mgdim)  :: hdwd
     integer, dimension(modmo2d_mgdim)  :: ng
     real,    dimension(modmo2d_mgdim)  :: og
     real,    dimension(modmo2d_mgdim)  :: dg
     real, pointer                    :: data(:)
   end type modmo2d_pprop_struct

   type, public :: modmo2d_deriv_vti1d_struct
     real                             :: p0
     real, pointer                    :: vp(:)
     real, pointer                    :: vs(:)
     real, pointer                    :: eps(:)
     real, pointer                    :: del(:)
     real                             :: z0
     real                             :: dz
     integer                          :: nz
     real                             :: ud
   end type modmo2d_deriv_vti1d_struct

   type, public :: modmo2d_deriv_vti2d_struct
     real                             :: p0
     real, pointer                    :: vp(:,:)
     real, pointer                    :: vs(:,:)
     real, pointer                    :: eps(:,:)
     real, pointer                    :: del(:,:)
     real                             :: x0
     real                             :: dx
     integer                          :: nx
     real                             :: z0
     real                             :: dz
     integer                          :: nz
     real                             :: ud
   end type modmo2d_deriv_vti2d_struct

   type, public :: modmo2d_deriv_iso2d_struct
     real                             :: p0
     real, pointer                    :: vp(:,:)
     real                             :: x0
     real                             :: dx
     integer                          :: nx
     real                             :: z0
     real                             :: dz
     integer                          :: nz
     real                             :: ud
   end type modmo2d_deriv_iso2d_struct

   type, public :: modmo2d_deriv_struct
     integer                          :: deriv_type
     type(modmo2d_deriv_vti1d_struct) :: vti1d
     type(modmo2d_deriv_vti2d_struct) :: vti2d
     type(modmo2d_deriv_iso2d_struct) :: iso2d
   end type modmo2d_deriv_struct



!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

   integer                  ,save     :: lunprint  ! unit number for printing.
   type(modmo2d_struct),pointer,save    :: object    ! needed for traps.
   integer, parameter                 :: num_opt_ani_type = 2
   character(len=10), parameter       :: opt_ani_type(num_opt_ani_type) &
                                            = (/'VTI       ', 'ISOTROPIC '/)
   integer, parameter            :: num_opt_constant_vs_vp = 2
   character(len=10), parameter  :: opt_constant_vs_vp(num_opt_constant_vs_vp) &
                                            = (/'NO ', 'YES'/)
   integer, parameter                 :: num_opt_time2depth = 2
   character(len=10), parameter       :: opt_time2depth(num_opt_time2depth) &
                                            = (/'TIME2DEPTH', 'DEPTH2TIME'/)
   integer, parameter                 :: num_opt_single_vz = 2
   character(len=3), parameter        :: opt_single_vz(num_opt_single_vz) &
                                            = (/'NO ', 'YES'/)
   integer, parameter                 :: num_opt_acq_azimuth = 2
   character(len=5), parameter        :: opt_acq_azimuth(num_opt_acq_azimuth) &
                                            = (/'ILINE', 'XLINE'/)
   integer, parameter                 :: num_opt_dump_vel = 2
   character(len=3), parameter        :: opt_dump_vel(num_opt_dump_vel) &
                                            = (/'NO ', 'YES'/)
   integer, parameter                 :: num_opt_grid_width_type = 3
   character(len=15), parameter       :: opt_grid_width_type( &
                                                      num_opt_grid_width_type) &
                                            = (/'X_GRID_DISTANCE',  &
                                                'Y_GRID_DISTANCE', &
                                                'USER_DEFINED   '/)


   contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


   subroutine modmo2d_create (obj)
      type(modmo2d_struct),pointer :: obj       ! arguments
      integer                    :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in modmo2d_create")

      nullify(obj%vp0_obj)
      nullify(obj%vp0_data)
      nullify(obj%vs0_obj)
      nullify(obj%vs0_data)
      nullify(obj%eps_obj)
      nullify(obj%eps_data)
      nullify(obj%del_obj)
      nullify(obj%del_data)
      nullify (obj%vp0_file_choose) ! jpa
      nullify (obj%vs0_file_choose) ! jpa
      nullify (obj%eps_file_choose) ! jpa
      nullify (obj%del_file_choose) ! jpa
      nullify (obj%vp0_dumpfile_choose) ! jpa
      nullify (obj%tmap) ! jpa
      nullify (obj%xr) ! jpa
      nullify (obj%tr) ! jpa
      nullify (obj%vp0) ! jpa
      nullify (obj%vs0) ! jpa
      nullify (obj%eps) ! jpa
      nullify (obj%del) ! jpa
      nullify (obj%cube) ! jpa
      nullify (obj%vp0_trcio) ! jpa

      call pathchoose_create(obj%vp0_file_choose, 'vp0_file', '*')
      call pathchoose_create(obj%vs0_file_choose, 'vs0_file', '*')
      call pathchoose_create(obj%eps_file_choose, 'eps_file', '*')
      call pathchoose_create(obj%del_file_choose, 'del_file', '*')
      call pathchoose_create(obj%vp0_dumpfile_choose, 'vp0_dumpfile', '*')

      ! Nullify any additional pointers in the OBJ data structure here.
      call memman_nullify(obj%tmap, 'tmap')
      call memman_nullify(obj%xr,   'xr')
      call memman_nullify(obj%tr,   'tr')
      call memman_nullify(obj%vp0,  'vp0')
      call memman_nullify(obj%vs0,  'vs0')
      call memman_nullify(obj%eps,  'eps')
      call memman_nullify(obj%del,  'del')
      call memman_nullify(obj%cube, 'cube')

      call modmo2d_initialize (obj)
   end subroutine modmo2d_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


   subroutine modmo2d_delete (obj)
      type(modmo2d_struct),pointer  :: obj       ! arguments

      ! Local variables
      integer                     :: ierr      ! for error checking

      ierr = 0

      call modmo2d_wrapup (obj)

      call modmo2d_dealloc(obj, ierr)
      if (ierr /= 0) &
         call pc_warning ("error deallocating obj members in modmo2d_delete")

      call memman_free(obj%tmap)
      call memman_free(obj%xr)
      call memman_free(obj%tr)
      call memman_free(obj%vp0)
      call memman_free(obj%vs0)
      call memman_free(obj%eps)
      call memman_free(obj%del)
      call memman_free(obj%cube)

      if (associated(obj%vp0_data)) nullify(obj%vp0_data)
      if (associated(obj%vs0_data)) nullify(obj%vs0_data)
      if (associated(obj%eps_data)) nullify(obj%eps_data)
      if (associated(obj%del_data)) nullify(obj%del_data)
      call modgrid_delete(obj%vp0_obj)
      call modgrid_delete(obj%vs0_obj)
      call modgrid_delete(obj%eps_obj)
      call modgrid_delete(obj%del_obj)
      call pathchoose_delete(obj%vp0_file_choose)
      call pathchoose_delete(obj%vs0_file_choose)
      call pathchoose_delete(obj%eps_file_choose)
      call pathchoose_delete(obj%del_file_choose)
      call pathchoose_delete(obj%vp0_dumpfile_choose)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning("error deallocating obj in modmo2d_delete")
   end subroutine modmo2d_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


   subroutine modmo2d_initialize (obj)
      type(modmo2d_struct), intent(inout) :: obj       ! arguments

      ! Default values
      obj%ang       = 89.9
      obj%dang      = 0.5
      obj%dxmap     = 5.0
      obj%nz        = 1000
      obj%z0        = 0.0
      obj%dz        = 5.0
      obj%depth_scale = 1.0
      obj%nzm       = obj%nz
      obj%z0m       = obj%z0
      obj%dzm       = obj%dz
      obj%nt        = 1000
      obj%t0        = 0.0
      obj%delt      = 0.004
      obj%ismoo     = 5
      obj%ani_type  = 'ISOTROPIC'
      obj%anisotropic = .false.
      obj%vp0_file  = PATHCHECK_EMPTY
      obj%constant_vs_vp   = 'NO'
      obj%constant_vs_vp_l = .false.
      obj%vs_vp_ratio      = 0.5
      obj%vs0_file  = PATHCHECK_EMPTY
      obj%eps_file  = PATHCHECK_EMPTY
      obj%del_file  = PATHCHECK_EMPTY
      obj%offmax    = 4000.0
      obj%time2depth= 'TIME2DEPTH'
      obj%t2d       = .true.
      obj%single_vz = 'NO'
      obj%ilinehdr  = 7
      obj%xlinehdr  = 8
      obj%iverbos   = 0
      obj%dump_vel    = 'NO'
      obj%dump_vel_l  = .false.
      obj%vp0_dumpfile = PATHCHECK_EMPTY
      obj%acq_azimuth = 'ILINE'
      obj%grid_width_type = 'USER_DEFINED'
      obj%grid_width_user = 1.0
      obj%grid_width      = obj%grid_width_user

      obj%vp0_desc  = 'vp0'
      obj%vp0_dfile = ' '
      obj%vp0_wtype = ' '
      obj%vp0_ftype = ' '
      obj%vp0_fsize = 0
      obj%vp0_xhdr  = obj%ilinehdr
      obj%vp0_yhdr  = obj%xlinehdr
      obj%vp0_xgridwidth = 1.0
      obj%vp0_ygridwidth = 1.0

      obj%vs0_desc  = 'vs0'
      obj%vs0_dfile = ' '
      obj%vs0_wtype = ' '
      obj%vs0_ftype = ' '
      obj%vs0_fsize = 0
      obj%vs0_xhdr  = obj%ilinehdr
      obj%vs0_yhdr  = obj%xlinehdr
      obj%vs0_xgridwidth = 1.0
      obj%vs0_ygridwidth = 1.0

      obj%eps_desc  = 'eps'
      obj%eps_dfile = ' '
      obj%eps_wtype = ' '
      obj%eps_ftype = ' '
      obj%eps_fsize = 0
      obj%eps_xhdr  = obj%ilinehdr
      obj%eps_yhdr  = obj%xlinehdr
      obj%eps_xgridwidth = 1.0
      obj%eps_ygridwidth = 1.0

      obj%del_desc  = 'del'
      obj%del_dfile = ' '
      obj%del_wtype = ' '
      obj%del_ftype = ' '
      obj%del_fsize = 0
      obj%del_xhdr  = obj%ilinehdr
      obj%del_yhdr  = obj%xlinehdr
      obj%del_xgridwidth = 1.0
      obj%del_ygridwidth = 1.0

      call modmo2d_update (obj)
   end subroutine modmo2d_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


   subroutine modmo2d_update (obj)
      type(modmo2d_struct), intent(inout),target :: obj             ! arguments

      ! Local variables
      integer :: ierr
      character(len=FILENAME_LENGTH) :: filename

      ierr = 0
      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      if (pathchoose_update(obj%vp0_file_choose, obj%vp0_file, &
                            modmo2d_vp0_file)) return
      if (pathchoose_update(obj%vs0_file_choose, obj%vs0_file, &
                            modmo2d_vs0_file)) return
      if (pathchoose_update(obj%eps_file_choose, obj%eps_file, &
                            modmo2d_eps_file)) return
      if (pathchoose_update(obj%del_file_choose, obj%del_file, &
                            modmo2d_del_file)) return
      if (pathchoose_update(obj%vp0_dumpfile_choose, obj%vp0_dumpfile, &
                            modmo2d_vp0_dumpfile)) return

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      obj%ipn = pc_get_ipn()

      call pc_get_global('numtr',    obj%numtr)
      call pc_get_global('gathered', obj%gathered)
      call pc_get_global('nwih',     obj%nwih)
      call pc_get_global('ndpt',     obj%ndpt)
      call pc_get_global('tstrt',    obj%tstrt)
      call pc_get_global('dt',       obj%dt)
      call pc_get_global('grid',     obj%grid)

      call pc_get('ANI_TYPE',   obj%ani_type,   modmo2d_ani_type)
      call pc_get('TIME2DEPTH', obj%time2depth, modmo2d_time2depth)
      call pc_get('ANG',        obj%ang,        modmo2d_ang)
      call pc_get('DANG',       obj%dang,       modmo2d_dang)
      call pc_get('DXMAP',      obj%dxmap,      modmo2d_dxmap)
      call pc_get('DEPTH_SCALE',obj%depth_scale,modmo2d_depth_scale)
      call pc_get('NZ',         obj%nz,         modmo2d_nz)
      call pc_get('DZ',         obj%dz,         modmo2d_dz)
      call pc_get('Z0',         obj%z0,         modmo2d_z0)
      call pc_get('NZM',        obj%nzm,        modmo2d_nzm)
      call pc_get('DZM',        obj%dzm,        modmo2d_dzm)
      call pc_get('Z0M',        obj%z0m,        modmo2d_z0m)
      call pc_get('NT',         obj%nt,         modmo2d_nt)
      call pc_get('DELT',       obj%delt,       modmo2d_delt)
      call pc_get('T0',         obj%t0,         modmo2d_t0)
      call pc_get('ISMOO ',     obj%ismoo,      modmo2d_ismoo)
      call pc_get('VP0_FILE',   obj%vp0_file,   modmo2d_vp0_file)
      call pc_get('CONSTANT_VS_VP', obj%constant_vs_vp, &
                                                modmo2d_constant_vs_vp)
      call pc_get('VS_VP_RATIO', obj%vs_vp_ratio, modmo2d_vs_vp_ratio)
      call pc_get('VS0_FILE',   obj%vs0_file,   modmo2d_vs0_file)
      call pc_get('EPS_FILE',   obj%eps_file,   modmo2d_eps_file)
      call pc_get('DEL_FILE',   obj%del_file,   modmo2d_del_file)
      call pc_get('OFFMAX',     obj%offmax,     modmo2d_offmax)
      call pc_get('SINGLE_VZ',  obj%single_vz,  modmo2d_single_vz)
      call pc_get('ILINEHDR',   obj%ilinehdr,   modmo2d_ilinehdr)
      call pc_get('XLINEHDR',   obj%xlinehdr,   modmo2d_xlinehdr)
      call pc_get('IVERBOS',    obj%iverbos,    modmo2d_iverbos)
      call pc_get('DUMP_VEL',   obj%dump_vel,   modmo2d_dump_vel)
      call pc_get('VP0_DUMPFILE',obj%vp0_dumpfile,modmo2d_vp0_dumpfile)
      call pc_get('ACQ_AZIMUTH', obj%acq_azimuth, modmo2d_acq_azimuth)
      call pc_get('GRID_WIDTH_TYPE', obj%grid_width_type, &
                                                modmo2d_grid_width_type)
      call pc_get('GRID_WIDTH_USER', obj%grid_width_user, &
                                                modmo2d_grid_width_user)

      call pc_put_options_field('ANI_TYPE',       opt_ani_type,       &
                                num_opt_ani_type)
      call pc_put_options_field('CONSTANT_VS_VP', opt_constant_vs_vp, &
                                num_opt_constant_vs_vp)
      call pc_put_options_field('TIME2DEPTH',     opt_time2depth,     &
                                num_opt_time2depth)
      call pc_put_options_field('SINGLE_VZ',      opt_single_vz,      &
                                num_opt_single_vz)
      call pc_put_options_field('DUMP_VEL',       opt_dump_vel,       &
                                num_opt_dump_vel)
      call pc_put_options_field('ACQ_AZIMUTH',    opt_acq_azimuth,    &
                                num_opt_acq_azimuth)
      call pc_put_options_field('GRID_WIDTH_TYPE',opt_grid_width_type,&
                                num_opt_grid_width_type)

!      *** screen traps ***

      call pc_call_screen_trap('MODMO2D', modmo2d_modmo2d)

!      *** end trap ***

      call pc_call_end_trap(modmo2d_end)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      ! Check global argument numtr
      if (obj%numtr < 1) then
         call pc_error('modmo2d: Global numtr must be >=1')
      end if
      ! Check global argument gathered
      if (.not.obj%gathered) then
         call pc_error('modmo2d: Input data must be gathered.  Please &
                       &insert GATHER before MODMO2D.')
      end if

      if (obj%t2d) then
         obj%ndpt_out  = obj%nz
         obj%tstrt_out = obj%z0/obj%depth_scale
         obj%dt_out    = obj%dz/obj%depth_scale
      else
         obj%ndpt_out  = obj%nt
         obj%tstrt_out = obj%t0
         obj%dt_out    = obj%delt
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_global('ndpt',     obj%ndpt_out)
      call pc_put_global('tstrt',    obj%tstrt_out)
      call pc_put_global('dt',       obj%dt_out)

      call pc_put('ANG',            obj%ang)
      call pc_put('DANG',           obj%dang)
      call pc_put('DXMAP',          obj%dxmap)
      call pc_put('DEPTH_SCALE',    obj%depth_scale)
      call pc_put('NZ',             obj%nz)
      call pc_put('DZ',             obj%dz)
      call pc_put('Z0',             obj%z0)
      call pc_put('NZM',            obj%nzm)
      call pc_put('DZM',            obj%dzm)
      call pc_put('Z0M',            obj%z0m)
      call pc_put('NT',             obj%nt)
      call pc_put('DELT',           obj%delt)
      call pc_put('T0',             obj%t0)
      call pc_put('ISMOO',          obj%ismoo)
      call pc_put('ANI_TYPE',       obj%ani_type)
      call pc_put('VP0_FILE',       obj%vp0_file)
      call pc_put('CONSTANT_VS_VP', obj%constant_vs_vp)
      call pc_put('VS_VP_RATIO',    obj%vs_vp_ratio)
      call pc_put('VS0_FILE',       obj%vs0_file)
      call pc_put('EPS_FILE',       obj%eps_file)
      call pc_put('DEL_FILE',       obj%del_file)
      call pc_put('OFFMAX',         obj%offmax)
      call pc_put('TIME2DEPTH',     obj%time2depth)
      call pc_put('SINGLE_VZ',      obj%single_vz)
      call pc_put('ILINEHDR',       obj%ilinehdr)
      call pc_put('XLINEHDR',       obj%xlinehdr)
      call pc_put('IVERBOS',        obj%iverbos)
      call pc_put('DUMP_VEL',       obj%dump_vel)
      call pc_put('VP0_DUMPFILE',   obj%vp0_dumpfile)
      call pc_put('ACQ_AZIMUTH',    obj%acq_azimuth)
      call pc_put('GRID_WIDTH_TYPE',obj%grid_width_type)
      call pc_put('GRID_WIDTH_USER',obj%grid_width_user)

      ! Control defaults
      call pc_put_control('ntapes',                0)
      call pc_put_control('need_request',          .false.)
      call pc_put_control('need_label',            .false.)
      call pc_put_control('twosets',               .false.)
      call pc_put_control('nscratch',              0)
      call pc_put_control('nstore',                0)
      call pc_put_control('iftd',                  .false.)
      call pc_put_control('ndisk',                 0)
      call pc_put_control('setup_only',            .false.)
      call pc_put_control('parallel_safe',         .true.)
      call pc_put_control('pcps_send_mode',        'pcps_send_first_avail')
      call pc_put_control('pcps_receive_mode',     'pcps_receive_passthru')
      call pc_put_control('pcps_bunch_mode',       'pcps_bunch_trace_groups')
      call pc_put_control('pcps_send_eof_mode',    'pcps_send_all_eof')
      call pc_put_control('pcps_alt_send_mode',    'pcps_send_all')
      call pc_put_control('pcps_alt_receive_mode', 'pcps_receive_all_eof')

      ! Add here any other parameter cache calls such as to set sensitivities.
      !call pc_put_sensitive_field_flag('vp0_file',        .true.)
      !call pc_put_sensitive_field_flag('select_vp0_file', .true.)
      !call pc_put_sensitive_field_flag('vs0_file',        .true.)
      !call pc_put_sensitive_field_flag('select_vs0_file', .true.)
      !call pc_put_sensitive_field_flag('eps_file',        .true.)
      !call pc_put_sensitive_field_flag('select_eps_file', .true.)
      !call pc_put_sensitive_field_flag('del_file',        .true.)
      !call pc_put_sensitive_field_flag('select_del_file', .true.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      ! Initialize variables for execution or deallocate arrays
      ! which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      ! Allocate needed permanent memory.
      call modmo2d_set_parms(obj, ierr)
      if (ierr /= 0) call pc_error('modmo2d: error setting parameters')
      call modmo2d_alloc(obj, ierr)
      if (ierr /= 0) call pc_error('modmo2d: error allocating permanent memory')

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      ! Initialize anything needed for actual execution of process.
      call modmo2d_vel_init(obj, obj%vp0_obj, obj%vp0_file, obj%vp0_desc, &
                          obj%vp0_dfile, obj%vp0_wtype, obj%vp0_ftype,    &
                          obj%vp0_xhdr, obj%vp0_yhdr, obj%vp0_xgridwidth, &
                          obj%vp0_ygridwidth, obj%vp0_ix,                 &
                          obj%vp0_iy, obj%vp0_iz, obj%vp0_hdwd,           &
                          obj%vp0_ng, obj%vp0_og, obj%vp0_dg, ierr)
      if (ierr /= 0) call pc_error('modmo2d: error in vel_init for vp0')
      if (obj%dump_vel_l) then
        if (pcps_get_num_procs() > 1) then
          write(filename, '(A,A,I4.4)') trim(obj%vp0_dumpfile), '.pe', &
                pcps_get_worker_num()
        else
          write(filename, '(A)') trim(obj%vp0_dumpfile)
        end if
        obj%vp0_trcio => trcio_open(filename, 'w', .false., 64, obj%nzm, 32, 64)
        if (.not.associated(obj%vp0_trcio)) &
          call pc_error('modmo2d: vp0 dump: error opening dump file', filename)
        ierr = trcio_set_ipn (obj%vp0_trcio, obj%ipn)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo2d: vp0 dump: error setting ipn')
        ierr = trcio_set_dt  (obj%vp0_trcio, obj%dzm)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo2d: vp0 dump: error setting dt')
        ierr = trcio_set_tmin(obj%vp0_trcio, obj%z0m)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo2d: vp0 dump: error setting tmin')
        ierr = trcio_writeheader(obj%vp0_trcio)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo2d: vp0 dump: error writing dump file header')
      end if

      if (obj%anisotropic) then
        if (.not.obj%constant_vs_vp_l) then
          call modmo2d_vel_init(obj, obj%vs0_obj, obj%vs0_file, obj%vs0_desc, &
                              obj%vs0_dfile, obj%vs0_wtype, obj%vs0_ftype,    &
                              obj%vs0_xhdr, obj%vs0_yhdr, obj%vs0_xgridwidth, &
                              obj%vs0_ygridwidth, obj%vs0_ix,                 &
                              obj%vs0_iy, obj%vs0_iz, obj%vs0_hdwd,           &
                              obj%vs0_ng, obj%vs0_og, obj%vs0_dg, ierr)
          if (ierr /= 0) call pc_error('modmo2d: error in vel_init for vs0')
        end if

        call modmo2d_vel_init(obj, obj%eps_obj, obj%eps_file, obj%eps_desc, &
                            obj%eps_dfile, obj%eps_wtype, obj%eps_ftype,    &
                            obj%eps_xhdr, obj%eps_yhdr, obj%eps_xgridwidth, &
                            obj%eps_ygridwidth, obj%eps_ix,                 &
                            obj%eps_iy, obj%eps_iz, obj%eps_hdwd,           &
                            obj%eps_ng, obj%eps_og, obj%eps_dg, ierr)
        if (ierr /= 0) call pc_error('modmo2d: error in vel_init for eps')

        call modmo2d_vel_init(obj, obj%del_obj, obj%del_file, obj%del_desc, &
                            obj%del_dfile, obj%del_wtype, obj%del_ftype,    &
                            obj%del_xhdr, obj%del_yhdr, obj%del_xgridwidth, &
                            obj%del_ygridwidth, obj%del_ix,                 &
                            obj%del_iy, obj%del_iz, obj%del_hdwd,           &
                            obj%del_ng, obj%del_og, obj%del_dg, ierr)
        if (ierr /= 0) call pc_error('modmo2d: error in vel_init for del')
      end if

      call timer_alloc(modmo2d_timers(MODMO2D_TIMER_MAIN), ierr)
      if (ierr /= 0) call pc_error('modmo2d_update: timer_alloc failed')
      call timer_clear(modmo2d_timers(MODMO2D_TIMER_MAIN))

      call timer_alloc(modmo2d_timers(MODMO2D_TIMER_DO_GATHER), ierr)
      if (ierr /= 0) call pc_error('modmo2d_update: timer_alloc failed')
      call timer_clear(modmo2d_timers(MODMO2D_TIMER_DO_GATHER))

      call timer_alloc(modmo2d_timers(MODMO2D_TIMER_RT), ierr)
      if (ierr /= 0) call pc_error('modmo2d_update: timer_alloc failed')
      call timer_clear(modmo2d_timers(MODMO2D_TIMER_RT))

      call timer_alloc(modmo2d_timers(MODMO2D_TIMER_TD), ierr)
      if (ierr /= 0) call pc_error('modmo2d_update: timer_alloc failed')
      call timer_clear(modmo2d_timers(MODMO2D_TIMER_TD))

      call timer_alloc(modmo2d_timers(MODMO2D_TIMER_DT), ierr)
      if (ierr /= 0) call pc_error('modmo2d_update: timer_alloc failed')
      call timer_clear(modmo2d_timers(MODMO2D_TIMER_DT))


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


   end subroutine modmo2d_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

   subroutine modmo2d_ani_type(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      call string_to_upper(object%ani_type)
      select case(object%ani_type)
         case('ISOTROPIC')
            object%anisotropic = .false.
            call pc_put_sensitive_field_flag('vp0_file',        .true.)
            call pc_put_sensitive_field_flag('select_vp0_file', .true.)
            call pc_put_sensitive_field_flag('constant_vs_vp',  .false.)
            call pc_put_sensitive_field_flag('vs_vp_ratio',     .false.)
            call pc_put_sensitive_field_flag('vs0_file',        .false.)
            call pc_put_sensitive_field_flag('select_vs0_file', .false.)
            call pc_put_visible_flag('vs0_file_info',           .false.)
            call pc_put_sensitive_field_flag('eps_file',        .false.)
            call pc_put_sensitive_field_flag('select_eps_file', .false.)
            call pc_put_visible_flag('eps_file_info',           .false.)
            call pc_put_sensitive_field_flag('del_file',        .false.)
            call pc_put_sensitive_field_flag('select_del_file', .false.)
            call pc_put_visible_flag('del_file_info',           .false.)
         case('VTI')
            object%anisotropic = .true.
            call pc_put_sensitive_field_flag('vp0_file',        .true.)
            call pc_put_sensitive_field_flag('select_vp0_file', .true.)
            call pc_put_sensitive_field_flag('constant_vs_vp',  .true.)
            call modmo2d_constant_vs_vp(keyword)
            call pc_put_sensitive_field_flag('eps_file',        .true.)
            call pc_put_sensitive_field_flag('select_eps_file', .true.)
            call pc_put_visible_flag('eps_file_info',           .true.)
            call pc_put_sensitive_field_flag('del_file',        .true.)
            call pc_put_sensitive_field_flag('select_del_file', .true.)
            call pc_put_visible_flag('del_file_info',           .true.)
         case default
            object%ani_type    = 'ISOTROPIC'
            object%anisotropic = .false.
            call pc_put_sensitive_field_flag('vp0_file',        .true.)
            call pc_put_sensitive_field_flag('select_vp0_file', .true.)
            call pc_put_sensitive_field_flag('constant_vs_vp',  .false.)
            call pc_put_sensitive_field_flag('vs_vp_ratio',     .false.)
            call pc_put_sensitive_field_flag('vs0_file',        .false.)
            call pc_put_sensitive_field_flag('select_vs0_file', .false.)
            call pc_put_visible_flag('vs0_file_info',           .false.)
            call pc_put_sensitive_field_flag('eps_file',        .false.)
            call pc_put_sensitive_field_flag('select_eps_file', .false.)
            call pc_put_visible_flag('eps_file_info',           .false.)
            call pc_put_sensitive_field_flag('del_file',        .false.)
            call pc_put_sensitive_field_flag('select_del_file', .false.)
            call pc_put_visible_flag('del_file_info',           .false.)
            call pc_warning('modmo2d: ani_type must be ISOTROPIC or &
                            &VTI; setting ani_type to ISOTROPIC')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo2d_ani_type

   subroutine modmo2d_time2depth(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      call string_to_upper(object%time2depth)
      select case(object%time2depth)
         case('TIME2DEPTH')
            object%t2d        = .true.
            call pc_put_sensitive_field_flag('nz',   .true.)
            call pc_put_sensitive_field_flag('dz',   .true.)
            call pc_put_sensitive_field_flag('z0',   .true.)
            call pc_put_sensitive_field_flag('nt',   .false.)
            call pc_put_sensitive_field_flag('delt', .false.)
            call pc_put_sensitive_field_flag('t0',   .false.)
         case('DEPTH2TIME')
            object%t2d        = .false.
            call pc_put_sensitive_field_flag('nz',   .false.)
            call pc_put_sensitive_field_flag('dz',   .false.)
            call pc_put_sensitive_field_flag('z0',   .false.)
            call pc_put_sensitive_field_flag('nt',   .true.)
            call pc_put_sensitive_field_flag('delt', .true.)
            call pc_put_sensitive_field_flag('t0',   .true.)
         case default
            object%time2depth = 'TIME2DEPTH'
            object%t2d        = .true.
            call pc_put_sensitive_field_flag('nz',   .true.)
            call pc_put_sensitive_field_flag('dz',   .true.)
            call pc_put_sensitive_field_flag('z0',   .true.)
            call pc_put_sensitive_field_flag('nt',   .false.)
            call pc_put_sensitive_field_flag('delt', .false.)
            call pc_put_sensitive_field_flag('t0',   .false.)
            call pc_warning('modmo2d: time2depth must be TIME2DEPTH or &
                            &DEPTH2TIME; setting time2depth to TIME2DEPTH')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo2d_time2depth

   subroutine modmo2d_ang(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%ang <= 0.) .or. (object%ang > 90.)) then
         call pc_error('modmo2d: ang must be > 0. or <= 90.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_ang

   subroutine modmo2d_dang(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%dang <= 0.) then
         call pc_error('modmo2d: dang must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_dang

   subroutine modmo2d_dxmap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%dxmap <= 0.) then
         call pc_error('modmo2d: dxmap must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_dxmap

   subroutine modmo2d_depth_scale(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%depth_scale <= 0.) then
         call pc_error('modmo2d: depth_scale must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_depth_scale

   subroutine modmo2d_nz(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (.not.object%t2d) then
         object%nz = object%ndpt
      end if
      if (object%nz <= 0) then
         call pc_error('modmo2d: nz must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_nz

   subroutine modmo2d_dz(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (.not.object%t2d) then
         object%dz = object%dt * object%depth_scale
      end if
      if (object%dz <= 0.) then
         call pc_error('modmo2d: dz must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_dz

   subroutine modmo2d_z0(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (.not.object%t2d) then
         object%z0 = object%tstrt * object%depth_scale
      end if
      if (object%z0 < 0.) then
         call pc_error('modmo2d: z0 must be >= 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_z0

   subroutine modmo2d_nzm(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%nzm <= 0) then
         call pc_error('modmo2d: nzm must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_nzm

   subroutine modmo2d_dzm(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%dzm <= 0.) then
         call pc_error('modmo2d: dzm must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_dzm

   subroutine modmo2d_z0m(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%z0m < 0.) then
         call pc_error('modmo2d: z0m must be >= 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_z0m

   subroutine modmo2d_nt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%t2d) then
         object%nt = object%ndpt
      end if
      if (object%nt <= 0) then
         call pc_error('modmo2d: nt must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_nt

   subroutine modmo2d_delt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%t2d) then
         object%delt = object%dt
      end if
      if (object%delt <= 0.) then
         call pc_error('modmo2d: delt must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_delt

   subroutine modmo2d_t0(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%t2d) then
         object%t0 = object%tstrt
      end if
      if (object%t0 < 0.) then
         call pc_error('modmo2d: t0 must be >= 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_t0

   subroutine modmo2d_ismoo(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%ismoo < 0) then
         call pc_error('modmo2d: ismoo must be >= 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_ismoo

   subroutine modmo2d_vp0_file(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      integer                      :: status
      character(len=120)           :: infostr
      character(len=7)             :: tmpstr
      character(len=PC_LENGTH)     :: tmpkeyword
      tmpstr = keyword(1:7)
      call string_to_upper(tmpstr)
      if (tmpstr == 'SELECT_') then
         tmpkeyword = keyword(8:)
      else
         tmpkeyword = keyword(1:)
      end if
      call pathcheck(tmpkeyword, object%vp0_file, required=.true., &
                     show=PATHCHECK_INFO_INPUT, status=status)
      if (status /= PATHCHECK_VALID) then
         call pc_error('modmo2d: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%vp0_ftype=modgrid_ftype(object%vp0_file, lunprint, &
                                     object%vp0_fsize)
      if (object%vp0_ftype == 'UNKNOWN') then
         call pc_error('modmo2d: vp0 modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo2d: vp0 modgrid filetype is "',        &
                          trim(object%vp0_ftype), '", filesize is ', &
                          object%vp0_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo2d_vp0_file

   subroutine modmo2d_constant_vs_vp(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (.not.object%anisotropic) then
         object%constant_vs_vp    = 'NO'
         object%constant_vs_vp_l = .false.
         call pc_put_sensitive_field_flag('vs0_file',        .false.)
         call pc_put_sensitive_field_flag('select_vs0_file', .false.)
         call pc_put_visible_flag('vs0_file_info',           .false.)
         call pc_put_sensitive_field_flag('vs_vp_ratio',     .false.)
         return
      end if

      call string_to_upper(object%constant_vs_vp)
      select case(object%constant_vs_vp)
         case('YES')
            object%constant_vs_vp_l = .true.
            call pc_put_sensitive_field_flag('vs0_file',        .false.)
            call pc_put_sensitive_field_flag('select_vs0_file', .false.)
            call pc_put_visible_flag('vs0_file_info',           .false.)
            call pc_put_sensitive_field_flag('vs_vp_ratio',     .true.)
         case('NO')
            object%constant_vs_vp_l = .false.
            call pc_put_sensitive_field_flag('vs0_file',        .true.)
            call pc_put_sensitive_field_flag('select_vs0_file', .true.)
            call pc_put_visible_flag('vs0_file_info',           .true.)
            call pc_put_sensitive_field_flag('vs_vp_ratio',     .false.)
         case default
            object%constant_vs_vp    = 'NO'
            object%constant_vs_vp_l = .false.
            call pc_put_sensitive_field_flag('vs0_file',        .true.)
            call pc_put_sensitive_field_flag('select_vs0_file', .true.)
            call pc_put_visible_flag('vs0_file_info',           .true.)
            call pc_put_sensitive_field_flag('vs_vp_ratio',     .false.)
            call pc_warning('modmo2d: constant_vs_vp must be YES or NO; &
                            &setting constant_vs_vp to NO')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo2d_constant_vs_vp

   subroutine modmo2d_vs_vp_ratio(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%vs_vp_ratio >= 1. .or. object%vs_vp_ratio < 0.) then
         call pc_error('modmo2d: vs_vp_ratio must be >= 0. and < 1.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_vs_vp_ratio

   subroutine modmo2d_vs0_file(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      integer                      :: status
      character(len=120)           :: infostr
      character(len=7)             :: tmpstr
      character(len=PC_LENGTH)     :: tmpkeyword

      if (.not.object%anisotropic) then
         return
      end if

      if (object%constant_vs_vp_l) then
         return
      end if

      tmpstr = keyword(1:7)
      call string_to_upper(tmpstr)
      if (tmpstr == 'SELECT_') then
         tmpkeyword = keyword(8:)
      else
         tmpkeyword = keyword(1:)
      end if
      call pathcheck(tmpkeyword, object%vs0_file, required=.true., &
                     show=PATHCHECK_INFO_INPUT, status=status)
      if (status /= PATHCHECK_VALID) then
         call pc_error('modmo2d: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%vs0_ftype=modgrid_ftype(object%vs0_file, lunprint, &
                                     object%vs0_fsize)
      if (object%vs0_ftype == 'UNKNOWN') then
         call pc_error('modmo2d: vs0 modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo2d: vs0 modgrid filetype is "',        &
                          trim(object%vs0_ftype), '", filesize is ', &
                          object%vs0_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo2d_vs0_file

   subroutine modmo2d_eps_file(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      integer                      :: status
      character(len=120)           :: infostr
      character(len=7)             :: tmpstr
      character(len=PC_LENGTH)     :: tmpkeyword

      if (.not.object%anisotropic) then
         return
      end if

      tmpstr = keyword(1:7)
      call string_to_upper(tmpstr)
      if (tmpstr == 'SELECT_') then
         tmpkeyword = keyword(8:)
      else
         tmpkeyword = keyword(1:)
      end if
      call pathcheck(tmpkeyword, object%eps_file, required=.true., &
                     show=PATHCHECK_INFO_INPUT, status=status)
      if (status /= PATHCHECK_VALID) then
         call pc_error('modmo2d: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%eps_ftype=modgrid_ftype(object%eps_file, lunprint, &
                                     object%eps_fsize)
      if (object%eps_ftype == 'UNKNOWN') then
         call pc_error('modmo2d: eps modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo2d: eps modgrid filetype is "',        &
                          trim(object%eps_ftype), '", filesize is ', &
                          object%eps_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo2d_eps_file

   subroutine modmo2d_del_file(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      integer                      :: status
      character(len=120)           :: infostr
      character(len=7)             :: tmpstr
      character(len=PC_LENGTH)     :: tmpkeyword

      if (.not.object%anisotropic) then
         return
      end if

      tmpstr = keyword(1:7)
      call string_to_upper(tmpstr)
      if (tmpstr == 'SELECT_') then
         tmpkeyword = keyword(8:)
      else
         tmpkeyword = keyword(1:)
      end if
      call pathcheck(tmpkeyword, object%del_file, required=.true., &
                     show=PATHCHECK_INFO_INPUT, status=status)
      if (status /= PATHCHECK_VALID) then
         call pc_error('modmo2d: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%del_ftype=modgrid_ftype(object%del_file, lunprint, &
                                     object%del_fsize)
      if (object%del_ftype == 'UNKNOWN') then
         call pc_error('modmo2d: del modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo2d: del modgrid filetype is "',        &
                          trim(object%del_ftype), '", filesize is ', &
                          object%del_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo2d_del_file

   subroutine modmo2d_offmax(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%offmax <= 0) then
         call pc_error('modmo2d: offmax must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_offmax

   subroutine modmo2d_single_vz(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      call string_to_upper(object%single_vz)
      select case(object%single_vz)
         case('NO', 'YES')
         case default
            call pc_error('modmo2d: single_vz must be YES or NO')
            call pc_jump_field(keyword)
      end select
      return
   end subroutine modmo2d_single_vz

   subroutine modmo2d_ilinehdr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%ilinehdr > object%nwih) .and. (object%ilinehdr < 1)) then
         call pc_error('modmo2d: ilinehdr must be between 1 and ', object%nwih)
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_ilinehdr

   subroutine modmo2d_xlinehdr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%xlinehdr > object%nwih) .and. (object%xlinehdr < 1)) then
         call pc_error('modmo2d: xlinehdr must be between 1 and ', object%nwih)
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_xlinehdr

   subroutine modmo2d_iverbos(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%iverbos /= 0) .and. (object%iverbos /= 1)) then
         call pc_error('modmo2d: iverbos must be 0 or 1')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_iverbos

   subroutine modmo2d_dump_vel(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      call string_to_upper(object%dump_vel)
      select case(object%dump_vel)
         case('YES')
            object%dump_vel_l = .true.
            call pc_put_sensitive_field_flag('vp0_dumpfile',        .true.)
            call pc_put_sensitive_field_flag('select_vp0_dumpfile', .true.)
            call pc_put_visible_flag('vp0_dumpfile_info',           .true.)
         case('NO')
            object%dump_vel_l = .false.
            call pc_put_sensitive_field_flag('vp0_dumpfile',        .false.)
            call pc_put_sensitive_field_flag('select_vp0_dumpfile', .false.)
            call pc_put_visible_flag('vp0_dumpfile_info',           .false.)
         case default
            object%dump_vel   = 'NO'
            object%dump_vel_l = .false.
            call pc_put_sensitive_field_flag('vp0_dumpfile',        .false.)
            call pc_put_sensitive_field_flag('select_vp0_dumpfile', .false.)
            call pc_put_visible_flag('vp0_dumpfile_info',           .false.)
            call pc_warning('modmo2d: dump_vel must be YES or NO; &
                            &setting dump_vel to NO')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo2d_dump_vel

   subroutine modmo2d_vp0_dumpfile(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      integer                      :: status

      character(len=7)             :: tmpstr
      character(len=PC_LENGTH)     :: tmpkeyword
      if (.not.object%dump_vel_l) return
      tmpstr = keyword(1:7)
      call string_to_upper(tmpstr)
      if (tmpstr == 'SELECT_') then
         tmpkeyword = keyword(8:)
      else
         tmpkeyword = keyword(1:)
      end if
      call pathcheck(tmpkeyword, object%vp0_dumpfile, required=.true., &
                     show=PATHCHECK_INFO_OUTPUT, status=status)
      if (status /= PATHCHECK_VALID) then
         call pc_error('modmo2d: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_vp0_dumpfile

   subroutine modmo2d_acq_azimuth(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      call string_to_upper(object%acq_azimuth)
      select case(object%acq_azimuth)
         case('ILINE', 'XLINE')
         case default
            object%acq_azimuth = 'ILINE'
            call pc_warning('modmo2d: acq_azimuth must be ILINE or XLINE; &
                            & setting acq_azimuth to ILINE')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo2d_acq_azimuth

   subroutine modmo2d_grid_width_type(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      call string_to_upper(object%grid_width_type)
      select case(object%grid_width_type)
         case('X_GRID_DISTANCE')
            call pc_put_sensitive_field_flag('grid_width_user', .false.)
         case('Y_GRID_DISTANCE')
            call pc_put_sensitive_field_flag('grid_width_user', .false.)
         case('USER_DEFINED')
            call pc_put_sensitive_field_flag('grid_width_user', .true.)
         case default
            call pc_put_sensitive_field_flag('grid_width_user', .true.)
            call pc_warning('modmo2d: grid_width_type not valid; &
                            &setting grid_width_type to USER_DEFINED')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo2d_grid_width_type

   subroutine modmo2d_grid_width_user(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%grid_width_user <= 0.) then
         call pc_error('modmo2d: grid_width_user must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo2d_grid_width_user


   ! *** Screen trap for  FOO ***
   subroutine modmo2d_modmo2d(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      ! Do nothing right now
      return
   end subroutine modmo2d_modmo2d

   ! *** End Trap ***
   subroutine modmo2d_end
      implicit none

      ! Do nothing right now
      return
   end subroutine modmo2d_end


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


   subroutine modmo2d (obj,ntr,hd,tr)

      implicit none

      ! Subroutine arguments
      type(modmo2d_struct), intent(inout) :: obj                    ! arguments
      integer,            intent(inout) :: ntr                    ! arguments
      double precision,   intent(inout) :: hd(:,:)                ! arguments
      real,               intent(inout) :: tr(:,:)                ! arguments

      ! Local variables
      integer :: ierr
      integer, save :: ntr_total=0

      ierr = 0
      call timer_start(modmo2d_timers(MODMO2D_TIMER_MAIN))
      select case (ntr)
      case (1:)
         call timer_start(modmo2d_timers(MODMO2D_TIMER_DO_GATHER))
         call modmo2d_do_gather(obj, ntr, hd, tr, ierr)
         call timer_stop(modmo2d_timers(MODMO2D_TIMER_DO_GATHER))
         ntr_total = ntr_total + ntr
         if (ierr /= 0) ntr = FATAL_ERROR
         call timer_stop(modmo2d_timers(MODMO2D_TIMER_MAIN))
         return
      case (NEED_TRACES)
         ntr = NEED_TRACES
         call timer_stop(modmo2d_timers(MODMO2D_TIMER_MAIN))
         return
      case (NO_MORE_TRACES)
         write(lunprint,*) 'modmo2d: Total number of traces processed = ', &
                           ntr_total
         call timer_stop(modmo2d_timers(MODMO2D_TIMER_MAIN))
         call modmo2d_wrapup(obj)
         return
      case default
         ntr = FATAL_ERROR
         call timer_stop(modmo2d_timers(MODMO2D_TIMER_MAIN))
         return
      end select
      call timer_stop(modmo2d_timers(MODMO2D_TIMER_MAIN))

      return
   end subroutine modmo2d


!!--------------------------- modmo2d_do_gather ------------------------------!!
!!--------------------------- modmo2d_do_gather ------------------------------!!
!!--------------------------- modmo2d_do_gather ------------------------------!!


   subroutine modmo2d_do_gather (obj, ntr, hd, tr, ierr)

      implicit none

      ! Subroutine arguments
      type(modmo2d_struct), intent(inout) :: obj                 ! arguments
      integer,            intent(inout) :: ntr                 ! arguments
      double precision,   intent(inout) :: hd(:,:)             ! arguments
      real,               intent(inout) :: tr(:,:)             ! arguments
      integer,            intent(inout) :: ierr

      ! Local variables
      integer, save      :: i_genmap=0
      integer, parameter :: hdi_off=6    ! offset header word index
      integer            :: itr
      integer            ::         i  
      real               :: offset
      real,    save      :: last_ycmp=-9999999.
      real               :: cur_xcmp, cur_ycmp
      double precision   :: vp0_hd(64)
      logical            :: dump
      integer            :: lundump


      ierr = 0
      dump = .false.

      ! Read in velocity function and create travel time map
      if (i_genmap == 0) then
         cur_ycmp = hd(obj%xlinehdr, 1)
         cur_xcmp = hd(obj%ilinehdr, 1)
         !if (obj%verbos) then
           write(lunprint,*) 'modmo2d_do_gather: cur_xcmp, cur_ycmp = ', &
                             cur_xcmp, cur_ycmp
         !end if
         call modmo2d_vel_paint_slice(obj%vp0_obj, obj%vp0_desc,              &
                                      obj%vp0_ix, obj%vp0_iy, obj%vp0_iz,     &
                                      obj%vp0_xhdr, obj%vp0_yhdr,             &
                                      obj%vp0_xgridwidth, obj%vp0_ygridwidth, &
                                      obj%nxmap, obj%dxmap,                   &
                                      obj%acq_azimuth,                        &
                                      obj%vp0_ng, obj%vp0_og, obj%vp0_dg,     &
                                      cur_xcmp, cur_ycmp,                     &
                                      obj%nzm, obj%z0m, obj%dzm,              &
                                      obj%vp0, obj%cube, ierr)
         if (obj%dump_vel_l) then
           vp0_hd = hd(:,1)
           do i=1,2*obj%nxmap+1
             vp0_hd(hdi_off) = (i - (obj%nxmap+1))*obj%dxmap
             call lav_set_hdr(vp0_hd, obj%vp0(:,i), obj%nzm)
             ierr = trcio_write_trace(obj%vp0_trcio, vp0_hd, obj%vp0(:,i))
             if (ierr /= TRCIO_OK) &
               call pc_error('modmo2d: vp0 dump: error writing dump file')
           end do
         end if
         if (obj%anisotropic) then
           if (obj%constant_vs_vp_l) then
             obj%vs0 = obj%vs_vp_ratio * obj%vp0
           else
             call modmo2d_vel_paint_slice(obj%vs0_obj, obj%vs0_desc,          &
                                          obj%vs0_ix, obj%vs0_iy, obj%vs0_iz, &
                                          obj%vs0_xhdr, obj%vs0_yhdr,         &
                                          obj%vs0_xgridwidth,                 &
                                          obj%vs0_ygridwidth,                 &
                                          obj%nxmap, obj%dxmap,               &
                                          obj%acq_azimuth,                    &
                                          obj%vs0_ng, obj%vs0_og, obj%vs0_dg, &
                                          cur_xcmp, cur_ycmp,                 &
                                          obj%nzm, obj%z0m, obj%dzm,          &
                                          obj%vs0, obj%cube, ierr)
           end if
           call modmo2d_vel_paint_slice(obj%eps_obj, obj%eps_desc,          &
                                        obj%eps_ix, obj%eps_iy, obj%eps_iz, &
                                        obj%eps_xhdr, obj%eps_yhdr,         &
                                        obj%eps_xgridwidth,                 &
                                        obj%eps_ygridwidth,                 &
                                        obj%nxmap, obj%dxmap,               &
                                        obj%acq_azimuth,                    &
                                        obj%eps_ng, obj%eps_og, obj%eps_dg, &
                                        cur_xcmp, cur_ycmp,                 &
                                        obj%nzm, obj%z0m, obj%dzm,          &
                                        obj%eps, obj%cube, ierr)
           call modmo2d_vel_paint_slice(obj%del_obj, obj%del_desc,          &
                                        obj%del_ix, obj%del_iy, obj%del_iz, &
                                        obj%del_xhdr, obj%del_yhdr,         &
                                        obj%del_xgridwidth,                 &
                                        obj%del_ygridwidth,                 &
                                        obj%nxmap, obj%dxmap,               &
                                        obj%acq_azimuth,                    &
                                        obj%del_ng, obj%del_og, obj%del_dg, &
                                        cur_xcmp, cur_ycmp,                 &
                                        obj%nzm, obj%z0m, obj%dzm,          &
                                        obj%del, obj%cube, ierr)
         end if

         call timer_start(modmo2d_timers(MODMO2D_TIMER_RT))
         if (obj%anisotropic) then
           if (obj%verbos) &
             write(lunprint,*) 'modmo2d_do_gather: before vzmapr_vti2d'
           call modmo2d_vzmapr_vti2d(obj%nxmap, obj%nzmap, obj%np,            &
                                     obj%theta_min, obj%theta_max,            &
                                     obj%vp0, obj%vs0, obj%eps, obj%del,      &
                                     obj%tmax, obj%dtmap, obj%tmap,           &
                                     obj%zsrc, obj%z0m, obj%dzmap,            &
                                     0., -obj%dxmap*obj%nxmap, obj%dxmap,     &
                                     obj%ismoo, obj%noint, 1, obj%xr, obj%tr, &
                                     ierr)
           if (obj%verbos) &
             write(lunprint,*) 'modmo2d_do_gather: after  vzmapr_vti2d'
         else
           if (obj%verbos) &
             write(lunprint,*) 'modmo2d_do_gather: before vzmapr_iso2d'
           call modmo2d_vzmapr_iso2d(obj%nxmap, obj%nzmap, obj%np,            &
                                     obj%theta_min, obj%theta_max,            &
                                     obj%vp0,                                 &
                                     obj%tmax, obj%dtmap, obj%tmap,           &
                                     obj%zsrc, obj%z0m, obj%dzmap,            &
                                     0., -obj%dxmap*obj%nxmap, obj%dxmap,     &
                                     obj%ismoo, obj%noint, 1, obj%xr, obj%tr, &
                                     ierr)
           if (obj%verbos) &
             write(lunprint,*) 'modmo2d_do_gather: after  vzmapr_iso2d'
         end if
      end if
      call timer_stop(modmo2d_timers(MODMO2D_TIMER_RT))
      if (dump) then
         lundump = cio_fopen('tmap_cerv2d.bin', 'w')
         if (lundump == 0) call pc_error('modmo2d_do_gather: error &
                                         &opening tmap_cerv2d.bin')
         i = cio_fwrite(obj%tmap, sizeof(obj%tmap(1,1)), &
                        obj%nzmap*obj%nxmap, lundump)
         if (i /= obj%nzmap*obj%nxmap) &
            call pc_error('modmo2d_do_gather: only wrote ', i, ' of ', &
                          obj%nzmap*obj%nxmap, ' items in tmap_cerv2d.bin')
         ierr = cio_fclose(lundump)
         if (ierr /= 0) call pc_error('modmo2d_do_gather: error closing &
                                      &tmap_cerv2d.bin')
      end if

      ! Reset i_genmap if there is only a single velocity trace
      if (obj%vzflag) i_genmap = 1

      ! Trace loop - convert to depth with moveout correction
      do itr=1,ntr
         offset = hd(hdi_off,itr)
         if (obj%t2d) then
            call timer_start(modmo2d_timers(MODMO2D_TIMER_TD))
            call modmo2d_t2dzmo(offset, obj%dxmap, obj%dz, obj%dzmap,    &
                                obj%dtmap, obj%delt, obj%tmax, obj%tmap, &
                                obj%nzmap, obj%nzd, obj%nxmap, obj%nt,   &
                                obj%z0, obj%z0m, obj%t0, tr(:,itr), ierr)
            call timer_stop(modmo2d_timers(MODMO2D_TIMER_TD))
         else
            call timer_start(modmo2d_timers(MODMO2D_TIMER_DT))
            call modmo2d_d2tzmo(offset, obj%dxmap, obj%dz, obj%dzmap,    &
                                obj%dtmap, obj%delt, obj%tmax, obj%tmap, &
                                obj%nzmap, obj%nzd, obj%nxmap, obj%nt,   &
                                obj%z0, obj%z0m, obj%t0, tr(:,itr), ierr)
            call timer_stop(modmo2d_timers(MODMO2D_TIMER_DT))
         end if
      end do

      ! Set largest absolute value for each trace
      call lav_set_hdr(hd, tr, obj%ndpt_out, ntr)

      return
   end subroutine modmo2d_do_gather


!!--------------------------- modmo2d_vzmapr_vti -----------------------------!!
!!--------------------------- modmo2d_vzmapr_vti -----------------------------!!
!!--------------------------- modmo2d_vzmapr_vti -----------------------------!!
   ! NOTE:
   !   1) need to move this stuff into a separate module?
   !   2) need to move memory stuff (nzmax) out of here
   !
   !  generate a v(z) based travel-time map
   !  Dan Whitmore
   !
   !  updated to VTI media, Brian Macy, March 2003
   !  To Do:
   !    1. Epsilon, delta, and Vs0/Vp0 are not smoothed currently.
   !    2. izf needs to be dimensioned to max(nx,nz)
   !
   subroutine modmo2d_vzmapr_vti(nx, nz, np, theta_max, vp0, eps, del, vs0, &
                                 tmax, dt, tmap, zsrc, z0, dz, dx, ismoo,   &
                                 noint, mode, xr, tr, ierr)

      implicit none

      ! Subroutine arguments
      integer,             intent(in)    :: nx, nz, np
      real,                intent(inout) :: theta_max
      real, target,        intent(in)    :: vp0(nz), eps(nz), del(nz), vs0(nz)
      real,                intent(in)    :: tmax, dt
      real,                intent(out)   :: tmap(nz, nx)
      real,                intent(in)    :: zsrc, z0, dz, dx
      integer,             intent(in)    :: ismoo, noint, mode
      real,                intent(out)   :: xr(nz, np), tr(nz, np)
      integer,             intent(inout) :: ierr

      ! Local variables
      integer, parameter :: nzmax=8000
      real,    parameter :: pi=3.141592653589793
      integer            :: icount, iflag, ismoott, ix1, ix2, nray, izsrc
      integer            :: jc, jp    , jray, js, jx, jx1, jx2, jz, jz1, jz2 
      real               :: a, aa, dr, dtheta, dx1, dz1, p, s, scale
      real               :: t, tbig, tdead, x, x1, x2, xmax, xray, z
      real               :: aaa, bbb, slow, theta, phi, cg, sg, slowg
      integer            :: izf(nzmax), icrit(nzmax)
      real               :: slo(nzmax)
      real, target       :: vpsm(nzmax)
      integer, parameter :: nvar=2
      integer            :: i, sz, kz
      real, allocatable  :: var(:), dvar(:), varout(:)
      real, allocatable  :: work(:, :) ! should be work(nvar,3)
      integer            :: lundump
      integer            :: zincr
      real               :: deltaz
      type(modmo2d_deriv_struct) :: o
      character(len=128) :: xrfile, trfile, dummy

      ierr = 0

      if (nzmax < nz) then
         ierr=-1
         return
      end if
      if (nzmax < nx) then   ! izf() needs to be dimensioned 1:max(nx,nz)
         ierr=-1
         return
      end if

      allocate(var(nvar))
      allocate(dvar(nvar))
      allocate(varout(nvar))
      allocate(work(nvar, 3))

      tdead     = 3000. * tmax
      tbig      =  2. * tmax
      xmax      = (nx - 1) * dx
      theta_max = abs(theta_max)
      if (theta_max >= 90.0) theta_max = 89.99999
      if (theta_max == 0.0)  theta_max = 90.

      ! Compute dtheta in radians
      dtheta = (theta_max * pi / 180.) / float(np-1)

      ! Smooth vertical velocity vector
      vpsm(1:nz) = vp0(1:nz)
      if (ismoo > 0) then
         scale = 1.0 / 3.0
         do js = 1, ismoo
            do jz = 2, nz - 1
               slo(jz) = scale * (1./vpsm(jz-1) + 1./vpsm(jz) + 1./vpsm(jz+1))
            end do
            do jz = 2, nz - 1
               vpsm(jz) = 1. / slo(jz)
            end do
         end do
      end if

      ! Fill travel-time map with tdead
      tmap(1:nz,1:nx) = tdead

      ! Initialize xr
      xr(1:nz,1:np) = -1.

      ! Compute rays and put in travel-time map
      icrit(1:nzmax) = 0

      ! Calculate xr, tr using old tried-n-true method
      if (mode == 1) then
      !call pc_warning('modmo2d_vzmapr_vti: downwards raytracing (old)')
      !do jp = 1, np
      do 100 jp = 1, np
         theta = float(jp-1)*dtheta
         p = sin(theta) / modmo2d_vpphase(theta,vpsm(1),vs0(1),eps(1),del(1))
         x = 0.0
         !z = (izsrc - 1) * dz
         z = zsrc
         jz =  (z-z0)/dz + 1
         jx = 1
         t  = 0.0
         !tmap(izsrc,1) = 0.0
         dr = dz
         bbb  = ((z-z0) - (jz-1)*dz)/dz
         aaa = 1. - bbb

         ! Begin ray path loop
         do while (1 == 1)
            if (jz < nz-1) then
               slow = aaa/modmo2d_vpphase(theta,vpsm(jz),vs0(jz),     &
                                        eps(jz),del(jz))            &
                    + bbb/modmo2d_vpphase(theta,vpsm(jz+1),vs0(jz+1), &
                                        eps(jz+1),del(jz+1))
               !slow = 1./modmo2d_vpphase(theta,vpsm(jz),vs0(jz), &
               !                        eps(jz),del(jz))
            end if
            s = p / slow

            ! First "critical" ray:
            ! (accuracy depends on using many rays)
            ! Assume horizontal propagation; then group vel = phase vel (VTI)
            if (s >= 0.999999) then
               if (icrit(jz) == 0 .and. noint == 0) then
                  nray = (xmax + dx - x) / dx
                  !write(lunprint, *) 'nray, jp, jz = ', nray, jp, jz
                  !write(lunprint, *) 'icrit = ', icrit(jz)
                  if (nray > 1) then
                     do jray = 1, nray
                        xray = x + (jray-1) * dx
                        jx   = nint(xray / dx)
                        t    = t + dx / (vpsm(jz)*sqrt(1.+2.*eps(jz)))
                        if (jx <= nx) then
                           xr(jz,jp) = xray
                           tr(jz,jp) = t
                        end if
                     end do
                  end if
                  icrit(jz) = 1
               end if
               go to 100
            end if

            ! Typical rays:
            ! Use group angle and velocity to update location and traveltime
            theta = asin(s)
            if (jz < nz-1) then
               phi = aaa*modmo2d_phigroup(theta, vpsm(jz),   vs0(jz),   &
                                          eps(jz),   del(jz))           &
                   + bbb*modmo2d_phigroup(theta, vpsm(jz+1), vs0(jz+1), &
                                          eps(jz+1), del(jz+1))
            else
               phi = modmo2d_phigroup(theta,vpsm(jz),vs0(jz),eps(jz),del(jz))
            end if
            sg    = abs(sin(phi))
            cg    = sqrt(1.0 - sg**2)
            dx1  = sg * dr
            dz1  = cg * dr
            x    = x + dx1
            z    = z + dz1
            jz   = (z-z0) / dz + 1
            if (jz < nz-1) then
               bbb   = ((z-z0) - (jz-1)*dz)/dz
               aaa   = 1. - bbb
               slowg = aaa/modmo2d_vpgroup(theta,vpsm(jz),vs0(jz),     &
                                           eps(jz),del(jz))            &
                     + bbb/modmo2d_vpgroup(theta,vpsm(jz+1),vs0(jz+1), &
                                           eps(jz+1),del(jz+1))
               !slowg = 1./modmo2d_vpgroup(theta,vpsm(jz),vs0(jz),      &
               !                         eps(jz),del(jz))
            end if
            t    = t +  dr * slowg

            if (jz <= nz) then
               xr(jz,jp) = x
               tr(jz,jp) = t
            else
               go to 100
            end if
         end do      ! End of ray path loop
 100  continue
!100     continue
      !end do         ! End of np loop

         write(xrfile,*) 'xr.bin'
         write(trfile,*) 'tr.bin'

      else if (mode == 2) then

         ! Calculate xr, tr using Runge-Kutta
         !call pc_warning('modmo2d_vzmapr_vti: downwards raytracing (rk)')
         xr = 0
         tr = 0
         do jp = 1, np
            !call pc_warning('modmo2d_vzmapr_vti: (down) jp = ', jp)
            sz    = (zsrc-z0)/dz + 1
            theta = float(jp-1)*dtheta
            p     = sin(theta) / modmo2d_vpphase(theta, vpsm(sz), vs0(sz), &
                                                 eps(sz), del(sz))
            xr(sz,jp) = 0.0
            tr(sz,jp) = 0.0
            do jz = sz, nz-1
               z       = z0 + (jz-1)*dz
               var(1)  = xr(jz,jp)
               var(2)  = tr(jz,jp)
               o%deriv_type = MODMO2D_DERIV_TYPE_VTI1D
               o%vti1d%p0  = p
               o%vti1d%z0  = z0
               o%vti1d%dz  = dz
               o%vti1d%nz  = nz
               o%vti1d%ud  = 1.0 ! downward propagation
               o%vti1d%vp  => vpsm
               o%vti1d%vs  => vs0
               o%vti1d%eps => eps
               o%vti1d%del => del
               call modmo2d_vtiderivs(z, nvar, var, o%vti1d, dvar)
               call modmo2d_rkstep(var, dvar, nvar, z, dz, o, varout, work)
               xr(jz+1,jp) = varout(1)
               tr(jz+1,jp) = varout(2)
            end do
         end do

         write(xrfile,*) 'xr_rk.bin'
         write(trfile,*) 'tr_rk.bin'

      else if (mode == 3) then

         ! Calculate 2nd xr, tr using Runge-Kutta and upwards ray-tracing
         !call pc_warning('modmo2d_vzmapr_vti: upwards raytracing (rk)')
         xr = 0
         tr = 0
         sz    = (zsrc-z0)/dz + 1
         do jp = 1, np
            theta = float(jp-1)*dtheta
            !call pc_warning('modmo2d_vzmapr_vti: (up) jp = ', jp)
            do kz = sz, nz ! loop over starting z locations for upward shooting
               p = sin(theta) / modmo2d_vpphase(theta, vpsm(kz), vs0(kz), &
                                                 eps(kz), del(kz))
               varout(1) = 0.0 !starting xr
               varout(2) = 0.0 !starting tr
               !zincr  = -1
               zincr  = -4
               deltaz = zincr * dz
               jz     = kz
               z      = z0 + (kz-1)*dz
               do while (z > zsrc)
                  deltaz = -min(abs(zincr)*dz, z-zsrc)
                  var(1)  = varout(1)
                  var(2)  = varout(2)
                  o%deriv_type = MODMO2D_DERIV_TYPE_VTI1D
                  o%vti1d%p0   = p
                  o%vti1d%z0   = z0
                  o%vti1d%dz   = dz
                  o%vti1d%nz   = nz
                  o%vti1d%ud   = -1.0 ! upward propagation
                  o%vti1d%vp   => vpsm
                  o%vti1d%vs   => vs0
                  o%vti1d%eps  => eps
                  o%vti1d%del  => del
                  call modmo2d_vtiderivs(z, nvar, var, o%vti1d, dvar)
                  call modmo2d_rkstep(var, dvar, nvar, z, deltaz, o, varout, &
                                      work)
                  jz = jz + zincr
                  z  = z0 + (jz-1)*dz
               end do
               xr(kz,jp) = varout(1)
               tr(kz,jp) = varout(2)
            end do
         end do

         write(xrfile,*) 'xr_rk2.bin'
         write(trfile,*) 'tr_rk2.bin'

      else if (mode == 4) then

         ! Calculate xr, tr using Euler method
         !call pc_warning('modmo2d_vzmapr_vti: downwards raytracing (euler)')
         xr = 0
         tr = 0
         do jp = 1, np
            !call pc_warning('modmo2d_vzmapr_vti: (down) jp = ', jp)
            sz    = (zsrc-z0)/dz + 1
            theta = float(jp-1)*dtheta
            p     = sin(theta) / modmo2d_vpphase(theta, vpsm(sz), vs0(sz), &
                                                 eps(sz), del(sz))
            xr(sz,jp) = 0.0
            tr(sz,jp) = 0.0
            do jz = sz, nz-1
               z       = z0 + (jz-1)*dz
               var(1)  = xr(jz,jp)
               var(2)  = tr(jz,jp)
               o%deriv_type = MODMO2D_DERIV_TYPE_VTI1D
               o%vti1d%p0  = p
               o%vti1d%z0  = z0
               o%vti1d%dz  = dz
               o%vti1d%nz  = nz
               o%vti1d%ud  = 1.0 ! downward propagation
               o%vti1d%vp  => vpsm
               o%vti1d%vs  => vs0
               o%vti1d%eps => eps
               o%vti1d%del => del
               call modmo2d_vtiderivs(z, nvar, var, o%vti1d, dvar)
               varout  = var + dz*dvar
               xr(jz+1,jp) = varout(1)
               tr(jz+1,jp) = varout(2)
            end do
         end do

         write(xrfile,*) 'xr_euler.bin'
         write(trfile,*) 'tr_euler.bin'

      else if (mode == 5) then

         ! Calculate xr, tr using old tried-n-true method, but upwards
         !call pc_warning('modmo2d_vzmapr_vti: upwards raytracing (old)')
         sz    = (zsrc-z0)/dz + 1
         do jp = 1, np
            theta = float(jp-1)*dtheta
            !call pc_warning('modmo2d_vzmapr_vti: (up) jp = ', jp)
            do kz = sz, nz ! loop over starting z locations for upward shooting
               !call pc_warning('modmo2d_vzmapr_vti: (up) jp=', jp, ', kz=', kz)
               p = sin(theta) / modmo2d_vpphase(theta, vpsm(kz), vs0(kz), &
                                                 eps(kz), del(kz))
               x  = 0.0
               t  = 0.0
               jx = 1
               jz = kz
               z  = z0 + (kz-1)*dz
               dr = dz
               ! Begin ray path loop
               do while (z > zsrc)
                  bbb  = ((z-z0) - (jz-1)*dz)/dz
                  aaa = 1. - bbb
                  if (jz < nz-1) then
                     slow = aaa/modmo2d_vpphase(theta, vpsm(jz),   vs0(jz),   &
                                                eps(jz),  del(jz))            &
                          + bbb/modmo2d_vpphase(theta, vpsm(jz+1), vs0(jz+1), &
                                                eps(jz+1), del(jz+1))
                  else
                     slow = 1.0/modmo2d_vpphase(theta, vpsm(jz), vs0(jz), &
                                                eps(jz), del(jz))
                  end if
                  s = p / slow

                  ! First "critical" ray:
                  ! (accuracy depends on using many rays)
                  ! Assume horizontal propagation; 
                  ! then group vel = phase vel (VTI)
                  if (s >= 0.999999) then
                     if (icrit(jz) == 0 .and. noint == 0) then
! Ignore for now
!
!                       nray = (xmax + dx - x) / dx
!                       !write(lunprint, *) 'nray, jp, jz = ', nray, jp, jz
!                       !write(lunprint, *) 'icrit = ', icrit(jz)
!                       if (nray > 1) then
!                          do jray = 1, nray
!                             xray = x + (jray-1) * dx
!                             jx   = nint(xray / dx)
!                             t    = t + dx / (vpsm(jz)*sqrt(1.+2.*eps(jz)))
!                             if (jx <= nx) then
!                                xr(jz,jp) = xray
!                                tr(jz,jp) = t
!                             end if
!                          end do
!                       end if
                        icrit(jz) = 1
                     end if
                     exit
                  end if

                  ! Typical rays: use group angle and velocity to update
                  !               location and traveltime
                  theta = asin(s)
                  if (jz < nz-1) then
                     phi = aaa*modmo2d_phigroup(theta, vpsm(jz),   vs0(jz),   &
                                                eps(jz),   del(jz))           &
                         + bbb*modmo2d_phigroup(theta, vpsm(jz+1), vs0(jz+1), &
                                                eps(jz+1), del(jz+1))
                  else
                     phi = modmo2d_phigroup(theta, vpsm(jz), vs0(jz), eps(jz), &
                                            del(jz))
                  end if
                  sg    = abs(sin(phi))
                  cg    = sqrt(1.0 - sg**2)
                  dx1  = sg * dr
                  dz1  = cg * dr
                  x    = x + dx1
                  z    = z - dz1
                  jz   = (z-z0) / dz + 1
                  if (jz < nz-1) then
                     bbb   = ((z-z0) - (jz-1)*dz)/dz
                     aaa   = 1. - bbb
                     slowg = aaa/modmo2d_vpgroup(theta, vpsm(jz),   vs0(jz),   &
                                                 eps(jz),   del(jz))           &
                           + bbb/modmo2d_vpgroup(theta, vpsm(jz+1), vs0(jz+1), &
                                                 eps(jz+1), del(jz+1))
                  else
                     slowg = 1./modmo2d_vpgroup(theta, vpsm(jz), vs0(jz), &
                                                eps(jz), del(jz))
                  end if
                  t    = t + dr * slowg
               end do      ! End of ray path loop
               xr(kz,jp) = x
               tr(kz,jp) = t
            end do
         end do            ! End of np loop

         write(xrfile,*) 'xr_up.bin'
         write(trfile,*) 'tr_up.bin'

      else
         ierr = -1
         call pc_error('modmo2d_vzmapr_vti: unknown rt mode = ', mode)
         return
      end if


      ! Dump xr/tr to file for inspection
      lundump = cio_fopen(xrfile, 'w')
      if (lundump == 0) &
         call pc_error('modmo2d_vzmapr_vti: error opening ', trim(xrfile))
      i = cio_fwrite(xr, sizeof(xr(1,1)), nz*np, lundump)
      if (i /= nz*np) then
         write(dummy,*) 'modmo2d_vzmapr_vti: only wrote ', i, ' of ', nz*np, &
                       ' items in ', trim(xrfile)
         call pc_error(dummy)
      endif
      ierr = cio_fclose(lundump)
      if (ierr /= 0) &
         call pc_error('modmo2d_vzmapr_vti: error closing ', trim(xrfile))

      lundump = cio_fopen(trfile, 'w')
      if (lundump == 0) &
         call pc_error('modmo2d_vzmapr_vti: error opening ', trim(trfile))
      i = cio_fwrite(tr, sizeof(tr(1,1)), nz*np, lundump)
      if (i /= nz*np) then
         write(dummy,*) 'modmo2d_vzmapr_vti: only wrote ', i, ' of ', nz*np, &
                       ' items in ', trim(trfile)
         call pc_error(dummy)
      endif
      ierr = cio_fclose(lundump)
      if (ierr /= 0) &
         call pc_error('modmo2d_vzmapr_vti: error closing ', trim(trfile))


      ! Scan travel-time map and fill in missing times
      do jz = 1, nz
         do jp = 1, np -1
            x1 = xr(jz,jp)
            x2 = xr(jz,jp+1)
            if (x1 /= x2 .and. x1 >= 0.0 .and. x2 >= 0.0) then
               ix1 = x1 / dx +1
               ix2 = x2 / dx + 1
               do jx = ix1, ix2
                  if (jx <= nx) then
                     aa = ((jx-1) * dx - x1) / (x2-x1)
                     tmap(jz,jx) = (1.0-aa)*tr(jz,jp) + aa*tr(jz,jp+1)
                  end if
               end do
            end if
         end do
      end do

      if (noint /= 0) go to 1000

      ! Horizontal scan
      iflag = 0
      do jz = 1, nz
         if (tmap(jz,nx) /= tdead) iflag = iflag + 1
         icount = 0
         do jx = 1, nx
            if (tmap(jz,jx) /= tdead) then
               icount = icount + 1
               izf(icount) = jx
            end if
         end do
         if (icount > 1) then
            do jc = 1, icount-1
               jx1 = izf(jc)
               jx2 = izf(jc+1)
               if (jx1 /= jx2) then
                  do jx = jx1, jx2
                     a = float(jx-jx1) / float(jx2-jx1)
                     tmap(jz,jx) = tmap(jz,jx1) + a*(tmap(jz,jx2)-tmap(jz,jx1))
                  end do
               end if

               ! If any rays go out the side fill last row to the corner
               if (jx2 < nx .and. iflag > 0 .and. jz == nz) then
                  do jx = jx2 + 1, nx
                     a = float(jx-jx1) / float(jx2-jx1)
                     tmap(jz,jx) = tmap(jz,jx1) + a*(tmap(jz,jx2)-tmap(jz,jx1))
                  end do
               end if
            end do
         end if
      end do

      ! Vertical scan
      izsrc = (zsrc-z0)/dz + 1
      do jx = 1, nx
         icount = 0
         do jz = izsrc, nz
            if (tmap(jz,jx) /= tdead) then
               icount = icount + 1
               izf(icount) = jz
            end if
         end do
         if (icount > 1) then
            do jc = 1, icount - 1
               jz1 = izf(jc)
               jz2 = izf(jc+1)
               if (jz1 /= jz2) then
                  do jz = jz1, jz2
                     a = float(jz-jz1) / float(jz2-jz1)
                     tmap(jz,jx) = tmap(jz1,jx) + a*(tmap(jz2,jx)-tmap(jz1,jx))
                  end do
               end if
            end do
         end if
      end do

      ! Vertically smooth travel-times
      ismoott = 0
      !scale = 1.0 / 9.0
      scale = 1.0 / 3.0
      if (ismoott > 0) then
         do js = 1, ismoott
            do jx = 2, nx-1
               do jz = 2, nz - 1
                  slo(jz) = scale*(tmap(jz-1,jx)+tmap(jz,jx)+tmap(jz+1,jx))
                  !       + tmap(jz-1,jx-1)+tmap(jz,jx-1)+tmap(jz+1,jx-1) &
                  !       + tmap(jz-1,jx+1)+tmap(jz,jx+1)+tmap(jz+1,jx+1) )
               end do
               do jz = 2, nz - 1
                  tmap(jz,jx) = slo(jz)
               end do
            end do
         end do
      end if

 1000 continue

      do jx = 1, nx
         do jz = 1, nz
            if (tmap(jz,jx) >= tbig) tmap(jz,jx) = tbig
         end do
      end do

      deallocate(var)
      deallocate(dvar)
      deallocate(varout)
      deallocate(work)

      return
   end subroutine modmo2d_vzmapr_vti


!!--------------------------- modmo2d_vzmapr_vti2d ---------------------------!!
!!--------------------------- modmo2d_vzmapr_vti2d ---------------------------!!
!!--------------------------- modmo2d_vzmapr_vti2d ---------------------------!!
   ! NOTE:
   !   1) need to move this stuff into a separate module?
   !   2) need to move memory stuff (nzmax) out of here
   !
   !  generate a v(z) based travel-time map
   !  Dan Whitmore
   !
   !  updated to VTI media, Brian Macy, March 2003
   !  To Do:
   !    1. Epsilon, delta, and Vs0/Vp0 are not smoothed currently.
   !    2. izf needs to be dimensioned to max(nx,nz)
   !    3. need to properly choose multi-arrivals
   !    4. need to take care of end of tmap array, when tmap(nz,jp) is
   !       not explicitly ray-traced (leaves tdead for last few iz's)
   !
   subroutine modmo2d_vzmapr_vti2d(nx, nz, np, theta_min, theta_max,   &
                                   vp0, vs0, eps, del, tmax, dt, tmap, &
                                   zsrc, z0, dz, xloc, x0, dx,         &
                                   ismoo, noint, mode, xr, tr, ierr)

      implicit none

      ! Subroutine arguments
      integer,             intent(in)    :: nx, nz, np
      real,                intent(in)    :: theta_min, theta_max
      real, target,        intent(in)    :: vp0(nz,2*nx+1), vs0(nz,2*nx+1)
      real, target,        intent(in)    :: eps(nz,2*nx+1), del(nz,2*nx+1)
      real,                intent(in)    :: tmax, dt
      real,                intent(out)   :: tmap(nz, nx)
      real,                intent(in)    :: zsrc, z0, dz
      real,                intent(in)    :: xloc  ! x location to shoot from
      real,                intent(in)    :: x0, dx
      integer,             intent(in)    :: ismoo, noint, mode
      real,                intent(out)   :: xr(nz, np), tr(nz, np)
      integer,             intent(out)   :: ierr

      ! Local variables
      integer, parameter :: nxmax=8000, nzmax=8000
      real,    parameter :: pi=3.141592653589793
      integer            :: icount, iflag, ismoott, ix1, ix2      , izsrc 
      integer            :: jc, jp          , js, jx, jx1, jx2, jz, jz1, jz2  
      integer            :: nx_model
      real               :: a, aa, dr, dtheta, dx1, dz1, p, s, scale
      real               :: t, tbig, tdead, x, x1, x2, xmax      , z 
      real               :: aaa, bbb, slow, theta, phi, cg, sg, slowg
      real               :: thetamin
      real               :: xold, zold, told
      integer            :: izf(nzmax), icrit(nzmax)
      real               :: slo(nzmax), vs_slo(nzmax)
      real, allocatable, target, save :: vpsm(:,:), vssm(:,:)
      integer, parameter :: nvar=3
      integer            :: i, sz, kx, kz, izinc, kz1, kz2
      real, allocatable, save  :: var(:), dvar(:), varout(:)
      real, allocatable, save  :: work(:, :) ! should be work(nvar,3)
      logical            :: dump
      logical, save      :: first = .true.
      integer            :: lundump
      real               :: deltat
      type(modmo2d_deriv_struct) :: o
      character(len=128) :: xrfile, trfile, dummy
      real               :: vsvp
      real               :: xrp, xrn, trp, trn

      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: inside routine vzmapr_vti2d'

      ierr = 0
      dump = .false.
      nx_model = 2*nx + 1

      if (nzmax < nz) then
         ierr=-1
         return
      end if
      if (nzmax < nx) then   ! izf() needs to be dimensioned 1:max(nx,nz)
         ierr=-1
         return
      end if
      if (nzmax < nx_model) then
         ierr=-1
         return
      end if

      if (first) then
         first = .false.
         !write(lunprint,*) 'modmo2d_vzmapr_vti2d: allocating memory'
         allocate(var(nvar))
         allocate(dvar(nvar))
         allocate(varout(nvar))
         allocate(work(nvar, 3))
         allocate(vpsm(nz, nx_model))
         allocate(vssm(nz, nx_model))

         if (dump) then
            lundump = cio_fopen('vp.bin', 'w')
            if (lundump == 0) call pc_error('modmo2d_vzmapr_vti2d: error &
                                            &opening ', trim('vp.bin'))
            i = cio_fwrite(vp0, sizeof(vp0(1,1)), nz*nx_model, lundump)
            if (i /= nz*nx_model) then
               write(dummy,*) 'modmo2d_vzmapr_vti2d: only wrote ', i, ' of ', &
                              nz*nx_model, ' items in ', trim('vp.bin')
               call pc_error(dummy)
            endif
            ierr = cio_fclose(lundump)
            if (ierr /= 0) call pc_error('modmo2d_vzmapr_vti2d: error &
                                         &closing ', trim('vp.bin'))
         end if
      end if
      if (nz > size(vpsm, 1) .or. nx_model > size(vpsm, 2)) then
         deallocate(vpsm)
         allocate(vpsm(nz, nx_model))
      end if
      if (nz > size(vssm, 1) .or. nx_model > size(vssm, 2)) then
         deallocate(vssm)
         allocate(vssm(nz, nx_model))
      end if

      tdead     = 3000. * tmax
      tbig      =  2. * tmax
      xmax      = (nx_model - 1) * dx

      ! Compute dtheta in radians
      dtheta   = ((theta_max-theta_min) * pi / 180.) / float(np-1)
      thetamin = theta_min * pi / 180.

      ! Smooth vertical velocity vector
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: smoothing velocity'
!     vpsm(1:nz,1:nx_model) = vp0(1:nz,1:nx_model)
      vpsm = vp0
      vssm(1:nz,1:nx_model) = vs0(1:nz,1:nx_model)
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: vpsm(nz,1) = ', vpsm(nz,1)
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: ismoo = ', ismoo
      if (ismoo > 0) then
         scale = 1.0 / 3.0
         do jx = 1, nx_model
            do js = 1, ismoo
               do jz = 2, nz - 1
                  slo(jz) = scale*(1./vpsm(jz-1,jx) + 1./vpsm(jz,jx) + &
                                   1./vpsm(jz+1,jx))
                  vs_slo(jz) = scale*(1./vssm(jz-1,jx) + 1./vssm(jz,jx) + &
                                      1./vssm(jz+1,jx))
               end do
               do jz = 2, nz - 1
                  vpsm(jz,jx) = 1. / slo(jz)
                  vssm(jz,jx) = 1. / vs_slo(jz)
               end do
            end do
         end do
      end if
!     if (ismoo > 0) then
!        scale = 1.0 / 3.0
!        do jz = 1, nz
!           do js = 1, ismoo
!              do jx = 2, nx_model-1
!                 slo(jx) = scale*(1./vpsm(jz,jx-1) + 1./vpsm(jz,jx) + &
!                                  1./vpsm(jz,jx+1))
!                 vs_slo(jx) = scale*(1./vssm(jz,jx-1) + 1./vssm(jz,jx) + &
!                                     1./vssm(jz,jx+1))
!              end do
!              do jx = 2, nx_model-1
!                 vpsm(jz,jx) = 1. / slo(jx)
!                 vssm(jz,jx) = 1. / vs_slo(jx)
!              end do
!           end do
!        end do
!     end if
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: vpsm(nz,1) = ', vpsm(nz,1)
      vsvp = vssm(1,1)/vpsm(1,1)
      do jx=1,nx_model
         do jz=1,nz
             vsvp = max(vsvp, vssm(jz,jx)/vpsm(jz,jx))
         end do
      end do
      write(lunprint,*) 'max vsvp for vssm/vpsm = ', vsvp
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: tmax = ', tmax

      ! Fill travel-time map with tdead
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: initializing tmap'
      tmap(1:nz,1:nx) = tdead

      ! Initialize xr
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: initializing xr'
      xr(1:nz,1:np) = -1.

      ! Compute rays and put in travel-time map
      !write(lunprint,*) 'modmo2d_vzmapr_vti2d: initializing icrit'
      icrit(1:nzmax) = 0

      if (mode == 1) then

         ! Calculate xr, tr using Cerveny's equations, Runge-Kutta and
         ! upwards ray-tracing
         !write(lunprint,*) 'modmo2d_vzmapr_vti2d: upwards raytracing (rk)'
         xr = 0
         tr = 0
         sz    = (zsrc-z0)/dz + 1
         do jp = 1, np
            theta = thetamin + float(jp-1)*dtheta
            !write(lunprint,*) 'modmo2d_vzmapr_vti2d: (up) jp = ', jp

            ! loop over starting z locations for upward shooting
            izinc = 4
            do kz = sz, nz, izinc
               !
               ! Do positive p's, find xrp, trp
               !
               kx = (xloc-x0)/dx + 1
               z  = z0 + (kz-1)*dz
               p  = sin(theta)/modmo2d_vpphase(theta,                    &
                                               vpsm(kz,kx), vssm(kz,kx), &
                                               eps(kz,kx), del(kz,kx))
               t  = 0.0
               o%deriv_type = MODMO2D_DERIV_TYPE_VTI2D
               o%vti2d%p0   = p
               o%vti2d%x0   = x0
               o%vti2d%dx   = dx
               o%vti2d%nx   = nx_model
               o%vti2d%z0   = z0
               o%vti2d%dz   = dz
               o%vti2d%nz   = nz
               o%vti2d%ud   = -1.0 ! upward propagation
               o%vti2d%vp   => vpsm
               o%vti2d%vs   => vssm
               o%vti2d%eps  => eps
               o%vti2d%del  => del
               varout(1) = xloc !starting x
               varout(2) = z    !starting z
               varout(3) = p    !starting p
               do while (z > zsrc)
                  deltat  = dt
                  deltat  = 4*dt
                  var(1)  = varout(1) ! x
                  var(2)  = varout(2) ! z
                  var(3)  = varout(3) ! p
                  call modmo2d_vtiderivs2d(t, nvar, var, o%vti2d, dvar)
                  call modmo2d_rkstep(var, dvar, nvar, t, deltat, o, varout, &
                                      work)
                  z = varout(2)
                  if (varout(2) == var(2)) exit
                  t = t + deltat
               end do
               ! Linearly interpolate to get x,t at zsrc
               x    = varout(1)
               xold = var(1)
               zold = var(2)
               told = t - deltat
               if (z == zold) then
                  xrp = xloc + 10.*(nx_model - 1)*dx
                  trp = tdead
               else
                  bbb  = (zsrc-z)/(zold-z)
                  aaa = 1. - bbb
                  xrp = aaa*x + bbb*xold
                  trp = aaa*t + bbb*told
               end if


               !
               ! Do negative p's, find xrn, trn
               !
               kx = (xloc-x0)/dx + 1
               z  = z0 + (kz-1)*dz
               p  = sin(-theta)/modmo2d_vpphase(-theta,                   &
                                                vpsm(kz,kx), vssm(kz,kx), &
                                                eps(kz,kx), del(kz,kx))
               t  = 0.0
               o%deriv_type = MODMO2D_DERIV_TYPE_VTI2D
               o%vti2d%p0   = p
               o%vti2d%x0   = x0
               o%vti2d%dx   = dx
               o%vti2d%nx   = nx_model
               o%vti2d%z0   = z0
               o%vti2d%dz   = dz
               o%vti2d%nz   = nz
               o%vti2d%ud   = -1.0 ! upward propagation
               o%vti2d%vp   => vpsm
               o%vti2d%vs   => vssm
               o%vti2d%eps  => eps
               o%vti2d%del  => del
               varout(1) = xloc !starting x
               varout(2) = z    !starting z
               varout(3) = p    !starting p
               do while (z > zsrc)
                  deltat  = dt
                  deltat  = 4*dt
                  var(1)  = varout(1) ! x
                  var(2)  = varout(2) ! z
                  var(3)  = varout(3) ! p
                  call modmo2d_vtiderivs2d(t, nvar, var, o%vti2d, dvar)
                  call modmo2d_rkstep(var, dvar, nvar, t, deltat, o, varout, &
                                      work)
                  z = varout(2)
                  if (varout(2) == var(2)) exit
                  t = t + deltat
               end do
               ! Linearly interpolate to get x,t at zsrc
               x    = varout(1)
               xold = var(1)
               zold = var(2)
               told = t - deltat
               if (z == zold) then
                  xrn = xloc - 10.*(nx_model - 1)*dx
                  trn = tdead
               else
                  bbb  = (zsrc-z)/(zold-z)
                  aaa = 1. - bbb
                  xrn = aaa*x + bbb*xold
                  trn = aaa*t + bbb*told
               end if


               !
               ! NEED: if xrp or xrn is "null", then xr = "null"
               ! NEED: if trp or trn is "null", then tr = "null"
               !
               ! 0.5 factor needed since xr,tr will get doubled later on
               !
               if (trp == tdead .or. trn == tdead) then
                  xr(kz,jp) = 10.0*(nx_model - 1)*dx
                  tr(kz,jp) = tdead
               else
                  xr(kz,jp) = 0.5*(xrp-xrn)
                  tr(kz,jp) = 0.5*(trp+trn)
               end if
            end do

            ! Linearly-interpolate missing kz's
            do kz = sz, nz ! loop over starting z locations for upward shooting
               kz1 = max(sz, sz + izinc*((kz-sz)/izinc))
               kz2 = min(nz, kz1 + izinc)
               if (kz == kz1) cycle
               if (tr(kz1,jp) == tdead .or. tr(kz2,jp) == tdead) then
                  xr(kz,jp) = 10.0*(nx_model - 1)*dx
                  tr(kz,jp) = tdead
               else
                  bbb  = real(kz-kz1)/real(izinc)
                  aaa = 1. - bbb
                  xr(kz,jp) = aaa*xr(kz1,jp) + bbb*xr(kz2,jp)
                  tr(kz,jp) = aaa*tr(kz1,jp) + bbb*tr(kz2,jp)
               end if
            end do
         end do

         write(xrfile,*) 'xr_cerv_rk.bin'
         write(trfile,*) 'tr_cerv_rk.bin'

      else if (mode == 2) then

         ierr = -1
         call pc_error('modmo2d_vzmapr_vti2d: rt mode = ', mode, ' unsupported')
         return

         ! Calculate xr, tr using old tried-n-true method, but upwards
         !call pc_warning('modmo2d_vzmapr_vti2d: upwards raytracing (old)')
         sz    = (zsrc-z0)/dz + 1
         do jp = 1, np
            theta = thetamin + float(jp-1)*dtheta
            !call pc_warning('modmo2d_vzmapr_vti2d: (up) jp = ', jp)
            do kz = sz, nz ! loop over starting z locations for upward shooting
               !call pc_warning('modmo2d_vzmapr_vti2d: (up) jp=',jp,',kz=',kz)
               kx = (xloc-x0)/dx + 1
               p  = sin(theta)/modmo2d_vpphase(theta, vpsm(kz,kx), vs0(kz,kx), &
                                               eps(kz,kx), del(kz,kx))
               x  = 0.0
               t  = 0.0
               jx = kx
               jz = kz
               z  = z0 + (kz-1)*dz
               dr = dz
               ! Begin ray path loop
               do while (z > zsrc)
                  bbb  = ((z-z0) - (jz-1)*dz)/dz
                  aaa = 1. - bbb
                  if (jz < nz-1) then
                     slow = aaa/modmo2d_vpphase(theta, vpsm(jz,jx),      &
                                                vs0(jz,jx),              &
                                                eps(jz,jx),  del(jz,jx)) &
                          + bbb/modmo2d_vpphase(theta, vpsm(jz+1,jx),    &
                                                vs0(jz+1,jx),            &
                                                eps(jz+1,jx), del(jz+1,jx))
                  else
                     slow = 1.0/modmo2d_vpphase(theta, vpsm(jz,jx), vs0(jz,jx),&
                                                eps(jz,jx), del(jz,jx))
                  end if
                  s = p / slow

                  ! First "critical" ray:
                  ! (accuracy depends on using many rays)
                  ! Assume horizontal propagation;
                  ! then group vel = phase vel (VTI)
                  if (s >= 0.999999) then
                     if (icrit(jz) == 0 .and. noint == 0) then
! Ignore for now
!
!                       nray = (xmax + dx - x) / dx
!                       !write(lunprint, *) 'nray, jp, jz = ', nray, jp, jz
!                       !write(lunprint, *) 'icrit = ', icrit(jz)
!                       if (nray > 1) then
!                          do jray = 1, nray
!                             xray = x + (jray-1) * dx
!                             jx   = nint(xray / dx)
!                             t    = t + dx / (vpsm(jz)*sqrt(1.+2.*eps(jz)))
!                             if (jx <= nx) then
!                                xr(jz,jp) = xray
!                                tr(jz,jp) = t
!                             end if
!                          end do
!                       end if
                        icrit(jz) = 1
                     end if
                     exit
                  end if

                  ! Typical rays: use group angle and velocity to update
                  !               location and traveltime
                  theta = asin(s)
                  if (jz < nz-1) then
                     phi = aaa*modmo2d_phigroup(theta, vpsm(jz,jx),       &
                                                vs0(jz,jx),               &
                                                eps(jz,jx),   del(jz,jx)) &
                         + bbb*modmo2d_phigroup(theta, vpsm(jz+1,jx),     &
                                                vs0(jz+1,jx),             &
                                                eps(jz+1,jx), del(jz+1,jx))
                  else
                     phi = modmo2d_phigroup(theta, vpsm(jz,jx), vs0(jz,jx), &
                                            eps(jz,jx), del(jz,jx))
                  end if
                  sg    = abs(sin(phi))
                  cg    = sqrt(1.0 - sg**2)
                  dx1  = sg * dr
                  dz1  = cg * dr
                  x    = x + dx1
                  z    = z - dz1
                  jz   = (z-z0) / dz + 1
                  if (jz < nz-1) then
                     bbb   = ((z-z0) - (jz-1)*dz)/dz
                     aaa   = 1. - bbb
                     slowg = aaa/modmo2d_vpgroup(theta, vpsm(jz,jx),       &
                                                 vs0(jz,jx),               &
                                                 eps(jz,jx),   del(jz,jx)) &
                           + bbb/modmo2d_vpgroup(theta, vpsm(jz+1,jx),     &
                                                 vs0(jz+1,jx),             &
                                                 eps(jz+1,jx), del(jz+1,jx))
                  else
                     slowg = 1./modmo2d_vpgroup(theta, vpsm(jz,jx), vs0(jz,jx),&
                                                eps(jz,jx), del(jz,jx))
                  end if
                  t    = t + dr * slowg
               end do      ! End of ray path loop
               xr(kz,jp) = x
               tr(kz,jp) = t
            end do
         end do            ! End of np loop

         write(xrfile,*) 'xr_old.bin'
         write(trfile,*) 'tr_old.bin'

      else
         ierr = -1
         call pc_error('modmo2d_vzmapr_vti2d: unknown rt mode = ', mode)
         return
      end if


      ! Dump xr/tr to file for inspection
      if (dump) then
         lundump = cio_fopen(xrfile, 'w')
         if (lundump == 0) &
            call pc_error('modmo2d_vzmapr_vti2d: error opening ', trim(xrfile))
         i = cio_fwrite(xr, sizeof(xr(1,1)), nz*np, lundump)
         if (i /= nz*np) then
            write(dummy,*) 'modmo2d_vzmapr_vti2d: only wrote ', i, ' of ', &
                           nz*np, ' items in ', trim(xrfile)
            call pc_error(dummy)
         endif
         ierr = cio_fclose(lundump)
         if (ierr /= 0) &
            call pc_error('modmo2d_vzmapr_vti2d: error closing ', trim(xrfile))

         lundump = cio_fopen(trfile, 'w')
         if (lundump == 0) &
            call pc_error('modmo2d_vzmapr_vti2d: error opening ', trim(trfile))
         i = cio_fwrite(tr, sizeof(tr(1,1)), nz*np, lundump)
         if (i /= nz*np) then
            write(dummy,*) 'modmo2d_vzmapr_vti2d: only wrote ', i, ' of ', &
                          nz*np, ' items in ', trim(trfile)
            call pc_error(dummy)
         endif
         ierr = cio_fclose(lundump)
         if (ierr /= 0) &
            call pc_error('modmo2d_vzmapr_vti2d: error closing ', trim(trfile))
      end if


      ! Scan travel-time map and fill in missing times
      do jz = 1, nz
         do jp = 1, np -1
            x1 = xr(jz,jp)
            x2 = xr(jz,jp+1)
            if (x1 /= x2 .and. x1 >= 0.0 .and. x2 >= 0.0) then
               ix1 = x1 / dx +1
               ix2 = x2 / dx + 1
               do jx = ix1, ix2
                  if (jx <= nx) then
                     aa = ((jx-1) * dx - x1) / (x2-x1)
                     tmap(jz,jx) = (1.0-aa)*tr(jz,jp) + aa*tr(jz,jp+1)
                  end if
               end do
            end if
         end do
      end do

      if (noint /= 0) go to 1000

      ! Horizontal scan
      iflag = 0
      do jz = 1, nz
         if (tmap(jz,nx) /= tdead) iflag = iflag + 1
         icount = 0
         do jx = 1, nx
            if (tmap(jz,jx) /= tdead) then
               icount = icount + 1
               izf(icount) = jx
            end if
         end do
         if (icount > 1) then
            do jc = 1, icount-1
               jx1 = izf(jc)
               jx2 = izf(jc+1)
               if (jx1 /= jx2) then
                  do jx = jx1, jx2
                     a = float(jx-jx1) / float(jx2-jx1)
                     tmap(jz,jx) = tmap(jz,jx1) + a*(tmap(jz,jx2)-tmap(jz,jx1))
                  end do
               end if

               ! If any rays go out the side fill last row to the corner
               if (jx2 < nx .and. iflag > 0 .and. jz == nz) then
                  do jx = jx2 + 1, nx
                     a = float(jx-jx1) / float(jx2-jx1)
                     tmap(jz,jx) = tmap(jz,jx1) + a*(tmap(jz,jx2)-tmap(jz,jx1))
                  end do
               end if
            end do
         end if
      end do

      ! Vertical scan
      izsrc = (zsrc-z0)/dz + 1
      do jx = 1, nx
         icount = 0
         do jz = izsrc, nz
            if (tmap(jz,jx) /= tdead) then
               icount = icount + 1
               izf(icount) = jz
            end if
         end do
         if (icount > 1) then
            do jc = 1, icount - 1
               jz1 = izf(jc)
               jz2 = izf(jc+1)
               if (jz1 /= jz2) then
                  do jz = jz1, jz2
                     a = float(jz-jz1) / float(jz2-jz1)
                     tmap(jz,jx) = tmap(jz1,jx) + a*(tmap(jz2,jx)-tmap(jz1,jx))
                  end do
               end if
            end do
         end if
      end do

      ! Vertically smooth travel-times
      ismoott = 0
      !scale = 1.0 / 9.0
      scale = 1.0 / 3.0
      if (ismoott > 0) then
         do js = 1, ismoott
            do jx = 2, nx-1
               do jz = 2, nz - 1
                  slo(jz) = scale*(tmap(jz-1,jx)+tmap(jz,jx)+tmap(jz+1,jx))
                  !       + tmap(jz-1,jx-1)+tmap(jz,jx-1)+tmap(jz+1,jx-1) &
                  !       + tmap(jz-1,jx+1)+tmap(jz,jx+1)+tmap(jz+1,jx+1) )
               end do
               do jz = 2, nz - 1
                  tmap(jz,jx) = slo(jz)
               end do
            end do
         end do
      end if

 1000 continue

      do jx = 1, nx
         do jz = 1, nz
            if (tmap(jz,jx) >= tbig) tmap(jz,jx) = tbig
         end do
      end do

      !deallocate(var)
      !deallocate(dvar)
      !deallocate(varout)
      !deallocate(work)
      !deallocate(vpsm)
      !deallocate(vssm)

      return
   end subroutine modmo2d_vzmapr_vti2d


!!--------------------------- modmo2d_vzmapr_iso2d ---------------------------!!
!!--------------------------- modmo2d_vzmapr_iso2d ---------------------------!!
!!--------------------------- modmo2d_vzmapr_iso2d ---------------------------!!
   ! NOTE:
   !   1) need to move this stuff into a separate module?
   !   2) need to move memory stuff (nzmax) out of here
   !
   !  generate a v(z) based travel-time map
   !  Dan Whitmore
   !
   !  updated to VTI media, Brian Macy, March 2003
   !  To Do:
   !    2. izf needs to be dimensioned to max(nx,nz)
   !    3. need to properly choose multi-arrivals
   !    4. need to take care of end of tmap array, when tmap(nz,jp) is
   !       not explicitly ray-traced (leaves tdead for last few iz's)
   !
   subroutine modmo2d_vzmapr_iso2d(nx, nz, np, theta_min, theta_max,   &
                                   vp0,                tmax, dt, tmap, &
                                   zsrc, z0, dz, xloc, x0, dx,         &
                                   ismoo, noint, mode, xr, tr, ierr)

      implicit none

      ! Subroutine arguments
      integer,             intent(in)    :: nx, nz, np
      real,                intent(in)    :: theta_min, theta_max
      real, target,        intent(in)    :: vp0(nz,2*nx+1)
      real,                intent(in)    :: tmax, dt
      real,                intent(out)   :: tmap(nz, nx)
      real,                intent(in)    :: zsrc, z0, dz
      real,                intent(in)    :: xloc  ! x location to shoot from
      real,                intent(in)    :: x0, dx
      integer,             intent(in)    :: ismoo, noint, mode
      real,                intent(out)   :: xr(nz, np), tr(nz, np)
      integer,             intent(out)   :: ierr

      ! Local variables
      integer, parameter :: nxmax=8000, nzmax=8000
      real,    parameter :: pi=3.141592653589793
      integer            :: icount, iflag, ismoott, ix1, ix2      , izsrc 
      integer            :: jc, jp          , js, jx, jx1, jx2, jz, jz1, jz2  
      integer            :: nx_model
      real               :: a, aa, dr, dtheta, dx1, dz1, p, s, scale
      real               :: t, tbig, tdead, x, x1, x2, xmax      , z 
      real               :: aaa, bbb, slow, theta, cg, sg, slowg
      real               :: thetamin
      real               :: xold, zold, told
      integer            :: izf(nzmax), icrit(nzmax)
      real               :: slo(nzmax)   
      real, allocatable, target, save :: vpsm(:,:)            
      integer, parameter :: nvar=3
      integer            :: i, sz, kx, kz, izinc, kz1, kz2
      real, allocatable, save  :: var(:), dvar(:), varout(:)
      real, allocatable, save  :: work(:, :) ! should be work(nvar,3)
      logical            :: dump
      logical, save      :: first = .true.
      integer            :: lundump
      real               :: deltat
      type(modmo2d_deriv_struct) :: o
      character(len=128) :: xrfile, trfile, dummy

      real               :: xrp, xrn, trp, trn

      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: inside routine vzmapr_iso2d'

      ierr = 0
      dump = .false.
      nx_model = 2*nx + 1

      if (nzmax < nz) then
         ierr=-1
         return
      end if
      if (nzmax < nx) then   ! izf() needs to be dimensioned 1:max(nx,nz)
         ierr=-1
         return
      end if
      if (nzmax < nx_model) then
         ierr=-1
         return
      end if

      if (first) then
         first = .false.
         !write(lunprint,*) 'modmo2d_vzmapr_iso2d: allocating memory'
         allocate(var(nvar))
         allocate(dvar(nvar))
         allocate(varout(nvar))
         allocate(work(nvar, 3))
         allocate(vpsm(nz, nx_model))

         if (dump) then
            lundump = cio_fopen('vp.bin', 'w')
            if (lundump == 0) call pc_error('modmo2d_vzmapr_iso2d: error &
                                            &opening ', trim('vp.bin'))
            i = cio_fwrite(vp0, sizeof(vp0(1,1)), nz*nx_model, lundump)
            if (i /= nz*nx_model) then
               write(dummy,*) 'modmo2d_vzmapr_iso2d: only wrote ', i, ' of ', &
                              nz*nx_model, ' items in ', trim('vp.bin')
               call pc_error(dummy)
            endif
            ierr = cio_fclose(lundump)
            if (ierr /= 0) call pc_error('modmo2d_vzmapr_iso2d: error &
                                         &closing ', trim('vp.bin'))
         end if
      end if
      if (nz > size(vpsm, 1) .or. nx_model > size(vpsm, 2)) then
         deallocate(vpsm)
         allocate(vpsm(nz, nx_model))
      end if

      tdead     = 3000. * tmax
      tbig      =  2. * tmax
      xmax      = (nx_model - 1) * dx

      ! Compute dtheta in radians
      dtheta   = ((theta_max-theta_min) * pi / 180.) / float(np-1)
      thetamin = theta_min * pi / 180.

      ! Smooth vertical velocity vector
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: smoothing velocity'
!     vpsm(1:nz,1:nx_model) = vp0(1:nz,1:nx_model)
      vpsm = vp0
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: vpsm(nz,1) = ', vpsm(nz,1)
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: ismoo = ', ismoo
      if (ismoo > 0) then
         scale = 1.0 / 3.0
         do jx = 1, nx_model
            do js = 1, ismoo
               do jz = 2, nz - 1
                  slo(jz) = scale*(1./vpsm(jz-1,jx) + 1./vpsm(jz,jx) + &
                                   1./vpsm(jz+1,jx))
               end do
               do jz = 2, nz - 1
                  vpsm(jz,jx) = 1. / slo(jz)
               end do
            end do
         end do
      end if
!     if (ismoo > 0) then
!        scale = 1.0 / 3.0
!        do jz = 1, nz
!           do js = 1, ismoo
!              do jx = 2, nx_model-1
!                 slo(jx) = scale*(1./vpsm(jz,jx-1) + 1./vpsm(jz,jx) + &
!                                  1./vpsm(jz,jx+1))
!              end do
!              do jx = 2, nx_model-1
!                 vpsm(jz,jx) = 1. / slo(jx)
!              end do
!           end do
!        end do
!     end if
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: vpsm(nz,1) = ', vpsm(nz,1)
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: tmax = ', tmax

      ! Fill travel-time map with tdead
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: initializing tmap'
      tmap(1:nz,1:nx) = tdead

      ! Initialize xr
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: initializing xr'
      xr(1:nz,1:np) = -1.

      ! Compute rays and put in travel-time map
      !write(lunprint,*) 'modmo2d_vzmapr_iso2d: initializing icrit'
      icrit(1:nzmax) = 0

      if (mode == 1) then

         ! Calculate xr, tr using Cerveny's equations, Runge-Kutta and
         ! upwards ray-tracing
         !write(lunprint,*) 'modmo2d_vzmapr_iso2d: upwards raytracing (rk)'
         xr = 0
         tr = 0
         sz    = (zsrc-z0)/dz + 1
         do jp = 1, np
            theta = thetamin + float(jp-1)*dtheta
            !write(lunprint,*) 'modmo2d_vzmapr_iso2d: (up) jp = ', jp

            ! loop over starting z locations for upward shooting
            izinc = 4
            do kz = sz, nz, izinc
               !
               ! Do positive p's, find xrp, trp
               !
               kx = (xloc-x0)/dx + 1
               z  = z0 + (kz-1)*dz
               p  = sin(theta)/vpsm(kz,kx)
               t  = 0.0
               o%deriv_type = MODMO2D_DERIV_TYPE_ISO2D
               o%iso2d%p0   = p
               o%iso2d%x0   = x0
               o%iso2d%dx   = dx
               o%iso2d%nx   = nx_model
               o%iso2d%z0   = z0
               o%iso2d%dz   = dz
               o%iso2d%nz   = nz
               o%iso2d%ud   = -1.0 ! upward propagation
               o%iso2d%vp   => vpsm
               varout(1) = xloc !starting x
               varout(2) = z    !starting z
               varout(3) = p    !starting p
               do while (z > zsrc)
                  deltat  = dt
                  deltat  = 4*dt
                  var(1)  = varout(1) ! x
                  var(2)  = varout(2) ! z
                  var(3)  = varout(3) ! p
                  call modmo2d_isoderivs2d(t, nvar, var, o%iso2d, dvar)
                  call modmo2d_rkstep(var, dvar, nvar, t, deltat, o, varout, &
                                      work)
                  z = varout(2)
                  if (varout(2) == var(2)) exit
                  t = t + deltat
               end do
               ! Linearly interpolate to get x,t at zsrc
               x    = varout(1)
               xold = var(1)
               zold = var(2)
               told = t - deltat
               if (z == zold) then
                  xrp = xloc + 10.*(nx_model - 1)*dx
                  trp = tdead
               else
                  bbb  = (zsrc-z)/(zold-z)
                  aaa = 1. - bbb
                  xrp = aaa*x + bbb*xold
                  trp = aaa*t + bbb*told
               end if


               !
               ! Do negative p's, find xrn, trn
               !
               kx = (xloc-x0)/dx + 1
               z  = z0 + (kz-1)*dz
               p  = sin(-theta)/vpsm(kz,kx)
               t  = 0.0
               o%deriv_type = MODMO2D_DERIV_TYPE_ISO2D
               o%iso2d%p0   = p
               o%iso2d%x0   = x0
               o%iso2d%dx   = dx
               o%iso2d%nx   = nx_model
               o%iso2d%z0   = z0
               o%iso2d%dz   = dz
               o%iso2d%nz   = nz
               o%iso2d%ud   = -1.0 ! upward propagation
               o%iso2d%vp   => vpsm
               varout(1) = xloc !starting x
               varout(2) = z    !starting z
               varout(3) = p    !starting p
               do while (z > zsrc)
                  deltat  = dt
                  deltat  = 4*dt
                  var(1)  = varout(1) ! x
                  var(2)  = varout(2) ! z
                  var(3)  = varout(3) ! p
                  call modmo2d_isoderivs2d(t, nvar, var, o%iso2d, dvar)
                  call modmo2d_rkstep(var, dvar, nvar, t, deltat, o, varout, &
                                      work)
                  z = varout(2)
                  if (varout(2) == var(2)) exit
                  t = t + deltat
               end do
               ! Linearly interpolate to get x,t at zsrc
               x    = varout(1)
               xold = var(1)
               zold = var(2)
               told = t - deltat
               if (z == zold) then
                  xrn = xloc - 10.*(nx_model - 1)*dx
                  trn = tdead
               else
                  bbb  = (zsrc-z)/(zold-z)
                  aaa = 1. - bbb
                  xrn = aaa*x + bbb*xold
                  trn = aaa*t + bbb*told
               end if


               !
               ! NEED: if xrp or xrn is "null", then xr = "null"
               ! NEED: if trp or trn is "null", then tr = "null"
               !
               ! 0.5 factor needed since xr,tr will get doubled later on
               !
               if (trp == tdead .or. trn == tdead) then
                  xr(kz,jp) = 10.0*(nx_model - 1)*dx
                  tr(kz,jp) = tdead
               else
                  xr(kz,jp) = 0.5*(xrp-xrn)
                  tr(kz,jp) = 0.5*(trp+trn)
               end if
            end do

            ! Linearly-interpolate missing kz's
            do kz = sz, nz ! loop over starting z locations for upward shooting
               kz1 = max(sz, sz + izinc*((kz-sz)/izinc))
               kz2 = min(nz, kz1 + izinc)
               if (kz == kz1) cycle
               if (tr(kz1,jp) == tdead .or. tr(kz2,jp) == tdead) then
                  xr(kz,jp) = 10.0*(nx_model - 1)*dx
                  tr(kz,jp) = tdead
               else
                  bbb  = real(kz-kz1)/real(izinc)
                  aaa = 1. - bbb
                  xr(kz,jp) = aaa*xr(kz1,jp) + bbb*xr(kz2,jp)
                  tr(kz,jp) = aaa*tr(kz1,jp) + bbb*tr(kz2,jp)
               end if
            end do
         end do

         write(xrfile,*) 'xr_cerv_rk.bin'
         write(trfile,*) 'tr_cerv_rk.bin'

      else if (mode == 2) then

         ierr = -1
         call pc_error('modmo2d_vzmapr_iso2d: rt mode = ', mode, ' unsupported')
         return

         ! Calculate xr, tr using old tried-n-true method, but upwards
         !call pc_warning('modmo2d_vzmapr_iso2d: upwards raytracing (old)')
         sz    = (zsrc-z0)/dz + 1
         do jp = 1, np
            theta = thetamin + float(jp-1)*dtheta
            !call pc_warning('modmo2d_vzmapr_iso2d: (up) jp = ', jp)
            do kz = sz, nz ! loop over starting z locations for upward shooting
               !call pc_warning('modmo2d_vzmapr_iso2d: (up) jp=',jp,', kz=',kz)
               kx = (xloc-x0)/dx + 1
               p  = sin(theta)/vpsm(kz,kx)
               x  = 0.0
               t  = 0.0
               jx = kx
               jz = kz
               z  = z0 + (kz-1)*dz
               dr = dz
               ! Begin ray path loop
               do while (z > zsrc)
                  bbb  = ((z-z0) - (jz-1)*dz)/dz
                  aaa = 1. - bbb
                  if (jz < nz-1) then
                     slow = aaa/vpsm(jz,jx) + bbb/vpsm(jz+1,jx)
                  else
                     slow = 1.0/vpsm(jz,jx)
                  end if
                  s = p / slow

                  ! First "critical" ray:
                  ! (accuracy depends on using many rays)
                  ! Assume horizontal propagation;
                  ! then group vel = phase vel (VTI)
                  if (s >= 0.999999) then
                     if (icrit(jz) == 0 .and. noint == 0) then
! Ignore for now
!
!                       nray = (xmax + dx - x) / dx
!                       !write(lunprint, *) 'nray, jp, jz = ', nray, jp, jz
!                       !write(lunprint, *) 'icrit = ', icrit(jz)
!                       if (nray > 1) then
!                          do jray = 1, nray
!                             xray = x + (jray-1) * dx
!                             jx   = nint(xray / dx)
!                             t    = t + dx / vpsm(jz)
!                             if (jx <= nx) then
!                                xr(jz,jp) = xray
!                                tr(jz,jp) = t
!                             end if
!                          end do
!                       end if
                        icrit(jz) = 1
                     end if
                     exit
                  end if

                  ! Typical rays: use group angle and velocity to update
                  !               location and traveltime
                  theta = asin(s)
                  sg    = abs(sin(theta))
                  cg    = sqrt(1.0 - sg**2)
                  dx1  = sg * dr
                  dz1  = cg * dr
                  x    = x + dx1
                  z    = z - dz1
                  jz   = (z-z0) / dz + 1
                  if (jz < nz-1) then
                     bbb   = ((z-z0) - (jz-1)*dz)/dz
                     aaa   = 1. - bbb
                     slowg = aaa/vpsm(jz,jx) + bbb/vpsm(jz+1,jx)
                  else
                     slowg = 1./vpsm(jz,jx)
                  end if
                  t    = t + dr * slowg
               end do      ! End of ray path loop
               xr(kz,jp) = x
               tr(kz,jp) = t
            end do
         end do            ! End of np loop

         write(xrfile,*) 'xr_old.bin'
         write(trfile,*) 'tr_old.bin'

      else
         ierr = -1
         call pc_error('modmo2d_vzmapr_iso2d: unknown rt mode = ', mode)
         return
      end if


      ! Dump xr/tr to file for inspection
      if (dump) then
         lundump = cio_fopen(xrfile, 'w')
         if (lundump == 0) &
            call pc_error('modmo2d_vzmapr_iso2d: error opening ', trim(xrfile))
         i = cio_fwrite(xr, sizeof(xr(1,1)), nz*np, lundump)
         if (i /= nz*np) then
            write(dummy,*) 'modmo2d_vzmapr_iso2d: only wrote ', i, ' of ', &
                           nz*np, ' items in ', trim(xrfile)
            call pc_error(dummy)
         endif
         ierr = cio_fclose(lundump)
         if (ierr /= 0) &
            call pc_error('modmo2d_vzmapr_iso2d: error closing ', trim(xrfile))

         lundump = cio_fopen(trfile, 'w')
         if (lundump == 0) &
            call pc_error('modmo2d_vzmapr_iso2d: error opening ', trim(trfile))
         i = cio_fwrite(tr, sizeof(tr(1,1)), nz*np, lundump)
         if (i /= nz*np) then
            write(dummy,*) 'modmo2d_vzmapr_iso2d: only wrote ', i, ' of ', &
                          nz*np, ' items in ', trim(trfile)
            call pc_error(dummy)
         endif
         ierr = cio_fclose(lundump)
         if (ierr /= 0) &
            call pc_error('modmo2d_vzmapr_iso2d: error closing ', trim(trfile))
      end if


      ! Scan travel-time map and fill in missing times
      do jz = 1, nz
         do jp = 1, np -1
            x1 = xr(jz,jp)
            x2 = xr(jz,jp+1)
            if (x1 /= x2 .and. x1 >= 0.0 .and. x2 >= 0.0) then
               ix1 = x1 / dx +1
               ix2 = x2 / dx + 1
               do jx = ix1, ix2
                  if (jx <= nx) then
                     aa = ((jx-1) * dx - x1) / (x2-x1)
                     tmap(jz,jx) = (1.0-aa)*tr(jz,jp) + aa*tr(jz,jp+1)
                  end if
               end do
            end if
         end do
      end do

      if (noint /= 0) go to 1000

      ! Horizontal scan
      iflag = 0
      do jz = 1, nz
         if (tmap(jz,nx) /= tdead) iflag = iflag + 1
         icount = 0
         do jx = 1, nx
            if (tmap(jz,jx) /= tdead) then
               icount = icount + 1
               izf(icount) = jx
            end if
         end do
         if (icount > 1) then
            do jc = 1, icount-1
               jx1 = izf(jc)
               jx2 = izf(jc+1)
               if (jx1 /= jx2) then
                  do jx = jx1, jx2
                     a = float(jx-jx1) / float(jx2-jx1)
                     tmap(jz,jx) = tmap(jz,jx1) + a*(tmap(jz,jx2)-tmap(jz,jx1))
                  end do
               end if

               ! If any rays go out the side fill last row to the corner
               if (jx2 < nx .and. iflag > 0 .and. jz == nz) then
                  do jx = jx2 + 1, nx
                     a = float(jx-jx1) / float(jx2-jx1)
                     tmap(jz,jx) = tmap(jz,jx1) + a*(tmap(jz,jx2)-tmap(jz,jx1))
                  end do
               end if
            end do
         end if
      end do

      ! Vertical scan
      izsrc = (zsrc-z0)/dz + 1
      do jx = 1, nx
         icount = 0
         do jz = izsrc, nz
            if (tmap(jz,jx) /= tdead) then
               icount = icount + 1
               izf(icount) = jz
            end if
         end do
         if (icount > 1) then
            do jc = 1, icount - 1
               jz1 = izf(jc)
               jz2 = izf(jc+1)
               if (jz1 /= jz2) then
                  do jz = jz1, jz2
                     a = float(jz-jz1) / float(jz2-jz1)
                     tmap(jz,jx) = tmap(jz1,jx) + a*(tmap(jz2,jx)-tmap(jz1,jx))
                  end do
               end if
            end do
         end if
      end do

      ! Vertically smooth travel-times
      ismoott = 0
      !scale = 1.0 / 9.0
      scale = 1.0 / 3.0
      if (ismoott > 0) then
         do js = 1, ismoott
            do jx = 2, nx-1
               do jz = 2, nz - 1
                  slo(jz) = scale*(tmap(jz-1,jx)+tmap(jz,jx)+tmap(jz+1,jx))
                  !       + tmap(jz-1,jx-1)+tmap(jz,jx-1)+tmap(jz+1,jx-1) &
                  !       + tmap(jz-1,jx+1)+tmap(jz,jx+1)+tmap(jz+1,jx+1) )
               end do
               do jz = 2, nz - 1
                  tmap(jz,jx) = slo(jz)
               end do
            end do
         end do
      end if

 1000 continue

      do jx = 1, nx
         do jz = 1, nz
            if (tmap(jz,jx) >= tbig) tmap(jz,jx) = tbig
         end do
      end do

      !deallocate(var)
      !deallocate(dvar)
      !deallocate(varout)
      !deallocate(work)
      !deallocate(vpsm)

      return
   end subroutine modmo2d_vzmapr_iso2d


!!--------------------------- modmo2d_rkstep ---------------------------------!!
!!--------------------------- modmo2d_rkstep ---------------------------------!!
!!--------------------------- modmo2d_rkstep ---------------------------------!!
   ! Runge-Kutta
   subroutine modmo2d_rkstep(f, df, n, x, h, o, fout, work)

      implicit none

      ! Subroutine arguments
      real,    intent(in)    :: f(:)
      real,    intent(in)    :: df(:)
      integer, intent(in)    :: n
      real,    intent(in)    :: x
      real,    intent(in)    :: h
      type(modmo2d_deriv_struct), intent(in)  :: o
      real,    intent(out)   :: fout(:)
      real,    intent(inout), target :: work(:, :) ! should be work(n,3)

      ! Local variables

      real, pointer :: dfm(:), dft(:), ft(:)

      dfm => work(:, 1)
      dft => work(:, 2)
      ft  => work(:, 3)

      ft  = f + 0.5*h*df
      if (o%deriv_type == MODMO2D_DERIV_TYPE_VTI1D) then
         call modmo2d_vtiderivs(x+0.5*h, n, ft, o%vti1d, dft)
      else if (o%deriv_type == MODMO2D_DERIV_TYPE_VTI2D) then
         call modmo2d_vtiderivs2d(x+0.5*h, n, ft, o%vti2d, dft)
      else if (o%deriv_type == MODMO2D_DERIV_TYPE_ISO2D) then
         call modmo2d_isoderivs2d(x+0.5*h, n, ft, o%iso2d, dft)
      else
         call pc_error('modmo2d_rkstep: unknown deriv_type = ', o%deriv_type)
      end if
      ft  = f + 0.5*h*dft
      if (o%deriv_type == MODMO2D_DERIV_TYPE_VTI1D) then
         call modmo2d_vtiderivs(x+0.5*h, n, ft, o%vti1d, dfm)
      else if (o%deriv_type == MODMO2D_DERIV_TYPE_VTI2D) then
         call modmo2d_vtiderivs2d(x+0.5*h, n, ft, o%vti2d, dfm)
      else if (o%deriv_type == MODMO2D_DERIV_TYPE_ISO2D) then
         call modmo2d_isoderivs2d(x+0.5*h, n, ft, o%iso2d, dfm)
      else
         call pc_error('modmo2d_rkstep: unknown deriv_type = ', o%deriv_type)
      end if
      ft  = f + h*dfm
      dfm = dfm + dft
      if (o%deriv_type == MODMO2D_DERIV_TYPE_VTI1D) then
         call modmo2d_vtiderivs(x+h,     n, ft, o%vti1d, dft)
      else if (o%deriv_type == MODMO2D_DERIV_TYPE_VTI2D) then
         call modmo2d_vtiderivs2d(x+h,     n, ft, o%vti2d, dft)
      else if (o%deriv_type == MODMO2D_DERIV_TYPE_ISO2D) then
         call modmo2d_isoderivs2d(x+h,     n, ft, o%iso2d, dft)
      else
         call pc_error('modmo2d_rkstep: unknown deriv_type = ', o%deriv_type)
      end if
      fout = f + h/6.0*(df+dft+2.0*dfm)
      !write(lunprint,*) 'dxdt,dzdt,dpdt== ', &
      !                  fout(1)-f(1), fout(2)-f(2), fout(3)-f(3)

      return
   end subroutine modmo2d_rkstep


!!--------------------------- modmo2d_vtiderivs ------------------------------!!
!!--------------------------- modmo2d_vtiderivs ------------------------------!!
!!--------------------------- modmo2d_vtiderivs ------------------------------!!
   subroutine modmo2d_vtiderivs(z, nvar, var, o, dvar)

      implicit none

      ! Subroutine arguments
      real,    intent(in)  :: z
      integer, intent(in)  :: nvar
      real,    intent(in)  :: var(:)
      type(modmo2d_deriv_vti1d_struct), intent(in)  :: o
      real,    intent(out) :: dvar(:)

      ! Local variables
      real, pointer :: vparr(:), vsarr(:), epsarr(:), delarr(:)
      real :: vp, vs, eps, del
      real :: a11, a13, a33, a44
      real :: z0, dz
      real :: p, q
      real :: ud   ! upgoing=-1, downgoing=1
      real :: a, d1, d2, b, c, denom, aaa, bbb
      real :: dxdz, dtdz
      integer :: nz, jz

      p   =  o%p0
      z0  =  o%z0
      dz  =  o%dz
      nz  =  o%nz
      ud  =  o%ud
      vparr  => o%vp
      vsarr  => o%vs
      epsarr => o%eps
      delarr => o%del

      ! Linearly interpolate velocities in between boundaries and use constant
      ! extrapolation beyond boundaries.
      jz  = (z-z0)/dz + 1
      if (jz < 1) then
         vp  = vparr(1)
         vs  = vsarr(1)
         eps = epsarr(1)
         del = delarr(1)
      else if (jz >= nz) then
         vp  = vparr(nz)
         vs  = vsarr(nz)
         eps = epsarr(nz)
         del = delarr(nz)
      else
         bbb = ((z-z0) - (jz-1)*dz)/dz
         aaa = 1. - bbb
         vp  = aaa*vparr(jz)  + bbb*vparr(jz+1)
         vs  = aaa*vsarr(jz)  + bbb*vsarr(jz+1)
         eps = aaa*epsarr(jz) + bbb*epsarr(jz+1)
         del = aaa*delarr(jz) + bbb*delarr(jz+1)
      end if

      a33 = vp*vp
      a44 = vs*vs
      a11 = a33*(1.+2.*eps)
      a13 = sqrt(((1.+2.*del)*a33 - a44)*(a33-a44)) - a44
      a   = a13*(a13 + 2.*a44) - a11*a33
      b   = a33 + a44 + a*p*p
      c   = 1. - (a11+a44)*p*p + a11*a44*p*p*p*p
      q   = ud*sqrt((b - sqrt(b*b - 4.*a33*a44*c))/(2.*a33*a44))
      d1  = 1. - a11*p*p - a44*q*q
      d2  = 1. - a44*p*p - a33*q*q
      denom = q*(a33 + a44 - 2.*a33*a44*q*q + a*p*p)
      dxdz  = p*(a11 + a44 - 2.*a11*a44*p*p + a*q*q)/denom
      dtdz  = (d1 + d2)/denom
      dvar(1) = dxdz
      dvar(2) = dtdz

      return
   end subroutine modmo2d_vtiderivs


!!--------------------------- modmo2d_vtiderivs2d ----------------------------!!
!!--------------------------- modmo2d_vtiderivs2d ----------------------------!!
!!--------------------------- modmo2d_vtiderivs2d ----------------------------!!
   subroutine modmo2d_vtiderivs2d(t, nvar, var, o, dvar)

      implicit none

      ! Subroutine arguments
      real,    intent(in)  :: t
      integer, intent(in)  :: nvar
      real,    intent(in)  :: var(:)
      type(modmo2d_deriv_vti2d_struct), intent(in)  :: o
      real,    intent(out) :: dvar(:)

      ! Local variables
      real, pointer :: vparr(:,:), vsarr(:,:), epsarr(:,:), delarr(:,:)
      real :: vp, vs, eps, del
      real :: dvpdx, dvsdx, depsdx, ddeldx
      real :: a11, a13, a33, a44
      real :: da11dx, da13dx, da33dx, da44dx
      real :: x, x0, dx
      real :: z, z0, dz
      real :: p, q, qtemp1
      real :: ud   ! upgoing=-1, downgoing=1
      real :: a, b, c, d, d1, d2, f, aaa, bbb, ccc, ddd
      real :: dxdt, dzdt, dpdt
      integer :: nx, jx
      integer :: nz, jz


      x   =  var(1)
      z   =  var(2)
      p   =  var(3)
      x0  =  o%x0
      dx  =  o%dx
      nx  =  o%nx
      z0  =  o%z0
      dz  =  o%dz
      nz  =  o%nz
      ud  =  o%ud
      vparr  => o%vp
      vsarr  => o%vs
      epsarr => o%eps
      delarr => o%del

      ! Linearly interpolate velocities in between boundaries and use constant
      ! extrapolation beyond boundaries.
      jx  = (x-x0)/dx + 1
      jz  = (z-z0)/dz + 1
      !write(lunprint,*) 'jx, jz         = ', jx, jz
      if (jx < 1 .or. jx >= nx) then
         jx = min(max(jx, 1), nx)
         if (jz < 1 .or. jz >= nz) then
            jz = min(max(jz, 1), nz)
            vp  = vparr (jz,jx)
            vs  = vsarr (jz,jx)
            eps = epsarr(jz,jx)
            del = delarr(jz,jx)
         else
            bbb = ((z-z0) - (jz-1)*dz)/dz
            aaa = 1. - bbb
            vp  = aaa*vparr (jz,jx) + bbb*vparr (jz+1,jx)
            vs  = aaa*vsarr (jz,jx) + bbb*vsarr (jz+1,jx)
            eps = aaa*epsarr(jz,jx) + bbb*epsarr(jz+1,jx)
            del = aaa*delarr(jz,jx) + bbb*delarr(jz+1,jx)
         end if
         dvpdx  = 0.
         dvsdx  = 0.
         depsdx = 0.
         ddeldx = 0.
         !write(lunprint,*) 'dvpdx1 = ', dvpdx
      else
         if (jz < 1 .or. jz >= nz) then
            jz = min(max(jz, 1), nz)
            ddd = ((x-x0) - (jx-1)*dx)/dx
            ccc = 1. - ddd
            vp  = ccc*vparr (jz,jx) + ddd*vparr (jz,jx+1)
            vs  = ccc*vsarr (jz,jx) + ddd*vsarr (jz,jx+1)
            eps = ccc*epsarr(jz,jx) + ddd*epsarr(jz,jx+1)
            del = ccc*delarr(jz,jx) + ddd*delarr(jz,jx+1)
            !dvpdx  = ud*(vparr (jz,jx+1) - vparr (jz,jx))/dx
            !dvsdx  = ud*(vsarr (jz,jx+1) - vsarr (jz,jx))/dx
            !depsdx = ud*(epsarr(jz,jx+1) - epsarr(jz,jx))/dx
            !ddeldx = ud*(delarr(jz,jx+1) - delarr(jz,jx))/dx
            dvpdx  = (vparr (jz,jx+1) - vparr (jz,jx))/dx
            dvsdx  = (vsarr (jz,jx+1) - vsarr (jz,jx))/dx
            depsdx = (epsarr(jz,jx+1) - epsarr(jz,jx))/dx
            ddeldx = (delarr(jz,jx+1) - delarr(jz,jx))/dx
            !write(lunprint,*) 'jx, jz         = ', jx, jz
            !write(lunprint,*) 'vparr(jz,jx+1) = ', vparr(jz,jx+1)
            !write(lunprint,*) 'vparr(jz,jx  ) = ', vparr(jz,jx  )
            !write(lunprint,*) 'dx             = ', dx
            !write(lunprint,*) 'dvpdx2         = ', dvpdx
         else
            ddd = ((x-x0) - (jx-1)*dx)/dx
            ccc = 1. - ddd
            bbb = ((z-z0) - (jz-1)*dz)/dz
            aaa = 1. - bbb
            vp  = ccc*(aaa*vparr (jz,jx  ) + bbb*vparr (jz+1,jx  )) + &
                  ddd*(aaa*vparr (jz,jx+1) + bbb*vparr (jz+1,jx+1))
            vs  = ccc*(aaa*vsarr (jz,jx  ) + bbb*vsarr (jz+1,jx  )) + &
                  ddd*(aaa*vsarr (jz,jx+1) + bbb*vsarr (jz+1,jx+1))
            eps = ccc*(aaa*epsarr(jz,jx  ) + bbb*epsarr(jz+1,jx  )) + &
                  ddd*(aaa*epsarr(jz,jx+1) + bbb*epsarr(jz+1,jx+1))
            del = ccc*(aaa*delarr(jz,jx  ) + bbb*delarr(jz+1,jx  )) + &
                  ddd*(aaa*delarr(jz,jx+1) + bbb*delarr(jz+1,jx+1))
            !dvpdx  = ud*((aaa*vparr (jz,jx+1) + bbb*vparr (jz+1,jx+1)) - &
            !             (aaa*vparr (jz,jx  ) + bbb*vparr (jz+1,jx  )))/dx
            !dvsdx  = ud*((aaa*vsarr (jz,jx+1) + bbb*vsarr (jz+1,jx+1)) - &
            !             (aaa*vsarr (jz,jx  ) + bbb*vsarr (jz+1,jx  )))/dx
            !depsdx = ud*((aaa*epsarr(jz,jx+1) + bbb*epsarr(jz+1,jx+1)) - &
            !             (aaa*epsarr(jz,jx  ) + bbb*epsarr(jz+1,jx  )))/dx
            !ddeldx = ud*((aaa*delarr(jz,jx+1) + bbb*delarr(jz+1,jx+1)) - &
            !             (aaa*delarr(jz,jx  ) + bbb*delarr(jz+1,jx  )))/dx
            dvpdx  = ((aaa*vparr (jz,jx+1) + bbb*vparr (jz+1,jx+1)) - &
                      (aaa*vparr (jz,jx  ) + bbb*vparr (jz+1,jx  )))/dx
            dvsdx  = ((aaa*vsarr (jz,jx+1) + bbb*vsarr (jz+1,jx+1)) - &
                      (aaa*vsarr (jz,jx  ) + bbb*vsarr (jz+1,jx  )))/dx
            depsdx = ((aaa*epsarr(jz,jx+1) + bbb*epsarr(jz+1,jx+1)) - &
                      (aaa*epsarr(jz,jx  ) + bbb*epsarr(jz+1,jx  )))/dx
            ddeldx = ((aaa*delarr(jz,jx+1) + bbb*delarr(jz+1,jx+1)) - &
                      (aaa*delarr(jz,jx  ) + bbb*delarr(jz+1,jx  )))/dx
            !write(lunprint,*) 'jx, jz           = ', jx, jz
            !write(lunprint,*) 'aaa              = ', aaa
            !write(lunprint,*) 'bbb              = ', bbb
            !write(lunprint,*) 'vparr(jz,  jx+1) = ', vparr(jz,  jx+1)
            !write(lunprint,*) 'vparr(jz+1,jx+1) = ', vparr(jz+1,jx+1)
            !write(lunprint,*) 'vparr(jz,  jx  ) = ', vparr(jz,  jx  )
            !write(lunprint,*) 'vparr(jz+1,jx  ) = ', vparr(jz+1,jx  )
            !write(lunprint,*) 'dx               = ', dx
            !write(lunprint,*) 'dvpdx3           = ', dvpdx
         end if
      end if


      a33 = vp*vp
      a44 = vs*vs
      a11 = a33*(1.+2.*eps)
      a13 = sqrt(((1.+2.*del)*a33 - a44)*(a33-a44)) - a44
      da33dx = 2.*vp*dvpdx
      da44dx = 2.*vs*dvsdx
      da11dx = da33dx*(1.+2.*eps) + 2.*a33*depsdx
      da13dx = (((1.+2.*del)*a33 - a44)*(da33dx-da44dx) &
                + ((1.+2.*del)*da33dx + 2.*a33*ddeldx - da44dx)*(a33-a44)) &
               /(2.*sqrt(((1.+2.*del)*a33 - a44)*(a33-a44))) - da44dx


      a   = a13*(a13 + 2.*a44) - a11*a33
      b   = a33 + a44 + a*p*p
      c   = 1. - (a11+a44)*p*p + a11*a44*p*p*p*p
      !q   = ud*sqrt((b - sqrt(b*b - 4.*a33*a44*c))/(2.*a33*a44))
      qtemp1 = b - sqrt(b*b - 4.*a33*a44*c)
      if (qtemp1 < 0.) then
         ! Critical angle reached?
         q   = 0.
         d1  = 1. - a11*p*p
         d2  = 1. - a44*p*p
         d   = d1 + d2
         f   = p*p*d1
         dxdt  = p*(a11 + a44 - 2.*a11*a44*p*p)/d
         dzdt  = 0.
         dpdt  = -(da11dx*p*p*d2 + da44dx*f)/(2.*d)
      else
         q   = ud*sqrt(qtemp1/(2.*a33*a44))
         d1  = 1. - a11*p*p - a44*q*q
         d2  = 1. - a44*p*p - a33*q*q
         d   = d1 + d2
         f   = p*p + q*q - a11*p*p*p*p - a33*q*q*q*q + 2.*p*p*q*q*a13
         dxdt  = p*(a11 + a44 - 2.*a11*a44*p*p + a*q*q)/d
         dzdt  = q*(a33 + a44 - 2.*a33*a44*q*q + a*p*p)/d
         dpdt  = -(da11dx*p*p*d2 + da33dx*q*q*d1 + da44dx*f &
                   + 2.*da13dx*p*p*q*q*(a13+a44))/(2.*d)
      end if


      dvar(1) = dxdt
      dvar(2) = dzdt
      dvar(3) = dpdt
      !write(lunprint,*) 'dxdt,dzdt,dpdt= ', dxdt, dzdt, dpdt
      if (clean_zero(dvar) > 0) then
         write(lunprint,*) 'vp  = ', vp
         write(lunprint,*) 'vs  = ', vs
         write(lunprint,*) 'eps = ', eps
         write(lunprint,*) 'del = ', del
         write(lunprint,*) 'dvpdx  = ', dvpdx
         write(lunprint,*) 'dvsdx  = ', dvsdx
         write(lunprint,*) 'depsdx = ', depsdx
         write(lunprint,*) 'ddeldx = ', ddeldx
         write(lunprint,*)
         write(lunprint,*) 'a33 = ', a33
         write(lunprint,*) 'a44 = ', a44
         write(lunprint,*) 'a11 = ', a11
         write(lunprint,*) 'a13 = ', a13
         write(lunprint,*) 'da33dx = ', da33dx
         write(lunprint,*) 'da44dx = ', da44dx
         write(lunprint,*) 'da11dx = ', da11dx
         write(lunprint,*) 'da13dx = ', da13dx
         write(lunprint,*)
         write(lunprint,*) 'a  = ', a
         write(lunprint,*) 'b  = ', b
         write(lunprint,*) 'c  = ', c
         write(lunprint,*) 'p  = ', p
         write(lunprint,*) 'q  = ', q
         write(lunprint,*) 'd1 = ', d1
         write(lunprint,*) 'd2 = ', d2
         write(lunprint,*) 'd  = ', d
         write(lunprint,*) 'f  = ', f
         call pc_error("modmo2d_vtiderivs2d: found NaNs!")
      end if

      return
   end subroutine modmo2d_vtiderivs2d


!!--------------------------- modmo2d_isoderivs2d ----------------------------!!
!!--------------------------- modmo2d_isoderivs2d ----------------------------!!
!!--------------------------- modmo2d_isoderivs2d ----------------------------!!
   subroutine modmo2d_isoderivs2d(t, nvar, var, o, dvar)

      implicit none

      ! Subroutine arguments
      real,    intent(in)  :: t
      integer, intent(in)  :: nvar
      real,    intent(in)  :: var(:)
      type(modmo2d_deriv_iso2d_struct), intent(in)  :: o
      real,    intent(out) :: dvar(:)

      ! Local variables
      real, pointer :: vparr(:,:)
      real :: vp, dvpdx, vpsq
      real :: x, x0, dx
      real :: z, z0, dz
      real :: p, q, qtemp1
      real :: ud   ! upgoing=-1, downgoing=1
      real :: aaa, bbb, ccc, ddd
      real :: dxdt, dzdt, dpdt
      integer :: nx, jx
      integer :: nz, jz


      x   =  var(1)
      z   =  var(2)
      p   =  var(3)
      x0  =  o%x0
      dx  =  o%dx
      nx  =  o%nx
      z0  =  o%z0
      dz  =  o%dz
      nz  =  o%nz
      ud  =  o%ud
      vparr  => o%vp

      ! Linearly interpolate velocities in between boundaries and use constant
      ! extrapolation beyond boundaries.
      jx  = (x-x0)/dx + 1
      jz  = (z-z0)/dz + 1
      if (jx < 1 .or. jx >= nx) then
         jx = min(max(jx, 1), nx)
         if (jz < 1 .or. jz >= nz) then
            jz = min(max(jz, 1), nz)
            vp = vparr (jz,jx)
         else
            bbb = ((z-z0) - (jz-1)*dz)/dz
            aaa = 1. - bbb
            vp  = aaa*vparr (jz,jx) + bbb*vparr (jz+1,jx)
         end if
         dvpdx  = 0.
      else
         if (jz < 1 .or. jz >= nz) then
            jz  = min(max(jz, 1), nz)
            ddd = ((x-x0) - (jx-1)*dx)/dx
            ccc = 1. - ddd
            vp  = ccc*vparr (jz,jx) + ddd*vparr (jz,jx+1)
            dvpdx  = (vparr (jz,jx+1) - vparr (jz,jx))/dx
         else
            ddd = ((x-x0) - (jx-1)*dx)/dx
            ccc = 1. - ddd
            bbb = ((z-z0) - (jz-1)*dz)/dz
            aaa = 1. - bbb
            vp  = ccc*(aaa*vparr (jz,jx  ) + bbb*vparr (jz+1,jx  )) + &
                  ddd*(aaa*vparr (jz,jx+1) + bbb*vparr (jz+1,jx+1))
            dvpdx  = ((aaa*vparr (jz,jx+1) + bbb*vparr (jz+1,jx+1)) - &
                      (aaa*vparr (jz,jx  ) + bbb*vparr (jz+1,jx  )))/dx
         end if
      end if

      vpsq = vp*vp
      !q   = ud*sqrt(1./vpsq - p*p)
      qtemp1 = 1./vpsq - p*p
      if (qtemp1 < 0.) then
         ! Critical angle reached?
         q    = 0.
         dzdt = 0.
      else
         q    = ud*sqrt(qtemp1)
         dzdt = q*vpsq
      end if
      dxdt  = p*vpsq
      dpdt  = -dvpdx/vp

      dvar(1) = dxdt
      dvar(2) = dzdt
      dvar(3) = dpdt
      if (clean_zero(dvar) > 0) then
         write(lunprint,*) 'vp  = ', vp
         write(lunprint,*) 'dvpdx  = ', dvpdx
         write(lunprint,*) 'p  = ', p
         write(lunprint,*) 'q  = ', q
         call pc_error("modmo2d_isoderivs2d: found NaNs!")
      end if

      return
   end subroutine modmo2d_isoderivs2d


!!--------------------------- modmo2d_vpphase --------------------------------!!
!!--------------------------- modmo2d_vpphase --------------------------------!!
!!--------------------------- modmo2d_vpphase --------------------------------!!
      ! P-wave phase velocity
      function modmo2d_vpphase(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo2d_vpphase

      ! Local variables
      real :: ab,deltastar,dstar
      real :: sintheta,sin2theta,costheta,cos2theta

      sintheta=sin(theta)
      costheta=cos(theta)
      sin2theta=sintheta*sintheta
      cos2theta=costheta*costheta

      ab=1.0-beta*beta/alpha/alpha
      deltastar=(2.0*delta-epsilon)*ab
      dstar=0.5*ab*(sqrt(1.0+4.0*deltastar*sin2theta*cos2theta/ab/ab &
           +4.0*(ab+epsilon)*epsilon*sin2theta*sin2theta/ab/ab)-1.0)
      modmo2d_vpphase=alpha*sqrt(1.0+epsilon*sin2theta+dstar)

      return
      end function modmo2d_vpphase


!!--------------------------- modmo2d_dvpdtheta ------------------------------!!
!!--------------------------- modmo2d_dvpdtheta ------------------------------!!
!!--------------------------- modmo2d_dvpdtheta ------------------------------!!
      ! P-wave phase velocity derivative w.r.t. phase angle
      function modmo2d_dvpdtheta(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo2d_dvpdtheta

      ! Local variables
      real :: ab,deltastar,dstar,ddstardtheta
      real :: sintheta,sin2theta,costheta,cos2theta

      sintheta=sin(theta)
      costheta=cos(theta)
      sin2theta=sintheta*sintheta
      cos2theta=costheta*costheta

      ab=1.0-beta*beta/alpha/alpha
      deltastar=(2.0*delta-epsilon)*ab
      dstar=0.5*ab*(sqrt(1.0+4.0*deltastar*sin2theta*cos2theta/ab/ab  &
           +4.0*(ab+epsilon)*epsilon*sin2theta*sin2theta/ab/ab)-1.0)
      ddstardtheta=ab/4.0/sqrt(1.0+4.0*deltastar*sin2theta*cos2theta  &
           /ab/ab+4.0*(ab+epsilon)*epsilon*sin2theta*sin2theta/ab/ab) &
           *(8.0*deltastar*sintheta*costheta*(cos2theta-sin2theta)    &
           /ab/ab+16.0*(ab+epsilon)*epsilon*sin2theta*sintheta*costheta/ab/ab)
      modmo2d_dvpdtheta=0.5*alpha/sqrt(1.0+epsilon*sin2theta+dstar)           &
                *(2.0*epsilon*sintheta*costheta+ddstardtheta)

      return
      end function modmo2d_dvpdtheta


!!--------------------------- modmo2d_vpgroup --------------------------------!!
!!--------------------------- modmo2d_vpgroup --------------------------------!!
!!--------------------------- modmo2d_vpgroup --------------------------------!!
      ! P-wave group velocity
      function modmo2d_vpgroup(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo2d_vpgroup

      ! Local variables
      real :: v,dv

      v  = modmo2d_vpphase(theta,alpha,beta,epsilon,delta)
      dv = modmo2d_dvpdtheta(theta,alpha,beta,epsilon,delta)
      modmo2d_vpgroup = sqrt(v*v + dv*dv)

      return
      end function modmo2d_vpgroup


!!--------------------------- modmo2d_phigroup -------------------------------!!
!!--------------------------- modmo2d_phigroup -------------------------------!!
!!--------------------------- modmo2d_phigroup -------------------------------!!
      ! P-wave group angle
      function modmo2d_phigroup(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo2d_phigroup

      ! Local variables
      real :: v,dv,s,c

      s  = sin(theta)
      c  = cos(theta)
      v  = modmo2d_vpphase(theta,alpha,beta,epsilon,delta)
      dv = modmo2d_dvpdtheta(theta,alpha,beta,epsilon,delta)
      modmo2d_phigroup = atan2((v*s + dv*c), (v*c - dv*s))

      return
      end function modmo2d_phigroup


!!--------------------------- modmo2d_t2dzmo ---------------------------------!!
!!--------------------------- modmo2d_t2dzmo ---------------------------------!!
!!--------------------------- modmo2d_t2dzmo ---------------------------------!!
   ! Reverse time to depth moveout
   ! NOTE:
   !   1) need to move memory stuff (ntmax) out of here
   !   2) maybe add a call to spline primitive so all interp gets done in 1 call
   subroutine modmo2d_t2dzmo(offset, dx, dz, dzmap, dtmap, dtsec, tmax, tmap, &
                             nzmap, nzd, nx, nt, z0, z0map, t0, data, ierr)

      implicit none

      ! Subroutine arguments
      real,        intent(in)    :: offset, dx, dz, dzmap, dtmap, dtsec, tmax
      integer,     intent(in)    :: nzmap, nzd, nx, nt
      real,        intent(in)    :: tmap(nzmap,nx)
      real,        intent(in)    :: z0, z0map, t0
      real,        intent(inout) :: data(:)
      integer,     intent(inout) :: ierr

      ! Local variables
      integer            :: i1, ix, ixhi, ixlo, izhi, izlo, jt, jz, jz1
      integer, save      :: nt1, nt2
      real               :: a, b, c, d, rat, t    , tbig   , x, xlo, z  
      logical, save      :: first=.true.
      integer, parameter :: ntmax=20000
      integer, parameter :: ideriv=0
      real, save         :: ttab1(ntmax), ttab2(ntmax)
      real               :: datat(ntmax)
      real               :: wrk(ntmax, 3)

      ierr = 0

      ! Build interpolation tables for resampling input time data
      if (first) then
         first  = .false.
         rat    = dtsec/dtmap
         nt1    = nt
         nt2    = nint(nt*rat)
         jz1    = 0
         do jt = 1, nt1
            ttab1(jt) = float(jt-1) * dtsec + t0
         end do
         do jt = 1, nt2
            ttab2(jt) = float(jt-1) * dtmap + t0
         end do
      end if

      ! Super-sample data into datat (using ttab1 and ttab2)
      datat(1:nt2) = 0.0
      call spline(nt1, ttab1, data, wrk(:,1), wrk(:,2), wrk(:,3))
      do jt = 1, nt2
         call spline_v(nt1, ttab1, data, wrk(:,1), wrk(:,2), wrk(:,3), &
                       ttab2(jt), ideriv, datat(jt))
      end do

      ! Regrid datat from time (in dtmap ) increments to depth
      data(1:nzd) = 0.0
      x      = .5 * abs(offset)
      ix     = x / dx
      xlo    = ix * dx
      ixlo   = ix + 1
      ixhi   = ixlo + 1
      if (ixhi > nx) return

!     tbig   = (nt2 - 1) * dtmap * 2.
      tbig   = (nt2 - 1) * dtmap + t0
      b      = (x - xlo) / dx
      a      = 1.0 - b
!dan  write(0,*) 'ixlo,ixhi,a,b ',ixlo,ixhi,a,b

      if (z0==z0map .and. dz==dzmap .and. nzd==nzmap) then
         do jz = 1, nzmap
            t = 2.*(a*tmap(jz,ixlo) + b*tmap(jz,ixhi))
            if ((t >= t0) .and. (t < tbig)) then
               i1 = nint((t-t0) / dtmap) + 1
               data(jz) = datat(i1)+data(jz)
            end if
         end do
      else
         do jz = 1, nzd
            z = z0 + (jz-1)*dz
            izlo = (z-z0map)/dzmap + 1
            izhi = izlo + 1
            if (izhi>nzmap .or. izlo<1) then
               data(jz) = 0.0 + data(jz)
            else
               d = (z - (z0map+(izlo-1)*dzmap))/dzmap
               c = 1.0 - d
               t = 2.*(c*(a*tmap(izlo,ixlo) + b*tmap(izlo,ixhi)) + &
                       d*(a*tmap(izhi,ixlo) + b*tmap(izhi,ixhi)))
               if ((t >= t0) .and. (t < tbig)) then
                  i1 = nint((t-t0) / dtmap) + 1
                  data(jz) = datat(i1)+data(jz)
               end if
            end if
         end do
      end if

      return
   end subroutine modmo2d_t2dzmo


!!--------------------------- modmo2d_d2tzmo ---------------------------------!!
!!--------------------------- modmo2d_d2tzmo ---------------------------------!!
!!--------------------------- modmo2d_d2tzmo ---------------------------------!!
   ! Reverse depth to time moveout
   ! NOTE:
   !   1) need to move memory stuff (nzmax, ntmax) out of here
   !   2) maybe add a call to spline primitive so all interp gets done in 1 call
   subroutine modmo2d_d2tzmo(offset, dx, dz, dzmap, dtmap, dtsec, tmax, tmap, &
                             nzmap, nzd, nx, nt, z0, z0map, t0, data, ierr)

      implicit none

      ! Subroutine arguments
      real,        intent(in)    :: offset, dx, dz, dzmap, dtmap, dtsec, tmax
      integer,     intent(in)    :: nzmap, nzd, nx, nt
      real,        intent(in)    :: tmap(nzmap,nx)
      real,        intent(in)    :: z0, z0map, t0
      real,        intent(inout) :: data(:)
      integer,     intent(inout) :: ierr

      ! Local variables
      integer            :: ix, ixhi, ixlo, izhi, izlo, jt, jz, jtau
      integer, save      :: nz1, nz2
      integer, save      :: nt1, nt2
      real               :: a, b, c, d, t1, t2, tbig, x, xlo, dz2, z
      logical, save      :: first=.true.
      integer, parameter :: nzmax=10000
      integer, parameter :: ntmax=20000
      integer, parameter :: ideriv=0
      real, save         :: ztab1(nzmax), ztab2(nzmax)
      real, save         :: ttab1(ntmax), ttab2(ntmax)
      real               :: dataz(4*nzmax)
      real               :: datat(4*ntmax)
      real               :: wrk(nzmax, 3)
 

      ierr = 0

      ! Build interpolation tables for resampling data from dz to dz2
      if (first) then
         first = .false.
         ! ASSUMES NO RESAMPLING OF THE TTMAP
         ! but the depth data will be resampled 4 to 1
         dz2 = dz/4.
         nz1   = nzd
         nz2   = nz1*4
         do jz  = 1, nz1
            ztab1(jz) = float(jz-1) * dz + z0
         enddo
         do jz = 1, nz2
            ztab2(jz) = float(jz-1) * dz2 + z0
         enddo
         do jt = 1, nt
            ttab2(jt) = float(jt-1) * dtsec + t0
         enddo
      endif

      ! Resample data into dataz (using ztab1 and ztab2)
      ! The z resampling is 4
      dataz(1:nz2) = 0.0
      call spline(nz1, ztab1, data, wrk(:,1), wrk(:,2), wrk(:,3))
      do jz = 1, nz2
         call spline_v(nz1, ztab1, data, wrk(:,1), wrk(:,2), wrk(:,3), &
                       ztab2(jz), ideriv, dataz(jz))
      end do

      ! Compute correct sample by moveout
      x      = .5 * abs(offset)
      ix     = x / dx
      xlo    = ix * dx
      ixlo   = ix + 1
      ixhi   = ixlo + 1
      if (ixhi > nx) return

      tbig   = (nt - 1) * dtsec + t0
      b      = (x - xlo) / dx
      a      = 1.0 - b
      nt1 = nt*dtsec/dtmap
      nt2 = nt
      datat(1:nz1*4) = 0.0
      data(1:nt2)    = 0.0

      if (z0==z0map .and. dz==dzmap .and. nzd==nzmap) then
         t2 = 2. * (a*tmap(1,ixlo) + b*tmap(1,ixhi))
      else
         z = z0
         izlo = (z-z0map)/dzmap + 1
         izhi = izlo + 1
         if (izhi>nzmap .or. izlo<1) then
            t2 = 2.*tbig
         else
            d  = (z - (z0map+(izlo-1)*dzmap))/dzmap
            c  = 1.0 - d
            t2 = 2.*(c*(a*tmap(izlo,ixlo) + b*tmap(izlo,ixhi)) + &
                    d*(a*tmap(izhi,ixlo) + b*tmap(izhi,ixhi)))
         end if
      end if
      ttab1(1) = 0.0
      jtau = 0
      do jz = 1, nz1-1
         t1 = t2
         if (z0==z0map .and. dz==dzmap .and. nzd==nzmap) then
            t2 = 2. * (a*tmap(jz+1,ixlo) + b*tmap(jz+1,ixhi))
         else
            z = z0 + jz*dz
            izlo = (z-z0map)/dzmap + 1
            izhi = izlo + 1
            if (izhi>nzmap .or. izlo<1) then
               t2 = 2.*tbig
            else
               d  = (z - (z0map+(izlo-1)*dzmap))/dzmap
               c  = 1.0 - d
               t2 = 2.*(c*(a*tmap(izlo,ixlo) + b*tmap(izlo,ixhi)) + &
                        d*(a*tmap(izhi,ixlo) + b*tmap(izhi,ixhi)))
            end if
         end if
         if (t1 <  t0   .or. t2 <  t0  ) cycle
         if (t1 >= tbig .or. t2 >= tbig) cycle
         if (t1 >= t2) cycle
         jtau = jtau + 1
         ttab1(jtau) = t1
         !datat(jtau) = dataz(1+(jz-1)*4)
         jt = (t1-t0)/dtsec+1
         data(jt) =  dataz(1+(jz-1)*4)
         jtau = jtau + 1
         ttab1(jtau) = (.75*t1+.25*t2)
         !datat(jtau) = dataz(2+(jz-1)*4)
         jt = (ttab1(jtau)-t0)/dtsec+1
         data(jt) =  dataz(2+(jz-1)*4)
         jtau = jtau + 1
         ttab1(jtau) = (.5*t1+.5*t2)
         !datat(jtau) = dataz(3+(jz-1)*4)
         jt = (ttab1(jtau)-t0)/dtsec+1
         data(jt) =  dataz(3+(jz-1)*4)
         jtau = jtau + 1
         ttab1(jtau) = (.25*t1+.75*t2)
         !datat(jtau) = dataz(4+(jz-1)*4)
         jt = (ttab1(jtau)-t0)/dtsec+1
         data(jt) =  dataz(4+(jz-1)*4)
      end do
      nt1 = jtau
      nt2 = (ttab1(nt1)-t0)/dtsec + 1

      ! NO RESAMPLING OF DATA
      !call ccuint(ttab1,datat,nt1,ttab2,data,nt2,iwrkt,wrkt, initt)

      return
   end subroutine modmo2d_d2tzmo


!!--------------------------- modmo2d_set_parms ------------------------------!!
!!--------------------------- modmo2d_set_parms ------------------------------!!
!!--------------------------- modmo2d_set_parms ------------------------------!!
   subroutine modmo2d_set_parms (obj, ierr)
      implicit none
      type(modmo2d_struct), intent(inout) :: obj       ! arguments
      integer,              intent(inout) :: ierr

      ierr = 0

      call string_to_upper(obj%ani_type)
      select case(obj%ani_type)
         case('ISOTROPIC')
            obj%anisotropic = .false.
         case('VTI')
            obj%anisotropic = .true.
         case default
            obj%ani_type    = 'ISOTROPIC'
            obj%anisotropic = .false.
            call pc_warning('modmo2d: ani_type must be ISOTROPIC or &
                            &VTI; setting ani_type to ISOTROPIC')
      end select

      call string_to_upper(obj%constant_vs_vp)
      select case(obj%constant_vs_vp)
         case('YES')
            obj%constant_vs_vp_l = .true.
         case('NO')
            obj%constant_vs_vp_l = .false.
         case default
            obj%constant_vs_vp   = 'NO'
            obj%constant_vs_vp_l = .false.
            call pc_warning('modmo2d: constant_vs_vp must be YES or NO; &
                            &setting constant_vs_vp to NO')
      end select

      call string_to_upper(obj%time2depth)
      select case(obj%time2depth)
         case('TIME2DEPTH')
            obj%t2d        = .true.
         case('DEPTH2TIME')
            obj%t2d        = .false.
         case default
            obj%time2depth = 'TIME2DEPTH'
            obj%t2d        = .true.
            call pc_warning('modmo2d: time2depth must be TIME2DEPTH or &
                            &DEPTH2TIME; setting time2depth to TIME2DEPTH')
      end select

      call string_to_upper(obj%single_vz)
      select case(obj%single_vz)
         case('YES')
            obj%vzflag    = .true.
         case('NO')
            obj%vzflag    = .false.
         case default
            obj%single_vz = 'NO'
            obj%vzflag    = .false.
            call pc_warning('modmo2d: single_vz must be YES or NO; &
                            &setting single_vz to NO')
      end select

      if (obj%iverbos == 0) then
         obj%verbos = .false.
      else
         obj%verbos = .true.
      end if

      call string_to_upper(obj%dump_vel)
      select case(obj%dump_vel)
         case('YES')
            obj%dump_vel_l = .true.
         case('NO')
            obj%dump_vel_l = .false.
         case default
            obj%dump_vel   = 'NO'
            obj%dump_vel_l = .false.
            call pc_warning('modmo2d: dump_vel must be YES or NO; &
                            &setting dump_vel to NO')
      end select

      call string_to_upper(obj%acq_azimuth)
      select case(obj%acq_azimuth)
         case('ILINE', 'XLINE')
         case default
            obj%acq_azimuth = 'ILINE'
            call pc_warning('modmo2d: acq_azimuth must be ILINE or XLINE; &
                            &setting acq_azimuth to ILINE')
      end select

      call string_to_upper(obj%grid_width_type)
      select case(obj%grid_width_type)
         case('X_GRID_DISTANCE')
            obj%grid_width = grid_get_xgrid_width(obj%grid)
         case('Y_GRID_DISTANCE')
            obj%grid_width = grid_get_ygrid_width(obj%grid)
         case('USER_DEFINED')
            obj%grid_width = obj%grid_width_user
         case default
            obj%grid_width = obj%grid_width_user
            call pc_warning('modmo2d: grid_width_type not valid; &
                            &setting grid_width_type to USER_DEFINED')
      end select

      obj%theta_min = 0.0
      obj%theta_max = abs(obj%ang)
      obj%dtheta    = abs(obj%dang)
      obj%hoffmax = .5 * obj%offmax
      obj%tmax   = obj%nt * obj%delt + obj%t0
      obj%dtmap  = .001
      obj%nzd    = obj%nz
      obj%nzmap  = obj%nzm
      obj%dzmap  = obj%dzm
      obj%nxmap  = abs(obj%hoffmax)/obj%dxmap + 4
      obj%np     = (obj%theta_max-obj%theta_min)/obj%dtheta + 1
      obj%zsrc   = obj%z0m
      obj%noint  = 0
      obj%vp0_xhdr = obj%ilinehdr
      obj%vs0_xhdr = obj%ilinehdr
      obj%eps_xhdr = obj%ilinehdr
      obj%del_xhdr = obj%ilinehdr
      obj%vp0_yhdr = obj%xlinehdr
      obj%vs0_yhdr = obj%xlinehdr
      obj%eps_yhdr = obj%xlinehdr
      obj%del_yhdr = obj%xlinehdr
      return
   end subroutine modmo2d_set_parms


!!--------------------------- modmo2d_alloc ----------------------------------!!
!!--------------------------- modmo2d_alloc ----------------------------------!!
!!--------------------------- modmo2d_alloc ----------------------------------!!
   subroutine modmo2d_alloc (obj, ierr)
      implicit none
      type(modmo2d_struct), intent(inout) :: obj       ! arguments
      integer,              intent(inout) :: ierr

      ierr = 0
      call memman_allocate(obj%tmap, obj%nzmap, obj%nxmap, ierr)
      if (ierr /= 0) return
      call memman_allocate(obj%xr,   obj%nzmap, obj%np,    ierr)
      if (ierr /= 0) return
      call memman_allocate(obj%tr,   obj%nzmap, obj%np,    ierr)
      if (ierr /= 0) return
      call memman_allocate(obj%vp0,  obj%nzm,   2*obj%nxmap+1, ierr, 'vp0')
      if (ierr /= 0) return
      call memman_allocate(obj%cube, obj%nzm,   2*obj%nxmap+1, 1, ierr, 'cube')
      if (ierr /= 0) return
      if (obj%anisotropic) then
        call memman_allocate(obj%vs0,  obj%nzm, 2*obj%nxmap+1, ierr, 'vs0')
        if (ierr /= 0) return
        call memman_allocate(obj%eps,  obj%nzm, 2*obj%nxmap+1, ierr, 'eps')
        if (ierr /= 0) return
        call memman_allocate(obj%del,  obj%nzm, 2*obj%nxmap+1, ierr, 'del')
        if (ierr /= 0) return
      end if
      return
   end subroutine modmo2d_alloc


!!--------------------------- modmo2d_dealloc --------------------------------!!
!!--------------------------- modmo2d_dealloc --------------------------------!!
!!--------------------------- modmo2d_dealloc --------------------------------!!
   subroutine modmo2d_dealloc (obj, ierr)
      implicit none
      type(modmo2d_struct), intent(inout) :: obj       ! arguments
      integer,              intent(inout) :: ierr

      ierr = 0
      call memman_deallocate(obj%tmap)
      call memman_deallocate(obj%xr)
      call memman_deallocate(obj%tr)
      call memman_deallocate(obj%vp0)
      call memman_deallocate(obj%vs0)
      call memman_deallocate(obj%eps)
      call memman_deallocate(obj%del)
      call memman_deallocate(obj%cube)
      return
   end subroutine modmo2d_dealloc


!!--------------------------- modmo2d_vel_init -------------------------------!!
!!--------------------------- modmo2d_vel_init -------------------------------!!
!!--------------------------- modmo2d_vel_init -------------------------------!!
   subroutine modmo2d_vel_init(obj, mgobj, prop_file, desc, dfile, wtype,   &
                               ftype, xhdr, yhdr, xgridwidth, ygridwidth,   &
                               ix, iy, iz, hdwd, ng, og, dg, ierr)
      implicit none

      ! Subroutine arguments
      type(modmo2d_struct),              intent(inout) :: obj
      type(modgrid_struct),              pointer       :: mgobj
      character(len=*),                  intent(in)    :: prop_file
      character(len=8),                  intent(inout) :: desc
      character(len=FILENAME_LENGTH),    intent(inout) :: dfile
      character(len=8),                  intent(inout) :: wtype
      character(len=8),                  intent(inout) :: ftype
      integer,                           intent(inout) :: xhdr
      integer,                           intent(inout) :: yhdr
      real,                              intent(inout) :: xgridwidth
      real,                              intent(inout) :: ygridwidth
      integer,                           intent(out)   :: ix, iy, iz
      integer, dimension(modmo2d_mgdim), intent(inout) :: hdwd
      integer, dimension(modmo2d_mgdim), intent(inout) :: ng
      real,    dimension(modmo2d_mgdim), intent(inout) :: og
      real,    dimension(modmo2d_mgdim), intent(inout) :: dg
      integer,                           intent(inout) :: ierr

      ! Local variables
      character(len=128) :: str
      character(len=64)  :: mg_name
      character(len=16)  :: mg_pname
      character(len=16)  :: mg_punits
      integer            :: mg_rank

      ierr = 0
      ierr=modgrid_rddesc(mgobj, prop_file, lunprint, &
                          dfile, wtype, ftype, xhdr, yhdr)
      if (ierr /= 0) then
         write(str,*)'modmo2d: error in modgrid_rddesc for ', desc, ierr
         call pc_error(trim(str))
         return
      end if
      write(lunprint,*)'***** begin modgrid info for ', desc, ' *****'
      call modgrid_print(mgobj, lunprint)
      call modgrid_get_griddesc(mgobj, mg_name, mg_pname, &
                                mg_punits, mg_rank, hdwd, &
                                ng, og, dg)
      ierr = modgrid_xyz_order(mgobj, ix, iy, iz)
      if (ierr /= 0) then
         write(str,*) 'modmo2d: error in modgrid_xyz_order', ierr
         call pc_error(trim(str))
         return
      end if
      write(lunprint,*) 'XYZ order = ', ix, iy, iz
      if (iz /= 1) then
         write(str,*) 'modmo2d: z must be fastest dimension!', ierr
         call pc_error(trim(str))
         return
      end if
      if (ftype == 'SEGY') then
         if (dg(iz) /= obj%dz) then
            write(lunprint,*)
            write(lunprint,*)'Input ', desc, ' filetype = SEGY; &
                             &resetting sampling interval to match output dz'
            write(lunprint,*)'original sampling interval = ', dg(iz)
            call modgrid_set_griddesc(mgobj, 1, -1, ng(iz), og(iz), obj%dz)
            call modgrid_get_griddesc(mgobj, mg_name, mg_pname, &
                                      mg_punits, mg_rank, hdwd, &
                                      ng, og, dg)
            write(lunprint,*)'updated  sampling interval = ', dg(iz)
            write(lunprint,*)
         end if
      end if
      if (obj%z0m < og(iz)) then
         ierr = -1
         write(str,*)'modmo2d: model z origin (z0m=', obj%z0m, ') is less than &
                     &input velocity model z origin (og=', og(iz), ')!'
         call pc_error(trim(str))
         return
      end if
      if (ftype == 'MODSPEC') then
         ! do nothing
      else if ((obj%z0m+obj%nzm*obj%dzm) > (og(iz)+ng(iz)*dg(iz))) then
         ierr = -1
         write(str,*)'modmo2d: model z extent (', obj%z0m+obj%nzm*obj%dzm, &
                     ') is greater than input velocity model extent (',    &
                     og(iz)+ng(iz)*dg(iz), ')!'
         call pc_error(trim(str))
         return
      end if

      if (xhdr == 7) then
         xgridwidth = obj%grid_width
      else
         xgridwidth = 1.0
      end if
      if (yhdr == 8) then
         ygridwidth = obj%grid_width
      else
         ygridwidth = 1.0
      end if
      write(lunprint,*)'xgridwidth = ', xgridwidth
      write(lunprint,*)'ygridwidth = ', ygridwidth
      write(lunprint,*)'***** end   modgrid info for ', desc, ' *****'

      return
   end subroutine modmo2d_vel_init


!!--------------------------- modmo2d_vel_read_slice -------------------------!!
!!--------------------------- modmo2d_vel_read_slice -------------------------!!
!!--------------------------- modmo2d_vel_read_slice -------------------------!!
   subroutine modmo2d_vel_read_slice(obj, mgobj, desc, ix, iy, iz, hdwd, ng, &
                                     og, dg, data, ycmp, ierr)

      implicit none

      ! Subroutine arguments
      type(modmo2d_struct),              intent(in)    :: obj
      type(modgrid_struct),              pointer       :: mgobj
      character(len=8),                  intent(inout) :: desc
      integer,                           intent(in)    :: ix, iy, iz
      integer, dimension(modmo2d_mgdim), intent(inout) :: hdwd
      integer, dimension(modmo2d_mgdim), intent(inout) :: ng
      real,    dimension(modmo2d_mgdim), intent(inout) :: og
      real,    dimension(modmo2d_mgdim), intent(inout) :: dg
      real,                              pointer       :: data(:)
      real,                              intent(in)    :: ycmp
      integer,                           intent(inout) :: ierr

      ! Local variables
      character(len=128) :: str
      integer            :: i
      integer, dimension(modmo2d_mgdim) :: hdwd_loc
      integer, dimension(modmo2d_mgdim) :: ng_loc
      real,    dimension(modmo2d_mgdim) :: og_loc
      real,    dimension(modmo2d_mgdim) :: dg_loc
      real,    dimension(modmo2d_mgdim) :: axs, axe

      ierr = 0

      ! Read in velocity function and create travel time map
      axs(ix) = og(ix)
      axe(ix) = og(ix) + ng(ix)*dg(ix)
      axs(iy) = ycmp
      axe(iy) = ycmp
      axs(iz) = og(iz)
      axe(iz) = og(iz) + ng(iz)*dg(iz)
      ierr = modgrid_rd_data(mgobj, axs, axe)
      if (ierr /= 0) then
         write(str,*)'modmo2d: error reading ', desc, ' file'
         call pc_error(trim(str))
         return
      end if
      ierr = modgrid_get_data(mgobj, data, hdwd_loc, ng_loc, og_loc, dg_loc)
      if (ierr < 0) then
         write(str,*) 'modmo2d: error getting ', desc, ' data'
         call pc_error(trim(str))
         return
      else
         ierr = 0
      end if

      !write(lunprint,*) 'i   og_loc   ng_loc   dg_loc   hdwd_loc (', desc, ')'
      !do i=1,modmo2d_mgdim
      !   write(lunprint,*) i, og_loc(i), ng_loc(i), dg_loc(i), hdwd_loc(i)
      !end do
      !write(lunprint,*) 'i   og       ng       dg       hdwd     (', desc, ')'
      !do i=1,modmo2d_mgdim
      !   write(lunprint,*) i, og(i), ng(i), dg(i), hdwd(i)
      !end do
      !write(lunprint,*) 'for i=', iy, 'ng=', 1

      ! Check og
      if (og_loc(ix) /= og(ix)) then
         write(str,*) 'modmo2d: inconsistency in results from &
                      &modgrid_get_data: og_loc, og = ',      &
                      og_loc(ix), og(ix), ix, desc
         call pc_error(trim(str))
         return
      end if
      if (og_loc(iy) /= ycmp) then
         write(str,*) 'modmo2d: inconsistency in results from &
                      &modgrid_get_data: og_loc, og = ',      &
                      og_loc(iy), ycmp, iy, desc
         call pc_error(trim(str))
         return
      end if
      if (og_loc(iz) /= og(iz)) then
         write(str,*) 'modmo2d: inconsistency in results from &
                      &modgrid_get_data: og_loc, og = ',      &
                      og_loc(iz), og(iz), iz, desc
         call pc_error(trim(str))
         return
      end if
      ! Check dg
      do i=1,modmo2d_mgdim
         if (dg_loc(i) /= dg(i)) then
            write(str,*) 'modmo2d: inconsistency in results from &
                         &modgrid_get_data: dg_loc, dg = ',      &
                         dg_loc(i), dg(i), i, desc
            call pc_error(trim(str))
            return
         end if
      end do
      ! Check hdwd
      do i=1,modmo2d_mgdim
         if (hdwd_loc(i) /= hdwd(i)) then
            write(str,*) 'modmo2d: inconsistency in results from &
                         &modgrid_get_data: hdwd_loc, hdwd = ',  &
                         hdwd_loc(i), hdwd(i), i, desc
            call pc_error(trim(str))
            return
         end if
      end do
      ! Check ng
      if (ng_loc(iz) /= ng(iz)) then
         write(str,*) 'modmo2d: inconsistency in results from &
                      &modgrid_get_data: ng_loc, ng = ',      &
                      ng_loc(iz), ng(iz), iz, desc
         call pc_error(trim(str))
         return
      end if
      if (ng_loc(ix) /= ng(ix)) then
         write(str,*) 'modmo2d: inconsistency in results from &
                      &modgrid_get_data: ng_loc, ng = ',      &
                      ng_loc(ix), ng(ix), iy, desc
         call pc_error(trim(str))
         return
      end if
      if (ng_loc(iy) /= 1) then
         write(str,*) 'modmo2d: inconsistency in results from &
                      &modgrid_get_data: ng_loc, ng = ',      &
                      ng_loc(iy), 1, iy, desc
         call pc_error(trim(str))
         return
      end if

      return
   end subroutine modmo2d_vel_read_slice


!!--------------------------- modmo2d_vel_read_pencil ------------------------!!
!!--------------------------- modmo2d_vel_read_pencil ------------------------!!
!!--------------------------- modmo2d_vel_read_pencil ------------------------!!
   subroutine modmo2d_vel_read_pencil(obj, desc, ix, iy, iz, ng, og, dg, &
                                      data, xcmp, pencil, ierr)

      implicit none

      ! Subroutine arguments
      type(modmo2d_struct),              intent(in)    :: obj
      character(len=8),                  intent(in)    :: desc
      integer,                           intent(in)    :: ix, iy, iz
      integer, dimension(modmo2d_mgdim), intent(in)    :: ng
      real,    dimension(modmo2d_mgdim), intent(in)    :: og
      real,    dimension(modmo2d_mgdim), intent(in)    :: dg
      real,                              pointer       :: data(:)
      real,                              intent(in)    :: xcmp
      real,                              intent(out)   :: pencil(:)
      integer,                           intent(inout) :: ierr

      ! Local variables

      integer            :: ix_use, iptr
      real               :: xcmp_use, xmin, xmax

      ierr = 0

      !if (ng(iz) /= obj%nz) then
      !   ierr = -1
      !   write(str,*)'modmo2d: vel_read_pencil: vel data nz != output nz &
      !               &for ', desc, ' pencil'
      !   call pc_error(trim(str))
      !   return
      !end if

      ! Find nearest pencil to xcmp
      xmin = og(ix)
      xmax = og(ix) + ng(ix)*dg(ix)
      if (xcmp < xmin) then
         call pc_warning('modmo2d: vel_read_pencil: xcmp < ', desc, &
                         ' minimum xcmp; using function at minimum xcmp')
         xcmp_use = xmin
      else if (xcmp > xmax) then
         call pc_warning('modmo2d: vel_read_pencil: xcmp > ', desc, &
                         ' maximum xcmp; using function at maximum xcmp')
         xcmp_use = xmax
      else
         xcmp_use = xcmp
      end if
      !ix_use = nint(xcmp-og(ix))/dg(ix) + 1
      ix_use = mth_bin_number(og(ix), dg(ix), xcmp_use)
      iptr = (ix_use-1)*ng(iz)
      !pencil(1:obj%nz) = data(iptr+1:iptr+obj%nz)
      if ((og(iz) == obj%z0m) .and. &
          (ng(iz) == obj%nzm) .and. &
          (dg(iz) == obj%dzm)) then
         pencil(1:obj%nzm) = data(iptr+1:iptr+obj%nzm)
      else
         call interpolate_1d_to_1d(ng(iz), og(iz), dg(iz),          &
                                   data(iptr+1:iptr+ng(iz)),        &
                                   obj%nzm, obj%z0m, obj%dzm, pencil)
      end if

      return
   end subroutine modmo2d_vel_read_pencil


!!--------------------------- modmo2d_vel_paint_pencil -----------------------!!
!!--------------------------- modmo2d_vel_paint_pencil -----------------------!!
!!--------------------------- modmo2d_vel_paint_pencil -----------------------!!
   subroutine modmo2d_vel_paint_pencil(mgobj, desc, ix, iy, iz, xhdr, yhdr,   &
                                       ng, og, dg, xcmp, ycmp, nzm, z0m, dzm, &
                                       pencil, cube, ierr)

      implicit none

      ! Subroutine arguments
      type(modgrid_struct),              pointer       :: mgobj
      character(len=8),                  intent(in)    :: desc
      integer,                           intent(in)    :: ix, iy, iz
      integer,                           intent(in)    :: xhdr, yhdr
      integer, dimension(modmo2d_mgdim), intent(in)    :: ng
      real,    dimension(modmo2d_mgdim), intent(in)    :: og
      real,    dimension(modmo2d_mgdim), intent(in)    :: dg
      real,                              intent(in)    :: xcmp, ycmp
      integer,                           intent(in)    :: nzm
      real,                              intent(in)    :: z0m, dzm
      real,                              intent(out)   :: pencil(:)
      real,                              intent(out)   :: cube(:,:,:)
      integer,                           intent(inout) :: ierr

      ! Local variables
      character(len=128) :: str
      character(len=8)   :: ftype
      character(len=3)   :: out_xyz
      integer            :: maxmem
      real               :: xmin, xmax, ymin, ymax

      ierr = 0

      ! Check to see if "cmp" location is within model; if not, print warning
      xmin = min(og(ix), og(ix)+ng(ix)*dg(ix))
      xmax = max(og(ix), og(ix)+ng(ix)*dg(ix))
      ymin = min(og(iy), og(iy)+ng(iy)*dg(iy))
      ymax = max(og(iy), og(iy)+ng(iy)*dg(iy))
      if ((xcmp > xmax).or.(xcmp < xmin).or. &
          (ycmp > ymax).or.(ycmp < ymin)) then
         call pc_warning("cmp location is outside model: x = ", xcmp, &
                         ", y = ", ycmp)
      end if

      ftype = ' '
      out_xyz(1:1) = 'Z'
      out_xyz(2:2) = 'X'
      out_xyz(3:3) = 'Y'
      maxmem = 2*max(ng(1),nzm)*ng(2)
      !ierr = modgrid_paint_by_obj(mgobj, maxmem, lunprint, 'V', 'S',     &
      ierr = modgrid_paint_by_obj(mgobj, maxmem, lunprint, 'V', 'V',     &
                                  xhdr, yhdr, nzm, z0m, dzm,             &
                                  1, xcmp, dg(ix), 1, ycmp, dg(iy),      &
                                  cube, out_xyz, ftype)
      if (ierr /= 0) then
         write(str,*) 'modmo2d: vel_paint_pencil: error in &
                      &modgrid_paint_by_obj for ', desc, ' = ', ierr
         call pc_error(trim(str))
      end if
      pencil(1:nzm) = cube(1:nzm,1,1)

      return
   end subroutine modmo2d_vel_paint_pencil


!!--------------------------- modmo2d_vel_paint_slice ------------------------!!
!!--------------------------- modmo2d_vel_paint_slice ------------------------!!
!!--------------------------- modmo2d_vel_paint_slice ------------------------!!
   subroutine modmo2d_vel_paint_slice(mgobj, desc, ix, iy, iz, xhdr, yhdr,   &
                                      xgridwidth, ygridwidth, nxmap, dxmap,  &
                                      acq_azimuth,                           &
                                      ng, og, dg, xcmp, ycmp, nzm, z0m, dzm, &
                                      slice, cube, ierr)

      implicit none

      ! Subroutine arguments
      type(modgrid_struct),             pointer       :: mgobj
      character(len=8),                 intent(in)    :: desc
      integer,                          intent(in)    :: ix, iy, iz
      integer,                          intent(in)    :: xhdr, yhdr
      real,                             intent(in)    :: xgridwidth, ygridwidth
      integer,                          intent(in)    :: nxmap
      real,                             intent(in)    :: dxmap
      character(len=*),                 intent(in)    :: acq_azimuth
      integer, dimension(modmo2d_mgdim),intent(in)    :: ng
      real,    dimension(modmo2d_mgdim),intent(in)    :: og
      real,    dimension(modmo2d_mgdim),intent(in)    :: dg
      real,                             intent(in)    :: xcmp, ycmp
      integer,                          intent(in)    :: nzm
      real,                             intent(in)    :: z0m, dzm
      real,                             intent(out)   :: slice(:,:)
      real,                             intent(out)   :: cube(:,:,:)
      integer,                          intent(inout) :: ierr

      ! Local variables
      character(len=128) :: str
      character(len=8)   :: ftype
      character(len=3)   :: out_xyz
      integer            :: maxmem
      real               :: xmin, xmax, ymin, ymax
      integer            :: n2, n3
      real               :: o2, d2, o3, d3
!REMOVE ME

!REMOVE ME

      ierr = 0

      ! Check to see if "cmp" location is within model; if not, print warning
      xmin = min(og(ix), og(ix)+ng(ix)*dg(ix))
      xmax = max(og(ix), og(ix)+ng(ix)*dg(ix))
      ymin = min(og(iy), og(iy)+ng(iy)*dg(iy))
      ymax = max(og(iy), og(iy)+ng(iy)*dg(iy))
      if ((xcmp > xmax).or.(xcmp < xmin).or. &
          (ycmp > ymax).or.(ycmp < ymin)) then
         call pc_warning("cmp location is outside model: x = ", xcmp, &
                         ", y = ", ycmp)
      end if

      ftype = ' '
      out_xyz(1:1) = 'Z'
      select case(acq_azimuth)
         case('XLINE')
            out_xyz(2:2) = 'Y'
            out_xyz(3:3) = 'X'
            n2 = 2*nxmap+1
            d2 = dxmap/ygridwidth
            o2 = ycmp - d2*nxmap
            n3 = 1
            d3 = dg(ix)
            o3 = xcmp
         case default ! ILINE
            out_xyz(2:2) = 'X'
            out_xyz(3:3) = 'Y'
            n2 = 2*nxmap+1
            d2 = dxmap/xgridwidth
            o2 = xcmp - d2*nxmap
            n3 = 1
            d3 = dg(iy)
            o3 = ycmp
      end select
      maxmem = 2 * max(ng(1),nzm) * max(ng(2),2*nxmap+1)
      !ierr = modgrid_paint_by_obj(mgobj, maxmem, lunprint, 'V', 'S',     &
      ierr = modgrid_paint_by_obj(mgobj, maxmem, lunprint, 'V', 'V',     &
                                  xhdr, yhdr, nzm, z0m, dzm,             &
                                  n2, o2, d2, n3, o3, d3,                &
                                  cube, out_xyz, ftype)
      if (ierr /= 0) then
         write(str,*) 'modmo2d: vel_paint_slice: error in modgrid_paint_by_obj &
                      & for ', desc, ' = ', ierr
         call pc_error(trim(str))
      end if
      slice(1:nzm,1:(2*nxmap+1)) = cube(1:nzm,1:(2*nxmap+1),1)

      return
   end subroutine modmo2d_vel_paint_slice


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


   subroutine modmo2d_wrapup (obj)
      type(modmo2d_struct), intent(inout) :: obj       ! arguments

      ! Local variables
      integer :: ierr

      ierr = 0
      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      ! Insert any required wrapup code here, including wrapups of
      ! internally-called processes.
      if (obj%dump_vel_l) then
         ierr = trcio_close(obj%vp0_trcio)
         if (ierr /= TRCIO_OK) &
            call pc_error('modmo2d: vp0 dump: error closing dump file')
      end if

      call timer_report(modmo2d_timers(MODMO2D_TIMER_MAIN), &
                        'main', lunprint)
      call timer_free(modmo2d_timers(MODMO2D_TIMER_MAIN), ierr)
      if (ierr /= 0) call pc_error('modmo2d_wrapup: timer_free failed')

      call timer_report(modmo2d_timers(MODMO2D_TIMER_DO_GATHER), &
                        'do gather', lunprint)
      call timer_free(modmo2d_timers(MODMO2D_TIMER_DO_GATHER), ierr)
      if (ierr /= 0) call pc_error('modmo2d_wrapup: timer_free failed')

      call timer_report(modmo2d_timers(MODMO2D_TIMER_RT), &
                        'tmap 2d cerveny up', lunprint)
      call timer_free(modmo2d_timers(MODMO2D_TIMER_RT), ierr)
      if (ierr /= 0) call pc_error('modmo2d_wrapup: timer_free failed')

      call timer_report(modmo2d_timers(MODMO2D_TIMER_TD), &
                        'time-to-depth', lunprint)
      call timer_free(modmo2d_timers(MODMO2D_TIMER_TD), ierr)
      if (ierr /= 0) call pc_error('modmo2d_wrapup: timer_free failed')

      call timer_report(modmo2d_timers(MODMO2D_TIMER_DT), &
                        'depth-to-time', lunprint)
      call timer_free(modmo2d_timers(MODMO2D_TIMER_DT), ierr)
      if (ierr /= 0) call pc_error('modmo2d_wrapup: timer_free failed')

   end subroutine modmo2d_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module modmo2d_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

