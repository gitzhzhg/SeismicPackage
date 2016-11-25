!<CPS_v1 type="PROCESS"/>
!!------------------------------- modmo.f90 ----------------------------------!!
!!------------------------------- modmo.f90 ----------------------------------!!
!!------------------------------- modmo.f90 ----------------------------------!!


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
! Name       : MODMO
! Category   : Transforms
! Written    : 2003-05-01   by: bkmacy
! Revised    : 2007-12-18   by: Brian Macy
! Maturity   : beta
! Purpose    : Time to depth model-based moveout.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! MODMO converts time-domain common image gathers with moveout to the
! depth domain using model-based moveout.  Alternatively, depth-domain
! gathers without moveout can be converted to the time domain with
! moveout.  The model-based approach uses a locally 1-D velocity model.
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
! The input data must be gathered into common image gathers prior to MODMO.
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
! GRID      grid transformation structure           Not used or changed.
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
!019. 2007-12-18  Brian Macy Fixed headers for velocity dump file.
!018. 2006-06-27  D. Glover  Added NULLIFY statements for Intel compiler.
!017. 2006-01-10  B. Menger  Removed Unused Variables.
! 16. 2005-01-31  bkmacy     Changed DUMP_VEL flag to be YES or NO instead of 1
!                              or 0, and to allow the user to specify the name
!                              of the output file (VP0_DUMPFILE).
!                            Introduced the use of timer routines and reporting.
! 15. 2004-07-13  bkmacy     Added trap call when pushbutton for selecting a
!                            file is used, which will properly update the
!                            file status.
! 14. 2004-05-25  bkmacy     Fixed vs0 trap when constant Vp/Vs is true.
! 13. 2004-02-18  bkmacy     Fixed bug in calculating maxmem for
!                            modgrid_paint_by_obj in routine vel_paint_pencil.
! 12. 2004-02-17  bkmacy     Changed DUMP_VEL to write out CPS TRCIO files
!                            rather than plain binary data.
! 11. 2004-01-08  bkmacy     Improved DEPTH_SCALE documentation.
! 10. 2004-01-06  bkmacy     Added DEPTH_SCALE option for scaling the depth
!                            interval and starting depth for input or output
!                            seismic data.
!                            Added a bounds check in modmo_vzmapr_vti.
!                            Changed order of variable declarations to satisfy
!                            Solaris 7.0 compiler.
!  9. 2003-10-06  rsday      Changed ***_fsize parameters to double precision
!  8. 2003-08-28  bkmacy     Added check for dumpvel flag when closing dumpvel
!                            files.
!  7. 2003-08-19  bkmacy     Changed name from TDRBMO to MODMO.  Promoted
!                            to betalib.
!                            Added DUMP_VEL option to write velocities used
!                            internally to a file.
!                            A warning is printed if the gather location is
!                            outside of the velocity model.
!  6. 2003-08-05  bkmacy     Changed default ilinehdr and xlinehdr from 17 and
!                            18 to 7 and 8.
!                            Changed default extension for velocity files from
!                            'segy' to '*'.
!                            Improved GUI behavior for Vs parameters for
!                            isotropic case.
!                            Removed requirement to enter Vs, epsilon, and
!                            delta filenames for isotropic case.
!  5. 2003-07-24  bkmacy     Added option for choosing a constant Vs/Vp
!                            velocity ratio.
!                            Changed from using vel_read_slice and
!                            vel_read_pencil to using new routine
!                            vel_paint_pencil, which uses the new modgrid
!                            paint functions.
!                            Fixed possible bug where nzm should have been used
!                            intead of nz when allocating velocity arrays.
!                            Modified check for z range in vel_init to
!                            accomodate modspec files.
!  4. 2003-07-17  bkmacy     Added support for parallel operation.
!                            Fixed bug in error checking for vel_read_slice.
!                            Commented out debugging statements.
!  3. 2003-07-07  bkmacy     Supports non-zero acquisition datums using the
!                            "model" z parameters.  Velocity model reading
!                            can now use in-line and cross-line grid values.
!                            The input velocity model can be isotropic,
!                            rather than just VTI.  The velocity model is
!                            interpolated in depth if the "file" z parameters
!                            do not match the "model" z parameters.
!  2. 2003-05-09  bkmacy     Added capability to convert from depth to time.
!  1. 2003-05-01  bkmacy     Initial version.
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
!<NS MODMO>
!                     [/C]MODMO (MODel-based Move-Out)
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
! ilinehdr=~~~~~`IIII
! xlinehdr=~~~~~`IIII
! iverbos=~~~~~~`IIII
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
! XXXX is the worker number. The output format is CPS TRCIO.  The header words
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
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module modmo_module
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

   implicit none

   private

   public :: modmo_create
   public :: modmo_initialize
   public :: modmo_update
   public :: modmo_delete
   public :: modmo            ! main trace processing routine.
   public :: modmo_wrapup

   character(len=100),public,save :: MODMO_IDENT = &
'$Id: modmo.f90,v 1.19 2007/12/20 15:08:58 Macy beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


   integer, parameter                 :: modmo_mgdim = 3
   integer, parameter, private        :: MODMO_TIMER_MAIN      = 1
   integer, parameter, private        :: MODMO_TIMER_DO_GATHER = 2
   integer, parameter, private        :: MODMO_TIMER_RT        = 3
   integer, parameter, private        :: MODMO_TIMER_TD        = 4
   integer, parameter, private        :: MODMO_TIMER_DT        = 5
   integer,            private        :: modmo_timers(5)

   type,public :: modmo_struct

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

     real                             :: theta_max
     real                             :: dtheta
     real                             :: tmax
     real                             :: hoffmax
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
     real, pointer                    :: vp0(:)
     !type(modmo_pprop_struct)         :: vp0obj
     type(modgrid_struct),pointer     :: vp0_obj
     character(len=8)                 :: vp0_desc
     character(len=FILENAME_LENGTH)   :: vp0_dfile
     character(len=8)                 :: vp0_wtype
     character(len=8)                 :: vp0_ftype
     double precision                 :: vp0_fsize
     integer                          :: vp0_xhdr
     integer                          :: vp0_yhdr
     integer                          :: vp0_ix
     integer                          :: vp0_iy
     integer                          :: vp0_iz
     type(trcio_struct),pointer       :: vp0_trcio
     integer, dimension(modmo_mgdim)  :: vp0_hdwd
     integer, dimension(modmo_mgdim)  :: vp0_ng
     real,    dimension(modmo_mgdim)  :: vp0_og
     real,    dimension(modmo_mgdim)  :: vp0_dg
     real, pointer                    :: vp0_data(:)
     real, pointer                    :: vs0(:)
     !type(modmo_pprop_struct)         :: vs0obj
     type(modgrid_struct),pointer     :: vs0_obj
     character(len=8)                 :: vs0_desc
     character(len=FILENAME_LENGTH)   :: vs0_dfile
     character(len=8)                 :: vs0_wtype
     character(len=8)                 :: vs0_ftype
     double precision                 :: vs0_fsize
     integer                          :: vs0_xhdr
     integer                          :: vs0_yhdr
     integer                          :: vs0_ix
     integer                          :: vs0_iy
     integer                          :: vs0_iz
     integer, dimension(modmo_mgdim)  :: vs0_hdwd
     integer, dimension(modmo_mgdim)  :: vs0_ng
     real,    dimension(modmo_mgdim)  :: vs0_og
     real,    dimension(modmo_mgdim)  :: vs0_dg
     real, pointer                    :: vs0_data(:)
     real, pointer                    :: eps(:)
     !type(modmo_pprop_struct)         :: epsobj
     type(modgrid_struct),pointer     :: eps_obj
     character(len=8)                 :: eps_desc
     character(len=FILENAME_LENGTH)   :: eps_dfile
     character(len=8)                 :: eps_wtype
     character(len=8)                 :: eps_ftype
     double precision                 :: eps_fsize
     integer                          :: eps_xhdr
     integer                          :: eps_yhdr
     integer                          :: eps_ix
     integer                          :: eps_iy
     integer                          :: eps_iz
     integer, dimension(modmo_mgdim)  :: eps_hdwd
     integer, dimension(modmo_mgdim)  :: eps_ng
     real,    dimension(modmo_mgdim)  :: eps_og
     real,    dimension(modmo_mgdim)  :: eps_dg
     real, pointer                    :: eps_data(:)
     real, pointer                    :: del(:)
     !type(modmo_pprop_struct)         :: delobj
     type(modgrid_struct),pointer     :: del_obj
     character(len=8)                 :: del_desc
     character(len=FILENAME_LENGTH)   :: del_dfile
     character(len=8)                 :: del_wtype
     character(len=8)                 :: del_ftype
     double precision                 :: del_fsize
     integer                          :: del_xhdr
     integer                          :: del_yhdr
     integer                          :: del_ix
     integer                          :: del_iy
     integer                          :: del_iz
     integer, dimension(modmo_mgdim)  :: del_hdwd
     integer, dimension(modmo_mgdim)  :: del_ng
     real,    dimension(modmo_mgdim)  :: del_og
     real,    dimension(modmo_mgdim)  :: del_dg
     real, pointer                    :: del_data(:)
   end type modmo_struct

   ! Data object for a physical property (velocity, anisotropy parameter, etc.)
   ! This type is not used due to memory problems -- needs further
   ! investigation.
   type,public :: modmo_pprop_struct
     private
     type(modgrid_struct),pointer     :: mgobj
     character(len=8)                 :: desc
     character(len=FILENAME_LENGTH)   :: dfile
     character(len=8)                 :: wtype
     character(len=8)                 :: ftype
     integer                          :: fsize
     integer                          :: xhdr
     integer                          :: yhdr
     integer, dimension(modmo_mgdim)  :: hdwd
     integer, dimension(modmo_mgdim)  :: ng
     real,    dimension(modmo_mgdim)  :: og
     real,    dimension(modmo_mgdim)  :: dg
     real, pointer                    :: data(:)
   end type modmo_pprop_struct



!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

   integer                  ,save     :: lunprint  ! unit number for printing.
   type(modmo_struct),pointer,save    :: object    ! needed for traps.
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
   integer, parameter                 :: num_opt_dump_vel = 2
   character(len=3), parameter        :: opt_dump_vel(num_opt_dump_vel) &
                                            = (/'NO ', 'YES'/)


   contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


   subroutine modmo_create (obj)
      type(modmo_struct),pointer :: obj       ! arguments
      integer                    :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in modmo_create")

      nullify(obj%vp0_obj)
      nullify(obj%vp0_data)
      nullify(obj%vs0_obj)
      nullify(obj%vs0_data)
      nullify(obj%eps_obj)
      nullify(obj%eps_data)
      nullify(obj%del_obj)
      nullify(obj%del_data)
      nullify (obj%tmap) ! jpa
      nullify (obj%xr) ! jpa
      nullify (obj%tr) ! jpa
      nullify (obj%vp0) ! jpa
      nullify (obj%vs0) ! jpa
      nullify (obj%eps) ! jpa
      nullify (obj%del) ! jpa
      nullify (obj%cube) ! jpa
      nullify (obj%vp0_trcio) ! jpa
      nullify (obj%vp0_file_choose) ! jpa
      nullify (obj%vs0_file_choose) ! jpa
      nullify (obj%eps_file_choose) ! jpa
      nullify (obj%del_file_choose) ! jpa
      nullify (obj%vp0_dumpfile_choose) ! jpa

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

      call modmo_initialize (obj)
   end subroutine modmo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


   subroutine modmo_delete (obj)
      type(modmo_struct),pointer  :: obj       ! arguments

      ! Local variables
      integer                     :: ierr      ! for error checking

      ierr = 0

      call modmo_wrapup (obj)

      call modmo_dealloc(obj, ierr)
      if (ierr /= 0) &
         call pc_warning ("error deallocating obj members in modmo_delete")

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
      if (ierr /= 0) call pc_warning ("error deallocating obj in modmo_delete")
   end subroutine modmo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


   subroutine modmo_initialize (obj)
      type(modmo_struct), intent(inout) :: obj       ! arguments

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
      obj%dump_vel     = 'NO'
      obj%dump_vel_l   = .false.
      obj%vp0_dumpfile = PATHCHECK_EMPTY

      obj%vp0_desc  = 'vp0'
      obj%vp0_dfile = ' '
      obj%vp0_wtype = ' '
      obj%vp0_ftype = ' '
      obj%vp0_fsize = 0
      obj%vp0_xhdr  = obj%ilinehdr
      obj%vp0_yhdr  = obj%xlinehdr

      obj%vs0_desc  = 'vs0'
      obj%vs0_dfile = ' '
      obj%vs0_wtype = ' '
      obj%vs0_ftype = ' '
      obj%vs0_fsize = 0
      obj%vs0_xhdr  = obj%ilinehdr
      obj%vs0_yhdr  = obj%xlinehdr

      obj%eps_desc  = 'eps'
      obj%eps_dfile = ' '
      obj%eps_wtype = ' '
      obj%eps_ftype = ' '
      obj%eps_fsize = 0
      obj%eps_xhdr  = obj%ilinehdr
      obj%eps_yhdr  = obj%xlinehdr

      obj%del_desc  = 'del'
      obj%del_dfile = ' '
      obj%del_wtype = ' '
      obj%del_ftype = ' '
      obj%del_fsize = 0
      obj%del_xhdr  = obj%ilinehdr
      obj%del_yhdr  = obj%xlinehdr

      call modmo_update (obj)
   end subroutine modmo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


   subroutine modmo_update (obj)
      type(modmo_struct), intent(inout),target :: obj             ! arguments

      ! Local variables
      integer :: ierr
      character(len=FILENAME_LENGTH) :: filename

      ierr = 0
      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      if (pathchoose_update(obj%vp0_file_choose, obj%vp0_file, &
                            modmo_vp0_file)) return
      if (pathchoose_update(obj%vs0_file_choose, obj%vs0_file, &
                            modmo_vs0_file)) return
      if (pathchoose_update(obj%eps_file_choose, obj%eps_file, &
                            modmo_eps_file)) return
      if (pathchoose_update(obj%del_file_choose, obj%del_file, &
                            modmo_del_file)) return
      if (pathchoose_update(obj%vp0_dumpfile_choose, obj%vp0_dumpfile, &
                            modmo_vp0_dumpfile)) return

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

      call pc_get('ANI_TYPE',   obj%ani_type,   modmo_ani_type)
      call pc_get('TIME2DEPTH', obj%time2depth, modmo_time2depth)
      call pc_get('ANG',        obj%ang,        modmo_ang)
      call pc_get('DANG',       obj%dang,       modmo_dang)
      call pc_get('DXMAP',      obj%dxmap,      modmo_dxmap)
      call pc_get('DEPTH_SCALE',obj%depth_scale,modmo_depth_scale)
      call pc_get('NZ',         obj%nz,         modmo_nz)
      call pc_get('DZ',         obj%dz,         modmo_dz)
      call pc_get('Z0',         obj%z0,         modmo_z0)
      call pc_get('NZM',        obj%nzm,        modmo_nzm)
      call pc_get('DZM',        obj%dzm,        modmo_dzm)
      call pc_get('Z0M',        obj%z0m,        modmo_z0m)
      call pc_get('NT',         obj%nt,         modmo_nt)
      call pc_get('DELT',       obj%delt,       modmo_delt)
      call pc_get('T0',         obj%t0,         modmo_t0)
      call pc_get('ISMOO ',     obj%ismoo,      modmo_ismoo)
      call pc_get('VP0_FILE',   obj%vp0_file,   modmo_vp0_file)
      call pc_get('CONSTANT_VS_VP', obj%constant_vs_vp, &
                                                modmo_constant_vs_vp)
      call pc_get('VS_VP_RATIO', obj%vs_vp_ratio, modmo_vs_vp_ratio)
      call pc_get('VS0_FILE',   obj%vs0_file,   modmo_vs0_file)
      call pc_get('EPS_FILE',   obj%eps_file,   modmo_eps_file)
      call pc_get('DEL_FILE',   obj%del_file,   modmo_del_file)
      call pc_get('OFFMAX',     obj%offmax,     modmo_offmax)
      call pc_get('SINGLE_VZ',  obj%single_vz,  modmo_single_vz)
      call pc_get('ILINEHDR',   obj%ilinehdr,   modmo_ilinehdr)
      call pc_get('XLINEHDR',   obj%xlinehdr,   modmo_xlinehdr)
      call pc_get('IVERBOS',    obj%iverbos,    modmo_iverbos)
      call pc_get('DUMP_VEL',   obj%dump_vel,   modmo_dump_vel)
      call pc_get('VP0_DUMPFILE',obj%vp0_dumpfile,modmo_vp0_dumpfile)

      call pc_put_options_field('ANI_TYPE',  opt_ani_type,   num_opt_ani_type)
      call pc_put_options_field('CONSTANT_VS_VP', opt_constant_vs_vp, &
                                num_opt_constant_vs_vp)
      call pc_put_options_field('TIME2DEPTH',opt_time2depth, num_opt_time2depth)
      call pc_put_options_field('SINGLE_VZ', opt_single_vz,  num_opt_single_vz)
      call pc_put_options_field('DUMP_VEL',  opt_dump_vel,   num_opt_dump_vel)

!      *** screen traps ***

      call pc_call_screen_trap('MODMO', modmo_modmo)

!      *** end trap ***

      call pc_call_end_trap(modmo_end)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      ! Check global argument numtr
      if (obj%numtr < 1) then
         call pc_error('modmo: Global numtr must be >=1')
      end if
      ! Check global argument gathered
      if (.not.obj%gathered) then
         call pc_error('modmo: Input data must be gathered.  Please &
                       &insert GATHER before MODMO.')
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
      call modmo_set_parms(obj, ierr)
      if (ierr /= 0) call pc_error('modmo: error setting parameters')
      call modmo_alloc(obj, ierr)
      if (ierr /= 0) call pc_error('modmo: error allocating permanent memory')

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      ! Initialize anything needed for actual execution of process.
      call modmo_vel_init(obj, obj%vp0_obj, obj%vp0_file, obj%vp0_desc, &
                          obj%vp0_dfile, obj%vp0_wtype, obj%vp0_ftype,  &
                          obj%vp0_xhdr, obj%vp0_yhdr, obj%vp0_ix,       &
                          obj%vp0_iy, obj%vp0_iz, obj%vp0_hdwd,         &
                          obj%vp0_ng, obj%vp0_og, obj%vp0_dg, ierr)
      if (ierr /= 0) call pc_error('modmo: error in vel_init for vp0')
      if (obj%dump_vel_l) then
        if (pcps_get_num_procs() > 1) then
          write(filename, '(A,A,I4.4)') trim(obj%vp0_dumpfile), '.pe', &
                pcps_get_worker_num()
        else
          write(filename, '(A)') trim(obj%vp0_dumpfile)
        end if
        obj%vp0_trcio => trcio_open(filename, 'w', .false., 64, obj%nzm, 32, 64)
        if (.not.associated(obj%vp0_trcio)) &
          call pc_error('modmo: vp0 dump: error opening dump file', filename)
        ierr = trcio_set_ipn (obj%vp0_trcio, obj%ipn)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo: vp0 dump: error setting ipn')
        ierr = trcio_set_dt  (obj%vp0_trcio, obj%dzm/obj%depth_scale)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo: vp0 dump: error setting dt')
        ierr = trcio_set_tmin(obj%vp0_trcio, obj%z0m/obj%depth_scale)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo: vp0 dump: error setting tmin')
        ierr = trcio_set_tmax(obj%vp0_trcio, (obj%z0m + obj%dzm*(obj%nzm-1)) &
                              /obj%depth_scale)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo: vp0 dump: error setting tmin')
        ierr = trcio_writeheader(obj%vp0_trcio)
        if (ierr /= TRCIO_OK) &
          call pc_error('modmo: vp0 dump: error writing dump file header')
      end if

      if (obj%anisotropic) then
        if (.not.obj%constant_vs_vp_l) then
          call modmo_vel_init(obj, obj%vs0_obj, obj%vs0_file, obj%vs0_desc, &
                              obj%vs0_dfile, obj%vs0_wtype, obj%vs0_ftype,  &
                              obj%vs0_xhdr, obj%vs0_yhdr, obj%vs0_ix,       &
                              obj%vs0_iy, obj%vs0_iz, obj%vs0_hdwd,         &
                              obj%vs0_ng, obj%vs0_og, obj%vs0_dg, ierr)
          if (ierr /= 0) call pc_error('modmo: error in vel_init for vs0')
        end if

        call modmo_vel_init(obj, obj%eps_obj, obj%eps_file, obj%eps_desc, &
                            obj%eps_dfile, obj%eps_wtype, obj%eps_ftype,  &
                            obj%eps_xhdr, obj%eps_yhdr, obj%eps_ix,       &
                            obj%eps_iy, obj%eps_iz, obj%eps_hdwd,         &
                            obj%eps_ng, obj%eps_og, obj%eps_dg, ierr)
        if (ierr /= 0) call pc_error('modmo: error in vel_init for eps')

        call modmo_vel_init(obj, obj%del_obj, obj%del_file, obj%del_desc, &
                            obj%del_dfile, obj%del_wtype, obj%del_ftype,  &
                            obj%del_xhdr, obj%del_yhdr, obj%del_ix,       &
                            obj%del_iy, obj%del_iz, obj%del_hdwd,         &
                            obj%del_ng, obj%del_og, obj%del_dg, ierr)
        if (ierr /= 0) call pc_error('modmo: error in vel_init for del')
      end if

      call timer_alloc(modmo_timers(MODMO_TIMER_MAIN), ierr)
      if (ierr /= 0) call pc_error('modmo_update: timer_alloc failed')
      call timer_clear(modmo_timers(MODMO_TIMER_MAIN))

      call timer_alloc(modmo_timers(MODMO_TIMER_DO_GATHER), ierr)
      if (ierr /= 0) call pc_error('modmo_update: timer_alloc failed')
      call timer_clear(modmo_timers(MODMO_TIMER_DO_GATHER))

      call timer_alloc(modmo_timers(MODMO_TIMER_RT), ierr)
      if (ierr /= 0) call pc_error('modmo_update: timer_alloc failed')
      call timer_clear(modmo_timers(MODMO_TIMER_RT))

      call timer_alloc(modmo_timers(MODMO_TIMER_TD), ierr)
      if (ierr /= 0) call pc_error('modmo_update: timer_alloc failed')
      call timer_clear(modmo_timers(MODMO_TIMER_TD))

      call timer_alloc(modmo_timers(MODMO_TIMER_DT), ierr)
      if (ierr /= 0) call pc_error('modmo_update: timer_alloc failed')
      call timer_clear(modmo_timers(MODMO_TIMER_DT))


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


   end subroutine modmo_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

   subroutine modmo_ani_type(keyword)
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
            call modmo_constant_vs_vp(keyword)
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
            call pc_warning('modmo: ani_type must be ISOTROPIC or &
                            &VTI; setting ani_type to ISOTROPIC')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo_ani_type

   subroutine modmo_time2depth(keyword)
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
            call pc_warning('modmo: time2depth must be TIME2DEPTH or &
                            &DEPTH2TIME; setting time2depth to TIME2DEPTH')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo_time2depth

   subroutine modmo_ang(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%ang <= 0.) .or. (object%ang > 90.)) then
         call pc_error('modmo: ang must be > 0. or <= 90.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_ang

   subroutine modmo_dang(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%dang <= 0.) then
         call pc_error('modmo: dang must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_dang

   subroutine modmo_dxmap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%dxmap <= 0.) then
         call pc_error('modmo: dxmap must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_dxmap

   subroutine modmo_depth_scale(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%depth_scale <= 0.) then
         call pc_error('modmo: depth_scale must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_depth_scale

   subroutine modmo_nz(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (.not.object%t2d) then
         object%nz = object%ndpt
      end if
      if (object%nz <= 0) then
         call pc_error('modmo: nz must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_nz

   subroutine modmo_dz(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (.not.object%t2d) then
         object%dz = object%dt * object%depth_scale
      end if
      if (object%dz <= 0.) then
         call pc_error('modmo: dz must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_dz

   subroutine modmo_z0(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (.not.object%t2d) then
         object%z0 = object%tstrt * object%depth_scale
      end if
      if (object%z0 < 0.) then
         call pc_error('modmo: z0 must be >= 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_z0

   subroutine modmo_nzm(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%nzm <= 0) then
         call pc_error('modmo: nzm must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_nzm

   subroutine modmo_dzm(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%dzm <= 0.) then
         call pc_error('modmo: dzm must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_dzm

   subroutine modmo_z0m(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%z0m < 0.) then
         call pc_error('modmo: z0m must be >= 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_z0m

   subroutine modmo_nt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%t2d) then
         object%nt = object%ndpt
      end if
      if (object%nt <= 0) then
         call pc_error('modmo: nt must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_nt

   subroutine modmo_delt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%t2d) then
         object%delt = object%dt
      end if
      if (object%delt <= 0.) then
         call pc_error('modmo: delt must be > 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_delt

   subroutine modmo_t0(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%t2d) then
         object%t0 = object%tstrt
      end if
      if (object%t0 < 0.) then
         call pc_error('modmo: t0 must be >= 0.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_t0

   subroutine modmo_ismoo(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%ismoo < 0) then
         call pc_error('modmo: ismoo must be >= 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_ismoo

   subroutine modmo_vp0_file(keyword)
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
         call pc_error('modmo: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%vp0_ftype=modgrid_ftype(object%vp0_file, lunprint, &
                                     object%vp0_fsize)
      if (object%vp0_ftype == 'UNKNOWN') then
         call pc_error('modmo: vp0 modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo: vp0 modgrid filetype is "',        &
                          trim(object%vp0_ftype), '", filesize is ', &
                          object%vp0_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo_vp0_file

   subroutine modmo_constant_vs_vp(keyword)
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
            call pc_warning('modmo: constant_vs_vp must be YES or NO; &
                            &setting constant_vs_vp to NO')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo_constant_vs_vp

   subroutine modmo_vs_vp_ratio(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%vs_vp_ratio >= 1. .or. object%vs_vp_ratio < 0.) then
         call pc_error('modmo: vs_vp_ratio must be >= 0. and < 1.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_vs_vp_ratio

   subroutine modmo_vs0_file(keyword)
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
         call pc_error('modmo: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%vs0_ftype=modgrid_ftype(object%vs0_file, lunprint, &
                                     object%vs0_fsize)
      if (object%vs0_ftype == 'UNKNOWN') then
         call pc_error('modmo: vs0 modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo: vs0 modgrid filetype is "',        &
                          trim(object%vs0_ftype), '", filesize is ', &
                          object%vs0_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo_vs0_file

   subroutine modmo_eps_file(keyword)
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
         call pc_error('modmo: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%eps_ftype=modgrid_ftype(object%eps_file, lunprint, &
                                     object%eps_fsize)
      if (object%eps_ftype == 'UNKNOWN') then
         call pc_error('modmo: eps modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo: eps modgrid filetype is "',        &
                          trim(object%eps_ftype), '", filesize is ', &
                          object%eps_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo_eps_file

   subroutine modmo_del_file(keyword)
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
         call pc_error('modmo: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
      object%del_ftype=modgrid_ftype(object%del_file, lunprint, &
                                     object%del_fsize)
      if (object%del_ftype == 'UNKNOWN') then
         call pc_error('modmo: del modgrid filetype is unknown!')
         call pc_jump_field(keyword)
      else
         write(infostr,*) 'modmo: del modgrid filetype is "',        &
                          trim(object%del_ftype), '", filesize is ', &
                          object%del_fsize, ' bytes'
         call pc_info(infostr)
      end if
      return
   end subroutine modmo_del_file

   subroutine modmo_offmax(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if (object%offmax <= 0) then
         call pc_error('modmo: offmax must be > 0')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_offmax

   subroutine modmo_single_vz(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      call string_to_upper(object%single_vz)
      select case(object%single_vz)
         case('NO', 'YES')
         case default
            call pc_error('modmo: single_vz must be YES or NO')
            call pc_jump_field(keyword)
      end select
      return
   end subroutine modmo_single_vz

   subroutine modmo_ilinehdr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%ilinehdr > object%nwih) .and. (object%ilinehdr < 1)) then
         call pc_error('modmo: ilinehdr must be between 1 and ', object%nwih)
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_ilinehdr

   subroutine modmo_xlinehdr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%xlinehdr > object%nwih) .and. (object%xlinehdr < 1)) then
         call pc_error('modmo: xlinehdr must be between 1 and ', object%nwih)
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_xlinehdr

   subroutine modmo_iverbos(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      if ((object%iverbos /= 0) .and. (object%iverbos /= 1)) then
         call pc_error('modmo: iverbos must be 0 or 1')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_iverbos

   subroutine modmo_dump_vel(keyword)
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
            call pc_warning('modmo: dump_vel must be YES or NO; &
                            &setting dump_vel to NO')
            call pc_jump_field(keyword)
      end select

      return
   end subroutine modmo_dump_vel

   subroutine modmo_vp0_dumpfile(keyword)
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
         call pc_error('modmo: Enter a valid filename.')
         call pc_jump_field(keyword)
      end if
      return
   end subroutine modmo_vp0_dumpfile


   ! *** Screen trap for  FOO ***
   subroutine modmo_modmo(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      ! Do nothing right now
      return
   end subroutine modmo_modmo

   ! *** End Trap ***
   subroutine modmo_end
      implicit none

      ! Do nothing right now
      return
   end subroutine modmo_end


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


   subroutine modmo (obj,ntr,hd,tr)

      implicit none

      ! Subroutine arguments
      type(modmo_struct), intent(inout) :: obj                    ! arguments
      integer,            intent(inout) :: ntr                    ! arguments
      double precision,   intent(inout) :: hd(:,:)                ! arguments
      real,               intent(inout) :: tr(:,:)                ! arguments

      ! Local variables
      integer :: ierr
      integer, save :: ntr_total=0

      ierr = 0
      call timer_start(modmo_timers(MODMO_TIMER_MAIN))
      select case (ntr)
      case (1:)
         call timer_start(modmo_timers(MODMO_TIMER_DO_GATHER))
         call modmo_do_gather(obj, ntr, hd, tr, ierr)
         call timer_stop(modmo_timers(MODMO_TIMER_DO_GATHER))
         ntr_total = ntr_total + ntr
         if (ierr /= 0) ntr = FATAL_ERROR
         call timer_stop(modmo_timers(MODMO_TIMER_MAIN))
         return
      case (NEED_TRACES)
         ntr = NEED_TRACES
         call timer_stop(modmo_timers(MODMO_TIMER_MAIN))
         return
      case (NO_MORE_TRACES)
         write(lunprint,*) 'modmo: Total number of traces processed = ', &
                           ntr_total
         call timer_stop(modmo_timers(MODMO_TIMER_MAIN))
         call modmo_wrapup(obj)
         return
      case default
         ntr = FATAL_ERROR
         call timer_stop(modmo_timers(MODMO_TIMER_MAIN))
         return
      end select
      call timer_stop(modmo_timers(MODMO_TIMER_MAIN))

      return
   end subroutine modmo


!!--------------------------- modmo_do_gather --------------------------------!!
!!--------------------------- modmo_do_gather --------------------------------!!
!!--------------------------- modmo_do_gather --------------------------------!!


   subroutine modmo_do_gather (obj, ntr, hd, tr, ierr)

      implicit none

      ! Subroutine arguments
      type(modmo_struct), intent(inout) :: obj                 ! arguments
      integer,            intent(inout) :: ntr                 ! arguments
      double precision,   intent(inout) :: hd(:,:)             ! arguments
      real,               intent(inout) :: tr(:,:)             ! arguments
      integer,            intent(inout) :: ierr

      ! Local variables
      integer, save      :: i_genmap=0
      integer, parameter :: hdi_off=6    ! offset header word index
      integer            :: itr

      real               :: offset
      real,    save      :: last_ycmp=-9999999.
      real               :: cur_xcmp, cur_ycmp
      double precision   :: vp0_hd(64)

      ierr = 0

      ! Read in velocity function and create travel time map
      if (i_genmap == 0) then
         ! Read in a whole line of velocity functions each time ycmp changes
         cur_ycmp = hd(obj%xlinehdr, 1)
         !write(lunprint,*) 'last_ycmp, cur_ycmp = ', last_ycmp, cur_ycmp
         !if (last_ycmp /= cur_ycmp) then
         !   call modmo_vel_read_slice(obj, obj%vp0_obj, obj%vp0_desc,       &
         !                             obj%vp0_ix, obj%vp0_iy, obj%vp0_iz,   &
         !                             obj%vp0_hdwd, obj%vp0_ng, obj%vp0_og, &
         !                             obj%vp0_dg, obj%vp0_data, cur_ycmp,   &
         !                             ierr)
         !   if (obj%anisotropic) then
         !     call modmo_vel_read_slice(obj, obj%vs0_obj, obj%vs0_desc,      &
         !                               obj%vs0_ix, obj%vs0_iy, obj%vs0_iz,  &
         !                               obj%vs0_hdwd, obj%vs0_ng, obj%vs0_og,&
         !                               obj%vs0_dg, obj%vs0_data, cur_ycmp,  &
         !                               ierr)
         !     call modmo_vel_read_slice(obj, obj%eps_obj, obj%eps_desc,      &
         !                               obj%eps_ix, obj%eps_iy, obj%eps_iz,  &
         !                               obj%eps_hdwd, obj%eps_ng, obj%eps_og,&
         !                               obj%eps_dg, obj%eps_data, cur_ycmp,  &
         !                               ierr)
         !     call modmo_vel_read_slice(obj, obj%del_obj, obj%del_desc,      &
         !                               obj%del_ix, obj%del_iy, obj%del_iz,  &
         !                               obj%del_hdwd, obj%del_ng, obj%del_og,&
         !                               obj%del_dg, obj%del_data, cur_ycmp,  &
         !                               ierr)
         !   end if
         !   last_ycmp = cur_ycmp
         !end if
         cur_xcmp = hd(obj%ilinehdr, 1)
         if (obj%verbos) then
           write(lunprint,*) 'cur_xcmp, cur_ycmp = ', cur_xcmp, cur_ycmp
         end if
         call modmo_vel_paint_pencil(obj%vp0_obj, obj%vp0_desc,            &
                                     obj%vp0_ix, obj%vp0_iy, obj%vp0_iz,   &
                                     obj%vp0_xhdr, obj%vp0_yhdr,           &
                                     obj%vp0_ng, obj%vp0_og, obj%vp0_dg,   &
                                     cur_xcmp, cur_ycmp,                   &
                                     obj%nzm, obj%z0m, obj%dzm,            &
                                     obj%vp0, obj%cube, ierr)
         !call modmo_vel_read_pencil(obj, obj%vp0_desc, obj%vp0_ix,        &
         !                           obj%vp0_iy, obj%vp0_iz, obj%vp0_ng,   &
         !                           obj%vp0_og, obj%vp0_dg, obj%vp0_data, &
         !                           cur_xcmp, obj%vp0, ierr)
         if (obj%dump_vel_l) then
           vp0_hd = hd(:,1)
           vp0_hd(6) = 0
           call lav_set_hdr(vp0_hd, obj%vp0, obj%nzm)
           ierr = trcio_write_trace(obj%vp0_trcio, vp0_hd, obj%vp0)
           if (ierr /= TRCIO_OK) &
             call pc_error('modmo: vp0 dump: error writing dump file')
         end if
         if (obj%anisotropic) then
           if (obj%constant_vs_vp_l) then
             obj%vs0 = obj%vs_vp_ratio * obj%vp0
           else
             call modmo_vel_paint_pencil(obj%vs0_obj, obj%vs0_desc,          &
                                         obj%vs0_ix, obj%vs0_iy, obj%vs0_iz, &
                                         obj%vs0_xhdr, obj%vs0_yhdr,         &
                                         obj%vs0_ng, obj%vs0_og, obj%vs0_dg, &
                                         cur_xcmp, cur_ycmp,                 &
                                         obj%nzm, obj%z0m, obj%dzm,          &
                                         obj%vs0, obj%cube, ierr)
           end if
           call modmo_vel_paint_pencil(obj%eps_obj, obj%eps_desc,          &
                                       obj%eps_ix, obj%eps_iy, obj%eps_iz, &
                                       obj%eps_xhdr, obj%eps_yhdr,         &
                                       obj%eps_ng, obj%eps_og, obj%eps_dg, &
                                       cur_xcmp, cur_ycmp,                 &
                                       obj%nzm, obj%z0m, obj%dzm,          &
                                       obj%eps, obj%cube, ierr)
           call modmo_vel_paint_pencil(obj%del_obj, obj%del_desc,          &
                                       obj%del_ix, obj%del_iy, obj%del_iz, &
                                       obj%del_xhdr, obj%del_yhdr,         &
                                       obj%del_ng, obj%del_og, obj%del_dg, &
                                       cur_xcmp, cur_ycmp,                 &
                                       obj%nzm, obj%z0m, obj%dzm,          &
                                       obj%del, obj%cube, ierr)
           !call modmo_vel_read_pencil(obj, obj%vs0_desc, obj%vs0_ix,        &
           !                           obj%vs0_iy, obj%vs0_iz, obj%vs0_ng,   &
           !                           obj%vs0_og, obj%vs0_dg, obj%vs0_data, &
           !                           cur_xcmp, obj%vs0, ierr)
           !call modmo_vel_read_pencil(obj, obj%eps_desc, obj%eps_ix,        &
           !                           obj%eps_iy, obj%eps_iz, obj%eps_ng,   &
           !                           obj%eps_og, obj%eps_dg, obj%eps_data, &
           !                           cur_xcmp, obj%eps, ierr)
           !call modmo_vel_read_pencil(obj, obj%del_desc, obj%del_ix,        &
           !                           obj%del_iy, obj%del_iz, obj%del_ng,   &
           !                           obj%del_og, obj%del_dg, obj%del_data, &
           !                           cur_xcmp, obj%del, ierr)
         end if

         call timer_start(modmo_timers(MODMO_TIMER_RT))
         if (obj%anisotropic) then
           call modmo_vzmapr_vti(obj%nxmap, obj%nzmap, obj%np, obj%theta_max, &
                                 obj%vp0, obj%eps, obj%del, obj%vs0, obj%tmax,&
                                 obj%dtmap, obj%tmap, obj%zsrc, obj%z0m,      &
                                 obj%dzmap, obj%dxmap, obj%ismoo, obj%noint,  &
                                 obj%xr, obj%tr, ierr)
         else
           call modmo_vzmapr_iso(obj%nxmap, obj%nzmap, obj%np, obj%theta_max, &
                                 obj%vp0,                            obj%tmax,&
                                 obj%dtmap, obj%tmap, obj%zsrc, obj%z0m,      &
                                 obj%dzmap, obj%dxmap, obj%ismoo, obj%noint,  &
                                 obj%xr, obj%tr, ierr)
         end if
         call timer_stop(modmo_timers(MODMO_TIMER_RT))
!        call modmo_rmsmap_vti(obj%nxmap, obj%nzmap, obj%vp0, obj%eps, &
!                              obj%del, obj%tmax, obj%dtmap, obj%tmap, &
!                              obj%izsrc, obj%dzmap, obj%dxmap, ierr)
      end if
!     do ix=1,obj%nxmap
!        write(200)(obj%tmap(iz,ix),iz=1,obj%nzmap)
!     end do

      ! Reset i_genmap if there is only a single velocity trace
      if (obj%vzflag) i_genmap = 1

      ! Trace loop - convert to depth with moveout correction
      do itr=1,ntr
         offset = hd(hdi_off,itr)
         if (obj%t2d) then
            call timer_start(modmo_timers(MODMO_TIMER_TD))
            call modmo_t2dzmo(offset, obj%dxmap, obj%dz, obj%dzmap, obj%dtmap,&
                              obj%delt, obj%tmax, obj%tmap, obj%nzmap,        &
                              obj%nzd, obj%nxmap, obj%nt, obj%z0, obj%z0m,    &
                              obj%t0, tr(:,itr), ierr)
            call timer_stop(modmo_timers(MODMO_TIMER_TD))
         else
            call timer_start(modmo_timers(MODMO_TIMER_DT))
            call modmo_d2tzmo(offset, obj%dxmap, obj%dz, obj%dzmap, obj%dtmap,&
                              obj%delt, obj%tmax, obj%tmap, obj%nzmap,        &
                              obj%nzd, obj%nxmap, obj%nt, obj%z0, obj%z0m,    &
                              obj%t0, tr(:,itr), ierr)
            call timer_stop(modmo_timers(MODMO_TIMER_DT))
         end if
      end do

      ! Set largest absolute value for each trace
      call lav_set_hdr(hd, tr, obj%ndpt_out, ntr)

      return
   end subroutine modmo_do_gather


!!--------------------------- modmo_vzmapr_iso -------------------------------!!
!!--------------------------- modmo_vzmapr_iso -------------------------------!!
!!--------------------------- modmo_vzmapr_iso -------------------------------!!
   ! NOTE:
   !   1) need to move this stuff into a separate module?
   !   2) need to move memory stuff (nzmax) out of here
   !
   !  generate a v(z) based travel-time map
   !  Dan Whitmore
   !
   !  To Do:
   !    1. izf needs to be dimensioned to max(nx,nz)
   !
   subroutine modmo_vzmapr_iso(nx, nz, np, theta_max, vp0, tmax, dt, tmap, &
                               zsrc, z0, dz, dx, ismoo, noint, xr, tr, ierr)

      implicit none

      ! Subroutine arguments
      integer,             intent(in)    :: nx, nz, np
      real,                intent(inout) :: theta_max
      real,                intent(in)    :: vp0(nz)
      real,                intent(in)    :: tmax, dt
      real,                intent(out)   :: tmap(nz, nx)
      real,                intent(in)    :: zsrc, z0, dz, dx
      integer,             intent(in)    :: ismoo, noint
      real,                intent(out)   :: xr(nz, np), tr(nz, np)
      integer,             intent(inout) :: ierr

      ! Local variables
      integer, parameter :: nzmax=8000
      real,    parameter :: pi=3.141592653589793
      integer            :: icount, iflag, ismoott, ix1, ix2, nray, izsrc
      integer            :: jc, jp    , jray, js, jx, jx1, jx2, jz, jz1, jz2 
      real               :: a, aa, c, dr, dtheta, dx1, dz1, p, s, scale
      real               :: t, tbig, tdead, x, x1, x2, xmax, xray, z
      real               :: aaa, bbb, slow
      integer            :: izf(nzmax), icrit(nzmax)
      real               :: slo(nzmax)
      real               :: vpsm(nzmax)

      ierr = 0

      if (nzmax < nz) then
         ierr=-1
         return
      end if
      if (nzmax < nx) then   ! izf() needs to be dimensioned 1:max(nx,nz)
         ierr=-1
         return
      end if

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

      !do jp = 1, np
      do 100 jp = 1, np
         p = sin(float(jp-1)*dtheta) / vpsm(1)
         x = 0.0
         !z = (izsrc - 1) * dz
         z = zsrc
         jz = (z-z0)/dz + 1
         jx = 1
         t  = 0.0
         !tmap(izsrc,1) = 0.0
         dr = dz
         bbb = ((z-z0) - (jz-1)*dz)/dz
         aaa = 1. - bbb

         ! Begin ray path loop
         do while (1 == 1)
            if (jz < nz-1) then
               slow = aaa/vpsm(jz) + bbb/vpsm(jz+1)
               !slow = 1./vpsm(jz)
            end if
            s = p / slow

            ! First "critical" ray:
            ! (accuracy depends on using many rays)
            if (s >= 0.999999) then
               if (icrit(jz) == 0 .and. noint == 0) then
                  nray = (xmax + dx - x) / dx
                  !write(lunprint, *) 'nray, jp, jz = ', nray, jp, jz
                  !write(lunprint, *) 'icrit = ', icrit(jz)
                  if (nray > 1) then
                     do jray = 1, nray
                        xray = x + (jray-1) * dx
                        jx   = nint(xray / dx)
                        t    = t + dx / vpsm(jz)
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
            c    = sqrt(1.0 - s**2)
            dx1  = s * dr
            dz1  = c * dr
            x    = x + dx1
            z    = z + dz1
            jz   = (z-z0) / dz + 1
            if (jz < nz-1) then
               bbb  = ((z-z0) - (jz-1)*dz)/dz
               aaa  = 1. - bbb
               slow = aaa/vpsm(jz) + bbb/vpsm(jz+1)
               !slow = 1./vpsm(jz)
            end if
            t    = t +  dr * slow

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

      return
   end subroutine modmo_vzmapr_iso


!!--------------------------- modmo_rmsmap_vti -------------------------------!!
!!--------------------------- modmo_rmsmap_vti -------------------------------!!
!!--------------------------- modmo_rmsmap_vti -------------------------------!!
   subroutine modmo_rmsmap_vti(nx, nz, vp0, eps, del, tmax, dt, tmap, &
                               izsrc, dz, dx, ierr)

      implicit none

      ! Subroutine arguments
      integer,             intent(in)    :: nx, nz
      real,                intent(in)    :: vp0(nz), eps(nz), del(nz)
      real,                intent(in)    :: tmax, dt
      real,                intent(out)   :: tmap(nz, nx)
      integer,             intent(in)    :: izsrc
      real,                intent(in)    :: dz, dx
      integer,             intent(inout) :: ierr

      ! Local variables
      integer, parameter :: nzmax=8000
      integer            :: jx, jz, js, ismoott
      real               :: tbig, tdead, xmax, scale, x
      real               :: vnmo, eta, t0_int
      real               :: vnmo_tmp, eta_tmp, tau_tmp, xdv
      real               :: vnmo_rms(nzmax), eta_rms(nzmax), t0(nzmax)
      real               :: slo(nzmax)

      ierr = 0

      if (nzmax < nz) then
         ierr=-1
         return
      end if

      ! Set up a few parameters and initialize travel-time map
      tdead     = 3000. * tmax
      tbig      =  2. * tmax
      xmax      = (nx - 1) * dx
      tmap(1:nz,1:nx) = tdead

      ! Create RMS velocity and eta functions
      if (izsrc /= 1) then
         ierr=-1
         return

         jz=izsrc
         do jz=izsrc-1,1,-1
         end do
         do jz=izsrc+1,nz
         end do
      else
         jz = 1
         vnmo = vp0(jz)*sqrt(1.+2.*del(jz))
         eta  = (eps(jz)-del(jz))/(1.+2*del(jz))
         t0      (jz) = 0.
         vnmo_tmp     = 0.
         eta_tmp      = 0.
         vnmo_rms(jz) = vnmo
         eta_rms (jz) = eta
         do jz=2,nz
            vnmo = vp0(jz)*sqrt(1.+2.*del(jz))
            eta  = (eps(jz)-del(jz))/(1.+2*del(jz))
            t0_int       = 2*dz/vp0(jz-1)
            t0      (jz) = t0(jz-1) + t0_int
            vnmo_tmp     = vnmo_tmp + vnmo*vnmo*t0_int
            vnmo_rms(jz) = sqrt(vnmo_tmp/t0(jz))
            eta_tmp      = eta_tmp  + vnmo**4*(1.+8.*eta)*t0_int
            eta_rms (jz) = 1./8.*(eta_tmp/(t0(jz)*vnmo_rms(jz)**4) - 1)
         end do
      end if

      ! Calculate traveltimes (equation is for full offset, 2-way tt; need
      ! to store in table as half offset and 1-way tt)
      do jx = 1, nx
         do jz = 1, nz
            x       = 2.*(jx-1)*dx
            xdv     = x/vnmo_rms(jz)
            eta     = eta_rms(jz)
            tau_tmp = t0(jz)**2
            tmap(jz,jx) = 0.5*sqrt(tau_tmp + xdv**2 &
                                 - 2*eta*xdv**4/(tau_tmp + (1.+2.*eta)*xdv**2))
         end do
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

      do jx = 1, nx
         do jz = 1, nz
            if (tmap(jz,jx) >= tbig) tmap(jz,jx) = tbig
         end do
      end do

      return
   end subroutine modmo_rmsmap_vti


!!--------------------------- modmo_vzmapr_vti -------------------------------!!
!!--------------------------- modmo_vzmapr_vti -------------------------------!!
!!--------------------------- modmo_vzmapr_vti -------------------------------!!
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
   subroutine modmo_vzmapr_vti(nx, nz, np, theta_max, vp0, eps, del, vs0, &
                               tmax, dt, tmap, zsrc, z0, dz, dx, ismoo,   &
                               noint, xr, tr, ierr)

      implicit none

      ! Subroutine arguments
      integer,             intent(in)    :: nx, nz, np
      real,                intent(inout) :: theta_max
      real,                intent(in)    :: vp0(nz), eps(nz), del(nz), vs0(nz)
      real,                intent(in)    :: tmax, dt
      real,                intent(out)   :: tmap(nz, nx)
      real,                intent(in)    :: zsrc, z0, dz, dx
      integer,             intent(in)    :: ismoo, noint
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
      real               :: vpsm(nzmax)

      ierr = 0

      if (nzmax < nz) then
         ierr=-1
         return
      end if
      if (nzmax < nx) then   ! izf() needs to be dimensioned 1:max(nx,nz)
         ierr=-1
         return
      end if

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

      !do jp = 1, np
      do 100 jp = 1, np
         theta = float(jp-1)*dtheta
         p = sin(theta) / modmo_vpphase(theta,vpsm(1),vs0(1),eps(1),del(1))
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
               slow = aaa/modmo_vpphase(theta,vpsm(jz),vs0(jz),     &
                                        eps(jz),del(jz))            &
                    + bbb/modmo_vpphase(theta,vpsm(jz+1),vs0(jz+1), &
                                        eps(jz+1),del(jz+1))
               !slow = 1./modmo_vpphase(theta,vpsm(jz),vs0(jz), &
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
               phi = aaa*modmo_phigroup(theta,vpsm(jz),vs0(jz),eps(jz),del(jz))&
                   + bbb*modmo_phigroup(theta,vpsm(jz+1),vs0(jz+1),eps(jz+1),  &
                                       del(jz+1))
            else
               phi = modmo_phigroup(theta,vpsm(jz),vs0(jz),eps(jz),del(jz))
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
               slowg = aaa/modmo_vpgroup(theta,vpsm(jz),vs0(jz),     &
                                         eps(jz),del(jz))            &
                     + bbb/modmo_vpgroup(theta,vpsm(jz+1),vs0(jz+1), &
                                         eps(jz+1),del(jz+1))
               !slowg = 1./modmo_vpgroup(theta,vpsm(jz),vs0(jz),      &
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

      return
   end subroutine modmo_vzmapr_vti


!!--------------------------- modmo_vpphase ----------------------------------!!
!!--------------------------- modmo_vpphase ----------------------------------!!
!!--------------------------- modmo_vpphase ----------------------------------!!
      ! P-wave phase velocity
      function modmo_vpphase(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo_vpphase

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
      modmo_vpphase=alpha*sqrt(1.0+epsilon*sin2theta+dstar)

      return
      end function modmo_vpphase


!!--------------------------- modmo_dvpdtheta --------------------------------!!
!!--------------------------- modmo_dvpdtheta --------------------------------!!
!!--------------------------- modmo_dvpdtheta --------------------------------!!
      ! P-wave phase velocity derivative w.r.t. phase angle
      function modmo_dvpdtheta(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo_dvpdtheta

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
      modmo_dvpdtheta=0.5*alpha/sqrt(1.0+epsilon*sin2theta+dstar)           &
                *(2.0*epsilon*sintheta*costheta+ddstardtheta)

      return
      end function modmo_dvpdtheta


!!--------------------------- modmo_vpgroup ----------------------------------!!
!!--------------------------- modmo_vpgroup ----------------------------------!!
!!--------------------------- modmo_vpgroup ----------------------------------!!
      ! P-wave group velocity
      function modmo_vpgroup(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo_vpgroup

      ! Local variables
      real :: v,dv

      v  = modmo_vpphase(theta,alpha,beta,epsilon,delta)
      dv = modmo_dvpdtheta(theta,alpha,beta,epsilon,delta)
      modmo_vpgroup = sqrt(v*v + dv*dv)

      return
      end function modmo_vpgroup


!!--------------------------- modmo_phigroup ---------------------------------!!
!!--------------------------- modmo_phigroup ---------------------------------!!
!!--------------------------- modmo_phigroup ---------------------------------!!
      ! P-wave group angle
      function modmo_phigroup(theta,alpha,beta,epsilon,delta)

      implicit none

      ! Function arguments
      real, intent(in)  :: theta, alpha, beta, epsilon, delta
      real              :: modmo_phigroup

      ! Local variables
      real :: v,dv,s,c

      s  = sin(theta)
      c  = cos(theta)
      v  = modmo_vpphase(theta,alpha,beta,epsilon,delta)
      dv = modmo_dvpdtheta(theta,alpha,beta,epsilon,delta)
      modmo_phigroup = atan2((v*s + dv*c), (v*c - dv*s))

      return
      end function modmo_phigroup


!!--------------------------- modmo_t2dzmo -----------------------------------!!
!!--------------------------- modmo_t2dzmo -----------------------------------!!
!!--------------------------- modmo_t2dzmo -----------------------------------!!
   ! Reverse time to depth moveout
   ! NOTE:
   !   1) need to move memory stuff (ntmax) out of here
   !   2) maybe add a call to spline primitive so all interp gets done in 1 call
   subroutine modmo_t2dzmo(offset, dx, dz, dzmap, dtmap, dtsec, tmax, tmap, &
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
   end subroutine modmo_t2dzmo


!!--------------------------- modmo_d2tzmo -----------------------------------!!
!!--------------------------- modmo_d2tzmo -----------------------------------!!
!!--------------------------- modmo_d2tzmo -----------------------------------!!
   ! Reverse depth to time moveout
   ! NOTE:
   !   1) need to move memory stuff (nzmax, ntmax) out of here
   !   2) maybe add a call to spline primitive so all interp gets done in 1 call
   subroutine modmo_d2tzmo(offset, dx, dz, dzmap, dtmap, dtsec, tmax, tmap, &
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
   end subroutine modmo_d2tzmo


!!--------------------------- modmo_set_parms --------------------------------!!
!!--------------------------- modmo_set_parms --------------------------------!!
!!--------------------------- modmo_set_parms --------------------------------!!
   subroutine modmo_set_parms (obj, ierr)
      implicit none
      type(modmo_struct), intent(inout) :: obj       ! arguments
      integer,            intent(inout) :: ierr

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
            call pc_warning('modmo: ani_type must be ISOTROPIC or &
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
            call pc_warning('modmo: constant_vs_vp must be YES or NO; &
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
            call pc_warning('modmo: time2depth must be TIME2DEPTH or &
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
            call pc_warning('modmo: single_vz must be YES or NO; &
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
            call pc_warning('modmo: dump_vel must be YES or NO; &
                            &setting dump_vel to NO')
      end select

      obj%theta_max = abs(obj%ang)
      obj%dtheta    = abs(obj%dang)
      obj%hoffmax = .5 * obj%offmax
      obj%tmax   = obj%nt * obj%delt + obj%t0
      obj%dtmap  = .001
      obj%nzd    = obj%nz
      obj%nzmap  = obj%nzm
      obj%dzmap  = obj%dzm
      obj%nxmap  = abs(obj%hoffmax)/obj%dxmap + 4
      obj%np     = obj%theta_max/obj%dtheta + 1
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
   end subroutine modmo_set_parms


!!--------------------------- modmo_alloc ------------------------------------!!
!!--------------------------- modmo_alloc ------------------------------------!!
!!--------------------------- modmo_alloc ------------------------------------!!
   subroutine modmo_alloc (obj, ierr)
      implicit none
      type(modmo_struct), intent(inout) :: obj       ! arguments
      integer,            intent(inout) :: ierr

      ierr = 0
      call memman_allocate(obj%tmap, obj%nzmap, obj%nxmap, ierr)
      if (ierr /= 0) return
      call memman_allocate(obj%xr,   obj%nzmap, obj%np,    ierr)
      if (ierr /= 0) return
      call memman_allocate(obj%tr,   obj%nzmap, obj%np,    ierr)
      if (ierr /= 0) return
      call memman_allocate(obj%vp0,  obj%nzm,   ierr, 'vp0')
      if (ierr /= 0) return
      call memman_allocate(obj%cube, obj%nzm,   1,   1,    ierr, 'cube')
      if (ierr /= 0) return
      if (obj%anisotropic) then
        call memman_allocate(obj%vs0,  obj%nzm, ierr, 'vs0')
        if (ierr /= 0) return
        call memman_allocate(obj%eps,  obj%nzm, ierr, 'eps')
        if (ierr /= 0) return
        call memman_allocate(obj%del,  obj%nzm, ierr, 'del')
        if (ierr /= 0) return
      end if
      return
   end subroutine modmo_alloc


!!--------------------------- modmo_dealloc ----------------------------------!!
!!--------------------------- modmo_dealloc ----------------------------------!!
!!--------------------------- modmo_dealloc ----------------------------------!!
   subroutine modmo_dealloc (obj, ierr)
      implicit none
      type(modmo_struct), intent(inout) :: obj       ! arguments
      integer,            intent(inout) :: ierr

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
   end subroutine modmo_dealloc


!!--------------------------- modmo_vel_init ---------------------------------!!
!!--------------------------- modmo_vel_init ---------------------------------!!
!!--------------------------- modmo_vel_init ---------------------------------!!
   subroutine modmo_vel_init(obj, mgobj, prop_file, desc, dfile, wtype,   &
                             ftype, xhdr, yhdr, ix, iy, iz, hdwd, ng, og, &
                             dg, ierr)
      implicit none

      ! Subroutine arguments
      type(modmo_struct),               intent(inout) :: obj
      type(modgrid_struct),             pointer       :: mgobj
      character(len=*),                 intent(in)    :: prop_file
      character(len=8),                 intent(inout) :: desc
      character(len=FILENAME_LENGTH),   intent(inout) :: dfile
      character(len=8),                 intent(inout) :: wtype
      character(len=8),                 intent(inout) :: ftype
      integer,                          intent(inout) :: xhdr
      integer,                          intent(inout) :: yhdr
      integer,                          intent(out)   :: ix, iy, iz
      integer, dimension(modmo_mgdim),  intent(inout) :: hdwd
      integer, dimension(modmo_mgdim),  intent(inout) :: ng
      real,    dimension(modmo_mgdim),  intent(inout) :: og
      real,    dimension(modmo_mgdim),  intent(inout) :: dg
      integer,                          intent(inout) :: ierr

      ! Local variables
      character(len=128) :: str
      character(len=64)  :: mg_name
      character(len=16)  :: mg_pname
      character(len=16)  :: mg_punits
      integer            :: mg_rank

      ierr = 0
      ierr=modgrid_rddesc(mgobj, prop_file, lunprint,              &
                          dfile, wtype,            &
                          ftype, xhdr, yhdr)
      if (ierr /= 0) then
         write(str,*)'modmo: error in modgrid_rddesc for ', desc, ierr
         call pc_error(trim(str))
         return
      end if
      write(lunprint,*)'***** begin modgrid info for ', desc, ' *****'
      call modgrid_print(mgobj, lunprint)
      call modgrid_get_griddesc(mgobj, mg_name, mg_pname,         &
                                mg_punits, mg_rank, hdwd, &
                                ng, og, dg)
      ierr = modgrid_xyz_order(mgobj, ix, iy, iz)
      if (ierr /= 0) then
         write(str,*) 'modmo: error in modgrid_xyz_order', ierr
         call pc_error(trim(str))
         return
      end if
      write(lunprint,*) 'XYZ order = ', ix, iy, iz
      if (iz /= 1) then
         write(str,*) 'modmo: z must be fastest dimension!', ierr
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
         write(str,*)'modmo: model z origin (z0m=', obj%z0m, ') is less than &
                     &input velocity model z origin (og=', og(iz), ')!'
         call pc_error(trim(str))
         return
      end if
      if (ftype == 'MODSPEC') then
         ! do nothing
      else if ((obj%z0m+obj%nzm*obj%dzm) > (og(iz)+ng(iz)*dg(iz))) then
         ierr = -1
         write(str,*)'modmo: model z extent (', obj%z0m+obj%nzm*obj%dzm, &
                     ') is greater than input velocity model extent (',   &
                     og(iz)+ng(iz)*dg(iz), ')!'
         call pc_error(trim(str))
         return
      end if
      write(lunprint,*)'***** end   modgrid info for ', desc, ' *****'

      return
   end subroutine modmo_vel_init


!!--------------------------- modmo_vel_read_slice ---------------------------!!
!!--------------------------- modmo_vel_read_slice ---------------------------!!
!!--------------------------- modmo_vel_read_slice ---------------------------!!
   subroutine modmo_vel_read_slice(obj, mgobj, desc, ix, iy, iz, hdwd, ng, &
                                   og, dg, data, ycmp, ierr)

      implicit none

      ! Subroutine arguments
      type(modmo_struct),               intent(in)    :: obj
      type(modgrid_struct),             pointer       :: mgobj
      character(len=8),                 intent(inout) :: desc
      integer,                          intent(in)    :: ix, iy, iz
      integer, dimension(modmo_mgdim),  intent(inout) :: hdwd
      integer, dimension(modmo_mgdim),  intent(inout) :: ng
      real,    dimension(modmo_mgdim),  intent(inout) :: og
      real,    dimension(modmo_mgdim),  intent(inout) :: dg
      real,                             pointer       :: data(:)
      real,                             intent(in)    :: ycmp
      integer,                          intent(inout) :: ierr

      ! Local variables
      character(len=128) :: str
      integer            :: i
      integer, dimension(modmo_mgdim) :: hdwd_loc
      integer, dimension(modmo_mgdim) :: ng_loc
      real,    dimension(modmo_mgdim) :: og_loc
      real,    dimension(modmo_mgdim) :: dg_loc
      real,    dimension(modmo_mgdim) :: axs, axe

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
         write(str,*)'modmo: error reading ', desc, ' file'
         call pc_error(trim(str))
         return
      end if
      ierr = modgrid_get_data(mgobj, data, hdwd_loc, ng_loc, og_loc, dg_loc)
      if (ierr < 0) then
         write(str,*) 'modmo: error getting ', desc, ' data'
         call pc_error(trim(str))
         return
      else
         ierr = 0
      end if

      !write(lunprint,*) 'i   og_loc   ng_loc   dg_loc   hdwd_loc (', desc, ')'
      !do i=1,modmo_mgdim
      !   write(lunprint,*) i, og_loc(i), ng_loc(i), dg_loc(i), hdwd_loc(i)
      !end do
      !write(lunprint,*) 'i   og       ng       dg       hdwd     (', desc, ')'
      !do i=1,modmo_mgdim
      !   write(lunprint,*) i, og(i), ng(i), dg(i), hdwd(i)
      !end do
      !write(lunprint,*) 'for i=', iy, 'ng=', 1

      ! Check og
      if (og_loc(ix) /= og(ix)) then
         write(str,*) 'modmo: inconsistency in results from &
                      &modgrid_get_data: og_loc, og = ',    &
                      og_loc(ix), og(ix), ix, desc
         call pc_error(trim(str))
         return
      end if
      if (og_loc(iy) /= ycmp) then
         write(str,*) 'modmo: inconsistency in results from &
                      &modgrid_get_data: og_loc, og = ',    &
                      og_loc(iy), ycmp, iy, desc
         call pc_error(trim(str))
         return
      end if
      if (og_loc(iz) /= og(iz)) then
         write(str,*) 'modmo: inconsistency in results from &
                      &modgrid_get_data: og_loc, og = ',    &
                      og_loc(iz), og(iz), iz, desc
         call pc_error(trim(str))
         return
      end if
      ! Check dg
      do i=1,modmo_mgdim
         if (dg_loc(i) /= dg(i)) then
            write(str,*) 'modmo: inconsistency in results from &
                         &modgrid_get_data: dg_loc, dg = ',    &
                         dg_loc(i), dg(i), i, desc
            call pc_error(trim(str))
            return
         end if
      end do
      ! Check hdwd
      do i=1,modmo_mgdim
         if (hdwd_loc(i) /= hdwd(i)) then
            write(str,*) 'modmo: inconsistency in results from &
                         &modgrid_get_data: hdwd_loc, hdwd = ',&
                         hdwd_loc(i), hdwd(i), i, desc
            call pc_error(trim(str))
            return
         end if
      end do
      ! Check ng
      if (ng_loc(iz) /= ng(iz)) then
         write(str,*) 'modmo: inconsistency in results from &
                      &modgrid_get_data: ng_loc, ng = ',    &
                      ng_loc(iz), ng(iz), iz, desc
         call pc_error(trim(str))
         return
      end if
      if (ng_loc(ix) /= ng(ix)) then
         write(str,*) 'modmo: inconsistency in results from &
                      &modgrid_get_data: ng_loc, ng = ',    &
                      ng_loc(ix), ng(ix), iy, desc
         call pc_error(trim(str))
         return
      end if
      if (ng_loc(iy) /= 1) then
         write(str,*) 'modmo: inconsistency in results from &
                      &modgrid_get_data: ng_loc, ng = ',    &
                      ng_loc(iy), 1, iy, desc
         call pc_error(trim(str))
         return
      end if

      return
   end subroutine modmo_vel_read_slice


!!--------------------------- modmo_vel_read_pencil --------------------------!!
!!--------------------------- modmo_vel_read_pencil --------------------------!!
!!--------------------------- modmo_vel_read_pencil --------------------------!!
   subroutine modmo_vel_read_pencil(obj, desc, ix, iy, iz, ng, og, dg, &
                                    data, xcmp, pencil, ierr)

      implicit none

      ! Subroutine arguments
      type(modmo_struct),               intent(in)    :: obj
      character(len=8),                 intent(in)    :: desc
      integer,                          intent(in)    :: ix, iy, iz
      integer, dimension(modmo_mgdim),  intent(in)    :: ng
      real,    dimension(modmo_mgdim),  intent(in)    :: og
      real,    dimension(modmo_mgdim),  intent(in)    :: dg
      real,                             pointer       :: data(:)
      real,                             intent(in)    :: xcmp
      real,                             intent(out)   :: pencil(:)
      integer,                          intent(inout) :: ierr

      ! Local variables

      integer            :: ix_use, iptr
      real               :: xcmp_use, xmin, xmax

      ierr = 0

      !if (ng(iz) /= obj%nz) then
      !   ierr = -1
      !   write(str,*)'modmo: vel_read_pencil: vel data nz != output nz for ',&
      !               desc, ' pencil'
      !   call pc_error(trim(str))
      !   return
      !end if

      ! Find nearest pencil to xcmp
      xmin = og(ix)
      xmax = og(ix) + ng(ix)*dg(ix)
      if (xcmp < xmin) then
         call pc_warning('modmo: vel_read_pencil: xcmp < ', desc, &
                         ' minimum xcmp; using function at minimum xcmp')
         xcmp_use = xmin
      else if (xcmp > xmax) then
         call pc_warning('modmo: vel_read_pencil: xcmp > ', desc, &
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
   end subroutine modmo_vel_read_pencil


!!--------------------------- modmo_vel_paint_pencil -------------------------!!
!!--------------------------- modmo_vel_paint_pencil -------------------------!!
!!--------------------------- modmo_vel_paint_pencil -------------------------!!
   subroutine modmo_vel_paint_pencil(mgobj, desc, ix, iy, iz, xhdr, yhdr,   &
                                     ng, og, dg, xcmp, ycmp, nzm, z0m, dzm, &
                                     pencil, cube, ierr)

      implicit none

      ! Subroutine arguments
      type(modgrid_struct),             pointer       :: mgobj
      character(len=8),                 intent(in)    :: desc
      integer,                          intent(in)    :: ix, iy, iz
      integer,                          intent(in)    :: xhdr, yhdr
      integer, dimension(modmo_mgdim),  intent(in)    :: ng
      real,    dimension(modmo_mgdim),  intent(in)    :: og
      real,    dimension(modmo_mgdim),  intent(in)    :: dg
      real,                             intent(in)    :: xcmp, ycmp
      integer,                          intent(in)    :: nzm
      real,                             intent(in)    :: z0m, dzm
      real,                             intent(out)   :: pencil(:)
      real,                             intent(out)   :: cube(:,:,:)
      integer,                          intent(inout) :: ierr

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
                         ", y = ", ycmp, ", type = "//desc)
         call pc_warning("model minimum location: x = ", xmin, ", y = ", ymin)
         call pc_warning("model maximum location: x = ", xmax, ", y = ", ymax)
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
         write(str,*) 'modmo: vel_paint_pencil: error in modgrid_paint_by_obj &
                      & for ', desc, ' = ', ierr
         call pc_error(trim(str))
      end if
      pencil(1:nzm) = cube(1:nzm,1,1)

      return
   end subroutine modmo_vel_paint_pencil


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


   subroutine modmo_wrapup (obj)
      type(modmo_struct), intent(inout) :: obj       ! arguments

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
            call pc_error('modmo: vp0 dump: error closing dump file')
      end if

      call timer_report(modmo_timers(MODMO_TIMER_MAIN), &
                        'main', lunprint)
      call timer_free(modmo_timers(MODMO_TIMER_MAIN), ierr)
      if (ierr /= 0) call pc_error('modmo_wrapup: timer_free failed')

      call timer_report(modmo_timers(MODMO_TIMER_DO_GATHER), &
                        'do gather', lunprint)
      call timer_free(modmo_timers(MODMO_TIMER_DO_GATHER), ierr)
      if (ierr /= 0) call pc_error('modmo_wrapup: timer_free failed')

      call timer_report(modmo_timers(MODMO_TIMER_RT), &
                        'tmap  cerveny up', lunprint)
      call timer_free(modmo_timers(MODMO_TIMER_RT), ierr)
      if (ierr /= 0) call pc_error('modmo_wrapup: timer_free failed')

      call timer_report(modmo_timers(MODMO_TIMER_TD), &
                        'time-to-depth', lunprint)
      call timer_free(modmo_timers(MODMO_TIMER_TD), ierr)
      if (ierr /= 0) call pc_error('modmo_wrapup: timer_free failed')

      call timer_report(modmo_timers(MODMO_TIMER_DT), &
                        'depth-to-time', lunprint)
      call timer_free(modmo_timers(MODMO_TIMER_DT), ierr)
      if (ierr /= 0) call pc_error('modmo_wrapup: timer_free failed')

   end subroutine modmo_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module modmo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

