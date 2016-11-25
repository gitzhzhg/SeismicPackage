!<CPS_v1 type="PRIMITIVE"/>
!
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
!
!<brief_doc>
!------------------------------------------------------------------------------
!                         C P S   P R I M I T I V E 
!
! Name       : SYNVOFZ
! Category   : Synthetics
! Written    : 2006-04-04   by: Douglas Hanson
! Revised    : 2007-01-25   by: Douglas Hanson Fix RAY_TOT name bug.
! Maturity   : beta
! Purpose    : Generate synthetic traces locations.
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
!------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE        2*NDPT     small   amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!    NTR ignored on input because this is a trace supplying process.
!
! Upon output, NTR will have one of these values:
!    NTR = 1- GROUP_SIZE   if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  SYNVOFZ used for velocity attributes for synthetic generation.
!
!
!------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
!  4  2007-01-25  Douglas Hanson Fix RAY_TOT name bug.
!  3  2007-01-23  Douglas Hanson Fix n0_tab mem alloc.
!  2  2007-01-03  Douglas Hanson New layout.
!  1  2006-04-04  Douglas Hanson Original version.
!
!------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>

!<int_calling_doc>
!------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!
! V(z) velocity reaytracing.
!
! OPT_VOFZ=`CC OPT_VEL~~~=`CCCCCCCCCC  RAY_TOT=`IIIIIIIII 
!
! RAY_DIST_TOT=`IIIIIIIII   RAY_DIST_INC=`FFFFFFFFF
! RAY_DIST_MAX=`FFFFFFFFF   RAY_DIST_MIN=`FFFFFFFFF 
!
! RAY_DEP_TOT =`IIIIIIIII   RAY_DEP_INC =`FFFFFFFFF
! RAY_DEP_MIN =`FFFFFFFFF   RAY_DEP_MAX =`FFFFFFFFF 
!
! CONST_VEL=`FFFFFFF DEPTH_VEL=`FFFFFFFF GRAD_VEL =`FFFFFFFF V_GAT_TIG=`FFFFFFFF
!
! Select PATH_VEL [PATH_VEL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="V_GAT_TIG">
!<Tip> Ratio of source vleocity to receiver Vs / Vr. </Tip>
! Default = 1
! Allowed = real > 0.
! Spike will use a ratio of V_GAT_TIG for the source and receiver velocities
! when computeing the travel times.
! The source ray paths will use the input velocity field.
! The receiver ray paths will use the input velocity field divided by V_GAT_TIG.
!</Help>
!
!<Help KEYWORD="RAY_TOT">
!<Tip> Number of rays traced. </Tip>
! Default = 1001
! Allowed = int > 0
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DIST_TOT">
!<Tip> Ray table horizontal distance number. </Tip>
! Default = 101
! Allowed = int > 0.
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DIST_MIN">
!<Tip> Ray table horizontal distance minimum. </Tip>
! Default = 0.
! Allowed = 0.
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DIST_MAX">
!<Tip> Ray table horizontal distance maximum. </Tip>
! Default = 10000.
! Allowed = real > 0.
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DIST_INC">
!<Tip> Ray table horizontal distance increment. </Tip>
! Default = 100.
! Allowed = real > 0.
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DEP_TOT">
!<Tip> Num ray table vertical distance nodes. </Tip>
! Default = 101
! Allowed = int > 0
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DEP_MIN">
!<Tip> Ray table vertical distance maximum. </Tip>
! Default = 10000.
! Allowed = real > 0.
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DEP_MAX">
!<Tip> Ray table vertical distance maximum. </Tip>
! Default = 10000.
! Allowed = real > 0.
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="RAY_DEP_INC">
!<Tip> Ray table vertical distance increment. </Tip>
! Default = 100.
! Allowed = real > 0.
! Spike  will tabulate rays on grid defined by
! RAY_DIST_TOT, RAY_DIST_MIN, RAY_DIST_MAX, RAY_DIST_INC,
! RAY_DEP_TOT,  RAY_DEP_MIN,  RAY_DEP_MAX,  RAY_DEP_INC
! By traceing RAY_TOT rays.
!</Help>
!
!<Help KEYWORD="OPT_VOFZ">
!<Tip> Whether to use V(Z) raytracing through an interval velocity model. </Tip>
! Allowed = NO   Do not use V(Z) raytracing through an interval velocity model.
! Default = NO
! Allowed = NO   Do not use V(Z) raytracing through an interval velocity model.
! Allowed = YES  Do     use V(Z) raytracing through an interval velocity model.
!</Help>
!
!<Help KEYWORD="OPT_VEL">
!<Tip> Whether to use constant or spatialy varying velocity. </Tip>
! Default = PATH
! Allowed = CONSTANT  Use a constant velocity value of CONST_VEL.
! Allowed = PATH      Use a spatialy varying velocity defined by PATH_VEL.
! If OPT_VEL=CONSTANT or PATH_VEL=NONE
! Spike will use a velocity ( Z - DPETH_VEL ) * GRAD_VEL + CONST_VEL
!</Help>
!
!<Help KEYWORD="CONST_VEL">
!<Tip> Constant velocity to use to set velocity without a velocity file. </Tip>
! Default = 1
! Allowed = real > 0.
! If OPT_VEL=CONSTANT or PATH_VEL=NONE
! Spike will use a velocity ( Z - DPETH_VEL ) * GRAD_VEL + CONST_VEL
!
!</Help>
!
!<Help KEYWORD="DEPTH_VEL">
!<Tip> Velocity origin to use to set velocity without a velocity file. </Tip>
! Default = 0.
! Allowed = real > 0.
! If OPT_VEL=CONSTANT or PATH_VEL=NONE
! Spike will use a velocity ( Z - DPETH_VEL ) * GRAD_VEL + CONST_VEL
!</Help>
!
!<Help KEYWORD="GRAD_VEL">
!<Tip> Velocity gradient to use to set velocity without a velocity file. </Tip>
! Default = 0.
! Allowed = real
! If OPT_VEL=CONSTANT or PATH_VEL=NONE
! Spike will use a velocity ( Z - DPETH_VEL ) * GRAD_VEL + CONST_VEL
!</Help>
!
!<Help KEYWORD="PATH_VEL">
!<Tip> Velocity model file name. </Tip>
! Default = NONE
! Allowed = character string
!</Help>
!
!<Help KEYWORD="SELECT_PATH_VEL">
!<Tip> Velocity model file name. </Tip>
! Default = NONE
! Allowed = character string
!</Help>
!
!</HelpSection>
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!
module synvofz_module
  !
  ! Module references
  !
  use amod_module
  use cio_module
  use getlun_module
  use grid_module
  use cpucount_module
  use cpsio_module
  use datumgrid_module
  use headsave_module
  use interpolate_module
  use matfun_module
  use memfun_module
  use migfun_module
  use named_constants_module
  use pathchoose_module
  use pathcheck_module
  use pattern_module
  use pc_module
  use pcpsx_module
  use string_module
  use timeglob_module
  use velgrid_module
  use wavelet_module
  use zoeppritz_module
  !
  implicit none
  !
  !private
  !
  public :: synvofz_create
  public :: synvofz_delete
  public :: synvofz_initialize
  public :: synvofz_get
  public :: synvofz_put
  public :: synvofz_verify
  public :: synvofz_read_velocity_file_0
  public :: synvofz_read_velocity_file
  !
  ! rcs identifier string
  !
  character(len=100),public,save :: synvofz_ident = &
  "$Id: synvofz.f90,v 1.4 2007/01/26 14:22:05 Hanson beta sps $"
  !
  integer,          parameter :: n_yes_no = 2
  character(len=3), save      :: c_yes_no ( n_yes_no ) &
  = (/'YES', 'NO '/)
  !
  integer,          parameter :: n_opt_const = 2
  character(len=8), save      :: c_opt_const ( n_opt_const ) &
  = (/'CONSTANT', 'PATH    '/)
  !
  type, public :: synvofz_struct
    !
    !private
    !public
    !
    integer                                 :: n0_vel
    integer                                 :: nx_vel
    real                                    :: x0_vel
    real                                    :: x1_vel
    real                                    :: dx_vel
    integer                                 :: ny_vel
    real                                    :: y0_vel
    real                                    :: y1_vel
    real                                    :: dy_vel
    integer                                 :: nz_vel
    real                                    :: z0_vel
    real                                    :: z1_vel
    real                                    :: dz_vel
    real,                           pointer :: rv_vel ( :,:,: )
    real                                    :: rv_min
    real                                    :: rv_max
    !
    real                                    :: rv_scale(2)
    integer                                 :: i3_norm
    integer                                 :: i3_turn
    integer                                 :: n3_tab
    integer                                 :: i4_gat
    integer                                 :: i4_tig
    integer                                 :: n4_tab
    integer                                 :: nx_tab
    real                                    :: x0_tab
    real                                    :: x1_tab
    real                                    :: dx_tab
    integer                                 :: nz_tab
    real                                    :: z0_tab
    real                                    :: z1_tab
    real                                    :: dz_tab
    real,                           pointer :: rx_tab ( : )
    real,                           pointer :: rz_tab ( : )
    real,                           pointer :: rs_tab ( : )
    real,                           pointer :: rv_tab ( : )
    real,                           pointer :: rg_tab ( : )
    real,                           pointer :: dg_tab ( : )
    integer,                        pointer :: n0_tab ( :, :, : )
    integer,                        pointer :: i1_tab ( :, :, : )
    integer,                        pointer :: i2_tab ( :, :, : )
    real,                           pointer :: rt_tab ( :, :, :, : )
    real,                           pointer :: ra_tab ( :, :, :, : )
    real,                           pointer :: rf_tab ( :, :, :, : )
    real,                           pointer :: rp_tab ( :, :, :, : )
    real                                    :: rs_min
    real                                    :: rs_max
    !
    character(len=filename_length)          :: path_vel
    type ( pathchoose_struct ),     pointer :: select_path_vel
    !
    real                                    :: v_gat_tig
    !
    integer                                 :: ray_tot
    !
    integer                                 :: ray_dist_tot
    real                                    :: ray_dist_min
    real                                    :: ray_dist_max
    real                                    :: ray_dist_inc
    !
    integer                                 :: ray_dep_tot
    real                                    :: ray_dep_min
    real                                    :: ray_dep_max
    real                                    :: ray_dep_inc
    !
    logical                                 :: opt_vofz
    character(len=8)                        :: opt_vel
    real                                    :: depth_vel
    real                                    :: const_vel
    real                                    :: grad_vel
    !
    integer                                 :: hdr_x  ! x header iindex
    integer                                 :: hdr_y  ! y header iindex
    real                                    :: rx_scl ! x scale factor
    real                                    :: ry_scl ! y scale factor
    !
    real                                    :: rv_gat
    real                                    :: rv_tig
    !
    integer                                 :: mp_ray ! number of rays
    !
    integer                                 :: nh_inp
    integer                                 :: nt_glb
    real                                    :: t0_glb
    real                                    :: t1_glb
    real                                    :: dt_glb
    !
    integer                                 :: ipn 
    character(len=32)                       :: c_title 
    !
    type ( grid_struct )                    :: grid_obj       ! trans grid
    !
  end type synvofz_struct
  !
  contains
  !
  subroutine synvofz_create ( v, c_title, i_err )
    !
    ! Create a synvofz structure
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    character(len=*),         intent(in   ) :: c_title 
    integer,                  intent(inout) :: i_err
    !
    i_err = 0
    !
    ! allocate the structure
    !
    allocate ( v )
    !
    ! initialize coefficients
    !
    call synvofz_initialize ( v )
    !
    v%c_title = c_title
    !
    call pathchoose_create ( v%Select_path_vel, 'path_vel', '*' )
    !
    return
    !
  end subroutine synvofz_create
  !
  subroutine synvofz_delete ( v )
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    !
    ! Begin synvofz_delete
    !
    call memfun_del ( v%rv_vel )
    call memfun_del ( v%rx_tab )
    call memfun_del ( v%rz_tab )
    call memfun_del ( v%rs_tab )
    call memfun_del ( v%rv_tab )
    call memfun_del ( v%rg_tab )
    call memfun_del ( v%dg_tab )
    call memfun_del ( v%n0_tab )
    call memfun_del ( v%i1_tab )
    call memfun_del ( v%i2_tab )
    call memfun_del ( v%rt_tab )
    call memfun_del ( v%ra_tab )
    call memfun_del ( v%rf_tab )
    call memfun_del ( v%rp_tab )
    !
    if ( associated        ( v%select_path_vel ) ) &
    call pathchoose_delete ( v%select_path_vel )
    !
    deallocate ( v )
    !
    return
    !
  end subroutine synvofz_delete
  !
  subroutine synvofz_nullify ( v )
    !
    ! Nullify synvofz pointers
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    !
    call memfun_nul ( v%rv_vel )
    call memfun_nul ( v%rx_tab )
    call memfun_nul ( v%rz_tab )
    call memfun_nul ( v%rs_tab )
    call memfun_nul ( v%rv_tab )
    call memfun_nul ( v%rg_tab )
    call memfun_nul ( v%dg_tab )
    call memfun_nul ( v%n0_tab )
    call memfun_nul ( v%i1_tab )
    call memfun_nul ( v%i2_tab )
    call memfun_nul ( v%rt_tab )
    call memfun_nul ( v%ra_tab )
    call memfun_nul ( v%rf_tab )
    call memfun_nul ( v%rp_tab )
    !
    nullify ( v%select_path_vel ) 
    !
    return
    !
  end subroutine synvofz_nullify 
  !
  subroutine synvofz_initialize ( v )
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    !
    call synvofz_nullify ( v )
    !
    call synvofz_initialize_0 ( v )
    !
    ! initialize parameters
    !
    ! get the current globals
    !
    call timeglob_get ( v%nt_glb, v%t0_glb, v%t1_glb, v%dt_glb )
    !
    v%path_vel  = pathcheck_empty ! velocity file
    !
    v%opt_vofz      = .false.
    v%opt_vel       = 'PATH'
    !
    v%ray_tot       = 1001
    v%ray_dist_tot  = 101
    v%ray_dist_min = 0.
    v%ray_dist_max = 10000.
    v%ray_dist_inc = 100.
    v%ray_dep_tot  = 101
    v%ray_dep_min  = 0.
    v%ray_dep_max  = 10000.
    v%ray_dep_inc  = 100.
    !
    v%v_gat_tig     = 1.    ! source to receiver velocity ratio
    v%depth_vel     = 0.    ! constant velocity depth
    v%const_vel     = 2000. ! constant velocity
    v%grad_vel      = 0.    ! constant velocity gradient
    !
    return
    !
  end subroutine synvofz_initialize
  !
  subroutine synvofz_initialize_0 ( v )
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    !
    ! Begin synvofz_initialize
    !
    call memfun_init ( v%n0_vel ) 
    call memfun_init ( v%nx_vel ) 
    call memfun_init ( v%x0_vel ) 
    call memfun_init ( v%x1_vel ) 
    call memfun_init ( v%dx_vel ) 
    call memfun_init ( v%ny_vel ) 
    call memfun_init ( v%y0_vel ) 
    call memfun_init ( v%y1_vel ) 
    call memfun_init ( v%dy_vel ) 
    call memfun_init ( v%nz_vel ) 
    call memfun_init ( v%z0_vel ) 
    call memfun_init ( v%z1_vel ) 
    call memfun_init ( v%dz_vel ) 
    call memfun_init ( v%rv_vel ) 
    call memfun_init ( v%rv_min ) 
    call memfun_init ( v%rv_max ) 
    call memfun_init ( v%i3_norm ) 
    call memfun_init ( v%i3_turn ) 
    call memfun_init ( v%n3_tab )
    call memfun_init ( v%i4_gat ) 
    call memfun_init ( v%i4_tig ) 
    call memfun_init ( v%n4_tab )
    call memfun_init ( v%nx_tab )
    call memfun_init ( v%x0_tab )
    call memfun_init ( v%x1_tab )
    call memfun_init ( v%dx_tab )
    call memfun_init ( v%nz_tab )
    call memfun_init ( v%z0_tab )
    call memfun_init ( v%z1_tab )
    call memfun_init ( v%dz_tab )
    call memfun_init ( v%rx_tab ) 
    call memfun_init ( v%rz_tab ) 
    call memfun_init ( v%rs_tab ) 
    call memfun_init ( v%rv_tab ) 
    call memfun_init ( v%rg_tab ) 
    call memfun_init ( v%dg_tab ) 
    call memfun_init ( v%n0_tab )
    call memfun_init ( v%i1_tab )
    call memfun_init ( v%i2_tab )
    call memfun_init ( v%rt_tab )
    call memfun_init ( v%ra_tab )
    call memfun_init ( v%rf_tab )
    call memfun_init ( v%rp_tab )
    call memfun_init ( v%rs_min ) 
    call memfun_init ( v%rs_max ) 
    call memfun_init ( v%path_vel ) 
    call memfun_init ( v%v_gat_tig ) 
    call memfun_init ( v%ray_tot ) 
    call memfun_init ( v%ray_dist_tot ) 
    call memfun_init ( v%ray_dist_min ) 
    call memfun_init ( v%ray_dist_max ) 
    call memfun_init ( v%ray_dist_inc ) 
    call memfun_init ( v%ray_dep_tot ) 
    call memfun_init ( v%ray_dep_min ) 
    call memfun_init ( v%ray_dep_max ) 
    call memfun_init ( v%ray_dep_inc ) 
    call memfun_init ( v%mp_ray ) 
    call memfun_init ( v%opt_vofz ) 
    call memfun_init ( v%opt_vel ) 
    call memfun_init ( v%depth_vel ) 
    call memfun_init ( v%const_vel ) 
    call memfun_init ( v%grad_vel ) 
    call memfun_init ( v%hdr_x ) 
    call memfun_init ( v%hdr_y ) 
    call memfun_init ( v%rx_scl ) 
    call memfun_init ( v%ry_scl ) 
    call memfun_init ( v%nh_inp ) 
    call memfun_init ( v%nt_glb ) 
    call memfun_init ( v%t0_glb ) 
    call memfun_init ( v%t1_glb ) 
    call memfun_init ( v%dt_glb ) 
    call memfun_init ( v%ipn ) 
    call memfun_init ( v%c_title ) 
    !
    return
    !
  end subroutine synvofz_initialize_0
  !
  subroutine synvofz_get ( v )
    !
    ! get synvofz parameters
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    !
    ! get the rotation grid object
    !
    v%ipn = pc_get_ipn()
    !
    call pc_get_global ( 'grid' , v%grid_obj )
    !
    call timeglob_get ( v%nt_glb, v%t0_glb, v%t1_glb, v%dt_glb )
    !
    call pc_get_global ( 'nwih',  v%nh_inp )
    !
    call pc_get ( 'path_vel',       v%path_vel         )
    !
    call pc_get ( 'ray_tot',        v%ray_tot          )
    call pc_get ( 'ray_dist_tot',   v%ray_dist_tot     )
    call pc_get ( 'ray_dist_min',   v%ray_dist_min     )
    call pc_get ( 'ray_dist_max',   v%ray_dist_max     )
    call pc_get ( 'ray_dist_inc',   v%ray_dist_inc     )
    call pc_get ( 'ray_dep_tot',    v%ray_dep_tot      )
    call pc_get ( 'ray_dep_min',    v%ray_dep_min      )
    call pc_get ( 'ray_dep_max',    v%ray_dep_max      )
    call pc_get ( 'ray_dep_inc',    v%ray_dep_inc      )
    call pc_get ( 'v_gat_rcv',      v%v_gat_tig        )
    call pc_get ( 'v_gat_tig',      v%v_gat_tig        )
    call pc_get ( 'opt_vofz',       v%opt_vofz         )
    call pc_get ( 'opt_vel',        v%opt_vel          )
    call pc_get ( 'depth_vel ',     v%depth_vel        )
    call pc_get ( 'const_vel',      v%const_vel        )
    call pc_get ( 'grad_vel',       v%grad_vel         )
    !
    return
    !
  end subroutine synvofz_get
  !
  subroutine synvofz_put ( v )
    !
    ! put synvofz parameters
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    !
    call amod_line_feed ( 'synvofz_ray' )
    !
    call pc_put ( 'opt_vofz',       v%opt_vofz         )
    call pc_put ( 'ray_tot',        v%ray_tot          )
    !
    call amod_line_feed ( 'synvofz_ray_dist' )
    !
    call pc_put ( 'ray_dist_tot',   v%ray_dist_tot     )
    call pc_put ( 'ray_dist_min',   v%ray_dist_min     )
    call pc_put ( 'ray_dist_max',   v%ray_dist_max     )
    call pc_put ( 'ray_dist_inc',   v%ray_dist_inc     )
    !
    call amod_line_feed ( 'synvofz_ray_dep' )
    !
    call pc_put ( 'ray_dep_tot',    v%ray_dep_tot      )
    call pc_put ( 'ray_dep_min',    v%ray_dep_min      )
    call pc_put ( 'ray_dep_max',    v%ray_dep_max      )
    call pc_put ( 'ray_dep_inc',    v%ray_dep_inc      )
    !
    call amod_line_feed ( 'synvofz_vel' )
    !
    call pc_put ( 'opt_vel',        v%opt_vel          )
    call pc_put ( 'path_vel',       v%path_vel         )
    call pc_put ( 'depth_vel ',     v%depth_vel        )
    call pc_put ( 'const_vel',      v%const_vel        )
    call pc_put ( 'grad_vel',       v%grad_vel         )
    call pc_put ( 'v_gat_tig',      v%v_gat_tig        )
    !
    return
    !
  end subroutine synvofz_put
  !
  subroutine synvofz_verify ( v )
    !
    ! verify synvofz parameters
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    !
    integer                                 :: i_err
    integer                                 :: i_stat
    !
    i_err = 0
    !
    v%ray_dist_min = 0.
    call pc_put_options_field ( 'opt_vofz', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_vel', c_opt_const, n_opt_const )
    !
    if ( string_upper_compare ( v%opt_vel, 'CONSTANT' ) ) &
                                v%path_vel = pathcheck_empty
    !
    if ( v%opt_vofz ) &
    i_stat = pattern_stop2 ( 'spike:', .true., &
v%ray_dist_min, v%ray_dist_inc, v%ray_dist_max, v%ray_dist_tot, &
 'ray_dist_min', 'ray_dist_inc', 'ray_dist_max', 'ray_dist_tot', &
pc_verify_scalar ( 'ray_dist_min' ), pc_verify_scalar ( 'ray_dist_inc' ), &
pc_verify_scalar ( 'ray_dist_max' ), pc_verify_scalar ( 'ray_dist_tot' )  )
    !
    if ( v%opt_vofz ) &
    i_stat = pattern_stop2 ( 'spike:', .true., &
v%ray_dep_min, v%ray_dep_inc, v%ray_dep_max, v%ray_dep_tot, &
 'ray_dep_min', 'ray_dep_inc', 'ray_dep_max', 'ray_dep_tot', &
pc_verify_scalar ( 'ray_dep_min' ), pc_verify_scalar ( 'ray_dep_inc' ), &
pc_verify_scalar ( 'ray_dep_max' ), pc_verify_scalar ( 'ray_dep_tot' )  )
    !
    call pathcheck ( 'path_vel', v%path_vel, required=.false. )
    !
    call string_replace_zeroes ( v%path_vel )
    !
    if ( len_trim ( v%path_vel ) .lt. 1 ) v%path_vel = pathcheck_empty
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_verify ", &
    & /," during xxx " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_verify " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine synvofz_verify
  !
  subroutine synvofz_prep ( v, hdr_x, hdr_y, rx_scl, ry_scl, i_err )
    !
    ! prep  synvofz parameters
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! synvofz structure
    integer,                  intent(in   ) :: hdr_x  ! x header iindex
    integer,                  intent(in   ) :: hdr_y  ! y header iindex
    real,                     intent(in   ) :: rx_scl ! x scale factor
    real,                     intent(in   ) :: ry_scl ! y scale factor
    integer,                  intent(  out) :: i_err
    !
    i_err = 0
    !
    if ( pc_do_not_process_traces()  ) return
    !
    write ( pc_get_lun(), '(  &
    & /, " synvofz_update " ,/ ," REVISION: ", &
    & " 4  2007-01-25  Douglas Hanson Fix RAY_TOT name bug. " &
    & )')
    !
    v%hdr_x  = hdr_x  ! x header iindex
    v%hdr_y  = hdr_y  ! y header iindex
    v%rx_scl = rx_scl ! x scale factor
    v%ry_scl = ry_scl ! y scale factor
    !
    ! read the velocity file
    !
    call synvofz_read_velocity_file_0 ( v, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_prep  ", &
    & /," during synvofz_read_velocity_file_0 " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_prep  " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine synvofz_prep
  !
  subroutine synvofz_read_velocity_file_0 ( v, i_err )
    !
    ! Arguments
    !
    type ( synvofz_struct ),        pointer :: v ! spike structure
    integer,                  intent(  out) :: i_err
    !
    ! Local variables
    !
    integer                                 :: i4_tab
    integer                                 :: iz_tab
    !
    i_err = 0
    !
    call synvofz_read_velocity_file ( &
                                      v%path_vel, v%opt_vofz, &
                                      v%depth_vel, v%const_vel, v%grad_vel, &
                                      v%hdr_x, v%rx_scl, &
                                      v%hdr_y, v%ry_scl, &
                                      v%nx_vel, v%x0_vel, v%dx_vel, &
                                      v%ny_vel, v%y0_vel, v%dy_vel, &
                                      v%nz_vel, v%z0_vel, v%dz_vel, &
                                      v%rv_vel, &
                                      i_err &
                                    )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    v%x1_vel = ( v%nx_vel - 1 ) * v%dx_vel + v%x0_vel
    !
    v%y1_vel = ( v%nx_vel - 1 ) * v%dx_vel + v%x0_vel
    !
    v%z1_vel = ( v%nz_vel - 1 ) * v%dz_vel + v%z0_vel
    !
    v%x1_vel = v%x1_vel * v%rx_scl
    !
    v%y1_vel = v%y1_vel * v%ry_scl
    !
    v%rv_min = minval ( v%rv_vel )
    !
    v%rv_max = maxval ( v%rv_vel )
    !
    ! compute the raytracing v(z) coefficients
    !
    v%mp_ray = v%ray_tot
    !
    v%nx_tab = v%ray_dist_tot
    !
    v%x0_tab = v%ray_dist_min
    !
    v%x1_tab = v%ray_dist_max
    !
    v%dx_tab = v%ray_dist_inc
    !
    v%nx_tab = max ( 1, nint ( ( v%x1_tab - v%x0_tab ) / v%dx_tab ) )
    !
    v%x1_tab = ( v%nx_tab - 1 ) * v%dx_tab + v%x0_tab
    !
    v%nz_tab = v%ray_dep_tot
    !
    v%z0_tab = v%ray_dep_min
    !
    v%z1_tab = v%ray_dep_max
    !
    v%dz_tab = v%ray_dep_inc
    !
    v%nz_tab = max ( 1, nint ( ( v%z1_tab - v%z0_tab ) / v%dz_tab ) )
    !
    v%z1_tab = ( v%nz_tab - 1 ) * v%dz_tab + v%z0_tab
    !
    v%n3_tab = 2
    !
    v%i3_norm = 1
    !
    v%i3_turn = 2
    !
    v%n4_tab = 2
    !
    v%i4_gat = 1
    !
    v%i4_tig = 2
    !
    v%rv_scale ( v%i4_gat ) = 1.
    !
    v%rv_scale ( v%i4_tig ) = v%v_gat_tig
    !
    call memfun_all ( v%rx_tab, v%nx_tab, 'rx_tab', i_err )
    call memfun_all ( v%rz_tab, v%nz_tab, 'rz_tab', i_err )
    call memfun_all ( v%rs_tab, v%nz_tab, 'rs_tab', i_err )
    call memfun_all ( v%rv_tab, v%nz_tab, 'rv_tab', i_err )
    call memfun_all ( v%rg_tab, v%nz_tab, 'rg_tab', i_err )
    call memfun_all ( v%dg_tab, v%nz_tab, 'dg_tab', i_err )
    call memfun_all ( &
    v%n0_tab, v%nz_tab,           v%n3_tab, v%n4_tab, 'n0_tab', i_err )
    call memfun_all ( &
    v%i1_tab, v%nz_tab,           v%n3_tab, v%n4_tab, 'i1_tab', i_err )
    call memfun_all ( &
    v%i2_tab, v%nz_tab,           v%n3_tab, v%n4_tab, 'i2_tab', i_err )
    call memfun_all ( &
    v%rt_tab, v%nx_tab, v%nz_tab, v%n3_tab, v%n4_tab, 'rt_tab', i_err )
    call memfun_all ( &
    v%ra_tab, v%nx_tab, v%nz_tab, v%n3_tab, v%n4_tab, 'ra_tab', i_err )
    call memfun_all ( &
    v%rf_tab, v%nx_tab, v%nz_tab, v%n3_tab, v%n4_tab, 'rf_tab', i_err )
    call memfun_all ( &
    v%rp_tab, v%nx_tab, v%nz_tab, v%n3_tab, v%n4_tab, 'rp_tab', i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    call interpolate_3d_to_1d ( &
                                v%nz_vel, v%z0_vel, v%dz_vel, &
                                v%nx_vel, v%x0_vel, v%dx_vel, &
                                v%ny_vel, v%y0_vel, v%dy_vel, &
                                v%rv_vel, &
                                v%nz_tab, v%z0_tab, v%dz_tab, &
                                0., &
                                0., &
                                v%rv_tab &
                              )
    !
    v%rs_tab = 1. / v%rv_tab
    !
    v%rs_min = minval ( v%rs_tab )
    !
    v%rs_max = maxval ( v%rs_tab )
    !
    v%rg_tab  ( 1 ) = 0.
    !
    v%dg_tab  ( 1 ) = 0.
    !
    ! compute the time and depth and time incrment between each node
    !
    call matfun_line ( v%nx_tab, v%rx_tab, v%x0_tab, v%dx_tab )
    !
    call matfun_line ( v%nz_tab, v%rz_tab, v%z0_tab, v%dz_tab )
    !
    do_iz_tab : do iz_tab = 2 , v%nz_tab
      !
          v%dg_tab ( iz_tab     ) = .5 &
      * ( v%rs_tab ( iz_tab - 1 ) + v%rs_tab ( iz_tab     ) ) &
      * ( v%rz_tab ( iz_tab     ) - v%rz_tab ( iz_tab - 1 ) )
      !
          v%rg_tab ( iz_tab     ) = &
          v%rg_tab ( iz_tab - 1 ) &
        + v%dg_tab ( iz_tab     )
      !
    end do do_iz_tab
    !
    write ( pc_get_lun(), '( &
    & /, " synvofz_read_velocity_file ", &
    & /, " nx_vel=",i8," x0_vel=",g10.4," x1_vel=",g10.4," dx_vel=",g10.4, &
    & /, " ny_vel=",i8," y0_vel=",g10.4," y1_vel=",g10.4," dy_vel=",g10.4, &
    & /, " nz_vel=",i8," z0_vel=",g10.4," z1_vel=",g10.4," dz_vel=",g10.4, &
    & /, " nx_tab=",i8," x0_tab=",g10.4," x1_tab=",g10.4," dx_tab=",g10.4, &
    & /, " nz_tab=",i8," z0_tab=",g10.4," z1_tab=",g10.4," dz_tab=",g10.4, &
    & /, " rv_min=",g10.4," rv_max=",g10.4, &
    & /, " rs_min=",g10.4," rs_max=",g10.4 &
    & )') &
    v%nx_vel, v%x0_vel, v%x1_vel, v%dx_vel, &
    v%ny_vel, v%y0_vel, v%y1_vel, v%dy_vel, &
    v%nz_vel, v%z0_vel, v%z1_vel, v%dz_vel, &
    v%nx_tab, v%x0_tab, v%x1_tab, v%dx_tab, &
    v%nz_tab, v%z0_tab, v%z1_tab, v%dz_tab, &
    v%rv_min, v%rv_max, &
    1./v%rs_min, 1./v%rs_max
    !
    write ( pc_get_lun(), '( &
    & /, " iz_tab  rz_tab     rg_tab     rv_tab     rs_tab  " &
    & )')
    !
    write ( pc_get_lun(), '( &
    & 1x, i8, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
    & )') &
    (          iz_tab, &
      v%rz_tab(iz_tab), v%rg_tab(iz_tab), &
      v%rv_tab(iz_tab), v%rs_tab(iz_tab), &
               iz_tab = 1 , v%nz_tab )
    !
    do_i4_tab : do i4_tab = 1 , v%n4_tab
      !
      call synvofz_raytrace_vofz ( &
                                   v%mp_ray, v%rv_scale(i4_tab), &
                                   v%nx_tab, v%x0_tab, v%dx_tab, &
                                   v%nz_tab, v%rz_tab, v%rs_tab, &
                                   v%n0_tab(  :,:,i4_tab), &
                                   v%i1_tab(  :,:,i4_tab), &
                                   v%i2_tab(  :,:,i4_tab), &
                                   v%rt_tab(:,:,:,i4_tab), &
                                   v%ra_tab(:,:,:,i4_tab), &
                                   v%rf_tab(:,:,:,i4_tab), &
                                   v%rp_tab(:,:,:,i4_tab), &
                                   i_err &
                                 )
      !
      !print'(" i3=",i8," t=",g10.4,1x,g10.4," p=",g10.4,1x,g10.4,&
      !& " a=",g10.4,1x,g10.4)',&
      !i4_tab, &
      !minval(v%rt_tab(:,:,1,i4_tab)), maxval(v%rt_tab(:,:,1,i4_tab)), &
      !minval(v%rp_tab(:,:,1,i4_tab)), maxval(v%rp_tab(:,:,1,i4_tab)), &
      !minval(v%ra_tab(:,:,1,i4_tab)), maxval(v%ra_tab(:,:,1,i4_tab))
      !
    end do do_i4_tab
    !
    if ( i_err .ne. 0 ) go to 996
    !
    return
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file ", &
    & /," during synref_raytrace_vofz " &
    & )')
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file ", &
    & /," during memory allocation " &
    & )')
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file ", &
    & /," during synvofz_read_velocity_file " &
    & )')
    !
    i_err = -1
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine synvofz_read_velocity_file_0
  !
  subroutine synvofz_read_velocity_file ( &
                                          path_vel, opt_vofz, &
                                          depth_vel, const_vel, grad_vel, &
                                          hdr_x, rx_scl, &
                                          hdr_y, ry_scl, &
                                          nx_vel, x0_vel, dx_vel, &
                                          ny_vel, y0_vel, dy_vel, &
                                          nz_vel, z0_vel, dz_vel, &
                                          rv_vel, &
                                          i_err &
                                        )
    !
    ! Arguments
    !
    character(len=*),         intent(in   ) :: path_vel
    real,                     intent(in   ) :: depth_vel
    real,                     intent(in   ) :: const_vel
    real,                     intent(in   ) :: grad_vel
    logical,                  intent(in   ) :: opt_vofz
    integer,                  intent(in   ) :: hdr_x
    real,                     intent(in   ) :: rx_scl
    integer,                  intent(in   ) :: hdr_y
    real,                     intent(in   ) :: ry_scl
    integer,                  intent(inout) :: nx_vel
    real,                     intent(inout) :: x0_vel
    real,                     intent(inout) :: dx_vel
    integer,                  intent(inout) :: ny_vel
    real,                     intent(inout) :: y0_vel
    real,                     intent(inout) :: dy_vel
    integer,                  intent(inout) :: nz_vel
    real,                     intent(inout) :: z0_vel
    real,                     intent(inout) :: dz_vel
    real,                           pointer :: rv_vel(:, :, :)
    integer,                  intent(  out) :: i_err
    !
    ! Local variables
    !
    integer                                 :: n0_vel
    integer                                 :: iz_vel
    real                                    :: rz_vel
    character(len=8)                        :: vel_type
    character(len=8)                        :: vel_parm
    !
    i_err = 0
    !
    !  initialize the vleocity grid values
    !
    nx_vel = 1
    !
    x0_vel = 0.
    !
    dx_vel = 1.
    !
    ny_vel = 1
    !
    y0_vel = 0.
    !
    dy_vel = 1.
    !
    nz_vel = 1
    !
    z0_vel = 0.
    !
    dz_vel = 1.
    !
    !  determine the size of the velocity file
    !  get on pe 0 and broadcast to all pes
    !
    if_path_vel_1 : &
    if ( .not. string_upper_compare ( path_vel, pathcheck_empty )  ) then
      !
      call velgrid_size_1 ( &
                            pc_get_lun(), 'spike velocity file', &
                            path_vel, vel_type, &
                            n0_vel, hdr_x,  hdr_y, &
                            nx_vel, x0_vel, dx_vel, &
                            ny_vel, y0_vel, dy_vel, &
                            nz_vel, z0_vel, dz_vel, &
                            i_err &
                          )
      if ( i_err .ne. 0 ) go to 997
      !
    end if if_path_vel_1
    !
    ! allocate the velocity memory
    !
    call memfun_all ( rv_vel, nz_vel, nx_vel, ny_vel, 'rv_vel', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    !  if the velocity file is not defined set the velocity equal to rv_vel
    !
    do_iz_vel : do iz_vel = 1 , nz_vel
      !
      rz_vel = ( iz_vel - 1 ) * dz_vel + z0_vel
      !
      rv_vel ( iz_vel, 1:nx_vel, 1:ny_vel )  = &
      max ( 0., rz_vel - depth_vel ) * grad_vel + const_vel
      !
    end do do_iz_vel
    !
    !  read the vertical velocity anisotropic coefficient from vx_file
    !  get on pe 0 and broadcast to all pes
    !
    xxif_opt_vofz : if ( opt_vofz ) then
      !
      vel_type = 'VZIN'
      !
    else xxif_opt_vofz
      !
      vel_type = 'VZRM'
      !
    end if xxif_opt_vofz
    !
    vel_parm = 'VELOCITY' ! interpolate velocity not slowness
    !
    print'(" synvofz_read_velocity_file vel_type=",a)', vel_type
    !
    if_path_vel_2 : &
    if ( .not. string_upper_compare ( path_vel, pathcheck_empty )  ) then
      !
      call velgrid_read ( &
                          pc_get_lun(), 'spike velocity file', &
                          path_vel, &
                          vel_type, vel_parm, &
                          hdr_x, hdr_y, &
                          nx_vel, x0_vel, dx_vel, &
                          ny_vel, y0_vel, dy_vel, &
                          nz_vel, z0_vel, dz_vel, &
                          rv_vel, &
                          i_err &
                        )
      !
      if ( i_err .ne. 0 ) go to 996
      !
    end if if_path_vel_2
    !
    !  scale the velocity x,y grid values to distance units
    !
    x0_vel = x0_vel * rx_scl
    !
    dx_vel = dx_vel * rx_scl
    !
    y0_vel = y0_vel * ry_scl
    !
    dy_vel = dy_vel * ry_scl
    !
    return
    !
  996 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file ", &
    & /," during velgrid_read " &
    & )')
    !
    go to 999
    !
  997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file ", &
    & /," during velgrid_size_1 " &
    & )')
    !
    go to 999
    !
  998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file ", &
    & /," during memory allocation " &
    & )')
    !
    i_err = -1
    !
    go to 999
    !
  999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synvofz_read_velocity_file " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine synvofz_read_velocity_file
  !
  subroutine synvofz_raytrace_vofz ( &
                                     mp_ray, rv_scale, &
                                     nx_tab, x0_tab, dx_tab, &
                                     nz_tab, rz_tab, rs_tab, &
                                     n0_tab, i1_tab, i2_tab, &
                                     rt_tab, ra_tab, rf_tab, rp_tab, &
                                     i_err &
                                   )
    !
    !  compute a v(z) travel time, amplitude, phase table
    !  note rv_tab is slowness
    !  use v(z) raytracing to trace np_ray down each of nt_tab depth steps
    !  traces a set of rays for the source   using slowness rs_ray
    !  note there is no anisotropic correction at this time 12-05-02
    !
    integer,          intent (in   ) :: mp_ray
    real,             intent (in   ) :: rv_scale
    integer,          intent (in   ) :: nx_tab
    real,             intent (in   ) :: x0_tab
    real,             intent (in   ) :: dx_tab
    integer,          intent (in   ) :: nz_tab
    real,             intent (in   ) :: rz_tab(:)
    real,             intent (in   ) :: rs_tab(:)
    integer,          intent (inout) :: n0_tab(:,:)
    integer,          intent (inout) :: i1_tab(:,:)
    integer,          intent (inout) :: i2_tab(:,:)
    real,             intent (inout) :: rt_tab(:,:,:)
    real,             intent (inout) :: ra_tab(:,:,:)
    real,             intent (inout) :: rf_tab(:,:,:)
    real,             intent (inout) :: rp_tab(:,:,:)
    !
    integer,          intent (inout) :: i_err        ! error flag 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                          :: np_ray ! number of rays
    double precision                 :: p0_ray ! min ray parameter value
    double precision                 :: dp_ray ! inc ray parameter value
    double precision                 :: p1_ray ! max ray parameter value
    !
    integer                          :: iz_dir
    integer                          :: iz_beg       ! beg z index
    integer                          :: iz_end       ! end z index
    integer                          :: iz_tab       ! vertical node index
    double precision                 :: dz_tab       ! z node increment
    !
    double precision                 :: bz_tab(nz_tab) ! source slowness
    double precision                 :: bs_tab(nz_tab) ! source slowness
    double precision                 :: bs_ray(nz_tab) ! source slowness
    double precision                 :: bv_ray(nz_tab) ! source velocity
    double precision                 :: bs_ave(nz_tab) ! source slowness
    double precision                 :: bv_ave(nz_tab) ! source velocity
    !
    integer                          :: nx_ray       ! number of radial points
    double precision                 :: x1_ray       ! max radial distance
    double precision                 :: dx_ray       ! inc radial distance
    !
    !
    integer                          :: i0_n_t
    integer                          :: n0_n_t(nz_tab,2) ! num   n,t p each z
    integer                          :: i1_n_t(nz_tab,2) ! first n,t p each z
    integer                          :: i2_n_t(nz_tab,2) ! last  n,t p each z
    !
    double precision                 :: bt_tab(nx_tab)
    double precision                 :: bp_tab(nx_tab)
    double precision                 :: bx_tab(nx_tab)
    double precision                 :: bx_ray(mp_ray) ! dist at ray end
    double precision                 :: bt_ray(mp_ray) ! time at ray end
    double precision                 :: bp_ray(mp_ray) ! ray parameter val
    !
    integer                          :: jx_tab
    integer                          :: ip_ray
    !
    integer                          :: np_beg
    integer                          :: np_end
    !
    integer                          :: j0_ray
    integer                          :: j1_ray
    integer                          :: j2_ray
    !
    integer                          :: j0_tab
    integer                          :: j1_tab
    integer                          :: j2_tab
    !
    integer                          :: j1_tmp
    integer                          :: j2_tmp
    !
    i_err = 0
    !
    ! initialize the table values
    !
    bz_tab(1:nz_tab) = rz_tab(1:nz_tab)
    !
    bs_tab(1:nz_tab) = rs_tab(1:nz_tab)
    !
    rt_tab = -1
    !
    ra_tab = 0.
    !
    rf_tab = 0.
    !
    rp_tab = 0.
    !
    ! get the interval slowness for the source, rs_ray
    ! rv_ray is the velocity
    ! the slownes is assumed to change linearly from node to node
    ! rs_ray(i) be the average slowness over the interval i to i+1
    ! rs_ray(i) is the source leg slowness
    !
    bs_ray ( 1:nz_tab ) = rv_scale * bs_tab ( 1:nz_tab )
    !
    bv_ray ( 1:nz_tab ) = 1.       / bs_ray ( 1:nz_tab )
    !
    ! fill in the first depth
    !
    ! set bx_tab the x tab node locations
    !
    do_jx_tab : do jx_tab = 1 , nx_tab
      !
      bx_tab ( jx_tab ) = ( jx_tab - 1 ) * dx_tab + x0_tab
      !
      rt_tab ( jx_tab, 1, 1 ) = bx_tab ( jx_tab ) * bs_ray ( 1 )
      !
    end do do_jx_tab
    !
    ra_tab ( 2:nx_tab, 1, 1 ) = 1. / rt_tab ( 2:nx_tab, 1, 1 )
    !
    ra_tab ( 1, 1, 1 ) = ra_tab ( 2, 1, 1 )
    !
    ! define the ray distance and ray parameter ranges
    ! x1_ray is the max distance we want the rays
    ! to span after the first depth step
    ! it is the maximum x,y table distance
    !
    dz_tab = bz_tab ( 2 ) - bz_tab ( 1 )
    !
    x1_ray = abs ( nx_tab * dx_tab )
    !
    dx_ray = dx_tab
    !
    nx_ray = int ( x1_ray / dx_ray ) + 2
    !
    x1_ray = ( nx_ray - 1 ) * dx_ray
    !
    np_ray = mp_ray
    !
    p1_ray = 1. / bv_ray(1)
    !
    !p1_ray = sin ( atan ( x1_ray / dz_tab ) ) / bv_ray(1)
    !
    p0_ray = 0.
    !
    dp_ray = ( p1_ray - p0_ray ) / ( np_ray - 1 )
    !
    p1_ray = p1_ray - dp_ray * 1.e-3
    !
    dp_ray = ( p1_ray - p0_ray ) / ( np_ray - 1 )
    !
    p1_ray = ( np_ray - 1 ) * dp_ray + p0_ray
    !
    !print'( &
    !& /, " synref_raytrace_vofz ", &
    !& /, " nz_tab=", i8, " dz_tab=", g12.6, &
    !& /, " rv_ray=", g12.6," v_gat_tig=", g12.6, &
    !& /, " nx_ray=", i8, " x1_ray=", g12.6, " dx_ray=", g12.6, &
    !& /, " np_ray=", i8, " p1_ray=", g12.6, " dp_ray=", g12.6, 1x, g12.6 &
    !& )', &
    !nz_tab, dz_tab, &
    !bv_ray(1), rv_scale, &
    !nx_ray, x1_ray, dx_ray, &
    !np_ray, p1_ray, dp_ray, &
    !sin ( atan ( x1_ray / dz_tab ) ) / bv_ray(1)
    !
    ! initalize the ray values
    !
    bx_ray = 0. ! radial dist ray has moved from source
    !
    bt_ray = 0. ! one way ray travel time from source
    !
    ! ray parameter values
    !
    do_ip_ray : do ip_ray = 1 , np_ray
      !
      bp_ray ( ip_ray ) = ( ip_ray - 1 ) * dp_ray
      !
    end do do_ip_ray
    !
    ! compute the ray parameter range for each depth
    ! and the turning depth for each ray parameter
    !
    bs_ave ( 1 ) = bs_ray ( 1 )
    !
    n0_n_t ( :, 1 ) = np_ray       ! num   p normal  waves
    !
    i1_n_t ( :, 1 ) = 1            ! first p normal  waves
    !
    i2_n_t ( :, 1 ) = np_ray       ! last  p normal  waves
    !
    n0_n_t ( :, 2 ) = 0            ! num   p normal  waves
    !
    i1_n_t ( :, 2 ) = np_ray + 1   ! first p turning waves
    !
    i2_n_t ( :, 2 ) = np_ray       ! last  p turning waves
    !
    do_iz_tab_1 : do iz_tab = 2 , nz_tab
      !
      ! p / v = sin angle v = slowness
      ! limit the range of p values to the critcal angle
      ! we assume bp_ray(i) = (i-1)*dp_ray+p0_ray
      !
      iz_beg = iz_tab - 1
      !
      iz_end = iz_tab
      !
      bs_ave ( iz_tab ) = .5 * ( bs_ray ( iz_beg ) + bs_ray ( iz_end ) )
      !
      np_beg = i2_n_t ( iz_beg, 1 )
      !
      np_end = min ( np_beg, int ( ( bs_ave(iz_tab) - p0_ray ) / dp_ray ) + 1 )
      !
      if ( bp_ray ( np_end ) .ge. bs_ave ( iz_tab ) ) &
      np_end = max ( 1, np_end - 1 )
      !
      i2_n_t ( iz_tab, 1 ) = np_end     ! last normal wave index
      !
      i1_n_t ( iz_tab, 2 ) = np_end + 1 ! first turning wave index
      !
      n0_n_t ( iz_tab, 1 ) = &
      i2_n_t ( iz_tab, 1 ) &
    - i1_n_t ( iz_tab, 1 ) + 1
      !
      n0_n_t ( iz_tab, 2 ) = &
      i2_n_t ( iz_tab, 2 ) &
    - i1_n_t ( iz_tab, 2 ) + 1
      !
      !if ( mod(iz_tab,20) .eq. 1 ) &
      !print'(" i=", i5, &
      !& " n1=", i5, 1x, i5, 1x, i5, &
      !& " n2=", i5, 1x, i5, 1x, i5 &
      !& )', &
      !iz_tab, &
      !n0_n_t ( iz_tab, 1 ), i1_n_t ( iz_tab, 1 ), i2_n_t ( iz_tab, 1 ), &
      !n0_n_t ( iz_tab, 2 ), i1_n_t ( iz_tab, 2 ), i2_n_t ( iz_tab, 2 )
      !
    end do do_iz_tab_1
    !
    bv_ave ( 1:nz_tab ) = 1. / bs_ave ( 1:nz_tab )
    !
    ! make two passes through the raytracing,
    ! the first traces normal rays down
    ! second traces turning wave upwards
    !
    do_norm_turn : do i0_n_t = 1 , 2
      !
      xxif_normal_waves : if ( i0_n_t .eq. 1 ) then
        !
        iz_end = 1
        !
        iz_dir = +1
        !
      else xxif_normal_waves
        !
        iz_end = nz_tab
        !
        iz_dir = -1
        !
      end if xxif_normal_waves
      !
      ! trace rays down each depth step from 2 to nz_tab
      !
      do_iz_tab_2 : do iz_tab = 2 , nz_tab
        !
        iz_beg = iz_end
        !
        iz_end = iz_end + iz_dir
        !
        j0_ray = n0_n_t ( iz_end, i0_n_t )
        !
        j1_ray = i1_n_t ( iz_end, i0_n_t )
        !
        j2_ray = i2_n_t ( iz_end, i0_n_t )
        !
        j1_tab = 1
        !
        j2_tab = nx_tab
        !
        ! extrapolate x and t values for rays j1_ray to j2_ray
        ! from rz_tab(iz_beg) to rz_tab(iz_end)
        ! using slowness rs_tab(iz_beg) and rs_tab(iz_end)
        !
        !print*,' iz=',iz_tab,' j=',j1_ray,j2_ray
        !
        call synvofz_raytrace_vofz_step ( &
                                          bz_tab(iz_beg), bz_tab(iz_end), &
                                          bs_ray(iz_beg), bs_ray(iz_end), &
                                          j1_ray, j2_ray, &
                                          bp_ray, bx_ray, bt_ray &
                                        )
        !
        ! get the time and ray parameters for the source and receiver
        ! interpolate_i_to_i lineraly interpolates
        ! an irregularly sampled function to an irregularly sampled set
        ! from np_ray values of t=bt_ray sampled at x=bx_ray,
        ! to   nx_tab values to t=bt_tab sampled at x=bx_tab
        !
        ! compute the range of table x values
        ! that can be set with the normal waves
        !
        xxif_have_ray : if ( j1_ray .le. j2_ray ) then
          !
          j1_tab = int ( ( bx_ray(j1_ray) - x0_tab ) / dx_tab ) + 1
          !
          j2_tab = int ( ( bx_ray(j2_ray) - x0_tab ) / dx_tab ) + 1
          !
          !print*,' jr=',j1_ray,j2_ray, ' jt=',j1_tab, j2_tab, &
          !' x=', bx_ray(j1_ray), bx_ray(j2_ray), &
          !' t=', bt_ray(j1_ray), bt_ray(j2_ray)
          !
          j1_tmp = max (      1, min ( j1_tab, j2_tab ) )
          !
          j2_tmp = min ( nx_tab, max ( j1_tab, j2_tab ) )
          !
          j1_tab = j1_tmp
          !
          j2_tab = j2_tmp
          !
          j0_tab = j2_tab - j1_tab + 1
          !
          ! save these values
          !
          i1_tab ( iz_end, i0_n_t ) = j1_tab
          !
          i2_tab ( iz_end, i0_n_t ) = j2_tab
          !
          n0_tab ( iz_end, i0_n_t ) = j0_tab
          !
          xxif_have_tab : if ( j1_tab .le. j2_tab ) then
            !
            call interpolate_i_to_i_d ( &
            j0_ray, bx_ray(j1_ray:j2_ray), bt_ray(j1_ray:j2_ray), &
            j0_tab, bx_tab(j1_tab:j2_tab), bt_tab(j1_tab:j2_tab) )
            !
            call interpolate_i_to_i_d ( &
            j0_ray, bx_ray(j1_ray:j2_ray), bp_ray(j1_ray:j2_ray), &
            j0_tab, bx_tab(j1_tab:j2_tab), bp_tab(j1_tab:j2_tab) )
            !
            rt_tab(j1_tab:j2_tab,iz_end,i0_n_t) = bt_tab(j1_tab:j2_tab)
            !
            rp_tab(j1_tab:j2_tab,iz_end,i0_n_t) = bp_tab(j1_tab:j2_tab)
            !
            !print*,' vv1 rt_tab=', rt_tab(j1_tab:j2_tab,iz_end,i0_n_t)
            !print*,' vv1 rp_tab=', rp_tab(j1_tab:j2_tab,iz_end,i0_n_t)
            !
            ra_tab(j1_tab:j2_tab,iz_end,i0_n_t) = &
       1. / rt_tab(j1_tab:j2_tab,iz_end,i0_n_t)
            !
          end if xxif_have_tab
          !
        end if xxif_have_ray
        !
        !ra_tab ( j1_tab:j2_tab, iz_end, i0_n_t ) = 1.
        !
        !if ( mod(iz_tab,20) .eq. 1 ) &
        !print'(" q0 i=", i1, 1x, i5, &
        !& " np=", i5, 1x, i5, &
        !& " nx=", i5, 1x, i5, &
        !& " x=", g10.4, 1x, g10.4, &
        !& " t=", g10.4, 1x, g10.4, &
        !& " z=", g10.4, " v=", g10.4, " p=", g10.4 &
        !& )',&
        !i0_n_t, iz_end, j1_ray, j2_ray, j1_tab, j2_tab, &
        !bx_ray(j1_ray), bx_ray(j2_ray), &
        !minval(rt_tab(j1_tab:j2_tab,iz_end,i0_n_t)), &
        !maxval(rt_tab(j1_tab:j2_tab,iz_end,i0_n_t)), &
        !bz_tab(iz_end), bv_ave(iz_end), 1. / bp_ray(j2_ray)
        !
      end do do_iz_tab_2
      !
    end do do_norm_turn
    !
    !print*,' ww1 rt_tab=', minval(rt_tab),maxval(rt_tab)
    !print*,' ww1 rp_tab=', minval(rp_tab),maxval(rp_tab)
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in synref_raytrace_vofz x range is too small ", &
    & /, " np_ray=",i8," x1_ray=", g12.6, &
    & /, " iz_tab=",i8," bx_ray=",g12.6,1x,g12.6," bt_ray=",g12.6,1x,g12.6 &
    & )') &
    np_ray, x1_ray, iz_tab, &
    bx_ray(1), bx_ray(np_ray), &
    bt_ray(1), bt_ray(np_ray)
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in synref_raytrace_vofz " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine synvofz_raytrace_vofz
  !
  subroutine synvofz_raytrace_vofz_step ( &
                                          bz_beg, bz_end, &
                                          bs_beg, bs_end, &
                                          i1_ray, i2_ray, &
                                          bp_ray, bx_ray, bt_ray &
                                        )
    !
    ! trace rays from z = rz_beg, slowness = rs_beg
    !              to z = rz_end, slowness = rs_end
    ! for rays i1_ray to i2_ray with ray paramters bp_ray
    ! note this can go up or down
    ! p * v = sin ( ray angle to vertical )
    !
    double precision, intent (in   ) :: bz_beg    ! depth    at beg of ray
    double precision, intent (in   ) :: bz_end    ! depth    at end of ray
    double precision, intent (in   ) :: bs_beg    ! slowness at beg of ray
    double precision, intent (in   ) :: bs_end    ! slowness at end   of ray
    integer,          intent (in   ) :: i1_ray    ! first ray index
    integer,          intent (in   ) :: i2_ray    ! last  ray index
    double precision, intent (in   ) :: bp_ray(:)   ! ray parameter values
    double precision, intent (inout) :: bx_ray(:)   ! x value at ray end point
    double precision, intent (inout) :: bt_ray(:)   ! t value at ray end point
    !
    integer                          :: ip_ray    ! ray index
    double precision                 :: bs_step   ! average slowness over step
    double precision                 :: bv_step   ! average velocity over step
    double precision                 :: br_step   ! x step length
    double precision                 :: bx_step   ! x step length
    double precision                 :: bz_step   ! depth step to extrap rays
    double precision                 :: bt_step   ! t step length
    double precision                 :: b_angle   ! angle
    double precision                 :: s2        ! slowness squared
    double precision                 :: b2        ! distance squared
    !
    ! p / v = sin angle v = slowness
    !
    ! cycle over rays
    !
    bs_step = ( bs_beg + bs_end ) * .5
    !
    s2 = bs_step ** 2
    !
    bv_step = 1. / bs_step
    !
    bz_step = ( bz_end - bz_beg )
    !
    do_ip_ray : do ip_ray = i1_ray , i2_ray
      !
      b_angle = asin ( min ( 1.d0, bp_ray( ip_ray ) * bv_step ) )
      !
      b2 = bp_ray( ip_ray )**2
      !
      bx_step = 0.
      !
      if ( s2 .gt. b2 ) &
      bx_step = bz_step * bp_ray( ip_ray ) / sqrt ( s2 - b2 )
      !
      br_step = sqrt ( bx_step**2 + bz_step**2 )
      !
      bt_step = br_step * bs_step          ! time along ray
      !
      bx_ray( ip_ray ) = bx_ray( ip_ray ) + bx_step
      !
      bt_ray( ip_ray ) = bt_ray( ip_ray ) + bt_step
      !
      !if ( ip_ray .eq. i2_ray ) &
      !print'(" q1 a=",g12.6," z=",g12.6," x=",g12.6,1x,g12.6,1x,g12.6)',&
      !b_angle * 90. /asin(1.), bz_step, bx_step, &
      !bx_ray( ip_ray ), bx_ray( ip_ray )-bx_step
      !
    end do do_ip_ray
    !
    return
    !
  end subroutine synvofz_raytrace_vofz_step
  !
end module synvofz_module
