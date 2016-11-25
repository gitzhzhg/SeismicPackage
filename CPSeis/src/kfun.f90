!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- kfun.f90 --------------------------------!!
!!------------------------------- kfun.f90 --------------------------------!!
!!------------------------------- kfun.f90 --------------------------------!!

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
!                        C P S   P R I M I T I V E        
!
! Name       : kfun 
! Category   : migrations
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2007-07-10   by: Douglas Hanson Add RAY_RAY_BEAM.
! Maturity   : beta
! Purpose    : basic table functions and modules
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! basic table functions and modules for Kirchhoff imaging
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i =  intent(in)    = value required upon INPUT.
!   o =  intent(out)   = value set by the routine upon OUTPUT.
!   b =  intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
! compute the maximum travel time size
! in the radial (r_xy_tab) and vertical (z1_tab) directions
!
!        call kfun_xyz_tab_max ( i, z1_tab, r_xy_tab )
!
!    type ( ktime_struct ),     pointer :: i  ! ktime  tables   structure
!
!    real,                intent(inout) :: z1_tab           ! tab max z value
!    real,                intent(inout) :: r_xy_tab         ! tab max r value
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author         Description
!     ----        ------         -----------
! 34  2007-07-10  Douglas Hanson Add RAY_RAY_BEAM.
!                                modify kfun_set_phase for inverse depth mig.
! 33  2007-05-10  Douglas Hanson Add ALL_OUT=ACTIVE_DEAD.
! 32  2007-05-10  Douglas Hanson Add ALL_OUT=DEAD.
! 31  2007-03-01  Douglas Hanson Add m_par_com.
! 30  2006-11-28  Douglas Hanson Remove print.
! 29  2006-10-31  Douglas Hanson Use grid, loc headers.
! 28  2006-10-17  Douglas Hanson Add Vs coefficients.
! 27  2006-08-29  Douglas Hanson Add fill_ratio print.
! 26  2006-08-24  Douglas Hanson Add TYPE_TABLE_CALC=RAYSHVTI.
! 25  2006-08-10  Douglas Hanson Change writes to prints.
! 24  2006-07-18  RSDay          Add kfun_is_cps(),add kfun_job_name.
! 23  2006-06-27  Douglas Hanson Add kfun_cps_code.
! 22  2006-06-22  Douglas Hanson Add kfun_swap.
! 21  2006-05-23  Douglas Hanson Change RAY_FILL_OPT to OPT_RAY_FILL.
! 20  2006-05-02  Douglas Hanson Add OPT_RAY_STEP.
! 19  2006-04-18  Douglas Hanson Add OPT_RAY_GRAD.
! 18  2006-03-28  Douglas Hanson Add kfun_print_time.
! 17  2006-03-21  Douglas Hanson Fix OPT_RAY_INC buffering.
! 16  2006-03-16  Douglas Hanson Add OPT_RAY_INC.
! 15  2005-11-22  Douglas Hanson Add OPT_RAY_FILL, SCALE_RAY_FILL
! 14  2005-10-13  Douglas Hanson Fix unreferenced variables.
!                                Add kpan_struct.
! 13  2005-04-21  Bill Menger    Uses kcpu struct pointer in kray struct 
! 12  2005-03-03  Douglas Hanson Add table_compute.
! 11  2005-01-31  Douglas Hanson preallocate kray memory.
! 10  2004-08-26  Douglas Hanson Separate s0_map, g0_map comp
! 9  2004-03-15  Bill Done      Fix kfun_file_name.
! 8  2003-10-08  Douglas Hanson Add v_dimension.
! 7  2003-09-30  Douglas Hanson Delete size_0=0 type statement.
! 6  2003-07-24  Douglas Hanson Add cx_tab, cy_tab.
! 5  2003-06-25  Douglas Hanson Add kfun_file_name
!                 Douglas Hanson Add kfun_stop routines.
! 4  2003-06-05  Done, Hanson   For debug printing, switch kfun from getting
!                                that setting from the JOB_DATA combo list to
!                                a setting supplied via new function
!                                kfun_print_debug_set.
! 3  2003-03-05  Douglas Hanson Add set functions.
!     2003-03-06  Douglas Hanson Add hconoco to opt_amp_cosine.
! 2  2002-11-11  Douglas Hanson Add opt_amp_cosine for kfun_time_forward.
! 1  2002-04-10  Douglas Hanson Extract from kmig.
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
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!
module kfun_module
  !
  ! travel time table computation routines
  !
  use cio_module
  use cnfg_module
  use getsys_module
  use interpolate_module
  use kcpu_module
  use matfun_module
  use memfun_module
  use migfun_module
  use named_constants_module
  use pattern_module
  use pathcheck_module
  use pc_module
  use pp_module
  use pcpsx_module
  use trcio_module
  use unix_module
  !
  implicit  none
  !
  private
  !
  ! subroutines
  !
  public :: kfun_time_forward          ! compute forward time migration map
  public :: kfun_time_inverse          ! compute inverse time migration map
  public :: kfun_dmo                   ! compute dmo forward and inverse map
  public :: kfun_set_range             ! set consistent num, min, max, inc
  public :: kfun_xyz_tab_max           ! return max spatial size of table
  public :: kfun_stop                  ! stop with print
  public :: kfun_grid_to_scale         ! get grid to dist scale
  public :: kfun_set_constant_path     ! set constant path
  public :: kfun_set_opt_ray           ! set opt_ray
  public :: kfun_set_opt_ray_grad      ! set opt_ray_grad
  public :: kfun_set_opt_ray_step      ! set opt_ray_step
  public :: kfun_set_opt_ray_bend      ! set opt_ray_bend
  public :: kfun_set_opt_ray_inc       ! set opt_ray_inc
  public :: kfun_set_opt_ray_fill      ! set opt_ray_fill
  public :: kfun_print_debug_set       ! set debug print level flag
  public :: kfun_file_name             ! construct a file name
  public :: kfun_file_size             ! construct a file name
  public :: kfun_file_open             ! open  a file
  public :: kfun_file_close            ! close a file
  public :: kfun_trace_file_open       ! open trace file
  public :: kfun_trace_file_close      ! close trace file
  public :: kfun_trace_file_write      ! write 3d array to trace file
  public :: kfun_trace_file_write_slice! write 2d slice to trace file
  public :: kfun_trace_file            ! open write close 3d array to file
  public :: kfun_table_file            ! write 3d table to trace file
  public :: kfun_print_time            ! print time date stamp
  public :: kfun_print_aperture
  public :: kfun_print_aperture_a
  public :: kfun_print_aperture_x
  public :: kfun_print_aperture_y
  public :: kfun_print_aperture_z
  public :: kfun_print_aperture_t
  public :: kfun_print_sample 
  !
  public :: kfun_set_max_offset 
  public :: kfun_set_table_parms
  public :: kfun_set_fctr 
  public :: kfun_set_filename_operation
  public :: kfun_set_path_table
  public :: kfun_set_sort_order 
  public :: kfun_set_mig_type
  public :: kfun_set_opt_aniso 
  public :: kfun_set_opt_map_apply 
  public :: kfun_set_opt_out_int 
  public :: kfun_set_dimension
  public :: kfun_set_opt_dir 
  public :: kfun_set_all_out
  public :: kfun_set_opt_output 
  public :: kfun_set_table_order
  public :: kfun_set_opt_table_mem
  public :: kfun_set_opt_table_disk
  public :: kfun_set_local_pack
  public :: kfun_set_opt_table_operation
  public :: kfun_set_apr
  public :: kfun_set_phase
  public :: kfun_set_dist_grid_angle
  public :: kfun_condition_depth_grid
  public :: kfun_condition_table_grid
  public :: kfun_condition_polygon_grid
  public :: kfun_condition_fctr_vel 
  public :: kfun_memory_size 
  public :: kfun_job_name 
  public :: kfun_is_cps 
  !
  ! functions
  !
  public :: kfun_iz                   ! the top z index
  public :: kfun_eps                  ! a small real value
  public :: kfun_inc                  ! a print increment
  public :: kfun_swap                 ! swap values
  public :: kfun_i_pel                ! pe index
  public :: kfun_n_pel                ! number of pes
  public :: kfun_print                ! return print flag
  public :: kfun_x_index              ! compute x      index from x,y index
  public :: kfun_y_index              ! compute y      index from x,y index
  public :: kfun_d_index              ! compute disk   index from x,y index
  public :: kfun_m_index              ! compute memory index from x,y index
  public :: kfun_mem_init             ! init  mem counter
  public :: kfun_mem_size             ! return mem counter
  !
  ! interfaces
  !
  interface kfun_swap
    !
    module procedure kfun_swap_r0
    module procedure kfun_swap_r1
    module procedure kfun_swap_r2
    !
    module procedure kfun_swap_i0
    module procedure kfun_swap_i1
    module procedure kfun_swap_i2
    !
  end interface 
  !
  interface kfun_stop
    !
    module procedure kfun_stop
    module procedure kfun_stop_r
    module procedure kfun_stop_d
    module procedure kfun_stop_c
    module procedure kfun_stop_l
    !
  end interface 
  !
  !logical,          public, save      :: kfun_cps_code = .true.
  logical,          public, save      :: kfun_cps_code = .false.
  integer,          public, save      :: kfun_num_cpus = 1
  real,             public, save      :: ray_init_tim = -999.
  real,             public, save      :: ray_init_amp = -0.00001
  real,             public, save      :: ray_init_phs = 0.
  integer,          public, save      :: opt_ray_a0 = 1
  integer,          public, save      :: opt_ray_v0 = 2
  integer,          public, save      :: opt_ray_t1 = 3
  integer,          public, save      :: opt_ray_t2 = 4
  integer,          public, save      :: opt_ray_s1 = 5
  integer,          public, save      :: opt_ray_s2 = 6
  integer,          public, save      :: opt_ray_t0 = 7
  integer,          public, save      :: opt_ray_s0 = 8
  integer,          public, save      :: kfun_mem_temp = 0
  integer,          public, save      :: kfun_i_shft   = 10
  real,             public, save      :: kfun_r_shft   = 2.**10
  integer,          public, save      :: kfun_debug_print_level = 1
  integer,          public, parameter :: opt_print_debug_n = 4
  integer,          public, parameter :: kray_mt_var = 20
  integer,          public, parameter :: kray_m_opt_ray = 8
  integer,          public, parameter :: kray_m_opt_tab_qc = 4
  character(len=8), public, save      :: opt_print_debug_c(opt_print_debug_n) &
      = (/ 'NONE    ', 'BRIEF   ', 'DETAILED', 'DEBUG   ' /)
  !
  type, public :: ktime_struct
    !
    ! the dimension of ix_tab, nx_tab, x0_tab is ny_tab
    ! the dimension of iz_tab, nz_tab, z0_tab is n_xy_tab
    ! the dimension of tim_tab is n_xyz_tab
    !
    ! where
    !
    ! ix_tab   = array of table x nodes pointers
    ! nx_tab   = array of number of table x nodes for each y
    ! x0_tab   = array of table x node origins for each y
    ! dx_tab   = table x node spacing
    !
    ! ny_tab   =          number of table y nodes
    ! y0_tab   =          table y node origin
    ! dy_tab   = table y node spacing
    !
    ! iz_tab   = array of table z nodes pointers
    ! nz_tab   = array of number of table z nodes for each x
    ! z0_tab   = array of table z node origins for each x
    ! dz_tab   = table z node spacing
    !
    ! n_xy_tab  = ix_tab ( ny_tab  ) + nx_tab ( ny_tab  ) 
    !           = sum ( nx_tab ( 1:ny_tab ) )
    ! n_xyz_tab = iz_tab ( n_xy_tab ) + nz_tab ( n_xy_tab )
    !           = sum ( nz_tab (1:n_xy_tab ) )
    !
    ! note x0_tab and y0_tab are relative to rx_src and ry_src
    ! while z0_tab is in absolute coordinates
    ! 
    !     ix_tab, nx_tab, x0_tab, dx_tab
    ! and         ny_tab, y0_tab, dy_tab
    ! and iz_tab, nz_tab, z0_tab, dz_tab
    !
    ! define a cropped 3D rectangular volume.
    !
    ! This volume has:
    !
    ! ny_tab y values ranging from y0_tab to (ny_tab-1)*dy_tab+y0_tab
    !
    ! the jy_tab y slice has nx_tab(jy_tab) x values
    ! ranging from x0_tab(jy_tab) to (nx_tab(jy_tab)-1)*dx_tab+x0_tab(jy_tab)
    ! the index within iz_tab, nz_tab, z0_tab of the first x column
    ! for y slice jy_tab is ix_tab(jy_tab)+1
    !
    ! the index within iz_tab, nz_tab, z0_tab of the last  x column
    ! for y slice jy_tab is ix_tab(jy_tab)+nx_tab(jy_tab)
    !
    ! the kx_tab x column of ry_slice jy_tab
    ! has nz_tab(jx_tab) z values
    ! ranging from z0_tab(jx_tab) to (nz_tab(jx_tab)-1)*dz_tab+z0_tab(jx_tab)
    ! where kx_tab ranges from 1 to nx_tab(jy_tab)
    ! and jx_tab = ix_tab(jy_tab) + kx_tab
    !
    ! the index within tim_tab of the first travel time node for this x column
    ! is iz_tab(jx_tab)+1
    !
    ! the index within tim_tab of the last   travel time node for this x column
    ! is iz_tab(jx_tab)+nz_tab(jx_tab)
    !
    type ( kgrid_struct ),   pointer :: g  ! kgrid  grid     structure
    !
    character(len=3)                 :: table_compute    ! compute tables
    character(len=3)                 :: opt_tab_qc       ! write qc files
    character(len=filename_length)   :: root_tab_qc      ! qc file name
    character(len=filename_length)   :: path_tab_qc      ! qc file name
    integer                          :: first_tab_qc     ! first qc file 
    integer                          :: last_tab_qc      ! last  qc file 
    logical                          :: l_opt_tab_qc(kray_m_opt_tab_qc)  
    character(len=1)                 :: c_opt_tab_qc(kray_m_opt_tab_qc)  
    integer                          :: r_opt_tab_qc     ! grid qc files
    integer                          :: t_opt_tab_qc     ! grid qc files
    integer                          :: v_opt_tab_qc     ! grid qc files
    integer                          :: i_opt_tab_qc     ! grid qc files
    !
    logical                          :: dep_mig  ! depth or time migration flag
    real                             :: ox_mig   ! max x offset 
    real                             :: oy_mig   ! max y offset 
    !
    integer                          :: nt_out   ! num fine uniform out nodes
    real                             :: t0_out   ! min fine uniform out nodes
    real                             :: t1_out   ! max fine uniform out nodes
    real                             :: dt_out   ! inc fine uniform out nodes
    !
    integer                          :: n_dimension  ! num dimension 2, 3
    integer                          :: v_dimension  ! vel dimension 2, 3
    real                             :: rz_datum     ! constant z datum 
    !
    character(len=8)                 :: table_shape  ! table chape cube, normal
    character(len=8)                 :: table_type   ! type of time calc
    !
    integer                          :: table_parms  ! num of table parameters 
    integer                          :: m_par_com    ! num of parm comp
    integer                          :: m_par_tab    ! num of table parameters 
    integer                          :: m_xyz_tab    ! num pts in table 
    integer                          :: n_xyz_tab    ! num x,y,z pts in table 
    integer                          :: n_xy_tab     ! num x,y,pts in table 
    real                             :: r_xy_tab     ! max radial table size
    !
    ! the following define a constant cartesian cube
    ! with uniform vertical grid which surrounds the
    ! cropped table with variable vertical grid
    !
    integer                          :: mx_tab    ! max num x nodes any y
    real                             :: x1_tab    ! max val x nodes any y
    real                             :: x2_tab    ! min val x nodes any y
    real                             :: dx_tab    !     inc x nodes all y
    real                             :: cx_tab    ! 1./ inc x nodes all y
    !
    integer                          :: my_tab    ! max num y nodes = ny_tab
    real                             :: y1_tab    ! max val y nodes 
    real                             :: y2_tab    ! min val y nodes 
    !
    integer                          :: ny_tab    !     num y nodes
    real                             :: y0_tab    ! min val y nodes 
    real                             :: dy_tab    ! inc val y nodes 
    real                             :: cy_tab    ! 1./ inc y nodes 
    !
    integer,                 pointer :: ix_tab ( : ) ! first x node pointer
    integer,                 pointer :: nx_tab ( : ) ! num   x nodes
    real,                    pointer :: x0_tab ( : ) ! min   x node
    !
    integer,                 pointer :: iz_tab ( : ) ! first z node pointer
    integer,                 pointer :: nz_tab ( : ) ! num   z nodes
    real,                    pointer :: z0_tab ( : ) ! min   z node
    !
    ! the following defines a spherical grid surrounding a travel time table
    ! r is radius measured from rx_tab=0., ry_tab=0., rz_tab=rz_datum
    ! a = angle 1 = angle measured from the z axis. 
    ! b = angle 2 = angle measured around the z axis in the x,y plane.
    ! angles a and b are measured in radians.
    ! in 2D 
    ! a = 0 on positiive z axis, a = pi on posiitve x axis
    ! b is not defined but can be addressed as zero.
    ! in 3D 
    ! a = 0 on positiive z axis, a = pi in x,y plane
    ! b = 0, 2 pi on positive x axis,
    ! b = pi on positive y axis,
    !
    integer                          :: mr_tab ! num radial nodes max 
    integer                          :: nr_tab ! num radial nodes
    real                             :: r0_tab ! min radial nodes
    real                             :: r1_tab ! max radial nodes
    real                             :: dr_tab ! inc radial nodes
    !
    integer                          :: ma_tab ! num angle 1 nodes max 
    integer                          :: na_tab ! num angle 1 nodes
    real                             :: a0_tab ! min angle 1 nodes
    real                             :: a1_tab ! max angle 1 nodes
    real                             :: da_tab ! inc angle 1 nodes
    !
    integer                          :: mb_tab ! num angle 2 nodes max 
    integer                          :: nb_tab ! num angle 2 nodes
    real                             :: b0_tab ! min angle 2 nodes
    real                             :: b1_tab ! max angle 2 nodes
    real                             :: db_tab ! inc angle 2 nodes
    !
    integer                          :: j_xyz_pos  ! table index
    integer                          :: ix_pos     ! x src idx
    integer                          :: iy_pos     ! y src idx
    integer                          :: iz_pos     ! z src idx
    real                             :: rx_pos     ! x src loc
    real                             :: ry_pos     ! y src loc
    real                             :: rz_pos     ! z src loc
    integer                          :: hx_mig     ! x header word
    integer                          :: hy_mig     ! y header word
    real                             :: rx_scl     ! x scl value
    real                             :: ry_scl     ! y scl value
    real                             :: rz_scl     ! z scl value
    real                             :: fill_ratio ! fill ratio
    !
    integer                          :: ktime_mem_size_0 ! mem usage
    !
    type ( grid_struct ),  pointer   :: grid_obj    ! global
    type ( kray_struct ),  pointer   :: r      ! raytracing structure
    !
  end type ktime_struct
  !
  type, public :: kgrid_struct
    !
    logical                          :: dep_mig       ! dep mig flag
    character(len=8)                 :: opt_step      ! variable step option
    logical                          :: con_step      ! constant step flag
    !
    integer                          :: nt_step       ! num var steps
    real,             pointer        :: gt_step ( : ) ! val var steps
    real,             pointer        :: dt_step ( : ) ! inc var steps
    !
    integer                          :: mz_tab       ! num constant tab t,z 
    real                             :: z0_tab       ! min constant tab t,z 
    real                             :: z1_tab       ! max constant tab t,z 
    real                             :: dz_tab       ! inc constant tab t,z 
    !
    integer                          :: nx_bdv          ! x bdv num 
    real                             :: x0_bdv          ! x bdv min 
    real                             :: x1_bdv          ! x bdv max 
    real                             :: dx_bdv          ! x bdv inc 
    real                             :: cx_bdv          ! x bdv inc inv 
    !
    integer                          :: nx_mig          ! x mig num 
    real                             :: x0_mig          ! x mig min 
    real                             :: x1_mig          ! x mig max 
    real                             :: dx_mig          ! x mig inc 
    real                             :: cx_mig          ! x mig inc inv 
    !
    integer                          :: ny_mig          ! y mig num 
    real                             :: y0_mig          ! y mig min 
    real                             :: y1_mig          ! y mig max 
    real                             :: dy_mig          ! y mig inc 
    real                             :: cy_mig          ! y mig inc inv 
    !
    integer                          :: nt_out          ! z mig num 
    real                             :: t0_out          ! z mig min 
    real                             :: t1_out          ! z mig max 
    real                             :: dt_out          ! z mig inc 
    real                             :: ct_out          ! z mig inc inv 
    !
    integer                          :: no_mig          ! o mig num 
    real                             :: o0_mig          ! o mig min 
    real                             :: o1_mig          ! o mig max 
    real                             :: do_mig          ! o mig inc 
    real                             :: co_mig          ! o mig inc inv 
    !
    integer                          :: lt_map       ! dz_tab / dt_out
    integer                          :: nt_map       ! map var t,z num 
    real,             pointer        :: gt_map ( : ) ! map var t,z val 
    real,             pointer        :: dt_map ( : ) ! map var t,z inc 
    !
    integer,          pointer        :: i1_map ( : ) ! map var t,z index 1
    integer,          pointer        :: i2_map ( : ) ! map var t,z index 2
    real,             pointer        :: f1_map ( : ) ! map var t,z coeff 1
    real,             pointer        :: f2_map ( : ) ! map var t,z coeff 2
    !
    integer                          :: ng_tab       ! tab var t,z num 
    real,             pointer        :: rg_tab ( : ) ! tab var t,z val
    real,             pointer        :: dg_tab ( : ) ! tab var t,z inc
    !
    integer,          pointer        :: i1_tab ( : ) ! tab var t,z index 1
    integer,          pointer        :: i2_tab ( : ) ! tab var t,z index 2
    real,             pointer        :: f1_tab ( : ) ! tab var t,z coeff 1
    real,             pointer        :: f2_tab ( : ) ! tab var t,z coeff 2
    !
    integer                          :: i0_tab       ! tab i0_tab = map 1
    !
    integer                          :: nt_mig       ! mig var t,z num 
    real,             pointer        :: gt_mig ( : ) ! mig var t,z val
    real,             pointer        :: dt_mig ( : ) ! mig var t,z inc
    !
    integer,          pointer        :: i1_mig ( : ) ! mig var t,z index 1
    integer,          pointer        :: i2_mig ( : ) ! mig var t,z index 2
    real,             pointer        :: f1_mig ( : ) ! mig var t,z coeff 1
    real,             pointer        :: f2_mig ( : ) ! mig var t,z coeff 2
    !
    real,             pointer        :: v0_rms ( : ) ! rms velocity to each map
    real,             pointer        :: v0_ave ( : ) ! ave velocity to each map
    real,             pointer        :: v0_int ( : ) ! int velocity to each map
    real,             pointer        :: z0_tmp ( : ) ! vert z to each map
    real,             pointer        :: z2_tmp ( : ) ! z0_tmp**2
    real,             pointer        :: t0_tmp ( : ) ! vertical time to each map
    real,             pointer        :: t2_tmp ( : ) ! t0_tmp**2
    real,             pointer        :: t3_tmp ( : ) ! t0_tmp**2 / 4.
    real,             pointer        :: t4_tmp ( : ) ! 1. / t2_tmp
    real,             pointer        :: t5_tmp ( : ) ! sqrt ( t0_tmp)
    !
    integer                          :: kgrid_mem_size_0 
    !
  end type kgrid_struct
  !
  ! ray tracing structure
  !
  type, public :: kray_struct
    !
    integer                          :: n_dimension      ! dimension 2,3
    !
    type ( kcpu_struct ),    pointer :: c                ! kcpu cpu structure
    logical                          :: tc_exist         ! tc exist flag
    logical                          :: ts_exist         ! ts exist flag
    type ( ktub_struct ),    pointer :: tc               ! ktub tub comp struc
    type ( ktub_struct ),    pointer :: ts               ! ktub tub save struc
    character(len=3)                 :: table_compute    ! compute tables
    character(len=8)                 :: table_type       ! table comp type
    real                             :: ray_pz_sign      ! shoot rays up
    character(len=9)                 :: opt_ray          ! ray select option
    character(len=8)                 :: opt_ray_grad     ! ray grad option
    character(len=8)                 :: opt_ray_step     ! ray step option
    character(len=8)                 :: opt_ray_bend     ! ray bend option
    character(len=8)                 :: opt_ray_inc      ! ray inc option
    character(len=filename_length)   :: path_ray_inc     ! ray inc file name
    logical                          :: ray_inc_constant
    logical                          :: ray_inc_variable
    logical                          :: ray_inc_read
    logical                          :: ray_inc_write
    logical                          :: ray_inc_compute
    character(len=8)                 :: opt_ray_fill     ! ray fill option
    real                             :: scale_ray_fill   ! ray fill scale
    real                             :: table_comp_inc   ! ray step length
    integer                          :: ray_max_number   ! max num rays
    integer                          :: ray_interpolation! ray interp factor 
    integer                          :: ray_micro_ratio  ! ratio of ray steps
    real                             :: ray_max_angle    ! max ray angle
    integer                          :: ray_beam_num     ! num ray beams
    real                             :: ray_tim_accuracy ! ray tim accuracy
    integer                          :: ray_tim_tot      ! max ray tim 
    real                             :: ray_tim_init     ! min ray tim 
    real                             :: ray_tim_last     ! max ray tim 
    real                             :: ray_tim_inc      ! inc ray tim 
    integer                          :: ray_ang1_tot     ! num ray ang1 
    real                             :: ray_ang1_init    ! min ray ang1 
    real                             :: ray_ang1_last    ! max ray ang1 
    real                             :: ray_ang1_inc     ! inc ray ang1 
    integer                          :: ray_ang2_tot     ! num ray ang2 
    real                             :: ray_ang2_init    ! min ray ang2 
    real                             :: ray_ang2_last    ! max ray ang2 
    real                             :: ray_ang2_inc     ! inc ray ang2 
    !
    logical                          :: rays_and_tubs    ! init rays, tubs 
    integer                          :: m0_beam          ! beam ray max
    integer                          :: n0_beam          ! ray beam num
    integer                          :: l0_beam          ! ray tub per beam
    integer                          :: j0_beam          ! ray beam idx
    integer                          :: j1_beam          ! ray beam idx 1
    integer                          :: j2_beam          ! ray beam idx 2
    !
    real                             :: r0_src(3)   ! src x,y,z loc
    real                             :: rp_src(3)   ! src x,y,z ray
    real                             :: rr_src      ! src init rad value
    real                             :: rt_src      ! src init tim value
    real                             :: vx_src      ! src x vel 
    real                             :: gx_src(3)   ! src x vel grad
    real                             :: vz_src      ! src z vel 
    real                             :: gz_src(3)   ! src z vel grad
    real                             :: vs_src      ! src s vel 
    real                             :: gs_src(3)   ! src s vel grad
    real                             :: vd_src      ! src d vel 
    real                             :: gd_src(3)   ! src d vel grad
    real                             :: dt_src      ! src ray tim inc
    real                             :: q1_src      ! src q
    real                             :: dq_src(3)   ! src dq
    !
    real                             :: r0_box(3,2) ! ray box range
    real                             :: rz_lim ! max z ray lim
    real                             :: cs_max ! max cos to propagate
    !
    integer                          :: na_ray ! num ang 1
    real                             :: a0_ray ! min ang 1
    real                             :: a1_ray ! max ang 1
    real                             :: da_ray ! inc ang 1
    integer                          :: nb_ray ! num ang 2
    real                             :: b0_ray ! min ang 2
    real                             :: b1_ray ! max ang 2
    real                             :: db_ray ! inc ang 2
    !
    integer                          :: lt_mac ! interp rays lt_mac macro steps
    integer                          :: jt_mac_1
    integer                          :: jt_mac_2
    integer                          :: jt_mac_3
    integer                          :: n0_mac
    integer                          :: jt_mac
    real                             :: rt_mac_1
    real                             :: rt_mac_2
    integer                          :: mt_mac ! num macro time steps max
    real                             :: t0_mac ! min macro time step
    real                             :: t1_mac ! max macro time step
    real                             :: dt_mac ! inc macro time step
    !
    integer                          :: n0_mic
    integer                          :: jt_mic
    real                             :: rt_mic_1
    real                             :: rt_mic_2
    integer                          :: lt_mic ! num micro per macro time step
    integer                          :: mt_mic ! num micro time steps max
    real                             :: t0_mic ! min micro time step
    real                             :: t1_mic ! max micro time step
    real                             :: dt_mic ! inc micro time step
    !
    integer                          :: lt_mec ! var num mic per mac time step
    integer                          :: mt_mec ! var num mic time steps max
    real                             :: t0_mec ! var min mic time step
    real                             :: t1_mec ! var max mic time step
    real                             :: dt_mec ! var inc mic time step
    !
    integer                          :: nt_var             ! num ray inc values
    real                             :: dt_var(kray_mt_var)! ray inc values
    integer                          :: mv_var             ! tot vel count
    integer                          :: nv_var(kray_mt_var)! ray inc count
    !
    integer                          :: i0_ray
    integer                          :: j0_ray
    !
    real,                    pointer :: vd_v2d(:, :)      ! grid vd
    real,                    pointer :: vd_vel(:, :, :)   ! grid vd
    !
    logical                          :: l_opt_ray(kray_m_opt_ray)! ray flags
    logical                          :: l_add_ab     ! add ab values
    !
    integer                          :: n_rv_tab     ! minimum velocity gredien
    integer                          :: n_rs_tab     ! set for shortest  
    integer                          :: n_rh_tab     ! phs 
    integer                          :: n_ra_tab     ! amp 
    integer                          :: n_dt_tab     ! ray step len
    !
    real,                    pointer :: dt_tab(:)       ! tab ray inc
    real,                    pointer :: rv_tab(:)       ! tab min max vel
    real,                    pointer :: rs_tab(:)       ! tab path len
    real,                    pointer :: rh_tab(:)       ! tab phs
    real,                    pointer :: ra_tab(:)       ! tab amp
    !
    integer                          :: nx_out          ! x out num 
    real                             :: x0_out          ! x out min 
    real                             :: x1_out          ! x out max 
    real                             :: dx_out          ! x out inc 
    !
    integer                          :: ny_out          ! y out num 
    real                             :: y0_out          ! y out min 
    real                             :: y1_out          ! y out max 
    real                             :: dy_out          ! y out inc 
    !
    integer                          :: nz_out          ! z out num 
    real                             :: z0_out          ! z out min 
    real                             :: z1_out          ! z out max 
    real                             :: dz_out          ! z out inc 
    !
    integer                          :: mx_vel
    integer                          :: my_vel
    integer                          :: nz_vel
    integer                          :: ny_tab
    integer                          :: mz_tab
    real                             :: cx_vel
    real                             :: cy_vel
    real                             :: cz_vel
    real                             :: z2_tab
    real                             :: dz_tab
    !
    integer,                 pointer :: i1_tab(:)    ! cnst to var tab index 1
    integer,                 pointer :: i2_tab(:)    ! cnst to var tab index 2
    real,                    pointer :: f1_tab(:)    ! cnst to var tab value 1
    real,                    pointer :: f2_tab(:)    ! cnst to var tab value 2
    !
    integer                          :: m_par_tab    ! num of table parameters 
    integer                          :: n_xy_tab     ! num x,y,pts in table 
    integer                          :: n_xyz_tab    ! num x,y,z pts in table 
    integer                          :: n_xyz_amp
    integer                          :: n_xyz_phs
    logical                          :: l_tim_tab
    logical                          :: l_amp_tab
    logical                          :: l_phs_tab
    integer                          :: m_xyz_fil
    integer                          :: n_xyz_fil
    integer                          :: n_xyz_set
    integer                          :: n_xyz_uns
    !
    logical                          :: l_2d_iso
    logical                          :: l_3d_iso
    logical                          :: l_2d_ani
    logical                          :: l_3d_ani
    logical                          :: l_2d_shr
    logical                          :: l_3d_shr
    logical                          :: l_ray_add
    !
    character(len=filename_length)   :: c_path_ray_qc
    integer                          :: i_path_ray_qc
    !
    double precision                 :: t1_cpu
    double precision                 :: t2_cpu
    double precision                 :: dt_cpu
    double precision                 :: t0_cpu(10,2)
    !
    logical                          :: rv_flg       ! maximum velocity along  
    logical                          :: rs_flg       ! len along rays
    logical                          :: rh_flg       ! phase along rays
    logical                          :: ra_flg       ! amplitude ray
    logical                          :: dt_flg       ! ray step len
    !
    real,                    pointer :: vx_ray(:, :)    ! ray vx
    real,                    pointer :: vz_ray(:, :)    ! ray vz
    real,                    pointer :: vs_ray(:, :)    ! ray vz
    real,                    pointer :: vd_ray(:, :)    ! ray vd
    real,                    pointer :: gx_ray(:, :, :) ! ray vx grad
    real,                    pointer :: gz_ray(:, :, :) ! ray vz grad
    real,                    pointer :: gs_ray(:, :, :) ! ray vz grad
    real,                    pointer :: gd_ray(:, :, :) ! ray vd grad
    real,                    pointer :: vp_ray(:, :)    ! ray phase vel
    real,                    pointer :: gp_ray(:, :, :) ! ray phase grad
    !
    real                             :: dr_ray          ! radial step length
    integer                          :: ni_ray          ! num rays inside
    integer                          :: no_ray          ! num rays outside
    integer,                 pointer :: io_ray(:)       ! ray outside index
    integer,                 pointer :: if_ray(:)       ! ray exist flag
    integer,                 pointer :: ip_ray(:)       ! ray mem pointer
    integer                          :: nr_add          ! num rays to add
    integer,                 pointer :: ir_add(:, :)    ! ray add index
    !
    real                             :: dr_tub          ! tub side limit
    integer                          :: ni_tub          ! num tubs inside
    integer                          :: no_tub          ! num tubs otuside
    integer,                 pointer :: io_tub(:)       ! tub outside index
    integer,                 pointer :: if_tub(:)       ! tub exist flag
    integer,                 pointer :: ip_tub(:)       ! tub mem pointer
    real,                    pointer :: rn_tub(:, :, :) ! tub normal
    !
    integer                          :: n_dt_ray     ! ray step len
    integer                          :: n_rv_ray     ! maximum velocity along  
    integer                          :: n_rs_ray     ! len along ray
    integer                          :: n_rh_ray     ! phase along rays
    integer                          :: n_ra_ray     ! amplitude ray
    !
    real,                    pointer :: dt_ray(:, :)    ! ray tim inc
    real,                    pointer :: rv_ray(:, :)    ! ray min max vel 
    real,                    pointer :: rs_ray(:, :)    ! ray path len 
    real,                    pointer :: rh_ray(:, :)    ! ray phs 
    real,                    pointer :: ra_ray(:, :)    ! ray amp 
    real,                    pointer :: q0_ray(:, :)    ! ray q0
    real,                    pointer :: q1_ray(:, :, :) ! ray q1
    real,                    pointer :: q2_ray(:, :, :) ! ray q2
    real,                    pointer :: dq_ray(:, :, :) ! ray dq
    !
    integer                          :: kray_mem_size_0 ! mem usage
    !
  end type kray_struct
  !
  ! ray tub structure
  !
  type, public :: ktub_struct
    !
    character(len=filename_length)   :: c_title
    integer                          :: n_dimension      ! dimension 2,3
    !
    integer                          :: m0_ray          ! num rays max
    integer                          :: n0_ray          ! num rays actual
    real,                    pointer :: r0_ray(:, :, :) ! ray x,y,z loc
    real,                    pointer :: rp_ray(:, :, :) ! ray x,y,z ray param
    !
    integer                          :: m0_tub          ! num tubs max
    integer                          :: n0_tub          ! num tubs actual
    integer,                 pointer :: ir_tub(:, :)    ! tub ray pointer
    !
    integer                          :: ktub_mem_size_0 ! mem usage
    !
  end type ktub_struct
  !
  ! rcs identifier string
  character(len=100),public,save :: kfun_ident = &
  '$Id: kfun.f90,v 1.34 2007/07/11 14:07:20 Hanson beta sps $'
  !
  contains
  !
! The following describes a proposed amplitude correction term.
!
! Zhang, Y., Gray, S. and Young, J., 
! Exact and approximate weights for Kirchhoff migration, 
! 2000, 70th Ann. Internat. Mtg: Soc. of Expl. Geophys., 1036-1039.
!
! 2D common offset Kirchhoff Migration weight = 
!      ( sqrt ( cos(aso) * cos(ar0)) / v0 )
!      * ( ( sqrt(qs/qr) + sqrt(qr/qs) ) )
! 
! 2.5D common offset Kirchhoff Migration weight = 
!      ( sqrt ( cos(aso) * cos(ar0)) / v0 )
!      * ( ( sqrt(qs/qr) + sqrt(qr/qs) ) * sqrt ( us + ur ) )
! 
! 3D common offset Kirchhoff Migration weight = 
!      ( sqrt ( cos(aso) * cos(ar0)) / v0 )
!      * ( ( sqrt(qs/qr) + sqrt(qr/qs) ) * ( sqrt(us/ur) + sqrt(ur/us) )
!          + .5 * L * sin(g)**2 / cos(i)**2 )
! 
! Where
! 
! L = 
! (      cos(as) + cos(ar) ) * ( sqrt((qs*qr)/(us*ur)) + sqrt((us*ur)/(qs*qr)) )
!- ( 1 + cos(as) * cos(ar) ) * ( sqrt((qs*ur)/(us*qr)) + sqrt((us*qr)/(qs*ur)) )
! 
! g = angle between the projection of source and receiver rays to the surface
! i = reflection angle 
! qs = cos(as) * ( dbs / dps ) = in     plane spreading term from the source
! qr = cor(ar) * ( dbr / dpr ) = in     plane spreading term from the receiver
! us = bs / ps                 = out of plane spreading term from the source
! ur = br / pr                 = out of plane spreading term from the receiver
! bs = sqrt ( (x0-xs)**2 + (y0-ys)**2 ) - horizontal distance, source   to image
! br = sqrt ( (x0-xr)**2 + (y0-yr)**2 ) - horizontal distance, receiver to image
! ps = sin ( as ) / v(z) - ray parameter from source   at the image point
! pr = sin ( ar ) / v(z) - ray parameter from receiver at the image point
! ( dbs / dps ) = deriviative of bs with respect to ps
! ( dbr / dpr ) = deriviative of br with respect to pr
! as = angle along the ray path from the source   relative to the vertical
! ar = angle along the ray path from the receiver relative to the vertical
! as0 = as at the surface
! ar0 = ar at the surface
! xo = image    x location, y0 = image    y location
! xs = source   x location, ys = source   y location
! xr = receiver x location, yr = receiver y location
!
  subroutine kfun_time_forward ( &
                                 opt_amp_cosine, &
                                 fa_compute, i_xy_sum, nc_vel, v_src_rec, &
                                 sr_mig, sx_mig, sy_mig, &
                                 gr_mig, gx_mig, gy_mig, &
                                 it_map_1, it_map_2, &
                                 num_aa, alias_inc, &
                                 fctr_aa, dist_aa,   &
                                 nt_inp, t0_inp, dt_inp, &
                                 nt_map, gt_map, dt_map, &
                                 t0_tmp, t3_tmp, z0_tmp, z2_tmp, &
                                 re_map, rv_map, rt_map, rf_map, ra_map &
                               )
    !
    ! compute the map from input to output for normal time migration
    ! note rv_map is slowness
    !
    character(len=*),  intent(in   ) :: opt_amp_cosine
    logical,           intent(in   ) :: fa_compute
    integer,           intent(in   ) :: i_xy_sum
    integer,           intent(in   ) :: nc_vel           ! num vel coef 1,2,3
    real,              intent(in   ) :: v_src_rec
    !
    real,              intent(in   ) :: sr_mig, sx_mig, sy_mig
    real,              intent(in   ) :: gr_mig, gx_mig, gy_mig
    !
    integer,           intent(inout) :: it_map_1
    integer,           intent(inout) :: it_map_2
    !
    integer,           intent(in   ) :: num_aa 
    integer,           intent(in   ) :: alias_inc
    real,              intent(in   ) :: fctr_aa
    real,              intent(in   ) :: dist_aa 
    !
    integer,           intent(in   ) :: nt_inp           ! num inp t
    real,              intent(in   ) :: t0_inp           ! min inp t
    real,              intent(in   ) :: dt_inp           ! inc inp t
    !
    integer,           intent(in   ) :: nt_map           ! map var t,z num
    real,              intent(in   ) :: gt_map ( : )     ! map var t,z val
    real,              intent(in   ) :: dt_map ( : )     ! map var t,z inc
    !
    real,              intent(in   ) :: t0_tmp ( : ) ! vertical time to each map
    real,              intent(in   ) :: t3_tmp ( : ) ! t0_tmp**2 / 4.
    real,              intent(in   ) :: z0_tmp ( : ) ! vert z to each map 
    real,              intent(in   ) :: z2_tmp ( : ) ! z0_tmp**2
    real,              intent(in   ) :: re_map ( : ) ! anisotropic coefficient
    real,              intent(in   ) :: rv_map ( : ) ! (nt_map)
    real,              intent(inout) :: rt_map ( : ) ! (nt_map)
    real,              intent(inout) :: rf_map ( : ) ! (nt_map)
    real,              intent(inout) :: ra_map ( : ) ! (nt_map)
    !
    integer                          :: jt_map_1
    integer                          :: it_map
    real                             :: f1_alias
    !
    real                             :: s0_map, s1_map, s2_map
    real                             :: g0_map, g1_map, g2_map
    real                             :: t2_map
    real                             :: t2_inp
    real                             :: cz_mig
    real                             :: cz_sqr
    real                             :: cs_dip
    real                             :: rf_dmo
    !
    real                             :: v_src_rec_sq 
    real                             :: vt_inp
    real                             :: r1_alias
    real                             :: sr_mig_sq
    real                             :: gr_mig_sq
    real                             :: sr_inv_sq
    real                             :: gr_inv_sq
    real                             :: rx_inv, ry_inv, rz_inv
    real                             :: sx_inv, sy_inv, sz_inv
    real                             :: gx_inv, gy_inv, gz_inv
    real                             :: vr_mig, sr_inv, gr_inv
    real                             :: a0_eta, s0_eta, g0_eta
    real                             :: t1_inp
    real                             :: amp_ave
    real                             :: amp_sum
    real                             :: r0_eps 
    real                             :: ra_eps ! min allowed amp
    !
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    !if ( kfun_i_pel() .eq. 0 ) &
    !print'(" ktime_time_forward p=",i4," c=",i8)', &
    !kfun_i_pel(), i_call
    !
    ra_eps = .001 ! min allowed amp
    !
    ! Tx   = sqrt[ T0**2 + x**2 / Vnmo**2 * B0]
    !
    ! where
    ! Tx   = the travel time from source to image location
    ! T0   = the vertical travel time
    ! x    = horizontal distance from source to image location
    ! Vnmo = NMO velocity
    ! A0   = 1 / (1. + 2. * ETA)
    ! B0   = (T0**2 + x**2 / Vnmo**2  * A0) / (T0**2 + x**2 / Vnmo**2 )
    ! ETA  = Anisotropic coefficient
    !      = (Vhorizontal**2 - Vnmo**2 ) / (2. * Vnmo**2 )
    ! take abs(sr_mig) , abs(gr_mig) to avoid them inside loops
    ! note times, distances and velocities are scaled by dt_inp
    ! all times have been scaled by this
    !
    ! compute a set of amplitude mask coeficients
    ! ave type cos(dip)                         amp_ave=1., amp_sum = 0.
    ! sum type cos(source)**2+cos(receiver)**2) amp_ave=0., amp_sum = 1.
    !
    r0_eps = .001
    !
    xxif_opt_amp_cosine_ave : &
    if ( string_upper_compare ( opt_amp_cosine(1:1), 'a' ) &
    .or. string_upper_compare ( opt_amp_cosine(1:2), 'hc' ) ) then
      !
      ! use the original amplitude correction ( out of date )
      !
      amp_ave = 1.
      !
      amp_sum = 0.
      !
    else xxif_opt_amp_cosine_ave 
      !
      ! use the correct amplitude correction 
      !
      amp_ave = 0.
      !
      amp_sum = 1.
      !
    end if xxif_opt_amp_cosine_ave 
    !
    v_src_rec_sq = v_src_rec ** 2
    !
    vt_inp    = 1. / dt_inp
    !
    t1_inp    = (nt_inp - 1) * dt_inp + t0_inp
    !
    f1_alias  = num_aa 
    !
    r1_alias  = 2. * dist_aa * vt_inp
    !
    sr_mig_sq = sr_mig**2
    gr_mig_sq = gr_mig**2
    !
    s0_map    = abs(sr_mig)
    g0_map    = abs(gr_mig)
    !
    ! avoid the s0_map = g0_map = t3_tmp(it_map_1) = 0. condition
    !
    jt_map_1 = it_map_1 
    !
    xxif_avoid_zero : if ( abs ( t3_tmp ( it_map_1 ) ) .lt. .001 &
                     .and. abs ( s0_map              ) .lt. .001 &
                     .and. abs ( g0_map              ) .lt. .001 &
                         ) then
      !
      rt_map(jt_map_1) = 0.
      !
      if ( fa_compute ) &
      ra_map(jt_map_1) = 1.
      !
      if ( fa_compute ) &
      rf_map(jt_map_1) = 1.
      !
      jt_map_1 = jt_map_1 + 1
      !
    end if xxif_avoid_zero 
    !
    ! compute the antialias and amplitude coefficients rf_map, ra_map
    !
    !if ( .not. fa_compute ) then
    !print*,' r1_alias=',r1_alias,' f1_alias=',f1_alias
    !print*,' dist_aa=',dist_aa,' vt_inp=',vt_inp
    !print*,' sx_mig=',sx_mig,' sy_mig=',sy_mig,' s0_map=',s0_map
    !print*,' gx_mig=',gx_mig,' gy_mig=',gy_mig,' g0_map=',g0_map
    !end if 
    !
    xxif_fa_compute : if ( fa_compute ) then
      !
      ! compute the mapping coefficients
      !
      do_it_map_1 : do it_map = nt_map , jt_map_1 , -1
        !
        ! compute the time map
        !
        a0_eta = 1 / ( 1. + 2. * re_map(it_map) )
        !
        s1_map = s0_map * rv_map(it_map)
        g1_map = g0_map * rv_map(it_map)
        !
        t2_map = t3_tmp(it_map)
        s2_map = s1_map**2
        g2_map = g1_map**2
        !
        s0_eta = ( t2_map + s2_map * a0_eta ) / ( t2_map + s2_map )
        g0_eta = ( t2_map + g2_map * a0_eta ) / ( t2_map + g2_map )
        !
        t2_inp = sqrt ( t2_map + s2_map * s0_eta ) &
               + sqrt ( t2_map + g2_map * g0_eta )
        !
        if ( t2_inp .lt. t0_inp ) go to 1
        !
        ! compute the time map
        !
        rt_map(it_map) = t2_inp
        !
        !rt_tmp = &
        !sqrt ( (sx_mig**2+sy_mig**2)/2000.**2 + gt_map(it_map)**2/4. ) &
        !+ &
        !sqrt ( (gx_mig**2+gy_mig**2)/2000.**2 + gt_map(it_map)**2/4. )
        !
        !if ( abs(rt_tmp-rt_map(it_map)) .gt. dt_inp ) then
        !print* )' it_map=',it_map,' rv_map=',1./rv_map(it_map)
        !print* )' sx_mig=',sx_mig,' sy_mig=',sy_mig
        !print* )' gx_mig=',gx_mig,' gy_mig=',gy_mig
        !print* )' rt_tmp=',rt_tmp,' rt_map=',rt_map(it_map)
        !print* )' t3_tmp=',t3_tmp(it_map)
        !print* )' t0_tmp=',t0_tmp(it_map)
        !print* )' gt_map=',gt_map(it_map)
        !call kfun_stop (' kfun_time_forward error ', 0 )
        !end if
        !
        ! set the top time map index
        !
        it_map_1 = it_map
        !
        ! compute the antialias and amplitude coefficients rf_map, ra_map
        !
        cz_mig = z0_tmp(it_map)
        !
        cz_sqr = z2_tmp(it_map)
        !
        sr_inv_sq = 1. / ( sr_mig_sq+cz_sqr ) * v_src_rec_sq
        gr_inv_sq = 1. / ( gr_mig_sq+cz_sqr)
        !
        sr_inv    = sqrt ( sr_inv_sq )
        gr_inv    = sqrt ( gr_inv_sq )
        !
        sx_inv = sx_mig * sr_inv
        sy_inv = sy_mig * sr_inv
        sz_inv = cz_mig * sr_inv
        !
        gx_inv = gx_mig * gr_inv
        gy_inv = gy_mig * gr_inv
        gz_inv = cz_mig * gr_inv
        !
        rx_inv = (sx_inv + gx_inv) * .5
        ry_inv = (sy_inv + gy_inv) * .5
        rz_inv = (sz_inv + gz_inv) * .5
        !
        vr_mig = 1. / max(1.e-6, sqrt ( rx_inv**2+ry_inv**2+rz_inv**2 ))
        rx_inv = rx_inv * vr_mig
        ry_inv = ry_inv * vr_mig
        rz_inv = rz_inv * vr_mig
        !
        cs_dip = rz_inv
        !
        ! compute the frequency and amplitude maps
        !
        rf_dmo = r1_alias * sqrt ( max(0., 1.-cs_dip**2 )) * rv_map(it_map)
        !
        rf_map(it_map) = max ( 1., min ( f1_alias, rf_dmo + 1. ) )
        !
        ra_map(it_map) = max ( ra_eps, &
                               amp_ave * cs_dip &
                             + amp_sum * cz_sqr * ( sr_inv_sq + gr_inv_sq ) ) 
        !
        !if ( .not. fa_compute ) &
        !print'(" i_xy=",i5," it=", i8, &
        !& " c=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6,&
        !&" t=", g12.6," f=", g12.6," a=", g12.6," v=", g12.6 )', &
        !i_xy_sum,it_map, &
        !cs_dip, z0_tmp(nt_map)*cz_mig * &
        !( sr_inv_sq + gr_inv_sq ), sr_inv_sq, gr_inv_sq, &
        !rt_map(it_map),rf_map(it_map),ra_map(it_map),1./rv_map(it_map)
        !
      end do do_it_map_1
      !
    else xxif_fa_compute
      !
      !jt_map_2 = it_map_1 - 1
      !
      xxif_nc_vel : if ( nc_vel .eq. 1 ) then    ! isotropic imaging
        !
        ! compute the mapping coefficients
        !
        do_it_map_2 : do it_map = jt_map_1 , it_map_2
          !
          s1_map = s0_map * rv_map(it_map)
          g1_map = g0_map * rv_map(it_map)
          !
          rt_map(it_map) = sqrt ( t3_tmp(it_map) + s1_map**2 ) &
                         + sqrt ( t3_tmp(it_map) + g1_map**2 )
          !
          !if ( .not. fa_compute ) &
          !print'(" i_xy=",i5," it=",i8,&
          !& " t3=",g12.6," s=",g12.6," g=",g12.6," t=",g12.6," v=",g12.6 )', &
          !i_xy_sum, it_map, sqrt(t3_tmp(it_map)), s1_map, g1_map, &
          !rt_map(it_map), 1./rv_map(it_map)
          !
        end do do_it_map_2
        !
      else xxif_nc_vel 
        !
        ! compute the mapping coefficients
        !
        do_it_map_3 : do it_map = jt_map_1 , it_map_2
          !
          ! compute the time map
          !
          a0_eta = 1 / ( 1. + 2. * re_map(it_map) )
          !
          s1_map = s0_map * rv_map(it_map)
          g1_map = g0_map * rv_map(it_map)
          !
          t2_map = t3_tmp(it_map)
          s2_map = s1_map**2
          g2_map = g1_map**2
          !
          s0_eta = ( t2_map + s2_map * a0_eta ) / ( t2_map + s2_map )
          g0_eta = ( t2_map + g2_map * a0_eta ) / ( t2_map + g2_map )
          !
          t2_inp = sqrt ( t2_map + s2_map * s0_eta ) &
                 + sqrt ( t2_map + g2_map * g0_eta )
          !
          ! compute the time map
          !
          rt_map(it_map) = t2_inp
          !
        end do do_it_map_3
        !
      end if xxif_nc_vel
      !
    end if xxif_fa_compute
    !
  1 continue
    !
    !it_map_2 = jt_map_2 
    !
    return
    !
  end subroutine kfun_time_forward
  !
  subroutine kfun_time_inverse ( &
                                 opt_amp_cosine, &
                                 fa_compute, i_xy_sum, v_src_rec, &
                                 sr_mig, sx_mig, sy_mig, &
                                 gr_mig, gx_mig, gy_mig, &
                                 it_map_1, it_map_2, &
                                 num_aa, alias_inc, fctr_aa, dist_aa, &
                                 nt_inp, t0_inp, dt_inp, &
                                 nt_map, gt_map, dt_map, &
                                 t0_tmp, t2_tmp, t3_tmp, t4_tmp, t5_tmp, &
                                 z0_tmp, z2_tmp, &
                                 re_map, rv_map, rt_map, rf_map, ra_map &
                               )
    !
    ! compute the map from input to output for inverse time migration
    ! note rv_map is slowness
    !
    character(len=*),  intent(in   ) :: opt_amp_cosine
    logical,           intent(in   ) :: fa_compute
    integer,           intent(in   ) :: i_xy_sum
    real,              intent(in   ) :: v_src_rec
    real,              intent(in   ) :: sr_mig, sx_mig, sy_mig
    real,              intent(in   ) :: gr_mig, gx_mig, gy_mig
    integer,           intent(inout) :: it_map_1, it_map_2
    integer,           intent(in   ) :: num_aa, alias_inc
    real,              intent(in   ) :: fctr_aa, dist_aa 
    !
    integer,           intent(in   ) :: nt_inp           ! num inp t
    real,              intent(in   ) :: t0_inp           ! min inp t
    real,              intent(in   ) :: dt_inp           ! inc inp t
    !
    integer,           intent(in   ) :: nt_map           ! map var t,z num
    real,              intent(in   ) :: gt_map ( : )     ! map var t,z val
    real,              intent(in   ) :: dt_map ( : )     ! map var t,z inc
    !
    real,              intent(in   ) :: z0_tmp ( : ) ! vert z to each map 
    real,              intent(in   ) :: z2_tmp ( : ) ! z0_tmp**2
    real,              intent(in   ) :: t0_tmp ( : ) ! vertical time to each map
    real,              intent(in   ) :: t2_tmp ( : ) ! t0_tmp**2
    real,              intent(in   ) :: t3_tmp ( : ) ! t0_tmp**2 / 4.
    real,              intent(in   ) :: t4_tmp ( : ) ! 1. / t2_tmp
    real,              intent(in   ) :: t5_tmp ( : ) ! sqrt ( t0_tmp)
    real,              intent(in   ) :: re_map ( : ) ! anisotropic coefficient
    real,              intent(in   ) :: rv_map ( : ) ! (nt_map)
    real,              intent(inout) :: rt_map ( : ) ! (nt_map)
    real,              intent(inout) :: rf_map ( : ) ! (nt_map)
    real,              intent(inout) :: ra_map ( : ) ! (nt_map)
    !
    integer                          :: it_map
    real                             :: f1_alias
    !
    real                             :: sg_map
    real                             :: s0_map, s1_map, s2_map
    real                             :: g0_map, g1_map, g2_map
    real                             :: t2_inp, q2_map
    real                             :: t1_inp
    real                             :: cz_mig, cz_sqr, cs_dip
    real                             :: rf_dmo
    !
    real                             :: r1_alias
    real                             :: vt_inp
    real                             :: sr_mig_sq
    real                             :: gr_mig_sq
    real                             :: rx_inv, ry_inv, rz_inv
    real                             :: sx_inv, sy_inv, sz_inv
    real                             :: gx_inv, gy_inv, gz_inv
    real                             :: vr_mig, sr_inv, gr_inv
    real                             :: amp_ave
    real                             :: amp_sum
    real                             :: sr_inv_sq
    real                             :: gr_inv_sq
    real                             :: v_src_rec_sq 
    real                             :: ra_eps ! min allowed amp
    !
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    !if ( kfun_i_pel() .eq. 0 ) &
    !print'(" ktime_time_inverse p=",i4," c=",i8)', &
    !kfun_i_pel(), i_call
    !
    ra_eps = .001 ! min allowed amp
    !
    v_src_rec_sq = v_src_rec ** 2
    !
    ! compute a set of amplitude mask coeficients
    ! ave type cos(dip)                         amp_ave=1., amp_sum = 0.
    ! sum type cos(source)**2+cos(receiver)**2) amp_ave=0., amp_sum = 1.
    !
    xxif_opt_amp_cosine_ave : &
    if ( string_upper_compare ( opt_amp_cosine(1:1), 'a' ) &
    .or. string_upper_compare ( opt_amp_cosine(1:2), 'hc' ) ) then
      !
      ! use the original amplitude correction ( out of date )
      !
      amp_ave = 1.
      !
      amp_sum = 0.
      !
    else xxif_opt_amp_cosine_ave 
      !
      ! use the correct amplitude correction 
      !
    end if xxif_opt_amp_cosine_ave 
    !
    vt_inp    = 1. / dt_inp
    t1_inp    = (nt_inp - 1) * dt_inp + t0_inp
    f1_alias  = num_aa 
    r1_alias  = 2. * dist_aa * vt_inp
    !
    sr_mig_sq = sr_mig**2
    gr_mig_sq = gr_mig**2
    !
    s0_map    = abs(sr_mig)
    g0_map    = abs(gr_mig)
    sg_map    = (s0_map + g0_map)
    !
    ! initialize the mapping time
    !
    rt_map = 1.
    !
    ! find the first value modeled time which has t_inp > 0
    !
    it_map_2 = nt_map
    !
    ! compute the mapping coefficients
    !
    do_it_map_1 : do it_map = it_map_1 , nt_map
      !
      ! compute the time map
      !
      if ( t0_tmp(it_map) .gt. sg_map * rv_map(it_map) ) go to 1
      it_map_1 = it_map + 1
      !
    end do do_it_map_1
    !
1 continue
    !
    ! compute the antialias and amplitude coefficients rf_map, ra_map
    !
    xxif_fa_compute : if ( fa_compute ) then
      !
      ! compute the mapping coefficients
      !
      do_it_map_2 : do it_map = it_map_1 , nt_map
        !
        ! compute the time map
        !
        cz_mig = z0_tmp(it_map)
        cz_sqr = z2_tmp(it_map)
        !
        s1_map = s0_map * rv_map(it_map)
        g1_map = g0_map * rv_map(it_map)
        s2_map = s1_map**2
        g2_map = g1_map**2
        q2_map = s2_map + g2_map - t2_tmp(it_map)
        t2_inp = sqrt (  max(0., &
       (q2_map**2 - 4. * s2_map * g2_map) * t4_tmp(it_map)))
        !
        rt_map(it_map) = t2_inp
        !
        ! compute the antialias and amplitude coefficients rf_map, ra_map
        !
        sr_inv = 1. / sqrt ( sr_mig_sq+cz_sqr ) * v_src_rec
        !
        gr_inv = 1. / sqrt ( gr_mig_sq+cz_sqr )
        !
        sx_inv = sx_mig * sr_inv
        sy_inv = sy_mig * sr_inv
        sz_inv = cz_mig * sr_inv
        !
        gx_inv = gx_mig * gr_inv
        gy_inv = gy_mig * gr_inv
        gz_inv = cz_mig * gr_inv
        !
        rx_inv = (sx_inv + gx_inv) * .5
        ry_inv = (sy_inv + gy_inv) * .5
        rz_inv = (sz_inv + gz_inv) * .5
        !
        vr_mig = 1. / max(1.e-6, sqrt ( rx_inv**2+ry_inv**2+rz_inv**2 ))
        rx_inv = rx_inv * vr_mig
        ry_inv = ry_inv * vr_mig
        rz_inv = rz_inv * vr_mig
        !
        cs_dip = rz_inv
        !
        ! compute the time, frequency and amplitude maps
        ! take dt_dx = sin(angle) * slowness
        ! or dt_alias = dist_aa * sqrt ( 1.-cs_dip**2 ) * rv_map
        !
        rf_dmo = r1_alias * sqrt ( max(0., 1.-cs_dip**2 )) * rv_map(it_map)
        !
        rf_map(it_map) = max ( 1., min ( f1_alias, rf_dmo + 1. ) )
        !
        !ra_map(it_map) = max ( ra_eps, cs_dip)
        !
        sr_inv_sq = 1. / ( sr_mig_sq+cz_sqr ) * v_src_rec_sq
        gr_inv_sq = 1. / ( gr_mig_sq+cz_sqr)
        !
        ra_map(it_map) = 1. / max ( ra_eps, t2_inp )
        !
        ra_map(it_map) = 1. / max ( ra_eps, t2_inp )
        !
        !ra_map(it_map) = 1. / max ( ra_eps, &
        !                       amp_ave * cs_dip &
        !                     + amp_sum * cz_sqr * ( sr_inv_sq + gr_inv_sq ) ) 
        !
      end do do_it_map_2 
      !
    else xxif_fa_compute
      !
      ! compute the mapping coefficients
      !
      do_it_map_3 : do it_map = it_map_1 , nt_map
        !
        ! compute the time map
        !
        cz_mig = z0_tmp(it_map)
        cz_sqr = z2_tmp(it_map)
        !
        s1_map = s0_map * rv_map(it_map)
        g1_map = g0_map * rv_map(it_map)
        s2_map = s1_map**2
        g2_map = g1_map**2
        q2_map = s2_map + g2_map - t2_tmp(it_map)
        t2_inp = sqrt (  max(0., &
       (q2_map**2 - 4. * s2_map * g2_map) * t4_tmp(it_map)))
        !
        rt_map(it_map) = t2_inp
        !
      end do do_it_map_3 
      !
    end if xxif_fa_compute
    !
    return
    !
  end subroutine kfun_time_inverse
  !
  subroutine kfun_dmo ( &
                        opt_dir, &
                        fa_compute, i_xy_sum, &
                        r0_mig, ro_inp, &
                        it_map_1, it_map_2, &
                        num_aa, alias_inc, fctr_aa, dist_aa, &
                        nt_inp, t0_inp, dt_inp, &
                        nt_out, t0_out, dt_out, &
                        nt_map, gt_map, dt_map, &
                        t5_tmp, &
                        i1_map, i2_map, f1_map, f2_map, &
                        rv_map, rt_map, rf_map, ra_map, &
                        dx_dmo &
                      )
    !
! DMO theory
!
! The forward and inverse DMO theory is based ON:
!
! Data Reconstruction and Reflectivity Mapping in 3 Dimensions
! Robert H. Stolt
! Research Report 2558-1-98, October 1998
!
! For forward DMO the output offset h_dmo, is 0. and equation 75 is : 
!
! data_out ( rx_out, ry_out, h_dmo, t_dmo ) = &
! integral dx_dmo 
! * f_dmo(+45) 
! 1. / sqrt ( 2 * pi ) / h_off 
! * ( 1. + rx_dmo**2 / h_off**2 ) 
! / ( 1. - rx_dmo**2 / h_off**2 ) ** 2
! * t_off / sqrt ( t_dmo ) 
! * data_inp ( rx_inp, ry_inp, h_off, t_off ) 
!
! using t_off = t_dmo * sqrt ( 1. - rx_dmo**2 / h_off**2 ) 
! t_off / sqrt ( t_dmo ) =
! sqrt ( t_dmo ) * ( 1. - rx_dmo**2 / h_off**2 ) ** 5
!
! data_out ( rx_out, ry_out, h_dmo, t_dmo ) = &
! integral dx_dmo 
! * f_dmo(+45) 
! 1. / sqrt ( 2 * pi ) / h_off 
! * ( 1. + rx_dmo**2 / h_off**2 ) 
! / ( 1. - rx_dmo**2 / h_off**2 ) ** 1.5
! * sqrt ( t_dmo ) 
! * data_inp ( rx_inp, ry_inp, h_off, t_off ) 
!
! The time map, equation 76, is:
!
! t_off = t_dmo * sqrt ( - rx_dmo**2 / h_off**2 ) 
!
! For inverse DMO the input offset h_dmo, is 0. and equation 80 is : 
!
! data_out ( rx_out, ry_out, h_off, t_off ) = &
! integral dx_dmo 
! * f_dmo(-45) 
! * 1. / sqrt ( 2 * pi ) / h_off 
! / ( 1. - rx_dmo**2 / h_off**2 ) 
! t_dmo / sqrt ( t_off )
! * data_inp ( rx_inp, ry_inp, h_dmo, t_dmo ) 
!
! using t_dmo = t_off / sqrt ( 1. - rx_dmo**2 / h_off**2 ) 
! t_dmo / sqrt ( t_off ) =
! sqrt ( t_off ) / ( 1. - rx_dmo**2 / h_off**2 ) ** .5
!
! and
!
! data_out ( rx_out, ry_out, h_off, t_off ) = &
! integral dx_dmo 
! * f_dmo(-45) 
! * 1. / sqrt ( 2 * pi ) / h_off 
! / ( 1. - rx_dmo**2 / h_off**2 ) ** 1.5
! * sqrt ( t_off ) 
! * data_inp ( rx_inp, ry_inp, h_dmo, t_dmo ) 
!
! for forward dmo rt_inp = t_off rt_out = t_dmo
! for inverse dmo rt_inp = t_dmo rt_out = t_off
! ra_map(it_map) = ra_dmo * sqrt ( rt_inp )  

!
! The time map, equation 76, is:
!
! t_dmo = t_off / sqrt ( 1. - rx_dmo**2 / h_off**2 ) 
!
! The aperture defined by equation 77, is:
!
! rx_dmo < h_off * b_inp / sqrt ( 1.+b_inp**2 )
!
! rx_dmo < h_off * b_out / sqrt ( 1.+b_out**2 )
!
! where
!
! f_dmo(+45) is a +45 degree phase shift frequency filter.
! f_dmo(-45) is a -45 degree phase shift frequency filter.
! t_dmo =     zero offset time
! t_off = non zero offset time
! t_out = ouptut time
! h_off = non zero half offset
! rx_dmo = distance between input and output midpoints
! b_inp = h_off * s_dmo / (t_off * v_dmo * .5)
! b_out = h_off * s_dmo / (t_off * v_dmo * .5)
! s_dmo = sine(maximum dip in the data) = 1. for this implementation.
! v_dmo = dmo velocity
!
! The aperture for 3D is defined by the grid of points falling to
! either side of the line from the source to the receiver.
!
    ! Dip moveout processing - Dave Hale SEG Course Notes Series, Volume 4.
    !
    ! t_nmo**2 = t_ref**2 - 4. * h_off ** 2 / r_vel**2
    ! t_dmo**2 = t_nmo**2 ( 1 - r_sep**2 / h_off**2 )
    !
    ! t_dmo = dmo time
    ! t_nmo = nmo time
    ! t_ref = reflection time
    ! r_sep = source receiver separation
    ! h_off = source receiver half offset
    ! r_vel = nmo velocity
    !
    ! compute the map from input to output time for dmo
    ! forward dmo rt_inp = rt_out / sqrt ( (rh_dmo**2 - x**2 )  / x**2 )
    ! inverse dmo rt_out = rt_inp * sqrt ( (rh_dmo**2 - x**2 )  / x**2 )
    ! where rh_dmo = input offset, x = distance between
    ! input midpoint and output midpoint
    !
    character(len=*),  intent(in   ) :: opt_dir 
    logical,           intent(in   ) :: fa_compute
    integer,           intent(in   ) :: i_xy_sum
    real,              intent(in   ) :: r0_mig
    real,              intent(in   ) :: ro_inp
    integer,           intent(inout) :: it_map_1
    integer,           intent(inout) :: it_map_2
    integer,           intent(in   ) :: num_aa
    integer,           intent(in   ) :: alias_inc
    real,              intent(in   ) :: fctr_aa
    real,              intent(in   ) :: dist_aa 
    !
    integer,           intent(in   ) :: nt_inp           ! num inp t
    real,              intent(in   ) :: t0_inp           ! min inp t
    real,              intent(in   ) :: dt_inp           ! inc inp t
    !
    integer,           intent(in   ) :: nt_out           ! num out t,z
    real,              intent(in   ) :: t0_out           ! min out t,z
    real,              intent(in   ) :: dt_out           ! inc out t,z
    !
    integer,           intent(in   ) :: nt_map           ! map var t,z num
    real,              intent(in   ) :: gt_map ( : )     ! map var t,z val
    real,              intent(in   ) :: dt_map ( : )     ! map var t,z inc
    real,              intent(in   ) :: t5_tmp ( : )     ! sqrt ( t0_tmp)
    !
    integer,           intent(in   ) :: i1_map ( : )     ! map var t,z index 1
    integer,           intent(in   ) :: i2_map ( : )     ! map var t,z index 2
    real,              intent(in   ) :: f1_map ( : )     ! map var t,z coeff 1
    real,              intent(in   ) :: f2_map ( : )     ! map var t,z coeff 2
    !
    real,              intent(in   ) :: rv_map ( : ) ! (nt_map)
    real,              intent(inout) :: rt_map ( : ) ! (nt_map)
    real,              intent(inout) :: rf_map ( : ) ! (nt_map)
    real,              intent(inout) :: ra_map ( : ) ! (nt_map)
    !
    real                             :: ro_loc
    real                             :: dx_dmo
    real                             :: dx_vh
    integer                          :: it_map
    real                             :: rt_inp
    real                             :: rt_out
    real                             :: vt_inp
    real                             :: t1_inp
    real                             :: rh_dmo
    real                             :: vh_dmo
    real                             :: rx_dmo
    real                             :: ra_dmo
    real                             :: rs_map_0
    real                             :: rs_map_1
    real                             :: rs_map_0_sq
    real                             :: rf_dmo
    real                             :: rf_map_1
    real                             :: f1_alias
    real                             :: r1_alias
    real                             :: r0_mig_vt_inp
    real                             :: vr_mig
    real                             :: h2_rs_map_0
    real                             :: t1_map ! max map time 
    real                             :: r0_eps ! small real value
    logical                          :: zero_offset      ! zero offset case
    logical                          :: zero_distance    ! zero distance case
    logical                          :: inside_aperture  ! inside aper case
    logical                          :: outside_aperture ! outside aper case
    !
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    !if ( i_call .eq. 1 ) &
    !print'( &
    !& /," top kfun_dmo p=",i4," c=",i8," it_map=",i8,1x,i8,&
    !& " r=",g12.6," o=",g12.6," v=",g12.6)', &
    !kfun_i_pel(), i_call, &
    !it_map_1 , it_map_2, r0_mig, ro_inp, rv_map(1)
    !
    ro_loc = max ( 1., abs ( ro_inp ) )
    !
    rh_dmo = .5 * ro_loc     ! half offset
    !
    vh_dmo = matfun_invert_1(rh_dmo) ! 1. / half offset
    !
    vt_inp = matfun_invert_1(dt_inp) ! 1. / dt_inp
    !
    r0_mig_vt_inp = r0_mig * vt_inp  ! mid to mig sep / dt_inp
    !
    vr_mig = matfun_invert_1(r0_mig) ! 1. / mid to mig sep
    !
    ! Data Reconstruction and Reflectivity Mapping in 3 Dimensions
    ! Robert H. Stolt
    ! Research Report 2558-1-98, October 1998
    !
    !rx_dmo = rx_dmo**2 / h_off**2 ) 
    !
    rx_dmo = ( r0_mig * vh_dmo ) ** 2 ! ( mid to mig sep / half offset )**2
    !
    xxif_forward_a : &
    if (  string_upper_compare ( opt_dir, 'FORWARD' ) ) then
      !
! data_out ( rx_out, ry_out, h_dmo, t_dmo ) = &
! integral dx_dmo 
! * f_dmo(+45) 
! 1. / sqrt ( 2 * pi ) / h_off 
! * ( 1. + rx_dmo**2 / h_off**2 ) 
! / ( 1. - rx_dmo**2 / h_off**2 ) ** 1.5
! * sqrt ( t_dmo ) 
! * data_inp ( rx_inp, ry_inp, h_off, t_off ) 
!
      !
      ra_dmo = 1. / sqrt ( 2 * pi ) / rh_dmo &
             * ( 1. + rx_dmo ) &
             / ( 1. - rx_dmo ) ** 1.5
      !
    else xxif_forward_a 
      !
! data_out ( rx_out, ry_out, h_off, t_off ) = &
! integral dx_dmo 
! * f_dmo(-45) 
! * 1. / sqrt ( 2 * pi ) / h_off 
! / ( 1. - rx_dmo**2 / h_off**2 ) ** 1.5
! * sqrt ( t_off ) 
! * data_inp ( rx_inp, ry_inp, h_dmo, t_dmo ) 
!
      !
      ra_dmo = 1. / sqrt ( 2 * pi ) / rh_dmo &
             / ( 1. - rx_dmo ) ** 1.5
      !
    end if xxif_forward_a 
    !
    dx_vh = dx_dmo * vh_dmo ! mig inc / half offset
    !
    r1_alias =  fctr_aa * dist_aa
    !
    f1_alias = num_aa
    !
    t1_inp = ( nt_inp - 1 ) * dt_inp * 1.5 + t0_inp 
    !
    t1_inp = ( nt_inp - 1 ) * dt_inp + t0_inp 
    !
    ! compute flags for special cases
    !
    r0_eps = 1.e-5   ! small real values
    !
    zero_offset      = abs ( ro_inp ) .le. r0_eps ! zero offset   case
    !
    zero_distance    = abs ( r0_mig ) .le. r0_eps ! zero distance case
    !
    inside_aperture  = abs ( rh_dmo ) .ge. abs ( r0_mig + 1. )! inside aper case
    !
    inside_aperture  = inside_aperture .or. ( zero_offset .and. zero_distance )
    !
    outside_aperture = .not. inside_aperture  ! outside aper case
    !
    !print'(" kfun_dmo p=",i4," c=",i8," o=",l2," d=",l2," i=",l2, &
    !& " it_map=",i8,1x,i8,&
    !& " r=",g12.6," h=",g12.6," v=",g12.6)', &
    !kfun_i_pel(), i_call, &
    !zero_offset, zero_distance, inside_aperture, &
    !it_map_1 , it_map_2, r0_mig, rh_dmo, rv_map(1)
    !
    ! if this output location is outside the aperture nullify the map and return
    !
    rt_map(1:nt_map) = 0
    !
    rf_map(1:nt_map) = 1
    !
    ra_map(1:nt_map) = 0
    !
    xxif_outside_aperture : if ( outside_aperture ) then
      !
      it_map_1 = 1
      !
      it_map_2 = 0
      !
      !rt_map(1:nt_map) = 0
      !
      !rf_map(1:nt_map) = 1
      !
      !ra_map(1:nt_map) = 0
      !
      go to 1
      !
    end if xxif_outside_aperture 
    !
    ! initalize the map function to the identity
    !
    it_map_1 = 1
    !
    it_map_2 = nt_map
    !
    !ra_dmo = dx_dmo * pi / rh_dmo ! pi * mig inc / half offset
    !
    ! a_dmo = ((1+a_inp) / (1.-a_inp)**3/2)
    ! a_inp = rx_dmo**2 / h_inp**2
    !
    do_it_map_0 : do it_map = it_map_1 , it_map_2
      !
      rt_inp         = gt_map(it_map)
      !
      rt_inp         = gt_map(it_map)
      !
      rt_map(it_map) = rt_inp
      !
      rf_map(it_map) = 1.
      !
      ra_map(it_map) = ra_dmo * sqrt ( rt_inp ) 
      !
    end do do_it_map_0 
    !
    ! for zero distance return the identity map
    ! this should capture the zero offset case as well
    !
    if ( zero_distance ) go to 1
    !
    ! stop if we find a logical error
    !
    if ( zero_offset ) call kfun_stop ( ' kfun_dmo zero_offset', i_call )
    !
    xxif_forward_0 : &
    if (  string_upper_compare ( opt_dir, 'FORWARD' ) ) then
      !
      ! for forward migration 
      ! t1_map is the max time after dmo
      ! t1_inp is the max time before dmo
      !
      t1_map = t1_inp * sqrt ( 1 - r0_mig**2 / rh_dmo ** 2 )
      !
    else xxif_forward_0 
      !
      ! for inverse migration 
      ! t1_inp is the max time after dmo
      ! t1_map is the max time before dmo
      !
      t1_map = t1_inp / sqrt ( 1 - r0_mig**2 / rh_dmo ** 2 )
      !
    end if xxif_forward_0 
    !
    !if ( i_call .eq. 1 ) &
    !print'(" kfun_dmo p=",i4," c=",i8," t1=",g12.6,1x,g12.6,&
    !& " dx_vh=",g12.6," rx_dmo=",g12.6)', &
    !kfun_i_pel(), i_call, t1_inp, t1_map, dx_vh, rx_dmo
    !
    ! compute 1. - x**2 / h**2 = 1. - rx_dmo
    ! compute the term sqrt ( 1 - r0_mig**2 / rh_dmo ** 2 )
    ! include the special case
    !
    rs_map_0_sq = max ( 0., 1. - rx_dmo )
    !
    rs_map_0 = sqrt ( rs_map_0_sq ) ! sqrt ( 1. - x**2 / h**2 )
    !
    h2_rs_map_0 = rs_map_0
    !
!print'(" aa1 r0_mig=",g12.6," dx_vh=",g12.6)', r0_mig, dx_vh
    !
    xxif_forward : &
    if ( string_upper_compare ( opt_dir, 'FORWARD' ) ) then
      !
      ! for forward migration 
      ! t1_map is the max time after dmo
      ! t1_inp is the max time before dmo
      !
      t1_map = t1_inp * sqrt ( 1 - r0_mig**2 / rh_dmo ** 2 )
      !
      ! for forward dmo rt_inp = rt_out / sqrt ( 1.-x**2/h**2 )
      !
      rs_map_1 = matfun_invert_1 ( rs_map_0 ) ! 1. / sqrt ( 1. - x**2 / h**2 )
      !
      !ra_dmo = matfun_invert_1 ( rs_map_0_sq ) * ( 1.+rx_dmo ) * dx_vh
      !
!print'(" aa3 ra_dmo=",g12.6)', ra_dmo
      !
      rf_map_1 = r1_alias  * vh_dmo**2 * r0_mig_vt_inp * rs_map_1**3 
      !
    else xxif_forward 
      !
      ! for inverse migration 
      ! t1_inp is the max time after dmo
      ! t1_map is the max time before dmo
      !
      t1_map = t1_inp / sqrt ( 1 - r0_mig**2 / rh_dmo ** 2 )
      !
      ! for inverse dmo rt_inp = rt_out * sqrt ( 1.-x**2/h**2 )
      !
      rs_map_1 = rs_map_0 ! sqrt ( 1. - x**2 / h**2 )
      !
      !ra_dmo = matfun_invert_1 ( rs_map_0_sq ) * dx_vh 
      !
!print'(" aa4 ra_dmo=",g12.6)', ra_dmo
      !
      rf_map_1 = r1_alias  * vh_dmo**2 * r0_mig_vt_inp  / rs_map_1
      !
    end if xxif_forward
    !
    ! it_map_2 and it_map_1 are the grid nodes on the non uniform grid
    ! defined by nt_map, gt_map
    !
    do_it_map_1 : do it_map = it_map_1 , it_map_2
      !
      ! for forward dmo rt_inp = t_off rt_out = t_dmo
      ! for inverse dmo rt_inp = t_dmo rt_out = t_off
      ra_map(it_map) = ra_dmo * sqrt ( rt_inp )  
      !
      rt_out = gt_map(it_map)
      !
      rt_inp = rt_out * rs_map_1
      !
      if ( rt_inp .lt. t0_inp .or. rt_inp .gt. t1_inp ) exit
      !
      it_map_2 = it_map
      !
      rt_map(it_map) = rt_inp
      !
! forward dmo * t_off / sqrt ( t_dmo ) 
! inverse dmo * t_dmo / sqrt ( t_off )
      ! t5_tmp ( : ) ! sqrt ( t0_tmp)
      !
      !ra_map(it_map) = ra_dmo * sqrt ( rt_inp )  
      !
      ra_map(it_map) = ra_dmo * t5_tmp ( it_map )
      !
!if ( it_map .eq. 21. ) &
!print'(" sqs c=",i8," a=",g12.6," 1=",g12.6," t=",g12.6)', &
!i_call, ra_map(it_map), ra_dmo, sqrt ( rt_inp )
! a_dmo = ((1+a_inp) / (1.-a_inp)**3/2)
! a_inp = rx_dmo**2 / h_inp**2
!      xxif_forward_2 : &
!      if ( string_upper_compare ( opt_dir,'FORWARD') ) then     
        !
! t_inp = t_out * sqrt ( 1.-a_inp)
!
! The amplitude term, equation 78, is:
!
! a_dmo = (sqrt ( t_out)/h_inp) * ((1+a_inp) / (1.-a_inp)**3/2)
! a_inp = rx_dmo**2 / h_inp**2
        !ra_dmo = dx_dmo * pi / rh_dmo ! pi * mig inc / half offset
        ! ra_map(it_map) = ra_dmo * sqrt ( rt_inp )  &
        !* ( 1 + rt_inp * r0_mig / h2_rs_map_0 )**0.25
        !
!         ra_map(it_map) = ra_dmo * sqrt ( rt_inp )  
        !
!if ( it_map .eq. 21. ) &
!print'(" sqs c=",i8," a=",g12.6," 1=",g12.6," t=",g12.6," b=",g12.6)', &
!i_call, ra_map(it_map), ra_dmo, sqrt ( rt_inp ), &
!( 1 + rt_inp * r0_mig / h2_rs_map_0 )**0.25
        !
!      else xxif_forward_2 
        !
!        ra_map(it_map) = ra_dmo * sqrt ( rt_inp )  &
!        * ( 1 + rt_inp * r0_mig / ( h2_rs_map_0 * sqrt ( rs_map_0 ) ) )**0.25
        !
!      end if xxif_forward_2 
      !
      ! compute the time, frequency and amplitude maps
      ! take dt_dx = 
      ! or dt_alias = dist_aa * dt_dx
      ! rf_map is the anti alias index to use
      !
      rf_dmo = rt_out * rf_map_1   
      !
      rf_map(it_map) = max(1., min(f1_alias, rf_dmo+1.))
      !
      if ( rt_inp .lt. t0_inp ) it_map_1 = it_map + 1
      !
      !if ( i_call .eq. 62 ) &
      !print'(" kfun_dmo a2 p=",i4," c=",i8," it_map=",i8, &
      !& " t=",f8.5,1x,f8.5,1x,f8.5,1x,f8.5,1x,g12.6,1x)', &
      !kfun_i_pel(), i_call, it_map, rt_inp, rt_out, &
      !rt_map(it_map), rf_map(it_map), ra_map(it_map) 
      !
    end do do_it_map_1 
    !
    ! come to here if zero distance
    !
  1 continue
    !
    !rf_map(1:nt_map) = 1.
    !
    !ra_map(1:nt_map) = 1.
    !
    !if ( i_call .eq. 1 ) &
    !print'(" aa2 kfun_dmo p=",i4," c=",i8," it_map=",i8,1x,i8," q=",g12.6)', &
    !kfun_i_pel(), i_call, it_map_1 , it_map_2, rx_dmo
    !
    !print'(" end kfun_dmo p=",i4," c=",i8," it_map=",i8,1x,i8,1x,g12.6,&
    !& " rt_map=",g12.6,1x,g12.6, &
    !& " rf_map=",g12.6,1x,g12.6, &
    !& " ra_map=",g12.6,1x,g12.6 &
    !& )', &
    !kfun_i_pel(), i_call, &
    !it_map_1 , it_map_2, &
    !ra_map(21), &
    !minval(rt_map(1:nt_map)), maxval(rt_map(1:nt_map)), &
    !minval(rf_map(1:nt_map)), maxval(rf_map(1:nt_map)), &
    !minval(ra_map(1:nt_map)), maxval(ra_map(1:nt_map))
    !
    !print'(" c=",i8," it=",i8," gt=",g12.6,&
    !& " rt=",g12.6," rf=",g12.6," ra=",g12.6)', &
    !( i_call, it_map, &
    !gt_map(it_map), rt_map(it_map), rf_map(it_map), ra_map(it_map), &
    !it_map = 1 , nt_map )
    !
    !if ( abs ( r0_mig ) .lt. abs ( rh_dmo ) .and. it_map_2 .lt. 10 ) &
    !call kfun_stop (' kfun_dmo ', i_call )
    !
    return
    !
  end subroutine kfun_dmo
  !
  subroutine kfun_set_range ( nx_mig, x0_mig, dx_mig, x1_mig)
    !
    ! make sure the image increment is positive and min is less than max
    !
    integer,           intent(inout) :: nx_mig
    real,              intent(inout) :: x0_mig
    real,              intent(inout) :: dx_mig
    real,              intent(inout) :: x1_mig
    !
    ! Local variables
    !
    real                             :: rx_temp
    !
    x1_mig = (nx_mig - 1) * dx_mig + x0_mig
    !
    xxif_dx_mig : if ( dx_mig .lt. 0 ) then
      !
      rx_temp = x1_mig
      x1_mig = x0_mig
      x0_mig = x1_mig
      dx_mig = abs(dx_mig)
      !
    end if xxif_dx_mig
    !
    return
    !
  end subroutine kfun_set_range
  !
  subroutine kfun_xyz_tab_max ( i, z1_tab, r_xy_tab )
    !
    ! compute the maximum travel time size
    ! in the radial (r_xy_tab) and vertical (z1_tab) directions
    !
    type ( ktime_struct ),     pointer :: i  ! ktime  tables   structure
    !
    real,                intent(inout) :: z1_tab           ! tab max z value
    real,                intent(inout) :: r_xy_tab         ! tab max r value
    !
    ! Local variables
    !
    integer                         :: iy
    !
    z1_tab = i%g%rg_tab ( i%g%ng_tab )
    !
    r_xy_tab = 0.
    !
    do_iy : do iy = 1 , i%ny_tab
      !
      r_xy_tab = max ( r_xy_tab, &
      sqrt ( i%x0_tab(iy) ** 2 + ( ( iy - 1 ) * i%dy_tab + i%y0_tab ) **2 ) )
      !
    end do do_iy
    !
    return
    !
  end subroutine kfun_xyz_tab_max
  !
  subroutine kfun_stop ( title, i_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    integer,           intent(in   ) :: i_flag
    !
    !print'( &
    print'( &
    & /, a, /, " p=", i4, " stopping i_flag=", i10 )', &
    trim(title), kfun_i_pel(), i_flag
    !
    if ( pc_get_lun() .ne. 6 ) &
    write(pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping i_flag=", i10, &
    & " print unit=", i8)')&
    trim(title), kfun_i_pel(), i_flag, pc_get_lun()
    !
    call pc_error (' end of kfun_stop  ' )
    !
    stop
    !
  end subroutine kfun_stop 
  !
  subroutine kfun_stop_r ( title, r_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    real,              intent(in   ) :: r_flag
    !
    !print'( &
    print'( &
    & /, a, /, " p=", i4, " stopping r_flag=", g12.6 )', &
    trim(title), kfun_i_pel(), r_flag
    !
    if ( pc_get_lun() .ne. 6) &
    write(pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping r_flag=", g12.6, &
    & " print unit=", i8)')&
    trim(title), kfun_i_pel(), r_flag, pc_get_lun()
    !
    call pc_error (' end of kfun_stop_r ' )
    !
    stop
    !
  end subroutine kfun_stop_r
  !
  subroutine kfun_stop_d ( title, d_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    double precision,  intent(in   ) :: d_flag
    !
    !print'( &
    print'( &
    & /, a, /, " p=", i4, " stopping d_flag=", g12.6 )', &
    trim(title), kfun_i_pel(), d_flag
    !
    if ( pc_get_lun() .ne. 6) &
    write(pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping d_flag=", g12.6, &
    & " print unit=", i8)') &
    trim(title), kfun_i_pel(), d_flag, pc_get_lun()
    !
    call pc_error (' end of kfun_stop_d ' )
    !
    stop
    !
  end subroutine kfun_stop_d
  !
  subroutine kfun_stop_l ( title, l_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    logical,           intent(in   ) :: l_flag
    !
    !print'( &
    print'( &
    & /, a, /, " p=", i4, " stopping l_flag=", l2 )', &
    trim(title), kfun_i_pel(), l_flag
    !
    if ( pc_get_lun() .ne. 6) &
    write(pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping l_flag=", l2, &
    & " print unit=", i8)')&
    trim(title), kfun_i_pel(), l_flag, pc_get_lun()
    !
    call pc_error (' end of kfun_stop_l ' )
    !
    stop
    !
  end subroutine kfun_stop_l
  !
  subroutine kfun_stop_c ( title, c_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    character(len=*),  intent(in   ) :: c_flag
    !
    !print'( &
    print'( &
    & /, a, /, " p=", i4, " stopping c_flag=", a )', &
    trim(title), kfun_i_pel(), trim(c_flag)
    !
    if ( pc_get_lun() .ne. 6) &
    write(pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping c_flag=", a, &
    & /, " print unit=", i8)')&
    trim(title), kfun_i_pel(), trim(c_flag), pc_get_lun()
    !
    call pc_error (' end of kfun_stop_c ' )
    !
    stop
    !
  end subroutine kfun_stop_c
  !
  integer function kfun_iz ( ng_tab, nz_tab, iz_tab )
    !
    ! return z index for variable depth step
    !
    integer,           intent(in   ) :: ng_tab           ! tab var t,z num
    integer,           intent(in   ) :: nz_tab
    integer,           intent(in   ) :: iz_tab
    !
    integer                          :: jz_tab
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    jz_tab = ng_tab - nz_tab + iz_tab
    !
    xxif_jz_tab : if ( jz_tab .lt. 1 .or. jz_tab .gt. ng_tab ) then
      !
      print'(" kfun_iz err c=",i8," p=",i4," ng_tab=", i8, &
      & " nz_tab=", i8," iz_tab=", i8," jz_tab=", i8)', &
      i_call, kfun_i_pel(), ng_tab, nz_tab, iz_tab, jz_tab
      !
      stop
      !
    end if xxif_jz_tab 
    !
    kfun_iz = jz_tab
    !
  end function kfun_iz 
  !
  integer function kfun_inc ( nx )
    !
    ! set an integer to 1 or nx / 100 if nx > 100
    !
    integer,           intent(in   ) :: nx ! num of points kfun_inc will sense
    !
    xxif_nx : if ( nx .le. 10 ) then
      !
      kfun_inc = 1
    else if ( nx .le. 100 ) then
      !
      kfun_inc = 10
      !
    else xxif_nx
      !
      kfun_inc = nx / 10
      !
    end if xxif_nx
    !
    return
    !
  end function kfun_inc
  !
  real function kfun_eps ( )
    !
    ! small real number
    !
    kfun_eps = 1.e-4
    !
    return
    !
  end function kfun_eps
  !
  integer function kfun_i_pel ( )
    !
    ! current worker index
    !
    kfun_i_pel = pcpsx_i_pel()
    !
    return
    !
  end function kfun_i_pel
  !
  integer function kfun_n_pel ( )
    !
    ! number of workers
    !
    integer                          :: num_cpus
    !
    xxif_process_traces : if ( .not. pc_do_not_process_traces() ) then
      !
      ! during execution use the mpi call for the number of pes
      !
      kfun_n_pel = pcpsx_n_pel()
      !
      !if ( kfun_cps_code ) &
      !kfun_n_pel = num_cpus
      !
    else xxif_process_traces
      !
      ! in the seisspace frontend use the setup value for the number of pes
      !
      num_cpus = kfun_num_cpus
      !
      ! in the cps frontend use the jobdata call for the number of pes
      !
     !if ( kfun_is_cps() ) &  !RSD
      if ( kfun_cps_code ) & 
      call pc_get_jdata ( 'num_cpus', num_cpus )
      !
      kfun_n_pel = num_cpus
      !
    end if xxif_process_traces
    !
    return
    !
  end function kfun_n_pel

  logical function kfun_is_cps() result(is_cps)
    character(len=64)                ::  sval
    sval = 'NONE'
    call getsys_env("RUNNING_SEISSPACE", sval)
    if(sval=='NONE') then
      is_cps = .true.
    else
      is_cps = .false.
    endif
    return
  end function kfun_is_cps
  !
  integer function kfun_print ()
    !
    ! return print flag
    !
    kfun_print = kfun_debug_print_level
    !
    return
    !
  end function kfun_print
  !
  integer function kfun_x_index ( i_xy_mig, nx_mig )
    !
    ! compute the x index, ix_mig, from the x,y index, i_xy_mig
    ! i_xy_mig =   ( iy_mig   - 1 ) * nx_mig   + ix_mig
    ! ix_mig = mod ( i_xy_mig - 1   , nx_mig ) + 1
    ! iy_mig =     ( i_xy_mig - 1 ) / nx_mig   + 1
    !
    integer,           intent(in   ) :: i_xy_mig ! x,y index
    integer,           intent(in   ) :: nx_mig           ! num mig x
    !
    ! Local variables
    !
    integer                          :: ix_mig   ! x index
    integer                          :: iy_mig   ! y index
    integer                          :: j_xy_mig ! x,y index
    !
    ix_mig = mod ( i_xy_mig - 1 , nx_mig ) + 1
    iy_mig =     ( i_xy_mig - 1 ) / nx_mig   + 1
    j_xy_mig =   ( iy_mig   - 1 ) * nx_mig   + ix_mig
    !
    if ( i_xy_mig .ne. j_xy_mig ) then
      !
      print'( &
      & /, " error in kfun_x_index ", &
      & /, " i_xy_mig=", i8, " j_xy_mig=", i8, " ix_mig=", i8, " iy_mig=", i8 &
      & )', &
      i_xy_mig, j_xy_mig, ix_mig, iy_mig
      !
      call kfun_stop ( ' kfun_x_index_error ', 0 )
      !
    end if    ! if ( i_xy_mig .ne. j_xy_mig ) then
    !
    kfun_x_index = ix_mig
    !
    return
    !
  end function kfun_x_index
  !
  integer function kfun_y_index ( i_xy_mig, nx_mig )
    !
    ! compute the y index, iy_mig, from the x,y index, i_xy_mig
    ! i_xy_mig =   ( iy_mig   - 1 ) * nx_mig   + ix_mig
    ! ix_mig = mod ( i_xy_mig - 1   , nx_mig ) + 1
    ! iy_mig =     ( i_xy_mig - 1 ) / nx_mig   + 1
    !
    integer,           intent(in   ) :: i_xy_mig ! x,y index
    integer,           intent(in   ) :: nx_mig           ! num mig x
    !
    ! Local variables
    !
    integer                          :: ix_mig   ! x index
    integer                          :: iy_mig   ! y index
    integer                          :: j_xy_mig ! x,y index
    !
    ix_mig = mod ( i_xy_mig - 1 , nx_mig ) + 1
    iy_mig =     ( i_xy_mig - 1 ) / nx_mig   + 1
    j_xy_mig =   ( iy_mig   - 1 ) * nx_mig   + ix_mig
    !
    if ( i_xy_mig .ne. j_xy_mig ) then
      !
      print'( &
      & /, " error in kfun_y_index ", &
      & /, " i_xy_mig=", i8, " j_xy_mig=", i8, " ix_mig=", i8, " iy_mig=", i8 &
      & )', &
      i_xy_mig, j_xy_mig, ix_mig, iy_mig
      !
      call kfun_stop ( ' kfun_y_index_error ', 0 )
      !
    end if    ! if ( i_xy_mig .ne. j_xy_mig ) then
    !
    kfun_y_index = iy_mig
    !
    return
    !
  end function kfun_y_index
  !
  integer function kfun_d_index ( i_xy_mig, i_xy_dsk )
    !
    ! compute the disk index of the i_xy_mig trace
    ! kfun_d_index = i_xy_dsk ( i_xy_mig ) 
    ! i_xy_mig =   ( iy_mig   - 1 ) * nx_mig   + ix_mig
    ! ix_mig = mod ( i_xy_mig - 1   , nx_mig ) + 1
    ! iy_mig =     ( i_xy_mig - 1 ) / nx_mig   + 1
    !
    integer,           intent(in   ) :: i_xy_mig        ! x,y index
    integer,           intent(in   ) :: i_xy_dsk ( : )  ! grid ->  disk index
    !
    ! Local variables
    !
    if ( i_xy_mig .le. 0 ) then
      !
      kfun_d_index = 0
      !
    else    ! if ( i_xy_mig .le. 0 ) then
      !
      kfun_d_index = i_xy_dsk ( i_xy_mig )
      !
    end if    ! ! if ( i_xy_mig .le. 0 ) then
    !
    return
    !
  end function kfun_d_index
  !
  integer function kfun_m_index ( i_xy_mig, i_xy_dsk, n_xy_mem )
    !
    ! compute the memory index of the i_xy_mig trace
    ! kfun_m_index = 
    ! mod ( abs ( i_xy_dsk ( (iy_mig-1)*nx_mig+ix_mig ) ) - 1 , n_xy_mem ) + 1
    ! i_xy_mig =   ( iy_mig   - 1 ) * nx_mig   + ix_mig
    ! ix_mig = mod ( i_xy_mig - 1   , nx_mig ) + 1
    ! iy_mig =     ( i_xy_mig - 1 ) / nx_mig   + 1
    !
    integer,           intent(in   ) :: i_xy_mig         ! x,y index
    integer,           intent(in   ) :: i_xy_dsk ( : )   ! grid ->  disk index
    integer,           intent(in   ) :: n_xy_mem         ! num trace in memory
    !
    ! Local variables
    !
    xxif_i_xy_mig : if ( i_xy_mig .le. 0 ) then
      !
      kfun_m_index = 0
      !
    else xxif_i_xy_mig
      !
      xxif_i_xy_dsk : if ( i_xy_dsk ( i_xy_mig ) .eq. 0 ) then
        !
        kfun_m_index = 0
        !
      else xxif_i_xy_dsk 
        !
        kfun_m_index = mod ( abs ( i_xy_dsk ( i_xy_mig ) ) - 1 , n_xy_mem ) + 1
        !
      end if xxif_i_xy_dsk 
      !
    end if xxif_i_xy_mig
    !
    return
    !
  end function kfun_m_index
  !
  subroutine kfun_mem_init 
    !
    ! init mem_size counter
    !
    call memfun_sum_get ( kfun_mem_temp )
    !
    !print*,' kfun_mem_init aft memfun_sum_get kfun_mem_temp=',kfun_mem_temp
    !
    call memfun_sum_put ( 0             )
    !
    return
    !
  end subroutine kfun_mem_init
  !
  subroutine kfun_mem_size ( mem_size )
    !
    ! init mem_size counter
    !
    integer,           intent(inout) :: mem_size
    !
    !print*,' kfun_mem_size bef memfun_sum_get kfun_mem_temp=',kfun_mem_temp
    !
    call memfun_sum_get ( mem_size )
    !
    !print*,' kfun_mem_size bef memfun_sum_get mem_size=', mem_size 
    !
    call memfun_sum_put ( kfun_mem_temp )
    !
    kfun_mem_temp = 0
    !
    return
    !
  end subroutine kfun_mem_size
  !
  subroutine kfun_grid_to_scale ( hdr_x, xg_scale, grid_obj )
    !
    ! compute the grid to distance scale coefficient xg_scale
    !
    integer,           intent(in   ) :: hdr_x
    real,              intent(inout) :: xg_scale
    type ( grid_struct )                   :: grid_obj    ! global
    !
    ! Local variables
    !
    integer                  :: i_err
    !
    i_err = 0
    !
    xg_scale = 1.
    !
    xxif_x : &
    if ( hdr_x .eq. HDR_MIDPOINT_XGRID &
    .or. hdr_x .eq. HDR_MIDPOINT_XLOC ) then
      !
      call migfun_get_scale ( 'kfun grid to distance x ', &
      pc_get_lun(), grid_obj, HDR_MIDPOINT_XGRID, xg_scale, i_err )
      !
    else xxif_x 
      !
      call migfun_get_scale ( 'kfun grid to distance y ', &
      pc_get_lun(), grid_obj, HDR_MIDPOINT_YGRID, xg_scale, i_err )
      !
    end if xxif_x 
    !
    return
    !
  end subroutine kfun_grid_to_scale
  !
  subroutine kfun_set_constant_path ( opt_file )
    !
    ! set the constant_path to CONSTANT or PATH
    !
    character(len=*),  intent(inout) :: opt_file
    !
    call string_to_upper ( opt_file )
    !
    xxif_constant : if ( string_upper_compare ( opt_file(1:1), 'C' ) ) then
      !
      opt_file = 'CONSTANT'
      !
    else xxif_constant
      !
      opt_file = 'PATH'
      !
    end if xxif_constant
    !
    return
    !
  end subroutine kfun_set_constant_path
  !
  subroutine kfun_set_opt_ray ( opt_ray, r, l0_flg ) 
    !
    ! set the rayselection type an allowed value - ENERGETIC, FIRST, SHORTEST
    ! old
    ! 1--most energetic    2--first arrival
    ! 3--shortest raypath  4--smallest maximum velocity
    ! 5--longest time      6--longest raypath
    ! new
    ! 1--most energetic    2--lowest max vel
    ! 3--first arrival     4--last arrival
    ! 5--shortest raypath  6--longest raypath  
    !
    !integer, public, save              :: opt_ray_a0 = 1
    !integer, public, save              :: opt_ray_v0 = 2
    !integer, public, save              :: opt_ray_t1 = 3
    !integer, public, save              :: opt_ray_t2 = 4
    !integer, public, save              :: opt_ray_s1 = 5
    !integer, public, save              :: opt_ray_s2 = 6
    !       1--most energetic    2--first arrival
    !       3--shortest raypath  4--smallest maximum velocity
    !       5--longest time      6--longest raypath
    !
    character(len=*),    intent(inout) :: opt_ray
    type ( kray_struct ),      pointer :: r      ! raytracing structure
    logical,             intent(in   ) :: l0_flg
    !
    integer                          :: n_opt_ray
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    call string_to_upper ( opt_ray )
    !
    xxif_opt_ray : if ( string_upper_compare ( opt_ray(1:1), 'E' ) ) then
      !
      opt_ray = 'ENERGETIC'
      !
      n_opt_ray = opt_ray_a0
      !
    else if ( string_upper_compare ( opt_ray(1:1), 'V' ) ) then
      !
      opt_ray = 'VELOCITY'
      !
      n_opt_ray = opt_ray_v0
      !
    else if ( string_upper_compare ( opt_ray(1:1), 'F' ) ) then
      !
      opt_ray = 'FIRST'
      !
      n_opt_ray = opt_ray_t1
      !
    else if ( string_upper_compare ( opt_ray(1:1), 'L' ) ) then
      !
      opt_ray = 'LAST'
      !
      n_opt_ray = opt_ray_t2
      !
    else if ( string_upper_compare ( opt_ray(1:1), 'S' ) ) then
      !
      opt_ray = 'SHORTEST'
      !
      n_opt_ray = opt_ray_s1
      !
    else  xxif_opt_ray
      !
      opt_ray = 'LONGEST'
      !
      n_opt_ray = opt_ray_s2
      !
    end if xxif_opt_ray
    !
    !print'(" kfun_set_opt_ray c=",i8," n_opt_ray=",i12," opt_ray=",a )', &
    !i_call, n_opt_ray, trim(opt_ray)
    !
    ! set the ray selection flags
    !
    xxif_l0_flg : if ( l0_flg ) then
    !
    r%l_opt_ray ( opt_ray_a0 ) = n_opt_ray .eq. opt_ray_a0        ! max amp
    r%l_opt_ray ( opt_ray_v0 ) = n_opt_ray .eq. opt_ray_v0        ! min vel
    r%l_opt_ray ( opt_ray_t1 ) = n_opt_ray .eq. opt_ray_t1        ! min tim
    r%l_opt_ray ( opt_ray_t2 ) = n_opt_ray .eq. opt_ray_t2        ! max tim
    r%l_opt_ray ( opt_ray_s1 ) = n_opt_ray .eq. opt_ray_s1        ! min len
    r%l_opt_ray ( opt_ray_s2 ) = n_opt_ray .eq. opt_ray_s2        ! max len
    r%l_opt_ray ( opt_ray_t0 ) =  r%l_opt_ray ( opt_ray_t1 ) &
                             .or. r%l_opt_ray ( opt_ray_t2 )         ! tim
    r%l_opt_ray ( opt_ray_s0 ) =  r%l_opt_ray ( opt_ray_s1 ) &
                             .or. r%l_opt_ray ( opt_ray_s2 )         ! len
    !
    end if xxif_l0_flg 
    !
    return
    !
  end subroutine kfun_set_opt_ray
  !
  subroutine kfun_set_opt_ray_grad ( opt_ray_grad )
    !
    ! set the ray trace increment option
    !
    character(len=*),    intent(inout) :: opt_ray_grad
    !
    call string_to_upper ( opt_ray_grad )
    !
    xxif_opt_ray_grad : &
    if ( string_upper_compare ( opt_ray_grad(1:1), 'V' ) ) then
      !
      opt_ray_grad = 'VELOCITY'
      !
    else 
      !
      opt_ray_grad = 'SLOWNESS'
      !
    end if xxif_opt_ray_grad
    !
    return
    !
  end subroutine kfun_set_opt_ray_grad
  !
  subroutine kfun_set_opt_ray_step ( opt_ray_step )
    !
    ! set the ray trace time to distance step option
    !
    character(len=*),    intent(inout) :: opt_ray_step
    !
    call string_to_upper ( opt_ray_step )
    !
    xxif_opt_ray_step : &
    if ( string_upper_compare ( opt_ray_step(1:1), 'C' ) ) then
      !
      opt_ray_step = 'CONSTANT'
      !
    else 
      !
      opt_ray_step = 'GRADIENT'
      !
    end if xxif_opt_ray_step
    !
    return
    !
  end subroutine kfun_set_opt_ray_step
  !
  subroutine kfun_set_opt_ray_bend ( opt_ray_bend )
    !
    ! set the ray bending option
    !
    character(len=*),    intent(inout) :: opt_ray_bend
    !
    call string_to_upper ( opt_ray_bend )
    !
    xxif_opt_ray_bend : &
    if ( string_upper_compare ( opt_ray_bend(1:1), 'S' ) ) then
      !
      opt_ray_bend = 'START'
      !
    else 
      !
      opt_ray_bend = 'AVERAGE'
      !
    end if xxif_opt_ray_bend
    !
    return
    !
  end subroutine kfun_set_opt_ray_bend
  !
  subroutine kfun_set_opt_ray_inc ( opt_ray_inc )
    !
    ! set the ray trace increment option
    !
    character(len=*),    intent(inout) :: opt_ray_inc
    !
    call string_to_upper ( opt_ray_inc )
    !
    xxif_opt_ray_inc : &
    if ( string_upper_compare ( opt_ray_inc(1:1), 'R' ) ) then
      !
      opt_ray_inc = 'READ'
      !
    else if ( string_upper_compare ( opt_ray_inc(1:1), 'W' ) ) then
      !
      opt_ray_inc = 'WRITE'
      !
    else if ( string_upper_compare ( opt_ray_inc(1:3), 'COM' ) &
         .or. string_upper_compare ( opt_ray_inc(1:1), 'V' ) ) then
      !
      opt_ray_inc = 'COMPUTE'
      !
    else 
      !
      opt_ray_inc = 'CONSTANT'
      !
    end if xxif_opt_ray_inc
    !
    return
    !
  end subroutine kfun_set_opt_ray_inc
  !
  subroutine kfun_set_opt_ray_fill ( opt_ray_fill )
    !
    ! set opt_ray_fill to old, eikonal or average
    !
    character(len=*),    intent(inout) :: opt_ray_fill
    !
    call string_to_upper ( opt_ray_fill )
    !
    xxif_opt_ray_fill : &
    if ( string_upper_compare ( opt_ray_fill(1:1), 'E' ) ) then
      !
      opt_ray_fill = 'EIKONAL'
      !
    else if ( string_upper_compare ( opt_ray_fill(1:1), 'A' ) ) then
      !
      opt_ray_fill = 'AVERAGE'
      !
    else  xxif_opt_ray_fill
      !
      opt_ray_fill = 'OLD'
      !
    end if xxif_opt_ray_fill
    !
    return
    !
  end subroutine kfun_set_opt_ray_fill 
  !
  subroutine kfun_print_debug_set ( print_debug )
    !
    character(len=*), intent(in)  :: print_debug
    !
    integer                       :: i
    !
    ! set default debug print level index
    !
    kfun_debug_print_level = 1
    !
    ! set print level index based on string input
    !
    do_opt_print_debug : do i = 1, opt_print_debug_n
      !
      xxif_print_debug : &
      if ( string_upper_compare ( print_debug, opt_print_debug_c(i) ) ) then
        !
        kfun_debug_print_level = i - 1
        !
        exit    ! exit the loop with debug print level index
        !
      end if xxif_print_debug 
      !
    end do do_opt_print_debug 
    !
    if ( .not. pc_do_not_process_traces() ) &
    print'( &
    & /, " kfun_print_debug_set p=", i4, " kfun_debug_print_level=", i8, &
    & " print_debug=", a &
    & )', &
    kfun_i_pel(), kfun_debug_print_level, trim ( print_debug )
    !
    return
    !
  end subroutine kfun_print_debug_set
  !
  subroutine kfun_file_name ( &
                              file_name, &
                              file_prefix, file_suffix, &
                              file_index_1, file_index_2 &
                            )
    !
    ! constuct a pathname from a root, extension and up to two indexes
    !
    ! - Arguments
    !
    character(len=*), intent(inout) :: file_name
    character(len=*), intent(in   ) :: file_prefix
    character(len=*), intent(in   ) :: file_suffix
    integer,          intent(in   ) :: file_index_1
    integer,          intent(in   ) :: file_index_2
    !
    ! - Local variables
    !
    character(len=filename_length)  :: job_name 
    !
    call kfun_job_name ( job_name )
    !
    ! open the image trace disk file each pe opens its own file
    !
    xxif_root_empty : &
    if ( string_upper_compare ( file_prefix, pathcheck_empty ) ) then
      !
      file_name = trim ( job_name ) 
      !
    else xxif_root_empty 
      !
      ! if file_prefix is not pathcheck_empty add file_prefix as a prefix
      !
      file_name  = trim ( file_prefix ) 
      !
    end if xxif_root_empty 
    !
    ! if fn_buf does not have a / in it add cpstemp
    !
    xxif_add_cps_temp : &
    if ( index ( file_name, '/' ) .le. 0 ) then
      !
      ! if fn_buf is     pathcheck_empty add cpstemp    as a prefix
      !
      file_name = "~/cpstemp/" // trim ( file_name )
      !
      call exptilde ( file_name )
      !
    end if xxif_add_cps_temp 
    !
    ! get a unique file name, including path, job, ipn and pe
    !
    if ( file_index_1 .ge. 0 .and. file_index_2 .ge. 0 ) &
    call pp_create_file_name (                           &
                               base_name = file_name,    &
                               file_name = file_name,    &
                               i_pn      = file_index_1, &
                               i_worker  = file_index_2  &
                             )
    !
    if ( file_index_1 .ge. 0 .and. file_index_2 .lt. 0 ) &
    call pp_create_file_name (                           &
                               base_name = file_name,    &
                               file_name = file_name,    &
                               i_pn      = file_index_1 &
                             )
    !
    ! add the file name suffix
    !
    if ( .not. string_upper_compare ( file_suffix, pathcheck_empty ) ) &
    file_name = trim ( file_name ) // trim ( file_suffix )
    !
    return
    !
  end subroutine kfun_file_name 
  !
  subroutine kfun_file_size ( file_size_word, file_ext_num, &
                              file_ext_word, file_ext_byte )
    !
    real,              intent(in   ) :: file_size_word
    integer,           intent(inout) :: file_ext_word
    integer,           intent(inout) :: file_ext_byte
    integer,           intent(inout) :: file_ext_num
    !
    integer                          :: two_giga_words
    integer                          :: two_giga_bytes
    integer                          :: file_size_int
    real                             :: file_ext_real
    !
    two_giga_bytes = 2147482624
    !
    two_giga_words = two_giga_bytes / sizeof(1)
    !
    file_size_int = nint ( file_size_word )
    !
    if ( file_size_int .le. 0 ) &
    file_size_int = two_giga_words
    !
    xxif_too_big : if ( file_size_int .gt. two_giga_words ) then
      !
      file_ext_word = two_giga_words
      !
    else xxif_too_big 
      !
      file_ext_word = file_size_int
      !
    end if xxif_too_big 
    !
    file_ext_real = float ( file_ext_word )
    !
    file_ext_byte = sizeof(1) * file_ext_word
    !
    file_ext_num  = nint ( file_size_word / file_ext_real )
    !
    return
    !
  end subroutine kfun_file_size 
  !
  subroutine kfun_file_open ( &
                              file_name, file_unit, &
                              file_prefix, file_suffix, &
                              file_index_1, file_index_2, &
                              file_status, file_mode, file_pel, &
                              file_zero, file_lock, file_alloc, &
                              rec_len, rec_dim, rec_num, &
                              i_err &
                            )
    !
    ! open and zero the local table disk buffer
    !
    character(len=*),  intent(inout) :: file_name
    integer,           intent(inout) :: file_unit
    character(len=*),  intent(in   ) :: file_prefix
    character(len=*),  intent(in   ) :: file_suffix
    integer,           intent(in   ) :: file_index_1
    integer,           intent(in   ) :: file_index_2
    character(len=*),  intent(in   ) :: file_status
    integer,           intent(in   ) :: file_mode
    integer,           intent(in   ) :: file_pel
    logical,           intent(in   ) :: file_zero
    logical,           intent(in   ) :: file_lock
    logical,           intent(in   ) :: file_alloc
    integer,           intent(in   ) :: rec_len
    integer,           intent(in   ) :: rec_dim
    integer,           intent(in   ) :: rec_num
    integer,           intent(inout) :: i_err
    !
    ! Local variables
    !
    real                            :: file_size_word
    real                            :: file_size_byte
    real                            :: file_size_giga
    integer                         :: file_ext_num
    integer                         :: file_ext_word
    integer                         :: file_ext_byte
    integer                         :: file_alloc_0
    integer                         :: file_lock_0
    integer                         :: rec_idx
    real                            :: tr_loc ( rec_len, rec_dim )
    !
    i_err = 0
    !
    ! set file name
    !
    call kfun_file_name ( &
                          file_name, file_prefix, file_suffix, &
                          file_index_1, file_index_2 &
                        )
    !
    ! set file size
    !
    file_size_word = float ( rec_len ) * float ( rec_dim ) * float ( rec_num )
    !
    call kfun_file_size ( &
                          file_size_word, file_ext_num, &
                          file_ext_word, file_ext_byte &
                        )
    !
    file_size_byte = file_size_word * sizeof(1)
    !
    file_size_giga = file_size_byte / 1.e9
    !
    ! set file alloc flag
    !
    xxif_file_alloc : if ( file_alloc ) then
      !
      file_alloc_0 = preallocate_file_space_enabled  ! do     preallocate
      !
    else xxif_file_alloc 
      !
      file_alloc_0 = preallocate_file_space_disabled ! do not preallocate
      !
    end if xxif_file_alloc 
    !
    ! set file lock flag
    !
    xxif_file_lock : if ( file_lock ) then
      !
      file_lock_0 = file_lock_enabled                ! do      lock file
      !
    else xxif_file_lock 
      !
      file_lock_0 = file_lock_disabled               ! do not lock file
      !
    end if xxif_file_lock 
    !
    ! open the file
    !
    call pp_open_file (                                           &
                        i_file            = file_unit,            &
                        i_worker          = file_pel,             &
                        file_name         = file_name,            &
                        file_stat         = file_status,          &
                        i_err             = i_err,                &
                        mode              = file_mode,            &
                        file_space_commit = file_alloc_0,         &
                        file_lock         = file_lock_0,          &
                        file_extent_size  = file_ext_byte         &
                      )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! zero the file
    !
    xxif_file_zero : if ( file_zero ) then
      !
      tr_loc = 0.
      !
      do_rec_idx : do rec_idx = 1 , rec_num
        !
        call pp_write_file ( &
                             i_file   = file_unit,         &
                             i_worker = file_pel,          &
                             n_write  = 1,                 &
                             i_first  = rec_idx,           &
                             n_stride = 1,                 &
                             n_data   = rec_len * rec_dim, &
                             x_data   = tr_loc,            &
                             i_err    = i_err,             &
                             mode     = file_mode          &
                           )
          !
          if ( i_err .ne. 0 ) go to 997
          !
      end do do_rec_idx
      !
    end if xxif_file_zero 
    !
    if ( kfun_i_pel() .eq. 0 ) &
    print'( &
    & /, " kfun_file_open p=", i4, &
    & /, " file_name        =", a, &
    & /, " file_unit        =", i12, &
    & /, " file_prefix      =", a, &
    & /, " file_suffix      =", a, &
    & /, " file_index_1     =", i12, &
    & /, " file_index_2     =", i12, &
    & /, " file_mode        =", i12, &
    & /, " file_pel         =", i12, &
    & /, " file_zero        =", l2, &
    & /, " file_alloc       =", l2, &
    & /, " file_lock        =", l2, &
    & /, " file_size_word   =", g12.6, &
    & /, " file_size_byte   =", g12.6, &
    & /, " file_size_giga   =", g12.6, &
    & /, " file_ext_num     =", i12, &
    & /, " file_ext_word    =", i12, &
    & /, " file_ext_byte    =", i12, &
    & /, " rec_len          =", i12, &
    & /, " rec_dim          =", i12, &
    & /, " rec_num          =", i12 &
    & )', &
    kfun_i_pel(), &
    trim(file_name), file_unit, &
    trim(file_prefix), trim(file_suffix), & 
    file_index_1, file_index_1, &
    file_mode, file_pel, &
    file_zero, file_alloc, file_lock, &
    file_size_word, file_size_byte, file_size_giga, &
    file_ext_num, file_ext_word, file_ext_byte, &
    rec_len, rec_dim, rec_num
    !
    return
    !
997 continue
    !
    if ( kfun_i_pel() .eq. 0 ) &
    print'( &
    & /, " error in kfun_file_open p=", i4, &
    & /, " during pp_write " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
998 continue
    !
    if ( kfun_i_pel() .eq. 0 ) &
    print'( &
    & /, " error in kfun_file_open p=", i4, &
    & /, " during pp_open " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( kfun_i_pel() .eq. 0 ) &
    print'( &
    & /, " error in kfun_file_open p=", i4 &
    & )', &
    kfun_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine kfun_file_open
  !
  subroutine kfun_file_close ( &
                               file_unit, file_mode, file_pel, file_remove, &
                               i_err &
                             )
    !
    ! clos a local file
    !
    integer,           intent(in   ) :: file_unit
    integer,           intent(in   ) :: file_mode
    integer,           intent(in   ) :: file_pel
    logical,           intent(in   ) :: file_remove
    integer,           intent(inout) :: i_err
    !
    i_err = 0
    !
    call pp_close_file ( &
                         i_file   = file_unit,   &
                         i_worker = file_pel,    &
                         l_remove = file_remove, &
                         i_err    = i_err,       &
                         mode     = file_mode    &
                       )
    !
    if ( i_err .ne. 0 ) &
    print'(" error in kfun_file_close p=",i8, &
    & /," during pp_close_file i_err=",i12, &
    & /," file_pel=",i8," file_unit=",i8, &
    & /, " file_remove=",l2," file_mode=",i12)', &
    kfun_i_pel(), i_err, &
    file_pel, file_unit, &
    file_remove, file_mode 
    !
    return
    !
  end subroutine kfun_file_close
  !
  subroutine kfun_set_max_offset ( &
    n_dimension, no_mig, o0_mig, do_mig, ox_mig, oy_mig )
    !
    ! set the max x and y offsets
    !
    integer,          intent(in   ) :: n_dimension ! dimension 2, 3
    integer,          intent(in   ) :: no_mig  ! num offset bins
    real,             intent(in   ) :: o0_mig  ! min offset bins
    real,             intent(in   ) :: do_mig  ! inc offset bins
    real,             intent(inout) :: ox_mig  ! max off in x dir
    real,             intent(inout) :: oy_mig  ! max off in y dir
    !
    real                            :: o1_mig  ! max offset bins
    !
    ! max offset in x
    !
    o1_mig = ( no_mig - 1 ) * do_mig + o0_mig 
    !
    if ( n_dimension .eq. 2 ) &
    ox_mig = - max( abs(o0_mig), abs(o1_mig) ) + do_mig * .5 
    !
    if ( ox_mig .lt. 0. ) &
    ox_mig = max ( abs ( o0_mig ), abs ( o1_mig ) ) + do_mig * .5 
    !
    !ox_mig = min ( ox_mig, max ( abs ( o0_mig), abs ( o1_mig ) ) )
    !
    ! max offset in y
    !
    if ( n_dimension .eq. 2 ) &
    oy_mig = 0.
    !
    if ( oy_mig .lt. 0. ) &
    oy_mig = max ( abs ( o0_mig ), abs ( o1_mig ) ) + do_mig * .5 
    !
    !oy_mig = min ( oy_mig, max ( abs ( o0_mig), abs ( o1_mig ) ) )
    !
    return
    !
  end subroutine kfun_set_max_offset 
  !
  subroutine kfun_set_table_parms ( &
                                    dep_mig, opt_table_operation, &
                                    table_initialize, table_compute, &
                                    table_check, table_image &
                                  )
    !
    ! - Arguments
    !
    ! 7 ) Table operations
    !
    logical,          intent(in   ) :: dep_mig
    character(len=*), intent(inout) :: opt_table_operation
    logical,          intent(inout) :: table_initialize
    character(len=*), intent(inout) :: table_compute
    logical,          intent(inout) :: table_check
    logical,          intent(inout) :: table_image
    !
    ! - Local variables
    !
    call kfun_set_opt_table_operation ( opt_table_operation )
    !
    xxif_table_operation : &
    if ( string_upper_compare ( opt_table_operation, 'USER' ) ) then
      !
      !table_initialize = .true.
      !table_compute    = .true.
      !table_check      = .true.
      !table_image      = .true.
      !
    else if &
    ( string_upper_compare ( opt_table_operation, 'INITIALIZE' )) then
      !
      table_initialize = .true.
      table_compute    = 'NO'
      table_check      = .false.
      table_image      = .false.
      !
    else if ( string_upper_compare ( opt_table_operation, 'CHECK' )) then
      !
      table_initialize = .false.
      table_compute    = 'NO'
      table_check      = .true.
      table_image      = .false.
      !
    else if ( string_upper_compare ( opt_table_operation, 'COMPUTE' )) then
      !
      table_initialize = .false.
      !
      xxif_table_compute_1 : &
      if ( string_upper_compare ( table_compute(1:1), 'A' )) then
        !
        table_compute    = 'ALL'
        !
      else xxif_table_compute_1 
        !
        table_compute    = 'YES'
        !
      end if xxif_table_compute_1 
      !
      table_check      = .false.
      table_image      = .false.
      !
    else if ( string_upper_compare ( opt_table_operation, 'ALL' )) then
      !
      table_initialize = .true.
      !
      xxif_table_compute_2 : &
      if ( string_upper_compare ( table_compute(1:1), 'A' )) then
        !
        table_compute    = 'ALL'
        !
      else xxif_table_compute_2 
        !
        table_compute    = 'YES'
        !
      end if xxif_table_compute_2 
      !
      table_check      = .false.
      table_image      = .true.
      !
    else xxif_table_operation 
      !
      table_initialize = .false.
      table_compute    = 'NO'
      table_check      = .false.
      table_image      = .true.
      !
    end if xxif_table_operation
    !
    call kfun_set_opt_table_operation ( opt_table_operation )
    !
    if ( .not. pc_do_not_process_traces() &
    .and. table_initialize &
    .and. string_upper_compare ( table_compute, 'NO' ) ) &
    table_image = .false.
    !
    ! we cannot use opt_table_operation='all' if depth migration and cpu > 1
    !
    xxif_all : &
    if ( kfun_i_pel() .eq. -999 ) then
    !if ( ( table_initialize .or. table_compute ) .and. table_image &
    !.and. dep_mig .and. kfun_n_pel() .gt. 1 ) then
      !
      xxif_do_not_process_1 : if ( pc_do_not_process_traces() ) then
        !
      call pc_info ( &
' Warning: you cannot use a combination of initalizing or computing ' )
      call pc_info ( &
' travel time tables and imageing data in multiple cpu depth migration jobs. ' )
        !
      else xxif_do_not_process_1
        !
        call pc_error ( &
' Warning: you cannot use a combination of initalizing or computing ' )
        call pc_error ( &
' travel time tables and imageing data in multiple cpu depth migration jobs. ' )
        !
      end if xxif_do_not_process_1
      !
      call pc_info ( ' OPT_TABLE_OPERATION  = ', opt_table_operation )
      call pc_info ( ' TABLE_IMAGE          = ', table_image          )
      call pc_info ( ' TABLE_INITIALIZE     = ', table_initialize     )
      call pc_info ( ' TABLE_COMPUTE        = ', table_compute        )
      call pc_info ( ' Number of cpus this job = ', kfun_n_pel () )
      call pc_info ( ' You should create tables in one job using: ' )
      call pc_info ( ' opt_table_operation = USER , TABLE_IMAGE = NO ' )
      call pc_info ( ' TABLE_INITIALIZE = YES , TABLE_COMPUTE = YES ' )
      call pc_info ( ' and use them in another job using: ' )
      call pc_info ( ' OPT_TABLE_OPERATION = USER , TABLE_IMAGE = YES ' )
      call pc_info ( ' TABLE_INITIALIZE = NO , TABLE_COMPUTE = NO ' )
      !
      xxif_do_not_process_2 : if ( pc_do_not_process_traces() ) then
        !
        call pc_info ( &
' Setting job parameters to initalize and compute travel time tables. ' )
        !
        opt_table_operation = 'USER'
        table_initialize = .true.
        !
        xxif_table_compute : &
        if ( string_upper_compare ( table_compute(1:1), 'A' ) ) then
          !
          table_compute = 'ALL'
          !
        else xxif_table_compute 
          !
          table_compute = 'YES'
          !
        end if xxif_table_compute 
        !
        table_check      = .false.
        table_image      = .false.
        !
      end if xxif_do_not_process_2
      !
    end if xxif_all
    !
    return
    !
  end subroutine kfun_set_table_parms
  !
  subroutine kfun_set_fctr ( opt_va, tim_mig, &
                             n_fctr_vel, fctr_vel, &
                             n_fctr_eta, fctr_eta )
    !
    ! set fctr_vel and fctr_eta
    !
    logical,          intent(in   ) :: opt_va
    logical,          intent(in   ) :: tim_mig
    !
    integer,          intent(inout) :: n_fctr_vel
    real,             intent(inout) :: fctr_vel(:)
    !
    integer,          intent(inout) :: n_fctr_eta
    real,             intent(inout) :: fctr_eta(:)
    !
    xxif_opt_va : if ( .not. opt_va ) then
      !
      n_fctr_vel = 1
      !
      fctr_vel(1) = 1.
      !
      n_fctr_eta = 1
      !
      fctr_eta(1) = 1.
      !
    end if xxif_opt_va
    !
    n_fctr_eta = n_fctr_vel 
    !
    if ( .not. tim_mig ) fctr_eta(1:n_fctr_eta) = &
                         fctr_vel(1:n_fctr_eta)
    !
    return
    !
  end subroutine kfun_set_fctr 
  !
  subroutine kfun_set_filename_operation ( &
                                  n_filename_operation, c_filename_operation, &
                                  n_path_table, path_table, &
                                  n_tt_name, c_tt_name &
                                          )
    !
    ! set the travel time table name option
    !
    ! - Arguments
    !
    integer,                 intent(inout) :: n_filename_operation
    character(len=*),        pointer        :: c_filename_operation(:)
    integer,                 intent(inout) :: n_path_table
    character(len=*),        pointer        :: path_table(:)
    integer,                 intent(inout) :: n_tt_name
    character(len=*),        pointer        :: c_tt_name(:)
    !
    integer                                 :: i_filename_operation
    integer                                 :: i_path_table
    integer                                 :: i_tt_name
    !
    call cnfg_get_tt_names ( n_tt_name, c_tt_name  )
    !
    if ( kfun_i_pel() .eq. 0 ) print'( &
    & /," kfun_set_filename_operation n_tt_name=", i8 &
    & )', &
    n_tt_name
    !
    if ( kfun_i_pel() .eq. 0 ) print'( &
    & " i_tt_name=",i8," c_tt_name=", a &
    & )', &
    (i_tt_name,trim(c_tt_name(i_tt_name)),i_tt_name=1,n_tt_name)
    !
    n_filename_operation = n_tt_name + 2
    !
    allocate ( c_filename_operation ( n_filename_operation ) )
    !
    c_filename_operation ( 1 ) = 'CLEAR'
    !
    c_filename_operation ( 2 ) = 'USER'
    !
    c_filename_operation ( 3:n_filename_operation ) = c_tt_name(1:n_tt_name) 
    !
    do_i_filename_operation : &
    do i_filename_operation = 1 , n_filename_operation
      !
      call string_to_upper ( c_filename_operation ( i_filename_operation ) )
      !
    end do do_i_filename_operation
    !
    if ( kfun_i_pel() .eq. 0 ) print'( &
    & /, " n_filename_operation=", i8 &
    & )', &
    n_filename_operation
    !
    if ( kfun_i_pel() .eq. 0 ) print'( &
    & " i_filename_operation=", i8, " c_filename_operation=", a &
    & )', &
    (i_filename_operation,trim(c_filename_operation(i_filename_operation)),&
     i_filename_operation=1,n_filename_operation)
    !
    do_i_tt_name : do i_tt_name = 1 , n_tt_name
      !
      call cnfg_get_tt_set_info ( &
      c_tt_name(i_tt_name), n_path_table, path_table )
      !
      if ( kfun_i_pel() .eq. 0 ) print'( &
      & /, " i_tt_name=", i8, " n_path_table=", i8, " c_tt_name=", a &
      & )', &
      i_tt_name, n_path_table, trim(c_tt_name(i_tt_name))
      !
      if ( kfun_i_pel() .eq. 0 ) print'( &
      & " i_path_table=", i8, " path_table=", a &
      & )', &
      (i_path_table, trim(path_table(i_path_table)), &
       i_path_table=1,n_path_table)
      !
    end do do_i_tt_name 
    !
    return
    !
  end subroutine kfun_set_filename_operation
  !
  subroutine kfun_set_path_table ( &
                                   filename_operation, &
                                   n_path_table, path_table, &
                                   n_tt_name, c_tt_name &
                                 )
    !
    ! set the path table
    !
    ! - Arguments
    !
    character(len=*),        intent(inout) :: filename_operation
    integer,                 intent(inout) :: n_path_table
    character(len=*),        pointer        :: path_table(:)
    integer,                 intent(in   ) :: n_tt_name
    character(len=*),        pointer        :: c_tt_name(:)
    !
    ! Local variables
    !
    integer                           :: i_tt_name
    integer                           :: i_err
    !
    i_err = 0
    !
    call string_to_upper ( filename_operation )
    !
    xxif_clear : &
    if ( string_upper_compare ( filename_operation, 'CLEAR' ) ) then
      !
      n_path_table = 0
      !
    else if ( string_upper_compare ( filename_operation, 'USER' ) ) then
      !
    else xxif_clear
      !
      do_i_tt_name : do i_tt_name = 1 , n_tt_name
        !
        xxif_same : &
        if ( string_upper_compare ( filename_operation, &
                                    c_tt_name(i_tt_name) ) ) then
          !
          call cnfg_get_tt_set_info ( &
          c_tt_name(i_tt_name), n_path_table, path_table )
          !
          go to 1
          !
        end if xxif_same 
        !
      end do do_i_tt_name 
      !
   1 continue
      !
      !call cnfg_get_tt_set_info ( &
      !trim(filename_operation), n_path_table, path_table )
      !
    end if xxif_clear
    !
    xxif_n_path_table : if ( n_path_table .le. 0 ) then
      !
      filename_operation = 'USER'
      !
      n_path_table = 1     
      !
      if ( associated ( path_table ) ) deallocate ( path_table )
      !
      call memfun_all ( path_table, n_path_table, 'path_table', i_err )
      !
      xxif_i_err_1 : if ( i_err .ne. 0 ) then
        !
call pc_error ( 'kfun_set_path_tale error in memory allocation 1 err=', i_err )
        !
        return
        !
      end if xxif_i_err_1
      !
      path_table(1) = ' '
      !
    end if xxif_n_path_table 
    !
    return
    ! 
  end subroutine kfun_set_path_table
  ! 
  subroutine kfun_set_sort_order ( sort_order )
    !
    ! set the sort_order parameter to allowed values
    !
    character(len=*), intent(inout) :: sort_order 
    !
    xxif_y_is_fast : if ( string_upper_compare ( sort_order(1:1),  'Y' ) ) then
      !
      sort_order = 'Y_IS_FAST'
      !
    else xxif_y_is_fast 
      !
      sort_order = 'X_IS_FAST'
      !
    end if xxif_y_is_fast 
    !
    return
    !
  end subroutine kfun_set_sort_order 
  !
  subroutine kfun_set_mig_type ( mig_type, tim_mig, dep_mig, dmo_mig, dep_not )
    !
    ! set the migration type flag to an allowed value - 
    ! TIM_MIG, DMO, DEP_MIG
    !
    character(len=*), intent(inout) :: mig_type
    logical,          intent(inout) :: tim_mig
    logical,          intent(inout) :: dep_mig
    logical,          intent(inout) :: dmo_mig
    logical,          intent(inout) :: dep_not
    !
    call string_to_upper ( mig_type)
    !
    xxif_mig_type_1 : if ( string_upper_compare ( mig_type(1:2), 'DM' ) ) then
      !
      mig_type = 'DMO'
      !
    else if ( string_upper_compare ( mig_type(1:1), 'D' ) ) then
      !
      mig_type = 'DEP_MIG'
      !
    else xxif_mig_type_1
      !
      mig_type = 'TIM_MIG'
      !
    end if xxif_mig_type_1
    !
    xxif_mig_type_2 : &
    if ( string_upper_compare ( mig_type(1:3), 'DEP' ) ) then
      !
      tim_mig = .false.
      dep_mig = .true.
      dmo_mig = .false.
      dep_not = .false.
      !
    else xxif_mig_type_2 
      !
      tim_mig = .true.
      dep_mig = .false.
      dep_not = .true.
      !
      xxif_dmo : if ( string_upper_compare ( mig_type(1:3), 'DMO' ) ) then
        !
        dmo_mig  = .true.
        !
      else xxif_dmo
        !
        dmo_mig  = .false.
        !
      end if xxif_dmo
      !
    end if xxif_mig_type_2
    !
    return
    !
  end subroutine kfun_set_mig_type
  !
  subroutine kfun_set_opt_aniso ( opt_aniso )
    !
    ! set the anisotropic flag an allowed value - 
    ! isotropic, ANISOTPRIC, TILTED_BED
    !
    character(len=*), intent(inout) :: opt_aniso 
    !
    call string_to_upper ( opt_aniso )
    !
    xxif_opt_aniso : if ( string_upper_compare ( opt_aniso(1:1), 'A' ) ) then
      !
      opt_aniso = 'ANISOTROPIC'
      !
    else if ( string_upper_compare ( opt_aniso(1:1), 'T' ) ) then
      !
      opt_aniso = 'TILTED_BED'
      !
    else xxif_opt_aniso 
      !
      opt_aniso = 'ISOTROPIC'
      !
    end if xxif_opt_aniso 
    !
    return
    !
  end subroutine kfun_set_opt_aniso 
  !
  subroutine kfun_set_opt_map_apply ( opt_map_apply, mig_type )
    !
    ! set the opt_map_apply flag to an allowed value - 
    ! MAP_APPLY_F, MAP_APPLY_H, MAP_APPLY_I, MAP_APPLY_G, MAP_APPLY_C
    !
    character(len=*), intent(inout) :: opt_map_apply 
    character(len=*), intent(in   ) :: mig_type
    !
    call string_to_upper ( opt_map_apply )
    !
    xxif_opt_map_apply : &
    if ( string_upper_compare ( opt_map_apply(11:11), 'G' ) ) then
      !
      opt_map_apply = 'MAP_APPLY_G'
      !
    else if ( string_upper_compare ( opt_map_apply(11:11), 'H' ) ) then
      !
      opt_map_apply = 'MAP_APPLY_H'
      !
    else if ( string_upper_compare ( opt_map_apply(11:11), 'C' ) ) then
      !
      opt_map_apply = 'MAP_APPLY_C'
      !
    else if ( string_upper_compare ( opt_map_apply(11:11), 'F' ) ) then
      !
      opt_map_apply = 'MAP_APPLY_F'
      !
    else xxif_opt_map_apply 
      !
      opt_map_apply = 'MAP_APPLY_I'
      !
    end if xxif_opt_map_apply 
    !
    if ( string_upper_compare ( mig_type(1:3), 'dmo' ) ) &
    opt_map_apply = 'MAP_APPLY_F'
    !
    return
    !
  end subroutine kfun_set_opt_map_apply 
  !
  subroutine kfun_set_opt_out_int ( opt_out_int, opt_step )
    !
    ! set the output interpolation flag, opt_out_int to an allowed value - 
    ! NEAREST, LAGRANGE, LINEAR, SINC, FFT, NONE
    !
    character(len=*), intent(inout) :: opt_out_int 
    character(len=*), intent(in   ) :: opt_step
    !
    call string_to_upper ( opt_out_int )
    !
    xxif_opt_out_int : &
    if ( string_upper_compare ( opt_out_int(1:2), 'NE' ) ) then
      !
      opt_out_int = 'NEAREST'
      !
    else if ( string_upper_compare ( opt_out_int(1:1), 'N' ) ) then
      !
      opt_out_int = 'NONE'
      !
    else if ( string_upper_compare ( opt_out_int(1:2), 'LA' ) ) then
      !
      opt_out_int = 'LAGRANGE'
      !
    else if ( string_upper_compare ( opt_out_int(1:1), 'S' ) ) then
      !
      opt_out_int = 'SINC'
      !
    else if ( string_upper_compare ( opt_out_int(1:1), 'F' ) ) then
      !
      opt_out_int = 'FFT'
      !
    else xxif_opt_out_int 
      !
      opt_out_int = 'LINEAR'
      !
    end if xxif_opt_out_int 
    !
    if ( string_upper_compare ( opt_step, 'CONSTANT' ) ) opt_out_int = 'LINEAR'
    !
    return
    !
  end subroutine kfun_set_opt_out_int 
  !
  subroutine kfun_set_dimension ( dimension, n_dimension, v_dimension )
    !
    ! set the migration direction flag to an allowed value - MIGRATE, MODEL
    !
    character(len=*), intent(inout) :: dimension
    integer,          intent(inout) :: n_dimension
    integer,          intent(inout) :: v_dimension
    !
    call string_to_upper ( dimension)
    !
    xxif_dimension : if ( string_upper_compare ( dimension(1:1), '3' ) ) then
      !
      dimension = '3D'
      !
      n_dimension = 3
      !
      v_dimension = 3
      !
    else if ( string_upper_compare ( dimension(1:1), 'M' ) ) then
      !
      dimension = 'M2D'
      !
      n_dimension = 2
      !
      v_dimension = 3
      !
    else xxif_dimension 
      !
      dimension = '2D'
      !
      n_dimension = 2
      !
      v_dimension = 2
      !
    end if xxif_dimension 
    !
    return
    !
  end subroutine kfun_set_dimension
  !
  subroutine kfun_set_opt_dir ( opt_dir, opt_dir_forward, opt_dir_inverse )
    !
    ! set the migration direction flag to an allowed value - MIGRATE, MODEL
    !
    character(len=*), intent(inout) :: opt_dir 
    logical,          intent(inout) :: opt_dir_forward 
    logical,          intent(inout) :: opt_dir_inverse 
    !
    call string_to_upper ( opt_dir )
    !
    opt_dir_forward = string_upper_compare ( opt_dir(1:1), 'F' ) 
    opt_dir_inverse = string_upper_compare ( opt_dir(1:1), 'I' ) 
    !
    xxif_opt_dir : if ( string_upper_compare ( opt_dir(1:1), 'I' ) ) then
      !
      opt_dir = 'INVERSE'
      !
    else xxif_opt_dir 
      !
      opt_dir = 'FORWARD'
      !
    end if xxif_opt_dir 
    !
    return
    !
  end subroutine kfun_set_opt_dir 
  !
  subroutine kfun_set_all_out ( all_out )
    !
    ! set the output flag to an allowed value - YES, NO, POLY_YES, POLY_NO
    !
    character(len=*), intent(inout) :: all_out
    !
    call string_to_upper ( all_out)
    !
    !print'(" top kfun_set_all_out all_out=",a)', trim ( all_out )
    !
    xxif_all_out : &
    if ( string_upper_compare ( all_out(1:8), 'ACTIVE_Y' ) ) then
      !
      all_out = 'ACTIVE_YES'
      !
    else if ( string_upper_compare ( all_out(1:8), 'ACTIVE_N' ) ) then
      !
      all_out = 'ACTIVE_NO'
      !
    else if ( string_upper_compare ( all_out(1:8), 'ACTIVE_D' ) ) then
      !
      all_out = 'ACTIVE_DEAD'
      !
    else if ( string_upper_compare ( all_out(1:1), 'N' ) ) then
      !
      all_out = 'NO'
      !
    else if ( string_upper_compare ( all_out(1:1), 'D' ) ) then
      !
      all_out = 'DEAD'
      !
    else xxif_all_out 
      !
      all_out = 'YES'
      !
    end if xxif_all_out 
    !
    !print'(" end kfun_set_all_out all_out=",a)', trim ( all_out )
    !
    return
    !
  end subroutine kfun_set_all_out
  !
  subroutine kfun_set_opt_output ( opt_output)
    !
    ! set oparameter opt_output to its allowed values
    !
    character(len=*), intent(inout) :: opt_output 
    !
    call string_to_upper ( opt_output)
    !
    xxif_opt_output : if ( string_upper_compare ( opt_output(1:1), 'V' ) ) then
      !
      opt_output = 'VELOCITY'
      !
    else if ( string_upper_compare ( opt_output(1:1), 'T' ) ) then
      !
      opt_output = 'TIME'
      !
    else if ( string_upper_compare ( opt_output(1:2), 'AM' ) ) then
      !
      opt_output = 'AMPLITUDE'
      !
    else if ( string_upper_compare ( opt_output(1:1), 'A' ) ) then
      !
      opt_output = 'ANTIALIAS'
      !
    else xxif_opt_output 
      !
      opt_output = 'IMAGE'
      !
    end if xxif_opt_output 
    !
    return
    !
  end subroutine kfun_set_opt_output 
  !
  subroutine kfun_set_table_order ( table_order )
    !
    ! set oparameter table_order to its allowed values
    !
    character(len=*), intent(inout) :: table_order
    !
    call string_to_upper ( table_order )
    !
    xxif_table_order : if ( string_upper_compare ( table_order(1:1), 'R' ) )then
      !
      table_order = 'ROUND_ROBIN'
      !
    else xxif_table_order 
      !
      table_order = 'BLOCK'
      !
    end if xxif_table_order 
    !
    return
    !
  end subroutine kfun_set_table_order
  !
  subroutine kfun_set_opt_table_mem ( opt_table_mem )
    !
    ! set parameter opt_table_mem to its allowed values
    !
    character(len=*), intent(inout) :: opt_table_mem
    !
    call string_to_upper ( opt_table_mem )
    !
    xxif_opt_table_mem : &
    if ( string_upper_compare ( opt_table_mem(1:6), 'tables' ) ) then
      !
      opt_table_mem = 'TABLES_IN_MEMORY'
      !
    else xxif_opt_table_mem 
      !
      opt_table_mem = 'TABLE_MEM_TO_USE'
      !
    end if xxif_opt_table_mem
    !
    return
    !
  end subroutine kfun_set_opt_table_mem
  !
  subroutine kfun_set_opt_table_disk ( opt_table_disk )
    !
    ! set parameter opt_table_disk to its allowed values
    !
    character(len=*), intent(inout) :: opt_table_disk
    !
    call string_to_upper ( opt_table_disk )
    !
    xxif_opt_table_disk : &
    if ( string_upper_compare ( opt_table_disk(1:6), 'tables' ) ) then
      !
      opt_table_disk = 'TABLES_ON_DISK'
      !
    else xxif_opt_table_disk 
      !
      opt_table_disk = 'TABLE_DISK_TO_USE'
      !
    end if xxif_opt_table_disk
    !
    return
    !
  end subroutine kfun_set_opt_table_disk
  !
  subroutine kfun_set_local_pack ( local_pack )
    !
    ! set parameter local_pack to its allowed values
    !
    character(len=*), intent(inout) :: local_pack
    !
    call string_to_upper ( local_pack )
    !
    xxif_local_pack : &
    if ( string_upper_compare ( local_pack(1:1), 'm' ) ) then
      !
      local_pack = 'MEMORY'
      !
    else xxif_local_pack 
      !
      local_pack = 'DISK'
      !
    end if xxif_local_pack
    !
    return
    !
  end subroutine kfun_set_local_pack
  !
  subroutine kfun_set_opt_table_operation ( opt_table_operation )
    !
    ! set oparameter table_operation to its allowed values
    !
    character(len=*), intent(inout) :: opt_table_operation
    !
    call string_to_upper ( opt_table_operation )
    !
    xxif_table_operation : &
    if ( string_upper_compare ( opt_table_operation(1:1), 'U' ) ) then
      !
      opt_table_operation = 'USER'
      !
    else if ( string_upper_compare ( opt_table_operation(1:2), 'IN' )) then
      !
      opt_table_operation = 'INITIALIZE'
      !
    else if ( string_upper_compare ( opt_table_operation(1:2), 'CH' )) then
      !
      opt_table_operation = 'CHECK'
      !
    else if ( string_upper_compare ( opt_table_operation(1:1), 'C' )) then
      !
      opt_table_operation = 'COMPUTE'
      !
    else if ( string_upper_compare ( opt_table_operation(1:1), 'A' )) then
      !
      opt_table_operation = 'ALL'
      !
    else xxif_table_operation 
      !
      opt_table_operation = 'IMAGE'
      !
    end if xxif_table_operation
    !
    return
    !
  end subroutine kfun_set_opt_table_operation
  !
  subroutine kfun_set_apr ( &
                            rx_apr_1, rx_apr_2, &
                            t_apr_1, t_apr_2, &
                            nx_apr, xd_apr, xt_apr &
                          )
    !
    ! set the default values for the aperture limits
    !
    real,             intent(in   ) :: rx_apr_1
    real,             intent(in   ) :: rx_apr_2
    real,             intent(in   ) :: t_apr_1
    real,             intent(in   ) :: t_apr_2
    integer,          intent(inout) :: nx_apr
    real,             intent(inout) :: xd_apr ( : )
    real,             intent(inout) :: xt_apr ( : )
    !
    xxif_nx_apr : if ( nx_apr .le. 0 ) then
      !
      nx_apr = 2
      xt_apr(1)   = t_apr_1
      xt_apr(2)   = t_apr_2
      xd_apr(1)   = rx_apr_1
      xd_apr(2)   = rx_apr_2
      !
    end if xxif_nx_apr
    !
    return
    !
  end subroutine kfun_set_apr
  !
  subroutine kfun_set_phase ( &
                              opt_dir, mig_type, n_dimension, &
                              phase_filt, pwr_filt &
                            )
    !
    ! set default value for phase_filt 
    !
    character(len=*), intent(in   ) :: opt_dir
    character(len=*), intent(in   ) :: mig_type
    integer,          intent(inout) :: n_dimension
    real,             intent(inout) :: phase_filt 
    real,             intent(inout) :: pwr_filt 
    !
    n_dimension = max ( 2, min ( 3, n_dimension ) )
    !
    ! default phase shift for inverse dmo is -45
    !
    if ( string_upper_compare ( mig_type(1:3), 'DMO' ) &
   .and. string_upper_compare ( opt_dir, 'INVERSE' ) &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = -45
    !
    ! default phase shift for forward dmo is +45
    !
    if ( string_upper_compare ( mig_type(1:3), 'DMO' ) &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = 45
    !
    ! default phase shift for inverse 2d time migration is -45
    !
    if ( string_upper_compare ( mig_type(1:3), 'TIM' ) &
   .and. string_upper_compare ( opt_dir, 'INVERSE' ) &
    .and. n_dimension .eq. 2 &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = -45
    !
    ! default phase shift for inverse 2d depth migration is -45
    !
    if ( string_upper_compare ( mig_type(1:3), 'DEP' ) &
   .and. string_upper_compare ( opt_dir, 'INVERSE' ) &
    .and. n_dimension .eq. 2 &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = -45
    !
    ! default phase shift for forward 2d time migration is +45
    !
    if ( string_upper_compare ( mig_type(1:3), 'TIM' ) &
    .and. n_dimension .eq. 2 &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = 45
    !
    ! default phase shift for inverse 3d time migration is -90
    !
    if ( string_upper_compare ( mig_type(1:3), 'TIM' ) &
   .and. string_upper_compare ( opt_dir, 'INVERSE' ) &
    .and. n_dimension .eq. 3 &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = -90
    !
    ! default phase shift for inverse 3d depth migration is -90
    !
    if ( string_upper_compare ( mig_type(1:3), 'DEP' ) &
   .and. string_upper_compare ( opt_dir, 'INVERSE' ) &
    .and. n_dimension .eq. 3 &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = -90
    !
    ! default phase shift for forward 3d time migration is +90
    !
    if ( string_upper_compare ( mig_type(1:3), 'TIM' ) &
    .and. n_dimension .eq. 3 &
   .and. abs(phase_filt+999.) .lt. 1. ) &
    phase_filt = 90
    !
    if ( abs(pwr_filt+999.) .lt. 1. &
    .and. string_upper_compare ( mig_type, 'DMO' ) ) &
    pwr_filt = 0.5
    !
    ! default phase shift 2d is 45, 3d is 90.
    !
    xxif_phase_filt : &
    if ( abs(phase_filt+999.) .lt. 1. .and. n_dimension .eq. 2 ) then
      !
      phase_filt = 45
      !
    else if ( abs(phase_filt+999.) .lt. 1. ) then
      !
      phase_filt = 90
      !
    end if xxif_phase_filt 
    !
    ! default pwr_filt 2d is .5, 3d is 1.
    !
    xxif_pwr_filt : &
    if ( abs(pwr_filt+999.) .lt. 1. .and. n_dimension .eq. 2 ) then
      !
      pwr_filt = .5
      !
    else if ( abs(pwr_filt+999.) .lt. 1. ) then
      !
      pwr_filt = 1.
      !
    end if xxif_pwr_filt 
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " kfun_set_phase", &
    & /, " n_dimension=", i2, " phase=", g10.4, " pwr_filt=", g10.4 &
    & )', &
    n_dimension, phase_filt, pwr_filt 
    !
    return
    !
  end subroutine kfun_set_phase
  !
  subroutine kfun_set_dist_grid_angle ( &
                                        opt_ap_units, t1_inp, xg_scale, &
                                      dist_ap_x_max, grid_ap_x_max, ap_x_max, &
                                      dist_ap_x_inc, grid_ap_x_inc, ap_x_inc, &
                                        n_ap_x, tim_ap_x, dep_ap_x,  &
                                        dist_ap_x, grid_ap_x, ang_ap_x &
                                      )
    !
    ! set ang_ap_x from dist_ap_xance and dep_ap_x
    ! angle in radians = atan (dx / dz)
    !
    real,             intent(in   ) :: t1_inp            ! max input time
    real,             intent(in   ) :: xg_scale          ! x grid to dist scale
    !
    character(len=*), intent(in   ) :: opt_ap_units      ! ap units g, d
    !
    real,             intent(inout) :: dist_ap_x_max     ! max x ap dist
    real,             intent(inout) :: grid_ap_x_max     ! max x ap grid
    real,             intent(inout) :: ap_x_max          ! max x ap used
    !
    real,             intent(inout) :: dist_ap_x_inc     ! inc x ap dist
    real,             intent(inout) :: grid_ap_x_inc     ! inc x ap grid
    real,             intent(inout) :: ap_x_inc          ! inc x ap used
    !
    integer,          intent(inout) :: n_ap_x            ! num of points
    real,             pointer       :: tim_ap_x ( : )    ! x tim
    real,             pointer       :: dep_ap_x ( : )  ! x depth
    real,             pointer       :: dist_ap_x  ( : )  ! x distance
    real,             pointer       :: grid_ap_x  ( : )  ! x grid
    real,             pointer       :: ang_ap_x ( : )    ! x angle
    !
    integer                         :: i_ap_x           ! x index
    integer                         :: i_err  
    !
    ! initialize the distance, grid, angle arrays
    !
    i_err = 0
    !
    xxif_n_ap_x : if ( n_ap_x .le. 0 ) then
      !
      n_ap_x       = 2     ! dip versus time arrays
      !
      call memfun_all ( dist_ap_x, n_ap_x, 'dist_ap_x', i_err )
      call memfun_all ( grid_ap_x, n_ap_x, 'grid_ap_x', i_err )
      call memfun_all ( ang_ap_x,  n_ap_x, 'ang_ap_x',  i_err )
      call memfun_all ( tim_ap_x,  n_ap_x, 'tim_ap_x',  i_err )
      call memfun_all ( dep_ap_x,  n_ap_x, 'dep_ap_x',  i_err )
      !
      dist_ap_x(1) = 0.
      dist_ap_x(2) = 100.
      !
      tim_ap_x(1) = 0.
      tim_ap_x(2) = t1_inp
      !
      dep_ap_x(1) = 0.
      dep_ap_x(2) = t1_inp * 1000.
      !
      grid_ap_x  ( 1:n_ap_x ) = dist_ap_x ( 1:n_ap_x ) * xg_scale
      !
    end if xxif_n_ap_x
    !
    ! aperture parameters 
    !
    dist_ap_x ( 1:n_ap_x  ) = abs ( dist_ap_x ( 1:n_ap_x ) ) 
    grid_ap_x ( 1:n_ap_x  ) = abs ( grid_ap_x ( 1:n_ap_x ) ) 
    ang_ap_x  ( 1:n_ap_x  ) = abs ( ang_ap_x  ( 1:n_ap_x ) ) 
    !
    !print'(" top kfun_set_dist_grid_angle scl=",g12.6, &
    !& " ap_x_max=",g12.6,1x,g12.6,g12.6," opt=",a)', &
    !xg_scale, ap_x_max, grid_ap_x_max, dist_ap_x_max, trim(opt_ap_units)
    !
    ! set the sensitivity characterisitcs
    !
    xxif_opt_ap_units : &
    if ( string_upper_compare ( opt_ap_units, 'dist' ) ) then
      !
      call pc_put_sensitive_field_flag ( 'dist_ap_x_inc', .true.  )
      call pc_put_sensitive_field_flag ( 'dist_ap_y_inc', .true.  )
      call pc_put_sensitive_field_flag ( 'grid_ap_x_inc', .false. )       
      call pc_put_sensitive_field_flag ( 'grid_ap_y_inc', .false. )
      call pc_put_sensitive_field_flag ( 'dist_ap_x_max', .true.  )      
      call pc_put_sensitive_field_flag ( 'dist_ap_y_max', .true.  )
      call pc_put_sensitive_field_flag ( 'grid_ap_x_max', .false. )       
      call pc_put_sensitive_field_flag ( 'grid_ap_y_max', .false. )     
      call pc_put_sensitive_array_flag ( 'dist_ap_x',     .true.  )
      call pc_put_sensitive_array_flag ( 'dist_ap_y',     .true.  )
      call pc_put_sensitive_array_flag ( 'grid_ap_x',     .false. )
      call pc_put_sensitive_array_flag ( 'grid_ap_y',     .false. )
      call pc_put_sensitive_array_flag ( 'ang_ap_x',      .false. )       
      call pc_put_sensitive_array_flag ( 'ang_ap_y',      .false. )     
      !
      ! compute the grid units from the distance units
      !
      grid_ap_x_max          = dist_ap_x_max          / xg_scale
      grid_ap_x_inc          = dist_ap_x_inc          / xg_scale
      grid_ap_x ( 1:n_ap_x ) = dist_ap_x ( 1:n_ap_x ) / xg_scale
      !
      ap_x_max = dist_ap_x_max
      ap_x_inc = dist_ap_x_inc
      !
    else if ( string_upper_compare ( opt_ap_units, 'grid' ) ) then
      !
      call pc_put_sensitive_field_flag ( 'dist_ap_x_inc' , .false. )
      call pc_put_sensitive_field_flag ( 'dist_ap_y_inc' , .false. )
      call pc_put_sensitive_field_flag ( 'grid_ap_x_inc' , .true.  )       
      call pc_put_sensitive_field_flag ( 'grid_ap_y_inc' , .true.  )
      call pc_put_sensitive_field_flag ( 'dist_ap_x_max',  .false. )      
      call pc_put_sensitive_field_flag ( 'dist_ap_y_max',  .false. )
      call pc_put_sensitive_field_flag ( 'grid_ap_x_max',  .true.  )       
      call pc_put_sensitive_field_flag ( 'grid_ap_y_max',  .true.  )
      call pc_put_sensitive_array_flag ( 'dist_ap_x',      .false. )
      call pc_put_sensitive_array_flag ( 'dist_ap_y',      .false. )
      call pc_put_sensitive_array_flag ( 'grid_ap_x',      .true.  )
      call pc_put_sensitive_array_flag ( 'grid_ap_y',      .true.  )
      call pc_put_sensitive_array_flag ( 'ang_ap_x',       .false. )       
      call pc_put_sensitive_array_flag ( 'ang_ap_y',       .false. )        
      !
      ! compute the distance units from the grid units
      !
      dist_ap_x_max          = grid_ap_x_max          * xg_scale
      dist_ap_x_inc          = grid_ap_x_inc          * xg_scale
      dist_ap_x ( 1:n_ap_x ) = grid_ap_x ( 1:n_ap_x ) * xg_scale
      !
      ap_x_max = grid_ap_x_max
      ap_x_inc = grid_ap_x_inc
      !
    else if ( string_upper_compare ( opt_ap_units, 'ang' ) ) then
      !
      call pc_put_sensitive_field_flag ( 'dist_ap_x_inc' , .false. )
      call pc_put_sensitive_field_flag ( 'dist_ap_y_inc' , .false. )
      call pc_put_sensitive_field_flag ( 'grid_ap_x_inc' , .false. )       
      call pc_put_sensitive_field_flag ( 'grid_ap_y_inc' , .false. )
      call pc_put_sensitive_field_flag ( 'dist_ap_x_max' , .false. )
      call pc_put_sensitive_field_flag ( 'dist_ap_y_max' , .false. )
      call pc_put_sensitive_field_flag ( 'grid_ap_x_max' , .false. )       
      call pc_put_sensitive_field_flag ( 'grid_ap_y_max' , .false. )
      call pc_put_sensitive_array_flag ( 'dist_ap_x'  ,    .false. )
      call pc_put_sensitive_array_flag ( 'dist_ap_y'  ,    .false. )
      call pc_put_sensitive_array_flag ( 'grid_ap_x' ,     .false. )
      call pc_put_sensitive_array_flag ( 'grid_ap_y' ,     .false. )
      call pc_put_sensitive_array_flag ( 'ang_ap_x' ,      .true.  )       
      call pc_put_sensitive_array_flag ( 'ang_ap_y' ,      .true.  )
      !
    end if xxif_opt_ap_units 
    !
    !print'(" aa1 kfun_set_dist_grid_angle scl=",g12.6, &
    !& " ap_x_max=",g12.6,1x,g12.6,g12.6," opt=",a)', &
    !xg_scale, ap_x_max, grid_ap_x_max, dist_ap_x_max, trim(opt_ap_units)
    !
    ! compute the angle from the distance values
    !
    xxif_ang : if ( string_upper_compare ( opt_ap_units, 'ang' ) ) then
      !
      ! compute the distance from the angle values
      !
      dist_ap_x(1) = 0.
      !
      do_i_ap_x_a_to_d : do i_ap_x = 1 , n_ap_x-1
        !
                dist_ap_x  ( i_ap_x + 1 ) = dist_ap_x  ( i_ap_x ) + &
          tan ( ang_ap_x   ( i_ap_x     ) / ( 90. / asin(1.) ) ) * &
              ( dep_ap_x   ( i_ap_x + 1 ) - dep_ap_x   ( i_ap_x ) )
        !
      end do do_i_ap_x_a_to_d
      !
      ! compute the grid units from the distance units
      !
      grid_ap_x ( 1:n_ap_x ) = dist_ap_x ( 1:n_ap_x ) / xg_scale
      !
    else xxif_ang 
      !
      do_i_ap_x_d_to_a : do i_ap_x = 1 , n_ap_x
        !
          ang_ap_x (i_ap_x  ) = (90. / asin(1.)) * abs ( atan ( &
        ( dist_ap_x(i_ap_x+1) - dist_ap_x(i_ap_x) ) &
        / &
        ( dep_ap_x (i_ap_x+1) - dep_ap_x (i_ap_x) ) &
        ) ) 
        !
      end do do_i_ap_x_d_to_a 
      !
      ang_ap_x ( n_ap_x ) = ang_ap_x ( n_ap_x - 1 )
      !
    end if xxif_ang 
    !
    !print'(" aa2 kfun_set_dist_grid_angle scl=",g12.6, &
    !& " ap_x_max=",g12.6,1x,g12.6,g12.6," opt=",a)', &
    !xg_scale, ap_x_max, grid_ap_x_max, dist_ap_x_max, trim(opt_ap_units)
    !
    grid_ap_x_max          = dist_ap_x_max          / xg_scale
    grid_ap_x_inc          = dist_ap_x_inc          / xg_scale
    !
    dist_ap_x ( 1:n_ap_x  ) = abs ( dist_ap_x ( 1:n_ap_x ) ) 
    grid_ap_x ( 1:n_ap_x  ) = abs ( grid_ap_x ( 1:n_ap_x ) ) 
    ang_ap_x  ( 1:n_ap_x  ) = abs ( ang_ap_x  ( 1:n_ap_x ) ) 
    !
    !print'(" end kfun_set_dist_grid_angle scl=",g12.6, &
    !& " ap_x_max=",g12.6,1x,g12.6,g12.6," opt=",a)', &
    !xg_scale, ap_x_max, grid_ap_x_max, dist_ap_x_max, trim(opt_ap_units)
    !
    return
    !
  end subroutine kfun_set_dist_grid_angle
  !
  subroutine kfun_condition_depth_grid ( &
     dep_tot,    dep_init,    dep_last,    dep_inc, &
  ap_dep_tot, ap_dep_init, ap_dep_last, ap_dep_inc &
                                         ) 
    !
    ! condition coarse grids so they fall on fine grids
    !
    integer,          intent(in   ) :: dep_tot        ! image grid num
    real,             intent(in   ) :: dep_init        ! image grid min
    real,             intent(in   ) :: dep_last        ! image grid max
    real,             intent(in   ) :: dep_inc        ! image grid inc
    !
    integer,          intent(inout) :: ap_dep_tot        ! table grid num
    real,             intent(inout) :: ap_dep_init        ! table grid init
    real,             intent(inout) :: ap_dep_last        ! table grid last
    real,             intent(inout) :: ap_dep_inc        ! table grid inc
    !
    ! Local variables
    !
    integer                          :: nx ! table grid num
    real                             :: x0 ! table grid min
    real                             :: x1 ! table grid max
    real                             :: dx ! table grid inc
    !
    xxif_ap_dep_tot : if ( ap_dep_tot .le. 0 ) then
      !
      dx = ap_dep_inc
      if ( dx .le. 0.) dx = dep_inc * 5 ! z spacing in time tables 
      x0 = dep_init
      x1 = dep_last
      nx = int((x1-x0)/dx)+1
      if ( (nx-1)*dx+x0 .lt. x1 ) nx = nx + 1
      x1 = (nx - 1 ) * dx + x0
      ap_dep_tot = - nx
      ap_dep_init = x0
      ap_dep_last = x1
      if ( ap_dep_inc .le. 0) ap_dep_inc = -dx
      !
    end if xxif_ap_dep_tot 
    !
    return
    !
  end subroutine kfun_condition_depth_grid
  !
  subroutine kfun_condition_table_grid ( &
                 dist_ap_x_max, &
                       x_tot,       x_init,       x_last,       x_inc, &
                 table_x_tot, table_x_init, table_x_last, table_x_inc &
                                         ) 
    !
    ! condition coarse grids so they fall on fine grids
    !
    real,             intent(in   ) :: dist_ap_x_max! image grid apertrue 
    !
    integer,          intent(in   ) :: x_tot        ! image grid num
    real,             intent(in   ) :: x_init       ! image grid init
    real,             intent(in   ) :: x_last       ! image grid last
    real,             intent(in   ) :: x_inc        ! image grid inc
    !
    integer,          intent(inout) :: table_x_tot  ! table grid num
    real,             intent(inout) :: table_x_init ! table grid min
    real,             intent(inout) :: table_x_last ! table grid max
    real,             intent(inout) :: table_x_inc  ! table grid inc
    !
    ! Local variables
    !
    integer                          :: nx ! table grid num
    real                             :: x0 ! table grid min
    real                             :: x1 ! table grid max
    real                             :: dx ! table grid inc
    !
    !
    !
    xxif_table_xin : if ( table_x_tot .le. 0 ) then
      !
      dx = table_x_inc
      if ( dx .le. 0. ) dx = abs ( 5. * x_inc )
      x0 = table_x_init
      x1 = table_x_last
      !
      if ( abs ( x0+999. ) .gt. .01 ) &
      x0 = min ( x_init, x_last ) - dist_ap_x_max
      !
      if ( abs ( x1 + 999. ) .gt. .01 ) &
      x1 = max ( x_init, x_last ) + dist_ap_x_max
      !
      nx = int ( ( x1 - x0 ) / dx ) + 1
      if ( (nx-1)*dx+x0 .lt. x1 ) nx = nx + 1
      !
      x1 = ( nx - 1 ) * dx + x0
      !
      table_x_tot = -nx
      table_x_init = x0
      table_x_last = x1
      !
      if ( table_x_inc .le. 0 ) table_x_inc = -dx
      !
    end if xxif_table_xin ! if ( table_x_tot .le. 0 ) then
    !
    return
    !
  end subroutine kfun_condition_table_grid
  !
  subroutine kfun_condition_polygon_grid ( &
                        c_title, ng_crs, &
                        fn_x_tot, fn_x_init, fn_x_last, fn_x_inc, &
                        sp_x_tot, sp_x_init, sp_x_last, sp_x_inc &
                                         ) 
    !
    ! condition coarse grids so they fall on fine grids
    !
    character(len=*), intent(in   ) :: c_title
    !
    integer,          intent(in   ) :: ng_crs           ! num coarse grids
    !
    integer,          intent(in   ) :: fn_x_tot        ! fine   grid num
    real,             intent(in   ) :: fn_x_init       ! fine   grid init
    real,             intent(in   ) :: fn_x_last       ! fine   grid last
    real,             intent(in   ) :: fn_x_inc        ! fine   grid inc
    !
    integer,          intent(inout) :: sp_x_tot ( : )  ! coarse grid num
    real,             intent(inout) :: sp_x_init ( : ) ! coarse grid init
    real,             intent(inout) :: sp_x_last ( : ) ! coarse grid last
    real,             intent(inout) :: sp_x_inc ( : )  ! coarse grid inc
    !
    ! Local variables
    !
    integer                          :: ig_crs ! coarse grid index
    integer                          :: i_stat ! status flag
    !
    integer                          :: ny ! fine   grid num
    real                             :: y0 ! fine   grid min
    real                             :: y1 ! fine   grid max
    real                             :: dy ! fine   grid inc
    !
    integer                          :: nx ! coarse grid num
    real                             :: x0 ! coarse grid min
    real                             :: x1 ! coarse grid max
    real                             :: dx ! coarse grid inc
    !
    character(len=12) :: c_tot
    character(len=12) :: c_init
    character(len=12) :: c_last
    character(len=12) :: c_inc
    !
    !
    !
    c_tot  = trim(c_title) // '_tot_1'
    c_init = trim(c_title) // '_init_1'
    c_last = trim(c_title) // '_last_1'
    c_inc  = trim(c_title) // '_init_1'
    !
    do_ig_crs : do ig_crs = 1 , ng_crs
      !
      if ( sp_x_tot(ig_crs) .gt. 0 ) then
        !
        ny = fn_x_tot
        dy = abs ( fn_x_inc )
        y0 = min ( fn_x_init, fn_x_last ) 
        y1 = max ( fn_x_init, fn_x_last ) 
        !
        nx = sp_x_tot(ig_crs)
        dx = abs ( sp_x_inc(ig_crs) )
        x0 = min ( sp_x_init(ig_crs), sp_x_last(ig_crs) ) 
        x1 = max ( sp_x_init(ig_crs), sp_x_last(ig_crs) ) 
        !
        x0 = max ( x0, min ( fn_x_init, fn_x_last ) )
        x1 = min ( x1, max ( fn_x_init, fn_x_last ) )
        !
        dx = dy * max ( 1, nint ( dx / dy ) )
        x0 = y0 + nint ( (x0 - y0) / dy ) * dy
        x1 = y0 + nint ( (x1 - y0) / dy ) * dy
        nx = max ( 1, nint ( ( x1 - x0 ) / dx ) + 1 )
        x1 = (nx - 1) * dx + x0
        !
        sp_x_tot(ig_crs) = nx
        sp_x_inc(ig_crs) = sign(1., sp_x_inc(ig_crs) ) * dx
        !
        if ( sp_x_init(ig_crs) .le. sp_x_last(ig_crs) ) then
          !
          sp_x_init(ig_crs) = x0
          sp_x_last(ig_crs) = x1
          !
        else    ! if ( sp_x_init(ig_crs) .le. sp_x_last(ig_crs) ) then
          !
          sp_x_init(ig_crs) = x1
          sp_x_last(ig_crs) = x0
          !
        end if    ! if ( sp_x_init(ig_crs) .le. sp_x_last(ig_crs) ) then
        !
        i_stat = pattern_stop2('kfun:', .true., &
        sp_x_init(ig_crs), sp_x_inc(ig_crs), &
        sp_x_last(ig_crs), sp_x_tot(ig_crs), &
        c_init, c_inc, c_last, c_tot, &
        pc_verify_scalar(c_init), pc_verify_scalar(c_inc), &
        pc_verify_scalar(c_last), pc_verify_scalar(c_tot) )
        !
      end if    ! if ( sp_x_tot(ig_crs) .gt. 0 ) then
      !
    end do do_ig_crs ! do ig_crs = 1 , ng_crs
    !
    return
    !
  end subroutine kfun_condition_polygon_grid
  !
  subroutine kfun_condition_fctr_vel ( n_fctr_vel, fctr_vel, &
                                       n_fctr_eta, fctr_eta )
    !
    ! condition the velocity scale array
    !
    integer,          intent(inout) :: n_fctr_vel     ! num fctr_vel 
    real,             intent(inout) :: fctr_vel ( : ) ! val fctr_vel 
    integer,          intent(inout) :: n_fctr_eta     ! num fctr_eta 
    real,             intent(inout) :: fctr_eta ( : ) ! val fctr_eta 
    !
    ! Local variabel
    !
    integer                          :: j_fctr_vel     ! value index
    integer                          :: i_fctr_vel     ! value index
    integer                          :: k_fctr_vel     ! value index
    !
    k_fctr_vel = 0
    !
    ! check each velocity scale to see if it is <=0. or 
    ! if it duplicates anearlier velocitys cale
    ! if either of these is true skip that velocity scale
    ! other wise save it as a valid value
    !
    do_j_fctr_vel : do j_fctr_vel = 1 , n_fctr_vel 
      !
      ! check for velocity scales <= vel_limit
      !
      if ( fctr_vel (j_fctr_vel) .lt. 0. ) go to 1
      !
      ! check for duplicated velocity scales
      !
      do_i_fctr_vel : do i_fctr_vel = 1 , k_fctr_vel 
        !
        if ( fctr_vel (j_fctr_vel) .eq. fctr_vel (i_fctr_vel) &
       .and. fctr_eta (j_fctr_vel) .eq. fctr_eta (i_fctr_vel) ) go to 1
        !
      end do do_i_fctr_vel ! do i_fctr_vel = 1 , k_fctr_vel 
      !
      ! save this velocity scale
      !
      k_fctr_vel = k_fctr_vel + 1
      !
      fctr_vel (k_fctr_vel) = &
      fctr_vel (j_fctr_vel) 
      !
      fctr_eta (k_fctr_vel) = &
      fctr_eta (j_fctr_vel) 
      !
      ! come to here to avoid saving a velocity scale
      !
    1 continue
      !
    end do do_j_fctr_vel ! do j_fctr_vel = 1 , n_fctr_vel 
    !
    ! reset the number of velocity scales
    !
    n_fctr_vel = k_fctr_vel 
    !
    ! if there are no velocity scales rest to 1
    !
    xxif_n_fctr_vel : if ( n_fctr_vel .le. 0 ) then
      !
      n_fctr_vel = 1
      fctr_vel (n_fctr_vel) = 1.
      fctr_eta (n_fctr_vel) = 1.
      !
    end if xxif_n_fctr_vel ! if ( n_fctr_vel .le. 0 ) then
    !
    n_fctr_eta = n_fctr_vel 
    !
    return
    !
  end subroutine kfun_condition_fctr_vel 
  !
  subroutine kfun_memory_size ( c_title, i_index )
    !
    character(len=*), intent(in   ) :: c_title
    integer,          intent(in   ) :: i_index
    !
    integer                         :: vsize
    integer                         :: rss     
    !
    call getsys_my_pid_memory ( vsize, rss )
    !
    print'(" kfun_memory_size p=",i4," i=",i8,&
    & " vsize=",i8," rss=",i8,1x,a)', &
    kfun_i_pel(), i_index, vsize, rss, trim ( c_title ) 
    !
    return
    !
  end subroutine kfun_memory_size 
  !
  subroutine kfun_trace_file_open ( &
                                    trcio_obj, path_out, &
                                    nz_out, z0_out, dz_out, rz_scl, &
                                    i_err &
                                  )
    !
    ! write a trace file for an array
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    character(len=*),    intent(in   ) :: path_out ! file name
    !
    integer,             intent(in   ) :: nz_out    ! z num trc
    real,                intent(in   ) :: z0_out    ! z min trc
    real,                intent(in   ) :: dz_out    ! z inc trc
    real,                intent(in   ) :: rz_scl    ! rz_scale
    !
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer,                 parameter :: mh_out = 64
    !
    integer                            :: j_err
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! nullify the trcio structure
    !
    nullify ( trcio_obj )
    !
    if ( string_upper_compare ( path_out(1:4), 'NONE' ) ) return
    !
    ! open and write the data file only on pe 0
    !
    j_err = cio_remove ( path_out )
    !
    !if ( kfun_i_pel() .eq. 0 ) &
    trcio_obj => trcio_open ( &
                              filename = path_out,        &
                              io_mode  = 'w',             &
                              scratch  = .false.,         &
                              nwih     = mh_out,          &
                              ndpt     = nz_out,          &
                              strt_val = z0_out / rz_scl, &
                              srate    = dz_out / rz_scl  &
                            )
    !
    if ( .not. associated ( trcio_obj ) ) i_err = -1
    !
    if ( i_err .lt. 0 ) go to 998
    !
    return
    !
998 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_open p=",i4, &
    & /, " filename =", a, &
    & /, " io_mode  =", a, &
    & /, " scratch  =", i12, &
    & /, " nwih     =", i12, & 
    & /, " ndpt     =", i12, &
    & /, " strt_val =", g12.6, &
    & /, " srate    =", g12.6 &
    & )', &
    trim( path_out), &
    trim( 'w'),      &
    .false.,         &
    mh_out,          &
    nz_out,          &
    z0_out / rz_scl, &
    dz_out / rz_scl 
    !
    go to 999
    !
999 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_open p=",i4 &
    & )', &
    kfun_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine kfun_trace_file_open 
  !
  subroutine kfun_trace_file_close ( trcio_obj, i_err )
    !
    ! write a trace file for an array
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    i_err = trcio_close ( file=trcio_obj, remove=.false. )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
998 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_close p=",i4, &
    & /, " during trcio_close " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_close p=",i4 &
    & )', &
    kfun_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine kfun_trace_file_close 
  !
  subroutine kfun_trace_file_write ( &
                                     trcio_obj, grid_obj, &
                                     invert_coefficient, &
                                     i0_out, &
                                     hx_out, jx_out, rx_out, rx_scl, &
                                     hy_out, jy_out, ry_out, ry_scl, &
                                     nz_out, z0_out, dz_out, rz_scl, &
                                     tr_out, &
                                     i_err &
                                   )
    !
    ! write a trace 
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    type ( grid_struct ),      pointer :: grid_obj  ! global
    logical,             intent(in   ) :: invert_coefficient ! invert flag
    !
    integer,             intent(in   ) :: i0_out    ! out index
    !
    integer,             intent(in   ) :: hx_out    ! x hdr
    integer,             intent(in   ) :: jx_out    ! x idx trc
    real,                intent(in   ) :: rx_out    ! x val trc
    real,                intent(in   ) :: rx_scl    ! x val scl
    !
    integer,             intent(in   ) :: hy_out    ! y hdr
    integer,             intent(in   ) :: jy_out    ! y idx trc
    real,                intent(in   ) :: ry_out    ! y val trc
    real,                intent(in   ) :: ry_scl    ! y val scl
    !
    integer,             intent(in   ) :: nz_out    ! z num trc
    real,                intent(in   ) :: z0_out    ! z min trc
    real,                intent(in   ) :: dz_out    ! z inc trc
    real,                intent(in   ) :: rz_scl    ! rz_scale
    !
    real,                intent(in   ) :: tr_out(:) ! rz_scale
    !
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer,                 parameter :: mh_out = 64
    !
    double precision                   :: hd_tmp ( mh_out )
    real                               :: tr_tmp ( nz_out )
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !print'(" kfun_trace_file_write c=",i8,&
    !& " jy=",i8," jx=",i8," i0=",i8," tr=",g12.6)', &
    !i_call, jy_out, jx_out, i0_out, maxval(abs(tr_out))
    !
    !if ( i_call .eq. 1 ) &
    !call pc_get_global ( 'GRID' , grid_obj )
    !
    tr_tmp ( 1:nz_out ) = tr_out ( 1:nz_out )
    !
    ! if need be invert this trace from slowness to velocity
    !
    if ( invert_coefficient ) &
     tr_tmp ( 1:nz_out ) = &
1. / tr_tmp ( 1:nz_out ) 
    !
    !print'(" qq0 i=",i8," x=",g12.6," y=",g12.6,&
    !& " t=",g12.6,1x,g12.6,1x,g12.6)', &
    !i0_out, rx_out, ry_out, &
    !tr_tmp(nz_out/2+1), minval ( tr_tmp ) , maxval ( tr_tmp )
    !
    ! set the output trace header words
    !
    hd_tmp = 0.
    !
    call migfun_output_headers (  &
                                  grid_obj=grid_obj, &
                                  trace_number=i0_out, &
                                  group_number=jy_out, &
                                  trace_group=jx_out, &
                                  hx_mig=hx_out, &
                                  xm_mig=rx_out, &
                                  x0_scl=rx_scl, &
                                  hy_mig=hy_out, &
                                  ym_mig=ry_out, &
                                  y0_scl=ry_scl, &
                                  om_mig=0., azimuth=0., &
                                  top_mute=1, bot_mute=nz_out, &
                                  nt_fos=0, at_fos=0., tr_fos=tr_tmp, &
                                  i_velocity_scale=1, velocity_scale=0., &
                                  mh_mig=mh_out, hd_mig=hd_tmp, &
                                  nt_mig=nz_out, tr_mig=tr_tmp,  &
                                  i_err=i_err &
                                )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! write this trace to disk location i0_out
    !
    i_err = trcio_write_trace ( &
                                file = trcio_obj, &
                                hd   = hd_tmp,    &
                                tr   = tr_tmp,    &
                                tnum = i0_out     &
                              )
    !
    if ( i_err .lt. 0 ) go to 997
    !
    return
    !
997 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_write p=",i4, &
    & /, " during trcio_write " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
998 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_write p=",i4, &
    & /, " during migfun_output_headers " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_write p=",i4 &
    & )', &
    kfun_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine kfun_trace_file_write 
  !
  subroutine kfun_trace_file_write_slice ( &
                                           trcio_obj, grid_obj, &
                                           invert_coefficient, &
                                      hx_out, nx_out, x0_out, dx_out, rx_scl, &
                                           hy_out, jy_out, ry_out, ry_scl, &
                                           nz_out, z0_out, dz_out, rz_scl, &
                                           tr_out, &
                                           i_err &
                                         )
    !
    ! write a trace 
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    type ( grid_struct ),      pointer :: grid_obj  ! global
    logical,             intent(in   ) :: invert_coefficient ! invert flag
    !
    integer,             intent(in   ) :: hx_out    ! x hdr
    integer,             intent(in   ) :: nx_out    ! x num trc
    real,                intent(in   ) :: x0_out    ! x min trc
    real,                intent(in   ) :: dx_out    ! x inc trc
    real,                intent(in   ) :: rx_scl    ! rx_scale
    !
    integer,             intent(in   ) :: hy_out    ! y hdr
    integer,             intent(in   ) :: jy_out    ! y idx trc
    real,                intent(in   ) :: ry_out    ! y val trc
    real,                intent(in   ) :: ry_scl    ! y val scl
    !
    integer,             intent(in   ) :: nz_out    ! z num trc
    real,                intent(in   ) :: z0_out    ! z min trc
    real,                intent(in   ) :: dz_out    ! z inc trc
    real,                intent(in   ) :: rz_scl    ! rz_scale
    !
    real,                intent(in   ) :: tr_out(:, :) ! rz_scale
    !
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer,                 parameter :: mh_out = 64
    integer                            :: i0_out
    integer                            :: jx_out    ! x idx trc
    real                               :: rx_out    ! x val trc
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !print'(" kfun_trace_file_write_slice c=",i8,&
    !& " jy=",i8," nx=",i8," nz=",i8," s1=",i8," s2=",i8)', &
    !i_call, jy_out, nx_out, nz_out, size(tr_out,1), size(tr_out,2) 
    !
    do_jx_out : do jx_out = 1 , nx_out
      !
      rx_out = ( jx_out - 1 ) * dx_out + x0_out
      !
      i0_out = ( jy_out - 1 ) * nx_out + jx_out
      !
      !print'(" a kfun_trace_file_write_slice c=",i8,&
      !& " jy=",i8," jx=",i8," i0=",i8," tr=",g12.6)', &
      !i_call, jy_out, jx_out, i0_out, maxval(abs(tr_out(:,jx_out)))
      !
      call kfun_trace_file_write ( &
                                   trcio_obj, grid_obj, &
                                   invert_coefficient, &
                                   i0_out, &
                                   hx_out, jx_out, rx_out, rx_scl, &
                                   hy_out, jy_out, ry_out, ry_scl, &
                                   nz_out, z0_out, dz_out, rz_scl, &
                                   tr_out(:,jx_out), &
                                   i_err &
                                 )
      !
      !print'(" b kfun_trace_file_write_slice c=",i8,&
      !& " jy=",i8," jx=",i8," i0=",i8," tr=",g12.6)', &
      !i_call, jy_out, jx_out, i0_out, maxval(abs(tr_out(:,jx_out)))
      !
      if ( i_err .lt. 0 ) go to 998
      !
    end do do_jx_out 
    !
    return
    !
998 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_write_slice p=",i4, &
    & /, " during kfun_trace_file_write " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file_write_slice p=",i4 &
    & )', &
    kfun_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine kfun_trace_file_write_slice 
  !
  subroutine kfun_trace_file ( &
                               grid_obj, path_out, &
                               invert_coefficient, &
                               hx_out, hy_out, &
                               rx_scl, ry_scl, rz_scl, &
                               nx_inp, x0_inp, dx_inp, &
                               ny_inp, y0_inp, dy_inp, &
                               nz_inp, z0_inp, dz_inp, &
                               nx_out, x0_out, dx_out, &
                               ny_out, y0_out, dy_out, &
                               nz_out, z0_out, dz_out, &
                               tr_inp, &
                               i_err &
                             )
    !
    ! write a trace file for an array
    !
    type ( grid_struct ),      pointer :: grid_obj  ! global
    character(len=*),    intent(in   ) :: path_out ! file name
    !
    logical,             intent(in   ) :: invert_coefficient ! invert flag
    !
    integer,             intent(in   ) :: hx_out    ! x hdr
    integer,             intent(in   ) :: hy_out    ! y hdr
    !
    real,                intent(in   ) :: rx_scl    ! rx_scale
    real,                intent(in   ) :: ry_scl    ! ry_scale
    real,                intent(in   ) :: rz_scl    ! rz_scale
    !
    integer,             intent(in   ) :: nx_inp    ! inp x num trc
    real,                intent(in   ) :: x0_inp    ! inp x min trc
    real,                intent(in   ) :: dx_inp    ! inp x inc trc
    !
    integer,             intent(in   ) :: ny_inp    ! inp y num trc
    real,                intent(in   ) :: y0_inp    ! inp y min trc
    real,                intent(in   ) :: dy_inp    ! inp y inc trc
    !
    integer,             intent(in   ) :: nz_inp    ! inp z num trc
    real,                intent(in   ) :: z0_inp    ! inp z min trc
    real,                intent(in   ) :: dz_inp    ! inp z inc trc
    !
    integer,             intent(in   ) :: nx_out    ! out x num trc
    real,                intent(in   ) :: x0_out    ! out x min trc
    real,                intent(in   ) :: dx_out    ! out x inc trc
    !
    integer,             intent(in   ) :: ny_out    ! out y num trc
    real,                intent(in   ) :: y0_out    ! out y min trc
    real,                intent(in   ) :: dy_out    ! out y inc trc
    !
    integer,             intent(in   ) :: nz_out    ! out z num trc
    real,                intent(in   ) :: z0_out    ! out z min trc
    real,                intent(in   ) :: dz_out    ! out z inc trc
    !
    real,                intent(in   ) :: tr_inp(:, :, :) ! array values
    !
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                            :: i0_out
    !
    integer                            :: jx_out
    integer                            :: jy_out
    !
    real                               :: rx_out
    real                               :: ry_out
    real                               :: tr_out(nz_out)
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    if ( string_upper_compare ( path_out(1:4), 'NONE' ) ) return
    !
    call kfun_trace_file_open ( &
                                trcio_obj, path_out, &
                                nz_out, z0_out, dz_out, rz_scl, &
                                i_err &
                              )
    !
    if ( i_err .lt. 0 ) go to 998
    !
    ! initialize the output trace location index, i0_out
    !
    i0_out = 0
    !
    ! write each column
    !
    ! cycle over each y node
    !
    do_jy_out : do jy_out = 1 , ny_out
      !
      ! compute the output trace y location
      !
      ry_out = ( jy_out - 1 ) * dy_out + y0_out
      !
      ! cycle over each x node
      !
      do_jx_out : do jx_out = 1 , nx_out
        !
        ! compute the output trace x location
        !
        rx_out = ( jx_out - 1 ) * dx_out + x0_out
        !
        ! increment the output trace location index, i0_out 
        !
        i0_out = ( jy_out - 1 ) * nx_out + jx_out
        !
        call interpolate_3d_to_1d ( &
                                    nz_inp, z0_inp, dz_inp, &
                                    nx_inp, x0_inp, dx_inp, &
                                    ny_inp, y0_inp, dy_inp, &
                                    tr_inp, &
                                    nz_out, z0_out, dz_out, &
                                    rx_out, &
                                    ry_out, &
                                    tr_out &
                                  )
        !
        call kfun_trace_file_write ( &
                                     trcio_obj, grid_obj, &
                                     invert_coefficient, &
                                     i0_out, &
                                     hx_out, jx_out, rx_out, rx_scl, &
                                     hy_out, jy_out, ry_out, ry_scl, &
                                     nz_out, z0_out, dz_out, rz_scl, &
                                     tr_out, &
                                     i_err &
                                   )
        !
        if ( i_err .lt. 0 ) go to 997
        !
      end do do_jx_out
      !
    end do do_jy_out
    !
    call kfun_trace_file_close ( trcio_obj, i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    return
    !
996 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file p=",i4, &
    & /, " during kfun_trace_file_close " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
997 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file p=",i4, &
    & /, " during kfun_trace_file_write " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
998 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file p=",i4, &
    & /, " during kfun_trace_file_open " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_trace_file p=",i4 &
    & )', &
    kfun_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine kfun_trace_file 
  !
  subroutine kfun_table_file ( &
                               i, &
                               path_out, &
                               invert_coefficient, &
                               hx_out, nx_out, x0_out, dx_out, rx_scl, rx_src,&
                               hy_out, ny_out, y0_out, dy_out, ry_scl, ry_src,&
                                       nz_out, z0_out, dz_out, rz_scl, &
                               tim_tab, &
                               i_err &
                             )
    !
    ! write a trace file for an array
    !
    type ( ktime_struct ),     pointer :: i  ! ktime  tables   structure
    !
    character(len=*),    intent(in   ) :: path_out ! file name
    !
    logical,             intent(in   ) :: invert_coefficient ! invert flag
    !
    integer,             intent(in   ) :: hx_out    ! x hdr
    integer,             intent(in   ) :: nx_out    ! x num trc
    real,                intent(in   ) :: x0_out    ! x min trc
    real,                intent(in   ) :: dx_out    ! x inc trc
    real,                intent(in   ) :: rx_scl    ! rx_scale
    real,                intent(in   ) :: rx_src    ! rx_source
    !
    integer,             intent(in   ) :: hy_out    ! y hdr
    integer,             intent(in   ) :: ny_out    ! y num trc
    real,                intent(in   ) :: y0_out    ! y min trc
    real,                intent(in   ) :: dy_out    ! y inc trc
    real,                intent(in   ) :: ry_scl    ! ry_scale
    real,                intent(in   ) :: ry_src    ! ry_source
    !
    integer,             intent(in   ) :: nz_out    ! z num trc
    real,                intent(in   ) :: z0_out    ! z min trc
    real,                intent(in   ) :: dz_out    ! z inc trc
    real,                intent(in   ) :: rz_scl    ! rz_scale
    !
    real,                intent(in   ) :: tim_tab(:) ! table values
    !
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                            :: i0_out
    integer                            :: j0_out
    !
    integer                            :: jx_out
    integer                            :: jy_out
    !
    real                               :: rx_out
    real                               :: ry_out
    !
    real                               :: rx_tab
    real                               :: ry_tab
    !
    real                               :: x1_out
    real                               :: y1_out
    real                               :: z1_out
    !
    integer                            :: i1_tab(nz_out)
    integer                            :: i2_tab(nz_out)
    real                               :: f1_tab(nz_out)
    real                               :: f2_tab(nz_out)
    !
    integer,                 parameter :: mh_out = 64
    real                               :: tr_tmp ( nz_out )
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    if ( string_upper_compare ( path_out(1:4), 'NONE' ) ) return
    !
    !if ( kfun_i_pel() .ne. -999 ) return
    !
    if ( kfun_i_pel() .eq. 0 .and. i_call .eq. 1 ) &
    print'(" top kfun_table_file p=", i4," c=",i8," path_out=", a)', &
    kfun_i_pel(), i_call, trim(path_out)
    !
    call kfun_trace_file_open ( &
                                trcio_obj, path_out, &
                                nz_out, z0_out, dz_out, rz_scl, &
                                i_err &
                              )
    !
    if ( i_err .lt. 0 ) go to 998
    !
    x1_out = ( nx_out - 1 ) * dx_out + x0_out
    y1_out = ( ny_out - 1 ) * dy_out + y0_out
    z1_out = ( nz_out - 1 ) * dz_out + z0_out
    !
    if ( kfun_i_pel() .eq. 0 .and. i_call .eq. 1 ) &
    print'( &
    & /, " kfun_table_file p=", i4," c=",i8, &
    & /, " path_out =", a, &
    & /, " mh_out=",i8, &
    & /, " hx_out=",i8," hy_out=",i8,&
    & /, " nx_out=",i8," x0_out=",g16.10," x1_out=",g16.10," dx_out=",g16.10, &
    & " rx_scl=",g16.10 &
    & /, " ny_out=",i8," y0_out=",g16.10," y1_out=",g16.10," dy_out=",g16.10, &
    & " ry_scl=",g16.10 &
    & /, " nz_out=",i8," z0_out=",g16.10," z1_out=",g16.10," dz_out=",g16.10, &
    & " rz_scl=",g16.10 &
    & )', &
    kfun_i_pel(), i_call, &
    trim(path_out), &
    mh_out, &
    hx_out, hy_out, &
    nx_out, x0_out, x1_out, dx_out, rx_scl, &
    ny_out, y0_out, y1_out, dy_out, rx_scl, &
    nz_out, z0_out, z1_out, dz_out, rz_scl
    !
    ! initialize the output trace location index, i0_out
    !
    i0_out = 0
    !
    ! write each column
    ! initialize the trace in group index, j0_inp
    !
    j0_out = 0
    !
    ! compute the vertical interpolation coefficients
    !
    call interpolate_find_index_g ( i%g%ng_tab, i%g%rg_tab, &
                                    nz_out, z0_out, dz_out, &
                                    i1_tab, i2_tab, &
                                    f1_tab, f2_tab &
                                   )
    !
    ! cycle over each y node
    !
    do_jy_out : do jy_out = 1 , ny_out
      !
      ! compute the output trace y location
      !
      ry_out = ( jy_out - 1 ) * dy_out + y0_out
      !
      ! cycle over each x node
      !
      !print'(" kfun_table_file jy_out=",i8," ry_out=",g12.6)', &
      !jy_out, ry_out
      !
      do_jx_out : do jx_out = 1 , nx_out
        !
        ! compute the output trace x location
        !
        rx_out = ( jx_out - 1 ) * dx_out + x0_out
        !
        !print'(" kfun_table_file jx_out=",i8," rx_out=",g12.6)', &
        !jx_out, rx_out
        !
        ! increment the output trace location index, i0_out 
        !
        i0_out = i0_out + 1
        !
        ! increment the trace in group index, j0_inp
        !
        j0_out = j0_out + 1
        !
        ! interpolate from the table grid to the output grid
        !
        rx_tab = rx_out - rx_src
        !
        ry_tab = ry_out - ry_src
        !
        !print'(" a table_file c=",i8, " e=",i8, &
        !& " x=",i4,1x,g12.6,1x,g12.6, &
        !& " y=",i8,1x,g12.6,1x,g12.6)', &
        !i_call, i_err, &
        !jx_out, rx_out, rx_tab, &
        !jy_out, ry_out, ry_tab
        !
        call kfun_compute_tab_xy_p ( i, tim_tab, &
                                     i1_tab, i2_tab, f1_tab, f2_tab, &
                                     rx_tab, ry_tab, &
                                     nz_out, z0_out, dz_out, &
                                     tr_tmp )
        !
        ! write this trace
        !
        !print'(" b table_file c=",i8, " e=",i8, &
        !& " x=",i4,1x,g12.6,1x,g12.6, &
        !& " y=",i8,1x,g12.6,1x,g12.6)', &
        !i_call, i_err, &
        !jx_out, rx_out, rx_tab, &
        !jy_out, ry_out, ry_tab
        !
        call kfun_trace_file_write ( &
                                     trcio_obj, i%grid_obj, &
                                     invert_coefficient, &
                                     i0_out, &
                                     hx_out, jx_out, rx_out, rx_scl, &
                                     hy_out, jy_out, ry_out, ry_scl, &
                                     nz_out, z0_out, dz_out, rz_scl, &
                                     tr_tmp, &
                                     i_err &
                                   )
        !
        !print'(" c table_file c=",i8, " e=",i8, &
        !& " x=",i4,1x,g12.6,1x,g12.6, &
        !& " y=",i8,1x,g12.6,1x,g12.6)', &
        !i_call, i_err, &
        !jx_out, rx_out, rx_tab, &
        !jy_out, ry_out, ry_tab
        !
        if ( i_err .lt. 0 ) go to 997
        !
      end do do_jx_out
      !
    end do do_jy_out
    !
    !if ( kfun_i_pel() .eq. 0 ) &
    !print'(" aa1 kfun_table_file p=", i4," c=",i8," path_out=", a)', &
    !kfun_i_pel(), i_call, trim(path_out)
    !
    call kfun_trace_file_close ( trcio_obj, i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    !if ( kfun_i_pel() .eq. 0 ) &
    !print'(" end kfun_table_file p=", i4," c=",i8," path_out=", a)', &
    !kfun_i_pel(), i_call, trim(path_out)
    !
    return
    !
996 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_table_file p=",i4, &
    & /, " during kfun_trace_file_close " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
997 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_table_file p=",i4, &
    & /, " during kfun_trace_file_write " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
998 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_table_file p=",i4, &
    & /, " during kfun_trace_file_open " &
    & )', &
    kfun_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( kfun_print() .ge. 1 ) print'( &
    & /, " error in kfun_table_file p=",i4 &
    & )', &
    kfun_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine kfun_table_file 
  !
  subroutine kfun_compute_tab_xy ( i, tim_tab, &
                                   rx_out, ry_out, &
                                   nz_out, z0_out, dz_out, &
                                   tim_out )
    !
    type ( ktime_struct ),     pointer :: i  ! ktime  tables   structure
    !
    real,                intent(in   ) :: tim_tab(:)
    !
    real,                intent(in   ) :: rx_out
    real,                intent(in   ) :: ry_out
    integer,             intent(in   ) :: nz_out
    real,                intent(in   ) :: z0_out
    real,                intent(in   ) :: dz_out
    !
    real,                intent(inout) :: tim_out(:)
    !
    integer                            :: jx_tab_1
    integer                            :: jx_tab_2
    !
    integer                            :: jy_tab_1
    integer                            :: jy_tab_2
    !
    integer                            :: ix_tab_1
    integer                            :: ix_tab_2
    !
    integer                            :: iy_tab_1
    integer                            :: iy_tab_2
    !
    real                               :: fx_tab_1
    real                               :: fx_tab_2
    !
    real                               :: fy_tab_1
    real                               :: fy_tab_2
    !
    real                               :: rx_tab_1
    real                               :: rx_tab_2
    !
    real                               :: ry_tab_1
    real                               :: ry_tab_2
    !
    integer                            :: jz_tab_11
    integer                            :: jz_tab_12
    integer                            :: jz_tab_21
    integer                            :: jz_tab_22
    !
    integer                            :: j_xy_tab_11
    integer                            :: j_xy_tab_21
    integer                            :: j_xy_tab_12
    integer                            :: j_xy_tab_22
    !
    integer                            :: j_xyz_tab_11
    integer                            :: j_xyz_tab_21
    integer                            :: j_xyz_tab_12
    integer                            :: j_xyz_tab_22
    !
    real                               :: tim_tab_11 ( nz_out )
    real                               :: tim_tab_21 ( nz_out )
    real                               :: tim_tab_12 ( nz_out )
    real                               :: tim_tab_22 ( nz_out )
    !
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    !print'(" aa1 kfun_compute_tab_xy c=",i8)', i_call
    !
    tim_out ( 1:nz_out ) = 0.
    !
    ! compute x grid locations
    !
    call kfun_xn_index ( rx_out, &
                         i%mx_tab, i%x2_tab, i%dx_tab, &
                         ix_tab_1, fx_tab_1, rx_tab_1, &
                         ix_tab_2, fx_tab_2, rx_tab_2  )
    !
    ! compute y grid locations
    !
    call kfun_xn_index ( ry_out, &
                         i%ny_tab, i%y0_tab, i%dy_tab, &
                         iy_tab_1, fy_tab_1, ry_tab_1, &
                         iy_tab_2, fy_tab_2, ry_tab_2 )
    !
    ! compute the x,y,z grid incdices for each x,y node
    !
    call kfun_xy_index ( &
    i, rx_tab_1, ry_tab_1, jx_tab_1, jy_tab_1, j_xy_tab_11, j_xyz_tab_11 )
    !
    call kfun_xy_index ( &
    i, rx_tab_2, ry_tab_1, jx_tab_2, jy_tab_1, j_xy_tab_21, j_xyz_tab_21 )
    !
    call kfun_xy_index ( &
    i, rx_tab_1, ry_tab_2, jx_tab_1, jy_tab_2, j_xy_tab_12, j_xyz_tab_12 )
    !
    call kfun_xy_index ( &
    i, rx_tab_2, ry_tab_2, jx_tab_2, jy_tab_2, j_xy_tab_22, j_xyz_tab_22 )
    !
    ! compute the start of each non uniform column within rg_tab
    !
    jz_tab_11 = i%g%ng_tab - i%nz_tab(j_xy_tab_11) + 1
    !
    jz_tab_21 = i%g%ng_tab - i%nz_tab(j_xy_tab_21) + 1
    !
    jz_tab_12 = i%g%ng_tab - i%nz_tab(j_xy_tab_12) + 1
    !
    jz_tab_22 = i%g%ng_tab - i%nz_tab(j_xy_tab_22) + 1
    ! 
    ! interpolate from the non uniform z grid the uniform z 
    !
    call interpolate_i_to_r ( &
                              i%nz_tab(j_xy_tab_11), i%g%rg_tab(jz_tab_11:), &
                              tim_tab(j_xyz_tab_11:), &
                              nz_out, z0_out, dz_out, &
                              tim_tab_11 )
    !
    call interpolate_i_to_r ( &
                              i%nz_tab(j_xy_tab_21), i%g%rg_tab(jz_tab_21:), &
                              tim_tab(j_xyz_tab_21:), &
                              nz_out, z0_out, dz_out, &
                              tim_tab_21 )
    !
    call interpolate_i_to_r ( &
                              i%nz_tab(j_xy_tab_12), i%g%rg_tab(jz_tab_12:), &
                              tim_tab(j_xyz_tab_12:), &
                              nz_out, z0_out, dz_out, &
                              tim_tab_12 )
    !
    call interpolate_i_to_r ( &
                              i%nz_tab(j_xy_tab_22), i%g%rg_tab(jz_tab_22:), &
                              tim_tab(j_xyz_tab_22:), &
                              nz_out, z0_out, dz_out, &
                              tim_tab_22 )
    !
    ! interpolate from the four uniform z grid tim_tabs
    !
                            tim_out    ( 1:nz_out ) = &
      fx_tab_1 * fy_tab_1 * tim_tab_11 ( 1:nz_out ) &
    + fx_tab_2 * fy_tab_1 * tim_tab_21 ( 1:nz_out ) &
    + fx_tab_1 * fy_tab_2 * tim_tab_12 ( 1:nz_out ) &
    + fx_tab_2 * fy_tab_2 * tim_tab_22 ( 1:nz_out ) 
    !
    !print'(" kfun_compute_tab_xy c=",i8, &
    !& " x=",g12.6,1x,g12.6,1x,g12.6,1x,1x,i8,1x,i8,g12.6,1x,g12.6,1x)', &
    !i_call, rx_out, rx_tab_1, ix_tab_1, ix_tab_2, fx_tab_1, fx_tab_2
    !
    !print'(" kfun_compute_tab_xy c=",i8, &
    !& " y=",g12.6,1x,g12.6,1x,g12.6,1x,1x,i8,1x,i8,g12.6,1x,g12.6,1x)', &
    !i_call, ry_out, ry_tab_1, iy_tab_1, iy_tab_2, fy_tab_1, fy_tab_2
    !
    return
    !
  end subroutine kfun_compute_tab_xy 
  !
  subroutine kfun_compute_tab_xy_p ( i, tim_tab, &
                                     i1_tab, i2_tab, f1_tab, f2_tab, &
                                     rx_out, ry_out, &
                                     nz_out, z0_out, dz_out, &
                                     tim_out )
    !
    type ( ktime_struct ),     pointer :: i  ! ktime  tables   structure
    !
    real,                intent(in   ) :: tim_tab(:)
    !
    integer,             intent(in   ) :: i1_tab(:)
    integer,             intent(in   ) :: i2_tab(:)
    real,                intent(in   ) :: f1_tab(:)
    real,                intent(in   ) :: f2_tab(:)
    real,                intent(in   ) :: rx_out
    real,                intent(in   ) :: ry_out
    integer,             intent(in   ) :: nz_out
    real,                intent(in   ) :: z0_out
    real,                intent(in   ) :: dz_out
    !
    real,                intent(inout) :: tim_out(:)
    !
    integer                            :: jx_tab_11
    integer                            :: jx_tab_21
    integer                            :: jx_tab_12
    integer                            :: jx_tab_22
    !
    integer                            :: jy_tab_11
    integer                            :: jy_tab_21
    integer                            :: jy_tab_12
    integer                            :: jy_tab_22
    !
    integer                            :: ix_tab_1
    integer                            :: ix_tab_2
    !
    integer                            :: iy_tab_1
    integer                            :: iy_tab_2
    !
    real                               :: fx_tab_1
    real                               :: fx_tab_2
    !
    real                               :: fy_tab_1
    real                               :: fy_tab_2
    !
    real                               :: rx_tab_1
    real                               :: rx_tab_2
    !
    real                               :: ry_tab_1
    real                               :: ry_tab_2
    !
    integer                            :: jz_tab_11
    integer                            :: jz_tab_12
    integer                            :: jz_tab_21
    integer                            :: jz_tab_22
    !
    integer                            :: j_xy_tab_11
    integer                            :: j_xy_tab_21
    integer                            :: j_xy_tab_12
    integer                            :: j_xy_tab_22
    !
    integer                            :: j_xyz_tab_11
    integer                            :: j_xyz_tab_21
    integer                            :: j_xyz_tab_12
    integer                            :: j_xyz_tab_22
    !
    !integer                            :: jz_out
    !
    real                               :: tim_tab_11 ( i%g%ng_tab )
    real                               :: tim_tab_21 ( i%g%ng_tab )
    real                               :: tim_tab_12 ( i%g%ng_tab )
    real                               :: tim_tab_22 ( i%g%ng_tab )
    !
    real                               :: tim_out_11 ( nz_out )
    real                               :: tim_out_21 ( nz_out )
    real                               :: tim_out_12 ( nz_out )
    real                               :: tim_out_22 ( nz_out )
    !
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    !print'(" aa1 kfun_compute_tab_xy c=",i8)', i_call
    !
    tim_tab_11 ( 1:i%g%ng_tab ) = 0.
    tim_tab_21 ( 1:i%g%ng_tab ) = 0.
    tim_tab_12 ( 1:i%g%ng_tab ) = 0.
    tim_tab_22 ( 1:i%g%ng_tab ) = 0.
    tim_out ( 1:nz_out ) = 0.
    !
    ! compute x grid locations
    !
    call kfun_xn_index ( rx_out, &
                         i%mx_tab, i%x2_tab, i%dx_tab, &
                         ix_tab_1, fx_tab_1, rx_tab_1, &
                         ix_tab_2, fx_tab_2, rx_tab_2  )
    !
    ! compute y grid locations
    !
    call kfun_xn_index ( ry_out, &
                         i%ny_tab, i%y0_tab, i%dy_tab, &
                         iy_tab_1, fy_tab_1, ry_tab_1, &
                         iy_tab_2, fy_tab_2, ry_tab_2 )
    !
    ! compute the x,y,z grid incdices for each x,y node
    !
    call kfun_xy_index ( &
    i, rx_tab_1, ry_tab_1, jx_tab_11, jy_tab_11, j_xy_tab_11, j_xyz_tab_11 )
    !
    call kfun_xy_index ( &
    i, rx_tab_2, ry_tab_1, jx_tab_21, jy_tab_21, j_xy_tab_21, j_xyz_tab_21 )
    !
    call kfun_xy_index ( &
    i, rx_tab_1, ry_tab_2, jx_tab_12, jy_tab_12, j_xy_tab_12, j_xyz_tab_12 )
    !
    call kfun_xy_index ( &
    i, rx_tab_2, ry_tab_2, jx_tab_22, jy_tab_22, j_xy_tab_22, j_xyz_tab_22 )
    !
    ! compute the start of each non uniform column within rg_tab
    !
    jz_tab_11 = i%g%ng_tab - i%nz_tab(j_xy_tab_11) + 1
    !
    jz_tab_21 = i%g%ng_tab - i%nz_tab(j_xy_tab_21) + 1
    !
    jz_tab_12 = i%g%ng_tab - i%nz_tab(j_xy_tab_12) + 1
    !
    jz_tab_22 = i%g%ng_tab - i%nz_tab(j_xy_tab_22) + 1
    ! 
    ! copy from the variable number of z nodes, tim_tab
    ! to the constant number of z  nodes, tim_tab_11, ...
    !
    tim_tab_11(jz_tab_11:i%g%ng_tab) = &
    tim_tab   (j_xyz_tab_11:j_xyz_tab_11+i%g%ng_tab-i%nz_tab(j_xy_tab_11))
    !
    tim_tab_21(jz_tab_21:i%g%ng_tab) = &
    tim_tab   (j_xyz_tab_21:j_xyz_tab_21+i%g%ng_tab-i%nz_tab(j_xy_tab_21))
    !
    tim_tab_12(jz_tab_12:i%g%ng_tab) = &
    tim_tab   (j_xyz_tab_12:j_xyz_tab_12+i%g%ng_tab-i%nz_tab(j_xy_tab_12))
    !
    tim_tab_22(jz_tab_22:i%g%ng_tab) = &
    tim_tab   (j_xyz_tab_22:j_xyz_tab_22+i%g%ng_tab-i%nz_tab(j_xy_tab_22))
    !
    ! interpolate from the non uniform z grid the uniform z 
    !
    call interpolate_i_to_r_p ( &
                                i1_tab, i2_tab, &
                                f1_tab, f2_tab, &
                                tim_tab_11, &
                                nz_out, &
                                tim_out_11 )
    !
    call interpolate_i_to_r_p ( &
                                i1_tab, i2_tab, &
                                f1_tab, f2_tab, &
                                tim_tab_21, &
                                nz_out, &
                                tim_out_21 )
    !
    call interpolate_i_to_r_p ( &
                                i1_tab, i2_tab, &
                                f1_tab, f2_tab, &
                                tim_tab_12, &
                                nz_out, &
                                tim_out_12 )
    !
    call interpolate_i_to_r_p ( &
                                i1_tab, i2_tab, &
                                f1_tab, f2_tab, &
                                tim_tab_22, &
                                nz_out, &
                                tim_out_22 )
    !
    ! interpolate from the four uniform z grid tim_tabs
    !
                            tim_out    ( 1:nz_out ) = &
      fx_tab_1 * fy_tab_1 * tim_out_11 ( 1:nz_out ) &
    + fx_tab_2 * fy_tab_1 * tim_out_21 ( 1:nz_out ) &
    + fx_tab_1 * fy_tab_2 * tim_out_12 ( 1:nz_out ) &
    + fx_tab_2 * fy_tab_2 * tim_out_22 ( 1:nz_out ) 
    !
    !if ( i_call .eq. 101 ) print*,' ix_tab_1=',ix_tab_1
    !if ( i_call .eq. 101 ) print*,' ix_tab_2=',ix_tab_2
    !if ( i_call .eq. 101 ) print*,' iy_tab_1=',iy_tab_1
    !if ( i_call .eq. 101 ) print*,' iy_tab_2=',iy_tab_2
    !
    !if ( i_call .eq. 101 ) print*,' fx_tab_1=',fx_tab_1
    !if ( i_call .eq. 101 ) print*,' fx_tab_2=',fx_tab_2
    !if ( i_call .eq. 101 ) print*,' fy_tab_1=',fy_tab_1
    !if ( i_call .eq. 101 ) print*,' fy_tab_2=',fy_tab_2
    !
    !if ( i_call .eq. 101 ) print*,' rx_tab_1=',rx_tab_1
    !if ( i_call .eq. 101 ) print*,' rx_tab_2=',rx_tab_2
    !if ( i_call .eq. 101 ) print*,' ry_tab_1=',ry_tab_1
    !if ( i_call .eq. 101 ) print*,' ry_tab_2=',ry_tab_2
    !
    !if ( i_call .eq. 101 ) &
    !print*,' x1y1=', &
    !rx_tab_1, ry_tab_1, jx_tab_11, jy_tab_11, j_xy_tab_11, j_xyz_tab_11 
    !
    !if ( i_call .eq. 101 ) &
    !print*,' x2y1=', &
    !rx_tab_2, ry_tab_1, jx_tab_21, jy_tab_21, j_xy_tab_21, j_xyz_tab_21 
    !
    !if ( i_call .eq. 101 ) &
    !print*,' x1y2=', &
    !rx_tab_1, ry_tab_2, jx_tab_12, jy_tab_12, j_xy_tab_12, j_xyz_tab_12 
    !
    !if ( i_call .eq. 101 ) &
    !print*,' x2y2=', &
    !rx_tab_2, ry_tab_2, jx_tab_22, jy_tab_22, j_xy_tab_22, j_xyz_tab_22 
    !
    !print'(" kfun_compute_tab_xy c=",i8, &
    !& " x=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& 1x,i8,1x,i8,g12.6,1x,g12.6,1x)', &
    !i_call, rx_out, rx_tab_1, rx_tab_2, ix_tab_1, ix_tab_2, fx_tab_1, fx_tab_2
    !
    !print'(" kfun_compute_tab_xy c=",i8, &
    !& " y=",g12.6,1x,g12.6,1x,g12.6,1x,1x,i8,1x,i8,g12.6,1x,g12.6,1x)', &
    !i_call, ry_out, ry_tab_1, iy_tab_1, iy_tab_2, fy_tab_1, fy_tab_2
    !
    !if ( i_call .eq. 100 &
    !.or. i_call .eq. 101 &
    !.or. i_call .eq. 102 ) &
    !print'(" kfun_compute_tab_xy c=",i8," z=",i8," t=", &
    !& 1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)', &
    !( i_call, jz_out, &
    ! tim_out    ( jz_out ), &
    ! tim_out_11 ( jz_out ), &
    ! tim_out_21 ( jz_out ), &
    ! tim_out_12 ( jz_out ), &
    ! tim_out_22 ( jz_out ), &
    ! jz_out = 1 , nz_out )
    !
    !if ( i_call .eq. 101 ) stop
    !
    return
    !
  end subroutine kfun_compute_tab_xy_p 
  !
  subroutine kfun_xn_index ( rx_tab, &
                             nx_tab, x0_tab, dx_tab, &
                             ix_tab_1, fx_tab_1, rx_tab_1, &
                             ix_tab_2, fx_tab_2, rx_tab_2 )
    !
    ! compute the nearest nodes
    !
    real,                intent(in   ) :: rx_tab
    integer,             intent(in   ) :: nx_tab
    real,                intent(in   ) :: x0_tab
    real,                intent(in   ) :: dx_tab
    !
    integer,             intent(inout) :: ix_tab_1
    real,                intent(inout) :: fx_tab_1
    real,                intent(inout) :: rx_tab_1
    !
    integer,             intent(inout) :: ix_tab_2
    real,                intent(inout) :: fx_tab_2
    real,                intent(inout) :: rx_tab_2
    !
    real                               :: r0_eps
    real                               :: rx_eps
    !
    integer, save                    :: i_call = 0
    i_call = i_call + 1
    !
    r0_eps = 1.e-3
    !
    rx_eps = dx_tab * r0_eps
    !
    ix_tab_1 = max ( 1, int ( ( rx_tab - x0_tab ) / dx_tab ) + 1 )
    !
    ix_tab_2 = min ( nx_tab, ix_tab_1 + 1 )
    !
    rx_tab_1 = ( ix_tab_1 - 1 ) * dx_tab + x0_tab 
    !
    rx_tab_2 = ( ix_tab_2 - 1 ) * dx_tab + x0_tab 
    !
    xxif_rx_tab : &
         if ( abs ( rx_tab - rx_tab_1 ) .le. rx_eps ) then
      !
      ix_tab_2 = ix_tab_1 
      !
    else if ( abs ( rx_tab - rx_tab_2 ) .le. rx_eps ) then
      !
      ix_tab_1 = ix_tab_2 
      !
    end if xxif_rx_tab 
    !
    rx_tab_1 = ( ix_tab_1 - 1 ) * dx_tab + x0_tab 
    !
    rx_tab_2 = ( ix_tab_2 - 1 ) * dx_tab + x0_tab 
    !
    fx_tab_2 = max ( 0., max ( 1., ( rx_tab - rx_tab_1 ) / dx_tab ) )
    !
    fx_tab_1 = 1. - fx_tab_2
    !
    !print'( &
    !& /, " kfun_xn_index c=",i8, " nx=",i8," x0=",g12.6," dx=",g12.6, &
    !& /, " kfun_xn_index x=",g12.6,1x,g12.6,1x,g12.6,&
    !& /, " kfun_xn_index f=",g12.6,1x,g12.6, &
    !& /, " kfun_xn_index i=",i8,1x,i8 &
    !& )', &
    !i_call, &
    !nx_tab, x0_tab, dx_tab, &
    !rx_tab, &
    !rx_tab_1, rx_tab_2, &
    !fx_tab_1, fx_tab_2, &
    !ix_tab_1, ix_tab_2
    !
    return
    !
  end subroutine kfun_xn_index 
  !
  subroutine kfun_xy_index ( i, rx_tab, ry_tab, &
                             jx_tab, jy_tab, j_xy_tab, j_xyz_tab )
    !
    type ( ktime_struct ),     pointer :: i  ! ktime  tables   structure
    real,                intent(in   ) :: rx_tab
    real,                intent(in   ) :: ry_tab
    integer,             intent(inout) :: jx_tab
    integer,             intent(inout) :: jy_tab
    integer,             intent(inout) :: j_xy_tab
    integer,             intent(inout) :: j_xyz_tab
    integer, save                    :: i_call = 0
    i_call = i_call + 1
    !
    jy_tab = max ( 1, min ( i%ny_tab, &
    int ( ( ry_tab - i%y0_tab ) / i%dy_tab ) + 1 ) ) 
    !
    jx_tab = max ( 1, min ( i%nx_tab(jy_tab), &
    int ( ( rx_tab - i%x0_tab(jy_tab) ) / i%dx_tab ) + 1 ) ) 
    !
    j_xy_tab = i%ix_tab(jy_tab) + jx_tab
    !
    j_xyz_tab = i%iz_tab(j_xy_tab) + 1
    !
    return
    !
  end subroutine kfun_xy_index 
  !
  subroutine kfun_print_time ( c0_prn, i1_prn, i2_prn, r1_prn, r2_prn )
    !
    character(len=*),    intent(in   ) :: c0_prn 
    integer,             intent(in   ) :: i1_prn 
    integer,             intent(in   ) :: i2_prn 
    real,                intent(in   ) :: r1_prn 
    real,                intent(in   ) :: r2_prn 
    !
    double precision                   :: d0_cpu
    character(len=12)                  :: c0_tim
    !
    d0_cpu = unix_utime ( )
    !
    call string_time ( c0_tim )
    !
    print'(" kfun_print_time ",i4,1x,a12,1x,g12.6,&
    & 1x,i8,1x,i8,1x,g12.6,1x,g12.6,1x,a)', &
    kfun_i_pel(), trim ( c0_tim ), d0_cpu, &
    i1_prn, i2_prn, r1_prn, r2_prn, trim ( c0_prn )
    !
    return
    !
  end subroutine kfun_print_time 
  !
  subroutine kfun_swap_r0 ( x1_dat, x2_dat )
    !
    ! swap two reals
    !
    real,                intent(inout) :: x1_dat
    real,                intent(inout) :: x2_dat
    !
    real                               :: x1_tmp
    !
    x1_tmp = x1_dat
    !
    x1_dat = x2_dat
    !
    x2_dat = x1_tmp 
    !
    return
    !
  end subroutine kfun_swap_r0 
  !
  subroutine kfun_swap_r1 ( x1_dat, x2_dat )
    !
    ! swap two reals
    !
    real,                intent(inout) :: x1_dat(:)
    real,                intent(inout) :: x2_dat(:)
    !
    integer                            :: n1_dat
    real                               :: x1_tmp(size(x1_dat,1))
    !
    n1_dat = size(x1_dat,1)
    !
    x1_tmp (1:n1_dat) = x1_dat (1:n1_dat)
    !
    x1_dat (1:n1_dat) = x2_dat (1:n1_dat) 
    !
    x2_dat (1:n1_dat) = x1_tmp (1:n1_dat) 
    !
    return
    !
  end subroutine kfun_swap_r1 
  !
  subroutine kfun_swap_r2 ( x1_dat, x2_dat )
    !
    ! swap two reals
    !
    real,                intent(inout) :: x1_dat(:,:)
    real,                intent(inout) :: x2_dat(:,:)
    !
    integer                            :: n1_dat
    integer                            :: n2_dat
    real                               :: x1_tmp(size(x1_dat,1), &
                                                 size(x1_dat,2) )
    !
    n1_dat = min ( size(x1_dat,1), size(x2_dat,1) )
    !
    n2_dat = min ( size(x1_dat,2), size(x2_dat,2) )
    !
    x1_tmp (1:n1_dat,1:n2_dat) = x1_dat (1:n1_dat,1:n2_dat)
    !
    x1_dat (1:n1_dat,1:n2_dat) = x2_dat (1:n1_dat,1:n2_dat) 
    !
    x2_dat (1:n1_dat,1:n2_dat) = x1_tmp (1:n1_dat,1:n2_dat) 
    !
    return
    !
  end subroutine kfun_swap_r2 
  !
  subroutine kfun_swap_i0 ( x1_dat, x2_dat )
    !
    ! swap two integers
    !
    integer,             intent(inout) :: x1_dat
    integer,             intent(inout) :: x2_dat
    !
    integer                            :: x1_tmp
    !
    x1_tmp = x1_dat
    !
    x1_dat = x2_dat
    !
    x2_dat = x1_tmp 
    !
    return
    !
  end subroutine kfun_swap_i0 
  !
  subroutine kfun_swap_i1 ( x1_dat, x2_dat )
    !
    ! swap two integers
    !
    integer,             intent(inout) :: x1_dat(:)
    integer,             intent(inout) :: x2_dat(:)
    !
    integer                            :: n1_dat
    integer                            :: x1_tmp(size(x1_dat,1))
    !
    n1_dat = size(x1_dat,1)
    !
    x1_tmp (1:n1_dat) = x1_dat (1:n1_dat)
    !
    x1_dat (1:n1_dat) = x2_dat (1:n1_dat) 
    !
    x2_dat (1:n1_dat) = x1_tmp (1:n1_dat) 
    !
    return
    !
  end subroutine kfun_swap_i1 
  !
  subroutine kfun_swap_i2 ( x1_dat, x2_dat )
    !
    ! swap two integers
    !
    integer,             intent(inout) :: x1_dat(:,:)
    integer,             intent(inout) :: x2_dat(:,:)
    !
    integer                            :: n1_dat
    integer                            :: n2_dat
    integer                            :: x1_tmp(size(x1_dat,1), &
                                                 size(x1_dat,2) )
    !
    n1_dat = min ( size(x1_dat,1), size(x2_dat,1) )
    !
    n2_dat = min ( size(x1_dat,2), size(x2_dat,2) )
    !
    x1_tmp (1:n1_dat,1:n2_dat) = x1_dat (1:n1_dat,1:n2_dat)
    !
    x1_dat (1:n1_dat,1:n2_dat) = x2_dat (1:n1_dat,1:n2_dat) 
    !
    x2_dat (1:n1_dat,1:n2_dat) = x1_tmp (1:n1_dat,1:n2_dat) 
    !
    return
    !
  end subroutine kfun_swap_i2 
  !
  subroutine kfun_print_aperture ( i, c_title, i_print )
    !
    ! print aperture characteristics
    !
    type ( ktime_struct ),   pointer :: i  ! ktime  tables   structure
    character(len=*),  intent(in   ) :: c_title
    integer,           intent(in   ) :: i_print
    !
    integer                          :: ix, iy
    integer                          :: ix_2, nx_2
    integer                          :: iy_2
    integer                          :: iz_2, nz_2
    integer, save                    :: i_call = 0
    i_call = i_call + 1
    !
 !print*,' kfun_print_aperture a1 '
 !print*,' kfun_print_aperture a51 '
    !
    iy_2 = i%ny_tab / 2 + 1    ! center y row
    nx_2 = i%nx_tab(iy_2)      ! number of x points in center y row
    ix_2 = i%ix_tab(iy_2) + nx_2 / 2 + 1 ! center x location in center y row
    nz_2 = i%nz_tab(ix_2)      ! number of z points in center x in center y
    iz_2 = i%iz_tab(ix_2) + 1  ! first table location in center x in center y
    !
    if ( i_print .ge. 1 ) &
    print'( &
    & /, " top kfun_print_aperture c=",i8, 1x, a, &
    & /, " travel time table aperture characteristics", &
    & /, " nx_tab=", i5, " x0_tab=", g12.6, " x1_tab=", g12.6, &
    & " dx_tab=", g12.6, &
    & /, " ny_tab=", i5, " y0_tab=", g12.6, " y1_tab=", g12.6, &
    & " dy_tab=", g12.6, &
    & /, " nz_tab=", i5, " z0_tab=", g12.6, " z1_tab=", g12.6, &
    & " dz_tab=", g12.6, &
    & /, " ng_tab=", i5, " g1_tab=", g12.6, " gn_tab=", g12.6, &
    & /, " d1_tab=", g12.6, " dn_tab=", g12.6, &
    & / &
    & )', &
    i_call, trim(c_title), &
    nx_2  ,i%x0_tab(iy_2), (nx_2  -1)*i%dx_tab+i%x0_tab(iy_2), i%dx_tab, &
    i%ny_tab, i%y0_tab, (i%ny_tab-1)*i%dy_tab+i%y0_tab, i%dy_tab, &
    nz_2  ,i%z0_tab(ix_2), (nz_2  -1)*i%g%dz_tab+i%z0_tab(ix_2), i%g%dz_tab, &
    i%g%ng_tab, i%g%rg_tab(1), i%g%rg_tab(i%g%ng_tab), &
    i%g%dg_tab(1), i%g%dg_tab(i%g%ng_tab)
    !
    ! print the table aperture along constant x slices
    !
 !print*,' kfun_print_aperture a2 '
    !
    !if ( i%ny_tab .eq. -999 ) &
    if ( i_print .ge. 2 ) &
    call kfun_print_aperture_x ( i, c_title )
    !
 !print*,' kfun_print_aperture a3 '
    !
    ! print the table aperture along constant y slices
    !
    !if ( i%ny_tab .eq. -999 ) &
    if ( i_print .ge. 3 ) &
    call kfun_print_aperture_y ( i, c_title )
    !
 !print*,' kfun_print_aperture a4 '
    !
    ! print the table aperture along constant z slices
    !
    !if ( i%ny_tab .eq. -999 ) &
    if ( i_print .ge. 4 ) &
    call kfun_print_aperture_z ( i, c_title )
    !
    xxif_i_print : if ( i_print .ge. 5 ) then
      !
      print'( &
      & /, " czzz" &
      & )'
      !
      print'(1x, i8, 1x, g12.6, 1x, g12.6 &
      & )', &
      i%ny_tab, i%y0_tab, i%dy_tab
      !
      do_iy : do iy = 1 , i%ny_tab , max(1, i%ny_tab/10)
        !
        print'(1x, i8, 1x, i8, 1x, g12.6, 1x, g12.6, 1x, i8&
        & )', &
        i%ix_tab(iy), i%nx_tab(iy), i%x0_tab(iy), i%dx_tab, iy
        !
        do_ix : do ix = i%ix_tab(iy)+1 , i%ix_tab(iy)+i%nx_tab(iy), &
                                         max(1, i%nx_tab(iy)/10)
          !
          print'(1x, i8, 1x, i8, 1x, g12.6, 1x, g12.6, 1x, i8&
          & )', &
          i%iz_tab(ix), i%nz_tab(ix), i%z0_tab(ix), i%g%dz_tab, ix
          !
        end do do_ix
        !
      end do do_iy
      !
      print'( &
      & /, " czzz" &
      & )'
      !
    end if xxif_i_print 
    !
    if ( i_print .ge. 1 ) &
    print'( &
    & /, " end kfun_print_aperture ", a &
    & / &
    & )', &
    trim(c_title)
    !
    return
    !
  end subroutine kfun_print_aperture
  !
  subroutine kfun_print_aperture_a ( i, c_title )
    !
    ! print the table aperture along constant z slices
    !
    type ( ktime_struct ),   pointer :: i  ! ktime  tables   structure
    character(len=*),  intent(in   ) :: c_title
    !
    integer                          :: ix, iy, iz, ia, ir
    integer                          :: ix_2, nx_2
    integer                          :: iy_2
    integer                          :: iz_2, nz_2
    real                             :: rx_tab, ry_tab, rz_tab, a_tab, r_tab
    integer                          :: na_tab
    real                             :: da_tab, a0_tab, a1_tab
    integer                          :: nr_tab
    real                             :: dr_tab, r0_tab, r1_tab
    integer, save                    :: i_call = 0
    i_call = i_call + 1
    !
    na_tab = 61
    na_tab = 121
    a0_tab = 0.
    a1_tab = 4. * asin(1.)
    da_tab = (a1_tab - a0_tab) / (na_tab - 1)
    !
    r0_tab = 0.
    nr_tab = 101
    r1_tab = i%r_xy_tab + 100.
    dr_tab = (r1_tab - r0_tab) / (nr_tab - 1)
    !
    iy_2 = i%ny_tab / 2 + 1    ! center y row
    nx_2 = i%nx_tab(iy_2)      ! number of x points in center y row
    ix_2 = i%ix_tab(iy_2) + nx_2 / 2 + 1 ! center x location in center y row
    nz_2 = i%nz_tab(ix_2)      ! number of z points in center x in center y
    iz_2 = i%iz_tab(ix_2) + 1  ! first table location in center x in center y
    !
    print'( &
    & " kfun_print_aperture_a top of z slices c=", i8, 1x, a &
    & /, " top of z slices", &
    & /, "      x                y             z", &
    & "        ix       iy       iz" &
    & )', &
    i_call, trim(c_title)
    !
    !write(83, '( &
    !& 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, 1x, i8&
    !& )', &
    !0., 0., 0., 0, 0, 0
    !
    do_iz : do iz = 1 , i%nz_tab(ix_2) , max(1, i%nz_tab(ix_2)/10)
      !
      rz_tab = (iz - 1) * i%g%dz_tab + i%z0_tab(ix_2)
      !
      do_ia : do ia = 1 , na_tab
        !
        a_tab = (ia - 1) * da_tab + a0_tab
        !
        do_ir : do ir = nr_tab , 1 , -1
          !
          r_tab = (ir - 1) * dr_tab + r0_tab
          !
          ry_tab = r_tab * sin(a_tab)
          rx_tab = r_tab * cos(a_tab)
          !
          iy = max(1, min(i%ny_tab    ,nint((ry_tab-i%y0_tab    )*i%cy_tab)+1))
          ix = max(1, min(i%nx_tab(iy),nint((rx_tab-i%x0_tab(iy))*i%cx_tab)+1))&
                        + i%ix_tab(iy)
          !
          if ( abs(ry_tab) .gt. abs(i%y0_tab    ) &
          .or. abs(rx_tab) .gt. abs(i%x0_tab(iy)) ) go to 2
          !
          xxif_z : if ( i%z0_tab(ix) .le. rz_tab+1.e-2) then
            !
            print'(&
            & 1x, g12.6, 1x, g12.6, 1x, g12.6, &
            & 1x, i8, 1x, i8, 1x, i8, 1x, i8, 1x, i8 &
            & )', &
            rx_tab, ry_tab, i%z0_tab(ix), ix, iy, iz, ia, ir
            !
            go to 1
            !
          end if xxif_z
          !
    2     continue
          !
        end do do_ir
        !
  1     continue
        !
      end do do_ia
      !
    end do do_iz
    !
    print'( &
    & " kfun_print_aperture_a end of z slices c=", i8, 1x, a &
    & )', &
    i_call, trim(c_title)
    !
    return
    !
  end subroutine kfun_print_aperture_a 
  !
  subroutine kfun_print_aperture_x ( i, c_title )
    !
    ! print the table aperture along constant x slices
    !
    type ( ktime_struct ),   pointer :: i  ! ktime  tables   structure
    character(len=*),  intent(in   ) :: c_title
    !
    ! Local variables
    !
    integer                          :: nx_inc
    integer                          :: ny_inc
    integer                          :: ix, iy, jx
    integer                          :: ix_2, nx_2
    integer                          :: iy_2
    integer                          :: iz_2, nz_2
    real                             :: rx_tab
    integer, save                    :: i_call = 0
    i_call = i_call + 1
    !
    iy_2 = i%ny_tab / 2 + 1    ! center y row
    nx_2 = i%nx_tab(iy_2)      ! number of x points in center y row
    ix_2 = i%ix_tab(iy_2) + nx_2 / 2 + 1 ! center x location in center y row
    nz_2 = i%nz_tab(ix_2)      ! number of z points in center x in center y
    iz_2 = i%iz_tab(ix_2) + 1  ! first table location in center x in center y
    !
    ! print out the central x aperture as a function of y
    !
    xxif_ny_tab : if ( i%ny_tab .gt. 1) then
      !
      print'( &
      & /, " kfun_print_aperture_x center x aperture c=", i8, 1x, a, &
      & /, "        x            y            z     iy", &
      & "      ix_tab   nx_tab   iz_tab   nz_tab" &
      & )', &
      i_call, trim(c_title)
      !
      print'( &
      & 1x, g12.6, 1x, g12.6, 1x, g12.6, &
      & 1x, i5, 1x, i8, 1x, i8, 1x, i8, 1x, i8 &
      & )', &
      (i%x0_tab(iy)+(i%nx_tab(iy)/2)*i%dx_tab, &
      (iy-1)*i%dy_tab+i%y0_tab, &
      i%z0_tab(i%ix_tab(iy)+i%nx_tab(iy)/2+1), &
      iy, &
      i%ix_tab(iy), i%nx_tab(iy), &
      i%iz_tab(i%ix_tab(iy)+i%nx_tab(iy)/2+1), &
      i%nz_tab(i%ix_tab(iy)+i%nx_tab(iy)/2+1), &
      iy=1, i%ny_tab)
      !
      !print*) &
      !(i%x0_tab(iy)+(i%nx_tab(iy)/2)*i%dx_tab, &
      !(iy-1)*i%dy_tab+i%y0_tab, &
      !i%z0_tab(i%ix_tab(iy)+i%nx_tab(iy)/2+1), &
      !iy, &
      !i%ix_tab(iy), i%nx_tab(iy), &
      !i%iz_tab(i%ix_tab(iy)+i%nx_tab(iy)/2+1), &
      !i%nz_tab(i%ix_tab(iy)+i%nx_tab(iy)/2+1), &
      !iy=1, i%ny_tab)
      !
      print'( &
      & /, " kfun_print_aperture_x top of x slices c=", i8, 1x, a, &
      & /, "      x                y             z", &
      & "        ix       iy" &
      & )', &
      i_call, trim(c_title)
      !
      nx_inc = max(1, min(100, nx_2/10))
      ny_inc = max(1, min(100, i%ny_tab/10))
      !
      do_ix : do ix = 1 , nx_2 , nx_inc
        !
        rx_tab = (ix - 1) * i%dx_tab + i%x0_tab(iy_2)
        !
        do_iy : do iy = 1 , i%ny_tab , ny_inc
          !
          xxif_x : if ( abs(i%x0_tab(iy)) .ge. abs(rx_tab)-1.e-3) then
            !
            jx = i%ix_tab(iy) + nint((rx_tab-i%x0_tab(iy))/i%dx_tab) + 1
            !
            print'( &
            & 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8&
            & )', &
            rx_tab, (iy-1)*i%dy_tab+i%y0_tab, i%z0_tab(jx), ix, iy
            !print*) &
            !x_tab, (iy-1)*i%dy_tab+i%y0_tab, i%z0_tab(jx), ix, iy
            !
            !write(81, '( &
            !& 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8&
            !& )', &
            !x_tab, (iy-1)*i%dy_tab+i%y0_tab, i%z0_tab(jx), ix, iy
            !
          end if xxif_x
          !
        end do do_iy
        !
      end do do_ix
      !
      print'( &
      & /, " kfun_print_aperture_x end of x slices c=",i8, a &
      & )', &
      i_call, trim(c_title)
      !
    end if xxif_ny_tab
    !
    return
    !
  end subroutine kfun_print_aperture_x
  !
  subroutine kfun_print_aperture_y ( i, c_title )
    !
    ! print the table aperture along constant y slices
    !
    type ( ktime_struct ),   pointer :: i  ! ktime  tables   structure
    character(len=*),  intent(in   ) :: c_title
    !
    integer                          :: ix
    integer                          :: iy
    integer                          :: ny_inc
    integer, save                    :: i_call = 0
    i_call = i_call + 1
    !
    !
    ! print out the central y aperture as a function of x
    !
    iy = i%ny_tab/2+1
    !
    print'( &
    & /, " kfun_print_aperture_y c=", i8, 1x, a, &
    & /, " center y aperture  iy=", i8, " y=", g12.6, &
    & /, "     x          y          z     ix", &
    & "      ix_tab      nx_tab      iz_tab      nz_tab" &
    & )', &
    i_call, trim(c_title), &
    iy, (iy-1)*i%dy_tab+i%y0_tab
    !
    print'( &
    & 1x, g12.6, 1x, g12.6, 1x, g12.6, &
    & 1x, i5, 1x, i7, 1x, i7, 1x, i8, 1x, i8 &
    & )', &
    ((ix-1)*i%dx_tab+i%x0_tab(iy), &
    (iy-1)*i%dy_tab+i%y0_tab, &
    i%z0_tab(i%ix_tab(iy)+ix), &
    ix, &
    i%ix_tab(iy), i%nx_tab(iy), &
    i%iz_tab(i%ix_tab(iy)+ix), i%nz_tab(i%ix_tab(iy)+ix), &
    ix=1, i%nx_tab(iy) )
    !
    print'( &
    & /, " kfun_print_aperture_y top of y slices c=", i8, 1x, a, &
    & /, "      x                y             z", &
    & "        ix       iy" &
    & )', &
    i_call, trim(c_title)
    !
    ny_inc = max(1, min(100, i%ny_tab/10))
    !
    do_iy : do iy = 1 , i%ny_tab , ny_inc
      !
      do_ix : do ix = i%ix_tab(iy)+1 , i%ix_tab(iy)+i%nx_tab(iy), &
                                       max(1, i%nx_tab(iy)/10)
        !
        print'( &
        &1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8&
        &)', &
        (ix-i%ix_tab(iy)-1)*i%dx_tab+i%x0_tab(iy), &
        (iy-1)*i%dy_tab+i%y0_tab, &
        i%z0_tab(ix), ix, iy
        !
        !write(82, '( &
        !& 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8&
        !& )', &
        !(ix-i%ix_tab(iy)-1)*i%dx_tab+i%x0_tab(iy), &
        !(iy-1)*i%dy_tab+i%y0_tab, &
        !i%z0_tab(ix), ix, iy
        !
      end do do_ix 
      !
    end do do_iy
    !
    print'( &
    & /, " kfun_print_aperture_y end of y slices c=", i8, 1x, a &
    & )', &
    i_call, trim(c_title)
    !
    return
    !
  end subroutine kfun_print_aperture_y
  !
  subroutine kfun_print_aperture_z ( i, c_title )
    !
    ! print the table aperture along constant z slices
    !
    type ( ktime_struct ),   pointer :: i  ! ktime  tables   structure
    character(len=*),  intent(in   ) :: c_title
    !
    integer                          :: ix, iy, iz
    integer                          :: ix_2, nx_2
    integer                          :: iy_2
    integer                          :: iz_2, nz_2
    real                             :: rx_tab, ry_tab, rz_tab
    integer, save                   :: i_call = 0
    i_call = i_call + 1
    !
    !
    iy_2 = i%ny_tab / 2 + 1    ! center y row
    nx_2 = i%nx_tab(iy_2)      ! number of x points in center y row
    ix_2 = i%ix_tab(iy_2) + nx_2 / 2 + 1 ! center x location in center y row
    nz_2 = i%nz_tab(ix_2)      ! number of z points in center x in center y
    iz_2 = i%iz_tab(ix_2) + 1  ! first table location in center x in center y
    !
    print'( &
    & /, " kfun_print_aperture_z top of z slices c=", i8, 1x, a, &
    & /, "      x                y             z", &
    & "        ix       iy       iz" &
    & )', &
    i_call, trim(c_title)
    !
    do_iz_1 : do iz = 1 , i%nz_tab(ix_2) , max(1, i%nz_tab(ix_2)/10)
      !
      rz_tab = (iz - 1) * i%g%dz_tab + i%z0_tab(ix_2)
      !
      do_iy_1 : do iy = 1 , i%ny_tab
        !
        ry_tab = (iy - 1) * i%dy_tab + i%y0_tab
        !
        do_ix_1 : do ix = i%ix_tab(iy)+1 , i%ix_tab(iy)+i%nx_tab(iy)/2+1
          !
          rx_tab = (ix - i%ix_tab(iy) - 1) * i%dx_tab + i%x0_tab(iy)
          !
          if ( i%z0_tab(ix) .le. rz_tab+1.e-2) then
            !
            print'( &
            & 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, 1x, i8&
            & )', &
            rx_tab, ry_tab, i%z0_tab(ix), ix, iy, iz
            !
            !write(83, '( &
            !& 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, 1x, i8&
            !& )', &
            !x_tab, ry_tab, i%z0_tab(ix), ix, iy, iz
            !
            go to 1
            !
          end if    ! if ( i%z0_tab(ix) .le. rz_tab+1.e-2) then
          !
        end do do_ix_1
        !
  1     continue
        !
      end do do_iy_1
      !
      do_iy_2 : do iy = i%ny_tab , 1 , -1
        !
        ry_tab = (iy - 1) * i%dy_tab + i%y0_tab
        !
        do_ix_2 : &
        do ix = i%ix_tab(iy)+i%nx_tab(iy) , i%ix_tab(iy)+i%nx_tab(iy)/2+1 , -1
            !
          rx_tab = (ix - i%ix_tab(iy) - 1) * i%dx_tab + i%x0_tab(iy)
          !
          if ( i%z0_tab(ix) .le. rz_tab+1.e-2) then
            !
            print'( &
            & 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, 1x, i8&
            & )', &
            rx_tab, ry_tab, i%z0_tab(ix), ix, iy, iz
            !
            !write(83, '( &
            !& 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, 1x, i8&
            !& )', &
            !x_tab, ry_tab, i%z0_tab(ix), ix, iy, iz
            !
            go to 2
            !
          end if    ! if ( i%z0_tab(ix) .le. rz_tab-1.e-2) then
          !
        end do do_ix_2
        !
  2     continue
        !
      end do do_iy_2
      !
      do_iy_3 : do iy = 1 , i%ny_tab
        !
        ry_tab = (iy - 1) * i%dy_tab + i%y0_tab
        !
        do_ix_3 : do ix = i%ix_tab(iy)+1 , i%ix_tab(iy)+i%nx_tab(iy)/2+1
          !
          rx_tab = (ix - i%ix_tab(iy) - 1) * i%dx_tab + i%x0_tab(iy)
          !
          if ( i%z0_tab(ix) .le. rz_tab+1.e-2) then
            print'( &
            & 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, 1x, i8&
            & )', &
            rx_tab, ry_tab, i%z0_tab(ix), ix, iy, iz
            !
            !write(83, '( &
            !& 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, 1x, i8&
            !& )', &
            !x_tab, ry_tab, i%z0_tab(ix), ix, iy, iz
            !
            go to 3
            !
          end if    ! if ( i%z0_tab(ix) .le. rz_tab+1.e-2) then
          !
        end do do_ix_3
        !
      end do do_iy_3
      !
   3  continue
      !
    end do do_iz_1
    !
    print'( &
    & /, " kfun_print_aperture_z end of z slices c=", i8, 1x, a &
    & )', &
    i_call, trim(c_title)
    !
    return
    !
  end subroutine kfun_print_aperture_z
  !
  subroutine kfun_print_aperture_t ( i, c_title, rt_tab )
    !
    ! print the table aperture along constant z slices
    !
    type ( ktime_struct ),   pointer :: i  ! ktime  tables   structure
    character(len=*),  intent(in   ) :: c_title
    real,               intent(in   ):: rt_tab(:)
    !
    integer                          :: j_xy_tab
    integer                          :: j_xyz_tab
    integer                          :: jx_tab
    integer                          :: jy_tab
    integer                          :: jz_tab
    integer                          :: jx_tab_2
    integer                          :: jy_tab_2
    integer                          :: jz_tab_2
    integer                          :: nx_tab_2
    integer                          :: ny_tab_2
    integer                          :: nz_tab_2
    real                             :: rx_tab
    real                             :: ry_tab
    real                             :: rz_tab
    integer, save                   :: i_call = 0
    i_call = i_call + 1
    !
    ny_tab_2 = i%ny_tab / 2 + 1        ! center y row
    jy_tab_2 = ny_tab_2                ! center y row
    nx_tab_2 = i%nx_tab(jy_tab_2)      ! num of x points in cen y row
    jx_tab_2 = i%ix_tab(jy_tab_2) + nx_tab_2 / 2 + 1 ! cen x loc in cen y row
    nz_tab_2 = i%nz_tab(jx_tab_2)      ! num of z points in cen x in cen y
    jz_tab_2 = i%iz_tab(jx_tab_2) + 1  ! first loc in cen x in cen y
    !
    print'( &
    & /, " kfun_print_aperture_t top of z slices c=", i8, 1x, a, &
    & /, " ny_tab=",i8," nx_tab=",i8," nz_tab=",i8, &
    & /, " jy_tab=",i8," jx_tab=",i8," jz_tab=",i8, &
    & /, " x            y            z            t", &
    & "                  jx       jy       jz       j_xyz_tab" &
    & )', &
    i_call, trim(c_title), &
    i%ny_tab, nx_tab_2, nz_tab_2, &
    jy_tab_2, jx_tab_2, jz_tab_2
    !
    j_xy_tab = jx_tab_2
    !
    do_jz_tab_1 : do jz_tab = 1 , i%nz_tab(j_xy_tab)
      !
      rz_tab = ( jz_tab -  1) * i%g%dz_tab + i%z0_tab(jx_tab_2)
      !
      jy_tab = jy_tab_2
        !
        ry_tab = ( jy_tab - 1 ) * i%dy_tab + i%y0_tab
        !
        jx_tab = nx_tab_2 / 2 + 1
          !
          rx_tab = ( jx_tab - 1 ) * i%dx_tab + i%x0_tab(jy_tab)
          !
            j_xyz_tab = i%iz_tab(j_xy_tab) + jz_tab
            !
            print'( &
            & 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, g12.6, &
            & 1x, i8, 1x, i8, 1x, i8, 1x, i8, 1x, i3, &
            & "time_zzz" &
            & )', &
            rx_tab, ry_tab, rz_tab, rt_tab(j_xyz_tab), &
            jx_tab, jy_tab, jz_tab, j_xyz_tab, i_call
            !
        !
    end do do_jz_tab_1 
    !
    print'( &
    & " kfun_print_aperture_t end of z slices c=", i8, 1x, a &
    & )', &
    i_call, trim(c_title)
    !
    return
    !
  end subroutine kfun_print_aperture_t
  !
  subroutine kfun_print_sample ( i, c_title, tim_tab, j_xyz_tab )
    !
    ! print a single time sampe
    !
    type ( ktime_struct ),   pointer :: i  ! ktime  tables   structure
    character(len=*),  intent(in   ) :: c_title
    real,              intent(in   ) :: tim_tab(:) 
    integer,           intent(in   ) :: j_xyz_tab
    !
    integer, save                   :: i_call = 0
    i_call = i_call + 1
    !
    ! Local variables
    !
    if ( j_xyz_tab .le. size(tim_tab,1) ) &
    print'(" kfun_print_sample j=",i8," t=",g12.6," c=",a)', &
    i_call, j_xyz_tab, tim_tab(j_xyz_tab), trim(c_title)
    !
    return
    !
  end subroutine kfun_print_sample
  !
  subroutine kfun_job_name ( job_name )
    !
    character(len=*),  intent(inout) :: job_name
    character(len=16)                :: c_time
    !
    ! for cps get the job name from pc_get_jdata
    !
    !xxif_cps_code : if ( kfun_cps_code ) then RSD
    xxif_cps_code : if ( kfun_is_cps() ) then
      !
      call pc_get_jdata ( 'jobname', job_name )
      !
    else xxif_cps_code 
      !
      ! for seisspace get the job name from string_time
      !
      call string_time ( c_time )
      !
      job_name = 'kfun_' // trim ( c_time )
      !
    end if xxif_cps_code 
    !
    return
    !
  end subroutine kfun_job_name 
  !
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!
end module kfun_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
                
