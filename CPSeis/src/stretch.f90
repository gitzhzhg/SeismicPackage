!<CPS_v1 type="PRIMITIVE"/>

!<-- This documentation header was last revised by CI Burch on 2000-01-28. />

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
!                         C P S   P R I M I T I V E
!
! Name       : STRETCH    (Stolt Trace Stretch)  [includes former SSTRANS]
! Category   : migrations
! Written    : 1994-12-01   by: Douglas Hanson
! Revised    : 2006-12-04   by: Goodger
! Maturity   : production
! Purpose    : Apply Stolt trace stretch for F-K and phase-shift migrations.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
! 
! STRETCH assumes the input traces represent reflections from an earth model
! whose velocity structure is given by the stacking velocity field in
! PATHNAME_VEL.  STRETCH performs a Stolt stretch on the input traces so that
! they are consistent with an earth model whose velocity structure is given by
! the reference stacking velocity function in PATHNAME_REF.
!
! STRETCH will stretch traces so they are consistent with a single reference
! velocity function (for phase-shift migration) or so they are consistent with
! a constant velocity (for Stolt F-K migration).
!
! IF OPT_DIR = FORWARD STRETCH applies a forward stretche from the lateraly 
! and veritcaly varying local velocity field, VEL_LOCAL to the lateraly 
! constant, verticaly varying reference velocity field VEL_REFERENCE.
! 
! IF OPT_DIR = INVERSE STRETCH applies an inverse stretch from the lateraly 
! constant, verticaly varying reference velocity field VEL_REFERENCE, to the 
! 
! If OPT_VEL_SCALE = YES,
! the lateraly varying local velocity field STRETCH actualy uses can
! VEL_LOCAL_USE = ( 1. - VEL_SCALE ) * VEL_REFERENCE + VEL_SCALE * VEL_LOCAL
! using a value of VEL_SCALE = 1. means STRETCH use the true local velocity 
! field and hence applies the full stretch correction.
! using a value of VEL_SCALE = 0. means STRETCH uses the reference velocity
! and hence their is no effective stretch applied.
! 
! If OPT_COMMON_ANGLE=NO XTRECTH will apply a stretch which trys to match the
! shape of a hyperbola as offset approaches zero.
!
! If OPT_COMMON_ANGLE=YES XTRECTH will apply a stretch which trys to match the
! travel time of a ray with ray parameter = header word (HDR_OFFSET) * 1.e-6
! within a v(z) or v(t) media.
!
! If OPT_ZERO_ANGLE = YES, STRETCH will set the ray parameter to zero.
! This is appropriate for the inverse stretch after migration.
! 
! If OPT_ZERO_ANGLE = NO, STRETCH will use the ray parameter 
! from header word (HDR_OFFSET) * 1.e-6
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Time migration requires a smooth migration velocity field.  If your migration
! velocity field is not naturally smooth and if you have not already smoothed 
! it, then you should probably use the OPT_SMOOTH = FIT option and smooth the 
! velocity field by using a polynomial fit.
!
! If the file of PATHNAME_VEL is specified, it requires either the parameter of
! PATHNAME_REF or VEL_CONST to calculate the stretch.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED          
!
! 
! Name     Description                           Action taken
! ----     -----------                           ------------
! IPN      process number                        used (must not be changed)
! MAXTR    max number of traces input/output     used but not changed
! NDPT     number of sample values in trace      used and may be changed
! TSTRT    starting time on trace                used and may be changed
! DT       trace sample interval                 used and may be changed

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
! 25      LAV                        recalculated
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author         Description
!     ----       ------         -----------
! 19. 2007-04-13 Goodger        Moved from processes/transforms to
!                               primitives/migrations.
! 19. 2006-12-04 D. Glover      Added NULLIFY statements for Intel compiler.
! 18. 2006-01-10 B. Menger      Removed Unused Variables.
! 17. 2005-09-13 R.S.Day        Fix bug in stretch_compute_max_time that
!                               was uncovered by behavior with intel compiler.
! 16. 2004-09-07 Douglas Hanson Add common angle stretch.
! 15. 2001-12-10 Chiu           Change local variables to double precision and
!                               check for mapping limit in routine 
!                               stretch_map_apply.
! 14. 2001-11-08 Chiu           Replace array(*) by array(:).
! 13. 2001-02-14 Chiu           Change wrapped_up to skip_wrapup.
! 12. 2000-11-29 Chiu           Fix problems of rw_fac and sq_fac by not 
!                               resettingthem to 1.
! 11. 2000-09-05 Chiu           Initialize rw_fac and sq_fac to 1.
! 10. 2000-06-14 Chiu           Reset the coarse grid back to 10*dt and fix 
!                               the timing shifts by polynomial smoothing the 
!                               velocities instead of the stretch times.
! 9.  2000-05-22 Chiu           Set the interpolation to every second sample.
! 8.  2000-05-05 Chiu           Match stretch velocity with Cray's version.
! 7.  2000-04-25 Chiu           Fix Gui problems.
! 6.  2000-04-06 Chiu           Add Gui.
! 5.  2000-04-04 Chiu           Convert into new CPS.
! 4.  1999-02-08 Hanson         Add internal setup.
!                               Begin using the fortran90 compiler.
! 3.  1998-07-23 Goodger        Moved to conlib.
! 2.  1998-04-28 Hanson         T3E port.  Read velocity on pe 0 and broadcast 
!                               to the rest.  Print info on pe only.
! 1.  1994-12-01 Hanson         Original version
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
! STRETCH should be updated with the autotasking compiler 
! (update_autotask_fixed)
!
! 
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS            
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR .ge. 1              means to process the input traces.
!    NTR .eq. NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR .ge. 1              if this process is outputting traces.
!    NTR .eq. NO_MORE_TRACES if there are no more traces to output.
!    NTR .eq. FATAL_ERROR    if this process has a fatal error.
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
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS STRETCH Process/NC=80>
!
!    Stolt Trace Stretch (includes former SSTRANS) Process
! Apply Stolt trace stretch for F-K and phase-shift migrations.
!
! PATHNAME_VEL=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! PATHNAME_REF=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! VELNAME_REF=~~`SSSSSSSSSSSSSSSSSSSS
!
! VEL_CONST=`FFFFFFFFFFF OPT_DIR=`CCCCCC  OPT_SMOOTH=`CCCCC  DEGREE_SMOOTH=`I
! OPT_CALC_CV~~=`CCCCCCC OPT_COMMON_ANGLE=`CC OPT_ZERO_ANGLE=`CC
! OPT_DOPPLER_MUTE=`CC   OPT_VEL_SCALE=`CC   VEL_SCALE=`FFFFFFFF 
!
! TIM_TOT=`IIIIIIII TIM_INIT=`FFFFFFFFFFF TIM_LAST=`FFFFFFFFFFF TIM_INC=`FFFFFFFFFFF
! RAY_TOT=`IIIIIIII RAY_INIT=`FFFFFFFFFFF RAY_LAST=`FFFFFFFFFFF RAY_INC=`FFFFFFFFFFF
!<PARMS PATHNAME_VEL[/ML=128/XST]>
!<PARMS PATHNAME_REF[/ML=128/XST]>
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="PATHNAME_VEL">
!<Tip> Pathname for stacking velocity file. </Tip>
! Default = NONE
! Allowed = char
! Pathname for velocity file containing stacking velocity field to be used for 
! migration. 
! If PATHNAME_VEL = NONE, stretch is not applied in Stolt migration. 
!</Help>
!
!<Help KEYWORD="PATHNAME_REF">
!<Tip> Pathname for velocity file specifying reference velocity function. </Tip>
! Default = NONE
! Allowed = char
! If PATHNAME_REF = NONE, use VEL_CONST as a constant velocity Stolt migration.
!</Help>
!
!<Help KEYWORD="VELNAME_REF">
!<Tip> Name of the desired velocity function within PATHNAME_REF. </Tip>
! Default = FIRST
! Allowed = char
! If VELNAME_REF = FIRST, then the first velocity function in the velocity file
! PATHNAME_REF will be used.
!</Help>
!
!<Help KEYWORD="VEL_CONST">
!<Tip> Constant velocity for Stolt F-K migration. </Tip>
! Default = 1500.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="OPT_DIR">
!<Tip> Whether to perform a forward or inverse stretch. </Tip>
! Default = FORWARD
! Allowed = FORWARD (Stretch from input velocity to reference velocity field.)
! Allowed = INVERSE (Unstretch from reference velocity to input velocity field.)
!</Help>
!
!<Help KEYWORD="OPT_SMOOTH">
!<Tip> Smooth the velocity field by interpolation or polynomial fitting. </Tip>
! Default = FIT
! Allowed = FIT   
! Allowed = INTERP 
! If OPT_SMOOTH = FIT, then smooth the migration velocity field by using a 
! least-squares fit to a polynomial of degree specified by the DEGREE_SMOOTH 
! parameter.
!
! If OPT_SMOOTH = INTERP, then do linear interpolation between velocity 
! function locations (and use constant extrapolation at the ends).  
! WARNING:  this option may cause chevron shaped artifacts unless the velocity 
! field is naturally very smooth or you have previously smoothed it.
!</Help>
!
!<Help KEYWORD="DEGREE_SMOOTH">
!<Tip> Degree of polynomial to use when OPT_SMOOTH = FIT. </Tip>
! Default = 1
! Allowed = int .ge. 0  
! Normally DEGREE_SMOOTH should not exceed 2 or 3.  
! DEGREE_SMOOTH = 0 allows no lateral
! variation.
!</Help>
!
!<Help KEYWORD="TIM_TOT">
!<Tip> Number of time samples of the forward stretched trace. </Tip>
! Default = NDPT
! Allowed = int > 0  
!</Help>
!
!<Help KEYWORD="TIM_INIT">
!<Tip> Time of the first sample of the forward stretched trace. </Tip>
! Default = TSTRT
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIM_LAST">
!<Tip> Time of the last sample of the forward stretched trace. </Tip>
! Default = TSTRT+(NDPT-1)*DT
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIM_INC">
!<Tip> Sample interval of the forward stretched trace. </Tip>
! Default = 0.0
! Allowed = real > = 0.0
! If TIM_INC = 0.0, then TIM_TOT and TIM_INC are computed internally to
! ensure that sampling is fine enough to avoid aliasing.
!</Help>
!
!<Help KEYWORD="OPT_CALC_CV">
!<Tip> Method to use for calculating constant velocity. </Tip>
! Default = MIN_STR
! Allowed = MIN_STR  (Use minimum stretching criteria.)
! Allowed = USER     (Constant velocity is input by the user.)
! Method to use for calculating constant velocity for Stolt F-K migration.  
! Normally the default is used.
!</Help>
!
!<Help KEYWORD="OPT_COMMON_ANGLE">
!<Tip> Use common angle imaging. </Tip>
! Default = NO
! Allowed = NO  STRETCH will not use a comon angle imaging algorithm.
! Allowed = YES STRETCH will     use a comon angle imaging algorithm.
!
! If OPT_COMMON_ANGLE=NO XTRECTH will apply a stretch which trys to match the
! shape of a hyperbola as offset approaches zero.
!
! If OPT_COMMON_ANGLE=YES XTRECTH will apply a stretch which trys to match the
! travel time of a ray with ray parameter = header word (HDR_OFFSET) * 1.e-6
! within a v(z) or v(t) media.
!
!</Help>
!
!<Help KEYWORD="OPT_ZERO_ANGLE">
!<Tip> Set the ray parameter to zero for the stretch. </Tip>
! Default = NO  STRETCH will use the ray parameter 
! from header word (HDR_OFFSET) * 1.e-6
! Allowed = YES STRETCH will set the ray parameter to zero.
! This is appropriate for the inverse stretch after migration.
! 
!</Help>
!
!<Help KEYWORD="RAY_TOT">
!<Tip> Number of ray parameter groups. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="RAY_INIT">
!<Tip> Minimum ray parameter in the output image.   </Tip>
! Default = 0
! Allowed = real
! Units are defined by velocity and are in microseconds per distance unit.
! If an input traces ray parameter is outside the range defined by 
! RAY_TOT,RAY_INIT,RAY_LAST,RAY_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="RAY_LAST">
!<Tip> Maximum ray parameter in the output image.   </Tip>
! Default = 0
! Allowed = real
! Units are defined by velocity and are in microseconds per distance unit.
! If an input traces ray parameter is outside the range defined by 
! RAY_TOT,RAY_INIT,RAY_LAST,RAY_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="RAY_INC">
!<Tip> Output image ray parameter spacing.   </Tip>
! Default = 2
! Allowed = real
! Units are defined by velocity and are in microseconds per distance unit.
! If an input traces ray parameter is outside the range defined by 
! RAY_TOT,RAY_INIT,RAY_LAST,RAY_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="OPT_DOPPLER_MUTE">
!<Tip> Scale the input velocity field. </Tip>
! Default = YES
! Allowed = YES STRETCH will     apply a doppler mute.
! Allowed = NO  STRETCH will not apply a doppler mute.
! 
! If OPT_DOPPLER_MUTE = YES,
! Allowed = YES STRETCH will mute the end of the output trace from the
! last output sample at which the input sample was changing.  
!</Help>
!
!<Help KEYWORD="OPT_VEL_SCALE">
!<Tip> Scale the input velocity field. </Tip>
! Default = NO
! Allowed = YES STRETCH will     scale the input velocity field. 
! Allowed = NO  STRETCH will not scale the input velocity field. 
! 
! IF OPT_DIR = FORWARD STRETCH applies a forward stretche from the lateraly 
! and veritcaly varying local velocity field, VEL_LOCAL to the lateraly 
! constant, verticaly varying reference velocity field VEL_REFERENCE.
! 
! IF OPT_DIR = INVERSE STRETCH applies an inverse stretch from the lateraly 
! constant, verticaly varying reference velocity field VEL_REFERENCE, to the 
! 
! If OPT_VEL_SCALE= = YES,
! the lateraly varying local velocity field STRETCH actualy uses can
! VEL_LOCAL_USE = ( 1. - VEL_SCALE ) * VEL_REFERENCE + VEL_SCALE * VEL_LOCAL
! using a value of VEL_SCALE = 1. means STRETCH use the true local velocity 
! field and hence applies the full stretch correction.
! using a value of VEL_SCALE = 0. means STRETCH uses the reference velocity
! and hence their is no effective stretch applied.
!</Help>
!
!<Help KEYWORD="VEL_SCALE">
!<Tip> Velocity scale factor. </Tip>
! Default = 1.
! Allowed = real > 0.0
! 
! IF OPT_DIR = FORWARD STRETCH applies a forward stretche from the lateraly 
! and veritcaly varying local velocity field, VEL_LOCAL to the lateraly 
! constant, verticaly varying reference velocity field VEL_REFERENCE.
! 
! IF OPT_DIR = INVERSE STRETCH applies an inverse stretch from the lateraly 
! constant, verticaly varying reference velocity field VEL_REFERENCE, to the 
! 
! If OPT_VEL_SCALE= = YES,
! the lateraly varying local velocity field STRETCH actualy uses can
! VEL_LOCAL_USE = ( 1. - VEL_SCALE ) * VEL_REFERENCE + VEL_SCALE * VEL_LOCAL
! using a value of VEL_SCALE = 1. means STRETCH use the true local velocity 
! field and hence applies the full stretch correction.
! using a value of VEL_SCALE = 0. means STRETCH uses the reference velocity
! and hence their is no effective stretch applied.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
! NOTES FOR CONVERSION PROGRAMMER
!
!-------------------------------------------------------------------------------
!
module stretch_module
  !
  use cpucount_module
  use gausselim_module
  use getlun_module
  use gridcheck_module
  use headsave_module
  use interpolate_module
  use lav_module
  use matfun_module
  use memfun_module
  use named_constants_module
  use pathcheck_module
  use pc_module
  use pcpsx_module
  use rcsum_module
  use string_module
  use timeglob_module
  use velgrid_module
  use velio_module
  use velutil_module
  !
  implicit none
  !
  private
  !
  public :: stretch_create     ! uses the parameter cache.
  public :: stretch_initialize
  public :: stretch_update     ! uses the parameter cache.
  public :: stretch_delete
  public :: stretch_get_fkpar
  !
  public :: stretch            ! main execution (trace processing) routine.
  public :: stretch_wrapup
  !
  character(len=100),public,save :: stretch_ident = &
  '$Id: stretch.f90,v 1.19 2007/04/13 20:07:39 Goodger prod sps $'
  !
  integer, parameter                        :: len_132 = 132
  real,    parameter                        :: stretch_ray_scale = 1.e-6
  !
  type,public :: stretch_struct
    !
    private
    logical                                 :: skip_wrapup      ! wrapup flag.
    logical                                 :: gathered         ! gathered flag
    !
    character(len=FILENAME_LENGTH)          :: pathname_vel
    character(len=FILENAME_LENGTH)          :: pathname_ref
    character(len=8)                        :: velname_ref
    real                                    :: vel_const
    character(len=8)                        :: opt_dir
    character(len=8)                        :: opt_smooth
    integer                                 :: degree_smooth
    integer                                 :: tim_tot
    real                                    :: tim_init
    real                                    :: tim_last
    real                                    :: tim_inc
    integer                                 :: nt_out
    real                                    :: t0_out
    real                                    :: t1_out
    real                                    :: dt_out
    integer                                 :: nt_inp
    real                                    :: t0_inp
    real                                    :: t1_inp
    real                                    :: dt_inp
    character (len=8)                       :: opt_calc_cv
    integer                                 :: nh_inp
    !
    logical                                 :: opt_doppler_mute
    logical                                 :: opt_vel_scale
    real                                    :: vel_scale
    !
    integer                                 :: ray_tot
    real                                    :: ray_init
    real                                    :: ray_last
    real                                    :: ray_inc
    !
    integer                                 :: np_ray
    real                                    :: p0_ray
    real                                    :: p1_ray
    real                                    :: dp_ray
    !
    logical                                 :: opt_common_angle
    logical                                 :: opt_zero_angle
    !
    real,                           pointer :: rx_pol(:)
    real,                           pointer :: ry_pol(:)
    real,                           pointer :: rx_map(:)
    real,                           pointer :: ry_map(:)
    integer,                        pointer :: lt_map(:,:)
    real,                           pointer :: rt_map(:,:,:)
    real,                           pointer :: ra_map(:,:,:)
    real,                           pointer :: rv_map(:,:,:)
    !
    integer,                        pointer :: i1_map(:)
    integer,                        pointer :: i2_map(:)
    !
    integer                                 :: mv_pol
    integer                                 :: nv_pol
    integer                                 :: nx_map
    integer                                 :: ny_map
    integer                                 :: i_trin
    integer                                 :: n_work
    integer                                 :: degree_smooth2
    integer                                 :: nv_map
    integer                                 :: hx_map
    integer                                 :: hy_map
    integer                                 :: nv_ref
    integer                                 :: hx_ref
    integer                                 :: hy_ref
    real                                    :: rx_ref
    real                                    :: ry_ref
    !
    logical                                 :: use_pathname_ref
    logical                                 :: use_pathname_vel
    !
    integer                                 :: nt_map
    real                                    :: t0_map
    real                                    :: t1_map
    real                                    :: dt_map
    real                                    :: x0_map
    real                                    :: x1_map
    real                                    :: dx_map
    real                                    :: y0_map
    real                                    :: y1_map
    real                                    :: dy_map
    !
    real                                    :: rw_fac
    real                                    :: sq_fac
    real                                    :: rv_stretch
    !
    double precision                        :: x0_inp
    double precision                        :: y0_inp
    double precision                        :: top_mute
    double precision                        :: bot_mute
    !
    integer                                 :: ipn
    !             
    integer                                 :: ip_inp
    integer                                 :: ip_grp
    real                                    :: rp_inp
    real                                    :: rp_grp
    !
    integer                                 :: nt_lay
    real,                           pointer :: rt_lay(:)
    real,                           pointer :: tz_lay(:)
    real,                           pointer :: rz_lay(:)
    real,                           pointer :: rv_lay(:)
    real,                           pointer :: rv_rms(:)
    real,                           pointer :: rv_ave(:)
    real,                           pointer :: rv_int(:)
    real,                           pointer :: rv_ref(:)
    character(len=4)                        :: ref_type
    character(len=4)                        :: vel_type
    character(len=8)                        :: vel_name
    !
    type ( velio_struct ),          pointer :: v
    type ( cpucount_struct ),       pointer :: c  ! cpucount structure
    type ( headsave_struct ),       pointer :: h  ! headsave structure
    !
    integer                                 :: c_stretch_total ! total cpu time
    integer                                 :: c_stretch_setup ! setup cpu time
    integer                                 :: c_stretch_comp  ! comp  cpu time
    integer                                 :: c_stretch_terp  ! terp  cpu time
    integer                                 :: c_stretch_apply ! apply cpu time
    integer                                 :: c_stretch_map_p ! map_p cpu time
    integer                                 :: iv_map
    !
  end type stretch_struct
  !
  type ( stretch_struct ),pointer,save :: object      ! needed for traps.
  !
  logical,          save, public       :: stretch_fktmig = .false.
  !
  integer,          parameter          :: n_opt_dir = 2
  character(len=8), save               :: c_opt_dir(n_opt_dir) &
  = (/ 'FORWARD ', 'INVERSE ' /)
  !
  integer,          parameter          :: n_opt_smooth = 2
  character(len=8),save                :: c_opt_smooth(n_opt_smooth) &
  = (/ 'FIT     ', 'INTERP  ' /)
  !
  integer,          parameter          :: n_opt_calc_cv = 2
  character(len=8),save                :: c_opt_calc_cv(n_opt_calc_cv) &
  = (/ 'MIN_STR ', 'USER    ' /)
  !
  integer,          parameter          :: n_yes_no = 2
  character(len=3), save               :: c_yes_no ( n_yes_no ) &
  = (/'YES', 'NO '/) 
  !
  contains
  !
  subroutine stretch_create ( o )
    !
    type ( stretch_struct ),        pointer :: o ! stretch structure
    !
    allocate ( o )
    !
    nullify ( o%v      )
    nullify (o%rx_pol) ! jpa
    nullify (o%ry_pol) ! jpa
    nullify (o%rx_map) ! jpa
    nullify (o%ry_map) ! jpa
    nullify (o%lt_map) ! jpa
    nullify (o%rt_map) ! jpa
    nullify (o%ra_map) ! jpa
    nullify (o%rv_map) ! jpa
    nullify (o%i1_map) ! jpa
    nullify (o%i2_map) ! jpa
    nullify (o%rt_lay) ! jpa
    nullify (o%tz_lay) ! jpa
    nullify (o%rz_lay) ! jpa
    nullify (o%rv_lay) ! jpa
    nullify (o%rv_rms) ! jpa
    nullify (o%rv_ave) ! jpa
    nullify (o%rv_int) ! jpa
    nullify (o%rv_ref) ! jpa
    nullify (o%c) ! jpa
    nullify (o%h) ! jpa
    !
    call stretch_mem_nul ( o )
    !
    call stretch_initialize ( o )
    !
    return
    !
  end subroutine stretch_create
  !
  subroutine stretch_delete ( o )
    !
    type ( stretch_struct ),        pointer :: o ! stretch structure
    !
    call stretch_wrapup ( o )
    !
    if (  associated ( o%h ) )  &
    call headsave_delete ( o%h )
    !
    if (  associated ( o%c ) )  &
    call cpucount_delete  ( o%c )
    !
    call stretch_velocity_close ( o%v )
    !
    ! Make sure all pointers in your parameter structure are deallocated
    !
    call stretch_mem_del ( o )
    !
    deallocate ( o )
    !
    return
    !
  end subroutine stretch_delete
  !
  subroutine stretch_initialize_0 ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    call memfun_init ( o%skip_wrapup )
    call memfun_init ( o%pathname_vel )
    call memfun_init ( o%pathname_ref )
    call memfun_init ( o%velname_ref )
    call memfun_init ( o%vel_const )
    call memfun_init ( o%opt_doppler_mute )
    call memfun_init ( o%opt_vel_scale )
    call memfun_init ( o%vel_scale )
    call memfun_init ( o%opt_dir )
    call memfun_init ( o%opt_smooth )
    call memfun_init ( o%degree_smooth )
    call memfun_init ( o%tim_tot )
    call memfun_init ( o%tim_init )
    call memfun_init ( o%tim_last )
    call memfun_init ( o%tim_inc )
    call memfun_init ( o%ray_tot )
    call memfun_init ( o%ray_init )
    call memfun_init ( o%ray_last )
    call memfun_init ( o%ray_inc )
    call memfun_init ( o%opt_common_angle )
    call memfun_init ( o%opt_zero_angle )
    call memfun_init ( o%opt_calc_cv )
    call memfun_init ( o%ref_type )
    call memfun_init ( o%nh_inp )
    call memfun_init ( o%nt_inp )
    call memfun_init ( o%dt_inp )
    call memfun_init ( o%t0_inp )
    call memfun_init ( o%t1_inp )
    call memfun_init ( o%nt_out )
    call memfun_init ( o%t0_out )
    call memfun_init ( o%t1_out )
    call memfun_init ( o%dt_out )
    call memfun_init ( o%nt_lay )
    call memfun_init ( o%nv_map )
    call memfun_init ( o%mv_pol )
    call memfun_init ( o%nv_pol )
    call memfun_init ( o%nt_map )
    call memfun_init ( o%nx_map )
    call memfun_init ( o%ny_map )
    call memfun_init ( o%i_trin )
    call memfun_init ( o%n_work )
    call memfun_init ( o%degree_smooth2 )
    call memfun_init ( o%hx_map )
    call memfun_init ( o%hy_map )
    call memfun_init ( o%nv_ref )
    call memfun_init ( o%hx_ref )
    call memfun_init ( o%hy_ref )
    call memfun_init ( o%rx_ref )
    call memfun_init ( o%ry_ref )
    call memfun_init ( o%use_pathname_vel )
    call memfun_init ( o%use_pathname_ref )
    call memfun_init ( o%t1_map )
    call memfun_init ( o%t0_map )
    call memfun_init ( o%dt_map )
    call memfun_init ( o%x0_map )
    call memfun_init ( o%x1_map )
    call memfun_init ( o%dx_map )
    call memfun_init ( o%y0_map )
    call memfun_init ( o%y1_map )
    call memfun_init ( o%dy_map )
    call memfun_init ( o%rw_fac )
    call memfun_init ( o%sq_fac )
    call memfun_init ( o%rv_stretch )
    !call memfun_init ( o%rt_max_out )
    call memfun_init ( o%rp_inp )
    call memfun_init ( o%rp_grp )
    call memfun_init ( o%ip_inp )
    call memfun_init ( o%ip_grp )
    call memfun_init ( o%c_stretch_total )
    call memfun_init ( o%c_stretch_setup )
    call memfun_init ( o%c_stretch_comp )
    call memfun_init ( o%c_stretch_terp )
    call memfun_init ( o%c_stretch_apply )
    call memfun_init ( o%c_stretch_map_p )
    call memfun_init ( o%ipn )
    call memfun_init ( o%rt_lay )
    call memfun_init ( o%tz_lay )
    call memfun_init ( o%rz_lay )
    call memfun_init ( o%rv_lay )
    call memfun_init ( o%rv_rms )
    call memfun_init ( o%rv_ave )
    call memfun_init ( o%rv_int )
    call memfun_init ( o%rv_ref )
    call memfun_init ( o%rx_pol )
    call memfun_init ( o%ry_pol )
    call memfun_init ( o%rx_map )
    call memfun_init ( o%ry_map )
    call memfun_init ( o%lt_map )
    call memfun_init ( o%rt_map )
    call memfun_init ( o%ra_map )
    call memfun_init ( o%rv_map )
    call memfun_init ( o%i1_map )
    call memfun_init ( o%i2_map )
    call memfun_init ( o%iv_map )
    !
    return
    !
  end subroutine stretch_initialize_0 
  !
  subroutine stretch_initialize ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    ! Initialize ALL NON-POINTER VARIABLES in the parameter structure
    !
    call stretch_initialize_0 ( o )
    !
    o%ipn = pc_get_ipn()
    call timeglob_get ( o%nt_inp, o%t0_inp, o%dt_inp )
    o%t1_inp = ( o%nt_inp - 1 ) * o%dt_inp + o%t0_inp
    !
    o%pathname_vel  = PATHCHECK_EMPTY
    o%pathname_ref  = PATHCHECK_EMPTY
    o%velname_ref   = 'FIRST'
    o%vel_const     = 1500.0
    o%opt_dir       = 'FORWARD'
    o%opt_smooth    = 'FIT'
    o%degree_smooth = 1
    o%tim_tot       = o%nt_inp
    o%tim_init      = o%t0_inp
    o%tim_last      = o%t1_inp
    o%tim_inc       = o%dt_inp
    o%nt_out        = o%tim_tot
    o%t0_out        = o%tim_init
    o%t1_out        = o%tim_last
    o%dt_out        = o%tim_inc
    o%opt_calc_cv       = 'MIN_STR'
    !
    o%ray_tot   = 1            ! number of ray parameter or group bin center
    o%ray_init  = 0            ! min ray parameter or group bin center
    o%ray_last  = 0            ! max ray parameter or group bin spacing
    o%ray_inc   = 2            ! ray parameter or group bin spacing
    !
    o%vel_scale        = 1.0
    o%opt_vel_scale    = .false.
    o%opt_doppler_mute = .true.
    o%opt_common_angle = .false. ! flag for common angle imaging
    o%opt_zero_angle   = .false.
    !
    o%gathered       = .false. ! hope it gets set right later
    o%nh_inp         = inil    ! must test later to make sure has been set
    o%nt_inp         = inil    ! must test later to make sure has been set
    o%t0_inp         = fnil    ! must test later to make sure has been set
    o%dt_inp         = fnil    ! must test later to make sure has been set
    !
    call stretch_update ( o )
    !
    return
    !
  end subroutine stretch_initialize
  !
  subroutine stretch_update ( o )
    !
    type ( stretch_struct ),  intent(inout), target :: o ! stretch structure
    !
    ! local variables
    !
    integer,                           save :: i_call = 0
    i_call = i_call + 1
    !
    object => o               ! needed for traps.
    !
    o%skip_wrapup = .true.    ! needed for the wrapup routine.
    !
    !print'(" qqq stretch version x6.01.a is good ")'
    !print'(" qqq stretch version x6.01.a ")'
    !
    call stretch_parameter_get ( o )
    !
    call stretch_parameter_verify ( o )
    !
    call stretch_parameter_put ( o )
    !
    call stretch_parameter_prep ( o )
    !
    return
    !
  end subroutine stretch_update
  !
  subroutine stretch_parameter_get ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: n0_inp

    !
    integer,                           save :: i_call = 0
    !
    i_call = i_call + 1
    !
    n0_inp = inil
    call pc_get_global ('numtr'   ,  n0_inp)   ! maximum number of traces.
    call pc_get_global ('gathered',o%gathered) ! whether properly gathered.
    call pc_get_global ('nwih'  , o%nh_inp)  ! number of header words.
    call timeglob_get ( o%nt_inp, o%t0_inp, o%dt_inp )
    o%t1_inp = ( o%nt_inp - 1 ) * o%dt_inp + o%t0_inp
    !
    !------------Check that globals are set:
    !
    if (   n0_inp .eq. inil) call pc_error ("NUMTR global hasn't been set.")
    if ( o%nh_inp .eq. inil) call pc_error ("NWIH global hasn't been set.")
    if ( o%nt_inp .eq. inil) call pc_error ("NDPT global hasn't been set.")
    if ( o%t0_inp .eq. fnil) call pc_error ("TSTRT global hasn't been set.")
    if ( o%dt_inp .eq. fnil) call pc_error ("DT global hasn't been set.")
    !
    xxif_old_pathname_vel : &
    if ( pc_process_keyword_present ( 'pathname_mig' ) ) then
    call pc_get ( 'pathname_mig',     o%pathname_vel     )
    else xxif_old_pathname_vel 
    call pc_get ( 'pathname_vel',     o%pathname_vel     )
    end if xxif_old_pathname_vel 
    !
    call pc_get ( 'pathname_ref',     o%pathname_ref     )
    !
    xxif_old_velname_ref : &
    if ( pc_process_keyword_present ( 'name_funct' ) ) then
    call pc_get ( 'name_funct',       o%velname_ref      )
    else xxif_old_velname_ref 
    call pc_get ( 'velname_ref',      o%velname_ref      )
    end if xxif_old_velname_ref 
    !
    call pc_get ( 'vel_const',        o%vel_const        )
    !
    xxif_old_opt_dir : &
    if ( pc_process_keyword_present ( 'mode' ) ) then
    call pc_get ( 'mode',             o%opt_dir          )
    else xxif_old_opt_dir 
    call pc_get ( 'opt_dir',          o%opt_dir          )
    end if xxif_old_opt_dir 
    !
    xxif_old_opt_smooth : &
    if ( pc_process_keyword_present ( 'smooth' ) ) then
    call pc_get ( 'smooth',           o%opt_smooth       )
    else xxif_old_opt_smooth 
    call pc_get ( 'opt_smooth',       o%opt_smooth       )
    end if xxif_old_opt_smooth 
    !
    xxif_degree_smooth : &
    if ( pc_process_keyword_present ( 'degree_smooth' ) ) then
    call pc_get ( 'degree_smooth',    o%degree_smooth    )
    else xxif_degree_smooth 
    call pc_get ( 'degree',           o%degree_smooth    )
    end if xxif_degree_smooth 
    !
    xxif_old_tim_tot : &
    if ( pc_process_keyword_present ( 'ndpt_out' ) ) then
    call pc_get ( 'ndpt_out',         o%tim_tot          )
    else xxif_old_tim_tot 
    call pc_get ( 'tim_tot',          o%tim_tot          )
    end if xxif_old_tim_tot 
    !
    xxif_old_tim_init : &
    if ( pc_process_keyword_present ( 'tstrt_out' ) ) then
    call pc_get ( 'tstrt_out',        o%tim_init         )
    else xxif_old_tim_init 
    call pc_get ( 'tim_init',         o%tim_init         )
    end if xxif_old_tim_init 
    !
    xxif_old_tim_inc : &
    if ( pc_process_keyword_present ( 'dt_out' ) ) then
    call pc_get ( 'dt_out',           o%tim_inc          )
    o%tim_last = ( o%tim_tot - 1 ) * o%tim_inc + o%tim_init
    else xxif_old_tim_inc 
    call pc_get ( 'tim_inc',          o%tim_inc          )
    call pc_get ( 'tim_last',         o%tim_last         )
    end if xxif_old_tim_inc 
    !
    call pc_get ( 'opt_doppler_mute', o%opt_doppler_mute )
    call pc_get ( 'opt_vel_scale',    o%opt_vel_scale    )
    call pc_get ( 'vel_scale',        o%vel_scale        )
    call pc_get ( 'opt_common_angle', o%opt_common_angle )
    call pc_get ( 'opt_zero_angle',   o%opt_zero_angle   )
    call pc_get ( 'ray_tot',          o%ray_tot          )
    call pc_get ( 'ray_init',         o%ray_init         )
    call pc_get ( 'ray_last',         o%ray_last         )
    call pc_get ( 'ray_inc',          o%ray_inc          )
    !
    o%nt_out = o%tim_tot
    o%t0_out = o%tim_init
    o%t1_out = o%tim_last
    o%dt_out = o%tim_inc
    !
    return
    !
  end subroutine stretch_parameter_get 
  !
  subroutine stretch_parameter_put ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer,                           save :: i_call = 0
    !
    i_call = i_call + 1
    !
    call pc_put_options_field ( 'opt_dir',     c_opt_dir,     n_opt_dir     )
    call pc_put_options_field ( 'opt_smooth',  c_opt_smooth,  n_opt_smooth  )
    call pc_put_options_field ( 'opt_calc_cv', c_opt_calc_cv, n_opt_calc_cv )
    call pc_put_options_field ( 'opt_common_angle', c_yes_no,  n_yes_no  )
    call pc_put_options_field ( 'opt_zero_angle',   c_yes_no,  n_yes_no  )
    !
    call pc_put ( 'pathname_vel',     o%pathname_vel     )
    call pc_put ( 'pathname_ref',     o%pathname_ref     )
    call pc_put ( 'velname_ref',      o%velname_ref      )
    call pc_put ( 'vel_const',        o%vel_const        )
    call pc_put ( 'opt_dir',          o%opt_dir          )
    call pc_put ( 'opt_smooth',       o%opt_smooth       )
    call pc_put ( 'degree_smooth',    o%degree_smooth    )
    call pc_put ( 'ndpt_out',         o%nt_out           )
    call pc_put ( 'tstrt_out',        o%t0_out           )
    call pc_put ( 'dt_out',           o%dt_out           )
    call pc_put ( 'tim_tot',          o%tim_tot          )
    call pc_put ( 'tim_init',         o%tim_init         )
    call pc_put ( 'tim_last',         o%tim_last         )
    call pc_put ( 'tim_inc',          o%tim_inc          )
    call pc_put ( 'opt_calc_cv',      o%opt_calc_cv      )
    call pc_put ( 'opt_doppler_mute', o%opt_doppler_mute )
    call pc_put ( 'opt_vel_scale',    o%opt_vel_scale    )
    call pc_put ( 'vel_scale',        o%vel_scale        )
    call pc_put ( 'opt_common_angle', o%opt_common_angle )
    call pc_put ( 'opt_zero_angle',   o%opt_zero_angle   )
    call pc_put ( 'ray_tot',          o%ray_tot          )
    call pc_put ( 'ray_init',         o%ray_init         )
    call pc_put ( 'ray_last',         o%ray_last         )
    call pc_put ( 'ray_inc',          o%ray_inc          )
    !
    call timeglob_put ( o%nt_out, o%t0_out, o%dt_out )
    !
    o%t1_out = ( o%nt_out - 1 ) * o%dt_out + o%t0_out
    !
    return
    !
  end subroutine stretch_parameter_put 
  !
  subroutine stretch_parameter_verify ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: i_err
    integer                                 :: i_stat
    !
    integer,                           save :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    o%ref_type = 'VTRM'
    !
    if ( o%opt_common_angle ) &
    o%ref_type = 'VTIN'
    !
    call string_to_upper ( o%velname_ref )
    call string_to_upper ( o%opt_dir )
    call string_to_upper ( o%opt_smooth )
    call string_to_upper ( o%opt_calc_cv )
    !
    if ( len_trim(o%pathname_vel) < 1 ) o%pathname_vel = PATHCHECK_EMPTY
    !
    if ( len_trim(o%pathname_ref) < 1 ) o%pathname_ref = PATHCHECK_EMPTY
    !
    if ( len_trim(o%velname_ref)  < 1 ) o%velname_ref = PATHCHECK_EMPTY
    !
    o%nt_lay = o%nt_inp
    o%nx_map = 1
    o%ny_map = 1
    o%nv_map = 1
    !
    call stretch_mem_all ( o, i_err )
    !
    if ( i_err .ne. 0 ) &
call pc_error ( ' Error in stretch_parameter_verify during stretch_mem_all ' )
    !
    o%rw_fac = 1
    !
    o%sq_fac = 1
    !
    ! check the files whether it exits or not.
    !
    xxif_pathname_ref_empty : if ( o%pathname_ref .ne. PATHCHECK_EMPTY ) then
      !
      call pathcheck ('PATHNAME_REF', o%pathname_ref, status=i_stat)
      !
      if ( o%velname_ref .eq. PATHCHECK_EMPTY ) &
      call pc_error ( 'Invalid in STRETCH: VELNAME_REF is required')
      !
      o%use_pathname_ref = .true.
      !
      call stretch_velocity_ref ( o )
      !
      call pc_put_sensitive_field_flag  ('VEL_CONST',  .false.)
      !
      call pc_put_sensitive_field_flag  ('OPT_CALC_CV',    .false.)
      !
    else xxif_pathname_ref_empty 
      !
      call pc_put_sensitive_field_flag  ('VEL_CONST', .true.)
      !
      call pc_put_sensitive_field_flag  ('OPT_CALC_CV',   .true.)
      !
      o%use_pathname_ref = .false.
      !
    end if xxif_pathname_ref_empty 
    !
    xxif_pathname_vel_empty : if ( o%pathname_vel .ne. PATHCHECK_EMPTY ) then
      !
      call pathcheck ('PATHNAME_VEL', o%pathname_vel, status=i_stat)
      !
      o%use_pathname_vel = .true.
      !
    else xxif_pathname_vel_empty 
      !
      o%use_pathname_vel = .false.
      !
      o%opt_calc_cv = 'USER'
      !
    end if xxif_pathname_vel_empty 
    !
    if ( o%opt_smooth .ne. 'FIT'.and. o%opt_smooth .ne. 'INTERP') &
    call pc_error ( 'Invalid in STRETCH: OPT_SMOOTH MUST BE FIT OR INTERP')
    !
    if ( o%opt_dir .ne. 'FORWARD'.and. o%opt_dir .ne. 'INVERSE') &
    call pc_error ( 'Invalid in STRETCH: OPT_DIR MUST BE FORWARD OR INVERSE')
    !
    if ( o%opt_calc_cv .ne. 'MIN_STR'.and. o%opt_calc_cv .ne. 'USER') &
    call pc_error ( 'Invalid in STRETCH: OPT_DIR MUST BE MIN_STR OR USER')
    !
    if ( o%nt_out .le. 0 ) &
    call pc_error ( 'Invalid in STRETCH: NDPT_OUT MUST BE > 0')
    !
    if ( o%dt_out .le. 0 ) &
    call pc_error ( 'Invalid in STRETCH: DT_OUT MUST BE >  0')
    !
    if ( o%degree_smooth .lt. 0 ) &
    call pc_error ( 'Invalid in STRETCH: DEGREE_SMOOTH MUST BE > = 0')
    !
    xxif_use_pathname_vel : if ( o%use_pathname_vel ) then
      !
      o%t0_map = 0
      !
      xxif_out_shorter_than_inp : if ( o%nt_out .le.  o%nt_inp ) then
        !
        o%t1_map = (o%nt_out - 1)*o%dt_out + o%t0_out
        o%dt_map = o%dt_out
        o%nt_map = (o%t1_map - o%t0_map)/o%dt_map + 1.0005
        !
      else xxif_out_shorter_than_inp 
        !
        o%t1_map = (o%nt_inp - 1)*o%dt_inp + o%t0_inp
        o%dt_map = o%dt_inp
        o%nt_map = (o%t1_map - o%t0_map)/o%dt_map + 1.0005
        !
      end if xxif_out_shorter_than_inp 
      !
      if ( .not. o%opt_common_angle ) &
      call stretch_compute_par ( o )
      !
    end if xxif_use_pathname_vel 
    !
    if ( o%opt_calc_cv .eq. 'MIN_STR' .and. (.not. o%use_pathname_ref) ) then
      !
      write (pc_get_lun(), *) ' Velocity changes from ', &
      o%vel_const,  ' to ', o%rv_stretch
      !
      o%vel_const = o%rv_stretch
      !
      call pc_put_sensitive_field_flag  ('VEL_CONST',  .false.)
      !
    end if
    !
    call pc_put_sensitive_field_flag  ( 'vel_scale', o%opt_vel_scale )
    !
    call pc_put_sensitive_field_flag  ( 'opt_zero_angle', o%opt_common_angle )
    !
    return
    !
  end subroutine stretch_parameter_verify 
  !
  subroutine stretch_parameter_prep ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: i_err


    logical                                 :: iftd
    integer                                 :: ntapes 
    integer                                 :: nscratch 
    integer                                 :: nstore 
    integer                                 :: ndisk

    character(len=3)                        :: need_label 
    character(len=3)                        :: need_request
    character(len=3)                        :: twosets


    !
    integer,                           save :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    ! Conditionally deallocate all arrays :
    !
    call stretch_mem_del ( o )
    !
    ! Initialize all variables (except allocated arrays) needed for execution
    !
    call stretch_cpucount_create ( o, i_err )
    !
    xxif_cpucount_create_err : if ( i_err .ne. 0 ) then
      !
      call pc_error ( 'Error during STRETCH stretch_cpucount_create ' ) 
      !
      return
      !
    end if xxif_cpucount_create_err 
    !
    call headsave_create ( o%h, 'stretch', o%nh_inp, i_err )
    !
    xxif_headsave_create_err : if ( i_err .ne. 0 ) then
      !
      call pc_error ( 'Error during STRETCH headsave_create ' ) 
      !
      return
      !
    end if xxif_headsave_create_err 
    !
    o%degree_smooth2 =  o%degree_smooth
    !
    if ( o%opt_smooth .eq. 'INTERP') &
    o%degree_smooth2 = -1
    !
    o%mv_pol = 37
    !
    o%t0_map = 0
    !
    o%rv_stretch = o%vel_const
    !
    o%t1_out   = o%nt_inp * o%dt_inp
    !
    !--------------------  hardwired  dt_map here ------------------
    !
    xxif_out_longer_than_inp : if ( o%nt_out .ge.  o%nt_inp ) then
      !
      o%t1_map = (o%nt_out - 1)*o%dt_out + o%t0_out
      o%dt_map = o%dt_out * 10
      o%nt_map = (o%t1_map - o%t0_map)/o%dt_map + 1.0005
      !
    else xxif_out_longer_than_inp 
      !
      o%t1_map = (o%nt_inp - 1)*o%dt_inp + o%t0_inp
      o%dt_map = o%dt_inp *10
      o%nt_map = (o%t1_map - o%t0_map)/o%dt_map + 1.0005
      !
    end if xxif_out_longer_than_inp 
    !
    o%n_work = max(max(1000,o%nt_inp*2),o%nt_out*2)
    o%n_work = max(o%n_work,o%mv_pol**2 + o%mv_pol*2)
    !
    o%np_ray = o%ray_tot
    o%p0_ray = o%ray_init * stretch_ray_scale
    o%p1_ray = o%ray_last * stretch_ray_scale
    o%dp_ray = o%ray_inc  * stretch_ray_scale
    !
    write (pc_get_lun(), '( &
    & /, " stretch time grid characteristics ipn=",i5, " opt_dir=", a, &
    & /, " in  nt=",i10," t_min=",g14.7," t_max=",g14.7," dt=",g14.7, &
    & /, " out nt=",i10," t_min=",g14.7," t_max=",g14.7," dt=",g14.7, &
    & /, " map nt=",i10," t_min=",g14.7," t_max=",g14.7," dt=",g14.7 &
    & )') &
    o%ipn, trim(o%opt_dir), &
    o%nt_inp, o%t0_inp, o%t1_inp, o%dt_inp, &
    o%nt_out, o%t0_out, o%t1_out, o%dt_out, &
    o%nt_map, o%t0_map, o%t1_map, o%dt_map
    !
    write (pc_get_lun(), '( &
    & /, " stretch ray grid characteristics stretch_ray_scale=", g14.7, &
    & /, " ray np=",i10," p_min=",g14.7," p_max=",g14.7," dp=",g14.7 &
    & )') &
    stretch_ray_scale, &
    o%np_ray, o%p0_ray, o%p1_ray, o%dp_ray
    !
    o%i_trin = 0
    !
    call stretch_velocity_size( o )
    !
    ! define control parameters
    !
    need_label   = 'NO'
    need_request = 'NO'
    twosets      = 'NO'
    iftd         = .false.
    ndisk        = 0
    ntapes       = 0
    !
    nstore = 4*o%nx_map*o%ny_map + o%nt_map*o%nx_map*o%ny_map + 2*o%nt_out
    nscratch = 1
    !
    call pc_put_control ('nstore',             nstore)
    call pc_put_control ('nscratch',         nscratch)
    call pc_put_control ('need_label',     need_label)
    call pc_put_control ('need_request', need_request)
    call pc_put_control ('twosets',           twosets)
    !
    if ( pc_do_not_process_traces()) return
    !
    o%skip_wrapup = .false.
    !
    ! Allocate your permanent memory buffers:
    !
    o%nv_map = o%nx_map * o%ny_map
    !
    call stretch_mem_all ( o, i_err )
    !
    if ( i_err .ne. 0 ) &
call pc_error ( ' Error in stretch_parameter_prep during stretch_mem_all ' )
    !
    xxif_use_pathname_vel : if ( o%use_pathname_vel ) then
      !
      call stretch_velocity_ref ( o )
      !
      call stretch_velocity_map ( o )
      !
      call stretch_compute_polynomial ( o )
      !
      call stretch_map_compute ( o )
      !
      call stretch_map_compute_index ( o )
      !
    end if xxif_use_pathname_vel 
    !
    return
    !
  end subroutine stretch_parameter_prep 
  !
  subroutine stretch ( o, n0_inp, hd_inp, tr_inp )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    integer,                  intent(inout) :: n0_inp
    double precision,         intent(inout) :: hd_inp(:,:)
    real,                     intent(inout) :: tr_inp(:,:)
    !
    real                                    :: rt_out(o%nt_map)
    real                                    :: ra_out(o%nt_map)
    real                                    :: tr_tmp(o%n_work)
    double precision                        :: x0_inp
    double precision                        :: y0_inp
    integer                                 :: nt_out
    integer                                 :: i0_inp
    integer                                 :: i_err
    integer                                 :: i_order = -1
    !
    call cpucount ( o%c, o%c_stretch_total, 1 )
    !
    ! process each input trace.
    !
    do_input_traces : do i0_inp = 1, n0_inp
      !
      o%i_trin = o%i_trin + 1
      !
      ! save headers for input traces ( locations 1 - 4 )
      !
      call headsave_store ( o%h, o%i_trin, 1, hd_inp ( :, i0_inp ) )
      !
      ! save headers for saved traces ( locations 5 - 8 )
      !
      call headsave_store ( o%h, o%i_trin, 5, hd_inp ( :, i0_inp ) )
      !
      x0_inp = hd_inp ( o%hx_map, i0_inp )
      !
      y0_inp = hd_inp ( o%hy_map, i0_inp )
      !
      !print'(" i0_inp=",i8," x=",g12.5," y=",g12.5," p=",g12.6)', &
      !i0_inp, x0_inp, y0_inp, hd_inp ( 6, i0_inp )
      !
      !print'(" qq2 i=",i2," i=",i8," x=",g12.6," y=",g12.6,&
      !& " h=",i8,1x,i8," t=",g12.6)',&
      !o%ipn, o%i_trin, x0_inp, y0_inp, &
      !nint(hd_inp ( 02, i0_inp )), nint(hd_inp ( 64, i0_inp )), &
      !maxval(abs(tr_inp(:,i0_inp)))
      !
      ! compute a new map if this is a new p value
      !
      call stretch_map_compute_p ( o, hd_inp ( :, i0_inp ), i_err ) 
      !
      if ( i_err .ne. 0 ) go to 998
      !
      xxif_opt_common_angle : if ( o%opt_common_angle ) then
        !
        call stretch_map_interpolate_p ( &
                                         o, x0_inp, y0_inp, &
                                         i_order, .true., .true., &
                                         o%lt_map, o%rt_map, o%ra_map, &
                                           nt_out,   rt_out,   ra_out &
                                       )
        !
      else xxif_opt_common_angle 
        !
        call stretch_map_interpolate ( &
                                       o, x0_inp, y0_inp, &
                                       i_order, .true., .true., &
                                       o%lt_map, o%rt_map, o%ra_map, &
                                         nt_out,   rt_out &
                                     )
        !
      end if xxif_opt_common_angle 
      !
      ! copy input data to tr_tmp area
      !
      tr_tmp ( 1:o%nt_inp ) = tr_inp ( 1:o%nt_inp, i0_inp )
      !
      ! map the input to the output
      !
      !if ( mod ( o%i_trin, 10 ) .eq. 1 ) &
      !print'(" qqq1 i=",i8," x=",g12.6," y=",g12.6,&
      !& " h=",i8,1x,i8," t=",g12.6)',&
      !o%i_trin, &
      !hd_inp ( 7, i0_inp ), hd_inp ( 8, i0_inp ), &
      !nint(hd_inp ( 02, i0_inp )), nint(hd_inp ( 64, i0_inp )), &
      !maxval(abs(tr_inp(:,i0_inp)))
      !
      call stretch_map_apply ( o, hd_inp(:,i0_inp), tr_inp(:,i0_inp), i_err )
      !
      !if ( mod ( o%i_trin, 10 ) .eq. 1 ) &
      !print'(" qq2 i=",i2," i=",i8," x=",g12.6," y=",g12.6,&
      !& " h=",i8,1x,i8," t=",g12.6)',&
      !o%ipn, o%i_trin, x0_inp, y0_inp, &
      !nint(hd_inp ( 02, i0_inp )), nint(hd_inp ( 64, i0_inp )), &
      !maxval(abs(tr_inp(:,i0_inp)))
      !
      if ( i_err .ne. 0 ) go to 997
      !
      ! save headers for output traces ( locations 9 - 12 )
      !
      call headsave_store ( o%h, o%i_trin, 9, hd_inp ( :, i0_inp ) )
      !
    end do do_input_traces 
    !
    ! recompute the max LAV
    !
    call lav_set_hdr ( hd_inp, tr_inp, o%nt_out, n0_inp )
    !
  1 continue
    !
    call cpucount ( o%c, o%c_stretch_total, 2 )
    !
    xxif_no_more_input : &
    if ( n0_inp .eq. NO_MORE_TRACES .or. n0_inp .eq. FATAL_ERROR ) then
      !
      if ( n0_inp .eq. 0 ) &
      write ( pc_get_lun(), '( &
      & /, " --- end of stretch processing --- ", &
      & /, " stretch has processed ",i10," traces.  opt_dir=", a, &
      & /, " input  nt=",i10," t0=",f10.4," dt=",f10.5, &
      & /, " output nt=",i10," t0=",f10.4," dt=",f10.5 &
      & )') &
      o%i_trin, o%opt_dir, &
      o%nt_inp, o%t0_inp, o%dt_inp, &
      o%nt_out, o%t0_out, o%dt_out
      !
      !print the current group of input header words
      !
      if ( pcpsx_i_pel() .eq. 0 ) &
      call headsave_print ( o%h, ' stretch end of input ', 1 )
      !
      !print the current group of output header words
      !
      if ( pcpsx_i_pel() .eq. 0 ) &
      call headsave_print ( o%h, ' stretch end of output ', 9 )
      !
      if ( pcpsx_i_pel() .eq. 0 ) &
      call cpucount_print ( o%c, ' stretch end of processing ' )
      !
      call stretch_wrapup ( o )
      !
    end if xxif_no_more_input 
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in stretch during stretch_map_apply ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch during stretch_map_compute_p ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch ' )
    !
    i_err = -1
    !
    n0_inp = FATAL_ERROR
    !
    go to 1
    !
  end subroutine stretch 
  !
  subroutine stretch_map_compute ( o )
    !
    !     compute the mapping between the input and stretched output.
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: iv_map
    integer                                 :: ix_map
    integer                                 :: iy_map

    !
    double precision                        :: rx_map
    double precision                        :: ry_map
    !
    integer                                 :: i_err
    !
    integer                                 :: lt_ref
    real                                    :: rv_ref(o%nt_map)
    real                                    :: rf_ref(o%nt_map)
    real                                    :: ra_ref(o%nt_map)
    !
    integer                                 :: lt_str
    real                                    :: rv_str(o%nt_map)
    real                                    :: rf_str(o%nt_map)
    real                                    :: ra_str(o%nt_map)
    !
    integer                                 :: lt_inp
    real                                    :: rv_inp(o%nt_map)
    real                                    :: rf_inp(o%nt_map)
    real                                    :: ra_inp(o%nt_map)
    !
    integer                                 :: lt_out
    real                                    :: rv_out(o%nt_map)
    real                                    :: rf_out(o%nt_map)
    real                                    :: ra_out(o%nt_map)
    integer,                           save :: i_call = 0
    i_call = i_call + 1
    !
    !  compute mapping time values with reference and stacking velocity files
    !
    i_err = 0
    !
    ! constant reference velocity
    !
    !print'(" top stretch_map_compute " )'
    !
    !  compute the stretch integeral for the reference function
    !
    rv_ref = o%rv_ref
    !
    o%lt_map = o%nt_map
    !
      lt_ref = o%nt_map
    !
    call stretch_map_compute_integral_n ( o, lt_ref, rv_ref, rf_ref, ra_ref )
    !
    !print'(" stretch_map_compute lt_ref=",i8," nv_map=",i8, &
    !& /, " mm ref v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
    !& /, " fl ref v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6 &
    !& )',&
    !lt_ref, o%nv_map, &
    !minval(rv_ref), maxval(rv_ref), minval(rf_ref), maxval(rf_ref), &
    !rv_ref(1), rv_ref(lt_ref), rf_ref(1), rf_ref(lt_ref)
    !
    ! for each stretch velocity function
    ! 1 interpolate the velocity function
    ! 2 compute the stretch integral
    ! 3 compute the mapping time from input to output
    !
    do_iv_map : do iv_map = 1, o%nv_map
      !
      rx_map = o%rx_map(iv_map)
      !
      ry_map = o%ry_map(iv_map)
      !
      ix_map = max(1,min(o%nx_map,                                    &
      nint((o%rx_map(iv_map)-o%x0_map)/o%dx_map)+1))
      !
      iy_map = max(1,min(o%ny_map,                                    &
      nint((o%ry_map(iv_map)-o%y0_map)/o%dy_map)+1))
      !
      rf_str = 0.
      !
      ! 1 interpolate the velocity function
      !
      call stretch_map_interpolate ( &
                                     o, rx_map, ry_map, &
                                     o%degree_smooth2, .false., .false., &
                                     o%lt_map, o%rv_map, o%rv_map, &
                                       lt_str,   rv_str   &
                                   )
      !
      ! 2 compute the stretch integral
      !
      call stretch_map_compute_integral_n ( o, lt_str, rv_str, rf_str, ra_str )
      !
      !print'(" lt_str=",i8," iv_map=",i8, &
      !& /, " mm str v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
      !& /, " fl str v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6 &
      !& )',&
      !lt_str, iv_map, &
      !minval(rv_str), maxval(rv_str), minval(rf_str), maxval(rf_str), &
      !rv_str(1), rv_str(lt_str), rf_str(1), rf_str(lt_str)
      !
      ! 3 compute the mapping function from input to output
      !
      xxif_forward : if ( o%opt_dir(1:7) .eq. 'FORWARD' ) then
        !
        lt_inp = lt_str
        rv_inp = rv_str
        rf_inp = rf_str
        ra_inp = ra_str
        !
        lt_out = lt_ref
        rv_out = rv_ref
        rf_out = rf_ref
        ra_out = ra_ref
        !
      else if ( o%opt_dir(1:7) .eq. 'INVERSE' ) then
        !
        lt_inp = lt_ref
        rv_inp = rv_ref
        rf_inp = rf_ref
        ra_inp = ra_ref
        !
        lt_out = lt_str
        rv_out = rv_str
        rf_out = rf_str
        ra_out = ra_str
        !
      end if xxif_forward 
      !
      !write ( pc_get_lun(), ' ( &
      !& /, " bef stretch_map_compute_time ", &
      !& " nt_map=", i8, " lt_inp=", i8, " lt_out=", i8, &
      !& /, " mm inp v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
      !& /, " mm out v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
      !& /, " fl inp v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
      !& /, " fl out v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6 &
      !& )') &
      !o%nt_map, lt_inp, lt_out, &
      !minval(rv_inp), maxval(rv_inp), minval(rf_inp), maxval(rf_inp), &
      !minval(rv_out), maxval(rv_out), minval(rf_out), maxval(rf_out), &
      !rv_inp(1),rv_inp(lt_inp), rf_inp(1),rf_inp(lt_inp), &
      !rv_out(1),rv_out(lt_out), rf_out(1),rf_out(lt_out)
      !
      call stretch_map_compute_time ( &
                                      o%nt_map, o%t0_map, o%dt_map, &
                                      lt_inp, rv_inp, rf_inp, ra_inp, &
                                      lt_out, rv_out, rf_out, ra_out, &
                                      o%lt_map (    ix_map, iy_map ), &
                                      o%rt_map ( :, ix_map, iy_map ), &
                                      o%ra_map ( :, ix_map, iy_map ), &
                                      i_err &
                                    )
      !
      if ( i_err .ne. 0 ) go to 998
      !
    end do do_iv_map 
    !
    !print'(" stretch_map_compute ", &
    !& /, " c=",i5," ipn=",i8," nx_map=",i8," ny_map=",i8,&
    !& /," nt_map=",i8," t0_map=",g10.4," dt_map=",g10.4, &
    !& /," qqq velocity " &
    !& )', &
    !i_call, o%ipn, o%nx_map, o%ny_map, o%nt_map, o%t0_map, o%dt_map
    !print'(1x,i5,1x,i5,12(1x,g10.4))',&
    !0.,0., &
    !(rv_ref(it_map),it_map=1,o%nt_map,o%nt_map/10)
    !do_iy_map_1 : do iy_map = 1 , o%ny_map, 10
    !do_ix_map_1 : do ix_map = 1 , o%nx_map, 10
    !print'(1x,i5,1x,i5,12(1x,g10.4))',&
    !ix_map,iy_map,&
    !(o%rv_map(it_map,ix_map,iy_map),it_map=1,o%nt_map,o%nt_map/10)
    !end do do_ix_map_1 
    !end do do_iy_map_1 
    !
    !print'(" stretch_map_compute ", &
    !& /, " c=",i5," ipn=",i8," nx_map=",i8," ny_map=",i8,&
    !& /," nt_map=",i8," t0_map=",g10.4," dt_map=",g10.4, &
    !& /," qqq time " &
    !& )', &
    !i_call, o%ipn, o%nx_map, o%ny_map, o%nt_map, o%t0_map, o%dt_map
    !do_iy_map : do iy_map = 1 , o%ny_map, 10
    !do_ix_map : do ix_map = 1 , o%nx_map, 10
    !print'(1x,i5,1x,i5,12(1x,g10.4))',&
    !ix_map,iy_map,&
    !(o%rt_map(it_map,ix_map,iy_map),it_map=1,o%nt_map,o%nt_map/10)
    !end do do_ix_map
    !end do do_iy_map
    !
    !print'(" end stretch_map_compute " )'
    !
    return
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_map_compute during stretch_map_compute_time ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_map_compute ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_map_compute 
  !
  subroutine stretch_compute_par ( o )
    !
    ! compute optimal stretch velocity, squeeze factor, and W factor
    ! using reference and stacking velocity files.
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: iv_map
    integer                                 :: nt_lay
    integer                                 :: it_map

    integer                                 :: i_err
    !
    real                                    :: rx_vel
    real                                    :: ry_vel
    real                                    :: rv_tmp
    real                                    :: rf_loc
    real                                    :: rv_min
    real                                    :: rv_loc
    real                                    :: dtf
    !
    integer                                 :: nv_ref
    integer                                 :: lt_str
    real                                    :: rv_ref(o%nt_map)
    real                                    :: rf_ref(o%nt_map)
    real                                    :: ra_ref(o%nt_map)
    real                                    :: rt_str(o%nt_map)
    real                                    :: rv_str(o%nt_map)
    real                                    :: rf_str(o%nt_map)
    real                                    :: ra_str(o%nt_map)
    real                                    :: rt_rms(o%nt_map)
    real                                    :: rv_rms(o%nt_map)
    real                                    :: rv_int(o%nt_map)
    real                                    :: rf_max(o%nt_map)
    real                                    :: rv_max(o%nt_map)
    !
    i_err = 0
    !
    rf_loc = 0.0
    !
    rv_min = 1.0e+10
    !
    rv_loc = 0.0
    !
    o%rv_stretch = 1000000.0
    !
    rv_tmp = o%vel_const
    !
    ! open and read the stretch velocity file
    !
    call stretch_velocity_open ( &
    o%v, o%pathname_vel, o%nv_map, o%hx_map, o%hy_map, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    !  for each stretch velocity function
    !    1  read the velocity function
    !    2  compute the stretch integral
    !    3  compute the mapping time from input to output
    !
    do_ixy_map : do iv_map = 1, o%nv_map
      !
      rt_str = 0.0
      !
      rv_str = 0.0
      !
      call stretch_velocity_read ( &
      o%v, o%ref_type, o%vel_type, o%vel_name, &
      rx_vel, ry_vel, nt_lay, rt_str, rv_str, i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      rt_rms(1:nt_lay) = rt_str(1:nt_lay)
      !
      rv_rms(1:nt_lay) = rv_str(1:nt_lay)
      !
      rv_min = min(rv_min, rv_rms(1))
      !
      call stretch_interpolate_i_to_r ( &
      nt_lay, rt_rms, rv_rms, o%nt_map, o%t0_map, o%dt_map, rv_str )
      !
      if ( i_err .ne. 0 ) go to 996
      !
      ! 2  compute the stretch integral
      !
      call stretch_map_compute_integral_n ( o, lt_str, rv_str, rf_str, ra_str )
      !
      call stretch_rv_stretch ( &
      o, rv_str, rf_str, rv_max, rf_max, rv_loc, rf_loc )
      !
    end do do_ixy_map 
    !
    xxif_opt_calc_cv : if ( o%opt_calc_cv .eq. 'MIN_STR' ) then
      !
      if ( o%rv_stretch .le. 0.0 ) go to 995
      !
      rv_tmp = o%rv_stretch
      !
    end if xxif_opt_calc_cv 
    !
    call stretch_velocity_close ( o%v )
    !
    if ( .not. stretch_fktmig ) return
    !
    ! compute W and squeeze factor for FKMIG
    !
    xxif_use_pathname_ref : if ( o%use_pathname_ref ) then
      !
      call stretch_interpolate_i_to_r ( &
      o%nt_lay, o%rt_lay, o%rv_lay, o%nt_map, o%t0_map, o%dt_map, rv_ref )
      !
    else xxif_use_pathname_ref 
      !
      ! constant reference velocity
      !
      rv_ref(1:o%nt_map) = rv_tmp
      !
    end if xxif_use_pathname_ref 
    !
    ! compute the stretch integeral for the reference function
    !
    rt_rms(1:o%nt_map) = (/(o%t0_map+it_map*o%dt_map,it_map=1,o%nt_map)/)
    !
    call stretch_map_compute_integral_n ( o, nv_ref, rv_ref, rf_ref, ra_ref )
    !
    call stretch_sq_fac ( o, rv_ref, rf_ref, rv_max, rf_max, rv_min, rv_loc )
    !
    dtf = o%dt_inp*o%sq_fac
    !
    call stretch_compute_max_time (o%sq_fac, &
    o%nt_inp, o%t0_inp, o%dt_inp, o%t1_inp, &
    rf_max, o%t1_out, rf_ref, o%nt_map, rt_rms, rv_ref, rv_int, dtf )
    !
    return
    !
995 continue
    !
    call pc_error ( &
    ' error in stretch_compute_par rv_xtretc <=0.0, opt_calc_cv=MIN_STR ' ) 
    !
    go to 999
    !
996 continue
    !
    call pc_error ( &
    ' error in stretch_compute_par during stretch_interpolate_i_to_r ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( &
    ' error in stretch_compute_par during stretch_velocity_read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_compute_par during stretch_velocity_open ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_compute_par ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_compute_par 
  !
  subroutine stretch_rv_stretch ( &
  o, rv_str, rf_str, rv_max, rf_max, rv_loc, rf_loc )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    real,                     intent(in   ) :: rv_str(:)
    real,                     intent(in   ) :: rf_str(:)
    real,                     intent(inout) :: rf_max(:)
    real,                     intent(inout) :: rv_max(:)
    real,                     intent(inout) :: rf_loc
    real,                     intent(inout) :: rv_loc
    !
    real                                    :: t1, t2, eta
    real                                    :: a1, b1, wrk1, w2
    integer                                 :: it_map, npv
    !
    xxif_rf_str : if ( rf_str(o%nt_map) > rf_loc ) then
      !
      rf_loc = rf_str(o%nt_map)
      !
      rf_max = rf_str
      !
      rv_max = rv_str
      !
    end if xxif_rf_str 
    !
    a1 = 0.0
    !
    b1 = 0.0
    !
    npv = 0.8 * o%nt_map
    !
    do_it_map : do it_map = 2, o%nt_map
      !
      ! correct the integration
      !
      wrk1 = 2.*rf_str(it_map)
      !
      eta = sqrt(wrk1)
      !
      a1 = a1 + wrk1*o%dt_map
      !
      b1 = b1 + eta*float(it_map - 1)*o%dt_map*o%dt_map
      !
      if ( it_map .eq. npv) w2 = eta
      !
    end do do_it_map 
    !
    a1 = a1 - rf_str(o%nt_map)*o%dt_map
    !
    b1 = b1 - 0.5*eta*(o%nt_map - 1)*o%dt_map*o%dt_map
    !
    t1 = a1/b1
    !
    ! take minimum for all input functions
    !
    o%rv_stretch = min(o%rv_stretch,t1)
    !
    t2 = w2/(( npv-1)*o%dt_map)
    !
    rv_loc = max( rv_loc, t2)
    !
    return
    !
  end subroutine stretch_rv_stretch
  !
  subroutine stretch_sq_fac ( &
  o, rv_ref, rf_ref, rv_max, rf_max, rv_min, rv_loc )
    !
    !     compute the resampling rate from minimum stretch function.
    !
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    real,                     intent(in   ) :: rv_ref(:)
    real,                     intent(in   ) :: rf_ref(:)
    real,                     intent(in   ) :: rf_max(:)
    real,                     intent(in   ) :: rv_max(:)
    real,                     intent(in   ) :: rv_min
    real,                     intent(in   ) :: rv_loc
    !
    real                                    :: rho     , t1, t2, delt1 
    integer                                 :: n1, n2, idel
    integer                                 :: map(o%nt_map)
    integer                                 :: iclip1,iclip2
    integer                                 :: i,i1,i2,i3
    !
    n1 = o%nt_map
    n2 = o%nt_map
    !
    rho = rv_loc/rv_min - 1.0
    if ( rv_loc < rv_min) rho = 0.0
    if ( o%use_pathname_ref ) then
      o%rw_fac = 1.0
    else
      o%rw_fac = 1.0/(1.0 + rho + 0.22*rho*rho)
    end if
    o%rw_fac = max(o%rw_fac,0.)
    o%rw_fac = min(o%rw_fac,1.)
    !
    !  given rf_ref(i) for 1.le.i.le.n1 find  rf_max(map(i)=j) s.t. j is the
    !  biggest index for which rf_max(j) .le. rf_ref(i). further find the
    !  correction frac(j) that gives the interpolation to the interval
    !  [t(j),t(j+1)].
    !  the functions eta1 and rf_max must be monotone increasing functions.
    !  map(i)=0 if no legitimate mapping is possible.
    !  iclip1....1st point such that rf_ref(iclip1) .ge. rf_max(1)
    !             1.le. iclip1 .le. n1
    !             1.le. iclip2 .le. n1
    !  iclip2....last point such that rf_ref(iclip2) .le.  rf_max(n2)
    !
    ! 1st check for pathological cases where no overlap occurs.
    !
     iclip1 =1
     iclip2 =n1
     if (  (rf_ref(n1) .le. rf_max(1)) .or.                &
        (rf_ref(1)  .ge. rf_max(n2)) ) then
      write(pc_get_lun(), *) 'STRETCH_SQ_FAC: N1,N2=',n1,n2
      write(pc_get_lun(), *) 'STRETCH_SQ_FAC: ETA1 LIMITS=',    &
            rf_ref(1),rf_ref(n1)
        write(pc_get_lun(), *) 'STRETCH_SQ_FAC: ETA2 LIMITS=',  &
            rf_max(1),rf_max(n2)
        call pc_error ( ' STRETCH: PATHOLOGICAL ERROR IN STRETCH_SQ_FAC ')
    return
      end if
      !
      i2     =0
    !       output time grid
      do i1=1,n1
         i3=i2+1
 55        continue
         if ( rf_max(i3) .le. rf_ref(i1) .and. i3 < n2 ) then
           i3=i3+1
           go to 55
         end if
         i2 =max(i3-1,0)
         map(i1) =i2
         if ( i2.eq.0) iclip1=i1+1
         if ( i2.ne.n2-1) iclip2=i1
      end do
    !
    !   find the differential time stretch. from 1 to 2
    !   integrals and velocitys must be defined to time=0.
    !   integrals and velocitys defined on the same grid.
    !   round the squeeze factor to the nearest per cent
    !
    o%sq_fac=10.0
    do i=iclip1,iclip2
       i1=max(i-1,1)
       i2=max(map(i)-1,1)
       t1=i1                   ! check v1 = rv_tmp, rv_loc = vmax
       t2=i2
       delt1=rv_max(i2)*rv_max(i2)*t2/(rv_ref(i1)*rv_ref(i1)*i1)
       o%sq_fac=min(delt1,o%sq_fac)
    end do
    !
    o%sq_fac = 100.0*o%sq_fac
    idel = nint(o%sq_fac + 0.49)
    o%sq_fac = 0.01* idel
    !
      write(pc_get_lun(), *)      &
        'STRETCH_SQ_FAC: Computed compression factor=',o%sq_fac
     if ( o%sq_fac < 0.20 ) then
      write(pc_get_lun(), *)                   &
         'STRETCH_SQ_FAC: Stretch compression=',o%sq_fac,' > 1:5!'
      write(pc_get_lun(), *)                   &
         'STRETCH_SQ_FAC: Compression is being clipped to 0.20'
      write(pc_get_lun(), *)                   &
         'STRETCH_SQ_FAC: You may lose accuracy for small t'
      write(pc_get_lun(), *)                    &
         'STRETCH_SQ_FAC: Consider doing a cascade migration ???'
      o%sq_fac = 0.20
     end if
     !
    return
    !
  end subroutine stretch_sq_fac
  !
  subroutine stretch_compute_max_time (sq_fac,&
  nt, ot, dt, ti, etai, to, etao, n, t, v, vdix, dtf )
    !
    ! compute the max stretched time.
    !
    !
    integer,                  intent(in   ) :: nt
    integer,                  intent(in   ) :: n
    real,                     intent(in   ) :: sq_fac
    real,                     intent(in   ) :: ot
    real,                     intent(in   ) :: dt
    real,                     intent(in   ) :: ti
    real,                     intent(  out) :: to
    real,                     intent(in   ) :: dtf
    real,                     intent(in   ) :: etai(:)
    real,                     intent(in   ) :: etao(:)
    real,                     intent(in   ) :: t(:)
    real,                     intent(in   ) :: v(:)
    real,                     intent(in   ) :: vdix(:)
    !
    integer                                 :: inpoint, i, ito
    real                                    :: vali, del, tlast, a, b, c
    integer,save :: cnt=0
    !
    !    map an input time ti in etai to an output time to in etao .
    !    dtf is the sample rate of the output grid - use to round to
    !    etai & etao are given on the grid defined by nt,dt,ot
    !    the velocities t(n),v(n) for etao allow us to project etao
    !    beyond the limits of where it is given.
    !    v(i).......rms velocity of etao system. 0<i.le.n
    !    t(i).......time coordinate of the v values
    !    vdix(i)....interval velocity of etao system. 0<i.le.n
    !
    to = ti
    inpoint = nint((ti - ot)/dt) + 1
    if(inpoint > size(etai)) then
     !print *," DBG ti=",ti," ot=",ot," dt=",dt
     !print *," DBG: size etai =", size(etai)," size etao=",size(etao)
     !print *," DBG: original inpoint =", inpoint
      inpoint = min(size(etai), nint(inpoint* sq_fac))
     !print *," DBG: altered inpoint =", inpoint
    endif
    if ( inpoint<1 .or. inpoint>nt ) then
      to = -ti
      call pc_error ( 'STRETCH_COMPUTE_MAX_TIME: ti is out of range' )
      !
    end if
    !
    vali = etai(inpoint)
    !
    ! case 1 : to < ot      , vdix=vrms=constant before ot
    if ( vali < etao(1) ) then
      to = sqrt(2*etai(1)/(v(1)*v(1)))
      go to 99
    end if
    !
    ! case 2 : to > tlast=ot + (nt-1)*dt
    ! project the etao integral. vdix=constant, vrms**2*t=linear
    ! vdix,v = dix and rms velocity after tlast
    !
    if ( vali > etao(nt) ) then
      del = vali - etao(nt)
      tlast = ot + (nt - 1)*dt
      a = 0.5*vdix(n)*vdix(n)
      b = tlast*v(n)*v(n)
      c = -del
      to = tlast + ((-b) + sqrt(b*b - 4*a*c))/(2*a)
      go to 99
    end if
    !
    ! case 3 :  ot < to < tlast=ot + (nt-1)*dt
    i = 1
    9 continue
    if ( etao(i)<vali .and. i<nt ) then
      i = i + 1
      go to 9
    end if
    to = ot + (i - 1)*dt
    !
   99 continue
    !     round to nearest dt before returning
    ito = nint(to/dtf)
    to = ito*dtf
    !
    return
    !
    return
    !
  end subroutine stretch_compute_max_time
  !
  subroutine stretch_get_fkpar ( o, rv_stretch, rw_fac, sq_fac, t1_out)
    !
    !  retrieve the info for FKMIG
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    real,                     intent(  out) :: rv_stretch
    real,                     intent(  out) :: rw_fac
    real,                     intent(  out) :: sq_fac
    real,                     intent(  out) :: t1_out
    !
    rv_stretch  = o%rv_stretch
    rw_fac      = o%rw_fac
    sq_fac      = o%sq_fac
    t1_out      = o%t1_out
    !
    return
    !
  end subroutine stretch_get_fkpar
  !
  subroutine stretch_map_compute_time ( &
                                        nt_map, t0_map, dt_map, &
                                        lt_inp, rv_inp, rf_inp, ra_inp, &
                                        lt_out, rv_out, rf_out, ra_out, &
                                        lt_map, rt_map, ra_map, &
                                        i_err &
                                       )
    !
    ! Compute the stretch mapping function.
    ! There may be problems when o%t0_map is > 0.
    ! The map may not be smooth in this situation!
    ! rf_inp = integral of o%dt_map * time * rv_rms**2 for input data
    !
    integer,                  intent(in   ) :: nt_map
    real,                     intent(in   ) :: t0_map
    real,                     intent(in   ) :: dt_map
    integer,                  intent(in   ) :: lt_inp
    real,                     intent(in   ) :: rv_inp ( : )
    real,                     intent(in   ) :: rf_inp ( : )
    real,                     intent(in   ) :: ra_inp ( : )
    integer,                  intent(in   ) :: lt_out
    real,                     intent(in   ) :: rv_out ( : )
    real,                     intent(in   ) :: rf_out ( : )
    real,                     intent(in   ) :: ra_out ( : )
    integer,                  intent(  out) :: lt_map
    real,                     intent(  out) :: rt_map ( : )
    real,                     intent(  out) :: ra_map ( : )
    integer,                  intent(inout) :: i_err
    !
    integer                                 :: it_out
    integer                                 :: it_inp
    integer                                 :: jt_inp

    real                                    :: rt_min
    real                                    :: rt_max
    real                                    :: df
    !
    i_err = 0
    !
    !  for each output time point compute the input time value
    !  for which rf_inp(rt_map(it)) = rf_out(it)
    !
    lt_map = min ( nt_map, lt_inp, lt_out )
    !
    !write ( pc_get_lun(), ' ( &
    !& /, " stretch_map_compute_time ", &
    !& " nt_map=", i8, " lt_inp=", i8, " lt_out=", i8, " lt_map=", i8, &
    !& /, " mm inp v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
    !& /, " mm out v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
    !& /, " fl inp v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6, &
    !& /, " fl out v=",g12.6,1x,g12.6," f=",g12.6,1x,g12.6 &
    !& )') &
    !nt_map, lt_inp, lt_out, lt_map, &
    !minval(rv_inp), maxval(rv_inp), minval(rf_inp), maxval(rf_inp), &
    !minval(rv_out), maxval(rv_out), minval(rf_out), maxval(rf_out), &
    !rv_inp(1), rv_inp(lt_map), rf_inp(1), rf_inp(lt_map), &
    !rv_out(1), rv_out(lt_map), rf_out(1), rf_out(lt_map)
    !
    !  check input  integral to make sure it increases monotonicaly
    !
    call matfun_check_change ( 1, lt_map, rf_inp, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    !  check output integral to make sure it increases monotonicaly
    !
    call matfun_check_change ( 1, lt_map, rf_out, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    rt_min = t0_map
    !
    rt_max = ( lt_map - 1 ) * dt_map + t0_map
    !
    it_inp = 1
    !
    rt_map ( : ) = 0.
    !
    ra_map ( : ) = 0.
    !
    do_it_out : do it_out = 1, lt_map
      !
      xxif_rf_out_lt_rf_inp : if ( rf_out ( it_out ) .lt. rf_inp ( 1 ) ) then
        !
        ! smaller than the first value
        !
        rt_map ( it_out ) = rt_min
        !
        ra_map ( it_out ) = ra_out ( 1 ) / ra_inp ( 1 )
        !
      else if ( rf_out ( it_out ) .ge. rf_inp ( lt_map ) ) then
        !
        !  bigger than the last value
        !
        rt_map ( it_out ) = rt_max
        !
        ra_map ( it_out ) = ra_out ( lt_map ) / ra_inp ( lt_map )
        !
      else xxif_rf_out_lt_rf_inp 
        !
        do_jt_inp : do jt_inp = it_inp, lt_map - 1
          !
          if ( rf_inp ( jt_inp ) .ge. rf_out ( it_out ) ) exit
          !
          it_inp = jt_inp
          !
        end do do_jt_inp 
        !
        df = rf_inp(it_inp+1) - rf_inp(it_inp)
        !
        if ( df .eq. 0.) df = 1.
        !
        rt_map ( it_out ) = ( it_inp - 1 ) * dt_map + t0_map &
        + max(0.,min(1.,(rf_out ( it_out )-rf_inp(it_inp))/df)) * dt_map
        !
        !print'(" it=",i8,1x,i8," rf=",g12.6,1x,g12.6,1x,g12.6)', &
        !it_out, it_inp, rf_out(it_out),rf_inp(it_inp),rf_inp(it_inp+1)
        !
        ra_map ( it_out ) = 1.
        !
        if ( rf_out ( it_out ) .lt. rf_inp(it_inp) &
        .or. rf_out ( it_out ) .gt. rf_inp(it_inp+1) ) go to 996
        !
      end if xxif_rf_out_lt_rf_inp 
      !
    end do do_it_out 
    !
    return
    !
996 continue
    !
    call pc_error ( ' error in stretch_map_compute_time ' )
    !
    write ( pc_get_lun(), '( &
    & /, " error in stretch_map_compute_time", &
    & /, "  it_out=" ,i10,"  it_inp=" ,i10, &
    & /, " rf_out=" ,g12.4, "  rf_inp=" , g12.4, 1x, g12.4 &
    & )') &
    it_out, it_inp, rf_out ( it_out ), rf_inp(it_inp), rf_inp(it_inp+1)
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in stretch_map_compute_time change 2 ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in stretch_map_compute_time change 1 ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_map_compute_time ' )
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in stretch_map_compute_time ", &
    & /, " nt_map=", i8, " lt_inp=", i8, " lt_out=", i8, " lt_map=", i8 &
    & )') &
    nt_map, lt_inp, lt_out, lt_map
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_map_compute_time
  !
  subroutine stretch_map_apply ( o, hd_inp, tr_inp, i_err )
    !
    ! map the input trace to output stretched trace.
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    double precision,         intent(inout) :: hd_inp ( : )  
    real,                     intent(inout) :: tr_inp ( : )  
    integer,                  intent(inout) :: i_err
    !
    real                                    :: rt_map(o%nt_map)
    real                                    :: ra_map(o%nt_map)
    real                                    :: tr_out(o%nt_out)
    !
    integer                                 :: top_mute
    integer                                 :: bot_mute


    integer                                 :: it_out
    integer                                 :: it_inp_1
    integer                                 :: it_inp_2
    double precision                        :: ft_inp_1
    double precision                        :: ft_inp_2
    double precision                        :: ra_inp
    double precision                        :: rt_inp
    double precision                        :: rt_out
    double precision                        :: rt_inp_1
    double precision                        :: rt_map_1
    double precision                        :: dt_inp_map
    double precision                        :: da_inp_map
    integer                                 :: nt_map
    integer                                 :: i1_map
    integer                                 :: i2_map
    integer                                 :: it_max
    integer                                 :: i_order = -1
    !
    integer,                           save :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    call cpucount ( o%c, o%c_stretch_apply, 1 )
    !
    o%x0_inp = hd_inp ( o%hx_map  )
    !
    o%y0_inp = hd_inp ( o%hy_map  )
    !
    tr_out = 0.
    !
    xxif_opt_common_angle : if ( o%opt_common_angle ) then
      !
      call stretch_map_interpolate_p ( &
                                       o, o%x0_inp, o%y0_inp, &
                                       i_order, .true., .true., &
                                       o%lt_map, o%rt_map, o%ra_map, &
                                         nt_map,   rt_map,   ra_map &
                                     )
      !
      !
      ! set the point at which the stretching is stopped
      ! for opt_doppler_mute=yes this is the max in the stretch function
      !
      it_max = o%nt_inp
      !
      if ( o%opt_doppler_mute ) it_max = min ( it_max, &
      nint ( ( maxval ( rt_map ) - o%t0_inp ) / o%dt_inp ) + 1 )
      !
      ! for each output sample determine the two input time samples
      ! which surround it
      !
      do_it_out_1 : do it_out = 1, o%nt_out
        !
        rt_out = ( it_out - 1 ) * o%dt_out + o%t0_out
        !
        ! get the two map times around this output time
        !
        i1_map = o%i1_map ( it_out ) 
        !
        i2_map = o%i2_map ( it_out ) 
        !
        xxif_image : if ( i1_map .ge. 1 .and. i1_map .le. nt_map  &
                    .and. i2_map .ge. 1 .and. i2_map .le. nt_map ) then
          !
          rt_map_1 = ( i1_map - 1 ) * o%dt_map + o%t0_map
          !
          dt_inp_map = ( rt_map ( i2_map ) - rt_map ( i1_map ) ) / o%dt_map
          !
          da_inp_map = ( ra_map ( i2_map ) - ra_map ( i1_map ) ) / o%dt_map
          !
          ! compute the correct input time for this output time
          !
          rt_inp = rt_map ( i1_map ) + ( rt_out - rt_map_1 ) * dt_inp_map
          !
          ra_inp = ra_map ( i1_map ) + ( rt_out - rt_map_1 ) * da_inp_map
          !
          ! if this input sample is within the input range map it to the
          ! output sample otherwise leave the output sample zero
          !
          it_inp_1 = nint ( ( rt_inp - o%t0_inp ) / o%dt_inp ) + 1
          !
          if ( it_inp_1 .lt. 1 ) cycle
          !
          if ( it_inp_1 .gt. o%nt_inp ) go to 1
          !
          rt_inp_1 = ( it_inp_1 - 1 ) * o%dt_inp - o%t0_inp
          !
          it_inp_2 = min ( o%nt_inp, it_inp_1 + 1 ) 
          !
          ft_inp_2 = &
          max ( 0.0d0, min ( 1.d0, ( rt_inp - rt_inp_1 ) / o%dt_inp ) ) 
          !
          ft_inp_1 = 1.0d0 - ft_inp_2
          !
          tr_out ( it_out ) = ra_inp * &
          ( ft_inp_1 * tr_inp ( it_inp_1 ) + ft_inp_2 * tr_inp ( it_inp_2 ) ) 
          !
          !if ( it_inp_1 .ge. o%nt_inp ) &
          !if ( mod ( o%i_trin, 50 ) .eq. 1 ) &
          !print'(" ww2 ", 1x, i5, 1x, i5, 1x, i5, 1x, i5, 1x, i5, &
          !& 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 )', &
          !o%ipn, o%i_trin, it_out, it_inp_1, it_inp_2, &
          !tr_out(it_out), tr_inp(it_inp_1), tr_inp(it_inp_2), ra_inp
          !
          ! if this output point has reached the end of the input trace
          ! stop stretching from input to output
          !
          if ( it_inp_1 .ge. it_max ) go to 1
          !
        end if xxif_image 
        !
      end do do_it_out_1 
      !
  1 continue
      !
    else xxif_opt_common_angle 
      !
      call stretch_map_interpolate ( &
                                     o, o%x0_inp, o%y0_inp, &
                                     i_order, .true., .true., &
                                     o%lt_map, o%rt_map, o%ra_map, &
                                       nt_map,   rt_map   &
                                   )
      !
      ! for each output sample determine the two input time samples
      ! which surround it
      !
      do_it_out : do it_out = 1, o%nt_out
        !
        rt_out = ( it_out - 1 ) * o%dt_out + o%t0_out
        !
        ! get the two map times around this output time
        !
        i1_map = max ( 1, o%i1_map ( it_out ) )
        !
        i2_map = max ( 1, o%i2_map ( it_out ) )
        !
        i1_map = min ( o%nt_map, i1_map )
        !
        i2_map = min ( o%nt_map, i2_map )
        !
        rt_map_1 = (i1_map-1)*o%dt_map + o%t0_map
        !
        dt_inp_map = ( rt_map(i2_map) - rt_map(i1_map) ) / o%dt_map
        !
        ! compute the correct input time for this output time
        !
        rt_inp = rt_map(i1_map) + ( rt_out - rt_map_1 ) * dt_inp_map
        !
        ! if this input sample is within the input range map it to the
        ! output sample otherwise leave the output sample zero
        !
        it_inp_1 = nint((rt_inp - o%t0_inp )/o%dt_inp) + 1
        !
        if ( it_inp_1<1 .or. it_inp_1>o%nt_inp) cycle
        !
        rt_inp_1 = (it_inp_1 - 1)*o%dt_inp - o%t0_inp
        !
        it_inp_2 = min(o%nt_inp,it_inp_1 + 1)
        !
        ft_inp_2 = max(0.0d0,min(1.d0,(rt_inp - rt_inp_1)/o%dt_inp))
        !
        ft_inp_1 = 1.0d0 - ft_inp_2
        !
        tr_out ( it_out ) = ft_inp_1 * tr_inp(it_inp_1) &
                          + ft_inp_2 * tr_inp(it_inp_2)
        !
        !if ( i_call .eq. 1    .or. i_call .eq. 1625 &
        !.or. i_call .eq. 1626 .or. i_call .eq. 3250 ) &
        !print'(" ww2 ", 1x, i5, 1x, i5, 1x, i5, 1x, i5, 1x, i5, &
        !& 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 )', &
        !o%ipn, o%i_trin, it_out, it_inp_1, it_inp_2, &
        !tr_out ( it_out ), tr_inp(it_inp_1), tr_inp(it_inp_2), 0.
        !
      end do do_it_out
      !
    end if xxif_opt_common_angle 
    !
    o%top_mute = hd_inp ( hdr_top_mute )
    !
    o%bot_mute = hd_inp ( hdr_bottom_mute )
    !
    top_mute = nint(o%top_mute)
    !
    bot_mute = nint(o%bot_mute)
    !
    !print'(" a i=",i5," c=",i8,&
    !& " tm=",i6," bm=",i6," tr=",g12.6," rt=",g12.6,1x,g12.6)',&
    !o%ipn, i_call, top_mute, bot_mute, &
    !maxval(abs(tr_inp)),minval(rt_map),maxval(rt_map)
    !
    !if ( o%i_trin .eq. 1 ) &
    !write ( pc_get_lun(),' ( &
    !& /, " stretch_map_apply ipn=", i6, " i_trin=", i6, &
    !& /, " lt_map=", i8, " top_mute=", i6, " bot_mute=", i6, &
    !& /, " nt_map=", i8, " t0_map=", f10.4, " dt_map=", f10.4, &
    !& /, " lt_inp=", i8, " t0_inp=", f10.4, " dt_inp=", f10.4, &
    !& /, " nt_out=", i8, " t0_out=", f10.4, " dt_out=", f10.4 &
    !& ) ' ) &
    !o%ipn, o%i_trin, &
    !nt_map, nint(o%top_mute), nint(o%bot_mute), &
    !o%nt_map, o%t0_map, o%dt_map, &
    !o%nt_inp, o%t0_inp, o%dt_inp, &
    !o%nt_out, o%t0_out, o%dt_out
    !
    !print'(" qq0 n=",i8," t=",g12.6,1x,g12.6," a=", g12.6, 1x, g12.6)', &
    !nt_map, minval(rt_map), maxval(rt_map), minval(ra_map), maxval(ra_map)
    !
    tr_inp = 0
    !
    !  map the top mute
    !
    call stretch_map_mute ( o, o%top_mute, rt_map, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! map the bottom mute
    !
    call stretch_map_mute ( o, o%bot_mute, rt_map, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! apply the top mute
    !
    top_mute = max(1,min(o%nt_out,nint(o%top_mute)))
    !
    tr_out(1:top_mute-1) = 0.0
    !
    ! apply the bottom mute
    !
    bot_mute = min(o%nt_out,nint(o%bot_mute))
    !
    tr_out(bot_mute+1:o%nt_out) = 0.0
    !
    tr_inp ( 1 : o%nt_out ) = tr_out ( 1 : o%nt_out ) 
    !
    hd_inp ( hdr_top_mute    ) = o%top_mute 
    !
    hd_inp ( hdr_bottom_mute ) = o%bot_mute 
    !
    !if ( mod ( o%i_trin, 10 ) .eq. 1 ) &
    !write ( pc_get_lun(),' ( &
    !& /, " ww0 p=", i6, " i=", i6, &
    !& " n=", i8, " t=", i6, " b=", i6, "m=",g12.6,1x,g12.6," tr=",g12.6&
    !& ) ' ) &
    !o%ipn, o%i_trin, &
    !nt_map, nint(o%top_mute), nint(o%bot_mute), &
    !minval(rt_map), maxval(rt_map), maxval(abs(tr_inp))
    !
    !print'(" b i=",i5," c=",i8,&
    !& " tm=",i6," bm=",i6," tr=",g12.6," rt=",g12.6,1x,g12.6)',&
    !o%ipn, i_call, top_mute, bot_mute, &
    !maxval(abs(tr_out)),minval(rt_map),maxval(rt_map)
    !
1999 continue
    !
    call cpucount ( o%c, o%c_stretch_apply, 2 )
    !
    return
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_map_apply during stretch_map_mute ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_map_apply ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine stretch_map_apply
  !
  subroutine stretch_map_mute ( o, rt_mute, rt_map, i_err )
    !
    ! compute stretched mute time.
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    double precision,         intent(inout) :: rt_mute
    real,                     intent(inout) :: rt_map(o%nt_map)
    integer,                  intent(inout) :: i_err
    !
    integer                                 :: it_map
    real                                    :: dt
    integer,                           save :: i_call = 0
    i_call = i_call + 1
    !
    !  convert from sample number to time
    rt_mute = (max(0,min(o%nt_inp + 1,nint(rt_mute))) - 1)*o%dt_inp + o%t0_inp
    !
    if ( rt_mute < rt_map(1) ) then
      rt_mute = o%t0_map
    else if ( rt_mute > rt_map(o%nt_map) ) then
      rt_mute = (o%nt_map - 1)*o%dt_map + o%t0_map
    else
    !
      do it_map = 1, o%nt_map - 1
        if ( rt_map(it_map+1) < rt_mute) cycle
        dt = rt_map(it_map+1) - rt_map(it_map)
        if ( dt .eq. 0.) dt = 1.
        rt_mute = (it_map - 1)*o%dt_map + o%t0_map + o%dt_map*        &
          (rt_mute - rt_map(it_map))/dt
          !
        go to 1
        !
      end do
      !
      write (pc_get_lun(), "(/,' error in stretch_map_mute')")
      i_err = -1
    return
    !
    1   continue
    end if
    !
    !  convert back to sample number
    !
    rt_mute = max(0,min(o%nt_out + 1,nint((rt_mute - o%t0_out)      &
             /o%dt_out)))
    !
    !
    return
    !
  end subroutine stretch_map_mute
  !
  subroutine stretch_map_interpolate ( &
                                       o, x0_inp, y0_inp, &
                                  degree_smooth, use_lt_map, interpolate_amp, &
                                       lt_map, rt_map, ra_map, &
                                       lt_out, rt_out )
    !
    !    Interpolate the mapping time/velocity to this x,y location
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    double precision,         intent(in   ) :: x0_inp
    double precision,         intent(in   ) :: y0_inp
    integer,                  intent(in   ) :: degree_smooth
    logical,                  intent(in   ) :: use_lt_map
    logical,                  intent(in   ) :: interpolate_amp
    integer,                  intent(in   ) :: lt_map ( :,: )
    real,                     intent(in   ) :: rt_map ( :,:,: )
    real,                     intent(in   ) :: ra_map ( :,:,: )
    integer,                  intent(  out) :: lt_out
    real,                     intent(  out) :: rt_out ( : )
   !real,                     intent(  out) :: ra_out ( : )
    !
    integer                                 :: iv_pol, it_map
    integer                                 :: ix, iy   
    integer                                 :: ix_map_1, ix_map_2
    integer                                 :: iy_map_1, iy_map_2
    !
    real                                    :: fx_map_1, fx_map_2
    real                                    :: fy_map_1, fy_map_2
    real                                    :: x1, y1
    real                                    :: rx_loc, ry_loc
    double precision                        :: weight
    !
    ! if order of fit is < 0 , we do linear inerpolation.
    ! if order of fit is > = 0 , we do least squares polynomial fitting
    ! to the velocity.
    !
    lt_out = o%nt_map
    !
    rx_loc = x0_inp
    ry_loc = y0_inp
    !
    xxif_degree_smooth_0 : if ( degree_smooth .ge. 0 ) then
      !
      x1 = max(o%x0_map,min((o%nx_map - 1)*o%dx_map + o%x0_map,real(x0_inp)))
      !
      if ( x1 .eq. 0.) x1 = 1.e-15
      !
      y1 = max(o%y0_map,min((o%ny_map - 1)*o%dy_map + o%y0_map,real(y0_inp)))
      !
      if ( y1 .eq. 0.) y1 = 1.e-15
      !
      rt_out(:o%nt_map) = 0.
      !
      do_iv_pol : do iv_pol = 1, o%nv_pol
        !
        weight = 1.d0*x1**o%rx_pol(iv_pol)*y1**o%ry_pol(iv_pol)
        !
        ix = mod ( iv_pol - 1, o%nx_map ) + 1
        !
        iy = ( iv_pol - 1 ) / o%nx_map + 1
        !
        do_it_map_1 : do it_map = 1, o%nt_map
          !
          rt_out(it_map) = 1.d0 * rt_out(it_map)                            &
                         + 1.d0 * rt_map(it_map,ix,iy) * weight
          !
        end do do_it_map_1
        !
      end do do_iv_pol 
      !
    else xxif_degree_smooth_0 
      !
      !  determine the 2 x and y values to interpolate between
      !
      call interpolate_find_index_h1 ( &
                                        o%nx_map, o%x0_map, o%dx_map, rx_loc, &
                                        ix_map_1, ix_map_2, fx_map_1, fx_map_2 &
                                      ) 
      !
      call interpolate_find_index_h1 ( &
                                        o%ny_map, o%y0_map, o%dy_map, ry_loc, &
                                        iy_map_1, iy_map_2, fy_map_1, fy_map_2 &
                                      ) 
      !
      !  interpolate each point
      !
      do_it_map_2 : do it_map = 1, o%nt_map
        !
        rt_out(it_map) =                                          &
          1.d0*fx_map_1*fy_map_1*rt_map(it_map,ix_map_1,iy_map_1) &
        + 1.d0*fx_map_2*fy_map_1*rt_map(it_map,ix_map_2,iy_map_1) &
        + 1.d0*fx_map_1*fy_map_2*rt_map(it_map,ix_map_1,iy_map_2) &
        + 1.d0*fx_map_2*fy_map_2*rt_map(it_map,ix_map_2,iy_map_2)
        !
      end do do_it_map_2 
      !
    end if xxif_degree_smooth_0 
    !
    return
    !
  end subroutine stretch_map_interpolate
  !
  subroutine stretch_map_interpolate_p ( &
                                        o, &
                                        x0_inp, y0_inp, &
                                        degree_smooth, &
                                        use_lt_map, interpolate_amp, &
                                        lt_map, rt_map, ra_map, &
                                        lt_out, rt_out, ra_out  &
                                      ) 
    !
    ! Interpolate the mapping time/velocity to this x,y location
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    double precision,         intent(in   ) :: x0_inp
    double precision,         intent(in   ) :: y0_inp
    integer,                  intent(in   ) :: degree_smooth
    logical,                  intent(in   ) :: use_lt_map
    logical,                  intent(in   ) :: interpolate_amp
    integer,                  intent(in   ) :: lt_map ( :,: )
    real,                     intent(in   ) :: rt_map ( :,:,: )
    real,                     intent(in   ) :: ra_map ( :,:,: )
    integer,                  intent(  out) :: lt_out
    real,                     intent(  out) :: rt_out ( : )
    real,                     intent(  out) :: ra_out ( : )
    !
    integer                                 :: iv_pol
    integer                                 :: it_map
    integer                                 :: ix_map
    integer                                 :: iy_map
    integer                                 :: ix_map_1
    integer                                 :: ix_map_2
    real                                    :: fx_map_1
    real                                    :: fx_map_2
    integer                                 :: iy_map_1
    integer                                 :: iy_map_2
    real                                    :: fy_map_1
    real                                    :: fy_map_2
    real                                    :: rx_map
    real                                    :: ry_map
    real                                    :: rx_loc
    real                                    :: ry_loc
    double precision                        :: weight
    double precision                        :: rf_map_11
    double precision                        :: rf_map_21
    double precision                        :: rf_map_12
    double precision                        :: rf_map_22
    !
    ! if order of fit is .lt. 0 , we do linear inerpolation.
    ! if order of fit is .gt. 0 , we do least squares polynomial fitting
    ! to the velocity.
    !
    call cpucount ( o%c, o%c_stretch_terp, 1 )
    !
    rx_loc = x0_inp
    !
    ry_loc = y0_inp
    !
    lt_out = o%nt_map
    !
    rt_out ( :o%nt_map ) = 0.
    !
    if ( interpolate_amp ) &
    ra_out ( :o%nt_map ) = 1.
    !
    xxif_degree_smooth : if ( degree_smooth .ge. 0 ) then
      !
      rx_map = max ( o%x0_map, min ( o%x1_map, real ( x0_inp ) ) ) 
      !
      if ( rx_map .eq. 0. ) rx_map = 1.e-15
      !
      ry_map = max ( o%y0_map, min ( o%y1_map, real ( y0_inp ) ) ) 
      !
      if ( ry_map .eq. 0. ) ry_map = 1.e-15
      !
      xxif_use_lt_map : if ( use_lt_map ) then
        !
        do_iv_pol_1 : do iv_pol = 1, o%nv_pol
          !
          ix_map = mod ( iv_pol - 1, o%nx_map ) + 1
          !
          iy_map = ( iv_pol - 1 ) / o%nx_map + 1
          !
          lt_out = min ( lt_out, lt_map ( ix_map, iy_map ) ) 
          !
        end do do_iv_pol_1 
        !
      end if xxif_use_lt_map 
      !
      do_iv_pol_2 : do iv_pol = 1, o%nv_pol
        !
        weight = 1.d0 * rx_map ** o%rx_pol ( iv_pol ) &
                      * ry_map ** o%ry_pol ( iv_pol ) 
        !
        ix_map = mod ( iv_pol - 1, o%nx_map ) + 1
        !
        iy_map = ( iv_pol - 1 ) / o%nx_map + 1
        !
        xxif_interpolate_amp_1 : if ( interpolate_amp ) then
          !
          do_it_map_1 : do it_map = 1, lt_out
            !
            rt_out ( it_map ) = 1.d0 * rt_out ( it_map ) &
            + 1.d0 * rt_map ( it_map, ix_map, iy_map ) * weight
            !
            ra_out ( it_map ) = 1.d0 * ra_out ( it_map ) &
            + 1.d0 * ra_map ( it_map, ix_map, iy_map ) * weight
            !
!print'(" i0=",i8," it=",i8," rt=",g12.6,1x,g12.6,1x,g12.6)',&
!iv_pol, it_map, rt_out ( it_map ), rt_map ( it_map, ix_map, iy_map ), weight
            !
          end do do_it_map_1 
          !
        else xxif_interpolate_amp_1 
          !
          do_it_map_2 : do it_map = 1, lt_out
            !
            rt_out ( it_map ) = 1.d0 * rt_out ( it_map ) &
            + 1.d0 * rt_map ( it_map, ix_map, iy_map ) * weight
            !
!print'(" j0=",i8," it=",i8," rt=",g12.6,1x,g12.6,1x,g12.6)',&
!iv_pol, it_map, rt_out ( it_map ), rt_map ( it_map, ix_map, iy_map ), weight
            !
          end do do_it_map_2 
          !
        end if xxif_interpolate_amp_1 
        !
      end do do_iv_pol_2
      !
    else xxif_degree_smooth 
      !
      !  determine the 2 x and y values to interpolate between
      !
      call interpolate_find_index_h1  ( &
                                        o%nx_map, o%x0_map, o%dx_map, rx_loc, &
                                        ix_map_1, ix_map_2, fx_map_1, fx_map_2 &
                                      ) 
      !
      call interpolate_find_index_h1 ( &
                                        o%ny_map, o%y0_map, o%dy_map, ry_loc, &
                                        iy_map_1, iy_map_2, fy_map_1, fy_map_2 &
                                      ) 
      !
      if ( use_lt_map ) &
      lt_out = min ( lt_out, &
                     lt_map ( ix_map_1 ,iy_map_1 ), &
                     lt_map ( ix_map_2 ,iy_map_1 ), &
                     lt_map ( ix_map_1 ,iy_map_2 ), &
                     lt_map ( ix_map_2 ,iy_map_2 ) )
      !
      rf_map_11 = 1.d0 * fx_map_1 * fy_map_1 
      rf_map_21 = 1.d0 * fx_map_2 * fy_map_1 
      rf_map_12 = 1.d0 * fx_map_1 * fy_map_2 
      rf_map_22 = 1.d0 * fx_map_2 * fy_map_2 
      !
      !  interpolate each point
      !
      xxif_interpolate_amp_2 : if ( interpolate_amp ) then
        !
        do_it_map_3 : do it_map = 1, lt_out
          !
                        rt_out ( it_map ) = &
            rf_map_11 * rt_map ( it_map, ix_map_1 ,iy_map_1 ) &
          + rf_map_21 * rt_map ( it_map, ix_map_2 ,iy_map_1 ) &
          + rf_map_12 * rt_map ( it_map, ix_map_1 ,iy_map_2 ) &
          + rf_map_22 * rt_map ( it_map, ix_map_2 ,iy_map_2 ) 
          !
                        ra_out ( it_map ) = &
            rf_map_11 * ra_map ( it_map, ix_map_1 ,iy_map_1 ) &
          + rf_map_21 * ra_map ( it_map, ix_map_2 ,iy_map_1 ) &
          + rf_map_12 * ra_map ( it_map, ix_map_1 ,iy_map_2 ) &
          + rf_map_22 * ra_map ( it_map, ix_map_2 ,iy_map_2 ) 
          !
          !print'(" a i=",i5, " rt=",5(1x,g12.6))',&
          !it_map, rt_out ( it_map ), &
          !rt_map ( it_map, ix_map_1 ,iy_map_1 ), &
          !rt_map ( it_map, ix_map_2 ,iy_map_1 ), &
          !rt_map ( it_map, ix_map_1 ,iy_map_2 ), &
          !rt_map ( it_map, ix_map_2 ,iy_map_2 ) 
          !
        end do do_it_map_3 
        !
      else xxif_interpolate_amp_2 
        !
        do_it_map_4 : do it_map = 1, lt_out
          !
                        rt_out ( it_map ) = &
            rf_map_11 * rt_map ( it_map, ix_map_1 ,iy_map_1 ) &
          + rf_map_21 * rt_map ( it_map, ix_map_2 ,iy_map_1 ) &
          + rf_map_12 * rt_map ( it_map, ix_map_1 ,iy_map_2 ) &
          + rf_map_22 * rt_map ( it_map, ix_map_2 ,iy_map_2 ) 
          !
          !print'(" b i=",i5, " rt=",5(1x,g12.6))',&
          !it_map, rt_out ( it_map ), &
          !rt_map ( it_map, ix_map_1 ,iy_map_1 ), &
          !rt_map ( it_map, ix_map_2 ,iy_map_1 ), &
          !rt_map ( it_map, ix_map_1 ,iy_map_2 ), &
          !rt_map ( it_map, ix_map_2 ,iy_map_2 ) 
          !
        end do do_it_map_4 
        !
      end if xxif_interpolate_amp_2 
      !
      !print'(" qq3 lt_out=",7(1x,i6))', &
      !lt_out, &
      !lt_map ( ix_map_1 ,iy_map_1 ), &
      !lt_map ( ix_map_2 ,iy_map_1 ), &
      !lt_map ( ix_map_1 ,iy_map_2 ), &
      !lt_map ( ix_map_2 ,iy_map_2 ), & 
      !ix_map_1, iy_map_1
      !
    end if xxif_degree_smooth 
    !
    call cpucount ( o%c, o%c_stretch_terp, 2 )
    !
    return
    !
  end subroutine stretch_map_interpolate_p
  !
  subroutine stretch_interpolate_i_to_r ( &
nt_inp, rt_inp, rv_inp, nt_out, t0_out , dt_out, rv_out )
    !
    ! interpoalte from an irregularly sampled array to a regular grid
    !
    integer,                  intent(in   ) :: nt_inp
    real,                     intent(in   ) :: rt_inp(nt_inp)
    real,                     intent(in   ) :: rv_inp(nt_inp)
    integer,                  intent(in   ) :: nt_out
    real,                     intent(in   ) :: t0_out
    real,                     intent(in   ) :: dt_out
    real,                     intent(  out) :: rv_out(nt_out)
    !
    integer                                 :: i_err
    integer                                 :: jt_inp, it_out    , it_inp_1 
    integer                                 :: it_inp_2
    real                                    :: rt_out, dt, dv
    !
    i_err = 0
    !
    !  check time to make sure it increases monotonicaly
    call matfun_check_change (1, nt_inp, rt_inp, i_err )
    if ( i_err .eq. 0 ) then
    !
      it_inp_1 = 1
      do it_out = 1, nt_out
        rt_out = (it_out - 1)*dt_out + t0_out
        !
        if ( rt_out .le. rt_inp(1) ) then
          it_inp_1 = 1
          it_inp_2 = 1
        else if ( rt_out .ge. rt_inp(nt_inp) ) then
          it_inp_1 = nt_inp
          it_inp_2 = nt_inp
        else
          do jt_inp = it_inp_1, nt_inp - 1
            if ( rt_inp(jt_inp) .ge. rt_out) exit
            it_inp_1 = jt_inp
          end do
          it_inp_2 = it_inp_1 + 1
        end if
        !
        dv = rv_inp(it_inp_2) - rv_inp(it_inp_1)
        dt = rt_inp(it_inp_2) - rt_inp(it_inp_1)
        if ( dt .eq. 0.) dt = 1.
        !
        rv_out ( it_out ) = max(min(rv_inp(it_inp_1),rv_inp(it_inp_2)),      &
          min(max(rv_inp(it_inp_1),rv_inp(it_inp_2)),rv_inp(it_inp_1)     &
          +(rt_out-rt_inp(it_inp_1))*dv/dt))
      end do
      !
    return
    !
    end if
    !
    write ( pc_get_lun(), '( &
    & " error in stretch_interpolate_i_to_rstretch_interpolate " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_interpolate_i_to_r
  !
  subroutine stretch_compute_polynomial ( o ) 
    !
    ! determine the allowed polynomial factors for order str_order
    ! nv_pol.....total number of polynomial factors to use
    ! rx_pol ( i ) ..exponent of x for the ith polynomial factor
    ! ry_pol ( i ) ..exponent of y for the ith polynomial factor
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !



    integer                                 :: ix_map
    integer                                 :: iy_map


    !
    write ( pc_get_lun(),' ( /," stretch_compute_polynomial",/, &
    & " degree_smooth=",i10," nv_map=",i10,"  mv_pol=",   i10,/, &
    & " nx_map=",i10," x0_map=",f10.2," dx_map=",   f10.4,/, &
    & " ny_map=",i10," y0_map=",f10.2," dy_map=",   f10.4,/, &
    & " nt_map=",i10 ) ' ) o%degree_smooth2, o%nv_map, o%mv_pol, &
    o%nx_map, o%x0_map, o%dx_map, o%ny_map, o%y0_map, o%dy_map, o%nt_map
    !
    xxif_use_degree_smooth : if ( o%degree_smooth2 .gt. o%nv_map - 1 ) then
      !
      o%degree_smooth2 = min ( o%degree_smooth2,o%nv_map - 1 ) 
      !
      write ( pc_get_lun(),' ( /," stretch_compute_polynomial:", &
      & " degree of fit reduced, new degree_smooth =",i10 ) ' ) &
      o%degree_smooth2
      !
    end if xxif_use_degree_smooth 
    !
    !  determine the number of polynomial coefficients
    !
    o%nv_pol = 0
    !
    do_iy_map : do iy_map = 1, min ( o%degree_smooth2 + 1,o%ny_map ) 
      !
      do_ix_map : do ix_map = 1, min ( o%degree_smooth2 + 1,o%nx_map ) 
        !
        if ( ix_map + iy_map - 2 .gt. o%degree_smooth2 ) cycle  do_iy_map 
        !
        o%nv_pol = o%nv_pol + 1
        o%rx_pol ( o%nv_pol ) = ix_map - 1
        o%ry_pol ( o%nv_pol ) = iy_map - 1
        !
        if ( o%nv_pol .le. o%nv_map ) cycle
        !
        write ( pc_get_lun(),' ( /,"  stretch_compute_polynomial:" , &
        & "  more polynomial factors than functions" ,/, &
        & "  discard x-order =" ,i10,"  y-order =" ,i10 ) ' ) ix_map, iy_map
        !
        o%nv_pol = o%nv_pol - 1
        !
      end do do_ix_map
      !
    end do do_iy_map
    !
    write ( pc_get_lun(),' ( /,"  stretch_compute_polynomial:" , &
    &"  number of polynomials =" ,i10 ) ' ) o%nv_pol
    !
    if ( o%nv_pol .gt. o%mv_pol ) go to 999
    !
    call stretch_compute_polynomial_1 ( o ) 
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(),' ( /," error in stretch_compute_polynomial:",/, &
    & " in number of polynomials",/," o%mv_pol=",i10," o%nv_pol=",i10 ) ' ) &
    o%mv_pol, o%nv_pol
    !
    call pc_error ( ' stretch: error in compute_polynomial ' ) 
    !
    return
    !
  end subroutine stretch_compute_polynomial
  !
  subroutine stretch_compute_polynomial_1 ( o ) 
    !
    ! determine the allowed polynomial factors for order str_order
    ! nv_pol.....total number of polynomial factors to use
    ! rx_pol ( i ) ..exponent of x for the ith polynomial factor
    ! ry_pol ( i ) ..exponent of y for the ith polynomial factor
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    double precision                        :: rt_sum
    integer                                 :: it_map, iv_pol
    integer                                 :: i0_map, jv_pol       , ix, iy 

    !
    double precision                        :: r_work ( o%nv_pol, o%nv_pol )
    double precision                        :: b ( o%nv_pol )
    double precision                        :: soln ( o%nv_pol )
    !
    logical                                 :: sing
    !
    ! shift the x,y location a small distance from 0 for stability
    !
    where ( o%rx_map ( :o%nv_map ) .eq. 0 ) 
            o%rx_map ( :o%nv_map ) = 0.01
    end where
    !
    where ( o%ry_map ( :o%nv_map ) .eq. 0 ) 
            o%ry_map ( :o%nv_map ) = 0.01
    end where
    !
    !  compute the expansion coeficients for the polynomial factors
    !  at each time level. use a least squares fitting proceedure.
    !  will solve :  a c = b   where c is the coeficient vector.
    !
    do_it_map : do it_map = 1 , o%nt_map  !loop over the time levels
      !
      b = 0.
      !
      soln = 0.
      !
      r_work = 0.0
      !
      do_iv_pol_1 : do iv_pol = 1 , o%nv_pol !first compute the vector b
        !
        rt_sum = 0.0
        i0_map = 0
        !
        do_iy_1 : do iy = 1,  o%ny_map
          !
          do_ix_1 : do ix = 1,  o%nx_map
            !
            i0_map = i0_map + 1
            !
            rt_sum = rt_sum  + 1.d0* o%rv_map ( it_map,ix,iy ) &
            * o%rx_map ( i0_map ) ** o%rx_pol ( iv_pol ) &
            * o%ry_map ( i0_map ) ** o%ry_pol ( iv_pol ) 
            !
          end do do_ix_1
          !
        end do do_iy_1
        !
        b ( iv_pol ) = rt_sum
        !
      end do do_iv_pol_1 
      !
      ! compute the symetric matrix a of size nv_pol
      !
      do_iv_pol_2 : do iv_pol = 1 , o%nv_pol
        !
        do_jv_pol_2 : do jv_pol = iv_pol , o%nv_pol
          !
          rt_sum = 0.
          i0_map = 0
          !
          do_iy_2 : do iy = 1,  o%ny_map
            !
            do_ix_2 : do ix = 1,  o%nx_map
              !
              i0_map = i0_map + 1
              !
              rt_sum = rt_sum &
              + ( 1.d0 &
                * o%rx_map ( i0_map ) ** o%rx_pol ( iv_pol ) &
                * o%ry_map ( i0_map ) ** o%ry_pol ( iv_pol ) ) &
              * ( o%rx_map ( i0_map ) ** o%rx_pol ( jv_pol ) &
                * o%ry_map ( i0_map ) ** o%ry_pol ( jv_pol ) ) 
              !
            end do do_ix_2 
            !
          end do do_iy_2 
          !
          r_work ( iv_pol, jv_pol ) = rt_sum
          !
          r_work ( jv_pol, iv_pol ) = rt_sum
          !
        end do do_jv_pol_2 
        !
      end do do_iv_pol_2 
      !
      xxif_nv_pol : if ( o%nv_pol .ge. 1 ) then
        !
        !  now solve for the expansion coeficients.
        !
        call  gausselim_lineqs ( o%nv_pol, r_work, soln, b, sing ) 
        !
        xxif_sing_err : if ( sing ) then
          !
          write ( pc_get_lun(), '( /, &
          & " error in stretch_compute_polynomial_1:", &
          & /, " during gausselim_lineqs call" ) ' ) 
          !
          call pc_error ( ' stretch: error in compute_polynomial_1 ' ) 
          !
          return
          !
        end if xxif_sing_err 
        !
      end if xxif_nv_pol
      !
      ! save the expansion coeficients for time level it_map.
      ! coeficients are overwritten into the velocity storage array.
      ! note that o%nv_pol is .le. nv_map
      !
      do_iv_pol_3 : do iv_pol = 1 , o%nv_pol
        !
        ix = mod ( iv_pol - 1,o%nx_map ) + 1
        !
        iy = ( iv_pol - 1 ) / o%nx_map + 1
        !
        o%rv_map ( it_map,ix,iy ) = soln ( iv_pol ) 
        !
      end do do_iv_pol_3 
      !
    end do do_it_map 
    !
    return
    !
  end subroutine stretch_compute_polynomial_1
  !
  subroutine stretch_map_compute_index ( o )
    !
    ! compute the map to output indexes
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    integer                                 :: it_out
    real                                    :: rt_out
    !
    do_it_out : do it_out = 1, o%nt_out
      !
      rt_out = ( it_out - 1 ) * o%dt_out + o%t0_out
      !
      !  get the two map times around this output time
      !
      o%i1_map ( it_out ) = min ( o%nt_out,                           &
           max ( 1, int ( ( rt_out - o%t0_map ) / o%dt_map ) + 1 ) )
      !
      o%i2_map ( it_out ) = min ( o%nt_out, o%i1_map ( it_out ) + 1 )
      !
    end do do_it_out
    !
    return
    !
  end subroutine stretch_map_compute_index
  !
  subroutine stretch_wrapup ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    if ( o%skip_wrapup) return
    o%skip_wrapup = .true.
    !
    return
    !
  end subroutine stretch_wrapup
  !
  subroutine stretch_map_compute_p ( o, hd_inp, i_err ) 
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    double precision,         intent(in   ) :: hd_inp ( : )
    integer,                  intent(inout) :: i_err
    !
    i_err = 0
    !
    call cpucount ( o%c, o%c_stretch_map_p, 1 )
    !
    xxif_opt_common_angle : &
    if ( o%opt_common_angle ) then
      !
      o%rp_inp = hd_inp ( hdr_offset ) * stretch_ray_scale
      !
      if ( o%opt_zero_angle ) o%rp_inp = 0.
      !
      o%ip_inp = nint ( ( o%rp_inp - o%p0_ray ) / o%dp_ray ) + 1
      !
      !if ( mod ( o%i_trin, 10 ) .eq. 1 ) &
      !print'(/," stretch_map_compute_p ", &
      !& " i_trin=",i8," ip_grp=",i8," ip_inp=",i8,&
      !& " rp_grp=",g12.6," rp_inp=",g12.6 &
      !& )',&
      !o%i_trin, o%ip_grp, o%ip_inp, o%rp_grp, o%rp_inp
      !
      xxif_new_group : if ( o%ip_inp .ne. o%ip_grp ) then
        !
        print'(" stretch_map_compute_p compute new table ", &
        & " i_trin=",i8," ip_grp=",i8," ip_inp=",i8,&
        & " rp_grp=",g12.6," rp_inp=",g12.6 &
        & )',&
        o%i_trin, o%ip_grp, o%ip_inp, o%rp_grp, o%rp_inp
        !
        o%rp_grp = o%rp_inp 
        !
        o%ip_grp = o%ip_inp 
        !
        call stretch_map_compute ( o )
        !
      end if xxif_new_group 
      !
    end if xxif_opt_common_angle 
    !
    call cpucount ( o%c, o%c_stretch_map_p, 2 )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
999 continue
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_map_compute_p 
  !
  subroutine stretch_map_compute_integral_n ( &
  o, lt_map, rv_map, rf_map, ra_map )
    !
    ! evaluate the integral needed for the stretch function
    ! i.e. the integral of rv_map*rv_map*time*delta_t
    ! Make sure we account for a posible time origin shift.
    ! velocity = v ( 1 ) for 0 < t < t0_map .
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    integer,                  intent(  out) :: lt_map
    real,                     intent(in   ) :: rv_map ( : )
    real,                     intent(  out) :: rf_map ( : )
    real,                     intent(  out) :: ra_map ( : )
    !
    ! local variables
    !
    lt_map = o%nt_map
    !
    xxif_common_angle : if ( o%opt_common_angle ) then
      !
      call stretch_map_compute_integral_p ( &
      o%rp_grp, lt_map, o%nt_map, o%t0_map, o%dt_map, rv_map, rf_map, ra_map ) 
      !
    else xxif_common_angle 
      !
      call stretch_map_compute_integral_0 ( &
      o%nt_map, o%t0_map, o%dt_map, rv_map, rf_map ) 
      !
      ra_map = 1.
      !
    end if xxif_common_angle 
    !
    return
    !
  end subroutine stretch_map_compute_integral_n
  !
  subroutine stretch_map_compute_integral_p ( &
  rp_grp, lt_map, nt_map, t0_map, dt_map, rv_map, rf_map, ra_map ) 
    !
    ! evaluate the integral needed for the stretch function
    ! i.e. the integral of rv_int/cos(angle)
    ! Make sure we account for a posible time origin shift.
    ! velocity = v ( 1 ) for 0 < t < t0_map .
    !
    real,                     intent(in   ) :: rp_grp
    integer,                  intent(inout) :: lt_map
    integer,                  intent(in   ) :: nt_map
    real,                     intent(in   ) :: t0_map
    real,                     intent(in   ) :: dt_map
    real,                     intent(in   ) :: rv_map ( : )
    real,                     intent(  out) :: rf_map ( : )
    real,                     intent(  out) :: ra_map ( : )
    !
    ! local variables
    !
    integer                                 :: it_map
    real                                    :: sin_angle
    real                                    :: cos_angle
    real                                    :: rt_tmp
    real                                    :: rf_tmp
    real                                    :: rt_map ( nt_map )
    !
    call matfun_line ( nt_map, rt_map, t0_map, dt_map )
    !
    rt_tmp = 0
    !
    rf_tmp = 0
    !
    lt_map = 0
    !
    rf_map ( 1:nt_map ) = 0.
    !
    ra_map ( 1:nt_map ) = 1.
    !
    !print'(" stretch_map_compute_integral_p angle=",g12.6, &
    !& " v=",g12.6, " p=",g12.6 )', &
    !asin(rp_grp * rv_map ( 1 ) ) * 90. / asin(1.), &
    !rv_map ( 1 ), rp_grp
    !
    do_it_map : do it_map = 1 , nt_map
      !
      sin_angle = rp_grp * rv_map ( it_map )
      !
      xif_sin_angle : if ( abs ( sin_angle ) .lt. 1. ) then
        !
        cos_angle = sqrt ( 1. - sin_angle ** 2 )
        !
        rf_tmp = rf_tmp &
             + ( rt_map ( it_map ) - rt_tmp ) &
               * rv_map ( it_map ) / cos_angle
        !
        rf_map ( it_map ) = rf_tmp
        !
        ra_map ( it_map ) = cos_angle 
        !
        rt_tmp = rt_map ( it_map ) 
        !
        lt_map = it_map
        !
      else xif_sin_angle 
        !
        go to 1
        !
      end if xif_sin_angle 
      !
    end do do_it_map 
    !
  1 continue
    !
    return
    !
  end subroutine stretch_map_compute_integral_p 
  !
  subroutine stretch_map_compute_integral_0 ( &
  nt_map, t0_map, dt_map, rv_map, rf_map)
    !
    !     evaluate the integral needed for the stretch function
    !     i.e. the integral of v*v*time*delta_t
    !     Make sure we account for a posible time origin shift.
    !     velocity = v(1) for 0 < t < t0 .
    !
    integer,                  intent(in   ) :: nt_map
    real,                     intent(in   ) :: t0_map
    real,                     intent(in   ) :: dt_map
    real,                     intent(in   ) :: rv_map ( : )
    real,                     intent(  out) :: rf_map ( : )
   !real,                     intent(  out) :: ra_map ( : )
    !
    ! local variables
    !
    integer                                 :: it_map
    real                                    :: dw1

    real                                    :: rf_tmp ( nt_map )
    !
    rf_tmp(:nt_map) = &
    (t0_map + (/(it_map,it_map=0,nt_map - 1)/)*dt_map)*dt_map &
                                              *rv_map(:nt_map)*rv_map(:nt_map)
    !
    !  compute derivative at first point for use in integration
    !
    dw1 = (-3.0*rf_tmp(1)) + 4.0*rf_tmp(2) - rf_tmp(3)
    !
    rf_tmp(1) = 0.5 * rf_tmp(1)
    !
    call rcsum ( nt_map, rf_tmp(1:), rf_map(1:) )
    !
    rf_map(1) = 0.5 * rv_map(1) ** 2 * t0_map ** 2
    !
    ! correct the integral for origin & end effects
    !
    rf_map(2:nt_map) = rf_map(1) + rf_map(2:nt_map) - 0.5 * rf_tmp(2:nt_map)
    !
    !  add correction terms for fourth order accuracy in integration
    !
    rf_map(2:nt_map-1) = &
    rf_map(2:nt_map-1) &
 + (rf_tmp(:nt_map-2)&
 - rf_tmp(3:nt_map) + dw1 ) * dt_map / 24.
    !
    rf_map(nt_map) = &
    rf_map(nt_map) &
 + ((-rf_tmp(nt_map-2)) &
 + 4.0 * rf_tmp(nt_map-1) &
 - 3.0 * rf_tmp(nt_map) ) * dt_map / 24.
    !
    return
    !
  end subroutine stretch_map_compute_integral_0
  !
  subroutine stretch_cpucount_create ( o, i_err )
    !
    ! setup the cpucount structure
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    integer,                  intent(inout) :: i_err ! 0 O.K. -1 err
    !
    ! - Local variables
    !
    i_err = 0
    !
    call cpucount_create ( o%c, 40 )
    !
    call cpucount_add ( o%c, 'stretch_total', o%c_stretch_total, i_err )
    call cpucount_add ( o%c, 'stretch_setup', o%c_stretch_setup, i_err )
    call cpucount_add ( o%c, 'stretch_comp',  o%c_stretch_comp,  i_err )
    call cpucount_add ( o%c, 'stretch_terp',  o%c_stretch_terp,  i_err )
    call cpucount_add ( o%c, 'stretch_apply', o%c_stretch_apply, i_err )
    call cpucount_add ( o%c, 'stretch_map_p', o%c_stretch_map_p, i_err )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call cpucount_print ( o%c, ' stretch_cpucount_create ' )
    !
    return
    !
  end subroutine stretch_cpucount_create 
  !
  subroutine stretch_velocity_size ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: i_err






    !
    !  stretch velocity function at x and y locations
    !
    !  read stretch velocity file and compute the required size arrays.
    !
    i_err = 0
    !
    o%vel_type = o%ref_type
    o%nv_map = 1
    o%nv_map   = 1
    !
    o%hx_map = 7
    o%nx_map = 1
    o%x0_map = 0.
    o%dx_map = 1.
    !
    o%hy_map = 8
    o%ny_map = 1
    o%y0_map = 0.
    o%dy_map = 1.
    !
    !  open and read the stretch velocity file
    !
    if ( o%use_pathname_vel ) &
    call velgrid_size_xy ( &
                           pc_get_lun(), 'stretch_velocity_size', &
                           o%pathname_vel, o%vel_type, &
                           o%nv_map, o%hx_map, o%hy_map, &
                           o%nx_map, o%x0_map, o%dx_map, &
                           o%ny_map, o%y0_map, o%dy_map, &
                           i_err &
                         )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_size during stretch_velocity_read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_size during velgrid_size_xy ' )
    !' error in stretch_velocity_size during stretch_velocity_open ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_velocity_size ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_velocity_size 
  !
  subroutine stretch_velocity_ref ( o )
    !
    ! check whether the reference function name is found or not.
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: iv_ref
    integer                                 :: it_lay
    integer                                 :: i_err
    !
    !  compute mapping time values with reference and stacking velocity files
    !
    iv_ref=0
    i_err = 0
    !
    o%hx_ref = hdr_midpoint_xgrid
    !
    o%hy_ref = hdr_midpoint_ygrid
    !
    o%rx_ref = 0.
    !
    o%ry_ref = 0.
    !
    o%hx_map = o%hx_ref 
    !
    o%hy_map = o%hy_ref 
    !
    xxif_use_pathname_ref : if ( o%use_pathname_ref ) then
      !
      !  open and read the reference velocity file
      !
      call stretch_velocity_open ( &
      o%v, o%pathname_ref, o%nv_ref, o%hx_ref, o%hy_ref, i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
      ! read the right velocity function
      !
      do_iv_ref : do iv_ref = 1, o%nv_ref
        !
        ! find and read the referenc function, convert to ref_type during read
        !
        call stretch_velocity_read ( &
        o%v, o%ref_type, o%vel_type, o%vel_name, &
        o%rx_ref, o%ry_ref, o%nt_lay, o%rt_lay, o%rv_lay, i_err )
        !
        if ( i_err .ne. 0 ) go to 997
        !
        if ( string_upper_compare ( o%velname_ref, 'FIRST' ) &
        .or. string_upper_compare ( o%velname_ref, o%vel_name ) ) go to 1
        !
      end do do_iv_ref
      !
      go to 996
      !
  1 continue
      !
      call stretch_velocity_close ( o%v )
      !
    else xxif_use_pathname_ref 
      !
      o%nt_lay = 2
      !
      o%rt_lay(1) = o%t0_map
      !
      o%rt_lay(2) = o%t1_map
      !
      o%rv_lay = o%vel_const
      !
    end if xxif_use_pathname_ref 
    !
    call velutil_generate ( o%ref_type, o%nt_lay, o%rt_lay, o%rv_lay, &
    o%tz_lay, o%rz_lay, o%rv_rms, o%rv_ave, o%rv_int, i_err )
    !
    if ( i_err .ne. 0 ) go to 995
    !
    ! interpolate the reference function to a uniform time grid
    !
    call stretch_interpolate_i_to_r ( &
    o%nt_lay, o%rt_lay, o%rv_lay, o%nt_map, o%t0_map, o%dt_map, o%rv_ref )
    !
    if ( i_err .ne. 0 ) go to 994
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /, " stretch_velocity_ref ipn=", i8, &
    & /, " pathname_ref=", a, &
    & /, " velname_ref =",  a, &
    & /, " vel_name    =",  a, &
    & /, " ref_type    =", a, &
    & /, " vel_type    =", a, &
    & /, " nv_ref=", i8," iv_ref=", i8, " nt_lay=", i8, &
    & /, " hx_ref=", i8, " rx_ref=", g12.6, &
    & /, " hy_ref=", i8, " ry_ref=", g12.6, &
    & /, " it_lay rt_lay rv_lay tz_lay rz_lay v_rms v_ave v_int " &
    & )') &
    o%ipn, &
    trim ( o%pathname_ref ), &
    trim ( o%velname_ref ), &
    trim ( o%vel_name), &
    trim ( o%ref_type ) , &
    trim ( o%vel_type ) , &
    o%nv_ref,   iv_ref, o%nt_lay, &
    o%hx_ref, o%rx_ref, o%hy_ref, o%ry_ref
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & 1x, i5,    1x, g10.4, 1x, g10.4, 1x, g10.4, &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
    & )') &
    ( it_lay, &
      o%rt_lay(it_lay), o%rv_lay(it_lay), &
      o%tz_lay(it_lay), o%rz_lay(it_lay), &
      o%rv_rms(it_lay), o%rv_ave(it_lay), o%rv_int(it_lay), &
      it_lay = 1 , o%nt_lay )
    !
    return
    !
994 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_ref during stretch_interpolate_i_to_r ' )
    !
995 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_ref during velutil_generate ' )
    !
996 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_ref could not find reference ', o%velname_ref )
    !
997 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_ref during stretch_velocity_read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_ref during stretch_velocity_open ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_velocity_ref ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_velocity_ref
  !
  subroutine stretch_velocity_map ( o )
    !
    !     Store velocity functions in coarse grid.
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    integer                                 :: iv_map
    integer                                 :: it_map
    integer                                 :: ix_map
    integer                                 :: iy_map 
    !
    integer                                 :: i_err
    integer                                 :: nt_str
    real                                    :: rt_str(max(o%nt_inp,o%nt_map))
    real                                    :: rv_str(max(o%nt_inp,o%nt_map))
    !
    i_err = 0
    !
    ! open and read the stretch velocity file
    !
    call stretch_velocity_open ( &
    o%v, o%pathname_vel, o%nv_map, o%hx_map, o%hy_map, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    xxif_not_use_pathname_ref : if ( .not. o%use_pathname_ref ) then
      !
      o%hx_ref = o%hx_map
      !
      o%hy_ref = o%hy_map
      !
    end if xxif_not_use_pathname_ref 
    !
    call stretch_velocity_hdr_check ( &
    o%hx_map, o%hy_map, o%hx_ref, o%hy_ref, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /, " stretch_velocity_map ipn=", i8, &
    & /, " pathname_vel=", a, &
    & /, " ref_type    =", a, &
    & /, " nv_map=", i8, &
    & /, " hx_map=", i8, &
    & /, " hy_map=", i8, &
    & /, " nt_map=",i8," t0_map=",g12.6," t1_map=",g12.6," dt_map=", g12.6, &
    & /, " iv_map ix_map iy_map rv_map " &
    & )') &
    o%ipn, &
    trim ( o%pathname_vel ), &
    trim ( o%ref_type ) , &
    o%nv_map, o%hx_map, o%hy_map, &
    o%nt_map, o%t0_map, o%t1_map, o%dt_map
    !
    ! read velocity functions, they are converted to ref_type during read
    !
    do_iv_map : do iv_map = 1, o%nv_map
      !
      rt_str = 0.0
      !
      rv_str = 0.0
      !
      call stretch_velocity_read ( &
      o%v, o%ref_type, o%vel_type, o%vel_name, &
      o%rx_map(iv_map), o%ry_map(iv_map), nt_str, rt_str, rv_str, i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
      ix_map = max(1,min(o%nx_map, &
      nint((o%rx_map(iv_map)-o%x0_map)/o%dx_map)+1))
      !
      iy_map = max(1,min(o%ny_map, &
      nint((o%ry_map(iv_map)-o%y0_map)/o%dy_map)+1))
      !
      call stretch_interpolate_i_to_r ( &
      nt_str, rt_str, rv_str, &
      o%nt_map, o%t0_map, o%dt_map, o%rv_map ( :, ix_map, iy_map ) )
      !
      ! scale the velocity by vel_scale
      !
      if ( o%opt_vel_scale ) &
      o%rv_map ( 1:o%nt_map, ix_map, iy_map ) = &
      o%rv_ref ( 1:o%nt_map                 ) * ( 1. - o%vel_scale ) &
    + o%rv_map ( 1:o%nt_map, ix_map, iy_map ) *        o%vel_scale 
      !
      !if ( iv_map .eq. 1) &
      !write ( pc_get_lun(), '( &
      !& " vv1 ", i8, 1x, g12.6, 1x, g12.6 &
      !& )') &
      !( it_map, rt_str ( it_map ), rv_str ( it_map ), &
      !  it_map = 1 , nt_str )
      !
      !if ( iv_map .eq. 1) &
      !write ( pc_get_lun(), '( &
      !& " vv2 ", i8, 1x, g12.6, 1x, g12.6 &
      !& )') &
      !( it_map, o%rv_ref ( it_map ), o%rv_map ( it_map, ix_map, iy_map ), &
      !  it_map = 1 , o%nt_map )
      !
      !write ( pc_get_lun(), '( &
      !& " vv3 ", i8, 1x, 11(g12.6,1x) &
      !& )') &
      !iv_map, ix_map, iy_map, &
      !( o%rv_map ( it_map, ix_map, iy_map ), it_map = 1 , o%nt_map / 10 )
      !
    end do do_iv_map 
    !
    call stretch_velocity_close ( o%v )
    !
    print'(" stretch_velocity_map ipn=",i8," nx_map=",i8," ny_map=",i8,&
    & /," nt_map=",i8," t0_map=",g10.4," dt_map=",g10.4)',&
    o%ipn, o%nx_map, o%ny_map, o%nt_map, o%t0_map, o%dt_map
    do_iy_map : do iy_map = 1 , o%ny_map, 10
    do_ix_map : do ix_map = 1 , o%nx_map, 10
    print'(1x,i5,1x,i5,12(1x,g10.4))',&
    ix_map,iy_map,&
    (o%rv_map(it_map,ix_map,iy_map),it_map=1,o%nt_map,o%nt_map/10)
    end do do_ix_map
    end do do_iy_map
    !
    return
    !
996 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_map during stretch_velocity_hdr_check ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_map during stretch_velocity_read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_map during stretch_velocity_open ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_velocity_map ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_velocity_map 
  !
  subroutine stretch_velocity_open ( &
  v_obj, pathname_vel, nv_map, hx_map, hy_map, i_err )
    !
    type ( velio_struct ),          pointer :: v_obj
    character(len=*),         intent(in   ) :: pathname_vel
    integer,                  intent(inout) :: nv_map
    integer,                  intent(inout) :: hx_map
    integer,                  intent(inout) :: hy_map
    integer,                  intent(inout) :: i_err ! 0 O.K. -1 err
    !
    character(len=len_132)                  :: crd_132
    !
    i_err = 0
    !
    ! open the file and get the size
    !
    call velio_open_read ( &
    v_obj, pathname_vel, nv_map, i_err, crd_132, hx_map, hy_map )
    !
    !print'(" aft velio_open_read nv=",i8," hx=",i8," hy=",i8," f=",a)', &
    !nv_map, hx_map, hy_map, trim(pathname_vel)
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    call pc_error ( ' error in stretch_velocity_open during velio_open_read ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_velocity_open ', pathname_vel )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_velocity_open 
  !
  subroutine stretch_velocity_close ( v_obj )
    !
    type ( velio_struct ),          pointer :: v_obj
    !
    if ( associated ( v_obj ) )  call velio_close ( v_obj )
    !
    return
    !
  end subroutine stretch_velocity_close 
  !
  subroutine stretch_velocity_read ( &
                                     v_obj, out_type, inp_type, vel_name, &
                                     rx_vel, ry_vel, nt_vel, rt_vel, rv_vel, &
                                     i_err &
                                   )

    !
    ! read a velocity function
    ! convert to out_type if out_type .ne. 'none' 
    ! if out_type .eq. 'none' do not convert, return type
    !
    type ( velio_struct ),          pointer :: v_obj
    !
    character(len=*),         intent(in   ) :: out_type
    character(len=*),         intent(inout) :: inp_type
    character(len=*),         intent(inout) :: vel_name
    real,                     intent(inout) :: rx_vel
    real,                     intent(inout) :: ry_vel
    integer,                  intent(inout) :: nt_vel
    real,                     intent(inout) :: rt_vel(:)
    real,                     intent(inout) :: rv_vel(:)
    integer,                  intent(inout) :: i_err
    !
    ! Local variables
    !
    character(len=len_132)                  :: crd_132
    !
    i_err = 0
    !
    rt_vel = 0.0
    !
    rv_vel = 0.0
    !
    ! read the next velocity function
    !
    !print'(" stretch_velocity_read bef velio_read_velfun size=",i8,1x,i8)', &
    !size(rt_vel,1), size(rv_vel,1)
    !
    call velio_read_velfun ( &
                             v_obj, rx_vel, ry_vel, &
                             nt_vel, rt_vel, rv_vel, &
                             i_err, crd_132, vel_name, inp_type &
                           )
    !
    !print'(" stretch_velocity_read aft velio_read_velfun size=",i8,&
    !& " t=",g12.6,1x,g12.6," v=",g12.6,1x,g12.6)', &
    !nt_vel, &
    !minval(rt_vel(1:nt_vel)), maxval(rt_vel(1:nt_vel)), &
    !minval(rv_vel(1:nt_vel)), maxval(rv_vel(1:nt_vel))
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! convert from input type to ref_type
    !
    call stretch_velocity_convert ( &
    inp_type, out_type, nt_vel, rt_vel, rv_vel, i_err )
    !
    !print'(" stretch_velocity_read aft stretch_velocity_convert size=",i8,&
    !& " t=",g12.6,1x,g12.6," v=",g12.6,1x,g12.6)', &
    !nt_vel, &
    !minval(rt_vel(1:nt_vel)), maxval(rt_vel(1:nt_vel)), &
    !minval(rv_vel(1:nt_vel)), maxval(rv_vel(1:nt_vel))
    !
    if ( i_err .ne. 0 ) go to 997
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_read during stretch_velocity_convert ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_read during velio_read_velfun ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_velocity_read ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_velocity_read
  !
  subroutine stretch_velocity_convert ( &
  inp_type, out_type, nt_vel, rt_vel, rv_vel, i_err )
    !
    ! convert a velocity function
    ! convert to out_type if out_type .ne. 'none' 
    ! if out_type .eq. 'none' do not convert, return type
    !
    character(len=*),         intent(inout) :: inp_type
    character(len=*),         intent(in   ) :: out_type
    integer,                  intent(inout) :: nt_vel
    real,                     intent(inout) :: rt_vel(:)
    real,                     intent(inout) :: rv_vel(:)
    integer,                  intent(inout) :: i_err
    !
    ! Local variables
    !
    real                                    :: rt_tmp(nt_vel)
    real                                    :: rv_tmp(nt_vel)
    !
    i_err = 0
    !
    !
    ! convert from input type to ref_type
    !
    xxif_convert : &
    if ( .not. string_upper_compare ( inp_type, out_type ) &
   .and. .not. string_upper_compare ( 'none' ,  out_type ) ) then
      !
      !print'(" qqq stretch_velocity_convert inp_type=",a8," out_type=",a8 )', &
      !trim ( inp_type ), trim ( out_type )
      !
      rt_tmp ( 1:nt_vel ) = rt_vel ( 1:nt_vel )
      rv_tmp ( 1:nt_vel ) = rv_vel ( 1:nt_vel )
      !
      call velutil_convert ( inp_type, nt_vel, rt_tmp, rv_tmp, &
                             out_type,         rt_vel, rv_vel, i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
    end if xxif_convert 
    !
    return
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_convert during velutil_convert ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_velocity_convert ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_velocity_convert
  !
  subroutine stretch_velocity_hdr_check ( hx_map, hy_map, hx_ref, hy_ref, i_err)
    !
    ! check the velocity header words
    !
    integer,                  intent(inout) :: hx_map
    integer,                  intent(inout) :: hy_map
    integer,                  intent(inout) :: hx_ref
    integer,                  intent(inout) :: hy_ref
    integer,                  intent(inout) :: i_err ! 0 O.K. -1 err
    !
    !
    i_err = 0
    !
    !
    if ( hx_map .ne. hx_ref ) go to 998
    !
    if ( hy_map .ne. hy_ref ) go to 997
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_hdr_check y hdr ref =', hy_ref, &
    ' should be =', hy_map )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in stretch_velocity_hdr_check x hdr ref =', hx_ref, &
    ' should be =', hx_map )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in stretch_velocity_hdr_check ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine stretch_velocity_hdr_check
  !
  subroutine stretch_mem_all ( o, i_err )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    integer,                  intent(inout) :: i_err
    !
    i_err  = 0
    !
    call memfun_sum_put ( 0 )
    call memfun_prn_put ( .true. )
    !
    call memfun_all ( o%rt_lay, o%nt_lay, 'rt_lay', i_err )
    call memfun_all ( o%tz_lay, o%nt_lay, 'tz_lay', i_err )
    call memfun_all ( o%rz_lay, o%nt_lay, 'rz_lay', i_err )
    call memfun_all ( o%rv_lay, o%nt_lay, 'rv_lay', i_err )
    call memfun_all ( o%rv_rms, o%nt_lay, 'rv_rms', i_err )
    call memfun_all ( o%rv_ave, o%nt_lay, 'rv_ave', i_err )
    call memfun_all ( o%rv_int, o%nt_lay, 'rv_int', i_err )
    call memfun_all ( o%rv_ref, o%nt_map, 'rv_ref', i_err )
    call memfun_all ( o%rx_pol, o%nv_map, 'rx_pol', i_err )
    call memfun_all ( o%ry_pol, o%nv_map, 'ry_pol', i_err )
    call memfun_all ( o%rx_map, o%nv_map, 'rx_map', i_err )
    call memfun_all ( o%ry_map, o%nv_map, 'ry_map', i_err )
    call memfun_all ( o%lt_map,           o%nx_map, o%ny_map, 'lt_map', i_err )
    call memfun_all ( o%rv_map, o%nt_map, o%nx_map, o%ny_map, 'rv_map', i_err )
    call memfun_all ( o%rt_map, o%nt_map, o%nx_map, o%ny_map, 'rt_map', i_err )
    call memfun_all ( o%ra_map, o%nt_map, o%nx_map, o%ny_map, 'ra_map', i_err )
    call memfun_all ( o%i1_map, o%nt_out, 'i1_map', i_err )
    call memfun_all ( o%i2_map, o%nt_out, 'i2_map', i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
999 continue
    !
    call pc_error ( ' Error during stretch_mem_all memory allocation ' ) 
    !
    return
    !
  end subroutine stretch_mem_all 
  !
  subroutine stretch_mem_del ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    call memfun_del ( o%rt_lay )
    call memfun_del ( o%tz_lay )
    call memfun_del ( o%rz_lay )
    call memfun_del ( o%rv_lay )
    call memfun_del ( o%rv_rms )
    call memfun_del ( o%rv_ave )
    call memfun_del ( o%rv_int )
    call memfun_del ( o%rv_ref )
    call memfun_del ( o%rx_pol )
    call memfun_del ( o%ry_pol )
    call memfun_del ( o%rx_map )
    call memfun_del ( o%ry_map )
    call memfun_del ( o%lt_map )
    call memfun_del ( o%rv_map )
    call memfun_del ( o%rt_map )
    call memfun_del ( o%ra_map )
    call memfun_del ( o%i1_map )
    call memfun_del ( o%i2_map )
    !
    return
    !
  end subroutine stretch_mem_del 
  !
  subroutine stretch_mem_nul ( o )
    !
    type ( stretch_struct ),  intent(inout) :: o ! stretch structure
    !
    call memfun_nul ( o%rt_lay )
    call memfun_nul ( o%tz_lay )
    call memfun_nul ( o%rz_lay )
    call memfun_nul ( o%rv_lay )
    call memfun_nul ( o%rv_rms )
    call memfun_nul ( o%rv_ave )
    call memfun_nul ( o%rv_int )
    call memfun_nul ( o%rv_ref )
    call memfun_nul ( o%rx_pol )
    call memfun_nul ( o%ry_pol )
    call memfun_nul ( o%rx_map )
    call memfun_nul ( o%ry_map )
    call memfun_nul ( o%lt_map )
    call memfun_nul ( o%rv_map )
    call memfun_nul ( o%rt_map )
    call memfun_nul ( o%ra_map )
    call memfun_nul ( o%i1_map )
    call memfun_nul ( o%i2_map )
    !
    return
    !
  end subroutine stretch_mem_nul 
  !
end module stretch_module
!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
