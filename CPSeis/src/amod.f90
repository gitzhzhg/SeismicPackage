!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- amod.f90 --------------------------------!!
!!------------------------------- amod.f90 --------------------------------!!
!!------------------------------- amod.f90 --------------------------------!!

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
! Name       : amod
! Category   : migrations
! Written    : 2004-03-12  by: Douglas Hanson
! Revised    : 2007-08-21  by: Douglas Hanson Add converted wave extrapolation.
! Maturity   : beta
! Purpose    : Anisotropic model description.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! Kirchhoff imaging velocity routines.
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
!
!  create the amod velocity structure
!  read a velocity model from disk. - this may have up to 5 components
!  defined by the file names in path_mod
!  or set to constant values
!
!        call amod_create ( o, z_units, i_err )
!
!  delete the amod velocity structure
!
!        call amod_delete ( o )
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
! 15  2007-08-21  Douglas Hanson Add converted wave extrapolation.
! 14  2006-09-21  Douglas Hanson Modify OPT_REFLECTIVITY.
! 13  2006-05-10  Douglas Hanson Add OPT_REFL.
! 12  2006-04-14  Douglas Hanson Register arrays.
! 11. 2006-01-10  B. Menger      Removed Unused Variables.
! 10. 2005-03-24  Douglas Hanson Add amod_stop.
!  9. 2005-01-31  R.S.Day        Added iso and ani elastic options. Added the
!                                amod_path_by_name, amod_path_n methods.
!  8. 2004-09-14  R.S.Day        Promoted to beta.
!  7  2004-04-22  R.S.Day        delete blank in gui for alignment
!  6  2004-04-07  Douglas Hanson Set OPT_VEL default to PATH.
!  5  2004-04-05  Douglas Hanson Add amod_put_process.
!  4  2004-03-24  Douglas Hanson Add title.
!  3  2004-03-22  Douglas Hanson Fix sensitivity.
!  2  2004-03-18  Douglas Hanson Add c_opt_aniso.
!  1  2004-03-12  Douglas Hanson Original version.
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



!-------------------------------------------------------------------------------
!<gui_def>
!`- Anisotropic velocity definition --------------------------------------------
!
! OPT_ANISO=`CCCCCCCCCC     OPT_CONVERTED=`CC 
! TITLE_MOD OPT_MOD CONST_MOD DEPTH_MOD GRAD_MOD  PATH_MOD
! `SSSSSSSSS`SSSSSSS`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSS`SSSSSSS`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSS`SSSSSSS`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSS`SSSSSSS`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSS`SSSSSSS`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!`------------------------------------------------------------------------------
!
!<PARMS TITLE_MOD_ARRAYSET[/XST/YST]>
!<PARMS PATH_MOD[/ML=128/XST]>
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="OPT_ANISO">
!<Tip> Velocity model file name. </Tip>
! Default = ISOTROPIC
! Allowed = ISOTROPIC    Isotropic time and depth migration.
! Allowed = ANISOTROPIC  Vertical anisotropic time and depth migration.
! Allowed = TILTED_BED   Tilted bed anisotropic depth migration.
! Allowed = ISOELASTIC   Vp, Vs, Denisty for E2D
! Allowed = ANIELASTIC   Vp, Vs, Denisty, epsilon, delta for E2D
!</Help>
!
!<Help KEYWORD="OPT_CONVERTED">
!<Tip> Converted wave option. </Tip>
! Default = NO
! Allowed = NO    Do not include a shear wave velocity for converted waves.
! Allowed = YES   Do     include a shear wave velocity for converted waves.
!</Help>
!
!<Help KEYWORD="TITLE_MOD">
!<Tip> Title of model coefficient. </Tip>
! Default = NONE
! Allowed = character
!</Help>
!
!<Help KEYWORD="OPT_MOD">
!<Tip> How each model coefficient is defined. </Tip>
! Default = NONE
! Allowed = character
!</Help>
!
!<Help KEYWORD="CONST_MOD">
!<Tip> Constant value for model coeffieicnt at depth DEPTH_MOD. </Tip>
! Default = 2000.
! Allowed = real
!</Help>
!
!<Help KEYWORD="DEPTH_MOD">
!<Tip> Depth from which graident is measured. </Tip>
! Default = 0.
! Allowed = real
!</Help>
!
!<Help KEYWORD="GRAD_MOD">
!<Tip> Gradient for mdoel coefficient. </Tip>
! Default = 0.
! Allowed = real
!</Help>
!
!<Help KEYWORD="PATH_MOD">
!<Tip> Name of model coefficient file. </Tip>
! Default = NONE
! Allowed = character
! The X_SIN coefficient is the sine of the anisotropic coefficient dip angle
! projected into the x,z plane.
! The Y_SIN coefficient is the sine of the anisotropic coefficient dip angle
! projected into the y,z plane.
! For a horizontal geometry x_sin = y_sin = 0.
! For a 30 degree dip from the horizontal in the x direction, x_sin = .5
! For a 45 degree dip from the horizontal in the x direction, x_sin = .7071
! The true dip of the anisotropic coefficient from the z axis is:
! asin ( sqrt ( 1. - x_sin**2 - y_sin**2 ) )
! If any PATH_MOD entry is other than NONE or CONST_MOD IS not equal to 0.
! AMOD assumes an anisotropic model.
! Then PATH_MOD(1) is the vZ values and the group velocity is
! 1/v(a)**2 = cos(a)**2/vz**2 + sin(a)**2/vx**2 * (1 + 2*ETA*cos(a)**2 )
! If TYPE_TABLE_CALC = TIME, PATH_MOD(1) is assumed to be the stacking velocity.
! Otherwise, PATH_MOD(1) is assumed to be the vertical interval velocity.
! If PATH_MOD(2) is NONE the steep dip anisotropic coefficient is CONST_MOD
!</Help>
!
!</HelpSection>

module amod_module
  !
  ! kirchoff velocity routines
  !
  use kfun_module
  use memfun_module
  use named_constants_module
  use pathcheck_module
  use pc_module
  use pcpsx_module
  use string_module
  !
  implicit  none
  !
  private
  !
  ! subroutines
  !
  public :: amod_create               ! create     the amod structure
  public :: amod_delete               ! delete     the amod structure
  public :: amod_initialize           ! iniitalize the amod structure
  public :: amod_get                  ! model coef get
  public :: amod_put                  ! model coef put
  public :: amod_put_process          ! model coef put process
  public :: amod_get_process          ! model coef get process
  public :: amod_verify               ! model coef verify
  public :: amod_sensitive            ! model coef sensiitivity
  public :: amod_all                  !   allocate amod memory
  public :: amod_del                  ! deallocate amod memory
  public :: amod_nul                  ! nullify    amod memory
  public :: amod_print                ! print velocity info
  public :: amod_parameter_print      ! print parameters
  public :: amod_set_coef             ! set some coef
  public :: amod_put_coef_k           ! coef put
  public :: amod_epsilon_delta_to_sx_eta ! convert from epsilon, delta to sx,eta
  public :: amod_epsilon_delta_to_vx_eta ! convert from epsilon, delta to vx,eta
  public :: amod_epsilon_to_vx        ! convert from epsilon to vx
  public :: amod_epsilon_to_sx        ! convert from epsilon to sx
  public :: amod_cg_fill              ! fill an array with values
  public :: amod_path_n
  public :: amod_path_by_name
  public :: amod_stop 
  public :: amod_line_feed
  public :: amod_opt_reflectivity
  !
  ! functions
  !
  ! interfaces
  !
  interface amod_cg_fill
    !
    module procedure amod_cg_fill_1
    module procedure amod_cg_fill_2
    module procedure amod_cg_fill_3
    !
  end interface
  !
  interface amod_stop 
    !
    module procedure amod_stop 
    module procedure amod_stop_r
    module procedure amod_stop_d
    module procedure amod_stop_l
    module procedure amod_stop_c
    !
  end interface
  !
  type, public :: amod_struct
    !
    ! velocity field structure sued for Kirchhoff imaging.
    !
    ! the following desribes the model coefficient field
    ! there may be up to 5 ansiotropic coefficients
    ! 1 vx    = veocity in parrallel to dip direction
    ! 2 eta   = anisotorpic eta coefficient
    ! 3 vz    = veocity in normal to dip direction
    ! 4 x_sin = sin of bed dip in x direction
    ! 5 y_sin = sin of bed dip in y direction
    !
    ! for isotropic imaging only one coefficient is used
    !
    ! for anisotropic time imaging two coefficients are used, vx and eta
    !
    ! for 2D anisotropic depth imaging three or four coefficients are used,
    ! vx, eta and vx if x_sin is not defined
    ! or
    ! vx, eta and vx and x_sin if x_sin is defined
    !
    ! for 3D anisotropic depth imaging three or four coefficients are used,
    ! vx, eta and vx if x_sin and y_sin are not defined
    ! or
    ! vx, eta and vx and x_sin and y_sin if either x_sin or y_sin is defined
    !
    ! each coefficient can be defined by a file name or by a constant value
    !
    ! in the case of vx it can be defined by
    ! vx(z) = const_mod(1) + ( z - depth_mod(1) ) * grad_mod(1)
    !
    ! in the case of vz it can be defined by a constant ratio
    ! of vz(x,y,z) = vx(x,y,z) * const_vzvx
    !
    ! the eta, x_sin and y_sin coefficients can be defined by
    ! file names or constant values const_eta, const_x_sin and const_y_sin
    !
    ! the values for the 5 coefficient are loaded into the following arrays
    ! for convenience
    ! vors_mod is either 'VELOCITY' or 'SLOWNESS'
    ! this is used to control how parameters are read into memory
    ! if vors_mod 'VELOCITY' the coefficient is held in memory as it appears
    ! in the orignal model
    ! if vors_mod = 'SLOWNESS' the coefficient is held in memory as the
    ! inverse of the original coefficient.
    ! Hence a velocity field which is stored as velocity
    ! in a standard model format will be converted to slowness in memory
    ! when it is read in.
    !
    ! vors_mod(2), vors_mod(4) and vors_mod(5) are always 'VELOCITY'
    ! indicating they are the original coefficient and have not been inverted
    !
    ! vors_mod(1) is 'VELOCITY' for time migration and 'SLOWNESS' for
    ! depth migration.
    !
    ! vors_mod(3) is inactive for time migration and 'SLOWNESS' for
    ! depth migration.
    !
    ! pwr_mod is used to raise a coeffieicnt to apower for printing
    ! pwr_mod = +1 for vors_mod= 'VELOCITY'
    ! pwr_mod = -1 for vors_mod= 'SLOWNESS'
    !
    ! depth_mod, grad_mod, and const_mod defoine constant gradients
    ! for the different coefficients.
    ! They are used if the file names are  undefined
    ! which is if path_mod = pathcheck_empty
    !
    character(len=filename_length)          :: c_title
    !
    character(len=11)                       :: opt_aniso ! aniso option flag
    logical                                 :: opt_converted    ! shear
    logical                                 :: opt_reflectivity ! reflectivity
    integer                                 :: m_coef_mod ! max coef 
    integer                                 :: n_coef_mod ! num coef input
    integer                                 :: l_coef_mod ! num coef active 
    integer                                 :: n_title_mod
    integer                                 :: n_opt_mod
    integer                                 :: n_const_mod
    integer                                 :: n_grad_mod
    integer                                 :: n_depth_mod
    integer                                 :: n_path_mod
    integer                                 :: n_pwr_mod
    integer                                 :: n_min_mod
    integer                                 :: n_max_mod
    integer,                        pointer :: index_mod(:) ! coef index
    character(len=8),               pointer :: title_mod(:) ! model coef title
    character(len=10),              pointer :: opt_mod  (:) ! coef option
    character(len=filename_length), pointer :: path_mod (:) ! coef file name
    real,                           pointer :: const_mod(:) ! coef constant
    real,                           pointer :: grad_mod (:) ! coef gradient
    real,                           pointer :: depth_mod(:) ! coef depth
    character(len=8),               pointer :: vors_mod (:) ! coef parm
    real,                           pointer :: pwr_mod  (:) ! coef pwr
    real,                           pointer :: min_mod  (:) ! coef min
    real,                           pointer :: max_mod  (:) ! coef max
    !
    integer                                 :: n_dimension
    character(len=8)                        :: vel_type      ! parameter type
    character(len=8)                        :: vel_parm      ! parameter title
    !
    ! coef k
    !
    logical                                 :: z_units       ! dep mig flag
    logical                                 :: t_units       ! dep mig flag
    logical                                 :: opt_curved_ray! curved ray tim
    !
    ! coef f
    integer                                 :: nz_mod        ! z mod num
    real                                    :: z0_mod        ! z mod min
    real                                    :: dz_mod        ! z mod inc
    !
    integer                                 :: amod_mem_size_0   ! mem usage
    !
    character (len=8)                       :: opt_vel       ! vel   option
    real                                    :: depth_vel_mig
    real                                    :: const_vel_mig
    real                                    :: grad_vel_mig
    character(len=filename_length)          :: path_vel
    !
    character (len=8)                       :: opt_vz        ! vz    option
    real                                    :: const_vzvx
    character(len=filename_length)          :: path_vz
    !
    character (len=8)                       :: opt_eta       ! eta   option
    real                                    :: const_eta
    character(len=filename_length)          :: path_eta
    !
    character (len=8)                       :: opt_x_sin     ! x_sin option
    real                                    :: const_x_sin
    character(len=filename_length)          :: path_x_sin
    !
    character (len=8)                       :: opt_y_sin     ! y_sin option
    real                                    :: const_y_sin
    character(len=filename_length)          :: path_y_sin
    !
    logical                                 :: l0_velocity ! velocity     exist
    logical                                 :: l0_epsilon  ! epsilon      exist
    logical                                 :: l0_delta    ! delta        exist
    logical                                 :: l0_x_sin    ! x dip        exist
    logical                                 :: l0_y_sin    ! y dip        exist
    logical                                 :: l0_refl     ! reflectivity exist
    logical                                 :: l0_vshear   ! shear vel    exist
    integer                                 :: j0_velocity ! velocity     index
    integer                                 :: j0_epsilon  ! epsilon      index
    integer                                 :: j0_delta    ! delta        index
    integer                                 :: j0_x_sin    ! x dip        index
    integer                                 :: j0_y_sin    ! y dip        index
    integer                                 :: j0_refl     ! reflectivity index
    integer                                 :: j0_vshear   ! shear vel    index
    integer                                 :: jv_gat      ! gat   vel    index
    integer                                 :: jv_tig      ! tig   vel    index
    character(len=8)                        :: c0_velocity ! velocity     title
    character(len=8)                        :: c0_epsilon  ! epsilon      title
    character(len=8)                        :: c0_delta    ! delta        title
    character(len=8)                        :: c0_x_sin    ! x dip        title
    character(len=8)                        :: c0_y_sin    ! y dip        title
    character(len=8)                        :: c0_refl     ! reflectivity title
    character(len=8)                        :: c0_vshear   ! shear vel    title
    !
  end type amod_struct
  !
  ! rcs identifier string
  character(len=100),public,save :: amod_ident = &
  '$Id: amod.f90,v 1.15 2007/08/22 13:48:12 Hanson beta sps $'
  integer, public, parameter :: n_opt_aniso = 5
  !
  integer,             parameter :: amod_m_title = 7
  character(len=8), save, public :: amod_c_title ( amod_m_title ) &
  = (/ 'VELOCITY', 'EPSILON ', 'DELTA   ', 'X_SIN   ', 'Y_SIN   ', &
       'REFL    ', 'VSHEAR  ' /)
  !
  integer,    parameter :: n_yes_no = 2
  character(len=3),save :: c_yes_no(n_yes_no) &
  = (/ 'YES', 'NO ' /)
  !
  character(len=11), public, save :: c_opt_aniso ( n_opt_aniso ) &
  = (/ 'ISOTROPIC  ', 'ANISOTROPIC', 'TILTED_BED ', &
       'ISOELASTIC ', 'ANIELASTIC ' /)
  !
  contains
  !
  subroutine amod_create ( o, c_title, i_err )
    !
    !  create the amod velocity structue
    !  read a velocity model from disk. - this may have up to 5 components
    !  defined by the file names in path_mod
    !  or set to constant values
    !  The vx coefficient can be set from a constant plus gradient term.
    !  The coefficient is read from disk if the file name is other than
    !  pathcheck_empty.
    !  Note this assumes every pe enters this routine at the same time.
    !
    type ( amod_struct ),      pointer :: o  ! amod   velocity structure
    character(len=*),    intent(in   ) :: c_title         ! title
    integer,             intent(inout) :: i_err           ! error 0 O.K. -1 err
    !
    ! Local variables
    !

    !
    i_err = 0
    !
    ! allocate the structure
    !
    if ( .not. associated ( o ) ) &
                 allocate ( o )
    !
    ! initalize structure coefficients
    !
    call amod_initialize ( o )
    !
    o%c_title = c_title
    !
    ! initialize the memory counter
    !
    call kfun_mem_init
    !
    !call amod_all ( o, i_err )
    !
    !if ( i_err .ne. 0 ) go to 998
    !
    ! set the number of coefficients and arrays, path_v, title_v etc
    !
    call amod_set_coef ( o )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print' ( &
    & /, " amod_create " , /, " REVISION: ", &
    &" 14  2006-09-21  Douglas Hanson Modify OPT_REFLECTIVITY. " &
    & )'
    !
    ! print the velocity structure info without the coefficients themselves
    !
    call amod_print ( o, ' amod_create start ' )
    !
    call kfun_mem_size ( o%amod_mem_size_0 )
    !
    !if ( .not. pc_do_not_process_traces() ) &
    !call amod_parameter_print ( o )
    !
    return
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in amod_create ", &
    & /, " during amod_all " &
    & )')
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in amod_create " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine amod_create
  !
  subroutine amod_delete ( o )
    !
    ! delete the amod structure
    !
    type ( amod_struct ),      pointer :: o  ! amod   velocity structure
    !
    ! Local variables
    !

    !
    call amod_del ( o )
    !
    ! delalocate the velocity structure
    !
    if ( associated ( o ) ) &
         deallocate ( o )
    !
    return
    !
  end subroutine amod_delete
  !
  subroutine amod_initialize ( o )
    !
    ! verify _mod parameters
    !
    type ( amod_struct ),    pointer :: o
    !
    integer                          :: i_err
    !
    i_err = 0
    !
    o%opt_aniso     = 'ISOTROPIC' ! anisotropic level
    o%opt_converted = .false.     ! converted wave option
    o%opt_reflectivity = .false.     ! reflectivity
    !
    o%l0_velocity = .false.            ! velocity     exist
    o%l0_epsilon  = .false.            ! epsilon      exist
    o%l0_delta    = .false.            ! delta        exist
    o%l0_x_sin    = .false.            ! x dip        exist
    o%l0_y_sin    = .false.            ! y dip        exist
    o%l0_refl     = .false.            ! reflectivity exist
    o%l0_vshear   = .false.            ! shear vel    exist
    o%j0_velocity = 1                  ! velocity     index
    o%j0_epsilon  = 1                  ! epsilon      index
    o%j0_delta    = 1                  ! delta        index
    o%j0_x_sin    = 1                  ! x dip        index
    o%j0_y_sin    = 1                  ! y dip        index
    o%j0_refl     = 1                  ! reflectivity index
    o%j0_vshear   = 1                  ! shear vel    index
    o%jv_gat     = 0                   ! gat   vel    index
    o%jv_tig     = 0                   ! tig   vel    index
    o%c0_velocity = amod_c_title ( 1 ) ! velocity     index
    o%c0_epsilon  = amod_c_title ( 2 ) ! epsilon      index
    o%c0_delta    = amod_c_title ( 3 ) ! delta        index
    o%c0_x_sin    = amod_c_title ( 4 ) ! x dip        index
    o%c0_y_sin    = amod_c_title ( 5 ) ! y dip        index
    o%c0_refl     = amod_c_title ( 6 ) ! reflectivity index
    o%c0_vshear   = amod_c_title ( 7 ) ! shear vel    index
    !
    o%m_coef_mod  = amod_m_title ! max coef
    o%n_coef_mod  = 0            ! num coef input
    o%l_coef_mod  = 0            ! num coef active 
    o%n_opt_mod   = 0
    o%n_const_mod = 0
    o%n_depth_mod = 0
    o%n_grad_mod  = 0
    o%n_path_mod  = 0
    o%n_dimension = 3
    !
    o%z_units        = .true.
    o%t_units        = .false.
    o%opt_curved_ray = .false.
    !
    o%nz_mod = 1
    o%z0_mod = 0.
    o%dz_mod = 1.
    !
    ! allocate mod parameters
    !
    call amod_all ( o, i_err )
    !
    o%opt_mod   ( : ) = 'CONSTANT'
    !
    o%const_mod ( : ) = 0.
    !
    o%const_mod ( 1 ) = 2000.
    !
    o%depth_mod ( : ) = 0.
    !
    o%grad_mod  ( : ) = 0.
    !
    o%path_mod  ( : ) = pathcheck_empty
    !
    o%const_vel_mig  = 2000.       ! constant migration velocity
    o%depth_vel_mig  = 0.          ! depth    migration velocity
    o%grad_vel_mig   = 0.          ! constant migration velocity gradient
    o%const_vzvx     = 1.          ! anisotropic coefficient
    o%const_eta      = 0.          ! anisotropic coefficient
    o%const_x_sin    = 0.          ! anisotropic x dip sin
    o%const_y_sin    = 0.          ! anisotropic x dip sin
    !
    o%opt_vel        = 'PATH'
    o%opt_vz         = 'CONSTANT'
    o%opt_eta        = 'CONSTANT'
    o%opt_x_sin      = 'CONSTANT'
    o%opt_y_sin      = 'CONSTANT'
    !
    o%path_vel       = pathcheck_empty ! migration velocity file
    o%path_vz        = pathcheck_empty ! vz file
    o%path_eta       = pathcheck_empty ! eta file
    o%path_x_sin     = pathcheck_empty ! x sin file
    o%path_y_sin     = pathcheck_empty ! y sin file
    !
    ! verify mod parameters
    !
    call amod_verify ( o )
    !
    return
    !
  end subroutine amod_initialize
  !
  subroutine amod_get ( o )
    !
    ! get mod parameters
    !
    type ( amod_struct ),    pointer :: o
    !
    call pc_register_array_names ( 'TITLE_MOD_ARRAYSET', &
                                 (/'TITLE_MOD', &
                                   'OPT_MOD  ', &
                                   'CONST_MOD', &
                                   'DEPTH_MOD', &
                                   'GRAD_MOD ', &
                                   'PATH_MOD ' /))

    !
    call pc_get ( 'opt_aniso',         o%opt_aniso         )
    call pc_get ( 'opt_converted',     o%opt_converted     )
    !
    call amod_verify ( o )
    !
    xxif_path_vel : if ( pc_process_keyword_present ( 'path_vel' ) ) then
      !
      call pc_get ( 'opt_vel',           o%opt_vel           )
      call pc_get ( 'depth_vel_mig',     o%depth_vel_mig     )
      call pc_get ( 'const_vel_mig',     o%const_vel_mig     )
      call pc_get ( 'grad_vel_mig',      o%grad_vel_mig      )
      call pc_get ( 'path_vel',          o%path_vel          )
      !
      call pc_get ( 'opt_vz',            o%opt_vz            )
      call pc_get ( 'const_vzvx',        o%const_vzvx        )
      call pc_get ( 'path_vz',           o%path_vz           )
      !
      call pc_get ( 'opt_eta',           o%opt_eta           )
      call pc_get ( 'const_eta',         o%const_eta         )
      call pc_get ( 'path_eta',          o%path_eta          )
      !
      call pc_get ( 'opt_x_sin',         o%opt_x_sin         )
      call pc_get ( 'const_x_sin',       o%const_x_sin       )
      call pc_get ( 'path_x_sin',        o%path_x_sin        )
      !
      call pc_get ( 'opt_y_sin',         o%opt_y_sin         )
      call pc_get ( 'const_y_sin',       o%const_y_sin       )
      call pc_get ( 'path_y_sin',        o%path_y_sin        )
      !
      o%opt_mod(1) = o%opt_vel
      o%opt_mod(2) = o%opt_eta
      o%opt_mod(3) = o%opt_vz
      o%opt_mod(4) = o%opt_x_sin
      o%opt_mod(5) = o%opt_y_sin
      !
      o%const_mod(1) = o%const_vel_mig
      o%const_mod(2) = o%const_eta
      o%const_mod(3) = o%const_vzvx * o%const_vel_mig
      o%const_mod(4) = o%const_x_sin
      o%const_mod(5) = o%const_y_sin
      !
      o%depth_mod(1) = o%depth_vel_mig
      o%depth_mod(2) = o%depth_vel_mig
      o%depth_mod(3) = o%depth_vel_mig
      o%depth_mod(4) = o%depth_vel_mig
      o%depth_mod(5) = o%depth_vel_mig
      !
      o%grad_mod(1) = o%grad_vel_mig
      o%grad_mod(2) = 0.
      o%grad_mod(3) = 0.
      o%grad_mod(4) = 0.
      o%grad_mod(5) = 0.
      !
      o%path_mod(1) = o%path_vel
      o%path_mod(2) = o%path_eta
      o%path_mod(3) = o%path_vz
      o%path_mod(4) = o%path_x_sin
      o%path_mod(5) = o%path_y_sin
      !
!print'(" bb3 amod_get opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
      !
    end if xxif_path_vel
    !
!print'(" aa3 amod_get opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
    !
    call pc_get ( 'title_mod', o%title_mod, o%n_coef_mod  )
    call pc_get ( 'opt_mod',   o%opt_mod,   o%n_opt_mod   )
    call pc_get ( 'const_mod', o%const_mod, o%n_const_mod )
    call pc_get ( 'depth_mod', o%depth_mod, o%n_depth_mod )
    call pc_get ( 'grad_mod',  o%grad_mod,  o%n_grad_mod  )
    call pc_get ( 'path_mod',  o%path_mod,  o%n_path_mod  )
    !
!print'(" aa4 amod_get opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
    !
    return
    !
  end subroutine amod_get
  !
  subroutine amod_put ( o )
    !
    ! put mod parameters
    !
    type ( amod_struct ),    pointer :: o
    !
!print'(" cc1 amod_put opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
    !
    call amod_line_feed ( 'amod_anisotropic' )
    call pc_put ( 'opt_aniso',         o%opt_aniso         )
    call pc_put ( 'opt_converted',     o%opt_converted     )
    !call pc_put ( 'off_far',           o%off_far           )
    !call pc_put ( 'tim_near',          o%tim_near          )
    !call pc_put ( 'tim_far',           o%tim_far           )
    !call pc_put ( 'vstk_to_vnmo',      o%vstk_to_vnmo      )
    !
    call amod_line_feed ( 'coef_mod' )
    call pc_put ( 'title_mod', o%title_mod, o%n_coef_mod  )
    call pc_put ( 'opt_mod',   o%opt_mod,   o%n_opt_mod   )
    call pc_put ( 'const_mod', o%const_mod, o%n_const_mod )
    call pc_put ( 'depth_mod', o%depth_mod, o%n_depth_mod )
    call pc_put ( 'grad_mod',  o%grad_mod,  o%n_grad_mod  )
    call pc_put ( 'path_mod',  o%path_mod,  o%n_path_mod  )
    !
!print'(" cc2 amod_put opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
    !
    return
    !
  end subroutine amod_put
  !
  subroutine amod_put_process ( o )
    !
    ! put process mod parameters
    !
    type ( amod_struct ),    pointer :: o
    !
    call pc_put_process ( 'opt_aniso', o%opt_aniso                )
    call pc_put_process ( 'opt_converted', o%opt_converted        )
    call pc_put_process ( 'title_mod', o%title_mod, o%n_coef_mod  )
    call pc_put_process ( 'opt_mod',   o%opt_mod,   o%n_opt_mod   )
    call pc_put_process ( 'const_mod', o%const_mod, o%n_const_mod )
    call pc_put_process ( 'depth_mod', o%depth_mod, o%n_depth_mod )
    call pc_put_process ( 'grad_mod',  o%grad_mod,  o%n_grad_mod  )
    call pc_put_process ( 'path_mod',  o%path_mod,  o%n_path_mod  )
    !
    return
    !
  end subroutine amod_put_process
  !
  subroutine amod_get_process ( o )
    !
    ! get process mod parameters
    !
    type ( amod_struct ),    pointer :: o
    !
    call pc_get_process ( 'opt_aniso', o%opt_aniso                )
    call pc_get_process ( 'opt_converted', o%opt_converted        )
    call pc_get_process ( 'title_mod', o%title_mod, o%n_coef_mod  )
    call pc_get_process ( 'opt_mod',   o%opt_mod,   o%n_opt_mod   )
    call pc_get_process ( 'const_mod', o%const_mod, o%n_const_mod )
    call pc_get_process ( 'depth_mod', o%depth_mod, o%n_depth_mod )
    call pc_get_process ( 'grad_mod',  o%grad_mod,  o%n_grad_mod  )
    call pc_get_process ( 'path_mod',  o%path_mod,  o%n_path_mod  )
    !
    return
    !
  end subroutine amod_get_process
  !
  subroutine amod_verify ( o )
    !
    ! verify mod parameters
    !
    type ( amod_struct ),    pointer :: o
    !
    integer                          :: j_coef_mod
    integer                          :: i_err
    !
    i_err = 0
    !
!print'(" dd2 amod_verify opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
    !
    if ( o%t_units ) call pc_put_options_field ( 'opt_aniso', c_opt_aniso, 2 )
    if ( o%z_units ) call pc_put_options_field ( 'opt_aniso', c_opt_aniso, 3 )
    call pc_put_options_field ( 'opt_aniso',     c_opt_aniso, 5        )
    call pc_put_options_field ( 'opt_converted', c_yes_no,    n_yes_no )
    !
!print'(" dd3 amod_verify opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
    !
    do_j_mod_parm : do j_coef_mod = 1 , o%m_coef_mod
      !
      call kfun_set_constant_path ( o%opt_mod ( j_coef_mod ) )
      !
      o%title_mod ( j_coef_mod ) = amod_c_title ( j_coef_mod )
      !
    end do do_j_mod_parm
    !
    if ( o%t_units .and. string_upper_compare ( o%opt_aniso, 'ISOTROPIC' ) ) &
    o%title_mod ( 2 ) = 'ETA'
    !
    if ( string_upper_compare ( o%opt_aniso, 'TILTED_BED' ) ) then
      !
      o%title_mod ( 4 ) = 'X_SIN'
      !
      o%title_mod ( 5 ) = 'Y_SIN'
      !
    else
      !
      o%title_mod ( 4 ) = 'VSHEAR'
      o%title_mod ( 5 ) = 'DENSITY'
      !
    end if
    !
    xxif_isotropic_2 : &
    if ( string_upper_compare ( o%opt_aniso, 'ISOTROPIC' ) ) then
      !
      ! isotropic migration
      !
      o%l_coef_mod = 1 ! num coef active 
      !
    else if ( string_upper_compare ( o%opt_aniso, 'ANISOTROPIC' ) &
              .and. o%t_units ) then
      !
      ! time migration
      !
      o%l_coef_mod = 2 ! num coef active 
      !
    else if ( string_upper_compare ( o%opt_aniso, 'ANISOTROPIC' ) &
              .and. o%z_units ) then
      !
      ! 2d and 3d anisotropic depth migration
      !
      o%l_coef_mod = 3 ! num coef active 
      !
    else if ( string_upper_compare ( o%opt_aniso, 'TILTED_BED' ) &
              .and. o%z_units &
              .and. o%n_dimension .eq. 2 ) then
      !
      ! 2d tilted bed anisotropic depth migration
      !
      o%l_coef_mod = 4 ! num coef active 
      !
    else if ( string_upper_compare ( o%opt_aniso, 'TILTED_BED' ) &
              .and. o%z_units &
              .and. o%n_dimension .eq. 3 ) then
      !
      ! 3d tilted bed anisotropic depth migration
      !
      o%l_coef_mod = 5 ! num coef active 
      !
    else if ( string_upper_compare ( o%opt_aniso, 'ISOELASTIC') ) then
      !
      o%l_coef_mod = 3 ! num coef active 
      o%title_mod ( 1 ) = 'VELOCITY'
      o%title_mod ( 2 ) = 'VSHEAR'
      o%title_mod ( 3 ) = 'DENSITY'
      !
    else if ( string_upper_compare ( o%opt_aniso, 'ANIELASTIC') ) then
      !
      o%l_coef_mod = 5 ! num coef active 
      o%title_mod ( 1 ) = 'VELOCITY'
      o%title_mod ( 2 ) = 'VSHEAR'
      o%title_mod ( 3 ) = 'DENSITY'
      o%title_mod ( 4 ) = 'EPSILON'
      o%title_mod ( 5 ) = 'DELTA'
      !
    else xxif_isotropic_2
      !
      ! error
      !
      call kfun_stop ( ' amod_verify error opt_aniso=', o%opt_aniso )
      !
    end if xxif_isotropic_2
    !
    ! add the reflectivity coefficient
    !
    xxif_opt_reflectivity  : if ( o%opt_reflectivity  ) then
      !
      o%l_coef_mod = o%l_coef_mod + 1 ! num coef active 
      !
      o%title_mod ( o%l_coef_mod ) = 'REFL'
      !
    end if xxif_opt_reflectivity  
    !
    ! set the shear and gat and tig slowness flags
    !
    o%j0_vshear = 0        ! shear velocity index
    !
    o%jv_gat    = 0        ! gat   velocity index
    !
    o%jv_tig    = 0        ! tig   velocity index
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( &
    !& /, " aa0 amod_verify opt_converted=",l2, &
    !& /, " aa0 amod_verify n_coef_mod=",i8, &
    !& /, " aa0 amod_verify l_coef_mod=",i8, &
    !& /, " aa0 amod_verify j0_vshear=",i8, &
    !& /, " aa0 amod_verify jv_gat   =",i8, &
    !& /, " aa0 amod_verify jv_tig   =",i8 &
    !& )', &
    !o%opt_converted, o%n_coef_mod, o%l_coef_mod, &
    !o%j0_vshear, o%jv_gat, o%jv_tig
    !
    do_j_coef_mod_1 : do j_coef_mod = 1 , o%l_coef_mod 
      !
      ! find the velocity coefficient
      !
      xxif_velocity_1 : &
      if ( string_upper_compare ( o%title_mod(j_coef_mod), 'VELOCITY' ) ) then
        !
        o%jv_gat = j_coef_mod        ! gat   velocity index
        !
        o%jv_tig = j_coef_mod        ! tig   velocity index
        !
        exit
        !
      end if xxif_velocity_1 
      !
    end do do_j_coef_mod_1 
    !
    ! find the shear coefficient
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( &
    !& /, " aa1 amod_verify opt_converted=",l2, &
    !& /, " aa1 amod_verify n_coef_mod=",i8, &
    !& /, " aa1 amod_verify l_coef_mod=",i8, &
    !& /, " aa1 amod_verify j0_vshear=",i8, &
    !& /, " aa1 amod_verify jv_gat   =",i8, &
    !& /, " aa1 amod_verify jv_tig   =",i8 &
    !& )', &
    !o%opt_converted, o%n_coef_mod, o%l_coef_mod, &
    !o%j0_vshear, o%jv_gat, o%jv_tig
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( " aa1 amod_verify n=",i8," l=",i8," j=",i8," t=",a8," p=",a)', &
    !( o%n_coef_mod, o%l_coef_mod, j_coef_mod, &
    !  trim ( o%title_mod(j_coef_mod) ), &
    !  trim ( o%path_mod(j_coef_mod) ), &
    !  j_coef_mod = 1 , o%n_coef_mod )
    !
    xxif_opt_converted : if ( o%opt_converted ) then
      !
      do_j_coef_mod : do j_coef_mod = 1 , o%n_coef_mod 
        !
        !if ( pcpsx_i_pel() .eq. 0 ) &
        !print'( " aa2 amod_verify l=",i8," j=",i8," t=",a8," p=",a)', &
        !o%l_coef_mod, j_coef_mod, &
        !trim ( o%title_mod(j_coef_mod) ), &
        !trim ( o%path_mod(j_coef_mod) ) 
        !
        xxif_vshear_1 : &
        if ( string_upper_compare ( o%title_mod(j_coef_mod), 'VSHEAR' ) ) then
          !
          o%l_coef_mod = max ( o%l_coef_mod, o%n_coef_mod ) ! num coef active 
          !
          o%j0_vshear = j_coef_mod        ! shear velocity index
          !
          o%jv_tig    = j_coef_mod        ! tig   velocity index
          !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( &
    !& /, " aa2 amod_verify opt_converted=",l2, &
    !& /, " aa2 amod_verify n_coef_mod=",i8, &
    !& /, " aa2 amod_verify l_coef_mod=",i8, &
    !& /, " aa2 amod_verify j0_vshear=",i8, &
    !& /, " aa2 amod_verify jv_gat   =",i8, &
    !& /, " aa2 amod_verify jv_tig   =",i8 &
    !& )', &
    !o%opt_converted, o%n_coef_mod, o%l_coef_mod, &
    !o%j0_vshear, o%jv_gat, o%jv_tig
          !
          exit
          !
        end if xxif_vshear_1 
        !
      end do do_j_coef_mod 
      !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( &
    !& /, " aa3 amod_verify opt_converted=",l2, &
    !& /, " aa3 amod_verify n_coef_mod=",i8, &
    !& /, " aa3 amod_verify l_coef_mod=",i8, &
    !& /, " aa3 amod_verify j0_vshear=",i8, &
    !& /, " aa3 amod_verify jv_gat   =",i8, &
    !& /, " aa3 amod_verify jv_tig   =",i8 &
    !& )', &
    !o%opt_converted, o%n_coef_mod, o%l_coef_mod, &
    !o%j0_vshear, o%jv_gat, o%jv_tig
      !
      ! if no shear velocity coefficient has been found set it here
      !
      xxif_vshear_2 : if ( o%j0_vshear .eq. 0 ) then
        !
        o%l_coef_mod = o%l_coef_mod + 1 ! num coef active 
        !
        o%title_mod ( o%l_coef_mod ) = 'VSHEAR'
        !
        o%j0_vshear = o%l_coef_mod        ! shear velocity index
        !
        o%jv_tig    = o%l_coef_mod        ! tig   velocity index
        !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( &
    !& /, " aa4 amod_verify opt_converted=",l2, &
    !& /, " aa4 amod_verify n_coef_mod=",i8, &
    !& /, " aa4 amod_verify l_coef_mod=",i8, &
    !& /, " aa4 amod_verify j0_vshear=",i8, &
    !& /, " aa4 amod_verify jv_gat   =",i8, &
    !& /, " aa4 amod_verify jv_tig   =",i8 &
    !& )', &
    !o%opt_converted, o%n_coef_mod, o%l_coef_mod, &
    !o%j0_vshear, o%jv_gat, o%jv_tig
        !
      end if xxif_vshear_2 
      !
    end if xxif_opt_converted 
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( " aa5 amod_verify n=",i8," l=",i8," j=",i8," t=",a8," p=",a)', &
    !( o%n_coef_mod, o%l_coef_mod, j_coef_mod, &
    !  trim ( o%title_mod(j_coef_mod) ), &
    !  trim ( o%path_mod(j_coef_mod) ), &
    !  j_coef_mod = 1 , o%n_coef_mod )
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'( &
    !& /, " aa5 amod_verify opt_converted=",l2, &
    !& /, " aa5 amod_verify n_coef_mod=",i8, &
    !& /, " aa5 amod_verify l_coef_mod=",i8, &
    !& /, " aa5 amod_verify j0_vshear=",i8, &
    !& /, " aa5 amod_verify jv_gat   =",i8, &
    !& /, " aa5 amod_verify jv_tig   =",i8 &
    !& )', &
    !o%opt_converted, o%n_coef_mod, o%l_coef_mod, &
    !o%j0_vshear, o%jv_gat, o%jv_tig
    !
    o%n_coef_mod  = o%l_coef_mod
    o%n_opt_mod   = o%l_coef_mod
    o%n_const_mod = o%l_coef_mod
    o%n_depth_mod = o%l_coef_mod
    o%n_grad_mod  = o%l_coef_mod
    o%n_path_mod  = o%l_coef_mod
    o%n_coef_mod  = o%l_coef_mod
    !
    if ( pcpsx_i_pel() .eq. 0 .and. .not. pc_do_not_process_traces() ) &
    print'( " amod_verify n=",i8," l=",i8," j=",i8," t=",a8," p=",a)', &
    ( o%n_coef_mod, o%l_coef_mod, j_coef_mod, &
      trim ( o%title_mod(j_coef_mod) ), &
      trim ( o%path_mod(j_coef_mod) ), &
      j_coef_mod = 1 , o%n_coef_mod )
    !
    if ( pcpsx_i_pel() .eq. 0 .and. .not. pc_do_not_process_traces() ) &
    print'( &
    & /, " amod_verify opt_converted=",l2, &
    & /, " amod_verify n_coef_mod=",i8, &
    & /, " amod_verify l_coef_mod=",i8, &
    & /, " amod_verify j0_vshear=",i8, &
    & /, " amod_verify jv_gat   =",i8, &
    & /, " amod_verify jv_tig   =",i8 &
    & )', &
    o%opt_converted, o%n_coef_mod, o%l_coef_mod, &
    o%j0_vshear, o%jv_gat, o%jv_tig
    !
    call amod_set_coef ( o )
    !
!print'(" dd4 amod_verify opt_aniso=",a11," n=",i2," opt_mod=",5(1x,a8))',&
!o%opt_aniso, o%n_opt_mod, o%opt_mod(1:5)
    !
    return
    !
  end subroutine amod_verify
  !
  subroutine amod_sensitive ( o )
    !
    ! turn anisotropic coefficients sensitive
    !
    type ( amod_struct ),  pointer :: o              ! amod object
    !
    xxif_isotropic : &
    if ( string_upper_compare ( o%opt_aniso, 'ISOTROPIC' ) ) then
      !
      ! for vertical anisotropic depth migration
      ! turn depth anisotropic coefficients sensitive
      !
      !call pc_put_sensitive_field_flag ( 'title_mod',  .false. )
      !call pc_put_sensitive_field_flag ( 'opt_mod',    .false. )
      !call pc_put_sensitive_field_flag ( 'const_mod',  .false. )
      !call pc_put_sensitive_field_flag ( 'depth_mod',  .false. )
      !call pc_put_sensitive_field_flag ( 'grad_mod',   .false. )
      !call pc_put_sensitive_field_flag ( 'path_mod',   .false. )
      !
    else xxif_isotropic
      !
      !call pc_put_sensitive_field_flag ( 'title_mod',  .true.  )
      !call pc_put_sensitive_field_flag ( 'opt_mod',    .true.  )
      !call pc_put_sensitive_field_flag ( 'const_mod',  .true.  )
      !call pc_put_sensitive_field_flag ( 'depth_mod',  .true.  )
      !call pc_put_sensitive_field_flag ( 'grad_mod',   .true.  )
      !call pc_put_sensitive_field_flag ( 'path_mod',   .true.  )
      !
    end if xxif_isotropic
    !
    return
    !
  end subroutine amod_sensitive
  !
  !
  subroutine amod_all ( o, i_err )
    !
    ! allocate mod parameters
    !
    type ( amod_struct ),    pointer :: o               ! amod structure
    integer,             intent(inout) :: i_err           ! error 0 O.K. -1 err
    !
    i_err = 0
    !
    call memfun_all ( o%title_mod, o%m_coef_mod,   'title_mod', i_err )
    call memfun_all ( o%opt_mod,   o%m_coef_mod,   'opt_mod',   i_err )
    call memfun_all ( o%const_mod, o%m_coef_mod,   'const_mod', i_err )
    call memfun_all ( o%depth_mod, o%m_coef_mod,   'depth_mod', i_err )
    call memfun_all ( o%grad_mod,  o%m_coef_mod,   'grad_mod',  i_err )
    call memfun_all ( o%path_mod,  o%m_coef_mod,   'path_mod',  i_err )
    call memfun_all ( o%vors_mod,  o%m_coef_mod,   'vors_mod',  i_err )
    call memfun_all ( o%pwr_mod,   o%m_coef_mod,   'pwr_mod',   i_err )
    call memfun_all ( o%min_mod,   o%m_coef_mod,   'min_mod',   i_err )
    call memfun_all ( o%max_mod,   o%m_coef_mod,   'max_mod',   i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in amod_all ", &
    & /, " during memory allocation " &
    & )')
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in amod_all " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine amod_all
  !
  subroutine amod_del ( o )
    !
    ! deallocate mod parameters
    !
    type ( amod_struct ),    pointer :: o               ! amod structure
    !
    call memfun_del ( o%title_mod )
    call memfun_del ( o%opt_mod )
    call memfun_del ( o%const_mod )
    call memfun_del ( o%depth_mod )
    call memfun_del ( o%grad_mod )
    call memfun_del ( o%path_mod )
    !
    return
    !
  end subroutine amod_del
  !
  subroutine amod_nul ( o )
    !
    ! nullify mod parameters
    !
    type ( amod_struct ),    pointer :: o               ! amod structure
    !
    call memfun_nul ( o%title_mod )
    call memfun_nul ( o%opt_mod )
    call memfun_nul ( o%const_mod )
    call memfun_nul ( o%depth_mod )
    call memfun_nul ( o%grad_mod )
    call memfun_nul ( o%path_mod )
    !
    return
    !
  end subroutine amod_nul
  !
  subroutine amod_parameter_print ( o )
    !
    type ( amod_struct ),    pointer :: o  ! amod   aperture structure
    !
    !call kprint_card ( ' top amod_parameter_print ' )
    !
    return
    !
  end subroutine amod_parameter_print
  !
  subroutine amod_print ( o, c_title )
    !
    ! print the model coefficients
    !
    type ( amod_struct ),      pointer :: o  ! amod   velocity structure
    character(len=*),    intent(in   ) :: c_title
    !
    ! Local variables
    !
    integer                            :: j_coef_mod
    !
    if ( pcpsx_i_pel() .eq. 0 .and. .not. pc_do_not_process_traces() ) &
    print' ( &
    & /, " amod_print " , a, &
    & /, " amod_print " , a, &
    & /, " amod_print j0_velocity=", i8, &
    & /, " amod_print j0_epsilon =", i8, &
    & /, " amod_print j0_delta   =", i8, &
    & /, " amod_print j0_x_sin   =", i8, &
    & /, " amod_print j0_y_sin   =", i8, &
    & /, " amod_print j0_refl    =", i8, &
    & /, " amod_print j0_vshear  =", i8, &
    & /, "   type  pwr min          max         ", &
    & "opt  title  const   depth   grad     path " &
    & )', &
    trim(o%c_title), trim(c_title), &
    o%j0_velocity, o%j0_epsilon, o%j0_delta, &
    o%j0_x_sin, o%j0_y_sin, o%j0_refl, o%j0_vshear 
    !
    if ( pcpsx_i_pel() .eq. 0 .and. .not. pc_do_not_process_traces() ) &
    print' ( &
    & 1x, i1, &
    & 1x, a8, 1x, f3.0, 1x, g12.6, 1x, g12.6, &
    & 1x, a8, 1x, a8, &
    & 1x, g12.6, 1x, g12.6, 1x, g12.6, &
    & 1x, a &
    & )', &
    (                  j_coef_mod, &
      trim(o%vors_mod (j_coef_mod)), &
           o%pwr_mod  (j_coef_mod), &
           o%min_mod  (j_coef_mod), &
           o%max_mod  (j_coef_mod), &
      trim(o%title_mod(j_coef_mod)), &
      trim(o%opt_mod  (j_coef_mod)), &
           o%const_mod(j_coef_mod), &
           o%depth_mod(j_coef_mod), &
           o%grad_mod (j_coef_mod), &
      trim(o%path_mod (j_coef_mod)), &
                       j_coef_mod = 1 , o%n_coef_mod )
    !
    return
    !
  end subroutine amod_print
  !
  subroutine amod_set_coef ( o )
    !
    ! set the number of coefficients and arrays, title_v, pram_v, path_v, etc
    !
    type ( amod_struct ),      pointer :: o  ! amod   velocity structure
    !
    integer                            :: j_coef_mod
    !
    ! Local variables
    !
    xxif_z_units : if ( o%z_units ) then
      !
      o%vel_type = 'VZIN'
      !
      o%vel_parm = 'SLOWNESS' ! interpolate velocity not slowness
      !
    else if ( o%opt_curved_ray ) then
      !
      o%vel_type = 'VTIN'
      !
      o%vel_parm = 'SLOWNESS' ! interpolate velocity not slowness
      !
    else xxif_z_units
      !
      o%vel_type = 'VTRM'
      !
      o%vel_parm = 'VELOCITY' ! interpolate velocity not slowness
      !
    end if xxif_z_units
    !
    !print'(" coef_modficient_set z_units=", l2, " opt_curved_ray=", l2, &
    !& " vel_type=", a8, " vel_parm=", a8 )', &
    !o%z_units, o%opt_curved_ray, trim(o%vel_type), trim ( o%vel_parm)
    !
    ! determine the dimensionality, nc_vel, of the velocity model
    ! nc_vel = 1 for isotropic, 3,4,5 for anisotropic
    !
    !call amod_ni ( o )
    !
    ! define descriptors for each coefficient
    !
    !
    o%vors_mod(1)              = o%vel_parm
    o%vors_mod(2:o%n_coef_mod) = 'VELOCITY'
    !
    do_j_coef_mod : do j_coef_mod = 1 , o%n_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod), o%c0_velocity ) ) &
      o%j0_velocity = j_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod), o%c0_epsilon ) ) &
      o%j0_epsilon = j_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod), o%c0_delta ) ) &
      o%j0_delta = j_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod), o%c0_x_sin ) ) &
      o%j0_x_sin = j_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod), o%c0_y_sin ) ) &
      o%j0_y_sin = j_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod), o%c0_refl ) ) &
      o%j0_refl = j_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod), o%c0_vshear ) ) &
      o%j0_vshear   = j_coef_mod
      !
      if ( string_upper_compare ( o%title_mod(j_coef_mod)(1:1), 'V' ) ) &
      o%vors_mod(j_coef_mod) = o%vel_parm
      !
      o%pwr_mod(j_coef_mod ) = -1.
      !
      if ( string_upper_compare ( o%vors_mod(j_coef_mod ), 'VELOCITY' ) ) &
      o%pwr_mod(j_coef_mod ) = 1.
      !
      !print'(" amod_set_coef j=",i8," p=",g12.6," v=",a)', &
      !j_coef_mod, o%pwr_mod(j_coef_mod ), trim ( o%vors_mod(j_coef_mod ) )
      !
    end do do_j_coef_mod 
    !
    return
    !
  end subroutine amod_set_coef
  !
  subroutine amod_path_n ( o, n_coef, path )
    !
    type ( amod_struct ), intent(in   ) :: o  ! amod   velocity structure
    integer,              intent(in   ) :: n_coef
    character(len=*),     intent(  out) :: path
    !
    path = ' '
    !
    if ( n_coef .ge. 1 .and. n_coef .le. o%n_coef_mod ) &
    path = o%path_mod(n_coef)
    !
    return
    !
  end subroutine amod_path_n
  !
  subroutine amod_path_by_name ( o, title, path )
    !
    type ( amod_struct ), intent(in   ) :: o  ! amod   velocity structure
    character(len=*),     intent(in   ) :: title
    character(len=*),     intent(  out) :: path
    !
    integer                             :: j_coef_mod
    !
    path = ' '
    !
    do_j_coef_mod : do j_coef_mod = 1,o%n_coef_mod
      !
      xxif_title : &
      if ( string_upper_compare ( title, o%title_mod(j_coef_mod) ) ) then
        !
        path = o%path_mod(j_coef_mod)
        !
        return
        !
      end if xxif_title 
      !
    end do do_j_coef_mod
    !
    return
    !
  end subroutine amod_path_by_name
  !
  subroutine amod_put_coef_k ( o, n_dimension, z_units, opt_curved_ray )
    !
    !  put the dimension and z_units ocef
    !
    type ( amod_struct ),      pointer :: o  ! amod   velocity structure
    !
    integer,             intent(in   ) :: n_dimension   ! dimension
    logical,             intent(in   ) :: z_units       ! dep mig flag
    logical,             intent(in   ) :: opt_curved_ray! curved ray option
    !
    ! Local variables
    !
    o%n_dimension     = n_dimension
    o%z_units         = z_units
    o%t_units         = .not. o%z_units
    o%opt_curved_ray  = opt_curved_ray
    !
    return
    !
  end subroutine amod_put_coef_k
  !
  subroutine amod_put_coef_f ( o, nz_mod, z0_mod, dz_mod )
    !
    !  put the dimension and z_units ocef
    !
    type ( amod_struct ),      pointer :: o  ! amod   velocity structure
    !
    integer,             intent(in   ) :: nz_mod        ! z mod num
    real,                intent(in   ) :: z0_mod        ! z mod min
    real,                intent(in   ) :: dz_mod        ! z mod inc
    !
    ! Local variables
    !
    o%nz_mod          = nz_mod
    o%z0_mod          = z0_mod
    o%dz_mod          = dz_mod
    !
    return
    !
  end subroutine amod_put_coef_f
  !
  subroutine amod_epsilon_delta_to_sx_eta ( n, sz, epsilon, delta, sx, eta )
    !
    ! convert sz, Thomsen epsilon and delta to sx, eta, sz
    ! Definition of terms (from Thomsen's 1986 paper,
    ! except for eta which was defined later by Alkhalifah in ~1995):
    ! sz = vertical   P-wave slowness
    ! sx = horizontal P-wave slowness
    ! Vp_nmo = NMO velocity for a homogeneous half-space
    ! epsilon = (C11 - C33) / (2*C33) = (Vx**2 - Vz**2) / (2*Vz**2)
    ! delta = ((C13 + C55)**2 - (C33 - C55)**2) / (2*C33*(C33 - C55))
    ! eta = (epsilon - delta) / (1 + 2*delta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    !
    ! Exact equations:
    ! delta = (epsilon - eta) / (1 + 2*eta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    ! Vp_nmo = Vz*sqrt(1+2*delta)    (in the limit as offset goes to zero)
    ! Vx = Vp_nmo*sqrt(1+2*eta)
    ! eps = 0 when vx=vz
    !
    integer,     intent(in   ) :: n
    real,        intent(in   ) :: sz(:)
    real,        intent(in   ) :: epsilon(:)
    real,        intent(in   ) :: delta(:)
    real,        intent(  out) :: sx(:)
    real,        intent(  out) :: eta(:)
    !
    integer,              save :: i_call = 0
    !
    i_call = i_call + 1
    !
    !eps   = ( vx - vz ) / vz
    !
    !delta = ( vx - ( 1 + eta ) * vz ) / ( ( 1 + 2. * eta + eta ) * vz )
    !
    xxif_size_err : &
    if ( n .gt. size(sx,1) &
    .or. n .gt. size(sz,1) &
    .or. n .gt. size(epsilon,1) &
    .or. n .gt. size(eta,1) &
    .or. n .gt. size(delta,1) ) then
      !
      print'(" amod_epsilon_delta_to_vx_eta size err ", &
      & /," n               =", i8, &
      & /," size(sx,1)      =", i8, &
      & /," size(sz,1)      =", i8, " min=", g12.6, " max=" ,g12.6, &
      & /," size(epsilon,1) =", i8, " min=", g12.6, " max=" ,g12.6, &
      & /," size(delta,1)   =", i8, " min=", g12.6, " max=" ,g12.6, &
      & /," size(eta,1)     =", i8 &
      & )', &
      n, &
      size(sx,1), &
      size(sz,1),      minval(sz),      maxval(sz), &
      size(epsilon,1), minval(epsilon), maxval(epsilon), &
      size(delta,1),   minval(delta),   maxval(delta), &
      size(eta,1)
      !
      stop
      !
    end if xxif_size_err
    !
    sx      ( 1:n ) = &
    sz      ( 1:n ) / sqrt ( 1. + 2. * &
    epsilon ( 1:n ) )
    !
    eta ( 1:n ) = ( epsilon ( 1:n )  - delta ( 1:n ) ) &
                         / ( 1. + 2. * delta ( 1:n ) )
    !
    !print'(" amod_epsilon_delta_to_sx_eta i=",i8," n=",i8, &
    !& " sx=", g12.6, 1x, g12.6, &
    !& " sz=", g12.6, 1x, g12.6, &
    !& " eta=", g12.6, 1x, g12.6, &
    !& " eps=", g12.6, 1x, g12.6, &
    !& " del=", g12.6, 1x, g12.6 &
    !& )', &
    !i_call, n, &
    !minval(sx(1:n)),      maxval(sx(1:n)), &
    !minval(sz(1:n)),      maxval(sz(1:n)), &
    !minval(eta(1:n)),     maxval(eta(1:n)),&
    !minval(epsilon(1:n)), maxval(epsilon(1:n)), &
    !minval(delta(1:n)),   maxval(delta(1:n))
    !
    return
    !
  end subroutine amod_epsilon_delta_to_sx_eta
  !
  subroutine amod_epsilon_delta_to_vx_eta ( n, vz, epsilon, delta, vx, eta )
    !
    ! convert vz, Thomsen epsilon and delta to vx, eta, vz
    ! Definition of terms (from Thomsen's 1986 paper,
    ! except for eta which was defined later by Alkhalifah in ~1995):
    ! Vz = vertical   P-wave velocity
    ! Vx = horizontal P-wave velocity
    ! Vp_nmo = NMO velocity for a homogeneous half-space
    ! epsilon = (C11 - C33) / (2*C33) = (Vx**2 - Vz**2) / (2*Vz**2)
    ! delta = ((C13 + C55)**2 - (C33 - C55)**2) / (2*C33*(C33 - C55))
    ! eta = (epsilon - delta) / (1 + 2*delta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    !
    ! Exact equations:
    ! delta = (epsilon - eta) / (1 + 2*eta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    ! Vp_nmo = Vz*sqrt(1+2*delta)    (in the limit as offset goes to zero)
    ! Vx = Vp_nmo*sqrt(1+2*eta)
    ! eps = 0 when vx=vz
    !
    integer,     intent(in   ) :: n
    real,        intent(in   ) :: vz(:)
    real,        intent(in   ) :: epsilon(:)
    real,        intent(in   ) :: delta(:)
    real,        intent(  out) :: vx(:)
    real,        intent(  out) :: eta(:)
    !
    !eps   = ( vx - vz ) / vz
    !
    !delta = ( vx - ( 1 + eta ) * vz ) / ( ( 1 + 2. * eta + eta ) * vz )
    !
    xxif_size_err : &
    if ( n .gt. size(vx,1) &
    .or. n .gt. size(vz,1) &
    .or. n .gt. size(epsilon,1) &
    .or. n .gt. size(eta,1) &
    .or. n .gt. size(delta,1) ) then
      !
      print'(" amod_epsilon_delta_to_vx_eta size err ", &
      & /," n               =", i8, &
      & /," size(vx,1)      =", i8, &
      & /," size(vz,1)      =", i8, " min=", g12.6, " max=" ,g12.6, &
      & /," size(epsilon,1) =", i8, " min=", g12.6, " max=" ,g12.6, &
      & /," size(delta,1)   =", i8, " min=", g12.6, " max=" ,g12.6, &
      & /," size(eta,1)     =", i8 &
      & )', &
      n, &
      size(vx,1), &
      size(vz,1),      minval(vz),      maxval(vz), &
      size(epsilon,1), minval(epsilon), maxval(epsilon), &
      size(delta,1),   minval(delta),   maxval(delta), &
      size(eta,1)
      !
      stop
      !
    end if xxif_size_err
    !
    vx      ( 1:n ) = &
    vz      ( 1:n ) * sqrt ( 1. + 2. * &
    epsilon ( 1:n ) )
    !
    eta ( 1:n ) = ( epsilon ( 1:n )  - delta ( 1:n ) ) &
                         / ( 1. + 2. * delta ( 1:n ) )
    !
    return
    !
  end subroutine amod_epsilon_delta_to_vx_eta
  !
  subroutine amod_epsilon_to_sx ( nz, nx, ny, sz, epsilon, sx )
    !
    ! convert sz, Thomsen epsilon and delta to sx, eta, sz
    ! Definition of terms (from Thomsen's 1986 paper,
    ! except for eta which was defined later by Alkhalifah in ~1995):
    ! Vz = vertical   P-wave velocity
    ! Vx = horizontal P-wave velocity
    ! Vp_nmo = NMO velocity for a homogeneous half-space
    ! epsilon = (C11 - C33) / (2*C33) = (Vx**2 - Vz**2) / (2*Vz**2)
    ! delta = ((C13 + C55)**2 - (C33 - C55)**2) / (2*C33*(C33 - C55))
    ! eta = (epsilon - delta) / (1 + 2*delta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    !
    ! Exact equations:
    ! delta = (epsilon - eta) / (1 + 2*eta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    ! Vp_nmo = Vz*sqrt(1+2*delta)    (in the limit as offset goes to zero)
    ! Vx = Vp_nmo*sqrt(1+2*eta)
    ! eps = 0 when sx=sz
    !
    integer,             intent(in   ) :: nx    ! x slowness node num
    integer,             intent(in   ) :: ny    ! y slowness node num
    integer,             intent(in   ) :: nz    ! z slowness node num
    real,                intent(in   ) :: sz      (:, :, : ) ! slowness z, x, y
    real,                intent(in   ) :: epsilon (:, :, : ) ! slowness z, x, y
    real,                intent(inout) :: sx      (:, :, : )
    !
    integer                            :: jx
    integer                            :: jy
    !
    do_jy : do jy = 1, ny
      !
      do_jx : do jx = 1, nx
        !
    sx      ( 1:nz, jx, jy ) = &
    sz      ( 1:nz, jx, jy )  / sqrt ( 1. + 2. * &
    epsilon ( 1:nz, jx, jy ) )
        !
      end do do_jx
      !
    end do do_jy
    !
    return
    !
  end subroutine amod_epsilon_to_sx
  !
  subroutine amod_epsilon_to_vx ( nz, nx, ny, vz, epsilon, vx )
    !
    ! convert vz, Thomsen epsilon and delta to vx, eta, vz
    ! Definition of terms (from Thomsen's 1986 paper,
    ! except for eta which was defined later by Alkhalifah in ~1995):
    ! Vz = vertical   P-wave velocity
    ! Vx = horizontal P-wave velocity
    ! Vp_nmo = NMO velocity for a homogeneous half-space
    ! epsilon = (C11 - C33) / (2*C33) = (Vx**2 - Vz**2) / (2*Vz**2)
    ! delta = ((C13 + C55)**2 - (C33 - C55)**2) / (2*C33*(C33 - C55))
    ! eta = (epsilon - delta) / (1 + 2*delta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    !
    ! Exact equations:
    ! delta = (epsilon - eta) / (1 + 2*eta)
    ! eta = (epsilon - delta) / (1 + 2*delta)
    ! Vx = Vz*sqrt(1+2*epsilon)
    ! Vp_nmo = Vz*sqrt(1+2*delta)    (in the limit as offset goes to zero)
    ! Vx = Vp_nmo*sqrt(1+2*eta)
    ! eps = 0 when vx=vz
    !
    integer,             intent(in   ) :: nx    ! x slowness node num
    integer,             intent(in   ) :: ny    ! y slowness node num
    integer,             intent(in   ) :: nz    ! z slowness node num
    real,                intent(in   ) :: vz      (:, :, : ) ! slowness z, x, y
    real,                intent(in   ) :: epsilon (:, :, : ) ! slowness z, x, y
    real,               intent(inout ) :: vx      (:, :, : )
    !
    integer                            :: jx
    integer                            :: jy
    !
    do_jy : do jy = 1, ny
      !
      do_jx : do jx = 1, nx
        !
    vx      ( 1:nz, jx, jy ) = &
    vz      ( 1:nz, jx, jy )   * sqrt ( 1. + 2. * &
    epsilon ( 1:nz, jx, jy ) )
        !
      end do do_jx
      !
    end do do_jy
    !
    return
    !
  end subroutine amod_epsilon_to_vx
  !
  subroutine amod_cg_fill_1 ( &
                              invert_mod, const_mod, depth_mod, grad_mod, &
                              nz_mod, z0_mod, dz_mod, &
                              rv_mod &
                            )
    !
    ! fill an array usin rv_mod(z) = ( z - depth_mod ) * grad_mod + const_mod
    !
    logical,            intent(in   ) :: invert_mod    ! invert coef
    real,               intent(in   ) :: const_mod     ! const val
    real,               intent(in   ) :: depth_mod     ! depth val
    real,               intent(in   ) :: grad_mod      ! grad  val
    integer,            intent(in   ) :: nz_mod        ! z mod num
    real,               intent(in   ) :: z0_mod        ! z mod min
    real,               intent(in   ) :: dz_mod        ! z mod inc
    real,               intent(  out) :: rv_mod(:)     ! v mod val
    !
    integer                           :: jz_mod ! z mod index
    real                              :: rz_mod ! z mod value
    !
    do_jz_mod : do jz_mod = 1 , nz_mod
      !
      rz_mod = ( jz_mod - 1 ) * dz_mod + z0_mod
      !
      rv_mod ( jz_mod ) = ( rz_mod - depth_mod ) * grad_mod + const_mod
      !
    end do do_jz_mod
    !
    if ( invert_mod ) rv_mod ( 1:nz_mod ) = 1. / rv_mod ( 1:nz_mod )
    !
    return
    !
  end subroutine amod_cg_fill_1
  !
  subroutine amod_cg_fill_2 ( &
                            invert_mod, const_mod, depth_mod, grad_mod, &
                            nx_mod, &
                            nz_mod, z0_mod, dz_mod, &
                            rv_mod &
                          )
    !
    ! fill an array usin rv_mod(z) = ( z - depth_mod ) * grad_mod + const_mod
    !
    logical,            intent(in   ) :: invert_mod    ! invert coef
    real,               intent(in   ) :: const_mod     ! const val
    real,               intent(in   ) :: depth_mod     ! depth val
    real,               intent(in   ) :: grad_mod      ! grad  val
    integer,            intent(in   ) :: nx_mod        ! x mod num
    integer,            intent(in   ) :: nz_mod        ! z mod num
    real,               intent(in   ) :: z0_mod        ! z mod min
    real,               intent(in   ) :: dz_mod        ! z mod inc
    real,               intent(  out) :: rv_mod(:,:)   ! v mod val
    !
    integer                           :: jx_mod ! x mod index
    !
    call amod_cg_fill ( &
                        invert_mod, const_mod, depth_mod, grad_mod, &
                        nz_mod, z0_mod, dz_mod, &
                        rv_mod(:,1) &
                      )
    !
    do_jx_mod : do jx_mod = 1 , nx_mod
      !
      rv_mod ( 1:nz_mod, jx_mod ) = rv_mod ( 1:nz_mod, 1 )
      !
    end do do_jx_mod
    !
    return
    !
  end subroutine amod_cg_fill_2
  !
  subroutine amod_cg_fill_3 ( &
                            invert_mod, const_mod, depth_mod, grad_mod, &
                            nx_mod, ny_mod, &
                            nz_mod, z0_mod, dz_mod, &
                            rv_mod &
                          )
    !
    ! fill an array usin rv_mod(z) = ( z - depth_mod ) * grad_mod + const_mod
    !
    logical,            intent(in   ) :: invert_mod    ! invert coef
    real,               intent(in   ) :: const_mod     ! const val
    real,               intent(in   ) :: depth_mod     ! depth val
    real,               intent(in   ) :: grad_mod      ! grad  val
    integer,            intent(in   ) :: nx_mod        ! x mod num
    integer,            intent(in   ) :: ny_mod        ! y mod num
    integer,            intent(in   ) :: nz_mod        ! z mod num
    real,               intent(in   ) :: z0_mod        ! z mod min
    real,               intent(in   ) :: dz_mod        ! z mod inc
    real,               intent(  out) :: rv_mod(:,:,:) ! v mod val
    !
    integer                           :: jx_mod ! x mod index
    integer                           :: jy_mod ! y mod index
    !
    call amod_cg_fill ( &
                        invert_mod, const_mod, depth_mod, grad_mod, &
                        nz_mod, z0_mod, dz_mod, &
                        rv_mod(:,1,1) &
                      )
    !
    do_jy_mod : do jy_mod = 1 , ny_mod
      !
      do_jx_mod : do jx_mod = 1 , nx_mod
        !
        rv_mod ( 1:nz_mod, jx_mod, jy_mod ) = rv_mod ( 1:nz_mod, 1, 1 )
        !
      end do do_jx_mod
      !
    end do do_jy_mod
    !
    return
    !
  end subroutine amod_cg_fill_3
  !
  subroutine amod_line_feed ( c_title )
    !
    ! add a line feed to the pc deck
    !
    character(len=*), intent(in   ) :: c_title
    !
    ! lcoal variables
    !
    character(len=60)               :: c_title_0
    !
    ! add a line feed
    !
    c_title_0 = &
'------------------------------------------------------------------------'
!123456789012345678901234567890123456789012345678901234567890123456789012
    !
    !c_title_0 (1:len_trim(c_title)) = trim ( c_title )
    !
    c_title_0 (60-len_trim(c_title):) = ' '
    !
    call pc_put ( c_title, c_title_0 )
    !
    return
    !
  end subroutine amod_line_feed
  !
  !
  subroutine amod_stop ( title, i_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    integer,           intent(in   ) :: i_flag
    !
    !write ( pc_get_lun(), '( &
    print'( &
    & /, a, /, " p=", i4, " stopping i_flag=", i10 )', &
    trim(title), pcpsx_i_pel(), i_flag
    !
    if ( pc_get_lun() .ne. 6) &
    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping i_flag=", i10, &
    & " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), i_flag, pc_get_lun()
    !
    call pc_warning  ( ' end of amod_stop  ' )
    !
    stop
    !
  end subroutine amod_stop 
  !
  subroutine amod_stop_r ( title, r_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    real,              intent(in   ) :: r_flag
    !
    !write ( pc_get_lun(), '( &
    print'( &
    & /, a, /, " p=", i4, " stopping r_flag=", g12.6 )', &
    trim(title), pcpsx_i_pel(), r_flag
    !
    if ( pc_get_lun() .ne. 6) &
    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping r_flag=", g12.6, &
    & " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), r_flag, pc_get_lun()
    !
    call pc_warning ( ' end of amod_stop_r ' )
    !
    stop
    !
  end subroutine amod_stop_r
  !
  subroutine amod_stop_d ( title, d_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    double precision,  intent(in   ) :: d_flag
    !
    !write ( pc_get_lun(), '( &
    print'( &
    & /, a, /, " p=", i4, " stopping d_flag=", g12.6 )', &
    trim(title), pcpsx_i_pel(), d_flag
    !
    if ( pc_get_lun() .ne. 6) &
    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping d_flag=", g12.6, &
    & " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), d_flag, pc_get_lun()
    !
    call pc_warning ( ' end of amod_stop_d ' )
    !
    stop
    !
  end subroutine amod_stop_d
  !
  subroutine amod_stop_l ( title, l_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    logical,           intent(in   ) :: l_flag
    !
    !write ( pc_get_lun(), '( &
    print'( &
    & /, a, /, " p=", i4, " stopping l_flag=", l2 )', &
    trim(title), pcpsx_i_pel(), l_flag
    !
    if ( pc_get_lun() .ne. 6) &
    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping l_flag=", l2, &
    & " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), l_flag, pc_get_lun()
    !
    call pc_warning ( ' end of amod_stop_l ' )
    !
    stop
    !
  end subroutine amod_stop_l
  !
  subroutine amod_stop_c ( title, c_flag )
    !
    ! execute stop with a print
    !
    character(len=*),  intent(in   ) :: title
    character(len=*),  intent(in   ) :: c_flag
    !
    !write ( pc_get_lun(), '( &
    print'( &
    & /, a, /, " p=", i4, " stopping c_flag=", a )', &
    trim(title), pcpsx_i_pel(), trim(c_flag)
    !
    if ( pc_get_lun() .ne. 6) &
    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping c_flag=", a, &
    & /, " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), trim(c_flag), pc_get_lun()
    !
    call pc_warning (' end of amod_stop_c ' )
    !
    stop
    !
  end subroutine amod_stop_c
  !
  subroutine amod_opt_reflectivity ( o, opt_reflectivity )
    !
    ! set opt_reflectivity 
    !
    type ( amod_struct ),    pointer :: o
    logical,           intent(in   ) :: opt_reflectivity 
    !
    o%opt_reflectivity = opt_reflectivity 
    !
    return
    !
  end subroutine amod_opt_reflectivity 
  !
end module amod_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
