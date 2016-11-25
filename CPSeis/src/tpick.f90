!<CPS_v1 type="PROCESS"/>
!
!!------------------------------ TPICK.f90 ---------------------------------!!
!!------------------------------ TPICK.f90 ---------------------------------!!
!!------------------------------ TPICK.f90 ---------------------------------!!

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
!                         C P S   P R O C E S S
!
! Name       : TPICK
! Category   : Synthetics
! Written    : 2003-01-21   by: Douglas Hanson
! Revised    : 2007-05-29   by: Douglas Hanson Fix initialize order.
! Maturity   : beta
! Purpose    : Pick events and compute ava curves.
! Portability: No known limitations.
! Parallel   : No.
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NUMTR     max number of traces input/output    Set to 1
! GATHERED  whether traces are gathered          Set to .false.
! NWIH      number of words in trace header      used but not changed
! NDPT      number of sample values in trace     used but not changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! HDR     Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Set
! 2       Head mute                  Set
! 3       Current gather number      Set
! 4       No. within current gather  Set
! 64      Tail mute                  Set
!         All X,Y headers            Set
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
! 15  2007-05-29  Douglas Hanson Fix initialize order.
! 14  2007-04-12  Douglas Hanson Datumgrid changes.
! 13  2007-01-03  Douglas Hanson Add process number index.
! 12  2006-08-24  D. Glover      Added NULLIFY statements for Intel compiler.
! 11  2006-06-06  Stoeckley      Add pc_register_array_names for SeisSpace.
! 10  2006-04-04  Douglas Hanson Use syn modules.
!  9  2006-03-30  Douglas Hanson Remove wavelet_obj.
!  8  2006-01-10  B. Menger      Removed Unused Variables.
!  7  2005-09-01  Douglas Hanson Add ref_scale_index. 
!  6  2005-08-16  Ioan Vlad      Accomodate new zoeppritz API
!  5  2005-01-31  Douglas Hanson Add init_scale_index.
!  4  2004-09-21  Douglas Hanson Add picked depth to output.
!  3  2004-09-02  Douglas Hanson Use tpick0 as flag string.
!  2  2003-06-11  Douglas Hanson New SPIKE usage.
!  1  2003-01-26  Douglas Hanson Original version from spike.
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
! TWOSETS        false     whether this process needs two trace/header arays.
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
! Tpick does not handle Y_DIP_REF <> 0. yet.
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
!<NS TPICK_1/NC=80>
!
! Create synthetic tpicks with noise traces on a grid
!
! HDR_X~~~~=`II      HDR_Y~~~~~=`II      AZIMUTH~~~=`FFFFFFF 
! DEP_SCALE=`FFFFFFF DEP_WIDTH=`FFFFFFF  COS_PWR~~~=`FFFFFFF MAX_INP=`IIIIIIII
! REF_SCALE_INDEX=`IIIIIIII INIT_SCALE_INDEX=`IIIIIIII LAST_SCALE_INDEX=`IIIIIIII
!
! CONST_VEL~~~~~=`FFFFFFF   V_SRC_REC~~~~~=`FFFFFFF 
! Select PATH_VEL [PATH_VEL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<NS TPICK_DATUM/NC=80>
!
!<include datumgrid.f90>
!
!<NS TPICK_REFLECTOR_DESCRIPTION/NC=80>
!
!
!                   REFLECTOR DEFINIITON
!              DEP_REF       X_DIP_REF     Y_DIP_REF        
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!
!                   ABOVE REFLECTOR DEFINIITON
!              DN1_REF       VP1_REF       VS1_REF       
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!
!                   BELOW REFLECTOR DEFINIITON
!              DN2_REF       VP2_REF       VS2_REF       
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!
!<PARMS DEP_REF_ARRAYSET[/ML=128/XST/YST]> 
!<PARMS DN1_REF_ARRAYSET[/ML=128/XST/YST]> 
!<PARMS DN2_REF_ARRAYSET[/ML=128/XST/YST]> 
!<PARMS TPICK_1[screen1]>
!<PARMS TPICK_DATUM[screen2]>
!<PARMS TPICK_REFLECTOR_DESCRIPTION[screen3]>
!</gui_def>
!<HelpSection>
!
!<Help KEYWORD="AZIMUTH">
!<Tip> Output trace azimuth in degrees. </Tip>
! Default = 0.0
! Allowed = real scalar
!  The trace azimuth is based upon the CPS definition and is independant of 
!  which header words are selected for HDR_X and HDR_Y and is defined 
!  in degrees.
!  Zero   degree azimuth is parrellel to the X axis.
!  Ninety degree azimuth is parrellel to the Y axis.
!</Help>
!
!<Help KEYWORD="COS_PWR">
!<Tip> Multiply picked aplitude by cos ( surface angle ) ** COS_PWR. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="MAX_INP">
!<Tip> Maximum number of input traces. </Tip>
! Default = 1000
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word designating fast direction. </Tip>
! Default = 7
! Allowed = 7, 8, 17, 18
!  HDR_X and HDR_Y and define the fast and slow lateral
!  position variation of ouput traces.  The allowed combinations are:
!  HDR_X =  7, HDR_Y = 18
!  HDR_X =  8, HDR_Y =  7
!  HDR_X = 17, HDR_Y = 18
!  HDR_X = 18, HDR_Y = 17
!
!</Help>
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word designating slow direction. </Tip>
! Default = 9
! Allowed = 7, 8, 17, 18
!  HDR_X and HDR_Y and define the fast and slow lateral
!  position variation of ouput traces.  The allowed combinations are:
!  HDR_X =  7, HDR_Y = 18
!  HDR_X =  8, HDR_Y =  7
!  HDR_X = 17, HDR_Y = 18
!  HDR_X = 18, HDR_Y = 17
!</Help>
!
!<Help KEYWORD="REF_SCALE_INDEX">
!<Tip> Reflector index to set scale. </Tip>
! Default = 1
! Allowed = int>0
! If REF_SCALE_INDEX = 0 each reflector will be scaled independantly.
! If REF_SCALE_INDEX > 0 each reflector will be scaled to 
! reflector REF_SCALE_INDEX
!</Help>
!
!<Help KEYWORD="INIT_SCALE_INDEX">
!<Tip> First input trace to set scale. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="LAST_SCALE_INDEX">
!<Tip> Last input trace to set scale. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="DEP_SCALE">
!<Tip> Scale fromfrom input vertical units to true depth units. </Tip>
! Default = 1000.
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="DEP_WIDTH">
!<Tip> Depth window width for picking event. </Tip>
! Default = 100.
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="V_SRC_REC">
!<Tip> Ratio of source vleocity to receiver Vs / Vr. </Tip>
! Default = 1
! Allowed = real > 0.
! Tpick will use a ratio of V_SRC_REC for the source and receiver velocities
! when computeing the travel times.
! The source ray paths will use the input velocity field.
! The receiver ray paths will use the input velocity field divided by V_SRC_REC.
!</Help>
!
!<Help KEYWORD="CONST_VEL">
!<Tip> Constant velocity to use if PATH_VEL=NONE. </Tip>
! Default = 1
! Allowed = real > 0.
! Tpick will use a constant velcoity of CONST_VEL if PATH_VEL=NONE.
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
!<Help KEYWORD="DEP_REF">
!<Tip> Reflector depth aray. </Tip>
! Default = 1
! Allowed = real (aray)
!
! TPICK assumes that each reflector depth, DEPTH ( X, Y ) 
! at any X, Y location is defined by 
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF )* X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in HDR_X and HDR_Y units.
!</Help>
!
!<Help KEYWORD="X_DIP_REF">
!<Tip> Reflector X dip aray. </Tip>
! Default = 1
! Allowed = real (aray)
! Allowed = real (aray)
!
! TPICK assumes that each reflector depth, DEPTH ( X, Y ) 
! at any X, Y location is defined by 
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF )* X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in HDR_X and HDR_Y units.
!</Help>
!
!<Help KEYWORD="Y_DIP_REF">
!<Tip> Reflector Y dip aray. </Tip>
! Default = 1
! Allowed = real (aray)
! Allowed = real (aray)
!
! TPICK assumes that each reflector depth, DEPTH ( X, Y ) 
! at any X, Y location is defined by 
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF )* X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in HDR_X and HDR_Y units.
!</Help>
!
!<Help KEYWORD="DN1_REF">
!<Tip> Above reflector density. </Tip>
! Default = 1
! Allowed = real (aray)
!</Help>
!
!<Help KEYWORD="VP1_REF">
!<Tip> Above reflector P velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!</Help>
!
!<Help KEYWORD="VS1_REF">
!<Tip> Above reflector S velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!</Help>
!
!<Help KEYWORD="DN2_REF">
!<Tip> Above reflector density. </Tip>
! Default = 1
! Allowed = real (aray)
!</Help>
!
!<Help KEYWORD="VP2_REF">
!<Tip> Above reflector P velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!</Help>
!
!<Help KEYWORD="VS2_REF">
!<Tip> Above reflector S velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
module tpick_module
  !
  ! Module references
  !
  use cio_module
  use cpsio_module
  use cpucount_module
  use datumgrid_module
  use surfacegrid_module
  use getlun_module
  use grid_module
  use interpolate_module
  use matfun_module
  use memfun_module
  use migfun_module
  use named_constants_module
  use pathchoose_module
  use pathcheck_module
  use pattern_module
  use pc_module
  use synref_module
  use synvofz_module
  use string_module
  use timeglob_module
  use velgrid_module
  use zoeppritz_module
  !
  implicit none
  !
  private
  !
  public :: tpick_create
  public :: tpick_delete
  public :: tpick_initialize
  public :: tpick_update
  public :: tpick
  public :: tpick_wrapup
  public :: tpick_print_info
  !
  ! rcs identifier string
  character(len=100),public,save :: tpick_ident = &
  '$Id: tpick.f90,v 1.15 2007/05/30 14:48:13 Hanson beta sps $'
  !
  type, public :: tpick_struct
    !
    private
    !
    type(grid_struct)                       :: grid_obj       ! transform grid 
    logical                                 :: skip_wrapup    ! wrapup flag
    integer                                 :: ipn           ! process number
    !
    integer                                 :: n_vel
    !
    integer                                 :: hx_vel
    integer                                 :: nx_vel
    real                                    :: x0_vel
    real                                    :: dx_vel
    !
    integer                                 :: hy_vel
    integer                                 :: ny_vel
    real                                    :: y0_vel
    real                                    :: dy_vel
    !
    integer                                 :: nz_vel
    real                                    :: z0_vel
    real                                    :: dz_vel
    !
    real,                           pointer :: v0_vel(:,:,:)
    !
    integer                                 :: i_rg_inp 
    integer                                 :: i_ra_inp 
    integer                                 :: i_rp_inp 
    integer                                 :: i_ro_ref 
    integer                                 :: i_rx_ref 
    integer                                 :: i_ry_ref 
    integer                                 :: i_rz_inp 
    integer                                 :: i_rz_pik 
    integer                                 :: m0_dat
    real,                           pointer :: r0_dat(:,:,:)
    integer                                 :: n0_ref
    integer                                 :: ndep_ref
    integer                                 :: nx_dip_ref
    integer                                 :: ny_dip_ref
    integer                                 :: ndn1_ref
    integer                                 :: nvp1_ref
    integer                                 :: nvs1_ref
    integer                                 :: ndn2_ref
    integer                                 :: nvp2_ref
    integer                                 :: nvs2_ref
    real,                           pointer :: dep_ref(:)
    real,                           pointer :: x_dip_ref(:)
    real,                           pointer :: y_dip_ref(:)
    real,                           pointer :: dn1_ref(:)
    real,                           pointer :: vp1_ref(:)
    real,                           pointer :: vs1_ref(:)
    real,                           pointer :: dn2_ref(:)
    real,                           pointer :: vp2_ref(:)
    real,                           pointer :: vs2_ref(:)
    !
    character(len=filename_length)          :: path_vel
    type(pathchoose_struct),        pointer :: path_src_datum_button
    type(pathchoose_struct),        pointer :: path_rec_datum_button
    type(pathchoose_struct),        pointer :: path_vel_button
    !
    real                                    :: azimuth
    real                                    :: cos_pwr
    real                                    :: dep_scale
    real                                    :: dep_width
    integer                                 :: ref_scale_index
    integer                                 :: init_scale_index
    integer                                 :: last_scale_index
    !
    integer                                 :: max_inp
    integer                                 :: m0_inp
    integer                                 :: hdr_x
    integer                                 :: hdr_y
    !
    real                                    :: v_src_rec
    real                                    :: const_vel
    real                                    :: const_source
    real                                    :: const_receiver
    !
    integer                                 :: i0_inp
    integer                                 :: nh_inp
    integer                                 :: nt_glb
    real                                    :: t0_glb
    real                                    :: dt_glb
    !
    integer                                 :: nz_glb
    real                                    :: z0_glb
    real                                    :: dz_glb
    !
    real                                    :: rx_scl
    real                                    :: ry_scl
    !
    integer                                 :: mh_inp
    integer                                 :: mt_inp
    character(len=16)                       :: inc_flag
    character(len=16)                       :: att_flag
    real                                    :: rx_tol
    real                                    :: rx_mid
    real                                    :: ry_mid
    real                                    :: rx_src
    real                                    :: ry_src
    real                                    :: rz_src
    real                                    :: rx_rec
    real                                    :: ry_rec
    real                                    :: rz_rec
    !
    real                                    :: ang_inc
    real                                    :: ang_src
    real                                    :: ang_rec
    real                                    :: xh_ref
    real                                    :: yh_ref
    real                                    :: rh_ref
    real                                    :: so_ref
    real                                    :: ro_ref
    real                                    :: ra_ref
    real                                    :: ra_scl
    real                                    :: rx_ref
    real                                    :: ry_ref
    real                                    :: rz_ref
    real                                    :: rz_pik_0
    real                                    :: rx_norm
    real                                    :: ry_norm
    real                                    :: rz_norm
    real                                    :: refl_p
    real                                    :: refl_s
    real                                    :: tran_p
    real                                    :: tran_s
    integer                                 :: i0_ref
    integer                                 :: iz_pik_0
    integer                                 :: iz_max(1)
    real                                    :: rz_max
    real                                    :: ra_max
    integer,                        pointer :: num_scale ( : )
    real,                           pointer :: amp_scale ( : )
    real,                           pointer :: dz_dx ( : )
    real,                           pointer :: dz_dy ( : )
    real,                           pointer :: dx_dz ( : )
    real,                           pointer :: dy_dz ( : )
    integer,                        pointer :: iz_inp ( : )
    real,                           pointer :: rz_inp ( : )
    real,                           pointer :: rz_pik ( : )
    real,                           pointer :: rg_inp ( : )
    real,                           pointer :: ra_inp ( : )
    real,                           pointer :: rp_inp ( : )
    type ( datumgrid_struct ),      pointer :: dtm  ! datumgrid structure
    !
  end type tpick_struct
  !
  type(tpick_struct), save,     pointer :: object      ! needed for traps.
  !
  contains
  !
  subroutine tpick_create ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    integer                             :: i_err
    integer, save                       :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    ! Begin tpick_create
    !
    print'(" top tpcik_create c=",i8)', i_call
    !
    allocate ( o )
    !
    nullify (o%v0_vel) ! jpa
    nullify (o%r0_dat) ! jpa
    nullify (o%dep_ref) ! jpa
    nullify (o%x_dip_ref) ! jpa
    nullify (o%y_dip_ref) ! jpa
    nullify (o%dn1_ref) ! jpa
    nullify (o%vp1_ref) ! jpa
    nullify (o%vs1_ref) ! jpa
    nullify (o%dn2_ref) ! jpa
    nullify (o%vp2_ref) ! jpa
    nullify (o%vs2_ref) ! jpa
    nullify (o%path_src_datum_button) ! jpa
    nullify (o%path_rec_datum_button) ! jpa
    nullify (o%path_vel_button) ! jpa
    nullify (o%num_scale) ! jpa
    nullify (o%amp_scale) ! jpa
    nullify (o%dz_dx) ! jpa
    nullify (o%dz_dy) ! jpa
    nullify (o%dx_dz) ! jpa
    nullify (o%dy_dz) ! jpa
    nullify (o%iz_inp) ! jpa
    nullify (o%rz_inp) ! jpa
    nullify (o%rz_pik) ! jpa
    nullify (o%rg_inp) ! jpa
    nullify (o%ra_inp) ! jpa
    nullify (o%rp_inp) ! jpa
    nullify (o%dtm) ! jpa

    call memfun_nul ( o%dep_ref )
    call memfun_nul ( o%x_dip_ref )
    call memfun_nul ( o%y_dip_ref )
    call memfun_nul ( o%dn1_ref )
    call memfun_nul ( o%vp1_ref )
    call memfun_nul ( o%vs1_ref )
    call memfun_nul ( o%dn2_ref )
    call memfun_nul ( o%vp2_ref )
    call memfun_nul ( o%vs2_ref )
    call memfun_nul ( o%num_scale )
    call memfun_nul ( o%amp_scale )
    call memfun_nul ( o%dx_dz )
    call memfun_nul ( o%dy_dz )
    call memfun_nul ( o%dz_dx )
    call memfun_nul ( o%dz_dy )
    call memfun_nul ( o%iz_inp )
    call memfun_nul ( o%rz_inp )
    call memfun_nul ( o%rz_pik )
    call memfun_nul ( o%rg_inp )
    call memfun_nul ( o%ra_inp )
    call memfun_nul ( o%rp_inp )
    call pathchoose_create (o%path_src_datum_button, 'path_src_datum', '*' )
    call pathchoose_create (o%path_rec_datum_button, 'path_rec_datum', '*' )
    call pathchoose_create (o%path_vel_button,       'path_vel',       '*' )
    !
    print'(" aa1 tpcik_create c=",i8)', i_call
    !
    call tpick_initialize ( o )
    !
    print'(" aa2 tpcik_create c=",i8)', i_call
    !
    call datumgrid_create ( o%dtm, 'tpick', i_err )
    !
    print'(" aa3 tpcik_create c=",i8)', i_call
    !
    call datumgrid_initialize ( o%dtm )
    !
    print'(" aa4 tpcik_create c=",i8)', i_call
    !
    call tpick_update ( o )
    !
    print'(" end tpcik_create c=",i8)', i_call
    !
    return
    !
  end subroutine tpick_create
  !
  subroutine tpick_delete ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Begin tpick_delete
    !
    ! delete the datumgrid structure
    !
    if ( associated       ( o%dtm ) ) &
    call datumgrid_delete ( o%dtm )
    call memfun_del ( o%dep_ref )
    call memfun_del ( o%x_dip_ref )
    call memfun_del ( o%y_dip_ref )
    call memfun_del ( o%dn1_ref )
    call memfun_del ( o%vp1_ref )
    call memfun_del ( o%vs1_ref )
    call memfun_del ( o%dn2_ref )
    call memfun_del ( o%vp2_ref )
    call memfun_del ( o%vs2_ref )
    call memfun_del ( o%num_scale )
    call memfun_del ( o%amp_scale )
    call memfun_del ( o%dx_dz )
    call memfun_del ( o%dy_dz )
    call memfun_del ( o%dz_dx )
    call memfun_del ( o%dz_dy )
    call memfun_del ( o%iz_inp )
    call memfun_del ( o%rz_inp )
    call memfun_del ( o%rz_pik )
    call memfun_del ( o%rg_inp )
    call memfun_del ( o%ra_inp )
    call memfun_del ( o%rp_inp )
    !
           if ( associated ( o%path_src_datum_button ) )  &
    call pathchoose_delete ( o%path_src_datum_button )
    !
           if ( associated ( o%path_rec_datum_button ) )  &
    call pathchoose_delete ( o%path_rec_datum_button )
    !
           if ( associated ( o%path_vel_button ) )  &
    call pathchoose_delete ( o%path_vel_button )
    !
    deallocate( o )
    !
  end subroutine tpick_delete
  !
  subroutine tpick_initialize ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Begin tpick_initialize
    !
    ! initialize parameters
    !
    !
    ! get the current globals
    !
    call timeglob_get ( o%nt_glb, o%t0_glb, o%dt_glb )
    !
    o%ref_scale_index  = 1
    o%init_scale_index = 1
    o%last_scale_index = 1
    o%dep_scale = 1000.
    o%nz_glb = o%nt_glb
    o%z0_glb = o%t0_glb * o%dep_scale
    o%dz_glb = o%dt_glb * o%dep_scale
    !
    o%hdr_x = 7
    o%hdr_y = 8
    !
    o%path_vel  = pathcheck_empty ! velocity file
    !
    o%azimuth       = 0.
    o%cos_pwr       = 0.
    o%max_inp       = 1000
    o%dep_scale     = 1000.
    o%dep_width     = 100.
    o%hdr_x      = 7
    o%hdr_y      = 8
    !
    o%v_src_rec = 1.    ! source to receiver velocity ratio
    o%const_vel   = 2000. ! constant velocity
    !
    o%n0_ref = 0
    !
    o%ipn = 0 ! process number
    !
    !call tpick_update ( o )
    !
  end subroutine tpick_initialize
  !
  subroutine tpick_update ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Local variables
    !
    ! Begin tpick_update
    !
    object => o         ! needed for traps.
    !
    o%skip_wrapup = .true.         
    !
    ! get the tpick parameters
    !
    call tpick_get ( o )
    !
    ! verify the tpick parameters
    !
    call tpick_verify ( o )
    !
    ! put the tpick parameters
    !
    call tpick_put ( o )
    !
    ! prep the tpick parameters
    !
    call tpick_prep ( o )
    !
  end subroutine tpick_update
  !
  subroutine tpick_get ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Local variables
    !
    ! Begin tpick_get 
    !
    ! get the rotation grid object
    !
    call pc_get_global ( 'grid' , o%grid_obj )
    !
    call timeglob_get ( o%nt_glb, o%t0_glb, o%dt_glb )
    !
    call pc_get_global ( 'nwih',  o%nh_inp )
    !
if ( pathchoose_update ( o%path_src_datum_button, o%dtm%s%path_surface )) &
return
if ( pathchoose_update ( o%path_rec_datum_button, o%dtm%r%path_surface )) &
return
if ( pathchoose_update ( o%path_vel_button,       o%path_vel      ) ) return
    !
      call pc_register_array_names ("dep_ref_arrayset", (/  &
                                    "dep_ref  ",            &
                                    "x_dip_ref",            &
                                    "y_dip_ref" /))

      call pc_register_array_names ("dn1_ref_arrayset", (/  &
                                    "dn1_ref",              &
                                    "vp1_ref",              &
                                    "vs1_ref" /))

      call pc_register_array_names ("dn2_ref_arrayset", (/  &
                                    "dn2_ref",              &
                                    "vp2_ref",              &
                                    "vs2_ref" /))

    !
    o%ipn = pc_get_ipn()
    !
    call pc_get ( 'path_vel',       o%path_vel       )
    call pc_get ( 'hdr_x',          o%hdr_x          )
    call pc_get ( 'hdr_y',          o%hdr_y          )
    call pc_get ( 'azimuth',        o%azimuth        )
    call pc_get ( 'cos_pwr',        o%cos_pwr        )
    call pc_get ( 'max_inp',        o%max_inp        )
    call pc_get ( 'ref_scale_index',  o%ref_scale_index  )
    call pc_get ( 'init_scale_index', o%init_scale_index )
    call pc_get ( 'last_scale_index', o%last_scale_index )
    call pc_get ( 'dep_scale',      o%dep_scale      )
    call pc_get ( 'dep_width',      o%dep_width      )
    call pc_get ( 'v_src_rcv',      o%v_src_rec      )
    call pc_get ( 'v_src_rec',      o%v_src_rec      )
    call pc_get ( 'const_vel',      o%const_vel      )
    !
    ! get datumgrid parameters
    !
    call datumgrid_get ( o%dtm )
    !
    !o%dep_scale = 1000.
    o%nz_glb = o%nt_glb
    o%z0_glb = o%t0_glb * o%dep_scale
    o%dz_glb = o%dt_glb * o%dep_scale
    !
    o%ndep_ref   = o%n0_ref
    o%nx_dip_ref = o%n0_ref
    o%ny_dip_ref = o%n0_ref
    o%ndn1_ref   = o%n0_ref
    o%nvp1_ref   = o%n0_ref
    o%nvs1_ref   = o%n0_ref
    o%ndn2_ref   = o%n0_ref
    o%nvp2_ref   = o%n0_ref
    o%nvs2_ref   = o%n0_ref
    call pc_alloc ( 'depth_ref', o%dep_ref,   o%ndep_ref   )
    call pc_alloc ( 'dep_ref',   o%dep_ref,   o%ndep_ref   )
    call pc_alloc ( 'x_dip_ref', o%x_dip_ref, o%nx_dip_ref )
    call pc_alloc ( 'y_dip_ref', o%y_dip_ref, o%ny_dip_ref )
    call pc_alloc ( 'dn1_ref',   o%dn1_ref,   o%ndn1_ref   )
    call pc_alloc ( 'vp1_ref',   o%vp1_ref,   o%nvp1_ref   )
    call pc_alloc ( 'vs1_ref',   o%vs1_ref,   o%nvs1_ref   )
    call pc_alloc ( 'dn2_ref',   o%dn2_ref,   o%ndn2_ref   )
    call pc_alloc ( 'vp2_ref',   o%vp2_ref,   o%nvp2_ref   )
    call pc_alloc ( 'vs2_ref',   o%vs2_ref,   o%nvs2_ref   )
    o%n0_ref     = o%ndep_ref
    !
    ! get datumgrid parameters
    !
    call datumgrid_get ( o%dtm )
    !
    return
    !
  end subroutine tpick_get 
  !
  subroutine tpick_put ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Local variables
    !
    integer                             :: n0_inp
    integer                             :: n_scr        
    integer                             :: n_sto        
    !
    ! Begin tpick_put
    !
    call pc_put ( 'path_vel',       o%path_vel       )
    call pc_put ( 'hdr_x',          o%hdr_x          )
    call pc_put ( 'hdr_y',          o%hdr_y          )
    call pc_put ( 'azimuth',        o%azimuth        )
    call pc_put ( 'cos_pwr',        o%cos_pwr        )
    call pc_put ( 'max_inp',        o%max_inp        )
    call pc_put ( 'ref_scale_index',  o%ref_scale_index  )
    call pc_put ( 'init_scale_index', o%init_scale_index )
    call pc_put ( 'last_scale_index', o%last_scale_index )
    call pc_put ( 'dep_scale',      o%dep_scale      )
    call pc_put ( 'dep_width',      o%dep_width      )
    call pc_put ( 'dep_ref',   o%dep_ref,   o%ndep_ref   )
    call pc_put ( 'x_dip_ref', o%x_dip_ref, o%nx_dip_ref )
    call pc_put ( 'y_dip_ref', o%y_dip_ref, o%ny_dip_ref )
    call pc_put ( 'dn1_ref',   o%dn1_ref,   o%ndn1_ref   )
    call pc_put ( 'vp1_ref',   o%vp1_ref,   o%nvp1_ref   )
    call pc_put ( 'vs1_ref',   o%vs1_ref,   o%nvs1_ref   )
    call pc_put ( 'dn2_ref',   o%dn2_ref,   o%ndn2_ref   )
    call pc_put ( 'vp2_ref',   o%vp2_ref,   o%nvp2_ref   )
    call pc_put ( 'vs2_ref',   o%vs2_ref,   o%nvs2_ref   )
    !
    call pc_put ( 'v_src_rec',         o%v_src_rec         )
    call pc_put ( 'const_vel',           o%const_vel           )
    !
    ! put datumgrid parameters
    !
    call datumgrid_put ( o%dtm )
    !
    !  put control characteristics
    !
    n0_inp     = 1
    n_scr      = 0
    n_sto      = 0
    !
    ! put the current globals
    !
    call timeglob_put ( o%nt_glb, o%t0_glb, o%dt_glb )
    !
    call pc_put_global  ('numtr',      n0_inp    )
    call pc_put_global  ('gathered',   .false.   )
    call pc_put_control ('need_label', .false.   )
    call pc_put_control ('nscratch',   n_scr     )
    call pc_put_control ('nstore',     n_sto     )
    !
    return
    !
  end subroutine tpick_put 
  !
  subroutine tpick_verify ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Local variables
    !
    integer                             :: i_err
    !
    ! Begin tpick_verify 
    !
    !
    if ( ( o%hdr_x == 07 .and. o%hdr_y /= 08 ) &
    .or. ( o%hdr_x == 08 .and. o%hdr_y /= 07 ) &
    .or. ( o%hdr_x == 17 .and. o%hdr_y /= 18 ) &
    .or. ( o%hdr_x == 18 .and. o%hdr_y /= 17 ) ) &
    call pc_error ( &
  'tpick : hdr_x and hdr_y must be a combination of 7,8 or 17,18 ')
    !
    ! check to tmake sure the x,y header words are correct
    !
    call migfun_check_xy_header_words ( &
    'tpick ', pc_get_lun(), o%hdr_x, o%hdr_y, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! get the y scale distance coefficient
    !
    call migfun_get_scale ( &
    'tpick fast', pc_get_lun(), o%grid_obj, o%hdr_x, o%rx_scl, i_err )
    !
    if ( i_err .ne. 0) go to 997
    !
    ! get the y scale distance coefficient
    !
    call migfun_get_scale ( &
    'tpick slow', pc_get_lun(), o%grid_obj, o%hdr_y, o%ry_scl, i_err )
    !
    if ( i_err .ne. 0) go to 996
    !
    call pathcheck ('path_vel',o%path_vel,required=.false.)
    !
    call string_replace_zeroes ( o%path_vel)
    !
    if ( len_trim(o%path_vel) .lt. 1) o%path_vel = pathcheck_empty
    !
    return
    !
996 continue
    !
    write(pc_get_lun(), ' ( &
    & /, "error in tpick_verify ", &
    & /," during migfun_get_scale y " &
    & /," hdr_x=", i8, " hdr_y=", i8 &
    & )') &
    o%hdr_x, o%hdr_y
    !
    i_err = -1
    !
    go to 999
    !
997 continue
    !
    write(pc_get_lun(), ' ( &
    & /, "error in tpick_verify ", &
    & /," during migfun_get_scale x " &
    & /," hdr_x=", i8, " hdr_y=", i8 &
    & )') &
    o%hdr_x, o%hdr_y
    !
    i_err = -1
    !
    go to 999
    !
998 continue
    !
    write(pc_get_lun(), ' ( &
    & /, "error in tpick_verify ", &
    & /," during migfun_check_xy_header_words x " &
    & /," hdr_x=", i8, " hdr_y=", i8 &
    & )') &
    o%hdr_x, o%hdr_y
    !
    i_err = -1
    !
    go to 999
    !
999 continue
    !
    write(pc_get_lun(), ' ( &
    & /, "error in tpick_verify " &
    & )') 
    !
    i_err = 0
    !
    return
    !
  end subroutine tpick_verify 
  !
  subroutine tpick_prep ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Local variables
    !
    integer                             :: i_err
    integer                             :: i0_ref
    !
    ! Begin tpick_prep
    !
    if ( pc_do_not_process_traces() ) return
    !
    o%skip_wrapup = .false.
    !
    write(pc_get_lun(), '(  &
    & /, " tpick_prep " ,/ ," REVISION: ", &
    & " 15  2007-05-29  Douglas Hanson Fix initialize order. " &
    & )')
    !
    o%ref_scale_index  = max ( o%ref_scale_index, 0 )
    !
    o%init_scale_index = max ( o%init_scale_index, 1 )
    !
    o%last_scale_index = max ( o%init_scale_index,  o%last_scale_index )
    !
    !  read the velocity file
    !
    call synvofz_read_velocity_file ( &
                                      o%path_vel, .false., &
                                      0., o%const_vel, 0., &
                                      o%hdr_x, o%rx_scl, &
                                      o%hdr_y, o%ry_scl, &
                                      o%nx_vel, o%x0_vel, o%dx_vel, &
                                      o%ny_vel, o%y0_vel, o%dy_vel, &
                                      o%nz_vel, o%z0_vel, o%dz_vel, &
                                      o%v0_vel, &
                                      i_err &
                                    )
    if ( i_err .ne. 0 ) go to 998
    !
    o%m0_inp   = o%max_inp
    o%i_rg_inp =              1
    o%i_ra_inp = o%i_rg_inp + 1
    o%i_rp_inp = o%i_ra_inp + 1
    o%i_ro_ref = o%i_rp_inp + 1
    o%i_rx_ref = o%i_ro_ref + 1
    o%i_ry_ref = o%i_rx_ref + 1
    o%i_rz_inp = o%i_ry_ref + 1
    o%i_rz_pik = o%i_rz_inp + 1
    o%m0_dat   = o%i_rz_pik 
    !
    call memfun_all ( o%r0_dat,    o%m0_inp, o%n0_ref, o%m0_dat, &
                                             'r0_dat',    i_err )
    call memfun_all ( o%num_scale, o%n0_ref, 'num_scale', i_err )
    call memfun_all ( o%amp_scale, o%n0_ref, 'amp_scale', i_err )
    call memfun_all ( o%dx_dz,     o%n0_ref, 'dx_dz',     i_err )
    call memfun_all ( o%dy_dz,     o%n0_ref, 'dy_dz',     i_err )
    call memfun_all ( o%dz_dx,     o%n0_ref, 'dz_dx',     i_err )
    call memfun_all ( o%dz_dy,     o%n0_ref, 'dz_dy',     i_err )
    call memfun_all ( o%iz_inp,    o%n0_ref, 'iz_inp',    i_err )
    call memfun_all ( o%rz_inp,    o%n0_ref, 'rz_inp',    i_err )
    call memfun_all ( o%rz_pik,    o%n0_ref, 'rz_pik',    i_err )
    call memfun_all ( o%rg_inp,    o%n0_ref, 'rg_inp',    i_err )
    call memfun_all ( o%ra_inp,    o%n0_ref, 'ra_inp',    i_err )
    call memfun_all ( o%rp_inp,    o%n0_ref, 'rp_inp',    i_err )
    !
    o%num_scale(1:o%n0_ref) = 0
    !
    o%amp_scale(1:o%n0_ref) = 0.
    !
    o%dz_dx(1:o%n0_ref) = 0.
    !
    o%dz_dy(1:o%n0_ref) = 0.
    !
    o%dz_dx(1:o%n0_ref) = &
    tan ( o%x_dip_ref(1:o%n0_ref) * radians_per_degree )
    !
    o%dz_dy(1:o%n0_ref) = &
    tan ( o%y_dip_ref(1:o%n0_ref) * radians_per_degree )
    !
    o%dx_dz(1:o%n0_ref) = 0.
    !
    o%dy_dz(1:o%n0_ref) = 0.
    !
    !do i0_ref = 1 , o%n0_ref
    !  print'( " i0_ref=", i8, " dz_dx=", g10.4 )', i0_ref, o%dz_dx(i0_ref) 
    !end do 
    !
    do_i0_ref : do i0_ref = 1 , o%n0_ref
      !
      if ( o%dz_dx(i0_ref) .ne. 0. ) &
           o%dx_dz(i0_ref) = &
      1. / o%dz_dx(i0_ref) 
      !
      if ( o%dz_dy(i0_ref) .ne. 0. ) &
           o%dy_dz(i0_ref) = &
      1. / o%dz_dy(i0_ref) 
      !
    end do do_i0_ref 
    !
    if ( string_upper_compare ( o%dtm%r%path_surface , 'SAME' ) ) &
                                o%dtm%r%path_surface = o%dtm%s%path_surface
    !
    ! surfacegrid_read will check this and change if need be.
    ! note x and y pos values are in original hdr_x, hdr_y units
    !
    ! get the source and receiver datums
    !
    call surfacegrid_read ( &
          o%dtm%s%opt_surface, o%dtm%s%path_surface,   o%dtm%s%const_surface, &
          o%hdr_x, o%dtm%s%nx_sur, o%dtm%s%x0_sur, o%dtm%s%dx_sur, 1., &
          o%hdr_y, o%dtm%s%ny_sur, o%dtm%s%y0_sur, o%dtm%s%dy_sur, 1., &
          o%dtm%s%rz_sur, &
          .true., .false., .true., &
          i_err &
                        )
    !
    if ( i_err .ne. 0 ) go to 997 
    !
    ! get the receiver datum
    !
    call surfacegrid_read ( &
            o%dtm%r%opt_surface, o%dtm%r%path_surface, o%dtm%r%const_surface, &
            o%hdr_x, o%dtm%s%nx_sur, o%dtm%s%x0_sur, o%dtm%s%dx_sur, 1., &
            o%hdr_y, o%dtm%s%ny_sur, o%dtm%s%y0_sur, o%dtm%s%dy_sur, 1., &
            o%dtm%r%rz_sur, &
            .true., .false., .true., &
            i_err &
                        )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! print the source and receiver datum levels
    !
    call surfacegrid_print ( &
            'tpick travel time datums', &
            pc_get_lun(), &
            o%dtm%s%opt_surface, o%dtm%s%path_surface, o%dtm%s%const_surface, &
            o%dtm%r%opt_surface, o%dtm%r%path_surface, o%dtm%r%const_surface, &
            o%hdr_x, o%dtm%s%nx_sur, o%dtm%s%x0_sur, o%dtm%s%dx_sur, 1., &
            o%hdr_y, o%dtm%s%ny_sur, o%dtm%s%y0_sur, o%dtm%s%dy_sur, 1., &
            2, &
            o%dtm%s%rz_sur, &
            o%dtm%r%rz_sur &
                         )
    !
    ! initialize the number of input traces, 
    !
    o%i0_inp = 0
    !
    o%ref_scale_index = min ( o%ref_scale_index, o%n0_ref )
    !
    ! print reflector info
    !
    call tpick_print_info ( o, pc_get_lun(), 'tpick_prep' )
    !
    return
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in tpick_prep ", &
    & /," during receiver surfacegrid_read " &
    & )') 
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in tpick_prep ", &
    & /," during source surfacegrid_read " &
    & )') 
    !
    go to 999
    !
998 continue
    !
    write(pc_get_lun(), ' ( &
    & /, "error in tpick_prep ", &
    & /," during tpick_read_velocity_file " &
    & )') 
    !
    i_err = -1
    !
    go to 999
    !
999 continue
    !
    write(pc_get_lun(), ' ( &
    &  /, " error in tpick_prep" &
    & )')
    !
    call pc_error('tpick_prep error ')
    !
    return
    !
  end subroutine tpick_prep 
  !
  subroutine tpick ( o, n0_inp, hd_inp, tr_inp )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    integer,             intent (inout) :: n0_inp           ! num input traces
    double precision,    intent (inout) :: hd_inp (:, :)    ! input headers
    real,                intent (inout) :: tr_inp (:, :)    ! input traces
    !
    ! Local variables
    !
    integer                             :: i_err
    integer                             :: j0_inp
    integer                             :: i0_inp
    integer                             :: i0_ref
    integer                             :: iz_pik_1
    integer                             :: iz_pik_2
    !
    ! Begin tpick
    !
    o%inc_flag = 'p'
    !
    o%att_flag = 'a'
    !
    o%mh_inp = size ( hd_inp , 1 )
    !
    o%mt_inp = size ( tr_inp , 1 )
    !
    !if ( o%i0_inp .eq. 0 ) &
    !print'(" aq2", &
    !& "  rg_inp   ra_inp   rp_inp ra_rec ", &
    !& " i0_ref i0_inp ro_ref rx_ref   ry_ref   rz_inp   " &
    !& )'
    !
    !if ( o%i0_inp .eq. 0 ) &
    !print'(" qq3", &
    !& " i0_ref i0_inp ro_ref ", &
    !& "rx_ref   rx_ref   ry_ref   ry_ref   rz_ref   rz_ref " &
    !& )'
    !
    do_traces : do j0_inp = 1 , n0_inp
      !
      o%i0_inp = o%i0_inp + 1
      !
      xxif_hdr_x : if ( o%hdr_x .eq. 7 ) then
        !
        o%so_ref = hd_inp ( HDR_SOURCE_XGRID   , j0_inp ) &
                 - hd_inp ( HDR_RECEIVER_XGRID , j0_inp )
        !
      else if ( o%hdr_x .eq. 17 ) then
        !
        o%so_ref = hd_inp ( HDR_SOURCE_XLOC   , j0_inp ) &
                 - hd_inp ( HDR_RECEIVER_XLOC , j0_inp )
        !
      else if ( o%hdr_x .eq. 8 ) then
        !
        o%so_ref = hd_inp ( HDR_SOURCE_YGRID   , j0_inp ) &
                 - hd_inp ( HDR_RECEIVER_YGRID , j0_inp )
        !
      else if ( o%hdr_x .eq. 18 ) then
        !
        o%so_ref = hd_inp ( HDR_SOURCE_YLOC   , j0_inp ) &
                 - hd_inp ( HDR_RECEIVER_YLOC , j0_inp )
        !
      else xxif_hdr_x 
        !
        o%so_ref = hd_inp ( HDR_SOURCE_YGRID   , j0_inp ) &
                 - hd_inp ( HDR_RECEIVER_YGRID , j0_inp )
        !
      end if xxif_hdr_x 
      !
      o%ro_ref = sign ( 1., o%so_ref ) * abs ( hd_inp ( hdr_offset, j0_inp ) ) 
      !
      o%rx_ref = hd_inp ( o%hdr_x,  j0_inp ) * o%rx_scl
      !
      o%ry_ref = hd_inp ( o%hdr_y,  j0_inp ) * o%ry_scl
      !
      o%rh_ref = o%ro_ref * .5
      !
      o%xh_ref = o%rh_ref * cos ( o%azimuth * radians_per_degree )
      !
      o%yh_ref = o%rh_ref * sin ( o%azimuth * radians_per_degree )
      !
      !print'(" tpick9 i=",i5," x=",g12.6," y=",g12.6," o=",g12.6)', &
      !o%i0_inp, o%rx_ref, o%ry_ref, o%ro_ref
      !
      !if ( o%i0_inp .eq. 1 ) &
      !print'(" qq0 j0_inp i0_inp ", &
      !& "rz_ref    ra_ref rz_max ra_max ro_ref  rx_ref    ry_ref  " )'
      !
      !  pick each event
      !
      !print*,' n0_ref=',o%n0_ref
      !
      do_i0_ref_2 : do i0_ref = 1 , o%n0_ref
        !
        o%i0_ref = i0_ref
        !
        ! compute the depth for this event
        !
        o%rz_ref = o%dep_ref(i0_ref) &
      + o%rx_ref * o%dz_dx (i0_ref) &
      + o%ry_ref * o%dz_dy( i0_ref) 
        !
        !print*,' i0_ref=',i0_ref,' rz_ref=',rz_ref
        !
        ! set the top and bot z search indicies, iz_pik_1, iz_pik_2
        !
        iz_pik_1 = max ( 1 , min( o%nz_glb, &
      nint ( ( o%rz_ref - o%dep_width - o%z0_glb ) / o%dz_glb ) + 1 ) )
        !
        iz_pik_2 = max ( 1 , min( o%nz_glb, &
      nint ( ( o%rz_ref + o%dep_width - o%z0_glb ) / o%dz_glb ) + 1 ) )
        !
        ! get the max amplitude index within the search window
        !
        o%iz_max = maxloc ( abs ( tr_inp(iz_pik_1:iz_pik_2,j0_inp ) ) ) 
        !
        o%iz_pik_0 = o%iz_max(1) + iz_pik_1 - 1
        !
        !print'(" q_i=",i3," x=",f8.2," y=",f8.2," z=",f8.2," i=",i6, &
        !& " dep=",f8.2," x_dip=",g12.6," y_dip=",g12.6, &
        !& " dz_dx=",g12.6," dz_dy=",g12.6)', &
        !!i0_ref, o%rx_ref, o%ry_ref, o%rz_ref, o%iz_pik_0, &
        !o%dep_ref(i0_ref), &
        !o%x_dip_ref(i0_ref), &
        !o%y_dip_ref(i0_ref), &
        !o%dz_dx (i0_ref), &
        !o%dz_dy (i0_ref)
        !
        ! do not do this, use the analytic depth from above instead
        ! get the max amplitude depth within the search window
        !
        !o%rz_ref = ( o%iz_pik_0 - 1 ) * o%dz_glb + o%z0_glb
        !
        o%rz_pik_0 = ( o%iz_pik_0 - 1 ) * o%dz_glb + o%z0_glb
        !
        ! compute the intersection of the plane normal at z=0
        !
        o%rx_norm = o%rx_ref + o%dz_dx (o%i0_ref ) * o%rz_ref
        !
        o%ry_norm = o%ry_ref + o%dz_dy (o%i0_ref ) * o%rz_ref
        !
        o%rz_norm = 0.
        !
        !print'(" q_xd=",g12.6," yd=",g12.6," zd=",g12.6)', &
        !o%dz_dx(o%i0_ref),o%dz_dy(o%i0_ref),o%dep_ref(o%i0_ref)
        !
        !print'(" q_xr=",g12.6," yr=",g12.6," zr=",g12.6," iz=",i8)', &
        !o%rx_ref, o%ry_ref, o%rz_ref, o%iz_pik_0 
        !
        !print'(" q_xn=",g12.6," yn=",g12.6," zn=",g12.6)', &
        !o%rx_norm, o%ry_norm, o%rz_norm
        !
        !print*,' i0_ref=',i0_ref,' iz_pik_0=',iz_pik_0
        !
        ! get the max amplitude value within the search window
        !
        o%ra_ref = tr_inp ( o%iz_pik_0, j0_inp ) 
        !
        !print*,' i0_ref=',i0_ref,' ra_ref=',ra_ref
        !
        ! get the global max loc and amplitude
        !
        o%iz_max = maxloc ( abs ( tr_inp(:,j0_inp ) ) )
        !
        o%rz_max = ( o%iz_max(1) - 1 ) * o%dz_glb + o%z0_glb
        !
        o%ra_max = maxval ( abs ( tr_inp(:,j0_inp ) ) )
        !
        !if ( j0_inp .eq. -1 ) &
        !print'(" qq0", &
        !& 1x, i4, 1x, i4, 1x, i4, &
        !& 1x, f8.2, 1x, f8.2, 1x, g12.6, 1x, g12.6, &
        !& 1x, f8.2, 1x, f8.2, 1x, f8.2 &
        !& )', &
        !j0_inp, o%i0_inp, o%i0_ref, &
        !o%rz_ref, o%rz_max, o%ra_ref, o%ra_max, &
        !o%ro_ref, o%rx_ref/o%rx_scl, o%ry_ref/o%ry_scl
        !
        !print'(" ww0", &
        !& 1x, i4, 1x, i4, 1x, i4, &
        !& 1x, f8.2, 1x, f8.2, &
        !& 1x, f8.2, 1x, f8.2, &
        !& 1x, f8.2, 1x, f8.2 &
        !& )', &
        !j0_inp, o%i0_inp, o%i0_ref, &
        !o%rx_ref, o%rx_norm, &
        !o%ry_ref, o%ry_norm, &
        !o%rz_ref, o%ro_ref
        !
        !xxif_z_err : if ( abs ( o%rz_ref - o%rz_max ) .gt. 10. ) then
          !
          !print'( &
          !& /, " i0_inp=",i8,&
          !& /, " rx_ref=", g12.6, " ry_ref=", g12.6, &
          !& /, " rz_ref=", g12.6, " rz_max=", g12.6, &
          !& /, " dz_dx=", g12.6, " dz_dy=", g12.6," dep_ref=",g12.6 &
          !& )', &
          !o%i0_inp, &
          !o%rx_ref, o%ry_ref, o%rz_ref,  o%rz_max, &
          !o%dz_dx(i0_ref), o%dz_dy(i0_ref), o%dep_ref(i0_ref) 
          !
          !stop
          !
        !end if xxif_z_err 
        !
        ! compute the angle of incidence and source receiver posiions
        !
        call tpick_compute_ang_src_rec ( o )
        !
        !print*,' dn1=', o%dn1_ref(i0_ref)
        !print*,' vp1=', o%vp1_ref(i0_ref)
        !print*,' vs1=', o%vs1_ref(i0_ref)
        !print*,' dn2=', o%dn2_ref(i0_ref)
        !print*,' vp2=', o%vp2_ref(i0_ref)
        !print*,' vs2=', o%vs2_ref(i0_ref)
        !if ( o%i0_inp .eq. 1 ) &
        !print'( &
        !& /, " qq1", 1x, i4, 1x, i4, 1x, i4, &
        !& /, " qq1", 1x, f8.2, &
        !& /, " qq1", 1x, f8.4, 1x, f8.2, 1x, f8.2, &
        !& /, " qq1", 1x, f8.4, 1x, f8.2, 1x, f8.2 &
        !& )', &
        !j0_inp, o%i0_inp, i0_ref, &
        !o%ang_inc*90./asin(1.), &
        !o%dn1_ref(i0_ref), o%vp1_ref(i0_ref), o%vs1_ref(i0_ref), &
        !o%dn2_ref(i0_ref), o%vp2_ref(i0_ref), o%vs2_ref(i0_ref)
        !
        ! solve the zoeppritz equations
        !
        ! solve the zoeppritz equations
        !
        call zoeppritz ( &
                      o%ang_inc, o%inc_flag, o%att_flag, &
                      o%dn1_ref(i0_ref), o%vp1_ref(i0_ref), o%vs1_ref(i0_ref), &
                      o%dn2_ref(i0_ref), o%vp2_ref(i0_ref), o%vs2_ref(i0_ref), &
                      o%refl_p, o%refl_s, o%tran_p, o%tran_s, i_err ) 
        !
        ! scale by angle ** cos_pwr ang_inc is in radians
        !
        !if ( o%i0_inp .eq. 1 ) &
        !print'(" tpickx amp_scale=",g12.6," cos_pwr=",g12.6, &
        !& " ra_ref=",g12.6," ra_ref*cos**cos_pwr=",g12.6 )', &
        !o%amp_scale ( i0_ref ), o%cos_pwr, &
        !o%ra_ref, o%ra_ref * cos ( o%ang_inc ) ** o%cos_pwr
        !
        o%ra_scl = o%ra_ref * cos ( o%ang_inc ) ** o%cos_pwr
        !
        ! compute the amplitude scale
        !
        xxif_amp_scale : &
        if ( o%i0_inp .ge. o%init_scale_index &
       .and. o%i0_inp .le. o%last_scale_index &
       .and. o%ra_scl .ne. 0. ) then
          !
          o%num_scale(i0_ref) = o%num_scale(i0_ref) + 1
          !
          o%amp_scale(i0_ref) = &
          o%amp_scale(i0_ref) + abs ( 1. / o%ra_scl )
          !o%amp_scale(i0_ref) + abs ( o%refl_p / o%ra_scl )
          !
          !print'(" tpickx i0_inp=",i8," s1=",i4," s2=",i4, &
          !& " num=",i4," amp=",g12.6," ra=",g12.6)', &
          !o%i0_inp, o%init_scale_index, o%last_scale_index, &
          !o%num_scale(i0_ref), o%amp_scale(i0_ref), o%ra_ref
          !
        end if xxif_amp_scale 
        !
        !print'(" tpicky i0_inp=",i8," s1=",i4," s2=",i4, &
        !& " num=",i4," amp=",g12.6," ra=",g12.6)', &
        !o%i0_inp, o%init_scale_index, o%last_scale_index, &
        !o%num_scale(i0_ref), o%amp_scale(i0_ref), o%ra_ref
        !
        !o%amp_scale(i0_ref) =     ( o%refl_p / o%ra_scl )
        !
        ! save this info
        !
        o%iz_inp ( i0_ref )  = o%iz_pik_0
        !
        o%rz_inp ( i0_ref )  = o%rz_ref
        !
        o%rz_pik ( i0_ref )  = o%rz_pik_0
        !
        o%rg_inp ( i0_ref ) = o%ang_inc * degrees_per_radian
        !
        o%ra_inp ( i0_ref ) = o%ra_scl 
        !
        o%rp_inp ( i0_ref ) = o%refl_p
        !
        ! print this info
        !
        i0_inp = o%i0_inp
        !
        !print'( &
        !& 1x, f8.2, 1x, g10.4, 1x, g10.4, &
        !& 1x, f8.2, 1x, f8.2, 1x, f8.2, 1x, f8.2, &
        !& 1x, i4, 1x, i4, &
        !& " tpick0" &
        !& )', &
        !o%rg_inp ( i0_ref ), o%ra_inp ( i0_ref ), o%rp_inp ( i0_ref ), &
        !o%ro_ref, o%rx_ref/o%rx_scl, o%ry_ref/o%ry_scl, o%rz_inp ( i0_ref ), & 
        !
        xxif_i0_inp : if ( o%i0_inp .le. o%m0_inp ) then
          !
          o%r0_dat ( i0_inp, i0_ref, o%i_rg_inp ) = o%rg_inp ( i0_ref )
          o%r0_dat ( i0_inp, i0_ref, o%i_ra_inp ) = o%ra_inp ( i0_ref )
          o%r0_dat ( i0_inp, i0_ref, o%i_rp_inp ) = o%rp_inp ( i0_ref )
          o%r0_dat ( i0_inp, i0_ref, o%i_ro_ref ) = o%ro_ref
          o%r0_dat ( i0_inp, i0_ref, o%i_rx_ref ) = o%rx_ref
          o%r0_dat ( i0_inp, i0_ref, o%i_ry_ref ) = o%ry_ref
          o%r0_dat ( i0_inp, i0_ref, o%i_rz_inp ) = o%rz_inp ( i0_ref ) 
          o%r0_dat ( i0_inp, i0_ref, o%i_rz_pik ) = o%rz_pik ( i0_ref ) 
          !
        end if xxif_i0_inp 
        !
        print'( &
        & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, &
        & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, &
        & 1x, i4, 1x, i4, 1x, i4, &
        & " tpick0" &
        & )', &
        o%r0_dat ( i0_inp, i0_ref, o%i_rg_inp ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_ra_inp ), & 
  sign ( 1., &
        o%r0_dat (      1, i0_ref, o%i_rp_inp ) ) * & 
  abs ( o%r0_dat ( i0_inp, i0_ref, o%i_ra_inp ) ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_rp_inp ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_ro_ref ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_rx_ref ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_ry_ref ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_rz_inp ), &
        o%r0_dat ( i0_inp, i0_ref, o%i_rz_pik ), &
        i0_inp, i0_ref, o%ipn
        !
      end do do_i0_ref_2 
      !
    end do do_traces 
    !
    if ( n0_inp .eq. 0 ) call tpick_print_results ( o )
    !
    return
    !
  end subroutine tpick
  !
  subroutine tpick_print_results ( o )
    !
    ! print results
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    ! Local variables
    !
    integer                             :: i0_inp
    integer                             :: i0_ref
    integer                             :: ref_scale_index 
    !
    print'(" tpick_print_results ", &
    & " init_scale_index=", i8, " last_scale_index=", i8 )', &
    o%init_scale_index, o%last_scale_index
    !
    do_i0_ref : do i0_ref = 1 , o%n0_ref
      !
      ref_scale_index = o%ref_scale_index
      !
      if ( ref_scale_index .le. 0 ) &
      ref_scale_index = i0_ref
      !
      o%amp_scale ( i0_ref ) = &
      o%amp_scale ( i0_ref ) / max ( 1, o%num_scale ( i0_ref ) ) 
      !
      print'(" tpick_print_results i0_ref=",i8, &
      & " num_scale=",i8," amp_scale=",g12.6)', &
      i0_ref, o%num_scale(i0_ref), o%amp_scale(i0_ref) 
      !
      do_i0_inp : do i0_inp = 1 , o%i0_inp
        !
        print'( &
        & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, &
        & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, &
        & 1x, i4, 1x, i4, 1x, i4, &
        & " tpick1" &
        & )', &
        o%r0_dat ( i0_inp, i0_ref, o%i_rg_inp ), & 
  sign ( 1., &
        o%r0_dat (      1, i0_ref, o%i_rp_inp ) ) * & 
        o%r0_dat ( i0_inp, i0_ref, o%i_ra_inp ), &
  abs ( o%r0_dat ( i0_inp, i0_ref, o%i_ra_inp ) ) * &
        o%amp_scale ( ref_scale_index ), & 
!  abs ( o%r0_dat ( i0_inp, i0_ref, o%i_ra_inp ) ) * o%amp_scale ( i0_ref ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_rp_inp ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_ro_ref ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_rx_ref ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_ry_ref ), & 
        o%r0_dat ( i0_inp, i0_ref, o%i_rz_inp ), &
        o%r0_dat ( i0_inp, i0_ref, o%i_rz_pik ), &
        i0_inp, i0_ref, o%ipn
        !
      end do do_i0_inp 
      !
      print'( " tpick1 ")'
      !
    end do do_i0_ref 
    !
    return
    !
  end subroutine tpick_print_results 
  !
  subroutine tpick_wrapup ( o )
    !
    ! Arguments
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    if ( o%skip_wrapup ) return
    !
         o%skip_wrapup = .true.
    !
  end subroutine tpick_wrapup
  !
  subroutine tpick_print_info ( o, lu_out, c_title )
    !
    ! print reflector or diffractor info
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    integer,             intent (in   ) :: lu_out         ! print unit number
    character (len = *), intent (in   ) :: c_title        ! title 1 to print
    !
    integer                             :: i0_ref
    !
    if (lu_out .lt. 0) return
    !
    write ( lu_out, '( &
    & /, " tpick_print_info ", &
    & /, a, &
    & /, " path_vel             =", a, &
    & /, " path_src_datum       =", a, &
    & /, " path_rec_datum       =", a, &
    & /, " const_vel            =", g12.6, &
    & /, " v_src_rec            =", g12.6, &
    & /, " const_source         =", g12.6, &
    & /, " const_receiver       =", g12.6, &
    & /, " dep_scale            =", g12.6, &
    & /, " dep_width            =", g12.6, &
    & /, " azimuth              =", g12.6, &
    & /, " cos_pwr              =", g12.6, &
    & /, " hdr_x                =", i8, &
    & /, " hdr_y                =", i8, &
    & /, " number of reflectors =", i8 &
    & )') &
    trim ( c_title ), &
    trim ( o%path_vel ), &
    trim ( o%dtm%s%path_surface ), &
    trim ( o%dtm%r%path_surface ), &
    o%const_vel,    o%v_src_rec, &
    o%dtm%s%const_surface, o%dtm%r%const_surface, &
    o%dep_scale,    o%dep_width, &
    o%azimuth,      o%cos_pwr, &
    o%hdr_x, o%hdr_y, &
    o%n0_ref
    !
    write ( lu_out, '( &
    & /, " tpick_print_info ", &
    & /, " fast       slow        depth     ", &
    &    " dn1        vp1       vs1        ", & 
    &    " dn2        vp2       vs2        ", &
    &    "     index" &
    & )') 
    !
    write ( lu_out, '( &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, &
    & 1x, i8 &
    & )') &
    ( o%x_dip_ref(i0_ref), o%y_dip_ref(i0_ref), o%dep_ref(i0_ref), &
      o%dn1_ref(i0_ref),   o%vp1_ref(i0_ref),   o%vs1_ref(i0_ref), &
      o%dn2_ref(i0_ref),   o%vp2_ref(i0_ref),   o%vs2_ref(i0_ref), &
      i0_ref, i0_ref = 1 , o%n0_ref )
    !
    write ( lu_out, '( &
    & /, " tpick_print_info ", &
    & /, " fast       slow        depth     ", &
    &    " dz_dx      dz_dy    ", & 
    &    " dx_dz      dy_dz    ", & 
    &    "     index" &
    & )') 
    !
    write ( lu_out, '( &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, &
    & 1x, g10.4, 1x, g10.4, &
    & 1x, g10.4, 1x, g10.4, &
    & 1x, i8 &
    & )') &
    ( o%x_dip_ref(i0_ref), o%y_dip_ref(i0_ref), o%dep_ref(i0_ref), &
      o%dz_dx(i0_ref), o%dz_dy(i0_ref), &
      o%dx_dz(i0_ref), o%dy_dz(i0_ref), &
      i0_ref, i0_ref = 1 , o%n0_ref )
    !
    return
    !
  end subroutine tpick_print_info
  !
  subroutine tpick_compute_ang_src_rec ( o )
    !
    ! compute the source and reciever posiitons and incidence angle 
    !
    type (tpick_struct),        pointer :: o              ! tpick structure
    !
    real                                :: rx_ref 
    real                                :: ry_ref 
    real                                :: rz_ref 
    !
    call tpick_compute_ref_pos ( o, o%rx_mid, o%ry_mid )
    !
    ! compute the source location from the midpoint and offset
    !
    o%rx_src = o%rx_mid + o%xh_ref
    !
    o%ry_src = o%ry_mid + o%yh_ref
    !
    ! get the source z location
    !
    call interpolate_2d_to_0d ( &
                              o%dtm%s%nx_sur, o%dtm%s%x0_sur, o%dtm%s%dx_sur, &
                              o%dtm%s%ny_sur, o%dtm%s%y0_sur, o%dtm%s%dy_sur, &
                              o%dtm%s%rz_sur, &
                              o%rx_src / o%rx_scl, &
                              o%ry_src / o%ry_scl, &
                              o%rz_src &
                              )
    !
    ! compute the receiver location from the midpoint and offset
    !
    o%rx_rec = o%rx_mid - o%xh_ref
    !
    o%ry_rec = o%ry_mid - o%yh_ref
    !
    ! get the receiver z location
    !
    call interpolate_2d_to_0d ( &
                              o%dtm%r%nx_sur, o%dtm%r%x0_sur, o%dtm%r%dx_sur, &
                              o%dtm%r%ny_sur, o%dtm%r%y0_sur, o%dtm%r%dy_sur, &
                              o%dtm%r%rz_sur, &
                              o%rx_rec / o%rx_scl, &
                              o%ry_rec / o%ry_scl, &
                              o%rz_rec &
                              )
    !
    ! compute the incidience angle
    !
    call tpick_compute_ang_inc ( &
                                 o%rx_src, o%ry_src, o%rz_src, &
                                 o%rx_rec, o%ry_rec, o%rz_rec, &
                                 o%rx_ref, o%ry_ref, o%rz_ref, &
                                 o%ang_inc &
                               )
    !
    call tpick_compute_ang_norm ( o, o%rx_src, o%ry_src, o%ang_src )
    !
    call tpick_compute_ang_norm ( o, o%rx_rec, o%ry_rec, o%ang_rec )
    !
    ! compute the reflection position for this source receiver pair
    ! to check against the input value
    !
    call synref_plane_reflection_pos ( &
     o%dz_dx(o%i0_ref), o%dz_dy(o%i0_ref), o%dep_ref(o%i0_ref), &
     o%rx_src, o%ry_src, o%rz_src, &
     o%rx_rec, o%ry_rec, o%rz_rec, &
       rx_ref,   ry_ref,   rz_ref &
                                    )
    !
    !print'( " qq3", &
    !& 1x, i4, 1x, i4, 1x, f8.2, &
    !& 1x, f8.2, 1x, f8.2, &
    !& 1x, f8.2, 1x, f8.2, &
    !& 1x, f8.2, 1x, f8.2 &
    !& )', &
    !o%i0_ref, o%i0_inp, o%ro_ref, &
    !o%rx_ref/o%rx_scl, rx_ref/o%rx_scl, &
    !o%ry_ref/o%ry_scl, ry_ref/o%ry_scl, &
    !o%rz_ref,          rz_ref
    !
    return
    !
  end subroutine tpick_compute_ang_src_rec 
  !
  subroutine tpick_compute_ref_pos ( o, rx_mid, ry_mid )
    !
    ! compute the trace midpoitn which matches this relfection position
    !
    type (tpick_struct),        pointer :: o      ! tpick structure
    real,                 intent(inout) :: rx_mid
    real,                 intent(inout) :: ry_mid
    !
    real                                :: pos_diff_try
    real                                :: pos_diff_gold
    real                                :: rx_try_m
    real                                :: rx_try_0
    real                                :: rx_try_p
    real                                :: rx_min
    real                                :: ry_try_m
    real                                :: ry_try_0
    real                                :: ry_try_p
    real                                :: ry_min
    !
    rx_mid = o%rx_norm
    !
    ry_mid = o%ry_norm
    !
    ! set the midpoint to the intersection 
    ! of the plane normal with the z=0 plane
    !
    rx_min   = 100.
    !
    rx_try_0 = rx_mid 
    !
    rx_try_m = rx_mid - max ( rx_min, abs ( o%xh_ref ) )
    !
    rx_try_p = rx_mid + max ( rx_min, abs ( o%xh_ref ) )
    !
    ry_min   = 100.
    !
    ry_try_0 = ry_mid
    !
    ry_try_m = ry_mid - max ( ry_min, abs ( o%yh_ref ) )
    !
    ry_try_p = ry_mid + max ( ry_min, abs ( o%yh_ref ) )
    !
    o%rx_tol = 1.e-3
    !
    ! compute the angle difference using the normal location
    !
    pos_diff_try = -1.
    !
    pos_diff_try = tpick_compute_pos_diff ( o, rx_try_0, ry_try_0 )
    !
    ! find the surface midpoint by a glden search over the x midpoint
    !
    pos_diff_gold = tpick_compute_pos_gold ( &
    o, rx_try_m, rx_try_0, rx_try_p, o%rx_tol, rx_mid, ry_mid )
    !
    !print'(" qq4 x=",g12.6,1x,g12.6," y=",g12.6,1x,g12.6,&
    !& " d=",g12.6,1x,g12.6)', &
    !rx_mid, o%rx_norm, &
    !ry_mid, o%ry_norm, &
    !pos_diff_gold, pos_diff_try
    !
    return
    !
  end subroutine tpick_compute_ref_pos
  !
  subroutine tpick_compute_ang_inc ( &
                                     rx_src, ry_src, rz_src, &
                                     rx_rec, ry_rec, rz_rec, &
                                     rx_ref, ry_ref, rz_ref, &
                                     ang_inc &
                                   )
    !
    ! compute the angle of incidence by taking the dot product 
    ! of   the source   to reflector vector 
    ! with the receiver to reflector vector 
    !
    real,                 intent(in   ) :: rx_src
    real,                 intent(in   ) :: ry_src
    real,                 intent(in   ) :: rz_src
    real,                 intent(in   ) :: rx_rec
    real,                 intent(in   ) :: ry_rec
    real,                 intent(in   ) :: rz_rec
    real,                 intent(in   ) :: rx_ref
    real,                 intent(in   ) :: ry_ref
    real,                 intent(in   ) :: rz_ref
    real,                 intent(  out) :: ang_inc
    !
    real                                :: dx_src
    real                                :: dy_src
    real                                :: dz_src
    real                                :: rr_src
    real                                :: dx_rec
    real                                :: dy_rec
    real                                :: dz_rec
    real                                :: rr_rec
    real                                :: ang_cos
    !
    dx_src = rx_src - rx_ref
    !
    dy_src = ry_src - ry_ref
    !
    dz_src = rz_src - rz_ref
    !
    rr_src = sqrt ( dx_src**2 + dy_src**2 + dz_src**2 )
    !
    dx_rec = rx_rec - rx_ref
    !
    dy_rec = ry_rec - ry_ref
    !
    dz_rec = rz_rec - rz_ref
    !
    rr_rec = sqrt ( dx_rec**2 + dy_rec**2 + dz_rec**2 )
    !
    ang_cos = ( dx_src * dx_rec + dy_src * dy_rec + dz_src * dz_rec ) &
            / ( rr_src * rr_rec )

    !
    ang_inc = .5 * acos ( max ( -1., min ( +1., ang_cos ) ) )
    !
    return
    !
  end subroutine tpick_compute_ang_inc
  !
  subroutine tpick_compute_ang_both ( o, rx_try, ry_try, ang_src, ang_rec )
    !
    ! compute the source and receiver angles relative to the surface normal
    ! for a given source, receiver, reflector configuration
    ! by taking the dot products of :
    ! the source   to reflector vector to the normal vector
    ! and 
    ! the receiver to reflector vector to the normal vector
    !
    type (tpick_struct),        pointer :: o      ! tpick structure
    real,                intent( in   ) :: rx_try
    real,                intent( in   ) :: ry_try
    real,                intent( inout) :: ang_src
    real,                intent( inout) :: ang_rec
    !
    real                                :: rx_src
    real                                :: ry_src
    real                                :: rx_rec
    real                                :: ry_rec
    !
    rx_src = rx_try + o%xh_ref
    !
    ry_src = ry_try + o%yh_ref
    !
    call tpick_compute_ang_norm ( o, rx_src, ry_src, ang_src )
    !
    rx_rec = rx_try - o%xh_ref
    !
    ry_rec = ry_try - o%yh_ref
    !
    call tpick_compute_ang_norm ( o, rx_rec, ry_rec, ang_rec )
    !
    return
    !
  end subroutine tpick_compute_ang_both
  !
  subroutine tpick_compute_ang_norm ( o, rx_src, ry_src, ang_src )
    !
    ! compute the source angle relative to the surface normal
    ! for a given source, reflector configuration
    ! by taking the dot products of :
    ! the source   to reflector vector to the normal vector
    !
    type (tpick_struct),        pointer :: o      ! tpick structure
    real,                intent( in   ) :: rx_src
    real,                intent( in   ) :: ry_src
    real,                intent( inout) :: ang_src
    !
    real                                :: dx_ref
    real                                :: dy_ref
    real                                :: dz_ref
    real                                :: rr_ref
    real                                :: dx_src
    real                                :: dy_src
    real                                :: dz_src
    real                                :: rr_src
    real                                :: rz_src
    real                                :: ang_cos
    !
    ! get the source z location
    !
    call interpolate_2d_to_0d ( &
                              o%dtm%s%nx_sur, o%dtm%s%x0_sur, o%dtm%s%dx_sur, &
                              o%dtm%s%ny_sur, o%dtm%s%y0_sur, o%dtm%s%dy_sur, &
                              o%dtm%s%rz_sur, &
                              rx_src / o%rx_scl, &
                              ry_src / o%ry_scl, &
                              rz_src &
                              )
    !
    ! the vector from the sorce to the plane is 
    ! dx_src/rr_src, dy_src/rr_src, dz_src/rr_src
    !
    dx_src = rx_src - o%rx_ref
    !
    dy_src = ry_src - o%ry_ref
    !
    dz_src = rz_src - o%rz_ref
    !
    rr_src = sqrt ( dx_src**2 + dy_src**2 + dz_src**2 )
    !
    ! the normal vector to the plane is 
    ! dx_ref/rr_ref, dy_ref/rr_ref, dz_ref/rr_ref
    !
    dx_ref = o%rx_norm - o%rx_ref
    !
    dy_ref = o%ry_norm - o%ry_ref
    !
    dz_ref = o%rz_norm - o%rz_ref
    !
    !dx_ref = o%dz_dx(o%i0_ref)
    !
    !dy_ref = o%dz_dy(o%i0_ref)
    !
    !dz_ref = - 1.
    !
    rr_ref = sqrt ( dx_ref**2 + dy_ref**2 + dz_ref**2 )
    !
    ang_cos = ( dx_src * dx_ref + dy_src * dy_ref + dz_src * dz_ref ) &
            / ( rr_src * rr_ref )

    !
    ang_src = acos ( max ( -1., min ( +1., ang_cos ) ) )
    !
    return
    !
  end subroutine tpick_compute_ang_norm
  !
  real function tpick_compute_pos_diff ( o, rx_try, ry_try ) 
    !
    ! compute the difference in positions 
    ! for a midpoint at rx_try, ry_try
    !
    type (tpick_struct),        pointer :: o      ! tpick structure
    real,                intent (in   ) :: rx_try
    real,                intent (in   ) :: ry_try
    !
    real                                :: pos_diff
    real                                :: rx_src
    real                                :: ry_src
    real                                :: rz_src
    real                                :: rx_rec
    real                                :: ry_rec
    real                                :: rz_rec
    real                                :: rx_ref
    real                                :: ry_ref
    real                                :: rz_ref
    !
    rx_src = rx_try + o%xh_ref
    !
    ry_src = ry_try + o%yh_ref
    !
    ! get the source z location
    !
    call interpolate_2d_to_0d ( &
                              o%dtm%s%nx_sur, o%dtm%s%x0_sur, o%dtm%s%dx_sur, &
                              o%dtm%s%ny_sur, o%dtm%s%y0_sur, o%dtm%s%dy_sur, &
                              o%dtm%s%rz_sur, &
                              rx_src / o%rx_scl, &
                              ry_src / o%ry_scl, &
                              rz_src &
                              )
    !
    rx_rec = rx_try - o%xh_ref
    !
    ry_rec = ry_try - o%yh_ref
    !
    ! get the source z location
    !
    call interpolate_2d_to_0d ( &
                              o%dtm%r%nx_sur, o%dtm%r%x0_sur, o%dtm%r%dx_sur, &
                              o%dtm%r%ny_sur, o%dtm%r%y0_sur, o%dtm%r%dy_sur, &
                              o%dtm%r%rz_sur, &
                              rx_rec / o%rx_scl, &
                              ry_rec / o%ry_scl, &
                              rz_rec &
                              )
    !
    ! compute the reflection position for this source receiver pair
    ! to check against the input value
    !
    call synref_plane_reflection_pos ( &
     o%dz_dx(o%i0_ref), o%dz_dy(o%i0_ref), o%dep_ref(o%i0_ref), &
     rx_src, ry_src, rz_src, &
     rx_rec, ry_rec, rz_rec, &
     rx_ref, ry_ref, rz_ref &
                                    )
    !
    ! compute the distance between the picked reflection position
    ! and the estimated reflection position
    !
    pos_diff = sqrt ( &
                        ( o%rx_ref - rx_ref ) ** 2 &
                      + ( o%ry_ref - ry_ref ) ** 2 &
                      + ( o%rz_ref - rz_ref ) ** 2 &
                    )
    !
    tpick_compute_pos_diff = pos_diff 
    !
    !print'(" qq4 m=",i4," o=",g10.4," d=",g12.6, &
    !& " x=",g10.4,1x,g10.4, &
    !& " y=",g10.4,1x,g10.4, &
    !& " z=",g10.4,1x,g10.4 &
    !& )', &
    !o%i0_inp, o%ro_ref, pos_diff, &
    !o%rx_ref, rx_ref, &
    !o%ry_ref, ry_ref, &
    !o%rz_ref, rz_ref
    !
    !print'( &
    !& /, " ww3 i0_inp=",i8," pos_diff=",g12.6, &
    !& /, " ww3  src x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ww3  rec x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ww3 cref x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ww3 iref x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ww3 norm x=",g12.6," y=",g12.6, &
    !& /, " ww3  try x=",g12.6," y=",g12.6, &
    !& /, " ww3 dz_d x=",g12.6," y=",g12.6, &
    !& /, " ww3 d_dz x=",g12.6," y=",g12.6, &
    !& /, " ww3 hoff x=",g12.6," y=",g12.6 &
    !& )', &
    !o%i0_inp, pos_diff, &
    !    rx_src,     ry_src,     rz_src, &
    !    rx_rec,     ry_rec,     rz_rec, &
    !    rx_ref,     ry_ref,     rz_ref, &
    !o%rx_ref, o%ry_ref, o%rz_ref, &
    !o%rx_norm, o%ry_norm, &
    !    rx_try,     ry_try, &
    !o%dz_dx(o%i0_ref), o%dz_dy(o%i0_ref), &
    !o%dx_dz(o%i0_ref), o%dy_dz(o%i0_ref), &
    !o%xh_ref, o%yh_ref
    !
    !if ( pos_diff .ge. 1. ) stop
    !if ( pos_diff .ne. -999 ) stop
    !
    return
    !
  end function tpick_compute_pos_diff 
  !
  real function tpick_compute_pos_gold0 ( &
  o, ax, bx, cx, rx_tol, rx_gold, ry_gold )
    !
    type (tpick_struct),        pointer :: o      ! tpick structure
    real,                intent (inout) :: ax
    real,                intent (inout) :: bx
    real,                intent (inout) :: cx
    real,                intent (in   ) :: rx_tol
    real,                intent (inout) :: rx_gold
    real,                intent (in   ) :: ry_gold
    !
    real                                :: r = .61803399
    real                                :: c = .38196602
    real                                :: x0
    real                                :: x1
    real                                :: x2
    real                                :: x3
    real                                :: f1
    real                                :: f2
    !
    !print'(" pos_gold 1 x=",g12.6,1x,g12.6,1x,g12.6)',&
    !ax, bx, cx
    !
    x0 = ax
    !
    x3 = cx
    !
    xxif_cx_bx : &
    if ( abs ( cx - bx ) .gt. abs ( bx - ax )  )  then
      !
      x1 = bx
      !
      x2 = bx + c *  ( cx - bx ) 
      !
    else xxif_cx_bx 
      !
      x2 = bx
      !
      x1 = bx - c *  ( bx - ax ) 
      !
    end if xxif_cx_bx 
    !
    f1 = tpick_compute_pos_diff ( o, x1, ry_gold ) 
    !
    f2 = tpick_compute_pos_diff ( o, x2, ry_gold ) 
    !
    !print'(" pos_gold 3 x=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
    !x1,x2,f1,f2
    !
1   continue
    !
    xxif_x3_x0 : &
    if ( abs ( x3 - x0 ) .gt. rx_tol *  ( abs ( x1 )  + abs ( x2 ) ) ) then
      !
      xxif_f2_f1 : if ( f2 .lt. f1 ) then
        !
        x0 = x1
        x1 = x2
        x2 = r * x1 + c * x3
        f1 = f2
        f2 = tpick_compute_pos_diff ( o, x2, ry_gold ) 
        !
    !print'(" pos_gold 4 x=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
    !x1,x2,f1,f2
        !
      else xxif_f2_f1
        !
        x3 = x2
        x2 = x1
        x1 = r * x2 + c * x0
        f2 = f1
        f1 = tpick_compute_pos_diff ( o, x1, ry_gold ) 
        !
    !print'(" pos_gold 5 x=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
    !x1,x2,f1,f2
        !
      end if xxif_f2_f1
      !
      go to 1
      !
    end if xxif_x3_x0 
    !
    !print'(" pos_gold 6 x=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
    !x1,x2,f1,f2
    !
    xxif_f1_f2 :  if ( f1 .lt. f2 ) then
      !
      tpick_compute_pos_gold0  = f1
      !
      rx_gold = x1
      !
    else xxif_f1_f2 
      !
      tpick_compute_pos_gold0  = f2
      !
      rx_gold = x2
      !
    end if xxif_f1_f2 
    !
    !print'(" pos_gold 7 x=",g12.6,1x,g12.6,1x,g12.6)',&
    !rx_gold,x1,x2
    !
    return
    !
  end function tpick_compute_pos_gold0 
  !
  real function tpick_compute_pos_gold ( &
  o, ax, bx, cx, rx_tol, rx_gold, ry_gold )
    !
    type (tpick_struct),        pointer :: o      ! tpick structure
    real,                intent (inout) :: ax
    real,                intent (inout) :: bx
    real,                intent (inout) :: cx
    real,                intent (in   ) :: rx_tol
    real,                intent (inout) :: rx_gold
    real,                intent (in   ) :: ry_gold
    !
    real                                :: r = .61803399
    real                                :: c = .38196602
    real                                :: x0
    real                                :: x1
    real                                :: x2
    real                                :: x3
    real                                :: f1
    real                                :: f2
    !
    x0 = ax
    !
    x3 = cx
    !
    xxif_cx_bx : &
    if ( abs ( cx - bx ) .gt. abs ( bx - ax )  )  then
      !
      x1 = bx
      !
      x2 = bx + c *  ( cx - bx ) 
      !
    else xxif_cx_bx 
      !
      x2 = bx
      !
      x1 = bx - c *  ( bx - ax ) 
      !
    end if xxif_cx_bx 
    !
    f1 = tpick_compute_pos_diff ( o, x1, ry_gold ) 
    !
    f2 = tpick_compute_pos_diff ( o, x2, ry_gold ) 
    !
1   continue
    !
    xxif_x3_x0 : &
    if ( abs ( x3 - x0 ) .gt. rx_tol *  ( abs ( x1 )  + abs ( x2 ) ) ) then
      !
      xxif_f2_f1 : if ( f2 .lt. f1 ) then
        !
        x0 = x1
        x1 = x2
        x2 = r * x1 + c * x3
        f1 = f2
        f2 = tpick_compute_pos_diff ( o, x2, ry_gold ) 
        !
      else xxif_f2_f1
        !
        x3 = x2
        x2 = x1
        x1 = r * x2 + c * x0
        f2 = f1
        f1 = tpick_compute_pos_diff ( o, x1, ry_gold ) 
        !
      end if xxif_f2_f1
      !
      go to 1
      !
    end if xxif_x3_x0 
    !
    xxif_f1_f2 :  if ( f1 .lt. f2 ) then
      !
      tpick_compute_pos_gold = f1
      !
      rx_gold = x1
      !
    else xxif_f1_f2 
      !
      tpick_compute_pos_gold = f2
      !
      rx_gold = x2
      !
    end if xxif_f1_f2 
    !
    return
    !
  end function tpick_compute_pos_gold 
  !
end module tpick_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
