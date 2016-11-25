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
! Name       : SYNGRID
! Category   : Synthetics
! Written    : 2006-04-04   by: Douglas Hanson
! Revised    : 2008-01-31   by: Douglas Hanson Add OPT_XY_ORDER.
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

!<int_calling_doc>
!------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  SYNGRID defines trace location attributes for synthetic generation.
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! NO input traces -- syngrid is a trace supplying process.
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs GROUP_SIZE traces at a time.
! This is a trace-supplying process.
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
!  8  2008-01-31  Douglas Hanson Add OPT_XY_ORDER.
!  7  2007-05-29  Douglas Hanson Fix migfun_offset hdr.
!  6  2007-05-03  Douglas Hanson Add datum headers.
!  5  2007-04-12  Douglas Hanson Datumgrid changes.
!  4  2007-01-03  Douglas Hanson Remove synmod parameters.
!  3  2006-07-06  Douglas Hanson Add synmod parameters.
!  2  2006-06-08  Douglas Hanson Remove print.
!  1  2006-04-20  Douglas Hanson Original version.
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
! Synthetic trace locations
!
! HDR_X=`C      HDR_Y=`C   OPT_XY_ORDER=`CC
! GATHER_TYPE~~~=`CCCCCCC  OPT_OFFSET_SIGN=`CCC    AZIMUTH~~~=`FFFFFFF 
!
! GAT_X_TOT=`IIIIIII GAT_X_INIT=`FFFFFFF GAT_X_LAST=`FFFFFFF GAT_X_INC=`FFFFFFFF
! GAT_Y_TOT=`IIIIIII GAT_Y_INIT=`FFFFFFF GAT_Y_LAST=`FFFFFFF GAT_Y_INC=`FFFFFFFF
! OFF_X_TOT=`IIIIIII OFF_X_INIT=`FFFFFFF OFF_X_LAST=`FFFFFFF OFF_X_INC=`FFFFFFFF
! OFF_Y_TOT=`IIIIIII OFF_Y_INIT=`FFFFFFF OFF_Y_LAST=`FFFFFFF OFF_Y_INC=`FFFFFFFF
!
! OPT_INPUT~~~~~=`CCCCCCCC                
! NUM_DUPLICATE =`IIIIIIIIIII
! GROUP_SIZE~~~~=`IIIIIIIIIII
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="OPT_XY_ORDER">
!<Tip> Output data in X_Y or Y_X order. </Tip>
! Default = X_Y
! Allowed = X_Y  Output data in X,Y order.
! Allowed = Y_X  Output data in Y,X order.
!</Help>
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
!<Help KEYWORD="HDR_X">
!<Tip> Header word designating fast direction. </Tip>
! Default = 7
! Allowed = 7, 8, 17, 18
!  HDR_X and HDR_Y and define the fast and slow lateral
!  position variation of ouput traces.  The allowed combinations are:
!  HDR_X =  7, HDR_Y =  8
!  HDR_X =  8, HDR_Y =  7
!  HDR_X = 17, HDR_Y = 18
!  HDR_X = 18, HDR_Y = 17
!
!</Help>
!
!<Help KEYWORD="GAT_X_TOT">
!<Tip> Number of gather bins in the fast direction. </Tip>
! Default = 1
! Allowed = int>0
!
! If GATHER_TYPE = OFFSET or CMP the GAT_X and GAT_Y grids 
! define the trace midpoint locations.
!
! If GATHER_TYPE = SOURCE the GAT_X and GAT_Y grids 
! define the trace source locations.
!
! If GATHER_TYPE = RECEIVER the GAT_X and GAT_Y grids 
! define the trace receiver locations.
!
! If GATHER_TYPE = hybrid the GAT_X and GAT_Y grids 
! define the source line, receiver line intersection locations.
!
!</Help>
!
!<Help KEYWORD="GAT_X_INIT">
!<Tip> First gather value for fast direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="GAT_X_LAST">
!<Tip> Last gather value for fast direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="GAT_X_INC">
!<Tip> Gather bin increment in the fast direction. </Tip>
! Default = 1.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word designating slow direction. </Tip>
! Default = 8
! Allowed = 7, 8, 17, 18
!  HDR_X and HDR_Y and define the fast and slow lateral
!  position variation of ouput traces.  The allowed combinations are:
!  HDR_X =  7, HDR_Y =  8
!  HDR_X =  8, HDR_Y =  7
!  HDR_X = 17, HDR_Y = 18
!  HDR_X = 18, HDR_Y = 17
!</Help>
!
!<Help KEYWORD="GAT_Y_TOT">
!<Tip> Number of gather bins in the slow direction. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="GAT_Y_INIT">
!<Tip> First gather value for slow direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="GAT_Y_LAST">
!<Tip> Last gather value for slow direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="GAT_Y_INC">
!<Tip> Output bin increment in the slow direction. </Tip>
! Default = 1.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OFF_X_TOT">
!<Tip> Number of output offset bins. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="OFF_X_INIT">
!<Tip> Minimum output offset value. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OFF_X_LAST">
!<Tip> Maximum output offset value. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OFF_X_INC">
!<Tip> Output offset bin increment. </Tip>
! Default = 1.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OFF_Y_TOT">
!<Tip> Number of output offset bins. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="OFF_Y_INIT">
!<Tip> Minimum output offset value. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OFF_Y_LAST">
!<Tip> Maximum output offset value. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OFF_Y_INC">
!<Tip> Output offset bin increment. </Tip>
! Default = 1.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OPT_OFFSET_SIGN">
!<Tip> Offset sign convention. </Tip>
! Default = NONE
! Allowed = NONE      Offset is always posiitive.
! Allowed = X         SIGN ( OFFSET ) * SIGN ( X_SOURCE - X_TIGIEVER )
! Allowed = Y         SIGN ( OFFSET ) * SIGN ( Y_SOURCE - Y_TIGIEVER )
!</Help>
!
!<Help KEYWORD="GATHER_TYPE">
!<Tip> Type of trace group order to output. </Tip>
! Default = OFFSET
! Allowed = CMP
! Allowed = SHOT
! Allowed = RECEIVER
! Allowed = HYBRID
!  Parameter GATHER_TYPE controls the order of the header word variation
!  within the output traces, that is the type of goup output by SYNGRID.
!
!  If GATHER_TYPE=OFFSET, the data is output in common offset order.
!  Then the grids defined by FAST and SLOW parameters define the midpoint
!  positions of the output traces.  The trace source and receiver positions
!  will be determined from the offset, azimuth and midpoint values.
!
!  If GATHER_TYPE=CMP, the data is output in common CMP order.
!  Then the grids defined by FAST and SLOW parameters define the midpoint
!  positions of the output traces.  The trace source and receiver positions
!  will be determined from the offset, azimuth and midpoint values.
!
!  If GATHER_TYPE=SHOT, the data is output in common shot order.
!  Then the grids defined by FAST and SLOW parameters define the shot
!  positions of the output traces.  The trace receiver and midpoint positions
!  will be determined from the offset, azimuth and source values.
!
!  If GATHER_TYPE=RECEIVER, the data is output in common receiver order.
!  Then the grids defined by FAST and SLOW parameters define the receiver
!  positions of the output traces.  The trace source and midpoint positions
!  will be determined from the offset, azimuth and receiver values.
!
!  If GATHER_TYPE=HYBRID, the data is output in hybrid gather order.
!  Then the grids defined by FAST and SLOW parameters define the source and 
!  receiver positions of the output traces.  
!  The sources will follow the x_offset = 0 values
!  The receivers will follow the y_offset = 0 values
!
!</Help>
!
!<Help KEYWORD="OPT_INPUT">
!<Tip> Whether to use internal external trace locations. </Tip>
! Default = INTERNAL
! Allowed = INTERNAL  Use defined x,y,o grid for trace locations.
! Allowed = EXTERNAL  Use input trace headers for trace locations.
! Allowed = DUPLICATE Use defined x,y,o grid for trace locations.
! duplicate input traces num_duplicate times.
!</Help>
!
!<Help KEYWORD="GROUP_SIZE">
!<Tip> Number of traces to output per call. </Tip>
! Default = 1
! Allowed = int>0
!  spike passes out its traces up to GROUP_SIZE traces at a time.
!  However spike will not pass out traces from two different groups together.
!  Hence if you define GATHER_TYPE=CMP, OFF_X_TOT=24, GROUP_SIZE=10
!  spike will pass out traces of each CMP groups of 10, 10, 4, 10, 10, 4 ...
!  So the traces from one CMP will not be combined with those from another CMP.
!</Help>
!
!<Help KEYWORD="NUM_DUPLICATE">
!<Tip> Number of duplicates if OPT_INPUT=DUPLICATE. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!</HelpSection>
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!
module syngrid_module
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
  public :: syngrid_create
  public :: syngrid_delete
  public :: syngrid_initialize
  public :: syngrid_get
  public :: syngrid_put
  public :: syngrid_verify
  public :: syngrid_set_gather_type
  public :: syngrid_compute_location
  public :: syngrid_compute_head
  !
  ! rcs identifier string
  !
  character(len=100),public,save :: syngrid_ident = &
  "$Id: syngrid.f90,v 1.8 2008/02/01 15:09:18 Hanson beta sps $"
  !
  integer, private, parameter :: n_opt_xy_order = 2
  character(len=3), save      :: c_opt_xy_order ( n_opt_xy_order ) &
  = (/'X_Y', 'Y_X'/)
  !
  integer, private, parameter :: n_opt_offset_sign = 3
  character(len=4), save      :: c_opt_offset_sign ( n_opt_offset_sign ) &
  = (/'NONE', 'X   ', 'Y   '/)
  !
  integer, private, parameter :: n_gather_type = 5
  character(len=8), save      :: c_gather_type ( n_gather_type ) &
  = (/'OFFSET  ' , 'CMP     ' , 'SOURCE  ' , 'RECEIVER', 'HYBRID  '/)
  !
  integer, private, parameter :: n_opt_input = 3
  character(len=9), save      :: c_opt_input ( n_opt_input ) &
  = (/'INTERNAL ', 'EXTERNAL ', 'DUPLICATE'/)
  !
  integer, private, parameter :: n_hdr_x = 4
  integer, private, save      :: c_hdr_x(n_hdr_x)         &
  = (/ 7, 8, 17, 18 /)
  !
  integer, private, parameter :: n_hdr_y = 4
  integer, private, save      :: c_hdr_y(n_hdr_y)         &
  = (/ 7, 8, 17, 18 /)
  !
  type, public :: syngrid_struct
    !
    !private
    !public
    !
    logical                                 :: group_end
    logical                                 :: l_gathered
    real                                    :: azimuth
    character(len=9)                        :: opt_input
    integer                                 :: num_duplicate
    integer                                 :: group_size
    logical                                 :: use_internal 
    logical                                 :: use_external 
    logical                                 :: use_duplicate
    !
    integer                                 :: xo_out
    integer                                 :: off_x_tot
    integer                                 :: off_x_dir
    real                                    :: off_x_init
    real                                    :: off_x_last
    real                                    :: x_off_min
    real                                    :: x_off_max
    real                                    :: off_x_inc
    !
    integer                                 :: yo_out
    integer                                 :: off_y_tot
    integer                                 :: off_y_dir
    real                                    :: off_y_init
    real                                    :: off_y_last
    real                                    :: y_off_min
    real                                    :: y_off_max
    real                                    :: off_y_inc
    !
    integer                                 :: jx_out
    integer                                 :: hdr_x
    integer                                 :: gat_x_tot
    integer                                 :: gat_x_dir
    real                                    :: gat_x_init
    real                                    :: gat_x_last
    real                                    :: gat_x_inc
    real                                    :: gat_x_min
    real                                    :: gat_x_max
    !
    integer                                 :: jy_out
    integer                                 :: hdr_y
    integer                                 :: gat_y_tot
    integer                                 :: gat_y_dir
    real                                    :: gat_y_init
    real                                    :: gat_y_last
    real                                    :: gat_y_inc
    real                                    :: gat_y_min
    real                                    :: gat_y_max
    !
    logical                                 :: opt_xy_order_x_y
    logical                                 :: opt_xy_order_y_x
    character(len=3)                        :: opt_xy_order
    character(len=4)                        :: opt_offset_sign
    character(len=8)                        :: gather_type
    !
    integer                                 :: hx_inp
    integer                                 :: hx_gat
    integer                                 :: hx_tig
    !
    integer                                 :: hy_inp
    integer                                 :: hy_gat
    integer                                 :: hy_tig
    !
    integer                                 :: ig_out
    integer                                 :: ig_lst
    !
    integer                                 :: i0_inp
    integer                                 :: n0_inp ! num input traces
    !
    integer                                 :: i0_out
    integer                                 :: n0_out
    !
    real                                    :: rx_scl
    real                                    :: ry_scl
    !
    integer                                 :: io_out
    integer                                 :: iq_out
    real                                    :: rx_out
    real                                    :: ry_out
    real                                    :: ro_out
    real                                    :: ro_x_y
    real                                    :: rx_cmp
    real                                    :: ry_cmp
    real                                    :: rx_gat
    real                                    :: ry_gat
    real                                    :: rz_gat
    real                                    :: rv_gat
    real                                    :: rx_tig
    real                                    :: ry_tig
    real                                    :: rz_tig
    real                                    :: rv_tig
    real                                    :: rx_off
    real                                    :: ry_off
    real                                    :: r_azimuth ! azimuth in radians
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
  end type syngrid_struct
  !
  contains
  !
  subroutine syngrid_create ( g, c_title, i_err )
    !
    ! Create a syngrid structure
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    character(len=*),         intent(in   ) :: c_title 
    integer,                  intent(inout) :: i_err
    !
    i_err = 0
    !
    ! allocate the structure
    !
    allocate ( g )
    !
    ! initialize coefficients
    !
    call syngrid_initialize ( g )
    !
    g%c_title = c_title
    !
    return
    !
  end subroutine syngrid_create
  !
  subroutine syngrid_delete ( g )
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    ! Begin syngrid_delete
    !
    !call memfun_del ( g%rp_tab )
    !
    deallocate ( g )
    !
    return
    !
  end subroutine syngrid_delete
  !
  subroutine syngrid_initialize ( g )
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    ! Begin syngrid_initialize
    !
    call syngrid_initialize_0 ( g )
    !
    ! initialize parameters
    !
    ! get the current globals
    !
    call timeglob_get ( g%nt_glb, g%t0_glb, g%t1_glb, g%dt_glb )
    !
    g%group_end     = .false.
    g%l_gathered    = .false.
    g%opt_xy_order  = 'X_Y'
    g%opt_offset_sign = 'NONE'
    g%gather_type   = 'OFFSET'
    g%l_gathered    = .false.
    g%num_duplicate = 1
    g%group_size    = 1
    g%opt_input     = 'INTERNAL'
    !
    g%off_x_tot      = 1
    g%x_off_min       = 0.
    g%x_off_max       = 0.
    g%off_x_inc       = 25.
    !
    g%off_y_tot       = 1
    g%y_off_min       = 0.
    g%y_off_max       = 0.
    g%off_y_inc       = 25.
    !
    g%hdr_x      = 7
    g%gat_x_tot      = 1
    g%gat_x_init     = 0.
    g%gat_x_last     = 0.
    g%gat_x_inc      = 1.
    !
    g%hdr_y      = 8
    g%gat_y_tot      = 1
    g%gat_y_init     = 0.
    g%gat_y_last     = 0.
    g%gat_y_inc      = 1.
    !
    return
    !
  end subroutine syngrid_initialize
  !
  subroutine syngrid_initialize_0 ( g )
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    ! Begin syngrid_initialize
    !
    !call memfun_init ( g%skip_wrapup )
    call memfun_init ( g%l_gathered )
    call memfun_init ( g%azimuth )
    call memfun_init ( g%group_end )
    call memfun_init ( g%group_size )
    call memfun_init ( g%opt_input )
    call memfun_init ( g%xo_out )
    call memfun_init ( g%off_x_tot )
    call memfun_init ( g%off_x_dir )
    call memfun_init ( g%off_x_init )
    call memfun_init ( g%off_x_last )
    call memfun_init ( g%x_off_min )
    call memfun_init ( g%x_off_max )
    call memfun_init ( g%off_x_inc )
    call memfun_init ( g%yo_out )
    call memfun_init ( g%off_y_tot )
    call memfun_init ( g%off_y_dir )
    call memfun_init ( g%off_y_init )
    call memfun_init ( g%off_y_last )
    call memfun_init ( g%y_off_min )
    call memfun_init ( g%y_off_max )
    call memfun_init ( g%off_y_inc )
    call memfun_init ( g%jx_out )
    call memfun_init ( g%hdr_x )
    call memfun_init ( g%gat_x_dir )
    call memfun_init ( g%gat_x_tot )
    call memfun_init ( g%gat_x_init )
    call memfun_init ( g%gat_x_last )
    call memfun_init ( g%gat_x_inc )
    call memfun_init ( g%gat_x_min )
    call memfun_init ( g%gat_x_max )
    call memfun_init ( g%jy_out )
    call memfun_init ( g%hdr_y )
    call memfun_init ( g%gat_y_tot )
    call memfun_init ( g%gat_y_dir )
    call memfun_init ( g%gat_y_init )
    call memfun_init ( g%gat_y_last )
    call memfun_init ( g%gat_y_inc )
    call memfun_init ( g%gat_y_min )
    call memfun_init ( g%gat_y_max )
    call memfun_init ( g%opt_xy_order )
    call memfun_init ( g%opt_offset_sign )
    call memfun_init ( g%gather_type )
    call memfun_init ( g%i0_inp )
    call memfun_init ( g%n0_inp )
    call memfun_init ( g%i0_out )
    call memfun_init ( g%n0_out )
    call memfun_init ( g%rx_scl )
    call memfun_init ( g%ry_scl )
    call memfun_init ( g%io_out )
    call memfun_init ( g%iq_out )
    call memfun_init ( g%rx_out )
    call memfun_init ( g%ry_out )
    call memfun_init ( g%ro_out )
    call memfun_init ( g%rx_cmp )
    call memfun_init ( g%ry_cmp )
    call memfun_init ( g%rx_gat )
    call memfun_init ( g%ry_gat )
    call memfun_init ( g%rz_gat )
    call memfun_init ( g%rv_gat )
    call memfun_init ( g%rx_tig )
    call memfun_init ( g%ry_tig )
    call memfun_init ( g%rz_tig )
    call memfun_init ( g%rv_tig )
    call memfun_init ( g%rx_off )
    call memfun_init ( g%ry_off )
    call memfun_init ( g%r_azimuth )
    !
    return
    !
  end subroutine syngrid_initialize_0
  !
  subroutine syngrid_get ( g )
    !
    ! get syngrid parameters
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    ! get the rotation grid object
    !
    g%ipn = pc_get_ipn()
    !
    call pc_get_global ( 'grid' , g%grid_obj )
    !
    call timeglob_get ( g%nt_glb, g%t0_glb, g%t1_glb, g%dt_glb )
    !
    call pc_get_global ( 'nwih',  g%nh_inp )
    !
    ! get old name parameters
    !
    call pc_get ( 'x_tot',          g%gat_x_tot        )
    call pc_get ( 'x_init',         g%gat_x_init       )
    call pc_get ( 'x_last',         g%gat_x_last       )
    call pc_get ( 'x_inc',          g%gat_x_inc        )
    !
    call pc_get ( 'y_tot',          g%gat_y_tot        )
    call pc_get ( 'y_init',         g%gat_y_init       )
    call pc_get ( 'y_last',         g%gat_y_last       )
    call pc_get ( 'y_inc',          g%gat_y_inc        )
    !
    call pc_get ( 'x_off_tot',      g%off_x_tot        )
    call pc_get ( 'x_off_init',     g%off_x_init       )
    call pc_get ( 'x_off_last',     g%off_x_last       )
    call pc_get ( 'x_off_inc',      g%off_x_inc        )
    !
    call pc_get ( 'y_off_tot',      g%off_y_tot        )
    call pc_get ( 'y_off_init',     g%off_y_init       )
    call pc_get ( 'y_off_last',     g%off_y_last       )
    call pc_get ( 'y_off_inc',      g%off_y_inc        )
    !
    call pc_get ( 'opt_xy_order',   g%opt_xy_order     )
    call pc_get ( 'opt_offset_sign',g%opt_offset_sign  )
    call pc_get ( 'gather_type',    g%gather_type      )
    call pc_get ( 'opt_input',      g%opt_input        )
    call pc_get ( 'num_duplicate',  g%num_duplicate    )
    call pc_get ( 'group_size',     g%group_size       )
    call pc_get ( 'azimuth',        g%azimuth          )
    !
    call pc_get ( 'hdr_x',          g%hdr_x            )
    call pc_get ( 'gat_x_tot',      g%gat_x_tot        )
    call pc_get ( 'gat_x_init',     g%gat_x_init       )
    call pc_get ( 'gat_x_last',     g%gat_x_last       )
    call pc_get ( 'gat_x_inc',      g%gat_x_inc        )
    !
    call pc_get ( 'hdr_y',          g%hdr_y            )
    call pc_get ( 'gat_y_tot',      g%gat_y_tot        )
    call pc_get ( 'gat_y_init',     g%gat_y_init       )
    call pc_get ( 'gat_y_last',     g%gat_y_last       )
    call pc_get ( 'gat_y_inc',      g%gat_y_inc        )
    !
    call pc_get ( 'off_x_tot',      g%off_x_tot        )
    call pc_get ( 'off_x_init',     g%off_x_init       )
    call pc_get ( 'off_x_last',     g%off_x_last       )
    call pc_get ( 'off_x_inc',      g%off_x_inc        )
    !
    call pc_get ( 'off_y_tot',      g%off_y_tot        )
    call pc_get ( 'off_y_init',     g%off_y_init       )
    call pc_get ( 'off_y_last',     g%off_y_last       )
    call pc_get ( 'off_y_inc',      g%off_y_inc        )
    !
    xxif_hdr : &
         if ( g%hdr_x .eq. 07 ) then
      !
      g%hdr_y = 08
      !
    else if ( g%hdr_x .eq. 08 ) then
      !
      g%hdr_y = 07
      !
    else if ( g%hdr_x .eq. 17 ) then
      !
      g%hdr_y = 18
      !
    else if ( g%hdr_x .eq. 18 ) then
      !
      g%hdr_y = 17
      !
    endif xxif_hdr 
    !
    return
    !
  end subroutine syngrid_get
  !
  subroutine syngrid_put ( g )
    !
    ! put syngrid parameters
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    call amod_line_feed ( 'syngrid_azimuth' )
    !
    call amod_line_feed ( 'syngrid_parameters' )
    call pc_put ( 'opt_xy_order',   g%opt_xy_order     )
    call pc_put ( 'opt_offset_sign',g%opt_offset_sign  )
    call pc_put ( 'gather_type',    g%gather_type      )
    call pc_put ( 'opt_input',      g%opt_input        )
    call pc_put ( 'num_duplicate',  g%num_duplicate    )
    call pc_put ( 'group_size',     g%group_size       )
    call pc_put ( 'azimuth',        g%azimuth          )
    !
    call amod_line_feed ( 'syngrid_hdr' )
    !
    call pc_put ( 'hdr_x',          g%hdr_x            )
    call pc_put ( 'hdr_y',          g%hdr_y            )
    !
    call amod_line_feed ( 'syngrid_x' )
    !
    call pc_put ( 'gat_x_tot',      g%gat_x_tot        )
    call pc_put ( 'gat_x_init',     g%gat_x_init       )
    call pc_put ( 'gat_x_last',     g%gat_x_last       )
    call pc_put ( 'gat_x_inc',      g%gat_x_inc        )
    !
    call amod_line_feed ( 'syngrid_y' )
    !
    call pc_put ( 'gat_y_tot',      g%gat_y_tot        )
    call pc_put ( 'gat_y_init',     g%gat_y_init       )
    call pc_put ( 'gat_y_last',     g%gat_y_last       )
    call pc_put ( 'gat_y_inc',      g%gat_y_inc        )
    !
    call amod_line_feed ( 'syngrid_x_off' )
    !
    call pc_put ( 'off_x_tot',      g%off_x_tot        )
    call pc_put ( 'off_x_init',     g%off_x_init       )
    call pc_put ( 'off_x_last',     g%off_x_last       )
    call pc_put ( 'off_x_inc',      g%off_x_inc        )
    !
    call amod_line_feed ( 'syngrid_y_off' )
    !
    call pc_put ( 'off_y_tot',      g%off_y_tot        )
    call pc_put ( 'off_y_init',     g%off_y_init       )
    call pc_put ( 'off_y_last',     g%off_y_last       )
    call pc_put ( 'off_y_inc',      g%off_y_inc        )
    !
    g%use_internal = string_upper_compare ( g%opt_input, 'internal' )
    !
    g%use_external = .not. g%use_internal 
    !
    g%use_duplicate = string_upper_compare ( g%opt_input, 'duplicate' )
    !
    if ( string_upper_compare ( g%opt_input, 'duplicate' ) ) &
    g%group_size = g%num_duplicate 
    !
    call pc_put_global  ( 'gathered',   g%l_gathered )
    call pc_put_global  ( 'numtr',      g%group_size )
    call pc_put_control ( 'need_label', g%use_internal )
    !
    return
    !
  end subroutine syngrid_put
  !
  subroutine syngrid_verify ( g )
    !
    ! verify syngrid parameters
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    integer                                 :: i_err
    integer                                 :: i_stat
    !
    i_err = 0
    !
    call pc_put_options_field ( 'opt_input', c_opt_input, n_opt_input )
    !
    call pc_put_options_field ( 'hdr_x',        c_hdr_x,        n_hdr_x        )
    !
    call pc_put_options_field ( 'hdr_y',        c_hdr_y,        n_hdr_y        )
    !
    call pc_put_sensitive_field_flag ( 'hdr_y',   .false.         )
    !
    call pc_put_options_field ( &
    'opt_xy_order', c_opt_xy_order, n_opt_xy_order )
    !
    g%opt_xy_order_x_y = string_upper_compare ( g%opt_xy_order, 'X_Y' )
    !
    g%opt_xy_order_y_x = .not. g%opt_xy_order_x_y 
    !
    call pc_put_options_field ( &
    'opt_offset_sign', c_opt_offset_sign, n_opt_offset_sign )
    !
    call pc_put_options_field ( 'gather_type', c_gather_type, n_gather_type )
    !
    if (  ( g%hdr_x .eq. 07 .and. g%hdr_y .ne. 08 ) &
     .or. ( g%hdr_x .eq. 08 .and. g%hdr_y .ne. 07 ) &
     .or. ( g%hdr_x .eq. 17 .and. g%hdr_y .ne. 18 ) &
     .or. ( g%hdr_x .eq. 18 .and. g%hdr_y .ne. 17 )  ) &
    call pc_warning ( &
    'syngrid : hdr_x and hdr_y must be a combination of 7,8 or 17,18 ' )
    !
    if ( g%gat_x_tot .le. 0 ) &
    call pc_warning ( 'syngrid : gat_x_tot must be > 0' )
    !
    if ( g%gat_x_inc .eq. 0.0 ) &
    call pc_warning ( 'syngrid : gat_x_inc must not be 0' )
    !
    if ( g%gat_y_tot .le. 0 ) &
    call pc_warning ( 'syngrid : gat_y_tot must be > 0' )
    !
    if ( g%gat_y_inc .eq. 0.0 ) &
    call pc_warning ( 'syngrid : gat_y_inc must not be 0 ' )
    !
    if ( g%off_x_tot .le. 0 ) &
    call pc_warning ( 'syngrid : off_x_tot must be > 0' )
    !
    if ( g%off_x_inc .eq. 0.0 ) &
    call pc_warning ( 'syngrid : off_x_inc must not be 0 ' )
    !
    if ( g%off_y_tot .le. 0 ) &
    call pc_warning ( 'syngrid : off_y_tot must be > 0' )
    !
    if ( g%off_y_inc .eq. 0.0 ) &
    call pc_warning ( 'syngrid : off_y_inc must not be 0 ' )
    !
    i_stat = pattern_stop2 ( 'syngrid:', .true., &
     g%gat_x_init, g%gat_x_inc, g%gat_x_last, g%gat_x_tot, &
     'gat_x_init', 'gat_x_inc', 'gat_x_last', 'gat_x_tot', &
     pc_verify_scalar ( 'gat_x_init' ), pc_verify_scalar ( 'gat_x_inc' ), &
     pc_verify_scalar ( 'gat_x_last' ), pc_verify_scalar ( 'gat_x_tot' )  )
    !
    i_stat = pattern_stop2 ( 'syngrid:', .true., &
     g%gat_y_init, g%gat_y_inc, g%gat_y_last, g%gat_y_tot, &
     'gat_y_init', 'gat_y_inc', 'gat_y_last', 'gat_y_tot', &
     pc_verify_scalar ( 'gat_y_init' ), pc_verify_scalar ( 'gat_y_inc' ), &
     pc_verify_scalar ( 'gat_y_last' ), pc_verify_scalar ( 'gat_y_tot' )  )
    !
    i_stat = pattern_stop2 ( 'syngrid:', .true., &
     g%off_x_init, g%off_x_inc, g%off_x_last, g%off_x_tot, &
     'off_x_init', 'off_x_inc', 'off_x_last', 'off_x_tot', &
     pc_verify_scalar ( 'off_x_init' ), pc_verify_scalar ( 'off_x_inc' ), &
     pc_verify_scalar ( 'off_x_last' ), pc_verify_scalar ( 'off_x_tot' )  )
    !
    i_stat = pattern_stop2 ( 'syngrid:', .true., &
     g%off_y_init, g%off_y_inc, g%off_y_last, g%off_y_tot, &
     'off_y_init', 'off_y_inc', 'off_y_last', 'off_y_tot', &
     pc_verify_scalar ( 'off_y_init' ), pc_verify_scalar ( 'off_y_inc' ), &
     pc_verify_scalar ( 'off_y_last' ), pc_verify_scalar ( 'off_y_tot' )  )
    !
    ! set the opt_input to allowed values
    !
    call syngrid_set_opt_input ( g%opt_input )
    !
    if ( string_upper_compare ( g%opt_input, 'duplicate' ) ) &
    g%group_size = g%num_duplicate 
    !
    ! set the gathered state flag, l_gather
    !
    xxif_gathered : if ( g%group_size .gt. 1 ) then
      !
      g%l_gathered = .true.
      !
    else xxif_gathered
      !
      g%l_gathered = .false.
      !
    end if xxif_gathered
    !
    ! set the gather_type to allowed values
    !
    call syngrid_set_gather_type ( g )
    !
    ! check to make sure the x,y header words are correct
    !
    call migfun_check_xy_header_words ( &
    'syngrid ', pc_get_lun(), g%hdr_x, g%hdr_y, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! get the y scale distance coefficient
    !
    call migfun_get_scale ( &
    'syngrid fast', pc_get_lun(), g%grid_obj, g%hdr_x, g%rx_scl, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! get the y scale distance coefficient
    !
    call migfun_get_scale ( &
    'syngrid slow', pc_get_lun(), g%grid_obj, g%hdr_y, g%ry_scl, i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
call syngrid_h_gat_tig ( g%gather_type, g%hdr_x, g%hx_gat, g%hx_tig, g%hx_inp )
    !
call syngrid_h_gat_tig ( g%gather_type, g%hdr_y, g%hy_gat, g%hy_tig, g%hy_inp )
    !
    call syngrid_visible ( g )
    !
    return
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in syngrid_verify ", &
    & /," during y migfun_get_scale " &
    & )')
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in syngrid_verify ", &
    & /," during x migfun_get_scale " &
    & )')
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in syngrid_verify ", &
    & /," during migfun_get_scale " &
    & /," hdr_x=", i8, " hdr_y=", i8 &
    & )') &
    g%hdr_x, g%hdr_y
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in syngrid_verify " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine syngrid_verify
  !
  subroutine syngrid_visible ( g )
    !
    ! visible syngrid parameters
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    return
    !
  end subroutine syngrid_visible 
  !
  subroutine syngrid_prep ( g, i_err )
    !
    ! prep  syngrid parameters
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    integer,                  intent(  out) :: i_err
    !
    i_err = 0
    !
    if ( pc_do_not_process_traces()  ) return
    !
    write ( pc_get_lun(), '(  &
    & /, " syngrid_update " ,/ ," REVISION: ", &
    & " 8  2008-01-31  Douglas Hanson Add OPT_XY_ORDER. " &
    & )')
    !
    ! get the min and max fast and slow directions
    !
    g%gat_x_min = min ( g%gat_x_init, g%gat_x_last )
    g%gat_x_max = max ( g%gat_x_init, g%gat_x_last )
    g%gat_x_inc = abs ( g%gat_x_inc )
    g%gat_x_dir = int ( sign ( 1., ( g%gat_x_last - g%gat_x_init )  )  )
    !
    g%gat_y_min = min ( g%gat_y_init, g%gat_y_last )
    g%gat_y_max = max ( g%gat_y_init, g%gat_y_last )
    g%gat_y_inc = abs ( g%gat_y_inc )
    g%gat_y_dir = int ( sign ( 1., ( g%gat_y_last - g%gat_y_init )  )  )
    !
    g%x_off_min = min ( g%off_x_init, g%off_x_last )
    g%x_off_max = max ( g%off_x_init, g%off_x_last )
    g%off_x_inc = abs ( g%off_x_inc )
    g%off_x_dir = int ( sign ( 1., ( g%off_x_last - g%off_x_init )  )  )
    !
    g%y_off_min = min ( g%off_y_init, g%off_y_last )
    g%y_off_max = max ( g%off_y_init, g%off_y_last )
    g%off_y_inc = abs ( g%off_y_inc )
    g%off_y_dir = int ( sign ( 1., ( g%off_y_last - g%off_y_init )  )  )
    !
    ! initialize the number of output traces,
    !   n0_out and the trace counter i0_out
    !
    g%i0_out = 0
    !
    g%n0_out = g%off_x_tot * g%off_y_tot * g%gat_x_tot * g%gat_y_tot
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in syngrid_prep  ", &
    & /," during xxx " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in syngrid_prep  " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine syngrid_prep
  !
  subroutine syngrid_set_gather_type ( g )
    !
    ! set the gather_type to allowed values
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid structure
    !
    call string_to_upper ( g%gather_type )
    !
    xxif_gather_type : &
    if ( string_upper_compare ( g%gather_type ( 1:1 ), 'C' )  ) then
      !
      g%gather_type = 'CMP'
      !
    else if ( string_upper_compare ( g%gather_type ( 1:1 ), 'S' )  ) then
      !
      g%gather_type = 'SOURCE'
      !
    else if ( string_upper_compare ( g%gather_type ( 1:1 ), 'R' )  ) then
      !
      g%gather_type = 'RECEIVER'
      !
    else if ( string_upper_compare ( g%gather_type ( 1:1 ), 'H' )  ) then
      !
      g%gather_type = 'HYBRID'
      !
    else xxif_gather_type 
      !
      g%gather_type = 'OFFSET'
      !
    end if xxif_gather_type 
    !
  end subroutine syngrid_set_gather_type
  !
  subroutine syngrid_set_opt_input ( opt_input )
    !
    ! set the opt_input to allowed values
    !
    character(len=*),         intent(inout) :: opt_input
    !
    call string_to_upper ( opt_input )
    !
    xxif_opt_input : &
    if ( string_upper_compare ( opt_input ( 1:1 ), 'E' )  ) then
      !
      opt_input = 'EXTERNAL'
      !
    else if ( string_upper_compare ( opt_input ( 1:1 ), 'D' )  ) then
      !
      opt_input = 'DUPLICATE'
      !
    else xxif_opt_input 
      !
      opt_input = 'INTERNAL'
      !
    end if xxif_opt_input 
    !
    return
    !
  end subroutine syngrid_set_opt_input 
  !
  subroutine syngrid_h_gat_tig ( gather_type, hdr_x, hx_gat, hx_tig, hx_inp )
    !
    ! set the source and receiver header words
    !
    character(len=*),  intent(in   ) :: gather_type       ! group type
    integer,           intent(in   ) :: hdr_x            ! x midpoint header
    integer,           intent(inout) :: hx_gat           ! x source   header
    integer,           intent(inout) :: hx_tig           ! x receiver header
    integer,           intent(inout) :: hx_inp           ! x group    header
    !
    xxif_hdr_x : if ( hdr_x .eq. hdr_midpoint_xgrid ) then
      !
      hx_tig = hdr_receiver_xgrid
      hx_gat = hdr_source_xgrid
      !
    else if ( hdr_x .eq. hdr_midpoint_ygrid ) then
      !
      hx_tig = hdr_receiver_ygrid
      hx_gat = hdr_source_ygrid
      !
    else if ( hdr_x .eq. hdr_midpoint_xloc ) then
      !
      hx_tig = hdr_receiver_xloc
      hx_gat = hdr_source_xloc
      !
    else if ( hdr_x .eq. hdr_midpoint_yloc ) then
      !
      hx_tig = hdr_receiver_yloc
      hx_gat = hdr_source_yloc
      !
    else xxif_hdr_x
      !
      call pc_error ( ' error in syngrid_h_gat_tig hdr_x=', hdr_x )
      !
    end if xxif_hdr_x
    !
    xxif_shot : if ( string_upper_compare ( gather_type, 'SOURCE' )  ) then
      !
      hx_inp = hx_gat
      !
    else if ( string_upper_compare ( gather_type, 'RECEIVER' )  ) then
      !
      hx_inp = hx_tig
      !
    else xxif_shot 
      !
      hx_inp = hdr_x
      !
    end if xxif_shot 
    !
    return
    !
  end subroutine syngrid_h_gat_tig 
  !
  subroutine syngrid_compute_location ( g, dtm, hd_inp, i_err )
    !
    ! compute the trace location
    !
    type ( syngrid_struct ),        pointer :: g   ! syngrid structure
    type ( datumgrid_struct ),     pointer :: dtm ! datumgrid structure
    double precision,         intent(in   ) :: hd_inp ( : )
    integer,                  intent(  out) :: i_err
    !
    i_err = 0
    !
    g%group_end = .false.
    !
    xxif_external_1 : if ( g%use_external .and. .not. g%use_duplicate ) then
      !
      ! get the trace locations from the input trace
      !
      ! get the input trace x location
      !
      call migfun_trace_location ( hd_inp ( g%hdr_x ), &
      g%gat_x_init, g%gat_x_inc, 1., g%jx_out, g%rx_out )
      !
      call migfun_trace_location ( hd_inp ( g%hdr_y ), &
      g%gat_y_init, g%gat_y_inc, 1., g%jy_out, g%ry_out )
      !
      call migfun_trace_offset ( &
                                 3, &
                                 g%ro_out, g%ro_x_y, &
                                 g%hdr_x, g%rx_scl, g%rx_off, &
                                 g%hdr_y, g%ry_scl, g%ry_off, &
                                 g%nh_inp, hd_inp &
                               )
      !
      g%rx_cmp = hd_inp ( g%hdr_x ) 
      !
      g%ry_cmp = hd_inp ( g%hdr_y ) 
      !
      ! set the x,y cmp positions 
      ! for SHOT and RECEIVER and OFFSET and CMP gathers
      !
      xxif_gather_type_1 : &
      if ( string_upper_compare ( g%gather_type, 'SOURCE' )  ) then
        !
        ! g%rx_out, g%ry_out are the source position,
        ! set them to the cmp position
        !
        g%rx_out = g%rx_cmp + .5 * g%rx_off / g%rx_scl ! x source
        !
        g%ry_out = g%ry_cmp +.5 * g%ry_off / g%ry_scl ! y source
        !
      else if ( string_upper_compare ( g%gather_type, 'RECEIVER' )  ) then
        !
        ! g%rx_out, g%ry_out are the receiver position,
        ! set them to the cmp position
        !
        g%rx_out = g%rx_cmp - .5 * g%rx_off / g%rx_scl ! x receiver
        !
        g%ry_out = g%ry_cmp - .5 * g%ry_off / g%ry_scl ! y receiver
        !
      else if ( string_upper_compare ( g%gather_type, 'HYBRID' )  ) then
        !
        ! g%rx_out, g%ry_out are the receiver position,
        ! set them to the cmp position
        !
        g%ry_gat = g%ry_cmp + g%rx_off / g%rx_scl ! y source
        !
        g%rx_tig = g%rx_cmp - g%rx_off / g%rx_scl ! y source
        !
        g%rx_out = g%rx_gat 
        !
        g%ry_out = g%ry_tig 
        !
      else xxif_gather_type_1
        !
        g%rx_out = g%rx_cmp 
        !
        g%ry_out = g%ry_cmp 
        !
      end if xxif_gather_type_1
      !
      g%jx_out = nint ( ( g%rx_out - g%gat_x_init ) / g%gat_x_inc ) + 1 
      !
      g%jy_out = nint ( ( g%ry_out - g%gat_y_init ) / g%gat_y_inc ) + 1 
      !
      g%ig_out = 1
      !
      g%iq_out = g%iq_out + 1
      !
    else xxif_external_1 
      !
      ! get the trace locations from the input trace
      ! from the defined x,y grid
      !
      if ( g%i0_out .gt. g%n0_out ) return
      !
      ! set the offset, fast and slow indicies for this g%gather_type
      !
      xxif_gather_type: &
      if ( string_upper_compare ( g%gather_type, 'OFFSET' )  ) then
        !
        ! compute the trace x,y gatehr and offset values 
        ! for offset gather order
        !
        if ( g%opt_xy_order_x_y ) &
        call syngrid_compute_location_off ( &
                                            g%i0_out, &
                                            g%gat_x_dir, g%gat_x_tot, g%jx_out, &
                                            g%off_x_dir, g%off_x_tot, g%xo_out, &
                                            g%gat_y_dir, g%gat_y_tot, g%jy_out, &
                                            g%off_y_dir, g%off_y_tot, g%yo_out, &
                                            g%io_out, g%ig_out, g%iq_out &
                                )
        !
        if ( g%opt_xy_order_y_x ) &
        call syngrid_compute_location_off ( &
                                            g%i0_out, &
                                            g%gat_y_dir, g%gat_y_tot, g%jy_out,&
                                            g%off_y_dir, g%off_y_tot, g%yo_out,&
                                            g%gat_x_dir, g%gat_x_tot, g%jx_out,&
                                            g%off_x_dir, g%off_x_tot, g%xo_out,&
                                            g%io_out, g%ig_out, g%iq_out &
                                )
        !
      else xxif_gather_type
        !
        ! compute the trace x,y gatehr and offset values 
        ! for common cmp, shot, receiver order
        !
        if ( g%opt_xy_order_x_y ) &
        call syngrid_compute_location_cmp ( &
                                            g%i0_out, &
                                            g%gat_x_dir, g%gat_x_tot, g%jx_out, &
                                            g%off_x_dir, g%off_x_tot, g%xo_out, &
                                            g%gat_y_dir, g%gat_y_tot, g%jy_out, &
                                            g%off_y_dir, g%off_y_tot, g%yo_out, &
                                            g%io_out, g%ig_out, g%iq_out &
                                )
        !
        if ( g%opt_xy_order_y_x ) &
        call syngrid_compute_location_cmp ( &
                                            g%i0_out, &
                                            g%gat_y_dir, g%gat_y_tot, g%jy_out,&
                                            g%off_y_dir, g%off_y_tot, g%yo_out,&
                                            g%gat_x_dir, g%gat_x_tot, g%jx_out,&
                                            g%off_x_dir, g%off_x_tot, g%xo_out,&
                                            g%io_out, g%ig_out, g%iq_out &
                                )
        !
      end if xxif_gather_type
      !
      ! if this is not the first trace in the output batch and
      ! this output trace is part of a new group
      ! output the current set of output traces
      !
      xxif_new_group : &
      if ( g%i0_inp .gt. 1 .and. g%ig_out .ne. g%ig_lst ) then
        !
        g%i0_out = g%i0_out - 1
        !
        g%ig_lst = g%ig_out
        !
        g%group_end = .true.
        !
        return
        !
      end if xxif_new_group
      !
      ! reset the last group index, g%ig_lst
      !
      g%ig_lst = g%ig_out
      !
      ! set the output offset, fast and slow trace positions
      !
      g%rx_off = ( g%xo_out - 1 ) * g%off_x_inc + g%x_off_min
      !
      g%ry_off = ( g%yo_out - 1 ) * g%off_y_inc + g%y_off_min
      !
      g%ro_out = sqrt ( g%rx_off**2 + g%ry_off**2 )
      !
      g%rx_out = ( g%jx_out - 1 ) * g%gat_x_inc + g%gat_x_min ! x gat
      !
      g%ry_out = ( g%jy_out - 1 ) * g%gat_y_inc + g%gat_y_min ! y gat
      !
      ! get the x,y offset
      !
      g%r_azimuth = matfun_deg_to_rad ( g%azimuth ) ! azimuth in radians
      !
      if ( g%off_y_tot .eq. 1 .and. abs(g%r_azimuth) .gt. .01 ) &
      call migfun_xy_offset (                        &
                              offset  = g%ro_out,    &
                              azimuth = g%r_azimuth, &
                              x_hdr   = g%hdr_x,     &
                              x_off   = g%rx_off,    &
                              y_hdr   = g%hdr_y,     &
                              y_off   = g%ry_off,    &
                              i_err   = i_err        &
                            )
      !
      !g%rx_off = sign ( 1., &
      !           ( g%xo_out - 1 ) * g%off_x_inc + g%x_off_min ) * g%rx_off
      !
      !g%ry_off = sign ( 1., &
      !           ( g%yo_out - 1 ) * g%off_y_inc + g%y_off_min ) * g%ry_off
      !
      if ( i_err .ne. 0 ) go to 998
      !
    end if xxif_external_1 
    !
    g%ro_out = sqrt ( g%rx_off**2 + g%ry_off**2 )
    !
    ! set the x,y cmp positions 
    ! for SHOT and RECEIVER and OFFSET and CMP gathers
    !
    xxif_gather_type_2 : &
    if ( string_upper_compare ( g%gather_type, 'SOURCE' )  ) then
      !
      ! g%rx_out, g%ry_out are the source position,
      ! set them to the cmp position
      !
      g%rx_cmp = g%rx_out - .5 * g%rx_off / g%rx_scl ! x source
      !
      g%ry_cmp = g%ry_out - .5 * g%ry_off / g%ry_scl ! y source
      !
    else if ( string_upper_compare ( g%gather_type, 'RECEIVER' )  ) then
      !
      ! g%rx_out, g%ry_out are the receiver position,
      ! set them to the cmp position
      !
      g%rx_cmp = g%rx_out + .5 * g%rx_off / g%rx_scl ! x receiver
      !
      g%ry_cmp = g%ry_out + .5 * g%ry_off / g%ry_scl ! y receiver
      !
    else if ( string_upper_compare ( g%gather_type, 'HYBRID' )  ) then
      !
      ! g%rx_out, g%ry_out are the receiver position,
      ! set them to the cmp position
      !
      g%rx_gat = g%rx_out
      !
      g%ry_gat = g%ry_out + g%rx_off / g%rx_scl ! y source
      !
      g%rx_tig = g%rx_out - g%rx_off / g%rx_scl ! y source
      !
      g%ry_tig = g%ry_out
      !
      g%rx_cmp = ( g%rx_gat + g%rx_tig ) * .5
      !
      g%ry_cmp = ( g%ry_gat + g%ry_tig ) * .5
      !
    else xxif_gather_type_2
      !
      g%rx_cmp = g%rx_out
      !
      g%ry_cmp = g%ry_out
      !
    end if xxif_gather_type_2
    !
    g%ro_out = sqrt ( g%rx_off**2 + g%ry_off**2 )
    !
    ! compute the source and receiver positions in distance units
    !
    g%rx_gat = g%rx_cmp * g%rx_scl + .5 * g%rx_off
    !
    g%ry_gat = g%ry_cmp * g%ry_scl + .5 * g%ry_off
    !
    g%rz_gat = 0.
    !
    g%rx_tig = g%rx_cmp * g%rx_scl - .5 * g%rx_off
    !
    g%ry_tig = g%ry_cmp * g%ry_scl - .5 * g%ry_off
    !
    g%rz_tig = 0.
    !
    !if ( g%ig_out .le. 10 ) &
    !print'( " syngrid_compute_location ipn=",i3," i=",i8,&
    !& " xo=",g10.4," xs=",g10.4," xr=",g10.4 )', &
    !g%ipn, g%i0_out, g%rx_off, g%rx_gat, g%rx_tig
    !
    !  get the source   depth from the source   datum horizon
    !
    if ( dtm%s%nx_sur .gt. 0 ) &
    call interpolate_2d_to_0d ( &
                                dtm%s%nx_sur, dtm%s%x0_sur, dtm%s%dx_sur, &
                                dtm%s%ny_sur, dtm%s%y0_sur, dtm%s%dy_sur, &
                                dtm%s%rz_sur, &
                                g%rx_gat / g%rx_scl, &
                                g%ry_gat / g%ry_scl, &
                                g%rz_gat &
                              )
    !
    !  get the receiver depth from the receiver datum horizon
    !
    if ( dtm%r%nx_sur .gt. 0 ) &
    call interpolate_2d_to_0d ( &
                                dtm%r%nx_sur, dtm%r%x0_sur, dtm%r%dx_sur, &
                                dtm%r%ny_sur, dtm%r%y0_sur, dtm%r%dy_sur, &
                                dtm%r%rz_sur, &
                                g%rx_tig / g%rx_scl, &
                                g%ry_tig / g%ry_scl, &
                                g%rz_tig &
                              )
    !
    !if ( g%ig_out .le. 10 ) &
    !print'( " syngrid_compute_location ipn=",i3," i=",i8," xo=",g10.4, &
    !& " xs=",g10.4," xr=",g10.4, &
    !& " zs=",g10.4," zr=",g10.4 &
    !& )', &
    !g%ipn, g%i0_out, g%rx_off, &
    !g%rx_gat, g%rx_tig, &
    !g%rz_gat, g%rz_tig
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in syngrid_compute_location " &
    & /, " during migfun_xy_offset " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & " error in syngrid_compute_location " &
    & )') 
    !
    i_err = -1
    !
    return
    !
  end subroutine syngrid_compute_location
  !
  subroutine syngrid_compute_head ( g, hd_inp, tr_inp, i_err )
    !
    ! Arguments
    !
    type ( syngrid_struct ),        pointer :: g   ! syngrid structure
    double precision,         intent(  out) :: hd_inp ( : )
    real,                     intent(in   ) :: tr_inp ( : )
    integer,                  intent(  out) :: i_err
    !
    ! Local variables
    !
    i_err = 0
    !
    ! initialize header and trace to zero
    !
    xxif_external : if ( g%use_external .and. .not. g%use_duplicate ) then
      !
      hd_inp ( hdr_lav ) = maxval ( abs ( tr_inp ( 1:g%nt_glb ) ) )
      !
    else xxif_external 
      !
      hd_inp ( : ) = 0.
      !
      ! set the output headers
      !
      call migfun_output_headers_sr ( &
                                      g%grid_obj, &
                                      g%i0_out, g%ig_out, g%iq_out, &
                                      g%hdr_x, g%rx_gat, g%rx_tig, g%rx_scl, &
                                      g%hdr_y, g%ry_gat, g%ry_tig, g%ry_scl, &
                                      1, g%nt_glb, &
                                      g%nh_inp, hd_inp ( : ), &
                                      g%nt_glb, tr_inp ( : ) &
                                    )
      !
      hd_inp ( hdr_source_elev   ) = g%rz_gat
      !
      hd_inp ( hdr_receiver_elev ) = g%rz_tig
      !
        hd_inp ( hdr_offset        ) = sqrt ( &
      ( hd_inp ( hdr_source_xloc   )  &
      - hd_inp ( hdr_receiver_xloc )  ) **2 + &
      ( hd_inp ( hdr_source_yloc   )  &
      - hd_inp ( hdr_receiver_yloc )  ) **2 )
      !
      ! add offset sign
      !
      if ( string_upper_compare ( g%opt_offset_sign, 'X' ) ) &
        hd_inp ( hdr_offset        ) = &
        hd_inp ( hdr_offset        ) * sign ( 1., g%rx_gat - g%rx_tig ) 
      !
      if ( string_upper_compare ( g%opt_offset_sign, 'Y' ) ) &
        hd_inp ( hdr_offset        ) = &
        hd_inp ( hdr_offset        ) * sign ( 1., g%ry_gat - g%ry_tig ) 
      !
    end if xxif_external 
    !
    !print'( " i=",i8," a=",g10.4," o=",g10.4," rx=",g10.4,&
    !& " h11=",g10.4," h14=",g10.4," h6=",g10.4 )',&
    !g%i0_out, g%r_azimuth, g%ro_out, g%rx_cmp, &
    !hd_inp ( 11 ), hd_inp ( 14 ), hd_inp ( 06 )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & " error in syngrid_compute_head  " &
    & )') 
    !
    i_err = -1
    !
    return
    !
  end subroutine syngrid_compute_head 
  !
  subroutine syngrid_compute_location_off ( &
                                            i0_out, &
                                            gat_x_dir, gat_x_tot, jx_out, &
                                            off_x_dir, off_x_tot, xo_out, &
                                            gat_y_dir, gat_y_tot, jy_out, &
                                            off_y_dir, off_y_tot, yo_out, &
                                            io_out, ig_out, iq_out &
                                           )
    !
    ! compute the trace x,y gather and offset values 
    ! for offset order
    !
    integer,                  intent(in   ) :: i0_out
    integer,                  intent(in   ) :: gat_x_dir
    integer,                  intent(in   ) :: gat_x_tot
    integer,                  intent(in   ) :: off_x_dir
    integer,                  intent(in   ) :: off_x_tot
    integer,                  intent(  out) :: jx_out
    integer,                  intent(  out) :: xo_out
    integer,                  intent(in   ) :: gat_y_dir
    integer,                  intent(in   ) :: gat_y_tot
    integer,                  intent(in   ) :: off_y_dir
    integer,                  intent(in   ) :: off_y_tot
    integer,                  intent(  out) :: jy_out
    integer,                  intent(  out) :: yo_out
    integer,                  intent(  out) :: io_out
    integer,                  intent(  out) :: ig_out
    integer,                  intent(  out) :: iq_out
    !
    jx_out = mod  ( i0_out - 1, gat_x_tot ) + 1
    !
    jy_out = mod ( ( i0_out - 1 ) /  gat_x_tot,  gat_y_tot ) + 1
    !
    io_out = ( i0_out - 1 ) /  ( gat_x_tot * gat_y_tot ) + 1
    !
    xo_out = mod ( ( i0_out - 1 ) / &
    ( gat_x_tot * gat_y_tot ), off_x_tot ) + 1
    !
    yo_out =       ( i0_out - 1 ) / &
    ( off_x_tot * gat_x_tot * gat_y_tot ) + 1
    !
    if ( gat_x_dir  .lt. 0 ) jx_out = gat_x_tot  - jx_out + 1
    !
    if ( gat_y_dir  .lt. 0 ) jy_out = gat_y_tot  - jy_out + 1
    !
    if ( off_x_dir  .lt. 0 ) xo_out = off_x_tot  - xo_out + 1
    !
    if ( off_y_dir  .lt. 0 ) yo_out = off_y_tot  - yo_out + 1
    !
    ig_out = io_out
    !
    iq_out = ( jy_out - 1 ) * gat_x_tot + jx_out
    !
    return
    !
  end subroutine syngrid_compute_location_off 
  !
  subroutine syngrid_compute_location_cmp ( &
                                            i0_out, &
                                            gat_x_dir, gat_x_tot, jx_out, &
                                            off_x_dir, off_x_tot, xo_out, &
                                            gat_y_dir, gat_y_tot, jy_out, &
                                            off_y_dir, off_y_tot, yo_out, &
                                            io_out, ig_out, iq_out &
                                           )
    !
    ! compute the trace x,y gather and offset values 
    ! for common cmp, shot, receiver order
    !
    integer,                  intent(in   ) :: i0_out
    integer,                  intent(in   ) :: gat_x_dir
    integer,                  intent(in   ) :: gat_x_tot
    integer,                  intent(in   ) :: off_x_dir
    integer,                  intent(in   ) :: off_x_tot
    integer,                  intent(  out) :: jx_out
    integer,                  intent(  out) :: xo_out
    integer,                  intent(in   ) :: gat_y_dir
    integer,                  intent(in   ) :: gat_y_tot
    integer,                  intent(in   ) :: off_y_dir
    integer,                  intent(in   ) :: off_y_tot
    integer,                  intent(  out) :: jy_out
    integer,                  intent(  out) :: yo_out
    integer,                  intent(  out) :: io_out
    integer,                  intent(  out) :: ig_out
    integer,                  intent(  out) :: iq_out
    !
    yo_out = mod ( i0_out - 1, off_x_tot * off_y_tot ) / off_x_tot + 1
    !
    xo_out = mod ( i0_out - 1, off_x_tot * off_y_tot ) &
             - ( yo_out - 1 ) * off_x_tot + 1
    !
    jx_out = mod ( ( i0_out - 1 ) / ( off_y_tot * off_x_tot ), gat_x_tot ) + 1
    !
    jy_out = ( i0_out - 1 ) / ( off_y_tot * off_x_tot * gat_x_tot ) + 1
    !
    if ( gat_x_dir .lt. 0 ) jx_out = gat_x_tot - jx_out + 1
    !
    if ( gat_y_dir .lt. 0 ) jy_out = gat_y_tot - jy_out + 1
    !
    if ( off_x_dir .lt. 0 ) xo_out = off_x_tot - xo_out + 1
    !
    if ( off_y_dir .lt. 0 ) yo_out = off_y_tot - yo_out + 1
    !
    ig_out = jx_out
    !
    iq_out = xo_out
    !
    return
    !
  end subroutine syngrid_compute_location_cmp 
  !
end module syngrid_module
