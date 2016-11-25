!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- surfacegrid.f90 ------------------------------!!
!!------------------------------- surfacegrid.f90 ------------------------------!!
!!------------------------------- surfacegrid.f90 ------------------------------!!


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
! Name       : surfacegrid     (read SURFACE file onto GRID.)
! Category   : io
! Written    : 2007-02-27   by: Douglas Hanson
! Revised    : 2007-05-31   by: Douglas Hanson Fix x,y file transpose.
! Maturity   : beta
! Purpose    : Read a surface file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!  Read surface files.
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
!   i = intent(in   ) = value required upon INPUT.
!   o = intent(  out) = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!  file format is ascii 
!  card 1                   = hx_fil, nx_fil, x0_fil, dx_fil
!  card 2                   = hy_fil, ny_fil, y0_fil, dy_fil
!  card 2+1:2+nx_fil*ny_fil = rz_fil ( ix_fil, iy_fil )
!  fil x,y can be pos x,y or pos y,x
!    
!  return the grid characteristics of a surface file
!  if  path_surface = pathcheck_empty 
!  nx_sur, x0_sur, dx_sur will be set to 1, 0., 1. respectively.
!  ny_sur, y0_sur, dy_sur will be set to 1, 0., 1. respectively.
!
!  call surfacegrid_size ( &
!                        i            i             i           
!                        opt_surface, path_surface, const_surface, &
!                        o       o       o       o
!                        hx_sur, nx_sur, x0_sur, dx_sur, &
!                        o       o       o       o
!                        hy_sur, ny_sur, y0_sur, dy_sur, &
!                        o
!                        i_err &
!                      )
!
!  read a surface file into surface grid rz_sur
!
!  if alloc_mem = .true. surfacegrid will allocate rz_sur as a 3d array with 
!  dimensions nx_sur, ny_sur, nz_sur.
!
!  if alloc_mem = .fals. surfacegrid assumes rz_sur has already been allocated
!
!  surfacegrid outputs a gridded surface array rz_sur (:, :, jz_sur )
!
!  if path_surface is pathcheck_empty these surfacev alues are the constant value 
!  const_surface.
!
!  if path_surface is not pathcheck_empty these surface values are interpolated 
!  from those in the file.
!
!  if file_head = .true. and path_surface is not pathcehck_empty
!  surfacegrid will set the output x,y header words, hx_sur, hy_sur,
!  to the file header words, hx_fil, hy_fil respectively.
!
!  if file_head = .true. and path_surface is pathcehck_empty
!  surfacegrid will set the output x,y header words, hx_sur, hy_sur,
!  to 7 and 8 respecitvely
!
!  if file_head = .false. surfacegrid will output the surface grid 
!  useing the header word values and order defined by hx_sur, hy_sur
!
!  if file_grid = .true. and path_surface is not pathcehck_empty
!
!  nx_sur, x0_sur, dx_sur will be set to the file grid values 
!  multiplied by rx_scl
!  ny_sur, y0_sur, dy_sur will be set to the file grid values 
!  multiplied by ry_scl
!
!  and surfacegrid will output the surface grid useing the grid defined by the file
!
!  if file_grid = .true. and path_surface is pathcehck_empty
!
!  nx_sur, x0_sur, dx_sur will be set to 1, 0., rx_scl respectively.
!  ny_sur, y0_sur, dy_sur will be set to 1, 0., ry_scl respectively.
!
!  if file_grid = .false. surfacegrid will output the surface grid 
!  useing the grid defined by the input values of nx_sur etc.
!
!  if path_surface is pathcheck_empty surfacegrid_read sets all surface values on the 
!  output grid equal to the constant surface value const_surface
!
!  if path_surface is not pathcheck_empty surfacegrid_read reads in the file surface 
!  values on the orignal file grid and lineraly interpolates these to the 
!  output grid.
!
!  if path_surface is not pathcheck_empty 
!  the file   x,y surface header words, hx_fil and hy_fil, 
!  must be consistent with
!  the output x,y surface header words, hx_sur and hy_sur.
!
!  That is: hx_fil=hx_sur and hy_fil=hy_sur
!       or: hx_fil=hy_sur and hy_fil=hx_sur
!
!  call surfacegrid_read ( &
!                        i           i
!                        path_surface, const_surface, &
!                        b       b       b       b       i
!                        hx_sur, nx_sur, x0_sur, dx_sur, rx_scl, &
!                        b       b       b       b       i
!                        hy_sur, ny_sur, y0_sur, dy_sur, ry_scl, &
!                        i       i
!                        nz_sur, jz_sur, &
!                        o
!                        rz_sur, &
!                        i          i          i
!                        alloc_mem, file_head, file_grid, &
!                        o
!                        i_err &
!                      )
!
!  print the grid characteristics for surfaces rz_sur_1 and rz_sur_2
!  if _lu_out < 0 no print is done
!
!  call surfacegrid_print ( &
!                         i        i
!                         c_title, lu_out, &
!                         i             i
!                         path_surface, const_surface, &
!                         i       i       i       i       i
!                         hx_sur, nx_sur, x0_sur, dx_sur, rx_scl, &
!                         i       i       i       i       i
!                         hy_sur, ny_sur, y0_sur, dy_sur, ry_scl, &
!                         i
!                         nz_sur, &
!                         i
!                         rz_sur_1, &
!                         i
!                         rz_sur_2 &
!                       )
!
! character(len=*)     c_title       = title to print on output
! integer              lu_out        = unit for printed output lu_out<0 no print
! character(len=*)     path_surface    = surface file name
! real                 const_surface   = constant surface value 
!                                      used if path_surface = pathcheck_empty
! integer              hx_sur        = fast surface header word
! integer              nx_sur        = number of fast surface grid nodes
! real                 x0_sur        = minimum fast surface grid nodes
! real                 dx_sur        = fast surface grid node increment
! integer              hy_sur        = slow surface header word
! integer              ny_sur        = number of slow surface grid nodes
! real                 y0_sur        = minimum slow surface grid nodes
! real                 dy_sur        = slow surface grid node increment
! integer              nz_sur        = number of surface grid levels in rz_sur
! integer              jz_sur        = surface grid level to fill in rz_sur
! real, 3d pointer     rz_sur        = surface grid (nx_sur, ny_sur, nz_sur)
! character(len=*)     path_surface_1  = first  surface file name
! character(len=*)     path_surface_2  = second surface file name
! real                 const_surface_1 = first  constant surface value to print
! real                 const_surface_2 = second constant surface value to print
! real, 2d array       rz_sur_1      = first  surface to print (nx_sur, ny_sur)
! real, 2d array       rz_sur_2      = second surface to print (nx_sur, ny_sur)
! logical              alloc_mem     = flag to allcoate memory ofr rz_sur
! logical              file_head     = flag to set hx_sur, hy_sur from the file
! logical              file_grid     = flag to set nx_sur etc from the file
! integer              i_err         = error flag 0 = o.k. -1 = error
!
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
!  5  2007-05-31  Douglas Hanson Fix x,y file transpose.
!  4  2007-05-29  Douglas Hanson Fix file name length. 
!  3  2007-05-01  Douglas Hanson Add surfacegrid flag.
!  2  2007-04-10  Douglas Hanson Move datumgrid info to surfacegrid.
!  1  2007-02-27  Douglas Hanson Modify from surfacegrid.
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
!<gui_def>
!
!`------------------------------------------------------------------------------
! OPT_SURFACE~~=`CCCCCCC
! CONST_SURFACE=`FFFFFFFFFFF     
! Select PATH_SURFACE [PATH_SURFACE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!`------------------------------------------------------------------------------
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="SELECT_PATH_SURFACE">
!<Tip> surface path name. </Tip>
!</Help>
!
!<Help KEYWORD="OPT_SURFACE">
!<Tip> Whether to use constant or spatialy varying SURFACE. </Tip>
! Default = CONSTANT  
! Allowed = CONSTANT  Use a constant value of CONST_SURFACE.
! Allowed = PATH      Use a spatialy varying defined by PATH_SURFACE.
!</Help>
!
!<Help KEYWORD="CONST_SURFACE">
!<Tip> Surface constant surface level depth. </Tip>
! Default = 0
! Allowed = real
! Not used for time migration and DMO.
! If the surface elevations vary, they should be in an ascci data file
! - CONST_SURFACE is other than NONE the Z values in it will be used as the
! surface level.
! CONST_SURFACE should then be set to an average depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!<Help KEYWORD="CONST_SURFACE">
!<Tip> Surface file. </Tip>
! Default = NONE
! Allowed = character
! Not used for time migration and DMO.
! CONST_SURFACE should then be set to an average depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!<Help KEYWORD="PATH_SURFACE">
!<Tip> Surface file. </Tip>
! Default = NONE
! Allowed = character
! Not used for time migration and DMO.
! If PATH_SURFACE is other than NONE the Z values in it will be used as the
! surface depth.
! CONST_SURFACE should then be set to an average depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!</HelpSection>
!
module surfacegrid_module
  !
  ! - Module references
  !
  use amod_module
  use cio_module
  use getsys_module
  use interpolate_module
  use matfun_module
  use memfun_module
  use named_constants_module
  use pathcheck_module
  use pathchoose_module
  use pc_module
  use pcpsx_module
  use string_module
  !
  implicit none
  !
  private
  public :: surfacegrid_create
  public :: surfacegrid_delete
  public :: surfacegrid_nullify
  public :: surfacegrid_initialize
  public :: surfacegrid_get 
  public :: surfacegrid_put 
  public :: surfacegrid_verify 
  public :: surfacegrid_visible 
  public :: surfacegrid_sensitive 
  public :: surfacegrid_depth
  public :: surfacegrid_size     ! return the size of a surface file
  public :: surfacegrid_set_constant_path 
  public :: surfacegrid_read     ! read  1 surface file
  public :: surfacegrid_read_p1 
  public :: surfacegrid_read_p2 
  public :: surfacegrid_read_1 
  public :: surfacegrid_read_2 
  public :: surfacegrid_print    ! print 1 surface surfaces
  public :: surfacegrid_print_0 
  public :: surfacegrid_print_1 
  public :: surfacegrid_print_2 
  public :: surfacegrid_compute 
  public :: surfacegrid_index 
  public :: surfacegrid_flag 
  !
  ! interfaces
  !
  interface surfacegrid_read
    !
    module procedure surfacegrid_read_0 
    module procedure surfacegrid_read_p1 
    module procedure surfacegrid_read_p2 
    module procedure surfacegrid_read_1 
    module procedure surfacegrid_read_2 
    !
  end interface 
  !
  interface surfacegrid_print
    !
    module procedure surfacegrid_print_0 
    module procedure surfacegrid_print_1 
    module procedure surfacegrid_print_2 
    !
  end interface 
  !
  interface surfacegrid_flag 
    !
    module procedure surfacegrid_flag_n 
    module procedure surfacegrid_flag_1 
    !
  end interface 
  !
  ! functions
  !
  type, public :: surfacegrid_struct
    !
    character(len=filename_length)     :: c_title
    character(len=8)                   :: opt_surface ! surface option
    logical                            :: opt_surface_cons
    logical                            :: opt_surface_path
    real                               :: const_surface 
    character(len=filename_length)     :: path_surface 
    type(pathchoose_struct),   pointer :: select_path_surface 
    !
    ! table surface depth definiton
    !
    integer                            :: hx_sur   ! x hdr word
    integer                            :: hy_sur   ! y hdr word
    !
    integer                            :: nx_sur
    real                               :: x0_sur
    real                               :: dx_sur
    integer                            :: ny_sur
    real                               :: y0_sur
    real                               :: dy_sur
    real,                      pointer :: rz_sur ( :, : )
    !
    integer                            :: nx_loc        ! loc x num values
    real                               :: x0_loc        ! loc x min value
    real                               :: x1_loc        ! loc x max value
    real                               :: dx_loc        ! loc x inc value
    real                               :: rx_scl        ! loc x scl
    integer                            :: ny_loc        ! loc y num values
    real                               :: y0_loc        ! loc y min value
    real                               :: y1_loc        ! loc y max value
    real                               :: dy_loc        ! loc y inc value
    real                               :: ry_scl        ! loc y scl
    integer                            :: nz_sur        ! sur z num values
    real                               :: z0_sur        ! sur z min value
    real                               :: z1_sur        ! sur z max value
    real                               :: dz_sur        ! sur z in value
    logical                            :: lz_sur        ! sur z flag 
    integer                            :: jz_sur_1      ! sur z top index
    integer                            :: jz_sur_2      ! sur z bot index
    real                               :: rz_sur_0      ! sur z index
    real                               :: rz_sur_1      ! sur z top value
    real                               :: rz_sur_2      ! sur z bot value
    integer,                   pointer :: jz_sur ( :, : )
    !
  end type surfacegrid_struct
  !
  integer,    parameter :: n_opt_const = 2
  character(len=8),save :: c_opt_const(n_opt_const) &
  = (/ 'CONSTANT', 'PATH    ' /)
  !
  ! rcs identifier string
  character(len=100),public,save :: surfacegrid_IDENT = &
    '$Id: surfacegrid.f90,v 1.5 2007/06/04 15:13:47 Hanson beta sps $'
  !
  contains
  !
  subroutine surfacegrid_create ( sur, c_title, i_err )
    !
    ! create the surfacegrid structure
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    character(len=*),    intent(in   ) :: c_title ! title
    integer,             intent(inout) :: i_err   ! err 0=o.k. <0=err
    !
    i_err = 0
    !
    allocate ( sur )
    !
    nullify ( sur%select_path_surface ) !jpa
    nullify ( sur%rz_sur ) !jpa
    nullify ( sur%jz_sur )
    !
    call surfacegrid_initialize ( sur )
    !
    call pathchoose_create ( sur%select_path_surface, 'path_surface', '*' )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    sur%c_title = c_title
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in surfacegrid_create ", &
    & /, " during xxx " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in surfacegrid_create " &
    & )') 
    !
    i_err = -1
    !
    return
    !
  end subroutine surfacegrid_create 
  !
  subroutine surfacegrid_delete ( sur )
    !
    !  delete the surfacegrid posiiton structure
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    ! delete the select strucutres
    !
           if ( associated ( sur%select_path_surface ) ) & 
    call pathchoose_delete ( sur%select_path_surface )
    !
    call memfun_del ( sur%rz_sur )
    call memfun_del ( sur%jz_sur )
    !
    if ( associated ( sur ) ) &
         deallocate ( sur )
    !
    return
    !
  end subroutine surfacegrid_delete
  !
  subroutine surfacegrid_nullify ( sur )
    !
    !  nullify the surfacegrid posiiton structure
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    if ( .not. associated ( sur ) ) return
    !
    sur%const_surface = 0        ! constant gat surface level
    !
    nullify ( sur%select_path_surface )
    !
    call memfun_nul ( sur%rz_sur )
    call memfun_nul ( sur%jz_sur )
    !
    return
    !
  end subroutine surfacegrid_nullify
  !
  subroutine surfacegrid_initialize ( sur )
    !
    !  initialize the surfacegrid posiiton structure
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    integer                            :: i_err
    !
    i_err = 0
    !
    if ( .not. associated ( sur ) ) return
    !
    !  nullify the surfacegrid posiiton structure
    !
    call surfacegrid_nullify ( sur )
    !
    call memfun_init ( sur%c_title  )
    call memfun_init ( sur%rz_sur   )
    call memfun_init ( sur%jz_sur   )
    call memfun_init ( sur%nx_loc   ) ! loc x num values
    call memfun_init ( sur%x0_loc   ) ! loc x min value
    call memfun_init ( sur%x1_loc   ) ! loc x max value
    call memfun_init ( sur%dx_loc   ) ! loc x inc value
    call memfun_init ( sur%rx_scl   ) ! loc x scl
    call memfun_init ( sur%ny_loc   ) ! loc y num values
    call memfun_init ( sur%y0_loc   ) ! loc y min value
    call memfun_init ( sur%y1_loc   ) ! loc y max value
    call memfun_init ( sur%dy_loc   ) ! loc y inc value
    call memfun_init ( sur%ry_scl   ) ! loc y scl
    call memfun_init ( sur%lz_sur   ) ! sur z flag
    call memfun_init ( sur%nz_sur   ) ! sur z num values
    call memfun_init ( sur%z0_sur   ) ! sur z min value
    call memfun_init ( sur%z1_sur   ) ! sur z max value
    call memfun_init ( sur%dz_sur   ) ! sur z in value
    call memfun_init ( sur%jz_sur_1 ) ! top sur datum index
    call memfun_init ( sur%jz_sur_2 ) ! bot sur datum index
    call memfun_init ( sur%rz_sur_0 ) ! sur datum index
    call memfun_init ( sur%rz_sur_1 ) ! top sur datum index
    call memfun_init ( sur%rz_sur_2 ) ! bot sur datum index
    !
    call memfun_init ( sur%hx_sur )
    call memfun_init ( sur%nx_sur )
    call memfun_init ( sur%x0_sur )
    call memfun_init ( sur%dx_sur )
    !
    call memfun_init ( sur%hy_sur )
    call memfun_init ( sur%ny_sur )
    call memfun_init ( sur%y0_sur )
    call memfun_init ( sur%dy_sur )
    !
    sur%opt_surface     = 'CONSTANT'
    sur%const_surface   = 0                   ! constant gat surface level
    sur%path_surface    = pathcheck_empty     ! gat surface file
    !
    sur%opt_surface_cons = &
    string_upper_compare ( sur%opt_surface, 'CONSTANT' ) 
    !
    sur%opt_surface_path = .not. sur%opt_surface_cons 
    !
    sur%hx_sur = 7
    sur%nx_sur = 1
    sur%x0_sur = 0.
    sur%dx_sur = 1.
    !
    sur%hy_sur = 8
    sur%ny_sur = 1
    sur%y0_sur = 0.
    sur%dy_sur = 1.
    !
    sur%lz_sur   = .false. ! sur z flag
    sur%nz_sur   = 1       ! sur z num values
    sur%z0_sur   = 0.      ! sur z min value
    sur%z1_sur   = 0.      ! sur z max value
    sur%dz_sur   = 1.      ! sur z in value
    sur%rz_sur_1 = 0.
    sur%rz_sur_2 = 0.
    sur%jz_sur_1 = 1 
    sur%jz_sur_2 = 1
    !
    sur%nx_loc   = 1  ! loc x num values
    sur%x0_loc   = 0. ! loc x min value
    sur%x1_loc   = 0. ! loc x max value
    sur%dx_loc   = 1. ! loc x inc value
    sur%rx_scl   = 1. ! loc x scl
    !
    sur%ny_loc   = 1  ! loc y num values
    sur%y0_loc   = 0. ! loc y min value
    sur%y1_loc   = 0. ! loc y max value
    sur%dy_loc   = 1. ! loc y inc value
    sur%ry_scl   = 1. ! loc y scl
    !
    return
    !
  end subroutine surfacegrid_initialize
  !
  subroutine surfacegrid_get ( sur )
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    if ( pathchoose_update ( sur%select_path_surface, &
                             sur%path_surface ) ) &
    return
    !
    call pc_get ( 'opt_datum',       sur%opt_surface     )
    call pc_get ( 'path_datum',      sur%path_surface    )
    call pc_get ( 'const_datum',     sur%const_surface   )
    !
    call pc_get ( 'opt_surface',     sur%opt_surface     )
    call pc_get ( 'path_surface',    sur%path_surface    )
    call pc_get ( 'const_surface',   sur%const_surface   )
    !
    return
    !
  end subroutine surfacegrid_get
  !
  subroutine surfacegrid_put ( sur )
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    ! put parameters
    !
    call amod_line_feed ( 'surfacegrid_surface' )
    call pc_put ( 'opt_surface',     sur%opt_surface     )
    call pc_put ( 'const_surface',   sur%const_surface   )
    call pc_put ( 'path_surface',    sur%path_surface    )
    !
    return
    !
  end subroutine surfacegrid_put
  !
  subroutine surfacegrid_verify ( sur )
    !
    ! verify parameters
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    call surfacegrid_set_constant_path ( sur%opt_surface )
    !
    if ( .not. pc_do_not_process_traces() &
    .and. string_upper_compare ( sur%path_surface, pathcheck_empty ) ) &
    sur%opt_surface = 'CONSTANT' 
    !
    if ( .not. pc_do_not_process_traces() &
    .and. string_upper_compare ( sur%opt_surface, 'CONSTANT' ) ) &
    sur%path_surface = pathcheck_empty
    !
    sur%opt_surface_cons = &
    string_upper_compare ( sur%opt_surface, 'CONSTANT' ) 
    !
    sur%opt_surface_path = .not. sur%opt_surface_cons 
    !
    call surfacegrid_sensitive ( sur )
    !
    return
    !
  end subroutine surfacegrid_verify
  !
  subroutine surfacegrid_sensitive ( sur )
    !
    ! put options
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    call pc_put_options_field ('opt_surface', c_opt_const,    n_opt_const    )
    !
    return
    !
  end subroutine surfacegrid_sensitive 
  !
  subroutine surfacegrid_visible ( sur )
    !
    ! set parameter visibility
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    logical                            :: ls0
    logical                            :: lsc
    logical                            :: lsp
    !
    call surfacegrid_vis_sen ( 'opt_surface',         .true.,  .true.  )
    call surfacegrid_vis_sen ( 'const_surface',       .true.,  .true.  )
    call surfacegrid_vis_sen ( 'select_path_surface', .true.,  .true.  )
    call surfacegrid_vis_sen ( 'path_surface',        .true.,  .true.  )
    !
    call surfacegrid_con_sen ( &
sur%opt_surface, 'const_surface', 'path_surface', 'select_path_surface')
    !
    ls0 = .true.
    !
    lsc = ls0
    !
    lsp = ls0
    !
    if ( .not. string_upper_compare ( sur%opt_surface, 'CONSTANT' ) ) &
    lsc = .false.
    !
    if ( .not. string_upper_compare ( sur%opt_surface, 'PATH'     ) ) &
    lsp = .false.
    !
    call pc_put_sensitive_field_flag ( 'opt_surface',         ls0    )
    call pc_put_sensitive_field_flag ( 'const_surface',       lsc    )
    call pc_put_sensitive_field_flag ( 'select_path_surface', lsp    )
    call pc_put_sensitive_field_flag ( 'path_surface',        lsp    )
    !
    return
    !
  end subroutine surfacegrid_visible 
  !
  subroutine surfacegrid_vis_sen ( par_0, vis_0, sen_0 )
    !
    ! set parameters visibility and sensitivity
    ! 
    character(len=*),    intent(in   ) :: par_0
    logical,             intent(in   ) :: vis_0
    logical,             intent(in   ) :: sen_0
    !
    call pc_put_visible_flag         ( par_0, vis_0 )
    call pc_put_sensitive_field_flag ( par_0, sen_0 )
    !
    return
    !
  end subroutine surfacegrid_vis_sen 
  !
  subroutine surfacegrid_con_sen ( par_0, par_1, par_2, par_3 )
    !
    ! set parameters sensitivity for constant or path variables
    ! if par_0=constant par_1 is unchanged   and par_2, par_3 are insensitive
    !         otherwise par_1 is insensitive and par_2, par_3 are unchanged   
    !
    character(len=*),    intent(in   ) :: par_0
    character(len=*),    intent(in   ) :: par_1
    character(len=*),    intent(in   ) :: par_2
    character(len=*),    intent(in   ) :: par_3
    !
    xxif_par_0 : if ( string_upper_compare ( par_0, 'CONSTANT' ) ) then
      !
      !call pc_put_sensitive_field_flag ( par_1, .true.  )
      call pc_put_sensitive_field_flag ( par_2, .false. )
      call pc_put_sensitive_field_flag ( par_3, .false. )
      !
    else xxif_par_0
      !
      call pc_put_sensitive_field_flag ( par_1, .false. )
      !call pc_put_sensitive_field_flag ( par_2, .true.  )
      !call pc_put_sensitive_field_flag ( par_3, .true.  )
      !
    end if xxif_par_0
    !
    return
    !
  end subroutine surfacegrid_con_sen 
  !
  subroutine surfacegrid_depth ( sur, rx_sur, ry_sur, rz_sur )
    !
    !  get the trace gat depths
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    real,                intent(in   ) :: rx_sur ! gat x location
    real,                intent(in   ) :: ry_sur ! gat y location
    real,                intent(inout) :: rz_sur ! gat z location
    !
    !  get the gat depth from the gat surface horizon
    !
    call interpolate_2d_to_0d ( &
                                sur%nx_sur, sur%x0_sur, sur%dx_sur, &
                                sur%ny_sur, sur%y0_sur, sur%dy_sur, &
                                sur%rz_sur, &
                                rx_sur, &
                                ry_sur, &
                                rz_sur &
                              )
    !
    return
    !
  end subroutine surfacegrid_depth 
  !
  ! get the size of a surface file
  !
  subroutine surfacegrid_size ( &
                                opt_surface, path_surface, &
                                hx_dtm, nx_dtm, x0_dtm, dx_dtm, &
                                hy_dtm, ny_dtm, y0_dtm, dy_dtm, &
                                i_err &
                               )
    !
    character(len=*),    intent(in   ) :: opt_surface ! surface option
    character(len=*),    intent(in   ) :: path_surface   ! surface surface file
    !
    integer,             intent(inout) :: hx_dtm ! file x header word
    integer,             intent(inout) :: nx_dtm ! file x number of points
    real,                intent(inout) :: x0_dtm ! file x origin
    real,                intent(inout) :: dx_dtm ! file x increment
    !
    integer,             intent(inout) :: hy_dtm ! file y header word
    integer,             intent(inout) :: ny_dtm ! file y number of points
    real,                intent(inout) :: y0_dtm ! file y origin
    real,                intent(inout) :: dy_dtm ! file y increment
    !
    integer,             intent(inout) :: i_err     ! err flag 0=o.k. -1=error
    !
    ! local variables
    !
    integer                            :: i_stat  ! open,read, close status flag
    integer                            :: lu_inp  ! input logical unit number
    !
    integer                            :: l_crd_160
    character(len=160)                 :: crd_160
    integer                            :: temp
    !
    i_err = 0
    !
    ! open and read the surface file header
    !
    xxif_opt_surface_constant : &
    if ( string_upper_compare ( path_surface, pathcheck_empty ) &
    .or. string_upper_compare ( opt_surface, 'CONSTANT' ) ) then
      !
      hx_dtm = 7
      nx_dtm = 1
      x0_dtm = 0.
      dx_dtm = 1.
      !
      hy_dtm = 8
      ny_dtm = 1
      y0_dtm = 0.
      dy_dtm = 1.
      !
    else xxif_opt_surface_constant
      !
      ! open the surface file
      !
      lu_inp = cio_fopen ( path_surface, 'r+' )
      !
      if ( lu_inp .le. 0) go to 998
      !
      ! read the x,y info
      !
      temp = len(crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
      !
      read ( crd_160, *, err=997 ) hx_dtm, nx_dtm, x0_dtm, dx_dtm
      !
      temp = len(crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
      !
      read ( crd_160, *, err=997 ) hy_dtm, ny_dtm, y0_dtm, dy_dtm
      !
      ! close the file
      !
      i_stat = cio_fclose (lu_inp )
      !
    end if xxif_opt_surface_constant 
    !
    ! if there has been an error we will come to here from below
    ! if there has been no error we will come to here from above
    !
1999 continue
    !
    ! all done
    !
    return
    !
997 continue
    !
    call pc_error ( ' error in surfacegrid_size during read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in surfacegrid_size during open ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( 'error during surfacegrid_size  ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine surfacegrid_size
  !
  subroutine surfacegrid_set_constant_path ( opt_file )
    !
    !  set the constant_path to CONSTANT or PATH
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
  end subroutine surfacegrid_set_constant_path
  !
  subroutine surfacegrid_read_0 ( &
    sur, i_pel, broadcast, hx_sur0, hy_sur0, i_err )
    !
    type ( surfacegrid_struct ), pointer :: sur   ! surfacegrid structure
    !
    integer,             intent(in   ) :: i_pel
    logical,             intent(in   ) :: broadcast
    integer,             intent(in   ) :: hx_sur0        ! x hdr word
    integer,             intent(in   ) :: hy_sur0        ! y hdr word
    integer,             intent(inout) :: i_err         ! error 0 O.K. -1 err
    integer                            :: hx_sur        ! x hdr word
    integer                            :: hy_sur        ! y hdr word
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    !print'(" top surfacegrid_read_0 p=",i4," c=",i8," b=",l2," h=",i8,1x,i8)',&
    !pcpsx_i_pel(), i_call, broadcast, hx_sur0, hy_sur0
    !
    if ( broadcast ) &
    call pcpsx_broadcast ( i_pel, i_err  )
    !
    !print'(" qq1 surfacegrid_read_0 p=",i4," b=",l2," h=",i8,1x,i8)', &
    !pcpsx_i_pel(), broadcast, hx_sur0, hy_sur0
    !
    hx_sur = hx_sur0
    !
    hy_sur = hy_sur0
    !
    sur%hx_sur = hx_sur ! x hdr word
    !
    sur%hy_sur = hy_sur ! y hdr word
    !
    ! initialize the number of datum locations to 1
    ! surfacegrid_read will check this and change if need be.
    ! note x and y pos values are in original hx_sur, hy_sur units
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /, "  surfacegrid revision: ", &
    & " 5  2007-05-31  Douglas Hanson Fix x,y file transpose. n" &
    & ) ' )
    !
    ! initialize the number of datum locations to 1
    ! surfacegrid_read will check this and change if need be.
    ! note x and y pos values are in original hx_sur, hy_sur units
    !
    sur%nx_sur = 1
    sur%x0_sur = 0.
    sur%dx_sur = 1.
    !
    sur%ny_sur = 1
    sur%y0_sur = 0.
    sur%dy_sur = 1.
    !
    !print'(" aa1 surfacegrid_read_0 p=",i4," e=",i8," a=",l2)', &
    !pcpsx_i_pel(), i_err, associated ( sur%rz_sur )
    !
    !print'(" aa1 surfacegrid_read_0 p=",i4," s=",i8)', &
    !pcpsx_i_pel(), size ( sur%rz_sur,1 )
    !
    !print'(" aa1 surfacegrid_read_0 p=",i4,1x,i8,1x,i8,1x,i8,1x,i8, &
    !& 1x,g12.6,1x,g12.6 )', &
    !pcpsx_i_pel(), &
    !sur%nx_sur, sur%ny_sur, &
    !size(sur%rz_sur,1), size(sur%rz_sur,2), & 
    !minval(sur%rz_sur), maxval(sur%rz_sur) 
    !
    ! get the surface values
    !
    call surfacegrid_read ( &
                            i_pel, broadcast, &
                            sur%opt_surface, &
                            sur%path_surface, sur%const_surface, &
                            hx_sur, sur%nx_sur, sur%x0_sur, sur%dx_sur, 1., &
                            hy_sur, sur%ny_sur, sur%y0_sur, sur%dy_sur, 1., &
                            sur%rz_sur, &
                            .true., .false., .true., &
                            i_err &
                          )
    !
    if ( broadcast ) &
    call pcpsx_broadcast ( i_pel, i_err  )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! print the surface datum levels
    !
    call surfacegrid_print ( &
                             'spike gat travel time datums', &
                             0, &
                             sur%opt_surface, &
                             sur%path_surface, sur%const_surface, &
                             hx_sur, sur%nx_sur, sur%x0_sur, sur%dx_sur, 1., &
                             hy_sur, sur%ny_sur, sur%y0_sur, sur%dy_sur, 1., &
                             sur%rz_sur &
                           )
    !
    !print'(" end surfacegrid_read_0 p=",i4)', pcpsx_i_pel()
    !
    !print'(" end surfacegrid_read_0 p=",i4," e=",i8)', &
    !pcpsx_i_pel(), i_err
    !
    return
    !
998 continue
    !
    print'(" error in surfacegrid_read_0 during gat surfacegrid_read " )'
    !
    go to 999
    !
999 continue
    !
    print'(" error in surfacegrid_read_0 " )'
    !
    i_err = -1
    !
    return
    !
  end subroutine surfacegrid_read_0 
  !
  subroutine surfacegrid_read_p1 ( &
                                   i_pel, broadcast, &
                                   opt_surface, path_surface, const_surface, &
                                   hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                                   hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                                   rz_dtm, &
                                   alloc_mem, file_head, file_grid, &
                                   i_err &
                                 )
    !
    integer,             intent(in   ) :: i_pel
    logical,             intent(in   ) :: broadcast
    !
    character(len=*),    intent(in   ) :: opt_surface ! surface option
    character(len=*),    intent(in   ) :: path_surface   ! surface surface file
    real,                intent(in   ) :: const_surface  ! constant surface depth
    !
    integer,             intent(inout) :: hx_dtm ! x output header word
    integer,             intent(inout) :: nx_dtm ! x output number of points
    real,                intent(inout) :: x0_dtm ! x output origin
    real,                intent(inout) :: dx_dtm ! x output increment
    real,                intent(in   ) :: rx_scl ! x scale factor
    !
    integer,             intent(inout) :: hy_dtm ! y output header word
    integer,             intent(inout) :: ny_dtm ! y output number of points
    real,                intent(inout) :: y0_dtm ! y output origin
    real,                intent(inout) :: dy_dtm ! y output increment
    real,                intent(in   ) :: ry_scl ! y scale factor
    !
    real,                      pointer :: rz_dtm ( :, : ) ! x,y,z surface
    !
    logical,             intent(in   ) :: alloc_mem ! allocate memory
    logical,             intent(in   ) :: file_head ! use file header values
    logical,             intent(in   ) :: file_grid ! use file grid   values
    !
    integer,             intent(inout) :: i_err     ! err flag 0=o.k. -1=error
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! local variables
    !
    !print'(" top surfacegrid_read_p1 c=",i8)', i_call
    !
    i_err = 0
    !
    if ( i_pel .eq. pcpsx_i_pel() ) &
    call surfacegrid_read_1 ( &
                              opt_surface, path_surface, const_surface, &
                              hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                              hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                              rz_dtm, &
                              alloc_mem, file_head, file_grid, &
                              i_err &
                            )
    !
    if ( broadcast ) call pcpsx_broadcast ( i_pel, i_err  )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    if_broadcast : if ( broadcast ) then
      !
      call pcpsx_broadcast ( i_pel, hx_dtm )
      call pcpsx_broadcast ( i_pel, nx_dtm )
      call pcpsx_broadcast ( i_pel, x0_dtm )
      call pcpsx_broadcast ( i_pel, dx_dtm )
      call pcpsx_broadcast ( i_pel, hy_dtm )
      call pcpsx_broadcast ( i_pel, ny_dtm )
      call pcpsx_broadcast ( i_pel, y0_dtm )
      call pcpsx_broadcast ( i_pel, dy_dtm )
      !
    end if if_broadcast
    !
    if ( i_pel .ne. pcpsx_i_pel() .and. alloc_mem ) &
    call memfun_all ( rz_dtm, nx_dtm, ny_dtm, 'rz_dtm', i_err )
    !
    if ( broadcast ) &
    call pcpsx_broadcast ( i_pel, ny_dtm, rz_dtm )
    !
    return
    !
999 continue
    !
    call pc_error ( 'error during surfacegrid_read_p1 ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine surfacegrid_read_p1 
  !
  subroutine surfacegrid_read_p2 ( &
                                   i_pel, broadcast, &
                                   opt_surface, path_surface, const_surface, &
                                   hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                                   hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                                   nz_dtm, jz_dtm, &
                                   rz_dtm, &
                                   alloc_mem, file_head, file_grid, &
                                   i_err &
                                 )
    !
    integer,             intent(in   ) :: i_pel
    logical,             intent(in   ) :: broadcast
    !
    character(len=*),    intent(in   ) :: opt_surface ! surface option
    character(len=*),    intent(in   ) :: path_surface   ! surface surface file
    real,                intent(in   ) :: const_surface  ! constant surface depth
    !
    integer,             intent(inout) :: hx_dtm ! x output header word
    integer,             intent(inout) :: nx_dtm ! x output number of points
    real,                intent(inout) :: x0_dtm ! x output origin
    real,                intent(inout) :: dx_dtm ! x output increment
    real,                intent(in   ) :: rx_scl ! x scale factor
    !
    integer,             intent(inout) :: hy_dtm ! y output header word
    integer,             intent(inout) :: ny_dtm ! y output number of points
    real,                intent(inout) :: y0_dtm ! y output origin
    real,                intent(inout) :: dy_dtm ! y output increment
    real,                intent(in   ) :: ry_scl ! y scale factor
    !
    integer,             intent(in   ) :: nz_dtm ! num surface to alloc in rz_dtm
    integer,             intent(in   ) :: jz_dtm ! idx surface to read  in rz_dtm
    !
    real,                      pointer :: rz_dtm ( :, :, : ) ! x,y,z surface
    !
    logical,             intent(in   ) :: alloc_mem ! allocate memory
    logical,             intent(in   ) :: file_head ! use file header values
    logical,             intent(in   ) :: file_grid ! use file grid   values
    !
    integer,             intent(inout) :: i_err     ! err flag 0=o.k. -1=error
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! local variables
    !
    !print'(" top surfacegrid_read_p2 c=",i8," i_pel=",i4," p=",i4)', &
    !i_call, i_pel, pcpsx_i_pel() 
    !
    i_err = 0
    !
    if ( i_pel .eq. pcpsx_i_pel() ) &
    call surfacegrid_read_2 ( &
                              opt_surface, path_surface, const_surface, &
                              hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                              hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                              nz_dtm, jz_dtm, &
                              rz_dtm, &
                              alloc_mem, file_head, file_grid, &
                              i_err &
                            )
    !
    if ( broadcast ) call pcpsx_broadcast ( i_pel, i_err  )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    if_broadcast : if ( broadcast ) then
      !
      call pcpsx_broadcast ( i_pel, hx_dtm )
      call pcpsx_broadcast ( i_pel, nx_dtm )
      call pcpsx_broadcast ( i_pel, x0_dtm )
      call pcpsx_broadcast ( i_pel, dx_dtm )
      call pcpsx_broadcast ( i_pel, hy_dtm )
      call pcpsx_broadcast ( i_pel, ny_dtm )
      call pcpsx_broadcast ( i_pel, y0_dtm )
      call pcpsx_broadcast ( i_pel, dy_dtm )
      !
    end if if_broadcast
    !
    if ( i_pel .ne. pcpsx_i_pel() .and. alloc_mem ) &
    call memfun_all ( rz_dtm, nx_dtm, ny_dtm, nz_dtm, 'rz_dtm', i_err )
    !
    if ( broadcast ) &
    call pcpsx_broadcast ( i_pel, ny_dtm, rz_dtm ( :, :, jz_dtm ) )
    !
    return
    !
999 continue
    !
    call pc_error ( 'error during surfacegrid_read_p2 ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine surfacegrid_read_p2 
  !
  subroutine surfacegrid_read_1 ( &
                                  opt_surface, path_surface, const_surface, &
                                  hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                                  hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                                  rz_dtm, &
                                  alloc_mem, file_head, file_grid, &
                                  i_err &
                                )
    !
    ! get the surface depths from a constant, zc_dtm or a file path_surface
    !
    ! if alloc_mem = .true. surfacegrid will allocate rz_dtm as a 2d array with 
    ! dimensions nx_dtm, ny_dtm. 
    !
    ! if alloc_mem = .fals. surfacegrid assumes rz_dtm has already been allocated
    !
    ! surfacegrid outputs a gridded surface array rz_dtm (:, : )
    !
    ! if path_surface is pathcheck_empty these surfacev alues are the constant 
    ! value const_surface.
    !
    ! if path_surface is not pathcheck_empty these surface values are interpolated 
    ! from those in the file.
    !
    ! if file_head = .true. and path_surface is not pathcehck_empty
    ! surfacegrid will set the output x,y header words, hx_dtm, hy_dtm,
    ! to the file header words, hx_fil, hy_fil respectively.
    !
    ! if file_head = .true. and path_surface is pathcehck_empty
    ! surfacegrid will set the output x,y header words, hx_dtm, hy_dtm,
    ! to 7 and 8 respecitvely
    !
    ! if file_head = .false. surfacegrid will output the surface grid 
    ! useing the header word values and order defined by hx_dtm, hy_dtm
    !
    ! if file_grid = .true. and path_surface is not pathcehck_empty
    !
    ! nx_dtm, x0_dtm, dx_dtm will be set to the file grid values 
    ! multiplied by rx_scl
    ! ny_dtm, y0_dtm, dy_dtm will be set to the file grid values 
    ! multiplied by ry_scl
    !
    ! and surfacegrid will output the surface grid useing the grid defined by the 
    ! file
    !
    ! if file_grid = .true. and path_surface is pathcehck_empty
    !
    ! nx_dtm, x0_dtm, dx_dtm will be set to 1, 0., rx_scl respectively.
    ! ny_dtm, y0_dtm, dy_dtm will be set to 1, 0., ry_scl respectively.
    !
    ! if file_grid = .false. surfacegrid will output the surface grid 
    ! useing the grid defined by the input values of nx_dtm etc.
    !
    ! if path_surface is pathcheck_empty surfacegrid_read_1 sets all surface values 
    ! on the output grid equal to the constant surface value const_surface
    !
    ! if path_surface is not pathcheck_empty surfacegrid_read_1 reads in the file 
    ! surface values on the orignal file grid and lineraly interpolates these to 
    ! the output grid.
    !
    ! if path_surface is not pathcheck_empty 
    ! the file   x,y surface header words, hx_fil and hy_fil, 
    ! must be consistent with
    ! the output x,y surface header words, hx_dtm and hy_dtm.
    !
    ! That is: hx_fil=hx_dtm and hy_fil=hy_dtm
    !      or: hx_fil=hy_dtm and hy_fil=hx_dtm
    !
    !
    character(len=*),    intent(in   ) :: opt_surface ! surface option
    character(len=*),    intent(in   ) :: path_surface   ! surface surface file
    real,                intent(in   ) :: const_surface  ! constant surface depth
    !
    integer,             intent(inout) :: hx_dtm ! x output header word
    integer,             intent(inout) :: nx_dtm ! x output number of points
    real,                intent(inout) :: x0_dtm ! x output origin
    real,                intent(inout) :: dx_dtm ! x output increment
    real,                intent(in   ) :: rx_scl ! x scale factor
    !
    integer,             intent(inout) :: hy_dtm ! y output header word
    integer,             intent(inout) :: ny_dtm ! y output number of points
    real,                intent(inout) :: y0_dtm ! y output origin
    real,                intent(inout) :: dy_dtm ! y output increment
    real,                intent(in   ) :: ry_scl ! y scale factor
    !
    real,                      pointer :: rz_dtm ( :, : ) ! x,y surface
    !
    logical,             intent(in   ) :: alloc_mem ! allocate memory
    logical,             intent(in   ) :: file_head ! use file header values
    logical,             intent(in   ) :: file_grid ! use file grid   values
    !
    integer,             intent(inout) :: i_err     ! err flag 0=o.k. -1=error
    !
    ! local variables
    !
    integer                            :: i_stat  ! open,read, close status flag
    integer                            :: lu_inp  ! input logical unit number
    !
    ! file x grid discription
    !
    integer                            :: hx_fil
    integer                            :: nx_fil
    integer                            :: ix_fil
    real                               :: x0_fil
    real                               :: dx_fil
    !
    ! file y grid discription
    !
    integer                            :: hy_fil
    integer                            :: ny_fil
    integer                            :: iy_fil
    real                               :: y0_fil
    real                               :: dy_fil
    !
    real                               :: rx_tmp
    real                               :: ry_tmp
    real                               :: rz_tmp
    real,                      pointer :: rz_fil ( :, : ) ! file surface depths
    !
    integer                            :: l_crd_160
    character(len=160)                 :: crd_160
    integer, save                           :: i_call = 0
    integer                            :: temp
    !
    i_call = i_call + 1
    !
    !print'(" top surfacegrid_read_1 c=",i8)', i_call
    !
    i_err = 0
    !
    ! open and read the surface file
    !
    !write (pc_get_lun(), '( &
    !& /, " surfacegrid_read_1 ", &
    !& /, " path_surface=", a &
    !& ) ' ) &
    !trim( path_surface )
    !
    ! open the surface file and get the file sizes
    !
    xxif_opt_surface_constant : &
    if ( string_upper_compare ( path_surface, pathcheck_empty ) &
    .or. string_upper_compare ( opt_surface, 'CONSTANT' ) ) then
      !
      hx_fil = hx_dtm
      nx_fil = 1
      x0_fil = 0.
      dx_fil = 1.
      !
      hy_fil = hy_dtm
      ny_fil = 1
      y0_fil = 0.
      dy_fil = 1.
      !
    else xxif_opt_surface_constant
      !
      ! open the surface file
      !
      lu_inp = cio_fopen ( path_surface, 'r+' )
      !
      if ( lu_inp .le. 0 ) go to 998
      !
      ! read the x,y info
      !
      temp = len(crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
      !
      read ( crd_160, *, err=997 ) hx_fil, nx_fil, x0_fil, dx_fil
      !
      temp = len(crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
      !
      read ( crd_160, *, err=997 ) hy_fil, ny_fil, y0_fil, dy_fil
      !
    end if xxif_opt_surface_constant
    !
    ! if the output grid has not been defined output the original grid
    !
    print*,' hx_fil=',hx_fil,' hy_fil=',hy_fil
    print*,' nx_fil=',nx_fil,' ny_fil=',ny_fil
    !
    if ( file_head ) hx_dtm = hx_fil
    if ( file_head ) hy_dtm = hy_fil
    !
    xxif_hx_dtm_1 : if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
      !
      ! set the x out to x fil and y out to y fil
      !
      if_file_grid_1 : if ( file_grid ) then
        !
        nx_dtm = nx_fil
        x0_dtm = x0_fil
        dx_dtm = dx_fil
        !
        ny_dtm = ny_fil
        y0_dtm = y0_fil
        dy_dtm = dy_fil
        !
      end if if_file_grid_1
      !
      call memfun_all ( rz_fil, nx_fil, ny_fil, 'rz_fil', i_err )
      !
    else if ( hx_dtm .eq. hy_fil .and. hy_dtm .eq. hx_fil ) then
      !
      ! set the x out to y fil and y out to y fil
      !
      if_file_grid_2 : if ( file_grid ) then
        !
        nx_dtm = ny_fil
        x0_dtm = y0_fil
        dx_dtm = dy_fil
        !
        ny_dtm = nx_fil
        y0_dtm = x0_fil
        dy_dtm = dx_fil
        !
      end if if_file_grid_2
      !
      call memfun_all ( rz_fil, ny_fil, nx_fil, 'rz_fil', i_err )
      !
      ! inconsistent header words for file and dtm
      !
    else xxif_hx_dtm_1
      !
      call pc_error ( &
      ' you must use consistent header words for the surfaces and output ' )
      call pc_error ( ' you must use hx_dtm=hx_fil, hy_dtm=hy_fil ' )
      call pc_error ( '           or hy_dtm=hx_fil, hx_dtm=hy_fil ' )
      call pc_error ( ' hx_dtm=',hx_dtm,' hy_dtm=', hy_dtm )
      call pc_error ( ' hx_fil=',hx_fil,' hy_fil=', hy_fil )
      !
      go to 996
      !
    end if xxif_hx_dtm_1
    !
    !print*,' hx_dtm=',hx_dtm,' hy_dtm=',hy_dtm
    !print*,' nx_dtm=',nx_dtm,' ny_dtm=',ny_dtm
    !
    if ( alloc_mem ) &
    call memfun_all ( rz_dtm, nx_dtm, ny_dtm, 'rz_dtm', i_err )
    !
    if ( i_err .ne. 0 ) go to 995
    !
    rz_fil ( :, : ) = const_surface
    !
    ! read in the surface depths in x,y fil order 
    ! the order of the x,y points in rz_fil is the same as ths pos x,y 
    !
    if_path_surface_2 : &
    if ( .not. string_upper_compare ( path_surface, pathcheck_empty ) ) then
      !
      do_iy_fil: do iy_fil = 1 , ny_fil
        !
        do_ix_fil: do ix_fil = 1 , nx_fil
          !
          temp = len(crd_160)
          l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
          read ( crd_160, *, err=997 ) rx_tmp, ry_tmp, rz_tmp
          !
          if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
            !
            rz_fil ( ix_fil, iy_fil ) = rz_tmp
            !
          else    ! if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
            !
            rz_fil ( iy_fil, ix_fil ) = rz_tmp
            !
          end if    ! if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
          !
        end do do_ix_fil
        !
      end do do_iy_fil
      !
    end if if_path_surface_2
    !
    ! scale the x,y grid orign and increments
    !
    !x0_dtm = x0_dtm * rx_scl
    !dx_dtm = dx_dtm * rx_scl
    !
    !y0_dtm = y0_dtm * ry_scl
    !dy_dtm = dy_dtm * ry_scl
    !
    !print*,' nx_fil=',nx_fil,' ny_fil=',ny_fil, &
    !' rz_fil=',minval(rz_fil), maxval(rz_fil)
    !
    ! interpolate from the fil x,y grid to the pos x,y grid
    !
    if_hx_dtm_2 : if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
      !
      call interpolate_2d_to_2d ( &
                    n1_inp=nx_fil, o1_inp=x0_fil, d1_inp=dx_fil, &
                    n2_inp=ny_fil, o2_inp=y0_fil, d2_inp=dy_fil, &
                    x0_inp=rz_fil, &
                    n1_out=nx_dtm, o1_out=x0_dtm, d1_out=dx_dtm, &
                    n2_out=ny_dtm, o2_out=y0_dtm, d2_out=dy_dtm, &
                    x0_out=rz_dtm &
                                )
      !
      ! interpolate from the fil y,x grid to the pos x,y grid
      !
    else if ( hy_dtm .eq. hx_fil .and. hx_dtm .eq. hy_fil ) then
      !
      call interpolate_2d_to_2d ( &
                    n1_inp=ny_fil, o1_inp=y0_fil, d1_inp=dy_fil, &
                    n2_inp=nx_fil, o2_inp=x0_fil, d2_inp=dx_fil, &
                    x0_inp=rz_fil, &
                    n1_out=nx_dtm, o1_out=x0_dtm, d1_out=dx_dtm, &
                    n2_out=ny_dtm, o2_out=y0_dtm, d2_out=dy_dtm, &
                    x0_out=rz_dtm &
                                )
      !
    end if if_hx_dtm_2 
    !
    !print*,' nx_dtm=',nx_dtm,' ny_dtm=',ny_dtm, &
    !' rz_dtm=',minval(rz_dtm), maxval(rz_dtm)
    !
    ! if there has been an error we will come to here from below
    ! if there has been no error we will come to here from above
    !
1999 continue
    !
    ! close the file
    !
    if ( .not. string_upper_compare ( path_surface, pathcheck_empty ) ) &
    i_stat = cio_fclose (lu_inp )
    !
    !print*,' hx_dtm=',hx_dtm,' hx_fil=',hx_fil,' rx_scl=',rx_scl
    !print*,' hy_dtm=',hy_dtm,' hy_fil=',hy_fil,' ry_scl=',ry_scl
    !print*,' nx_fil=',nx_fil,' x0_fil=',x0_fil,' dx_fil=',dx_fil
    !print*,' ny_fil=',ny_fil,' y0_fil=',y0_fil,' dy_fil=',dy_fil
    !print*,' rz_fil=',minval(rz_fil),maxval(rz_fil)
    !print*,' nx_dtm=',nx_dtm,' x0_dtm=',x0_dtm,'dx_dtm=',dx_dtm
    !print*,' ny_dtm=',ny_dtm,' y0_dtm=',y0_dtm,'dy_dtm=',dy_dtm
    !print*,' rz_dtm=',minval(rz_dtm),maxval(rz_dtm)
    !
    ! free memory
    !
    call memfun_del ( rz_fil )
    !
    ! all done
    !
    return
    !
995 continue
    !
    call pc_error ( ' error in surfacegrid_read_1 during mem alloc ' )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( ' error in surfacegrid_read_1 in header ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in surfacegrid_read_1 during read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in surfacegrid_read_1 during open  ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( 'error during surfacegrid_read_1  ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine surfacegrid_read_1
  !
  subroutine surfacegrid_read_2 ( &
                                  opt_surface, path_surface, const_surface, &
                                  hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                                  hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                                  nz_dtm, jz_dtm, &
                                  rz_dtm, &
                                  alloc_mem, file_head, file_grid, &
                                  i_err &
                                )
    !
    ! get the surface depths from a constant, zc_dtm or a file path_surface
    !
    ! if alloc_mem = .true. surfacegrid will allocate rz_dtm as a 3d array with 
    ! dimensions nx_dtm, ny_dtm, nz_dtm.
    !
    ! if alloc_mem = .fals. surfacegrid assumes rz_dtm has already been allocated
    !
    ! surfacegrid outputs a gridded surface array rz_dtm (:, :, jz_dtm )
    !
    ! if path_surface is pathcheck_empty these surfacev alues are the constant 
    ! value const_surface.
    !
    ! if path_surface is not pathcheck_empty these surface values are interpolated 
    ! from those in the file.
    !
    ! if file_head = .true. and path_surface is not pathcehck_empty
    ! surfacegrid will set the output x,y header words, hx_dtm, hy_dtm,
    ! to the file header words, hx_fil, hy_fil respectively.
    !
    ! if file_head = .true. and path_surface is pathcehck_empty
    ! surfacegrid will set the output x,y header words, hx_dtm, hy_dtm,
    ! to 7 and 8 respecitvely
    !
    ! if file_head = .false. surfacegrid will output the surface grid 
    ! useing the header word values and order defined by hx_dtm, hy_dtm
    !
    ! if file_grid = .true. and path_surface is not pathcehck_empty
    !
    ! nx_dtm, x0_dtm, dx_dtm will be set to the file grid values 
    ! multiplied by rx_scl
    ! ny_dtm, y0_dtm, dy_dtm will be set to the file grid values 
    ! multiplied by ry_scl
    !
    ! and surfacegrid will output the surface grid useing the grid defined by the 
    ! file
    !
    ! if file_grid = .true. and path_surface is pathcehck_empty
    !
    ! nx_dtm, x0_dtm, dx_dtm will be set to 1, 0., rx_scl respectively.
    ! ny_dtm, y0_dtm, dy_dtm will be set to 1, 0., ry_scl respectively.
    !
    ! if file_grid = .false. surfacegrid will output the surface grid 
    ! useing the grid defined by the input values of nx_dtm etc.
    !
    ! if path_surface is pathcheck_empty surfacegrid_read_2 sets all surface values 
    ! on the output grid equal to the constant surface value const_surface
    !
    ! if path_surface is not pathcheck_empty surfacegrid_read_2 reads in the file 
    ! surface values on the orignal file grid and lineraly interpolates these to 
    ! the output grid.
    !
    ! if path_surface is not pathcheck_empty 
    ! the file   x,y surface header words, hx_fil and hy_fil, 
    ! must be consistent with
    ! the output x,y surface header words, hx_dtm and hy_dtm.
    !
    ! That is: hx_fil=hx_dtm and hy_fil=hy_dtm
    !      or: hx_fil=hy_dtm and hy_fil=hx_dtm
    !
    character(len=*),    intent(in   ) :: opt_surface ! surface option
    character(len=*),    intent(in   ) :: path_surface   ! surface surface file
    real,                intent(in   ) :: const_surface  ! constant surface depth
    !
    integer,             intent(inout) :: hx_dtm ! x output header word
    integer,             intent(inout) :: nx_dtm ! x output number of points
    real,                intent(inout) :: x0_dtm ! x output origin
    real,                intent(inout) :: dx_dtm ! x output increment
    real,                intent(in   ) :: rx_scl ! x scale factor
    !
    integer,             intent(inout) :: hy_dtm ! y output header word
    integer,             intent(inout) :: ny_dtm ! y output number of points
    real,                intent(inout) :: y0_dtm ! y output origin
    real,                intent(inout) :: dy_dtm ! y output increment
    real,                intent(in   ) :: ry_scl ! y scale factor
    !
    integer,             intent(in   ) :: nz_dtm ! num surface to alloc in rz_dtm
    integer,             intent(in   ) :: jz_dtm ! surface index to read in rz_dtm
    !
    real,                      pointer :: rz_dtm ( :, :, : ) ! x,y,z surface
    !
    logical,             intent(in   ) :: alloc_mem ! allocate memory
    logical,             intent(in   ) :: file_head ! use file header values
    logical,             intent(in   ) :: file_grid ! use file grid   values
    !
    integer,             intent(inout) :: i_err     ! err flag 0=o.k. -1=error
    !
    ! local variables
    !
    integer                            :: i_stat  ! open,read, close status flag
    integer                            :: lu_inp  ! input logical unit number
    !
    ! file x grid discription
    !
    integer                            :: hx_fil
    integer                            :: nx_fil
    integer                            :: ix_fil
    real                               :: x0_fil
    real                               :: dx_fil
    !
    ! file y grid discription
    !
    integer                            :: hy_fil
    integer                            :: ny_fil
    integer                            :: iy_fil
    real                               :: y0_fil
    real                               :: dy_fil
    !
    real                               :: rx_tmp
    real                               :: ry_tmp
    real                               :: rz_tmp
    real,                      pointer :: rz_fil ( :, : ) ! file surface depths
    !
    integer                            :: l_crd_160
    character(len=160)                 :: crd_160
    integer, save                           :: i_call = 0
    integer                                 :: temp
    !
    i_call = i_call + 1
    !
    !print'(" top surfacegrid_read_2 c=",i8)', i_call
    !
    nullify (rz_fil) ! jpa
    !
    i_err = 0
    !
    ! open and read the surface file
    !
    !print'( &
    !&    " surfacegrid_read_2 opt_surface=", a, &
    !& /, " surfacegrid_read_2 path_surface=", a &
    !& )', &
    !trim(opt_surface), &
    !trim( path_surface )
    !
    ! open the surface file and get the file sizes
    !
    xxif_opt_surface_constant : &
    if ( string_upper_compare ( path_surface, pathcheck_empty ) &
    .or. string_upper_compare ( opt_surface, 'CONSTANT' ) ) then
      !
      hx_fil = hx_dtm
      nx_fil = 1
      x0_fil = 0.
      dx_fil = 1.
      !
      hy_fil = hy_dtm
      ny_fil = 1
      y0_fil = 0.
      dy_fil = 1.
      !
    else xxif_opt_surface_constant
      !
      ! open the surface file
      !
      lu_inp = cio_fopen ( path_surface, 'r+' )
      !
      if ( lu_inp .le. 0 ) go to 998
      !
      ! read the x,y info
      !
      temp = len(crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
      !
      read ( crd_160, *, err=997 ) hx_fil, nx_fil, x0_fil, dx_fil
      !
      temp = len(crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
      !
      read ( crd_160, *, err=997 ) hy_fil, ny_fil, y0_fil, dy_fil
      !
    end if xxif_opt_surface_constant
    !
    ! if the output grid has not been defined output the original grid
    !
    if ( file_head ) hx_dtm = hx_fil
    if ( file_head ) hy_dtm = hy_fil
    !
    xxif_hx_dtm_1 : if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
      !
      ! set the x out to x fil and y out to y fil
      !
      if_file_grid_1 : if ( file_grid ) then
        !
        nx_dtm = nx_fil
        x0_dtm = x0_fil
        dx_dtm = dx_fil
        !
        ny_dtm = ny_fil
        y0_dtm = y0_fil
        dy_dtm = dy_fil
        !
      end if if_file_grid_1
      !
      call memfun_all ( rz_fil, nx_fil, ny_fil, 'rz_fil', i_err )
      !
    else if ( hx_dtm .eq. hy_fil .and. hy_dtm .eq. hx_fil ) then
      !
      ! set the x out to y fil and y out to y fil
      !
      if_file_grid_2 : if ( file_grid ) then
        !
        nx_dtm = ny_fil
        x0_dtm = y0_fil
        dx_dtm = dy_fil
        !
        ny_dtm = nx_fil
        y0_dtm = x0_fil
        dy_dtm = dx_fil
        !
      end if if_file_grid_2
      !
      call memfun_all ( rz_fil, ny_fil, nx_fil, 'rz_fil', i_err )
      !
      ! inconsistent header words for file and dtm
      !
    else xxif_hx_dtm_1
      !
      call pc_error ( &
      ' you must use consistent header words for the surfaces and output ' )
      call pc_error ( ' you must use hx_dtm=hx_fil, hy_dtm=hy_fil ' )
      call pc_error ( '           or hy_dtm=hx_fil, hx_dtm=hy_fil ' )
      call pc_error ( ' hx_dtm=',hx_dtm,' hy_dtm=', hy_dtm )
      call pc_error ( ' hx_fil=',hx_fil,' hy_fil=', hy_fil )
      !
      go to 996
      !
    end if xxif_hx_dtm_1
    !
    if ( alloc_mem ) &
    call memfun_all ( rz_dtm, nx_dtm, ny_dtm, nz_dtm, 'rz_dtm', i_err )
    !
    if ( i_err .ne. 0 ) go to 995
    !
    rz_fil ( :, : ) = const_surface
    !
    ! read in the surface depths in x,y fil order 
    ! the order of the x,y points in rz_fil is the same as ths pos x,y 
    !
    if_path_surface_2 : &
    if ( .not. string_upper_compare ( path_surface, pathcheck_empty ) ) then
      !
      do_iy_fil: do iy_fil = 1 , ny_fil
        !
        do_ix_fil: do ix_fil = 1 , nx_fil
          !
          temp = len(crd_160)
          l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp)
          read ( crd_160, *, err=997 ) rx_tmp, ry_tmp, rz_tmp
          !
          if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
            !
            rz_fil ( ix_fil, iy_fil ) = rz_tmp
            !
          else    ! if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
            !
            rz_fil ( iy_fil, ix_fil ) = rz_tmp
            !
          end if    ! if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
          !
        end do do_ix_fil
        !
      end do do_iy_fil
      !
    end if if_path_surface_2
    !
    ! scale the x,y grid orign and increments
    !
    !x0_dtm = x0_dtm * rx_scl
    !dx_dtm = dx_dtm * rx_scl
    !
    !y0_dtm = y0_dtm * ry_scl
    !dy_dtm = dy_dtm * ry_scl
    !
    ! interpolate from the fil x,y grid to the pos x,y grid
    !
    if_hx_dtm_2 : if ( hx_dtm .eq. hx_fil .and. hy_dtm .eq. hy_fil ) then
      !
      call interpolate_2d_to_2d ( &
                    n1_inp=nx_fil, o1_inp=x0_fil, d1_inp=dx_fil, &
                    n2_inp=ny_fil, o2_inp=y0_fil, d2_inp=dy_fil, &
                    x0_inp=rz_fil, &
                    n1_out=nx_dtm, o1_out=x0_dtm, d1_out=dx_dtm, &
                    n2_out=ny_dtm, o2_out=y0_dtm, d2_out=dy_dtm, &
                    x0_out=rz_dtm(:, :, jz_dtm)  &
                                )
      !
      ! interpolate from the fil y,x grid to the pos x,y grid
      !
    else if ( hy_dtm .eq. hx_fil .and. hx_dtm .eq. hy_fil ) then
      !
      call interpolate_2d_to_2d ( &
                    n1_inp=ny_fil, o1_inp=y0_fil, d1_inp=dy_fil, &
                    n2_inp=nx_fil, o2_inp=x0_fil, d2_inp=dx_fil, &
                    x0_inp=rz_fil, &
                    n1_out=nx_dtm, o1_out=x0_dtm, d1_out=dx_dtm, &
                    n2_out=ny_dtm, o2_out=y0_dtm, d2_out=dy_dtm, &
                    x0_out=rz_dtm(:, :, jz_dtm)  &
                                )
      !
    end if if_hx_dtm_2 
    !
    ! if there has been an error we will come to here from below
    ! if there has been no error we will come to here from above
    !
1999 continue
    !
    ! close the file
    !
    if ( .not. string_upper_compare ( path_surface, pathcheck_empty ) ) &
    i_stat = cio_fclose (lu_inp )
    !
    !print*,' hx_dtm=',hx_dtm,' hx_fil=',hx_fil,' rx_scl=',rx_scl
    !print*,' hy_dtm=',hy_dtm,' hy_fil=',hy_fil,' ry_scl=',ry_scl
    !print*,' nx_fil=',nx_fil,' x0_fil=',x0_fil,' dx_fil=',dx_fil
    !print*,' ny_fil=',ny_fil,' y0_fil=',y0_fil,' dy_fil=',dy_fil
    !print*,' rz_fil=',minval(rz_fil),maxval(rz_fil)
    !print*,' nx_dtm=',nx_dtm,' x0_dtm=',x0_dtm,'dx_dtm=',dx_dtm
    !print*,' ny_dtm=',ny_dtm,' y0_dtm=',y0_dtm,'dy_dtm=',dy_dtm
    !print*,' rz_dtm=',minval(rz_dtm(:,:,jz_dtm)),maxval(rz_dtm(:,:,jz_dtm))
    !
    ! free memory
    !
    call memfun_del ( rz_fil )
    !
    ! all done
    !
    return
    !
995 continue
    !
    call pc_error ( ' error in surfacegrid_read_2 during mem alloc ' )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( ' error in surfacegrid_read_2 in header ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in surfacegrid_read_2 during read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in surfacegrid_read_2 during open  ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( 'error during surfacegrid_read_2  ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine surfacegrid_read_2 
  !
  subroutine surfacegrid_print_0 ( sur, c_title )
    !
    ! print the sur top and bottom source z indices
    ! pes are in sync and do the same things
    !
    type ( surfacegrid_struct ),    pointer :: sur   ! surfacegrid structure
    character(len=*),         intent(in   ) :: c_title ! print title
    !
    real                                    :: rz_tmp ( sur%nx_loc, sur%ny_loc )
    integer                                 :: jx_loc
    integer                                 :: jy_loc
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    print'( &
    & /, " surfacegrid_print p=",i4, 1x, a, &
    & /, " surfacegrid_print p=",i4, 1x, a, &
    & /, " opt_surface=",a, 1x, a, &
    & /, " opt_surface_cons=",l2, 1x, a, &
    & /, " opt_surface_path=",l2, 1x, a, &
    & /, " path_surface=",a, 1x, a, &
    & /, " const_surface=",g12.6, 1x, a, &
    & /, " lz_sur  =",l2, 1x, a, &
    & /, " nz_sur  =",i8, 1x, a, &
    & /, " rz_sur_1=",g12.6," jz_sur_1=",i8, 1x, a, &
    & /, " rz_sur_2=",g12.6," jz_sur_2=",i8, 1x, a, &
    & /, " nx_sur=",i8," x0_sur=",g12.6," dx_sur=",g12.6, 1x, a, &
    & /, " ny_sur=",i8," y0_sur=",g12.6," dy_sur=",g12.6, 1x, a, &
    & /, " nx_loc=",i8," x0_loc=",g12.6," dx_loc=",g12.6, 1x, a, &
    & /, " ny_loc=",i8," y0_loc=",g12.6," dy_loc=",g12.6, 1x, a &
    & )', &
    pcpsx_i_pel(), trim(sur%c_title), &
    pcpsx_i_pel(), trim(c_title), &
    trim(sur%opt_surface), trim(c_title), &
    sur%opt_surface_cons, trim(c_title), &
    sur%opt_surface_path, trim(c_title), &
    trim(sur%path_surface), trim(c_title), &
    sur%const_surface, trim(c_title), &
    sur%lz_sur, trim(c_title), &
    sur%nz_sur, trim(c_title), &
    sur%rz_sur_1, sur%jz_sur_1, trim(c_title), &
    sur%rz_sur_2, sur%jz_sur_2, trim(c_title), &
    sur%nx_sur, sur%x0_sur, sur%dx_sur, trim(c_title), &
    sur%ny_sur, sur%y0_sur, sur%dy_sur, trim(c_title), &
    sur%nx_loc, sur%x0_loc/sur%rx_scl, sur%dx_loc/sur%rx_scl, trim(c_title), &
    sur%ny_loc, sur%y0_loc/sur%rx_scl, sur%dy_loc/sur%rx_scl, trim(c_title) 
    !
    print'(" p=",i4," s=",i8,1x,i8," jz_sur=",i12,1x,i12,1x,a)', &
    pcpsx_i_pel(), &
    size(sur%jz_sur,1), size(sur%jz_sur,2), &
    minval(sur%jz_sur), maxval(sur%jz_sur), &
    trim(c_title) 
    !
    print'(" p=",i4," s=",i8,1x,i8," rz_sur=",g12.6,1x,g1.26,1x,a)', &
    pcpsx_i_pel(), &
    size(sur%rz_sur,1), size(sur%rz_sur,2), &
    minval(sur%rz_sur), maxval(sur%rz_sur), &
    trim(c_title) 
    !
    if ( sur%opt_surface_path .and. sur%lz_sur ) &
      rz_tmp ( 1:sur%nx_loc, 1:sur%ny_loc ) = &
( sur%jz_sur ( 1:sur%nx_loc, 1:sur%ny_loc ) - 1 ) * sur%dz_sur + sur%z0_sur 
    !
    if ( sur%opt_surface_path .and. sur%lz_sur ) &
    print'(" p=",i4," s=",i8,1x,i8," rz_tmp=",g12.6,1x,g1.26,1x,a)', &
    pcpsx_i_pel(), &
    size(rz_tmp,1), size(rz_tmp,2), minval(rz_tmp), maxval(rz_tmp), &
    trim(c_title) 
    !
    do_jy_loc : do jy_loc = 1 , sur%ny_loc 
    !
    if ( sur%opt_surface_path .and. sur%lz_sur ) &
      !
      print'(1x,i8,1x,i8,1x,i8,1x,g12.6,1x,g12.6,1x,a )', &
      ( jx_loc, jy_loc, sur%jz_sur ( jx_loc, jy_loc ), &
      ((jx_loc-1)*sur%dx_loc+sur%x0_loc)/sur%rx_scl, &
      rz_tmp ( jx_loc, jy_loc ), trim(c_title), jx_loc = 1 , sur%nx_loc )
      !
    end do do_jy_loc 
    !
    call surfacegrid_print ( &
                       c_title, pc_get_lun(), &
                       sur%opt_surface, sur%path_surface, sur%const_surface, &
                       sur%hx_sur, sur%nx_sur, sur%x0_sur, sur%dx_sur, 1., &
                       sur%hy_sur, sur%ny_sur, sur%y0_sur, sur%dy_sur, 1., &
                       sur%rz_sur &
                           )
    !
    return
    !
  end subroutine surfacegrid_print_0 
  !
  subroutine surfacegrid_print_1 ( &
                                   c_title, lu_out, &
                                   opt_surface, path_surface, const_surface, &
                                   hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                                   hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                                   rz_dtm &
                                 )
    !
    !  print the surface level depths
    !
    character(len=*),    intent(in   ) :: c_title   ! title for printing
    !
    integer,             intent(in   ) :: lu_out ! unit number for print
    !
    character(len=*),    intent(in   ) :: opt_surface ! surface option
    character(len=*),    intent(in   ) :: path_surface
    real,                intent(in   ) :: const_surface ! constant surface level
    !
    integer,             intent(in   ) :: hx_dtm ! head x table location
    integer,             intent(in   ) :: nx_dtm ! num x table location
    real,                intent(in   ) :: x0_dtm ! min x table location
    real,                intent(in   ) :: dx_dtm ! inc x table location
    real,                intent(in   ) :: rx_scl ! x scale factor
    !
    integer,             intent(in   ) :: hy_dtm ! head y table location
    integer,             intent(in   ) :: ny_dtm ! num y table location
    real,                intent(in   ) :: y0_dtm ! min y table location
    real,                intent(in   ) :: dy_dtm ! inc y table location
    real,                intent(in   ) :: ry_scl ! y scale factor
    !
    real,                intent(in   ) :: rz_dtm(:, : ) ! (nx_dtm, ny_dtm)
    !
    integer                            :: jx_dtm
    real                               :: rx_dtm
    !
    integer                            :: jy_dtm
    real                               :: ry_dtm
    !
    real                               :: rz_min
    real                               :: rz_max
    !
    integer                            :: ix_inc
    integer                            :: iy_inc
    !
    ! no print if lu_out < 0
    !
    if (lu_out .lt. 0 ) return
    !
    ! get min and max surface values
    !
    call matfun_min_max ( &
                          rz_min, rz_max, &
                          nx_dtm, ny_dtm, rz_dtm ( :, :) &
                        )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write(lu_out, '( &
    & /, "  surfacegrid_print_1 ", a, &
    & /, " opt_surface=", a8, &
    & " const_surface=", g12.6, " path_surface=", a, &
    & /, " hx_dtm=", i8, " nx_dtm=", i8, " x0_dtm=", g12.6, " x1_dtm=", g12.6, &
    & " dx_dtm=", g12.6, " rx_scl=", g12.6, &
    & /, " hy_dtm=", i8, " ny_dtm=", i8, " y0_dtm=", g12.6, " y1_dtm=", g12.6, &
    & " dy_dtm=", g12.6, " ry_scl=", g12.6, &
    & /, " rz_min=", g12.6, " rz_max=", g12.6, &
    & /, "      rx_dtm      ry_dtm      rz_dtm      " &
    & ) ' ) &
    trim(c_title), &
    trim(opt_surface), &
    const_surface, trim(path_surface), &
    hx_dtm, nx_dtm, x0_dtm, (nx_dtm-1)*dx_dtm+x0_dtm, dx_dtm, rx_scl, &
    hy_dtm, ny_dtm, y0_dtm, (ny_dtm-1)*dy_dtm+y0_dtm, dy_dtm, ry_scl, &
    rz_min, rz_max
    !
    ix_inc = 10
    if ( ny_dtm .eq. 1 ) ix_inc = 5
    if ( ny_dtm .eq. 1 ) ix_inc = 1
    !
    iy_inc = 10
    if ( ny_dtm .eq. 1 ) iy_inc = 5
    if ( nx_dtm .eq. 1 ) iy_inc = 1
    !
    do_jy_dtm : do jy_dtm = 1 , ny_dtm , iy_inc
      !
      ry_dtm = (jy_dtm - 1) * dy_dtm + y0_dtm
      !
      do_jx_dtm : do jx_dtm = 1 , nx_dtm , ix_inc
        !
        rx_dtm = (jx_dtm - 1) * dx_dtm + x0_dtm
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), '( &
        & 1x, g12.6, 1x, g12.6, 1x, g12.6 &
        & ) ' ) &
        rx_dtm, ry_dtm, rz_dtm(jx_dtm, jy_dtm)
        !
      end do do_jx_dtm
      !
    end do do_jy_dtm
    !
    return
    !
  end subroutine surfacegrid_print_1 
  !
  subroutine surfacegrid_print_2 ( &
                                 c_title, lu_out, &
                              opt_surface_1, path_surface_1, const_surface_1, &
                              opt_surface_2, path_surface_2, const_surface_2, &
                                 hx_dtm, nx_dtm, x0_dtm, dx_dtm, rx_scl, &
                                 hy_dtm, ny_dtm, y0_dtm, dy_dtm, ry_scl, &
                                 nz_dtm, &
                                 rz_dtm_1, &
                                 rz_dtm_2 &
                               )
    !
    !  print the surface level depths
    !
    character(len=*),    intent(in   ) :: c_title   ! title for printing
    !
    integer,             intent(in   ) :: lu_out ! unit number for print
    !
    character(len=*),    intent(in   ) :: opt_surface_1 ! surface option 1
    character(len=*),    intent(in   ) :: path_surface_1
    real,                intent(in   ) :: const_surface_1 ! constant surface level
    !
    character(len=*),    intent(in   ) :: opt_surface_2 ! surface option 2
    character(len=*),    intent(in   ) :: path_surface_2
    real,                intent(in   ) :: const_surface_2 ! constant surface level
    !
    integer,             intent(in   ) :: hx_dtm ! head x table location
    integer,             intent(in   ) :: nx_dtm ! num x table location
    real,                intent(in   ) :: x0_dtm ! min x table location
    real,                intent(in   ) :: dx_dtm ! inc x table location
    real,                intent(in   ) :: rx_scl ! x scale factor
    !
    integer,             intent(in   ) :: hy_dtm ! head y table location
    integer,             intent(in   ) :: ny_dtm ! num y table location
    real,                intent(in   ) :: y0_dtm ! min y table location
    real,                intent(in   ) :: dy_dtm ! inc y table location
    real,                intent(in   ) :: ry_scl ! y scale factor
    !
    integer,             intent(in   ) :: nz_dtm ! num z table location
    !
    real,                intent(in   ) :: rz_dtm_1(:, : ) ! (nx_dtm, ny_dtm)
    real,                intent(in   ) :: rz_dtm_2(:, : ) ! (nx_dtm, ny_dtm)
    !
    integer                            :: jx_dtm
    real                               :: rx_dtm
    !
    integer                            :: jy_dtm
    real                               :: ry_dtm
    !
    real                               :: rz_min_1
    real                               :: rz_max_1
    real                               :: rz_min_2
    real                               :: rz_max_2
    !
    integer                            :: ix_inc
    integer                            :: iy_inc
    !
    ! no print if lu_out < 0
    !
    if (lu_out .lt. 0 ) return
    !
    ! get min and max surface values
    !
    call matfun_min_max ( &
                          rz_min_1, rz_max_1, &
                          nx_dtm, ny_dtm, rz_dtm_1 ( :, :) &
                        )
    !
    call matfun_min_max ( &
                          rz_min_2, rz_max_2, &
                          nx_dtm, ny_dtm, rz_dtm_2 ( :, :) &
                        )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write(lu_out, '( &
    & /, "  surfacegrid_print_2 ", a, &
    & /, " hx_dtm=", i8, " nx_dtm=", i8, " x0_dtm=", g12.6, " x1_dtm=", g12.6, &
    & " dx_dtm=", g12.6, " rx_scl=", g12.6, &
    & /, " hy_dtm=", i8, " ny_dtm=", i8, " y0_dtm=", g12.6, " y1_dtm=", g12.6, &
    & " dy_dtm=", g12.6, " ry_scl=", g12.6, &
    & /, " nz_dtm=", i8, &
    & /, " opt_surface_1=",a8, &
    & " const_surface_1=", g12.6, " path_surface_1=", a, &
    & /, " rz_min_1=", g12.6, " rz_max_1=", g12.6, &
    & /, " opt_surface_2=",a8, &
    & " const_surface_2=", g12.6, " path_surface_2=", a, &
    & /, " rz_min_2=", g12.6, " rz_max_2=", g12.6, &
    & /, "      rx_dtm      ry_dtm      rz_dtm_1      rz_dtm_2" &
    & ) ' ) &
    trim(c_title), &
    hx_dtm, nx_dtm, x0_dtm, (nx_dtm-1)*dx_dtm+x0_dtm, dx_dtm, rx_scl, &
    hy_dtm, ny_dtm, y0_dtm, (ny_dtm-1)*dy_dtm+y0_dtm, dy_dtm, ry_scl, &
    nz_dtm, &
    trim(opt_surface_1), const_surface_1, trim(path_surface_1), &
    rz_min_1, rz_max_1, &
    trim(opt_surface_2), const_surface_2, trim(path_surface_2), &
    rz_min_2, rz_max_2
    !
    ix_inc = 10
    if ( ny_dtm .eq. 1 ) ix_inc = 5
    if ( ny_dtm .eq. 1 ) ix_inc = 1
    !
    iy_inc = 10
    if ( ny_dtm .eq. 1 ) iy_inc = 5
    if ( nx_dtm .eq. 1 ) iy_inc = 1
    !
    do_jy_dtm : do jy_dtm = 1 , ny_dtm , iy_inc
      !
      ry_dtm = (jy_dtm - 1) * dy_dtm + y0_dtm
      !
      do_jx_dtm : do jx_dtm = 1 , nx_dtm , ix_inc
        !
        rx_dtm = (jx_dtm - 1) * dx_dtm + x0_dtm
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), '( &
        & 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, g12.6 &
        & ) ' ) &
        rx_dtm, ry_dtm, rz_dtm_1(jx_dtm, jy_dtm), rz_dtm_2(jx_dtm, jy_dtm)
        !
      end do do_jx_dtm
      !
    end do do_jy_dtm
    !
    return
    !
  end subroutine surfacegrid_print_2
  !
  subroutine surfacegrid_compute ( &
                                   sur, l0_print, &
                                   nx_loc, x0_loc, dx_loc, rx_scl, &
                                   ny_loc, y0_loc, dy_loc, ry_scl, &
                                   nz_sur, z0_sur, dz_sur, &
                                   i_err &
                                 )
    !
    ! compute the sur top and bottom source z indices
    ! pes are in sync and do the same things
    !
    type ( surfacegrid_struct ),    pointer :: sur   ! surfacegrid structure
    logical,                  intent(in   ) :: l0_print      ! print flag
    integer,                  intent(in   ) :: nx_loc        ! loc x num values
    real,                     intent(in   ) :: x0_loc        ! loc x min value
    real,                     intent(in   ) :: dx_loc        ! loc x inc value
    real,                     intent(in   ) :: rx_scl        ! loc x scl
    integer,                  intent(in   ) :: ny_loc        ! loc y num values
    real,                     intent(in   ) :: y0_loc        ! loc y min value
    real,                     intent(in   ) :: dy_loc        ! loc y inc value
    real,                     intent(in   ) :: ry_scl        ! loc y scl
    integer,                  intent(in   ) :: nz_sur        ! sur z num values
    real,                     intent(in   ) :: z0_sur        ! sur z min value
    real,                     intent(in   ) :: dz_sur        ! sur z in value
    integer,                  intent(  out) :: i_err
    !
    real                                    :: rz_tmp ( nx_loc, ny_loc )
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    sur%lz_sur = .true.        ! sur z flag
    sur%nz_sur = nz_sur        ! sur z num values
    sur%z0_sur = z0_sur        ! sur z min value
    sur%dz_sur = dz_sur        ! sur z in value
    sur%z1_sur =  ( sur%nz_sur - 1 ) * sur%dz_sur + sur%z0_sur ! sur z max value
    !
    sur%nx_loc = nx_loc        ! loc x num values
    sur%x0_loc = x0_loc        ! loc x min value
    sur%dx_loc = dx_loc        ! loc x inc value
    sur%x1_loc =  ( sur%nx_loc - 1 ) * sur%dx_loc + sur%x0_loc ! loc x max value
    sur%rx_scl = rx_scl        ! loc x scl
    !
    sur%ny_loc = ny_loc        ! loc y num values
    sur%y0_loc = y0_loc        ! loc y min value
    sur%dy_loc = dy_loc        ! loc y inc value
    sur%y1_loc =  ( sur%ny_loc - 1 ) * sur%dy_loc + sur%y0_loc ! loc ny max value
    sur%ry_scl = ry_scl        ! loc y scl
    !
    sur%jz_sur_1 = 1 ! top rec datum index
    !
    sur%jz_sur_2 = 1 ! bot rec datum index
    !
    xxif_constant : if ( sur%opt_surface_cons ) then
      !
      call memfun_all ( sur%jz_sur , 1,        1,        'jz_sur', i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
      !  get the sur depth from the sur horizon
      !
      rz_tmp ( :, : ) = sur%rz_sur ( 1, 1 ) 
      !
      sur%jz_sur ( 1, 1 ) = &
      nint ( ( rz_tmp ( 1, 1 ) - sur%z0_sur ) / sur%dz_sur ) + 1
      !
    else xxif_constant 
      !
      call memfun_all ( sur%jz_sur, sur%nx_loc, sur%ny_loc, 'jz_sur', i_err )
      !
      !  if ( i_err .ne. 0 ) go to 998
      !
      call interpolate_2d_to_2d ( &
                    sur%nx_sur, sur%x0_sur, sur%dx_sur, &
                    sur%ny_sur, sur%y0_sur, sur%dy_sur, &
                    sur%rz_sur, &
                    sur%nx_loc, sur%x0_loc/sur%rx_scl, sur%dx_loc/sur%rx_scl, &
                    sur%ny_loc, sur%y0_loc/sur%rx_scl, sur%dy_loc/sur%rx_scl, &
                    rz_tmp &
                                )
      !
       sur%jz_sur ( 1:sur%nx_loc, 1:sur%ny_loc ) = &
  nint ( ( rz_tmp ( 1:sur%nx_loc, 1:sur%ny_loc ) - sur%z0_sur ) / sur%dz_sur ) + 1
      !
    end if xxif_constant 
    !
    sur%jz_sur_1 = minval ( sur%jz_sur ) 
    !
    sur%jz_sur_2 = maxval ( sur%jz_sur ) 
    !
    sur%rz_sur_1 = ( sur%jz_sur_1 - 1 ) * sur%dz_sur + sur%z0_sur
    !
    sur%rz_sur_2 = ( sur%jz_sur_2 - 1 ) * sur%dz_sur + sur%z0_sur
    !
    sur%nz_sur = sur%jz_sur_2 - sur%jz_sur_1 + 1 ! num rec datum indices
    !
    if ( l0_print ) &
    call surfacegrid_print ( sur, 'surface_grid_compute' )
    !
    return
    !
998 continue
    !
    print'( &
    & /, "error in surface_grid_compute p=",i4, &
    & /," during memory allocation " &
    & /," nx_loc=",i8, &
    & /," ny_loc=",i8 &
    & )', &
    pcpsx_i_pel(), sur%nx_loc, sur%ny_loc
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /, "error in surface_grid_compute p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine surfacegrid_compute
  !
  integer function surfacegrid_index ( sur, jx_loc, jy_loc )
    !
    ! return the sur z index
    !
    type ( surfacegrid_struct ),    pointer :: sur   ! surfacegrid structure
    integer,                  intent(in   ) :: jx_loc
    integer,                  intent(in   ) :: jy_loc
    !
    integer                                 :: jz_sur
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    xxif_constant : if ( sur%opt_surface_cons ) then
      !
      jz_sur = sur%jz_sur ( 1, 1 ) 
      !
    else xxif_constant 
      !
      jz_sur = sur%jz_sur ( jx_loc, jy_loc ) 
      !
    end if xxif_constant 
    !
    surfacegrid_index = jz_sur
    !
    return
    !
  end function surfacegrid_index 
  !
  logical function surfacegrid_flag_n ( sur, jz_mig_1, jz_mig_2 )
    !
    ! determine if any x,y tig ndoes exist in this z range
    !
    type ( surfacegrid_struct ),    pointer :: sur   ! surfacegrid structure
    integer,                  intent(in   ) :: jz_mig_1
    integer,                  intent(in   ) :: jz_mig_2
    !
    integer                                 :: jx_loc
    integer                                 :: jy_loc
    logical                                 :: l0_tig
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    l0_tig = .false.
    !
    ! do nothing if the mig depth range falls outside the tig depth range
    !
    if ( .not. sur%lz_sur &
    .or. min ( jz_mig_1, jz_mig_2 ) .gt. sur%jz_sur_2 &
    .or. max ( jz_mig_1, jz_mig_2 ) .lt. sur%jz_sur_1 ) go to 1
    !
    do_jy_loc : do jy_loc = 1 , sur%ny_loc
      !
      ! cycle over x locations
      !
      do_jx_loc : do jx_loc = 1 , sur%nx_loc
        !
        ! if this is the correct datum level copy the tig value
        !
        l0_tig = surfacegrid_flag_1 ( sur, jz_mig_1, jz_mig_2, jx_loc, jy_loc )
        !
        if ( l0_tig ) go to 1
        !
      end do do_jx_loc 
      !
    end do do_jy_loc 
    !
  1 continue
    !
    surfacegrid_flag_n = l0_tig 
    !
    return
    !
  end function surfacegrid_flag_n 
  !
  logical function surfacegrid_flag_1 ( &
    sur, jz_mig_1, jz_mig_2, jx_loc, jy_loc )
    !
    ! determine if any x,y tig ndoes exist in this z range
    !
    type ( surfacegrid_struct ),    pointer :: sur   ! surfacegrid structure
    integer,                  intent(in   ) :: jz_mig_1
    integer,                  intent(in   ) :: jz_mig_2
    integer,                  intent(in   ) :: jx_loc
    integer,                  intent(in   ) :: jy_loc
    !
    integer                                 :: jz_loc
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    jz_loc = surfacegrid_index ( sur, jx_loc, jy_loc )
    !
    surfacegrid_flag_1 = min ( jz_mig_1, jz_mig_2 ) .le. jz_loc &
                   .and. max ( jz_mig_1, jz_mig_2 ) .ge. jz_loc 
    !
    return
    !
  end function surfacegrid_flag_1 
  !
end module surfacegrid_module
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
