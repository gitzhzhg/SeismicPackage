!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- datumgrid.f90 ------------------------------!!
!!------------------------------- datumgrid.f90 ------------------------------!!
!!------------------------------- datumgrid.f90 ------------------------------!!


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
! Name       : datumgrid     (read DATUM file onto GRID.)
! Category   : io
! Written    : 2001-06-21   by: Douglas Hanson
! Revised    : 2007-05-31   by: Douglas Hanson Remove print.
! Maturity   : beta
! Purpose    : Read a datum file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!  Read datum files.
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
!  return the grid characteristics of a datum file
!  if  path_datum = pathcheck_empty 
!  nx_dtm, x0_dtm, dx_dtm will be set to 1, 0., 1. respectively.
!  ny_dtm, y0_dtm, dy_dtm will be set to 1, 0., 1. respectively.
!
!  call datumgrid_size ( &
!                        i           
!                        path_datum, &
!                        o       o       o       o
!                        hx_dtm, nx_dtm, x0_dtm, dx_dtm, &
!                        o       o       o       o
!                        hy_dtm, ny_dtm, y0_dtm, dy_dtm, &
!                        o
!                        i_err &
!                      )
!
!  read a datum file into datum grid rz_dtm
!
!  if alloc_mem = .true. datumgrid will allocate rz_dtm as a 3d array with 
!  dimensions nx_dtm, ny_dtm, nz_dtm.
!
!  if alloc_mem = .fals. datumgrid assumes rz_dtm has already been allocated
!
!  datumgrid outputs a gridded datum array rz_dtm (:, :, jz_dtm )
!
!  if path_datum is pathcheck_empty these datumv alues are the constant value 
!  const_datum.
!
!  if path_datum is not pathcheck_empty these datum values are interpolated 
!  from those in the file.
!
!  if file_head = .true. and path_datum is not pathcehck_empty
!  datumgrid will set the output x,y header words, hx_dtm, hy_dtm,
!  to the file header words, hx_fil, hy_fil respectively.
!
!  if file_head = .true. and path_datum is pathcehck_empty
!  datumgrid will set the output x,y header words, hx_dtm, hy_dtm,
!  to 7 and 8 respecitvely
!
!  if file_head = .false. datumgrid will output the datum grid 
!  useing the header word values and order defined by hx_dtm, hy_dtm
!
!  if file_grid = .true. and path_datum is not pathcehck_empty
!
!  nx_dtm, x0_dtm, dx_dtm will be set to the file grid values 
!  multiplied by rx_scl
!  ny_dtm, y0_dtm, dy_dtm will be set to the file grid values 
!  multiplied by ry_scl
!
!  and datumgrid will output the datum grid useing the grid defined by the file
!
!  if file_grid = .true. and path_datum is pathcehck_empty
!
!  nx_dtm, x0_dtm, dx_dtm will be set to 1, 0., rx_scl respectively.
!  ny_dtm, y0_dtm, dy_dtm will be set to 1, 0., ry_scl respectively.
!
!  if file_grid = .false. datumgrid will output the datum grid 
!  useing the grid defined by the input values of nx_dtm etc.
!
!  if path_datum is pathcheck_empty surfacegrid_read sets all datum values on the 
!  output grid equal to the constant datum value const_datum
!
!  if path_datum is not pathcheck_empty surfacegrid_read reads in the file datum 
!  values on the orignal file grid and lineraly interpolates these to the 
!  output grid.
!
!  if path_datum is not pathcheck_empty 
!  the file   x,y datum header words, hx_fil and hy_fil, 
!  must be consistent with
!  the output x,y datum header words, hx_dtm and hy_dtm.
!
!  That is: hx_fil=hx_dtm and hy_fil=hy_dtm
!       or: hx_fil=hy_dtm and hy_fil=hx_dtm
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
! 18  2007-05-31  Douglas Hanson Remove print.
! 17  2007-05-29  Douglas Hanson Fix file name length.
! 16  2007-04-12  Douglas Hanson Surfacegrid changes.
! 15  2007-02-08  Douglas Hanson Fix rz_fil mem_all.
! 14  2007-02-01  Douglas Hanson Change defaults.
! 13  2006-10-17  Douglas Hanson Modify prints.
! 12  2006-09-21  Douglas Hanson Add logical flags.
! 11  2006-08-29  D. Glover      Added NULLIFY statements for Intel compiler.
! 10  2006-03-30  Douglas Hanson Remove NS
!  9  2006-03-28  Douglas Hanson Add datumgrid frontend.
!  8  2006-01-10  B. Menger      Removed Unused Variables.
!  7  2002-02-04  Karen Goodger  Rename labels beginning with if to get around
!                                intel compiler bug.
!  6  2001-08-22  Douglas Hanson Fix parrallel error bug.
!  5  2001-08-13  Douglas Hanson Add parrallel read.
!  4  2001-07-13  Douglas Hanson Remove scaling.
!  3  2001-07-12  Douglas Hanson Fix hw 17 bug.
!  2  2001-07-02  Douglas Hanson Set grid defaults.
!  1  2001-06-21  Douglas Hanson Original from kmig.
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
! OPT_DATUM=`CC
! OPT_GAT_DATUM~~=`CCCCCCCCCC      OPT_TIG_DATUM~~=`CCCCCCCCCC
! CONST_GAT_DATUM=`FFFFFFFFFFF     CONST_TIG_DATUM=`FFFFFFFFFFF
! Select PATH_GAT_DATUM [PATH_GAT_DATUM]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! Select PATH_TIG_DATUM [PATH_TIG_DATUM]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!`------------------------------------------------------------------------------
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="SELECT_PATH_GAT_DATUM">
!<Tip> gat_datum path name. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATH_TIG_DATUM">
!<Tip> tig_datum path name. </Tip>
!</Help>
!
!<Help KEYWORD="OPT_DATUM">
!<Tip> Whether datum parameters are set. </Tip>
! Default = NO
! Allowed = YES Datum parameters are enabled.
! Allowed = NO  Datum parameters are disabled. 
!</Help>
!
!<Help KEYWORD="OPT_GAT_DATUM">
!<Tip> Whether to use constant or spatialy varying GAT_DATUM. </Tip>
! Default = CONSTANT  
! Allowed = CONSTANT  Use a constant value of CONST_GAT_DATUM.
! Allowed = PATH      Use a spatialy varying defined by PATH_GAT_DATUM.
!</Help>
!
!<Help KEYWORD="OPT_TIG_DATUM">
!<Tip> Whether to use constant or spatialy varying TIG_DATUM. </Tip>
! Default = CONSTANT  
! Allowed = CONSTANT  Use a constant value of CONST_TIG_DATUM.
! Allowed = PATH      Use a spatialy varying defined by PATH_TIG_DATUM.
!</Help>
!
!<Help KEYWORD="CONST_GAT_DATUM">
!<Tip> Gather constant datum level depth. </Tip>
! Default = 0
! Allowed = real
! Not used for time migration and DMO.
! If the gather elevations vary, they should be in an ascci data file
! - CONST_GAT_DATUM is other than NONE the Z values in it will be used as the
! gather datum level.
! CONST_GAT_DATUM should then be set to an average gather depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!<Help KEYWORD="CONST_TIG_DATUM">
!<Tip> Trace in gather constant datum level depth. </Tip>
! Default = 0
! Allowed = real
! Not used for time migration and DMO.
! If the trace in gather elevations vary, they should be in an ascci data file
! - CONST_GAT_DATUM is other than NONE the Z values in it will be used as the
! trace in gather datum level.
! CONST_TIG_DATUM should then be set to an average trace in gather depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!<Help KEYWORD="CONST_GAT_DATUM">
!<Tip> Gather datum file. </Tip>
! Default = NONE
! Allowed = character
! Not used for time migration and DMO.
! If CONST_GAT_DATUM is other than NONE the Z values in it will be used as the
! TIG datum level.
! CONST_GAT_DATUM should then be set to an average gather depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!<Help KEYWORD="PATH_GAT_DATUM">
!<Tip> Gather datum file. </Tip>
! Default = NONE
! Allowed = character
! Not used for time migration and DMO.
! If PATH_GAT_DATUM is other than NONE the Z values in it will be used as the
! gather datum level.
! CONST_GAT_DATUM should then be set to an average gather depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!<Help KEYWORD="PATH_TIG_DATUM">
!<Tip> TIG datum file. </Tip>
! Default = NONE
! Allowed = character
! Not used for time migration and DMO.
! If PATH_TIG_DATUM is other than NONE the Z values in it will be used as the
! trace in gather datum level.
! CONST_TIG_DATUM should then be set to an average trace in gather depth.
! It will be used for setting the aliasing condition.
!</Help>
!
!</HelpSection>
!
module datumgrid_module
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
  use surfacegrid_module
  !
  implicit none
  !
  private
  public :: datumgrid_create
  public :: datumgrid_delete
  !public :: datumgrid_nullify
  public :: datumgrid_initialize
  public :: datumgrid_get 
  public :: datumgrid_put 
  public :: datumgrid_verify 
  public :: datumgrid_visible 
  public :: datumgrid_sensitive 
  public :: datumgrid_depth
  public :: datumgrid_read
  !
  ! interfaces
  !
  ! functions
  !
  type, public :: datumgrid_struct
    !
    character(len=filename_length)     :: c_title ! title
    logical                            :: opt_datum
    type(pathchoose_struct),   pointer :: select_path_gat_datum 
    type(pathchoose_struct),   pointer :: select_path_tig_datum
    !
    type(surfacegrid_struct),  pointer :: s ! gat surface structure
    type(surfacegrid_struct),  pointer :: r ! tig surface structure
    !
    ! table datum depth definiton
    !
    integer                            :: hx_dtm   ! x hdr word
    integer                            :: hy_dtm   ! y hdr word
    integer                            :: hz_gat   ! gat z hdr
    integer                            :: hz_tig   ! tig z hdr
    !
    real                               :: rz_gat_0 ! input gather elevation (z)
    real                               :: rz_tig_0 ! input traces elevation (z)
    !
    logical                            :: source_gather
    logical                            :: receiver_gather
    !
  end type datumgrid_struct
  !
  integer,    parameter :: n_opt_const = 2
  character(len=8),save :: c_opt_const(n_opt_const) &
  = (/ 'CONSTANT', 'PATH    ' /)
  !
  ! rcs identifier string
  character(len=100),public,save :: datumgrid_IDENT = &
    '$Id: datumgrid.f90,v 1.18 2007/06/04 15:13:46 Hanson beta sps $'
  !
  contains
  !
  subroutine datumgrid_create ( dtm, c_title, i_err )
    !
    ! create the datumgrid structure
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    character(len=*),    intent(in   ) :: c_title ! title
    integer,             intent(inout) :: i_err   ! err 0=o.k. <0=err
    !
    !print'(" top datumgrid_create ",a)', trim(c_title)
    !
    i_err = 0
    !
    allocate ( dtm )
    !
    nullify (dtm%select_path_gat_datum) !jpa
    nullify (dtm%select_path_tig_datum) !jpa
    nullify (dtm%s) !jpa
    nullify (dtm%r) !jpa
    !
    call surfacegrid_create ( dtm%s, 'datumgrid_create_s', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call surfacegrid_create ( dtm%r, 'datumgrid_create_r', i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
call pathchoose_create ( dtm%select_path_gat_datum, 'path_gat_datum', '*' )
call pathchoose_create ( dtm%select_path_tig_datum, 'path_tig_datum', '*' )
    !
    dtm%c_title = c_title ! title
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in datumgrid_create ", &
    & /, " during datumgrid_create r " &
    & )') 
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in datumgrid_create ", &
    & /, " during datumgrid_create r " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in datumgrid_create " &
    & )') 
    !
    i_err = -1
    !
    return
    !
  end subroutine datumgrid_create 
  !
  subroutine datumgrid_delete ( dtm )
    !
    !  delete the datumgrid posiiton structure
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    ! delete the select strucutres
    !
           if ( associated ( dtm%select_path_gat_datum ) ) & 
    call pathchoose_delete ( dtm%select_path_gat_datum )
           if ( associated ( dtm%select_path_tig_datum ) ) & 
    call pathchoose_delete ( dtm%select_path_tig_datum )
    !
    call surfacegrid_delete ( dtm%s )
    call surfacegrid_delete ( dtm%r )
    !
    if ( associated ( dtm ) ) &
         deallocate ( dtm )
    !
    return
    !
  end subroutine datumgrid_delete
  !
!  subroutine datumgrid_nullify ( dtm )
!    !
!    !  nullify the datumgrid posiiton structure
!    !
!    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
!    !
!    if ( .not. associated ( dtm ) ) return
!    !
!    nullify ( dtm%select_path_gat_datum )
!    nullify ( dtm%select_path_tig_datum )
!    !
!    return
!    !
!  end subroutine datumgrid_nullify
  !
  subroutine datumgrid_initialize ( dtm )
    !
    !  initialize the datumgrid posiiton structure
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    integer                            :: i_err
    !
    i_err = 0
    !
    if ( .not. associated ( dtm ) ) return
    !
    !  nullify the datumgrid posiiton structure
    !
    !-wmm 9/21/2009 call datumgrid_nullify ( dtm )
    !
    call memfun_init ( dtm%hx_dtm   ) ! x hdr word
    call memfun_init ( dtm%hy_dtm   ) ! y hdr word
    call memfun_init ( dtm%hz_gat   ) ! gat z hdr
    call memfun_init ( dtm%hz_tig   ) ! tig z hdr
    call memfun_init ( dtm%rz_gat_0 ) ! input gather elevation (z)
    call memfun_init ( dtm%rz_tig_0 ) ! input traces elevation (z)
    call memfun_init ( dtm%source_gather ) 
    call memfun_init ( dtm%receiver_gather ) 
    call memfun_init ( dtm%opt_datum       ) 
    call memfun_init ( dtm%c_title         ) 
    !
    dtm%opt_datum         = .false.
    dtm%c_title           = 'NONE'
    !
    return
    !
  end subroutine datumgrid_initialize
  !
  subroutine datumgrid_get ( dtm )
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    if ( pathchoose_update ( dtm%select_path_gat_datum, &
                             dtm%s%path_surface ) ) &
    return
    !
    if ( pathchoose_update ( dtm%select_path_tig_datum, &
                             dtm%r%path_surface ) ) &
    return
    !
    call pc_get ( 'opt_src_datum',     dtm%s%opt_surface     )
    call pc_get ( 'opt_rec_datum',     dtm%r%opt_surface     )
    call pc_get ( 'path_src_datum',    dtm%s%path_surface    )
    call pc_get ( 'path_rec_datum',    dtm%r%path_surface    )
    call pc_get ( 'const_src_datum',   dtm%s%const_surface   )
    call pc_get ( 'const_rec_datum',   dtm%r%const_surface   )
    !
    call pc_get ( 'opt_datum',         dtm%opt_datum         )
    call pc_get ( 'opt_gat_datum',     dtm%s%opt_surface     )
    call pc_get ( 'opt_tig_datum',     dtm%r%opt_surface     )
    call pc_get ( 'path_gat_datum',    dtm%s%path_surface    )
    call pc_get ( 'path_tig_datum',    dtm%r%path_surface    )
    call pc_get ( 'const_gat_datum',   dtm%s%const_surface   )
    call pc_get ( 'const_tig_datum',   dtm%r%const_surface   )
    !
    return
    !
  end subroutine datumgrid_get
  !
  subroutine datumgrid_put ( dtm )
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    ! put parameters
    !
    call amod_line_feed ( 'datumgrid_datum' )
    call pc_put ( 'opt_datum',         dtm%opt_datum         )
    !
    call amod_line_feed ( 'datumgrid_gat_datum' )
    call pc_put ( 'opt_gat_datum',     dtm%s%opt_surface     )
    call pc_put ( 'const_gat_datum',   dtm%s%const_surface   )
    call pc_put ( 'path_gat_datum',    dtm%s%path_surface    )
    !
    call amod_line_feed ( 'datumgrid_tig_datum' )
    call pc_put ( 'opt_tig_datum',     dtm%r%opt_surface     )
    call pc_put ( 'const_tig_datum',   dtm%r%const_surface   )
    call pc_put ( 'path_tig_datum',    dtm%r%path_surface    )
    !
    return
    !
  end subroutine datumgrid_put
  !
  subroutine datumgrid_verify ( dtm )
    !
    ! verify parameters
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    call surfacegrid_set_constant_path ( dtm%s%opt_surface      )
    call surfacegrid_set_constant_path ( dtm%r%opt_surface      )
    !
    if ( .not. pc_do_not_process_traces() &
    .and. string_upper_compare ( dtm%s%opt_surface, 'CONSTANT' ) ) &
                                 dtm%s%path_surface = pathcheck_empty
    !
    if ( .not. pc_do_not_process_traces() &
    .and. string_upper_compare ( dtm%r%opt_surface, 'CONSTANT' ) ) &
                                 dtm%r%path_surface = pathcheck_empty
    !
    dtm%s%opt_surface_cons = &
    string_upper_compare ( dtm%s%opt_surface, 'CONSTANT' ) 
    !
    dtm%r%opt_surface_cons = &
    string_upper_compare ( dtm%r%opt_surface, 'CONSTANT' ) 
    !
    dtm%s%opt_surface_path = .not. dtm%s%opt_surface_cons 
    dtm%r%opt_surface_path = .not. dtm%r%opt_surface_cons 
    !
    call datumgrid_sensitive ( dtm )
    !
    return
    !
  end subroutine datumgrid_verify
  !
  subroutine datumgrid_sensitive ( dtm )
    !
    ! put options
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    call pc_put_options_field ('opt_datum',     c_opt_const,    n_opt_const    )
    call pc_put_options_field ('opt_gat_datum', c_opt_const,    n_opt_const    )
    call pc_put_options_field ('opt_tig_datum', c_opt_const,    n_opt_const    )
    !
    return
    !
  end subroutine datumgrid_sensitive 
  !
  subroutine datumgrid_visible ( dtm )
    !
    ! set parameter visibility
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    logical                            :: ls0
    logical                            :: lsc
    logical                            :: lsp
    logical                            :: lr0
    logical                            :: lrc
    logical                            :: lrp
    !
    call datumgrid_vis_sen ( 'opt_gat_datum',         .true.,  .true.  )
    call datumgrid_vis_sen ( 'const_gat_datum',       .true.,  .true.  )
    call datumgrid_vis_sen ( 'select_path_gat_datum', .true.,  .true.  )
    call datumgrid_vis_sen ( 'path_gat_datum',        .true.,  .true.  )
    call datumgrid_vis_sen ( 'opt_tig_datum',         .true.,  .true.  )
    call datumgrid_vis_sen ( 'const_tig_datum',       .true.,  .true.  )
    call datumgrid_vis_sen ( 'select_path_tig_datum', .true.,  .true.  )
    call datumgrid_vis_sen ( 'path_tig_datum',        .true.,  .true.  )
    !
    call datumgrid_con_sen ( &
dtm%s%opt_surface, 'const_gat_datum', 'path_gat_datum', 'select_path_gat_datum')
    !
    call datumgrid_con_sen ( &
dtm%r%opt_surface, 'const_tig_datum', 'path_tig_datum', 'select_path_tig_datum')
    !
    ls0 = dtm%opt_datum 
    !
    lsc = ls0
    !
    lsp = ls0
    !
    if ( .not. string_upper_compare ( dtm%s%opt_surface, 'CONSTANT' ) ) &
    lsc = .false.
    !
    if ( .not. string_upper_compare ( dtm%s%opt_surface, 'PATH'     ) ) &
    lsp = .false.
    !
    lr0 = dtm%opt_datum 
    !
    lrc = lr0
    !
    lrp = lr0
    !
    if ( .not. string_upper_compare ( dtm%r%opt_surface, 'CONSTANT' ) ) &
    lrc = .false.
    !
    if ( .not. string_upper_compare ( dtm%r%opt_surface, 'PATH'     ) ) &
    lrp = .false.
    !
    call pc_put_sensitive_field_flag ( 'opt_datum',             .true. )
    call pc_put_sensitive_field_flag ( 'opt_gat_datum',         ls0    )
    call pc_put_sensitive_field_flag ( 'const_gat_datum',       lsc    )
    call pc_put_sensitive_field_flag ( 'select_path_gat_datum', lsp    )
    call pc_put_sensitive_field_flag ( 'path_gat_datum',        lsp    )
    call pc_put_sensitive_field_flag ( 'opt_tig_datum',         lr0    )
    call pc_put_sensitive_field_flag ( 'const_tig_datum',       lrc    )
    call pc_put_sensitive_field_flag ( 'select_path_tig_datum', lrp    )
    call pc_put_sensitive_field_flag ( 'path_tig_datum',        lrp    )
    !
    return
    !
  end subroutine datumgrid_visible 
  !
  subroutine datumgrid_vis_sen ( par_0, vis_0, sen_0 )
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
  end subroutine datumgrid_vis_sen 
  !
  subroutine datumgrid_con_sen ( par_0, par_1, par_2, par_3 )
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
  end subroutine datumgrid_con_sen 
  !
  subroutine datumgrid_depth ( &
                               dtm, &
                               rx_gat, ry_gat, rz_gat, &
                               rx_tig, ry_tig, rz_tig &
                             )
    !
    !  get the trace gat and tig depths
    !
    type ( datumgrid_struct ), pointer :: dtm   ! datumgrid structure
    !
    real,                intent(in   ) :: rx_gat ! gat x location
    real,                intent(in   ) :: ry_gat ! gat y location
    real,                intent(inout) :: rz_gat ! gat z location
    !
    real,                intent(in   ) :: rx_tig ! tig x location
    real,                intent(in   ) :: ry_tig ! tig y location
    real,                intent(inout) :: rz_tig ! tig z location
    !
    !  get the gat depth from the gat datum horizon
    !
    call surfacegrid_depth ( dtm%s, rx_gat, ry_gat, rz_gat )
    !
    !  get the tig depth from the tig datum horizon
    !
    call surfacegrid_depth ( dtm%r, rx_tig, ry_tig, rz_tig )
    !
    return
    !
  end subroutine datumgrid_depth 
  !
  subroutine datumgrid_read ( &
    dtm, i_pel, broadcast, source_gather, hx_dtm0, hy_dtm0, i_err )
    !
    type ( datumgrid_struct ), pointer :: dtm   ! surfacegrid structure
    !
    integer,             intent(in   ) :: i_pel
    logical,             intent(in   ) :: broadcast
    logical,             intent(in   ) :: source_gather ! source gather flag
    integer,             intent(in   ) :: hx_dtm0        ! x hdr word
    integer,             intent(in   ) :: hy_dtm0        ! y hdr word
    integer,             intent(inout) :: i_err         ! error 0 O.K. -1 err
    integer                            :: hx_dtm        ! x hdr word
    integer                            :: hy_dtm        ! y hdr word
    !
    hx_dtm = hx_dtm0
    hy_dtm = hy_dtm0
    !
    dtm%source_gather = source_gather
    dtm%receiver_gather = .not. dtm%source_gather 
    !
    dtm%hx_dtm = hx_dtm ! x hdr word
    dtm%hy_dtm = hy_dtm ! y hdr word
    !
    xxif_opt_datum : if ( .not. dtm%opt_datum ) then
      !
      dtm%s%opt_surface = 'CONSTANT'
      !
      dtm%s%const_surface= 0.
      !
      dtm%s%path_surface = pathcheck_empty
      !
      dtm%r%opt_surface = 'CONSTANT'
      !
      dtm%r%const_surface= 0.
      !
      dtm%r%path_surface = pathcheck_empty
      !
    end if xxif_opt_datum
    !
    xxif_gat_gather: &
    if ( dtm%source_gather ) then
      !
      dtm%hz_gat = hdr_source_elev
      !
      dtm%hz_tig = hdr_receiver_elev
      !
    else xxif_gat_gather
      !
      dtm%hz_gat = hdr_receiver_elev
      !
      dtm%hz_tig = hdr_source_elev
      !
    end if xxif_gat_gather
    !
    ! initialize the number of datum locations to 1
    ! surfacegrid_read will check this and change if need be.
    ! note x and y pos values are in original hx_dtm, hy_dtm units
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /, "  datumgrid revision: ", &
    & " 18  2007-05-31  Douglas Hanson Remove print. " &
    & ) ' )
    !
    ! initialize the number of datum locations to 1
    ! surfacegrid_read will check this and change if need be.
    ! note x and y pos values are in original hx_dtm, hy_dtm units
    !
    dtm%s%nx_sur = 0
    dtm%s%x0_sur = 0.
    dtm%s%dx_sur = 1.
    !
    dtm%s%ny_sur = 0
    dtm%s%y0_sur = 0.
    dtm%s%dy_sur = 1.
    !
    !  get the source and receiver datums
    !
    call surfacegrid_read ( &
                  i_pel, broadcast, &
                  dtm%s%opt_surface, dtm%s%path_surface, dtm%s%const_surface, &
                  hx_dtm, dtm%s%nx_sur, dtm%s%x0_sur, dtm%s%dx_sur, 1., &
                  hy_dtm, dtm%s%ny_sur, dtm%s%y0_sur, dtm%s%dy_sur, 1., &
                  dtm%s%rz_sur, &
                  .true., .false., .true., &
                  i_err &
                        )
    !
    if ( broadcast ) &
    call pcpsx_broadcast ( i_pel, i_err  )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! get the receiver datum
    !
    dtm%r%nx_sur = 0
    dtm%r%x0_sur = 0.
    dtm%r%dx_sur = 1.
    !
    dtm%r%ny_sur = 0
    dtm%r%y0_sur = 0.
    dtm%r%dy_sur = 1.
    !
    call surfacegrid_read ( &
                  i_pel, broadcast, &
                  dtm%r%opt_surface, dtm%r%path_surface, dtm%r%const_surface, &
                  hx_dtm, dtm%r%nx_sur, dtm%r%x0_sur, dtm%r%dx_sur, 1., &
                  hy_dtm, dtm%r%ny_sur, dtm%r%y0_sur, dtm%r%dy_sur, 1., &
                  dtm%r%rz_sur, &
                  .true., .false., .true., &
                  i_err &
                         )
    !
    if ( broadcast ) &
    call pcpsx_broadcast ( i_pel, i_err  )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    !  print the source and receiver datum levels
    !
    call surfacegrid_print ( &
                  'spike gat travel time datums', &
                  pc_get_lun(), &
                  dtm%s%opt_surface, dtm%s%path_surface, dtm%s%const_surface, &
                  dtm%r%opt_surface, dtm%r%path_surface, dtm%r%const_surface, &
                  hx_dtm, dtm%s%nx_sur, dtm%s%x0_sur, dtm%s%dx_sur, 1., &
                  hy_dtm, dtm%s%ny_sur, dtm%s%y0_sur, dtm%s%dy_sur, 1., &
                  1, &
                  dtm%s%rz_sur, &
                  dtm%s%rz_sur &
                        )
    !  print the source and receiver datum levels
    !
    call surfacegrid_print ( &
                  'spike tig travel time datums', &
                  pc_get_lun(), &
                  dtm%s%opt_surface, dtm%s%path_surface, dtm%s%const_surface, &
                  dtm%r%opt_surface, dtm%r%path_surface, dtm%r%const_surface, &
                  hx_dtm, dtm%r%nx_sur, dtm%r%x0_sur, dtm%r%dx_sur, 1., &
                  hy_dtm, dtm%r%ny_sur, dtm%r%y0_sur, dtm%r%dy_sur, 1., &
                  1, &
                  dtm%r%rz_sur, &
                  dtm%r%rz_sur &
                        )

    !
    !print'(" end datumgrid_read p=",i4)', pcpsx_i_pel()
    !
    return
    !
997 continue
    !
    print'(" error in datumgrid_read during tig surfacegrid_read " )'
    !
    go to 999
    !
998 continue
    !
    print'(" error in datumgrid_read during gat surfacegrid_read " )'
    !
    go to 999
    !
999 continue
    !
    print'(" error in datumgrid_read " )'
    !
    i_err = -1
    !
    return
    !
  end subroutine datumgrid_read 
  !
end module datumgrid_module
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
