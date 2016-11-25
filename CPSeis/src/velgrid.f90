!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velgrid.f90 --------------------------------!!
!!------------------------------- velgrid.f90 --------------------------------!!
!!------------------------------- velgrid.f90 --------------------------------!!


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
! Name       : VELGRID     (read VELocity functions onto GRID.)
! Category   : io
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2006-12-11   by: Douglas Hanson Interpolate changes.
! Maturity   : production
! Purpose    : Read a set of velocity functions onto a defined grid.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!  Read cps velocity functions into a gridded format.
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
!   i = intent(in   )    = value required upon INPUT.
!   o = intent(  out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!
! return the uniform grid which best describes the velocity file
! x,y are the fast and slow dimensions in the file
!
!                           i       i        i         o
!        call velgrid_size ( lu_out, c_title, vel_file, vel_type,             & 
!                           o       o        o          
!                           n_vel,  hx_vel, hy_vel,                           & 
!                           o       o        o          
!                           nx_vel, x0_vel, dx_vel,                           & 
!                           o       o        o          
!                           ny_vel, y0_vel, dy_vel,                           & 
!                           o       o        o         o
!                           nt_vel, t0_vel, dt_vel, i_err)
!
! return the uniform grid which best describes the velocity file
! x,y are the fast and slow dimensions defined by the user
! they must be the same pair as those in the file but can be transposed
! that is the file may be 7,8 and the user specify either 7,8 or 8,7
!
!                           i       i        i         o
!      call velgrid_size_1 ( lu_out, c_title, vel_file, vel_type,            &
!                           o       i       i        
!                           n_vel,  hx_vel, hy_vel,                         &
!                           o       o       o         
!                           nx_vel, x0_vel, dx_vel,                         &
!                           o       o       o         
!                           ny_vel, y0_vel, dy_vel,                         &
!                           o       o       o       o
!                           nt_vel, t0_vel, dt_vel, i_err)
!
! return the velocity or slowness field on the uniform grid defined by
! nx_vel, x0_vel, dx_vel
! ny_vel, y0_vel, dy_vel
! nt_vel, t0_vel, dt_vel
! This grid need not be the same as that of the file.
! hx_vel, hy_vel are the fast and slow dimensions defined by the user
! they must be the same pair as those in the file but can be transposed
! that is the file may be 7,8 and the user specify either 7,8 or 8,7
!
!                           i       i        i         
!        call velgrid_read ( lu_out, c_title, vel_file,                    &
!                           i       i        
!                           vel_type, vel_parm,                           &
!                           i       i
!                           hx_vel, hy_vel,                               &
!                           i       i       i
!                           nx_vel, x0_vel, dx_vel,                       &
!                           i       i       i
!                           ny_vel, y0_vel, dy_vel,                       &
!                           i       i       i
!                           nt_vel, t0_vel, dt_vel,                       &
!                           o       o
!                           v0_vel, i_err, vel_order )
!
! character(len=*)           c_title = title to print on output
! integer                    lu_out  = unit for printed output lu_out<0 no print
! character(len=*)           vel_file= velocity file name
! character(len=*)           vel_type= output velocity type VTRM, VTIN, VZIN
! character(len=*)           vel_parm= type of parameter returned 
!                                      VELOCITY, SLOWNESS
! integer                    n_vel    = total number of points in the velocity
! integer                    hx_vel   = fast velocity header word
! integer                    nx_vel   = number of fast velocity grid nodes
! real                       x0_vel   = minimum fast velocity grid nodes
! real                       dx_vel   = fast velocity grid node increment
! integer                    hy_vel   = slow velocity header word
! integer                    ny_vel   = number of slow velocity grid nodes
! real                       y0_vel   = minimum slow velocity grid nodes
! real                       dy_vel   = slow velocity grid node increment
! integer                    nt_vel   = number of time velocity grid nodes
! real                       t0_vel   = minimum time velocity grid nodes
! real                       dt_vel   = time velocity grid node increment
! real, pointer              v0_vel   = velocity grid (nt_vel, nx_vel, ny_vel)
! integer                    i_err    = error flag 0 = o.k. -1 = error
! character(len=*)           vel_order= output model order TXY and XYT
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
! 28  2006-12-11  Douglas Hanson Interpolate changes.
! 27. 2006-08-24  D. Glover      Added NULLIFY statements for Intel compiler.
! 26. 2006-06-12  R.S.Day        Use grid rather than survey coords for trc
!                                and segy file scans.
! 25. 2006-01-10  B. Menger      Removed Unused Variables.
! 24  2005-01-31  R.S.Day        Return an error if file type is UNKNOWN
!                                Fix problem with undefined scan_yhdr
! 23  2003-10-06  R.S.Day        fsize variable converted to double precision.
! 22  2003-07-17  R.S.Day        Added velgrid_paint functions. Group support.
! 21  2003-06-11  Douglas Hanson Add velgrid_size_xy.
! 20  2003-05-27  R.S.Day        Avoid mem alloc if possible(DH). 
!                                Recognize MODSPEC files. Added velgrid_paint
! 19  2003-04-22  R.S.Day        updated for change in modgrid_rddesc call
! 18  2002-08-26  R.S.Day        Changed velgrid_read so modgrid_rd uses a 
!                                smaller buffer and avoids an extra copy of a 
!                                3D model.
! 17  2002-04-18  R.S.Day        Fix memory deallocation bug       
! 16  2002-02-04  Stoeckley      Change argument name UNITS to DUNITS in call
!                                 to velio_scan_alloc.
! 15  2001-12-18  R.S.Day        Made doc and velgrid_read consistent. Changed
!                                intent of some args of velgrid_read_p.
! 14  2001-10-29  R.S.Day        Bypass velutil_convert if in=out
! 13  2001-10-25  Karen Goodger  Rename labels beginning with if to get around
!                                intel compiler bug.
! 12  2001-08-22  Douglas Hanson Fix parallel error bug.
! 11  2001-08-13  Douglas Hanson Add parrallel read.
! 10  2001-04-26  RSDay          argument of modgrid_get_data  call was changed.
!  9  2001-03-26  RSDay          removed some xml tags
!  8  2001-03-21  RSDay          Increased format specifier,i2 to i4.
!                                Set hx_inp, and hy_inp when modgrid is called.
!  7  2001-03-13  RSDay          End tag fix
!  6  2001-02-22  RSDay          Generalized for gridded models
!  5  2001-01-10  Douglas Hanson Add vel_order to velgrid_read
!  4  2000-09-06  Douglas Hanson Improve documentation
!  3  2000-08-25  Douglas Hanson cpsfcr
!  2  2000-06-16  Brad Kruse     Review for standards.
!  1  2000-05-01  Douglas Hanson Initial version.
!
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


module velgrid_module
  !
  ! - Module references
  !
  use getsys_module
  use interpolate_module
  use matfun_module
  use memfun_module
  use modgrid_module
  use named_constants_module
  use pathcheck_module
  use pc_module
  use pcpsx_module
  use string_module
  use trcio_module
  use velio_module
  use velutil_module
  !
  implicit none

  private
  public :: velgrid_size      ! get a velocity grid size
  public :: velgrid_size_xy   ! get a velocity grid size
  public :: velgrid_size_p    ! get a velocity grid size with defined x,y
  public :: velgrid_size_1    ! get a velocity grid size with defined x,y
  public :: velgrid_read_p    ! get a velocity grid
  public :: velgrid_read      ! get a velocity grid
  public :: velgrid_paint_by_file_par! paint output grid
  public :: velgrid_paint_by_file    ! paint output grid
  public :: velgrid_paint_by_obj_par
  public :: velgrid_paint_by_obj_gpar
  public :: velgrid_paint_by_obj
  public :: velgrid_create    ! create a velocity grid object
  public :: velgrid_delete    ! delete a velocity grid object
  public :: velgrid_init      ! iniitlaize a velocity grid object
  !
  type,  public :: velgrid_struct
    !
    private
    !
    integer                             :: n0_vel        ! num velocity planes
    !
    character(len=filename_length)      :: path_vel     ! velocity file
    real                                :: const_vel    ! constant velocity 
    !
    character(len=3)                    :: vel_order    ! velocity file order
    character(len=4)                    :: vel_type     ! inp  vel type
    character(len=4)                    :: fil_type     ! file vel type
    !
    logical                             :: use_const    ! use input constant vel
    logical                             :: use_scale    ! use input x,y scale
    logical                             :: use_head     ! use input header value
    logical                             :: use_grid     ! use input grid values
    !
    character(len=4)                    :: use_vel_type ! inp vel type
    character(len=4)                    :: out_vel_type ! out vel type
    character(len=8)                    :: vel_parm     ! vel parameter
    !
    integer                             :: hx_vel       ! image x head
    real                                :: sx_vel       ! image x scale
    integer                             :: nx_vel       ! num image x nodes
    real                                :: x0_vel       ! min image x nodes
    real                                :: dx_vel       ! inc image x nodes
    !
    integer                             :: hy_vel       ! image y head
    real                                :: sy_vel       ! image y scale
    integer                             :: ny_vel       ! num image y nodes
    real                                :: y0_vel       ! min image y nodes
    real                                :: dy_vel       ! inc image y nodes
    !
    integer                             :: nt_vel       ! num image t nodes
    real                                :: t0_vel       ! min image t nodes
    real                                :: dt_vel       ! inc image t nodes
    !
    integer                             :: nq_vel(3)    ! num image nodes
    real                                :: q0_vel(3)    ! min image nodes
    real                                :: dq_vel(3)    ! inc image nodes
    !
    real,                      pointer  :: s0_vel (:, :, : ) ! velocity
    !
  end type velgrid_struct
  !
  !type ( velgrid_struct ), pointer, save :: object      ! needed for traps.
  !
  character(len=100),public,save :: VELGRID_IDENT = &
    '$Id: velgrid.f90,v 1.28 2006/12/11 14:18:38 Hanson prod sps $'
  !
contains

  !!---------------------------- velgrid_size ------------------------------!!
  !!---------------------------- velgrid_size ------------------------------!!
  !!---------------------------- velgrid_size ------------------------------!!

  !
  ! - get the x,y,t sizes of a velocity grid
  !
  subroutine velgrid_size ( lu_out, c_title, vel_file, vel_type,               &
                           n_vel,  hx_vel, hy_vel,                            &
                           nx_vel, x0_vel, dx_vel,                            &
                           ny_vel, y0_vel, dy_vel,                            &
                           nt_vel, t0_vel, dt_vel, i_err)
    !
    ! - Arguments
    !
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: vel_file
    character (len = *), intent (inout) :: vel_type
    integer,             intent (in   ) :: lu_out
    integer,             intent (  out) :: hx_vel
    integer,             intent (  out) :: hy_vel
    integer,             intent (  out) :: i_err
    integer,             intent (  out) :: nt_vel
    integer,             intent (  out) :: n_vel
    integer,             intent (  out) :: nx_vel
    integer,             intent (  out) :: ny_vel
    real,                intent (  out) :: dt_vel
    real,                intent (  out) :: dx_vel
    real,                intent (  out) :: dy_vel
    real,                intent (  out) :: t0_vel
    real,                intent (  out) :: x0_vel
    real,                intent (  out) :: y0_vel
    !
    ! - Local variables
    !
    type(velio_struct), pointer :: velio_obj ! pointer to VELIO data structure
    !
    character (len = 80) :: crd80
    character (len = 80) :: dunits       ! units of velocity functions 
                                         !  (feet or meters).
    character (len = 80) :: attname      ! any attribute name given to the 
                                         !  ordinate (e.g. ve
    character (len = 80) :: attunits     ! attribute units of the ordinate 
                                         !  (e.g. feet/sec).
    character (len = 80) :: tdunits      ! time/depth units of the abscissa 
                                         !  (e.g. sec, feet,
    character (len = 80) :: encoding     ! encoding format of velocity file.
    character (len = 80) :: nilstring    ! string for nil values in the file 
                                         !  (default '-nil-
    character (len = 80) :: fields(8)    ! list of velocity function fields 
                                         !  to read or write
    character (len =  8) :: inp_name*8   ! input velocity function name
    character (len =  8) :: inp_type*8   ! input velocity function type
    !
    integer       :: nfields      ! number of velocity function fields 
                                        !  to read or write
    integer       :: maxpicks    ! maximum number of picks in a velocity funct
    integer       :: nx_bins     ! number of X coordinates in the x_bins array
    integer       :: ny_bins     ! number of Y coordinates in the y_bins array
    real, pointer :: x_cord(:)   ! pointer to X coordinates of all NFUN velocit
    real, pointer :: y_cord(:)   ! pointer to Y coordinates of all NFUN velocit
    real, pointer :: x_bins(:)   ! pointer to X coordinates of rectangular arr
    real, pointer :: y_bins(:)   ! pointer to Y coordinates of rectangular arr
    real, pointer :: t_bins(:)   ! pointer to T coordinates of rectangular arr
    real, pointer :: v_bins(:)   ! pointer to V coordinates of rectangular arr
    real          :: nmo_sign
    real          :: nmo_exp
    real          :: x_tmp,y_tmp
    integer       :: ix_bins, iy_bins, it_bins, nt_bins

!!START! RS DAY
    type(modgrid_struct),pointer    :: obj
    character(len=8) :: ftype   ! input velocity function type
    character(len=96):: dfile
    integer          ::       hdwd,rank 
    double precision :: fsize
    real             :: xangle

    character(len=32) :: lab1,lab2,lab3
    character(len=8)  :: xyz_order
    integer           :: scan_xhdr,scan_yhdr
!!/START! RS DAY

    nullify (velio_obj) ! jpa
    nullify (obj) ! jpa
    !
    ! - Begin velgrid_size
    !
    i_err = 0
    crd80 = vel_file

    call string_to_upper(crd80)
    !
    ! - initialize the values
    !
    xangle = 0.0
    n_vel  = 0
    nx_bins = 0
    hx_vel = 7
    nx_vel = 0
    x0_vel = 0.
    dx_vel = 1.
    ny_bins = 0
    hy_vel = 8
    ny_vel = 0
    y0_vel = 0.
    dy_vel = 1.
    nt_vel = 0
    t0_vel = 0.
    dt_vel = 1.
    maxpicks = 0
    nmo_sign = 0.
    nmo_exp = 0.
    dunits = ' '
    tdunits = ' '
!!START! RS DAY
! determine file type
    ftype =   modgrid_ftype(vel_file,lu_out,fsize)
    if(ftype=='UNKNONW') then
      write(lu_out,*) 'velgrid_size: error UNKNOWN type, vel_file=',&
      trim(vel_file)
      i_err = -1
      return
    endif
    if(ftype == 'TRCIO' .or. ftype=='HGRID' .or. &
       ftype == 'VOXET' .or. ftype=='MODSPEC' .or. &
       ftype == 'SEGY' ) then
      scan_xhdr= hx_vel  !HDR_MIDPOINT_XLOC
      scan_yhdr= hy_vel  !HDR_MIDPOINT_YLOC
      vel_type = ' '  !to force return of input type
      i_err = modgrid_rddesc_verbose(obj,vel_file,lu_out,dfile,ftype,rank,&
              lab1,hdwd  ,nt_vel,t0_vel,dt_vel, & 
              lab2,hx_vel,nx_vel,x0_vel,dx_vel, & 
              lab3,hy_vel,ny_vel,y0_vel,dy_vel, & 
              xyz_order, scan_xhdr,scan_yhdr,vel_type)
      if( i_err < 0) return
      call modgrid_print(obj,lu_out)
      xangle = modgrid_xangle(obj)
      if(xangle /= 0) then
        write(lu_out,*) 'velgrid_size: xangle = ',xangle
      endif
      n_vel = nx_vel*ny_vel
      call modgrid_delete(obj)
      i_err = 0
      return
    end if
!!/START! RS DAY
    !
    ! - nullify x,y info before velio_scan_alloc
    !
    nullify (x_cord)
    nullify (y_cord)
    nullify (x_bins)
    nullify (y_bins)
    !
    ! - if this is the correct pe read the info
    !
    if ( .not. string_upper_compare ( crd80, 'NONE' ) ) then
      !
      call velio_scan_alloc (filename  = vel_file,    &
                             nfun      = n_vel,       &
                             err       = i_err,       &
                             msg       = crd80,       &
                             nhx       = hx_vel,      &
                             nhy       = hy_vel,      &
                             nmosign   = nmo_sign,    &
                             nmoexp    = nmo_exp,     &
                             maxpicks  = maxpicks,    &
                             xcoords   = x_cord,      &
                             ycoords   = y_cord,      &
                             xbins     = x_bins,      &
                             ybins     = y_bins,      &
                             nxbins    = nx_bins,     &
                             nybins    = ny_bins,     &
                             dunits    = dunits,      &
                             attname   = attname,     &
                             attunits  = attunits,    &
                             tdunits   = tdunits,     &
                             encoding  = encoding,    &
                             fields    = fields,      &
                             nfields   = nfields,     &
                             nilstring = nilstring)
      !
      if ( i_err .ne. 0 ) go to 998
      !
      ! - if the velocity file is not uniformly sampled in x and y abort
      !
    validate_nx_n_ny_bins:    &
      if (nx_bins .le. 0 .or. ny_bins .le. 0 ) then

        go to 997

      else validate_nx_n_ny_bins
        !
        ! - determine the x dimension of the velocity grid
        !
        call matfun_array_size (nx    = nx_vel,    & 
                                x0    = x0_vel,    & 
                                dx    = dx_vel,    & 
                                n     = nx_bins,   &
                                x     = x_bins,    &
                                i_err = i_err)
        !
        if ( i_err .ne. 0 ) go to 996
        !
        ! - determine the x dimension of the velocity grid
        !
        call matfun_array_size (nx    = ny_vel,    & 
                                x0    = y0_vel,    & 
                                dx    = dy_vel,    & 
                                n     = ny_bins,   &
                                x     = y_bins,    &
                                i_err = i_err)
        !
      end if validate_nx_n_ny_bins
      !
      ! - allocate memory
      !
      allocate(t_bins(maxpicks), stat=i_err)
      if ( i_err .ne. 0 ) go to 992
      !
      allocate(v_bins(maxpicks), stat=i_err)
      if ( i_err .ne. 0 ) go to 992
      !
      ! - open the velocity file
      !
      call velio_open_read (obj      = velio_obj,    &
                            filename = vel_file,     &
                            nfun     = n_vel,        &
                            err      = i_err,        &
                            msg      = crd80,        &
                            nhx      = hx_vel,       &
                            nhy      = hy_vel)
      !
      ! - read the first velocity function
      !   read the function into t_inp, v_inp
      !
      call velio_read_velfun (obj     = velio_obj,    &
                              xcoord  = x_tmp,        &
                              ycoord  = y_tmp,        &
                              npicks  = nt_bins,      &
                              tpicks  = t_bins,       &
                              vpicks  = v_bins,       &
                              err     = i_err,        &
                              msg     = crd80,        &
                              velname = inp_name,     &
                              veltype = inp_type)
      !
      vel_type = inp_type
      if ( i_err .ne. 0 ) go to 994
      !
      ! - determine the x dimension of the velocity grid
      !
      call matfun_array_size (nx    = nt_vel,    & 
                              x0    = t0_vel,    & 
                              dx    = dt_vel,    & 
                              n     = nt_bins,   &
                              x     = t_bins,    &
                              i_err = i_err)
      !
      if ( i_err .ne. 0 ) go to 993
      !
    end if    ! if ( .not. string_upper_compare ( crd80, 'NONE' ) ) then
    !
    ! - come to here to deallocate memory and return
    !   this may be from above or below
    !
  1999 continue
    !
    if ( lu_out .ge. 0 ) then
      !
      write ( lu_out, '( &
     & /, " velgrid_size ", a, &
     & /, " vel_file=", a &
     & /, " dunits  =", a &
     & /, " tdunits =", a &
     & /, " nmo_sign=", g10.4," nmo_exp=", g10.4 &
     & /, " n_vel=", i8, " nx_bins=", i8, " ny_bins=", i8, &
     & " maxpicks=", i8, &
     & /, " hx_vel=", i8, " nx_vel=", i8, &
     & " x0_vel=", g10.4, " x1_vel=", g10.4, " dx_vel=", g10.4, &
     & /, " hy_vel=", i8, " ny_vel=", i8, &
     & " y0_vel=", g10.4, " y1_vel=", g10.4, " dy_vel=", g10.4, &
     & /, "        ", 8x, " nt_vel=", i8, &
     & " t0_vel=", g10.4, " t1_vel=", g10.4, " dt_vel=", g10.4 &
     & )') &
       trim(c_title), &
       trim(vel_file), &
       trim(dunits), &
       trim(tdunits), &
       nmo_sign, nmo_exp, &
       n_vel, nx_bins, ny_bins, maxpicks, &
       hx_vel, nx_vel ,x0_vel, (nx_vel-1)*dx_vel+x0_vel, dx_vel, &
       hy_vel, ny_vel ,y0_vel, (ny_vel-1)*dy_vel+y0_vel, dy_vel, &
               nt_vel ,t0_vel, (nt_vel-1)*dt_vel+t0_vel, dt_vel
      !
      write ( lu_out, '(/, " ix_bins  x_bins")')
      if (associated(x_bins)) &
     & write ( lu_out, '(1x, i8, 1x, g10.4)') &
     & (ix_bins, x_bins(ix_bins), ix_bins=1, nx_bins)
      !
      write ( lu_out, '(/, " iy_bins  y_bins")')
      if (associated(y_bins)) &
     & write ( lu_out, '(1x, i8, 1x, g10.4)') &
     & (iy_bins, y_bins(iy_bins), iy_bins=1, ny_bins)
      !
      write ( lu_out, '(/, " it_bins  t_bins v_bins ")')
      if (associated(t_bins)) &
     & write ( lu_out, '(1x, i8, 1x, g10.4, 1x, g10.4)') &
     & (it_bins, t_bins(it_bins), v_bins(it_bins), it_bins=1, nt_bins)
      !
    end if    ! if ( lu_out .ge. 0 ) then
!      if (nt_bins .ne. -999) stop
    !
    ! - close the velocity file
    !
    if (associated(velio_obj)) call velio_close (velio_obj)
!      if (nt_bins .ne. -999) stop
    !
    if (associated(x_cord)) deallocate (x_cord)
    if (associated(y_cord)) deallocate (y_cord)
    if (associated(x_bins)) deallocate (x_bins)
    if (associated(y_bins)) deallocate (y_bins)
    if (associated(t_bins)) deallocate (t_bins)
    if (associated(v_bins)) deallocate (v_bins)
!      if (nt_bins .ne. -999) stop
    !
    return
    !
  992 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size ",&
!     & /, " during memory allocate " &
!     & )') 
    call pc_error (msg1 = "Error in velgrid_size during memory allocate")
    call pc_error (msg1 = "Allocate status returned: ",    &
                   var1 = i_err)
    go to 999
    !
  993 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size ",&
!     & /, " during matfun_array_size t " &
!     & )') 
    call pc_error (msg1 = "Error in velgrid_size during matfun_array_size t.")
    call pc_error (msg1 = "matfun_array_size returned status: ",    &
                   var1 = i_err)
    go to 999
    !
  994 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size ",&
!     & /, " during matfun_array_size y " &
!     & )') 
    call pc_error (msg1 = "Error in velgrid_size during matfun_array_size y.")
    call pc_error (msg1 = "matfun_array_size returned status: ",    &
                   var1 = i_err)
    go to 999
    !
  996 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size ",&
!     & /, " during matfun_array_size x " &
!     & )') 
    call pc_error (msg1 = "Error in velgrid_size during matfun_array_size x.")
    call pc_error (msg1 = "matfun_array_size returned status: ",    &
                   var1 = i_err)
    go to 999
    !
  997 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size ",&
!     & /, " the velocity file must be uniformly sampled in x and y " &
!     & )') 

    call pc_error (msg1 = "Error in velgrid_size.")
    call pc_error (msg1 = "The velocity file must be uniformly "    &
                          // "sampled in x (",                      &
                   var1 = nx_bins,                                  &
                   msg2 = ") and y (",                              &
                   var2 = ny_bins)

    go to 999
    !
  998 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size ",&
!     & /, " during velio_scan_alloc" &
!     & /, " error=", a &
!     & )') 
!     & trim(crd80)
    call pc_error (msg1 = "Error in velgrid_size during velio_scan_alloc.")
    call pc_error (msg1 = "velio_scan_alloc returned status: ",    &
                   var1 = i_err,                                   &
                   msg2 = ", " // trim (crd80))
    go to 999
    !
  999 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size ",&
!     & /, " file =", a &
!     & )') 
!     & trim(vel_file)
    call pc_error (msg1 = "Error in velgrid_size.  File: ",    &
                   var1 = trim (vel_file))
    i_err = -1
    go to 1999
    !
  end subroutine velgrid_size
  subroutine velgrid_size_xy ( lu_out, c_title, vel_file, vel_type, &
                               n_vel,  hx_vel, hy_vel,              &
                               nx_vel, x0_vel, dx_vel,              &
                               ny_vel, y0_vel, dy_vel,              &
                               i_err)
    !
    ! - Arguments
    !
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: vel_file
    character (len = *), intent (inout) :: vel_type
    integer,             intent (in   ) :: lu_out
    integer,             intent (  out) :: hx_vel
    integer,             intent (  out) :: hy_vel
    integer,             intent (  out) :: i_err
    integer,             intent (  out) :: n_vel
    integer,             intent (  out) :: nx_vel
    integer,             intent (  out) :: ny_vel
    real,                intent (  out) :: dx_vel
    real,                intent (  out) :: dy_vel
    real,                intent (  out) :: x0_vel
    real,                intent (  out) :: y0_vel
    !
    ! - Local variables
    !
    type(velio_struct), pointer :: velio_obj ! pointer to VELIO data structure
    !
    character (len = 80) :: crd80
    character (len = 80) :: dunits       ! units of velocity functions 
                                         !  (feet or meters).
    character (len = 80) :: attname      ! any attribute name given to the 
                                         !  ordinate (e.g. ve
    character (len = 80) :: attunits     ! attribute units of the ordinate 
                                         !  (e.g. feet/sec).
    character (len = 80) :: tdunits      ! time/depth units of the abscissa 
                                         !  (e.g. sec, feet,
    character (len = 80) :: encoding     ! encoding format of velocity file.
    character (len = 80) :: nilstring    ! string for nil values in the file 
                                         !  (default '-nil-
    character (len = 80) :: fields(8)    ! list of velocity function fields 
                                         !  to read or write
    character (len =  8) :: inp_name*8   ! input velocity function name
    character (len =  8) :: inp_type*8   ! input velocity function type
    !
    integer       :: nfields      ! number of velocity function fields 
                                        !  to read or write
    integer       :: maxpicks    ! maximum number of picks in a velocity funct
    integer       :: nx_bins     ! number of X coordinates in the x_bins array
    integer       :: ny_bins     ! number of Y coordinates in the y_bins array
    real, pointer :: x_cord(:)   ! pointer to X coordinates of all NFUN velocit
    real, pointer :: y_cord(:)   ! pointer to Y coordinates of all NFUN velocit
    real, pointer :: x_bins(:)   ! pointer to X coordinates of rectangular arr
    real, pointer :: y_bins(:)   ! pointer to Y coordinates of rectangular arr
    real, pointer :: t_bins(:)   ! pointer to T coordinates of rectangular arr
    real, pointer :: v_bins(:)   ! pointer to V coordinates of rectangular arr
    real          :: nmo_sign
    real          :: nmo_exp
    real          :: x_tmp,y_tmp
    integer       :: ix_bins, iy_bins, it_bins, nt_bins

!!START! RS DAY
    type(modgrid_struct),pointer    :: obj
    character(len=8) :: ftype   ! input velocity function type
    character(len=96):: dfile 
    double precision :: fsize
    integer          ::       hdwd,rank 
    integer          :: nt_vel
    real             :: dt_vel,t0_vel
    character(len=32):: lab1,lab2,lab3
    character(len=8) :: xyz_order
    integer          :: scan_xhdr,scan_yhdr
!!/START! RS DAY

    nullify (velio_obj) ! jpa
    nullify (obj) ! jpa
    !
    ! - Begin velgrid_size_xy
    !
    i_err = 0
    crd80 = vel_file

    call string_to_upper(crd80)
    !
    ! - initialize the values
    !
    n_vel  = 0
    nx_bins = 0
    hx_vel = 7
    nx_vel = 0
    x0_vel = 0.
    dx_vel = 1.
    ny_bins = 0
    hy_vel = 8
    ny_vel = 0
    y0_vel = 0.
    dy_vel = 1.
    maxpicks = 0
    nmo_sign = 0.
    nmo_exp = 0.
    dunits = ' '
    tdunits = ' '
!!START! RS DAY
! determine file type
    ftype =   modgrid_ftype(vel_file,lu_out,fsize)
    if(ftype == 'TRCIO' .or. ftype=='HGRID' .or. &
       ftype == 'VOXET' .or. ftype=='MODSPEC' .or. &
       ftype == 'SEGY' ) then
      scan_xhdr= hx_vel !HDR_MIDPOINT_XLOC
      scan_yhdr= hy_vel ! HDR_MIDPOINT_YLOC
      vel_type = ' '  !to force return of input type
      i_err = modgrid_rddesc_verbose(obj,vel_file,lu_out,dfile,ftype,rank,&
              lab1,hdwd  ,nt_vel,t0_vel,dt_vel, & 
              lab2,hx_vel,nx_vel,x0_vel,dx_vel, & 
              lab3,hy_vel,ny_vel,y0_vel,dy_vel, & 
              xyz_order, scan_xhdr,scan_yhdr,vel_type)
      if( i_err < 0) return
      call modgrid_print(obj,lu_out)
      n_vel = nx_vel*ny_vel
      call modgrid_delete(obj)
      i_err = 0
      return
    end if
!!/START! RS DAY
    !
    ! - nullify x,y info before velio_scan_alloc
    !
    nullify (x_cord)
    nullify (y_cord)
    nullify (x_bins)
    nullify (y_bins)
    !
    ! - if this is the correct pe read the info
    !
    if ( .not. string_upper_compare ( crd80, 'NONE' ) ) then
      !
      call velio_scan_alloc (filename  = vel_file,    &
                             nfun      = n_vel,       &
                             err       = i_err,       &
                             msg       = crd80,       &
                             nhx       = hx_vel,      &
                             nhy       = hy_vel,      &
                             nmosign   = nmo_sign,    &
                             nmoexp    = nmo_exp,     &
                             maxpicks  = maxpicks,    &
                             xcoords   = x_cord,      &
                             ycoords   = y_cord,      &
                             xbins     = x_bins,      &
                             ybins     = y_bins,      &
                             nxbins    = nx_bins,     &
                             nybins    = ny_bins,     &
                             dunits    = dunits,      &
                             attname   = attname,     &
                             attunits  = attunits,    &
                             tdunits   = tdunits,     &
                             encoding  = encoding,    &
                             fields    = fields,      &
                             nfields   = nfields,     &
                             nilstring = nilstring)
      !
      if ( i_err .ne. 0 ) go to 998
      !
      ! - if the velocity file is not uniformly sampled in x and y abort
      !
    validate_nx_n_ny_bins:    &
      if (nx_bins .le. 0 .or. ny_bins .le. 0 ) then

        go to 997

      else validate_nx_n_ny_bins
        !
        ! - determine the x dimension of the velocity grid
        !
        call matfun_array_size (nx    = nx_vel,    & 
                                x0    = x0_vel,    & 
                                dx    = dx_vel,    & 
                                n     = nx_bins,   &
                                x     = x_bins,    &
                                i_err = i_err)
        !
        if ( i_err .ne. 0 ) go to 996
        !
        ! - determine the x dimension of the velocity grid
        !
        call matfun_array_size (nx    = ny_vel,    & 
                                x0    = y0_vel,    & 
                                dx    = dy_vel,    & 
                                n     = ny_bins,   &
                                x     = y_bins,    &
                                i_err = i_err)
        !
      end if validate_nx_n_ny_bins
      !
      ! - allocate memory
      !
      allocate(t_bins(maxpicks), stat=i_err)
      if ( i_err .ne. 0 ) go to 992
      !
      allocate(v_bins(maxpicks), stat=i_err)
      if ( i_err .ne. 0 ) go to 992
      !
      ! - open the velocity file
      !
      call velio_open_read (obj      = velio_obj,    &
                            filename = vel_file,     &
                            nfun     = n_vel,        &
                            err      = i_err,        &
                            msg      = crd80,        &
                            nhx      = hx_vel,       &
                            nhy      = hy_vel)
      !
      ! - read the first velocity function
      !   read the function into t_inp, v_inp
      !
      call velio_read_velfun (obj     = velio_obj,    &
                              xcoord  = x_tmp,        &
                              ycoord  = y_tmp,        &
                              npicks  = nt_bins,      &
                              tpicks  = t_bins,       &
                              vpicks  = v_bins,       &
                              err     = i_err,        &
                              msg     = crd80,        &
                              velname = inp_name,     &
                              veltype = inp_type)
      !
      vel_type = inp_type
      if ( i_err .ne. 0 ) go to 994
      !
    end if    ! if ( .not. string_upper_compare ( crd80, 'NONE' ) ) then
    !
    ! - come to here to deallocate memory and return
    !   this may be from above or below
    !
  1999 continue
    !
    if ( lu_out .ge. 0 ) then
      !
      write ( lu_out, '( &
     & /, " velgrid_size_xy ", a, &
     & /, " vel_file=", a &
     & /, " dunits  =", a &
     & /, " tdunits =", a &
     & /, " nmo_sign=", g10.4," nmo_exp=", g10.4 &
     & /, " n_vel=", i8, " nx_bins=", i8, " ny_bins=", i8, &
     & " maxpicks=", i8, &
     & /, " hx_vel=", i8, " nx_vel=", i8, &
     & " x0_vel=", g10.4, " x1_vel=", g10.4, " dx_vel=", g10.4, &
     & /, " hy_vel=", i8, " ny_vel=", i8, &
     & " y0_vel=", g10.4, " y1_vel=", g10.4, " dy_vel=", g10.4 &
     & )') &
       trim(c_title), &
       trim(vel_file), &
       trim(dunits), &
       trim(tdunits), &
       nmo_sign, nmo_exp, &
       n_vel, nx_bins, ny_bins, maxpicks, &
       hx_vel, nx_vel ,x0_vel, (nx_vel-1)*dx_vel+x0_vel, dx_vel, &
       hy_vel, ny_vel ,y0_vel, (ny_vel-1)*dy_vel+y0_vel, dy_vel
      !
      write ( lu_out, '(/, " ix_bins  x_bins")')
      if (associated(x_bins)) &
     & write ( lu_out, '(1x, i8, 1x, g10.4)') &
     & (ix_bins, x_bins(ix_bins), ix_bins=1, nx_bins)
      !
      write ( lu_out, '(/, " iy_bins  y_bins")')
      if (associated(y_bins)) &
     & write ( lu_out, '(1x, i8, 1x, g10.4)') &
     & (iy_bins, y_bins(iy_bins), iy_bins=1, ny_bins)
      !
      write ( lu_out, '(/, " it_bins  t_bins v_bins ")')
      if (associated(t_bins)) &
     & write ( lu_out, '(1x, i8, 1x, g10.4, 1x, g10.4)') &
     & (it_bins, t_bins(it_bins), v_bins(it_bins), it_bins=1, nt_bins)
      !
    end if    ! if ( lu_out .ge. 0 ) then
!      if (nt_bins .ne. -999) stop
    !
    ! - close the velocity file
    !
    if (associated(velio_obj)) call velio_close (velio_obj)
!      if (nt_bins .ne. -999) stop
    !
    if (associated(x_cord)) deallocate (x_cord)
    if (associated(y_cord)) deallocate (y_cord)
    if (associated(x_bins)) deallocate (x_bins)
    if (associated(y_bins)) deallocate (y_bins)
    if (associated(t_bins)) deallocate (t_bins)
    if (associated(v_bins)) deallocate (v_bins)
!      if (nt_bins .ne. -999) stop
    !
    return
    !
  992 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_xy ",&
!     & /, " during memory allocate " &
!     & )') 
    call pc_error (msg1 = "Error in velgrid_size_xy during memory allocate")
    call pc_error (msg1 = "Allocate status returned: ",    &
                   var1 = i_err)
    go to 999
    !
  994 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_xy ",&
!     & /, " during matfun_array_size y " &
!     & )') 
call pc_error (msg1 = "Error in velgrid_size_xy during matfun_array_size y.")
    call pc_error (msg1 = "matfun_array_size returned status: ",    &
                   var1 = i_err)
    go to 999
    !
  996 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_xy ",&
!     & /, " during matfun_array_size x " &
!     & )') 
call pc_error (msg1 = "Error in velgrid_size_xy during matfun_array_size x.")
    call pc_error (msg1 = "matfun_array_size returned status: ",    &
                   var1 = i_err)
    go to 999
    !
  997 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_xy ",&
!     & /, " the velocity file must be uniformly sampled in x and y " &
!     & )') 

    call pc_error (msg1 = "Error in velgrid_size_xy.")
    call pc_error (msg1 = "The velocity file must be uniformly "    &
                          // "sampled in x (",                      &
                   var1 = nx_bins,                                  &
                   msg2 = ") and y (",                              &
                   var2 = ny_bins)

    go to 999
    !
  998 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_xy ",&
!     & /, " during velio_scan_alloc" &
!     & /, " error=", a &
!     & )') 
!     & trim(crd80)
    call pc_error (msg1 = "Error in velgrid_size_xy during velio_scan_alloc.")
    call pc_error (msg1 = "velio_scan_alloc returned status: ",    &
                   var1 = i_err,                                   &
                   msg2 = ", " // trim (crd80))
    go to 999
    !
  999 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_xy ",&
!     & /, " file =", a &
!     & )') 
!     & trim(vel_file)
    call pc_error (msg1 = "Error in velgrid_size_xy.  File: ",    &
                   var1 = trim (vel_file))
    i_err = -1
    go to 1999
    !
  end subroutine velgrid_size_xy

  !!---------------------------- velgrid_size_p ----------------------------!!
  !!---------------------------- velgrid_size_p ----------------------------!!
  !!---------------------------- velgrid_size_p ----------------------------!!

  !
  ! - get the x,y,t sizes of a velocity grid with assigned header words
  !
  subroutine velgrid_size_p ( &
                              i_pel, l_broadcast,                             &
                              lu_out, c_title, vel_file, vel_type,            &
                              n_vel,  hx_vel, hy_vel,                         &
                              nx_vel, x0_vel, dx_vel,                         &
                              ny_vel, y0_vel, dy_vel,                         &
                              nt_vel, t0_vel, dt_vel, i_err &
                            )
    !
    ! - Arguments
    !
    integer,             intent (in   ) :: i_pel
    logical,             intent (in   ) :: l_broadcast
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: vel_file
    character (len = *), intent (inout) :: vel_type
    integer,             intent (in   ) :: lu_out
    integer,             intent (in   ) :: hx_vel
    integer,             intent (in   ) :: hy_vel
    integer,             intent (  out) :: i_err
    integer,             intent (  out) :: nt_vel
    integer,             intent (  out) :: n_vel
    integer,             intent (  out) :: nx_vel
    integer,             intent (  out) :: ny_vel
    real,                intent (  out) :: dt_vel
    real,                intent (  out) :: dx_vel
    real,                intent (  out) :: dy_vel
    real,                intent (  out) :: t0_vel
    real,                intent (  out) :: x0_vel
    real,                intent (  out) :: y0_vel
    !
    ! - Local variables
    !
    i_err = 0
    !
    if ( i_pel .eq. pcpsx_i_pel() ) &
    call velgrid_size_1 (                                                 &
                          lu_out, c_title, vel_file, vel_type,            &
                          n_vel,  hx_vel, hy_vel,                         &
                          nx_vel, x0_vel, dx_vel,                         &
                          ny_vel, y0_vel, dy_vel,                         &
                          nt_vel, t0_vel, dt_vel,                         &
                          i_err                                           &
                        )
    !
    if ( l_broadcast ) call pcpsx_broadcast ( i_pel, i_err  )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! broadcast the info
    !
    if_broadcast : if ( l_broadcast ) then
      !
      call pcpsx_broadcast ( i_pel, vel_type )
      call pcpsx_broadcast ( i_pel, n_vel  )
      call pcpsx_broadcast ( i_pel, nx_vel )
      call pcpsx_broadcast ( i_pel, x0_vel )
      call pcpsx_broadcast ( i_pel, dx_vel )
      call pcpsx_broadcast ( i_pel, ny_vel )
      call pcpsx_broadcast ( i_pel, y0_vel )
      call pcpsx_broadcast ( i_pel, dy_vel )
      call pcpsx_broadcast ( i_pel, nt_vel )
      call pcpsx_broadcast ( i_pel, t0_vel )
      call pcpsx_broadcast ( i_pel, dt_vel )
      !
    end if if_broadcast
    !
    return
    !
  999 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_p ",&
!     & /, " file =", a &
!     & )') 
!     & trim(vel_file)
    call pc_error (msg1 = "Error in velgrid_size_p.  File: ",    &
                   var1 = trim (vel_file))
    i_err = -1
    return
    !
  end subroutine velgrid_size_p

  !!---------------------------- velgrid_size_1 ----------------------------!!
  !!---------------------------- velgrid_size_1 ----------------------------!!
  !!---------------------------- velgrid_size_1 ----------------------------!!

  !
  ! - get the x,y,t sizes of a velocity grid with assigned header words
  !
  subroutine velgrid_size_1 (                                                 &
                              lu_out, c_title, vel_file, vel_type,            &
                              n_vel,  hx_vel, hy_vel,                         &
                              nx_vel, x0_vel, dx_vel,                         &
                              ny_vel, y0_vel, dy_vel,                         &
                              nt_vel, t0_vel, dt_vel,                         &
                              i_err                                           &
                            )
    !
    ! - Arguments
    !
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: vel_file
    character (len = *), intent (inout) :: vel_type
    integer,             intent (in   ) :: lu_out
    integer,             intent (in   ) :: hx_vel
    integer,             intent (in   ) :: hy_vel
    integer,             intent (  out) :: i_err
    integer,             intent (  out) :: nt_vel
    integer,             intent (  out) :: n_vel
    integer,             intent (  out) :: nx_vel
    integer,             intent (  out) :: ny_vel
    real,                intent (  out) :: dt_vel
    real,                intent (  out) :: dx_vel
    real,                intent (  out) :: dy_vel
    real,                intent (  out) :: t0_vel
    real,                intent (  out) :: x0_vel
    real,                intent (  out) :: y0_vel
    !
    ! - Local variables
    !
    character (len =160) :: crd80_1
    character (len =160) :: crd80_2
    character (len =160) :: crd80_3
    integer              hx_inp, nx_inp
    real                 x0_inp,dx_inp

    integer              hy_inp, ny_inp
    real                 y0_inp,dy_inp

    !
    ! - Begin velgrid_size_1
    !
    i_err = 0
    call velgrid_size ( lu_out, c_title, vel_file, vel_type,                   &
                       n_vel,  hx_inp, hy_inp,                                &
                       nx_inp, x0_inp, dx_inp,                                &
                       ny_inp, y0_inp, dy_inp,                                &
                       nt_vel, t0_vel, dt_vel, i_err)
    if ( i_err .ne. 0 ) go to 998
    !
    ! assign the velocity x,y grid valuer from the input gird values
    !
    if (hx_inp .eq. hx_vel .and. hy_inp .eq. hy_vel ) then
      nx_vel = nx_inp
      x0_vel = x0_inp
      dx_vel = dx_inp
      ny_vel = ny_inp
      y0_vel = y0_inp
      dy_vel = dy_inp
    else if (hx_inp .eq. hy_vel .and. hy_inp .eq. hx_vel ) then
      nx_vel = ny_inp
      x0_vel = y0_inp
      dx_vel = dy_inp
      ny_vel = nx_inp
      y0_vel = x0_inp
      dy_vel = dx_inp
    else 
      go to 997
    end if
    return
    !
  997 continue
    !
    write (crd80_1,'("Error in velgrid_size_1.", &
   & "The velocity file does not have the correct header words. ")')
    write (crd80_2,'(" desired headers x=",i8," y=",i8)') hx_vel, hy_vel
    write (crd80_3,'(" file    headers x=",i8," y=",i8)') hx_inp, hy_inp
    call pc_error (trim(crd80_1))
    call pc_error (trim(crd80_2))
    call pc_error (trim(crd80_3))

    go to 999
    !
  998 continue
    !
    call pc_error (msg1 = "Error in velgrid_size_1 during velgrid_size .")
    go to 999
    !
  999 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_size_1 ",&
!     & /, " file =", a &
!     & )') 
!     & trim(vel_file)
    call pc_error (msg1 = "Error in velgrid_size_1.  File: ",    &
                   var1 = trim (vel_file))
    i_err = -1
    return
    !
  end subroutine velgrid_size_1

  !!---------------------------- velgrid_read_p ----------------------------!!
  !!---------------------------- velgrid_read_p ----------------------------!!
  !!---------------------------- velgrid_read_p ----------------------------!!

  subroutine velgrid_read_p ( &
                              i_pel, l_broadcast,                             &
                              lu_out, c_title, vel_file,                    &
                              out_type, vel_parm,                           &
                              hx_out, hy_out,                               &
                              nx_out, x0_out, dx_out,                       &
                              ny_out, y0_out, dy_out,                       &
                              nt_out, t0_out, dt_out,                       &
                              v0_out,                                       &
                              i_err, vel_order &
                            )
    !
    ! - Arguments
    !
    integer,             intent (in   ) :: i_pel
    logical,             intent (in   ) :: l_broadcast
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: out_type ! output velocity type:
                                                    !   VTRM, VTIN, VZIN
    character (len = *), intent (in   ) :: vel_file !  file and function names
    character (len = *), intent (in   ) :: vel_parm ! parameter type:
                                                    !   VELOCITY, SLOWNESS
    !
    integer, intent (in   ) :: hx_out    !  x location
    integer, intent (in   ) :: hy_out    !  y location
    integer, intent (in   ) :: lu_out
    integer, intent (in   ) :: nt_out
    integer, intent (in   ) :: nx_out
    integer, intent (in   ) :: ny_out
    real,    intent (in   ) :: dt_out
    real,    intent (  inout):: dx_out
    real,    intent (  inout):: dy_out
    real,    intent (in   ) :: t0_out
    real,    intent (  out) :: v0_out (:, :, : ) ! (t,x,y) or (x,y,t)
    real,    intent (  inout) :: x0_out
    real,    intent (  inout) :: y0_out
    integer, intent (  out) :: i_err
    character (len = *), intent (in   ), optional :: vel_order ! grid order
    integer                :: temp
    !
    ! Local variables
    !
    character(len=3) :: a_order          ! order string
    !
    a_order = 'TXY'
    !
    i_err = 0
    !
    if ( present(vel_order) ) a_order = vel_order (1:3)
    !
    if ( i_pel .eq. pcpsx_i_pel() ) &
    call velgrid_read (                                               &
                        lu_out, c_title, vel_file,                    &
                        out_type, vel_parm,                           &
                        hx_out, hy_out,                               &
                        nx_out, x0_out, dx_out,                       &
                        ny_out, y0_out, dy_out,                       &
                        nt_out, t0_out, dt_out,                       &
                        v0_out,                                       &
                        i_err, vel_order                              &
                      )
    !
    if ( l_broadcast ) call pcpsx_broadcast ( i_pel, i_err  )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! broadcast the info
    !
    if_broadcast : if ( l_broadcast ) then
      !
      call pcpsx_broadcast ( i_pel, x0_out )
      call pcpsx_broadcast ( i_pel, dx_out )
      call pcpsx_broadcast ( i_pel, y0_out )
      call pcpsx_broadcast ( i_pel, dy_out )
      temp = size(v0_out,3)
      call pcpsx_broadcast ( i_pel, temp , v0_out )
      !
    end if if_broadcast
    !
    return
    !
  999 continue
    !
    call pc_error (msg1 = "Error in velgrid_read.")
    !
    i_err = -1
    !
    return
    !
  end subroutine velgrid_read_p

  !!---------------------------- velgrid_read ------------------------------!!
  !!---------------------------- velgrid_read ------------------------------!!
  !!---------------------------- velgrid_read ------------------------------!!

  !
  ! - read a velocity file into a grid
  !    if i_bcast=0 broadcast from pe 0 to the rest
  !
  subroutine velgrid_read (                                               &
                            lu_out, c_title, vel_file,                    &
                            out_type, vel_parm,                           &
                            hx_vel, hy_vel,                               &
                            nx_vel, x0_vel, dx_vel,                       &
                            ny_vel, y0_vel, dy_vel,                       &
                            nt_vel, t0_vel, dt_vel,                       &
                            v0_vel,                                       &
                            i_err, vel_order                              &
                          )
    !
    ! - Arguments
    !
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: out_type ! output velocity type:
                                                    !   VTRM, VTIN, VZIN
    character (len = *), intent (in   ) :: vel_file !  file and function names
    character (len = *), intent (in   ) :: vel_parm ! parameter type:
                                                    !   VELOCITY, SLOWNESS
    !
    integer, intent (in   ) :: hx_vel    !  x location
    integer, intent (in   ) :: hy_vel    !  y location
    integer, intent (in   ) :: lu_out
    integer, intent (in   ) :: nt_vel
    integer, intent (in   ) :: nx_vel
    integer, intent (in   ) :: ny_vel
    real,    intent (in   ) :: dt_vel
    real,    intent (  in) :: dx_vel
    real,    intent (  in) :: dy_vel
    real,    intent (in   ) :: t0_vel
    real,    intent (  out) :: v0_vel (:, :, : ) ! (t,x,y) or (x,y,t)
    real,    intent (  in) :: x0_vel
    real,    intent (  in) :: y0_vel
    integer, intent (  out) :: i_err
    character (len = *), intent (in   ), optional :: vel_order ! grid order
    !
    ! - Local variables
    !
    character :: attname*80   ! any attribute name given to the ordinate 
    character :: attunits*80  ! attribute units of the ordinate (e.g. feet/sec).
    character :: crd80*80
    character :: encoding*80  ! encoding format of velocity file.
    character :: fields(8)*80 ! list of velocity function fields to read or wri
    character :: inp_name*8   ! input velocity function name
    character :: inp_type*8   ! input velocity function type
    character :: nilstring*80 ! string for nil values in the file (def: '-nil-
    character :: tdunits*80   ! time/depth units of the abscissa 
                              !   (e.g. sec, feet,
    character :: dunits*80    ! units of velocity functions (feet or meters).
    !
    integer       :: hx_inp, hy_inp
    integer       :: it_out
    integer       :: ix_inp, iy_inp
    integer       :: ix_inp_1(max(nx_vel,ny_vel))    ! temporary array
    integer       :: ix_inp_2(max(nx_vel,ny_vel))    ! temporary array
    integer       :: ix_out, iy_out
    integer       :: iy_inp_1(max(nx_vel,ny_vel))    ! temporary array
    integer       :: iy_inp_2(max(nx_vel,ny_vel))    ! temporary array
    integer       :: mt_inp              ! maximum number of picks in any 
                                         !   function
    integer       :: nfields             ! number of velocity function fields 
                                         !   to read or wri
    integer       :: nt_inp, n_xy_inp
    integer       :: nx_inp              ! number of X coordinates in the 
                                         !  x_inp array.
    integer       :: ny_inp              ! number of Y coordinates in the
                                         !  y_inp array.
    real          :: fx_inp_1(nx_vel)    ! temporary array
    real          :: fx_inp_2(nx_vel)    ! temporary array
    real          :: fy_inp_1(ny_vel)    ! temporary array
    real          :: fy_inp_2(ny_vel)    ! temporary array
    real          :: nmo_exp
    real          :: nmo_sign
    real          :: x_tmp, y_tmp
    real, pointer :: t_bin (:)           ! velocity on original grid
    real, pointer :: t_inp (:)           ! velocity on original grid
    real, pointer :: v0_tmp(:)           ! velocity on original vertical grid
    real, pointer :: v0_inp(:, :, :)     ! velocity on original grid
    real, pointer :: v_bin (:)           ! velocity on original grid
    real, pointer :: v_inp (:)           ! velocity on original grid
    real, pointer :: x_cord(:)           ! pointer to X coordinates of all 
                                         !  NFUN velocit
    real, pointer :: x_inp(:)            ! pointer to X coordinates of 
                                         !  rectangular array
    real, pointer :: y_cord(:)           ! pointer to Y coordinates of all 
                                         !  NFUN velocit
    real, pointer :: y_inp(:)            ! pointer to Y coordinates of 
                                         !  rectangular array
    integer   :: i1_out, i2_out, i3_out  ! output indices
    integer   :: j1_out, j2_out, j3_out  ! output indices
    integer   :: i0_out(3)               ! output indices
    character(len=3) :: a_order          ! order string
!!START! RS DAY
    type(modgrid_struct),pointer    :: gobj
    character(len=8) :: ftype   ! input velocity function type
    character(len=96):: dfile
    double precision :: fsize
    real,pointer  :: dpntr(:)
    integer       :: ng(3),hdwd(3),rank
    integer       :: npts,ig
    real          :: og(3),dg(3)
    integer       :: i_eps
    real          :: r_eps



    logical       :: not_v0_inp
    logical       :: use_v0_inp
    real          :: xangle
    character(len=32) :: lab1,lab2,lab3
    character(len=8)  :: xyz_order
    integer           :: scan_xhdr,scan_yhdr
    integer       :: private_stride, private_u
!!/START! RS DAY
    !
    type (velio_struct), pointer :: velio_obj ! pointer to VELIO data structur
    !
    ! - Begin velgrid_read
    !
    i_err = 0
    !
    ! determine the output order indices
    !
    ! i0_out(1) = ix_out
    ! i0_out(2) = iy_out
    ! i0_out(3) = it_out
    ! i1_out = i0_out(j1_out)
    ! i2_out = i0_out(j2_out)
    ! i3_out = i0_out(j3_out)
    ! v0_out(i1_out, i2_out, i3_out)
    !
    a_order = 'TXY'
    !
    if ( present(vel_order) ) a_order = vel_order (1:3)
    !
    if ( string_upper_compare ( a_order(1:1), 'X' ) ) j1_out = 1
    if ( string_upper_compare ( a_order(1:1), 'Y' ) ) j1_out = 2
    if ( string_upper_compare ( a_order(1:1), 'T' ) ) j1_out = 3
    !
    if ( string_upper_compare ( a_order(2:2), 'X' ) ) j2_out = 1
    if ( string_upper_compare ( a_order(2:2), 'Y' ) ) j2_out = 2
    if ( string_upper_compare ( a_order(2:2), 'T' ) ) j2_out = 3
    !
    if ( string_upper_compare ( a_order(3:3), 'X' ) ) j3_out = 1
    if ( string_upper_compare ( a_order(3:3), 'Y' ) ) j3_out = 2
    if ( string_upper_compare ( a_order(3:3), 'T' ) ) j3_out = 3
    !
    ! make sure we have got the correct indices
    !
    if (j1_out + j2_out + j3_out .ne. 6 ) go to 996
    !
    nullify (x_cord)
    nullify (y_cord)
    nullify (x_inp )
    nullify (y_inp )
    nullify (gobj )
    nullify (dpntr )
    !
!!START! RS DAY
    ftype =   modgrid_ftype(vel_file,lu_out,fsize)
    if(ftype=='UNKNOWN') then
      write(lu_out,*) 'velgrid_read: error UNKNOWN type, vel_file=',&
      trim(vel_file)
      i_err = -1
      return
    endif
    if(ftype == 'TRCIO' .or. ftype=='HGRID' .or. &
       ftype == 'VOXET' .or. ftype=='MODSPEC' .or. &
       ftype == 'SEGY' ) then
      ! get info about the input model object
      scan_xhdr= hx_vel !HDR_MIDPOINT_XLOC
      scan_yhdr= hy_vel !HDR_MIDPOINT_YLOC
      ng=1
      inp_type = ' '
      i_err = modgrid_rddesc_verbose(gobj,vel_file,lu_out,dfile,ftype,rank,&
              lab1,hdwd(1),ng(1),og(1),dg(1), & 
              lab2,hdwd(2),ng(2),og(2),dg(2), & 
              lab3,hdwd(3),ng(3),og(3),dg(3), & 
              xyz_order, scan_xhdr,scan_yhdr,inp_type)
      if( i_err < 0) return
      if(ftype=='MODSPEC') then !override modspec z grid
        call modgrid_set_modspec_zgrid(gobj, nt_vel,t0_vel,dt_vel)
      endif
      call modgrid_print(gobj,lu_out)
      xangle = modgrid_xangle(gobj)
      if(xangle /= 0) then
        write(lu_out,*) 'velgrid_read: xangle = ',xangle
      endif
     !dunits = punits
      mt_inp = ng(1)
      allocate(dpntr(ng(1)))
      nx_inp = ng(2)
      ny_inp = ng(3)
      n_xy_inp = ng(2)*ng(3)
      ix_out = hdwd(2)
      hx_inp = hdwd(2)
      iy_out = hdwd(3)
      hy_inp = hdwd(3)
     
      allocate(x_inp(ng(2)))
      do ig = 1,ng(2)
        x_inp(ig) = og(2) + (ig-1)*dg(2)
      enddo
      allocate(y_inp(ng(3)))
      do ig = 1,ng(3)
        y_inp(ig) = og(3) + (ig-1)*dg(3)
      enddo
      i_err = 0
    else
      ftype= 'VELF'
    end if
!!/START! RS DAY
    !
    ! - if this is the correct pe read the info
    !
    if ( ftype=='VELF' ) then
    call velio_scan_alloc (  filename  = vel_file,    &
                             nfun      = n_xy_inp,    &
                             err       = i_err,       &
                             msg       = crd80,       &
                             nhx       = ix_out,      &
                             nhy       = iy_out,      &
                             nmosign   = nmo_sign,    &
                             nmoexp    = nmo_exp,     &
                             maxpicks  = mt_inp,      &
                             xcoords   = x_cord,      &
                             ycoords   = y_cord,      &
                             xbins     = x_inp,       &
                             ybins     = y_inp,       &
                             nxbins    = nx_inp,      &
                             nybins    = ny_inp,      &
                             dunits    = dunits,      &
                             attname   = attname,     &
                             attunits  = attunits,    &
                             tdunits   = tdunits,     &
                             encoding  = encoding,    &
                             fields    = fields,      &
                             nfields   = nfields,     &
                             nilstring = nilstring)
      !
      if ( i_err .ne. 0 ) go to 998
      !
      write ( lu_out, '(  &
                          & /, " velgrid_read" &
                          & /, " vel_file=", a &
                          & /, " units   =", a &
                          & /, " tdunits =", a &
                          & /, " order   =", a &
                          & /, " j1_out=", i1, &
                          &    " j2_out=", i1, &
                          &    " j3_out=", i1, &
                          & /, " n_xy_inp=", i8, &
                          & " nx_inp=", i8, " ny_inp=", i8, &
                          & " mt_inp=", i8, &
                          & /, " n_xyt_inp=", i8 &
                          & )') &
                          trim ( vel_file ) , &
                          trim ( dunits ) , &
                          trim ( tdunits ) , &
                          trim ( a_order ) , &
                          j1_out, j2_out, j3_out, &
                          n_xy_inp, nx_inp, ny_inp, mt_inp, &
                          nx_inp*ny_inp*mt_inp
      !
      write ( lu_out, '(/, " ix_inp  x_inp")')
      write ( lu_out, '(1x, i8, 1x, g10.4)') &
                            (ix_inp, x_inp(ix_inp), ix_inp=1, nx_inp)
      !
      write ( lu_out, '(/, " iy_inp  y_inp")')
      write ( lu_out, '(1x, i8, 1x, g10.4)') &
                            (iy_inp, y_inp(iy_inp), iy_inp=1, ny_inp)
      !
      end if
      !
      ! - allocate memory
      !
      allocate(t_bin(mt_inp), stat=i_err)
      if ( i_err .ne. 0 ) go to 997
      !
      allocate(v_bin(mt_inp), stat=i_err)
      if ( i_err .ne. 0 ) go to 997
      !
      allocate(t_inp(mt_inp), stat=i_err)
      if ( i_err .ne. 0 ) go to 997
      !
      allocate(v_inp(mt_inp), stat=i_err)
      if ( i_err .ne. 0 ) go to 997
      !
      allocate(v0_tmp(nt_vel), stat=i_err)
      if ( i_err .ne. 0 ) go to 997
      !
      ! set a pair of flags not_v0_inp and use_v0_inp
      ! which will control weather or nbot the 3D array v0_inp is used
      ! if not velgrid read puts the 1D velocity v0_tmp directly into 
      ! v0_vel and avoid using v0_inp
      ! to do this the output and input grids and orders must be identical
      ! currently 10-30 am 5-1-03 this check is incomplete - hanson
      !
      not_v0_inp = .true.
      !
      i_eps = 0
      !
      r_eps = 1.e-3
      !
      ! not the increment and origin tests are currently disabled 
      !
!print*,' not_v0_inp=',not_v0_inp
      !
      if ( abs ( hx_inp - hx_vel ) .gt. i_eps ) not_v0_inp = .false.
!print*,' hx_vel=',hx_vel,' hx_inp=',hx_inp,' not_v0_inp=',not_v0_inp
      !
      if ( abs ( hy_inp - hy_vel ) .gt. i_eps ) not_v0_inp = .false.
!print*,' hy_vel=',hy_vel,' hy_inp=',hy_inp,' not_v0_inp=',not_v0_inp
      !
      if ( abs ( nx_inp - nx_vel ) .gt. i_eps ) not_v0_inp = .false.
!print*,' nx_vel=',nx_vel,' nx_inp=',nx_inp,' not_v0_inp=',not_v0_inp
      !if ( abs ( x0_inp - x0_vel ) .gt. r_eps ) not_v0_inp = .false.
      !if ( abs ( dx_inp - dx_vel ) .gt. r_eps ) not_v0_inp = .false.
      !
      if ( abs ( ny_inp - ny_vel ) .gt. i_eps ) not_v0_inp = .false.
!print*,' ny_vel=',ny_vel,' ny_inp=',ny_inp,' not_v0_inp=',not_v0_inp
      !if ( abs ( y0_inp - y0_vel ) .gt. r_eps ) not_v0_inp = .false.
      !if ( abs ( dy_inp - dy_vel ) .gt. r_eps ) not_v0_inp = .false.
      !
      use_v0_inp = .not. not_v0_inp 
      !
      !if ( nt_inp .ne. -999 ) stop
      !
      if ( use_v0_inp ) &
      allocate(v0_inp(nt_vel, nx_inp, ny_inp), stat=i_err)
      if ( i_err .ne. 0 ) go to 997
      !
      ! - get the velocity
      !
      nullify (velio_obj) ! pointer to VELIO data structure.
      !
      ! - open the velocity file
      !
      if(ftype=='VELF') then
        call velio_open_read (obj      = velio_obj,    &
                            filename = vel_file,     &
                            nfun     = n_xy_inp,     &
                            err      = i_err,        &
                            msg      = crd80,        &
                            nhx      = hx_inp,       &
                            nhy      = hy_inp)
      !
        if (n_xy_inp .ne. nx_inp*ny_inp) then
          call pc_error (msg1 = "Error in velgrid_read. n_xy_inp (",   &
                       var1 = n_xy_inp,                             &
                       msg2 = ") .ne. nx_inp*ny_inp (",             &
                       var2 = nx_inp*ny_inp,                        &
                       msg3 = ") after velio_open_read.")
          go to 999
        end if
      end if
      !
      if (hx_inp .eq. 0 ) hx_inp = hx_vel
      if (hy_inp .eq. 0 ) hy_inp = hy_vel
! make sure one of hx_inp or hy_inp is hx_vel
      if (hx_inp .ne. hx_vel .and. hy_inp .ne. hx_vel) then
        call pc_error (msg1 = "Error in velgrid_read. "            &
                              // "The header words do not match"   &
                              // " Found hx_inp = ",               &
                      var1 = hx_inp,                               &
                      msg2 = ", wanted hx_vel = ",                 &
                      var2 = hx_vel)
        !
         go to 999
        !
      end if
      !
! make sure one of hx_inp or hy_inp is hy_vel
      if (hx_inp .ne. hy_vel .and. hy_inp .ne. hy_vel) then
        call pc_error (msg1 = "Error in velgrid_read. "              &
                              // "The header words do not match. "   &
                              // " Found hy_inp = ",                 &
                      var1 = hy_inp,                                 &
                      msg2 = ", wanted hy_vel = ",                   &
                      var2 = hy_vel)
        !
        go to 999
        !
      end if
      !
      write ( lu_out, '( &
      & /, " velgrid_read ", &
      & /, " file=", a, &
      & /, " use_v0_inp=", l2," not_v0_inp=",l2, &
      & /, " n_xy_inp=", i8, &
      & /, " hx_inp=", i2, " hy_inp=", i2, &
      & /, " nx_inp=", i4, " ny_inp=", i4)') &
      vel_file, &
      use_v0_inp, not_v0_inp, &
      n_xy_inp, &
      hx_inp, hy_inp, &
      nx_inp, ny_inp
      !
      do iy_inp = 1 , ny_inp
        !
        if(ftype/='VELF') then
         i_err = modgrid_rd(gobj,vel_file,lu_out,iy_inp, 1)
         if(i_err /= 0) then
           write(lu_out) 'velgrid_read: error from modgrid_rd'
           goto 999
         endif
        endif
        do ix_inp = 1 , nx_inp
          !
          ! - read the functin into t_inp, v_inp
          !
          if(ftype=='VELF') then
            call velio_read_velfun (obj     = velio_obj,    &
                                  xcoord  = x_tmp,        &
                                  ycoord  = y_tmp,        &
                                  npicks  = nt_inp,       &
                                  tpicks  = t_bin,        &
                                  vpicks  = v_bin,        &
                                  err     = i_err,        &
                                  msg     = crd80,        &
                                  velname = inp_name,     &
                                  veltype = inp_type)
          else
            npts = ng(1)
            inp_name='GRID'
            private_stride=1
            private_u     =1
            i_err = modgrid_get_data(gobj,dpntr,npts, private_stride,private_u,ix_inp,iy_inp)
            if(i_err /= 0) then
              write(lu_out) 'velgrid_read: error from modgrid_get_data'
              goto 999
            endif
            x_tmp = og(2) + (ix_inp-1)*dg(2)
            y_tmp = og(3) + (iy_inp-1)*dg(3)
            nt_inp= ng(1)
            do ig = 1,ng(1)
             t_bin(ig)= og(1) + (ig-1)*dg(1)
            enddo
            v_bin(1:ng(1)) = dpntr(1:ng(1))
          end if
          !
          if ( ( i_err .ne. 0 )    &
                   !
                   ! - make sure we have the correct x, y
                   !
             .or. (abs(x_tmp-x_inp(ix_inp)) .gt. .01)   &
             .or. (abs(y_tmp-y_inp(iy_inp)) .gt. .01) ) then
            !
            call pc_error (msg1 = "Error in velgrid_read.  Error ",   &
                           var1 = i_err,                              &
                           msg2 = " returned from velio_read_velfun")
            !
            go to 999
            !
          end if
          !
          ! - convert from the input type to the output type
          !
          if(inp_type== out_type) then
            t_inp = t_bin
            v_inp = v_bin
          else
            call velutil_convert (veltype    = inp_type,    &
                                npicks     = nt_inp,      &
                                X          = t_bin,       &
                                V          = v_bin,       &
                                veltypeout = out_type,    &
                                xout       = t_inp,       &
                                vout       = v_inp,       &
                                ierr       = i_err)
          endif
          !
          if ( i_err .ne. 0 ) then
            call pc_error (msg1 = "Error in velgrid_read. Error ",   &
                           var1 = i_err,                             &
                           msg2 = " returned from velutil_convert")
            go to 999
            !
          end if
          !
          ! - interpolate in the the direction into v0_inp (:, ix_inp, iy_inp)
          ! -  make sure the travel times change monotonicly
          !
          call matfun_check_change (i_dir = +1,        & 
                                    n     = nt_inp,    & 
                                    x     = t_inp,     & 
                                    i_err = i_err)
          !
          if ( i_err .ne. 0 ) then
            call pc_error (msg1 = "Error in velgrid_read. Error ",  &
                           var1 = i_err,                            &
                           msg2 = " returned from matfun_check_change")
            go to 999
            !
          end if
          !
          ! - convert from velocity to slowness
          !
          if ( string_upper_compare ( vel_parm(1:1), 'S' ) )then
            !
            call matfun_invert (n1 = nt_inp,    &
                                x  = v_inp)
            !
          end if
          !
          ! - interpolate to a uniform fine grid in v0_vel(:, ix_out, iy_out)
          !
          v0_tmp=0.0
          !
          if(ftype /= 'VELF') then
            !
            call interpolate_1d_to_1d (nt_inp, og(1), dg(1),    &
                 v_inp,                    &
                 nt_vel, t0_vel, dt_vel,    &
                 v0_tmp )
            !
          else
            !
            call interpolate_i_to_r (nx_inp = nt_inp,     & 
                 rx_inp = t_inp,      & 
                 ry_inp = v_inp,      &
                 nx_out = nt_vel,     & 
                 x0_out = t0_vel,     & 
                 dx_out = dt_vel,     & 
                 ry_out  = v0_tmp )
            !
          end if
          !
          xxif_use_v0_inp_1 : if ( use_v0_inp ) then
            !
            v0_inp (:, ix_inp, iy_inp) = v0_tmp (:)
            !
          else xxif_use_v0_inp_1 
            !
            v0_vel (:, ix_inp, iy_inp) = v0_tmp (:)
            !
          end if xxif_use_v0_inp_1 
          !
        end do    ! do ix_inp = 1 , nx_inp
        if(ftype/='VELF') then
          if(associated(gobj)) call modgrid_delete(gobj)
        endif
        !
      end do    ! do iy_inp = 1 , ny_inp
      !
      ! input and output x,y are the same
      !
      if(associated(dpntr)) deallocate(dpntr)
      if(associated(gobj)) call modgrid_delete(gobj)
      !
      xxif_use_v0_inp_2 : if ( use_v0_inp ) then
        !
      xxif_xy_the_same : &
      if (hx_inp .eq. hx_vel .and. hy_inp .eq. hy_vel ) then
        !
        ! - for each output x point determine the input x interpolation 
        !   index and coefficient
        !
      call interpolate_find_index_g (nx_inp   = nx_inp,      & 
                                     rx_inp   = x_inp,       & 
                                     nx_out   = nx_vel,      & 
                                     x0_out   = x0_vel,      & 
                                     dx_out   = dx_vel,      &
                                     ix_inp_1 = ix_inp_1,    & 
                                     ix_inp_2 = ix_inp_2,    & 
                                     fx_inp_1 = fx_inp_1,    & 
                                     fx_inp_2 = fx_inp_2)
      !
      ! - for each output y point determine the input y interpolation 
      !   index and coefficient
      !
      call interpolate_find_index_g (nx_inp   = ny_inp,      & 
                                     rx_inp   = y_inp,       & 
                                     nx_out   = ny_vel,      & 
                                     x0_out   = y0_vel,      & 
                                     dx_out   = dy_vel,      &
                                     ix_inp_1 = iy_inp_1,    & 
                                     ix_inp_2 = iy_inp_2,    & 
                                     fx_inp_1 = fy_inp_1,    & 
                                     fx_inp_2 = fy_inp_2)
      !
      ! - compute the output x,y points from the input x,y points
      !
      do iy_out = 1 , ny_vel
        !
        do ix_out = 1 , nx_vel
          !
          do it_out = 1 , nt_vel
            !
            i0_out(1) = ix_out
            i0_out(2) = iy_out
            i0_out(3) = it_out
            !
            i1_out = i0_out(j1_out)
            i2_out = i0_out(j2_out)
            i3_out = i0_out(j3_out)
            !
            !print'(" a itxy=",i3,1x,i3,1x,i3," i123=",i3,1x,i3,1x,i3)',&
            !& it_out,ix_out,iy_out,i1_out,i2_out,i3_out
            !
            v0_vel(i1_out, i2_out, i3_out)                                 &
              =                  fx_inp_1(ix_out) * fy_inp_1(iy_out)       &
                * v0_inp(it_out, ix_inp_1(ix_out) , iy_inp_1(iy_out))      &
                +                fx_inp_2(ix_out) * fy_inp_1(iy_out)       &
                * v0_inp(it_out, ix_inp_2(ix_out) , iy_inp_1(iy_out))      &
                +                fx_inp_1(ix_out) * fy_inp_2(iy_out)       &
                * v0_inp(it_out, ix_inp_1(ix_out) , iy_inp_2(iy_out))      &
                +                fx_inp_2(ix_out) * fy_inp_2(iy_out)       &
                * v0_inp(it_out, ix_inp_2(ix_out) , iy_inp_2(iy_out))
            !
          end do    ! do it_out = 1 , nt_vel
          !
        end do    ! do ix_out = 1 , nx_vel
        !
      end do    ! do iy_out = 1 , ny_vel
      !
    else xxif_xy_the_same
      !
      ! - for each output x point determine the input y interpolation 
      !   index and coefficient
      !
      call interpolate_find_index_g (nx_inp   = ny_inp,      & 
                                     rx_inp   = y_inp,       & 
                                     nx_out   = nx_vel,      & 
                                     x0_out   = x0_vel,      & 
                                     dx_out   = dx_vel,      &
                                     ix_inp_1 = iy_inp_1,    & 
                                     ix_inp_2 = iy_inp_2,    & 
                                     fx_inp_1 = fy_inp_1,    & 
                                     fx_inp_2 = fy_inp_2)
      !
      ! - for each output y point determine the input x interpolation 
      !   index and coefficient
      !
      call interpolate_find_index_g (nx_inp   = nx_inp,      & 
                                     rx_inp   = x_inp,       & 
                                     nx_out   = ny_vel,      & 
                                     x0_out   = y0_vel,      & 
                                     dx_out   = dy_vel,      &
                                     ix_inp_1 = ix_inp_1,    & 
                                     ix_inp_2 = ix_inp_2,    & 
                                     fx_inp_1 = fx_inp_1,    & 
                                     fx_inp_2 = fx_inp_2)
      !
      ! - compute the output x,y points from the input y,x points
      !
      do iy_out = 1 , ny_vel
        !
        do ix_out = 1 , nx_vel
          !
          do it_out = 1 , nt_vel
            !
            i0_out(1) = ix_out
            i0_out(2) = iy_out
            i0_out(3) = it_out
            !
            i1_out = i0_out(j1_out)
            i2_out = i0_out(j2_out)
            i3_out = i0_out(j3_out)
            !
            !print'(" b itxy=",i3,1x,i3,1x,i3," i123=",i3,1x,i3,1x,i3)',&
            !& it_out,ix_out,iy_out,i1_out,i2_out,i3_out
            !
            v0_vel(i1_out, i2_out, i3_out)                                 &
              =                  fx_inp_1(iy_out) * fy_inp_1(ix_out)       &
                * v0_inp(it_out, ix_inp_1(iy_out) , iy_inp_1(ix_out))      &
                +                fx_inp_2(iy_out) * fy_inp_1(ix_out)       &
                * v0_inp(it_out, ix_inp_2(iy_out) , iy_inp_1(ix_out))      &
                +                fx_inp_1(iy_out) * fy_inp_2(ix_out)       &
                * v0_inp(it_out, ix_inp_1(iy_out) , iy_inp_2(ix_out))      &
                +                fx_inp_2(iy_out) * fy_inp_2(ix_out)       &
                * v0_inp(it_out, ix_inp_2(iy_out) , iy_inp_2(ix_out))
            !
          end do    ! do it_out = 1 , nt_vel
          !
        end do    ! do ix_out = 1 , nx_vel
        !
      end do    ! do iy_out = 1 , ny_vel
      !
    end if xxif_xy_the_same
    end if xxif_use_v0_inp_2 
    !
    ! determing the output range
    !
    i0_out(1) = nx_vel
    i0_out(2) = ny_vel
    i0_out(3) = nt_vel
    !
    i1_out = i0_out(j1_out)
    i2_out = i0_out(j2_out)
    i3_out = i0_out(j3_out)
    !
    ! - convert from velocity to slowness
    !
    if ( string_upper_compare ( vel_parm(1:1), 'S' ) )then
      !
      if ( use_v0_inp ) &
      call matfun_invert (n1 = nt_vel,    &
                          n2 = nx_inp,    &
                          n3 = ny_inp,    &
                          x  = v0_inp)
      call matfun_invert (n1 = i1_out,    &
                          n2 = i2_out,    &
                          n3 = i3_out,    &
                          x  = v0_vel)
    !
    end if    ! if ( string_upper_compare ( vel_parm(1:1), 'S' ) )then
    !
    if ( lu_out >= 0) &
    write ( lu_out, '( &
    & /, "  velgrid revision: ", &
    &" 28  2006-09-21  Douglas Hanson Interpolate changes. " &
    & )')
    !
    if ( lu_out >= 0) &
    write ( lu_out, '( &
    & /, " velgrid_read ", a,  &
    & /, " vel_file=", a, &
    & /, " inp_type=", a8, &
    & /, " out_type=", a8, &
    & /, " vel_parm=", a8 &
    & /, " n_xyt_vel=", i12, &
    & /, " nx_vel=", i8, " x0_vel=", f10.2, " dx_vel=", f10.2, &
    & " hx_vel=", i4, &
    & /, " ny_vel=", i8, " y0_vel=", f10.2, " dy_vel=", f10.2, &
    & " hy_vel=", i4, &
    & /, " nt_vel=", i8, " t0_vel=", f10.2, " dt_vel=", f10.2 &
    & )') &
    trim(c_title), &
    trim(vel_file), &
    trim(inp_type), &
    trim(out_type), &
    trim(vel_parm), &
    nx_vel * ny_vel * nt_vel, &
    nx_vel, x0_vel, dx_vel, hx_vel, &
    ny_vel, y0_vel, dy_vel, hy_vel, &
    nt_vel, t0_vel, dt_vel
    !
    if ( use_v0_inp .and. lu_out >= 0) &
    write ( lu_out, '( &
    & /, " input  velocity " &
    & /, " min  =", g10.4, " max  =", g10.4, &
    & /, " first=", g10.4, " last =", g10.4 &
    & )') &
    minval(v0_inp), maxval(v0_inp), &
    v0_inp(1, 1, 1), v0_inp(nt_vel, nx_inp, ny_inp)
    !
    if ( not_v0_inp .and. lu_out >= 0) &
    write ( lu_out, '( &
    & /, " input  velocity " &
    & /, " min  =", g10.4, " max  =", g10.4, &
    & /, " first=", g10.4, " last =", g10.4 &
    & )') &
    minval(v0_vel), maxval(v0_vel), &
    v0_vel(1, 1, 1), v0_vel(i1_out, i2_out, i3_out)
    !
    if ( lu_out >= 0) &
    write ( lu_out, '( &
    & /, " output velocity " &
    & /, " min  =", g10.4, " max  =", g10.4, &
    & /, " first=", g10.4, " last =", g10.4 &
    & )') &
    minval(v0_vel), maxval(v0_vel), &
    v0_vel(1, 1, 1), v0_vel(i1_out, i2_out, i3_out)
    !
    ! - convert from slowness to velocity
    !
    if ( string_upper_compare ( vel_parm(1:1), 'S' ) )then
      !
      if ( use_v0_inp ) &
      call matfun_invert (n1 = nt_vel,    &
                          n2 = nx_inp,    &
                          n3 = ny_inp,    &
                          x  = v0_inp )
      !
      call matfun_invert (n1 = i1_out,    &
                          n2 = i2_out,    &
                          n3 = i3_out,    &
                          x  = v0_vel )
      !
    end if    ! if ( string_upper_compare ( vel_parm(1:1), 'S' ) )then
    !
    ! - come to here to deallocate memory and return
    !    this may be from above or below
    !
    1999 continue
    !
    ! - deallocate memory
    !
    if (associated(velio_obj)) call velio_close (obj = velio_obj)
    !
    if (associated(x_cord)) deallocate (x_cord)
    if (associated(y_cord)) deallocate (y_cord)
    if (associated(x_inp )) deallocate (x_inp )
    if (associated(y_inp )) deallocate (y_inp )
    if (associated(t_bin )) deallocate (t_bin )
    if (associated(v_bin )) deallocate (v_bin )
    if (associated(t_inp )) deallocate (t_inp )
    if (associated(v_inp )) deallocate (v_inp )
    if (associated(v0_tmp)) deallocate (v0_tmp)
    if ( use_v0_inp &
  .and. associated(v0_inp)) deallocate (v0_inp)
    !
    return
    !
  996 continue
    !
    call pc_error (msg1 = "Error in velgrid_read in vel_order ")
    call pc_error (msg1 = a_order )
    go to 999
    !
  997 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_read ", &
!     & /, " during memory allocation" &
!     & )') 
    call pc_error (msg1 = "Error in velgrid_read during memory allocate")
    call pc_error (msg1 = "Allocate status returned: ",    &
                   var1 = i_err)
    go to 999
    !
  998 continue
    !
!    write ( pc_get_lun(), '( &
!!     & /, " error in velgrid_read ", &
!     & /, " during velio_scan_alloc ", &
!     & /, " error=", a &
!     & )') &
!     & trim(crd80)
    call pc_error (msg1 = "Error in velgrid_read during velio_scan_alloc")
    call pc_error (msg1 = "velio_scan_alloc returned status: ",    &
                   var1 = i_err)
    go to 999
    !
  999 continue
    !
!    write ( pc_get_lun(), '( &
!     & /, " error in velgrid_read " &
!     & )') 
    call pc_error (msg1 = "Error in velgrid_read.")
    i_err = -1
    go to 1999
    !
  end subroutine velgrid_read
  !
  ! - create a velocity function object
  !
  subroutine velgrid_create ( &
                                lu_out, c_title,                              &
                                n0_vel, i0_vel,                               &
                                path_vel, const_vel,                          &
                                vel_parm, vel_order, vel_type,                &
                                use_const, use_scale, use_head, use_grid,     &
                                hx_vel, hy_vel,                               &
                                sx_vel, sy_vel,                               &
                                nx_vel, x0_vel, dx_vel,                       &
                                ny_vel, y0_vel, dy_vel,                       &
                                nt_vel, t0_vel, dt_vel,                       &
                                v0_obj,                                &
                                i_err                                         &
                              )
    !
    ! - Arguments
    !
    integer,             intent (in   ) :: lu_out      ! print unit number
    !
    character(len=*),    intent (in   ) :: c_title     ! print title
    !
    integer,             intent (in   ) :: n0_vel      ! number of planes
    !
    integer,             intent (in   ) :: i0_vel      ! plane index
    !
    character(len=*),    intent (in   ) :: path_vel    ! file name
    !
    real,                intent (in   ) :: const_vel   ! constant velocity value
    !
    character(len=*),    intent (in   ) :: vel_parm    ! parameter type:
                                                       !   VELOCITY, SLOWNESS
    !
    character(len=*),    intent (in   ) :: vel_order   ! order (x,y,t)or(t,x,y)
    !
    character(len=*),    intent (in   ) :: vel_type    ! output velocity type:
                                                       !   VTRM, VTIN, VZIN
    !
    logical,             intent (in   ) :: use_const   ! use a constant value
    logical,             intent (in   ) :: use_scale   ! apply x,y scale
    logical,             intent (in   ) :: use_head    ! use input header values
    logical,             intent (in   ) :: use_grid    ! use input grid values
    !
    integer,             intent (inout) :: hx_vel      ! x location
    integer,             intent (inout) :: hy_vel      ! y location
    !
    real,                intent (in   ) :: sx_vel      ! x value scale
    real,                intent (in   ) :: sy_vel      ! y value scale
    !
    integer,             intent (inout) :: nx_vel      ! number of x nodes
    real,                intent (inout) :: x0_vel      ! min x node value value
    real,                intent (inout) :: dx_vel      ! x node increment
    !
    integer,             intent (inout) :: ny_vel      ! number of y nodes
    real,                intent (inout) :: y0_vel      ! min y node value value
    real,                intent (inout) :: dy_vel      ! y node increment
    !
    integer,             intent (inout) :: nt_vel      ! number of t nodes
    real,                intent (inout) :: t0_vel      ! min t node value value
    real,                intent (inout) :: dt_vel      ! t node increment
    !
    type ( velgrid_struct ), pointer    :: v0_obj ( : )! vel object
    !
    integer,             intent (  out) :: i_err
    !
    ! - Local variables
    !
    integer                             :: jx_vel
    integer                             :: jy_vel
    integer                             :: jt_vel
    !
    character(len=4)                    :: fil_type    ! output velocity type:
    !
    integer                             :: n_fil
    !
    integer                             :: hx_fil      ! file x location
    integer                             :: hy_fil      ! file y location
    !
    integer                             :: nx_fil      ! file number of x nodes
    real                                :: x0_fil      ! file min x node value
    real                                :: dx_fil      ! file x node increment
    !
    integer                             :: ny_fil      ! file number of y nodes
    real                                :: y0_fil      ! file min y node value
    real                                :: dy_fil      ! file y node increment
    !
    integer                             :: nt_fil      ! file number of t nodes
    real                                :: t0_fil      ! file min t node value
    real                                :: dt_fil      ! file t node increment
    !

    !
    !
    ! - Begin velgrid_create
    !
    i_err = 0
    !
    !
    ! save the number of planes
    !
    xxif_io_vel : if ( i0_vel .eq. 1 ) then
      !
      ! allocate the n0_vel attribute structure, v0_obj
      !
      allocate ( v0_obj ( n0_vel ) )
      !
      ! set the number of planes value
      !
      v0_obj(:)%n0_vel = n0_vel
      !
      !  initialize each of the n0_vel structures
      !
      call velgrid_init ( &
                          v0_obj, &
                          i_err   &
                        )
      !
      if ( i_err .ne. 0 ) goto 997
      !
    else xxif_io_vel
      !
      ! check the number of planes value
      !
      if ( v0_obj(i0_vel)%n0_vel .ne. n0_vel ) goto 998
      !
    end if xxif_io_vel
    !
    ! use either the input value or the file values
    !
    ! set the default file characterisics
    !
    hx_fil = 7
    nx_fil = 1
    x0_fil = 0.
    dx_fil = 1.
    !
    hy_fil = 7
    ny_fil = 1
    y0_fil = 0.
    dy_fil = 1.
    !
    nt_fil = 1
    t0_fil = 0.
    dt_fil = 1.
    !
    fil_type = vel_type
    !
    ! If we are to use the file grid dimensions get them here
    !
    if ( .not. use_const .and. &
         .not. string_upper_compare ( path_vel, pathcheck_empty ) ) &
    call velgrid_size ( &
                        lu_out, c_title, path_vel, fil_type, &
                        n_fil,  hx_fil, hy_fil, &
                        nx_fil, x0_fil, dx_fil, &
                        ny_fil, y0_fil, dy_fil, &
                        nt_fil, t0_fil, dt_fil, &
                        i_err &
                      )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! if we are to use the file characteristics set the input values
    !
    ! save the scale coefficients
    !
    xxif_vel_scale : if ( use_scale ) then
      !
      v0_obj(i0_vel)%sx_vel = sx_vel
      v0_obj(i0_vel)%sy_vel = sy_vel
      !
    else xxif_vel_scale
      !
      v0_obj(i0_vel)%sx_vel = 1.
      v0_obj(i0_vel)%sy_vel = 1.
      !
    end if xxif_vel_scale
    !
    if ( .not. use_head ) hx_vel = hx_fil
    if ( .not. use_grid ) nx_vel = nx_fil
    if ( .not. use_grid ) x0_vel = x0_fil * v0_obj(i0_vel)%sx_vel
    if ( .not. use_grid ) dx_vel = dx_fil * v0_obj(i0_vel)%sx_vel
    !
    if ( .not. use_head ) hy_vel = hy_fil
    if ( .not. use_grid ) ny_vel = ny_fil
    if ( .not. use_grid ) y0_vel = y0_fil * v0_obj(i0_vel)%sy_vel
    if ( .not. use_grid ) dy_vel = dy_fil * v0_obj(i0_vel)%sy_vel
    !
    if ( .not. use_grid ) nt_vel = nt_fil
    if ( .not. use_grid ) t0_vel = t0_fil
    if ( .not. use_grid ) dt_vel = dt_fil
    !
    ! set the object parameters
    !
    v0_obj(i0_vel)%path_vel  = path_vel
    v0_obj(i0_vel)%const_vel = const_vel
    v0_obj(i0_vel)%vel_parm  = vel_parm
    v0_obj(i0_vel)%vel_order = vel_order
    v0_obj(i0_vel)%vel_type  = vel_type
    v0_obj(i0_vel)%fil_type  = fil_type
    v0_obj(i0_vel)%use_const = use_const
    v0_obj(i0_vel)%use_scale = use_scale
    v0_obj(i0_vel)%use_head  = use_head
    v0_obj(i0_vel)%use_grid  = use_grid
    !
    v0_obj(i0_vel)%hx_vel    = hx_vel
    v0_obj(i0_vel)%nx_vel    = nx_vel
    v0_obj(i0_vel)%x0_vel    = x0_vel
    v0_obj(i0_vel)%dx_vel    = dx_vel
    !
    v0_obj(i0_vel)%hy_vel    = hy_vel
    v0_obj(i0_vel)%ny_vel    = ny_vel
    v0_obj(i0_vel)%y0_vel    = y0_vel
    v0_obj(i0_vel)%dy_vel    = dy_vel
    !
    v0_obj(i0_vel)%nt_vel    = nt_vel
    v0_obj(i0_vel)%t0_vel    = t0_vel
    v0_obj(i0_vel)%dt_vel    = dt_vel
    !
    ! set the dimensions
    !
    if ( string_upper_compare ( vel_order(1:1), 'x' ) ) jx_vel = 1
    if ( string_upper_compare ( vel_order(2:2), 'x' ) ) jx_vel = 2
    if ( string_upper_compare ( vel_order(3:3), 'x' ) ) jx_vel = 3
    !
    if ( string_upper_compare ( vel_order(1:1), 'y' ) ) jy_vel = 1
    if ( string_upper_compare ( vel_order(2:2), 'y' ) ) jy_vel = 2
    if ( string_upper_compare ( vel_order(3:3), 'y' ) ) jy_vel = 3
    !
    if ( string_upper_compare ( vel_order(1:1), 't' ) ) jt_vel = 1
    if ( string_upper_compare ( vel_order(2:2), 't' ) ) jt_vel = 2
    if ( string_upper_compare ( vel_order(3:3), 't' ) ) jt_vel = 3
    !
    v0_obj(i0_vel)%nq_vel ( jx_vel ) = v0_obj(i0_vel)%nx_vel
    v0_obj(i0_vel)%q0_vel ( jx_vel ) = v0_obj(i0_vel)%x0_vel
    v0_obj(i0_vel)%dq_vel ( jx_vel ) = v0_obj(i0_vel)%dx_vel
    !
    v0_obj(i0_vel)%nq_vel ( jy_vel ) = v0_obj(i0_vel)%ny_vel
    v0_obj(i0_vel)%q0_vel ( jy_vel ) = v0_obj(i0_vel)%y0_vel
    v0_obj(i0_vel)%dq_vel ( jy_vel ) = v0_obj(i0_vel)%dy_vel
    !
    v0_obj(i0_vel)%nq_vel ( jt_vel ) = v0_obj(i0_vel)%nt_vel
    v0_obj(i0_vel)%q0_vel ( jt_vel ) = v0_obj(i0_vel)%t0_vel
    v0_obj(i0_vel)%dq_vel ( jt_vel ) = v0_obj(i0_vel)%dt_vel
    !
    ! make sure we have got the correct indices
    !
    if ( jx_vel + jy_vel + jt_vel .ne. 6 ) go to 997
    !
    ! adjust the memory printing
    !
    call memfun_prn_off ( )
    !
    ! allocate memory
    !
    call memfun_all ( &
                      v0_obj(i0_vel)%s0_vel, &
                      v0_obj(i0_vel)%nq_vel(1), &
                      v0_obj(i0_vel)%nq_vel(2), &
                      v0_obj(i0_vel)%nq_vel(3), &
                      's0_vel', i_err &
                    )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    xxif_use_const : if ( v0_obj(i0_vel)%use_const &
.or. string_upper_compare ( v0_obj(i0_vel)%path_vel, pathcheck_empty ) ) then
      !
      ! set the function to a constant
      !
      v0_obj(i0_vel)%s0_vel (:, :, : ) = v0_obj(i0_vel)%const_vel
      !
      ! if need be convert to slowness
      !
      if ( string_upper_compare ( vel_parm, 'SLOWNESS' ) ) &
      v0_obj(i0_vel)%s0_vel (:, :, : ) = 1. / v0_obj(i0_vel)%s0_vel (:, :, : ) 
      !
    else xxif_use_const
      !
      v0_obj(i0_vel)%x0_vel = v0_obj(i0_vel)%x0_vel / v0_obj(i0_vel)%sx_vel
      v0_obj(i0_vel)%dx_vel = v0_obj(i0_vel)%dx_vel / v0_obj(i0_vel)%sx_vel
      v0_obj(i0_vel)%y0_vel = v0_obj(i0_vel)%y0_vel / v0_obj(i0_vel)%sy_vel
      v0_obj(i0_vel)%dy_vel = v0_obj(i0_vel)%dy_vel / v0_obj(i0_vel)%sy_vel
      !
      call velgrid_read ( &
                          lu_out, c_title, &
                          v0_obj(i0_vel)%path_vel, &
                          v0_obj(i0_vel)%vel_type, v0_obj(i0_vel)%vel_parm, &
                          v0_obj(i0_vel)%hx_vel, v0_obj(i0_vel)%hy_vel, &
v0_obj(i0_vel)%nx_vel, v0_obj(i0_vel)%x0_vel, v0_obj(i0_vel)%dx_vel, &
v0_obj(i0_vel)%ny_vel, v0_obj(i0_vel)%y0_vel, v0_obj(i0_vel)%dy_vel, &
v0_obj(i0_vel)%nt_vel, v0_obj(i0_vel)%t0_vel, v0_obj(i0_vel)%dt_vel, &
                          v0_obj(i0_vel)%s0_vel, &
                          i_err, v0_obj(i0_vel)%vel_order &
                        )
      !
      v0_obj(i0_vel)%x0_vel = v0_obj(i0_vel)%x0_vel * v0_obj(i0_vel)%sx_vel
      v0_obj(i0_vel)%dx_vel = v0_obj(i0_vel)%dx_vel * v0_obj(i0_vel)%sx_vel
      v0_obj(i0_vel)%y0_vel = v0_obj(i0_vel)%y0_vel * v0_obj(i0_vel)%sy_vel
      v0_obj(i0_vel)%dy_vel = v0_obj(i0_vel)%dy_vel * v0_obj(i0_vel)%sy_vel
      !
      if ( i_err .ne. 0 ) go to 995
      !
    end if xxif_use_const
    !
    ! - come to here to deallocate memory and return
    !    this may be from above or below
    !
1999 continue
    !
    ! - deallocate memory
    !
    return
    !
994 continue
    !
    call pc_error ( msg1 = " Error in velgrid_create i0_vel=", var1 = i0_vel )
    call pc_error ( msg1 = " v0_obj(i0_vel)%n0_vel=", &
                    var1 = v0_obj(i0_vel)%n0_vel, &
                    msg2 = " n0_vel=", var2 = n0_vel )
    !
    go to 999
    !
995 continue
    !
    call pc_error ( &
    msg1 = " Error in velgrid_create during velgrid_read. " )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( &
    msg1 = " Error in velgrid_create during memory allocation. " )
    !
    go to 999
    !
997 continue
    !
    call pc_error (msg1 = "Error in velgrid_create in vel_order ")
    call pc_error (msg1 = vel_order )
    go to 999
    !
998 continue
    !
    call pc_error ( &
    msg1 = " Error in velgrid_create during velgrid_size. " )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( msg1 = " Error in velgrid_create. " )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine velgrid_create
  !
  ! - delete a velocity function object
  !
  subroutine velgrid_delete ( &
                              v0_obj, &
                              i_err   &
                              )
    !
    ! - Arguments
    !
    type ( velgrid_struct ), pointer    :: v0_obj ( : )! vel object
    !
    integer,             intent (  out) :: i_err
    !
    ! - Local variables
    !
    integer                             :: j0_vel      ! attribute index
    !
    ! - Begin velgrid_delete
    !
    i_err = 0
    !
    if ( .not. associated ( v0_obj ) ) return
    !
    ! - deallocate memory
    !
    do_j0_vel : do j0_vel = 1 , v0_obj(1)%n0_vel
      !
      call memfun_del ( v0_obj(j0_vel)%s0_vel )
      call memfun_nul ( v0_obj(j0_vel)%s0_vel )
      !
    end do do_j0_vel
    !
    deallocate ( v0_obj )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! - come to here to deallocate memory and return
    !    this may be from above or below
    !
    1999 continue
    !
    return
    !
999 continue
    !
    call pc_error ( msg1 = " Error in velgrid_delete. " )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine velgrid_delete
  !
  ! - init a velocity function object
  !
  subroutine velgrid_init ( &
                            v0_obj, &
                            i_err   &
                          )
    !
    ! - Arguments
    !
    type ( velgrid_struct ), pointer    :: v0_obj ( : )! vel object
    !
    integer,             intent (  out) :: i_err
    !
    ! - Local variables
    !
    integer                             :: j0_vel      ! attribute index
    !
    ! - Begin velgrid_init
    !
    i_err = 0
    !
    !
    ! - nullify memory
    !
    do_j0_vel : do j0_vel = 1 , v0_obj(1)%n0_vel
      !
      call memfun_nul ( v0_obj(j0_vel)%s0_vel )
      !
    end do do_j0_vel
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! - come to here to deallocate memory and return
    !    this may be from above or below
    !
    1999 continue
    !
    ! - deallocate memory
    !
    !
    return
    !
999 continue
    !
    call pc_error ( msg1 = " Error in velgrid_init. " )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine velgrid_init                   

  !!---------------------------- velgrid_paint ------------------------------!!
  !!---------------------------- velgrid_paint ------------------------------!!
  !!---------------------------- velgrid_paint ------------------------------!!
  ! The velgrid_paint functions are a wrapper to the modgrid_paint functions
  ! Paint parameter values on an output grid from an input file or object.
 !hfast  ... The CPS header consistent with the faster of the X or Y axis
 !hslow  ... The CPS header consistent with the slower of the X or Y axis
 !           hfast, hslow will be mapped to hx and hy
 !xyz_out... A permutation of X,Y,Z that labels an index as  X,Y or Z
 !           Use to control the ordering of output relative to input.
 !           If input as ' ', the input object order will be maintained.
 !vtyp_out.. The desired output type(i.e. VTIN,VZIN). Ignored unless the
 !           input object type is CPSVEL
 !ovors  ... Controls whether data is returned in normal or inverted state
 !           Assumes data is in non-inverted state on disk.
 !           'VELOCITY' | 'SLOWNESS'
 !ivors  ... Controls whether data interpolated in normal or inverted state

  integer function velgrid_paint_by_file_par (vel_file, lu_out, c_title, &
           maxmem, ovors, ivors,      &
           root, do_bcast,            &
           hfast , hslow ,            &
           n1_out, o1_out, d1_out,    &
           n2_out, o2_out, d2_out,    &
           n3_out, o3_out, d3_out,    &
           v0_out, xyz_out, vtyp_out  ) result(status)
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: vel_file
    character (len = *), intent (inout) :: xyz_out ! grid order
    character (len = *), intent (inout) :: vtyp_out! velocity type
    character (len = *), intent (in   ) :: ovors ! output flag
    character (len = *), intent (in   ) :: ivors ! interpolation flag 
                                                 !  VELOCITY or SLOWNESS
    logical, intent (in   ) :: do_bcast
    integer, intent (in   ) :: root
    integer, intent (in   ) :: maxmem
    integer, intent (in   ) :: hfast                ! fast map view header
    integer, intent (in   ) :: hslow                ! slow map view header
    integer, intent (in   ) :: lu_out
    integer, intent (in   ) :: n1_out
    integer, intent (in   ) :: n2_out
    integer, intent (in   ) :: n3_out
    real,    intent (in   ) :: d1_out
    real,    intent (in   ) :: d2_out
    real,    intent (in   ) :: d3_out
    real,    intent (in   ) :: o1_out
    real,    intent (in   ) :: o2_out
    real,    intent (in   ) :: o3_out
    real,    intent (out  ) :: v0_out (n1_out, n2_out, n3_out)
    status = -1
    if(pcpsx_i_pel()== root) then
      status = velgrid_paint_by_file (vel_file, lu_out, c_title,  &
              maxmem, ovors , ivors ,    &
              hfast , hslow ,            &
              n1_out, o1_out, d1_out,    &
              n2_out, o2_out, d2_out,    &
              n3_out, o3_out, d3_out,    &
              v0_out, xyz_out, vtyp_out  )
      if(status /= 0) then
        write(lu_out,*) 'velgrid_paint_by_file_par: error'
      endif
    endif
    if(do_bcast) then
      call pcpsx_broadcast ( root, status  )
      call pcpsx_broadcast ( root, vtyp_out)
      call pcpsx_broadcast ( root, xyz_out)
    endif
    if ( status < 0 ) return
    if(do_bcast) then
      call pcpsx_broadcast ( root, n3_out , v0_out)
    endif
    return
  end function velgrid_paint_by_file_par

  integer function velgrid_paint_by_obj_gpar (gobj, &
           lu_out, c_title,           &
           ovors,ivors,               &
           root, do_bcast, workers,   &
           maxmem,                    &
           hfast , hslow ,            &
           n1_out, o1_out, d1_out,    &
           n2_out, o2_out, d2_out,    &
           n3_out, o3_out, d3_out,    &
           v0_out, xyz_out, vtyp_out  ) result(status)
    type(modgrid_struct),intent(inout)  :: gobj
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: ovors ! output flag
    character (len = *), intent (in   ) :: ivors ! interpolation flag 
                                                 !  VELOCITY or SLOWNESS
    logical, intent (in   ) :: do_bcast
    integer, intent (in   ) :: root
    integer, intent (in   ) :: workers(:)
    integer, intent (in   ) :: maxmem
    integer, intent (in   ) :: hfast                ! fast map view header
    integer, intent (in   ) :: hslow                ! slow map view header
    integer, intent (in   ) :: lu_out
    integer, intent (in   ) :: n1_out
    integer, intent (in   ) :: n2_out
    integer, intent (in   ) :: n3_out
    real,    intent (in   ) :: d1_out
    real,    intent (in   ) :: d2_out
    real,    intent (in   ) :: d3_out
    real,    intent (in   ) :: o1_out
    real,    intent (in   ) :: o2_out
    real,    intent (in   ) :: o3_out
    real,    intent (out  ) :: v0_out (n1_out, n2_out, n3_out)
    character (len = *), intent (inout):: xyz_out ! grid order
    character (len = *), intent (inout):: vtyp_out! velocity type
    integer   :: nworkers
  status = -1
  nworkers = size(workers)
  if(pcpsx_i_pel()== root) then
    status = velgrid_paint_by_obj (gobj, &
              lu_out, c_title,ovors,ivors,  &
              maxmem,                    &
              hfast , hslow ,            &
              n1_out, o1_out, d1_out,    &
              n2_out, o2_out, d2_out,    &
              n3_out, o3_out, d3_out,    &
              v0_out, xyz_out, vtyp_out  )
    if(status /= 0) then
      write(lu_out,*) 'velgrid_paint_by_obj_gpar: error on cpu=',root
    endif
  endif
  if(do_bcast) then
    call pcpsx_broadcast_group ( root,nworkers,workers, status  )
    call pcpsx_broadcast_group ( root,nworkers,workers, vtyp_out)
    call pcpsx_broadcast_group ( root,nworkers,workers, xyz_out)
  endif
  if ( status < 0 ) return
  if(do_bcast) then
    call pcpsx_broadcast_group ( root, nworkers, workers, n3_out , v0_out)
  endif
  return
  end function velgrid_paint_by_obj_gpar

  integer function velgrid_paint_by_obj_par (gobj, &
           lu_out, c_title,           &
           ovors,ivors,               &
           root, do_bcast,            &
           maxmem,                    &
           hfast , hslow ,            &
           n1_out, o1_out, d1_out,    &
           n2_out, o2_out, d2_out,    &
           n3_out, o3_out, d3_out,    &
           v0_out, xyz_out, vtyp_out  ) result(status)
    type(modgrid_struct),intent(inout)  :: gobj
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: ovors ! output flag
    character (len = *), intent (in   ) :: ivors ! interpolation flag 
                                                 !  VELOCITY or SLOWNESS
    logical, intent (in   ) :: do_bcast
    integer, intent (in   ) :: root
    integer, intent (in   ) :: maxmem
    integer, intent (in   ) :: hfast                ! fast map view header
    integer, intent (in   ) :: hslow                ! slow map view header
    integer, intent (in   ) :: lu_out
    integer, intent (in   ) :: n1_out
    integer, intent (in   ) :: n2_out
    integer, intent (in   ) :: n3_out
    real,    intent (in   ) :: d1_out
    real,    intent (in   ) :: d2_out
    real,    intent (in   ) :: d3_out
    real,    intent (in   ) :: o1_out
    real,    intent (in   ) :: o2_out
    real,    intent (in   ) :: o3_out
    real,    intent (out  ) :: v0_out (n1_out, n2_out, n3_out)
    character (len = *), intent (inout):: xyz_out ! grid order
    character (len = *), intent (inout):: vtyp_out! velocity type
  status = -1
  if(pcpsx_i_pel()== root) then
    status = velgrid_paint_by_obj (gobj, &
              lu_out, c_title,ovors,ivors,  &
              maxmem,                    &
              hfast , hslow ,            &
              n1_out, o1_out, d1_out,    &
              n2_out, o2_out, d2_out,    &
              n3_out, o3_out, d3_out,    &
              v0_out, xyz_out, vtyp_out  )
    if(status /= 0) then
      write(lu_out,*) 'velgrid_paint_by_obj_par: error on cpu=',root
    endif
  endif
  if(do_bcast) then
    call pcpsx_broadcast ( root, status  )
    call pcpsx_broadcast ( root, xyz_out)
    call pcpsx_broadcast ( root, vtyp_out)
  endif
  if ( status < 0 ) return
  if(do_bcast) then
    call pcpsx_broadcast ( root, n3_out , v0_out)
  endif
  return
  end function velgrid_paint_by_obj_par

  integer function velgrid_paint_by_file (vel_file, lu_out, c_title,  &
                            maxmem, ovors , ivors ,     &
                            hfast , hslow ,             &
                            n1_out, o1_out, d1_out,     &
                            n2_out, o2_out, d2_out,     &
                            n3_out, o3_out, d3_out,     &
                            v0_out,                     &
                            xyz_out, vtyp_out  ) result(status)
    !
    ! - Arguments
    !
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: vel_file !  file and function names
    character (len = *), intent (inout) :: xyz_out ! grid order
    character (len = *), intent (inout) :: vtyp_out! velocity type
    character (len = *), intent (in   ) :: ovors ! output flag
    character (len = *), intent (in   ) :: ivors ! interpolation flag 
                                                 !  VELOCITY or SLOWNESS
    integer, intent (in   ) :: maxmem               ! max memory to use
    integer, intent (in   ) :: hfast                ! fast map view header
    integer, intent (in   ) :: hslow                ! slow map view header
    integer, intent (in   ) :: lu_out
    integer, intent (in   ) :: n1_out
    integer, intent (in   ) :: n2_out
    integer, intent (in   ) :: n3_out
    real,    intent (in   ) :: d1_out
    real,    intent (in   ) :: d2_out
    real,    intent (in   ) :: d3_out
    real,    intent (in   ) :: o1_out
    real,    intent (in   ) :: o2_out
    real,    intent (in   ) :: o3_out
    real,    intent (out  ) :: v0_out (n1_out, n2_out, n3_out)
    !
    ! - Local variables
    !
    !
    type(modgrid_struct),pointer    :: gobj
    character(len=8)     :: ftype
    character(len=96)    :: dfile
    character(len=4)     :: xyz_in
    double precision     :: fsize
    integer              :: rank
    integer              :: ixlab,iylab,izlab
    integer              :: ngi(3)
    real                 :: dgi(3)
    real                 :: ogi(3)
    integer              :: hdi(3)
    character(len=32)    :: lab1,lab2,lab3
    integer              :: hx_out               ! header for x location
    integer              :: hy_out               ! header for y location
    integer              :: scan_xhdr,scan_yhdr
    integer              :: i_err
    !
    ! - Begin velgrid_paint
    !
    status= -1

    ngi   = 1
    nullify (gobj )

    ftype =   modgrid_ftype(vel_file,lu_out,fsize)

    if(ftype == 'TRCIO' .or. ftype == 'HGRID' .or. &
       ftype == 'VOXET' .or. ftype == 'MODSPEC' .or.&
       ftype == 'CPSVEL'.or. ftype == 'SEGY' ) then
   
      scan_xhdr= hfast !HDR_MIDPOINT_XLOC
      scan_yhdr= hslow !HDR_MIDPOINT_YLOC
      xyz_in   = ' '
      i_err = modgrid_rddesc_verbose(gobj,vel_file,lu_out,dfile,ftype,rank,&
              lab1,hdi(1),ngi(1),ogi(1),dgi(1), & 
              lab2,hdi(2),ngi(2),ogi(2),dgi(2), & 
              lab3,hdi(3),ngi(3),ogi(3),dgi(3), & 
              xyz_in, scan_xhdr,scan_yhdr,vtyp_out)
      if( i_err < 0) then
        write(lu_out,*) 'velgrid_paint_: error calling modgrid_rddesc'
        goto 999
      endif
      i_err = modgrid_xyz_order(gobj, ixlab,iylab,izlab)
      if(i_err /=0) then
        write(lu_out,*) 'velgrid_paint_: error, bad input order'
        goto 999
      endif
      xyz_in=' '
      xyz_in(ixlab:ixlab)='X'
      xyz_in(iylab:iylab)='Y'
      xyz_in(izlab:izlab)='Z'
      if(xyz_out==' ') xyz_out=xyz_in

     !call modgrid_print(gobj,lu_out)

    else
      write(lu_out,*) 'velgrid_paint: invalid model type, ftype=',ftype
      goto 999
    endif
     
    ! translate fast & slow to x & y
    if(hfast == HDR_MIDPOINT_XGRID .or. hfast== HDR_MIDPOINT_XLOC) then
      hx_out = hfast
      hy_out = hslow
    else
      hy_out = hfast
      hx_out = hslow
    endif
    i_err =  modgrid_paint_by_obj(gobj,maxmem, lu_out, &
        ovors , ivors ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        v0_out,  xyz_out, vtyp_out )
    if(i_err < 0) then
      write(lu_out,*) 'velgrid_paint_: call to modgrid_paint_by_obj error'
      goto 999
    endif
    !
    ! - come to here to deallocate memory and return
    !   this may be from above or below
    !
    status= 0
    1999 continue
    !
    ! - deallocate memory
    !
    call modgrid_delete(gobj)
    return
    !
  999 continue
    !
    call pc_error (msg1 = "velgrid_paint_by_file: error")
    i_err = -1
    go to 1999
    !
  end function velgrid_paint_by_file

  integer function velgrid_paint_by_obj (gobj, &
              lu_out, c_title,ovors,ivors,  &
              maxmem,                    &
              hfast , hslow ,            &
              n1_out, o1_out, d1_out,    &
              n2_out, o2_out, d2_out,    &
              n3_out, o3_out, d3_out,    &
              v0_out, xyz_out, vtyp_out  ) result(status)
    !
    ! - Arguments
    !
    type(modgrid_struct),intent(inout)  :: gobj
    character (len = *), intent (in   ) :: c_title
    character (len = *), intent (in   ) :: ovors ! output flag
    character (len = *), intent (in   ) :: ivors ! interpolation flag 
                                                 !  VELOCITY or SLOWNESS
    integer, intent (in   ) :: maxmem
    integer, intent (in   ) :: hfast                ! fast map view header
    integer, intent (in   ) :: hslow                ! slow map view header
    integer, intent (in   ) :: lu_out
    integer, intent (in   ) :: n1_out
    integer, intent (in   ) :: n2_out
    integer, intent (in   ) :: n3_out
    real,    intent (in   ) :: d1_out
    real,    intent (in   ) :: d2_out
    real,    intent (in   ) :: d3_out
    real,    intent (in   ) :: o1_out
    real,    intent (in   ) :: o2_out
    real,    intent (in   ) :: o3_out
    real,    intent (out  ) :: v0_out (n1_out, n2_out, n3_out)
    character (len = *), intent (inout):: xyz_out ! grid order
    character (len = *), intent (inout):: vtyp_out! velocity type
    !
    ! - Local variables
    !
    !
    character(len=8)  :: ftype
    character(len=4)  :: xyz_in
    integer           :: rank
    character(len=64) :: name
    character(len=64) :: pname
    character(len=16) :: punits
    integer           :: ixlab,iylab,izlab
    integer           :: hx_out               ! header for x location
    integer           :: hy_out               ! header for y location
    integer           :: ngi(3)
    real              :: ogi(3)
    real              :: dgi(3)
    integer           :: hdi(3)
    integer           :: i_err
    !
    ! - Begin velgrid_paint
    !
    status = -1

    ngi   = 1

    ftype =   modgrid_ftype(gobj)

    if(ftype == 'TRCIO' .or. ftype == 'HGRID' .or. &
       ftype == 'VOXET' .or. ftype == 'MODSPEC' .or.&
       ftype == 'CPSVEL'.or. ftype == 'SEGY' ) then
   
      xyz_in   = ' '
      !get input object grid details
      call modgrid_get_griddesc(gobj,name,pname,punits,rank,&
       hdi,ngi,ogi,dgi)
      ! identify 1-2-3 axis with X-Y-Z labels
      i_err = modgrid_xyz_order(gobj, ixlab,iylab,izlab)
      if(i_err /=0) then
        write(lu_out,*) 'velgrid_paint_by_obj: bad input order'
        goto 999
      endif
      xyz_in(ixlab:ixlab)='X'
      xyz_in(iylab:iylab)='Y'
      xyz_in(izlab:izlab)='Z'
      if(xyz_out==' ') xyz_out=xyz_in

    else
      write(lu_out,*) 'velgrid_paint_by_obj: invalid type, ftype=',ftype
      goto 999
    endif
     
    ! translate fast & slow to x & y
    if(hfast == HDR_MIDPOINT_XGRID .or. hfast== HDR_MIDPOINT_XLOC) then
      hx_out = hfast
      hy_out = hslow
    else
      hy_out = hfast
      hx_out = hslow
    endif
    i_err =  modgrid_paint_by_obj(gobj,maxmem, lu_out, ovors,ivors, &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        v0_out,  xyz_out, vtyp_out )
    if(i_err < 0) then
      write(lu_out,*) 'velgrid_paintby_obj: error calling modgrid_paint_by_obj'
      goto 999
    endif
    !
    ! - come to here to deallocate memory and return
    !   this may be from above or below
    !
    status = 0
    1999 continue
    !
    ! - deallocate memory
    !
    return
    !
  999 continue
    !
    write(lu_out,*) 'velgrid_paint_by_obj: ',trim(c_title)
    call pc_error (msg1 = "velgrid_paint_by_obj: Error")
    i_err = -1
    go to 1999
    !
  end function velgrid_paint_by_obj
  !
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !
end module velgrid_module


!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
