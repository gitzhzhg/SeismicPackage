!<CPS_v1 type="PROCESS"/>

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
! Name       : SETPOLY    (Set a header word if a trace is inside a polygon)
! Category   : headers
! Written    : 1999-07-21   by: Douglas Hanson
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : Set a header word if a trace is inside a polygon.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! SETPOLY sets header word HDR_SET to the value of POLY_VAL_IN for traces whose
! X, Y locations, as defined by header words HDR_X and HDR_Y respectively, are
! INSIDE the polygon.  (Traces whose X,Y locations lie on a vertex or on a line
! joining two adjacent vertices are also considered to be INSIDE the polygon.)
!
! For traces OUTSIDE the polygon:
!
!     If POLY_VAL_OUT_FLAG = NO, then header word HDR_SET is not changed from
!     its input value.
!
!     If POLY_VAL_OUT_FLAG = YES, then header word HDR_SET is set to
!     POLY_VAL_OUT.
!
!
! Defining the Polygon
!
! The polygon can be defined in two ways.
!
!     1. The vertices of the polygon can be defined by coordinate pairs as
!     specified in the GUI arrays POLY_X, POLY_Y.  Vertex coordinate values
!     must be entered sequentially in clockwise order.
!
!     2. Or you can put the coordinate pairs in a two column space delimited
!     text file where the first column contains the HDR_X value and the second
!     column the HDR_Y value.  Each row in the text file is a coordinate pair.
!     Vertex coordinate values must be entered sequentially in clockwise
!     order.  The pathname of this text file must be entered in the
!     PATHNAME_POLY parameter.
!
!     Up to 200 data entry lines are allowed in the file; text after the second
!     space on the line is ignored.  Only the first 80 characters of each line
!     are used.  Comment lines beginning with "#" are ignored if they are 
!     placed AFTER the data lines.
!
! If OPT_POLY = ARRAY_POLY, then define polygons by the GUI arrays (POLY_X, 
! and POLY_Y).
!
! If OPT_POLY = PATHNAME_POLY, then define polygons by the polygon file whose
! pathname is entered in PATHNAME_POLY.
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
! All entries in POLY_X and the first column of the text file must pertain to
! the coordinate designated by HDR_X (typically header word 7).  All entries in
! POLY_Y and the second column of the text file must pertain to the coordinate
! designated by HDR_Y (typically header word 8).
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process may alter header words.
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! HDR_X   Trace X Location           Used to locate trace.
! HDR_Y   Trace Y Location           Used to locate trace.
! HDR_SET Set for Poly location      Set with POLY_VAL_IN or POLY_VAL_OUT.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author          Description
!     ----        ------          -----------
! 12. 2006-12-04  D. Glover       Added NULLIFY statements for Intel compiler
! 11. 2006-06-06  Stoeckley       Add pc_register_array_names for SeisSpace.
! 10. 2006-01-10  B. Menger       Removed Unused Variables.
!  9. 2002-02-21  CI Burch        Documentation change only
!  8. 2002-02-18  Douglas Hanson  Use cio for read.
!  7. 2002-02-14  Douglas Hanson  Use dp inside_poly
!  6. 2002-01-07  Douglas Hanson  Add opt_poly.
!  5. 2001-11-26  Douglas Hanson  Set pathname_poly sensitive.
!  4. 2001-11-15  Douglas Hanson  Increase gui filename length.
!  3. 2001-08-01  Douglas Hanson  Fix poly_x access error.
!  2. 2001-05-16  Brad Kruse      Converted from old system.
!  1. 1999-07-21  Douglas Hanson  Original version
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
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
!
!<gui_def>
!<NS SETPOLY Process/NC=80>
!
!            SETPOLY - Set Header Word For Traces Inside A Polygon
!
!  Trace position:
!     HDR_X~~~~~~~~~= `II      HDR_Y~~~~~~~~~= `II
!
!  Header word to set:         HDR_SET~~~~~~~= `II
!
!  Values to set into HDR_SET
!     POLY_VAL_IN~~= `FFFFFFFFFFFF
!     POLY_VAL_OUT = `FFFFFFFFFFFF     POLY_VAL_OUT_FLAG = `CCCC
!
!  The Polygon:
!
!  OPT_POLY =`CCCCCCCCCC
!
!  Select PATHNAME_POLY [PATHNAME_POLY]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!   -- or --
!  POLY_X    POLY_Y
!  `FFFFFFFFF`FFFFFFFFF
!  `FFFFFFFFF`FFFFFFFFF
!  `FFFFFFFFF`FFFFFFFFF
!  `FFFFFFFFF`FFFFFFFFF
!  `FFFFFFFFF`FFFFFFFFF
!
!<PARMS POLY_X_ARRAYSET[/XST/YST]>
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing x coordinate values. </Tip>
! Default = 7
! Allowed = 1-NWIH
!
!</Help>
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing y coordinate values. </Tip>
! Default = 8
! Allowed = 1-NWIH
!
!</Help>
!
!<Help KEYWORD="HDR_SET">
!<Tip> Header word set to POLY_VAL_IN if a trace is inside the polygon. </Tip>
! Default = 48
! Allowed = 1-NWIH
! SETPOLY sets header word HDR_SET = POLY_VAL_IN for traces whose X, Y
! locations, as defined by header words HDR_X and HDR_Y respectively, are
! INSIDE the polygon.  (Traces whose X,Y locations lie on a vertex or on a line
! joining two adjacent vertices are also considered INSIDE the polygon.)
!
! For traces OUTSIDE the polygon:
!
!     If POLY_VAL_OUT_FLAG = NO, then header word HDR_SET is not changed from
!     its input value.
!
!     If POLY_VAL_OUT_FLAG = YES, then header word HDR_SET is set to
!     POLY_VAL_OUT.
!</Help>
!
!<Help KEYWORD="POLY_VAL_IN">
!<Tip> Value placed in HDR_SET for traces inside polygon. </Tip>
! Default = 1.0
! Allowed = real
! SETPOLY sets header word HDR_SET = POLY_VAL_IN for traces whose X, Y
! locations, as defined by header words HDR_X and HDR_Y respectively, are
! INSIDE the polygon.  (Traces whose X,Y locations lie on a vertex or on a line
! joining two adjacent vertices are also considered INSIDE the polygon.)
!</Help>
!
!<Help KEYWORD="POLY_VAL_OUT">
!<Tip> Value placed in HDR_SET for traces outside polygon. </Tip>
! Default = 0.0
! Allowed = real
! For traces OUTSIDE the polygon:
!
!     If POLY_VAL_OUT_FLAG = NO, then header word HDR_SET is not changed from
!     its input value.
!
!     If POLY_VAL_OUT_FLAG = YES, then header word HDR_SET is set to
!     POLY_VAL_OUT.
!</Help>
!
!<Help KEYWORD="POLY_VAL_OUT_FLAG">
!<Tip> Whether to set HDR_SET for traces outside the polygon. </Tip>
! Default = YES
! Allowed = YES
! Allowed = NO
! HDR_SET will be set to POLY_VAL_OUT if a trace is outside the polygon, only
! if POLY_VAL_OUT_FLAG = YES.
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_POLY">
!<Tip> polygon path name. </Tip>
!</Help>
!
!<Help KEYWORD="OPT_POLY">
!<Tip> Whether to use polygon arrays or polygon file. </Tip>
! Default = ARRAY_POLY
! Allowed = ARRAY_POLY
! Allowed = PATHNAME_POLY
!
! If OPT_POLY = ARRAY_POLY, then define polygons by arrays on GUI (POLY_X, 
! and POLY_Y).
!
! If OPT_POLY = PATHNAME_POLY, then define polygons by polygon file whose
! pathname is entered in PATHNAME_POLY.
!
!</Help>
!
!<Help KEYWORD="PATHNAME_POLY">
!<Tip> Name of the text file defining polygon. </Tip>
! Default = NONE
! Allowed = char
! You can put the coordinate pairs of vertices in a two column space delimited
! text file where the first column contains the HDR_X value and the second
! column the HDR_Y value.  Each row in the text file is a coordinate pair.
! Vertex coordinate values must be entered sequentially in clockwise
! order.  The pathname of this text file must be entered in the PATHNAME_POLY
! parameter.
! 
! Up to 200 data entry lines are allowed in the file; text after the second
! space on the line is ignored.  Only the first 80 characters of each line are
! used.  Comment lines beginning with "#" are ignored if they are placed AFTER
! the data lines.
!</Help>
!
!<Help KEYWORD="POLY_X">
!<Tip> Array of polygon vertex x coordinate values, linked with POLY_Y. </Tip>
! Default = -
! Allowed = real (linked array)
! Vertex coordinate values must be entered sequentially in clockwise order.  
! POLY_X is limited to 200 entries.
!</Help>
!
!<Help KEYWORD="POLY_Y">
!<Tip> Array of polygon vertex y coordinate values, linked with POLY_X.</Tip>
! Default = -
! Allowed = real (linked array)
! Vertex coordinate values must be entered sequentially in clockwise order.  
! POLY_Y is limited to 200 entries.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module setpoly_module
  !
  ! - Module references
  !
  use pc_module
  use pathchoose_module
  use cio_module
  !
  use named_constants_module,    &
        path_len => FILENAME_LENGTH
  !
  use mem_module            ! to use the memory allocation module.
  !
  use pathcheck_module, only:    &
        pathcheck,               &
        pathcheck_empty,         &
        path_incomplete,         &
        path_invalid,            &
        path_valid,              &
        path_unspecified
  !
  use getlun_module, only: getlun
  use cmpi_module
  !
  use matfun_module, only:    &
        matfun_inside_polygon
  !
  implicit none
  !
  private
  !
  public :: setpoly_create
  public :: setpoly_initialize
  public :: setpoly_update
  public :: setpoly_delete
!<execute_only>
  public :: setpoly            ! main execution (trace processing) routine.
  public :: setpoly_wrapup
!</execute_only>

  character (len = 100), public, save :: SETPOLY_IDENT = &
    '$Id: setpoly.f90,v 1.12 2006/12/04 13:29:56 Glover prod sps $'

  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!

  type, private :: stat_struct
    integer :: count
    integer :: first
    integer :: last
  end type stat_struct
  !
  integer, parameter :: max_poly_vertices = 200
  !
  type, public :: setpoly_struct
    !
    private
    !
    logical                    :: skip_wrapup      ! wrapup flag.
    logical                    :: read_file_flag
    !
    integer                    :: nwih             ! Number of header words
    !
    integer                    :: hdr_x
    integer                    :: hdr_y
    integer                    :: hdr_set
    double precision           :: poly_val_in
    double precision           :: poly_val_out
    logical                    :: poly_val_out_flag
    character (len=13)         :: opt_poly      ! polygon option
    !
    type(pathchoose_struct), pointer :: pathname_poly_button
    character (len = path_len) :: pathname_poly      ! pc - char
    integer                    :: num_poly_vertices
    double precision           :: poly_x (max_poly_vertices)
    double precision           :: poly_y (max_poly_vertices)
    !
    integer                    :: num_input_tr
    type (stat_struct)         :: inside
    type (stat_struct)         :: outside
    !
  end type setpoly_struct

  !!---------------------------- interfaces -------------------------------!!
  !!---------------------------- interfaces -------------------------------!!
  !!---------------------------- interfaces -------------------------------!!


  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!


  type (setpoly_struct), pointer, save :: object      ! needed for traps.

    integer,    parameter :: opt_poly_n = 2
    character(len=13),save :: opt_poly_c(opt_poly_n) &
    = (/ 'ARRAY_POLY   ', 'PATHNAME_POLY' /)

contains


  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!

  subroutine setpoly_create (obj)
    !
    ! - Arguments
    !
    type (setpoly_struct), pointer :: obj       ! arguments
    !
    ! - Begin setpoly_create
    !
    allocate (obj)
    nullify (obj%pathname_poly_button) ! jpa
    !
    call pathchoose_create (obj%pathname_poly_button, 'PATHNAME_POLY', '.poly')
    !
    call setpoly_initialize (obj)
    !
  end subroutine setpoly_create


  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!

  subroutine setpoly_delete (obj)
    !
    ! - Arguments
    !
    type (setpoly_struct), pointer :: obj       ! arguments
    !
    ! - Begin setpoly_delete
    !

!<execute_only>
    call setpoly_wrapup (obj)
!</execute_only>

           if ( associated ( obj%pathname_poly_button ) )  &
    call pathchoose_delete ( obj%pathname_poly_button )

    deallocate(obj)
    !
  end subroutine setpoly_delete


  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!


  subroutine setpoly_initialize (obj)
    !
    ! - Arguments
    !
    type (setpoly_struct), intent (inout) :: obj       ! arguments
    !
    ! - Begin setpoly_initialize
    !
    !
    obj%hdr_set           = 48
    obj%hdr_x             = 7
    obj%hdr_y             = 8
    obj%poly_x            = 0.0
    obj%poly_y            = 0.0
    obj%num_poly_vertices = 0
    obj%opt_poly          = 'PATHNAME_POLY'
    obj%pathname_poly     = pathcheck_empty
    obj%poly_val_in       = 1.0d0
    obj%poly_val_out      = 0.0d0
    obj%poly_val_out_flag = .true.
    obj%read_file_flag    = .false.
    !
    call setpoly_update (obj)
    !
  end subroutine setpoly_initialize


  !!-------------------------- start of update -----------------------------!!
  !!-------------------------- start of update -----------------------------!!
  !!-------------------------- start of update -----------------------------!!

  subroutine setpoly_update (obj)
    !
    ! - Arguments
    !
    type (setpoly_struct), intent (inout), target :: obj
    !
    ! - Local variables
    !
    integer :: action
    integer :: indx

    integer :: num_x
    integer :: num_y
    integer :: path_status
    integer :: status
    !
    ! - Begin setpoly_update
    !
    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!

    call pc_get_global (keyword = 'nwih', scalar = obj%nwih)
    !
    if (pathchoose_update (obj%pathname_poly_button, obj%pathname_poly)) return

      call pc_register_array_names ("poly_x_arrayset", (/  &
                                    "poly_x",              &
                                    "poly_y" /))

    call pc_get (keyword = 'HDR_SET',       scalar = obj%hdr_set)
    call pc_get (keyword = 'HDR_X',         scalar = obj%hdr_x)
    call pc_get (keyword = 'HDR_Y',         scalar = obj%hdr_y)
    call pc_get (keyword = 'OPT_POLY',      scalar = obj%opt_poly)
    call pc_get (keyword = 'PATHNAME_POLY', scalar = obj%pathname_poly)
    call pc_get (keyword = 'POLY_VAL_IN',   scalar = obj%poly_val_in)
    call pc_get (keyword = 'POLY_VAL_OUT',  scalar = obj%poly_val_out)
    call pc_get (keyword = 'POLY_VAL_OUT_FLAG',     &
                 scalar  = obj%poly_val_out_flag)
    !
    num_x = obj%num_poly_vertices
    call pc_get (keyword   = 'POLY_X',         &
                 array     = obj%poly_x,     &
                 nelements = num_x)
    !
    num_y = obj%num_poly_vertices
    call pc_get (keyword   = 'POLY_Y',         &
                 array     = obj%poly_y,     &
                 nelements = num_y)

    !
    if (num_x /= num_y) then
      call pc_error ('POLY_X, TPOLY_Y arrays have different lengths')
      obj%num_poly_vertices = min (a1 = num_x,     &
                                a2 = num_y)
    else
      obj%num_poly_vertices = num_x
    end if


    !!----------------------- verify parameters ----------------------------!!
    !!----------------------- verify parameters ----------------------------!!
    !!----------------------- verify parameters ----------------------------!!

    !
    ! - Check HDR_SET
    !
    if (obj%hdr_set < 1 .or. obj%hdr_set > obj%nwih) then
      !
      call pc_error ("SETPOLY: HDR_SET is not between 1 - ", obj%nwih,    &
                     ". Found ", obj%hdr_set)
      obj%hdr_set = 48
      !
    end if

    !
    ! - Check HDR_X
    !
    if (obj%hdr_x < 1 .or. obj%hdr_x > obj%nwih) then
      !
      call pc_error ("SETPOLY: HDR_X is not between 1 - ", obj%nwih,    &
                     ". Found ", obj%hdr_x, ".  Setting to 7.")
      obj%hdr_x = 7
      !
    end if

    !
    ! - Check HDR_Y
    !
    if (obj%hdr_y < 1 .or. obj%hdr_y > obj%nwih) then
      !
      call pc_error ("SETPOLY: HDR_Y is not between 1 - ", obj%nwih,    &
                     ". Found ", obj%hdr_y, ".  Setting to 8.")
      obj%hdr_x = 8
      !
    end if

    !
    ! - POLY_VAL_IN is not checked.
    !

    !
    ! - POLY_VAL_OUT is not checked.
    !

    !
    ! - POLY_VAL_OUT_FLAG is not checked.
    !


    !
    ! - Check PATHNAME_POLY
    !
    call pathcheck (keyword  = 'PATHNAME_POLY',        &
                    pathname = obj%pathname_poly,    &
                    status   = path_status)
    !

  check_pathname_diag:   &
    if (path_status == path_invalid) then
      !
      call pc_error (msg1 = 'SETPOLY:  Invalid pathname for PATHNAME_POLY (' &
                            // trim (obj%pathname_poly) // ')')
      !
    else if (path_status == path_incomplete) then
      !
      call pc_error (msg1 = 'SETPOLY:  Incomplete pathname for '    &
                            // 'PATHNAME_POLY ('                    &
                            // trim (obj%pathname_poly) // ').')
      call pc_error (msg1 = '        Path ends with a directory not a file.')
      !
    end if check_pathname_diag
    !
    obj%read_file_flag = path_status == PATH_VALID
    !
    ! - Check POLY_X
    !
    if (pc_verify_element  (keyword = 'POLY_X',    &
                            indx    = indx,        &
                            action  = action)) then
      if ((action == PC_INSERT)      &
        .and. (obj%poly_x (indx) == fnil)) obj%poly_x (indx) = 0.0
    end if

    !
    ! - Check POLY_Y
    !
    if (pc_verify_element  (keyword = 'POLY_Y',    &
                            indx    = indx,        &
                            action  = action)) then
      if ((action == PC_INSERT)      &
        .and. (obj%poly_y (indx) == fnil)) obj%poly_y (indx) = 0.0
    end if

    !
    ! - Adjust the GUI for filename OR x-y pairs
    !
    call setpoly_sensitive (obj)
    !

    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!

    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!


    call pc_put_control (keyword = 'ntapes',    &
                         scalar  = 0)             ! default 0
    call pc_put_control ('need_request', .false.)       ! default .false..
    call pc_put_control ('need_label',   .false.)       ! default .false.
    call pc_put_control ('twosets',      .false.)       ! default .false
    call pc_put_control ('nscratch',     0)             ! default 0
    call pc_put_control ('nstore',       0)             ! default 0
    call pc_put_control ('iftd',         .false.)       ! default .false.
    call pc_put_control ('ndisk',        0)             ! default 0
    call pc_put_control ('setup_only',   .false.)       ! default .false.
    !
    call pc_put_global (keyword = 'nwih', scalar = obj%nwih)
    !
    call pc_put_options_field ('opt_poly',      opt_poly_c,     opt_poly_n     )
    !
    call pc_put (keyword = 'HDR_SET',       scalar = obj%hdr_set)
    call pc_put (keyword = 'HDR_X',         scalar = obj%hdr_x)
    call pc_put (keyword = 'HDR_Y',         scalar = obj%hdr_y)
    call pc_put (keyword = 'OPT_POLY',      scalar = obj%opt_poly)
    call pc_put (keyword = 'PATHNAME_POLY', scalar = obj%pathname_poly)
    call pc_put (keyword = 'POLY_VAL_IN',   scalar = obj%poly_val_in)
    call pc_put (keyword = 'POLY_VAL_OUT',  scalar = obj%poly_val_out)
    call pc_put (keyword = 'POLY_VAL_OUT_FLAG',     &
                 scalar  = obj%poly_val_out_flag)
    !
    call pc_put (keyword   = 'POLY_X',         &
                 array     = obj%poly_x,     &
                 nelements = obj%num_poly_vertices)
    !
    call pc_put (keyword   = 'POLY_Y',         &
                 array     = obj%poly_y,     &
                 nelements = obj%num_poly_vertices)

    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!

!<execute_only>

    if (pc_do_not_process_traces()) return
    obj%skip_wrapup = .false.


    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    !
    ! - Read X-Y's from file
    !
    if (obj%read_file_flag) then
      !
      call setpoly_read_polygon (obj   = obj,    &
                                 i_err = status)
      !
      if (status /= 0) then
        call pc_error ("Setpoly_update: Error returned from "    &
                       // "setpoly_read_polygon ", status)
      end if
      !
    end if
    !
    ! print the polygon
    !
    !call setpoly_print_polygon ( obj = obj , &
    !                             c_title = 'before setpoly_close_polygon')
    !
    if (obj%num_poly_vertices <= 0) then
      call pc_error ('Setpoly_update:  No x/y points entered.')
    else
      !
      ! - make sure the polygon is closed
      !
      call setpoly_close_polygon (obj = obj)
    end if
    !
    ! print the polygon
    !
    call setpoly_print_polygon ( obj = obj , &
                                 c_title = 'after setpoly_close_polygon')
    !
    ! - Clear trace counts
    !
    obj%num_input_tr    = 0
    obj%inside%count  = 0
    obj%inside%first  = 0
    obj%inside%last   = 0
    obj%outside%count = 0
    obj%outside%first = 0
    obj%outside%last  = 0
    !
!</execute_only>


    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!

    !
  end subroutine setpoly_update


  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!


  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!

!<execute_only>

  subroutine setpoly (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type (setpoly_struct), intent (inout) :: obj
    integer,               intent (inout) :: ntr
    double precision,      intent (inout) :: hd (:, :)
    real,                  intent (in)    :: tr (:, :)
    !
    ! - variables
    !

    integer :: i_inp


    !
    logical :: inside_flag
    !
    ! - Begin setpoly
    !
    ! - output the next trace
    !
  loop_thru_traces:    &
    do i_inp = 1 , ntr
      !
      ! - increment the input trace counter
      !
      obj%num_input_tr = obj%num_input_tr + 1
      !
      ! - set i_insde = 1 if the trace is inside the polygon
      !
      call matfun_inside_polygon (x0     = hd (obj%hdr_x, i_inp), &
                                  y0     = hd (obj%hdr_y, i_inp), &
                                  n_poly = obj%num_poly_vertices, &
                                  x_poly = obj%poly_x,            &
                                  y_poly = obj%poly_y,            &
                                  inside = inside_flag              )
      !
      ! - count the trace inside
      !
    check_inside_polygon:    &
      if (inside_flag) then
        !
        obj%inside%count = obj%inside%count + 1      ! total inside
        !
        if (obj%inside%count .eq. 1) then
          obj%inside%first = obj%num_input_tr          ! first inside
        end if
        !
        obj%inside%last = obj%num_input_tr             ! last inside
        hd (obj%hdr_set, i_inp) = obj%poly_val_in
        !
      else check_inside_polygon
        !
        obj%outside%count = obj%outside%count + 1    ! total outside
        !
        if (obj%outside%count .eq. 1) then
          obj%outside%first = obj%num_input_tr         ! first outside
        end if
        !
        obj%outside%last = obj%num_input_tr            ! last outside
        !
        if (obj%poly_val_out_flag) then
          hd (obj%hdr_set, i_inp) = obj%poly_val_out
        end if
        !
      end if check_inside_polygon
      !
    end do loop_thru_traces
    !
    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      call setpoly_wrapup (obj)
    end if
    !
  end subroutine setpoly


  !!------------------------ setpoly_read_polygon --------------------------!!
  !!------------------------ setpoly_read_polygon --------------------------!!
  !!------------------------ setpoly_read_polygon --------------------------!!

  !
  ! - read the polygon x,y values from a disk file
  !
  subroutine setpoly_read_polygon (obj, i_err)
    !
    ! - Arguments
    !
    type (setpoly_struct), intent (inout) :: obj
    integer,               intent (out)   :: i_err
    !
    ! - Local variables
    !
    integer                               :: lu_poly
    integer                               :: n_card
    real                                  :: x0
    real                                  :: y0
    character(len=160)                    :: crd_160
    !
    ! - Begin setpoly_read_polygon
    !
    i_err = 0
    !
!!    if (cmpi_i_pel() .eq. 0) then
      !
      call pc_print ("setpoly_read_polygon: Reading polygon x,ys from "    &
                     // "disk file.  PATHNAME_POLY="                       &
                     // trim (obj%pathname_poly))
      !
      obj%num_poly_vertices = 0
      !
      ! - open the file
      !
      lu_poly = cio_fopen ( filename=obj%pathname_poly, mode='r' )
      !
      if (lu_poly .le. 0) then
        call pc_error ("setpoly_read_polygon: error in "    &
                       // trim (obj%pathname_poly)        &
                       // " during open")
        return
      end if
      !
      ! - read each card image
      !
      call  cio_frewind ( lu_poly )
      !
    loop_thru_cards:    &
      do 
        !
        n_card = cio_fgetline ( string = crd_160, imax = 160, unit = lu_poly )
        !
        if ( n_card .lt. 1 ) exit loop_thru_cards
        !
        read ( crd_160, *, end=2, iostat=i_err ) x0,y0
        !
        if (i_err /= 0) exit loop_thru_cards
        !
        obj%num_poly_vertices = min (a1 = obj%num_poly_vertices + 1,   &
                                     a2 = max_poly_vertices)
        !
        obj%poly_x (obj%num_poly_vertices) = x0
        obj%poly_y (obj%num_poly_vertices) = y0
        !
      end do loop_thru_cards
      !
    2 continue
      !
      i_err = cio_fclose ( unit=lu_poly )
      !
!!    end if    ! if (cmpi_i_pel() .eq. 0) then
    !
    i_err = 0
!!    call cmpi_bcast_i(0, 1, i_err)
    !
    if (i_err == 0) then
      !
      ! - make sure the polygon is closed
      !
      call setpoly_close_polygon (obj = obj)
      !
      ! - broadcast the results
      !
      !call cmpi_bcast_i (0, 1, obj%num_poly_vertices)
      !call cmpi_bcast_r (0, obj%num_poly_vertices, obj%poly_x)
      !call cmpi_bcast_r (0, obj%num_poly_vertices, obj%poly_y)
      !
    else
      !
      call pc_error ("setpoly_read_polygon: error during inter-pe"    &
                     // " broadcast of X-Y's ", i_err)
      i_err = -1
      !
    end if
    !
  end subroutine setpoly_read_polygon


  !!----------------------- setpoly_close_polygon --------------------------!!
  !!----------------------- setpoly_close_polygon --------------------------!!
  !!----------------------- setpoly_close_polygon --------------------------!!

  !
  ! - close the polygon x,y points
  !
  subroutine setpoly_close_polygon (obj)
    !
    ! - Arguments
    !
    type (setpoly_struct), intent (inout) :: obj
    !
    ! - Local variables
    !
    integer :: last
    !
    ! - Begin setpoly_close_polygon
    !
  verify_points_present:    &
    if (obj%num_poly_vertices .ge. 1) then
      !
      last = obj%num_poly_vertices
      !
      ! - The end points are different, add a point
      !
    check_if_ends_match:    &
      if ((abs (obj%poly_x (1) - obj%poly_x (last)) > .001)    &
          .or. (abs (obj%poly_y (1) - obj%poly_y (last)) > .001)) then
        !
        if (cmpi_i_pel () .eq. 0) then
          !
          call pc_print ("Setpoly_Close_Polygon: adding a point to "    &
                         // "close the polygon")
          !
        end if
        !
        obj%num_poly_vertices = min (a1 = last + 1,    &
                                       a2 = max_poly_vertices)
        !
        obj%poly_x (obj%num_poly_vertices) = obj%poly_x (1)
        obj%poly_y (obj%num_poly_vertices) = obj%poly_y (1)
        !
      end if check_if_ends_match
      !
    end if verify_points_present
    !
  end subroutine setpoly_close_polygon
  !
  ! - print the polygon x,y points
  !
  subroutine setpoly_print_polygon ( obj, c_title )
    !
    ! - Arguments
    !
    type (setpoly_struct), intent (inout) :: obj
    character(len=*),      intent (in   ) :: c_title
    !
    ! - Local variables
    !
    integer :: i
    !
    ! - Begin setpoly_print_polygon
    !
    write ( pc_get_lun(), ' ( &
    & /, " setpoly_print_polygon ipn=", i8, &
    & /, a, &
    & /, " number of vertices =", i8, &
    & /, " index  x  y " &
    & )') &
    & pc_get_ipn(), &
    trim ( c_title ), &
    obj%num_poly_vertices 
    !
    write ( pc_get_lun(), ' ( &
    & 1x, i8, 1x, g12.6, 1x, g12.6 &
    & )') &
    & ( i, obj%poly_x(i), obj%poly_y(i), i = 1 , obj%num_poly_vertices )
    !
    return
    !
  end subroutine setpoly_print_polygon

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

  subroutine setpoly_wrapup (obj)
    !
    ! - Arguments
    !
    type (setpoly_struct), intent (inout) :: obj       ! arguments
    !
    ! - Begin
    !
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    !
    if (cmpi_i_pel() .eq. 0) then
      !
      call pc_print ("Setpoly: pe = ", cmpi_i_pel())
      call pc_print ("Setpoly: total input traces", obj%num_input_tr)
      call pc_print ("Setpoly: traces inside the polygon",            &
                     obj%inside%count)
      call pc_print ("Setpoly: first trace  inside the polygon",      &
                     obj%inside%first)
      call pc_print ("Setpoly: last  trace  inside the polygon",      &
                     obj%inside%last)
      call pc_print ("Setpoly: traces outside the polygon",           &
                     obj%outside%count)
      call pc_print ("Setpoly: first trace  outside the polygon",     &
                     obj%outside%first)
      call pc_print ("Setpoly: last  trace  outside the polygon",     &
                     obj%outside%last)
      !
    end if
    !
  end subroutine setpoly_wrapup

  subroutine setpoly_sensitive (obj)
    !
    type (setpoly_struct), intent (inout), target :: obj
    !
    ! - Adjust the GUI for filename OR x-y pairs
    !
    xxif_opt_poly : &
    if ( string_upper_compare ( obj%opt_poly, 'array_poly' ) ) then
      !
      call pc_put_sensitive_array_flag (keyword   = 'POLY_X',    &
                                        sensitive = .true.)
      call pc_put_sensitive_array_flag (keyword   = 'POLY_Y',    &
                                        sensitive = .true.)
      call pc_put_sensitive_field_flag (keyword   = 'PATHNAME_POLY', &
                                        sensitive = .false.)
      call pc_put_sensitive_field_flag (keyword   = 'SELECT_PATHNAME_POLY',  &
                                        sensitive = .false.)
      !
      obj%read_file_flag = .false.
      !
    else xxif_opt_poly 
      !
      call pc_put_sensitive_array_flag (keyword   = 'POLY_X',    &
                                        sensitive = .false.)
      call pc_put_sensitive_array_flag (keyword   = 'POLY_Y',    &
                                        sensitive = .false.)
      call pc_put_sensitive_field_flag (keyword   = 'PATHNAME_POLY', &
                                        sensitive = .true.)
      call pc_put_sensitive_field_flag (keyword   = 'SELECT_PATHNAME_POLY',  &
                                        sensitive = .true.)
      !
      obj%read_file_flag = .true.
      !
    end if xxif_opt_poly 
    !
    return
    !
  end subroutine setpoly_sensitive 

!</execute_only>


  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!


end module setpoly_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

