!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- taper.f90 --------------------------------!!
!!------------------------------- taper.f90 --------------------------------!!
!!------------------------------- taper.f90 --------------------------------!!
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
! Name       : TAPER 
! Category   : filters
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2006-10-26   by: Douglas Hanson
! Maturity   : beta
! Purpose    : Compute and apply various tapers.
! Portability: No known limitations.
!

!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!  compute and apply various tapers.
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
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!
!                  o                i     b      o
!                hello = TAPER     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    TAPER_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    TAPER_ZZZ (bbbb, indx, value)
!
!
! character(len=*)           aaaa(*) =    --> description 
! character(len=8),pointer   bbbb(:) =    --> description 
! double precision           cvar    =    --> description
! character(len=*)           msg     =    --> description 
! integer                    hello   =    --> description
! integer         ,optional  indx    =    --> description
! double precision,optional  value   =    --> description
!
!
!  apply a linear taper that varies 
!  from ax_taper_1 at ix = 1 to ax_taper_2 at ix = nx_inp
!
!       call taper_linear (ax_taper_1, ax_taper_2, nx_inp, x_inp)
!
!  real     ax_taper_1 = amplitude at start of taper,  
!                          x_inp(1     ) = ax_taper_1
!  real     ax_taper_2 = amplitude at end   of taper,  
!                          x_inp(nx_inp) = ax_taper_2
!  integer  nx_inp     = number of elements in array x_inp
!  real     x_inp  (nx_inp) = taper coefficients
!
!
! To apply a taper that varies
!   from ax_taper_1 at ix = 1 to ax_taper_2 at ix = nx_inp
!                            i        i         i      b
!       call taper_vector (i_taper, a_taper, nx_inp, x_inp)
!
!   integer   i_taper = type of taper 1 = liner,  2 = exponential,  
!                                       3 = gaussian,  4 = cosine
!   real     a_taper        = taper value
!   integer  nx_inp         = number of elements in array x_inp
!   real     x_inp (nx_inp) = taper coefficients
!
!
! To compute a single taper value
!   from ax_taper_1 at ix = 1 to 1 at ix = nx_inp
!                               i        i         i       i      b
!       call taper_compute_1 (i_taper, a_taper, ix_inp, nx_inp, x_inp)
!
!   integer  i_taper = type of taper 1 = liner,  2 = exponential,  
!                                    3 = gaussian,  4 = cosine
!   real     a_taper = taper value
!   integer  ix_inp  = taper element to compute
!   integer  nx_inp  = number of elements in array x_inp
!   real     x_inp   = taper coefficients
!
!
! To compute an exponential taper that varies
!   from a_taper at ix = 1 to 1. at ix = nx_taper
!                          i        i         i        o
!    call taper_compute (i_taper, a_taper, nx_taper, x_taper)
!
!   real     a_taper    = taper value
!   integer  i_taper    = type of taper 1 = liner,  2 = exponential,  
!                                       3 = gaussian,  4 = cosine
!   integer  nx_taper   = number of taper coefficients > =  1
!   real     x_taper    = taper coefficients
!
!
! To print a taper
!                          i        i      i        i         i        i
!    call taper_print (title, lu_out, i_taper, a_taper, nx_taper, x_taper)
!
!    character   title*(*)
!    integer     lu_out, i_taper, nx_taper
!    real        a_taper, x_taper (nx_taper)
!
!
! To apply 2d taper to array tr of lengths nx_taper, ny_taper
!   tr is at node ix, iy within a grid nx by ny
!                      i   i   i   i      i         i        i   b
!       call taper_xy (ix, iy, nx, ny, nx_taper, ny_taper, n_tr, tr)
!
!    integer   ix, iy, nx, ny, nx_taper, ny_taper, n_tr
!    real      tr (n_tr)
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
!  6. 2006-10-26  Douglas Hanson Limit do loops to size of array
!  5. 2006-04-25  B. Menger      Removed Unused Variables.
!  4. 2002-03-26  Douglas Hanson Set taper end to exact values.
!  3. 2001-07-10  Douglas Hanson cpsfcr  PRODUCTION.
!  2. 2000-05-24  Brad Kruse     Converted from old system.
!  1. 1999-12-01  Douglas Hanson Initial version.
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


module taper_module
  !
  use pc_module
  !
  implicit none
  !
  ! - set default to all routines private
  private
  !
  ! - subroutines
  !
  public :: taper_linear
  public :: taper_vector
  public :: taper_compute_1
  public :: taper_compute
  public :: taper_print
  public :: taper_xy

  character(len=100), public, save :: TAPER_IDENT = &
    '$Id: taper.f90 1.6 2006-10-26 14.42.47 Hanson beta sps $'



  !!-------------------------------- data -----------------------------------!!
  !!-------------------------------- data -----------------------------------!!
  !!-------------------------------- data -----------------------------------!!

  integer, parameter, public :: TAPER_FN_LINEAR      = 1,    &
                                TAPER_FN_EXPONENTIAL = 2,    &
                                TAPER_FN_GAUSSIAN    = 3,    &
                                TAPER_FN_COSINE      = 4

contains


  !!---------------------------- taper_linear ------------------------------!!
  !!---------------------------- taper_linear ------------------------------!!
  !!---------------------------- taper_linear ------------------------------!!


  !
  !  apply a linear taper that varies 
  !  from ax_taper_1 at ix=1 to ax_taper_2 at ix=nx_inp
  !  ax_taper_1 = amplitude at start of taper,  x_inp(1     )=ax_taper_1
  !  ax_taper_2 = amplitude at end   of taper,  x_inp(nx_inp)=ax_taper_2
  !  nx_inp     = number of elements in array x_inp
  !  x_inp      = taper coefficients
  !
  subroutine taper_linear (ax_taper_1, ax_taper_2, nx_inp, x_inp)
    !
    ! - Arguments
    !
    real,    intent (in)    :: ax_taper_1
    real,    intent (in)    :: ax_taper_2
    integer, intent (in)    :: nx_inp
    real,    intent (inout) :: x_inp (:)
    !
    ! - Local variables
    !
    integer :: nx_tmp 
    integer :: ix_inp
    real    :: x_taper
    real    :: da_dx
    !
    ! - Begin taper_linear
    !
    nx_tmp = min ( nx_inp, size(x_inp,1) )
    !
    da_dx = (ax_taper_2 - ax_taper_1) / max(1, nx_inp-1)
    !
    ! - linear taper
    !
    do_ix_inp : do ix_inp = 1 , nx_tmp
      !
      x_taper = ax_taper_1 + (ix_inp - 1) * da_dx
      x_inp(ix_inp) = x_inp(ix_inp) * x_taper
      !
    end do do_ix_inp 
    !
  end subroutine taper_linear


  !!---------------------------- taper_vector ------------------------------!!
  !!---------------------------- taper_vector ------------------------------!!
  !!---------------------------- taper_vector ------------------------------!!


  !
  ! - apply a taper that varies
  !   from ax_taper_1 at ix=1 to ax_taper_2 at ix=nx_inp
  !   i_taper = type of taper 1=liner,  2=exponential,  3=gaussian,  4=cosine
  !   a_taper = taper value
  !   nx_inp  = number of elements in array x_inp
  !   x_inp   = taper coefficients
  !
  subroutine taper_vector (i_taper, a_taper, nx_inp, x_inp)
    !
    ! - Arguments
    !
    integer, intent (in)    :: i_taper
    real,    intent (in)    :: a_taper
    integer, intent (in)    :: nx_inp
    real,    intent (inout) :: x_inp (:)
    !
    ! - Local variables
    !
    integer :: nx_tmp
    integer :: ix_inp
    real    :: alpha
    real    :: b_taper

    real    :: x_taper
    !
    nx_tmp = min ( nx_inp, size(x_inp,1) )
    !
    ! - Begin 
    !
    b_taper = max (a1 = 0.0,              &
                   a2 = min (a1 = 1.0,    &
                             a2 = a_taper))
    !
    select case (i_taper)
    case (TAPER_FN_LINEAR)
      !
      ! - linear taper
      !
      alpha = (1.0 - b_taper) / max(1, nx_inp-1)
      !
      do_ix_inp_1 : do ix_inp = 1 , nx_tmp
        !
        x_taper = max(0., min(1., b_taper+(ix_inp-1)*alpha))
        x_inp(ix_inp) = x_inp(ix_inp) * x_taper
        !
      end do do_ix_inp_1 
      !
    case (TAPER_FN_EXPONENTIAL)
      !
      ! - exponential taper
      !
      ! - exponential decay factor
      !
      b_taper = max(1.e-3, b_taper)
      alpha = -log(b_taper) / max(1, nx_inp-1)
      !
      do_ix_inp_2 : do ix_inp = 1 , nx_tmp-1
        !
        x_taper = max(0., min(1., exp((-nx_inp+ix_inp)*alpha)))
        x_inp(ix_inp) = x_inp(ix_inp) * x_taper
        !
      end do do_ix_inp_2 
      !
    case (TAPER_FN_GAUSSIAN)
      !
      ! - gaussian taper
      !
      ! - exponential decay factor
      !
      b_taper = max(1.e-3, b_taper)
      alpha = -log(b_taper) / max(1, nx_inp-1)**2
      !
      do_ix_inp_3 : do ix_inp = 1 , nx_tmp-1
        !
        x_taper = max(1., min(0., exp(-(-nx_inp+ix_inp)**2*alpha)))
        x_inp(ix_inp) = x_inp(ix_inp) * x_taper
        !
      end do do_ix_inp_3 
      !
    case (TAPER_FN_COSINE)
      !
      ! - cosine taper
      !
      alpha = acos(b_taper) / max(1, nx_inp-1)
      !
      do_ix_inp_4 : do ix_inp = 1 , nx_tmp-1
        !
        x_taper = max(0., min(1., cos((nx_inp-ix_inp)*alpha)))
        x_inp(ix_inp) = x_inp(ix_inp) * x_taper
        !
      end do do_ix_inp_4 
      !
    case default
      !
      call pc_error ("taper_vector: Error -- unknown i_taper", i_taper)
      !
    end select
    !
  end subroutine taper_vector


  !!-------------------------- taper_compute_1 -----------------------------!!
  !!-------------------------- taper_compute_1 -----------------------------!!
  !!-------------------------- taper_compute_1 -----------------------------!!

  !
  ! - compute a single taper value
  !   from ax_taper_1 at ix=1 to 1 at ix=nx_inp
  !   i_taper = type of taper 1=liner,  2=exponential,  3=gaussian,  4=cosine
  !   a_taper = taper value
  !   ix_inp  = taper element to compute
  !   nx_inp  = number of elements in array x_inp
  !   x_inp   = taper coefficients
  !
  subroutine taper_compute_1 (i_taper, a_taper, ix_inp, nx_inp, x_inp)
    !
    ! - Arguments
    !
    integer, intent (in)  :: i_taper
    real,    intent (in)  :: a_taper
    integer, intent (in)  :: ix_inp
    integer, intent (in)  :: nx_inp
    real,    intent (out) :: x_inp
    !
    ! - Local variables
    !
    real :: b_taper
    real :: x_taper

    real :: alpha
    !
    ! - Begin taper_compute_1
    !
    b_taper = max (a1 = 0.,              &
                   a2 = min (a1 = 1.,    &
                             a2 = a_taper))
    !       
    select case (i_taper)
    case (TAPER_FN_LINEAR)
      !
      ! - linear taper
      !
      alpha   = (1.0 - b_taper) / max(1, nx_inp-1)
      !
      x_taper = max (a1 = 0.0,              &
                     a2 = min (a1 = 1.0,    &
                               a2 = b_taper + (ix_inp - 1) * alpha))
    case (TAPER_FN_EXPONENTIAL)
      !
      ! - exponential taper
      !
      ! - exponential decay factor
      !
      b_taper = max(1.e-3, b_taper)
      alpha = -log(b_taper) / max(1, nx_inp-1)
      !
      x_taper = max (a1 = 0.0,              &
                     a2 = min (a1 = 1.0,    &
                               a2 = exp ((-nx_inp + ix_inp) * alpha)))
      !
    case (TAPER_FN_GAUSSIAN)
      !
      ! - gaussian taper
      !
      ! - exponential decay factor
      !
      b_taper = max (a1 = 1.e-3, a2 = b_taper)
      alpha = -log (b_taper) / max (a1 = 1, a2 = nx_inp-1)**2
      !
      x_taper = max (a1 = 1.0,               &
                     a2 =  min (a1 = 0.0,    &
                                a2 = exp (- (-nx_inp + ix_inp)**2 * alpha)))
      !
    case (TAPER_FN_COSINE)
      !
      ! - cosine taper
      !
      alpha   = acos (b_taper) / max(1, nx_inp-1)
      x_taper = max (a1 = 0.0,             &
                     a2 = min (a1 = 1.,    &
                               a2 = cos ((nx_inp - ix_inp) * alpha)))
      !
    case default
      !
      call pc_error ("taper_compute_1: Error -- unknown i_taper", i_taper)
      !
    end select
    !
    x_inp = x_taper
    !
  end subroutine taper_compute_1


  !!--------------------------- taper_compute ------------------------------!!
  !!--------------------------- taper_compute ------------------------------!!
  !!--------------------------- taper_compute ------------------------------!!

  !
  ! - compute an exponential taper that varies
  !   from a_taper at ix=1 to 1. at ix=nx_taper
  !   i_taper = type of taper 1=liner,  2=exponential,  3=gaussian,  4=cosine
  !   nx_taper   = number of taper coefficients >= 1
  !   x_taper    = taper coefficients
  !
  subroutine taper_compute (i_taper, a_taper, nx_taper, x_taper)
    !
    ! - Arguments
    !
    integer, intent (in)  :: i_taper
    real,    intent (in)  :: a_taper
    integer, intent (in)  :: nx_taper
    real,    intent (out) :: x_taper (:)
    !
    ! - Begin taper_compute
    !
    if ( nx_taper .lt. 1 ) return
    !
    x_taper = 1.0
    !
    call taper_vector (i_taper = i_taper,     &
                       a_taper = a_taper,     &
                       nx_inp  = nx_taper,    &
                       x_inp   = x_taper)
    !
    ! make sure end values are exact
    !
    x_taper (        1 ) = a_taper
    x_taper ( nx_taper ) = 1.
    !
  end subroutine taper_compute

  !!---------------------------- taper_print -------------------------------!!
  !!---------------------------- taper_print -------------------------------!!
  !!---------------------------- taper_print -------------------------------!!


  !
  ! - print a taper
  !
  subroutine taper_print (title, lu_out, i_taper, a_taper, nx_taper, x_taper)
    !
    ! - Arguments
    !
    character, intent (in) :: title*(*)
    integer,   intent (in) :: lu_out
    integer,   intent (in) :: i_taper
    real,      intent (in) :: a_taper
    integer,   intent (in) :: nx_taper
    real,      intent (in) :: x_taper (:)
    !
    ! - Local variables
    !
    integer :: ix_taper
    !
    ! - Begin taper_print
    !
    if (lu_out .lt. 0 .or. lu_out .gt. 999) return
    !
    write (lu_out, "(/, ' taper_print',  /, a,  /, ' i_taper=', i8, "   &
                   // "' a_taper=', f10.4, ' nx_taper=', i8,  /, "      &
                   // "'    ix       x_taper'  )")                      &
                  trim(title), i_taper, a_taper, nx_taper
    write (lu_out, '(1x, i8, 1x, f10.6)')     &
                    (ix_taper, x_taper (ix_taper), ix_taper = 1, nx_taper)
    !
  end subroutine taper_print


  !!------------------------------ taper_xy --------------------------------!!
  !!------------------------------ taper_xy --------------------------------!!
  !!------------------------------ taper_xy --------------------------------!!


  !
  ! - apply 2d taper to array tr of lengths nx_taper, ny_taper
  !   tr is at node ix, iy within a grid nx by ny
  !
  subroutine taper_xy (ix, iy, nx, ny, nx_taper, ny_taper, n_tr, tr)
    !
    ! - Arguments
    !
    integer, intent (in)    :: ix
    integer, intent (in)    :: iy
    integer, intent (in)    :: nx
    integer, intent (in)    :: ny
    integer, intent (in)    :: nx_taper
    integer, intent (in)    :: ny_taper
    integer, intent (in)    :: n_tr
    real,    intent (inout) :: tr (:)
    !
    ! - Local variables
    !
    integer :: jx
    integer :: jy
    integer, save :: i_call = 0   ! Legacy, unreachable trip counter
    real    :: x_scale
    real    :: y_scale
    !
    ! - Begin taper_xy
    !
    i_call = i_call + 1
    !
    jx = ix
    !
    if (ix .gt. nx/2) jx = nx - ix + 1
    !
  verify_nx_taper:    &
    if (nx .gt. nx_taper .and. jx .le. nx_taper) then
      !
      x_scale = float(jx) / float(max(1, nx_taper))
      !
    else verify_nx_taper
      !
      x_scale = 1.
      !
    end if verify_nx_taper
    !
    jy = iy
    !
    if (iy .gt. ny/2) jy = ny - iy + 1
    !
  verify_ny_taper:    &
    if (ny .gt. ny_taper .and. jy .le. ny_taper) then
      !
      y_scale = float(jy) / float(max(1, ny_taper))
      !
    else  verify_ny_taper
      !
      y_scale = 1.
      !
    end if verify_ny_taper
    !
    tr = tr * x_scale * y_scale
    !
  end subroutine taper_xy


  !!---------------------------- end of module ------------------------------!!
  !!---------------------------- end of module ------------------------------!!
  !!---------------------------- end of module ------------------------------!!


end module taper_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

