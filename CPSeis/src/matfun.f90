!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- matfun.f90 --------------------------------!!
!!------------------------------- matfun.f90 --------------------------------!!
!!------------------------------- matfun.f90 --------------------------------!!

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
! Name       : MATFUN 
! Category   : math
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2007-01-03   by: Douglas Hanson Clean up bandps usage.
! Maturity   : production
! Purpose    : various mathematical operations.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! various mathematical operations.
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
!                  o                i     b      o
!                hello = MATFUN     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    MATFUN_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    MATFUN_ZZZ (bbbb, indx, value)
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
! To create and delete an FFT object:
!                    o         i     i     i      opt
!    i_err =  fft_create (obj, sign, size, ctype, opt_scale)
!            - OR for F77 access -
!     ui   =  fft_new( sign, size, ctype )
!
!    ui  .... is a unique integer returned by the fft_new call
!    obj .... is a pointer to type(fft_struct)
!    ctype .. is one of the following strings
!            'ctoc' = complex to complex fft
!            'rtoc' = real to complex fft
!            'ctor' = complex to real fft
!    sign ... is a negative or positive integer
!    size ... can be a mixed radix number (a factor of small primes)
!            size >= 2
!    opt_scale. Optional argument to apply a user defined scaling to
!             the output buffer when the fft is applied.
!                   i
!    call fft_delete (obj)
!            - OR for F77 access -
!    call fft_del (ui)

!
! To set n elements of a real array x to values x0 + (i-1) * dx for i=1, n
!                        i  o   i   i
!    call matfun_line_r (n, x, x0, dx)
!
!    real      x0, dx, x (n)
!    integer   n
!
!
! To set n elements of a integer array x to values x0 + (i-1) * dx for i=1, n
!                        i  o  i   i 
!    call matfun_line_i (n, x, x0, dx)
!
!    integer    x0, dx, n, x (n)
!
!
! To set n elements of a real array x to values x0 + (i-1) * dx for i=1, n
!                        i  o  i   i
!    call matfun_line_c (n, x, x0, dx)
!
!    complex  x0, dx, x (n)
!    integer  n
!
!
! To set n elements of a double precision array x 
!    to values x0 + (i-1) * dx for i=1, n
!                        i  o  i   i
!    call matfun_line_d (n, x, x0, dx)
!
!    double precision  x0, dx, x (n)
!    integer           n
!
!
! To get min and max values of n1 elements of 1d real array x0
!                              o      o    i   i
!    call matfun_min_max_r1 (x_min, x_max, n1, x0)
!
!    real     x_min, x_max, x0(n1)
!    integer  n1
!
!
! To get min and max values of n1, n2 elements of 2d real array x0
!                              o      o    i   i   i
!    call matfun_min_max_r2 (x_min, x_max, n1, n2, x0)
!
!    real      x_min, x_max, x0 (n1, n2)
!    integer   n1,  n2
!
!
! To get min and max values of n1, n2, n3 elements of 3d real array x0
!                              o      o    i   i   i   i
!    call matfun_min_max_r3 (x_min, x_max, n1, n2, n3, x0)
!
!    real     x_min, x_max, x0 (n1, n2, n3)
!    integer  n1, n2, n3
!
!
! To get min and max values of n1 elements of 1d integer array x0
!                              o      o    i   i
!    call matfun_min_max_i1 (x_min, x_max, n1, x0)
!
!    integer   x_min, x_max, n1, x0 (n1)
!
!
! To get min and max values of n1, n2 elements of 2d integer array x0
!                              o      o    i   i   i
!    call matfun_min_max_i2 (x_min, x_max, n1, n2, x0)
!
!    integer   x_min, x_max, n1, n2, x0 (n1, n2)
!
!
! To get min and max values of n1, n2, n3 elements of 3d integer array x0
!                              o      o    i   i   i   i
!    call matfun_min_max_i3 (x_min, x_max, n1, n2, n3, x0)
!
!    integer   x_min, x_max, n1, n2, n3, x0 (n1, n2, n3)
!
!
! To return the min and max differences between adjacent values of x
!                                o      o    i   i 
!    call matfun_min_max_diff (x_min, x_max, nx, x)
!
!    real      x_min, x_max, x (nx)
!    integer   nx
!
!
! To normalize nx elements of real array x by real value x0
!                             i   b  i 
!    call matfun_normalize_r (nx, x, x0)
!
!    real      x0      = value to normalize to
!    integer   nx      = number of elements in array x
!    real      x (nx)  = array to normalize
!
!
! To convolve x1 into x2  x3 = x1 * x2, may be inplace
!                          i   i   i   i    o
!    call matfun_convolve (n1, x1, n2, x2, x3)
!
!    integer   n1, n2
!    real      x1 (n1), x2 (n2), x3 (n2)
!
!
! To invert n1 values of 1d real array x, avoid divide by 0.
!                           i   b
!    call matfun_invert_1d (n1, x)
!
!    integer   n1
!    real      x (n1)
!
!
! To invert n1, n2 values of 2d real array x, avoid divide by 0.
!                           i   i   b
!    call matfun_invert_2d (n1, n2, x)
!
!    integer   n1, n2
!    real      x (n1, n2)
!
!
! To invert n1, n2, n3  values of 3d real array x, avoid divide by 0.
!                           i   i   i   b
!    call matfun_invert_3d (n1, n2, n3, x)
!
!    integer   n1, n2, n3
!    real      x (n1, n2, n3)
!
!
! To make sure an array is monotonicly 
!   increasing, i_dir>=0, or decreasing, i_dir<0
!                                i    i  i    o
!    call matfun_check_change (i_dir, n, x, i_err)
!
!    integer  i_dir, n, i_err
!    real     x (n)
!
!
! To determine the regularity of a set of numbers
!                            o   o   o   i  i    o
!    call matfun_array_size (nx, x0, dx, n, x, i_err)
!
!    integer   nx, n, i_err
!    real      x0, dx, x (n)
!
!
! To compute the normal to a line defined by the points (x1, y1) ,  (x2, y2)
!   which passes through the point (x0, y0)
!   this point will be (xn, yn)
!                                 o   o   i   i   i   i   i   i
!    call matfun_normal_to_line ( xn, yn, x0, y0, x1, y1, x2, y2)
!
!    real   xn, yn, x0, y0, x1, y1, x2, y2
!
!
! To compute the normal to a plane defined by the points
!   (x1, y1, z1) ,  (x2, y2, z2) ,  (x3, y3, z3)
!    which passes through the point (x0, y0, z0)
!    this point will be (xn, yn, zn)
!                                 o   o   o   i   i   i
!    call matfun_normal_to_plane (xn, yn, zn, x0, y0, z0, &
!                                 i   i   i   i   i   i   i   i   i
!                                 x1, y1, z1, x2, y2, z2, x3, y3, z3)
!
!    real   xn, yn, zn -- Intersect with defined normal line and the plane
!    real   x0, y0, z0 -- Defining point on a line normal to the plane
!    real   x1, y1, z1 -- Defining point on plane
!    real   x2, y2, z2 -- Second defining point on plane
!    real   x3, y3, z3 -- Third defining point on plane
!
!
! To determine if a point x0, y0 is inside a closed polygon x, y
!   inside inside = .true.  outside inside = .false.
!                                i   i     i       i       i       o
!    call matfun_inside_polygon (x0, y0, n_poly, x_poly, y_poly, inside)
!
!    real      x0, y0, x_poly(n_poly), y_poly(n_poly)
!    integer   n_poly
!    logical   inside
!
!
!  filter a time trace
!                             i              i              
!    call matfun_filter (freq_low_none, freq_low_full, &
!                             i               i              i       
!                        freq_high_full, freq_high_none, freq_phase, &
!                           i       i       i       b      o
!                        nt_inp, t0_inp, dt_inp, tr_inp, i_err)
!
!    real      t0_inp, dt_inp -- Trace start, length in seconds
!    real      freq_low_none, freq_low_full, freq_high_none -- filter, in Hertz
!    real      freq_high_full, freq_phase, tr_inp(nt_inp)
!    integer   nt_inp, i_err
!
!
! To  take real to complex fft of fft_inp into fft_out
!    fft_sign 
!    fft_size = number of points in trace
!    fft_inp = real input array to be transformed
!    fft_out = complex output array
!
! To this does no scaling - scale by 1./fft_size during inverse fft
!
!    call matfun_fft_rc (fft_sign, fft_size, fft_inp, fft_out)
!
!    integer   fft_sign               = -1 forward transform,
!                                        1 - inverse transform
!    integer   fft_size               = number of points in trace
!    real      fft_inp (fft_size)     = real input array to be transformed
!    complex   fft_out (fft_size/2+1) = complex output array
!
!
! To take complex to real fft of fft_inp into fft_out
!
!    This does no scaling - scale by 1./fft_size during inverse fft
!
!    call matfun_fft_cr (fft_sign, fft_size, fft_inp, fft_out)
!
!    integer   fft_sign               = -1 forward transform, 
!                                        1 - inverse transform
!    integer   fft_size               = number of points in trace
!    complex   fft_inp (fft_size/2+1) = complex input array to be transformed
!    real      fft_out (fft_size)     = real output array
!
!
! To take real to complex fft of fft_inp into fft_out
!
!   fft_sign = -1 forward transform , 1 - inverse transform
!   fft_size = number of points in trace
!   fft_inp = complex input array to be transformed
!   fft_out = complex output array
!
! To this does no scaling - scale by 1./fft_size during inverse fft
!                            i         i         i        o
!    call matfun_fft_cc (fft_sign, fft_size, fft_inp, fft_out)
!
!    integer   fft_sign = -1 forward transform , 1 - inverse transform
!    integer   fft_size = number of points in trace
!    complex   fft_inp (fft_size)
!    complex   fft_out (fft_size)
!
!
! To take complex to complex fft in the x direction of nt slices
!
!                           i        i       i        i       i
!    call matfun_mcfft (fft_sign, nx_fft, ix_do_1, nx_do, fft_scale,    &
!                          i       i         i        o       i 
!                        x_inp, ix_inp_1, ix_inp_2, x_out, ix_out_1,    &
!                           i        o
!                        ix_out_2, i_err)
!
!    integer   ix_do_1
!    integer   fft_sign  = -1 forward transform , 1 - inverse transform
!    real      fft_scale = scale for fft
!    integer   nx_fft    = number of points in fft
!    integer   nx_do     = number of vecotrs to fft
!    real      x_inp     = input vectors
!    integer   ix_inp_1  = stride between elements of a single input vector
!    integer   ix_inp_2  = stride between input vectors
!    real      x_out     = output vectors
!    integer   ix_out_1  = stride between elements of a single output vector
!    integer   ix_out_2  = stride between output vectors
!    integer   i_err
!
!
! To reapply mutes
!                               i       i       i       b
!    call matfun_mute_apply (mh_inp, hd_inp, nt_inp, tr_inp)
!
!    double precision   hd_inp (mh_inp)
!    integer            mh_inp, nt_inp
!    real               tr_inp (nt_inp)
!
!
! To set the top and bottom mute indexes to the first nonzero value location
!                            i   b     i   i
!    call matfun_mute_set (m_hd, hd, n_tr, tr)
!
!    integer            m_hd, n_tr
!    double precision   hd (m_hd)
!    real               tr (n_tr)
!
!
! To get trace location
!                                     i       b      i       i
!    call matfun_trace_location_sp (n_dim, ix_inp, x_inp, x0_mig,     &
!                                      i       b      i       i       i
!                                   dx_mig, iy_inp, y_inp, y0_mig, dy_mig)
!
!    integer   n_dim, ix_inp, iy_inp
!    real      x_inp, x0_mig, dx_mig, y_inp, y0_mig, dy_mig
!
!
! To get trace location
!                                     i       o      i       i
!    call matfun_trace_location_dp (n_dim, ix_inp, x_inp, x0_mig,     &
!                                      i       o      i       i       i
!                                   dx_mig, iy_inp, y_inp, y0_mig, dy_mig)
!
!    integer           n_dim, ix_inp, iy_inp
!    double precision   x_inp, y_inp
!    real               x0_mig, dx_mig, y0_mig, dy_mig
!
!
! To define a small positive
!     o                i
!    eps = matfun_eps (x)
!
!    real   x, eps
!
!
! To compute the average of a 1d vector
!       o                         i   i
!    average = matfun_average_1d (n1, x)
!
!    integer   n1
!    real      x (n1), average
!
!
! To compute the average of a 2d vector
!       o                         i   i   i
!    average = matfun_average_2d (n1, n2, x)
!
!    integer   n1, n2
!    real      x (n1, n2), average
!
!
! To compute the average of a 3d vector
!       o                         i   i   i   i
!    average = matfun_average_3d (n1, n2, n3, x)
!
!    integer   n1, n2, n3
!    real      x (n1, n2, n3), average
!
!
! To average over n2 values of x_inp into x_out
!                            i   i   i      o
!    call matfun_average_n1 (n1, n2, x_inp, x_out)
!
!    integer   n1, n2
!    real      x_inp (n1, n2), x_out (n1)
!
!
! To average over n2, n3 values of x_inp into x_out
!                            i   i   i     i      o
!    call matfun_average_n2 (n1, n2, n3, x_inp, x_out)
!
!    integer   n1, n2, n3
!    real   x_inp (n1, n2, n3), x_out (n1)
!
!
! To return absolute max of a real array x with n elements
!      o                            i  i
!    x_max =  function matfun_amax (n, x)
!
!    integer   n
!    real      x (n), x_max
!
!
! To return absolute max of an integer array x with n elements
!      o                            i  i
!    x_max =  function matfun_imax (n, x)
!
!    integer   :: n, x (n), x_max
!
!
! To return absolute max magnitude of a complex array x with n elements
!      o                  i  i
!    x_max = matfun_cmax (n, x)
!
!    integer   n
!    complex   x (n)
!    real      x_max
!
!
! To convert x from radians to degrees
!     o                       i
!    deg = matfun_rad_to_deg (x)
!
!    real   x, deg
!
!
! To convert x from degrees to radians
!
!    deg = matfun_deg_to_rad (x)
!
!    real   x, deg
!
!
! To convert x from degrees to radians
!      o                        i
!    hertz = matfun_rad_to_htz (x)
!
!    real   x, hertz
!
!
!    To convert x from degrees to radians
!      o                        i
!    hertz = matfun_htz_to_rad (x)
!
!    real   x, hertz
!
!
! To invert a real value x, avoid divide by 0.
!   o                     i
!  inv = matfun_invert_1 (x)
!
!    real   x, inv
!
!
! To tan(matfun_atan(x, z)) = x/z
!   return arc tangent avoid 0., 0. problems
!    o                 i  i
!    a = matfun_atan2 (x, z)
!
!    real   x, z, a
!
! To return the next power of 2 >= n
!    o                i
!    p = matfun_pow2 (n)
!
!    integer   n, p
!
!
! To return the smallest even product of 2's, 3's, 4's and 5's 
!   which contains n.
!   from Stolt's fft code
!    o                 i
!    p = matfun_pown ( n )
!
!    integer   n, p
!
!
! To round n up to m values
!    o                 i  i
!    r = matfun_round (n, m)
!
!    integer   n, m, r
!
!
! To number of records length n in m words
!     o                  i  i
!    rinw = matfun_rinw (n, m)
!
!    integer   n, m, rinw
!
!
! To find the index, i_flag within array x whose first value is x_flag
!   search from (i1-1)*abs(inc)+1, (i2-1)*abs(inc)+1 incrementing by inc
!      o                          i   i   i    i    i
!    i_flag = matfun_find_flag_r (i1, i2, inc, x, x_flag)
!
!    integer   i1, i2, inc, i_flag
!    real      x (*), x_flag
!
!
! To find the index, i_flag within array x whose first value is x_flag
!   search from (i1-1)*abs(inc)+1, (i2-1)*abs(inc)+1 incrementing by inc
!      o                          i   i   i    i    i
!    i_flag = matfun_find_flag_i (i1, i2, inc, x, x_flag)
!
!    integer   i1, i2, inc, x (*), x_flag, i_flag
!
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
! 18  2007-01-03  Douglas Hanson Clean up bandps usage.
!017. 2006-09-18  D. Glover      Added NULLIFY statements for Intel compiler.
! 16. 2006-01-10  B. Menger      Removed Unused Variables.
! 15  2005-08-09  Douglas Hanson Fix freq_inc in matfun_filter.
! 14  2003-06-05  Douglas Hanson Add line_r_a routines.
! 13  2003-03-04  Douglas Hanson Add matfun_same_value
! 12  2003-01-26  Douglas Hanson Add matfun_filter_compute
! 11  2002-09-06  R.S.Day        Use mth_module for binning computations in 
!                                matfun_trace_location.
! 10  2002-08-26  R.S.Day        Changed matfun_invert_3d to avoid compiler 
!                                copy of large array.
!  9  2002-02-21  Douglas Hanson Add dp inside_polygon.
!  8  2002-02-04  Douglas Hanson Remove print in matfun_array_size
!  7  2001-03-21  Douglas Hanson fix matfun_htz_to_rad bug
!  6  2001-01-21  Douglas Hanson fix matfun_copy_i_to_r and r_to_i bug
!  5  2000-09-22  Douglas Hanson add matfun_copy_i_to_r and r_to_i
!  4  2000-09-19  Douglas Hanson improve matfun_inside_polygon
!  3  2000-08-25  Douglas Hanson cpsfcr
!  2  2000-05-19  Brad Kruse     Converted from old system.
!  1  1999-12-01  Douglas Hanson Initial version.
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


module matfun_module
  !
  ! - Module reference
  !
  use bandps_module
  use fft_module
  use memfun_module
  use mth_module
  use named_constants_module
  use pc_module
  use taper_module
  !
  implicit none
  !
  private
  public :: matfun_line              ! set real array to a linear gradient
  public :: matfun_min_max           ! min and max of nd array
  public :: matfun_min_max_diff      ! min and max diff in adjacent elements
  public :: matfun_normalize_r       ! normalize a real array to a constant
  public :: matfun_convolve          ! convolve two vectors
  public :: matfun_invert            ! invert n values of real array x,avoid d
  public :: matfun_check_change      ! make sure an array is monotonic
  public :: matfun_array_size        ! determine an array sampling increment
  public :: matfun_normal_to_line    ! compute the normal to a line
  public :: matfun_normal_to_plane   ! compute the normal to a plane
  public :: matfun_inside_polygon    ! determine if point is inside a polygon
  public :: matfun_filter            ! apply a filter
  public :: matfun_filter_compute    ! compute a time filter
  public :: matfun_fft               ! standalone fft
  public :: matfun_mcfft             ! standalone multiple fft
  public :: matfun_mute_apply        ! reapply a mute
  public :: matfun_mute_set          ! set mute header words
  public :: matfun_trace_location    ! return a trace x,y from header words
  public :: matfun_copy_i_to_r       ! copy real to integer through equivalence
  public :: matfun_copy_r_to_i       ! copy integer to real through equivalence

! functions
  public :: matfun_eps               ! return a small positive number
  public :: matfun_average           ! return the average of a d real array
  public :: matfun_amax              ! maximum absolute value in real array
  public :: matfun_imax              ! maximum absolute value in integer arra
  public :: matfun_cmax              ! maximum absolute value in complexarray
  public :: matfun_rad_to_deg        ! convert from radians to degrees
  public :: matfun_deg_to_rad        ! convert from degrees to radians
  public :: matfun_rad_to_htz        ! convert from radians to hertz
  public :: matfun_htz_to_rad        ! convert from hertz to radians
  public :: matfun_invert_1          ! invert a real value x,avoid divide by 0
  public :: matfun_atan2             ! return arc tangent avoid 0.,0. problems
  public :: matfun_pow2              ! power of 2 >= n
  public :: matfun_pown              ! power of 2, 3, 5 >= n
  public :: matfun_round             ! round n up to m vlaues
  public :: matfun_rinw              ! number of records length n in m words
  public :: matfun_find_flag         ! find the location of a flag
  public :: matfun_same_value               ! compare values

  character (len=100), public, save :: MATFUN_IDENT = &
       '$Id: matfun.f90,v 1.18 2007/01/03 14:01:42 Hanson prod sps $'


  !!------------------------- generic interfaces ---------------------------!!
  !!------------------------- generic interfaces ---------------------------!!
  !!------------------------- generic interfaces ---------------------------!!

  !
  ! - matfun_inside_polygon
  !
  interface matfun_atan2
    !
    module procedure matfun_atan2_r
    module procedure matfun_atan2_d
    !
  end interface 
  !
  ! - matfun_inside_polygon
  !
  interface matfun_inside_polygon
    !
    module procedure matfun_inside_polygon_r
    module procedure matfun_inside_polygon_d
    !
  end interface 
  !
  ! - matfun_line
  !
  interface matfun_line
    !
    module procedure matfun_line_r_a
    module procedure matfun_line_i_a
    module procedure matfun_line_d_a
    module procedure matfun_line_r
    module procedure matfun_line_i
    module procedure matfun_line_c
    module procedure matfun_line_d
    !
  end interface 
  !
  ! - matfun_min_max
  !
  interface matfun_min_max
    !
    module procedure matfun_min_max_r1
    module procedure matfun_min_max_r2
    module procedure matfun_min_max_r3
    module procedure matfun_min_max_i1
    module procedure matfun_min_max_i2
    module procedure matfun_min_max_i3
    !
  end interface 
  !
  ! - matfun_invert
  !
  interface matfun_invert
    !
    module procedure matfun_invert_1d
    module procedure matfun_invert_2d
    module procedure matfun_invert_3d
    !
  end interface 
  !
  ! - matfun_trace_location
  !
  interface matfun_trace_location
    !
    module procedure matfun_trace_location_sp
    module procedure matfun_trace_location_dp
    !
  end interface 
  !
  ! - matfun_find_flag
  !
  interface matfun_find_flag
    !
    module procedure matfun_find_flag_r
    module procedure matfun_find_flag_i
    !
  end interface 
  !
  ! - matfun_fft
  !
  interface matfun_fft
    !
    module procedure matfun_fft_rc
    module procedure matfun_fft_cr
    module procedure matfun_fft_cc
    !
  end interface 
  !
  ! - matfun_average
  !
  interface matfun_average
    !
    module procedure matfun_average_1d
    module procedure matfun_average_2d
    module procedure matfun_average_3d
    !
  end interface 
  !
  ! - matfun_average_n
  !
  interface matfun_average_n
    !
    module procedure matfun_average_n1
    module procedure matfun_average_n2
    !
  end interface 
  !
  ! matfun_same_value
  !
  interface matfun_same_value
    !
    module procedure matfun_same_value_l
    module procedure matfun_same_value_i
    module procedure matfun_same_value_r
    module procedure matfun_same_value_d
    module procedure matfun_same_value_c
    !
  end interface 


contains


  !!---------------------------- matfun_line -------------------------------!!
  !!---------------------------- matfun_line -------------------------------!!
  !!---------------------------- matfun_line -------------------------------!!

  !
  ! - set n elements of a real array x to values x0 + (i-1) * dx for i=1, n
  !
  subroutine matfun_line_r (n, x, x0, dx)
    !
    ! - Arguments
    !
    real,    intent (in   )  :: x0
    real,    intent (in   )  :: dx
    integer, intent (in   )  :: n
    real,    intent (  out) :: x (n)
    !
    ! - Local variables
    !
    integer :: i
    !
    ! - Begin matfun_line_r
    !
    do i = 1, n
      !
      x (i) = x0 + real (i - 1 ) * dx
      !
    end do
    !
  end subroutine matfun_line_r

  !
  ! - set n elements of a integer array x to values x0 + (i-1) * dx for i=1, n
  !
  subroutine matfun_line_i (n, x, x0, dx)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: x0
    integer, intent (in   )  :: dx
    integer, intent (in   )  :: n
    integer, intent (  out) :: x (n)
    !
    ! - Local variables
    !
    integer :: i
    !
    ! - Begin matfun_line_i
    !
    do i = 1, n
      !
      x (i) = x0 + (i - 1 ) * dx
      !
    end do
    !
  end subroutine matfun_line_i

  !
  ! - set n elements of a complex array x to values x0 + (i-1) * dx for i=1, n
  !
  subroutine matfun_line_c (n, x, x0, dx)
    !
    ! - Arguments
    !
    complex, intent (in   )  :: x0
    complex, intent (in   )  :: dx
    integer, intent (in   )  :: n
    complex, intent (  out) :: x (n)
    !
    ! - Local variables
    !
    integer :: i
    !
    ! - Begin matfun_line_c
    !
    do i = 1, n
      !
      x (i) = x0 + cmplx (i - 1 ) * dx
      !
    end do
    !
  end subroutine matfun_line_c

  !
  ! - set n elements of a double precision array x 
  !   to values x0 + (i-1) * dx for i=1, n
  !
  subroutine matfun_line_d (n, x, x0, dx)
    !
    ! - Arguments
    !
    double precision, intent (in   )  :: x0
    double precision, intent (in   )  :: dx
    integer,          intent (in   )  :: n
    double precision, intent (  out) :: x (n)
    !
    ! - Local variables
    !
    integer :: i
    !
    ! - Begin matfun_line_d
    !
    do i = 1, n
      !
      x (i) = x0 + dble (i - 1 ) * dx
      !
    end do
    !
  end subroutine matfun_line_d
  subroutine matfun_line_i_a ( cx_dat, nx_dat, x0_dat, dx_dat, rx_dat, i_err )
    !
    ! allocate and set a linear array
    !
    character(len=*),    intent(in   ) :: cx_dat       ! character flag
    integer,             intent(in   ) :: nx_dat       ! num array elements
    integer,             intent(in   ) :: x0_dat       ! array origin
    integer,             intent(in   ) :: dx_dat       ! array increment
    integer,                   pointer :: rx_dat ( : ) ! array elements
    integer,             intent(inout) :: i_err        ! err flag 0 O.K. -1 err
    !
    i_err = 0
    !
    call memfun_all ( rx_dat, nx_dat, cx_dat, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    call matfun_line ( nx_dat, rx_dat, x0_dat, dx_dat )
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in matfun_line_i_a ",  &
    & /, " during memory allocation cx_dat=", a, &
    & /, " nx_dat=", i12, " x0_dat=", i12, " dx_dat=", i12 &
    & )' ) &
    trim ( cx_dat ) , nx_dat, x0_dat, dx_dat
    !
    i_err = -1
    !
    return
    !
  end subroutine matfun_line_i_a 
  !
  subroutine matfun_line_r_a ( cx_dat, nx_dat, x0_dat, dx_dat, rx_dat, i_err )
    !
    ! allocate and set a linear array
    !
    character(len=*),    intent(in   ) :: cx_dat       ! character flag
    integer,             intent(in   ) :: nx_dat       ! num array elements
    real,                intent(in   ) :: x0_dat       ! array origin
    real,                intent(in   ) :: dx_dat       ! array increment
    real,                      pointer :: rx_dat ( : ) ! array elements
    integer,             intent(inout) :: i_err        ! err flag 0 O.K. -1 err
    !
    i_err = 0
    !
    call memfun_all ( rx_dat, nx_dat, cx_dat, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    call matfun_line ( nx_dat, rx_dat, x0_dat, dx_dat )
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in matfun_line_r_a ",  &
    & /, " during memory allocation cx_dat=", a, &
    & /, " nx_dat=", i12, " x0_dat=", g12.6, " dx_dat=", g12.6 &
    & )' ) &
    trim ( cx_dat ) , nx_dat, x0_dat, dx_dat
    !
    i_err = -1
    !
    return
    !
  end subroutine matfun_line_r_a 
  !
  subroutine matfun_line_d_a ( cx_dat, nx_dat, x0_dat, dx_dat, rx_dat, i_err )
    !
    ! allocate and set a linear array
    !
    character(len=*),    intent(in   ) :: cx_dat       ! character flag
    integer,             intent(in   ) :: nx_dat       ! num array elements
    double precision,    intent(in   ) :: x0_dat       ! array origin
    double precision,    intent(in   ) :: dx_dat       ! array increment
    double precision,    pointer       :: rx_dat ( : ) ! array elements
    integer,             intent(inout) :: i_err        ! err flag 0 O.K. -1 err
    !
    i_err = 0
    !
    call memfun_all ( rx_dat, nx_dat, cx_dat, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    call matfun_line ( nx_dat, rx_dat, x0_dat, dx_dat )
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in matfun_line_d_a ",  &
    & /, " during memory allocation cx_dat=", a, &
    & /, " nx_dat=", i12, " x0_dat=", g12.6, " dx_dat=", g12.6 &
    & )' ) &
    trim ( cx_dat ) , nx_dat, x0_dat, dx_dat
    !
    i_err = -1
    !
    return
    !
  end subroutine matfun_line_d_a 


  !!--------------------------- matfun_min_max -----------------------------!!
  !!--------------------------- matfun_min_max -----------------------------!!
  !!--------------------------- matfun_min_max -----------------------------!!

  !
  ! - get min and max values of n1 elements of 1d real array x0
  !
  subroutine matfun_min_max_r1 (x_min, x_max, n1, x0)
    !
    ! - Arguments
    !
    real,    intent (  out) :: x_min
    real,    intent (  out) :: x_max
    integer, intent (in   )  :: n1
    real,    intent (in   )  :: x0(n1)
    !
    ! - Begin matfun_min_max_r1
    !
    x_min = minval (x0)
    x_max = maxval (x0)
    !
  end subroutine matfun_min_max_r1

  !
  ! - get min and max values of n1, n2 elements of 2d real array x0
  !
  subroutine matfun_min_max_r2 (x_min, x_max, n1, n2, x0)
    !
    ! - Arguments
    !
    real,    intent (  out) :: x_min
    real,    intent (  out) :: x_max
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: n2
    real,    intent (in   )  :: x0 (n1, n2)
    !
    ! - Begin matfun_min_max_r2
    !
    x_min = minval (x0)
    x_max = maxval (x0)
    !
  end subroutine matfun_min_max_r2


  !
  ! - get min and max values of n1, n2, n3 elements of 3d real array x0
  !
  subroutine matfun_min_max_r3 (x_min, x_max, n1, n2, n3, x0)
    !
    ! - Arguments
    !
    real,    intent (  out) :: x_min
    real,    intent (  out) :: x_max
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: n2
    integer, intent (in   )  :: n3
    real,    intent (in   )  :: x0 (n1, n2, n3)
    !
    ! - Begin matfun_min_max_r2
    !
    x_min = minval (x0)
    x_max = maxval (x0)
    !
  end subroutine matfun_min_max_r3

  !
  ! - get min and max values of n1 elements of 1d integer array x0
  !
  subroutine matfun_min_max_i1 (x_min, x_max, n1, x0)
    !
    ! - Arguments
    !
    integer, intent (  out) :: x_min
    integer, intent (  out) :: x_max
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: x0 (n1)
    !
    ! - Begin matfun_min_max_i1
    !
    x_min = minval (x0)
    x_max = maxval (x0)
    !
  end subroutine matfun_min_max_i1

  !
  ! - get min and max values of n1, n2 elements of 2d integer array x0
  !
  subroutine matfun_min_max_i2 (x_min, x_max, n1, n2, x0)
    !
    ! - Arguments
    !
    integer, intent (  out) :: x_min
    integer, intent (  out) :: x_max
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: n2
    integer, intent (in   )  :: x0 (n1, n2)
    !
    ! - Begin matfun_min_max_i2
    !
    x_min = minval (x0)
    x_max = maxval (x0)
    !
  end subroutine matfun_min_max_i2


  !
  ! - get min and max values of n1, n2, n3 elements of 3d integer array x0
  !
  subroutine matfun_min_max_i3 (x_min, x_max, n1, n2, n3, x0)
    !
    ! - Arguments
    !
    integer, intent (  out) :: x_min
    integer, intent (  out) :: x_max
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: n2
    integer, intent (in   )  :: n3
    integer, intent (in   )  :: x0 (n1, n2, n3)
    !
    ! - Begin matfun_min_max_i2
    !
    x_min = minval (x0)
    x_max = maxval (x0)
    !
  end subroutine matfun_min_max_i3

  !!------------------------ matfun_min_max_diff ---------------------------!!
  !!------------------------ matfun_min_max_diff ---------------------------!!
  !!------------------------ matfun_min_max_diff ---------------------------!!

  !
  ! - return the min and max differences between adjacent values of x
  !
  subroutine matfun_min_max_diff(x_min, x_max, nx, x)
    !
    ! - Arguments
    !
    real,    intent (  out) :: x_min
    real,    intent (  out) :: x_max
    integer, intent (in   )  :: nx
    real,    intent (in   )  :: x (nx)
    !
    ! - Local variables
    !
    real      dx
    integer   ix
    !
    ! - Begin matfun_min_max_diff
    !
    if (nx > 1) then
      !
      x_min = x(2) - x(1)
      x_max = x(2) - x(1)
      !
      do ix = 3 , nx
        !
        dx = x (ix) - x (ix-1)
        x_min = min (x_min, dx)
        x_max = max (x_max, dx)
        !
      end do    ! do ix = 3 , nx
      !
    else
      !
      x_min = 0.0
      x_max = 0.0
      !
    end if    ! if (nx >= 1) then
    !
  end subroutine matfun_min_max_diff


  !!------------------------- matfun_normalize_r ---------------------------!!
  !!------------------------- matfun_normalize_r ---------------------------!!
  !!------------------------- matfun_normalize_r ---------------------------!!

  !
  ! - normalize nx elements of real array x by real value x0
  !   x = array to normalize
  !   nx = number of elements in array x
  !   x0 = value to normalize to
  !
  subroutine matfun_normalize_r (nx, x, x0)
    !
    ! - Arguments
    !
    real,    intent (in   ) :: x0
    integer, intent (in   ) :: nx
    real,    intent (inout) :: x (nx)
    !
    ! - Local variables
    !
    real :: x_max
    !
    ! - Begin matfun_normalize_r
    !
    x_max = matfun_amax (n = nx, x = x)
    !
    if (x_max > 0.0) then
      x = x * x0 / x_max
    endif
   !
  end subroutine matfun_normalize_r


  !!-------------------------- matfun_convolve -----------------------------!!
  !!-------------------------- matfun_convolve -----------------------------!!
  !!-------------------------- matfun_convolve -----------------------------!!

  !
  ! - convolve x1 into x2  x3 = x1 * x2
  !   may be inplace
  !
  subroutine matfun_convolve (n1, x1, n2, x2, x3)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: n2
    real,    intent (in   )  :: x1 (n1)
    real,    intent (in   )  :: x2 (n2)
    real,    intent (  out) :: x3 (n2)
    !
    ! - Local variables
    !
    integer :: i1, j1
    integer :: i2
    integer :: i3
    integer :: i3_1
    integer :: i3_2
    real    :: x4 (n2)
    !
    ! - Begin matfun_convolve
    !
    ! - initalize the output to zero
    !
   x4 = 0.0
    !
    ! - cycle over filter coefficients
    !
    do i1 = 1 , n1
      !
      ! - determine the filter coefficient shift
      !
      j1 = n1 - n1/2 - i1
      !
      ! - compute the valid range of output samples for this filter element
      !
      i3_1 = max ( 1,  1+j1)
      i3_2 = min (n2, n2+j1)
      !
      ! - compute the first input element index
      !
      i2 = i3_1 - j1 - 1
      !
      ! - cycle over output elements
      !
      do i3 = i3_1 , i3_2
        !
        ! - increment the input element index
        !
        i2 = i2 + 1
        !
        ! - sum this term into the convolution sum
        !
        x4 (i3) = x4 (i3) + x1 (i1) * x2 (i2)
        !
      end do    ! do i3 = i3_1 , i3_2
      !
    end do    ! do i1 = 1 , n1
    !
    ! - transfer the result from x4 to x3, this allows in place operation
    !
    x3 = x4
    !
  end subroutine matfun_convolve


  !!--------------------------- matfun_invert ------------------------------!!
  !!--------------------------- matfun_invert ------------------------------!!
  !!--------------------------- matfun_invert ------------------------------!!

  !
  ! - invert n1 values of 1d real array x, avoid divide by 0.
  !
  subroutine matfun_invert_1d (n1, x)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n1
    real,    intent (inout) :: x (n1)
    !
    ! - Begin matfun_invert_1d
    !
    where (x /= 0.0)
      !
      x = 1.0 / x
      !
    elsewhere    ! where (x /= 0.0)
      !
      x = 0.0
      !
    endwhere    ! where (x /= 0.0)
    !
  end subroutine matfun_invert_1d


  !
  ! - invert n1, n2 values of 2d real array x, avoid divide by 0.
  !
  subroutine matfun_invert_2d (n1, n2, x)
    !
    ! - Arguments
    !
    integer, intent (in   )     :: n1
    integer, intent (in   )     :: n2
    real,    intent (inout ) :: x (n1, n2)
    !
    ! - Begin matfun_invert_2d
    !
    where (x /= 0.0)
      !
      x = 1.0 / x
      !
    elsewhere    ! where (x /= 0.0)
      !
      x = 0.0
      !
    endwhere    ! where (x /= 0.0)
    !
  end subroutine matfun_invert_2d


  !
  ! - invert n1, n2, n3  values of 3d real array x, avoid divide by 0.
  !
  subroutine matfun_invert_3d (n1, n2, n3, x)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n1
    integer, intent (in   ) :: n2
    integer, intent (in   ) :: n3
    real,    intent (inout) :: x (n1, n2, n3)
    integer   :: i,j,k
    !
    ! - Begin matfun_invert_3d
    !
    do k = 1,n3
      do j = 1,n2
        do i = 1,n1
          if(x(i,j,k) /= 0.0)  x(i,j,k) = 1.0/x(i,j,k)
        enddo
      enddo
    enddo

!   where (x /= 0.0)
!     !
!     x = 1.0 / x
!     !
!   elsewhere    ! where (x /= 0.0)
!     !
!     x = 0.0
!     !
!   endwhere    ! where (x /= 0.0)
!   !
  end subroutine matfun_invert_3d


  !!------------------------ matfun_check_change ---------------------------!!
  !!------------------------ matfun_check_change ---------------------------!!
  !!------------------------ matfun_check_change ---------------------------!!

  !
  ! - make sure an array is monotonicly 
  !   increasing, i_dir>=0, or decreasing, i_dir<0
  !
  subroutine matfun_check_change (i_dir, n, x, i_err)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: i_dir
    integer, intent (in   )  :: n
    integer, intent (  out) :: i_err
    real,    intent (in   )  :: x (n)
    !
    ! - Local variables
    !
    integer  :: i
    integer  :: j
    !
    ! - Begin matfun_check_change
    !
    i_err = 0
    j     = 1
    !
  select_direction:    &
    if (i_dir >= 0) then
      !
    loop_increases:    &
      do i = 2 , n
        !
        if (x (i) < x (i - 1)) then
          i_err = -1
          exit loop_increases
        end if
        j = i
        !
      end do loop_increases
      !
    else select_direction
      !
    loop_decreases:    &
      do i = 2 , n
        !
        if (x (i) > x (i - 1)) then
          i_err = -1
          exit loop_decreases
        end if 
        j = i
        !
      end do loop_decreases
      !
    end if select_direction
    !
    ! - Check for unsuccessful status
    !
    if (i_err /= 0) then
      !
      write (pc_get_lun(), &
             "(/, ' error in matfun_check_change', "    &
              // " /, ' i_dir=', i2, ' n=', i8, ' j=', i10)")   &
            i_dir, n, j
      !
      write (pc_get_lun(), '(1x, i8, 1x, g16.9)')    &
            (i, x(i), i=max(1, j-10), min(n, j+10))
      !
    end if
    !
  end subroutine matfun_check_change


  !!------------------------- matfun_array_size ----------------------------!!
  !!------------------------- matfun_array_size ----------------------------!!
  !!------------------------- matfun_array_size ----------------------------!!

  !
  ! - determine the regularity of a set of numbers
  !
  subroutine matfun_array_size (nx, x0, dx, n, x, i_err)
    !
    ! - Arguments
    !
    integer, intent (  out) :: nx
    real,    intent (  out)  :: x0
    real,    intent (  out)  :: dx
    integer, intent (in   )  :: n
    integer, intent (  out) :: i_err
    real,    intent (in   )  :: x (n)
    !
    ! - Local variables
    !
    integer :: i

    real    :: x_min, x_max, x_eps
    !
    ! - Begin matfun_array_size
    !
    i_err = 0
    !
!      write(pc_get_lun(), &
!     & '(" matfun_array_size n=", i8, /, "  i  x")') n
!      write(pc_get_lun(), &
!     & '(1x, i8, 1x, g16.9)')(i, x(i), i=1, n)
    !
    x_min = minval (x)
    x_max = maxval (x)
    !
    nx = 1
    x0 = x_min
    dx = 1.
    !
    if (x_min == x_max) then
      !
      x_eps = 1.e-6
      !
      if (n <= 0) nx = 0
      !
    else if (n > 1) then    ! if (x_min == x_max) then
      !
      x_eps = (x_max - x_min) / 10000.0
      !
      dx = x_max - x_min
      dx = sign(1., dx) * max(abs(dx), x_eps)
      !
      do i = 2 , n
        !
        if (abs(x(i)-x(i-1)) > x_eps) then
            dx = min (a1 = dx,    &
                      a2 = abs (x (i) - x (i - 1)))
        end if
        !
      end do    ! do i = 2 , n
      !
      nx = nint((x_max - x_min) / dx) + 1
      !
    end if    ! if (x_min == x_max) then
    !
    !write (pc_get_lun(),    &
    !         "(/, ' matfun_array_size', "                                    &
    !         // " /, ' n    =', i10, ' x_min=', g14.7, ' x_max=', g14.7, "   &
    !         // " /, ' nx   =', i10, ' x0   =', g14.7, ' xl   =', g14.7, "   &
    !         // " ' dx=', g14.7)")                                           &
    !           n, x_min, x_max, nx, x0, (nx-1)*dx+x0, dx
    !
    ! - make sure the array is regular
    !
    if ((mod (n, nx) /= 0)    &
       .or. (abs ((nx - 1) * dx + x0 - x_max) > x_eps)) then
      !
      write (pc_get_lun(),                                                  &
                  "(/, ' error in matfun_array_size - grid is not regular', " &
               // " /, ' n    =', i10, ' x_min=', g14.7, ' x_max=', g14.7, "  &
               // " /, ' nx   =', i10, ' x0   =', g14.7, ' xl   =', g14.7, "  &
              // " ' dx=', g14.7)")                                           &
              n, x_min, x_max, nx, x0, (nx - 1) * dx + x0, dx
      !
      i_err = -1

    end if
    !
  end subroutine matfun_array_size


  !!----------------------- matfun_normal_to_line --------------------------!!
  !!----------------------- matfun_normal_to_line --------------------------!!
  !!----------------------- matfun_normal_to_line --------------------------!!

  !
  ! - compute the normal to a line defined by the points (x1, y1) ,  (x2, y2)
  !   which passes through the point (x0, y0)
  !   this point will be (xn, yn)
  !
  subroutine matfun_normal_to_line ( xn, yn, x0, y0, x1, y1, x2, y2)
    !
    ! - Arguments
    !
    real, intent (  out) :: xn
    real, intent (  out) :: yn
    real, intent (in   )  :: x0
    real, intent (in   )  :: y0
    real, intent (in   )  :: x1
    real, intent (in   )  :: y1
    real, intent (in   )  :: x2
    real, intent (in   )  :: y2
    !
    ! - Local variables
    !
    real :: dx
    real :: dy
    real :: ds
    real :: ds_sq_i
    real :: alpha
    !
    ! - Begin matfun_normal_to_line
    !
    dx = x2 - x1
    dy = y2 - y1
    ds = sqrt(dx**2 + dy**2)
    !
  verify_ds:    &
    if (ds <= matfun_eps (x = 1.0)) then
      !
      xn = x0
      yn = y0
      !
    else  verify_ds
      !
      ds_sq_i = 1.0 / max (a1 = matfun_eps (x = 1.), a2 = ds ** 2)
      !
      ! - compute the distance between the first line point 
      !   and the object point
      !
      alpha = ds_sq_i * ((x0 - x1) * dx     &
                        + (y0 - y1) * dy)
      !
      ! - the normal from the line to the object point is xn, yn
      !
      xn = x1 + dx * alpha
      yn = y1 + dy * alpha
      !
    end if  verify_ds
    !
!      write(81, '(1x, g10.4, 1x, g10.4, 1x, i8)') &
!     & x1, y1, 0, &
!     & x2, y2, 0, &
!     & xn, yn, 1, &
!     & x0, y0, 1
    !
  end subroutine matfun_normal_to_line


  !!----------------------- matfun_normal_to_plane -------------------------!!
  !!----------------------- matfun_normal_to_plane -------------------------!!
  !!----------------------- matfun_normal_to_plane -------------------------!!

  !
  ! - compute the normal to a plane defined by the points
  !   (x1, y1, z1) ,  (x2, y2, z2) ,  (x3, y3, z3)
  !    which passes through the point (x0, y0, z0)
  !    this point will be (xn, yn, zn)
  !
  subroutine matfun_normal_to_plane (xn, yn, zn, &
                                     x0, y0, z0, &
                                     x1, y1, z1, &
                                     x2, y2, z2, &
                                     x3, y3, z3)
    !
    ! - Arguments
    !
    real, intent (  out) :: xn
    real, intent (  out) :: yn
    real, intent (  out) :: zn
    real, intent (in   )  :: x0
    real, intent (in   )  :: y0
    real, intent (in   )  :: z0
    real, intent (in   )  :: x1
    real, intent (in   )  :: y1
    real, intent (in   )  :: z1
    real, intent (in   )  :: x2
    real, intent (in   )  :: y2
    real, intent (in   )  :: z2
    real, intent (in   )  :: x3
    real, intent (in   )  :: y3
    real, intent (in   )  :: z3
    !
    ! - Local variables
    !
    real :: dx_21
    real :: dy_21
    real :: dz_21
    real :: dx_31
    real :: dy_31
    real :: dz_31
    real :: ax
    real :: ay
    real :: az
    real :: c1
    real :: c0
    !
    ! - Begin matfun_normal_to_plane
    !
    dx_21 = x2 - x1
    dy_21 = y2 - y1
    dz_21 = z2 - z1
    !
    dx_31 = x3 - x1
    dy_31 = y3 - y1
    dz_31 = z3 - z1
    !
    c0 = dx_31 * dy_21 - dx_21 * dy_31
    !
    if (abs(c0) <= matfun_eps(1.)) then
      c0 =sign(1., c0) * matfun_eps(1.)
    end if
    !
    ! - compute the coefficents for the plane z = az + ax x + ay y
    !
    ax = + (dz_31 * dy_21 - dz_21 * dy_31) / c0
    ay = - (dz_31 * dx_21 - dz_21 * dx_31) / c0
    az = (z1 + z2 + z3              &
          - ax * (x1 + x2 + x3)     &
          - ay * (x1 + x2 + x3))    &
         / 3.0
    !
    ! - compute the point x0, y0, z0 within,  and normal to,  the plane 
    !   reflector which pass through the source xs, ys, zs
    !
    c1 = (ax * x0 + ay * y0 + az - z0) / (1 + ax * ax + ay * ay)
    xn = x0 - ax * c1
    yn = y0 - ay * c1
    zn = az + ax * x0 + ay * y0
    !
!      write(81, '(1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, i8)') &
!     & x1, y1, z1, ax*x1+ay*y1+az, 0, &
!     & x2, y2, z2, ax*x2+ay*y2+az, 0, &
!     & x3, y3, z3, ax*x3+ay*y3+az, 0, &
!     & xn, yn, zn, ax*xn+ay*yn+az, 0, &
!     & x0, y0, z0, 0., 0
    !
  end subroutine matfun_normal_to_plane

  !!----------------------- matfun_inside_polygon_r --------------------------!!
  !!----------------------- matfun_inside_polygon_r --------------------------!!
  !!----------------------- matfun_inside_polygon_r --------------------------!!

  !
  !  determine if a target point x0, y0 is inside a closed polygon x, y
  !  by considering the angular range around the target
  !  swept from the start to the end of the polygon
  !  inside inside = .true. ,  outside inside = .false.
  !  the first x,y point of the polygon should be the same as the last x,y 
  !  point in the polygon
  !
  subroutine matfun_inside_polygon_r (x0, y0, n_poly, x_poly, y_poly, inside)
    !
    ! - Arguments
    !
    real,    intent (in   )  :: x0              ! target point x value
    real,    intent (in   )  :: y0              ! target point y value
    integer, intent (in   )  :: n_poly          ! number of polygon x, y values
    real,    intent (in   )  :: x_poly(n_poly)  ! polygon y values
    real,    intent (in   )  :: y_poly(n_poly)  ! polygon y values
    logical, intent (  out) :: inside           ! inside the polygon
    !
    ! - Local variables
    !
    integer   :: i_poly     ! current polygon node counter
    real      :: x1, y1, r1 ! target to previous node vector x, y, distance
    real      :: x2, y2, r2 ! target to current  node vector x, y, distance
    real      :: ang_eps    ! angle epsilon - a small number
    real      :: ang_cos    ! swept angle increment cosine
    real      :: ang_sin    ! swept angle increment sine
    real      :: ang_inc    ! swept angle increment
    real      :: ang_sum    ! swept angle sum
    !
    inside = .false.  ! initalize the inside flag to false
    !
    ang_eps = 1.e-6 ! angle epsilon for comparisons
    !
    ! if the target falls directly on a node point 
    ! or along a straight x,y line between two adjacent node points
    ! it is considered inside
    !
    loop_poly_1: do i_poly = 1 ,  n_poly-1
      !
      if ( &
        !
        ! the target is considered inside if it falls directly on a node point
        !
        ( x0 .eq. x_poly(i_poly) .and. y0 .eq. y_poly(i_poly)  ) &
    .or. &
        !
        ! the target is considered inside if it falls directly on a 
        ! constant x line between two nodes
        !
        ( x_poly(i_poly) .eq. x_poly(i_poly+1) &
    .and. x_poly(i_poly) .eq. x0 &
    .and. y0 .ge. min(y_poly(i_poly), y_poly(i_poly+1)) &
    .and. y0 .le. max(y_poly(i_poly), y_poly(i_poly+1)) ) &
    .or. &
        !
        ! the target is considered inside if it falls directly on a 
        ! constant y line between two nodes
        !
        ( y_poly(i_poly) .eq. y_poly(i_poly+1) &
    .and. y_poly(i_poly) .eq. y0 &
    .and. x0 .ge. min(x_poly(i_poly), x_poly(i_poly+1)) &
    .and. x0 .le. max(x_poly(i_poly), x_poly(i_poly+1)) ) &
        ) then
        !
        inside = .true.
        goto 1
        !
      end if    ! if (
      !
    end do loop_poly_1    ! do i_poly = 1 ,  n_poly-1
    !
    ! compute the total angle swept, ang_sum, about the target point.
    !
    ang_sum = 0.
    !
    ! x1,y1,r1 define the unit vector from the target to the previous node
    ! x2,y2,r2 define the unit vector from the target to the current  node
    !
    ! initialize the target to previous node vector values
    !
    i_poly = 1               ! node for previous vector
    x1 = x_poly(i_poly) - x0 !  target to previous node vector x component
    y1 = y_poly(i_poly) - y0 !  target to previous node vector y component
    r1 = sqrt(x1*x1 + y1*y1) !  target to previous node vector length
    x1 = x1 / r1             !  target to previous node unit vector x component
    y1 = y1 / r1             !  target to previous node unit vector y component
    !
    ! for each point in the polygon, compute the additional angle swept, ang_inc
    ! and increment the total swept angle, ang_sum.
    !
    loop_poly_2: do i_poly = 2 ,  n_poly
      !
      x2 = x_poly(i_poly) - x0 !  target to current node vector x component
      y2 = y_poly(i_poly) - y0 !  target to current node vector y component
      r2 = sqrt(x2*x2 + y2*y2) !  target to current node vector length
      x2 = x2 / r2             !  target to current node unit vector x component
      y2 = y2 / r2             !  target to current node unit vector y component
      !
      ! compute the angle cosine and sine from the dot and cross product of the
      ! previous and current unit vectors
      !
      ang_cos = x1*x2 + y1*y2
      ang_sin = x1*y2 - y1*x2
      !
      ! compute the angle increment, ang_inc, from the angle cossine and sine
      ! and increment the angle sum, ang_sum
      !
      ang_inc = matfun_atan2(ang_sin, ang_cos)
      ang_sum = ang_sum + ang_inc
      !
      ! update the previous unit vector values.
      !
      x1 = x2
      y1 = y2
      r1 = r2
      !
      ! the target is considerd inside if it falls very close
      ! to a straight line connecting the two node points.
      ! the the angle increment cosine is close to -1.
      !
      if ( abs ( ang_cos + 1. ) .le. ang_eps ) then
        !
        inside = .true.
        goto 1
        !
      end if    ! if ( abs ( ang_cos + 1. ) .le. ang_eps ) then
      !
      !      write(89, '( &
      !     & 1x, i6, 1x, i6, &
      !     & 1x, f6.1, 1x, f6.1, &
      !     & 1x, f10.4, 1x, f10.4, &
      !     & 1x, f10.4, 1x, f10.4 &
      !     & )') &
      !     & i_call, i_poly, &
      !     & x_poly(i_poly), y_poly(i_poly), &
      !     & ang_sum*90./asin(1.), ang_inc*90./asin(1.), &
      !     & ang_cos, ang_sin
      !
    end do loop_poly_2 ! do i_poly = 2 ,  n_poly
    !
    ! the target is considerd inside if the total swept angle is very small
    !
    if (    abs(ang_sum)             .gt. .01) inside = .true.
    !
    ! come to here to jump out of the testing
    !
  1 continue
    !
    ! write(89, '(1x, i6, 1x, f6.1, 1x, f6.1, 1x, f10.4, 1x, i6)') &
    ! & i_call, x0, y0, ang_sum, inside
    !
    return
    !
  end subroutine matfun_inside_polygon_r
  !!----------------------- matfun_inside_polygon_r --------------------------!!
  !!----------------------- matfun_inside_polygon_r --------------------------!!
  !!----------------------- matfun_inside_polygon_r --------------------------!!

  !
  !  determine if a target point x0, y0 is inside a closed polygon x, y
  !  by considering the angular range around the target
  !  swept from the start to the end of the polygon
  !  inside inside = .true. ,  outside inside = .false.
  !  the first x,y point of the polygon should be the same as the last x,y 
  !  point in the polygon
  !
  subroutine matfun_inside_polygon_d (x0, y0, n_poly, x_poly, y_poly, inside)
    !
    ! - Arguments
    !
    double precision, intent (in   ) :: x0              ! target point x value
    double precision, intent (in   ) :: y0              ! target point y value
    integer,          intent (in   ) :: n_poly          ! number of polygon x,y
    double precision, intent (in   ) :: x_poly(n_poly)  ! polygon y values
    double precision, intent (in   ) :: y_poly(n_poly)  ! polygon y values
    logical,          intent (  out) :: inside          ! inside the polygon
    !
    ! - Local variables
    !
    integer   :: i_poly     ! current polygon node counter
    double precision :: x1, y1, r1 ! target to previous node vector x, y, dist
    double precision :: x2, y2, r2 ! target to current  node vector x, y, dist
    double precision :: ang_eps    ! angle epsilon - a small number
    double precision :: ang_cos    ! swept angle increment cosine
    double precision :: ang_sin    ! swept angle increment sine
    double precision :: ang_inc    ! swept angle increment
    double precision :: ang_sum    ! swept angle sum
    !
    inside = .false.  ! initalize the inside flag to false
    !
    ang_eps = 1.e-12 ! angle epsilon for comparisons
    !
    ! if the target falls directly on a node point 
    ! or along a straight x,y line between two adjacent node points
    ! it is considered inside
    !
    loop_poly_1: do i_poly = 1 ,  n_poly-1
      !
      if ( &
        !
        ! the target is considered inside if it falls directly on a node point
        !
        ( x0 .eq. x_poly(i_poly) .and. y0 .eq. y_poly(i_poly)  ) &
    .or. &
        !
        ! the target is considered inside if it falls directly on a 
        ! constant x line between two nodes
        !
        ( x_poly(i_poly) .eq. x_poly(i_poly+1) &
    .and. x_poly(i_poly) .eq. x0 &
    .and. y0 .ge. min(y_poly(i_poly), y_poly(i_poly+1)) &
    .and. y0 .le. max(y_poly(i_poly), y_poly(i_poly+1)) ) &
    .or. &
        !
        ! the target is considered inside if it falls directly on a 
        ! constant y line between two nodes
        !
        ( y_poly(i_poly) .eq. y_poly(i_poly+1) &
    .and. y_poly(i_poly) .eq. y0 &
    .and. x0 .ge. min(x_poly(i_poly), x_poly(i_poly+1)) &
    .and. x0 .le. max(x_poly(i_poly), x_poly(i_poly+1)) ) &
        ) then
        !
        inside = .true.
        goto 1
        !
      end if    ! if (
      !
    end do loop_poly_1    ! do i_poly = 1 ,  n_poly-1
    !
    ! compute the total angle swept, ang_sum, about the target point.
    !
    ang_sum = 0.
    !
    ! x1,y1,r1 define the unit vector from the target to the previous node
    ! x2,y2,r2 define the unit vector from the target to the current  node
    !
    ! initialize the target to previous node vector values
    !
    i_poly = 1               ! node for previous vector
    x1 = x_poly(i_poly) - x0 !  target to previous node vector x component
    y1 = y_poly(i_poly) - y0 !  target to previous node vector y component
    r1 = sqrt(x1*x1 + y1*y1) !  target to previous node vector length
    x1 = x1 / r1             !  target to previous node unit vector x component
    y1 = y1 / r1             !  target to previous node unit vector y component
    !
    ! for each point in the polygon, compute the additional angle swept, ang_inc
    ! and increment the total swept angle, ang_sum.
    !
    loop_poly_2: do i_poly = 2 ,  n_poly
      !
      x2 = x_poly(i_poly) - x0 !  target to current node vector x component
      y2 = y_poly(i_poly) - y0 !  target to current node vector y component
      r2 = sqrt(x2*x2 + y2*y2) !  target to current node vector length
      x2 = x2 / r2             !  target to current node unit vector x component
      y2 = y2 / r2             !  target to current node unit vector y component
      !
      ! compute the angle cosine and sine from the dot and cross product of the
      ! previous and current unit vectors
      !
      ang_cos = x1*x2 + y1*y2
      ang_sin = x1*y2 - y1*x2
      !
      ! compute the angle increment, ang_inc, from the angle cossine and sine
      ! and increment the angle sum, ang_sum
      !
      ang_inc = matfun_atan2(ang_sin, ang_cos)
      ang_sum = ang_sum + ang_inc
      !
      ! update the previous unit vector values.
      !
      x1 = x2
      y1 = y2
      r1 = r2
      !
      ! the target is considerd inside if it falls very close
      ! to a straight line connecting the two node points.
      ! the the angle increment cosine is close to -1.
      !
      if ( abs ( ang_cos + 1. ) .le. ang_eps ) then
        !
        inside = .true.
        goto 1
        !
      end if    ! if ( abs ( ang_cos + 1. ) .le. ang_eps ) then
      !
      !      write(89, '( &
      !     & 1x, i6, 1x, i6, &
      !     & 1x, f6.1, 1x, f6.1, &
      !     & 1x, f10.4, 1x, f10.4, &
      !     & 1x, f10.4, 1x, f10.4 &
      !     & )') &
      !     & i_call, i_poly, &
      !     & x_poly(i_poly), y_poly(i_poly), &
      !     & ang_sum*90./asin(1.), ang_inc*90./asin(1.), &
      !     & ang_cos, ang_sin
      !
    end do loop_poly_2 ! do i_poly = 2 ,  n_poly
    !
    ! the target is considerd inside if the total swept angle is very small
    !
    if (    abs(ang_sum)             .gt. .01) inside = .true.
    !
    ! come to here to jump out of the testing
    !
  1 continue
    !
    ! write(89, '(1x, i6, 1x, f6.1, 1x, f6.1, 1x, f10.4, 1x, i6)') &
    ! & i_call, x0, y0, ang_sum, inside
    !
    return
    !
  end subroutine matfun_inside_polygon_d


  !!--------------------------- matfun_filter ------------------------------!!
  !!--------------------------- matfun_filter ------------------------------!!
  !!--------------------------- matfun_filter ------------------------------!!

  !
  !  filter a time trace
  !  freq_beg, freq_end, taper_low, taper_high are in hertz
  !  t0_inp, dt_inp are in seconds
  !  where freq_num is the next power of 2>nt_inp
  !
  subroutine matfun_filter ( freq_low_none,  freq_low_full, &
                             freq_high_full, freq_high_none, freq_phase, &
                             nt_inp, t0_inp, dt_inp, tr_inp, i_err )
    !
    ! - Arguments
    !
    integer, intent (in   ) :: nt_inp
    real,    intent (in   ) :: t0_inp, dt_inp
    real,    intent (in   ) :: freq_low_none
    real,    intent (in   ) :: freq_low_full
    real,    intent (in   ) :: freq_high_full
    real,    intent (in   ) :: freq_high_none
    real,    intent (in   ) :: freq_phase
    real,    intent (inout) :: tr_inp(nt_inp)
    integer, intent (  out) :: i_err
    !
    ! - Local variables
    !
    integer :: freq_num
    real    :: f_nyquist
    real    :: freq_inc 
    integer, save  :: i_call = 0
    i_call = i_call + 1
    !
    ! - Begin matfun_filter
    !
    ! - initialize the error flag
    !
    i_err = 0
    !
    ! - get the fft length, next power of 2 >= nt_inp
    !
    ! Get the fft size
      !obj%npo2 = 8
      !do while ( obj%npo2 < nint(obj%ndpt*1.3) )
      !  obj%npo2 = obj%npo2 * 2
      !enddo
    ! Set nw, df, dw for frequency domain filters.
      !obj%nw = obj%npo2/2 + 1
      !obj%df = 1.0 / (obj%npo2 * obj%dt)
      !obj%dw = (obj%df * 2.0) * PI
    ! Find Nyquist frequency
      !obj%fnyq = 0.5 / obj%dt
    !
    freq_num  = 2 * matfun_pow2 ( n = nt_inp )
    !
    freq_inc = 1. / ( freq_num * dt_inp )
    !
    f_nyquist = .5 / dt_inp 
    !
    !freq_inc = f_nyquist / ( freq_num - 1 )
    !
    !freq_inc = f_nyquist / ( freq_num / 2 )
    !
    !freq_inc = f_nyquist / ( freq_num - 1 )
    !
    !freq_inc = f_nyquist / freq_num 
    !
    !freq_inc = .5 * f_nyquist / freq_num 
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( &
    & /, " matfun_filter  time filter characteristics", &
    & /, " matfun_filter  freq_num       =", i8, &
    & /, " matfun_filter  freq_inc       =", g12.6, &
    & /, " matfun_filter  freq_low_none  =", g12.6, &
    & /, " matfun_filter  freq_low_full  =", g12.6, &
    & /, " matfun_filter  freq_high_full =", g12.6, &
    & /, " matfun_filter  freq_high_none =", g12.6, &
    & /, " matfun_filter  freq_phase     =", g12.6, &
    & /, " matfun_filter  f_nyquist      =", g12.6, &
    & /, " matfun_filter  nt_inp         =", i8, &
    & /, " matfun_filter  t0_inp         =", g12.6, &
    & /, " matfun_filter  dt_inp         =", g12.6  &
    & )', &
    freq_num, freq_inc, &
    freq_low_none,  freq_low_full, &
    freq_high_full, freq_high_none, freq_phase, &
    f_nyquist, &
    nt_inp, t0_inp, dt_inp 
    !
    ! frequencies are in hertz
    !
    call matfun_filter2                                               &
           (freq_inc      = freq_inc,                                 &
            freq_num      = freq_num,                                 &
            freq_low_none = freq_low_none,                            &
            freq_low_full = freq_low_full,                            &
            freq_high_full= freq_high_full,                           &
            freq_high_none= freq_high_none,                           &
            freq_phase    = freq_phase,                               &
            nt_inp        = nt_inp,                                   &
            tr_inp        = tr_inp,                                   &
            i_err         = i_err)

      return

  end subroutine matfun_filter


  subroutine matfun_filter2 ( &
                             freq_inc, freq_num, &
                             freq_low_none, freq_low_full, &
                             freq_high_full, freq_high_none, &
                             freq_phase, &
                             nt_inp, tr_inp, i_err)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: freq_num
    real,    intent (in   ) :: freq_inc
    real,    intent (in   ) :: freq_low_none
    real,    intent (in   ) :: freq_low_full
    real,    intent (in   ) :: freq_high_full
    real,    intent (in   ) :: freq_high_none
    real,    intent (in   ) :: freq_phase
    integer, intent (in   ) :: nt_inp
    real,    intent (inout) :: tr_inp(nt_inp)
    integer, intent (inout) :: i_err
    !
    ! - Local variables
    !

    complex :: c_filter (freq_num/2+1)
    complex :: c_work (freq_num/2+1)
    real    :: r_work (freq_num)
    real    :: tr_inp_max
    real    :: tr_out_max
    real    :: dt_inp
    integer :: jt_inp
    character(len=10) filter_type
    integer, save  :: i_call = 0
    i_call = i_call + 1
    !
    ! - Begin matfun_filter2
    !
    !
    ! - get the inital max of the trace
    !
    !freq_inc = 1. / ( freq_num * dt_inp )
    !
    dt_inp = 1. / ( freq_num * freq_inc )
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, " matfun_filter2_qq1 " )', &
    ((jt_inp-1)*dt_inp, tr_inp(jt_inp), jt_inp, i_call, jt_inp = 1 , nt_inp )
    !
    tr_inp_max = matfun_amax (n = nt_inp, x = tr_inp)
    !
    ! - initialize work space to zero
    !
    r_work   = 0.0
    c_work   = 0.0
    c_filter = 0.0
    !
    ! - copy the real input trace to the real part of the complex work array
    !
    r_work(1:nt_inp) = tr_inp(1:nt_inp)
    !
    ! - take real to complex fft
    !
    call matfun_fft ( fft_sign = -1, &
                      fft_size = freq_num, &
                      fft_inp  = r_work, &
                      fft_out  = c_work )
    !
    ! - compute a bandpass filter
    !
    filter_type    = 'BANDPASS'
    !
    call bandps ( filter         = c_filter, &
                  num_freq       = freq_num / 2 + 1, &
                  freq_inc       = freq_inc, &
                  filter_type    = filter_type, &
                  freq_low_none  = freq_low_none, &
                  freq_low_full  = freq_low_full, &
                  freq_high_full = freq_high_full, &
                  freq_high_none = freq_high_none, &
                  phase          = freq_phase )
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, &
    & 1x, i8, 1x, i8, " matfun_filter2_qq2 " )', &
    ((jt_inp-1)*freq_inc, c_filter(jt_inp), c_work(jt_inp), &
    c_filter(jt_inp)*c_work(jt_inp), jt_inp, i_call, &
    jt_inp = 1 , freq_num / 2 + 1 )
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, 1x, g12.6, &
    & 1x, i8, 1x, i8, " matfun_filter2_qq3 " )', &
    ((jt_inp-1)*freq_inc, cabs(c_filter(jt_inp)), cabs(c_work(jt_inp)), &
    cabs(c_filter(jt_inp)*c_work(jt_inp)), jt_inp, i_call, &
    jt_inp = 1 , freq_num / 2 + 1 )
    !
    ! - apply filter
    !
    c_work = c_work * c_filter
    !
    ! - take complex to real fft
    !
    call matfun_fft (fft_sign = +1,          &
                     fft_size = freq_num,    &
                     fft_inp  = c_work,      &
                     fft_out  = r_work )
    !
    ! - copy real part of work back to tr_inp, include the fft scale
    ! - note cray scaling is by 1 . / n where n is forward fft
    !
    tr_inp(1:nt_inp) = r_work(1:nt_inp) / float(freq_num)
    !
    ! - take real to complex fft
    !
    call matfun_fft ( fft_sign = -1, &
                      fft_size = freq_num, &
                      fft_inp  = r_work, &
                      fft_out  = c_work )
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, &
    & 1x, i8, 1x, i8, " matfun_filter2_qq2b " )', &
    ((jt_inp-1)*freq_inc, c_filter(jt_inp), c_work(jt_inp), &
    c_filter(jt_inp)*c_work(jt_inp), jt_inp, i_call, &
    jt_inp = 1 , freq_num / 2 + 1 )
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( 1x, g12.6, &
    & 1x, g12.6, 1x, g12.6, 1x, g12.6, &
    & 1x, i8, 1x, i8, " matfun_filter2_qq3b " )', &
    ((jt_inp-1)*freq_inc, cabs(c_filter(jt_inp)), cabs(c_work(jt_inp)), &
    cabs(c_filter(jt_inp)*c_work(jt_inp)), jt_inp, i_call, &
    jt_inp = 1 , freq_num / 2 + 1 )
    !
    tr_out_max = matfun_amax ( n = nt_inp, &
                               x = tr_inp )
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( &
    & /, " matfun_filter2 time filter characteristics", &
    & /, " matfun_filter2 freq_num      =", i8, &
    & /, " matfun_filter2 freq_inc      =", g12.6, &
    & /, " matfun_filter2 freq_low_none =", g12.6, &
    & /, " matfun_filter2 freq_low_full =", g12.6, &
    & /, " matfun_filter2 freq_high_full=", g12.6, &
    & /, " matfun_filter2 freq_high_none=", g12.6, &
    & /, " matfun_filter2 freq_phase    =", g12.6, &
    & /, " matfun_filter2 tr_inp_max    =", g16.9, &
    & /, " matfun_filter2 tr_out_max    =", g16.9 &
    & )', &
    freq_num, freq_inc, &
    freq_low_none, freq_low_full, &
    freq_high_full, freq_high_none, &
    freq_phase, &
    tr_inp_max, tr_out_max
    !
    if ( i_call .le. -2 ) &
    !if ( i_call .le. 2 ) &
    print'( 1x, g12.6, 1x, g12.6, 1x, i8, 1x, i8, " matfun_filter2_qq4 " )', &
    ((jt_inp-1)*dt_inp, tr_inp(jt_inp), jt_inp, i_call, jt_inp = 1 , nt_inp )
    !
    !print*,' matfun_filter2 freq_phase =',freq_phase 
    !
    !if ( freq_phase .gt. 1.e-4 ) stop
    !
  end subroutine matfun_filter2

  subroutine matfun_filter_compute ( &
                                     lu_out, c_title, &
                                     freq_low_none, freq_low_full, &
                                     freq_high_full, freq_high_none, &
                                     freq_phase, &
                                     nt_inp, t0_inp, dt_inp, &
                                     n_taper, &
                                     n_filter, r_filter, &
                                     i_err &
                                   )
    !
    ! compute a time domain filter
    ! to apply this filter use: x_out = filter convolved x_inp
    !
    integer,          intent (in   ) :: lu_out
    character(len=*), intent (in   ) :: c_title
    real,             intent (in   ) :: freq_low_none
    real,             intent (in   ) :: freq_low_full
    real,             intent (in   ) :: freq_high_full
    real,             intent (in   ) :: freq_high_none
    real,             intent (in   ) :: freq_phase
    integer,          intent (in   ) :: nt_inp
    real,             intent (in   ) :: t0_inp
    real,             intent (in   ) :: dt_inp
    integer,          intent (in   ) :: n_taper
    integer,          intent (in   ) :: n_filter
    real,             intent (inout) :: r_filter ( : )
    integer,          intent (inout) :: i_err
    !
    integer                          :: nt_fft
    integer                          :: i_filter
    integer                          :: j_taper
    integer                          :: i_taper
    real                             :: a_taper
    real                             :: x_taper
    real,                    pointer :: s_filter ( : )
    !
    i_err = 0
    !
    nt_fft = matfun_pow2 ( nt_inp )
    !
    allocate ( s_filter ( nt_fft ) )
    !
    ! make sure the filter and taper lengths are valid
    !
    if ( n_filter .gt. nt_fft / 2 .or. n_taper .gt. n_filter / 2 ) &
    go to 998
    !
    !write ( pc_get_lun(), '( &
    !& / , " matfun_filter_compute &
    !& / , " n_filter=", i8, &
    !& / , " n_taper=", i8, &
    !& / , " nt_fft=", i8, &
    !& / , " nt_inp=", i8, &
    !& / , " t0_inp=", f10.4, &
    !& / , " dt_inp=", f10.4, &
    !& / , " freq_low_none =", f10.2, &
    !& / , " freq_low_full =", f10.2, &
    !& / , " freq_high_full=", f10.2, &
    !& / , " freq_high_none=", f10.2, &
    !& / , " freq_phase    =", f10.2, &
    !& ) ' )
    !n_filter, &
    !n_taper, &
    !nt_fft, &
    !nt_inp,t0_inp,dt_inp, &
    !nt_inp, t0_inp, dt_inp, &
    !freq_low_none, freq_low_full, &
    !freq_high_full, freq_high_none, &
    !freq_phase
    !
    s_filter ( : ) = 0.
    !                             i              i              i
    s_filter ( 1 ) = 1.
    !
    !  filter s_filter
    !
    call matfun_filter ( &
                         freq_low_none, freq_low_full, &
                         freq_high_full, freq_high_none, &
                         freq_phase, &
                         nt_inp, t0_inp, dt_inp, &
                         s_filter, &
                         i_err &
                       )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    !real      t0_inp, dt_inp -- Trace start, length in seconds
    !real      freq_low_none, freq_low_full, freq_high_none -- filter, in Hertz
    !real      freq_high_full, freq_phase, tr_inp(nt_inp)
    !integer   nt_inp, i_err
    !
    ! apply a taper to the filter
    ! compute a single taper value
    ! from ax_taper at ix=1 to 1 at ix=nx_inp
    ! i_taper = type of taper 1=liner, 2=exponential, 3=gaussian, 4=cosine
    ! a_taper = taper value
    ! ix_inp  = taper element to compute
    ! nx_inp  = number of elements in array x_inp
    ! x_inp   = taper coefficients
    !
    r_filter ( 1: n_filter ) = s_filter ( 1 : n_filter )
    !
    i_taper = 4    ! 1=liner, 2=exponential, 3=gaussian, 4=cosine
    !
    a_taper = 1. / float(max(1,n_taper))
    !
    do_taper : do j_taper = 1 , n_taper-1
      !
      ! compute the taper coefficient
      !
      call taper_compute_1 ( i_taper, a_taper, j_taper, n_taper, x_taper )
      !
      !  apply the taper to each end of the filter
      !
      r_filter(         j_taper  ) = &
      r_filter(         j_taper  ) * x_taper
      !
      r_filter(n_filter-j_taper+1) = &
      r_filter(n_filter-j_taper+1) * x_taper
      !
    end do do_taper
    !
    ! print the filter coefficents
    !
    if ( lu_out .ge. 0 ) write ( lu_out, &
    & ' ( / , " kxmig_filter_compute", &
    & / , " n_taper = ", i8, " n_filter = ", i8, &
    & / , " time         filter index " &
    & ) ' ) &
    n_taper, n_filter
    !
    if ( lu_out .ge. 0 ) write ( lu_out, &
    & ' ( 1x, g10.4, 1x, g12.6, 1x, i8 ) ' ) &
    ( ( i_filter - 1 ) * dt_inp, r_filter ( i_filter ), i_filter, &
    i_filter = 1, n_filter )
    !
1999 continue
    !
    if ( associated ( s_filter ) ) &
         deallocate ( s_filter )
    !
    return
    !
997 continue
    !
    write ( pc_get_lun (), ' ( &
    & / , " error in matfun_filter_compute " &
    & / , " in filter length " &
    & ) ' )
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun (), ' ( &
    & / , " error in matfun_filter_compute " &
    & ) ' )
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun (), &
    & ' ( / , " error in matfun_filter_compute " &
    & ) ' )
    !
    i_err = - 1
    !
    go to 1999
    !
  end subroutine matfun_filter_compute 
  !

  !!----------------------------- matfun_fft -------------------------------!!
  !!----------------------------- matfun_fft -------------------------------!!
  !!----------------------------- matfun_fft -------------------------------!!

  !
  ! -  take real to complex fft of fft_inp into fft_out
  !    fft_sign = -1 forward transform , 1 - inverse transform
  !    fft_size = number of points in trace
  !    fft_inp = real input array to be transformed
  !    fft_out = complex output array
  !
  ! - this does no scaling - scale by 1./fft_size during inverse fft
  !
  subroutine matfun_fft_rc (fft_sign, fft_size, fft_inp, fft_out)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: fft_sign
    integer, intent (in   )  :: fft_size
    real,    intent (in   )  :: fft_inp (fft_size)
    complex, intent (  out) :: fft_out (fft_size/2+1)
    !
    ! - Local variables
    !
    type(fft_struct), pointer :: fft_obj
    !
    integer :: i_err

    nullify (fft_obj) ! jpa
    !
    ! - Begin matfun_fft_rc
    ! 
    ! - create the fft_object
    !
    i_err =  fft_create (obj   = fft_obj,     &
                         sign  = fft_sign,    &
                         size  = fft_size,    &
                         ctype = 'rtoc' )
    !
    ! - if error during create
    !
    if (i_err /= 0) then
      !
      call pc_info ('matfun_fft_rc.  Error returned from fft_create:', i_err)
      !
    else    ! if (i_err /= 0) then
      !
      ! - complex to complex in place fft
      !
      call fft_rc_transform (obj  = fft_obj,    &
                             bufi = fft_inp,    &
                             bufo = fft_out)         ! real to complex
      !
    end if    ! if (i_err /= 0) then
    !
    ! - delete the fft object
    !
    call fft_delete (obj = fft_obj)
    !
  end subroutine matfun_fft_rc


  !
  ! - take complex to real fft of fft_inp into fft_out
  !   fft_sign = -1 forward transform , 1 - inverse transform
  !   fft_size = number of points in trace
  !   fft_inp = real input array to be transformed
  !   fft_out = complex output array
  !
  ! - this does no scaling - scale by 1./fft_size during inverse fft
  !
  subroutine matfun_fft_cr (fft_sign, fft_size, fft_inp, fft_out)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: fft_sign
    integer, intent (in   )  :: fft_size
    complex, intent (in   )  :: fft_inp (fft_size/2+1)
    real,    intent (  out) :: fft_out (fft_size)
    !
    ! - Local variables
    !
    type(fft_struct), pointer :: fft_obj
    integer :: i_err

    nullify (fft_obj) ! jpa
    !
    ! - Begin matfun_fft_cr
    !
    ! - create the fft_object
    !
    i_err =  fft_create (obj   = fft_obj,     &
                         sign  = fft_sign,    &
                         size  = fft_size,    &
                         ctype = 'ctor' )
    !
    ! - if error during create
    if (i_err /= 0) then
      !
      call pc_info ('Matfun_fft_cr.  Error returned from fft_create:', i_err)
      !
    else    ! if (i_err /= 0) then
      !
      ! - complex to complex in place fft
      !
      call fft_cr_transform (obj  = fft_obj,    &
                             bufi = fft_inp,    &
                             bufo = fft_out) ! real to complex
      !
    end if    ! if (i_err /= 0) then
    !
    ! - delete the fft object
    !
    call fft_delete (obj = fft_obj)
    !
  end subroutine matfun_fft_cr


  !
  ! - take real to complex fft of fft_inp into fft_out
  !
  !   fft_sign = -1 forward transform , 1 - inverse transform
  !   fft_size = number of points in trace
  !   fft_inp = complex input array to be transformed
  !   fft_out = complex output array
  !
  ! - this does no scaling - scale by 1./fft_size during inverse fft
  !
  subroutine matfun_fft_cc (fft_sign, fft_size, fft_inp, fft_out)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: fft_sign
    integer, intent (in   )  :: fft_size
    complex, intent (in   )  :: fft_inp (fft_size)
    complex, intent (  out) :: fft_out (fft_size)
    !
    ! - Local variables
    !
    type (fft_struct), pointer :: fft_obj
    integer                    :: i_err

    nullify (fft_obj) ! jpa
    !
    ! - Begin matfun_fft_cr
    !
    ! - create the fft_object
    !
    i_err =  fft_create (obj   = fft_obj,     &
                         sign  = fft_sign,    &
                         size  = fft_size,    &
                         ctype = 'ctoc' )
    !
    ! - if error during create
    !
    if (i_err /= 0) then
      !
      call pc_error (' error in matfun_fft_cc', i_err)
      !
    else    ! if (i_err /= 0) then
      !
      ! - complex to complex in place fft
      !
      call fft_cc_transform (obj  = fft_obj,    &
                             bufi = fft_inp,    &
                             bufo = fft_out) ! real to complex
      !
    end if    ! if (i_err /= 0) then
    !
    ! - delete the fft object
    !
    call fft_delete (obj = fft_obj)
    !
  end subroutine matfun_fft_cc

  !
  ! - take complex to complex fft in the x direction of nt slices
  !
  !   fft_sign  = -1 forward transform , 1 - inverse transform
  !   fft_scale = scale for fft
  !   nx_fft    = number of points in fft
  !   nx_do     = number of vecotrs to fft
  !   x_inp     = input vectors
  !   ix_inp_1  = stride between elements of a single input vector
  !   ix_inp_2  = stride between input vectors
  !   x_out     = output vectors
  !   ix_out_1  = stride between elements of a single output vector
  !   ix_out_2  = stride between output vectors

  !
  subroutine matfun_mcfft (fft_sign, nx_fft, ix_do_1, nx_do, fft_scale,    &
                           x_inp, ix_inp_1, ix_inp_2, x_out, ix_out_1,    &
                           ix_out_2, i_err)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: fft_sign
    integer, intent (in   )  :: nx_fft
    integer, intent (in   )  :: ix_do_1
    integer, intent (in   )  :: nx_do
    real,    intent (in   )  :: fft_scale
    integer, intent (in   )  :: ix_inp_1
    integer, intent (in   )  :: ix_inp_2
    real,    intent (in   )  :: x_inp(2, *)
    integer, intent (in   )  :: ix_out_1
    integer, intent (in   )  :: ix_out_2
    real,    intent (  out) :: x_out(2, *)
    integer, intent (  out) :: i_err
    !
    ! - Local variables
    !
    type(fft_struct), pointer :: fft_obj
    type(fft_struct), pointer :: fft_obj_1
    type(fft_struct), pointer :: fft_obj_2

    complex  fft_data(nx_fft)    ! automatic array

    integer  ix_fft, ix_do
    integer  ix_inp, ix_out

    character fft_type*8

    nullify (fft_obj) ! jpa
    nullify (fft_obj_1) ! jpa
    nullify (fft_obj_2) ! jpa
    !
    ! - Begin matfun_mcfft
    !
    ! - complex to complex transform
    !
    fft_type = 'ctoc'
    !
    ! - create the fft_object
    !
    i_err =  fft_create (fft_obj, fft_sign, nx_fft, fft_type )
    if (i_err /= 0) goto 999
    !
    ! - create the fft_object
    !
    i_err =  fft_create (fft_obj_1, +fft_sign, nx_fft, fft_type )
    if (i_err /= 0) goto 999
    !
    ! - create the fft_object
    !
    i_err =  fft_create (fft_obj_2, -fft_sign, nx_fft, fft_type )
    !
    ! - if error during create
    !
    if (i_err /= 0) goto 999
    !
    ! - cycle over nx_do vectors
    !
    do ix_do = ix_do_1, ix_do_1+nx_do-1
      !
      ! - copy from data to fft_data
      !
      ix_inp = (ix_do - 1) * ix_inp_2 + 1
      !
      do ix_fft = 1 , nx_fft
        !
        fft_data(ix_fft) = cmplx(x_inp(1, ix_inp), x_inp(2, ix_inp))
        ix_inp = ix_inp + ix_inp_1
        !
      end do    ! do ix_fft = 1 , nx_fft
      !
      ! - complex to complex in place fft
      !
      call fft_cc_transform(fft_obj, fft_data) ! complex to complex, inplace
!        call fft_cc_transform(fft_obj_1, fft_data) ! complex to complex, inplac
!        call fft_cc_transform(fft_obj_2, fft_data) ! complex to complex, inplac
!        fft_data = fft_data * fft_scale
!        call fft_cc_transform(fft_obj_1, fft_data) ! complex to complex, inplac
!        fft_data = fft_data * fft_scale
      !
      ! - copy from fft_data to x_out
      !
      ix_out = (ix_do - 1) * ix_out_2 + 1
      !
      do ix_fft = 1 , nx_fft
        !
        x_out(1, ix_out) =  real(fft_data(ix_fft)) * fft_scale
        x_out(2, ix_out) = aimag(fft_data(ix_fft)) * fft_scale
!          x_out(1, ix_out) =  real(fft_data(ix_fft))
!          x_out(2, ix_out) = aimag(fft_data(ix_fft))
        ix_out = ix_out + ix_out_1
        !
      end do    ! do ix_fft = 1 , nx_fft
      !
    end do    ! do ix_do = ix_do_1, ix_do_1+nx_do-1
    !
  1999 continue
    !
    ! - delete the fft object
    !
    call fft_delete (fft_obj)
    call fft_delete (fft_obj_1)
    call fft_delete (fft_obj_2)
    !
    return
    !
  999 continue
    !
    write (pc_get_lun(), '(/, " error in matfun_mcfft")')
    !
    i_err = -1
    goto 1999
    !
  end subroutine matfun_mcfft


  !!-------------------------- matfun_mute_apply ---------------------------!!
  !!-------------------------- matfun_mute_apply ---------------------------!!
  !!-------------------------- matfun_mute_apply ---------------------------!!

  !
  ! - reapply mutes
  !
  subroutine matfun_mute_apply (mh_inp, hd_inp, nt_inp, tr_inp)
    !
    ! - Arguments
    !
    integer,          intent (in   ) :: mh_inp
    double precision, intent (in   ) :: hd_inp (mh_inp)
    integer,          intent (in   ) :: nt_inp
    real,             intent (inout) :: tr_inp (nt_inp)
    !
    ! - Local variables
    !
    integer :: it_mute
    integer :: nt_mute
    !
    ! - Begin matfun_mute_apply
    !
    ! - reapply top mute
    !
    it_mute = 1
    nt_mute = max(0, min(nt_inp, nint(hd_inp( 2))-1))
    tr_inp(it_mute:it_mute+nt_mute-1) = 0.
    !
    ! - reapply bottom mute
    !
    it_mute = max(1, min(nt_inp, nint(hd_inp(64))+1))
    nt_mute = max(0, min(nt_inp, nt_inp-it_mute+1))
    tr_inp(it_mute:it_mute+nt_mute-1) = 0.
    !
  end subroutine matfun_mute_apply


  !!-------------------------- matfun_mute_set -----------------------------!!
  !!-------------------------- matfun_mute_set -----------------------------!!
  !!-------------------------- matfun_mute_set -----------------------------!!

  !
  ! - set the top and bottom mute indexes to the first nonzero value location
  !
  subroutine matfun_mute_set (m_hd, hd, n_tr, tr)
    !
    ! - Arguments
    !
    integer,          intent (in   ) :: m_hd
    integer,          intent (in   ) :: n_tr
    double precision, intent (inout) :: hd (m_hd)
    real,             intent (in   ) :: tr (n_tr)
    !
    ! - Local variables
    !
    integer :: i_tr
    !
    ! - Begin 
    !
    ! - set head mute
    !
    hd (HDR_TOP_MUTE)    = n_tr + 1
    hd (HDR_BOTTOM_MUTE) = 0
    !
  loop_top_mute_search:    &
    do i_tr = 1, n_tr
      !
      if (tr (i_tr) /= 0.) then
        !
        hd (HDR_TOP_MUTE) = dble (i_tr)
        !
        exit loop_top_mute_search
        !
      end if
      !
    end do loop_top_mute_search
    !
    ! - set tail mute
    !
  loop_bottom_mute_search:    &
    do i_tr = n_tr, 1, -1
      !
      if (tr(i_tr) /= 0.) then
        !
        hd (HDR_BOTTOM_MUTE) = dble (i_tr)
        !
        exit loop_bottom_mute_search
        !
      end if
      !
    end do loop_bottom_mute_search
    !
  end subroutine matfun_mute_set


  !!----------------------- matfun_trace_location --------------------------!!
  !!----------------------- matfun_trace_location --------------------------!!
  !!----------------------- matfun_trace_location --------------------------!!

  !
  ! - get trace location
  !
  subroutine matfun_trace_location_sp (n_dim, ix_inp, x_inp, x0_mig,     &
                                       dx_mig, iy_inp, y_inp, y0_mig, dy_mig)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: n_dim
    integer, intent (  out) :: ix_inp
    real,    intent (in   )  :: x_inp
    real,    intent (in   )  :: x0_mig
    real,    intent (in   )  :: dx_mig
    integer, intent (  out) :: iy_inp
    real,    intent (in   )  :: y_inp
    real,    intent (in   )  :: y0_mig
    real,    intent (in   )  :: dy_mig
    !
    ! - Begin matfun_trace_location_sp
    !
   !ix_inp = nint ((x_inp - x0_mig) / dx_mig) + 1
   !iy_inp = nint ((y_inp - y0_mig) / dy_mig) + 1
    ix_inp =  mth_bin_number(x0_mig,dx_mig,x_inp)
    iy_inp =  mth_bin_number(y0_mig,dy_mig,y_inp)
    !
    if (n_dim == 2) iy_inp = 1

!      if (pc_get_lun() >= 0) write(pc_get_lun(), &
!     & '(" matfun_trace_location n_dim=", i2, " ix=", i8, " nx=", i8,&
!     & " x=", g12.6, " x0=", g12.6, " dx=", f10.4)') &
!     & n_dim, ix, nx, x, x0, dx

    !
  end subroutine matfun_trace_location_sp

  !
  ! - get trace location
  !
  subroutine matfun_trace_location_dp (n_dim, ix_inp, x_inp, x0_mig,     &
                                       dx_mig, iy_inp, y_inp, y0_mig, dy_mig)
    !
    ! - Arguments
    !
    integer,          intent (in   )  :: n_dim
    integer,          intent (  out)  :: ix_inp
    double precision, intent (in   ) :: x_inp
    real,             intent (in   )  :: x0_mig
    real,             intent (in   )  :: dx_mig
    integer,          intent (  out)  :: iy_inp
    double precision, intent (in   ) :: y_inp
    real,             intent (in   )  :: y0_mig
    real,             intent (in   )  :: dy_mig
    double precision  :: org,inc
    !
    ! - Begin matfun_trace_location_dp
    !
   !ix_inp = nint ((x_inp - x0_mig) / dx_mig) + 1
   !iy_inp = nint ((y_inp - y0_mig) / dy_mig) + 1
    org = x0_mig
    inc = dx_mig
    ix_inp =  mth_bin_number(org,inc,x_inp)
    org = y0_mig
    inc = dy_mig
    iy_inp =  mth_bin_number(org,inc,y_inp)
    !
    if (n_dim == 2) iy_inp = 1
    !
    !if (pc_get_lun() >= 0) write(pc_get_lun(), &
    !& '(" matfun_trace_location n_dim=", i2, " ix=", i8, " nx=", i8,&
    !& " x=", g12.6, " x0=", g12.6, " dx=", f10.4)') &
    !n_dim, ix, nx, x, x0, dx
    !
  end subroutine matfun_trace_location_dp


  !!----------------------------- matfun_eps -------------------------------!!
  !!----------------------------- matfun_eps -------------------------------!!
  !!----------------------------- matfun_eps -------------------------------!!

  !
  ! - define a small positive
  !
  function matfun_eps (x)  result (eps)
    !
    ! - Arguments
    !
    real, intent (in   ) :: x
    real              :: eps
    !
    ! - Begin matfun_eps
    !
    if (x /= 0) then
      !
      eps = 1.e-6 * abs(x)
      !
    else
      !
      eps = 1.e-6
      !
    end if
    !
  end function matfun_eps


  !!--------------------------- matfun_average -----------------------------!!
  !!--------------------------- matfun_average -----------------------------!!
  !!--------------------------- matfun_average -----------------------------!!

  !
  ! - compute the average of a 1d vector
  !
   function matfun_average_1d (n1, x)  result (average)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n1
    real,    intent (in   ) :: x (n1)
    real                 :: average
    !
    ! - Local variables
    !

    !
    ! - Begin matfun_average_1d
    !
    if (n1 == 0) then
      !
      average = 0.
      !
    else
      !
      average = sum(x) / (n1)
      !
    end if
    !
  end function matfun_average_1d

  !
  ! - compute the average of a 2d vector
  !
   function matfun_average_2d (n1, n2, x)  result (average)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n1
    integer, intent (in   ) :: n2
    real,    intent (in   ) :: x (n1, n2)
    real                 :: average
    !
    ! - Local variables
    !

    !
    ! - Begin matfun_average_2d
    !
    if (n1*n2 == 0) then
      !
      average = 0.
      !
    else
      !
      average = sum (x) / (n1*n2)
      !
    end if
    !
  end function matfun_average_2d

  !
  ! - compute the average of a 3d vector
  !
   function matfun_average_3d (n1, n2, n3, x)  result (average)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n1
    integer, intent (in   ) :: n2
    integer, intent (in   ) :: n3
    real,    intent (in   ) :: x (n1, n2, n3)
    real                 :: average
    !
    ! - Local variables
    !

    !
    ! - Begin matfun_average_3d
    !
    if (n1*n2*n3 == 0) then
      !
      average = 0.
      !
    else
      !
      average = sum (x) / (n1*n2*n3)
      !
    end if
    !
  end function matfun_average_3d


  !
  ! - average over n2 values of x_inp into x_out
  !
  subroutine matfun_average_n1 (n1, n2, x_inp, x_out)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: n2
    real,    intent (in   )  :: x_inp (n1, n2)
    real,    intent (  out) :: x_out (n1)
    !
    ! - Local variables
    !
    integer :: i1
    integer :: i2
    !
    ! - Begin matfun_average_n1
    !
    do i1 = 1 , n1
      !
      x_out(i1) = 0.
      !
      do i2 = 1 , n2
        !
        x_out(i1) = x_out(i1) + x_inp(i1, i2)
        !
      end do    ! do i2 = 1 , n2
      !
      if (n2 /= 0) x_out(i1) = x_out(i1) / (n2)
      !
    end do    ! do i1 = 1 , n1
    !
  end subroutine matfun_average_n1

  !
  ! - average over n2, n3 values of x_inp into x_out
  !
  subroutine matfun_average_n2(n1, n2, n3, x_inp, x_out)
    !
    ! - Arguments
    !
    integer, intent (in   )  :: n1
    integer, intent (in   )  :: n2
    integer, intent (in   )  :: n3
    real,    intent (in   )  :: x_inp (n1, n2, n3)
    real,    intent (  out) :: x_out (n1)
    !
    ! - Local variables
    !
    integer :: i1
    integer :: i2
    integer :: i3
    !
    ! - Begin matfun_average_n2
    !
    do i1 = 1 , n1
      !
      x_out(i1) = 0.
      !
      do i3 = 1 , n3
        !
        do i2 = 1 , n2
          !
          x_out(i1) = x_out(i1) + x_inp(i1, i2, i3)
          !
        end do    ! do i2 = 1 , n2
        !
      end do    ! do i3 = 1 , n3
      !
      if (n2*n3 /= 0) x_out(i1) = x_out(i1) / (n2 * n3)
      !
    end do    ! do i1 = 1 , n1
    !
  end subroutine matfun_average_n2

  !!------------------------ matfun_copy_i_to_r --------------------------!!
  !!------------------------ matfun_copy_i_to_r --------------------------!!
  !!------------------------ matfun_copy_i_to_r --------------------------!!
  !
  ! copy nx element of integer array ix to real array rx through equivalence
  !
  subroutine matfun_copy_i_to_r ( nx, ix, rx )
    !
    ! - Arguments
    !
    integer, intent (in   ) :: nx    ! number of values to copy
    integer, intent (in   ) :: ix(:) ! input  integer array
    real,    intent (  out) :: rx(:) ! output real    array
    !
    ! - Local variables
    !
    integer                  :: jx    ! array index
    integer                  :: il    ! local real value
    real                     :: rl    ! local real value
    equivalence               ( il, rl )
    !
    loop_jx: do jx = 1 , nx
      !
      il     = ix(jx) ! copy input integer array value to local real value
      rx(jx) = rl     ! copy local real value to output integer array value
      !
    end do loop_jx
    !
  end subroutine matfun_copy_i_to_r

  !!------------------------ matfun_copy_r_to_i --------------------------!!
  !!------------------------ matfun_copy_r_to_i --------------------------!!
  !!------------------------ matfun_copy_r_to_i --------------------------!!
  !
  ! copy nx element of real array rx to integer array ix through equivalence
  !
  subroutine matfun_copy_r_to_i ( nx, rx, ix )
    !
    ! - Arguments
    !
    integer, intent (in   ) :: nx    ! number of  values to copy
    real,    intent (in   ) :: rx(:) ! input  real    array
    integer, intent (  out) :: ix(:) ! output integer array
    !
    ! - Local variables
    !
    integer                  :: jx    ! array index
    real                     :: rl    ! local real value
    integer                  :: il    ! local real value
    equivalence               ( il, rl )
    !
    loop_jx: do jx = 1 , nx
      !
      rl     = rx(jx) ! copy input real array value to local real value
      ix(jx) = il     ! copy local integer value to output integer array value
      !
    end do loop_jx
    !
  end subroutine matfun_copy_r_to_i

  !!---------------------------- matfun_max --------------------------------!!
  !!---------------------------- matfun_max --------------------------------!!
  !!---------------------------- matfun_max --------------------------------!!

  !
  ! - return absolute max of a real array x with n elements
  !
  function matfun_amax (n, x)  result (x_max)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n
    real,    intent (in   ) :: x (n)
    real                 :: x_max
    !
    ! - Local variables
    !
    real :: x_max_pos
    real :: x_max_neg
    !
    ! - Begin matfun_amax
    !
    x_max_pos = maxval (x)
    x_max_neg = minval (x)
    x_max     = max (a1 = abs (x_max_pos),    &
                     a2 = abs (x_max_neg))
    !
  end function matfun_amax


  !
  ! - return absolute max of an integer array x with n elements
  !
  function matfun_imax (n, x)  result (x_max)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n
    integer, intent (in   ) :: x (n)
    integer              :: x_max
    !
    ! - Local variables
    !
    integer :: x_max_pos
    integer :: x_max_neg
    !
    ! - Begin matfun_imax
    !
    x_max_pos = maxval (x)
    x_max_neg = minval (x)
    x_max     = max (a1 = abs (x_max_pos),    &
                     a2 = abs (x_max_neg))
    !
  end function matfun_imax


  !
  ! - return absolute max magnitude of a complex array x with n elements
  !
  function matfun_cmax (n, x)  result (x_max)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n
    complex, intent (in   ) :: x (n)
    real                 :: x_max
    !
    ! - Local variables
    !


    !
    ! - Begin matfun_cmax
    !
    x_max = maxval (abs (x))
    !
  end function matfun_cmax


  !!------------------------- matfun_rad_to_deg ----------------------------!!
  !!------------------------- matfun_rad_to_deg ----------------------------!!
  !!------------------------- matfun_rad_to_deg ----------------------------!!

  !
  ! - convert x from radians to degrees
  !
  function matfun_rad_to_deg (x) result (deg)
    !
    ! - Arguments
    !
    real, intent (in   ) :: x
    real              :: deg
    !
    ! - Begin matfun_rad_to_deg
    !
    deg = x * DEGREES_PER_RADIAN
    !
  end function matfun_rad_to_deg


  !!------------------------- matfun_deg_to_rad ----------------------------!!
  !!------------------------- matfun_deg_to_rad ----------------------------!!
  !!------------------------- matfun_deg_to_rad ----------------------------!!

  !
  ! - convert x from degrees to radians
  !
  function matfun_deg_to_rad (x) result (deg)
    !
    ! - Arguments
    !
    real, intent (in   ) :: x
    real              :: deg
    !
    ! - Begin matfun_deg_to_rad
    !
    deg = x * RADIANS_PER_DEGREE
    !
  end function matfun_deg_to_rad


  !!------------------------- matfun_rad_to_htz ----------------------------!!
  !!------------------------- matfun_rad_to_htz ----------------------------!!
  !!------------------------- matfun_rad_to_htz ----------------------------!!

  !
  ! - convert x from radians to hertz - 2 pi radians / sec = 1 hertz
  !
  function matfun_rad_to_htz (x) result (hertz)
    !
    ! - Arguments
    !
    real, intent (in   ) :: x
    real              :: hertz
    !
    ! - Begin matfun_rad_to_htz
    !
    hertz = x * .5 / PI
    !
  end function matfun_rad_to_htz


  !!------------------------- matfun_htz_to_rad ----------------------------!!
  !!------------------------- matfun_htz_to_rad ----------------------------!!
  !!------------------------- matfun_htz_to_rad ----------------------------!!

  !
  ! - convert x from hetrtz to radians - 2 pi radians / sec = 1 hertz
  !
  function matfun_htz_to_rad (x) result (hertz)
    !
    ! - Arguments
    !
    real, intent (in   ) :: x
    real              :: hertz
    !
    ! - Begin matfun_htz_to_rad
    !
    hertz = x * 2. * PI
    !
  end function matfun_htz_to_rad


  !!------------------------- matfun_invert_1 ----------------------------!!
  !!------------------------- matfun_invert_1 ----------------------------!!
  !!------------------------- matfun_invert_1 ----------------------------!!

  !
  ! - invert a real value x, avoid divide by 0.
  !
  function matfun_invert_1 (x) result (inv)
    !
    ! - Arguments
    !
    real, intent (in   ) :: x
    real              :: inv
    !
    ! - Begin matfun_invert_1
    !
    if (x == 0.0) then
      !
      inv = 0.0
      !
    else
      !
      inv = 1.0 / x
      !
    end if
    !
  end function matfun_invert_1


  !!---------------------------- matfun_atan2_r ------------------------------!!
  !!---------------------------- matfun_atan2_r ------------------------------!!
  !!---------------------------- matfun_atan2_r ------------------------------!!

  !
  ! - tan(matfun_atan(x, z)) = x/z
  !   return arc tangent avoid 0., 0. problems
  !
  function matfun_atan2_r(x, z)  result (a)
    !
    ! - Arguments
    !
    real, intent (in   ) :: x
    real, intent (in   ) :: z
    real              :: a
    !
    ! - Begin matfun_atan2_r
    !
    if (x == 0.0 .and. z == 0.0) then
      !
      a = 0.0
      !
    else    ! if (x == 0.0 .and. z == 0.0) then
      !
      a = atan2 (x, z)
      !
     end if    ! if (x == 0.0 .and. z == 0.0) then
     !
  end function matfun_atan2_r
  !!---------------------------- matfun_atan2_d ------------------------------!!
  !!---------------------------- matfun_atan2_d ------------------------------!!
  !!---------------------------- matfun_atan2_d ------------------------------!!

  !
  ! - tan(matfun_atan(x, z)) = x/z
  !   return arc tangent avoid 0., 0. problems
  !
  function matfun_atan2_d(x, z)  result (a)
    !
    ! - Arguments
    !
    double precision, intent (in   ) :: x
    double precision, intent (in   ) :: z
    !
    double precision                 :: a
    !
    ! - Begin matfun_atan2_d
    !
    if (x == 0.0 .and. z == 0.0) then
      !
      a = 0.0
      !
    else    ! if (x == 0.0 .and. z == 0.0) then
      !
      a = atan2 (x, z)
      !
     end if    ! if (x == 0.0 .and. z == 0.0) then
     !
  end function matfun_atan2_d


  !!---------------------------- matfun_pow2 -------------------------------!!
  !!---------------------------- matfun_pow2 -------------------------------!!
  !!---------------------------- matfun_pow2 -------------------------------!!

  !
  ! - return the next power of 2 >= n
  !
  function matfun_pow2 (n)  result (p)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n
    integer              :: p
    !
    ! - Begin matfun_pow2
    !
    p = 2 ** ceiling (log (real (n)) / log (2.0))
    if (n*2 <= p) p = p / 2
    !
  end function matfun_pow2

  !!---------------------------- matfun_pown -------------------------------!!
  !!---------------------------- matfun_pown -------------------------------!!
  !!---------------------------- matfun_pown -------------------------------!!

  !
  ! - return the smallest even product of 2's, 3's, 4's and 5's 
  !   which contains n.
  !   from Stolt's fft code
  !
  function matfun_pown ( n ) result (p)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n
    integer              :: p
    !
    ! - Local variables
    !
    integer :: nfac
    integer :: m2
    integer :: m3
    integer :: m4
    integer :: m5
    integer :: ifac( 12 )
    integer :: n2
    integer :: n23
    integer :: n235
    integer :: m2r
    integer :: m3r
    integer :: m5r
    integer :: j
    !
    ! - Begin matfun_pown
    !
    ! -  check for n smaller than 5
    !
    if (n < 5) then
      !
      p = 4
      nfac = 2
      m2 = 2
      m3 = 0
      m4 = 0
      m5 = 0
      ifac(1) = 2
      ifac(2) = 2
      return
      !
    end if
    !
    ! - strategy:  examine all possible combinations, pick the smallest
    !
    ! - examine all factors of two, beginning with 2
    !
    p = n*2
    n2 = 1
    m2 = 0
    m4 = 0
    !
  loop_200:    &
    do
      !
      n2 = n2 * 2
      m2 = m2 + 1
      !
      ! - for a given number of two's, examine all powers of three,
      !   starting with 3^0.
      !
      n23 = n2
      m3  = 0
      !
      ! - for a given number of two's and three's, examine all powers of five,
      !  starting with 5^0.
      !
    loop_300:   &
      do
        !
        if (n23 >= n) exit loop_300
        !
        n235 = n23
        m5   = 0
        !
      loop_500:    &
        do
          !
          n235 = n235 * 5
          m5 = m5 + 1
          !
          if (n235 >= n) exit loop_500
          !
        end do loop_500
        !
        if (n235 < p) then
          !
          p   = n235
          m2r = m2
          m3r = m3
          m5r = m5
          !
        end if
        !
        n23 = n23 * 3
        m3  = m3 + 1
        m5  = 0
        !
      end do loop_300
      !
      if (n23 < p) then
        p  = n23
        m2r = m2
        m3r = m3
        m5r = m5
      end if
      !
      if (n2 >= n) exit loop_200
      !
    end do loop_200
    !
    m4   = m2r / 2
    m2   = m2r - m4*2
    m3   = m3r
    m5   = m5r
    nfac = m2 + m3 + m4 + m5
    !
    do j = 1, nfac
      !
      if ( m5 >= j ) then
        ifac(j) = 5
        !
      else if ( m4 + m5 >= j ) then
        ifac (j) = 4
        !
      else if ( m3 + m4 + m5 >= j ) then
        ifac (j) = 3
        !
      else if ( m2 + m3 + m4 + m5 >= j ) then
        ifac (j) = 2
        !
      else
        ifac (j) = 0
      end if
      !
    end do
    !
  end function matfun_pown


  !!---------------------------- matfun_round ------------------------------!!
  !!---------------------------- matfun_round ------------------------------!!
  !!---------------------------- matfun_round ------------------------------!!

  !
  ! - round n up to m values
  !
  function matfun_round (n, m)  result (r)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n
    integer, intent (in   ) :: m
    integer              :: r
    !
    ! - Begin matfun_round
    !
    r = m * ( (n + m - 1) / m)
    !
  end function matfun_round


  !!---------------------------- matfun_rinw -------------------------------!!
  !!---------------------------- matfun_rinw -------------------------------!!
  !!---------------------------- matfun_rinw -------------------------------!!

  !
  ! - number of records length n in m words
  !
  function matfun_rinw (n, m)   result (rinw)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: n
    integer, intent (in   ) :: m
    integer              :: rinw
    !
    ! - Begin matfun_rinw
    !
    if (m == 0) then
      !
      rinw = 1
      !
    else    ! if (m == 0) then
      !
      rinw = (n - 1) / m + 1
      !
    end if    ! if (m == 0) then
    !
    if (n == 0) rinw = 0
    !
  end function matfun_rinw


  !!-------------------------- matfun_find_flag ----------------------------!!
  !!-------------------------- matfun_find_flag ----------------------------!!
  !!-------------------------- matfun_find_flag ----------------------------!!

  !
  ! - find the index, i_flag within array x whose first value is x_flag
  !   search from (i1-1)*abs(inc)+1, (i2-1)*abs(inc)+1 incrementing by inc
  !
  function matfun_find_flag_r (i1, i2, inc, x, x_flag)  result (i_flag)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: i1
    integer, intent (in   ) :: i2
    integer, intent (in   ) :: inc
    real,    intent (in   ) :: x (*)
    real,    intent (in   ) :: x_flag
    integer              :: i_flag
    !
    ! - Local variables
    !
    real    :: eps
    integer :: i
    integer :: j
    integer :: inc_sign
    !
    ! - Begin matfun_find_flag_r
    !
    i_flag = 0
    eps    = max (a1 = 1e-10,    &
                  a2 = abs (x_flag) * 1e-10)
    !
    inc_sign = isign (1, i2-i1)
    i        = 1
    !
    ! - find the first non flag value
    !
  loop_thru_array:    &
    do j = i1 , i2
      !
      if (abs (x (i) - x_flag) > eps) then
        !
        i_flag = i
        exit loop_thru_array
        !
      end if
      !
      i = i + inc_sign
      !
    end do loop_thru_array
    !
  end function matfun_find_flag_r

  !
  ! - find the index, i_flag within array x whose first value is x_flag
  !   search from (i1-1)*abs(inc)+1, (i2-1)*abs(inc)+1 incrementing by inc
  !
  function matfun_find_flag_i(i1, i2, inc, x, x_flag)  result (i_flag)
    !
    ! - Arguments
    !
    integer, intent (in   ) :: i1
    integer, intent (in   ) :: i2
    integer, intent (in   ) :: inc
    integer, intent (in   ) :: x (*)
    integer, intent (in   ) :: x_flag
    integer              :: i_flag
    !
    ! - Local variables
    !
    integer :: i
    integer :: j
    integer :: inc_sign
    !
    ! - Begin matfun_find_flag_i
    !
    i_flag   = 0
    inc_sign = isign (1, i2 - i1)
    i        = 1
    !
    ! - find the first non flag value
    !
  loop_thru_array:    &
    do j = i1 , i2
      !
      if (x (i) /= x_flag) then
        !
        i_flag = i
        exit loop_thru_array
        !
      end if
      !
      i = i + inc_sign
      !
    end do loop_thru_array
    !
  end function matfun_find_flag_i
  !
  logical function matfun_same_value_l ( l1, l2 )
    !
    ! set logical flag for l1=l2
    !
    logical,          intent (in   ) :: l1
    logical,          intent (in   ) :: l2
    !
    ! Local variables
    !
    logical                          :: same_value
    !
    same_value = .false.
    !
    if ( l1 .and. l2 .or. ( .not. l1 .and. .not. l2 ) ) &
    same_value = .true.
    !
    matfun_same_value_l = same_value 
    !
    return
    !
  end function matfun_same_value_l
  !
  logical function matfun_same_value_i ( n1 , n2 )
    !
    ! set logical flag for n1=n2
    !
    integer,          intent (in   ) :: n1
    integer,          intent (in   ) :: n2
    !
    ! Local variables
    !
    logical                          :: same_value
    !
    same_value = .false.
    !
    if ( n1 .eq. n2 ) &
    same_value = .true.
    !
    matfun_same_value_i = same_value 
    !
    return
    !
  end function matfun_same_value_i
  !
  logical function matfun_same_value_r ( r1 , r2 )
    !
    ! set logical flag for r1=r2
    !
    real,             intent (in   ) :: r1
    real,             intent (in   ) :: r2
    !
    ! Local variables
    !
    logical                          :: same_value
    !
    same_value = .false.
    !
    if ( abs ( r1 - r2 ) .lt. matfun_eps(min(abs(r1),abs(r2))) ) &
    same_value = .true.
    !
    matfun_same_value_r = same_value 
    !
    return
    !
  end function matfun_same_value_r 
  !
  logical function matfun_same_value_d ( d1, d2 )
    !
    ! set logical flag for c1=c2
    !
    double precision, intent (in   ) :: d1
    double precision, intent (in   ) :: d2
    !
    ! Local variables
    !
    logical                          :: same_value
    !
    same_value = .false.
    !
    if ( abs ( d1 - d2 ) .lt. matfun_eps(real(min(abs(d1),abs(d2)))) ) &
    same_value = .true.
    !
    matfun_same_value_d = same_value 
    !
    return
    !
  end function matfun_same_value_d
  !
  logical function matfun_same_value_c ( c1, c2 )
    !
    ! set logical flag for c1=c2
    !
    character(len=*), intent (in   ) :: c1
    character(len=*), intent (in   ) :: c2
    !
    ! Local variables
    !
    logical                          :: same_value
    !
    same_value = .false.
    !
    if ( string_upper_compare ( c1, c2 ) ) &
    same_value = .true.
    !
    matfun_same_value_c = same_value 
    !
    return
    !
  end function matfun_same_value_c
  !
  !!----------------------------- end of module ----------------------------!!
  !!----------------------------- end of module ----------------------------!!
  !!----------------------------- end of module ----------------------------!!

end module matfun_module


!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!

