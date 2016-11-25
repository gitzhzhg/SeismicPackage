!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- interpolate.f90 -----------------------------!!
!!---------------------------- interpolate.f90 -----------------------------!!
!!---------------------------- interpolate.f90 -----------------------------!!

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
! Name       : INTERPOLATE 
! Category   : math
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2006-12-11   by: D. Glover
! Maturity   : production
! Purpose    : Interpolation utilities.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Interpolation utilities.
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
!                                i       i       i       i       i
!  call interpolate_3d_to_3d (n1_inp, o1_inp, d1_inp, n2_inp, o2_inp,    &
!                                i       i       i       i       i
!                             d2_inp, n3_inp, o3_inp, d3_inp, x0_inp,    &
!                                i       i       i       i       i
!                             n1_out, o1_out, d1_out, n2_out, o2_out,    &
!                                i       i       i       i       o
!                             d2_out, n3_out, o3_out, d3_out, x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp, d1_inp
!    integer   n2_inp
!    real      o2_inp, d2_inp
!    integer   n3_inp
!    real      o3_inp, d3_inp
!    real      x0_inp (n1_inp, n2_inp, n3_inp)
!    integer   n1_out
!    real      o1_out, d1_out
!    integer   n2_out
!    real      o2_out, d2_out
!    integer   n3_out
!    real      o3_out, d3_out
!    real      x0_out (n1_out, n2_out, n3_out)
!
!
!                                i       i       i
!  call interpolate_3d_to_2d (n1_inp, o1_inp, d1_inp, &
!                                i       i       i
!                             n2_inp, o2_inp, d2_inp, &
!                                i       i       i       i
!                             n3_inp, o3_inp, d3_inp, x0_inp, &
!                                i       i       i
!                             n1_out, o1_out, d1_out, &
!                                i       i       i       i
!                             n2_out, o2_out, d2_out, x3_out, &
!                                o
!                             x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    integer   n2_inp
!    real      o2_inp
!    real      d2_inp
!    integer   n3_inp
!    real      o3_inp
!    real      d3_inp
!    real      x0_inp (n1_inp, n2_inp, n3_inp)
!    integer   n1_out
!    real      o1_out
!    real      d1_out
!    integer   n2_out
!    real      o2_out
!    real      d2_out
!    real      x3_out
!    real      x0_out (n1_out, n2_out)
!
!
!                                i       i       i
!  call interpolate_3d_to_1d (n1_inp, o1_inp, d1_inp,    &
!                                i       i       i
!                             n2_inp, o2_inp, d2_inp,    &
!                                i       i       i
!                             n3_inp, o3_inp, d3_inp,    &
!                                i
!                             x0_inp,                    &
!                                i       i       i
!                             n1_out, o1_out, d1_out,    &
!                                i       i       o
!                             x2_out, x3_out, x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    integer   n2_inp
!    real      o2_inp
!    real      d2_inp
!    integer   n3_inp
!    real      o3_inp
!    real      d3_inp
!    real      x0_inp (n1_inp, n2_inp, n3_inp)
!    integer   n1_out
!    real      o1_out
!    real      d1_out
!    real      x2_out
!    real      x3_out
!    real      x0_out (n1_out)
!
!
!                                i       i       i
!  call interpolate_3d_to_0d (n1_inp, o1_inp, d1_inp, &
!                                i       i       i
!                             n2_inp, o2_inp, d2_inp, &
!                                i       i       i
!                             n3_inp, o3_inp, d3_inp, &
!                                i
!                             x0_inp, &
!                                i       i       i
!                             x1_out, x2_out, x3_out, &
!                                o
!                             x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    integer   n2_inp
!    real      o2_inp
!    real      d2_inp
!    integer   n3_inp
!    real      o3_inp
!    real      d3_inp
!    real      x0_inp (n1_inp, n2_inp, n3_inp)
!    real      x1_out
!    real      x2_out
!    real      x3_out
!    real      x0_out
!
!                                i       i       i       i       i       i
!  call interpolate_2d_to_2d (n1_inp, o1_inp, d1_inp, n2_inp, o2_inp, d2_inp, &
!                                i
!                             x0_inp,                    &
!                                i       i       i       i       i       i
!                             n1_out, o1_out, d1_out, n2_out, o2_out, d2_out, &
!                                o
!                             x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    integer   n2_inp
!    real      o2_inp
!    real      d2_inp
!    real      x0_inp (n1_inp, n2_inp)
!    integer   n1_out
!    real      o1_out
!    real      d1_out
!    integer   n2_out
!    real      o2_out
!    real      d2_out
!    real      x0_out (n1_out, n2_out)
!
!
!                                i       i       i 
!  call interpolate_2d_to_1d (n1_inp, o1_inp, d1_inp, &
!                                i       i       i       i
!                             n2_inp, o2_inp, d2_inp, x0_inp, &
!                                i       i       i       i
!                             n1_out, o1_out, d1_out, x2_out, &
!                                o
!                             x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    integer   n2_inp
!    real      o2_inp
!    real      d2_inp
!    real      x0_inp (n1_inp, n2_inp)
!    integer   n1_out
!    real      o1_out
!    real      d1_out
!    real      x2_out
!    real      x0_out (n1_out)
!
!
!                                i       i       i
!  call interpolate_2d_to_0d (n1_inp, o1_inp, d1_inp,    &
!                                i       i       i
!                             n2_inp, o2_inp, d2_inp,    &
!                                i       i       i
!                             x0_inp, x1_out, x2_out,    &
!                                o
!                             x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    integer   n2_inp
!    real      o2_inp
!    real      d2_inp
!    real      x0_inp (n1_inp, n2_inp)
!    real      x1_out
!    real      x2_out
!    real      x0_out
!
!                                  i       i       i       i
!  call interpolate_2d_to_0d_n (n1_inp, n2_inp, o2_inp, d2_inp,    &
!                                          i       i       i
!                                       n3_inp, o3_inp, d3_inp,    &
!                                  i       i       i       o
!                               x0_inp, x2_out, x3_out, x0_out)
!    !
!    integer   n1_inp
!    integer   n2_inp
!    real      o2_inp
!    real      d2_inp
!    integer   n3_inp
!    real      o3_inp
!    real      d3_inp
!    real      x0_inp (n1_inp, n2_inp, n3_inp)
!    real      x2_out
!    real      x3_out
!    real      x0_out (n1_inp)
!
!                                i       i       i       i
!  call interpolate_1d_to_1d (n1_inp, o1_inp, d1_inp, x0_inp,  &
!                                i       i       i       o
!                             n1_out, o1_out, d1_out, x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    real      x0_inp (n1_inp)
!    integer   n1_out
!    real      o1_out, d1_out
!    real      x0_out (n1_out)
!
!                                i       i       i       i       i       o
!  call interpolate_1d_to_0d (n1_inp, o1_inp, d1_inp, x0_inp, x1_out, x0_out)
!    !
!    integer   n1_inp
!    real      o1_inp
!    real      d1_inp
!    real      x0_inp (n1_inp)
!    real      x1_out
!    real      x0_out
!
!
!                            i        i       i      i      o      o
!  call interpolate_brace (i_flag, nx_inp, nx_pad, rx_inp, rx_out, rx_cof)
!    !
!    integer   i_flag
!
!    integer   nx_inp, nx_pad
!    real      rx_inp (nx_inp+2)
!    real      rx_out ((nx_inp-1)*nx_pad+1)
!
!    real      rx_cof (nx_pad, 5)
!
!
!                                         i       i      i      o
!  call interpolate_bracewell_0 (nx_inp, nx_pad, rx_inp, rx_out)
!    !
!    integer   nx_inp
!    integer   nx_pad
!    real      rx_inp (nx_inp + 2)
!    real      rx_out ((nx_inp-1)*nx_pad+1)
!
!
!                              i      i      i       i       i       i      o
!  call interpolate_i_to_r (nx_inp, rx_inp, ry_inp, nx_out, x0_out, dx_out, ry_out)
!    !
!    integer   nx_inp
!    real      rx_inp (nx_inp)
!    real      ry_inp (nx_inp)
!
!    integer   nx_out
!    real      x0_out
!    real      dx_out
!    real      ry_out (nx_out)
!
!
!                              i    i    i    i    i   i   i  
!  call interpolate_i_to_r_2d (nyi, ixi, nxi, mxi, xi, yi, zi,  &
!                                 i       i       i
!                              nx_grd, x0_grd, dx_grd,    &
!                                 i       i       i
!                              ny_grd, y0_grd, dy_grd,    &
!                                o    o     o     o       o
!                              z_hor, yofy, zofx, zofy, z_xy)
!    !
!    integer   nyi
!    integer   ixi (nyi)
!    integer   nxi (nyi)
!
!    integer   mxi
!    real      xi (mxi)
!    real      yi (mxi)
!    real      zi (mxi)
!
!    integer   nx_grd
!    real      x0_grd
!    real      dx_grd
!
!    integer   ny_grd
!    real      y0_grd
!    real      dy_grd
!
!    real      z_hor (nx_grd, ny_grd)
!
!    real      yofy (nyi)
!    real      zofx (nyi)
!    real      zofy (ny_grd)
!    real      z_xy (nx_grd, nyi)
!
!
!  find the two indices ix_inp_1, ix_inp_2 
!  and the interpolation coefficients, fx_inp_1, fx_inp_2,
!  for the point rx_out within the array of points rx_inp.
!
!                                  i      i      i
!  call interpolate_find_index (nx_inp, rx_inp, rx_out,   &
!                                  o         o         o         o
!                               ix_inp_1, ix_inp_2, fx_inp_1, fx_inp_2)
!    !
!    integer   nx_inp # of elements in rx_inp to consider
!    real      rx_inp (nx_inp)
!
!    real      rx_out
!
!    integer   ix_inp_1 (nx_out) nearest point to left (rx_inp<rx_out)
!    integer   ix_inp_2 (nx_out) nearest point to right (rx_inp>rx_out)
!    real      fx_inp_1 (nx_out) weight to left side
!    real      fx_inp_2 (nx_out) weight to right side
!
!
!                                    i      i       i      i
!  call interpolate_find_index_n (nx_inp, rx_inp, nx_out, rx_out, &
!                                    o         o         o         o
!                                 ix_inp_1, ix_inp_2, fx_inp_1, fx_inp_2)
!    !
!    integer   nx_inp # of elements in rx_inp to consider
!    real      rx_inp    (nx_inp)
!
!    integer   nx_out
!    real      rx_out    (nx_out)
!
!    integer   ix_inp_1 (nx_out) nearest point to left (rx_inp<rx_out)
!    integer   ix_inp_2 (nx_out) nearest point to right (rx_inp>rx_out)
!    real      fx_inp_1 (nx_out) weight to left side
!    real      fx_inp_2 (nx_out) weight to right side
!
!
!                                    i      i       i       i       i
!  call interpolate_find_index_g (nx_inp, rx_inp, nx_out, x0_out, dx_out,   &
!                                    o         o         o         o
!                                 ix_inp_1, ix_inp_2, fx_inp_1, fx_inp_2)
!    !
!    integer   nx_inp # of elements in rx_inp to consider
!    real      rx_inp (nx_inp)
!
!    integer   nx_out
!    real      x0_out
!    real      dx_out
!
!    integer   ix_inp_1 (nx_out) nearest point to left (rx_inp<rx_out)
!    integer   ix_inp_2 (nx_out) nearest point to right (rx_inp>rx_out)
!    real      fx_inp_1 (nx_out) weight to left side
!    real      fx_inp_2 (nx_out) weight to right side
! 
!                                    i      i       i       
!  call interpolate_find_index_h (nx_inp, x0_inp, dx_inp, &
!                                    i      i       i       
!                                 nx_out, x0_out, dx_out, &
!                                    o         o         o         o
!                                 ix_inp_1, ix_inp_2, fx_inp_1, fx_inp_2)
!    !
!  call interpolate_find_index_h1 (nx_inp, x0_inp, dx_inp, &
!                                     i      i       i       
!                                  x1_out, &
!                                    o         o         o         o
!                                 ix_inp_1, ix_inp_2, fx_inp_1, fx_inp_2)
!    !
!    integer   nx_inp 
!    real      x0_inp
!    real      dx_inp
!
!    integer   nx_out
!    real      x0_out
!    real      dx_out
!
!    integer   ix_inp_1 (nx_out) nearest point to left (rx_inp<rx_out)
!    integer   ix_inp_2 (nx_out) nearest point to right (rx_inp>rx_out)
!    real      fx_inp_1 (nx_out) weight to left side
!    real      fx_inp_2 (nx_out) weight to right side
! 
!    o                           i
!  x_inv = interpolate_invert_1 (x)  result ()
!    !
!    real    x_inv    - Inverse of x (0.0 if x = 0.0)
!    real    x
!
!
! return the next power of 2 >= n
!  o                     i
!  p = interpolate_pow2 (n)  result (p)
!    integer :: n
!    integer :: p
!
!  interpolate_fft  is used to interpolate a vector using ffts
!  create the interpolate_fft object
!                                o    i
!  call interpolate_fft_create ( o, c_title, &
!                                i                     i
!                                interpolation_factor, nt_inp, &
!                                i       o
!                                lu_out, i_err )
!    character (len=*) :: c_title ! title for print
!    integer           :: interpolation factor ! interp factor
!    integer           :: nt_inp     ! input vector length
!    integer           :: lu_out     ! print unit number lu_out<0 no print
!    integer           :: i_err      ! error flag 0-ok <0-error
!
!  interpolate nt_inp samples of tr_inp into nt_out samples of tr_out
!    call interpolate_fft_apply ( o, nt_inp, tr_inp, nt_out, tr_out )
!    integer           :: nt_inp     ! number of samples in input trace
!    real              :: tr_inp (:) ! input trace
!    integer           :: nt_inp     ! number of samples in output trace
!    real              :: tr_out (:) ! output trace
!
!  delete the interpolate_fft object
!    call interpolate_fft_delete ( o )
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
! 31. 2006-12-11  D. Glover      Added NULLIFY statements for Intel compiler
! 30  2006-09-21  Douglas Hanson Add interpolate_i_to_r_0
! 29. 2006-08-31  Douglas Hanson Fix print.
! 28. 2006-06-22  Douglas Hanson Add 1d_to_0d_c
! 27. 2006-04-25  Douglas Hanson Add 2d_to_1d_j to interface.
! 26. 2006-01-10  B. Menger      Removed Unused Variables.
! 25  2005-06-14  Goodger        Change arguments zofy and ix_grd in subroutine
!                                I_TO_R_2D from intent out to intent inout to
!                                satisfy absoft 9.0 compiler.
! 24  2005-01-31  R.S.Day        Remove print in interpolate_lagrange_create.
! 23  2004-09-14  R.S.Day        Promoted to beta.
! 22  2004-08-03  Douglas Hanson Fix lagrange array size.
! 21  2004-07-01  Douglas Hanson Fix complex fft interpoaltion.
! 20  2004-06-22  Douglas Hanson Add interpolator print.
! 19  2004-06-10  Douglas Hanson linear interpolator changes.
! 18  2004-04-22  Douglas Hanson Sinc interpolation changes.
! 17  2004-03-29  Douglas Hanson Add interpolate_lagrange_apply_c
! 16  2004-03-04  R.S.Day        Initialize a0_out in interpolate_fft_apply.
! 15  2004-02-16  Douglas Hanson Modify Langrange normalization.
! 14  2003-07-10  Douglas Hanson Add double precision routines.
! 13  2003-05-23  Douglas Hanson Add Lagrange interpolation
! 12  2002-02-04  Douglas Hanson Modify dx_inp, dx_out in sinc.
! 11  2001-12-14  Douglas Hanson Add interpolate_i_to_r_fft
! 10  2001-11-12  Douglas Hanson Add interpolate_3d_to_1d_j
!  9  2001-11-08  Douglas Hanson Add interpolate_i_to_r_p
!  8  2001-11-07  Douglas Hanson Add interpolate_3d_to_1d_a, b
!  7  2001-10-25  Karen Goodger  Change labels on if blocks to satisfy intel
!                                compiler. 
!  6  2001-08-22  Douglas Hanson Add interpolate_amp_phase
!  5  2001-08-01  Douglas Hanson Add interpolate_i_to_r_0d
!  4  2000-09-13  Douglas Hanson Add interpolate_fft
!  3  2000-08-25  Douglas Hanson cpsfcr
!  2  2000-06-14  Brad Kruse     Review for standards.
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
!
module interpolate_module
  !
  ! Module references
  !
  use fft_module
  use getsys_module
  use memfun_module
  use named_constants_module
  use pc_module
  use pcpsx_module
  !
  implicit none
  !
  private
  public :: interpolate
  public :: interpolate_3d_to_1d_i
  public :: interpolate_3d_to_1d_j
  public :: interpolate_3d_to_1d_a
  public :: interpolate_3d_to_1d_b
  public :: interpolate_3d_to_3d
  public :: interpolate_3d_to_2d
  public :: interpolate_3d_to_1d
  public :: interpolate_3d_to_0d
  public :: interpolate_2d_to_2d
  public :: interpolate_2d_to_1d
  public :: interpolate_2d_to_1d_j
  public :: interpolate_2d_to_0d
  public :: interpolate_2d_to_0d_n
  public :: interpolate_1d_to_1d
  public :: interpolate_1d_to_0d
  public :: interpolate_brace
  public :: interpolate_bracewell_0
  public :: interpolate_r_to_i
  public :: interpolate_i_to_i
  public :: interpolate_i_to_i_d
  public :: interpolate_i_to_r_0
  public :: interpolate_i_to_r
  public :: interpolate_i_to_r_p
  public :: interpolate_i_to_r_0d
  public :: interpolate_i_to_r_2d
  public :: interpolate_i_to_r_fft
  public :: interpolate_i_to_r_nearest
  public :: interpolate_find_index
  public :: interpolate_find_index_d
  public :: interpolate_find_index_n
  public :: interpolate_find_index_g
  public :: interpolate_find_index_h
  public :: interpolate_find_index_h1
  public :: interpolate_find_index_hn
  public :: interpolate_invert_1
  public :: interpolate_fft_create
  public :: interpolate_fft_delete
  public :: interpolate_fft_apply
  public :: interpolate_fft_test
  public :: interpolate_linear_create
  public :: interpolate_linear_delete
  public :: interpolate_linear_apply
  public :: interpolate_linear_apply_r
  public :: interpolate_linear_apply_c
  public :: interpolate_linear_print
  public :: interpolate_lagrange_create
  public :: interpolate_lagrange_r_to_i
  public :: interpolate_lagrange_i_to_r
  public :: interpolate_lagrange_r_to_r
  public :: interpolate_lagrange_delete
  public :: interpolate_lagrange_apply
  public :: interpolate_lagrange_apply_r
  public :: interpolate_lagrange_apply_c
  public :: interpolate_sinc_create
  public :: interpolate_sinc_i_to_r
  public :: interpolate_sinc_r_to_i
  public :: interpolate_sinc_r_to_r
  public :: interpolate_sinc_delete
  public :: interpolate_sinc_apply
  public :: interpolate_sinc_apply_r
  public :: interpolate_sinc_apply_c
  public :: interpolate_amp_phase
  public :: interpolate_array_size
  public :: interpolate_pow2
  !
  interface interpolate
    !
    module procedure interpolate_3d_to_1d_i
    module procedure interpolate_3d_to_1d_j
    module procedure interpolate_3d_to_1d_a
    module procedure interpolate_3d_to_1d_b
    module procedure interpolate_3d_to_3d
    module procedure interpolate_3d_to_2d
    module procedure interpolate_3d_to_1d
    module procedure interpolate_3d_to_0d
    module procedure interpolate_2d_to_2d
    module procedure interpolate_2d_to_1d
    module procedure interpolate_2d_to_1d_j
    module procedure interpolate_2d_to_0d
    module procedure interpolate_2d_to_0d_n
    module procedure interpolate_1d_to_1d
    module procedure interpolate_1d_to_0d
    module procedure interpolate_1d_to_0d_c
    !
  end interface 
  !
  interface interpolate_find_index
    !
    module procedure interpolate_find_index
    module procedure interpolate_find_index_d
    module procedure interpolate_find_index_n
    module procedure interpolate_find_index_g
    module procedure interpolate_find_index_h
    module procedure interpolate_find_index_h1
    module procedure interpolate_find_index_hn
    !
  end interface 
  !
  interface interpolate_i
    !
    module procedure interpolate_i_to_i_d
    module procedure interpolate_i_to_i
    module procedure interpolate_i_to_r
    module procedure interpolate_i_to_r_p
    module procedure interpolate_i_to_r_0d
    module procedure interpolate_i_to_r_2d
    module procedure interpolate_i_to_r_fft
    !
  end interface 
  !
  !
  interface interpolate_linear_create
    !
    module procedure interpolate_linear_create
    module procedure interpolate_linear_create_ir
    module procedure interpolate_linear_create_ri
    module procedure interpolate_linear_create_rr
    !
  end interface 
  !
  interface interpolate_linear_apply
    !
    module procedure interpolate_linear_apply_r
    module procedure interpolate_linear_apply_c
    !
  end interface 
  !
  interface interpolate_lagrange_create
    !
    module procedure interpolate_lagrange_create
    module procedure interpolate_lagrange_create_ir
    module procedure interpolate_lagrange_create_ri
    module procedure interpolate_lagrange_create_rr
    !
  end interface 
  !
  interface interpolate_lagrange_apply
    !
    module procedure interpolate_lagrange_apply_r
    module procedure interpolate_lagrange_apply_c
    !
  end interface 
  !
  interface interpolate_sinc_create
   !
    module procedure interpolate_sinc_create
    module procedure interpolate_sinc_create_ir
    module procedure interpolate_sinc_create_ri
    module procedure interpolate_sinc_create_rr
    !
  end interface 
  !
  interface interpolate_sinc_apply
    !
    module procedure interpolate_sinc_apply_r
    module procedure interpolate_sinc_apply_c
    !
  end interface 
  !
  interface interpolate_fft_apply
    !
    module procedure interpolate_fft_apply_r
    module procedure interpolate_fft_apply_c
    !
  end interface 
  !
  type, public :: interpolate_linear_struct
    !
    !private
    !
    integer                   :: i_call
    integer                   :: lu_out    ! print unit
    !
    integer                   :: nx_inp    ! number of  input points
    real                      :: x0_inp    ! min input value
    real                      :: x1_inp    ! max input value
    real                      :: dx_inp    ! min input separation
    integer                   :: ny_inp    ! number of  input points
    real                      :: y0_inp    ! min input value
    real                      :: y1_inp    ! max input value
    !
    integer                   :: nx_out    ! number of output points
    real                      :: x0_out    ! output point origin
    real                      :: x1_out    ! max output value
    real                      :: dx_out    ! max input separation
    integer                   :: ny_out    ! number of output points
    real                      :: y0_out    ! output point origin
    real                      :: y1_out    ! max output value
    !
    real,             pointer :: ry_inp(:) ! input point values
    real,             pointer :: ry_out(:) ! output point values
    !
    integer,          pointer :: ix_inp_1 ( : ) ! x int index 1
    integer,          pointer :: ix_inp_2 ( : ) ! x int index 2
    real,             pointer :: fx_inp_1 ( : ) ! x int coeff 1
    real,             pointer :: fx_inp_2 ( : ) ! x int coeff 2
    !
  end type interpolate_linear_struct
  !
  type, public :: interpolate_lagrange_struct
    !
    private
    !
    integer                   :: lu_out    ! print unit
    real                      :: rx_inc    ! one sided interp window width
    !
    integer                   :: nx_inp    ! number of  input points orig
    real                      :: x0_inp    ! min input value orig
    real                      :: x1_inp    ! max input value orig
    real                      :: dx_inp    ! min input separation orig
    !
    integer                   :: ny_inp    ! number of  input points uniform
    real                      :: y0_inp    ! min input value uniform
    real                      :: y1_inp    ! max input value uniform
    real                      :: dy_inp    ! min input separation uniform
    !real,             pointer :: rx_inp(:) ! input point values
    !
    integer                   :: nx_out    ! number of output points orig
    real                      :: x0_out    ! output point origin orig
    real                      :: x1_out    ! max output value orig
    real                      :: dx_out    ! max input separation orig
    !
    integer                   :: ny_out    ! number of output points uniform
    real                      :: y0_out    ! output point origin uniform
    real                      :: y1_out    ! max output value uniform
    real                      :: dy_out    ! max input separation uniform
    !
    !real,             pointer :: rx_out(:) ! output point values
    !
    integer                   :: n0_cof    ! number interpolation coefficients
    integer,          pointer :: i0_cof(:,:) ! num   input nodes to output node
    real,             pointer :: w0_cof(:,:) ! weight for input gt_inp
    !
  end type interpolate_lagrange_struct
  !
  type, public :: interpolate_sinc_struct
    !
    private
    !
    integer                   :: lu_out    ! print unit
    real                      :: rx_inc    ! one sided interp window width
    !
    integer                   :: nx_inp    ! number of  input points
    real,             pointer :: rx_inp(:) ! input point values
    !
    real                      :: x0_inp    ! min input value
    real                      :: x1_inp    ! max input value
    real                      :: dx_inp    ! min input separation
    !
    integer                   :: nx_out    ! number of output points
    real,             pointer :: rx_out(:) ! output point values
    !
    real                      :: x0_out    ! output point origin
    real                      :: x1_out    ! max output value
    real                      :: dx_out    ! max input separation
    !
    integer                   :: n0_cof    ! number interpolation coefficients
    integer,          pointer :: i1_inp(:) ! first input node  to output node
    integer,          pointer :: i2_inp(:) ! last  input node  to output node
    integer,          pointer :: i0_inp(:) ! first index within w0_inp
    integer,          pointer :: n0_inp(:) ! num   input nodes to output node
    real,             pointer :: w0_inp(:) ! weight for input gt_inp
    !
  end type interpolate_sinc_struct
  !
  type, public :: interpolate_fft_struct              
    !
    private
    !
    character(len=filename_length) :: c_title ! char title
    !
    logical                    :: origin_shift       ! apply origin shift
    logical                    :: interpolate_real   ! real    vector interp
    logical                    :: interpolate_cplx   ! complex vector interp
    real                       :: interpolation_factor ! interp factor
    real                       :: sr_inp ! real    to complex scale f
    real                       :: sr_out ! complex to real    scale i
    real                       :: sc_inp ! complex to complex scale f
    real                       :: sc_out ! complex to complex scale i
    !
    integer                    :: sg_inp ! f fft sign
    integer                    :: sg_out ! i fft sign
    !
    integer                    :: ft_inp
    integer                    :: nt_inp
    real                       :: t0_inp
    real                       :: t1_inp
    real                       :: t2_inp
    real                       :: dt_inp
    !
    integer                    :: ft_out
    integer                    :: nt_out
    real                       :: t0_out
    real                       :: t1_out
    real                       :: t2_out
    real                       :: dt_out
    !
    type (fft_struct), pointer :: or_inp
    type (fft_struct), pointer :: or_out
    !
    type (fft_struct), pointer :: oc_inp
    type (fft_struct), pointer :: oc_out
    !
  end type interpolate_fft_struct
  !
  character (len=100), public, save :: INTERPOLATE_IDENT = &
    '$Id: interpolate.f90,v 1.31 2006/12/11 14:18:36 Glover prod sps $'
  !
  contains
  !
  subroutine interpolate_3d_to_3d (n1_inp, o1_inp, d1_inp, n2_inp, o2_inp,    &
                                   d2_inp, n3_inp, o3_inp, d3_inp, x0_inp,    &
                                   n1_out, o1_out, d1_out, n2_out, o2_out,    &
                                   d2_out, n3_out, o3_out, d3_out, x0_out)
    !
    ! 3D interpolation from x0_inp to x0_out
    !   should be changed to precompute indices and coefficients 
    !   for each dimension
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: o1_out
    real,     intent(in   ) :: d1_out

    integer,  intent(in   ) :: n2_out
    real,     intent(in   ) :: o2_out
    real,     intent(in   ) :: d2_out

    integer,  intent(in   ) :: n3_out
    real,     intent(in   ) :: o3_out
    real,     intent(in   ) :: d3_out

    real,     intent(  out) :: x0_out (n1_out, n2_out, n3_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp, x1_out, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2

    integer :: i2_out
    real    :: x2_inp, x2_out, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2

    integer :: i3_out
    real    :: x3_inp, x3_out, d3_inv
    integer :: i3_1, i3_2
    real    :: f3_1, f3_2
    !
    ! Begin 
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    do_i3 : do i3_out = 1, n3_out
      !
      x3_out = (i3_out - 1) * d3_out + o3_out
      i3_1 = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
      i3_2 = max(1, min(n3_inp, i3_1+1))
      x3_inp = (i3_1 - 1) * d3_inp + o3_inp
      f3_2 = max(0., min(1., (x3_out-x3_inp)*d3_inv))
      f3_1 = 1. - f3_2
      !
      do_i2 : do i2_out = 1, n2_out
        !
        x2_out = (i2_out - 1) * d2_out + o2_out
        i2_1 = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
        i2_2 = max(1, min(n2_inp, i2_1+1))
        x2_inp = (i2_1 - 1) * d2_inp + o2_inp
        f2_2 = max(0., min(1., (x2_out-x2_inp)*d2_inv))
        f2_1 = 1. - f2_2
        !
      do_i1 : do i1_out = 1, n1_out
          !
          x1_out = (i1_out - 1) * d1_out + o1_out
          i1_1 = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
          i1_2 = max(1, min(n1_inp, i1_1+1))
          x1_inp = (i1_1 - 1) * d1_inp + o1_inp
          f1_2 = max(0., min(1., (x1_out-x1_inp)*d1_inv))
          f1_1 = 1. - f1_2
          !
          x0_out (i1_out, i2_out, i3_out)                         &
            =   f1_1 * f2_1 * f3_1 * x0_inp (i1_1, i2_1, i3_1)    &
              + f1_2 * f2_1 * f3_1 * x0_inp (i1_2, i2_1, i3_1)    &
              + f1_1 * f2_2 * f3_1 * x0_inp (i1_1, i2_2, i3_1)    &
              + f1_2 * f2_2 * f3_1 * x0_inp (i1_2, i2_2, i3_1)    &
              + f1_1 * f2_1 * f3_2 * x0_inp (i1_1, i2_1, i3_2)    &
              + f1_2 * f2_1 * f3_2 * x0_inp (i1_2, i2_1, i3_2)    &
              + f1_1 * f2_2 * f3_2 * x0_inp (i1_1, i2_2, i3_2)    &
              + f1_2 * f2_2 * f3_2 * x0_inp (i1_2, i2_2, i3_2)
          !
        end do do_i1
        !
      end do do_i2
      !
    end do do_i3
    !
    return
    !
  end subroutine interpolate_3d_to_3d
  !
  ! 3D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_3d_to_2d (n1_inp, o1_inp, d1_inp, &
                                   n2_inp, o2_inp, d2_inp, &
                                   n3_inp, o3_inp, d3_inp, &
                                   x0_inp, &
                                   n1_out, o1_out, d1_out, &
                                   n2_out, o2_out, d2_out, &
                                   x3_out, &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: o1_out
    real,     intent(in   ) :: d1_out

    integer,  intent(in   ) :: n2_out
    real,     intent(in   ) :: o2_out
    real,     intent(in   ) :: d2_out

    real,     intent(in   ) :: x3_out

    real,     intent(  out) :: x0_out (n1_out, n2_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp, x1_out, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2

    integer :: i2_out
    real    :: x2_inp, x2_out, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2


    real    :: x3_inp, d3_inv
    integer :: i3_1, i3_2
    real    :: f3_1, f3_2
    !
    ! Begin interpolate_3d_to_2d
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    i3_1 = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
    i3_2 = max(1, min(n3_inp, i3_1+1))
    x3_inp = (i3_1 - 1) * d3_inp + o3_inp
    f3_2 = max(0., min(1., (x3_out-x3_inp)*d3_inv))
    f3_1 = 1. - f3_2
    !
  do_i2:    &
    do i2_out = 1, n2_out
      !
      x2_out = (i2_out - 1) * d2_out + o2_out
      i2_1 = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
      i2_2 = max(1, min(n2_inp, i2_1+1))
      x2_inp = (i2_1 - 1) * d2_inp + o2_inp
      f2_2 = max(0., min(1., (x2_out-x2_inp)*d2_inv))
      f2_1 = 1. - f2_2
      !
    do_i1:    &
      do i1_out = 1, n1_out
        !
        x1_out = (i1_out - 1) * d1_out + o1_out
        i1_1   = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
        i1_2   = max(1, min(n1_inp, i1_1+1))
        x1_inp = (i1_1 - 1) * d1_inp + o1_inp
        f1_2   = max(0., min(1., (x1_out-x1_inp)*d1_inv))
        f1_1   = 1. - f1_2
        !
        x0_out (i1_out, i2_out)     &
          =   f1_1 * f2_1 * f3_1 * x0_inp (i1_1, i2_1, i3_1) &
            + f1_2 * f2_1 * f3_1 * x0_inp (i1_2, i2_1, i3_1) &
            + f1_1 * f2_2 * f3_1 * x0_inp (i1_1, i2_2, i3_1) &
            + f1_2 * f2_2 * f3_1 * x0_inp (i1_2, i2_2, i3_1) &
            + f1_1 * f2_1 * f3_2 * x0_inp (i1_1, i2_1, i3_2) &
            + f1_2 * f2_1 * f3_2 * x0_inp (i1_2, i2_1, i3_2) &
            + f1_1 * f2_2 * f3_2 * x0_inp (i1_1, i2_2, i3_2) &
            + f1_2 * f2_2 * f3_2 * x0_inp (i1_2, i2_2, i3_2)
        !
      end do do_i1
      !
    end do do_i2
    !
    return
    !
  end subroutine interpolate_3d_to_2d
  !
  ! 3D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_3d_to_1d (n1_inp, o1_inp, d1_inp,    &
                                   n2_inp, o2_inp, d2_inp,    &
                                   n3_inp, o3_inp, d3_inp,    &
                                   x0_inp,                    &
                                   n1_out, o1_out, d1_out,    &
                                   x2_out,                    &
                                   x3_out,                    &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: o1_out
    real,     intent(in   ) :: d1_out

    real,     intent(in   ) :: x2_out

    real,     intent(in   ) :: x3_out

    real,     intent(  out) :: x0_out (n1_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp, x1_out, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2

    real    :: x2_inp, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2


    real    :: x3_inp, d3_inv
    integer :: i3_1, i3_2
    real    :: f3_1, f3_2
    !
    ! Begin interpolate_3d_to_1d
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    i3_1   = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
    i3_2   = max(1, min(n3_inp, i3_1+1))
    x3_inp = (i3_1 - 1) * d3_inp + o3_inp
    f3_2   = max(0., min(1., (x3_out-x3_inp)*d3_inv))
    f3_1   = 1. - f3_2
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
    do i1_out = 1, n1_out
      !
      x1_out = (i1_out - 1) * d1_out + o1_out
      i1_1   = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
      i1_2   = max(1, min(n1_inp, i1_1+1))
      x1_inp = (i1_1 - 1) * d1_inp + o1_inp
      f1_2   = max(0., min(1., (x1_out-x1_inp)*d1_inv))
      f1_1   = 1.0 - f1_2
      !
      x0_out (i1_out) =   f1_1 * f2_1 * f3_1 * x0_inp (i1_1, i2_1, i3_1)    &
                        + f1_2 * f2_1 * f3_1 * x0_inp (i1_2, i2_1, i3_1)    &
                        + f1_1 * f2_2 * f3_1 * x0_inp (i1_1, i2_2, i3_1)    &
                        + f1_2 * f2_2 * f3_1 * x0_inp (i1_2, i2_2, i3_1)    &
                        + f1_1 * f2_1 * f3_2 * x0_inp (i1_1, i2_1, i3_2)    &
                        + f1_2 * f2_1 * f3_2 * x0_inp (i1_2, i2_1, i3_2)    &
                        + f1_1 * f2_2 * f3_2 * x0_inp (i1_1, i2_2, i3_2)    &
                        + f1_2 * f2_2 * f3_2 * x0_inp (i1_2, i2_2, i3_2)
      !
    end do    ! do i1_out = 1, n1_out
    !
    return
    !
  end subroutine interpolate_3d_to_1d
  !
  ! 3D interpolation from x0_inp to x0_out at x1_out, x2_out, x3_out
  !
  subroutine interpolate_3d_to_0d (n1_inp, o1_inp, d1_inp, &
                                   n2_inp, o2_inp, d2_inp, &
                                   n3_inp, o3_inp, d3_inp, &
                                   x0_inp, &
                                   x1_out, x2_out, x3_out, &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)

    real,     intent(in   ) :: x1_out
    real,     intent(in   ) :: x2_out
    real,     intent(in   ) :: x3_out
    real,     intent(  out) :: x0_out
    !
    ! Local variables
    !

    real    :: x1_inp, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2


    real    :: x2_inp, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2


    real    :: x3_inp, d3_inv
    integer :: i3_1, i3_2
    real    :: f3_1, f3_2
    !
    ! Begin 
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    i3_1   = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
    i3_2   = max(1, min(n3_inp, i3_1+1))
    x3_inp = (i3_1 - 1) * d3_inp + o3_inp
    f3_2   = max(0., min(1., (x3_out-x3_inp)*d3_inv))
    f3_1   = 1. - f3_2
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
    i1_1   = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
    i1_2   = max(1, min(n1_inp, i1_1+1))
    x1_inp = (i1_1 - 1) * d1_inp + o1_inp
    f1_2   = max(0., min(1., (x1_out-x1_inp)*d1_inv))
    f1_1   = 1. - f1_2
    !
    x0_out =   f1_1 * f2_1 * f3_1 * x0_inp (i1_1, i2_1, i3_1)    &
             + f1_2 * f2_1 * f3_1 * x0_inp (i1_2, i2_1, i3_1)    &
             + f1_1 * f2_2 * f3_1 * x0_inp (i1_1, i2_2, i3_1)    &
             + f1_2 * f2_2 * f3_1 * x0_inp (i1_2, i2_2, i3_1)    &
             + f1_1 * f2_1 * f3_2 * x0_inp (i1_1, i2_1, i3_2)    &
             + f1_2 * f2_1 * f3_2 * x0_inp (i1_2, i2_1, i3_2)    &
             + f1_1 * f2_2 * f3_2 * x0_inp (i1_1, i2_2, i3_2)    &
             + f1_2 * f2_2 * f3_2 * x0_inp (i1_2, i2_2, i3_2)

    return
    !
  end subroutine interpolate_3d_to_0d
  !
  ! 3D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_3d_to_1d_i (n1_inp, o1_inp, d1_inp,    &
                                     n2_inp, o2_inp, d2_inp,    &
                                     n3_inp, o3_inp, d3_inp,    &
                                     x0_inp,                    &
                                     n1_out, x1_out,            &
                                     x2_out,                    &
                                     x3_out,                    &
                                     x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: x1_out(n1_out)

    real,     intent(in   ) :: x2_out

    real,     intent(in   ) :: x3_out

    real,     intent(  out) :: x0_out (n1_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2

    real    :: x2_inp, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2


    real    :: x3_inp, d3_inv
    integer :: i3_1, i3_2
    real    :: f3_1, f3_2
    !
    ! Begin interpolate_3d_to_1d
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    i3_1   = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
    i3_2   = max(1, min(n3_inp, i3_1+1))
    x3_inp = (i3_1 - 1) * d3_inp + o3_inp
    f3_2   = max(0., min(1., (x3_out-x3_inp)*d3_inv))
    f3_1   = 1. - f3_2
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
    do i1_out = 1, n1_out
      !
      i1_1   = max(1, min(n1_inp, int((x1_out(i1_out)-o1_inp)*d1_inv)+1))
      i1_2   = max(1, min(n1_inp, i1_1+1))
      x1_inp = (i1_1 - 1) * d1_inp + o1_inp
      f1_2   = max(0., min(1., (x1_out(i1_out)-x1_inp)*d1_inv))
      f1_1   = 1.0 - f1_2
      !
      x0_out (i1_out) =   f1_1 * f2_1 * f3_1 * x0_inp (i1_1, i2_1, i3_1)    &
                        + f1_2 * f2_1 * f3_1 * x0_inp (i1_2, i2_1, i3_1)    &
                        + f1_1 * f2_2 * f3_1 * x0_inp (i1_1, i2_2, i3_1)    &
                        + f1_2 * f2_2 * f3_1 * x0_inp (i1_2, i2_2, i3_1)    &
                        + f1_1 * f2_1 * f3_2 * x0_inp (i1_1, i2_1, i3_2)    &
                        + f1_2 * f2_1 * f3_2 * x0_inp (i1_2, i2_1, i3_2)    &
                        + f1_1 * f2_2 * f3_2 * x0_inp (i1_1, i2_2, i3_2)    &
                        + f1_2 * f2_2 * f3_2 * x0_inp (i1_2, i2_2, i3_2)
      !
    end do    ! do i1_out = 1, n1_out
    !
    return
    !
  end subroutine interpolate_3d_to_1d_i
  !
  subroutine interpolate_3d_to_1d_j (n1_inp, o1_inp, d1_inp,         &
                                     n2_inp, o2_inp, d2_inp,         &
                                     n3_inp, o3_inp, d3_inp,         &
                                     x0_inp,                         &
                                     n1_out, i1_1, i1_2, f1_1, f1_2, &
                                     x2_out,                         &
                                     x3_out,                         &
                                     x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)

    integer,  intent(in   ) :: n1_out
    integer,  intent(in   ) :: i1_1(n1_out)
    integer,  intent(in   ) :: i1_2(n1_out)
    real,     intent(in   ) :: f1_1(n1_out)
    real,     intent(in   ) :: f1_2(n1_out)

    real,     intent(in   ) :: x2_out

    real,     intent(in   ) :: x3_out

    real,     intent(  out) :: x0_out (n1_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    ::         d1_inv 

    real    :: x2_inp, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2


    real    :: x3_inp, d3_inv
    integer :: i3_1, i3_2
    real    :: f3_1, f3_2
    !
    ! Begin interpolate_3d_to_1d
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    i3_1   = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
    i3_2   = max(1, min(n3_inp, i3_1+1))
    x3_inp = (i3_1 - 1) * d3_inp + o3_inp
    f3_2   = max(0., min(1., (x3_out-x3_inp)*d3_inv))
    f3_1   = 1. - f3_2
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
    do i1_out = 1, n1_out
      !
      x0_out (i1_out) = &
  f1_1(i1_out) * f2_1 * f3_1 * x0_inp (i1_1(i1_out), i2_1, i3_1)    &
+ f1_2(i1_out) * f2_1 * f3_1 * x0_inp (i1_2(i1_out), i2_1, i3_1)    &
+ f1_1(i1_out) * f2_2 * f3_1 * x0_inp (i1_1(i1_out), i2_2, i3_1)    &
+ f1_2(i1_out) * f2_2 * f3_1 * x0_inp (i1_2(i1_out), i2_2, i3_1)    &
+ f1_1(i1_out) * f2_1 * f3_2 * x0_inp (i1_1(i1_out), i2_1, i3_2)    &
+ f1_2(i1_out) * f2_1 * f3_2 * x0_inp (i1_2(i1_out), i2_1, i3_2)    &
+ f1_1(i1_out) * f2_2 * f3_2 * x0_inp (i1_1(i1_out), i2_2, i3_2)    &
+ f1_2(i1_out) * f2_2 * f3_2 * x0_inp (i1_2(i1_out), i2_2, i3_2)
      !
    end do    ! do i1_out = 1, n1_out
    !
    return
    !
  end subroutine interpolate_3d_to_1d_j
  !
  ! 2D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_2d_to_2d (n1_inp, o1_inp, d1_inp,    &
                                   n2_inp, o2_inp, d2_inp,    &
                                   x0_inp,                    &
                                   n1_out, o1_out, d1_out,    &
                                   n2_out, o2_out, d2_out,    &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp)

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: o1_out, d1_out

    integer,  intent(in   ) :: n2_out
    real,     intent(in   ) :: o2_out, d2_out

    real,     intent(  out) :: x0_out (n1_out, n2_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp, x1_out, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2

    integer :: i2_out
    real    :: x2_inp, x2_out, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2
    !
    ! Begin 
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    !
  do_i2:    &
    do i2_out = 1, n2_out
      !
      x2_out = (i2_out - 1) * d2_out + o2_out
      i2_1 = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
      i2_2 = max(1, min(n2_inp, i2_1+1))
      x2_inp = (i2_1 - 1) * d2_inp + o2_inp
      f2_2 = max(0., min(1., (x2_out-x2_inp)*d2_inv))
      f2_1 = 1. - f2_2
      !
    do_i1:    &
      do i1_out = 1, n1_out
        !
        x1_out = (i1_out - 1) * d1_out + o1_out
        i1_1 = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
        i1_2 = max(1, min(n1_inp, i1_1+1))
        x1_inp = (i1_1 - 1) * d1_inp + o1_inp
        f1_2 = max(0., min(1., (x1_out-x1_inp)*d1_inv))
        f1_1 = 1. - f1_2
        !
        x0_out (i1_out, i2_out)                 &
          =   f1_1 * f2_1 * x0_inp (i1_1, i2_1) &
            + f1_2 * f2_1 * x0_inp (i1_2, i2_1) &
            + f1_1 * f2_2 * x0_inp (i1_1, i2_2) &
            + f1_2 * f2_2 * x0_inp (i1_2, i2_2)
        !
      end do do_i1
      !
    end do do_i2
    !
    return
    !
  end subroutine interpolate_2d_to_2d
  !
  ! 2D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_2d_to_1d (n1_inp, o1_inp, d1_inp, &
                                   n2_inp, o2_inp, d2_inp, &
                                   x0_inp, &
                                   n1_out, o1_out, d1_out, &
                                   x2_out, &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp)

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: o1_out, d1_out

    real,     intent(in   ) :: x2_out

    real,     intent(  out) :: x0_out (n1_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp, x1_out, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2


    real    :: x2_inp, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2
    !
    ! Begin 
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
  do_i1:    &
    do i1_out = 1, n1_out
      !
      x1_out = (i1_out - 1) * d1_out + o1_out
      i1_1   = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
      i1_2   = max(1, min(n1_inp, i1_1+1))
      x1_inp = (i1_1 - 1) * d1_inp + o1_inp
      f1_2   = max(0., min(1., (x1_out-x1_inp)*d1_inv))
      f1_1   = 1. - f1_2

      x0_out (i1_out) =    f1_1 * f2_1 * x0_inp (i1_1, i2_1)    &
                         + f1_2 * f2_1 * x0_inp (i1_2, i2_1)    &
                         + f1_1 * f2_2 * x0_inp (i1_1, i2_2)    &
                         + f1_2 * f2_2 * x0_inp (i1_2, i2_2)
      !
    end do do_i1
    !
    return
    !
  end subroutine interpolate_2d_to_1d
  !
  subroutine interpolate_2d_to_1d_j (n1_inp, o1_inp, d1_inp,         &
                                     n2_inp, o2_inp, d2_inp,         &
                                     x0_inp,                         &
                                     n1_out, i1_1, i1_2, f1_1, f1_2, &
                                     x2_out,                         &
                                     x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    real,     intent(in   ) :: x0_inp(:,:)

    integer,  intent(in   ) :: n1_out
    integer,  intent(in   ) :: i1_1(:)
    integer,  intent(in   ) :: i1_2(:)
    real,     intent(in   ) :: f1_1(:)
    real,     intent(in   ) :: f1_2(:)

    real,     intent(in   ) :: x2_out

    real,     intent(  out) :: x0_out(:)
    !
    ! Local variables
    !
    integer :: i1_out
    real    ::         d1_inv 

    real    :: x2_inp, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    !
    !
    ! Begin interpolate_2d_to_1d
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
    do i1_out = 1, n1_out
      !
      x0_out (i1_out) = &
         f1_1(i1_out) * f2_1 * x0_inp (i1_1(i1_out), i2_1 )   &
       + f1_2(i1_out) * f2_1 * x0_inp (i1_2(i1_out), i2_1 )   &
       + f1_1(i1_out) * f2_2 * x0_inp (i1_1(i1_out), i2_2 )   &
       + f1_2(i1_out) * f2_2 * x0_inp (i1_2(i1_out), i2_2 )
      !
    end do    ! do i1_out = 1, n1_out
    !
    return
    !
  end subroutine interpolate_2d_to_1d_j
  !
  ! 2D interpolation from x0_inp to x0_out at x1_out, x2_out
  !
  subroutine interpolate_2d_to_0d (n1_inp, o1_inp, d1_inp,    &
                                   n2_inp, o2_inp, d2_inp,    &
                                   x0_inp,                    &
                                   x1_out, x2_out,            &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp)

    real,     intent(in   ) :: x1_out, x2_out

    real,     intent(  out) :: x0_out
    !
    ! Local variables
    !

    real    :: x1_inp, x2_inp, d1_inv, d2_inv
    integer :: i1_1, i1_2, i2_1, i2_2
    real    :: f1_1, f1_2, f2_1, f2_2
    !
    ! Begin 
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    d2_inv = interpolate_invert_1 (d2_inp)
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
    i1_1   = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
    i1_2   = max(1, min(n1_inp, i1_1+1))
    x1_inp = (i1_1 - 1) * d1_inp + o1_inp
    f1_2   = max(0., min(1., (x1_out-x1_inp)*d1_inv))
    f1_1   = 1. - f1_2
    !
    x0_out =    f1_1 * f2_1 * x0_inp (i1_1, i2_1) &
              + f1_2 * f2_1 * x0_inp (i1_2, i2_1) &
              + f1_1 * f2_2 * x0_inp (i1_1, i2_2) &
              + f1_2 * f2_2 * x0_inp (i1_2, i2_2)
    !
    return
    !
  end subroutine interpolate_2d_to_0d
  !
  ! 3D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_2d_to_0d_n (n1_inp,                    &
                                     n2_inp, o2_inp, d2_inp,    &
                                     n3_inp, o3_inp, d3_inp,    &
                                     x0_inp, x2_out, x3_out,    &
                                     x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp

    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp

    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp

    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)
    real,     intent(in   ) :: x2_out
    real,     intent(in   ) :: x3_out

    real,     intent(  out) :: x0_out (n1_inp)
    !
    ! Local variables
    !
    integer :: n1_out
    integer :: i1_out


    real    :: x2_inp, d2_inv

    integer :: i2_1, i2_2
    real    :: f2_1, f2_2


    real    :: x3_inp, d3_inv

    integer :: i3_1, i3_2
    real    :: f3_1, f3_2

    real    :: f2_1_f3_1
    real    :: f2_2_f3_1
    real    :: f2_1_f3_2
    real    :: f2_2_f3_2
    !
    ! Begin 
    !
    n1_out = n1_inp
    !
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    i3_1 = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
    i3_2 = max(1, min(n3_inp, i3_1+1))
    x3_inp = (i3_1 - 1) * d3_inp + o3_inp
    f3_2 = max(0., min(1., (x3_out-x3_inp)*d3_inv))
    f3_1 = 1. - f3_2
    !
    i2_1 = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2 = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2 = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1 = 1. - f2_2
    !
    f2_1_f3_1 = f2_1 * f3_1
    f2_2_f3_1 = f2_2 * f3_1
    f2_1_f3_2 = f2_1 * f3_2
    f2_2_f3_2 = f2_2 * f3_2
    !
  do_i1:    &
    do i1_out = 1, n1_out
      !
      x0_out (i1_out) =   f2_1_f3_1 * x0_inp (i1_out, i2_1, i3_1) &
                        + f2_2_f3_1 * x0_inp (i1_out, i2_2, i3_1) &
                        + f2_1_f3_2 * x0_inp (i1_out, i2_1, i3_2) &
                        + f2_2_f3_2 * x0_inp (i1_out, i2_2, i3_2)
      !
    end do do_i1
    !
    return
    !
  end subroutine interpolate_2d_to_0d_n
  !
  ! 1D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_1d_to_1d (n1_inp, o1_inp, d1_inp,    &
                                   x0_inp,                    &
                                   n1_out, o1_out, d1_out,    &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    real,     intent(in   ) :: x0_inp (n1_inp)

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: o1_out, d1_out

    real,     intent(  out) :: x0_out (n1_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp, x1_out, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2
    !
    ! Begin 
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    !
  do_i1:   &
    do i1_out = 1, n1_out
      !
      x1_out = (i1_out - 1) * d1_out + o1_out
      i1_1 = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
      i1_2 = max(1, min(n1_inp, i1_1+1))
      x1_inp = (i1_1 - 1) * d1_inp + o1_inp
      f1_2 = max(0., min(1., (x1_out-x1_inp)*d1_inv))
      f1_1 = 1. - f1_2
      !
      x0_out (i1_out) =   f1_1 * x0_inp (i1_1) &
                        + f1_2 * x0_inp (i1_2)
      !
    end do do_i1
    !
    return
    !
  end subroutine interpolate_1d_to_1d
  !
  ! 1D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_1d_to_0d (n1_inp, o1_inp, d1_inp,    &
                                   x0_inp,                    &
                                   x1_out,                    &
                                   x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    real,     intent(in   ) :: x0_inp (n1_inp)

    real,     intent(in   ) :: x1_out

    real,     intent(  out) :: x0_out
    !
    ! Local variables
    !

    real    :: x1_inp, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2
    !
    ! Begin interpolate_1d_to_0d
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    !
    i1_1 = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
    i1_2 = max(1, min(n1_inp, i1_1+1))
    x1_inp = (i1_1 - 1) * d1_inp + o1_inp
    f1_2 = max(0., min(1., (x1_out-x1_inp)*d1_inv))
    f1_1 = 1. - f1_2
    !
    x0_out =   f1_1 * x0_inp (i1_1)    &
             + f1_2 * x0_inp (i1_2)
    !
    return
    !
  end subroutine interpolate_1d_to_0d
  !
  subroutine interpolate_1d_to_0d_c (n1_inp, o1_inp, d1_inp,    &
                                     x0_inp,                    &
                                     x1_out,                    &
                                     x0_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp

    complex,  intent(in   ) :: x0_inp (n1_inp)

    real,     intent(in   ) :: x1_out

    complex,  intent(  out) :: x0_out
    !
    ! Local variables
    !

    real    :: x1_inp, d1_inv
    integer :: i1_1, i1_2
    real    :: f1_1, f1_2
    !
    ! Begin interpolate_1d_to_0d
    !
    d1_inv = interpolate_invert_1 (d1_inp)
    !
    i1_1 = max(1, min(n1_inp, int((x1_out-o1_inp)*d1_inv)+1))
    i1_2 = max(1, min(n1_inp, i1_1+1))
    x1_inp = (i1_1 - 1) * d1_inp + o1_inp
    f1_2 = max(0., min(1., (x1_out-x1_inp)*d1_inv))
    f1_1 = 1. - f1_2
    !
    x0_out =   f1_1 * x0_inp (i1_1)    &
             + f1_2 * x0_inp (i1_2)
    !
    return
    !
  end subroutine interpolate_1d_to_0d_c
  !
  ! bracewell interpolation
  !    i_flag = -1 compute coeff and interpolate
  !    i_flag =  0 compute coeff only
  !    i_flag = +1 interpolate only
  !
  subroutine interpolate_brace (i_flag, nx_inp, nx_pad, rx_inp, rx_out, rx_cof)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: i_flag

    integer,  intent(in   ) :: nx_inp, nx_pad
    real,     intent(in   ) :: rx_inp (nx_inp+2)
    real,     intent(  out) :: rx_out ((nx_inp-1)*nx_pad+1)

    real,     intent(  out) :: rx_cof (nx_pad, 5)
    !
    ! Local variables
    !
    integer :: i, j, k, l
    real    :: nx_pad_i, s1, s2, rx_cof_0
    !
    ! Begin interpolate_brace
    !
  select_init:    &
    if (i_flag .le. 0) then
      !
      nx_pad_i = 1. / nx_pad
      !
    do_clear_x_cof:    &
      do i = 1, nx_pad
        !
        rx_cof (i, 1) = 0.0
        !
      end do do_clear_x_cof
      !
    do_init_x_cof:    &
      do j = 2, 5
        !
        if (j .eq. 2 .or. j .eq. 5) then
          !
        do_init_x_cof_25:    &
          do i = 1, nx_pad
            !
            rx_cof_0 = abs(j - 3.0 - (i-1) * nx_pad_i)
            rx_cof (i, j) =   2.0                 &
                           - 4.0 * rx_cof_0       &
                           + 2.5 * rx_cof_0**2    &
                           - 0.5 * rx_cof_0**3
            !
          end do do_init_x_cof_25
          !
        else if (j .eq. 3.or. j .eq. 4) then
          !
        do_init_x_cof_34:    &
          do i = 1, nx_pad
            !
            rx_cof_0 = abs(j-3.-(i-1)*nx_pad_i)
            rx_cof (i, j) = 1. - 2.5 * rx_cof_0**2 + 1.5 * rx_cof_0**3
            !
          end do do_init_x_cof_34
          !
        end if    ! if (j .eq. 2 .or. j .eq. 5) then
        !
      end do do_init_x_cof
      !
    end if select_init
    !
  select_interpolate_flag:    &
    if (i_flag .ne. 0) then
      !
      ! s1 and s2 are linear extrpapolations of 1st 2 data points.
      !
      s1 = 2. * rx_inp (1) -      rx_inp (2)
      s2 = 3. * rx_inp (1) - 2. * rx_inp (2)
      !
    do_k:    &
      do k = 1, nx_pad
        !
        l = k
        !
        rx_out (l) = rx_cof (k, 1)*s2 &
                  + rx_cof (k, 2)*s1 &
                  + rx_cof (k, 3) * rx_inp (1) &
                  + rx_cof (k, 4) * rx_inp (2) &
                  + rx_cof (k, 5) * rx_inp (2)
        !
        l = l + nx_pad
        !
        rx_out (l) = rx_cof (k, 1)*s1 &
                  + rx_cof (k, 2) * rx_inp (1) &
                  + rx_cof (k, 3) * rx_inp (2) &
                  + rx_cof (k, 4) * rx_inp (3) &
                  + rx_cof (k, 5) * rx_inp (4)
        !
        l = l + nx_pad
        !
      do_i:    &
        do i = 3, nx_inp-2
          !
          rx_out (l) = rx_cof (k, 1) * rx_inp (i-2)    &
                    + rx_cof (k, 2) * rx_inp (i-1)    &
                    + rx_cof (k, 3) * rx_inp (i  )    &
                    + rx_cof (k, 4) * rx_inp (i+1)    &
                    + rx_cof (k, 5) * rx_inp (i+2)
          !
          l = l + nx_pad
          !
        end do do_i
        !
        i = nx_inp-1
        !
        rx_out (l) = rx_cof (k, 1) * rx_inp (i-2)    &
                   + rx_cof (k, 2) * rx_inp (i-1)    &
                   + rx_cof (k, 3) * rx_inp (i  )    &
                   + rx_cof (k, 4) * rx_inp (i+1)    &
                   + rx_cof (k, 5) * rx_inp (i+1)
        !
        l = l + nx_pad
        !
        i = nx_inp
        !
        rx_out (l) =  rx_cof (k, 1) * rx_inp (i-2)    &
                    + rx_cof (k, 2) * rx_inp (i-1)    &
                    + rx_cof (k, 3) * rx_inp (i  )    &
                    + rx_cof (k, 4) * rx_inp (i+0)    &
                    + rx_cof (k, 5) * rx_inp (i+0)
        !
        l = l + nx_pad
        !
      end do do_k
      !
    end if select_interpolate_flag
    !
    return
    !
  end subroutine interpolate_brace
  !
  ! bracewell interpolation
  !
  subroutine interpolate_bracewell_0 (nx_inp, nx_pad, rx_inp, rx_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    integer,  intent(in   ) :: nx_pad
    real,     intent(in   ) :: rx_inp (nx_inp + 2)
    real,     intent(  out) :: rx_out ((nx_inp-1)*nx_pad+1)
    !
    ! Local variables
    !
    integer :: i, j, k, l
    real    :: nx_pad_i, s1, s2, rx_cof_0
    real    :: rx_cof (5)
    !
    ! Begin interpolate_bracewell_0
    !
    nx_pad_i = 1. / nx_pad
    !
    ! s1 and s2 are linear extrpapolations of 1st 2 data points.
    !
    s1 = 2.0 * rx_inp (1) -       rx_inp (2)
    s2 = 3.0 * rx_inp (1) - 2.0 * rx_inp (2)
    !
  do_k:    &
    do k = 1, nx_pad
      !
      ! compute the coeficients on the fly
      !
      rx_cof (1) = 0.0
      !
    do_j:    &
      do j = 2, 5
        !
        if (j .eq. 2 .or. j .eq. 5) then
          !
          rx_cof_0 = abs(j-3.-(i-1)*nx_pad_i)
          rx_cof (j) = 2. - 4 * rx_cof_0 + 2.5 * rx_cof_0**2 - 0.5 * rx_cof_0**3
          !
        else if (j .eq. 3.or. j .eq. 4) then
          !
          rx_cof_0 = abs(j-3.-(i-1)*nx_pad_i)
          rx_cof (j) = 1. -             2.5 * rx_cof_0**2 + 1.5 * rx_cof_0**3
          !
        end if    ! if (j .eq. 2 .or. j .eq. 5) then
        !
      end do do_j
      !
      l = k
      !
      rx_out (l) =  rx_cof (1) * s2 &
                  + rx_cof (2) * s1 &
                  + rx_cof (3) * rx_inp (1) &
                  + rx_cof (4) * rx_inp (2) &
                  + rx_cof (5) * rx_inp (2)
      !
      l = l + nx_pad
      !
      rx_out (l) =  rx_cof (1)*s1 &
                  + rx_cof (2) * rx_inp (1)    &
                  + rx_cof (3) * rx_inp (2)    &
                  + rx_cof (4) * rx_inp (3)    &
                  + rx_cof (5) * rx_inp (4)
      !
      l = l + nx_pad
      !
    do_i:    &
      do i = 3, nx_inp-2
        !
        rx_out (l) =  rx_cof (1) * rx_inp (i-2)    &
                    + rx_cof (2) * rx_inp (i-1)    &
                    + rx_cof (3) * rx_inp (i  )    &
                    + rx_cof (4) * rx_inp (i+1)    &
                    + rx_cof (5) * rx_inp (i+2)
        !
        l = l + nx_pad
        !
      end do do_i
      !
      i = nx_inp-1
      !
      rx_out (l) =  rx_cof (1) * rx_inp (i-2)    &
                  + rx_cof (2) * rx_inp (i-1)    &
                  + rx_cof (3) * rx_inp (i  )    &
                  + rx_cof (4) * rx_inp (i+1)    &
                  + rx_cof (5) * rx_inp (i+1)
      !
      l = l + nx_pad
      !
      i = nx_inp
      !
      rx_out (l) =  rx_cof (1) * rx_inp (i-2)    &
                  + rx_cof (2) * rx_inp (i-1)    &
                  + rx_cof (3) * rx_inp (i  )    &
                  + rx_cof (4) * rx_inp (i+0)    &
                  + rx_cof (5) * rx_inp (i+0)
      !
      l = l + nx_pad
      !
    end do do_k
    !
    return
    !
  end subroutine interpolate_bracewell_0
  !
  ! Interpolate from an regularly sampled function rx_inp, ry_inp
  !   to an irregularly sampled function rx_out, ry_out
  !   rx_out can be arbitrarily ordered
  !
  subroutine interpolate_r_to_i (nx_inp, x0_inp, dx_inp, ry_inp,    &
                                 nx_out, rx_out, ry_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: x0_inp
    real,     intent(in   ) :: dx_inp
    real,     intent(in   ) :: ry_inp (nx_inp)
    integer,  intent(in   ) :: nx_out
    real,     intent(in   ) :: rx_out (nx_out)
    real,     intent(  out) :: ry_out (nx_out)
    !
    ! Local variables
    !
    integer :: ix_out
    !
    ! Begin interpolate_r_to_i
    !
    ! compute the map from input to output
    !   for each value of rx_out find the two values of rx_inp that stradlle it
    !
    do_ix_out : do ix_out = 1, nx_out
      !
      call interpolate_1d_to_0d (nx_inp, x0_inp, dx_inp, ry_inp, &
                                 rx_out(ix_out), ry_out(ix_out) )
      !
    end do do_ix_out
    !
    return
    !
  end subroutine interpolate_r_to_i
  !
  ! Interpolate from an irregularly sampled function rx_inp, ry_inp
  !   to a regularly sampled function rx_out, ry_out
  !   rx_inp can be arbitrarily ordered
  !
  subroutine interpolate_i_to_i (nx_inp, rx_inp, ry_inp,    &
                                 nx_out, rx_out, ry_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: rx_inp (nx_inp)
    real,     intent(in   ) :: ry_inp (nx_inp)

    integer,  intent(in   ) :: nx_out
    real,     intent(in   ) :: rx_out (nx_out)
    real,     intent(  out) :: ry_out (nx_out)
    !
    ! Local variables
    !
    integer :: ix_out
    integer :: ix_inp_1, ix_inp_2
    real    :: fx_inp_1, fx_inp_2
    !
    ! Begin interpolate_i_to_i
    !
    ! compute the map from input to output
    !   for each value of rx_out find the two values of rx_inp that stradlle it
    !
    do_ix_out : do ix_out = 1, nx_out
      !
      ! get the indices and weight coefficients of rx_out within rx_inp
      !
      call interpolate_find_index (nx_inp   = nx_inp,        &
                                   rx_inp    = rx_inp,         &
                                   rx_out    = rx_out(ix_out), &
                                   ix_inp_1 = ix_inp_1,      &
                                   ix_inp_2 = ix_inp_2,      &
                                   fx_inp_1 = fx_inp_1,      &
                                   fx_inp_2 = fx_inp_2)
      !
      ry_out (ix_out) =   fx_inp_1 * ry_inp (ix_inp_1) &
                       + fx_inp_2 * ry_inp (ix_inp_2)
      !
    end do do_ix_out
    !
    return
    !
  end subroutine interpolate_i_to_i
  !
  subroutine interpolate_i_to_i_d (nx_inp, rx_inp, ry_inp,    &
                                   nx_out, rx_out, ry_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    double precision,  intent(in   ) :: rx_inp (nx_inp)
    double precision,  intent(in   ) :: ry_inp (nx_inp)

    integer,  intent(in   ) :: nx_out
    double precision,  intent(in   ) :: rx_out (nx_out)
    double precision,  intent(  out) :: ry_out (nx_out)
    !
    ! Local variables
    !
    integer :: ix_out
    integer :: ix_inp_1, ix_inp_2
    double precision :: fx_inp_1, fx_inp_2
    !
    ! Begin interpolate_i_to_i_d
    !
    ! compute the map from input to output
    !   for each value of rx_out find the two values of rx_inp that stradlle it
    !
    do_ix_out : do ix_out = 1, nx_out
      !
      ! get the indices and weight coefficients of rx_out within rx_inp
      !
      call interpolate_find_index_d (nx_inp   = nx_inp,        &
                                     rx_inp    = rx_inp,         &
                                     rx_out    = rx_out(ix_out), &
                                     ix_inp_1 = ix_inp_1,      &
                                     ix_inp_2 = ix_inp_2,      &
                                     fx_inp_1 = fx_inp_1,      &
                                     fx_inp_2 = fx_inp_2)
      !
      ry_out (ix_out) =   fx_inp_1 * ry_inp (ix_inp_1) &
                       + fx_inp_2 * ry_inp (ix_inp_2)
      !
    end do do_ix_out
    !
    return
    !
  end subroutine interpolate_i_to_i_d
  !
  subroutine interpolate_i_to_r_0 ( nx_inp, rx_inp, ry_inp,    &
                                    nx_out, x0_out, dx_out, ry_out )
    !
    ! Interpolate from an irregularly sampled function rx_inp, ry_inp
    !   to a regularly sampled function rx_out, ry_out
    !   rx_inp can be arbitrarily ordered
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: rx_inp (:)
    real,     intent(in   ) :: ry_inp (:)
    !
    integer,  intent(in   ) :: nx_out
    real,     intent(in   ) :: x0_out
    real,     intent(in   ) :: dx_out
    real,     intent(  out) :: ry_out (:)
    !
    ! Local variables
    !
    integer                 :: jx_out
    real                    :: rx_out
    integer                 :: ix_out_1, ix_out_2, ix_out_0
    integer                 :: jx_out_1, jx_out_2
    integer                 :: jx_inp
    integer                 :: ix_inp_1, ix_inp_2, ix_inp_0
    integer                 :: jx_inp_1, jx_inp_2
    real                    :: fx_inp_1, fx_inp_2
    real                    :: rx_inp_1, rx_inp_2
    real                    :: dx_inp
    integer                 :: n0_dif
    real                    :: ry_dif
    real                    :: ry_err
    real                    :: ry_tmp(nx_out)
    real                    :: rx_out_1
    real                    :: rx_out_2
    !
    integer, save           :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! compute the map from input to output
    !   for each value of rx_out find the two values of rx_inp that stradlle it
    !
    ! if there is a single input point set the output and return
    !
    xxif_nx_inp : if ( nx_inp .eq. 1 ) then
      !
      ry_out ( 1: nx_out ) = ry_inp ( 1 )
      !
      return
      !
    end if xxif_nx_inp 
    !
    ! set the input direction
    !
    xxif_ix_inp_dir : if ( rx_inp ( 1 ) .le. rx_inp ( nx_inp ) ) then
      !
      ix_inp_0 = +1
      !
      ix_inp_1 = 1
      !
      ix_inp_2 = nx_inp
      !
    else xxif_ix_inp_dir 
      !
      ix_inp_0 = -1
      !
      ix_inp_1 = nx_inp
      !
      ix_inp_2 = 1
      !
    end if xxif_ix_inp_dir 
    !
    ! set the output direction
    !
    xxif_ix_out_dir : if ( dx_out .ge. 0 ) then
      !
      ix_out_0 = +1
      !
      ix_out_1 = 1
      !
      ix_out_2 = nx_out
      !
    else xxif_ix_out_dir 
      !
      ix_out_0 = -1
      !
      ix_out_1 = nx_out
      !
      ix_out_2 = 1
      !
      print'(" error in interpolate_i_to_r_0 ix_out_0=",i8)', ix_out_0
      !
      stop
      !
    end if xxif_ix_out_dir 
    !
    !print'( &
    !& /, " qqq c=",i8, &
    !& /, " qqq nx_inp=",i6, &
    !& /, " qqq ix_inp=",i6,1x,i6,1x,i2, &
    !& /, " qqq rx_inp=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& /, " qqq nx_out=",i8,1x,g12.6,1x,g12.6,1x,g12.6 &
    !& )', &
    !i_call, &
    !nx_inp, &
    !ix_inp_1, ix_inp_2, ix_inp_0, &
    !rx_inp(ix_inp_1), rx_inp(ix_inp_2), rx_inp(1), rx_inp(nx_inp), &
    !nx_out, x0_out, (nx_out-1)*dx_out+x0_out, dx_out
    !
    jx_out_1 = ix_out_1
    !
    do_jx_inp : &
    do jx_inp = ix_inp_1, ix_inp_2-ix_inp_0, ix_inp_0
      !
      jx_inp_1 = jx_inp
      !
      jx_inp_2 = jx_inp_1 + ix_inp_0
      !
      rx_inp_1 = rx_inp(jx_inp_1)
      !
      rx_inp_2 = rx_inp(jx_inp_2)
      !
      dx_inp = rx_inp(jx_inp_2) - rx_inp ( jx_inp_1 )
      !
      if ( dx_inp .eq. 0 ) dx_inp = 1
      !
      ! set the last output indices for this input interval
      !
      jx_out_2 = min ( nx_out, int ( ( rx_inp_2 - x0_out ) / dx_out ) + 1 )
      !
      rx_out_1 = ( jx_out_1 - 1 ) * dx_out + x0_out
      !
      rx_out_2 = ( jx_out_2 - 1 ) * dx_out + x0_out
      !
      if ( rx_out_2 .gt. rx_inp_2 ) jx_out_2 = jx_out_2 - 1 
      !
      ! if this is the last input interval set the last output index to the max
      !
      if ( jx_inp .eq. ix_inp_2-ix_inp_0 ) jx_out_2 = nx_out
      !
      rx_out_2 = ( jx_out_2 - 1 ) * dx_out + x0_out
      !
      !print'( &
      !& " qqq ji=",i8,1x,i8,1x,i8," jo=",i8,1x,i8, &
      !& " ri=",g12.6,1x,g12.6," ro=",g12.6,1x,g12.6 &
      !& )', &
      !jx_inp, &
      !jx_inp_1, jx_inp_2, &
      !jx_out_1, jx_out_2, &
      !rx_inp_1, rx_inp_2, &
      !rx_out_1, rx_out_2 
      !
      do_jx_out : do jx_out = jx_out_1 , jx_out_2
        !
        rx_out = ( jx_out - 1 ) * dx_out + x0_out
        !
        fx_inp_2 = max ( 0., min ( 1., ( rx_out - rx_inp_1 ) / dx_inp ) ) 
        !
        fx_inp_1 = 1. - fx_inp_2
        !
        ry_out ( jx_out ) = fx_inp_1 * ry_inp(jx_inp_1) &
                          + fx_inp_2 * ry_inp(jx_inp_2) 
        !
        ! set the first output indices for the next input interval
        !
        jx_out_1 = jx_out + 1
        !
        if ( jx_out_1 .gt. nx_out ) go to 1
        !
      end do do_jx_out 
      !
    end do do_jx_inp 
    !
1 continue
    !
    ! test against i_to_r 
    !
    xxif_test : if ( nx_out .eq. -999 ) then
    !xxif_test : if ( nx_out .ne. -999 ) then
    !
    call interpolate_i_to_r ( nx_inp, rx_inp, ry_inp,  &
                              nx_out, x0_out, dx_out, ry_tmp )
    !
    n0_dif = 0
    !
    ry_err = maxval(abs(ry_inp(:))) * .001
    !
    do_jx_out_2 : &
    do jx_out = ix_out_1, ix_out_2, ix_out_0
      !
      rx_out = ( jx_out - 1 ) * dx_out + x0_out
      !
      ry_dif = ry_out(jx_out) - ry_tmp(jx_out) 
      !
      xxif_diff : if ( abs ( ry_dif ) .gt. ry_err ) then
        !
        n0_dif = n0_dif + 1
        !
        print'(1x,i8,1x,i8,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6," n0_dif2")', &
        n0_dif, jx_out, rx_out, ry_out(jx_out), ry_tmp(jx_out), ry_dif 
        !
      end if xxif_diff 
      !
    end do do_jx_out_2 
    !
    if ( n0_dif .ne. 0 ) stop
    !
    end if xxif_test 
    !
    return
    !
  end subroutine interpolate_i_to_r_0 
  !
  subroutine interpolate_i_to_r ( nx_inp, rx_inp, ry_inp,    &
                                  nx_out, x0_out, dx_out, ry_out )
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: rx_inp (:)
    real,     intent(in   ) :: ry_inp (:)
    !
    integer,  intent(in   ) :: nx_out
    real,     intent(in   ) :: x0_out
    real,     intent(in   ) :: dx_out
    real,     intent(  out) :: ry_out (:)
    !
    ! Local variables
    !
    integer                 :: ix_out
    real                    :: rx_out
    integer                 :: ix_inp_1, ix_inp_2
    real                    :: fx_inp_1, fx_inp_2
    !
    ! Begin interpolate_i_to_r
    !
    ! compute the map from input to output
    !   for each value of rx_out find the two values of rx_inp that stradlle it
    !
    do_ix_out : &
    do ix_out = 1, nx_out
      !
      rx_out = (ix_out - 1) * dx_out + x0_out
      !
      ! get the indices and weight coefficients of rx_out within rx_inp
      !
      call interpolate_find_index (nx_inp   = nx_inp,      &
                                   rx_inp    = rx_inp,       &
                                   rx_out    = rx_out,       &
                                   ix_inp_1 = ix_inp_1,    &
                                   ix_inp_2 = ix_inp_2,    &
                                   fx_inp_1 = fx_inp_1,    &
                                   fx_inp_2 = fx_inp_2)
      !
      ry_out (ix_out) =   fx_inp_1 * ry_inp (ix_inp_1) &
                       + fx_inp_2 * ry_inp (ix_inp_2)
      !
    end do do_ix_out
    !
    return
    !
  end subroutine interpolate_i_to_r
  !
  ! Interpolate from an irregularly sampled function rx_inp, ry_inp
  !   to a regularly sampled function rx_out, ry_out
  !   rx_inp can be arbitrarily ordered using precomputed coefficients
  !
  subroutine interpolate_i_to_r_p ( &
                                ix_inp_1, ix_inp_2, fx_inp_1, fx_inp_2, ry_inp, &
                                    nx_out, ry_out &
                                  )
    !
    ! Arguments
    !
    integer,  intent(in   ) :: ix_inp_1 (:)
    integer,  intent(in   ) :: ix_inp_2 (:)
    real,     intent(in   ) :: fx_inp_1 (:)
    real,     intent(in   ) :: fx_inp_2 (:)
    real,     intent(in   ) :: ry_inp   (:)
    !
    integer,  intent(in   ) :: nx_out
    real,     intent(  out) :: ry_out   (:)
    !
    ! Local variables
    !
    integer :: ix_out
    !
    ! Begin interpolate_i_to_r_p
    !
    ! compute the map from input to output
    !   for each value of rx_out find the two values of rx_inp that stradlle it
    !
  do_ix_out:    &
    do ix_out = 1, nx_out
      !
      ry_out ( ix_out ) =   fx_inp_1 (ix_out) * ry_inp ( ix_inp_1 ( ix_out ) ) &
                         + fx_inp_2 (ix_out) * ry_inp ( ix_inp_2 ( ix_out ) ) 
      !
    end do do_ix_out
    !
    return
    !
  end subroutine interpolate_i_to_r_p
  !
  ! Interpolate from an irregularly sampled function rx_inp, ry_inp
  !   to a single point rx_out, ry_out
  !   rx_inp can be arbitrarily ordered
  !
  subroutine interpolate_i_to_r_0d (nx_inp, rx_inp, ry_inp, rx_out, ry_out)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: rx_inp (:)
    real,     intent(in   ) :: ry_inp (:)
    !
    real,     intent(in   ) :: rx_out
    real,     intent(  out) :: ry_out
    !
    ! Local variables
    !

    integer :: ix_inp_1, ix_inp_2
    real    :: fx_inp_1, fx_inp_2
    !
    ! Begin interpolate_i_to_r_0d
    !
    ! compute the map from input to output
    !   for each value of rx_out find the two values of rx_inp that stradlle it
    !
    !
    ! get the indices and weight coefficients of rx_out within rx_inp
    !
    call interpolate_find_index (nx_inp   = nx_inp,      &
                                 rx_inp    = rx_inp,       &
                                 rx_out    = rx_out,       &
                                 ix_inp_1 = ix_inp_1,    &
                                 ix_inp_2 = ix_inp_2,    &
                                 fx_inp_1 = fx_inp_1,    &
                                 fx_inp_2 = fx_inp_2)
    !
    ry_out =   fx_inp_1 * ry_inp (ix_inp_1) &
            + fx_inp_2 * ry_inp (ix_inp_2)
    !
    return
    !
  end subroutine interpolate_i_to_r_0d
  !
  ! interpolate from a pointed 2d grid to a uniform 2d grid
  !
  subroutine interpolate_i_to_r_2d (nyi, ixi, nxi, mxi,        &
                                    xi, yi, zi,                &
                                    nx_grd, x0_grd, dx_grd,    &
                                    ny_grd, y0_grd, dy_grd,    &
                                    z_hor,                     &
                                    yofy, zofx, zofy, z_xy)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nyi
    integer,  intent(in   ) :: ixi (:)
    integer,  intent(in   ) :: nxi (:)

    integer,  intent(in   ) :: mxi
    real,     intent(in   ) :: xi (:)
    real,     intent(in   ) :: yi (:)
    real,     intent(in   ) :: zi (:)

    integer,  intent(in   ) :: nx_grd
    real,     intent(in   ) :: x0_grd
    real,     intent(in   ) :: dx_grd

    integer,  intent(in   ) :: ny_grd
    real,     intent(in   ) :: y0_grd
    real,     intent(in   ) :: dy_grd

    real,     intent(  out) :: z_hor (nx_grd, ny_grd)

    real,     intent(  out) :: yofy (nyi)
    real,     intent(  out) :: zofx (nyi)
    real,     intent(inout) :: zofy (ny_grd)
    real,     intent(inout) :: z_xy (nx_grd, nyi)
    !
    ! Local variables
    !
    integer :: iy_hor, ix_1, ix_grd, iyi
    !
    ! Begin interpolate_i_to_r_2d
    !
    ! interpolate to the x grid at each y location
    !
    do iy_hor = 1, nyi
      !
      ix_1 = ixi (iy_hor) + 1    ! first point in this x line
      !
      yofy(iy_hor) = yi(ix_1)   ! first y value in this x line
      !
      call interpolate_i_to_r (nx_inp = nxi (iy_hor),    &
                               rx_inp  = xi (ix_1:),       &
                               ry_inp  = zi (ix_1:),       &
                               nx_out = nx_grd,          &
                               x0_out = x0_grd,          &
                               dx_out = dx_grd,          &
                               ry_out  = z_xy (:, iy_hor))
      !
    end do    ! do iy_hor = 1, nyi
    !
    ! for each x_grd value interpolate from between ys
    !
    do ix_grd = 1, nx_grd
      !
      ! copy from z_xy to zofx
      !
      do iyi = 1, nyi
        zofx(iyi) = z_xy(ix_grd, iyi)
      end do    ! do iyi = 1, nyi
      !
      ! interpolate from zofx to zofy
      !
      !
      call interpolate_i_to_r (nx_inp = nyi,       &
                               rx_inp  = yofy,      &
                               ry_inp  = zofx,      &
                               nx_out = ny_grd,    &
                               x0_out = y0_grd,    &
                               dx_out = dy_grd,    &
                               ry_out  = zofy)
      !
      ! copy from zofy to z_hor
      !
      do iyi = 1, ny_grd
        z_hor(ix_grd, iyi) = zofy(iyi)
      end do    ! do iyi = 1, ny_grd
      !
    end do    ! do ix_grd = 1, nx_grd
    !
    return
    !
  end subroutine interpolate_i_to_r_2d
  !
  subroutine interpolate_i_to_r_nearest ( &
                                          i_trot, &
                                          nx_inp, gx_inp, rx_inp, &
                                          nx_out, x0_out, dx_out, rx_out &
                                        )
    !
    ! interpolate an irregularly sampled array to a regulary sampled array
    ! by placing each input in the nearest output location
    !
    integer,  intent(in   ) :: i_trot    !  call index for print
    integer,  intent(in   ) :: nx_inp    !  input grid node num
    real,     intent(in   ) :: gx_inp(:) !  input grid node val
    real,     intent(in   ) :: rx_inp(:) !  input grid trace value
    !
    integer,  intent(in   ) :: nx_out    ! output grid node num
    real,     intent(in   ) :: x0_out    ! output grid node min
    real,     intent(in   ) :: dx_out    ! output grid node inc
    real,     intent(inout) :: rx_out(:) ! output grid trace value
    !
    integer                 :: ix_inp    !  input grid node index
    integer                 :: ix_out    ! output grid node index
    !
    do_ix_inp : do  ix_inp = 1 , nx_inp
      !
      ix_out = nint ( ( gx_inp(ix_inp) - x0_out) / dx_out ) + 1
      !
      !print'(" interpolate_i_to_r_nearest ", 1x,i8,1x,i8,1x,i8,1x,g12.6)', &
      !i_trot, ix_inp, ix_out, rx_inp ( ix_inp )
      !
      if ( ix_out .ge. 1 .and. ix_out .le. nx_out ) &
      rx_out ( ix_out ) = rx_inp ( ix_inp )
      !
    end do do_ix_inp 
    !
    return
    !
  end subroutine interpolate_i_to_r_nearest 
  !
  subroutine interpolate_i_to_r_fft ( &
                                      i_trot, &
                                      nx_inp, gx_inp, rx_inp, &
                                      nx_out, x0_out, dx_out, rx_out &
                                    )
    !
    ! interpolate an irregularly sampled array to a regulary sampled array
    ! by taking a slow fft from the irreguarly sampled array
    ! and a fast fft back to the fast array
    !
    integer,  intent(in   ) :: i_trot    !  call index for print
    integer,  intent(in   ) :: nx_inp    !  input grid node num
    real,     intent(in   ) :: gx_inp(:) !  input grid node val
    real,     intent(in   ) :: rx_inp(:) !  input grid trace value
    !
    integer,  intent(in   ) :: nx_out    ! output grid node num
    real,     intent(in   ) :: x0_out    ! output grid node min
    real,     intent(in   ) :: dx_out    ! output grid node inc
    real,     intent(inout) :: rx_out(:) ! output grid trace value
    !
    integer                 :: i_err     ! error flag 0=o.k. <0 = err
    !
    integer                 :: ix_inp    !  input grid node index
    integer                 :: ix_out    ! output grid node index
    !
    integer                 :: ix_fft    ! output grid fft  index
    integer                 :: nx_fft    ! output grid fft   num
    real                    :: x0_fft    ! output grid fft   min
    real                    :: dx_fft    ! output grid fft   inc
    real                    :: nx_nyq    ! output grid fft   nyquist index
    real                    :: rx_nyq    ! output grid fft   nyquist value
    real                    :: vx_fft    ! 1. / nx_fft
    !
    integer                 :: gx_fft    ! r to c fft sign
    real                    :: sx_fft    ! r to c fft scale
    real                    :: rx_tmp(nx_out) ! output grid trace value
    real,           pointer :: rx_fft(:) ! real    fft array
    complex,        pointer :: cx_fft(:) ! complex fft array
    type (fft_struct), pointer :: ox_fft ! fft object
    !

    real                    :: prx_out    ! output phase
    real                    :: px_fft    !    fft phase
    real                    :: px_tot    !  total phase
    !
    nullify (rx_fft) ! jpa
    nullify (cx_fft) ! jpa
    nullify (ox_fft) ! jpa
    !
    i_err = 0
    !
    nx_fft = 2 * interpolate_pow2 ( nx_out )
    !
    gx_fft = + 1
    !
    sx_fft = + 1. / nx_fft
    !
    vx_fft = 1. / nx_fft
    !
    call memfun_all ( rx_fft, nx_fft, 'rx_fft', i_err )
    call memfun_all ( cx_fft, nx_fft, 'cx_fft', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    nx_nyq = nx_fft / 2 + 1            ! nyquist index
    !
    rx_nyq = 2. * pi / ( 2. * dx_out ) ! nyquist frequency in rad / sec
    !
    dx_fft = rx_nyq / ( nx_nyq - 1 )   ! frequency increment
    !
    x0_fft = 0.                        ! frequency origin
    !
    ! take slow fft from rx_inp to rx_fft
    !
    do_ix_fft_1 : do ix_fft = 1 , nx_fft
      !
      cx_fft (ix_fft ) = cmplx ( 0., 0. )
      !
      px_fft = ( ix_fft - 1 ) * dx_fft + x0_fft
      !
      if ( ix_fft .gt. nx_fft / 2 + 1 ) &
      px_fft = - ( ( nx_fft - ix_fft + 1 ) * dx_fft + x0_fft )
      !
      do_ix_inp : do ix_inp = 1 , nx_inp
        !
        px_tot = + px_fft * gx_inp ( ix_inp )
        !
        cx_fft ( ix_fft ) = &
        cx_fft ( ix_fft ) + &
        rx_inp ( ix_inp ) * cmplx ( cos ( px_tot ), sin ( px_tot ) )
        !
      end do do_ix_inp 
      !
    end do do_ix_fft_1 
    !
    xxif_it_trot : if ( i_trot .eq. 1 ) then
    !
    !print'(" slow interpolate_fft i_trot=", i8 )', i_trot
    !
    ! take slow fft from rx_fft to rx_out
    !
    do_ix_out : do ix_out = 1 , nx_out
      !
      rx_tmp ( ix_out ) = 0.
      !
      prx_out = ( ix_out - 1 ) * dx_out + x0_out
      !
      do_ix_fft_2 : do ix_fft = 1 , nx_fft
        !
        px_fft = ( ix_fft - 1 ) * dx_fft + x0_fft
        !
        if ( ix_fft .gt. nx_fft / 2 + 1 ) &
        px_fft = - ( ( nx_fft - ix_fft + 1 ) * dx_fft + x0_fft )
        !
        px_tot = - px_fft * prx_out
        !
        rx_tmp ( ix_out ) = &
        rx_tmp ( ix_out ) + &
        real ( cx_fft ( ix_fft ) * cmplx ( cos ( px_tot ), sin ( px_tot ) ) )
        !
      end do do_ix_fft_2
      !
    end do do_ix_out
    !
    rx_tmp ( 1:nx_out ) = rx_tmp ( 1:nx_out ) * vx_fft
    !
    end if xxif_it_trot 
    !
    ! create the complex to real fft object, ox_fft
    !
    i_err =  fft_create ( ox_fft, -gx_fft, nx_fft, 'ctor', sx_fft )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! take fast fft from cx_fft to rx_fft
    !
    call fft_cr_transform ( ox_fft, cx_fft, rx_fft )
    !
!    if ( i_trot .eq. 1 ) &
!print*,' nx_out=',nx_out,' x0_out=',x0_out,' dx_out=',dx_out,' vx_fft=',vx_fft
!    if ( i_trot .eq. 1 ) &
!print*,' nx_fft=',nx_fft,' x0_fft=',x0_fft,' dx_fft=',dx_fft,' sx_fft=',sx_fft
!    if ( i_trot .eq. 1 ) &
!    print'(" ff0 ",i8,1x,g12.6,1x,g12.6)',&
!    (ix_out,rx_fft(ix_out),rx_tmp(ix_out),ix_out=1,nx_out)
    !
    ! copy from rx_fft to rx_out
    !
    rx_out ( 1:nx_out ) = rx_fft ( 1:nx_out ) 
    !
    ! delete memory
    !
    call memfun_del ( rx_fft )
    call memfun_del ( cx_fft )
    !
    ! delete the fft object
    !
    call fft_delete ( ox_fft )
    !
    ! print debug info
    !
    !if ( i_trot .eq. 1 ) &
    !print'( " i_to_r_fft i=", i6, " c=", g12.6, &
    !& " nf=", i6, " ni=", i6, " no=", i6 &
    !& )', &
    !i_trot, getsys_seconds(), &
    !nx_fft, nx_inp, nx_out
    !
    !if ( i_trot .ge. 1 ) &
    !print'( " i_to_r_fft i=", i6, " c=", g12.6, &
    !& " ai=", g12.6, " ao=", g12.6 &
    !& )', &
    !i_trot, getsys_seconds(), &
    !maxval ( abs ( rx_inp ( 1:nx_inp ) ) ), &
    !maxval ( abs ( rx_out ( 1:nx_out ) ) )
    !
    return
    !
997 continue
    !
    call pc_error (msg1 = 'interpolate_fft: Error status ',    &
                   var1 = i_err,                         &
                   msg2 = ' returned from fft_create (ctor)')
    !
    go to 999
    !
998 continue
    !
    call pc_error (msg1 = 'interpolate_fft: Error status ',    &
                   var1 = i_err,                         &
                   msg2 = ' returned from memfun_all ' )
    !
    go to 999
    !
 999 continue
    !
    stop
    !
  end subroutine interpolate_i_to_r_fft 
  !
  ! get nearest index to rx_out from rx_inp to left and right.
  !
  !   if rx_inp is outside range of rx_out use nearest end member
  !   for both ix_inp_1, ix_inp_2
  !   nx_inp = # of elements in rx_inp to consider
  !   ix_inp_1 = nearest point to left (rx_inp<rx_out)
  !   ix_inp_2 = nearest point to right (rx_inp>rx_out)
  !   fx_inp_1 = weight to left side
  !   fx_inp_2 = weight to right side
  !
  subroutine interpolate_find_index (nx_inp, rx_inp, rx_out,   &
                                     ix_inp_1, ix_inp_2,     &
                                     fx_inp_1, fx_inp_2)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: rx_inp (nx_inp)

    real,     intent(in   ) :: rx_out

    integer,  intent(  out) :: ix_inp_1 
    integer,  intent(  out) :: ix_inp_2 
    real,     intent(  out) :: fx_inp_1 
    real,     intent(  out) :: fx_inp_2 
    !
    ! Local variables
    !
    integer :: ix_inp

    real    :: rx_inp_min, rx_inp_max, eps, dx
    !
    ! Begin interpolate_find_index
    !
    rx_inp_min = rx_out
    rx_inp_max = rx_out
    !
    ix_inp_1 = 0
    ix_inp_2 = 0
    !
  do_ix_inp:    &
    do ix_inp = 1, nx_inp
      !
      rx_inp_min = min(rx_inp_min, rx_inp (ix_inp))
      rx_inp_max = max(rx_inp_max, rx_inp (ix_inp))
      !
      if (rx_inp (ix_inp) .lt. rx_out) then
        !
      select_initial_value_1:    &
        if (ix_inp_1 .eq. 0) then
          !
          ix_inp_1 = ix_inp
          !
        else if (rx_inp (ix_inp) .gt. rx_inp (ix_inp_1)) then &
      select_initial_value_1
          !
          ix_inp_1 = ix_inp
          !
        end if select_initial_value_1
        !
      else if (rx_inp (ix_inp) .gt. rx_out) then   ! if (rx_inp (ix_inp) .le. rx_out
        !
      select_initial_value_2:    &
        if (ix_inp_2 .eq. 0) then
          !
          ix_inp_2 = ix_inp
          !
        else if (rx_inp (ix_inp) .lt. rx_inp (ix_inp_2)) then
          !
          ix_inp_2 = ix_inp
          !
        end if select_initial_value_2
        !
      else    ! if (rx_inp (ix_inp) .lt. rx_out) then
        !
        ix_inp_1 = ix_inp
        ix_inp_2 = ix_inp
        !
      end if    ! if (rx_inp (ix_inp) .lt. rx_out) then
      !
    end do do_ix_inp
    !
    ! see if output point is off end of input points.
    !
    if (ix_inp_1 .eq. 0) ix_inp_1 = ix_inp_2
    if (ix_inp_2 .eq. 0) ix_inp_2 = ix_inp_1
    !
    if (ix_inp_1 .eq. 0) ix_inp_1 = 1
    if (ix_inp_2 .eq. 0) ix_inp_2 = 1
    !
    ! now have two points spaning the output point
    !
    dx = rx_inp (ix_inp_2) - rx_inp (ix_inp_1)
    eps = (1.0e-8) * abs (rx_inp_max - rx_inp_min)
    !
  verify_valid_dx:    &
    if (dx .gt. eps) then
      !
      fx_inp_2 = (rx_out - rx_inp (ix_inp_1)) / dx
      !
    else verify_valid_dx
      !
      ! same point on left and right
      !
      fx_inp_2 = 0.5
      !
    end if verify_valid_dx
    !
    fx_inp_1 = 1. - fx_inp_2
    !
    return
    !
  end subroutine interpolate_find_index
  !
  subroutine interpolate_find_index_d (nx_inp, rx_inp, rx_out,   &
                                       ix_inp_1, ix_inp_2,     &
                                       fx_inp_1, fx_inp_2)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    double precision,     intent(in   ) :: rx_inp (nx_inp)

    double precision,     intent(in   ) :: rx_out

    integer,  intent(  out) :: ix_inp_1 
    integer,  intent(  out) :: ix_inp_2 
    double precision,     intent(  out) :: fx_inp_1 
    double precision,     intent(  out) :: fx_inp_2 
    !
    ! Local variables
    !
    integer :: ix_inp

    double precision    :: rx_inp_min, rx_inp_max, eps, dx
    !
    ! Begin interpolate_find_index
    !
    rx_inp_min = rx_out
    rx_inp_max = rx_out
    !
    ix_inp_1 = 0
    ix_inp_2 = 0
    !
  do_ix_inp:    &
    do ix_inp = 1, nx_inp
      !
      rx_inp_min = min(rx_inp_min, rx_inp (ix_inp))
      rx_inp_max = max(rx_inp_max, rx_inp (ix_inp))
      !
      if (rx_inp (ix_inp) .lt. rx_out) then
        !
      select_initial_value_1:    &
        if (ix_inp_1 .eq. 0) then
          !
          ix_inp_1 = ix_inp
          !
        else if (rx_inp (ix_inp) .gt. rx_inp (ix_inp_1)) then &
      select_initial_value_1
          !
          ix_inp_1 = ix_inp
          !
        end if select_initial_value_1
        !
      else if (rx_inp (ix_inp) .gt. rx_out) then   ! if (rx_inp (ix_inp) .le. rx_out
        !
      select_initial_value_2:    &
        if (ix_inp_2 .eq. 0) then
          !
          ix_inp_2 = ix_inp
          !
        else if (rx_inp (ix_inp) .lt. rx_inp (ix_inp_2)) then
          !
          ix_inp_2 = ix_inp
          !
        end if select_initial_value_2
        !
      else    ! if (rx_inp (ix_inp) .lt. rx_out) then
        !
        ix_inp_1 = ix_inp
        ix_inp_2 = ix_inp
        !
      end if    ! if (rx_inp (ix_inp) .lt. rx_out) then
      !
    end do do_ix_inp
    !
    ! see if output point is off end of input points.
    !
    if (ix_inp_1 .eq. 0) ix_inp_1 = ix_inp_2
    if (ix_inp_2 .eq. 0) ix_inp_2 = ix_inp_1
    !
    if (ix_inp_1 .eq. 0) ix_inp_1 = 1
    if (ix_inp_2 .eq. 0) ix_inp_2 = 1
    !
    ! now have two points spaning the output point
    !
    dx = rx_inp (ix_inp_2) - rx_inp (ix_inp_1)
    eps = (1.0e-8) * abs (rx_inp_max - rx_inp_min)
    !
  verify_valid_dx:    &
    if (dx .gt. eps) then
      !
      fx_inp_2 = (rx_out - rx_inp (ix_inp_1)) / dx
      !
    else verify_valid_dx
      !
      ! same point on left and right
      !
      fx_inp_2 = 0.5
      !
    end if verify_valid_dx
    !
    fx_inp_1 = 1. - fx_inp_2
    !
    return
    !
  end subroutine interpolate_find_index_d
  !
  ! get nearest index to rx_out from rx_inp to left and right.
  !
  !   for an array of output points
  !    if rx_inp is outside range of rx_out use nearest end member
  !    for both ix_inp_1, ix_inp_2
  !    nx_inp = # of elements in rx_inp to consider
  !    ix_inp_1 = nearest point to left (rx_inp<rx_out)
  !    ix_inp_2 = nearest point to right (rx_inp>rx_out)
  !    fx_inp_1 = weight to left side
  !    fx_inp_2 = weight to right side
  !
  subroutine interpolate_find_index_n (nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       ix_inp_1, ix_inp_2, &
                                       fx_inp_1, fx_inp_2)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: rx_inp    (nx_inp)

    integer,  intent(in   ) :: nx_out
    real,     intent(in   ) :: rx_out    (nx_out)

    integer,  intent(  out) :: ix_inp_1 (nx_out)
    integer,  intent(  out) :: ix_inp_2 (nx_out)
    real,     intent(  out) :: fx_inp_1 (nx_out)
    real,     intent(  out) :: fx_inp_2 (nx_out)
    !
    ! Local variables
    !
    integer :: ix_out
    !
    ! Begin interpolate_find_index_n
    !
    do ix_out = 1, nx_out
      !
      call interpolate_find_index (nx_inp   = nx_inp,               &
                                   rx_inp    = rx_inp,                &
                                   rx_out    = rx_out    (ix_out),    &
                                   ix_inp_1 = ix_inp_1 (ix_out),    &
                                   ix_inp_2 = ix_inp_2 (ix_out),    &
                                   fx_inp_1 = fx_inp_1 (ix_out),    &
                                   fx_inp_2 = fx_inp_2 (ix_out))
      !
    end do    ! do ix_out = 1, nx_out
    !
    return
    !
  end subroutine interpolate_find_index_n
  !
  ! get nearest index to rx_out from rx_inp to left and right.
  !   for a uniformly sampled set of output points
  !    if rx_inp is outside range of rx_out use nearest end member
  !    for both ix_inp_1, ix_inp_2
  !    nx_inp = # of elements in rx_inp to consider
  !    ix_inp_1 = nearest point to left (rx_inp<rx_out)
  !    ix_inp_2 = nearest point to right (rx_inp>rx_out)
  !    fx_inp_1 = weight to left side
  !    fx_inp_2 = weight to right side
  !
  subroutine interpolate_find_index_g (nx_inp, rx_inp, &
                                       nx_out, x0_out, dx_out, &
                                       ix_inp_1, ix_inp_2, &
                                       fx_inp_1, fx_inp_2)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: rx_inp (nx_inp)

    integer,  intent(in   ) :: nx_out
    real,     intent(in   ) :: x0_out
    real,     intent(in   ) :: dx_out

    integer,  intent(  out) :: ix_inp_1 (nx_out)
    integer,  intent(  out) :: ix_inp_2 (nx_out)
    real,     intent(  out) :: fx_inp_1 (nx_out)
    real,     intent(  out) :: fx_inp_2 (nx_out)
    !
    ! Local variables
    !
    integer :: ix_out
    real    :: rx_out
    !
    ! Begin 
    !
    do ix_out = 1, nx_out
      !
      rx_out = (ix_out - 1) * dx_out + x0_out
      !
      call interpolate_find_index (nx_inp   = nx_inp,               &
                                   rx_inp    = rx_inp,                &
                                   rx_out    = rx_out,                &
                                   ix_inp_1 = ix_inp_1 (ix_out),    &
                                   ix_inp_2 = ix_inp_2 (ix_out),    &
                                   fx_inp_1 = fx_inp_1 (ix_out),    &
                                   fx_inp_2 = fx_inp_2 (ix_out))
      !
    end do    ! do ix_out = 1, nx_out
    !
    return
    !
  end subroutine interpolate_find_index_g
  !
  ! get nearest index to a uniformly sampled set of input points
  !   for a uniformly sampled set of output points
  !    if rx_inp is outside range of rx_out use nearest end member
  !    for both ix_inp_1, ix_inp_2
  !    nx_inp = # of elements in rx_inp to consider
  !    ix_inp_1 = nearest point to left (rx_inp<rx_out)
  !    ix_inp_2 = nearest point to right (rx_inp>rx_out)
  !    fx_inp_1 = weight to left side
  !    fx_inp_2 = weight to right side
  !
  subroutine interpolate_find_index_h (nx_inp, x0_inp, dx_inp, &
                                       nx_out, x0_out, dx_out, &
                                       ix_inp_1, ix_inp_2, &
                                       fx_inp_1, fx_inp_2)
    !
    ! Arguments
    !

    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: x0_inp
    real,     intent(in   ) :: dx_inp

    integer,  intent(in   ) :: nx_out
    real,     intent(in   ) :: x0_out
    real,     intent(in   ) :: dx_out

    integer,  intent(  out) :: ix_inp_1 (nx_out)
    integer,  intent(  out) :: ix_inp_2 (nx_out)
    real,     intent(  out) :: fx_inp_1 (nx_out)
    real,     intent(  out) :: fx_inp_2 (nx_out)
    !
    ! Local variables
    !
    integer :: ix_out
    real    :: x1_inp
    real    :: x1_out
    real    :: dx_inv
    !
    ! Begin 
    !
    dx_inv = interpolate_invert_1 (dx_inp)
    !
    do_ix_out : do ix_out = 1 , nx_out
      !
      x1_out = ( ix_out - 1 ) * dx_out + x0_out
      !
      ix_inp_1(ix_out) = max(1, min(nx_inp, int((x1_out-x0_inp)*dx_inv)+1))
      ix_inp_2(ix_out) = max(1, min(nx_inp, ix_inp_1(ix_out)+1))
      !
      x1_inp = (ix_inp_1(ix_out) - 1) * dx_inp + x0_inp
      !
      fx_inp_2(ix_out) = max(0., min(1., (x1_out-x1_inp)*dx_inv))
      fx_inp_1(ix_out) = 1. - fx_inp_2(ix_out)
      !
    end do do_ix_out
    !
    return
    !
  end subroutine interpolate_find_index_h
  !
  ! get nearest index to a uniformly sampled set of input points
  !   for a single output point
  !    if rx_inp is outside range of rx_out use nearest end member
  !    for both ix_inp_1, ix_inp_2
  !    nx_inp = # of elements in rx_inp to consider
  !    ix_inp_1 = nearest point to left (rx_inp<rx_out)
  !    ix_inp_2 = nearest point to right (rx_inp>rx_out)
  !    fx_inp_1 = weight to left side
  !    fx_inp_2 = weight to right side
  !
  subroutine interpolate_find_index_h1 (nx_inp, x0_inp, dx_inp, &
                                        x1_out, &
                                        ix_inp_1, ix_inp_2, &
                                        fx_inp_1, fx_inp_2)
    !
    ! Arguments
    !

    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: x0_inp
    real,     intent(in   ) :: dx_inp

    real,     intent(in   ) :: x1_out

    integer,  intent(  out) :: ix_inp_1 
    integer,  intent(  out) :: ix_inp_2 
    real,     intent(  out) :: fx_inp_1 
    real,     intent(  out) :: fx_inp_2 
    !
    ! Local variables
    !

    real    :: x1_inp
    real    :: dx_inv
    !
    ! Begin 
    !
    dx_inv = interpolate_invert_1 (dx_inp)
    !
    !
    ix_inp_1 = max(1, min(nx_inp, int((x1_out-x0_inp)*dx_inv)+1))
    ix_inp_2 = max(1, min(nx_inp, ix_inp_1+1))
    !
    x1_inp = (ix_inp_1 - 1) * dx_inp + x0_inp
    !
    fx_inp_2 = max(0., min(1., (x1_out-x1_inp)*dx_inv))
    fx_inp_1 = 1. - fx_inp_2
    !
    return
    !
  end subroutine interpolate_find_index_h1
  !
  subroutine interpolate_find_index_hn (nx_inp, x0_inp, dx_inp, &
                                        n1_out, x1_out, &
                                        ix_inp_1, ix_inp_2, &
                                        fx_inp_1, fx_inp_2)
    !
    ! Arguments
    !

    integer,  intent(in   ) :: nx_inp
    real,     intent(in   ) :: x0_inp
    real,     intent(in   ) :: dx_inp

    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: x1_out(n1_out)

    integer,  intent(  out) :: ix_inp_1(n1_out)
    integer,  intent(  out) :: ix_inp_2(n1_out)
    real,     intent(  out) :: fx_inp_1(n1_out)
    real,     intent(  out) :: fx_inp_2(n1_out)
    !
    ! Local variables
    !
    integer :: i1_out
    real    :: x1_inp
    real    :: dx_inv
    !
    ! Begin 
    !
    dx_inv = interpolate_invert_1 (dx_inp)
    !
    do_i1_out : do i1_out = 1 , n1_out
      !
      ix_inp_1(i1_out) = max(1, min(nx_inp, &
                         int((x1_out(i1_out)-x0_inp)*dx_inv)+1)) 
      !
      ix_inp_2(i1_out) = max(1, min(nx_inp, ix_inp_1(i1_out)+1))
      !
      x1_inp = (ix_inp_1(i1_out) - 1) * dx_inp + x0_inp
      !
      fx_inp_2(i1_out) = max ( 0., min ( 1., (x1_out(i1_out)-x1_inp)*dx_inv) )
      !
      fx_inp_1(i1_out) = 1. - fx_inp_2(i1_out)
      !
    end do do_i1_out 
    !
    return
    !
  end subroutine interpolate_find_index_hn
  !
  ! invert a real value x, avoid divide by 0.
  !
  function interpolate_invert_1 (x)  result (rx_out)
    !
    ! Arguments
    !
    real              :: rx_out
    real,  intent(in   ) :: x
    !
    ! Begin 
    !
    if (x == 0.0) then
      rx_out = 0.0
    else
      rx_out = 1.0 / x
    end if
    !
  end function interpolate_invert_1
  !
  ! return the next power of 2 >= n
  !
  function interpolate_pow2 (n)  result (p)
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n
    integer              :: p
    !
    ! Begin interpolate_pow2
    !
    p = 2 ** ceiling (log (real (n)) / log (2.0))
    if (n*2 <= p) p = p / 2
    !
  end function interpolate_pow2
  !
  subroutine interpolate_fft_create ( o, c_title, &
                                      interpolate_real, interpolate_cplx, &
                                      ft_inp, nt_inp, t0_inp, dt_inp, &
                                      ft_out, nt_out, t0_out, dt_out, &
                                      lu_out, i_err )
    !
    ! create fft interpolation object
    !
    type (interpolate_fft_struct), pointer :: o 
    !
    character (len=*),  intent(in   ) :: c_title  ! title for print
    logical,            intent(in   ) :: interpolate_real! real    interpolation
    logical,            intent(in   ) :: interpolate_cplx! complex interpolation
    integer,            intent(in   ) :: ft_inp   ! input  fft    length
    integer,            intent(in   ) :: nt_inp   ! input  vector length
    real,               intent(in   ) :: t0_inp   ! input  vector min
    real,               intent(in   ) :: dt_inp   ! input  vector inc
    integer,            intent(in   ) :: ft_out   ! output fft    length
    integer,            intent(in   ) :: nt_out   ! output vector length
    real,               intent(in   ) :: t0_out   ! output vector min
    real,               intent(in   ) :: dt_out   ! output vector inc
    integer,            intent(in   ) :: lu_out   ! print unit lu_out<0 no print
    integer,            intent(  out) :: i_err    ! error flag 0-ok <0-error
    !
    ! Local variables
    !
    ! Begin interpolate_fft_create
    !
    ! allocate the anti alias filter object
    !
    allocate ( o )
    !
    nullify (o%or_inp) ! jpa
    nullify (o%or_out) ! jpa
    nullify (o%oc_inp) ! jpa
    nullify (o%oc_out) ! jpa
    !
    i_err = 0
    !
    ! copy the local parameters to structure parameters
    !
    o%c_title = c_title
    o%interpolation_factor = ft_out / ft_inp
    o%interpolate_real    = interpolate_real
    o%interpolate_cplx    = interpolate_cplx
    !
    o%ft_inp = ft_inp 
    o%nt_inp = nt_inp 
    o%t0_inp = t0_inp 
    o%dt_inp = dt_inp 
    o%t1_inp = ( o%ft_inp - 1 ) * o%dt_inp + o%t0_inp 
    o%t2_inp = ( o%nt_inp - 1 ) * o%dt_inp + o%t0_inp 
    !
    o%ft_out = ft_out
    o%nt_out = nt_out
    o%t0_out = t0_out
    o%dt_out = dt_out
    o%t1_out = ( o%ft_out - 1 ) * o%dt_out + o%t0_out 
    o%t2_out = ( o%nt_out - 1 ) * o%dt_out + o%t0_out 
    !
    o%sr_inp  = 1. /        float ( ft_inp )   ! real to complex    scale f
    !
    o%sr_out  = 1.                             ! complex to real    scale i
    !
    o%sc_inp  = 1. / sqrt ( float ( ft_inp ) ) ! complex to complex scale f
    !
    o%sc_out  = 1. / sqrt ( float ( ft_inp ) ) ! complex to complex scale i
    !
    o%origin_shift = abs ( o%t0_inp - o%t0_out ) &
    .gt. min ( abs ( o%dt_inp ) , abs ( o%dt_out ) ) * .01
    !
    if ( lu_out .ge. 0 ) &
    write ( lu_out, "( &
    & /, ' interpolate_fft_create', a, &
    & /, ' interpolation_factor=', g12.6, ' nt_inp=', i8, &
    & /, ' ft_inp=', i8, ' ft_out=', i8 &
    & ) " )    &
    trim(c_title), &
    o%interpolation_factor, o%nt_inp, &
    o%ft_inp, o%ft_out
    !
    ! set the sign of the real to complex fft = + sg_inp &
    !     the sign of the complex to real fft = - sg_inp = sg_out 
    !
    o%sg_inp = +1
    !
    o%sg_out = -1
    !
    ! create the forward real to complex fft object or_inp
    !
    if ( o%interpolate_real ) &
    i_err =  &
    fft_create ( o%or_inp, o%sg_inp, o%ft_inp, 'rtoc', o%sr_inp )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! create the forward complex to complex fft object oc_inp
    !
    if ( o%interpolate_cplx ) &
    i_err =  &
    fft_create ( o%oc_inp, o%sg_inp, o%ft_inp, 'ctoc', o%sc_inp )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! create the inverse complex to real fft object, or_out
    !
    if ( o%interpolate_real ) &
    i_err = &
    fft_create ( o%or_out, o%sg_out, o%ft_out, 'ctor', o%sr_out )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! create the inverse complex to complex fft object, oc_out
    !
    if ( o%interpolate_cplx ) &
    i_err = &
    fft_create ( o%oc_out, o%sg_out, o%ft_out, 'ctoc', o%sc_out )
    !
    if ( i_err .ne. 0 ) go to 995
    !
    return
    !
    ! error
    !
995 continue
    !
    call pc_error ( msg1 = 'interpolate_fft: Error status ',    &
                    var1 = i_err,                         &
                    msg2 = ' returned from fft_create (rtoc)' )
    !
    goto 999
    !
996 continue
    !
    call pc_error ( msg1 = 'interpolate_fft: Error status ',    &
                    var1 = i_err,                         &
                    msg2 = ' returned from fft_create (ctor)' )
    !
    goto 999
    !
997 continue
    !
    call pc_error ( msg1 = 'interpolate_fft: Error status ',    &
                    var1 = i_err,                         &
                    msg2 = ' returned from fft_create (ctoc f)' )
    !
    goto 999
    !
998 continue
    !
    call pc_error ( msg1 = 'interpolate_fft: Error status ', &
                    var1 = i_err, &
                    msg2 = ' returned from fft_create (ctoc i)' )
    !
    goto 999
    !
  999 continue
    !
    call interpolate_fft_delete ( o )
    !
    i_err = -1
    !
    return
    !
  end subroutine interpolate_fft_create
  !
  ! delete the interpolate_fft object
  !
  subroutine interpolate_fft_delete ( o )
    !
    ! Arguments
    !
    type (interpolate_fft_struct), pointer :: o 
    !
    ! Begin interpolate_fft_delete
    !
    if ( o%interpolate_real ) &
    call fft_delete(o%or_inp)
    if ( o%interpolate_real ) &
    call fft_delete(o%or_out)
    if ( o%interpolate_cplx ) &
    call fft_delete(o%oc_inp)
    if ( o%interpolate_cplx ) &
    call fft_delete(o%oc_out)
    !
    if ( associated ( o ) ) &
         deallocate ( o )
    !
    return
    !
  end subroutine interpolate_fft_delete
  !
  subroutine interpolate_fft_apply_r ( o, tr_inp, tr_out )
    !
    ! Arguments
    !
    type (interpolate_fft_struct), pointer :: o 
    real,                    intent(in   ) :: tr_inp (:) ! input trace
    real,                    intent(inout) :: tr_out (:) ! output trace
    !
    ! Local variables
    !
    real    :: r0_tmp (max(o%ft_inp,o%ft_out) )      ! real 
    complex :: c1_tmp (max(o%ft_inp,o%ft_out)/2 + 1) ! complex
    real    :: r2_tmp (max(o%ft_inp,o%ft_out) )      ! real 
    !complex :: c1_slw (max(o%ft_inp,o%ft_out)/2 + 1) ! complex
    !real    :: r2_slw (max(o%ft_inp,o%ft_out) )      ! real 
    !


    integer :: lt_inp     ! length of input to real   copy
    integer :: lt_out     ! length of real  to output copy
    integer :: nt_inp     ! length of input 
    integer :: nt_out     ! length of output 
    integer :: ft_out     ! number of samples in forward fft
    integer :: ft_inp     ! number of samples in inverse fft
    !
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    !
    ! Begin interpolate_fft_apply_r
    !
    ! copy the local parameters to structure parameters
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'(/, " interpolate_fft_apply_r p=",i4, " c=", i8)', &
    !pcpsx_i_pel(), i_call
    !
    nt_inp = o%nt_inp
    !
    nt_out = o%nt_out
    !
    ft_inp = o%ft_inp
    !
    ft_out = o%ft_out
    !
    lt_inp = min ( a1 = ft_inp, &
                   a2 = nt_inp )       
    !
    lt_out = min ( a1 = ft_out, &
                   a2 = nt_out )       
    !
    ! copy tr_inp to r1_tmp
    !
    r0_tmp ( 1:lt_inp ) = tr_inp ( 1:lt_inp )
    !
    ! zero the remainder of r1_tmp
    !
    r0_tmp ( lt_inp+1:ft_inp ) = 0.0
    !
    c1_tmp ( : ) = cmplx ( 0.0, 0.0 )
    !
    ! real to complex fft of r1_tmp into c2_tmp
    !
    call fft_rc_transform ( o%or_inp, r0_tmp, c1_tmp )
    !
    !call interpolate_fft_rc_slow ( &
    !o%sg_inp, o%sr_inp, o%ft_inp, o%dt_inp, r0_tmp, c1_slw )
    !
    ! apply an origin shift
    !
    if ( o%origin_shift ) &
    call interpolate_fft_origin_shift ( &
    o%sg_inp, .false., o%t0_inp, o%nt_out, o%t0_out, o%dt_out, c1_tmp )
    !
    ! if need be zero the nyquist freuqnecy
    !
    if ( o%ft_out .lt. o%ft_inp ) &
    c1_tmp(o%ft_out/2+1) = cmplx ( 0., 0. )
    !
    ! take complex to real fft - ft_out long of c1_tmp into r2_tmp
    !
    call fft_cr_transform ( o%or_out, c1_tmp, r2_tmp )
    !
    !call interpolate_fft_cr_slow ( &
    !o%sg_out, o%sr_out, o%ft_out, o%dt_out, c1_tmp, r2_slw )
    !
    ! copy r2_tmp to tr_out
    !
    tr_out ( 1:lt_out ) = r2_tmp ( 1:lt_out )
    !
    ! zero the remainder of tr_out
    !
    tr_out ( lt_out+1:nt_out ) = 0.
    !
    !print'(" interpolate_fft_apply_r c=",i8, &
    !& /, " lt_inp=",i8," nt_inp=",i8," tr_inp=",g12.6,1x,g12.6, &
    !& /, " lt_out=",i8," nt_out=",i8," tr_out=",g12.6,1x,g12.6 &
    !& )', &
    !i_call, &
    !lt_inp, nt_inp, minval(tr_inp), maxval(tr_inp), &
    !lt_out, nt_out, minval(tr_out), maxval(tr_out)
    !
    !print'(/," lt_inp=",i8," nt_inp=",i8," tr_inp=",g12.6,1x,g12.6 &
    !& )', &
    !lt_inp, nt_inp, minval(tr_inp), maxval(tr_inp)
    !
    !print'(" real ", i8," f=",g12.6," s=",g12.6)', &
    !( jt_out, r2_tmp(jt_out), r2_slw(jt_out), jt_out = 1 , ft_out )
    !
    !print'(/," lt_out=",i8," nt_out=",i8," tr_out=",g12.6,1x,g12.6 &
    !& )', &
    !lt_out, nt_out, minval(tr_out), maxval(tr_out)
    !
    !print'(" cplx ", i8," f=",g12.6,1x,g12.6," s=",g12.6,1x,g12.6)', &
    !( jt_out, c1_tmp(jt_out), c1_slw(jt_out), jt_out = 1 , ft_out/2+1 )
    !
    return
    !
  end subroutine interpolate_fft_apply_r
  !
  subroutine interpolate_fft_apply_c ( o, tr_inp, tr_out )
    !
    ! Arguments
    !
    type (interpolate_fft_struct), pointer :: o 
    complex,                 intent(in   ) :: tr_inp (:) ! input trace
    complex,                 intent(inout) :: tr_out (:) ! output trace
    !
    ! Local variables
    !
    complex :: c0_tmp (max(o%ft_inp,o%ft_out) )      ! complex
    complex :: c1_tmp (max(o%ft_inp,o%ft_out) )      ! complex
    complex :: c2_tmp (max(o%ft_inp,o%ft_out) )      ! complex
    !complex :: c1_slw (max(o%ft_inp,o%ft_out) )      ! complex
    !complex :: c2_slw (max(o%ft_inp,o%ft_out) )      ! complex
    !


    integer :: lt_inp     ! length of input to real   copy
    integer :: lt_out     ! length of real  to output copy
    integer :: nt_inp     ! length of input 
    integer :: nt_out     ! length of output 
    integer :: ft_inp     ! number of samples in forward fft
    integer :: ft_out     ! number of samples in inverse fft
    !
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    !
    ! Begin interpolate_fft_apply_c
    !
    ! copy the local parameters to structure parameters
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'(/, " interpolate_fft_apply_c p=",i4, " c=", i8)', &
    !pcpsx_i_pel(), i_call
    !
    nt_inp = o%nt_inp
    !
    nt_out = o%nt_out
    !
    ft_inp = o%ft_inp
    !
    ft_out = o%ft_out
    !
    lt_inp = min ( a1 = ft_inp, &
                   a2 = nt_inp )       
    !
    lt_out = min ( a1 = ft_out, &
                   a2 = nt_out ) 
    !
    ! copy tr_inp to c0_tmp
    !
    c0_tmp ( 1:lt_inp ) = tr_inp ( 1:lt_inp )
    !
    ! zero the remainder of c0_tmp
    !
    c0_tmp ( lt_inp+1:ft_inp ) = cmplx ( 0., 0. )
    !
    c1_tmp ( : ) = cmplx ( 0., 0. )
    !
    ! complex to complex fft of c0_tmp into c1_tmp
    !
    call fft_cc_transform ( o%oc_inp, c0_tmp, c1_tmp )
    !
    !call interpolate_fft_cc_slow ( &
    !o%sg_inp, o%sc_inp, o%ft_inp, o%dt_inp, c0_tmp, c1_slw )
    !
    ! apply an origin shift
    !
    if ( o%origin_shift ) &
    call interpolate_fft_origin_shift ( &
    o%sg_inp, .true., o%t0_inp, o%nt_out, o%t0_out, o%dt_out, c1_tmp )
    !
    ! shift the positive and negative frequencies in c1_tmp
    !
    call interpolate_nyquist_shift ( ft_inp, ft_out, c1_tmp, c1_tmp )
    !
    !call interpolate_nyquist_shift ( ft_inp, ft_out, c1_slw, c1_slw )
    !
    ! take complex to complex fft - ft_out long of c1_tmp into c2_tmp 
    !
    call fft_cc_transform ( o%oc_out, c1_tmp, c2_tmp )
    !
    !call interpolate_fft_cc_slow ( &
    !o%sg_out, o%sc_out, o%ft_out, o%dt_out, c1_tmp, c2_slw )
    !
    ! copy c2_tmp to tr_out
    !
    tr_out ( 1:lt_out ) = c2_tmp ( 1:lt_out )
    !
    ! zero the remainder of tr_out
    !
    tr_out ( lt_out+1:nt_out ) = cmplx ( 0., 0. )
    !
    !xxif_i_call : if ( i_call .eq. -1 ) then
    !
    !print'(" interpolate_fft_apply_c c=",i8, &
    !& /, " lt_inp=",i8," nt_inp=",i8," tr_inp=",g12.6,1x,g12.6, &
    !& /, " lt_out=",i8," nt_out=",i8," tr_out=",g12.6,1x,g12.6 &
    !& )', &
    !i_call, &
    !lt_inp, nt_inp, minval(real(tr_inp)), maxval(real(tr_inp)), &
    !lt_out, nt_out, minval(real(tr_out)), maxval(real(tr_out))
    !
    !print'(/," lt_inp=",i8," nt_inp=",i8," tr_inp=",g12.6,1x,g12.6 &
    !& )', &
    !lt_inp, nt_inp, minval(real(tr_inp)), maxval(real(tr_inp))
    !
    !print'(" for ", i8," f=",g12.6,1x,g12.6," s=",g12.6,1x,g12.6)', &
    !( jt_inp, c1_tmp(jt_inp), c1_slw(jt_inp), jt_inp = 1 , ft_inp )
    !
    !print'(/," lt_out=",i8," nt_out=",i8," tr_out=",g12.6,1x,g12.6 &
    !& )', &
    !lt_out, nt_out, minval(real(tr_out)), maxval(real(tr_out))
    !
    !print'(" inv ", i8," f=",g12.6,1x,g12.6," s=",g12.6,1x,g12.6)', &
    !( jt_out, c2_tmp(jt_out), c2_slw(jt_out), jt_out = 1 , ft_out )
    !
    !if ( ft_out .ne. -999 ) stop
    !
    !end if xxif_i_call 
    !
    return
    !
  end subroutine interpolate_fft_apply_c
  !
  subroutine interpolate_nyquist_shift ( ft_inp, ft_out, c0_inp, c0_out )
    !
    ! apply a nyquist shift
    !
    integer,                 intent(in   ) :: ft_inp     ! inp fft len
    integer,                 intent(in   ) :: ft_out     ! out fft len
    complex,                 intent(in   ) :: c0_inp (:) ! inp vector
    complex,                 intent(inout) :: c0_out (:) ! out vector
    !
    ! Local variables
    !
    complex                                :: c0_tmp (max(ft_inp,ft_out) )
    !
    integer                                :: qt_nyq !     nyquist
    integer                                :: qt_inp ! inp nyquist
    integer                                :: qt_out ! out nyquist

    integer                                :: jt_out ! out index
    !
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    !
    qt_inp = ft_inp / 2 + 1
    !
    qt_out = ft_out / 2 + 1
    !
    qt_nyq = min ( qt_inp, qt_out ) 
    !
    c0_tmp ( 1:ft_inp ) = c0_inp ( 1:ft_inp )
    !
    c0_out ( 1:ft_out ) = cmplx ( 0., 0. )
    !
    ! copy the zero freq value
    !
    c0_out ( 1 ) = c0_tmp ( 1 )
    !
    ! copy the nyquist freq value
    !
    if ( qt_out .ge. qt_inp ) &
    c0_out ( qt_inp ) = c0_tmp ( qt_inp )
    !
    ! copy the other positive and negative frequencies
    !
    do_jt_out : do jt_out = 2 , qt_nyq - 1
      !
      c0_out ( jt_out ) = c0_tmp ( jt_out )
      !
      c0_out ( ft_out - jt_out + 2 ) = c0_tmp ( ft_inp - jt_out + 2 )
      !
    end do do_jt_out 
    !
    return
    !
  end subroutine interpolate_nyquist_shift 
  !
  subroutine interpolate_fft_origin_shift ( &
  sign_fft, two_sided, t0_inp, nt_out, t0_out, dt_out, tr_fft )
    !
    ! apply a phase shift to account for an origin difference
    ! for an fft interpolation
    !
    !type (interpolate_fft_struct), pointer :: o 
    integer,                 intent(in   ) :: sign_fft   ! fft sign
    logical,                 intent(in   ) :: two_sided  ! two_sided array
    real,                    intent(in   ) :: t0_inp     ! output origin
    integer,                 intent(in   ) :: nt_out     ! output num
    real,                    intent(in   ) :: t0_out     ! output origin
    real,                    intent(in   ) :: dt_out     ! output inc
    complex,                 intent(inout) :: tr_fft (:) ! output trace
    !
    ! Local variables
    !
    integer                                :: nf_nyq     ! nyquist freq num
    real                                   :: rf_nyq     ! nyquist freq value
    real                                   :: df_out     ! freq inc
    integer                                :: jt_pos     ! pos index
    integer                                :: jt_neg     ! neg index
    complex                                :: c_phase
    real                                   :: c_arg
    !
    nf_nyq = nt_out / 2 + 1                     ! frequency nyquist index
    !
    rf_nyq = .5 / dt_out                        ! frequency nyquist value
    !
    df_out = rf_nyq / ( nf_nyq - 1 )            ! frequency inc 
    !
    c_arg = - sign_fft * ( t0_out - t0_inp ) * 2. * pi
    !
    !c_arg = + sign_fft * ( t0_out - t0_inp ) * 2. * pi
    !
    do_jt_pos : do jt_pos = 2 , nt_out / 2
      !
      jt_neg = nt_out - jt_pos + 2   !-freq index 
      !
      c_phase = c_arg * ( jt_pos - 1 ) * df_out ! complex phase
      !
      tr_fft ( jt_pos ) = tr_fft ( jt_pos ) * cexp ( +c_phase )  !+freq
      !
      if ( two_sided ) &
      tr_fft ( jt_neg ) = tr_fft ( jt_neg ) * cexp ( -c_phase )  !-freq
      !
    end do do_jt_pos 
    !
    ! handle the zero frequency separately
    !
    jt_pos = 1
    !
    c_phase = c_arg * ( jt_pos - 1 ) * df_out ! complex phase
    !
    tr_fft ( jt_pos ) = tr_fft ( jt_pos ) * cexp ( +c_phase )  !+freq
    !
    ! handle the nyquist frequency separately
    !
    jt_pos = nt_out / 2 + 1
    !
    c_phase = c_arg * ( jt_pos - 1 ) * df_out ! complex phase
    !
    tr_fft ( jt_pos ) = tr_fft ( jt_pos ) * cexp ( +c_phase )  !+freq
    !
    return
    !
  end subroutine interpolate_fft_origin_shift 
  !
  subroutine interpolate_amp_phase ( f1_inp, c1_inp, f2_inp, c2_inp, c0_out )
    !
    ! interpolate between c1_inp and c2_inp using amplitude and phase
    !
    real,              intent(in   ) :: f1_inp         ! int coeff 1
    real,              intent(in   ) :: f2_inp         ! int coeff 2
    !
    complex,           intent(in   ) :: c1_inp         ! input  value 1
    complex,           intent(in   ) :: c2_inp         ! input  value 2
    complex,           intent(inout) :: c0_out         ! output value 
    !
    ! Local variables
    !
    real                             :: a0_out         ! output amplitude
    real                             :: p0_out         ! output phase
    !
    real                             :: a1_inp         ! input  amplitude 1
    real                             :: p1_inp         ! input  phase     1
    !
    real                             :: a2_inp         ! input  amplitude 2
    real                             :: p2_inp         ! input  phase     2
    !
    real                             :: d0_inp         ! no wrap phase diff
    real                             :: d1_inp         ! p1_inp wrapped diff
    real                             :: d2_inp         ! p2_inp wrapped diff
    !
    real                             :: s1_inp         ! p1_inp + two_pi
    real                             :: s2_inp         ! p2_inp + two_pi
    !
    real                             :: two_pi         ! 2. * pi
    !
    two_pi = 2. * pi
    !
    ! compute amplitude and phase 1
    !
    a1_inp = abs ( c1_inp )
    !
    xxif_a1_inp : if ( a1_inp .ne. 0 ) then
      !
      p1_inp = atan2 ( aimag ( c1_inp ) , real ( c1_inp ) )
      !
    else xxif_a1_inp ! if ( a1_inp .ne. 0 ) then
      !
      p1_inp = 0.
      !
    end if xxif_a1_inp ! if ( a1_inp .ne. 0 ) then
    !
    ! compute amplitude and phase 2
    !
    a2_inp = real ( c2_inp )
    a2_inp = abs ( c2_inp )
    !
    xxif_a2_inp : if ( a2_inp .ne. 0 ) then
      !
      p2_inp = atan2 ( aimag ( c2_inp ) , real ( c2_inp ) )
      !
    else xxif_a2_inp ! if ( a2_inp .ne. 0 ) then
      !
      p2_inp = 0.
      !
    end if xxif_a2_inp ! if ( a2_inp .ne. 0 ) then
    !
    ! it is possible that one or both of the phases, 
    ! p1_inp, p2_inp have wrapped around +- two_pi .  
    ! If neither or both have there is no problem.
    ! if only one has wrapped the linear interpolation will yield the 
    ! wrong result.
    ! So we look for the combination of the phase differences 
    ! +- two_pi which yield the smallest phase difference between 
    ! the two.  This assumes there should not be > two_pi difference.
    !
    s1_inp  = p1_inp + two_pi          ! p1_inp has wrapped
    s2_inp  = p2_inp + two_pi          ! p2_inp has wrapped
    !
    d0_inp = abs ( p2_inp - p1_inp ) ! neither or both wrapped
    d1_inp = abs ( p2_inp - s1_inp ) ! p1_inp has wrapped
    d2_inp = abs ( s2_inp - p1_inp ) ! p2_inp has wrapped 
    !
    if_dp_inp : if ( d1_inp .le. min ( d0_inp , d2_inp ) ) then
      !
      ! p1_inp has wrapped 
      !
      p1_inp = s1_inp
      !
    else if ( d2_inp .le. min ( d0_inp , d1_inp ) ) then
      !
      ! p2_inp has wrapped 
      !
      p2_inp = s2_inp
      !
    end if if_dp_inp
    !
    ! interpolate the amplitude and phase
    !
    a0_out = f1_inp * a1_inp + f2_inp * a2_inp
    !
    p0_out = f1_inp * p1_inp + f2_inp * p2_inp
    !
    ! reconstruct c0_out from the interpolated amplitude and phase
    !
    c0_out = cmplx ( a0_out * cos ( p0_out ), a0_out * sin ( p0_out ) ) 
    !
    return
    !
  end subroutine interpolate_amp_phase 
  !
  ! 3D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_3d_to_1d_a ( n1_inp, o1_inp, d1_inp, &
                                      n2_inp, o2_inp, d2_inp, &
                                      n3_inp, o3_inp, d3_inp, &
                                      x0_inp,                 &
                                      n1_out, o1_out, d1_out, &
                                      i1_1, i1_2, f1_1, f1_2, &
                                      x2_out,                 &
                                      x3_out,                 &
                                      x0_out )
    !
    ! Arguments
    !
    integer,  intent(in   ) :: n1_inp
    real,     intent(in   ) :: o1_inp
    real,     intent(in   ) :: d1_inp
    !
    integer,  intent(in   ) :: n2_inp
    real,     intent(in   ) :: o2_inp
    real,     intent(in   ) :: d2_inp
    !
    integer,  intent(in   ) :: n3_inp
    real,     intent(in   ) :: o3_inp
    real,     intent(in   ) :: d3_inp
    !
    real,     intent(in   ) :: x0_inp (n1_inp, n2_inp, n3_inp)
    !
    integer,  intent(in   ) :: n1_out
    real,     intent(in   ) :: o1_out
    real,     intent(in   ) :: d1_out
    !
    real,     intent(in   ) :: x2_out
    !
    real,     intent(in   ) :: x3_out
    !
    real,     intent(  out) :: x0_out (n1_out)
    !
    integer,  intent(in   ) :: i1_1 ( : )
    integer,  intent(in   ) :: i1_2 ( : )
    real,     intent(in   ) :: f1_1 ( : )
    real,     intent(in   ) :: f1_2 ( : )
    !
    ! Local variables
    !
    integer :: i1_out
    !
    real    :: x2_inp, d2_inv
    integer :: i2_1, i2_2
    real    :: f2_1, f2_2
    !

    real    :: x3_inp, d3_inv
    integer :: i3_1, i3_2
    real    :: f3_1, f3_2
    real    :: f2_1_f3_1 ! f2_1 * f3_1
    real    :: f2_2_f3_1 ! f2_2 * f3_1 
    real    :: f2_1_f3_2 ! f2_1 * f3_2 
    real    :: f2_2_f3_2 ! f2_2 * f3_2 
    !
    ! Begin interpolate_3d_to_1d_a
    !
    d2_inv = interpolate_invert_1 (d2_inp)
    d3_inv = interpolate_invert_1 (d3_inp)
    !
    i3_1   = max(1, min(n3_inp, int((x3_out-o3_inp)*d3_inv)+1))
    i3_2   = max(1, min(n3_inp, i3_1+1))
    x3_inp = (i3_1 - 1) * d3_inp + o3_inp
    f3_2   = max(0., min(1., (x3_out-x3_inp)*d3_inv))
    f3_1   = 1. - f3_2
    !
    i2_1   = max(1, min(n2_inp, int((x2_out-o2_inp)*d2_inv)+1))
    i2_2   = max(1, min(n2_inp, i2_1+1))
    x2_inp = (i2_1 - 1) * d2_inp + o2_inp
    f2_2   = max(0., min(1., (x2_out-x2_inp)*d2_inv))
    f2_1   = 1. - f2_2
    !
    f2_1_f3_1 = f2_1 * f3_1
    f2_2_f3_1 = f2_2 * f3_1 
    f2_1_f3_2 = f2_1 * f3_2 
    f2_2_f3_2 = f2_2 * f3_2 
    !
    do i1_out = 1, n1_out
      !
      x0_out (i1_out) =   &
                  f2_1_f3_1 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_1, i3_1) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_1, i3_1) ) &
                + f2_2_f3_1 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_2, i3_1) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_2, i3_1) ) &
                + f2_1_f3_2 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_1, i3_2) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_1, i3_2) ) &
                + f2_2_f3_2 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_2, i3_2) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_2, i3_2) ) 
      !  
    end do    ! do i1_out = 1, n1_out
    !
    return
    !
  end subroutine interpolate_3d_to_1d_a
  !
  ! 3D interpolation from x0_inp to x0_out
  !
  subroutine interpolate_3d_to_1d_b ( x0_inp,                  &
                                      n1_out,                  &
                                      i1_1, i1_2, f1_1, f1_2,  &
                                      i2_1, i2_2, f2_1, f2_2,  &
                                      i3_1, i3_2, f3_1, f3_2,  &
                                      x0_out )
    !
    ! Arguments
    !
    real,     intent(in   ) :: x0_inp (:, :, :)
    !
    integer,  intent(in   ) :: n1_out
    !
    real,     intent(  out) :: x0_out (:)
    !
    integer,  intent(in   ) :: i1_1 ( : )
    integer,  intent(in   ) :: i1_2 ( : )
    real,     intent(in   ) :: f1_1 ( : )
    real,     intent(in   ) :: f1_2 ( : )
    !
    integer,  intent(in   ) :: i2_1 
    integer,  intent(in   ) :: i2_2 
    real,     intent(in   ) :: f2_1 
    real,     intent(in   ) :: f2_2 
    !
    integer,  intent(in   ) :: i3_1 
    integer,  intent(in   ) :: i3_2 
    real,     intent(in   ) :: f3_1 
    real,     intent(in   ) :: f3_2 
    !
    ! Local variables
    !
    integer :: i1_out
    !
    real    :: f2_1_f3_1 ! f2_1 * f3_1
    real    :: f2_2_f3_1 ! f2_2 * f3_1 
    real    :: f2_1_f3_2 ! f2_1 * f3_2 
    real    :: f2_2_f3_2 ! f2_2 * f3_2 
    !
    ! Begin interpolate_3d_to_1d_b
    !
    f2_1_f3_1 = f2_1 * f3_1
    f2_2_f3_1 = f2_2 * f3_1 
    f2_1_f3_2 = f2_1 * f3_2 
    f2_2_f3_2 = f2_2 * f3_2 
    !
    do i1_out = 1, n1_out
      !
      x0_out (i1_out) =   &
                  f2_1_f3_1 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_1, i3_1) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_1, i3_1) ) &
                + f2_2_f3_1 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_2, i3_1) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_2, i3_1) ) &
                + f2_1_f3_2 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_1, i3_2) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_1, i3_2) ) &
                + f2_2_f3_2 &
                        * ( f1_1(i1_out) * x0_inp (i1_1(i1_out), i2_2, i3_2) &
                          + f1_2(i1_out) * x0_inp (i1_2(i1_out), i2_2, i3_2) ) 
      !  
    end do    ! do i1_out = 1, n1_out
    !
    return
    !
  end subroutine interpolate_3d_to_1d_b
  !
  subroutine interpolate_linear_create ( &
                                         o, &
                                         lu_out, &
                                         nx_inp, rx_inp, &
                                         nx_out, rx_out, &
                                         i_err &
                                       )
    !
    ! compute linear interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_linear_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point  values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 


    integer                    :: i_err_inp ! inp regularity flag
    integer                    :: i_err_out ! out regularity flag
    ! 
    real                       :: ra_inp(nx_inp)
    real                       :: ra_out(nx_out)
    complex                    :: ca_inp(nx_inp)
    complex                    :: ca_out(nx_out)
    ! 
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    ! 
    ! intialize the error flag
    ! 
! if ( pcpsx_i_pel() .eq. 0 &
! .and. ( &
!         i_call .le. 100 .or. mod ( i_call ,1000 ) .eq. 1 &
!  .or. ( i_call .ge. 1000 .and. i_call .le. 2000 ) &
!       ) ) &
!print'(" top interpolate_linear_create p=",i4," c=",i8)', pcpsx_i_pel(), i_call
    ! 
    i_err = 0
    ! 
    ! allocate the object
    ! 
    allocate ( o )
    !
    nullify (o%ry_inp) ! jpa
    nullify (o%ry_out) ! jpa
    nullify (o%ix_inp_1) ! jpa
    nullify (o%ix_inp_2) ! jpa
    nullify (o%fx_inp_1) ! jpa
    nullify (o%fx_inp_2) ! jpa
    ! 
    ! copy the input info
    ! 
    o%i_call = i_call
    ! 
    o%lu_out = lu_out
    ! 
    o%nx_inp = nx_inp
    o%x0_inp = minval ( rx_inp ( 1:nx_inp ) )
    o%x1_inp = maxval ( rx_inp ( 1:nx_inp ) )
    ! 
    o%ny_inp = o%nx_inp
    o%y0_inp = o%x0_inp 
    o%y1_inp = o%x1_inp 
    ! 
    o%nx_out = nx_out
    o%x0_out = minval ( rx_out ( 1:nx_out ) )
    o%x1_out = maxval ( rx_out ( 1:nx_out ) )
    ! 
    o%ny_out = o%nx_out
    o%y0_out = o%x0_out 
    o%y1_out = o%x1_out 
    ! 
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " interpolate_linear_create ", &
    & /, " lu_out=", i8 &
    & )') &
    lu_out
    !
    ! allocate linear coefficients
    ! 
    call memfun_all ( o%ry_inp,   o%ny_inp, 'ry_inp',   i_err )
    call memfun_all ( o%ry_out,   o%ny_out, 'ry_out',   i_err )
    call memfun_all ( o%ix_inp_1, o%nx_out, 'ix_inp_1', i_err )
    call memfun_all ( o%ix_inp_2, o%nx_out, 'ix_inp_2', i_err )
    call memfun_all ( o%fx_inp_1, o%nx_out, 'fx_inp_1', i_err )
    call memfun_all ( o%fx_inp_2, o%nx_out, 'fx_inp_2', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    ! 
    o%ry_inp ( 1:o%ny_inp ) = rx_inp ( 1:o%ny_inp ) 
    ! 
    o%ry_out ( 1:o%ny_out ) = rx_out ( 1:o%ny_out ) 
    ! 
    ! compute the regular grid which describes inp, out
    ! 
    call interpolate_array_size ( o%nx_inp, o%x0_inp, o%dx_inp, &
                                  o%ny_inp, o%ry_inp, i_err_inp )
    ! 
    call interpolate_array_size ( o%nx_out, o%x0_out, o%dx_out, &
                                  o%ny_out, o%ry_out, i_err_out )
    ! 
    ! compute the x inp to out linear interpolation coefficients
    !
    xxif_regular : if ( i_err_inp .eq. 0 .and. i_err_out .eq. 0 ) then
      ! 
      ! both inp and out are regular
      !
      call interpolate_find_index_h ( &
                                      o%nx_inp, o%x0_inp, o%dx_inp, &
                                      o%nx_out, o%x0_out, o%dx_out, &
                                      o%ix_inp_1, o%ix_inp_2, &
                                      o%fx_inp_1, o%fx_inp_2 &
                                    )
      ! 
    else xxif_regular 
      !
      ! one or both of inp, out are not regular
      ! 
      call interpolate_find_index_n ( &
                                      o%nx_inp, rx_inp, &
                                      o%nx_out, rx_out, &
                                      o%ix_inp_1, o%ix_inp_2, &
                                      o%fx_inp_1, o%fx_inp_2 &
                                    )
      ! 
    end if xxif_regular 
    ! 
    if ( lu_out .ge. 0 ) &
    call interpolate_linear_print ( o, 'interpolate_linear_create' )
    !
    if ( minval ( o%ix_inp_1 ( 1:o%nx_out ) ) .lt. 1 &
    .or. maxval ( o%ix_inp_1 ( 1:o%nx_out ) ) .gt. o%nx_inp &
    .or. minval ( o%ix_inp_2 ( 1:o%nx_out ) ) .lt. 1 &
    .or. maxval ( o%ix_inp_2 ( 1:o%nx_out ) ) .gt. o%nx_inp ) go to 997
    !
    xxif_test : &
    if ( pcpsx_i_pel() .eq. -1 &
    .and. ( &
            i_call .le. 100 .or. mod ( i_call ,1000 ) .eq. 1 &
     .or. ( i_call .ge. 1000 .and. i_call .le. 2000 ) &
          ) ) then
    !
    ra_inp(1:nx_inp) = 1.
    ca_inp(1:nx_inp) = cmplx(1.,0.)
    ra_out(1:nx_out) = 1.
    ca_out(1:nx_out) = cmplx(1.,0.)
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
print'(" bef interpolate_linear_apply_r p=",i4," c=",i8)', &
pcpsx_i_pel(), i_call
    !
    call interpolate_linear_apply_r ( o, ra_inp, ra_out )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
print'(" bef interpolate_linear_apply_c p=",i4," c=",i8)', &
pcpsx_i_pel(), i_call
    !
    call interpolate_linear_apply_c ( o, ca_inp, ca_out )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
print'(" aft interpolate_linear_apply_r p=",i4," c=",i8)', &
pcpsx_i_pel(), i_call
    !
    end if xxif_test 
    !
    return
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_linear_create " &
    & /, " min(ix_inp_1)=", i8, & 
    & /, " max(ix_inp_1)=", i8, & 
    & /, " min(ix_inp_1)=", i8, & 
    & /, " max(ix_inp_2)=", i8, & 
    & /, "     nx_inp   =", i8  &
    & )') &
    minval ( o%ix_inp_1 ( 1:o%nx_out ) ), &
    maxval ( o%ix_inp_1 ( 1:o%nx_out ) ), &
    minval ( o%ix_inp_2 ( 1:o%nx_out ) ), &
    maxval ( o%ix_inp_2 ( 1:o%nx_out ) ), &
    o%nx_inp
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_linear_create " &
    & /, " during memory allocation " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_linear_create " &
    & )') 
    !
    call interpolate_linear_print ( o, 'interpolate_linear_create' )
    !
    return
    !
  end subroutine interpolate_linear_create
  !
  subroutine interpolate_linear_print ( o, c_title )
    !
    ! compute linear interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_linear_struct), pointer  :: o 
    character(len=*), intent(in   ) :: c_title
    !
    ! Local variables
    ! 
    integer                    :: iy_inp    !  input index
    integer                    :: iy_out    ! output index
    integer                    :: ix_inp    !  input index
    integer                    :: ix_out    ! output index
    real                       :: rx_inp(o%nx_inp)
    real                       :: rx_out(o%nx_out)
    ! 
    do_ix_inp : do ix_inp = 1 , o%nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * o%dx_inp + o%x0_inp
      ! 
    end do do_ix_inp 
    ! 
    do_ix_out : do ix_out = 1 , o%nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * o%dx_out + o%x0_out
      ! 
    end do do_ix_out 
    ! 
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " interpolate_linear_print ", a &
    & )') &
    trim ( c_title )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " original input ", &
    & /, " ny_inp=", i8, " y0_inp=" , g12.6, " y1_inp=", g12.6, &
    & /, " iy_inp  ry_inp " &
    & )') &
    o%ny_inp, o%y0_inp, o%y1_inp
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & 1x, i8, 1x, g12.6 &
    & )') &
    ( iy_inp, o%ry_inp ( iy_inp ), iy_inp = 1 , o%ny_inp )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " original output ", &
    & /, " ny_out=", i8, " y0_out=" , g12.6, " y1_out=", g12.6, &
    & /, " iy_out  ry_out " &
    & )') &
    o%ny_out, o%y0_out, o%y1_out
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & 1x, i8, 1x, g12.6 &
    & )') &
    ( iy_out, o%ry_out ( iy_out ), iy_out = 1 , o%ny_out )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " min(ix_inp_1)=", i8, & 
    & /, " max(ix_inp_1)=", i8, & 
    & /, " min(ix_inp_1)=", i8, & 
    & /, " max(ix_inp_2)=", i8 & 
    & )') &
    minval ( o%ix_inp_1 ( 1:o%nx_out ) ), &
    maxval ( o%ix_inp_1 ( 1:o%nx_out ) ), &
    minval ( o%ix_inp_2 ( 1:o%nx_out ) ), &
    maxval ( o%ix_inp_2 ( 1:o%nx_out ) )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " gridded input and output ", &
    & /, " nx_inp=", i8, " x0_inp=" , g12.6, " x1_inp=", g12.6, &
    & " dx_inp=", g12.6, &
    & /, " nx_out=", i8, " x0_out=" , g12.6, " x1_out=", g12.6, &
    & " dx_out=", g12.6, &
    & /, " ix_out  rx_out       rx_inp_1     rx_inp_2      ", &
    & " ix_inp_1 ix_inp_2 fx_inp_1 fx_inp_2 " &
    & )') &
    o%nx_inp, o%x0_inp, o%x1_inp, o%dx_inp, &
    o%nx_out, o%x0_out, o%x1_out, o%dx_out
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & 1x, i8, 1x, g12.6, 1x, g12.6, 1x, g12.6, &
    & 1x, i6, 1x, i6, 1x, g12.6, 1x, g12.6 &
    & )') &
    ( ix_out, rx_out ( ix_out ), &
      rx_inp ( o%ix_inp_1 ( ix_out ) ), &
      rx_inp ( o%ix_inp_2 ( ix_out ) ), &
      o%ix_inp_1 ( ix_out ), o%ix_inp_2 ( ix_out ), &
      o%fx_inp_1 ( ix_out ), o%fx_inp_2 ( ix_out ), &
      ix_out = 1 , o%nx_out )
    !
    return
    !
  end subroutine interpolate_linear_print
  !
  subroutine interpolate_linear_create_ir ( &
                                            o, &
                                            lu_out, &
                                            nx_inp, rx_inp, &
                                            nx_out, x0_out, dx_out, &
                                            i_err &
                                          )
    !
    ! compute linear interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_linear_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: x0_out    ! output point  min
    real,        intent(in   ) :: dx_out    ! output point  inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_out    ! output index
    real                       :: rx_out(nx_out) ! output values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_out values
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_linear_create ( &
                                     o, &
                                     lu_out, &
                                     nx_inp, rx_inp, &
                                     nx_out, rx_out, &
                                     i_err &
                                   )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_linear_create_ir " &
    & )') 
    !
    return
    !
  end subroutine interpolate_linear_create_ir 
  !
  subroutine interpolate_linear_create_ri ( &
                                            o, &
                                            lu_out, &
                                            nx_inp, x0_inp, dx_inp, &
                                            nx_out, rx_out, &
                                            i_err &
                                          )
    !
    ! compute linear interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_linear_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: x0_inp    !  input point  min
    real,        intent(in   ) :: dx_inp    !  input point  inc
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! input index
    real                       :: rx_inp(nx_inp) ! input values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    call interpolate_linear_create ( &
                                     o, &
                                     lu_out, &
                                     nx_inp, rx_inp, &
                                     nx_out, rx_out, &
                                     i_err &
                                   )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_linear_create_ri " &
    & )') 
    !
    return
    !
  end subroutine interpolate_linear_create_ri 
  !
  subroutine interpolate_linear_create_rr ( &
                                            o, &
                                            lu_out, &
                                            nx_inp, x0_inp, dx_inp, &
                                            nx_out, x0_out, dx_out, &
                                            i_err &
                                          )
    !
    ! compute linear interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_linear_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    !
    integer,     intent(in   ) :: nx_inp    ! x inp num
    real,        intent(in   ) :: x0_inp    ! x inp min
    real,        intent(in   ) :: dx_inp    ! x inp inc
    !
    integer,     intent(in   ) :: nx_out    ! x out num
    real,        intent(in   ) :: x0_out    ! x out min
    real,        intent(in   ) :: dx_out    ! x out inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! x inp index
    real                       :: rx_inp(nx_inp) ! x inp values
    ! 
    integer                    :: ix_out         ! x out index
    real                       :: rx_out(nx_out) ! x out value 
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_linear_create ( &
                                     o, &
                                     lu_out, &
                                     nx_inp, rx_inp, &
                                     nx_out, rx_out, &
                                     i_err &
                                   )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_linear_create_rr " &
    & )') 
    !
    return
    !
  end subroutine interpolate_linear_create_rr 
  !
  subroutine interpolate_linear_delete ( o )
    !
    !  delete the linear interpolation object
    !
    type (interpolate_linear_struct), pointer  :: o 
    !
    ! Local variables
    ! 
    call memfun_del ( o%ry_inp   )
    call memfun_del ( o%ry_out   )
    call memfun_del ( o%ix_inp_1 )
    call memfun_del ( o%ix_inp_2 )
    call memfun_del ( o%fx_inp_1 )
    call memfun_del ( o%fx_inp_2 )
    ! 
    ! deallocate the object
    ! 
    deallocate ( o )
    !
    return
    !
  end subroutine interpolate_linear_delete
  !
  subroutine interpolate_linear_apply_r ( o, ry_inp, ry_out )
    !
    !  apply linear interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_linear_struct), pointer  :: o 
    !
    real,        intent(in   ) :: ry_inp(:) ! input  values
    !
    real,        intent(inout) :: ry_out(:) ! output values
    !
    ! Local variables
    ! 

    integer                    :: ix_out    ! output index


    real                       :: ry_tmp(o%nx_inp)
    ! 
    ry_tmp ( 1:o%nx_inp ) = ry_inp ( 1:o%nx_inp )
    ! 
    do_ix_out : do ix_out = 1 , o%nx_out
      ! 
      ry_out ( ix_out ) = &
      o%fx_inp_1(ix_out) * ry_tmp ( o%ix_inp_1(ix_out) ) &
    + o%fx_inp_2(ix_out) * ry_tmp ( o%ix_inp_2(ix_out) ) 
      ! 
      !print'(" q12",1x,i6,1x,g12.6,1x,g12.6)',ix_out,ry_out(ix_out),&
      !maxval(abs(ry_out( 1:o%nx_out)))
      ! 
    end do do_ix_out 
    ! 
    return
    !
  end subroutine interpolate_linear_apply_r 
  !
  subroutine interpolate_linear_apply_c ( o, ry_inp, ry_out )
    !
    !  apply linear interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_linear_struct), pointer  :: o 
    !
    complex,     intent(in   ) :: ry_inp(:) ! input  values
    !
    complex,     intent(inout) :: ry_out(:) ! output values
    !
    ! Local variables
    ! 

    integer                    :: ix_out    ! output index


    complex                    :: ry_tmp(o%nx_inp)
    ! 
    ry_tmp ( 1:o%nx_inp ) = ry_inp ( 1:o%nx_inp )
    ! 
    do_ix_out : do ix_out = 1 , o%nx_out
      ! 
      ry_out ( ix_out ) = &
      o%fx_inp_1(ix_out) * ry_tmp ( o%ix_inp_1(ix_out) ) &
    + o%fx_inp_2(ix_out) * ry_tmp ( o%ix_inp_2(ix_out) ) 
      ! 
      !print'(" q12",1x,i6,1x,g12.6,1x,g12.6)',ix_out,ry_out(ix_out),&
      !maxval(abs(ry_out( 1:o%nx_out)))
      ! 
    end do do_ix_out 
    ! 
    return
    !
  end subroutine interpolate_linear_apply_c 
  !
  subroutine interpolate_lagrange_create ( &
                                           o, &
                                           lu_out, rx_inc, &
                                           nx_inp, rx_inp, &
                                           nx_out, rx_out, &
                                           i_err &
                                         )
    !
    ! compute lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    ! Encyclopedic Dictionary of Exploration Geophysics Second Edition
    ! Page 138
    ! y(x) = sum a1(x) * y1
    ! ai(x) = b(x) / ci / ( x - xi )
    ! b(x) = ( x  - x1 ) * ( x  - x2 ) * ... * ( x  - xn )
    ! ci   = ( xi - x1 ) * ( xi - x2 ) * ... * ( xi - xn ) / ( xi - xi )
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point  values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp_1 ( nx_out )
    integer                    :: ix_inp_2 ( nx_out )
    real                       :: fx_inp_1 ( nx_out )
    real                       :: fx_inp_2 ( nx_out )
    ! 
    integer                    :: i1_cof
    integer                    :: i2_cof
    integer                    :: i3_cof
    integer                    :: i4_cof

    integer                    :: j0_cof    ! interp coef index
    integer                    :: k0_cof    ! interp coef index
    integer                    :: ix_err    !  error index
    integer                    :: ix_inp    !  input index
    integer                    :: ix_out    ! output index
    real                       :: dx_inc    ! sinc inc
    real                       :: dx_eps    ! min x distance for normalization
    integer                    :: i_err_inp ! inp regularity flag
    integer                    :: i_err_out ! out regularity flag
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    ! 
    ! intiialize the error flag
    ! 
    i_err = 0
    ! 
    ! allocate the object
    ! 
    allocate ( o )
    !
    nullify (o%i0_cof) ! jpa
    nullify (o%w0_cof) ! jpa
    ! 
    !print'(" top interpolate_lagrange_create p=",i4," c=",i8)', &
    !pcpsx_i_pel(), i_call
    !
    o%nx_inp = nx_inp
    o%x0_inp = minval ( rx_inp ( 1:nx_inp ) )
    o%x1_inp = maxval ( rx_inp ( 1:nx_inp ) )
    o%dx_inp = ( o%x1_inp - o%x0_inp ) / max ( 1, o%nx_inp - 1 )
    ! 
    o%nx_out = nx_out
    o%x0_out = minval ( rx_out ( 1:nx_out ) )
    o%x1_out = maxval ( rx_out ( 1:nx_out ) )
    o%dx_out = ( o%x1_out - o%x0_out ) / max ( 1, o%nx_out - 1 )
    ! 
    o%lu_out = lu_out
    ! 
    o%rx_inc = rx_inc
    ! 
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " interpolate_lagrange_create ", &
    & /, " lu_out=", i8, &
    & /, " rx_inc=" , g12.6 &
    & )') &
    lu_out, rx_inc
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " nx_inp=", i8, " x0_inp=" , g12.6, " x1_inp=", g12.6, &
    & /, " ix_inp  rx_inp " &
    & )') &
    o%nx_inp, o%x0_inp, o%x1_inp
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & 1x, i8, 1x, g12.6 &
    & )') &
    ( ix_inp, rx_inp ( ix_inp ), ix_inp = 1 , nx_inp )
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " nx_out=", i8, " x0_out=" , g12.6, " x1_out=", g12.6, &
    & /, " ix_out  rx_out " &
    & )') &
    o%nx_out, o%x0_out, o%x1_out
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & 1x, i8, 1x, g12.6 &
    & )') &
    ( ix_out, rx_out ( ix_out ), ix_out = 1 , nx_out )
    ! 
    ! allocate struct version of rx_inp
    ! 
    !call memfun_all ( o%rx_inp, nx_inp, 'rx_inp', i_err )
    ! 
    !call memfun_all ( o%rx_out, nx_out, 'rx_out', i_err )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    ! compute the regular grid which describes inp, out
    ! 
    call interpolate_array_size ( o%ny_inp, o%y0_inp, o%dy_inp, &
                                    nx_inp, rx_inp, i_err_inp )
    ! 
    call interpolate_array_size ( o%ny_out, o%y0_out, o%dy_out, &
                                    nx_out, rx_out, i_err_out )
    ! 
    ! allocate the memory for the interpoaltion indices and weights
    ! each output point has n0_cof coeficients
    ! 
    dx_inc = min ( o%dy_inp, o%dy_out )
    ! 
    dx_eps = dx_inc * 1.e-3
    ! 
    !print'( &
    !& /, " ss0 dx_inp=",g12.6, " dx_out=",g12.6, &
    !& /, " ss0 dy_inp=",g12.6, " dy_out=",g12.6, &
    !& /, " ss0 dx_inc=",g12.6, " dx_eps=",g12.6 )', &
    !o%dx_inp, o%dx_out, &
    !o%dy_inp, o%dy_out, &
    !dx_inc, dx_eps
    ! 
    !print*,' bb0 rx_inc=',rx_inc,' dx_inc=',dx_inc
    ! 
    o%n0_cof = max ( 4, min ( 4, nint ( rx_inc / dx_inc ) ) ) 
    ! 
    !print'(" bb0 b n0_cof=",i8," nx_out=",i8)', o%n0_cof, o%nx_out
    ! 
    o%n0_cof = max ( 1, nint ( rx_inc / dx_inc ) ) 
    ! 
    !print'(" bb0 c n0_cof=",i8," nx_out=",i8)', o%n0_cof, o%nx_out
    ! 
    o%n0_cof = ( o%n0_cof / 2 ) * 2
    ! 
    !print'(" bb0 d n0_cof=",i8," nx_out=",i8)', o%n0_cof, o%nx_out
    ! 
    call memfun_all ( o%i0_cof, o%n0_cof, o%nx_out, 'i0_cof', i_err )
    call memfun_all ( o%w0_cof, o%n0_cof, o%nx_out, 'w0_cof', i_err )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    ! compute the x inp to out linear interpolation coefficients
    ! 
    !print'(" bb0 i_err_inp=",i8," i_err_out=",i8)', i_err_inp, i_err_out 
    ! 
    xxif_regular : if ( i_err_inp .eq. 0 .and. i_err_out .eq. 0 ) then
      ! 
      ! both inp and out are regular
      !
      call interpolate_find_index_h ( &
                                      o%nx_inp, o%x0_inp, o%dx_inp, &
                                      o%nx_out, o%x0_out, o%dx_out, &
                                      ix_inp_1, ix_inp_2, &
                                      fx_inp_1, fx_inp_2 &
                                    )
      ! 
    else xxif_regular 
      !
      ! one or both of inp, out are not regular
      ! 
      call interpolate_find_index_n ( &
                                      o%nx_inp, rx_inp, &
                                      o%nx_out, rx_out, &
                                      ix_inp_1, ix_inp_2, &
                                      fx_inp_1, fx_inp_2 &
                                    )
      ! 
    end if xxif_regular 
    !
    do_ix_out_1 : do ix_out = 1 , nx_out
      !
      do_j0_cof_1 : do j0_cof = 1 , o%n0_cof/2
        !
        o%i0_cof ( j0_cof, ix_out ) = max ( 1 , min ( nx_inp, &
        ix_inp_1 ( ix_out ) - ( o%n0_cof/2 - j0_cof ) ) )
        !
        !if ( ix_out .eq. 1 ) &
        !print'(" bb0 j0_cof=",i8," i0_cof=",i8," ix_inp_1=",i8)', &
        !j0_cof, o%i0_cof ( j0_cof, ix_out ), ix_inp_1 ( ix_out )
        !
      end do do_j0_cof_1 
      !
      do_j0_cof_2 : do j0_cof = o%n0_cof/2+1 , o%n0_cof
        !
        o%i0_cof ( j0_cof, ix_out ) = max ( 1 , min ( nx_inp, &
        ix_inp_2 ( ix_out ) + ( j0_cof - o%n0_cof/2 - 1 ) ) )
        !
        !if ( ix_out .eq. 1 ) &
        !print'(" bb0 j0_cof=",i8," i0_cof=",i8," ix_inp_2=",i8)', &
        !j0_cof, o%i0_cof ( j0_cof, ix_out ), ix_inp_2 ( ix_out )
        !
      end do do_j0_cof_2 
      !
!if ( ix_inp_1(ix_out) .lt. 1 .or. ix_inp_1(ix_out) .gt. o%nx_inp &
!.or. ix_inp_2(ix_out) .lt. 1 .or. ix_inp_2(ix_out) .gt. o%nx_inp ) stop
!print'(" err ix_out=",i8, " nx_inp=",i8, &
!& " ix_inp_1=",i8," ix_inp_2=",i8)', &
!ix_out, o%nx_inp, &
!ix_inp_1(ix_out), ix_inp_1(ix_out) 
      !
!if ( ix_inp_1(ix_out) .lt. 1 .or. ix_inp_1(ix_out) .gt. o%nx_inp &
!.or. ix_inp_2(ix_out) .lt. 1 .or. ix_inp_2(ix_out) .gt. o%nx_inp ) stop
      !
    end do do_ix_out_1 
    !
    do_ix_out_2 : do ix_out = 1 , nx_out
      !
      do_j0_cof_3 : do j0_cof = 1, o%n0_cof
        !
        o%w0_cof ( j0_cof, ix_out ) = 1.
        !
        ! if the input and output x locations are the same
        ! set this coefficent to 1 and the rest to zero
        ! then cycle to the next output x location
        !
        xxif_inp_out_the_same : &
        if ( &
             abs ( rx_out(                ix_out  )  &
                 - rx_inp(o%i0_cof(j0_cof,ix_out) ) ) .le. dx_eps ) then
          !
          o%w0_cof ( :,      ix_out ) = 0.
          !
          o%w0_cof ( j0_cof, ix_out ) = 1.
          !
         ! print'(" ss0 ix_out=",i8," j0_cof=",i8," i0_cof=",i8,&
         ! & " rx_inp=",g12.6," rx_out=",g12.6)', &
         ! ix_out, j0_cof, o%i0_cof(j0_cof,ix_out), &
         ! rx_inp(o%i0_cof(j0_cof,ix_out)), rx_out ( ix_out )
          !
          go to 1
          !
        end if xxif_inp_out_the_same 
        !
        ! normalize the weight coefficient
        !
        do_k0_cof_3 : do k0_cof = 1, o%n0_cof
          !
          if ( j0_cof .ne. k0_cof &
       .and. o%i0_cof(j0_cof,ix_out) .ne. o%i0_cof(k0_cof,ix_out) ) &
          o%w0_cof ( j0_cof, ix_out ) = &
          o%w0_cof ( j0_cof, ix_out ) &
* ( rx_out(                  ix_out)  &
  - rx_inp(o%i0_cof(k0_cof,ix_out)) ) &
/ ( rx_inp(o%i0_cof(j0_cof,ix_out)) &
  - rx_inp(o%i0_cof(k0_cof,ix_out)) ) 
          !
        end do do_k0_cof_3 
        !
      end do do_j0_cof_3 
      !
      ! come to here if the the input and output x locations are the same
      !
    1 continue
      !
    end do do_ix_out_2 
    ! 
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " interpolate_lagrange_create ", &
    & /, " number of interpolation coefficients n0_cof=", i8 &
    & )') &
    o%n0_cof
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " nx_out=", i8, " x0_out=" , g12.6, " x1_out=", g12.6, &
    & " dx_out=", g12.6, " dx_inp=", g12.6, &
    & /, " ix_out  rx_out       rx_inp_1     rx_inp_2      ", &
    & " ix_inp_1 ix_inp_2 fx_inp_1 fx_inp_2 " &
    & )') &
    o%nx_out, o%x0_out, o%x1_out, o%dx_out, o%dx_inp
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & 1x, i8, 1x, g12.6, 1x, g12.6, 1x, g12.6, &
    & 1x, i6, 1x, i6, 1x, g12.6, 1x, g12.6 &
    & )') &
    ( ix_out, rx_out ( ix_out ), &
      rx_inp ( ix_inp_1 ( ix_out ) ), rx_inp ( ix_inp_2 ( ix_out ) ), &
      ix_inp_1 ( ix_out ), ix_inp_2 ( ix_out ), &
      fx_inp_1 ( ix_out ), fx_inp_2 ( ix_out ), &
      ix_out = 1 , nx_out )
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " ix_out  rx_out       i0_cof w0_cof " &
    & )') 
    !
    i1_cof = max ( 1 , min ( o%n0_cof, o%n0_cof / 2 - 2 + 1 ) )
    i2_cof = max ( 1 , min ( o%n0_cof, o%n0_cof / 2 - 2 + 2 ) )
    i3_cof = max ( 1 , min ( o%n0_cof, o%n0_cof / 2 - 2 + 3 ) )
    i4_cof = max ( 1 , min ( o%n0_cof, o%n0_cof / 2 - 2 + 4 ) )
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 .and. o%n0_cof .ge. 4 ) &
    write ( lu_out, ' ( &
    & 1x, i8, 1x, g12.6, &
    & 1x, i6, 1x, i6, 1x, i6, 1x, i6, &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
    & )') &
    ( ix_out, rx_out ( ix_out ), &
      o%i0_cof ( i1_cof, ix_out ), &
      o%i0_cof ( i2_cof, ix_out ), &
      o%i0_cof ( i3_cof, ix_out ), &
      o%i0_cof ( i4_cof, ix_out ), &
      o%w0_cof ( i1_cof, ix_out ), &
      o%w0_cof ( i2_cof, ix_out ), &
      o%w0_cof ( i3_cof, ix_out ), &
      o%w0_cof ( i4_cof, ix_out ), &
      ix_out = 1 , nx_out )
    ! 
    !print'(" bb00 ", &
    !& 1x, i8, 1x, i8, 1x, g12.6 &
    !& )', &
    !( i1_cof, &
    !  o%i0_cof ( i1_cof, 1 ), &
    !  o%w0_cof ( i1_cof, 1 ), &
    !  i1_cof = 1 , o%n0_cof )
    ! 
    return
    !
996 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_create " &
    & /, " output data must increase in order bad index=", i8 &
    & )') &
    ix_err
    !
    go to 999
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_create " &
    & /, " input data must increase in order bad index=", i8 &
    & )') &
    ix_err
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_create " &
    & /, " during memory allocation " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_create " &
    & )') 
    !
    return
    !
  end subroutine interpolate_lagrange_create
  !
  subroutine interpolate_lagrange_i_to_r ( &
                                              o, &
                                              lu_out, rx_inc, &
                                              nx_inp, rx_inp, &
                                              nx_out, x0_out, dx_out, &
                                              i_err &
                                            )
    !
    ! compute lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: x0_out    ! output point  min
    real,        intent(in   ) :: dx_out    ! output point  inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_out    ! output index
    real                       :: rx_out(nx_out) ! output values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_out values
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_lagrange_create ( &
                                       o, &
                                       lu_out, rx_inc, &
                                       nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       i_err &
                                     )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_i_to_r " &
    & )') 
    !
    return
    !
  end subroutine interpolate_lagrange_i_to_r 
  !
  subroutine interpolate_lagrange_r_to_i ( &
                                              o, &
                                              lu_out, rx_inc, &
                                              nx_inp, x0_inp, dx_inp, &
                                              nx_out, rx_out, &
                                              i_err &
                                            )
    !
    ! compute lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: x0_inp    !  input point  min
    real,        intent(in   ) :: dx_inp    !  input point  inc
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! input index
    real                       :: rx_inp(nx_inp) ! input values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    call interpolate_lagrange_create ( &
                                       o, &
                                       lu_out, rx_inc, &
                                       nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       i_err &
                                     )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_r_to_i " &
    & )') 
    !
    return
    !
  end subroutine interpolate_lagrange_r_to_i 
  !
  subroutine interpolate_lagrange_r_to_r ( &
                                              o, &
                                              lu_out, rx_inc, &
                                              nx_inp, x0_inp, dx_inp, &
                                              nx_out, x0_out, dx_out, &
                                              i_err &
                                            )
    !
    ! compute lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! x inp num
    real,        intent(in   ) :: x0_inp    ! x inp min
    real,        intent(in   ) :: dx_inp    ! x inp inc
    !
    integer,     intent(in   ) :: nx_out    ! x out num
    real,        intent(in   ) :: x0_out    ! x out min
    real,        intent(in   ) :: dx_out    ! x out inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! x inp index
    real                       :: rx_inp(nx_inp) ! x inp values
    ! 
    integer                    :: ix_out         ! x out index
    real                       :: rx_out(nx_out) ! x out value 
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_lagrange_create ( &
                                       o, &
                                       lu_out, rx_inc, &
                                       nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       i_err &
                                     )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_r_to_r " &
    & )') 
    !
    return
    !
  end subroutine interpolate_lagrange_r_to_r 
  !
  subroutine interpolate_lagrange_create_ir ( &
                                              o, &
                                              lu_out, rx_inc, &
                                              nx_inp, rx_inp, &
                                              nx_out, x0_out, dx_out, &
                                              i_err &
                                            )
    !
    ! compute lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: x0_out    ! output point  min
    real,        intent(in   ) :: dx_out    ! output point  inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_out    ! output index
    real                       :: rx_out(nx_out) ! output values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_out values
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_lagrange_create ( &
                                       o, &
                                       lu_out, rx_inc, &
                                       nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       i_err &
                                     )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_create_ir " &
    & )') 
    !
    return
    !
  end subroutine interpolate_lagrange_create_ir 
  !
  subroutine interpolate_lagrange_create_ri ( &
                                              o, &
                                              lu_out, rx_inc, &
                                              nx_inp, x0_inp, dx_inp, &
                                              nx_out, rx_out, &
                                              i_err &
                                            )
    !
    ! compute lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: x0_inp    !  input point  min
    real,        intent(in   ) :: dx_inp    !  input point  inc
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! input index
    real                       :: rx_inp(nx_inp) ! input values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    call interpolate_lagrange_create ( &
                                       o, &
                                       lu_out, rx_inc, &
                                       nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       i_err &
                                     )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_create_ri " &
    & )') 
    !
    return
    !
  end subroutine interpolate_lagrange_create_ri 
  !
  subroutine interpolate_lagrange_create_rr ( &
                                              o, &
                                              lu_out, rx_inc, &
                                              nx_inp, x0_inp, dx_inp, &
                                              nx_out, x0_out, dx_out, &
                                              i_err &
                                            )
    !
    ! compute lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! x inp num
    real,        intent(in   ) :: x0_inp    ! x inp min
    real,        intent(in   ) :: dx_inp    ! x inp inc
    !
    integer,     intent(in   ) :: nx_out    ! x out num
    real,        intent(in   ) :: x0_out    ! x out min
    real,        intent(in   ) :: dx_out    ! x out inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! x inp index
    real                       :: rx_inp(nx_inp) ! x inp values
    ! 
    integer                    :: ix_out         ! x out index
    real                       :: rx_out(nx_out) ! x out value 
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_lagrange_create ( &
                                       o, &
                                       lu_out, rx_inc, &
                                       nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       i_err &
                                     )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_lagrange_create_rr " &
    & )') 
    !
    return
    !
  end subroutine interpolate_lagrange_create_rr 
  !
  subroutine interpolate_lagrange_delete ( o )
    !
    !  delete the lagrange interpolation object
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    ! Local variables
    ! 
    ! call memfun_del ( o%rx_inp )
    ! call memfun_del ( o%rx_out )
    call memfun_del ( o%i0_cof )
    call memfun_del ( o%w0_cof )
    ! 
    ! deallocate the object
    ! 
    deallocate ( o )
    !
    return
    !
  end subroutine interpolate_lagrange_delete
  !
  subroutine interpolate_lagrange_apply_r ( o, ry_inp, ry_out )
    !
    !  apply lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    real,        intent(in   ) :: ry_inp(:) ! input  values
    !
    real,        intent(inout) :: ry_out(:) ! output values
    !
    ! Local variables
    ! 

    integer                    :: ix_out    ! output index
    integer                    :: j0_cof    ! interp coef index

    real                       :: ry_tmp(o%nx_inp)
    integer, save                    :: i_call = 0       ! sub call index
    !
    i_call = i_call + 1        ! sub call index
    ! 
    ry_tmp ( 1:o%nx_inp ) = ry_inp ( 1:o%nx_inp )
    ! 
    do_ix_out : do ix_out = 1 , o%nx_out
      ! 
      ry_out ( ix_out ) = 0.
      ! 
      do_j0_cof : do j0_cof = 1 , o%n0_cof
        ! 
        ry_out (                      ix_out ) = &
        ry_out (                      ix_out ) &
      +          o%w0_cof ( j0_cof, ix_out ) &
      * ry_tmp ( o%i0_cof ( j0_cof, ix_out ) ) 
        !
        !if ( ix_out .eq. 1 ) &
        !print'(" bb11",1x,i8,1x,i6,1x,g12.6,1x,g12.6,1x,i6,1x,g12.6)', &
        !i_call, j0_cof, &
        !ry_out(ix_out), ry_inp ( o%i0_cof ( j0_cof, ix_out ) ), &
        !o%i0_cof ( j0_cof, ix_out ), o%w0_cof ( j0_cof, ix_out ) 
        !
      end do do_j0_cof 
      ! 
      !print'(" bb12",1x,i8,1x,i6,1x,g12.6,1x,g12.6)', &
      !i_call, ix_out,ry_out(ix_out), maxval(abs(ry_out( 1:o%nx_out)))
      ! 
    end do do_ix_out 
    ! 
    return
    !
  end subroutine interpolate_lagrange_apply_r 
  !
  subroutine interpolate_lagrange_apply_c ( o, ry_inp, ry_out )
    !
    !  apply lagrange interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_lagrange_struct), pointer  :: o 
    !
    complex,     intent(in   ) :: ry_inp(:) ! input  values
    !
    complex,     intent(inout) :: ry_out(:) ! output values
    !
    ! Local variables
    ! 

    integer                    :: ix_out    ! output index
    integer                    :: j0_cof    ! interp coef index

    complex                    :: ry_tmp(o%nx_inp)
    ! 
    ry_tmp ( 1:o%nx_inp ) = ry_inp ( 1:o%nx_inp )
    ! 
    do_ix_out : do ix_out = 1 , o%nx_out
      ! 
      ry_out ( ix_out ) = cmplx ( 0., 0. )
      ! 
      do_j0_cof : do j0_cof = 1 , o%n0_cof
        ! 
        ry_out (                      ix_out ) = &
        ry_out (                      ix_out ) &
      +          o%w0_cof ( j0_cof, ix_out ) &
      * ry_inp ( o%i0_cof ( j0_cof, ix_out ) ) 
        ! 
      end do do_j0_cof 
      ! 
      !print'(" q12",1x,i6,1x,g12.6,1x,g12.6)',ix_out,ry_out(ix_out),&
      !maxval(abs(ry_out( 1:o%nx_out)))
      ! 
    end do do_ix_out 
    ! 
    return
    !
  end subroutine interpolate_lagrange_apply_c 
  !
  subroutine interpolate_sinc_create ( &
                                       o, &
                                       lu_out, rx_inc, &
                                       nx_inp, rx_inp, &
                                       nx_out, rx_out, &
                                       i_err &
                                     )
    !
    ! compute sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point  values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: j1_cof    ! interp coef index
    integer                    :: j0_cof    ! interp coef index
    integer                    :: i0_cof    ! interp coef index
    integer                    :: i1_cof    ! min interp coef index for output i
    integer                    :: i2_cof    ! max interp coef index for output i
    real                       :: w0_sum    ! interp coef sum
    real                       :: weight    ! interp weight
    integer                    :: ix_err    !  error index
    integer                    :: ix_inp    !  input index
    integer                    :: ix_out    ! output index
    real                       :: rm_inp    ! min  input value to include
    real                       :: rp_inp    ! max  input value to include
    real                       :: x0_inp    ! min  input value
    real                       :: x1_inp    ! max  input value
    real                       :: x0_out    ! min output value
    real                       :: x1_out    ! max output value
    real                       :: dx_inp    ! min  input separation
    real                       :: dx_out    ! max  input separation
    real                       :: dx_ham    ! hamming cosine 
    real                       :: dx_int    ! sinc period
    real                       :: dx_inc    ! sinc inc
    real                       :: dx_min    ! min sinc inc
    real                       :: dx_max    ! max sinc inc
    ! 
    ! intiialize the error flag
    ! 
    i_err = 0
    ! 
    ! allocate the object
    ! 
    allocate ( o )
    ! 
    nullify (o%rx_inp) ! jpa
    nullify (o%rx_out) ! jpa
    nullify (o%i1_inp) ! jpa
    nullify (o%i2_inp) ! jpa
    nullify (o%i0_inp) ! jpa
    nullify (o%n0_inp) ! jpa
    nullify (o%w0_inp) ! jpa
    ! 
    ! copy the input info
    ! 
    ! compute the min and max input and output
    ! 
    x0_inp = minval ( rx_inp ( 1:nx_inp ) )
    x1_inp = maxval ( rx_inp ( 1:nx_inp ) )
    ! 
    x0_out = minval ( rx_out ( 1:nx_out ) )
    x1_out = maxval ( rx_out ( 1:nx_out ) )
    ! 
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " interpolate_sinc_create ", &
    & /, " lu_out=", i8, &
    & /, " rx_inc=" , g12.6 &
    & )') &
    lu_out, rx_inc
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " nx_inp=", i8, " x0_inp=" , g12.6, " x1_inp=", g12.6, &
    & /, " ix_inp  rx_inp " &
    & )') &
    nx_inp, x0_inp, x1_inp
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & 1x, i8, 1x, g12.6 &
    & )') &
    ( ix_inp, rx_inp ( ix_inp ), ix_inp = 1 , nx_inp )
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " nx_out=", i8, " x0_out=" , g12.6, " x1_out=", g12.6, &
    & /, " ix_out  rx_out " &
    & )') &
    nx_out, x0_out, x1_out
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & 1x, i8, 1x, g12.6 &
    & )') &
    ( ix_out, rx_out ( ix_out ), ix_out = 1 , nx_out )
    ! 
    ! make sure the input points are ordered from min to max
    ! 
    dx_inp = rx_inp ( min ( nx_inp , 2 ) ) - rx_inp ( 1 )
    ! 
    do_ix_inp_0 : do ix_inp = 2 , nx_inp
      ! 
      ix_err = ix_inp
      ! 
      if ( rx_inp ( ix_inp ) .ne. rx_inp ( ix_inp - 1 ) ) &
      dx_inp = max ( dx_inp, rx_inp ( ix_inp ) - rx_inp ( ix_inp - 1 ) )
      ! 
      !print'(" s1",1x,i8,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
      !ix_inp, dx_inp, rx_inp(ix_inp)-rx_inp(ix_inp-1), &
      !rx_inp(ix_inp), rx_inp(ix_inp-1)
      ! 
      if ( dx_inp .le. 0. ) go to 997
      ! 
    end do do_ix_inp_0
    ! 
    if ( dx_inp .eq. 0. ) dx_inp = 1.
    ! 
    ! make sure the input points are ordered from min to max
    ! 
    dx_out = rx_out ( min ( nx_out , 2 ) ) - rx_out ( 1 )
    ! 
    do_ix_out_0 : do ix_out = 2 , nx_out
      ! 
      ix_err = ix_out
      ! 
      if ( rx_out ( ix_out ) .ne. rx_out ( ix_out - 1 ) ) &
      dx_out = max ( dx_out, rx_out ( ix_out ) - rx_out ( ix_out - 1 ) )
      ! 
      !print'(" s2",1x,i8,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
      !ix_out, dx_out, rx_out(ix_out)-rx_out(ix_out-1), &
      !rx_out(ix_out), rx_out(ix_out-1)
      ! 
      if ( dx_out .le. 0. ) go to 997
      ! 
    end do do_ix_out_0
    ! 
    if ( dx_out .eq. 0. ) dx_out = 1.
    ! 
    ! allocate struct version of rx_inp
    ! 
    ! call memfun_all ( o%rx_inp, nx_inp, 'rx_inp', i_err )
    ! 
    ! call memfun_all ( o%rx_out, nx_out, 'rx_out', i_err )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    o%lu_out = lu_out
    o%rx_inc = rx_inc
    ! 
    ! o%rx_inp = rx_inp
    o%dx_inp = dx_inp
    o%nx_inp = nx_inp
    o%x0_inp = x0_inp
    o%x1_inp = x1_inp
    ! 
    !o%rx_out = rx_out
    o%dx_out = dx_out
    o%nx_out = nx_out
    o%x0_out = x0_out
    o%x1_out = x1_out
    !
    !  allocate memory for i0_inp, n0_inp, i1_inp, and i2_inp 
    !  each uses nx_out elements
    ! 
    call memfun_all ( o%i1_inp, nx_out, 'i1_inp', i_err )
    call memfun_all ( o%i2_inp, nx_out, 'i2_inp', i_err )
    call memfun_all ( o%n0_inp, nx_out, 'n0_inp', i_err )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    ! for each output node determine the first and last input nodes which
    ! are within rx_inc
    ! 
    ! initialize counters
    ! 
    o%n0_cof = 0                 ! number of interpolation coefficients
    !
    o%n0_inp ( 1:o%nx_out ) = 0  ! interp coef 
    o%i1_inp ( 1:o%nx_out ) = 99999999
    o%i2_inp ( 1:o%nx_out ) = 0
    ! 
    ! for each output point determine the input points which barckett them
    ! 
    do_ix_out_1 : do ix_out = 1 , nx_out
      ! 
      rm_inp = rx_out ( ix_out ) - rx_inc ! min input value to add to output
      ! 
      rp_inp = rx_out ( ix_out ) + rx_inc ! max input value to add to output
      ! 
      do_ix_inp_1 : do ix_inp = 1 , nx_inp
        ! 
        ! if this input point is with the range rm_inp to rp_inp
        ! increment n0_cof by 1
        ! set i1_inp to the min value of i1_inp, ix_inp
        ! set i2_inp to the max value of i2_inp, ix_inp
        ! 
        xxif_include_1 : if ( rx_inp ( ix_inp ) .ge. rm_inp &
                        .and. rx_inp ( ix_inp ) .le. rp_inp ) then 
          ! 
          o%n0_cof = o%n0_cof + 1   ! number of interp coef
          ! 
          o%n0_inp ( ix_out ) = o%n0_inp ( ix_out ) + 1
          ! 
          o%i1_inp ( ix_out ) = min ( o%i1_inp ( ix_out ), ix_inp )
          ! 
          o%i2_inp ( ix_out ) = max ( o%i2_inp ( ix_out ), ix_inp )
          ! 
        end if xxif_include_1 
        ! 
      end do do_ix_inp_1
      ! 
    end do do_ix_out_1 
    ! 
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " interpolate_sinc_create ", &
    & /, " number of interpolation coefficients n0_cof=", i8 &
    & )') &
    o%n0_cof
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & /, " nx_out=", i8, " x0_out=" , g12.6, " x1_out=", g12.6, &
    & " dx_out=", g12.6, " dx_inp=", g12.6, &
    & /, " ix_out  rx_out       rx_inp_1     rx_inp_2      ", &
    & " n0_inp i1_inp i2_inp" &
    & )') &
    nx_out, x0_out, x1_out, dx_out, dx_inp
    !
    if ( pcpsx_i_pel() .eq. 0 .and. lu_out .ge. 0 ) &
    write ( lu_out, ' ( &
    & 1x, i8, 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i6 , 1x, i6, 1x, i6 &
    & )') &
    ( ix_out, rx_out ( ix_out ), &
      rx_inp ( o%i1_inp ( ix_out ) ), rx_inp ( o%i2_inp ( ix_out ) ), &
      o%n0_inp ( ix_out ), &
      o%i1_inp ( ix_out ), o%i2_inp ( ix_out ), &
      ix_out = 1 , nx_out )
    !
    ! allocate memory for interpolation coefficients, w0_inp
    !
    call memfun_all ( o%w0_inp, o%n0_cof, 'w0_inp', i_err )
    ! 
    ! compute the interpolation coefficients
    ! 
    i0_cof = 0
    ! 
    dx_inc = min ( dx_inp, dx_out )
    ! 
    dx_min = pi * rx_inc / dx_inc * 1.E-6     ! min sinc inc
    ! 
    ! dx_min = pi * rx_inc / dx_inc * 1.E-3     ! min sinc inc
    ! 
    dx_max = pi * rx_inc / dx_inc             ! max sinc inc
    ! 
    do_ix_out_2 : do ix_out = 1 , nx_out
      ! 
      rm_inp = rx_out ( ix_out ) - rx_inc ! min input value to add to output
      ! 
      rp_inp = rx_out ( ix_out ) + rx_inc ! max input value to add to output
      ! 
      i1_cof = o%n0_cof + 1     ! min coef this output
      ! 
      i2_cof = 0                  ! max coef this output
      ! 
      w0_sum = 0.                 ! interpolation coef sum
      ! 
      j0_cof = i0_cof
      ! 
      j1_cof = 0
      ! 
      do_ix_inp_2 : do ix_inp = o%i1_inp ( ix_out ) , o%i2_inp ( ix_out ) 
        ! 
        ! compute the interpolation coefficient
        ! 
        dx_int = pi * ( rx_out ( ix_out ) - rx_inp ( ix_inp ) ) / dx_inc
        ! 
        dx_ham = pi * ( rx_out ( ix_out ) - rx_inp ( ix_inp ) ) / rx_inc
        ! 
        xxif_same : if ( abs ( dx_int ) .lt. dx_min ) then
          ! 
          weight = 1.
          ! 
          ! if ( j1_cof .eq. 0 ) &
! print'(" w00 i0=",i8," i=",i8," o=",i8, " x=",g12.6,1x,g12.6,1x,g12.6)',&
! i0_cof, ix_inp, ix_out, dx_int, rx_inp(ix_inp), rx_out(ix_out)
          ! 
          ! if ( j1_cof .eq. 0 ) j1_cof = i0_cof + 1
          ! 
        else if ( abs ( dx_int ) .gt. dx_max ) then
          ! 
          weight = 0.
          ! 
        else xxif_same
          ! 
          weight = ( 0.54 + 0.46 * cos ( dx_ham ) ) * sin ( dx_int ) / dx_int
          ! 
        end if xxif_same
        ! 
        i0_cof = i0_cof + 1   ! index of interp coef
        ! 
        i1_cof = min ( i0_cof, i1_cof ) ! min coef this output point
        ! 
        i2_cof = max ( i0_cof, i2_cof ) ! max coef this output point
        ! 
        o%w0_inp ( i0_cof ) = weight  ! interp coef
        ! 
        w0_sum = w0_sum + weight        ! sum of weights this output sample
        ! 
! if ( ix_out .ge. 618 .and. ix_out .le. 620 ) &
! print'(" q21",1x,i6,1x,i6,1x,i6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
! ix_out,ix_inp,i0_cof,o%w0_inp(i0_cof),w0_sum,dx_int,dx_ham
        ! 
      end do do_ix_inp_2
      ! 
      ! if there is a point very close set the weight to 1 for that value 
      ! 
      ! xxif_j1_cof : if ( j1_cof .ne. 0 ) then
        ! 
      !   ix_inp = o%i1_inp ( ix_out ) + j1_cof - j0_cof
        ! 
      !   print'(" q00 i0=",i8," j1=",i8," i=",i8," o=",i8, " w=", g12.6 )', &
      !   i0_cof, j1_cof, ix_inp, ix_out, o%w0_inp ( j1_cof ) 
        ! 
        ! weight = o%w0_inp ( j1_cof ) 
        ! 
      !   weight = 1.
        ! 
      !   o%w0_inp ( j0_cof+1:i0_cof ) = 0.
        ! 
      !   o%w0_inp ( j1_cof ) = weight 
        ! 
      !   w0_sum = weight        ! sum of weights this output sample
        ! 
      ! end if xxif_j1_cof 
      ! 
      ! normalize the interpolation coefficients for this output point
      ! 
      ! if ( w0_sum .ne. 0. ) &
      ! o%w0_inp ( i1_cof:i2_cof ) = &
      ! o%w0_inp ( i1_cof:i2_cof ) / abs ( w0_sum )
      ! 
      do_ix_inp_3 : do ix_inp = o%i1_inp ( ix_out ) , o%i2_inp ( ix_out ) 
        ! 
        j0_cof = j0_cof + 1   ! index of interp coef
        ! 
        !if ( ix_out .ge. 618 .and. ix_out .le. 620 ) &
        !print'(" q22",1x,i6,1x,i6,1x,i6,1x,g12.6,1x,g12.6)',&
        !ix_out,ix_inp,j0_cof,o%w0_inp(j0_cof),w0_sum
        ! 
      end do do_ix_inp_3
      ! 
    end do do_ix_out_2 
    ! 
    ! make sure we have counted coefficients correctly
    ! 
    if ( i0_cof .ne. o%n0_cof ) go to 995
    ! 
    if ( minval ( o%n0_inp ) .lt. 1 ) goto 994
    ! 
    return
    !
994 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create " &
    & /, " in min number of coefficients n0_cof=", i8 &
    & )') &
    minval ( o%n0_inp )
    !
    go to 999
    !
995 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create " &
    & /, " in number of coefficients n0_cof=", i8, " i0_cof=", i8 &
    & )') &
    o%n0_cof, i0_cof
    !
    go to 999
    !
996 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create " &
    & /, " output data must increase in order bad index=", i8 &
    & )') &
    ix_err
    !
    go to 999
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create " &
    & /, " input data must increase in order bad index=", i8 &
    & )') &
    ix_err
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create " &
    & /, " during memory allocation " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create " &
    & )') 
    !
    return
    !
  end subroutine interpolate_sinc_create
  !
  subroutine interpolate_sinc_i_to_r ( &
                                          o, &
                                          lu_out, rx_inc, &
                                          nx_inp, rx_inp, &
                                          nx_out, x0_out, dx_out, &
                                          i_err &
                                        )
    !
    ! compute sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: x0_out    ! output point  min
    real,        intent(in   ) :: dx_out    ! output point  inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_out    ! output index
    real                       :: rx_out(nx_out) ! output values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_out values
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_sinc_create ( &
                                   o, &
                                   lu_out, rx_inc, &
                                   nx_inp, rx_inp, &
                                   nx_out, rx_out, &
                                   i_err &
                                 )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_i_to_r " &
    & )') 
    !
    return
    !
  end subroutine interpolate_sinc_i_to_r
  !
  subroutine interpolate_sinc_r_to_i ( &
                                          o, &
                                          lu_out, rx_inc, &
                                          nx_inp, x0_inp, dx_inp, &
                                          nx_out, rx_out, &
                                          i_err &
                                        )
    !
    ! compute sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: x0_inp    !  input point  min
    real,        intent(in   ) :: dx_inp    !  input point  inc
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! input index
    real                       :: rx_inp(nx_inp) ! input values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    call interpolate_sinc_create ( &
                                   o, &
                                   lu_out, rx_inc, &
                                   nx_inp, rx_inp, &
                                   nx_out, rx_out, &
                                   i_err &
                                 )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_r_to_i " &
    & )') 
    !
    return
    !
  end subroutine interpolate_sinc_r_to_i
  !
  subroutine interpolate_sinc_r_to_r ( &
                                          o, &
                                          lu_out, rx_inc, &
                                          nx_inp, x0_inp, dx_inp, &
                                          nx_out, x0_out, dx_out, &
                                          i_err &
                                        )
    !
    ! compute sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! x inp num
    real,        intent(in   ) :: x0_inp    ! x inp min
    real,        intent(in   ) :: dx_inp    ! x inp inc
    !
    integer,     intent(in   ) :: nx_out    ! x out num
    real,        intent(in   ) :: x0_out    ! x out min
    real,        intent(in   ) :: dx_out    ! x out inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! x inp index
    real                       :: rx_inp(nx_inp) ! x inp values
    ! 
    integer                    :: ix_out         ! x out index
    real                       :: rx_out(nx_out) ! x out value 
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_sinc_create ( &
                                   o, &
                                   lu_out, rx_inc, &
                                   nx_inp, rx_inp, &
                                   nx_out, rx_out, &
                                   i_err &
                                 )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_r_to_r " &
    & )') 
    !
    return
    !
  end subroutine interpolate_sinc_r_to_r 
  !
  subroutine interpolate_sinc_create_ir ( &
                                          o, &
                                          lu_out, rx_inc, &
                                          nx_inp, rx_inp, &
                                          nx_out, x0_out, dx_out, &
                                          i_err &
                                        )
    !
    ! compute sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: rx_inp(:) ! input point values
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: x0_out    ! output point  min
    real,        intent(in   ) :: dx_out    ! output point  inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_out    ! output index
    real                       :: rx_out(nx_out) ! output values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_out values
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_sinc_create ( &
                                   o, &
                                   lu_out, rx_inc, &
                                   nx_inp, rx_inp, &
                                   nx_out, rx_out, &
                                   i_err &
                                 )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_create_ir " &
    & )') 
    !
    return
    !
  end subroutine interpolate_sinc_create_ir
  !
  subroutine interpolate_sinc_create_ri ( &
                                          o, &
                                          lu_out, rx_inc, &
                                          nx_inp, x0_inp, dx_inp, &
                                          nx_out, rx_out, &
                                          i_err &
                                        )
    !
    ! compute sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! number of  input points
    real,        intent(in   ) :: x0_inp    !  input point  min
    real,        intent(in   ) :: dx_inp    !  input point  inc
    !
    integer,     intent(in   ) :: nx_out    ! output points num
    real,        intent(in   ) :: rx_out(:) ! output point values
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! input index
    real                       :: rx_inp(nx_inp) ! input values
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    call interpolate_sinc_create ( &
                                   o, &
                                   lu_out, rx_inc, &
                                   nx_inp, rx_inp, &
                                   nx_out, rx_out, &
                                   i_err &
                                 )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create_ri " &
    & )') 
    !
    return
    !
  end subroutine interpolate_sinc_create_ri
  !
  subroutine interpolate_sinc_create_rr ( &
                                          o, &
                                          lu_out, rx_inc, &
                                          nx_inp, x0_inp, dx_inp, &
                                          nx_out, x0_out, dx_out, &
                                          i_err &
                                        )
    !
    ! compute sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    integer,     intent(in   ) :: lu_out    ! print unit
    real,        intent(in   ) :: rx_inc    ! one sided interp window width
    !
    integer,     intent(in   ) :: nx_inp    ! x inp num
    real,        intent(in   ) :: x0_inp    ! x inp min
    real,        intent(in   ) :: dx_inp    ! x inp inc
    !
    integer,     intent(in   ) :: nx_out    ! x out num
    real,        intent(in   ) :: x0_out    ! x out min
    real,        intent(in   ) :: dx_out    ! x out inc
    !
    integer,     intent(inout) :: i_err     ! error flag 0=o.k. -1 = error
    ! 
    ! Local variables
    ! 
    integer                    :: ix_inp         ! x inp index
    real                       :: rx_inp(nx_inp) ! x inp values
    ! 
    integer                    :: ix_out         ! x out index
    real                       :: rx_out(nx_out) ! x out value 
    ! 
    ! intialize the error flag
    ! 
    i_err = 0
    ! 
    ! set rx_inp values
    ! 
    do_ix_inp : do ix_inp = 1 , nx_inp
      ! 
      rx_inp ( ix_inp ) = ( ix_inp - 1 ) * dx_inp + x0_inp
      ! 
    end do do_ix_inp 
    ! 
    do_ix_out : do ix_out = 1 , nx_out
      ! 
      rx_out ( ix_out ) = ( ix_out - 1 ) * dx_out + x0_out
      ! 
    end do do_ix_out 
    ! 
    call interpolate_sinc_create ( &
                                   o, &
                                   lu_out, rx_inc, &
                                   nx_inp, rx_inp, &
                                   nx_out, rx_out, &
                                   i_err &
                                 )
    ! 
    if ( i_err .ne. 0 ) go to 999
    ! 
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in interpolate_sinc_create_rr " &
    & )') 
    !
    return
    !
  end subroutine interpolate_sinc_create_rr 
  !
  subroutine interpolate_sinc_delete ( o )
    !
    !  delete the sinc interpolation object
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    ! Local variables
    ! 
    ! call memfun_del ( o%rx_inp )
    ! call memfun_del ( o%rx_out )
    call memfun_del ( o%i1_inp )
    call memfun_del ( o%i2_inp )
    call memfun_del ( o%n0_inp )
    call memfun_del ( o%w0_inp )
    ! 
    ! deallocate the object
    ! 
    deallocate ( o )
    !
    return
    !
  end subroutine interpolate_sinc_delete
  !
  subroutine interpolate_sinc_apply_r ( o, ry_inp, ry_out )
    !
    !  apply sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    real,        intent(in   ) :: ry_inp(:) ! input  values
    !
    real,        intent(inout) :: ry_out(:) ! output values
    !
    ! Local variables
    ! 
    integer                    :: ix_inp    ! input  index
    integer                    :: ix_out    ! output index
    integer                    :: i0_cof    ! interp coef index
    real                       :: ri_max    ! max  input amp
    real                       :: ro_max    ! max output amp
    real                       :: ry_scl    ! scale factor
    real                       :: ry_tmp(o%nx_inp)
    ! 
    i0_cof = 0
    ! 
    ry_tmp ( 1:o%nx_inp ) = ry_inp ( 1:o%nx_inp )
    ! 
    ri_max = maxval ( abs ( ry_tmp ( 1:o%nx_inp ) ) ) 
    ! 
    !ri_max = sum ( abs ( ry_tmp ( 1:o%nx_inp ) ) ) / max ( 1, o%nx_inp )
    ! 
    !print'(" q10 nx_inp=",i8," nx_out=",i8)', o%nx_inp, o%nx_out
    ! 
    do_ix_out : do ix_out = 1 , o%nx_out
      ! 
      ry_out ( ix_out ) = 0.
      ! 
      ry_scl = 0.
      ! 
      do_ix_inp : do ix_inp = o%i1_inp ( ix_out ) , o%i2_inp ( ix_out ) 
        ! 
        i0_cof = i0_cof + 1
        ! 
        ry_out ( ix_out ) = &
        ry_out ( ix_out ) + &
        ry_tmp ( ix_inp ) * &
    o%w0_inp ( i0_cof )
        ! 
       !if ( ix_out .ge. 618 .and. ix_out .le. 620 ) &
       !print'(" q11",1x,i6,1x,i6,1x,i6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
       !ix_out,ix_inp,i0_cof,o%w0_inp(i0_cof),ry_tmp(ix_inp),ry_out(ix_inp),&
       !o%w0_inp ( i0_cof )
        ! 
        ! ry_scl = ry_scl + o%w0_inp ( i0_cof )
        ! 
      end do do_ix_inp
      ! 
      !if ( abs ( ry_scl ) .ge. 1.e-3 ) then
      !  ry_out ( ix_out ) = ry_out ( ix_out ) / ry_scl
      !else 
      !  ry_out ( ix_out ) = 0.
      !end if
      ! 
      !print'(" q12",1x,i6,1x,g12.6,1x,g12.6)',ix_out,ry_out(ix_out),&
      !maxval(abs(ry_out( 1:o%nx_out)))
      ! 
    end do do_ix_out 
    ! 
    !ro_max = sum ( abs ( ry_out ( 1:o%nx_out ) ) ) / max ( 1, o%nx_out )
    ! 
    ro_max = maxval ( abs ( ry_out ( 1:o%nx_out ) ) ) 
    ! 
    ry_scl = 0.
    ! 
    if ( ro_max .ne. 0. ) ry_scl = ri_max / ro_max
    ! 
    ry_out ( 1:o%nx_out ) = ry_out ( 1:o%nx_out ) * ry_scl
    ! 
    return
    !
  end subroutine interpolate_sinc_apply_r 
  !
  subroutine interpolate_sinc_apply_c ( o, ry_inp, ry_out )
    !
    !  apply sinc interpolation coefficients for interpolating
    ! from an input set of points to an output set of points
    !
    type (interpolate_sinc_struct), pointer  :: o 
    !
    complex,     intent(in   ) :: ry_inp(:) ! input  values
    !
    complex,     intent(inout) :: ry_out(:) ! output values
    !
    ! Local variables
    ! 
    integer                    :: ix_inp    ! input  index
    integer                    :: ix_out    ! output index
    integer                    :: i0_cof    ! interp coef index
    real                       :: ri_max    ! max  input amp
    real                       :: ro_max    ! max output amp
    real                       :: ry_scl    ! scale factor
    ! 
    complex                    :: ry_tmp(o%nx_inp)
    ! 
    i0_cof = 0
    ! 
    ry_tmp ( 1:o%nx_inp ) = ry_inp ( 1:o%nx_inp )
    ! 
    ri_max = maxval ( abs ( ry_tmp ( 1:o%nx_inp ) ) ) 
    ! 
    !ri_max = sum ( abs ( ry_tmp ( 1:o%nx_inp ) ) ) / max ( 1, o%nx_inp )
    ! 
    !print'(" q10 nx_inp=",i8," nx_out=",i8)', o%nx_inp, o%nx_out
    ! 
    do_ix_out : do ix_out = 1 , o%nx_out
      ! 
      ry_out ( ix_out ) = 0.
      ! 
      ry_scl = 0.
      ! 
      do_ix_inp : do ix_inp = o%i1_inp ( ix_out ) , o%i2_inp ( ix_out ) 
        ! 
        i0_cof = i0_cof + 1
        ! 
        ry_out ( ix_out ) = &
        ry_out ( ix_out ) + &
        ry_tmp ( ix_inp ) * &
    o%w0_inp ( i0_cof )
        ! 
       !if ( ix_out .ge. 618 .and. ix_out .le. 620 ) &
       !print'(" q11",1x,i6,1x,i6,1x,i6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
       !ix_out,ix_inp,i0_cof,o%w0_inp(i0_cof),ry_tmp(ix_inp),ry_out(ix_inp),&
       !o%w0_inp ( i0_cof )
        ! 
        ! ry_scl = ry_scl + o%w0_inp ( i0_cof )
        ! 
      end do do_ix_inp
      ! 
      !if ( abs ( ry_scl ) .ge. 1.e-3 ) then
      !  ry_out ( ix_out ) = ry_out ( ix_out ) / ry_scl
      !else 
      !  ry_out ( ix_out ) = 0.
      !end if
      ! 
      !print'(" q12",1x,i6,1x,g12.6,1x,g12.6)',ix_out,ry_out(ix_out),&
      !maxval(abs(ry_out( 1:o%nx_out)))
      ! 
    end do do_ix_out 
    ! 
    !ro_max = sum ( abs ( ry_out ( 1:o%nx_out ) ) ) / max ( 1, o%nx_out )
    ! 
    ro_max = maxval ( abs ( ry_out ( 1:o%nx_out ) ) ) 
    ! 
    ry_scl = 0.
    ! 
    if ( ro_max .ne. 0. ) ry_scl = ri_max / ro_max
    ! 
    ry_out ( 1:o%nx_out ) = ry_out ( 1:o%nx_out ) * ry_scl
    ! 
    return
    !
  end subroutine interpolate_sinc_apply_c 
  !
  subroutine interpolate_array_size ( nx, x0, dx, n, x, i_err )
    !
    ! determine the regularity of a set of numbers
    !
    integer,  intent(  out) :: nx
    real,     intent(  out) :: x0
    real,     intent(  out) :: dx
    integer,  intent(in   ) :: n
    integer,  intent(  out) :: i_err
    real,     intent(in   ) :: x (:)
    !
    ! Local variables
    !
    integer :: i

    real    :: x_min, x_max, x_eps
    !
    ! Begin interpolate_array_size
    !
    i_err = 0
    !
    !write(pc_get_lun(), &
    !& '('' interpolate_array_size n='', i8, /, ''  i  x'')') n
    !write(pc_get_lun(), &
    !& '(1x, i8, 1x, g16.9)')(i, x(i), i=1, n)
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
    !"(/, ' interpolate_array_size', "                                    &
    !// " /, ' n    =', i10, ' x_min=', g14.7, ' x_max=', g14.7, "   &
    !// " /, ' nx   =', i10, ' x0   =', g14.7, ' xl   =', g14.7, "   &
    !// " ' dx=', g14.7)")                                           &
    !n, x_min, x_max, nx, x0, (nx-1)*dx+x0, dx
    !
    ! make sure the array is regular
    !
    if ((mod (n, nx) /= 0)    &
       .or. (abs ((nx - 1) * dx + x0 - x_max) > x_eps)) then
      !
      !write ( pc_get_lun(), &
      !"(/, ' error in interpolate_array_size - grid is not regular', " &
      !// " /, ' n    =', i10, ' x_min=', g14.7, ' x_max=', g14.7, "  &
      !// " /, ' nx   =', i10, ' x0   =', g14.7, ' xl   =', g14.7, "  &
      !// " ' dx=', g14.7)") &
      !n, x_min, x_max, nx, x0, (nx - 1) * dx + x0, dx
      !
      i_err = -1
      !
    end if
    !
  end subroutine interpolate_array_size
  !
  subroutine interpolate_fft_test
    !
    ! Test the fft interpoaltion
    ! extrapolation of a frequency slice
    !
    !
    ! Local variables
    !
    integer                                :: i_err   ! err 0=o.k. -1=error
    !
    integer, parameter                     :: mx_inp = 16
    integer                                :: jx_inp
    integer                                :: fx_inp           ! x inp fft
    integer                                :: nx_inp           ! x inp num 
    real                                   :: x0_inp           ! x inp min 
    real                                   :: x1_inp           ! x inp max
    real                                   :: x2_inp           ! x inp max+inc
    real                                   :: dx_inp           ! x inp inc 
    real                                   :: rx_inp(mx_inp)
    real                                   :: ry_inp(mx_inp)
    real                                   :: rz_inp(mx_inp)
    complex                                :: cy_inp(mx_inp)
    complex                                :: cz_inp(mx_inp)
    !
    integer, parameter                     :: mrx_out = 32
    integer                                :: jx_out
    integer                                :: frx_out           ! x out fft
    integer                                :: nx_out           ! x out num 
    real                                   :: x0_out           ! x out min 
    real                                   :: x1_out           ! x out max
    real                                   :: x2_out           ! x out max+inc
    real                                   :: dx_out           ! x out inc 
    integer                                :: lx_inp_out 
    integer                                :: lrx_out_inp 
    real                                   :: rx_inp_out 
    real                                   :: rx_out_inp 
    real                                   :: rx_out(mrx_out)
    real                                   :: ry_out(mrx_out)
    real                                   :: rz_out(mrx_out)
    complex                                :: cy_out(mrx_out)
    complex                                :: cz_out(mrx_out)
    !
    type (interpolate_fft_struct), pointer :: fft_f
    type (interpolate_fft_struct), pointer :: fft_i
    !
    integer, save                    :: i_call = 0       ! sub call index
    !
    i_call = i_call + 1        ! sub call index
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(/, " interpolate_fft_test p=",i4," c=", i8 )', pcpsx_i_pel(), i_call
    !
    i_err = 0
    !
    fx_inp = mx_inp
    nx_inp = fx_inp
    x0_inp = 0.
    dx_inp = 1.
    x1_inp = ( fx_inp - 1 ) * dx_inp + x0_inp
    !
    frx_out = mrx_out
    nx_out = frx_out 
    dx_out = dx_inp * float ( mx_inp ) / float ( mrx_out )
    x0_out = dx_out
    !x0_out = x0_inp + .2 * dx_inp
    !
    x1_out = ( frx_out - 1 ) * dx_out + x0_out
    !
    x2_inp = fx_inp * dx_inp + x0_inp
    !
    x2_out = frx_out * dx_out + x0_out
    !
    rx_inp_out = dx_inp / dx_out
    !
    rx_out_inp = dx_out / dx_inp 
    !
    lx_inp_out = nint ( rx_inp_out )
    !
    lrx_out_inp = nint ( rx_out_inp )
    !
    ! compute the forward interpolation structure
    !
    allocate ( fft_f )
    !
    nullify (fft_f%or_inp) ! jpa
    nullify (fft_f%or_out) ! jpa
    nullify (fft_f%oc_inp) ! jpa
    nullify (fft_f%oc_out) ! jpa
    nullify (fft_i%or_inp) ! jpa
    nullify (fft_i%or_out) ! jpa
    nullify (fft_i%oc_inp) ! jpa
    nullify (fft_i%oc_out) ! jpa
    !
    call interpolate_fft_create ( &
                                  o                = fft_f, &
                                  c_title          = 'interpolate_fft_test f', &
                                  interpolate_real = .true., &
                                  interpolate_cplx = .true., &
                                  ft_inp           = fx_inp, &
                                  nt_inp           = nx_inp, &
                                  t0_inp           = x0_inp, &
                                  dt_inp           = dx_inp, &
                                  ft_out           = frx_out, &
                                  nt_out           = nx_out, &
                                  t0_out           = x0_out, &
                                  dt_out           = dx_out, &
                                  lu_out           = -1, &
                                  i_err            = i_err &
                                )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! compute the forward interpolation structure
    !
    allocate ( fft_i )
    !
    call interpolate_fft_create ( &
                                  o                = fft_i, &
                                  c_title          = 'interpolate_fft_test i', &
                                  interpolate_real = .true., &
                                  interpolate_cplx = .true., &
                                  ft_inp           = frx_out, &
                                  nt_inp           = nx_out, &
                                  t0_inp           = x0_out, &
                                  dt_inp           = dx_out, &
                                  ft_out           = fx_inp, &
                                  nt_out           = nx_inp, &
                                  t0_out           = x0_inp, &
                                  dt_out           = dx_inp, &
                                  lu_out           = -1, &
                                  i_err            = i_err &
                                )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    rx_out = 0
    rx_out = 0
    rx_out = 0
    !
    do_jx_out : do jx_out = 1 , nx_out
      !
      rx_out ( jx_out ) = ( jx_out - 1 ) * dx_out + x0_out
      !
      !ry_out ( jx_out ) = sin ( 2. * pi * ( rx_out ( jx_out ) - x0_out ) &
      !                                  / ( x1_out - x0_out ) )
      !
      !ry_out ( jx_out ) = abs ( rx_out ( jx_out ) - .5*(x0_out+x1_out) )
      !
      ry_out ( jx_out ) = sin ( 2. * pi * ( rx_out ( jx_out ) - x0_out ) &
                                        / ( x2_out - x0_out ) )
      !
      !ry_out ( jx_out ) = 1.
      !
      cy_out ( jx_out ) = cmplx ( ry_out ( jx_out ) , 0. )
      !
    end do do_jx_out 
    !
    rx_inp = 0
    ry_inp = 0
    rz_inp = 0
    !
    do_jx_inp : do jx_inp = 1 , nx_inp
      !
      !rx_inp ( jx_inp ) = rx_out ( (jx_inp-1)*lx_inp_out+1 )
      !
      !ry_inp ( jx_inp ) = ry_out ( (jx_inp-1)*lx_inp_out+1 )
      !
      rx_inp ( jx_inp ) = ( jx_inp - 1 ) * dx_inp + x0_inp
      !
      !ry_inp ( jx_inp ) = sin ( 2. * pi * ( rx_inp ( jx_inp ) - x0_inp ) &
      !                                  / ( x1_inp - x0_inp + dx_out ) )
      !
      !ry_inp ( jx_inp ) = abs ( rx_inp ( jx_inp ) - .5*(x0_out+x1_out) )
      !
      ry_inp ( jx_inp ) = sin ( 2. * pi * ( rx_inp ( jx_inp ) - x0_out ) &
                                        / ( x2_out - x0_out ) )
      !
      !ry_inp ( jx_inp ) = 1.
      !
      cy_inp ( jx_inp ) = cmplx ( ry_inp ( jx_inp ) , 0. )
      !
    end do do_jx_inp 
    !
    ! interpolate from ry_inp into rz_out using fft_f
    !
    call interpolate_fft_apply ( fft_f, ry_inp, rz_out )
    !
    call interpolate_fft_apply ( fft_f, cy_inp, cz_out )
    !
    ! interpolate from rz_out into rz_inp using fft_i
    !
    call interpolate_fft_apply ( fft_i, rz_out, rz_inp )
    !
    call interpolate_fft_apply ( fft_i, cz_out, cz_inp )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(" nx_inp=",i8," x0_inp=",g12.6," x1_inp=",g12.5," dx_inp=",g12.6)', &
    nx_inp, x0_inp, x1_inp, dx_inp
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(" nx_out=",i8," x0_out=",g12.6," x1_out=",g12.5," dx_out=",g12.6)', &
    nx_out, x0_out, x1_out, dx_out
    !
    if ( pcpsx_i_pel() .eq. -1 ) &
    print'(" rx_inp ", 1x,i8,1x,g12.6,1x,g12.6,1x,g12.6 )', &
    ( jx_inp, rx_inp(jx_inp), ry_inp(jx_inp), rz_inp(jx_inp), &
      jx_inp=1,nx_inp )
    !
    if ( pcpsx_i_pel() .eq. -1 ) &
    print'(" rx_out ", 1x,i8,1x,g12.6,1x,g12.6,1x,g12.6 )', &
    ( jx_out, rx_out(jx_out), ry_out(jx_out), rz_out(jx_out), &
      jx_out=1,nx_out )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(" cx_inp ", 1x,i8, &
    & 1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)', &
    ( jx_inp, rx_inp(jx_inp), cy_inp(jx_inp), cz_inp(jx_inp), &
      jx_inp=1,nx_inp )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(" cx_out ", 1x,i8, &
    & 1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)', &
    ( jx_out, rx_out(jx_out), cy_out(jx_out), cz_out(jx_out), &
      jx_out=1,nx_out )
    !
    if ( pcpsx_i_pel() .eq. -1 ) &
    print'(" rx_yyy ", 1x,i8, 1x,i8, &
    & 1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)', &
    ( jx_inp, (jx_inp-1)*lx_inp_out+1, & 
      rx_inp(jx_inp), rx_out((jx_inp-1)*lx_inp_out+1), &
      ry_inp(jx_inp), ry_out((jx_inp-1)*lx_inp_out+1), &
      rz_inp(jx_inp), rz_out((jx_inp-1)*lx_inp_out+1), &
      jx_inp=1,nx_inp )
    !
    if ( i_err .ne. -999 ) stop
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /," error in interpolate_fft_test ", &
    & " during interpolate_fft_create i " &
    & )') 
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /," error in interpolate_fft_test ", &
    & " during interpolate_fft_create f " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /," error in interpolate_fft_test " &
    & )')
    !
    i_err = -1
    !
    stop
    !
  end subroutine interpolate_fft_test
  !
  subroutine interpolate_fft_rc_slow ( &
    sign_fft, scale_fft, nt_fft, dt_fft, a0_inp, a0_out )
    !
    ! Arguments
    !
    integer,                 intent(in   ) :: sign_fft   ! fft sign
    real,                    intent(in   ) :: scale_fft  ! fft scale
    integer,                 intent(in   ) :: nt_fft     ! fft len
    real,                    intent(in   ) :: dt_fft     ! fft inc
    real,                    intent(in   ) :: a0_inp (:) ! input trace
    complex,                 intent(inout) :: a0_out (:) ! output trace
    !
    ! Local variables
    !
    integer                                :: nf_nyq
    integer                                :: nf_fft
    real                                   :: df_fft

    !
    integer                                :: jf_fft
    real                                   :: rf_fft
    !
    integer                                :: jt_fft
    real                                   :: rt_fft
    real                                   :: r_phase

    !
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    !
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !print'(/, " fft_rc_slown p=",i4, " c=", i8)', &
    !pcpsx_i_pel(), i_call
    !
    ! Begin interpolate_fft_apply_r
    !
    ! copy the local parameters to structure parameters
    !
    nf_fft = nt_fft 
    !
    nf_nyq = nt_fft / 2 + 1            ! nyquist index
    !
    !rf_nyq = 1. / ( 2. * dt_fft ) ! nyquist frequency in cycles / sec
    !
    !df_fft = rf_nyq / ( nf_fft - 1 )   ! frequency increment
    !
    df_fft = 1. / ( nt_fft * dt_fft )
    !
    a0_out ( : ) = cmplx ( 0., 0. )
    !
    do_jf_fft : do jf_fft = 1 , nf_nyq
      !
      rf_fft = ( jf_fft - 1 ) * df_fft
      !
      do_jt_fft : do jt_fft = 1 , nt_fft
        !
        rt_fft = ( jt_fft - 1 ) * dt_fft
        !
        r_phase = sign_fft * rf_fft * rt_fft * 2. * pi
        !
        a0_out ( jf_fft ) = &
        a0_out ( jf_fft ) &
      + a0_inp ( jt_fft ) * cexp ( cmplx ( 0., + r_phase ) )
        !
      end do do_jt_fft 
      !
      a0_out ( jf_fft ) = &
      a0_out ( jf_fft ) * scale_fft
      !
    end do do_jf_fft 
    !
    return
    !
  end subroutine interpolate_fft_rc_slow 
  !
  subroutine interpolate_fft_cr_slow ( &
    sign_fft, scale_fft, nt_fft, dt_fft, a0_inp, a0_out )
    !
    ! Arguments
    !
    integer,                 intent(in   ) :: sign_fft   ! fft sign
    real,                    intent(in   ) :: scale_fft  ! fft scale
    integer,                 intent(in   ) :: nt_fft     ! fft len
    real,                    intent(in   ) :: dt_fft     ! fft inc
    complex,                 intent(in   ) :: a0_inp (:) ! output trace
    real,                    intent(inout) :: a0_out (:) ! input trace
    !
    ! Local variables
    !
    integer                                :: nf_nyq
    integer                                :: nf_fft
    real                                   :: df_fft

    !
    integer                                :: jf_fft
    real                                   :: rf_fft
    !
    integer                                :: jt_fft
    real                                   :: rt_fft
    real                                   :: r_phase

    !
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    !
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(/, " fft_cr_slow p=",i4, " c=", i8)', pcpsx_i_pel(), i_call
    !
    ! Begin interpolate_fft_apply_r
    !
    ! copy the local parameters to structure parameters
    !
    nf_fft = nt_fft 
    !
    nf_nyq = nt_fft / 2 + 1            ! nyquist index
    !
    df_fft = 1. / ( nt_fft * dt_fft )
    !
    a0_out ( : ) = 0. 
    !
    do_jt_fft : do jt_fft = 1 , nt_fft
      !
      rt_fft = ( jt_fft - 1 ) * dt_fft
      !
      do_jf_fft : do jf_fft = 1 , nf_nyq
        !
        rf_fft = ( jf_fft - 1 ) * df_fft
        !
        r_phase = sign_fft * rf_fft * rt_fft * 2. * pi
        !
        a0_out ( jt_fft ) = &
        a0_out ( jt_fft ) &
      +         a0_inp ( jf_fft )   * cexp ( cmplx ( 0., + r_phase ) ) 
        !
        if ( jf_fft .gt. 1 .and. jf_fft .lt. nf_nyq ) &
        a0_out ( jt_fft ) = &
        a0_out ( jt_fft ) &
      + conjg ( a0_inp ( jf_fft ) ) * cexp ( cmplx ( 0., - r_phase ) ) 
        !
      end do do_jf_fft 
      !
      a0_out ( jt_fft ) = &
      a0_out ( jt_fft ) * scale_fft 
      !
      !print'(" fft_cr_slow jt_fft=",i8," a0_out=",g12.6)', &
      !jt_fft, a0_out ( jt_fft ) 
      !
    end do do_jt_fft 
    !
    return
    !
  end subroutine interpolate_fft_cr_slow 
  !
  subroutine interpolate_fft_cc_slow ( &
    sign_fft, scale_fft, nt_fft, dt_fft, a0_inp, a0_out )
    !
    ! Arguments
    !
    integer,                 intent(in   ) :: sign_fft   ! fft sign
    real,                    intent(in   ) :: scale_fft  ! fft scale
    integer,                 intent(in   ) :: nt_fft     ! fft len
    real,                    intent(in   ) :: dt_fft     ! fft inc
    complex,                 intent(in   ) :: a0_inp (:) ! output trace
    complex,                 intent(inout) :: a0_out (:) ! input trace
    !
    ! Local variables
    !
    integer                                :: nf_fft
    integer                                :: nf_nyq
    real                                   :: df_fft

    !
    integer                                :: jf_fft
    real                                   :: rf_fft
    !
    integer                                :: jt_fft
    real                                   :: rt_fft
    real                                   :: r_phase

    !
    integer, save              :: i_call = 0
    ! 
    i_call = i_call + 1
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(/, " fft_cc_slow p=",i4, " c=", i8)', pcpsx_i_pel(), i_call
    !
    ! Begin interpolate_fft_apply_r
    !
    ! copy the local parameters to structure parameters
    !
    nf_fft = nt_fft 
    !
    nf_nyq = nt_fft / 2 + 1 ! nyquist index
    !
    df_fft = 1. / ( nt_fft * dt_fft )
    !
    a0_out ( : ) = cmplx ( 0., 0. )
    !
    do_jt_fft : do jt_fft = 1 , nt_fft
      !
      rt_fft = ( jt_fft - 1 ) * dt_fft
      !
      do_jf_fft : do jf_fft = 1 , nf_nyq
        !
        rf_fft = ( jf_fft - 1 ) * df_fft
        !
        r_phase = sign_fft * rf_fft * rt_fft * 2. * pi
        !
        a0_out ( jt_fft ) = &
        a0_out ( jt_fft ) &
      + a0_inp ( jf_fft ) * cexp ( cmplx ( 0., + r_phase ) ) 
        !
        if ( jf_fft .gt. 1 .and. jf_fft .lt. nf_nyq ) &
        a0_out ( jt_fft ) = &
        a0_out ( jt_fft ) &
      + a0_inp ( nt_fft - jf_fft + 2 ) * cexp ( cmplx ( 0., - r_phase ) ) 
        !
      end do do_jf_fft 
      !
      a0_out ( jt_fft ) = &
      a0_out ( jt_fft ) * scale_fft
      !
    end do do_jt_fft 
    !
    return
    !
  end subroutine interpolate_fft_cc_slow 
  !
end module interpolate_module
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
