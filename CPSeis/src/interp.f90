!<CPS_v1 type="PRIMITIVE"/> 

!!--------------------------- interp.f90 --------------------------------!! 
!!--------------------------- interp.f90 --------------------------------!! 
!!--------------------------- interp.f90 --------------------------------!! 
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
! Name       : interp
! Category   : math 
! Written    : 1986-07-23   by: JB Sinton 
! Revised    : 2006-06-05   by: B. Menger
! Maturity   : production
! Purpose    : Interpolate function 1D to 1D, 3D to 1D or 1D regular grid 
! Portability: No known limitations. 
! 
!-------------------------------------------------------------------------------
!</brief_doc> 

!<descript_doc> 
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                    
! 
! 
!  1. interp_1d_var_lin_real: Interpolate a 1D function Y(X) to a new 1D 
!     function YP(XP). This is a linear interpolation. Y(X) can have 
!     a variable sample rate.  YP(XP) is evenly sampled.        
!                        
!  2. interp_2d_var_lin_real: Interpolate a 3D function TAB(Z,XB,YB) to 
!     a 1D function F(Z,X=XF,Y=YF).   This entry point is for bi- 
!     linearly interpolating REAL functions.  TAB(Z,XB,YB) can be have 
!     a variable sample rate in XB and YB.  Interpolation is per- 
!     formed only on the variables XB and YB. 
! 
!  3. interp_2d_var_lin_int: Interpolate a 3D function ITAB(Z,XB,YB) to 
!     a 1D function IF(Z,X=XF,Y=YF). This entry point is for bi- 
!     linearly interpolating INTEGER functions.  ITAB(Z,XB,YB) can have 
!     a variable sample rate in XB and YB.  Interpolation is 
!     performed only on the variables XB and YB. 
! 
!  4. interp_1d_con_lin_real: Linearly interpolate constant sample rate
!     1D data by a constant stride. Both input and output functions must 
!     have a constant sample rate. 
! 
!  5. interp_1d_con_bw_real: Interpolate evenly sampled 1D data using 
!     a 5 point Bracewell convolutional operator.  This interpolates 
!     using a cubic spline approximation to a sinc function.  Both input 
!     and output functions must have constant sample rates. 
!
!  6. interp_2d_var_real: Function to interpolate a 2D function TAB(XB,YB), 
!     returning point function F at (X=XF,Y=YF).  This function will bi-linearly
!     interpolate the REAL functions.  TAB (XB, YB) can have a variable
!     sample rate in XB and YB. Interpolation is performed on the 
!     variables XB and YB. interp_2d_var_real is a degenerate case for 
!     interp_2d_var_lin_real where that procedure's nr = 1.
!
!  7. interp_1d_lin:  Utility function to interpolate a point along
!     a 1D list of values.
!
!  8. interp_2d_var_lin_grid:  Subroutine to interpolate window begin
!     (WIN_TIM_BEG) and end (WIN_TIM_END) times for spatially variant
!     trace window definition. This is a variant of the (old system)
!     int2d and interp_2d_var_lin_real.
! 
!  9. interp_1d_con_lin_real2_init: Prepare constant-stride arrays for 
!     interp_1d_con_lin_real2.  These values are identical to G and H
!     from interp_1d_con_lin_real with IFLG set to -1 or 0.
!
! 10. interp_1d_con_lin_real2: Linearly interpolate constant sample rate
!     1D data by a constant stride. Both input and output functions must 
!     have a constant sample rate. Similar to interp_1d_con_lin_real with
!     IFLG = +1.  G and H arrays must be defined, as by calling 
!     interp_1d_con_lin_real2_init.
!
! 11. interp_1d_con_bw_real2_init: Prepare Bracewell function for 
!     interp_1d_con_bw_real2.  These values are identical to B
!     from interp_1d_con_bw_real with IFLG set to -1 or 0.
!
! 12. interp_1d_con_bw_real2: Interpolate evenly sampled 1D data using 
!     a 5 point Bracewell convolutional operator.  This interpolates 
!     using a cubic spline approximation to a sinc function.  Both input 
!     and output functions must have constant sample rates.  Similar to
!     interp_1d_con_bw_real with IFLG set to -1 or 0.  B array muset be 
!     defined, as by calling interp_1d_con_bw_real2_init.
!
! 
!-------------------------------------------------------------------------------
!</descript_doc> 

!<calling_doc> 
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                       
! 
! For each subroutine or function documented below, each argument is 
! flagged as follows: 
!      i = value required upon INPUT. 
!      o = value set by the routine upon OUTPUT. 
!      b = value BOTH required upon input and changed upon output. 
! 
!                               i  i  i  o   o   i   i   i 
!  CALL interp_1d_var_lin_real (X, Y, N, XP, YP, NP, XS, XE)
!    1D variable increment linear interpolation of reals. 
!    Arguments: 
!    X      - Input array of X coordinates for Y. 
!    Y      - Input array of values to interpolate with. 
!    N      - # of values in X and Y. 
!             NOTE: X must be ordered from the smallest to largest value. 
!    XP     - Output array of X coordinates for YP. 
!    YP     - Output array of interpolate values. 
!    NP     - # of values in XP and YP. 
!    XS     - Starting XP value. 
!    XE     - Ending XP value. 
! 
!                                      opt opt
!                               i   i   i   i    i   i   i   i   o 
!  CALL interp_2d_var_lin_real (XB, YB, NX, NY, TAB, NR, XF, YF, F)
!    2D variable increment Bi-linear interpolation of reals. 
!    Arguments: 
!    XB     - Input array of X coordinates for TAB. 
!    YB     - Input array of Y coordinates for TAB. 
!    NX     - # of values in XB. 
!    NY     - # of values in YB. 
!    TAB    - Real 3D matrix of values to interpolate with. 
!             Dimensioned to NR*NX*NY. 
!    NR     - # of output values in F.                                   
!    XF     - Input X coordinate to interpolate TAB to. 
!    YF     - Input Y coordinate to interpolate TAB to. 
!    F      - Output array of interpolated values (size=NR). 
! 
!                              i   i   i   i    i    i   i   i   o 
!  CALL interp_2d_var_lin_int (XB, YB, NX, NY, ITAB, NI, XF, YF, IF)
!    2D variable increment Bi-linear interpolation of integers. 
!    Arguments: 
!    XB     - Input array of X coordinates for ITAB. 
!    YB     - Input array of Y coordinates for ITAB. 
!    NX     - # of values in XB. 
!    NY     - # of values in YB. 
!    ITAB   - Integer 3D matrix of values to interpolate with. 
!             Dimensioned to NR*NX*NY. 
!    NI     - # of output values in IF. 
!    XF     - Input X coordinate to interpolate ITAB to. 
!    YF     - Input Y coordinate to interpolate ITAB to. 
!    IF     - Output array of interpolated values. 
! 
!                                                     opt  opt
!                                 i    i    i    b  b  i    o 
!  CALL interp_1d_con_lin_real (INTRP, NT, IFLG, H, G, TR, ETR)
!    1D constant increment linear interpolation of reals. 
!    Arguments: 
!    INTRP  - Interpolation factor, eg. =4 then compute NT*4 values. 
!    NT     - # of values in array TR. 
!    IFLG   - Processing switch. 
!             =-1, then set up H and G and return. 
!             = 0, then set up H and G, interpolate TR and return. 
!             = 1, then use previous H and G to interpolate TR. 
!    H      - Work array, do not alter. Must be dimensioned INTRP. 
!    G      - Work array, do not alter. Must be dimensioned INTRP. 
!    TR     - Input array of values. ==>Dimensioned to NT+1.<== 
!    ETR    - Output array of interpolated values (size=INTRP*NT). 
! 
!  CALL interp_1d_con_lin_real2 (INTRP, NDPT, H, G, TR, ETR)
!    1D constant increment linear interpolation of reals. 
!    Use H and G to interpolate TR into ETR.
!    Arguments: 
!    INTRP   - Interpolation factor, eg. =4 then compute NT*4 values. 
!    NDPT    - # of values in array TR. 
!    H (:)   - Increasing linear sequence. Must be dimensioned INTRP. 
!              Use interp_1d_con_lin_real2_init to calculate.
!    G (:)   - Decreasing linear sequence. Must be dimensioned INTRP. 
!              Use interp_1d_con_lin_real2_init to calculate.
!              H (n) + G(n) = 1.0, H (n) > 0.0, G (n) > 0.0, for n = 1, INTRP
!    TR (:)  - Input array of values.  
!    ETR (:) - Output array of interpolated values, size=(INTRP * NDPT). 
! 
!                                     i      o  o 
!  CALL interp_1d_con_lin_real2_init (INTRP, H, G)
!    Calculate 1D constant increment sequence for interp_1d_con_lin_real2. 
!    Arguments: 
!    INTRP  - Interpolation factor, eg. =4 then compute NT*4 values. 
!    H (:)  - Increasing linear sequence. Must be dimensioned INTRP. 
!    G (:)  - Decreasing linear sequence. Must be dimensioned INTRP. 
!
!                                                  opt  opt
!                                i    i    i    b  i     o 
!  CALL interp_1d_con_bw_real (INTRP, NT, IFLG, B, TR,  ETR)
!    1D constant increment Bracewell interpolation of reals. 
!    Arguments: 
!    INTRP  - Interpolation factor, eg. =4 then compute NT*4 values. 
!    NT     - # of values in array TR. 
!    IFLG   - Processing switch. 
!             =-1, then set up B and return. 
!             = 0, then set up B, interpolate TR and return. 
!             = 1, then use previous B to interpolate TR. 
!    B      - Work array, do not alter. Must be dimensioned INTRP*5. 
!    TR     - Input array of values. 
!    ETR    - Output array of interpolated values (size=INTRP*NT). 
! 
!                                    i      o 
!  CALL interp_1d_con_bw_real2_init (INTRP, B)
!    Setup constant increment Bracewell interpolation table for use with
!    interp_1d_con_bw_real2. 
!    Arguments: 
!    INTRP  - Interpolation factor, eg. =4 then compute NT*4 values. 
!    B      - Work array, do not alter. Must be dimensioned INTRP*5. 
! 
!                               i      i   i  i     o 
!  CALL interp_1d_con_bw_real2 (INTRP, NT, B, TR, ETR)
!    1D constant increment Bracewell interpolation of reals.   Requites
!    B table to be setup by interp_1d_con_bw_real2_init.
!    Arguments: 
!    INTRP  - Interpolation factor, eg. =4 then compute NT*4 values. 
!    NT     - # of values in array TR. 
!    B      - Work array, do not alter. Must be dimensioned INTRP*5. 
!    TR     - Input array of values. 
!    ETR    - Output array of interpolated values (size=INTRP*NT). 
! 
!                                                opt  opt
!                              i    i    i    b   i    o 
!  CALL interp_1d_con_bw_cpx(INTRP, NT, IFLG, B, CTR, CETR)
!    1D constant increment Bracewell interpolation of complex. 
!    Arguments: 
!    INTRP  - Interpolation factor, eg. =4 then compute NT*4 values. 
!    NT     - # of values in array TR. 
!    IFLG   - Processing switch. 
!             =-1, then set up H and G and return. 
!             = 0, then set up H and G, interpolate TR and return. 
!             = 1, then use previous H and G to interpolate TR. 
!    B      - Work array, do not alter. Must be dimensioned INTRP*5. 
!    CTR    - Complex input array of values.  ==>Dimensioned to NT+2.<== 
!    CETR   - Complex output array of interpolated values (size=INTRP*NT). 
!
!                                  opt opt
!                           i   i   i   i   i    i   i 
!  F = interp_2d_var_real (XB, YB, NX, NY, TAB, XF, YF)
!    2D variable increment Bi-linear interpolation of reals. 
!    Arguments: 
!    XB     - Input array of X coordinates for TAB. 
!    YB     - Input array of Y coordinates for TAB. 
!    NX     - # of values in XB. 
!    NY     - # of values in YB. 
!    Z      - Data
!    TAB    - Real 2D matrix of values to interpolate with. 
!             Dimensioned to NX*NY. 
!    XF     - Input X coordinate to interpolate TAB to. 
!    YF     - Input Y coordinate to interpolate TAB to. 
!    F      - Output interpolated value. 
!
!                      i  i  o   o   o 
!  call interp_1d_lin (b, f, i1, i2, fac)
!    b      - Input array of X coordinates for TAB. 
!    f      - Input coordinate to interpolate b to. 
!    i1     - b index before f. 
!    i2     - b index after f. 
!    fac    - Interpolation factor for f, with relation to b(i1). 
!
!                               i     i        i     i
!  call interp_2d_var_lin_grid (ninl, inlines, ncrl, crosslines,    &
!
!                               i        i
!                               beg_tab, end_tab,                   &
!
!                               i    i           i
!                               ntr, inline_pos, crossline_pos,     &
!
!                               o        o
!                               beg_tim, end_tim
!
!    ninl          - Length of inlines array
!    inlines       - Real array of inline positions (increasing order)
!    ncrl          - Length of crosslines array
!    crosslines    - Real array of crossline positions (increasing order)
!    beg_tab       - WIN_TIM_BEG table of trace starts, by inline x crossline
!                    position.  Real 2D array.
!    end_tab       - WIN_TIM_END table of trace end times. Real 2D array,
!                    same shape as WIN_TIM_BEG, ninl x ncrl.
!    ntr           - Number of traces
!    inline_pos    - Real array of inline positions for each trace
!    crossline_pos - Real array of crossline positions for each trace.
!                    inline_pos and crossline_pos entries correspond to 
!                    the same traces.  Same length as inline_pos, ntr.
!    beg_tim       - Output real array, containing trace start time
!                    for each trace.
!    end_tim       - Output real array, containing trace end time for 
!                    each trace.
!
!  example:
!    call interp_2d_var_lin_grid (ninl          = obj%n_inlines,               &
!                                 inlines       = obj%inlines,                 &
!                                 ncrl          = obj%n_crosslines,            &
!                                 crosslines    = obj%crosslines,              &
!                                 beg_tab       = obj%win_tim_beg,             &
!                                 end_tab       = obj%win_tim_end,             &
!                                 ntr           = ntr,                         &
!                                 inline_pos    = real(hd (obj%hdr_inl, :ntr)),&
!                                 crossline_pos = real(hd (obj%hdr_crl, :ntr)),&
!                                 beg_tim       = tmin_list,                   &
!                                 end_tim       = tmax_list)
! 
!                                 i     i     i  i  i   o 
! 
!-------------------------------------------------------------------------------
!</calling_doc> 

!<history_doc> 
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                      
! 
!     Date        Author       Description 
!     ----        ------       ----------- 
! 17. 2006-06-05  B. Menger    Removed Unused Variables.
! 16. 2001-06-11  Chiu         Corrected the endpoint problem  
!                              in interp_1d_con_bw_real. PRODUCTION.
! 15. 2001-04-04  Selzler      Corrected hist_doc tag to be history_doc
! 14. 2000-06-16  Brad Kruse   Updated Maturity from raw to production
! 13. 2000-01-27  Kruse        Corrected degenerate case when interp_1d_lin
!.                             is called with a 1d array of length = 1.
!.                             Added interp_1d_con_bw_real2 and 
!.                             interp_1d_con_bw_real2_init as alternatives to
!.                             interp_1d_con_bw_real (was INTBRC), to 
!.                             improve array bounds checking.
!.                             Added interp_1d_con_lin_real2 and 
!.                             interp_1d_con_lin_real2_init as alternatives to
!.                             interp_1d_con_lin_real (was INTERP), to 
!.                             improve array bounds checking.
!.                             Corrected interp_1d_con_bw_real to remove need
!.                             to dimension TR as 1:NT+2, where NT = NDPT.
!.                             Corrected interp_1d_con_lin_real to remove need
!.                             to dimension TR as 1:NT+1, where NT = NDPT.
!.                             Extra TR elements are no longer needed.
! 12. 1999-12-07  Kruse        Added interp_2d_var_lin_grid to support
!                              OPT_WIN='GRID' in process window definitions
! 11. 1999-11-29  Kruse        Restored previous interface to 
!                              interp_2d_var_lin_real.
! 10. 1999-11-18  Kruse        Modified interp_2d_var_lin_real to use new
!                              interp_1d_lin simple interpolate procedure,
!                              and added interp_2d_var_real, a scalar version
!                              of interp_2d_var_lin_real, which also uses 
!                              the new interp_1d_lin
!  9. 1999-08-12  Selzler      Conversion to fortran 90 compiler. 
!                              rename INTP1D -> interp_1d_var_lin_real
!                              rename INT2DR -> interp_2d_var_lin_real
!                              rename INT2DI -> interp_2d_var_lin_int
!                              rename INTERP -> interp_1d_con_lin_real
!                              rename INTBRC -> interp_1d_con_bw_real
!                              rename CINTBRC -> interp_1d_con_bw_cpx
!  8. 1999-01-11  Goodger      Begin using the fortran90 compiler. 
!  7. 1988-06-18  B Baumel     Make interp_1d_var_lin_real more efficient. 
!  6. 1988-05-17  JB Sinton    Improved efficiency of interp_1d_var_lin_real
!                              (see notes). 
!  5. 1987-12-10  JB Sinton    Corrected Bracewell interpolators. 
!  4. 1987-12-10  DW Hanson    add interp_1d_con_bw_cpx entry for complex
!  3. 1987-11-11  Tom Hill and B Baumel  Improve efficiency of
!                              interp_2d_var_lin_real and int entries.
!  2. 86/10/29; B  Baumel      Put in correct version of bilinear
!                              interpolation; correct documentation for
!                              interp_2d_var_lin_real, interp_2d_var_lin_int. 
!  1. 86/07/23; JB Sinton; Add Bracewell interpolation. 

!-------------------------------------------------------------------------------
!</history_doc> 

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! 
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<programming_doc> 
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES                     
! 
! 
!  1. These routines do not call any other routines. 
!  2. To use the entry points interp_1d_var_lin_real, interp_2d_var_lin_real,
!     and interp_2d_var_lin_int the user must 
!     build the input function, Y, TAB, and ITAB first.  Then each time 
!     call one of these routines each time an interpolated 1D output 
!     function is desired.  The arrays TAB and ITAB are set up so that 
!     the Z coordinate index comes first, then the X coordinate, and 
!     last the Y coordinate. See diagram below. 
!                                           Y 
!                  +----+-----+--+-------+  1 
!                  +----+-----+--+-------+  2 
!                  |    |     |  |       |  | 
!                  +----+-----+--*-------*  3 
!                  |    |     |  |       |  | 
!                  |    |     |  |   .   |  | 
!                  |    |     |  |       |  | 
!                  +----+-----+--*-------*  4 
!             X----1----2-----3--4-------5--+--> 
!                                           | 
!                                           V 
! 
!     Below each "+" hangs NR values on a even grid in the Z coor- 
!     dinate.  Note that the X/Y grid can be rectangular. 
!     If the "." represents the position where you need to inter- 
!     polate (I)TAB to then interp_2d_var_lin_real(I) will use the values
!     of (I)TAB at the "*"'s to complete the bi-linear interpolation.
!     At each Z-coordinate level 4 values will be used to obtain the 
!     corresponding value at the "." position. 
!     
!  3. To use the entry points interp_1d_con_lin_real and interp_1d_con_bw_real
!     the user must initially call either routine with IFLG=-1 or =0.  
!     This will set up the working arrays for these interpolators.  
!     After the first call these routines can be call again with the 
!     same value of INTRP without setting up the work arrays.  
!     The work arrays should NOT be altered any time after they have 
!     been set up. 
!
!     An alternative to interp_1d_con_lin_real is to call
!     interp_1d_con_lin_real2_init to initialize the H and G sequences, 
!     then call interp_1d_con_lin_real2 (or interp_1d_con_lin_real
!     with IFLG = 0) to perform interpolation.
! 
!     An alternative to interp_1d_con_bw_real is to call
!     interp_1d_con_bw_real2_init to initialize the H and G sequences, 
!     then call interp_1d_con_bw_real2 (or interp_1d_con_bw_real
!     with IFLG = 0) to perform interpolation.
! 
!-------------------------------------------------------------------------------
!</programming_doc> 

!!-------------------------- start of module ------------------------------!! 
!!-------------------------- start of module ------------------------------!! 
!!-------------------------- start of module ------------------------------!! 

      module interp_module 
      implicit none 

      private 
      public :: interp_1d_var_lin_real 
      public :: interp_2d_var_lin_real 
      public :: interp_2d_var_lin_int 
      public :: interp_1d_con_lin_real 
      public :: interp_1d_con_lin_real2
      public :: interp_1d_con_lin_real2_init
      public :: interp_1d_con_bw_real 
      public :: interp_1d_con_bw_real2
      public :: interp_1d_con_bw_real2_init
      public :: interp_1d_con_bw_cpx 
      public :: interp_2d_var_real
      public :: interp_2d_var_lin_grid
      public :: interp_1d_lin

      character(len=100),public,save :: interp_ident = &
       '$Id: interp.f90,v 1.17 2006/06/05 13:19:07 Menger prod sps $'

      contains 

!!------------------ interp_1d_var_lin_real -------------------------------!! 
!!------------------ interp_1d_var_lin_real -------------------------------!! 
!!------------------ interp_1d_var_lin_real -------------------------------!! 

      SUBROUTINE interp_1d_var_lin_real(X, Y, N, XP, YP, NP, XS, XE) 
      IMPLICIT NONE 

      REAL,    INTENT(IN)    :: X(*)   ! argument
      REAL,    INTENT(IN)    :: Y(*)   ! argument
      INTEGER, INTENT(IN)    :: N      ! argument
      REAL,    INTENT(INOUT) :: XP(*)  ! argument
      REAL,    INTENT(OUT)   :: YP(*)  ! argument
      INTEGER, INTENT(IN)    :: NP     ! argument
      REAL,    INTENT(IN)    :: XS     ! argument
      REAL,    INTENT(IN)    :: XE     ! argument

      integer :: j1, isi, ip ! local
      real :: dxp ! local

      IF (NP > 1) THEN 
        DXP = (XE - XS)/(NP - 1) 
      ELSE 
        DXP = 0. 
      ENDIF 

      XP(:NP) = XS + (/(J1,J1=0,NP - 1)/)*DXP 
      ISI = 1 

      DO IP = 1, NP 
!----------------------------------------------------------------------- 
!   Note: The following search loop (which increments the variable ISI) 
!   is intentionally written as a NON-VECTOR loop, as it typically goes 
!   through too few iterations to justify the overhead of a vector loop. 
!----------------------------------------------------------------------- 
   10   CONTINUE 
        IF (XP(IP) > X(ISI)) THEN 
          IF (ISI == N) GO TO 20 
          ISI = ISI + 1 
          GO TO 10 
        ENDIF 
        IF (ISI == 1) THEN 
          YP(IP) = Y(1) 
        ELSE 
          YP(IP) = Y(ISI-1) + (XP(IP)-X(ISI-1))*(Y(ISI)-Y(ISI-1)) / &
            (X(ISI)-X(ISI -1)) 
        ENDIF 
      END DO 
      RETURN  
   20 CONTINUE 

      YP(IP:NP) = Y(N) 

      RETURN  
      END SUBROUTINE interp_1d_var_lin_real 


  !!------------------------ interp_2d_var_lin_real ------------------------!!
  !!------------------------ interp_2d_var_lin_real ------------------------!!
  !!------------------------ interp_2d_var_lin_real ------------------------!!

  subroutine interp_2d_var_lin_real (xb, yb, nx, ny, tab, nr, xf, yf, f) 
    !
    ! - Arguments
    !
    real,    intent (in)  :: xb (*)         ! argument
    real,    intent (in)  :: yb (*)         ! argument
    integer, intent (in)  :: nx             ! argument
    integer, intent (in)  :: ny             ! argument
    real                  :: tab (nr,nx,ny) ! argument
    integer, intent (in)  :: nr             ! argument
    real,    intent (in)  :: xf             ! argument
    real,    intent (in)  :: yf             ! argument
    real,    intent (out) :: f (*)          ! argument
    !
    ! - Local variables
    !
    integer :: il1, il2, ib1, ib2             ! local
    real    :: fac1, fac2, fx1, fx2, fx3, fx4 ! local
    !
    ! - Begin interp_2d_var_lin_real
    !
    ! -  interpolate y location yf from yb (ylen) 
    !
    call interp_1d_lin (b   = yb (:ny),   &
                        f   = yf,         &
                        i1  = il1,        &
                        i2  = il2,        &
                        fac = fac1)
    !
    ! - interpolate x location xf from xb (xlen) 
    !
    call interp_1d_lin (b   = xb (:nx),   &
                        f   = xf,         &
                        i1  = ib1,        &
                        i2  = ib2,        &
                        fac = fac2)
    !
    ! - get interpolated real function from table 
    !
    fx4 = fac1 * fac2 
    fx3 = fac1 - fx4 
    fx2 = fac2 - fx4 
    fx1 = 1.0 - fac1 - fx2 
    f(:nr) = fx1 * tab (:nr, ib1, il1)     &
             + fx2 * tab (:nr, ib2, il1)   &
             + fx3 * tab (:nr, ib1, il2)   &
             + fx4 * tab (:nr, ib2, il2) 
    !
  end subroutine interp_2d_var_lin_real 

  !!-------------------------- interp_2d_var_real --------------------------!!
  !!-------------------------- interp_2d_var_real --------------------------!!
  !!-------------------------- interp_2d_var_real --------------------------!!

  function interp_2d_var_real (xb, yb, nx, ny, tab, xf, yf) result (f)
    !
    ! - Arguments
    !
    integer, intent (in), optional :: nx          ! argument
    integer, intent (in), optional :: ny          ! argument
    real,    intent (in)           :: xb  (:)     ! argument
    real,    intent (in)           :: yb  (:)     ! argument
    real,    intent (in)           :: tab (:,:)   ! argument
    real,    intent (in)           :: xf          ! argument
    real,    intent (in)           :: yf          ! argument
    real                           :: f           ! argument
    !
    ! - Local variables
    !
    integer :: il1, il2, ib1, ib2             ! local
    integer :: xlen, ylen                     ! local
    real    :: fac1, fac2, fx1, fx2, fx3, fx4 ! local
    !
    ! - Begin interp_2d_var_real
    !
    ! -  interpolate y location yf from yb (ylen) 
    !
    if (present (a = ny)) then
      ylen = ny
    else
      ylen = size (yb)
    end if
    !
    call interp_1d_lin (b   = yb (:ylen),   &
                        f   = yf,         &
                        i1  = il1,        &
                        i2  = il2,        &
                        fac = fac1)
    !
    ! - interpolate x location xf from xb (xlen) 
    !
    if (present (a = nx)) then
      xlen = nx
    else
      xlen = size (xb)
    end if
    !
    call interp_1d_lin (b   = xb (:xlen),   &
                        f   = xf,         &
                        i1  = ib1,        &
                        i2  = ib2,        &
                        fac = fac2)
    !
    ! - get interpolated real function from table 
    !
    fx4 = fac1 * fac2 
    fx3 = fac1 - fx4 
    fx2 = fac2 - fx4 
    fx1 = 1.0 - fac1 - fx2 
    f = fx1 * tab(ib1, il1)     &
             + fx2 * tab(ib2, il1)   &
             + fx3 * tab(ib1, il2)   &
             + fx4 * tab(ib2, il2) 
    !
  end function interp_2d_var_real 

  !!------------------------ interp_2d_var_lin_grid ------------------------!!
  !!------------------------ interp_2d_var_lin_grid ------------------------!!
  !!------------------------ interp_2d_var_lin_grid ------------------------!!

  subroutine interp_2d_var_lin_grid (ninl, inlines, ncrl, crosslines,    &
                                     beg_tab, end_tab,                   &
                                     ntr, inline_pos, crossline_pos,     &
                                          beg_tim, end_tim)
    !
    ! - Arguments
    !
    integer, intent (in)  :: ninl                  ! argument
    integer, intent (in)  :: ncrl                  ! argument
    integer, intent (in)  :: ntr                   ! argument
    real,    intent (in)  :: inlines (ninl)        ! argument
    real,    intent (in)  :: crosslines (ncrl)     ! argument
    real,    intent (in)  :: beg_tab (ninl,ncrl)   ! argument
    real,    intent (in)  :: end_tab (ninl,ncrl)   ! argument
    real,    intent (in)  :: inline_pos (ntr)      ! argument
    real,    intent (in)  :: crossline_pos (ntr)   ! argument
    real,    intent (out) :: beg_tim (ntr)         ! argument
    real,    intent (out) :: end_tim (ntr)         ! argument
    !
    ! - Local variables
    !
    integer :: il1, il2, ib1, ib2, pos        ! local

    real    :: fac1, fac2, fx1, fx2, fx3, fx4 ! local
    !
    ! - Begin interp_2d_var_lin_grid
    !
    do pos = 1, ntr
      !
      ! -  interpolate inline_pos location from inlines 
      !
      call interp_1d_lin (b   = inlines,            &
                          f   = inline_pos (pos),   &
                          i1  = il1,                &
                          i2  = il2,                &
                          fac = fac1)
      !
      ! - interpolate crossline_pos location from crosslines 
      !
      call interp_1d_lin (b   = crosslines,            &
                          f   = crossline_pos (pos),   &
                          i1  = ib1,        &
                          i2  = ib2,        &
                          fac = fac2)
      !
      ! - get interpolated real function from table 
      !
      fx4 = fac1 * fac2 
      fx3 = fac1 - fx4 
      fx2 = fac2 - fx4 
      fx1 = 1.0 - fac1 - fx2 
      beg_tim (pos) = fx1 * beg_tab (ib1, il1)     &
                      + fx2 * beg_tab (ib2, il1)   &
                      + fx3 * beg_tab (ib1, il2)   &
                      + fx4 * beg_tab (ib2, il2) 
      !
      end_tim (pos) = fx1 * end_tab (ib1, il1)     &
                      + fx2 * end_tab (ib2, il1)   &
                      + fx3 * end_tab (ib1, il2)   &
                      + fx4 * end_tab (ib2, il2) 
      !
    end do
    !
  end subroutine interp_2d_var_lin_grid 

  !!---------------------------- interp_1d_lin -----------------------------!!
  !!---------------------------- interp_1d_lin -----------------------------!!
  !!---------------------------- interp_1d_lin -----------------------------!!

  !
  ! - A utility function to interpolate point F in B.
  !   Return the surrounding point indices i1 & i2, and the 
  !   interpolation factor fac.
  !
  subroutine interp_1d_lin (b, f, i1, i2, fac)
    !
    ! - Arguments
    !
    real,    intent(in)  :: b (:)
    real,    intent(in)  :: f
    integer, intent (out) :: i1
    integer, intent (out) :: i2
    real,    intent (out) :: fac
    !
    ! - Local variables
    !
    integer :: i
    integer :: n
    real    :: fl
    !
    ! - Begin interp_1d_lin
    !
    n = size (b)
    fl = min (a1 = max (a1 = f,   &
                        a2 = b (1)),   &
              a2 = b (n)) 
    !
    i2 = n 
    i1 = n 
    fac = 0.0 
    !
    loop_scan_points:   &
      do i = 2, n 
      !
      if (fl < b (i)) then
        i1 = i - 1 
        i2 = i
        fac = (fl - b (i1))   &
               / (b (i2) - b (i1)) 
        exit loop_scan_points
      end if 
      !
    end do loop_scan_points
    !
  end subroutine interp_1d_lin


!!------------------ interp_2d_var_lin_int -------------------------------!! 
!!------------------ interp_2d_var_lin_int -------------------------------!! 
!!------------------ interp_2d_var_lin_int -------------------------------!! 

      SUBROUTINE interp_2d_var_lin_int (XB, YB, NX, NY, ITAB, NI, XF, YF, IF) 
      IMPLICIT NONE 
!  ****  INTEPOLATE Y LOCATION YF FROM YB(NY) 
      REAL,    INTENT(IN)  :: XB(*)           ! argument
      REAL,    INTENT(IN)  :: YB(*)           ! argument
      INTEGER, INTENT(IN)  :: NX              ! argument
      INTEGER, INTENT(IN)  :: NY              ! argument
      INTEGER, INTENT(IN)  :: NI              ! argument
      INTEGER, INTENT(IN)  :: ITAB(NI,NX,NY)  ! argument
      REAL,    INTENT(IN)  :: XF              ! argument
      REAL,    INTENT(IN)  :: YF              ! argument
      INTEGER, INTENT(OUT) :: IF(*)           ! argument

      integer :: il1, il2, ib1, ib2 ! local
      real :: yfl, fac1, fac2, fx1, fx2, fx3, fx4, xfl ! local

      YFL = MIN(MAX(YF,YB(1)),YB(NY)) 

      DO IL2 = 2, NY 
        IF (YFL < YB(IL2)) GO TO 404 
      END DO 

      IL2 = NY 
      IL1 = NY 
      FAC1 = 0.0 
      GO TO 405 
  404 CONTINUE 
      IL1 = IL2 - 1 
      FAC1 = (YFL - YB(IL1))/(YB(IL2)-YB(IL1)) 
 
!  ****  INTERPOLATE X LOCATION XF FROM XB(NX) 
  405 CONTINUE 
      XFL = MIN(MAX(XF,XB(1)),XB(NX)) 

      DO IB2 = 2, NX 
        IF (XFL < XB(IB2)) GO TO 408 
      END DO 

      IB2 = NX 
      IB1 = NX 
      FAC2 = 0.0 
      GO TO 409 
  408 CONTINUE 
      IB1 = IB2 - 1 
      FAC2 = (XFL - XB(IB1))/(XB(IB2)-XB(IB1)) 
 
!  ****  GET INTERPOLATED INTEGER FUNCTION FROM TABLE 
  409 CONTINUE 
      FX4 = FAC1*FAC2 
      FX3 = FAC1 - FX4 
      FX2 = FAC2 - FX4 
      FX1 = 1.0 - FAC1 - FX2 

      IF(:NI) = FX1*ITAB(:,IB1,IL1) + FX2*ITAB(:,IB2,IL1) + &
        FX3*ITAB(:,IB1,IL2) + FX4*ITAB(:,IB2,IL2) 

      RETURN  
      END SUBROUTINE interp_2d_var_lin_int 

!!------------------ interp_1d_con_lin_real -------------------------------!! 
!!------------------ interp_1d_con_lin_real -------------------------------!! 
!!------------------ interp_1d_con_lin_real -------------------------------!! 

      SUBROUTINE interp_1d_con_lin_real (INTRP, NT, IFLG, H, G, TR, ETR) 
      IMPLICIT NONE 
      INTEGER, INTENT(IN)              :: INTRP   ! argument
      INTEGER, INTENT(IN)              :: NT      ! argument
      INTEGER, INTENT(IN)              :: IFLG    ! argument
      REAL,    INTENT(INOUT)           :: H(*)    ! argument
      REAL,    INTENT(INOUT)           :: G(*)    ! argument
      REAL,    INTENT(INOUT), optional :: TR(*)   ! argument
      REAL,    INTENT(OUT), optional   :: ETR(*)  ! argument

      integer :: j2, k, l ! local

      IF (IFLG<=0 .OR. .not. present(tr)) THEN 
        H(:INTRP) = 1. - ((/(J2,J2=1,INTRP)/) - 1.)/INTRP 
        G(:INTRP) = 1. - H(:INTRP) 
        IF (IFLG<0 .OR. .not. present(tr)) RETURN  
      ENDIF 

      DO K = 1, INTRP 
        L = K 
        ETR(L:(NT-2)*INTRP+L:INTRP) = H(K)*TR(:NT-1) + G(K)*TR(2:NT) 
        ETR((NT-1)*INTRP+L)         = H(K)*TR(NT) + G(K)*TR(NT) 
      END DO 

      RETURN  
      END SUBROUTINE interp_1d_con_lin_real 

!!-------------------- interp_1d_con_lin_real2_init ------------------------!! 
!!-------------------- interp_1d_con_lin_real2_init ------------------------!! 
!!-------------------- interp_1d_con_lin_real2_init ------------------------!! 

      SUBROUTINE interp_1d_con_lin_real2_init (INTRP, H, G) 
      !
      ! - Arguments
      ! 
      INTEGER, INTENT (IN)  :: INTRP
      REAL,    INTENT (OUT) :: H (:)
      REAL,    INTENT (OUT) :: G (:)
      !
      ! - Local variables
      !
      INTEGER :: J2
      !
      ! - Begin interp_1d_con_lin_real2_init
      !
      H (:INTRP) = 1.0 - ((/(J2, J2 = 1, INTRP)/) - 1.0) / INTRP 
      G (:INTRP) = 1.0 - H (:INTRP) 
      !  
      END SUBROUTINE interp_1d_con_lin_real2_init 

!!------------------ interp_1d_con_lin_real2 -------------------------------!! 
!!------------------ interp_1d_con_lin_real2 -------------------------------!! 
!!------------------ interp_1d_con_lin_real2 -------------------------------!! 

      SUBROUTINE interp_1d_con_lin_real2 (INTRP, NDPT, H, G, TR, ETR) 
      ! 
      ! - Arguments
      !
      INTEGER, INTENT (IN)    :: INTRP
      INTEGER, INTENT (IN)    :: NDPT
      REAL,    INTENT (IN)    :: H   (:)
      REAL,    INTENT (IN)    :: G   (:)
      REAL,    INTENT (INOUT) :: TR  (:)
      REAL,    INTENT (OUT)   :: ETR (:)
      !
      ! - Local variables
      !
      INTEGER :: K
      INTEGER :: L
      !
      ! - Begin interp_1d_con_lin_real2
      !
      L = 1
      !
      DO K = 1, NDPT - 1
        !
        ETR (L:L + INTRP - 1) = H (1:INTRP) * TR (K)     &
                                + G (1:INTRP) * TR (K+1) 
        L = L + INTRP
        !
      END DO 
      !
      ETR (L:L + INTRP - 1) = H (1:INTRP) * TR (NDPT)     &
                              + G (1:INTRP) * TR (NDPT) 
      !
      END SUBROUTINE interp_1d_con_lin_real2 

!!------------------ interp_1d_con_bw_real -------------------------------!! 
!!------------------ interp_1d_con_bw_real -------------------------------!! 
!!------------------ interp_1d_con_bw_real -------------------------------!! 

      SUBROUTINE interp_1d_con_bw_real (INTRP, NT, IFLG, B, TR, ETR) 
      IMPLICIT NONE 
      INTEGER, INTENT(IN)              :: INTRP       ! argument
      INTEGER, INTENT(IN)              :: NT          ! argument
      INTEGER, INTENT(IN)              :: IFLG        ! argument
      REAL,    INTENT(INOUT)           :: B(INTRP,*)  ! argument
      REAL,    INTENT(INOUT), optional :: TR(*)       ! argument
      REAL,    INTENT(OUT), optional   :: ETR(*)      ! argument

      INTEGER :: J, I, K, L ! local
      REAL :: RINTRP, D, S1, S2 ! local

      IF (IFLG<=0 .OR. .not. present(tr)) THEN 
        RINTRP = 1./INTRP 
        B(:,1) = 0.0 

        DO J = 2, 5 
          IF (J==2 .OR. J==5) THEN 
            DO I = 1, INTRP 
              D = ABS(J - 3. - (I - 1)*RINTRP) 
              B(I,J) = 2. - 4*D + 2.5*D**2 - 0.5*D**3 
            END DO 
          ELSE IF (J==3 .OR. J==4) THEN 
            DO I = 1, INTRP 
              D = ABS(J - 3. - (I - 1)*RINTRP) 
              B(I,J) = 1. - 2.5*D**2 + 1.5*D**3 
            END DO 
          ENDIF 
        END DO 

!      DO 611 I=1,INTRP 
!611   PRINT '('' =>interp_1d_con_bw_real; i='',i3,5f8.3)', i,(b(i,j),j=1,5) 

        IF (IFLG<0 .OR. .not. present(tr)) RETURN  
      ENDIF 

! 
!********************************************************** 
! S1 and S2 are linear extrapolations of 1st 2 data points. 
      S1 = 2.*TR(1) - TR(2) 
      S2 = 3.*TR(1) - 2.*TR(2) 
!********************************************************** 
! 
      DO K = 1, INTRP 
        L = K 
        ETR(L) = B(K,1)*S2 + B(K,2)*S1 + B(K,3)*TR(1) + B(K,4)*TR(2) + &
          B(K,5)* TR(2) 

        L = L + INTRP 
        ETR(L) = B(K,1)*S1 + B(K,2)*TR(1) + B(K,3)*TR(2) + B(K,4)*TR(3) + &
          B(K,5)*TR(4) 

        L = L + INTRP 
        ETR (L:(NT - 3) * INTRP + L:INTRP) = B (K, 1)   * TR (1:NT-4)     &
                                             + B (K, 2) * TR (2:NT-3)     &
                                             + B (K, 3) * TR (3:NT-2)     &
                                             + B (K, 4) * TR (4:NT-1)     &
                                             + B (K, 5) * TR (5:NT)

        L = K + (NT-2)*INTRP 
        ETR(L) = B (K, 1)  * TR (NT - 3)   &
                 + B(K, 2) * TR (NT - 2)   &
                 + B(K, 3) * TR (NT - 1)   &
                 + B(K, 4) * TR (NT)       &
                 + B(K, 5) * TR (NT) 

        L = L + INTRP 
        ETR(L) = B (K, 1)   * TR (NT - 2)   &
                 + B (K, 2) * TR (NT - 1)   &
                 + B (K, 3) * TR (NT)       &
                 + B (K, 4) * TR (NT)       &
                 + B (K, 5) * TR (NT) 
 
      END DO

      !
      END SUBROUTINE interp_1d_con_bw_real 


!!------------------ interp_1d_con_bw_real2_init ---------------------------!! 
!!------------------ interp_1d_con_bw_real2_init ---------------------------!! 
!!------------------ interp_1d_con_bw_real2_init ---------------------------!! 

      SUBROUTINE interp_1d_con_bw_real2_init (INTRP, B) 
      !
      ! - Arguments
      !
      INTEGER, INTENT (IN)  :: INTRP
      REAL,    INTENT (OUT) :: B (:,:)    ! (INTRP, 5)
      !
      ! - Local variables
      !
      INTEGER :: J
      INTEGER :: I
      REAL    :: RINTRP
      REAL    :: D
      !
      ! - Begin interp_1d_con_bw_real2_init
      !
      RINTRP = 1./INTRP 
      B(:,1) = 0.0 
      !
      DO J = 2, 5, 3 
        DO I = 1, INTRP 
          D        = ABS (J - 3. - (I - 1) * RINTRP)
          B (I, J) = 2.0 - 4 * D + 2.5 * D**2 - 0.5 * D**3 
        END DO 
      END DO
      ! 
      DO J = 3, 4 
        DO I = 1, INTRP 
          D        = ABS (J - 3.0 - (I - 1) * RINTRP) 
          B (I, J) = 1.0 - 2.5 * D**2 + 1.5 * D**3 
        END DO 
      END DO 
      !
      END SUBROUTINE interp_1d_con_bw_real2_init


!!------------------ interp_1d_con_bw_real2 -------------------------------!! 
!!------------------ interp_1d_con_bw_real2 -------------------------------!! 
!!------------------ interp_1d_con_bw_real2 -------------------------------!! 

      SUBROUTINE interp_1d_con_bw_real2 (INTRP, NT, B, TR, ETR) 
      !
      ! - Arguments
      !
      INTEGER, INTENT (IN)    :: INTRP
      INTEGER, INTENT (IN)    :: NT
      REAL,    INTENT (IN)    :: B   (:,:)    ! (INTRP, NT)
      REAL,    INTENT (INOUT) :: TR  (:)
      REAL,    INTENT (OUT)   :: ETR (:)
      !
      ! - Local variables
      !
      INTEGER ::       K, L ! local
      REAL ::            S1, S2 ! local
      REAL :: SN1, SN2
      !
      ! - Begin interp_1d_con_bw_real2
      !
      ! - S1 and S2 are linear extrapolations of 1st 2 data points. 
      !
      S1 = 2.0 * TR (1) - TR (2) 
      S2 = 3.0 * TR (1) - 2.0 * TR (2) 
      ! 
      ! - SN1 and SN2 are linear extrapolations of last 2 data points. 
      ! 
      SN1 = 2.0 * TR (NT) - TR (NT - 1) 
      SN2 = 3.0 * TR (NT) - 2.0 * TR(NT - 1) 
      ! 
      DO K = 1, INTRP 
        !
        L = K 
        ETR(L) = B (K, 1)   * S2        &
                 + B (K, 2) * S1        &
                 + B (K, 3) * TR (1)    &
                 + B (K, 4) * TR (2)    &
                 + B (K, 5) * TR (2) 
        !
        L = L + INTRP 
        ETR(L) = B (K, 1)   * S1        &
                 + B (K, 2) * TR (1)    &
                 + B (K, 3) * TR (2)    &
                 + B (K, 4) * TR (3)    &
                 + B (K, 5) * TR (4) 
        !
        L = L + INTRP 
        ETR (L:(NT - 5) * INTRP + L:INTRP) = B (K, 1)   * TR (1:NT-4)     &
                                             + B (K, 2) * TR (2:NT-3)     &
                                             + B (K, 3) * TR (3:NT-2)     &
                                             + B (K, 4) * TR (4:NT-1)     &
                                             + B (K, 5) * TR (5:NT)

        L = K + (NT-2) * INTRP 
        ETR(L) = B(K,1)   * TR(NT-3)   &
                 + B(K,2) * TR(NT-2)   &
                 + B(K,3) * TR(NT-1)   &
                 + B(K,4) * TR(NT)     &
                 + B(K,5) * SN1 

        L = L + INTRP 
        ETR(L) = B(K,1)   * TR(NT-2)   &
                 + B(K,2) * TR(NT-1)   &
                 + B(K,3) * TR(NT)     &
                 + B(K,4) * SN1        &
                 + B(K,5) * SN2  
      END DO
      !  
      END SUBROUTINE interp_1d_con_bw_real2

!!------------------ interp_1d_con_bw_cpx -------------------------------!! 
!!------------------ interp_1d_con_bw_cpx -------------------------------!! 
!!------------------ interp_1d_con_bw_cpx -------------------------------!! 

      SUBROUTINE interp_1d_con_bw_cpx (INTRP, NT, IFLG, B, CTR, CETR) 
      IMPLICIT NONE 
      INTEGER, INTENT(IN)              :: INTRP       ! argument
      INTEGER, INTENT(IN)              :: NT          ! argument
      INTEGER, INTENT(IN)              :: IFLG        ! argument
      REAL,    INTENT(INOUT)           :: B(INTRP,*)  ! argument
      COMPLEX, INTENT(INOUT), optional :: CTR(*)      ! argument
      COMPLEX, INTENT(OUT), optional   :: CETR(*)     ! argument

      integer :: j, i, k, l ! local
      real :: rintrp, c1, c2, d ! local

      IF (IFLG<=0 .OR. .not. present(ctr)) THEN 
        RINTRP = 1./INTRP 
        B(:,1) = 0.0 

        DO J = 2, 5 
          IF (J==2 .OR. J==5) THEN 
            DO I = 1, INTRP 
              D = ABS(J - 3. - (I - 1)*RINTRP) 
              B(I,J) = 2. - 4*D + 2.5*D**2 - 0.5*D**3 
            END DO 
          ELSE IF (J==3 .OR. J==4) THEN 
            DO I = 1, INTRP 
              D = ABS(J - 3. - (I - 1)*RINTRP) 
              B(I,J) = 1. - 2.5*D**2 + 1.5*D**3 
            END DO 
          ENDIF 
        END DO 

!       DO 711 I=1,INTRP 
! 711   PRINT '('' =>interp_1d_con_bw_cpx; i='',i3,5f6.2)', i,(b(i,j),j=1,5) 

        IF (IFLG<0 .OR. .not. present(ctr)) RETURN  
      ENDIF 

      CTR(NT+1) = CTR(NT) 
      CTR(NT+2) = CTR(NT) 
! 
!********************************************************** 
! C1 and C2 are linear extrapolations of 1st 2 data points. 
      C1 = 2.*CTR(1) - CTR(2) 
      C2 = 3.*CTR(1) - 2.*CTR(2) 
!********************************************************** 
! 
      DO K = 1, INTRP 
        L = K 
        CETR(L) = B(K,1)*C2 + B(K,2)*C1 + B(K,3)*CTR(1) + B(K,4)*CTR(2) + &
          B(K,5)*CTR(2) 
        L = L + INTRP 
        CETR(L) = B(K,1)*C1 + B(K,2)*CTR(1) + B(K,3)*CTR(2) + B(K,4)*CTR(3) + &
          B(K,5)*CTR(4) 
        L = L + INTRP 
        CETR(L:(NT-3)*INTRP+L:INTRP) = B(K,1)*CTR(:NT-2) + B(K,2)*CTR(2:NT-1)& 
           + B(K,3)*CTR(3:NT) + B(K,4)*CTR(4:NT+1) + B(K,5)*CTR(5:NT+2) 
      END DO 
      RETURN  
      END SUBROUTINE interp_1d_con_bw_cpx 

!!----------------------------- end of module -----------------------------!! 
!!----------------------------- end of module -----------------------------!! 
!!----------------------------- end of module -----------------------------!! 

      end module interp_module 

!!--------------------------------- end -----------------------------------!! 
!!--------------------------------- end -----------------------------------!! 
!!--------------------------------- end -----------------------------------!! 
