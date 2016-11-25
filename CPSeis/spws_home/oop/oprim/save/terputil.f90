!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- terputil.f90 --------------------------------!!
!!---------------------------- terputil.f90 --------------------------------!!
!!---------------------------- terputil.f90 --------------------------------!!


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
! Name       : TERPUTIL         (inTERPolation UTILities)
! Category   : math
! Written    : 2000-12-14   by: Tom Stoeckley
! Revised    : 2003-06-17   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : miscellaneous linear interpolation utilities.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive contains various linear interpolation utilities.
!
!-------------------------------------------------------------------------------
!</descript_doc>


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
! To linearly interpolate a 1D array:
!
!            o                              i      i     i   i
!   (1)      wx   = terputil_get_weights (xcoord,xfirst,xinc,nx)
!   (2)      wx   = terputil_get_weights (xcoord,xarray,     nx)
!
!                                                  opt
!            o                           i    i     o
!   (3)    zvalue = terputil_get_answer (wx,zarray,dzdx)
!                                                                  opt
!            o                         i      i     i   i    i      o
!   (4)    ZVALUE = TERPUTIL_INTERP (XCOORD,XFIRST,XINC,NX,ZARRAY, DZDX)
!   (5)    ZVALUE = TERPUTIL_INTERP (XCOORD,XARRAY,     NX,ZARRAY, DZDX)
!
! type(terputil_weights)  WX  = weights and indices (not a pointer).
! real                XCOORD  = desired abscissa      (independent variable).
! real                ZVALUE  = interpolated ordinate (dependent variable).
! real                  DZDX  = interpolated gradient (dependent variable).
! real                XFIRST  = first abscissa.
! real                  XINC  = abscissa increment (positive or negative).
! real             XARRAY(NX) = array of abscissae.
! real             ZARRAY(NX) = array of ordinates.
! integer                 NX  = number of abscissae and ordinates.
! 
! XARRAY(NX) must be either increasing or decreasing.
! It is okay to have adjacent equal values in XARRAY(NX).
! Flat extrapolation is performed.
! If NX is zero, ZVALUE and DZDX are set to zero.
! If XCOORD is at or beyond either end of ZARRAY(NX), DZDX is set to zero.
! Function (2) uses a binary search through XARRAY(NX).
!
! First call either function (1) or (2) to get the weights.
! Then call function (3) to get the ordinate and (optionally) the gradient.
! Function (4) is a combination of functions (1) and (3).
! Function (5) is a combination of functions (2) and (3).
! Function (2) calls terputil_binary_search.
!
!-------------------------------------------------------------------------------
! To linearly interpolate a 2D array:
!
!            o                                i      i     i   i
!   (1x)     wx   = terputil_get_weights   (xcoord,xfirst,xinc,nx)
!   (1y)     wy   = terputil_get_weights   (ycoord,yfirst,yinc,ny)
!   (2x)     wx   = terputil_get_weights   (xcoord,xarray,     nx)
!   (2y)     wy   = terputil_get_weights   (ycoord,yarray,     ny)
!
!                                                        opt  opt
!            o                              i  i    i     o    o
!   (3)    zvalue = terputil_get_answer    (wx,wy,zarray,dzdx,dzdy)
!
!                                                                  opt  opt
!            o                         i      i     i   i    i      o    o
!   (4)    ZVALUE = TERPUTIL_INTERP (XCOORD,XFIRST,XINC,NX,
!                                    YCOORD,YFIRST,YINC,NY,ZARRAY, DZDX,DZDY)
!   (5)    ZVALUE = TERPUTIL_INTERP (XCOORD,XARRAY,     NX,
!                                    YCOORD,YARRAY,     NY,ZARRAY, DZDX,DZDY)
!
! type(terputil_weights)  WX  = X weights and indices (not a pointer).
! type(terputil_weights)  WY  = Y weights and indices (not a pointer).
! real                XCOORD  = desired X abscissa    (independent variable).
! real                YCOORD  = desired Y abscissa    (independent variable).
! real                ZVALUE  = interpolated ordinate   (dependent variable).
! real                  DZDX  = interpolated X gradient (dependent variable).
! real                  DZDY  = interpolated Y gradient (dependent variable).
! real                XFIRST  = first X abscissa.
! real                YFIRST  = first Y abscissa.
! real                  XINC  = X abscissa increment (positive or negative).
! real                  YINC  = Y abscissa increment (positive or negative).
! real          XARRAY   (NX) = array of X abscissae.
! real          YARRAY   (NY) = array of Y abscissae.
! real          ZARRAY(NX,NY) = array of ordinates.
! integer                 NX  = number of X abscissae.
! integer                 NY  = number of Y abscissae.
! 
! XARRAY(NX) and YARRAY(NY) must be either increasing or decreasing.
! It is okay to have adjacent equal values in XARRAY(NX) and YARRAY(NY).
! Flat extrapolation is performed.
! If NX or NY is zero, ZVALUE and DZDX and DZDY are set to zero.
! If XCOORD or YCOORD is at or beyond either end of ZARRAY(NX,NY), DZDX and
!   DZDY are set to zero.
! Functions (2x) and (2y) use a binary search.
!
! First call either function (1x) or (2x), and either (1y) or (2y).
! Then call function (3) to get the ordinate and (optionally) the gradients.
! Function (4) is a combination of functions (1x), (1y), and (3).
! Function (5) is a combination of functions (2x), (2y), and (3).
!
!-------------------------------------------------------------------------------
! To resample an array of ordinates using linear interpolation:
!
!                               i       i    i      i
!    CALL TERPUTIL_RESAMPLE (XFIRST1, XINC1, N1, ZARRAY1,   ! from even array
!                            XFIRST2, XINC2, N2, ZARRAY2)   ! to even array
!                               i       i    i      o
!
!                               i       i    i      i
!    CALL TERPUTIL_RESAMPLE (XFIRST1, XINC1, N1, ZARRAY1,   ! from even array
!                            XARRAY2,        N2, ZARRAY2)   ! to uneven array
!                               i            i      o
!
!                               i            i      i
!    CALL TERPUTIL_RESAMPLE (XARRAY1,        N1, ZARRAY1,   ! from uneven array
!                            XFIRST2, XINC2, N2, ZARRAY2)   ! to even array
!                               i       i    i      o
!
!                               i            i      i
!    CALL TERPUTIL_FASTSAMP (XARRAY1,        N1, ZARRAY1,   ! from uneven array
!                            XFIRST2, XINC2, N2, ZARRAY2)   ! to even array
!                               i       i    i      o       ! (faster)
!
!                               i            i      i
!    CALL TERPUTIL_RESAMPLE (XARRAY1,        N1, ZARRAY1,   ! from uneven array
!                            XARRAY2,        N2, ZARRAY2)   ! to uneven array
!                               i            i      o
!
! real    XFIRST1     = abscissa for first point in input  array.
! real    XFIRST2     = abscissa for first point in output array.
! real    XINC1       = abscissa increment for input  array.
! real    XINC2       = abscissa increment for output array.
! integer N1          = number of points in input  array.
! integer N2          = number of points in output array.
! real    XARRAY1(N1) = input  array of abscissae.
! real    XARRAY2(N2) = output array of abscissae.
! real    ZARRAY1(N1) = input  array of ordinates.
! real    ZARRAY2(N2) = output array of ordinates.
!
! Flat extrapolation is performed.
! These routines call TERPUTIL_INTERP (except for TERPUTIL_FASTSAMP).
!
!-------------------------------------------------------------------------------
! To get a point on a line:
!
!             o                       i    i  i  i  i
!           YCOORD = TERPUTIL_ROOT (XCOORD,XA,XB,YA,YB)
!
! real XCOORD = abscissa of point on a line (independent variable).
! real YCOORD = ordinate of point on a line (dependent variable).
! real XA     = abscissa of the first point defining the line.
! real XB     = abscissa of the second point defining the line.
! real YA     = ordinate of the first point defining the line.
! real YB     = ordinate of the second point defining the line.
!
! This function linearly interpolates between the points (XA,YA) and (XB,YB)
!  to find the YCOORD value for the point (XCOORD,YCOORD).
! This function returns YA + (XCOORD-XA) * (YB-YA)/(XB-XA).
! This function returns the average of YA and YB if XA == XB.
! Sloping extrapolation is performed.
!
!-------------------------------------------------------------------------------
! To get the fractional distance from one point to the next:
!
! To get the weights to use at two points on a line to calculate the value
! at an intermediate point:
!
! To get the weights to use at four points on a plane to calculate the value
! at an intermediate point:
!
!              o                                  i    i  i
!           FRACDIST =       TERPUTIL_FRACDIST (XCOORD,XA,XB)
!              WA    = 1.0 - TERPUTIL_FRACDIST (XCOORD,XA,XB)
!              WB    =       TERPUTIL_FRACDIST (XCOORD,XA,XB)
!
!                                i    i  i   o  o
!    CALL TERPUTIL_1D_WEIGHTS (XCOORD,XA,XB, WA,WB)
!
!    CALL TERPUTIL_2D_WEIGHTS (XCOORD,XA,XB, YCOORD,YA,YB, WAA,WAB,WBA,WBB)
!                                i    i  i     i    i  i    o   o   o   o
!
! real FRACDIST = fractional distance of XCOORD from point XA to point XB.
! real XCOORD   = abscissa of point on a line between XA and XB.
! real XA       = abscissa of the point on the line preceding XCOORD.
! real XB       = abscissa of the point on the line following XCOORD.
! real WA       = weight to use at point XA.
! real WB       = weight to use at point XB.
!
! These functions return FRACDIST = (XCOORD-XA) / (XB-XA).
! These functions return    WA    = (XB-XCOORD) / (XB-XA).
! These functions return    WB    = (XCOORD-XA) / (XB-XA).
!
! These functions return FRACDIST = 0.5 if XA == XB.
! These functions return    WA    = 0.5 if XA == XB.
! These functions return    WB    = 0.5 if XA == XB.
!
! For 2D weights:
!
! real XCOORD   = X coordinate of point on a plane between XA and XB.
! real YCOORD   = Y coordinate of point on a plane between YA and YB.
! real XA       = X coordinate of the point on the line preceding XCOORD.
! real YA       = Y coordinate of the point on the line preceding YCOORD.
! real XB       = X coordinate of the point on the line following XCOORD.
! real YB       = Y coordinate of the point on the line following YCOORD.
! real WAA      = weight to use at point (XA,YA).
! real WAB      = weight to use at point (XA,YB).
! real WBA      = weight to use at point (XB,YA).
! real WBB      = weight to use at point (XB,YB).
!
!-------------------------------------------------------------------------------
! To find bracketing indices with a binary search:
!                                                        <-------opt-------->
!                                         i      i    i  o  o  o  o  o  o  o
!          call terputil_binary_search (xcoord,xarray,nx,i1,i2,x1,x2,w1,w2,wg)
!
! real    XCOORD     = abscissa (independent variable).
! real    XARRAY(NX) = array of abscissae.
! integer NX         = number of points in array.
! integer I1,I2      = indices of XARRAY which bracket XCOORD.
! real    X1,X2      = values of XARRAY which bracket XCOORD.
! real    W1,W2      = weights for the two points which bracket XCOORD.
! real    WG         = reciprocal of (X2-X1).
!
! XARRAY(NX) must be either increasing or decreasing.
! It is okay to have adjacent equal values in XARRAY(NX).
! The bracketing indices will have values such that XCOORD lies between
!  XARRAY(I1) and XARRAY(I2), where I2=I1+1 or I2=I1.
! If XCOORD precedes XARRAY(1), this routine returns I1=I2=1.
! If XCOORD follows XARRAY(NX), this routine returns I1=I2=NX.
! If NX=0,  this routine returns I1=I2=0 and X1=X2=0 and W1=W2=0.5 and WG=0.
! If I1=I2, this routine returns X1=X2=XARRAY(I1)    and W1=W2=0.5 and WG=0.
! If X1=X2, this routine returns                         W1=W2=0.5 and WG=0.
!
!-------------------------------------------------------------------------------
! To replace nils by interpolated values in a 2D array:
!
!                                                   opt
!                                       i  i    b    i
!          call terputil_replace_nilx  (NX,NY,ARRAY,only)
!          call terputil_replace_nily  (NX,NY,ARRAY,only)
!
! real    ARRAY(NX,NY) = array containing some nil values.
! logical        ONLY  = whether to interpolate only in one direction.
!
! If ONLY is present and true:
!   terputil_replace_nilx interpolates only in X direction.
!   terputil_replace_nily interpolates only in Y direction.
!
! If ONLY is absent or false:
!   terputil_replace_nilx interpolates first in X direction, then Y direction.
!   terputil_replace_nily interpolates first in Y direction, then X direction.
!
! The array can have either one dimension (nx*ny) or two dimensions (nx,ny).
! The output array will not contain any nils, unless the input array is
!  entirely nil, in which case the array is unchanged.
! Flat extrapolation is performed.
!
!-------------------------------------------------------------------------------
! To replace nils by interpolated values in a 1D array:
!
!                                                opt
!                                        b    i   i
!          call terputil_replace_nils (ARRAY, N, MODE)
!
! real or integer  ARRAY(N) = array containing some nil values.
! integer            MODE   = the type of replacements to perform.
!
!   MODE == TERPUTIL_FLATKEEP does flat extrapolation (this is the default).
!   MODE == TERPUTIL_FLAT     does flat extrapolation.
!   MODE == TERPUTIL_SLOPING  does sloping extrapolation.
!   MODE == TERPUTIL_DOWN     does downward repetition of previous value.
!   MODE == TERPUTIL_ZERO     replaces all nils by zero.
!   MODE == TERPUTIL_ONE      replaces all nils by one.
!
! TERPUTIL_DOWN also does upward repetition from the first non-nil value.
!
! If the array consists of all nils, the following actions are taken:
!
!   MODE == TERPUTIL_FLATKEEP keeps the entire array set to nils.
!   MODE == TERPUTIL_FLAT     sets the entire array to zero.
!   MODE == TERPUTIL_SLOPING  sets the entire array to zero.
!   MODE == TERPUTIL_DOWN     sets the entire array to zero.
!   MODE == TERPUTIL_ZERO     sets the entire array to zero.
!   MODE == TERPUTIL_ONE      sets the entire array to one.
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
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  5. 2003-06-17  Stoeckley  Add TERPUTIL_FASTSAMP, TERPUTIL_1D_WEIGHTS, and
!                             TERPUTIL_2D_WEIGHTS.
!  4. 2001-12-10  Stoeckley  Add optional argument ONLY to the routines
!                             TERPUTIL_REPLACE_NILX and TERPUTIL_REPLACE_NILY.
!  3. 2001-08-17  Stoeckley  Expand the TERPUTIL_REPLACE_NILS routine to
!                             include options originally residing in the
!                             GEOMDATA primitive.
!  2. 2001-02-13  Stoeckley  Add the following routines to support VTRIM:
!                             terputil_get_weights (even and uneven increments),
!                             terputil_get_answer (1D and 2D),
!                             terputil_interp (1D even increments).
!                             terputil_interp (2D even and uneven increments).
!                             terputil_resample,
!                            Modify the following routine to support VTRIM:
!                             terputil_interp (1D uneven increments).
!                            Remove use of negative argument N as a flag for
!                             sloping extrapolation; if needed later, should
!                             implement use of a named constant flag instead.
!  1. 2000-12-14  Stoeckley  Initial version, with some code moved from the
!                             STATUTIL primitive or from workstation code.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module terputil_module
      use named_constants_module
      implicit none
      public

      character(len=100),public,save :: TERPUTIL_IDENT = &
'$Id: terputil.f90,v 1.5 2003/06/16 14:13:00 Stoeckley prod sps $'

      integer,public,parameter :: TERPUTIL_FLATKEEP = 1
      integer,public,parameter :: TERPUTIL_FLAT     = 2
      integer,public,parameter :: TERPUTIL_SLOPING  = 3
      integer,public,parameter :: TERPUTIL_DOWN     = 4
      integer,public,parameter :: TERPUTIL_ZERO     = 5
      integer,public,parameter :: TERPUTIL_ONE      = 6

      type,public :: terputil_weights
        private
        integer :: i1,i2
        real    :: w1,w2,wg
      end type terputil_weights


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface terputil_get_weights
           module procedure terputil_get_weights_even
           module procedure terputil_get_weights_uneven
      end interface

      interface terputil_get_answer
           module procedure terputil_get_answer_1d
           module procedure terputil_get_answer_2d
      end interface

      interface terputil_interp
           module procedure terputil_interp_1d_even
           module procedure terputil_interp_1d_uneven
           module procedure terputil_interp_2d_even
           module procedure terputil_interp_2d_uneven
      end interface

      interface terputil_replace_nils
           module procedure terputil_replace_nils_real
           module procedure terputil_replace_nils_integer
      end interface

      interface terputil_resample
           module procedure terputil_resample_even_even
           module procedure terputil_resample_even_uneven
           module procedure terputil_resample_uneven_even
           module procedure terputil_resample_uneven_uneven
      end interface

      contains


!!-------------------- terputil get weights even --------------------------!!
!!-------------------- terputil get weights even --------------------------!!
!!-------------------- terputil get weights even --------------------------!!


    function terputil_get_weights_even (xcoord,xfirst,xinc,nx) result (wx)
    implicit none
    real             ,intent(in) :: xcoord,xfirst,xinc        ! arguments
    integer          ,intent(in) :: nx                        ! arguments
    type(terputil_weights)       :: wx                        ! result
    integer                      :: indx                      ! local
    real                         :: xindx,frac                ! local

    if (nx <= 0) then
         wx%i1 = 0
         wx%i2 = 0
         wx%w1 = 0.5
         wx%w2 = 0.5
         wx%wg = 0.0
         return
    end if

    if (xinc == 0.0) then
         wx%i1 = 1
         wx%i2 = 1
         wx%w1 = 0.5
         wx%w2 = 0.5
         wx%wg = 0.0
         return
    end if

    xindx = 1.0 + (xcoord - xfirst) / xinc
    indx  = xindx
    frac  = xindx - indx

    if (indx < 1) then
         wx%i1 = 1
         wx%i2 = 1
         wx%w1 = 0.5
         wx%w2 = 0.5
         wx%wg = 0.0
         return
    end if

    if (indx >= nx) then
         wx%i1 = nx
         wx%i2 = nx
         wx%w1 = 0.5
         wx%w2 = 0.5
         wx%wg = 0.0
         return
    end if

    wx%i1 = indx
    wx%i2 = indx + 1
    wx%w1 = 1.0 - frac
    wx%w2 = frac
    wx%wg = 1.0 / xinc
    return
    end function terputil_get_weights_even


!!-------------------- terputil get weights uneven --------------------------!!
!!-------------------- terputil get weights uneven --------------------------!!
!!-------------------- terputil get weights uneven --------------------------!!


    function terputil_get_weights_uneven (xcoord,xarray,nx) result (wx)
    implicit none
    real             ,intent(in) :: xcoord,xarray(:)          ! arguments
    integer          ,intent(in) :: nx                        ! arguments
    type(terputil_weights)       :: wx                        ! result
    real                         :: x1,x2                     ! local

    call terputil_binary_search &
                  (xcoord,xarray,nx, wx%i1,wx%i2, x1,x2, wx%w1,wx%w2, wx%wg)
    return
    end function terputil_get_weights_uneven


!!-------------------- terputil get answer 1d -------------------------------!!
!!-------------------- terputil get answer 1d -------------------------------!!
!!-------------------- terputil get answer 1d -------------------------------!!


    function terputil_get_answer_1d (wx,zarray,dzdx) result (zvalue)
    implicit none
    type(terputil_weights),intent(in)  :: wx                 ! arguments
    real                  ,intent(in)  :: zarray(:)          ! arguments
    real      ,optional   ,intent(out) :: dzdx               ! arguments
    real                               :: zvalue             ! result

    if (wx%i1 == 0) then
                            zvalue = 0.0
         if (present(dzdx)) dzdx   = 0.0
         return
    end if

                       zvalue = wx%w1 * zarray(wx%i1) + wx%w2 * zarray(wx%i2)
    if (present(dzdx)) dzdx   = wx%wg * (zarray(wx%i2) - zarray(wx%i1))
    return
    end function terputil_get_answer_1d


!!-------------------- terputil get answer 2d -------------------------------!!
!!-------------------- terputil get answer 2d -------------------------------!!
!!-------------------- terputil get answer 2d -------------------------------!!


    function terputil_get_answer_2d (wx,wy,zarray,dzdx,dzdy) result (zvalue)
    implicit none
    type(terputil_weights),intent(in)  :: wx,wy            ! arguments
    real                  ,intent(in)  :: zarray(:,:)      ! arguments (nx,ny)
    real      ,optional   ,intent(out) :: dzdx,dzdy        ! arguments
    real                               :: zvalue           ! result

    if (wx%i1 == 0 .or. wy%i1 == 0) then
                         zvalue = 0.0
      if (present(dzdx)) dzdx   = 0.0
      if (present(dzdy)) dzdy   = 0.0
      return
    end if

    zvalue =                                                                  &
       wx%w1 * (zarray(wx%i1,wy%i1) * wy%w1 + zarray(wx%i1,wy%i2) * wy%w2) +  &
       wx%w2 * (zarray(wx%i2,wy%i1) * wy%w1 + zarray(wx%i2,wy%i2) * wy%w2)

    if (present(dzdx)) then
      dzdx = wx%wg * ( (zarray(wx%i2,wy%i1) - zarray(wx%i1,wy%i1)) * wy%w1 +  &
                       (zarray(wx%i2,wy%i2) - zarray(wx%i1,wy%i2)) * wy%w2  )
    end if

    if (present(dzdy)) then
      dzdy = wy%wg * ( (zarray(wx%i1,wy%i2) - zarray(wx%i1,wy%i1)) * wx%w1 +  &
                       (zarray(wx%i2,wy%i2) - zarray(wx%i2,wy%i1)) * wx%w2  )
    end if
    return
    end function terputil_get_answer_2d


!!----------------------- terputil interp 1d even ---------------------------!!
!!----------------------- terputil interp 1d even ---------------------------!!
!!----------------------- terputil interp 1d even ---------------------------!!


    function terputil_interp_1d_even (xcoord,xfirst,xinc,nx,zarray,dzdx) &
                              result (zvalue)
    implicit none
    real             ,intent(in)  :: xcoord,xfirst,xinc        ! arguments
    integer          ,intent(in)  :: nx                        ! arguments
    real             ,intent(in)  :: zarray(:)                 ! arguments
    real ,optional   ,intent(out) :: dzdx                      ! arguments
    real                          :: zvalue                    ! result
    type(terputil_weights)        :: wx                        ! local

    wx     = terputil_get_weights_even (xcoord,xfirst,xinc,nx)
    zvalue = terputil_get_answer_1d    (wx,zarray,dzdx)
    return
    end function terputil_interp_1d_even


!!----------------------- terputil interp 1d uneven -------------------------!!
!!----------------------- terputil interp 1d uneven -------------------------!!
!!----------------------- terputil interp 1d uneven -------------------------!!


    function terputil_interp_1d_uneven (xcoord,xarray,nx,zarray,dzdx) &
                                result (zvalue)
    implicit none
    real             ,intent(in)  :: xcoord,xarray(:)          ! arguments
    integer          ,intent(in)  :: nx                        ! arguments
    real             ,intent(in)  :: zarray(:)                 ! arguments
    real ,optional   ,intent(out) :: dzdx                      ! arguments
    real                          :: zvalue                    ! result
    type(terputil_weights)        :: wx                        ! local

    wx     = terputil_get_weights_uneven (xcoord,xarray,nx)
    zvalue = terputil_get_answer_1d      (wx,zarray,dzdx)
    return
    end function terputil_interp_1d_uneven


!!----------------------- terputil interp 2d even ---------------------------!!
!!----------------------- terputil interp 2d even ---------------------------!!
!!----------------------- terputil interp 2d even ---------------------------!!


    function terputil_interp_2d_even (xcoord,xfirst,xinc,nx, &
                                      ycoord,yfirst,yinc,ny, &
                                      zarray,dzdx,dzdy)      &
                              result (zvalue)
    implicit none
    real             ,intent(in)  :: xcoord,xfirst,xinc    ! arguments
    real             ,intent(in)  :: ycoord,yfirst,yinc    ! arguments
    integer          ,intent(in)  :: nx,ny                 ! arguments
    real             ,intent(in)  :: zarray(:,:)           ! arguments (nx,ny)
    real ,optional   ,intent(out) :: dzdx,dzdy             ! arguments
    real                          :: zvalue                ! result
    type(terputil_weights)        :: wx,wy                 ! local

    wx     = terputil_get_weights_even (xcoord,xfirst,xinc,nx)
    wy     = terputil_get_weights_even (ycoord,yfirst,yinc,ny)
    zvalue = terputil_get_answer_2d    (wx,wy,zarray,dzdx,dzdy)
    return
    end function terputil_interp_2d_even


!!----------------------- terputil interp 2d uneven -------------------------!!
!!----------------------- terputil interp 2d uneven -------------------------!!
!!----------------------- terputil interp 2d uneven -------------------------!!


    function terputil_interp_2d_uneven (xcoord,xarray,nx, &
                                        ycoord,yarray,ny, &
                                        zarray,dzdx,dzdy) &
                                result (zvalue)
    implicit none
    real             ,intent(in)  :: xcoord,xarray(:)      ! arguments
    real             ,intent(in)  :: ycoord,yarray(:)      ! arguments
    integer          ,intent(in)  :: nx,ny                 ! arguments
    real             ,intent(in)  :: zarray(:,:)           ! arguments (nx,ny)
    real ,optional   ,intent(out) :: dzdx,dzdy             ! arguments
    real                          :: zvalue                ! result
    type(terputil_weights)        :: wx,wy                 ! local

    wx     = terputil_get_weights_uneven (xcoord,xarray,nx)
    wy     = terputil_get_weights_uneven (ycoord,yarray,ny)
    zvalue = terputil_get_answer_2d      (wx,wy,zarray,dzdx,dzdy)
    return
    end function terputil_interp_2d_uneven


!!------------------ terputil replace nils real --------------------------!!
!!------------------ terputil replace nils real --------------------------!!
!!------------------ terputil replace nils real --------------------------!!


      subroutine terputil_replace_nils_real (array,n,mode)
      implicit none
      real       ,intent(inout)       :: array(:)                ! arguments
      integer    ,intent(in)          :: n                       ! arguments
      integer    ,intent(in),optional :: mode                    ! arguments
      integer                         :: i,ilo,iup,ia,j,mode2    ! local
      real                            :: temp,dif                ! local

!----------obtain the mode to use.

      if (present(mode)) then
           mode2 = mode
      else
           mode2 = TERPUTIL_FLATKEEP
      end if

      if (mode2 /= TERPUTIL_FLATKEEP .and. &
          mode2 /= TERPUTIL_FLAT     .and. &
          mode2 /= TERPUTIL_SLOPING  .and. &
          mode2 /= TERPUTIL_DOWN     .and. &
          mode2 /= TERPUTIL_ZERO     .and. &
          mode2 /= TERPUTIL_ONE) mode2 = TERPUTIL_FLATKEEP

!----------replace all nils by zero or one.

      if (n == 0) return
      if (mode2 == TERPUTIL_ZERO) then
           do i = 1,n
                if (array(i) == FNIL) array(i) = 0.0
           end do
           return
      else if (mode2 == TERPUTIL_ONE) then
           do i = 1,n
                if (array(i) == FNIL) array(i) = 1.0
           end do
           return
      end if

!----------find first non-nil value.

      do i = 1,n
           if (array(i) /= FNIL) go to 11
      end do

!----------we have all nils.

      if (mode2 /= TERPUTIL_FLATKEEP) array(1:n) = 0.0
      return

!----------we have found the first non-nil value.

11    if (n == 1) return
      ilo = i

!----------downward repetition.

      if (mode2 == TERPUTIL_DOWN) then
           array(1) = array(ilo)
           do i = 2,n
                if (array(i) == FNIL) array(i) = array(i-1)
           end do
           return
      end if

!----------find last non-nil value.

      do i = n,1,-1
           if (array(i) /= FNIL) go to 13
      end do
!!!   (we should not get to here)
13    iup = i
      if (iup >  ilo) go to 15

!----------we have only one non-nil value.

      temp = array(ilo)
      array(1:n) = temp
      return

!----------do the interpolation.

15    do i = ilo+1,iup
           if (array(i-1) /= FNIL .and. array(i) == FNIL) then
                ia = i-1
                temp = array(ia)
           else if (array(i-1) == FNIL .and. array(i) /= FNIL) then
                dif = (array(i)-temp)/(i-ia)
                do j = ia+1,i-1
                     array(j) = temp+(j-ia)*dif
                end do
           end if
      end do

!----------do the extrapolation at the beginning.

      if (ilo >  1) then
           temp = array(ilo)
           if (mode2 == TERPUTIL_FLAT .or. mode2 == TERPUTIL_FLATKEEP) then
                do i = 1,ilo-1
                     array(i) = temp
                end do
           else
                dif = array(ilo+1)-array(ilo)
                do i = 1,ilo-1
                     array(i) = temp+(i-ilo)*dif
                end do
           end if
      end if

!----------do the extrapolation at the end.

      if (iup <  n) then
           temp = array(iup)
           if (mode2 == TERPUTIL_FLAT .or. mode2 == TERPUTIL_FLATKEEP) then
                do i = iup+1,n
                     array(i) = temp
                end do
           else
                dif = array(iup)-array(iup-1)
                do i = iup+1,n
                     array(i) = temp+(i-iup)*dif
                end do
           end if
      end if
      return
      end subroutine terputil_replace_nils_real


!!------------------ terputil replace nils integer -----------------------!!
!!------------------ terputil replace nils integer -----------------------!!
!!------------------ terputil replace nils integer -----------------------!!


      subroutine terputil_replace_nils_integer (array,n,mode)
      implicit none
      integer         ,intent(inout)       :: array(:)            ! arguments
      integer         ,intent(in)          :: n                   ! arguments
      integer         ,intent(in),optional :: mode                ! arguments
      real                                 :: temp(n)             ! local
      integer                              :: i                   ! local

      do i = 1,n
          if (array(i) == INIL) then
               temp(i) = FNIL
          else
               temp(i) = array(i)
          end if
      end do

      call terputil_replace_nils_real (temp,n,mode)

      do i = 1,n
          if (temp(i) == FNIL) then
               array(i) = INIL
          else
               array(i) = temp(i)
          end if
      end do
      return
      end subroutine terputil_replace_nils_integer


!!!-------------------- terputil replace nils real -------------------------!!
!!!-------------------- terputil replace nils real -------------------------!!
!!!-------------------- terputil replace nils real -------------------------!!
!
!
!      subroutine terputil_replace_nils_real (array,n)
!      implicit none
!      real             ,intent(inout) :: array(:)             ! arguments
!      integer          ,intent(in)    :: n                    ! arguments
!      integer                         :: indx,ia,ib,ic        ! local
!      real                            :: factor               ! local
!
!!----------find first non-nil value.
!
!      do indx = 1,n
!        if (array(indx) /= FNIL) go to 10
!      end do
!      return
!10    array(1) = array(indx)
!
!!----------find last non-nil value.
!
!      do indx = n, 1, -1
!        if (array(indx) /= FNIL) go to 20
!      end do
!      return
!20    array(n) = array(indx)
!
!!----------do the work.
!
!      do indx = 2, n
!        if (array(indx-1) /= FNIL .and. array(indx) == FNIL) then
!          ia = indx-1
!        else if (array(indx-1) == FNIL .and. array(indx) /= FNIL) then
!          ib = indx
!          do ic = ia+1, ib-1
!            factor = (array(ib) - array(ia)) / real(ib-ia)
!            array(ic) = array(ia) + (ic-ia) * factor
!          end do
!        endif
!      end do
!      return
!      end subroutine terputil_replace_nils_real
!
!
!!!-------------------- terputil replace nils integer -----------------------!!
!!!-------------------- terputil replace nils integer -----------------------!!
!!!-------------------- terputil replace nils integer -----------------------!!
!
!
!      subroutine terputil_replace_nils_integer (array,n)
!      implicit none
!      integer          ,intent(inout) :: array(:)             ! arguments
!      integer          ,intent(in)    :: n                    ! arguments
!      integer                         :: indx,ia,ib,ic        ! local
!      real                            :: factor               ! local
!
!!----------find first non-nil value.
!
!      do indx = 1,n
!        if (array(indx) /= INIL) go to 10
!      end do
!      return
!10    array(1) = array(indx)
!
!!----------find last non-nil value.
!
!      do indx = n, 1, -1
!        if (array(indx) /= INIL) go to 20
!      end do
!      return
!20    array(n) = array(indx)
!
!!----------do the work.
!
!      do indx = 2, n
!        if (array(indx-1) /= INIL .and. array(indx) == INIL) then
!          ia = indx-1
!        else if (array(indx-1) == INIL .and. array(indx) /= INIL) then
!          ib = indx
!          do ic = ia+1, ib-1
!            factor = real(array(ib) - array(ia)) / real(ib-ia)
!            array(ic) = array(ia) + nint((ic-ia) * factor)
!          end do
!        endif
!      end do
!      return
!      end subroutine terputil_replace_nils_integer

 
!!----------------------- terputil replace nilx ------------------------------!!
!!----------------------- terputil replace nilx ------------------------------!!
!!----------------------- terputil replace nilx ------------------------------!!


      subroutine terputil_replace_nilx (nx, ny, statics, only)
      implicit none
      integer,         intent(in)    :: nx,ny                 ! arguments
      real,            intent(inout) :: statics(nx,ny)        ! arguments
      logical,optional,intent(in)    :: only                  ! arguments
      integer                        :: ix, iy                ! local

      do iy = 1, ny
        call terputil_replace_nils (statics(1:nx,iy), nx)
      enddo

      if (present(only)) then ; if (only) return ; end if

      do ix = 1, nx
        call terputil_replace_nils (statics(ix,1:ny), ny)
      enddo
      return
      end subroutine terputil_replace_nilx


!!----------------------- terputil replace nily ------------------------------!!
!!----------------------- terputil replace nily ------------------------------!!
!!----------------------- terputil replace nily ------------------------------!!
 

      subroutine terputil_replace_nily (nx, ny, statics, only)
      implicit none
      integer,         intent(in)    :: nx,ny                 ! arguments
      real,            intent(inout) :: statics(nx,ny)        ! arguments
      logical,optional,intent(in)    :: only                  ! arguments
      integer                        :: ix, iy                ! local

      do ix = 1, nx
        call terputil_replace_nils (statics(ix,1:ny), ny)
      enddo

      if (present(only)) then ; if (only) return ; end if

      do iy = 1, ny
        call terputil_replace_nils (statics(1:nx,iy), nx)
      enddo
      return
      end subroutine terputil_replace_nily


!!----------------------- terputil binary search ---------------------------!!
!!----------------------- terputil binary search ---------------------------!!
!!----------------------- terputil binary search ---------------------------!!


      subroutine terputil_binary_search (xcoord,xarray,nx,i1,i2,x1,x2,w1,w2,wg)
      implicit none
      real,    intent(in)           :: xcoord,xarray(:)          ! argument
      integer, intent(in)           :: nx                        ! argument
      integer, intent(out),optional :: i1,i2                     ! argument
      real   , intent(out),optional :: x1,x2                     ! argument
      real   , intent(out),optional :: w1,w2,wg                  ! argument
      integer                       :: indx1,indx2,indx3         ! local
      real                          :: xrange,xwidth             ! local

! Test for no points:

      if (nx == 0) then
        if (present(i1)) i1 = 0
        if (present(i2)) i2 = 0
        if (present(x1)) x1 = 0.0
        if (present(x2)) x2 = 0.0
        if (present(w1)) w1 = 0.5
        if (present(w2)) w2 = 0.5
        if (present(wg)) wg = 0.0
        return
      end if

! The sign of xrange tells if xarray is in ascending or decending order:

      xrange = xarray(nx) - xarray(1)

! If xrange is 0.0 we're already done:

      if (xrange == 0.0) then
        if (present(i1)) i1 = 1
        if (present(i2)) i2 = 1
        if (present(x1)) x1 = xarray(1)
        if (present(x2)) x2 = xarray(1)
        if (present(w1)) w1 = 0.5
        if (present(w2)) w2 = 0.5
        if (present(wg)) wg = 0.0
        return
      end if

! Test for extrapolation:

      if (xrange*(xarray(1)-xcoord) >= 0.0) then
        if (present(i1)) i1 = 1
        if (present(i2)) i2 = 1
        if (present(x1)) x1 = xarray(1)
        if (present(x2)) x2 = xarray(1)
        if (present(w1)) w1 = 0.5
        if (present(w2)) w2 = 0.5
        if (present(wg)) wg = 0.0
        return
      end if

      if (xrange*(xarray(nx)-xcoord) <= 0.0) then
        if (present(i1)) i1 = nx
        if (present(i2)) i2 = nx
        if (present(x1)) x1 = xarray(nx)
        if (present(x2)) x2 = xarray(nx)
        if (present(w1)) w1 = 0.5
        if (present(w2)) w2 = 0.5
        if (present(wg)) wg = 0.0
        return
      end if

! Do a binary search into xarray to find indices for values bracketing xcoord:

      indx1 = 1
      indx2 = nx
      do
        if (indx2 - indx1 <= 1) exit
        indx3 = (indx1 + indx2) / 2
        if(xrange * (xarray(indx3)-xcoord) < 0.0) then
          indx1 = indx3
        else
          indx2 = indx3
        end if
      end do

      if (present(i1)) i1 = indx1
      if (present(i2)) i2 = indx2
      if (present(x1)) x1 = xarray(indx1)
      if (present(x2)) x2 = xarray(indx2)

      xwidth = xarray(indx2) - xarray(indx1)

      if (xwidth == 0.0) then
        if (present(w1)) w1 = 0.5
        if (present(w2)) w2 = 0.5
        if (present(wg)) wg = 0.0
      else
        if (present(w1)) w1 = (xarray(indx2) - xcoord) / xwidth
        if (present(w2)) w2 = (xcoord - xarray(indx1)) / xwidth
        if (present(wg)) wg =                      1.0 / xwidth
      end if
      return
      end subroutine terputil_binary_search


!!------------------------- terputil fracdist -------------------------------!!
!!------------------------- terputil fracdist -------------------------------!!
!!------------------------- terputil fracdist -------------------------------!!


      function terputil_fracdist (xcoord,xa,xb) result (fracdist)
      implicit none
      real, intent(in) :: xcoord,xa,xb                     ! arguments
      real             :: fracdist                         ! result

      if (xa /= xb) then
        fracdist = (xcoord-xa)/(xb-xa)
      else
        fracdist = 0.5
      endif
      return
      end function terputil_fracdist


!!------------------------- terputil 1d weights -----------------------------!!
!!------------------------- terputil 1d weights -----------------------------!!
!!------------------------- terputil 1d weights -----------------------------!!


      subroutine terputil_1d_weights (xcoord,xa,xb, wa,wb)
      implicit none
      real                ,intent(in)    :: xcoord,xa,xb       ! arguments
      real                ,intent(out)   :: wa,wb              ! arguments

      if (xa == xb) then
           wa = 0.5
           wb = 0.5
      else
           wa = (xb - xcoord) / (xb - xa)
           wb = (xcoord - xa) / (xb - xa)
      end if
      return
      end subroutine terputil_1d_weights


!!------------------------- terputil 2d weights -----------------------------!!
!!------------------------- terputil 2d weights -----------------------------!!
!!------------------------- terputil 2d weights -----------------------------!!


      subroutine terputil_2d_weights &
                             (xcoord,xa,xb, ycoord,ya,yb, waa,wab,wba,wbb)
      implicit none
      real                ,intent(in)    :: xcoord,xa,xb       ! arguments
      real                ,intent(in)    :: ycoord,ya,yb       ! arguments
      real                ,intent(out)   :: waa,wab,wba,wbb    ! arguments
      real                               :: wxa,wxb,wya,wyb    ! local

      call terputil_1d_weights (xcoord,xa,xb, wxa,wxb)
      call terputil_1d_weights (ycoord,ya,yb, wya,wyb)

      waa = wxa * wya
      wab = wxa * wyb
      wba = wxb * wya
      wbb = wxb * wyb
      return
      end subroutine terputil_2d_weights


!!------------------------- terputil root -------------------------------!!
!!------------------------- terputil root -------------------------------!!
!!------------------------- terputil root -------------------------------!!


      function terputil_root (xcoord,xa,xb,ya,yb) result (ycoord)
      implicit none
      real, intent(in) :: xcoord,xa,xb,ya,yb               ! arguments
      real             :: ycoord                           ! result

      if (xa /= xb) then
        ycoord = ya + (xcoord-xa) * (yb-ya)/(xb-xa)
      else
        ycoord = 0.5*(ya+yb)
      endif
      return
      end function terputil_root


!!------------------------- terputil resample -------------------------------!!
!!------------------------- terputil resample -------------------------------!!
!!------------------------- terputil resample -------------------------------!!


    subroutine terputil_resample_even_even (xfirst1, xinc1, nx1, zarray1, &
                                            xfirst2, xinc2, nx2, zarray2)
    implicit none
    real   , intent(in)  :: xfirst1,xinc1,xfirst2,xinc2    ! arguments
    integer, intent(in)  :: nx1,nx2                        ! arguments
    real   , intent(in)  :: zarray1(:)                     ! arguments (nx1)
    real   , intent(out) :: zarray2(:)                     ! arguments (nx2)
    integer              :: ix2                            ! local
    real                 :: xcoord                         ! local

    do ix2 = 1, nx2
        xcoord       = xfirst2 + float(ix2-1) * xinc2
        zarray2(ix2) = terputil_interp (xcoord, xfirst1, xinc1, nx1, zarray1)
    end do
    return
    end subroutine terputil_resample_even_even


    subroutine terputil_resample_even_uneven (xfirst1, xinc1, nx1, zarray1, &
                                              xarray2,        nx2, zarray2)
    implicit none
    real   , intent(in)  :: xfirst1,xinc1,xarray2(:)       ! arguments
    integer, intent(in)  :: nx1,nx2                        ! arguments
    real   , intent(in)  :: zarray1(:)                     ! arguments (nx1)
    real   , intent(out) :: zarray2(:)                     ! arguments (nx2)
    integer              :: ix2                            ! local
    real                 :: xcoord                         ! local

    do ix2 = 1, nx2
        xcoord       = xarray2(ix2)
        zarray2(ix2) = terputil_interp (xcoord, xfirst1, xinc1, nx1, zarray1)
    end do
    return
    end subroutine terputil_resample_even_uneven


    subroutine terputil_resample_uneven_even (xarray1,        nx1, zarray1, &
                                              xfirst2, xinc2, nx2, zarray2)
    implicit none
    real   , intent(in)  :: xarray1(:),xfirst2,xinc2       ! arguments
    integer, intent(in)  :: nx1,nx2                        ! arguments
    real   , intent(in)  :: zarray1(:)                     ! arguments (nx1)
    real   , intent(out) :: zarray2(:)                     ! arguments (nx2)
    integer              :: ix2                            ! local
    real                 :: xcoord                         ! local

    do ix2 = 1, nx2
        xcoord       = xfirst2 + float(ix2-1) * xinc2
        zarray2(ix2) = terputil_interp (xcoord, xarray1,        nx1, zarray1)
    end do
    return
    end subroutine terputil_resample_uneven_even


    subroutine terputil_resample_uneven_uneven (xarray1,        nx1, zarray1, &
                                                xarray2,        nx2, zarray2)
    implicit none
    real   , intent(in)  :: xarray1(:),xarray2(:)          ! arguments
    integer, intent(in)  :: nx1,nx2                        ! arguments
    real   , intent(in)  :: zarray1(:)                     ! arguments (nx1)
    real   , intent(out) :: zarray2(:)                     ! arguments (nx2)
    integer              :: ix2                            ! local
    real                 :: xcoord                         ! local

    do ix2 = 1, nx2
        xcoord       = xarray2(ix2)
        zarray2(ix2) = terputil_interp (xcoord, xarray1,        nx1, zarray1)
    end do
    return
    end subroutine terputil_resample_uneven_uneven


!!-------------------------- terputil fastsamp ---------------------------!!
!!-------------------------- terputil fastsamp ---------------------------!!
!!-------------------------- terputil fastsamp ---------------------------!!


      subroutine terputil_fastsamp (xarray1,        nx1, zarray1, &
                                    xfirst2, xinc2, nx2, zarray2)
      implicit none
      real   , intent(in)  :: xarray1(:),xfirst2,xinc2       ! arguments
      integer, intent(in)  :: nx1,nx2                        ! arguments
      real   , intent(in)  :: zarray1(:)                     ! arguments (nx1)
      real   , intent(out) :: zarray2(:)                     ! arguments (nx2)
      integer              :: ix1,ix2                        ! local
      real                 :: xcoord,slope                   ! local

      ix1 = 1
      do ix2 = 1, nx2
          xcoord = xfirst2 + (ix2-1) * xinc2
          do
               if (xcoord <= xarray1(ix1)) then
                    if (ix1 == 1) then
                         zarray2(ix2) = zarray1(ix1)
                    else
                         slope = (zarray1(ix1) - zarray1(ix1-1)) /  &
                                 (xarray1(ix1) - xarray1(ix1-1))
                         zarray2(ix2) = zarray1(ix1-1) +  &
                                        (xcoord - xarray1(ix1-1)) * slope
                    end if
                    exit
               else if (ix1 == nx1) then
                    zarray2(ix2) = zarray1(ix1)
                    exit
               else
                    ix1 = ix1 + 1
               end if
          end do
      end do
      return
      end subroutine terputil_fastsamp


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module terputil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

