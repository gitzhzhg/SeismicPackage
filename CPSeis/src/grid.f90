
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ grid.f90 ----------------------------------!!
!!------------------------------ grid.f90 ----------------------------------!!
!!------------------------------ grid.f90 ----------------------------------!!


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
! Name       : GRID
! Category   : math
! Written    : 1999-06-16   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Grid transformation primitive.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This primitive contains a data structure which defines an orthogonal
! coordinate transform from the survey coordinate system to the grid
! coordinate system.  Subroutines are available to get and set individual
! data structure variables, define or adjust the coordinate transform in
! various ways, do coordinate transformations, and do CMP binning.
!
! The order of the member variables dx11,dx21,dx12,dx22 matches the
! consecutive order of the correponding 4-element CPS array in memory.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                       INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented here, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                       INITIALIZE A GRID TRANSFORM
!
!                                     o
!              call grid_initialize (obj)
!
! type(grid_struct) obj = grid transformation structure.
!
! The grid transformation structure is passed by reference (by address),
! not as a pointer, and is not allocated or deallocated by any subroutines
! in this module.
!
! This subroutine is to be called to define the grid transform before
! initially using it or accessing any of its member variables.  This
! subroutine can also be called to reset it to its initial values.  The
! initial values correspond to an identity (do-nothing) transformation.
!
!-------------------------------------------------------------------------------
!                 TEST TWO DATA STRUCTURES FOR EQUALITY
!
!                                    i     i
!             equal = grid_equal   (obj1, obj2)
!           unequal = grid_unequal (obj1, obj2)
!
! type(grid_struct) obj1 = any grid transformation structure.
! type(grid_struct) obj2 = another grid transformation structure.
! logical          equal = true if the two data structures are identical.
! logical        unequal = true if the two data structures are different.
!
! Or simply do a comparison like this:
!
!                       obj1 == obj2
!                       obj1 /= obj2
!
! These tests for equality or inequality will do almost-equal comparisons
! and will declare two data structures to be identical if they differ by
! only very small errors.  The direct comparisons using == or /= use the
! functions GRID_EQUAL and GRID_UNEQUAL and therefore give the same results
! as the two functions.
!
!-------------------------------------------------------------------------------
!                    COPY ONE DATA STRUCTURE TO ANOTHER
!
! Simply do a copy like this:
!
!                       obj_output = obj_input
!
! type(grid_struct) obj_input  = input  grid transformation structure.
! type(grid_struct) obj_output = output grid transformation structure.
!
! Since the data structure does not contain any pointers, copying the contents
! of one data structure to another does not involve any requirement to
! allocate and copy pointees.
!
!-------------------------------------------------------------------------------
!               GET INDIVIDUAL VALUES FROM THE DATA STRUCTURE
!
! Normally independent variables:
!
!             o                                 i
!           xorigin = grid_get_xorigin        (obj)
!           yorigin = grid_get_yorigin        (obj)
!           angle   = grid_get_rotation_angle (obj)
!           xwidth  = grid_get_xgrid_width    (obj)
!           ywidth  = grid_get_ygrid_width    (obj)
!           hand    = grid_get_handedness     (obj)
!           right   = grid_is_right_handed    (obj)
!           left    = grid_is_left_handed     (obj)
!
!                                               i      o        o
!           call      grid_get_origin         (obj, xorigin, yorigin)
!           call      grid_get_widths         (obj, xwidth,  ywidth)
!
! Normally dependent variables:
!
!             o                                   i
!           cosa      = grid_get_cosine_angle   (obj)
!           sina      = grid_get_sine_angle     (obj)
!           dx11      = grid_get_dx11           (obj)
!           dx21      = grid_get_dx21           (obj)
!           dx12      = grid_get_dx12           (obj)
!           dx22      = grid_get_dx22           (obj)
!           dn11      = grid_get_dn11           (obj)
!           dn21      = grid_get_dn21           (obj)
!           dn12      = grid_get_dn12           (obj)
!           dn22      = grid_get_dn22           (obj)
!           det       = grid_get_determinant    (obj)
!
!                                                 i    o    o    o    o
!           call        grid_get_dx             (obj, dx11,dx21,dx12,dx22)
!           call        grid_get_dn             (obj, dn11,dn21,dn12,dn22)
!
! type(grid_struct) obj     = grid transformation structure.
! double precision  xorigin = X coord of grid origin (feet or meters).
! double precision  yorigin = Y coord of grid origin (feet or meters).
! double precision  angle   = rotation angle of +Xgrid axis from
!                               +Xsurvey axis (counter-clockwise looking
!                               down toward the earth) (degrees).
! double precision  xwidth  = X  (inline)   grid spacing (feet or meters).
! double precision  ywidth  = Y (crossline) grid spacing (feet or meters).
! integer           hand    = +1 (right-handed) or -1 (left-handed).
! logical           right   = true if transform is right-handed.
! logical           left    = true if transform is left-handed.
! double precision  sina    = sine   of the rotation angle.
! double precision  cosa    = cosine of the rotation angle.
! double precision  dx...   = forward rotation matrix (dx11,dx21,dx12,dx22).
! double precision  dn...   = inverse rotation matrix (dn11,dn21,dn12,dn22).
! double precision  det     = determinant of forward rotation matrix.
!
!-------------------------------------------------------------------------------
!               SET INDIVIDUAL VALUES INTO THE DATA STRUCTURE
!
! Normally independent variables:
!
!                                          b      i
!           call grid_set_xorigin        (obj, xorigin)
!           call grid_set_yorigin        (obj, yorigin)
!           call grid_set_rotation_angle (obj, angle)
!           call grid_set_xgrid_width    (obj, xwidth)
!           call grid_set_ygrid_width    (obj, ywidth)
!           call grid_set_handedness     (obj, hand)
!           call grid_set_right_handed   (obj)
!           call grid_set_left_handed    (obj)
!
!                                          b      i        i
!           call grid_set_origin         (obj, xorigin, yorigin)
!           call grid_set_widths         (obj, xwidth,  ywidth)
!
!                                b     i       i       i     i      i    i
!  call grid_set_transform     (obj,xorigin,yorigin,angle,xwidth,ywidth,hand)
!  call grid_set_right_handed_transform
!                              (obj,xorigin,yorigin,angle,xwidth,ywidth)
!  call grid_set_left_handed_transform
!                              (obj,xorigin,yorigin,angle,xwidth,ywidth)
!
! Normally dependent variables:
!
!                                          b    i
!           call grid_set_dx11           (obj, dx11)
!           call grid_set_dx21           (obj, dx21)
!           call grid_set_dx12           (obj, dx12)
!           call grid_set_dx22           (obj, dx22)
!           call grid_set_dn11           (obj, dn11)
!           call grid_set_dn21           (obj, dn21)
!           call grid_set_dn12           (obj, dn12)
!           call grid_set_dn22           (obj, dn22)
!
!                                          b    i    i    i    i
!           call grid_set_dx             (obj, dx11,dx21,dx12,dx22)
!           call grid_set_dn             (obj, dn11,dn21,dn12,dn22)
!
! type(grid_struct) obj     = grid transformation structure.
! double precision  xorigin = X coord of grid origin (feet or meters).
! double precision  yorigin = Y coord of grid origin (feet or meters).
! double precision  angle   = rotation angle of +Xgrid axis from
!                               +Xsurvey axis (counter-clockwise looking
!                               down toward the earth) (degrees).
! double precision  xwidth  = X  (inline)   grid spacing (feet or meters).
! double precision  ywidth  = Y (crossline) grid spacing (feet or meters).
! integer           hand    = +1 (right-handed) or -1 (left-handed).
! double precision  dx...   = forward rotation matrix (dx11,dx21,dx12,dx22).
! double precision  dn...   = inverse rotation matrix (dn11,dn21,dn12,dn22).
! integer           index   = index (1 thru 4) of desired matrix element.
!
! Calling any of these routines to set any values will adjust all other
! values to be consistent with the new value.  For example, setting any
! normally independent variable (except for the origin) will recalculate
! the forward and reverse rotation matrices.  Setting any one element in
! the forward or reverse rotation matrix will adjust the transform to remain
! orthogonal and will reset the normally independent variables which might
! have changed.
!
!-------------------------------------------------------------------------------
!                        GET TRANSFORMED COORDINATES
!
!          o                                i     i      i
!        xloc   =  grid_get_xsurvey_coord (obj, xgrid, ygrid)
!        yloc   =  grid_get_ysurvey_coord (obj, xgrid, ygrid)
!        xgrid  =  grid_get_xgrid_coord   (obj, xloc , yloc )
!        ygrid  =  grid_get_ygrid_coord   (obj, xloc , yloc )
!
!                                   i     i      i      o      o
!     call grid_get_survey_coords (obj, xgrid, ygrid, xloc , yloc )
!     call grid_get_grid_coords   (obj, xloc , yloc , xgrid, ygrid)
!
! type(grid_struct)  obj = grid transformation structure.
! real or double    xloc = X coord in survey coord system (feet or meters).
! real or double    yloc = Y coord in survey coord system (feet or meters).
! real or double   xgrid = X coord in  grid  coord system.
! real or double   ygrid = Y coord in  grid  coord system.
!
! The real or double precision arguments must be either all real or all
! double precision in any one function or subroutine call.  The values
! returned from the functions are actually always double precision, but
! of course they can be assigned to real variables.
!
!-------------------------------------------------------------------------------
!                            DO CMP BINNING
!
!            o                                 i     i      i
!        xbin_number =  grid_get_xbin_number (obj, xloc , yloc )
!        ybin_number =  grid_get_ybin_number (obj, xloc , yloc )
!        xbin_center =  grid_get_xbin_center (obj, xgrid, ygrid)
!        ybin_center =  grid_get_ybin_center (obj, xgrid, ygrid)
!
!                                i     i      i        o            o
!    call grid_get_bin_numbers (obj, xloc , yloc , xbin_number, ybin_number)
!    call grid_get_bin_centers (obj, xgrid, ygrid, xbin_center, ybin_center)
!
!
! type(grid_struct)   obj = grid transformation structure.
! real or double     xloc = X coord in survey coord system (feet or meters).
! real or double     yloc = Y coord in survey coord system (feet or meters).
! real or double    xgrid = X coord in  grid  coord system.
! real or double    ygrid = Y coord in  grid  coord system.
! integer     xbin_number = bin number in the X direction (integerized xgrid).
! integer     ybin_number = bin number in the Y direction (integerized ygrid).
! real or dbl xbin_center = bin center in the X direction (survey system).
! real or dbl ybin_center = bin center in the Y direction (survey system).
!
!
! A mid point is the location half way between the source and receiver of a
! seismic trace.  A common mid point gather (CMP gather) is a collection of
! all traces with midpoints falling into the came bin.
!
! Binning is defined in such a way that the center of each bin has X and Y
! grid coordinates which are exact integers.
!
! A bin number is the X or Y grid coordinate of the center of a bin, which
! is an integer because of the way binning is defined above.  Therefore a
! bin number is simply the grid coordinate rounded to the nearest integer.
! All traces in a CMP gather have the same bin number.
!
! A bin center is the X or Y survey coordinate of the center of a bin.
!
! The real or double precision arguments must be either all real or all
! double precision in any one function or subroutine call.  The values
! returned from the functions are actually always double precision, but
! of course they can be assigned to real variables.
!
!-------------------------------------------------------------------------------
!             USEFUL SUBROUTINES TO DEFINE THE COORDINATE SYSTEM
!
!                                      b     i     i      i     i
!  call grid_define_origin           (obj, xgrid,ygrid, xloc ,yloc)
!  call grid_define_rotation_angle   (obj, xloc1,yloc1, xloc2,yloc2)
!
!                                      b     i     i
!  call grid_define_origin_and_angle (obj, xgrid,ygrid,
!                                          xloc1,yloc1, xloc2,yloc2)
!                                            i     i      i     i
!
!                                      b    i     i
!  call grid_refine_bin_center       (obj, xloc ,yloc)
!  call grid_refine_rotation_angle   (obj, xloc ,yloc)
!  call grid_increment_grid_coords   (obj, xstep,ystep)
!
!
!  call grid_define_transform       o      i       i     i      i      i
!  call grid_define_transform_alt (obj, npoints, xlocs,ylocs, xgrids,ygrids,
!                                  rxlocs,rylocs, rxgrids,rygrids, errors)
!                                    o      o        o       o       o   
!                                   opt    opt      opt     opt     opt 
!
!
! type(grid_struct)    obj = grid transformation structure (not a pointer).
! double              xloc = X coord in survey coord system (feet or meters).
! double              yloc = Y coord in survey coord system (feet or meters).
! double             xgrid = X coord in  grid  coord system.
! double             ygrid = Y coord in  grid  coord system.
! double             xstep = increment for X coord in grid coord system.
! double             ystep = increment for Y coord in grid coord system.
! integer          npoints = number of points in arrays.
! double    xlocs(npoints) = X coord in survey coord system (feet or meters).
! double    ylocs(npoints) = Y coord in survey coord system (feet or meters).
! double   xgrids(npoints) = X coord in  grid  coord system.
! double   ygrids(npoints) = Y coord in  grid  coord system.
! double   rxlocs(npoints) = residuals in xloc(*) calculated from (xgrid,ygrid).
! double   rylocs(npoints) = residuals in yloc(*) calculated from (xgrid,ygrid).
! double  rxgrids(npoints) = residuals in xgrid(*) calculated from (xloc,yloc).
! double  rygrids(npoints) = residuals in ygrid(*) calculated from (xloc,yloc).
! type(grid_errors) errors = standard errors (see below).
!
!
! grid_define_origin:
!
!    ---Places origin so that specified (xgrid,ygrid) is at
!         location (xloc,yloc).
!    ---Only the variables XORIGIN and YORIGIN are changed.
!
! grid_define_rotation_angle:
!
!    ---Sets rotation angle to the direction from (xloc1,yloc1)
!         to (xloc2,yloc2).
!    ---Only the independent variable ANGLE is changed.
!    ---Some dependent variables are also changed.
!
! grid_define_origin_and_angle:
!
!    ---Places origin so that specified (xgrid,ygrid) is at
!         location (xloc1,yloc1).
!    ---Sets rotation angle to the direction from (xloc1,yloc1)
!         to (xloc2,yloc2).
!    ---Only the independent variables XORIGIN, YORIGIN, and
!         ANGLE are changed.
!    ---Some dependent variables are also changed.
!
! grid_refine_bin_center:
!
!    ---Adjusts the origin slightly so that specified (xloc,yloc)
!         becomes the new center of the bin in which the point resides.
!    ---Only the variables XORIGIN and YORIGIN are changed.
!
! grid_refine_rotation_angle:
!
!    ---Adjusts the rotation angle slightly so that specified (xloc,yloc)
!         becomes a point on a line from the origin through the new center
!         of the bin in which the point resides.
!    ---Only the independent variable ANGLE is changed.
!    ---Some dependent variables are also changed.
!
! grid_increment_grid_coords:
!
!    ---Adjusts XORIGIN and YORIGIN so that:
!         all grid X coordinates are incremented by xstep and
!         all grid Y coordinates are incremented by ystep.
!
! grid_define_transform:
! grid_define_transform_alt:
!
!    ---Two different algorithms are implemented by the two routines.
!    ---Defines the coordinate transform using arrays xlocs(npoints),
!         ylocs(npoints), xgrids(npoints), ygrids(npoints).
!    ---Nil values in the above arrays are treated as exactly zero.
!    ---The number of points must be at least three.
!    ---The coordinate transform will be orthogonal.
!    ---If an error occurs, the transform is not changed and some or all
!         of the ERRORS structure will be nil.
!
! grid_define_transform:
!
!    ---Minimizes errors in the survey coordinates (dependent values).
!    ---Derives xorigin,yorigin,dx11,dx21,dx12,dx22.
!    ---The standard errors are as follows:
!         errors%xorigin = estimated error in xorigin.
!         errors%yorigin = estimated error in yorigin.
!         errors%dx11    = estimated error in dx11.
!         errors%dx21    = estimated error in dx21.
!         errors%dx12    = estimated error in dx12.
!         errors%dx22    = estimated error in dx22.
!         errors%xsigma  = standard deviation of xlocs(*).
!         errors%ysigma  = standard deviation of ylocs(*).
!
! grid_define_transform_alt:
!
!    ---Minimizes errors in the grid coordinates (dependent values).
!    ---Derives xorigin,yorigin,dn11,dn21,dn12,dn22.
!    ---The standard errors are as follows:
!         errors%xorigin = estimated error in xorigin.
!         errors%yorigin = estimated error in yorigin.
!         errors%dx11    = estimated error in dn11.
!         errors%dx21    = estimated error in dn21.
!         errors%dx12    = estimated error in dn12.
!         errors%dx22    = estimated error in dn22.
!         errors%xsigma  = standard deviation of xgrids(*).
!         errors%ysigma  = standard deviation of ygrids(*).
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!               COORDINATE SYSTEMS AND COORDINATE DEFINITIONS
!
! Positions on the earth can be defined in either one of the following
! two orthogonal coordinate systems:
!
!      Survey coordinates  (XLOC,YLOC)   usually in meters or feet.
!      Survey coordinates  (XLOC,YLOC)   usually in meters or feet.
!
!      Grid coordinates   (XGRID,YGRID)  (unitless) usually in CMP counts.
!      Grid coordinates   (XGRID,YGRID)  (unitless) usually in CMP counts.
!
!      (XLOC,YLOC) are often referred to as EASTING and NORTHING.
!
!-------------------------------------------------------------------------------
!                    COORDINATE TRANSFORMATION PARAMETERS
!
! The relationship between these coordinate systems is defined by the 
! following interrelated parameters:
!
!   (XORIGIN,YORIGIN) = origin (in survey coordinates) of the grid
!                       coordinate system.
!
!   (XWIDTH,YWIDTH)   = widths (in survey coordinates) of one CMP bin
!                       with widths (1,1) in the grid coordinate system.
!
!    ANGLE            = angle measured (usually counter-clockwise while
!                       looking down toward the ground from above) from the
!                       survey system X axis to the grid system X axis.
!
!    HAND or HANDEDNESS = +1 for right handed coordinate transformation.
!    HAND or HANDEDNESS = -1 for left handed coordinate transformation.
!
!   (DX11,DX21,DX12,DX22) = rotation matrix.
!   (DN11,DN21,DN12,DN22) = inverse rotation matrix.
!
!   XWIDTH and YWIDTH are often often called the X (or INLINE) and Y (or
!   crossline) grid spacing or grid width or grid increment.
!
!   In a right handed transformation, the angle measured in the same
!   direction as ANGLE above, from the survey system Y axis to the grid
!   system Y axis, is also the ANGLE specified above.
!
!   In a left handed transformation, the angle measured in the opposite
!   direction as ANGLE above, from the survey system Y axis to the grid
!   system Y axis, is also the ANGLE specified above.
!
!   Sometimes coordinate systems are referred to as "right handed" or
!   "left handed", as these examples indicate:
!
!                        Y                          +-----X
!        Right handed:   |           Left handed:   |
!                        |                          |
!                        |                          |
!                        +-----X                    Y
!
!   Actually, in the context of this module, a coordinate transformation
!   is right handed if both coordinate systems have the same handedness,
!   and left handed if the two coordinate systems have opposite handedness.
!
!-------------------------------------------------------------------------------
!                    COORDINATE TRANSFORMATION EQUATIONS
!
! The relationship between these coordinate systems is defined by the 
! following equations:
!
!
!          / XLOC \       / DX11   DX12 \     / XGRID \       / XORIGIN \
!          |      |   =   |             |  *  |       |   +   |         |
!          \ YLOC /       \ DX21   DX22 /     \ YGRID /       \ YORIGIN /
!
!
!         / XGRID \       / DN11   DN12 \     / XLOC - XORIGIN \
!         |       |   =   |             |  *  |                 |
!         \ YGRID /       \ DN21   DN22 /     \ YLOC - XORIGIN /
!
!
!          / DN11   DN12 \          1.0         /  DX22  -DX12 \
!          |             |   =  -----------  *  |              |
!          \ DN21   DN22 /      DETERMINANT     \ -DX21   DX11 /
!
!
!            where DETERMINANT  =  DX11 * DX22  -  DX21 * DX12
!
!    X  (inline)   grid spacing  =  XWIDTH  =  SQRT (DX11**2 + DX21**2)
!    Y (crossline) grid spacing  =  YWIDTH  =  SQRT (DX12**2 + DX22**2)
!
!
! Additional useful relationships are as follows:
!
!
!    / DX11   DX12 \       /  XWIDTH * COSA          -YWIDTH * SINA * HAND \
!    |             |   =   |                                               |
!    \ DX21   DX22 /       \  XWIDTH * SINA           YWIDTH * COSA * HAND /
!
!
!    / DN11   DN12 \       /  COSA / XWIDTH           SINA / XWIDTH        \
!    |             |   =   |                                               |
!    \ DN21   DN22 /       \ -SINA / YWIDTH * HAND    COSA / YWIDTH * HAND /
!
!
!      xloc = dx11 * xgrid + dx12 * ygrid + xorigin
!      yloc = dx21 * xgrid + dx22 * ygrid + yorigin
!
!      xgrid = dn11 * (xloc - xorigin) + dn12 * (yloc - yorigin)
!      ygrid = dn21 * (xloc - xorigin) + dn22 * (yloc - yorigin)
!
!      xorigin = xloc - xgrid * dx11 - ygrid * dx12
!      yorigin = yloc - xgrid * dx21 - ygrid * dx22
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!012. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
! 11. 2006-01-17  RSDay      Fixed -1 bug in grid_set_dx11
!010. 2006-01-10  B. Menger  Removed Unused Variables.
!  9. 2002-04-11  Stoeckley  Make grid_define_transform more robust; add more
!                             optional arguments to grid_define_transform; fix
!                             documentation error in the order of rotation
!                             matrix arguments in grid_get/set_dx/dn; add
!                             additional documentation.
!  8. 2001-02-13  Stoeckley  Implement grid_define_transform.
!  7. 2000-12-14  Stoeckley  Implement most of the useful subroutines for
!                             defining the coordinate system.
!  6. 2000-10-19  Stoeckley  Change hist_doc tags to history_doc; fix two
!                             lines which were >80 characters; add missing
!                             required documentation section.
!  5. 2000-04-07  Stoeckley  Change GRID_EQUAL and GRID_UNEQUAL to allow
!                             tiny differences without declaring the two
!                             data structures to be unequal.
!  4. 1999-11-17  Stoeckley  Add ident string for RCS; add double precision
!                             argument options.
!  3. 1999-10-20  Stoeckley  Fix argument lists in grid_get_dx... and
!                             grid_get_dn..., and document the fact that some
!                             subroutines are not yet implemented.
!  2. 1999-09-10  Stoeckley  Minor documentation changes.
!  1. 1999-06-16  Stoeckley  Initial version.
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



!<prog_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</prog_doc>



!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!


      module grid_module
      use named_constants_module
      use mth_module
      use least_module
      implicit none
      public
      private :: grid_set_dependent_values
      private :: grid_set_forward_from_reverse
      private :: grid_set_reverse_from_forward
      private :: grid_get_residuals
      private :: grid_define_3d_transform
      private :: grid_define_3d_transform_alt

      character(len=100),public,save :: GRID_IDENT = &
       '$Id: grid.f90,v 1.12 2006/09/18 13:32:48 Glover prod sps $'

      type,public :: grid_errors
           double precision xorigin,yorigin
           double precision dx11,dx21,dx12,dx22
           double precision xsigma,ysigma
      end type grid_errors


!!--------------------------- data structure -----------------------------!!
!!--------------------------- data structure -----------------------------!!
!!--------------------------- data structure -----------------------------!!


type,public :: grid_struct

  private

!!!!! independent variables:

  double precision xorigin    ! X coord of grid origin (feet or meters).
  double precision yorigin    ! Y coord of grid origin (feet or meters).
  double precision angle      ! rotation angle of +Xgrid axis from +Xloc axis.
  double precision xwidth     ! X  (inline)   grid spacing (feet or meters).
  double precision ywidth     ! Y (crossline) grid spacing (feet or meters).
  integer          handedness ! 1 if righthanded and -1 if lefthanded.

!!!!! dependent variables:

  double precision cosa, sina              ! cosine and sine of angle.
  double precision dx11,dx21,dx12,dx22     ! rotation matrix.
  double precision dn11,dn21,dn12,dn22     ! inverse rotation matrix.
  double precision determinant             ! determinant.

end type grid_struct


!!--------------------------- interfaces -------------------------------!!
!!--------------------------- interfaces -------------------------------!!
!!--------------------------- interfaces -------------------------------!!


interface operator (==)
      module procedure grid_equal
end interface

interface operator (/=)
      module procedure grid_unequal
end interface

interface grid_get_xbin_number
      module procedure grid_get_xbin_number_real
      module procedure grid_get_xbin_number_double
end interface

interface grid_get_ybin_number
      module procedure grid_get_ybin_number_real
      module procedure grid_get_ybin_number_double
end interface

interface grid_get_xbin_center
      module procedure grid_get_xbin_center_real
      module procedure grid_get_xbin_center_double
end interface

interface grid_get_ybin_center
      module procedure grid_get_ybin_center_real
      module procedure grid_get_ybin_center_double
end interface

interface grid_get_bin_numbers
      module procedure grid_get_bin_numbers_real
      module procedure grid_get_bin_numbers_double
end interface

interface grid_get_bin_centers
      module procedure grid_get_bin_centers_real
      module procedure grid_get_bin_centers_double
end interface

interface grid_get_xsurvey_coord
      module procedure grid_get_xsurvey_coord_real
      module procedure grid_get_xsurvey_coord_double
end interface

interface grid_get_ysurvey_coord
      module procedure grid_get_ysurvey_coord_real
      module procedure grid_get_ysurvey_coord_double
end interface

interface grid_get_xgrid_coord
      module procedure grid_get_xgrid_coord_real
      module procedure grid_get_xgrid_coord_double
end interface

interface grid_get_ygrid_coord
      module procedure grid_get_ygrid_coord_real
      module procedure grid_get_ygrid_coord_double
end interface

interface grid_get_survey_coords
      module procedure grid_get_survey_coords_real
      module procedure grid_get_survey_coords_double
end interface

interface grid_get_grid_coords
      module procedure grid_get_grid_coords_real
      module procedure grid_get_grid_coords_double
end interface


!!------------------------ start of subroutines ---------------------------!!
!!------------------------ start of subroutines ---------------------------!!
!!------------------------ start of subroutines ---------------------------!!


      contains


!!--------------------------- initialize -------------------------------!!
!!--------------------------- initialize -------------------------------!!
!!--------------------------- initialize -------------------------------!!


      subroutine grid_initialize (obj)
      implicit none
      type(grid_struct),intent(out) :: obj                  ! argument

      obj%xorigin     = 0.0
      obj%yorigin     = 0.0
      obj%angle       = 0.0
      obj%xwidth      = 1.0
      obj%ywidth      = 1.0
      obj%handedness  = 1
      obj%sina        = 0.0
      obj%cosa        = 1.0
      obj%dx11        = 1.0
      obj%dx21        = 0.0
      obj%dx12        = 0.0
      obj%dx22        = 1.0
      obj%dn11        = 1.0
      obj%dn21        = 0.0
      obj%dn12        = 0.0
      obj%dn22        = 1.0
      obj%determinant = 1.0
      return
      end subroutine grid_initialize


!!--------------------------- grid equal -------------------------------!!
!!--------------------------- grid equal -------------------------------!!
!!--------------------------- grid equal -------------------------------!!


      function grid_equal (obj1, obj2) result (equal)
      implicit none
      type(grid_struct),intent(in) :: obj1,obj2            ! argument
      logical                      :: equal                ! result

      equal = (mth_ameq(obj1%xorigin, obj2%xorigin, 1.0d-5) .and.  &
               mth_ameq(obj1%yorigin, obj2%yorigin, 1.0d-5) .and.  &
               mth_ameq(obj1%angle  , obj2%angle  , 1.0d-5) .and.  &
               mth_ameq(obj1%xwidth , obj2%xwidth , 1.0d-5) .and.  &
               mth_ameq(obj1%ywidth , obj2%ywidth , 1.0d-5) .and.  &
               obj1%handedness == obj2%handedness)

!     equal = (obj1%xorigin    == obj2%xorigin    .and.  &
!              obj1%yorigin    == obj2%yorigin    .and.  &
!              obj1%angle      == obj2%angle      .and.  &
!              obj1%xwidth     == obj2%xwidth     .and.  &
!              obj1%ywidth     == obj2%ywidth     .and.  &
!              obj1%handedness == obj2%handedness)
      return
      end function grid_equal


!!--------------------------- grid unequal -------------------------------!!
!!--------------------------- grid unequal -------------------------------!!
!!--------------------------- grid unequal -------------------------------!!


      function grid_unequal (obj1, obj2) result (unequal)
      implicit none
      type(grid_struct),intent(in) :: obj1,obj2            ! argument
      logical                      :: unequal              ! result

      unequal = .not.grid_equal(obj1, obj2)
      return
      end function grid_unequal


!!----------------------------- get values -----------------------------!!
!!----------------------------- get values -----------------------------!!
!!----------------------------- get values -----------------------------!!


      function grid_get_xorigin (obj) result (xorigin)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: xorigin              ! result
      xorigin = obj%xorigin
      return
      end function grid_get_xorigin



      function grid_get_yorigin (obj) result (yorigin)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: yorigin              ! result
      yorigin = obj%yorigin
      return
      end function grid_get_yorigin



      function grid_get_rotation_angle (obj) result (angle)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: angle                ! result
      angle = obj%angle
      return
      end function grid_get_rotation_angle



      function grid_get_xgrid_width (obj) result (xwidth)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: xwidth               ! result
      xwidth = obj%xwidth
      return
      end function grid_get_xgrid_width



      function grid_get_ygrid_width (obj) result (ywidth)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: ywidth               ! result
      ywidth = obj%ywidth
      return
      end function grid_get_ygrid_width



      function grid_get_handedness (obj) result (handedness)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      integer                      :: handedness           ! result
      handedness = obj%handedness
      return
      end function grid_get_handedness



      function grid_is_right_handed (obj) result (right)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      logical                      :: right                ! result
      right = (obj%handedness > 0)
      return
      end function grid_is_right_handed



      function grid_is_left_handed (obj) result (left)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      logical                      :: left                 ! result
      left = (obj%handedness <= 0)
      return
      end function grid_is_left_handed


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_get_origin (obj, xorigin, yorigin)
      implicit none
      type(grid_struct),intent(in)  :: obj                  ! argument
      double precision ,intent(out) :: xorigin              ! argument
      double precision ,intent(out) :: yorigin              ! argument

      xorigin = obj%xorigin
      yorigin = obj%yorigin
      return
      end subroutine grid_get_origin



      subroutine grid_get_widths (obj, xwidth, ywidth)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      double precision ,intent(out) :: xwidth              ! argument
      double precision ,intent(out) :: ywidth              ! argument

      xwidth = obj%xwidth
      ywidth = obj%ywidth
      return
      end subroutine grid_get_widths


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_get_cosine_angle (obj) result (cosa)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: cosa                 ! result
      cosa = obj%cosa
      return
      end function grid_get_cosine_angle



      function grid_get_sine_angle (obj) result (sina)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: sina                 ! result
      sina = obj%sina
      return
      end function grid_get_sine_angle


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_get_dx11 (obj) result (dx11)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dx11                 ! result
      dx11 = obj%dx11
      return
      end function grid_get_dx11


      function grid_get_dx21 (obj) result (dx21)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dx21                 ! result
      dx21 = obj%dx21
      return
      end function grid_get_dx21


      function grid_get_dx12 (obj) result (dx12)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dx12                 ! result
      dx12 = obj%dx12
      return
      end function grid_get_dx12


      function grid_get_dx22 (obj) result (dx22)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dx22                 ! result
      dx22 = obj%dx22
      return
      end function grid_get_dx22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_get_dn11 (obj) result (dn11)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dn11                 ! result
      dn11 = obj%dn11
      return
      end function grid_get_dn11


      function grid_get_dn21 (obj) result (dn21)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dn21                 ! result
      dn21 = obj%dn21
      return
      end function grid_get_dn21


      function grid_get_dn12 (obj) result (dn12)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dn12                 ! result
      dn12 = obj%dn12
      return
      end function grid_get_dn12


      function grid_get_dn22 (obj) result (dn22)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: dn22                 ! result
      dn22 = obj%dn22
      return
      end function grid_get_dn22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_get_determinant (obj) result (determinant)
      implicit none
      type(grid_struct),intent(in) :: obj                  ! arguments
      double precision             :: determinant          ! result
      determinant = obj%determinant
      return
      end function grid_get_determinant


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_get_dx (obj, dx11,dx21,dx12,dx22)
      implicit none
      type(grid_struct),intent(in)  :: obj                  ! arguments
      double precision ,intent(out) :: dx11,dx21,dx12,dx22  ! arguments
      dx11 = obj%dx11
      dx21 = obj%dx21
      dx12 = obj%dx12
      dx22 = obj%dx22
      return
      end subroutine grid_get_dx



      subroutine grid_get_dn (obj, dn11,dn21,dn12,dn22)
      implicit none
      type(grid_struct),intent(in)  :: obj                  ! arguments
      double precision ,intent(out) :: dn11,dn21,dn12,dn22  ! arguments
      dn11 = obj%dn11
      dn21 = obj%dn21
      dn12 = obj%dn12
      dn22 = obj%dn22
      return
      end subroutine grid_get_dn


!!----------------------------- set values -----------------------------!!
!!----------------------------- set values -----------------------------!!
!!----------------------------- set values -----------------------------!!


      subroutine grid_set_xorigin (obj, xorigin)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments
      double precision ,intent(in)    :: xorigin              ! arguments

      obj%xorigin = xorigin
      return
      end subroutine grid_set_xorigin



      subroutine grid_set_yorigin (obj, yorigin)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments
      double precision ,intent(in)    :: yorigin              ! arguments

      obj%yorigin = yorigin
      return
      end subroutine grid_set_yorigin



      subroutine grid_set_rotation_angle (obj, angle)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments
      double precision ,intent(in)    :: angle                ! arguments

      obj%angle = angle
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_rotation_angle



      subroutine grid_set_xgrid_width (obj, xwidth)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments
      double precision ,intent(in)    :: xwidth               ! arguments

      if (xwidth <= 0.0) return
      obj%xwidth = xwidth
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_xgrid_width



      subroutine grid_set_ygrid_width (obj, ywidth)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments
      double precision ,intent(in)    :: ywidth               ! arguments

      if (ywidth <= 0.0) return
      obj%ywidth = ywidth
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_ygrid_width



      subroutine grid_set_handedness (obj, handedness)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments
      integer          ,intent(in)    :: handedness           ! arguments

      if (handedness /= 1 .and. handedness /= -1) return
      obj%handedness = handedness
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_handedness



      subroutine grid_set_right_handed (obj)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments

      obj%handedness = 1
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_right_handed



      subroutine grid_set_left_handed (obj)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! arguments

      obj%handedness = -1
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_left_handed


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_set_origin (obj, xorigin, yorigin)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: xorigin              ! argument
      double precision ,intent(in)    :: yorigin              ! argument

      obj%xorigin = xorigin
      obj%yorigin = yorigin
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_origin



      subroutine grid_set_widths (obj, xwidth, ywidth)
      implicit none
      type(grid_struct),intent(inout) :: obj                 ! argument
      double precision ,intent(in)    :: xwidth              ! argument
      double precision ,intent(in)    :: ywidth              ! argument

      if (xwidth <= 0.0) return
      if (ywidth <= 0.0) return
      obj%xwidth = xwidth
      obj%ywidth = ywidth
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_widths


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_set_transform                                      &
                   (obj,xorigin,yorigin,angle,xwidth,ywidth,handedness)
      implicit none
      type(grid_struct),intent(inout) :: obj                 ! argument
      double precision ,intent(in)    :: xorigin             ! argument
      double precision ,intent(in)    :: yorigin             ! argument
      double precision ,intent(in)    :: angle               ! argument
      double precision ,intent(in)    :: xwidth              ! argument
      double precision ,intent(in)    :: ywidth              ! argument
      integer          ,intent(in)    :: handedness          ! argument

      if (xwidth <= 0.0) return
      if (ywidth <= 0.0) return
      if (handedness /= 1 .and. handedness /= -1) return
      obj%xorigin     = xorigin
      obj%yorigin     = yorigin
      obj%angle       = angle
      obj%xwidth      = xwidth
      obj%ywidth      = ywidth
      obj%handedness  = handedness
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_transform



      subroutine grid_set_right_handed_transform                         &
                   (obj,xorigin,yorigin,angle,xwidth,ywidth)
      implicit none
      type(grid_struct),intent(inout) :: obj                 ! argument
      double precision ,intent(in)    :: xorigin             ! argument
      double precision ,intent(in)    :: yorigin             ! argument
      double precision ,intent(in)    :: angle               ! argument
      double precision ,intent(in)    :: xwidth              ! argument
      double precision ,intent(in)    :: ywidth              ! argument

      call grid_set_transform (obj,xorigin,yorigin,angle,xwidth,ywidth,1)
      return
      end subroutine grid_set_right_handed_transform



      subroutine grid_set_left_handed_transform                         &
                   (obj,xorigin,yorigin,angle,xwidth,ywidth)
      implicit none
      type(grid_struct),intent(inout) :: obj                 ! argument
      double precision ,intent(in)    :: xorigin             ! argument
      double precision ,intent(in)    :: yorigin             ! argument
      double precision ,intent(in)    :: angle               ! argument
      double precision ,intent(in)    :: xwidth              ! argument
      double precision ,intent(in)    :: ywidth              ! argument

      call grid_set_transform (obj,xorigin,yorigin,angle,xwidth,ywidth,-1)
      return
      end subroutine grid_set_left_handed_transform


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_set_dx11 (obj, dx11)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dx11                 ! argument

      if (dx11 == 0.0) return
      obj%dx11 =   dx11   !remove - sign, RSD 12-13-06
      obj%dx21 = - dx11 * obj%dx12 / obj%dx22
      call grid_set_reverse_from_forward (obj)
      return
      end subroutine grid_set_dx11



      subroutine grid_set_dx21 (obj, dx21)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dx21                 ! argument

      obj%dx21 =   dx21
      obj%dx12 = - dx21 * obj%dx22 / obj%dx11
      call grid_set_reverse_from_forward (obj)
      return
      end subroutine grid_set_dx21



      subroutine grid_set_dx12 (obj, dx12)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dx12                 ! argument

      obj%dx12 =   dx12
      obj%dx21 = - dx12 * obj%dx11 / obj%dx22
      call grid_set_reverse_from_forward (obj)
      return
      end subroutine grid_set_dx12



      subroutine grid_set_dx22 (obj, dx22)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dx22                 ! argument

      if (dx22 == 0.0) return
      obj%dx22 =   dx22
      obj%dx12 = - dx22 * obj%dx21 / obj%dx11
      call grid_set_reverse_from_forward (obj)
      return
      end subroutine grid_set_dx22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_set_dn11 (obj, dn11)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dn11                 ! argument

      if (dn11 == 0.0) return
      obj%dn11 = - dn11
      obj%dn12 = - dn11 * obj%dn21 / obj%dn22
      call grid_set_forward_from_reverse (obj)
      return
      end subroutine grid_set_dn11



      subroutine grid_set_dn21 (obj, dn21)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dn21                 ! argument

      obj%dn21 =   dn21
      obj%dn12 = - dn21 * obj%dn11 / obj%dn22
      call grid_set_forward_from_reverse (obj)
      return
      end subroutine grid_set_dn21



      subroutine grid_set_dn12 (obj, dn12)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dn12                 ! argument

      obj%dn12 =   dn12
      obj%dn21 = - dn12 * obj%dn22 / obj%dn11
      call grid_set_forward_from_reverse (obj)
      return
      end subroutine grid_set_dn12



      subroutine grid_set_dn22 (obj, dn22)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dn22                 ! argument

      if (dn22 == 0.0) return
      obj%dn22 =   dn22
      obj%dn21 = - dn22 * obj%dn12 / obj%dn11
      call grid_set_forward_from_reverse (obj)
      return
      end subroutine grid_set_dn22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_set_dx (obj, dx11,dx21,dx12,dx22)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dx11,dx21,dx12,dx22  ! argument

      if (dx11 == 0.0 .or. dx22 == 0.0) return
      obj%dx11 =   dx11
      obj%dx21 =   dx21
      obj%dx12 = - dx21 * dx22 / dx11      ! dx12 not used.
      obj%dx22 =   dx22
      call grid_set_reverse_from_forward (obj)
      return
      end subroutine grid_set_dx



      subroutine grid_set_dn (obj, dn11,dn21,dn12,dn22)
      implicit none
      type(grid_struct),intent(inout) :: obj                  ! argument
      double precision ,intent(in)    :: dn11,dn21,dn12,dn22  ! argument

      if (dn11 == 0.0 .or. dn22 == 0.0) return
      obj%dn11 =   dn11
      obj%dn21 = - dn12 * dn22 / dn11      ! dn21 not used.
      obj%dn12 =   dn12
      obj%dn22 =   dn22
      call grid_set_forward_from_reverse (obj)
      return
      end subroutine grid_set_dn


!!--------------------- get transformed coordinates ----------------------!!
!!--------------------- get transformed coordinates ----------------------!!
!!--------------------- get transformed coordinates ----------------------!!


      function grid_get_xsurvey_coord_real (obj, xgrid, ygrid) result (xloc)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xgrid               ! argument
      real             ,intent(in) :: ygrid               ! argument
      double precision             :: xloc                ! result

      xloc = obj%dx11 * xgrid + obj%dx12 * ygrid + obj%xorigin
      return
      end function grid_get_xsurvey_coord_real



      function grid_get_xsurvey_coord_double (obj, xgrid, ygrid) result (xloc)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xgrid               ! argument
      double precision ,intent(in) :: ygrid               ! argument
      double precision             :: xloc                ! result

      xloc = obj%dx11 * xgrid + obj%dx12 * ygrid + obj%xorigin
      return
      end function grid_get_xsurvey_coord_double



      function grid_get_ysurvey_coord_real (obj, xgrid, ygrid) result (yloc)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xgrid               ! argument
      real             ,intent(in) :: ygrid               ! argument
      double precision             :: yloc                ! result

      yloc = obj%dx21 * xgrid + obj%dx22 * ygrid + obj%yorigin
      return
      end function grid_get_ysurvey_coord_real



      function grid_get_ysurvey_coord_double (obj, xgrid, ygrid) result (yloc)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xgrid               ! argument
      double precision ,intent(in) :: ygrid               ! argument
      double precision             :: yloc                ! result

      yloc = obj%dx21 * xgrid + obj%dx22 * ygrid + obj%yorigin
      return
      end function grid_get_ysurvey_coord_double



      function grid_get_xgrid_coord_real (obj, xloc, yloc) result (xgrid)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xloc                ! argument
      real             ,intent(in) :: yloc                ! argument
      double precision             :: xgrid               ! result
      double precision             :: xloc2,yloc2         ! local

      xloc2 = xloc - obj%xorigin
      yloc2 = yloc - obj%yorigin
      xgrid = obj%dn11 * xloc2 + obj%dn12 * yloc2
      return
      end function grid_get_xgrid_coord_real



      function grid_get_xgrid_coord_double (obj, xloc, yloc) result (xgrid)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xloc                ! argument
      double precision ,intent(in) :: yloc                ! argument
      double precision             :: xgrid               ! result
      double precision             :: xloc2,yloc2         ! local

      xloc2 = xloc - obj%xorigin
      yloc2 = yloc - obj%yorigin
      xgrid = obj%dn11 * xloc2 + obj%dn12 * yloc2
      return
      end function grid_get_xgrid_coord_double



      function grid_get_ygrid_coord_real (obj, xloc, yloc) result (ygrid)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xloc                ! argument
      real             ,intent(in) :: yloc                ! argument
      double precision             :: ygrid               ! result
      double precision             :: xloc2,yloc2         ! local

      xloc2 = xloc - obj%xorigin
      yloc2 = yloc - obj%yorigin
      ygrid = obj%dn21 * xloc2 + obj%dn22 * yloc2
      return
      end function grid_get_ygrid_coord_real



      function grid_get_ygrid_coord_double (obj, xloc, yloc) result (ygrid)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xloc                ! argument
      double precision ,intent(in) :: yloc                ! argument
      double precision             :: ygrid               ! result
      double precision             :: xloc2,yloc2         ! local

      xloc2 = xloc - obj%xorigin
      yloc2 = yloc - obj%yorigin
      ygrid = obj%dn21 * xloc2 + obj%dn22 * yloc2
      return
      end function grid_get_ygrid_coord_double


                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_get_survey_coords_real (obj, xgrid, ygrid, xloc, yloc)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      real             ,intent(in)  :: xgrid               ! argument
      real             ,intent(in)  :: ygrid               ! argument
      real             ,intent(out) :: xloc                ! argument
      real             ,intent(out) :: yloc                ! argument

      xloc = obj%dx11 * xgrid + obj%dx12 * ygrid + obj%xorigin
      yloc = obj%dx21 * xgrid + obj%dx22 * ygrid + obj%yorigin
      return
      end subroutine grid_get_survey_coords_real



      subroutine grid_get_survey_coords_double (obj, xgrid, ygrid, xloc, yloc)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      double precision ,intent(in)  :: xgrid               ! argument
      double precision ,intent(in)  :: ygrid               ! argument
      double precision ,intent(out) :: xloc                ! argument
      double precision ,intent(out) :: yloc                ! argument

      xloc = obj%dx11 * xgrid + obj%dx12 * ygrid + obj%xorigin
      yloc = obj%dx21 * xgrid + obj%dx22 * ygrid + obj%yorigin
      return
      end subroutine grid_get_survey_coords_double



      subroutine grid_get_grid_coords_real (obj, xloc, yloc, xgrid, ygrid)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      real             ,intent(in)  :: xloc                ! argument
      real             ,intent(in)  :: yloc                ! argument
      real             ,intent(out) :: xgrid               ! argument
      real             ,intent(out) :: ygrid               ! argument
      real                          :: xloc2,yloc2         ! local

      xloc2 = xloc - obj%xorigin
      yloc2 = yloc - obj%yorigin
      xgrid = obj%dn11 * xloc2 + obj%dn12 * yloc2
      ygrid = obj%dn21 * xloc2 + obj%dn22 * yloc2
      return
      end subroutine grid_get_grid_coords_real



      subroutine grid_get_grid_coords_double (obj, xloc, yloc, xgrid, ygrid)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      double precision ,intent(in)  :: xloc                ! argument
      double precision ,intent(in)  :: yloc                ! argument
      double precision ,intent(out) :: xgrid               ! argument
      double precision ,intent(out) :: ygrid               ! argument
      double precision              :: xloc2,yloc2         ! local

      xloc2 = xloc - obj%xorigin
      yloc2 = yloc - obj%yorigin
      xgrid = obj%dn11 * xloc2 + obj%dn12 * yloc2
      ygrid = obj%dn21 * xloc2 + obj%dn22 * yloc2
      return
      end subroutine grid_get_grid_coords_double


!!--------------------------- do cmp binning -----------------------------!!
!!--------------------------- do cmp binning -----------------------------!!
!!--------------------------- do cmp binning -----------------------------!!


      function grid_get_xbin_number_real  &
                                   (obj, xloc, yloc) result (xbin_number)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xloc                ! argument
      real             ,intent(in) :: yloc                ! argument
      integer                      :: xbin_number         ! result
      double precision             :: xgrid               ! local

      xgrid = grid_get_xgrid_coord (obj, xloc, yloc)
      xbin_number = nint(xgrid)
      return
      end function grid_get_xbin_number_real



      function grid_get_xbin_number_double  &
                                   (obj, xloc, yloc) result (xbin_number)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xloc                ! argument
      double precision ,intent(in) :: yloc                ! argument
      integer                      :: xbin_number         ! result
      double precision             :: xgrid               ! local

      xgrid = grid_get_xgrid_coord (obj, xloc, yloc)
      xbin_number = nint(xgrid)
      return
      end function grid_get_xbin_number_double



      function grid_get_ybin_number_real  &
                                   (obj, xloc, yloc) result (ybin_number)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xloc                ! argument
      real             ,intent(in) :: yloc                ! argument
      integer                      :: ybin_number         ! result
      double precision             :: ygrid               ! local

      ygrid = grid_get_ygrid_coord (obj, xloc, yloc)
      ybin_number = nint(ygrid)
      return
      end function grid_get_ybin_number_real



      function grid_get_ybin_number_double  &
                                   (obj, xloc, yloc) result (ybin_number)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xloc                ! argument
      double precision ,intent(in) :: yloc                ! argument
      integer                      :: ybin_number         ! result
      double precision             :: ygrid               ! local

      ygrid = grid_get_ygrid_coord (obj, xloc, yloc)
      ybin_number = nint(ygrid)
      return
      end function grid_get_ybin_number_double



      function grid_get_xbin_center_real  &
                                   (obj, xgrid, ygrid) result (xbin_center)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xgrid               ! argument
      real             ,intent(in) :: ygrid               ! argument
      double precision             :: xbin_center         ! result
      double precision             :: xbin_number         ! local
      double precision             :: ybin_number         ! local

      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      xbin_center = grid_get_xsurvey_coord (obj, xbin_number, ybin_number)
      return
      end function grid_get_xbin_center_real



      function grid_get_xbin_center_double  &
                                   (obj, xgrid, ygrid) result (xbin_center)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xgrid               ! argument
      double precision ,intent(in) :: ygrid               ! argument
      double precision             :: xbin_center         ! result
      double precision             :: xbin_number         ! local
      double precision             :: ybin_number         ! local

      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      xbin_center = grid_get_xsurvey_coord (obj, xbin_number, ybin_number)
      return
      end function grid_get_xbin_center_double



      function grid_get_ybin_center_real  &
                                   (obj, xgrid, ygrid) result (ybin_center)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      real             ,intent(in) :: xgrid               ! argument
      real             ,intent(in) :: ygrid               ! argument
      double precision             :: ybin_center         ! result
      double precision             :: xbin_number         ! local
      double precision             :: ybin_number         ! local

      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      ybin_center = grid_get_ysurvey_coord (obj, xbin_number, ybin_number)
      return
      end function grid_get_ybin_center_real



      function grid_get_ybin_center_double  &
                                   (obj, xgrid, ygrid) result (ybin_center)
      implicit none
      type(grid_struct),intent(in) :: obj                 ! argument
      double precision ,intent(in) :: xgrid               ! argument
      double precision ,intent(in) :: ygrid               ! argument
      double precision             :: ybin_center         ! result
      double precision             :: xbin_number         ! local
      double precision             :: ybin_number         ! local

      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      ybin_center = grid_get_ysurvey_coord (obj, xbin_number, ybin_number)
      return
      end function grid_get_ybin_center_double


                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_get_bin_numbers_real  &
                             (obj, xloc, yloc, xbin_number, ybin_number)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      real             ,intent(in)  :: xloc                ! argument
      real             ,intent(in)  :: yloc                ! argument
      integer          ,intent(out) :: xbin_number         ! argument
      integer          ,intent(out) :: ybin_number         ! argument
      real                          :: xgrid               ! local
      real                          :: ygrid               ! local

      call grid_get_grid_coords (obj, xloc, yloc, xgrid, ygrid)
      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      return
      end subroutine grid_get_bin_numbers_real



      subroutine grid_get_bin_numbers_double  &
                             (obj, xloc, yloc, xbin_number, ybin_number)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      double precision ,intent(in)  :: xloc                ! argument
      double precision ,intent(in)  :: yloc                ! argument
      integer          ,intent(out) :: xbin_number         ! argument
      integer          ,intent(out) :: ybin_number         ! argument
      double precision              :: xgrid               ! local
      double precision              :: ygrid               ! local

      call grid_get_grid_coords (obj, xloc, yloc, xgrid, ygrid)
      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      return
      end subroutine grid_get_bin_numbers_double



      subroutine grid_get_bin_centers_real  &
                            (obj, xgrid, ygrid, xbin_center, ybin_center)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      real             ,intent(in)  :: xgrid               ! argument
      real             ,intent(in)  :: ygrid               ! argument
      real             ,intent(out) :: xbin_center         ! argument
      real             ,intent(out) :: ybin_center         ! argument
      real                          :: xbin_number         ! local
      real                          :: ybin_number         ! local

      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      call grid_get_survey_coords  &
              (obj, xbin_number, ybin_number, xbin_center, ybin_center)
      return
      end subroutine grid_get_bin_centers_real



      subroutine grid_get_bin_centers_double  &
                            (obj, xgrid, ygrid, xbin_center, ybin_center)
      implicit none
      type(grid_struct),intent(in)  :: obj                 ! argument
      double precision ,intent(in)  :: xgrid               ! argument
      double precision ,intent(in)  :: ygrid               ! argument
      double precision ,intent(out) :: xbin_center         ! argument
      double precision ,intent(out) :: ybin_center         ! argument
      double precision              :: xbin_number         ! local
      double precision              :: ybin_number         ! local

      xbin_number = nint(xgrid)
      ybin_number = nint(ygrid)
      call grid_get_survey_coords  &
              (obj, xbin_number, ybin_number, xbin_center, ybin_center)
      return
      end subroutine grid_get_bin_centers_double


!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!


!!-------------------------- define origin ------------------------------!!
!!-------------------------- define origin ------------------------------!!
!!-------------------------- define origin ------------------------------!!
 
! Places origin so that specified (xgrid,ygrid) is at
!   location (xloc,yloc).
! Only the variables obj%xorigin and obj%yorigin are changed.

 
      subroutine grid_define_origin (obj, xgrid, ygrid, xloc, yloc)
      implicit none
      type(grid_struct)     ,intent(inout) :: obj                 ! argument
      double precision      ,intent(in)    :: xgrid               ! argument
      double precision      ,intent(in)    :: ygrid               ! argument
      double precision      ,intent(in)    :: xloc                ! argument
      double precision      ,intent(in)    :: yloc                ! argument

      obj%xorigin = xloc - xgrid * obj%dx11 - ygrid * obj%dx12
      obj%yorigin = yloc - xgrid * obj%dx21 - ygrid * obj%dx22
      return
      end subroutine grid_define_origin


!!----------------------- define rotation angle -------------------------!!
!!----------------------- define rotation angle -------------------------!!
!!----------------------- define rotation angle -------------------------!!
 
! Sets rotation angle to the direction from (xloc1,yloc1)
!   to (xloc2,yloc2).
! Only the independent variable obj%angle is changed.
! Some dependent variables are also changed.


      subroutine grid_define_rotation_angle &
                                      (obj, xloc1, yloc1, xloc2, yloc2)
      implicit none
      type(grid_struct)     ,intent(inout) :: obj                 ! argument
      double precision      ,intent(in)    :: xloc1               ! argument
      double precision      ,intent(in)    :: yloc1               ! argument
      double precision      ,intent(in)    :: xloc2               ! argument
      double precision      ,intent(in)    :: yloc2               ! argument
      double precision                     :: xdiff,ydiff         ! local

      ydiff = yloc2 - yloc1
      xdiff = xloc2 - xloc1
      if (ydiff == 0.0 .and. xdiff == 0.0) return
      obj%angle = DEGREES_PER_RADIAN * atan2(ydiff, xdiff)
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_define_rotation_angle

 
!!----------------------- define origin and angle -----------------------!!
!!----------------------- define origin and angle -----------------------!!
!!----------------------- define origin and angle -----------------------!!
 
! Places origin so that specified (xgrid,ygrid) is at
!   location (xloc1,yloc1).
! Sets rotation angle to the direction from (xloc1,yloc1)
!   to (xloc2,yloc2).
! Only the independent variables obj%xorigin, obj%yorigin, and
!   obj%angle are changed.
! Some dependent variables are also changed.

 
      subroutine grid_define_origin_and_angle (obj, xgrid,ygrid,  &
                                          xloc1, yloc1, xloc2, yloc2)
      implicit none
      type(grid_struct)     ,intent(inout) :: obj                 ! argument
      double precision      ,intent(in)    :: xgrid               ! argument
      double precision      ,intent(in)    :: ygrid               ! argument
      double precision      ,intent(in)    :: xloc1               ! argument
      double precision      ,intent(in)    :: yloc1               ! argument
      double precision      ,intent(in)    :: xloc2               ! argument
      double precision      ,intent(in)    :: yloc2               ! argument

      call grid_define_rotation_angle (obj, xloc1, yloc1, xloc2, yloc2)
      call grid_define_origin         (obj, xgrid, ygrid, xloc1, yloc1)
                  !!! (must be done in the above order)
      return
      end subroutine grid_define_origin_and_angle


!!----------------------- refine bin center -----------------------------!!
!!----------------------- refine bin center -----------------------------!!
!!----------------------- refine bin center -----------------------------!!
 
! Adjusts the origin slightly so that specified (xloc,yloc)
!   becomes the new center of the bin in which the point resides.
! Only the variables obj%xorigin and obj%yorigin are changed.

 
      subroutine grid_refine_bin_center (obj, xloc , yloc)
      implicit none
      type(grid_struct)     ,intent(inout) :: obj                 ! argument
      double precision      ,intent(in)    :: xloc                ! argument
      double precision      ,intent(in)    :: yloc                ! argument
      double precision                     :: xgrid,ygrid         ! local

      xgrid = grid_get_xgrid_coord (obj, xloc, yloc)
      ygrid = grid_get_ygrid_coord (obj, xloc, yloc)
      xgrid = nint(xgrid)
      ygrid = nint(ygrid)
      call grid_define_origin (obj, xgrid, ygrid, xloc, yloc)
      return
      end subroutine grid_refine_bin_center


!!----------------------- refine rotation angle --------------------------!!
!!----------------------- refine rotation angle --------------------------!!
!!----------------------- refine rotation angle --------------------------!!
 
! Adjusts the rotation angle slightly so that specified (xloc,yloc)
!   becomes a point on a line from the origin through the new center
!   of the bin in which the point resides.
! Only the independent variable obj%angle is changed.
! Some dependent variables are also changed.

 
      subroutine grid_refine_rotation_angle (obj, xloc , yloc)
      implicit none
      type(grid_struct)     ,intent(inout) :: obj                 ! argument
      double precision      ,intent(in)    :: xloc                ! argument
      double precision      ,intent(in)    :: yloc                ! argument
      double precision                     :: xgrid,ygrid         ! local
      double precision                     :: xmid,ymid           ! local
      double precision                     :: xlocdiff,ylocdiff   ! local
      double precision                     :: xmiddiff,ymiddiff   ! local
      double precision                     :: aloc,amid           ! local

      xgrid    = grid_get_xgrid_coord (obj, xloc, yloc)
      ygrid    = grid_get_ygrid_coord (obj, xloc, yloc)
      xgrid    = nint(xgrid)
      ygrid    = nint(ygrid)
      xmid     = grid_get_xsurvey_coord (obj, xgrid, ygrid)
      ymid     = grid_get_ysurvey_coord (obj, xgrid, ygrid)
      ylocdiff = yloc - obj%yorigin
      xlocdiff = xloc - obj%xorigin
      ymiddiff = ymid - obj%yorigin
      xmiddiff = xmid - obj%xorigin
      if(ylocdiff == 0.0 .and. xlocdiff == 0.0) return
      if(ymiddiff == 0.0 .and. xmiddiff == 0.0) return
      aloc = DEGREES_PER_RADIAN * atan2(ylocdiff, xlocdiff)
      amid = DEGREES_PER_RADIAN * atan2(ymiddiff, xmiddiff)
      obj%angle = obj%angle + aloc - amid
      if(obj%angle >  180.0) obj%angle = obj%angle - 360.0
      if(obj%angle < -180.0) obj%angle = obj%angle + 360.0
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_refine_rotation_angle
 
 
!!------------------------- increment grid coords -------------------------!!
!!------------------------- increment grid coords -------------------------!!
!!------------------------- increment grid coords -------------------------!!
 
! Adjusts obj%xorigin and obj%yorigin so that:
!   all grid X coordinates are incremented by xstep and
!   all grid Y coordinates are incremented by ystep.

 
      subroutine grid_increment_grid_coords (obj, xstep, ystep)
      implicit none
      type(grid_struct)     ,intent(inout) :: obj                 ! argument
      double precision      ,intent(in)    :: xstep               ! argument
      double precision      ,intent(in)    :: ystep               ! argument

      if (xstep == DNIL .or. ystep == DNIL) return
      obj%xorigin = grid_get_xsurvey_coord (obj, -xstep, -ystep)
      obj%yorigin = grid_get_ysurvey_coord (obj, -xstep, -ystep)
      return
      end subroutine grid_increment_grid_coords


!!-------------------------- define transform -----------------------------!!
!!-------------------------- define transform -----------------------------!!
!!-------------------------- define transform -----------------------------!!


      subroutine grid_define_transform                                   &
                              (obj, npoints, xlocs,ylocs, xgrids,ygrids, &
                               rxlocs,rylocs, rxgrids,rygrids, errors)
      implicit none
      type(grid_struct),intent(inout)        :: obj               ! argument
      integer          ,intent(in)           :: npoints           ! argument
      double precision ,intent(in)           :: xlocs  (:)        ! argument
      double precision ,intent(in)           :: ylocs  (:)        ! argument
      double precision ,intent(in)           :: xgrids (:)        ! argument
      double precision ,intent(in)           :: ygrids (:)        ! argument
      double precision ,intent(out),optional :: rxlocs (:)        ! argument
      double precision ,intent(out),optional :: rylocs (:)        ! argument
      double precision ,intent(out),optional :: rxgrids(:)        ! argument
      double precision ,intent(out),optional :: rygrids(:)        ! argument
      type(grid_errors),intent(out),optional :: errors            ! argument

      call grid_define_3d_transform &
                          (obj, npoints, xlocs,ylocs, xgrids,ygrids, errors)

      call grid_get_residuals (obj, npoints, xlocs,ylocs, xgrids,ygrids,  &
                                    rxlocs,rylocs, rxgrids,rygrids)
      return
      end subroutine grid_define_transform


!!-------------------------- define transform alt -------------------------!!
!!-------------------------- define transform alt -------------------------!!
!!-------------------------- define transform alt -------------------------!!


      subroutine grid_define_transform_alt                               &
                              (obj, npoints, xlocs,ylocs, xgrids,ygrids, &
                               rxlocs,rylocs, rxgrids,rygrids, errors)
      implicit none
      type(grid_struct),intent(inout)        :: obj               ! argument
      integer          ,intent(in)           :: npoints           ! argument
      double precision ,intent(in)           :: xlocs  (:)        ! argument
      double precision ,intent(in)           :: ylocs  (:)        ! argument
      double precision ,intent(in)           :: xgrids (:)        ! argument
      double precision ,intent(in)           :: ygrids (:)        ! argument
      double precision ,intent(out),optional :: rxlocs (:)        ! argument
      double precision ,intent(out),optional :: rylocs (:)        ! argument
      double precision ,intent(out),optional :: rxgrids(:)        ! argument
      double precision ,intent(out),optional :: rygrids(:)        ! argument
      type(grid_errors),intent(out),optional :: errors            ! argument

      call grid_define_3d_transform_alt &
                          (obj, npoints, xlocs,ylocs, xgrids,ygrids, errors)

      call grid_get_residuals (obj, npoints, xlocs,ylocs, xgrids,ygrids,  &
                                    rxlocs,rylocs, rxgrids,rygrids)
      return
      end subroutine grid_define_transform_alt


!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!
!!------------------------ private routines -------------------------------!!



!!--------------------- set dependent values ----------------------------!!
!!--------------------- set dependent values ----------------------------!!
!!--------------------- set dependent values ----------------------------!!

! Given obj%angle, obj%xwidth, obj%ywidth, and obj%handedness:


      subroutine grid_set_dependent_values (obj)           ! private
      implicit none
      type(grid_struct),intent(inout) :: obj               ! argument

      obj%cosa = cos(obj%angle * RADIANS_PER_DEGREE)
      obj%sina = sin(obj%angle * RADIANS_PER_DEGREE)

!!!!!!!!!!! this has to be commented out until the set functions
!!!!!!!!!!! for the matrices are made better.
!     if(obj%cosa == 1.0 || obj%cosa == -1.0) obj%sina = 0.0
!     if(obj%sina == 1.0 || obj%sina == -1.0) obj%cosa = 0.0

      obj%dn11 =   obj%cosa / obj%xwidth
      obj%dn21 = - obj%sina / obj%ywidth * obj%handedness
      obj%dn12 =   obj%sina / obj%xwidth
      obj%dn22 =   obj%cosa / obj%ywidth * obj%handedness
  
      obj%dx11 =   obj%cosa * obj%xwidth
      obj%dx21 =   obj%sina * obj%xwidth
      obj%dx12 = - obj%sina * obj%ywidth * obj%handedness
      obj%dx22 =   obj%cosa * obj%ywidth * obj%handedness
  
      obj%determinant = obj%dx11 * obj%dx22 - obj%dx21 * obj%dx12
      return
      end subroutine grid_set_dependent_values


!!--------------------- set forward from reverse ------------------------!!
!!--------------------- set forward from reverse ------------------------!!
!!--------------------- set forward from reverse ------------------------!!

! Given obj%dn11, obj%dn21, obj%dn12, obj%dn22 (guaranteed orthogonal):


      subroutine grid_set_forward_from_reverse (obj)       ! private
      implicit none
      type(grid_struct),intent(inout) :: obj               ! argument
      double precision                :: det               ! local

      if (obj%dn12 == 0.0 .and. obj%dn22 == 0.0) return
      obj%angle  = DEGREES_PER_RADIAN * atan2( obj%dn12, obj%dn22)
      obj%xwidth = 1.0 / sqrt(obj%dn11 * obj%dn11 + obj%dn12 * obj%dn12)
      obj%ywidth = 1.0 / sqrt(obj%dn21 * obj%dn21 + obj%dn22 * obj%dn22)
      det        = obj%dn11 * obj%dn22 - obj%dn21 * obj%dn12
      if(det >= 0.0)then
            obj%handedness =  1
      else
            obj%handedness = -1
      end if
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_forward_from_reverse


!!--------------------- set reverse from forward ------------------------!!
!!--------------------- set reverse from forward ------------------------!!
!!--------------------- set reverse from forward ------------------------!!

! Given obj%dx11, obj%dx21, obj%dx12, obj%dx22 (guaranteed orthogonal):


      subroutine grid_set_reverse_from_forward (obj)       ! private
      implicit none
      type(grid_struct),intent(inout) :: obj               ! argument
      double precision                :: det               ! local
 
      if (obj%dx21 == 0.0 .and. obj%dx11 == 0.0) return
      obj%angle  = DEGREES_PER_RADIAN * atan2( obj%dx21, obj%dx11)
      obj%xwidth = sqrt(obj%dx11 * obj%dx11 + obj%dx21 * obj%dx21)
      obj%ywidth = sqrt(obj%dx12 * obj%dx12 + obj%dx22 * obj%dx22)
      det        = obj%dx11 * obj%dx22 - obj%dx21 * obj%dx12
      if(det >= 0.0) then
           obj%handedness =  1
      else
           obj%handedness = -1
      end if
      call grid_set_dependent_values (obj)
      return
      end subroutine grid_set_reverse_from_forward


!!--------------------------- get residuals -----------------------------!!
!!--------------------------- get residuals -----------------------------!!
!!--------------------------- get residuals -----------------------------!!


      subroutine grid_get_residuals                                  &
                       (obj, npoints, xlocs, ylocs, xgrids, ygrids,  &
                        rxlocs,rylocs, rxgrids,rygrids)
      implicit none
      type(grid_struct),intent(inout)        :: obj               ! argument
      integer          ,intent(in)           :: npoints           ! argument
      double precision ,intent(in)           :: xlocs (:)         ! argument
      double precision ,intent(in)           :: ylocs (:)         ! argument
      double precision ,intent(in)           :: xgrids(:)         ! argument
      double precision ,intent(in)           :: ygrids(:)         ! argument
      double precision ,intent(out),optional :: rxlocs (:)        ! argument
      double precision ,intent(out),optional :: rylocs (:)        ! argument
      double precision ,intent(out),optional :: rxgrids(:)        ! argument
      double precision ,intent(out),optional :: rygrids(:)        ! argument
      integer                                :: i                 ! local

      if (present(rxgrids)) then
       do i = 1,npoints
        rxgrids(i) = xgrids(i) - grid_get_xgrid_coord (obj,xlocs(i),ylocs(i))
       end do
      end if

      if (present(rygrids)) then
       do i = 1,npoints
        rygrids(i) = ygrids(i) - grid_get_ygrid_coord (obj,xlocs(i),ylocs(i))
       end do
      end if

      if (present(rxlocs)) then
       do i = 1,npoints
        rxlocs(i) = xlocs(i) - grid_get_xsurvey_coord (obj,xgrids(i),ygrids(i))
       end do
      end if

      if (present(rylocs)) then
       do i = 1,npoints
        rylocs(i) = ylocs(i) - grid_get_ysurvey_coord (obj,xgrids(i),ygrids(i))
       end do
      end if
      return
      end subroutine grid_get_residuals


!!-------------------------- define 3d transform ---------------------------!!
!!-------------------------- define 3d transform ---------------------------!!
!!-------------------------- define 3d transform ---------------------------!!

!         coef1  term1   coef2  term2   coef3
!  xloc = dx11 * xgrid + dx12 * ygrid + xorigin
!  yloc = dx21 * xgrid + dx22 * ygrid + yorigin


      subroutine grid_define_3d_transform  &
                        (obj, npoints, xlocs, ylocs, xgrids, ygrids, errors)
      implicit none
      type(grid_struct),intent(inout)        :: obj               ! argument
      integer          ,intent(in)           :: npoints           ! argument
      double precision ,intent(in)           :: xlocs (:)         ! argument
      double precision ,intent(in)           :: ylocs (:)         ! argument
      double precision ,intent(in)           :: xgrids(:)         ! argument
      double precision ,intent(in)           :: ygrids(:)         ! argument
      type(grid_errors),intent(out),optional :: errors            ! argument

      double precision                       :: dx12,dx21         ! local
      double precision                       :: dx11,dx22         ! local
      double precision                       :: xorigin,yorigin   ! local
      integer                                :: i                 ! local
      double precision                       :: observation       ! local
      double precision                       :: sigma             ! local
      double precision ,parameter            :: weight = 1.0      ! local
      integer          ,parameter            :: nterms = 3        ! local
      double precision                       :: terms (nterms)    ! local
      double precision                       :: coefs (nterms)    ! local
      double precision                       :: ecoefs(nterms)    ! local
      type(least_struct),pointer             :: least             ! local

      nullify (least) ! jpa

!----------use observations xlocs:

      call least_create (least,nterms)
      do i = 1,npoints
           observation = xlocs (i)
           terms(1)    = xgrids(i)
           terms(2)    = ygrids(i)
           terms(3)    = 1.0
           call least_add_point (least,terms,observation,weight)
      end do
      call least_delete (least,coefs,ecoefs,sigma)
!print *, ' coefs dx11,dx12,xorigin = ',coefs
!print *, 'ecoefs dx11,dx12,xorigin = ',ecoefs,sigma
      dx11    = coefs(1)
      dx12    = coefs(2)
      xorigin = coefs(3)
      if (present(errors)) then
           errors%dx11    = ecoefs(1)
           errors%dx12    = ecoefs(2)
           errors%xorigin = ecoefs(3)
           errors%xsigma  = sigma
      end if

!----------use observations ylocs:

      call least_create (least,nterms)
      do i = 1,npoints
           observation = ylocs (i)
           terms(1)    = xgrids(i)
           terms(2)    = ygrids(i)
           terms(3)    = 1.0
           call least_add_point (least,terms,observation,weight)
      end do
      call least_delete (least,coefs,ecoefs,sigma)
!print *, ' coefs dx21,dx22,yorigin = ',coefs
!print *, 'ecoefs dx21,dx22,yorigin = ',ecoefs,sigma
      dx21    = coefs(1)
      dx22    = coefs(2)
      yorigin = coefs(3)
      if (present(errors)) then
           errors%dx21    = ecoefs(1)
           errors%dx22    = ecoefs(2)
           errors%yorigin = ecoefs(3)
           errors%ysigma  = sigma
      end if

!temporary:
obj%xorigin = xorigin
obj%yorigin = yorigin
obj%dx11    = dx11   
obj%dx12    = dx12   
obj%dx21    = dx21   
obj%dx22    = dx22   
call grid_set_reverse_from_forward (obj)
return

!----------check for nil coefficients:

      if (dx12 == DNIL .and. dx22 == DNIL) then
           if (dx11 == DNIL .or. dx11 == 0.0 .or. dx21 == DNIL) return
           dx22 = 1.0
           dx12 = - dx21 * dx22 / dx11
           if (present(errors)) then
                errors%dx22 = 0.0
                errors%dx12 = errors%dx21 * abs(dx22 / dx11)
           end if
      end if

      if (dx11 == DNIL .and. dx21 == DNIL) then
           if (dx22 == DNIL .or. dx22 == 0.0 .or. dx12 == DNIL) return
           dx11 = 1.0
           dx21 = - dx12 * dx11 / dx22
           if (present(errors)) then
                errors%dx11 = 0.0
                errors%dx21 = errors%dx12 * abs(dx11 / dx22)
           end if
      end if

      if (dx11    == DNIL .or. dx22    == DNIL) return
      if (dx21    == DNIL .or. dx12    == DNIL) return
      if (xorigin == DNIL .or. yorigin == DNIL) return

!----------average two solutions for dx12 and dx21:

  !   if (dx11 /= 0.0) then
  !        dx12_alt = - dx21 * dx22 / dx11
  !        dx12     = 0.5 * (dx12 + dx12_alt)
  !   end if

  !   if (dx22 /= 0.0) then
  !        dx21_alt = - dx12 * dx11 / dx22
  !        dx21     = 0.5 * (dx21 + dx21_alt)
  !   end if

      ! The above values and averages might not provide orthogonality.
      ! Therefore using grid_set_dx below guarantees orthogonality.

!----------save new transform:

      call grid_set_origin (obj,xorigin,yorigin)
      call grid_set_dx     (obj,dx11,dx21,dx12,dx22)    ! dx12 not used.
      return
      end subroutine grid_define_3d_transform


!!------------------------ define 3d transform alt -------------------------!!
!!------------------------ define 3d transform alt -------------------------!!
!!------------------------ define 3d transform alt -------------------------!!

!  xgrid = dn11 * (xloc - xorigin) + dn12 * (yloc - yorigin)
!  ygrid = dn21 * (xloc - xorigin) + dn22 * (yloc - yorigin)

!          coef1  term1  coef2  term2  --------------coef3--------------
!  xgrid = dn11 * xloc + dn12 * yloc - (dn11 * xorigin + dn12 * yorigin)
!  ygrid = dn21 * xloc + dn22 * yloc - (dn21 * xorigin + dn22 * yorigin)


      subroutine grid_define_3d_transform_alt  &
                        (obj, npoints, xlocs, ylocs, xgrids, ygrids, errors)
      implicit none
      type(grid_struct),intent(inout)        :: obj               ! argument
      integer          ,intent(in)           :: npoints           ! argument
      double precision ,intent(in)           :: xlocs (:)         ! argument
      double precision ,intent(in)           :: ylocs (:)         ! argument
      double precision ,intent(in)           :: xgrids(:)         ! argument
      double precision ,intent(in)           :: ygrids(:)         ! argument
      type(grid_errors),intent(out),optional :: errors            ! argument

      double precision                       :: dn12,dn21         ! local
      double precision                       :: dn11,dn22         ! local
      double precision                       :: xorigin,yorigin   ! local
      double precision                       :: coef3x,coef3y     ! local
      double precision                       :: ecoef3x,ecoef3y   ! local
      double precision                       :: denom             ! local

      double precision :: frac11,frac21,frac12,frac22,frac3x,frac3y
      double precision :: termx1,termx2,termy1,termy2
      integer                                :: i                 ! local
      double precision                       :: observation       ! local
      double precision                       :: sigma             ! local
      double precision ,parameter            :: weight = 1.0      ! local
      integer          ,parameter            :: nterms = 3        ! local
      double precision                       :: terms (nterms)    ! local
      double precision                       :: coefs (nterms)    ! local
      double precision                       :: ecoefs(nterms)    ! local
      type(least_struct),pointer             :: least             ! local

      nullify (least) ! jpa

!----------use observations xgrids:

      call least_create (least,nterms)
      do i = 1,npoints
           observation = xgrids(i)
           terms(1)    = xlocs (i)
           terms(2)    = ylocs (i)
           terms(3)    = 1.0
           call least_add_point (least,terms,observation,weight)
      end do
      call least_delete (least,coefs,ecoefs,sigma)
!print *, ' coefs dn11,dn12,coef3x = ',coefs
!print *, 'ecoefs dn11,dn12,coef3x = ',ecoefs,sigma
      dn11   = coefs(1)
      dn12   = coefs(2)
      coef3x = coefs(3)
      if (present(errors)) then
           errors%dx11    = ecoefs(1)
           errors%dx12    = ecoefs(2)
           errors%xorigin = DNIL             ! maybe temporary.
           ecoef3x        = ecoefs(3)
           errors%xsigma  = sigma
      end if

!----------use observations ygrids:

      call least_create (least,nterms)
      do i = 1,npoints
           observation = ygrids(i)
           terms(1)    = xlocs (i)
           terms(2)    = ylocs (i)
           terms(3)    = 1.0
           call least_add_point (least,terms,observation,weight)
      end do
      call least_delete (least,coefs,ecoefs,sigma)
!print *, ' coefs dn21,dn22,coef3y = ',coefs
!print *, 'ecoefs dn21,dn22,coef3y = ',ecoefs,sigma
      dn21   = coefs(1)
      dn22   = coefs(2)
      coef3y = coefs(3)
      if (present(errors)) then
           errors%dx21    = ecoefs(1)
           errors%dx22    = ecoefs(2)
           errors%yorigin = DNIL             ! maybe temporary.
           ecoef3y        = ecoefs(3)
           errors%ysigma  = sigma
      end if

!temporary:
denom = dn11 * dn22 - dn21 * dn12
if (dn12 == DNIL .or. dn11 == DNIL .or. coef3x == DNIL .or. &
    dn22 == DNIL .or. dn21 == DNIL .or. coef3y == DNIL .or. denom == 0.0) then
  obj%xorigin = DNIL
  obj%yorigin = DNIL
else
  obj%xorigin =   (dn12 * coef3y - dn22 * coef3x) / denom
  obj%yorigin = - (dn11 * coef3y - dn21 * coef3x) / denom
end if
obj%dn11    = dn11   
obj%dn12    = dn12   
obj%dn21    = dn21   
obj%dn22    = dn22   
call grid_set_forward_from_reverse (obj)
return

!----------check for nil coefficients:

      if (dn12 == DNIL .and. dn22 == DNIL) then
           if (dn11 == DNIL .or. dn11 == 0.0 .or. dn21 == DNIL) return
           dn22 = 1.0
           dn12 = - dn21 * dn11 / dn22
           if (present(errors)) then
                errors%dx22 = 0.0
                errors%dx12 = errors%dx21 * abs(dn11 / dn22)
           end if
      end if

      if (dn11 == DNIL .and. dn21 == DNIL) then
           if (dn22 == DNIL .or. dn22 == 0.0 .or. dn12 == DNIL) return
           dn11 = 1.0
           dn21 = - dn12 * dn22 / dn11
           if (present(errors)) then
                errors%dx11 = 0.0
                errors%dx21 = errors%dx12 * abs(dn22 / dn11)
           end if
      end if

      if (dn11   == DNIL .or. dn22   == DNIL) return
      if (dn21   == DNIL .or. dn12   == DNIL) return
      if (coef3x == DNIL .or. coef3y == DNIL) return

!----------derive origin (must do this before averaging two solutions below):

      denom = dn11 * dn22 - dn21 * dn12

      if (denom == 0.0) return

      xorigin =   (dn12 * coef3y - dn22 * coef3x) / denom
      yorigin = - (dn11 * coef3y - dn21 * coef3x) / denom

      if (present(errors)) then
           frac11 = 0.0
           frac21 = 0.0
           frac12 = 0.0
           frac22 = 0.0
           frac3x = 0.0
           frac3y = 0.0
           if (dn11   /= 0.0) frac11 = errors%dx11 / dn11
           if (dn21   /= 0.0) frac21 = errors%dx21 / dn21
           if (dn12   /= 0.0) frac12 = errors%dx12 / dn12
           if (dn22   /= 0.0) frac22 = errors%dx22 / dn22
           if (coef3x /= 0.0) frac3x = ecoef3x     / coef3x
           if (coef3y /= 0.0) frac3y = ecoef3y     / coef3y
           termx1 = dn12 * coef3y * dsqrt(frac12**2 + frac3y**2)
           termx2 = dn22 * coef3x * dsqrt(frac22**2 + frac3x**2)
           termy1 = dn11 * coef3y * dsqrt(frac11**2 + frac3y**2)
           termy2 = dn21 * coef3x * dsqrt(frac21**2 + frac3x**2)
           errors%xorigin = dsqrt(termx1**2 + termx2**2) / denom
           errors%yorigin = dsqrt(termy1**2 + termy2**2) / denom
      end if

!----------average two solutions for dn12 and dn21:

  !   if (dn11 /= 0.0) then
  !        dn21_alt = - dn12 * dn22 / dn11
  !        dn21     = 0.5 * (dn21 + dn21_alt)
  !   end if

  !   if (dn22 /= 0.0) then
  !        dn12_alt = - dn21 * dn11 / dn22
  !        dn12     = 0.5 * (dn12 + dn12_alt)
  !   end if

      ! The above values and averages might not provide orthogonality.
      ! Therefore using grid_set_dn below guarantees orthogonality.

!----------save new transform:

      call grid_set_origin (obj,xorigin,yorigin)
      call grid_set_dn     (obj,dn11,dn21,dn12,dn22)    ! dn21 not used.
      return
      end subroutine grid_define_3d_transform_alt


!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!


      end module grid_module


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

