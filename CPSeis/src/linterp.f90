!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- linterp.f90 --------------------------------!!
!!---------------------------- linterp.f90 --------------------------------!!
!!---------------------------- linterp.f90 --------------------------------!!


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
! Name       : LINTERP         (LINear inTERPolation object)
! Category   : math
! Written    : 2000-12-14   by: Tom Stoeckley
! Revised    : 2000-12-14   by: Tom Stoeckley
! Maturity   : production   2001-02-22
! Purpose    : 2D linear interpolation.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive interpolates among control points distributed on a 2D plane.
! The algorithm is bilinear interpolation, first in the X direction along
! lines of constant Y coordinate, and then in the Y direction between the
! two bracketing Y lines.
!
! There must be one or more lines of constant Y coordinate.  The spacing
! between these lines can be variable.  Each line must contain one or more
! control points.  The spacing between the control points can be variable
! along the line.  The number and distribution of control points can vary
! from one line to another.  No two control points can occupy the same
! location.
!
! To get an interpolated point at location (XCOORD,YCOORD), this algorithm
! first finds the two lines bracketing YCOORD.  Then, on each line, the
! algorithm finds the two control points bracketing XCOORD.  Finally, the
! algorithm calculates the weights to use for the four control points to
! determine an interpolated value.
!
! Example of a valid distribution of control points on a plane, showing the
! interpolation procedure:
!
! The control points are marked with the # character.
! The interpolated point is marked with the $ character.
!
!         line Y1 -->   - - - # - - - - # - - - - - - - - # - # - - -
!
!
!         line Y2 -->   - - # - # - - # - - #-----# - - - - - - - - -
!                                             |
!                                             |
!                                             $
!                                             |
!         line Y3 -->   - - - - #-------------------# - - - - - # - -
!     
!         line Y4 -->   - - - - - - - - # - - - - - - - # - - - - - -
!
!
!                                     X ------>
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
! To get an interpolated value at a point on a plane:
!
!                                o     i       i       i     o
!          call linterp_create (obj,xcoords,ycoords,ncoords,msg)
!          call linterp_delete (obj)
!                                b
!                                         <---------------opt----------------->
!                        b    i      i     o  o  o  o  o  o  o  o   o      o
! call linterp_weights (obj,xcoord,ycoord,i1,i2,i3,i4,w1,w2,w3,w4,moved,escaped)
!
!          zvalue = linterp_answer (obj,zvalues)
!                                    i     i   
!
! type(linterp_struct)   obj = pointer to the LINTERP data structure.
! real      xcoords(ncoords) = list of X coordinates of points on a plane.
! real      ycoords(ncoords) = list of Y coordinates of points on a plane.
! real      zvalues(ncoords) = ordinates of points on a plane.
! integer            ncoords = number of points on a plane.
! character(len=*)       msg = error message (blank if no error).
! real                xcoord = X coordinate of interpolated point.
! real                ycoord = Y coordinate of interpolated point.
! real                zvalue = interpolated zvalue.
! integer        i1,i2,i3,i4 = control point indices used (1 thru ncoords).
! real           w1,w2,w3,w4 = weights for each control point used (add to 1).
! logical              moved = whether coords have moved since the last call.
! logical            escaped = whether control points changed since last call.
!
! LINTERP_CREATE verifies that the points are sorted in the correct order
! and sets MSG to an error message if they are not.  The correct order is
! a primary sort of YCOORDS in increasing order, and for each value of
! YCOORD, a secondary sort of XCOORDS in increasing order.  If there is
! an error, the returned OBJ is nullified.
!
! LINTERP_WEIGHTS finds the bounding control points and weights to use to get
! an interpolated ordinate at the specified location.
!
! LINTERP_ANSWER uses the bounding control points and weights to get the
! interpolated ordinate.  This routine can be called several times with the
! same weights to get different interpolated values at the same location.
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
!  2.
!  1. 2001-02-22  Stoeckley  Initial version.
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


      module linterp_module
      use named_constants_module
      use terputil_module
      implicit none
      public

      character(len=100),public,save :: LINTERP_IDENT = &
'$Id: linterp.f90,v 1.1 2001/02/21 21:12:42 sps prod sps $'


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      type,public :: linterp_struct
           private
           real   ,pointer :: xcoords (:)
           real   ,pointer :: ycoords (:)
           integer,pointer :: ifirst  (:)
           integer,pointer :: ilast   (:)
           integer         :: ncoords,nycoords
           integer         :: i1,i2,i3,i4,i12,i34
           real            :: w1,w2,w3,w4
           real            :: x1,x2,x3,x4,x13,x24,y12,y34,x,y
           logical         :: starting
      end type


      contains


!!-------------------------- linterp create ----------------------------!!
!!-------------------------- linterp create ----------------------------!!
!!-------------------------- linterp create ----------------------------!!


      subroutine linterp_create (obj,xcoords,ycoords,ncoords,msg)
      implicit none
      type(linterp_struct),pointer     :: obj                 ! arguments
      real                ,intent(in)  :: xcoords(:)          ! arguments
      real                ,intent(in)  :: ycoords(:)          ! arguments
      integer             ,intent(in)  :: ncoords             ! arguments
      character(len=*)    ,intent(out) :: msg                 ! arguments
      integer                          :: icoord,nycoords     ! local

!----------check for errors and count the Y lines:

      nullify (obj)
      msg = ' '

      if (ncoords == 0) then
           msg = 'no coordinates specified'
           return
      end if

      nycoords = 1
      do icoord = 2,ncoords
           if (ycoords(icoord) > ycoords(icoord-1)) then
                nycoords = nycoords + 1
           else if (ycoords(icoord) < ycoords(icoord-1)) then
                msg = 'Y coordinates out of order'
                return
           else if (xcoords(icoord) == xcoords(icoord-1)) then
                msg = 'duplicate coordinate locations'
                return
           else if (xcoords(icoord) < xcoords(icoord-1)) then
                msg = 'X coordinates out of order'
                return
           end if
      end do

!----------allocate the object and arrays:

      allocate (obj)
      allocate (obj%xcoords(ncoords))
      allocate (obj%ycoords(nycoords))
      allocate (obj%ifirst (nycoords))
      allocate (obj%ilast  (nycoords))

!----------initialize the member variables:

      nycoords = 1
      obj%ifirst (nycoords) = 1
      do icoord = 2,ncoords
           if (ycoords(icoord) /= ycoords(icoord-1)) then
                obj%ilast  (nycoords) = icoord - 1
                nycoords              = nycoords + 1
                obj%ycoords(nycoords) = ycoords(icoord)
                obj%ifirst (nycoords) = icoord
           end if
      end do
      obj%ilast  (nycoords) = ncoords

      obj%ncoords  = ncoords
      obj%nycoords = nycoords
      obj%xcoords  = xcoords(1:ncoords)
      obj%i1       = 1
      obj%i2       = 1
      obj%i3       = 1
      obj%i4       = 1
      obj%w1       = 0.0
      obj%w2       = 0.0
      obj%w3       = 0.0
      obj%w4       = 0.0
      obj%starting = .true.

! print *, 'ncoords  xcoords = ',obj%ncoords,obj%xcoords
! print *, 'nycoords ycoords = ',obj%nycoords,obj%ycoords
! print *, 'nycoords ifirst  = ',obj%nycoords,obj%ifirst
! print *, 'nycoords ilast   = ',obj%nycoords,obj%ilast 

      return
      end subroutine linterp_create


!!-------------------------- linterp delete ----------------------------!!
!!-------------------------- linterp delete ----------------------------!!
!!-------------------------- linterp delete ----------------------------!!


      subroutine linterp_delete (obj)
      implicit none
      type(linterp_struct),pointer     :: obj                 ! arguments

      if (associated(obj)) then
           if (associated (obj%xcoords)) deallocate (obj%xcoords)
           if (associated (obj%ycoords)) deallocate (obj%ycoords)
           deallocate (obj)
      end if
      return
      end subroutine linterp_delete


!!---------------------------- linterp answer ------------------------------!!
!!---------------------------- linterp answer ------------------------------!!
!!---------------------------- linterp answer ------------------------------!!


      function linterp_answer (obj,zvalues) result (zvalue)
      implicit none
      type(linterp_struct),intent(inout) :: obj                   ! arguments
      real                ,intent(in)    :: zvalues(:)            ! arguments
      real                               :: zvalue                ! result

      zvalue = obj%w1 * zvalues(obj%i1) +  &
               obj%w2 * zvalues(obj%i2) +  &
               obj%w3 * zvalues(obj%i3) +  &
               obj%w4 * zvalues(obj%i4)
      return
      end function linterp_answer


!!-------------------------- linterp weights ------------------------------!!
!!-------------------------- linterp weights ------------------------------!!
!!-------------------------- linterp weights ------------------------------!!


      subroutine linterp_weights &
                    (obj,xcoord,ycoord,i1,i2,i3,i4,w1,w2,w3,w4,moved,escaped)
      implicit none
      type(linterp_struct),intent(inout) :: obj                   ! arguments
      real                ,intent(in)    :: xcoord                ! arguments
      real                ,intent(in)    :: ycoord                ! arguments
      integer,optional    ,intent(out)   :: i1,i2,i3,i4           ! arguments
      real   ,optional    ,intent(out)   :: w1,w2,w3,w4           ! arguments
      logical,optional    ,intent(out)   :: moved,escaped         ! arguments
      logical                            :: moved2x,escaped2x     ! local
      logical                            :: moved2y,escaped2y     ! local
      integer                            :: ifirst,ilast,n        ! local
      real                               :: fx12,fx34,fy          ! local
      real                               :: mx12,mx34,my          ! local
      real,parameter                     :: TOL = 0.01            ! local

!----------get started:

      if (obj%starting) then
           moved2y      = .true.
           escaped2y    = .true.
           obj%starting = .false.
      else
           moved2y   = (ycoord < obj%y   - TOL .or. ycoord > obj%y   + TOL)
           escaped2y = (ycoord < obj%y12 - TOL .or. ycoord > obj%y34 + TOL) &
                             .and. moved2y
      end if

!----------get bracketing Y coordinates:

      if (escaped2y) then
           call terputil_binary_search (ycoord,obj%ycoords,obj%nycoords, &
                                        obj%i12,obj%i34,obj%y12,obj%y34)
           moved2x   = .true.
           escaped2x = .true.
      else if (moved2y) then
           moved2x   = .true.
           obj%x13   = terputil_root (ycoord,obj%y12,obj%y34,obj%x1,obj%x3)
           obj%x24   = terputil_root (ycoord,obj%y12,obj%y34,obj%x2,obj%x4)
           escaped2x = (xcoord < obj%x13 - TOL .or. xcoord > obj%x24 + TOL)
      else
           moved2x   = (xcoord < obj%x   - TOL .or. xcoord > obj%x   + TOL)
           escaped2x = (xcoord < obj%x13 - TOL .or. xcoord > obj%x24 + TOL) &
                             .and. moved2x
      end if

!----------get bracketing X coordinates for first bracketing Y coordinate:

      if (escaped2x) then
           ifirst = obj%ifirst(obj%i12)
           ilast  = obj%ilast (obj%i12)
           n      = ilast - ifirst + 1

           call terputil_binary_search &
                      (xcoord,obj%xcoords(ifirst:ilast),n,i1,i2,obj%x1,obj%x2)

           obj%i1  = i1 + ifirst - 1
           obj%i2  = i2 + ifirst - 1

!----------get bracketing X coordinates for second bracketing Y coordinate:

           ifirst = obj%ifirst(obj%i34)
           ilast  = obj%ilast (obj%i34)
           n      = ilast - ifirst + 1

           call terputil_binary_search &
                      (xcoord,obj%xcoords(ifirst:ilast),n,i3,i4,obj%x3,obj%x4)

           obj%i3  = i3 + ifirst - 1
           obj%i4  = i4 + ifirst - 1

!----------get bracketing X coordinates for the exact Y coordinate:

           obj%x13 = terputil_root (ycoord,obj%y12,obj%y34,obj%x1,obj%x3)
           obj%x24 = terputil_root (ycoord,obj%y12,obj%y34,obj%x2,obj%x4)
      end if

!----------get weights:

      if (moved2x .or. moved2y) then
           fy     = terputil_fracdist (ycoord,obj%y12,obj%y34)
           fx12   = terputil_fracdist (xcoord,obj%x1,obj%x2)
           fx34   = terputil_fracdist (xcoord,obj%x3,obj%x4)
           mx12   = 1.0 - fx12
           mx34   = 1.0 - fx34
           my     = 1.0 - fy  
           obj%w1 = mx12 * my
           obj%w2 = fx12 * my
           obj%w3 = mx34 * fy
           obj%w4 = fx34 * fy
      end if

!----------finish up and return:

! print *, ' '
! print *, 'xcoord,ycoord,moved2y,escaped2y,moved2x,escaped2x = ', &
!           xcoord,ycoord,moved2y,escaped2y,moved2x,escaped2x
! print *, 'i1,i2,i3,i4,w1,w2,w3,w4 = ', &
!           obj%i1,obj%i2,obj%i3,obj%i4,obj%w1,obj%w2,obj%w3,obj%w4

      obj%x = xcoord
      obj%y = ycoord

      if (present(i1     )) i1      = obj%i1
      if (present(i2     )) i2      = obj%i2
      if (present(i3     )) i3      = obj%i3
      if (present(i4     )) i4      = obj%i4
      if (present(w1     )) w1      = obj%w1
      if (present(w2     )) w2      = obj%w2
      if (present(w3     )) w3      = obj%w3
      if (present(w4     )) w4      = obj%w4
      if (present(moved  )) moved   = (moved2x   .or. moved2y)
      if (present(escaped)) escaped = (escaped2x .or. escaped2y)
      return
      end subroutine linterp_weights


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module linterp_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

