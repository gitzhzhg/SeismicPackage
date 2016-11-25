!<CPS_v1 type="AUXILIARY_FILE"/>
!!----------------------------- grid_frou.f90 -----------------------------!!
!!----------------------------- grid_frou.f90 -----------------------------!!
!!----------------------------- grid_frou.f90 -----------------------------!!

   ! other files are:  grid_transform.cc  grid_transform.hh  grid.f90


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


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2002-04-23  Stoeckley  Fix compiler warnings.
!  1. 2002-04-11  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>



!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!


      module grid_frou_module
      use grid_module
      implicit none
      public

      type,public :: grid_frou_struct
        type(grid_struct),pointer :: obj
      end type grid_frou_struct

      end module grid_frou_module


!!----------------------- create and delete ----------------------------!!
!!----------------------- create and delete ----------------------------!!
!!----------------------- create and delete ----------------------------!!


      subroutine grid_frou_create (fpoint)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(out) :: fpoint               ! argument
      type(grid_struct)     ,pointer     :: obj                  ! local

      allocate (obj)
      call grid_initialize (obj)
      fpoint%obj => obj
      return
      end subroutine grid_frou_create



      subroutine grid_frou_delete (fpoint)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      deallocate (obj)
      fpoint%obj => obj
      return
      end subroutine grid_frou_delete


!!--------------------------- initialize -------------------------------!!
!!--------------------------- initialize -------------------------------!!
!!--------------------------- initialize -------------------------------!!


      subroutine grid_frou_initialize (fpoint)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_initialize (obj)
      return
      end subroutine grid_frou_initialize


!!--------------------------- grid equal -------------------------------!!
!!--------------------------- grid equal -------------------------------!!
!!--------------------------- grid equal -------------------------------!!


      function grid_frou_equal (fpoint1, fpoint2) result (equal)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint1,fpoint2      ! argument
      integer                           :: equal                ! result
      type(grid_struct)     ,pointer    :: obj1,obj2            ! local

      obj1 => fpoint1%obj
      obj2 => fpoint2%obj
      if (grid_equal(obj1,obj2)) then
           equal = 1
      else
           equal = 0
      end if
      return
      end function grid_frou_equal


!!--------------------------- grid unequal -------------------------------!!
!!--------------------------- grid unequal -------------------------------!!
!!--------------------------- grid unequal -------------------------------!!


      function grid_frou_unequal (fpoint1, fpoint2) result (unequal)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint1,fpoint2      ! argument
      integer                           :: unequal              ! result
      type(grid_struct)     ,pointer    :: obj1,obj2            ! local

      obj1 => fpoint1%obj
      obj2 => fpoint2%obj
      if (grid_unequal(obj1,obj2)) then
           unequal = 1
      else
           unequal = 0
      end if
      return
      end function grid_frou_unequal


!!------------------------------ grid copy -------------------------------!!
!!------------------------------ grid copy -------------------------------!!
!!------------------------------ grid copy -------------------------------!!


      subroutine grid_frou_copy (fpoint2, fpoint1)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint2              ! argument
      type(grid_frou_struct),intent(in)    :: fpoint1              ! argument
      type(grid_struct)     ,pointer       :: obj2,obj1            ! local

      obj2 => fpoint2%obj     ! output
      obj1 => fpoint1%obj     ! input
      obj2 = obj1
      return
      end subroutine grid_frou_copy


!!----------------------------- get values -----------------------------!!
!!----------------------------- get values -----------------------------!!
!!----------------------------- get values -----------------------------!!


      function grid_frou_get_xorigin (fpoint) result (xorigin)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: xorigin              ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      xorigin = grid_get_xorigin (obj)
      return
      end function grid_frou_get_xorigin



      function grid_frou_get_yorigin (fpoint) result (yorigin)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: yorigin              ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      yorigin = grid_get_yorigin (obj)
      return
      end function grid_frou_get_yorigin



      function grid_frou_get_rotation_angle (fpoint) result (angle)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: angle                ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      angle = grid_get_rotation_angle (obj)
      return
      end function grid_frou_get_rotation_angle



      function grid_frou_get_xgrid_width (fpoint) result (xwidth)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: xwidth               ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      xwidth = grid_get_xgrid_width (obj)
      return
      end function grid_frou_get_xgrid_width



      function grid_frou_get_ygrid_width (fpoint) result (ywidth)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: ywidth               ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      ywidth = grid_get_ygrid_width (obj)
      return
      end function grid_frou_get_ygrid_width



      function grid_frou_get_handedness (fpoint) result (handedness)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      integer                           :: handedness           ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      handedness = grid_get_handedness (obj)
      return
      end function grid_frou_get_handedness


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_get_origin (fpoint, xorigin, yorigin)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint               ! argument
      double precision      ,intent(out) :: xorigin              ! argument
      double precision      ,intent(out) :: yorigin              ! argument
      type(grid_struct)     ,pointer     :: obj                  ! local

      obj => fpoint%obj
      call grid_get_origin (obj,xorigin,yorigin)
      return
      end subroutine grid_frou_get_origin



      subroutine grid_frou_get_widths (fpoint, xwidth, ywidth)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(out) :: xwidth              ! argument
      double precision      ,intent(out) :: ywidth              ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_get_widths (obj,xwidth,ywidth)
      return
      end subroutine grid_frou_get_widths


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_frou_get_cosine_angle (fpoint) result (cosa)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: cosa                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      cosa = grid_get_cosine_angle (obj)
      return
      end function grid_frou_get_cosine_angle



      function grid_frou_get_sine_angle (fpoint) result (sina)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: sina                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      sina = grid_get_sine_angle (obj)
      return
      end function grid_frou_get_sine_angle


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_frou_get_dx11 (fpoint) result (dx11)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dx11                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dx11 = grid_get_dx11 (obj)
      return
      end function grid_frou_get_dx11


      function grid_frou_get_dx21 (fpoint) result (dx21)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dx21                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dx21 = grid_get_dx21 (obj)
      return
      end function grid_frou_get_dx21


      function grid_frou_get_dx12 (fpoint) result (dx12)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dx12                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dx12 = grid_get_dx12 (obj)
      return
      end function grid_frou_get_dx12


      function grid_frou_get_dx22 (fpoint) result (dx22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dx22                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dx22 = grid_get_dx22 (obj)
      return
      end function grid_frou_get_dx22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_frou_get_dn11 (fpoint) result (dn11)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dn11                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dn11 = grid_get_dn11 (obj)
      return
      end function grid_frou_get_dn11


      function grid_frou_get_dn21 (fpoint) result (dn21)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dn21                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dn21 = grid_get_dn21 (obj)
      return
      end function grid_frou_get_dn21


      function grid_frou_get_dn12 (fpoint) result (dn12)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dn12                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dn12 = grid_get_dn12 (obj)
      return
      end function grid_frou_get_dn12


      function grid_frou_get_dn22 (fpoint) result (dn22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: dn22                 ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      dn22 = grid_get_dn22 (obj)
      return
      end function grid_frou_get_dn22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      function grid_frou_get_determinant (fpoint) result (determinant)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint               ! arguments
      double precision                  :: determinant          ! result
      type(grid_struct)     ,pointer    :: obj                  ! local

      obj => fpoint%obj
      determinant = grid_get_determinant (obj)
      return
      end function grid_frou_get_determinant


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_get_dx (fpoint, dx11,dx21,dx12,dx22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint               ! arguments
      double precision      ,intent(out) :: dx11,dx21,dx12,dx22  ! arguments
      type(grid_struct)     ,pointer     :: obj                  ! local

      obj => fpoint%obj
      call grid_get_dx (obj, dx11,dx21,dx12,dx22)
      return
      end subroutine grid_frou_get_dx



      subroutine grid_frou_get_dn (fpoint, dn11,dn21,dn12,dn22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint               ! arguments
      double precision      ,intent(out) :: dn11,dn21,dn12,dn22  ! arguments
      type(grid_struct)     ,pointer     :: obj                  ! local

      obj => fpoint%obj
      call grid_get_dn (obj, dn11,dn21,dn12,dn22)
      return
      end subroutine grid_frou_get_dn


!!----------------------------- set values -----------------------------!!
!!----------------------------- set values -----------------------------!!
!!----------------------------- set values -----------------------------!!


      subroutine grid_frou_set_xorigin (fpoint, xorigin)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! arguments
      double precision      ,intent(in)    :: xorigin              ! arguments
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_xorigin (obj, xorigin)
      return
      end subroutine grid_frou_set_xorigin



      subroutine grid_frou_set_yorigin (fpoint, yorigin)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! arguments
      double precision      ,intent(in)    :: yorigin              ! arguments
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_yorigin (obj, yorigin)
      return
      end subroutine grid_frou_set_yorigin



      subroutine grid_frou_set_rotation_angle (fpoint, angle)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! arguments
      double precision      ,intent(in)    :: angle                ! arguments
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_rotation_angle (obj, angle)
      return
      end subroutine grid_frou_set_rotation_angle



      subroutine grid_frou_set_xgrid_width (fpoint, xwidth)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! arguments
      double precision      ,intent(in)    :: xwidth               ! arguments
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_xgrid_width (obj, xwidth)
      return
      end subroutine grid_frou_set_xgrid_width



      subroutine grid_frou_set_ygrid_width (fpoint, ywidth)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! arguments
      double precision      ,intent(in)    :: ywidth               ! arguments
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_ygrid_width (obj, ywidth)
      return
      end subroutine grid_frou_set_ygrid_width



      subroutine grid_frou_set_handedness (fpoint, handedness)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! arguments
      integer               ,intent(in)    :: handedness           ! arguments
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_handedness (obj, handedness)
      return
      end subroutine grid_frou_set_handedness


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_set_origin (fpoint, xorigin, yorigin)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: xorigin              ! argument
      double precision      ,intent(in)    :: yorigin              ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_origin (obj, xorigin,yorigin)
      return
      end subroutine grid_frou_set_origin



      subroutine grid_frou_set_widths (fpoint, xwidth, ywidth)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint              ! argument
      double precision      ,intent(in)    :: xwidth              ! argument
      double precision      ,intent(in)    :: ywidth              ! argument
      type(grid_struct)     ,pointer       :: obj                 ! local

      obj => fpoint%obj
      call grid_set_widths (obj, xwidth,ywidth)
      return
      end subroutine grid_frou_set_widths


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_set_transform                                    &
                   (fpoint,xorigin,yorigin,angle,xwidth,ywidth,handedness)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint              ! argument
      double precision      ,intent(in)    :: xorigin             ! argument
      double precision      ,intent(in)    :: yorigin             ! argument
      double precision      ,intent(in)    :: angle               ! argument
      double precision      ,intent(in)    :: xwidth              ! argument
      double precision      ,intent(in)    :: ywidth              ! argument
      integer               ,intent(in)    :: handedness          ! argument
      type(grid_struct)     ,pointer       :: obj                 ! local

      obj => fpoint%obj
      call grid_set_transform                                      &
                   (obj,xorigin,yorigin,angle,xwidth,ywidth,handedness)
      return
      end subroutine grid_frou_set_transform


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_set_dx11 (fpoint, dx11)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dx11                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dx11 (obj, dx11)
      return
      end subroutine grid_frou_set_dx11



      subroutine grid_frou_set_dx21 (fpoint, dx21)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dx21                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dx21 (obj, dx21)
      return
      end subroutine grid_frou_set_dx21



      subroutine grid_frou_set_dx12 (fpoint, dx12)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dx12                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dx12 (obj, dx12)
      return
      end subroutine grid_frou_set_dx12



      subroutine grid_frou_set_dx22 (fpoint, dx22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dx22                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dx22 (obj, dx22)
      return
      end subroutine grid_frou_set_dx22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_set_dn11 (fpoint, dn11)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dn11                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dn11 (obj, dn11)
      return
      end subroutine grid_frou_set_dn11



      subroutine grid_frou_set_dn21 (fpoint, dn21)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dn21                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dn21 (obj, dn21)
      return
      end subroutine grid_frou_set_dn21



      subroutine grid_frou_set_dn12 (fpoint, dn12)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dn12                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dn12 (obj, dn12)
      return
      end subroutine grid_frou_set_dn12



      subroutine grid_frou_set_dn22 (fpoint, dn22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dn22                 ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dn22 (obj, dn22)
      return
      end subroutine grid_frou_set_dn22


                         !!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_set_dx (fpoint, dx11,dx21,dx12,dx22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dx11,dx21,dx12,dx22  ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dx (obj, dx11,dx21,dx12,dx22)
      return
      end subroutine grid_frou_set_dx



      subroutine grid_frou_set_dn (fpoint, dn11,dn21,dn12,dn22)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(inout) :: fpoint               ! argument
      double precision      ,intent(in)    :: dn11,dn21,dn12,dn22  ! argument
      type(grid_struct)     ,pointer       :: obj                  ! local

      obj => fpoint%obj
      call grid_set_dn (obj, dn11,dn21,dn12,dn22)
      return
      end subroutine grid_frou_set_dn


!!--------------------- get transformed coordinates ----------------------!!
!!--------------------- get transformed coordinates ----------------------!!
!!--------------------- get transformed coordinates ----------------------!!


      function grid_frou_get_xsurvey_coord (fpoint, xgrid, ygrid) result (xloc)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xgrid               ! argument
      double precision      ,intent(in) :: ygrid               ! argument
      double precision                  :: xloc                ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      xloc = grid_get_xsurvey_coord (obj, xgrid, ygrid)
      return
      end function grid_frou_get_xsurvey_coord



      function grid_frou_get_ysurvey_coord (fpoint, xgrid, ygrid) result (yloc)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xgrid               ! argument
      double precision      ,intent(in) :: ygrid               ! argument
      double precision                  :: yloc                ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      yloc = grid_get_ysurvey_coord (obj, xgrid, ygrid)
      return
      end function grid_frou_get_ysurvey_coord



      function grid_frou_get_xgrid_coord (fpoint, xloc, yloc) result (xgrid)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xloc                ! argument
      double precision      ,intent(in) :: yloc                ! argument
      double precision                  :: xgrid               ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      xgrid = grid_get_xgrid_coord (obj, xloc, yloc)
      return
      end function grid_frou_get_xgrid_coord



      function grid_frou_get_ygrid_coord (fpoint, xloc, yloc) result (ygrid)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xloc                ! argument
      double precision      ,intent(in) :: yloc                ! argument
      double precision                  :: ygrid               ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      ygrid = grid_get_ygrid_coord (obj, xloc, yloc)
      return
      end function grid_frou_get_ygrid_coord


                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_get_survey_coords (fpoint, xgrid, ygrid, xloc, yloc)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xgrid               ! argument
      double precision      ,intent(in)  :: ygrid               ! argument
      double precision      ,intent(out) :: xloc                ! argument
      double precision      ,intent(out) :: yloc                ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_get_survey_coords (obj, xgrid, ygrid, xloc, yloc)
      return
      end subroutine grid_frou_get_survey_coords



      subroutine grid_frou_get_grid_coords (fpoint, xloc, yloc, xgrid, ygrid)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xloc                ! argument
      double precision      ,intent(in)  :: yloc                ! argument
      double precision      ,intent(out) :: xgrid               ! argument
      double precision      ,intent(out) :: ygrid               ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_get_grid_coords (obj, xloc, yloc, xgrid, ygrid)
      return
      end subroutine grid_frou_get_grid_coords


!!--------------------------- do cmp binning -----------------------------!!
!!--------------------------- do cmp binning -----------------------------!!
!!--------------------------- do cmp binning -----------------------------!!


      function grid_frou_get_xbin_number  &
                                   (fpoint, xloc, yloc) result (xbin_number)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xloc                ! argument
      double precision      ,intent(in) :: yloc                ! argument
      integer                           :: xbin_number         ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      xbin_number = grid_get_xbin_number (obj, xloc, yloc)
      return
      end function grid_frou_get_xbin_number



      function grid_frou_get_ybin_number  &
                                   (fpoint, xloc, yloc) result (ybin_number)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xloc                ! argument
      double precision      ,intent(in) :: yloc                ! argument
      integer                           :: ybin_number         ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      ybin_number = grid_get_ybin_number (obj, xloc, yloc)
      return
      end function grid_frou_get_ybin_number



      function grid_frou_get_xbin_center  &
                                   (fpoint, xgrid, ygrid) result (xbin_center)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xgrid               ! argument
      double precision      ,intent(in) :: ygrid               ! argument
      double precision                  :: xbin_center         ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      xbin_center = grid_get_xbin_center (obj, xgrid, ygrid)
      return
      end function grid_frou_get_xbin_center



      function grid_frou_get_ybin_center  &
                                   (fpoint, xgrid, ygrid) result (ybin_center)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in) :: fpoint              ! argument
      double precision      ,intent(in) :: xgrid               ! argument
      double precision      ,intent(in) :: ygrid               ! argument
      double precision                  :: ybin_center         ! result
      type(grid_struct)     ,pointer    :: obj                 ! local

      obj => fpoint%obj
      ybin_center = grid_get_ybin_center (obj, xgrid, ygrid)
      return
      end function grid_frou_get_ybin_center


                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine grid_frou_get_bin_numbers  &
                             (fpoint, xloc, yloc, xbin_number, ybin_number)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xloc                ! argument
      double precision      ,intent(in)  :: yloc                ! argument
      integer               ,intent(out) :: xbin_number         ! argument
      integer               ,intent(out) :: ybin_number         ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_get_bin_numbers (obj, xloc, yloc, xbin_number, ybin_number)
      return
      end subroutine grid_frou_get_bin_numbers



      subroutine grid_frou_get_bin_centers  &
                            (fpoint, xgrid, ygrid, xbin_center, ybin_center)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xgrid               ! argument
      double precision      ,intent(in)  :: ygrid               ! argument
      double precision      ,intent(out) :: xbin_center         ! argument
      double precision      ,intent(out) :: ybin_center         ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_get_bin_centers (obj, xgrid, ygrid, xbin_center, ybin_center)
      return
      end subroutine grid_frou_get_bin_centers


!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!
!!-------------------------- define coordinate system ---------------------!!


      subroutine grid_frou_define_origin (fpoint, xgrid, ygrid, xloc, yloc)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xgrid               ! argument
      double precision      ,intent(in)  :: ygrid               ! argument
      double precision      ,intent(in)  :: xloc                ! argument
      double precision      ,intent(in)  :: yloc                ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_define_origin (obj, xgrid, ygrid, xloc, yloc)
      return
      end subroutine grid_frou_define_origin



      subroutine grid_frou_define_rotation_angle &
                                      (fpoint, xloc1, yloc1, xloc2, yloc2)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xloc1               ! argument
      double precision      ,intent(in)  :: yloc1               ! argument
      double precision      ,intent(in)  :: xloc2               ! argument
      double precision      ,intent(in)  :: yloc2               ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_define_rotation_angle (obj, xloc1, yloc1, xloc2, yloc2)
      return
      end subroutine grid_frou_define_rotation_angle



      subroutine grid_frou_def_origin_and_angle (fpoint, xgrid,ygrid, &
                                          xloc1, yloc1, xloc2, yloc2)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xgrid               ! argument
      double precision      ,intent(in)  :: ygrid               ! argument
      double precision      ,intent(in)  :: xloc1               ! argument
      double precision      ,intent(in)  :: yloc1               ! argument
      double precision      ,intent(in)  :: xloc2               ! argument
      double precision      ,intent(in)  :: yloc2               ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_define_origin_and_angle (obj, xgrid,ygrid, &
                                          xloc1, yloc1, xloc2, yloc2)
      return
      end subroutine grid_frou_def_origin_and_angle



      subroutine grid_frou_refine_bin_center            (fpoint, xloc , yloc)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xloc                ! argument
      double precision      ,intent(in)  :: yloc                ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_refine_bin_center            (obj, xloc , yloc)
      return
      end subroutine grid_frou_refine_bin_center



      subroutine grid_frou_refine_rotation_angle        (fpoint, xloc , yloc)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xloc                ! argument
      double precision      ,intent(in)  :: yloc                ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_refine_rotation_angle        (obj, xloc , yloc)
      return
      end subroutine grid_frou_refine_rotation_angle



      subroutine grid_frou_increment_grid_coords   (fpoint, xstep, ystep)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      double precision      ,intent(in)  :: xstep               ! argument
      double precision      ,intent(in)  :: ystep               ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_increment_grid_coords   (obj, xstep, ystep)
      return
      end subroutine grid_frou_increment_grid_coords



      subroutine grid_frou_define_transform &
                   (fpoint, npoints, xloc, yloc, xgrid, ygrid, xresid, yresid)
      use grid_frou_module
      implicit none
      type(grid_frou_struct),intent(in)  :: fpoint              ! argument
      integer               ,intent(in)  :: npoints             ! argument
      double precision      ,intent(in)  :: xloc  (npoints)     ! argument
      double precision      ,intent(in)  :: yloc  (npoints)     ! argument
      double precision      ,intent(in)  :: xgrid (npoints)     ! argument
      double precision      ,intent(in)  :: ygrid (npoints)     ! argument
      double precision      ,intent(out) :: xresid(npoints)     ! argument
      double precision      ,intent(out) :: yresid(npoints)     ! argument
      type(grid_struct)     ,pointer     :: obj                 ! local

      obj => fpoint%obj
      call grid_define_transform &
                      (obj, npoints, xloc, yloc, xgrid, ygrid, xresid, yresid)
      return
      end subroutine grid_frou_define_transform


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

