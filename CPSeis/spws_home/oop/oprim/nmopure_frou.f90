!<CPS_v1 type="AUXILIARY_FILE"/>
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
!!-------------------------- nmopure_frou.f90 ------------------------------!!
!!-------------------------- nmopure_frou.f90 ------------------------------!!
!!-------------------------- nmopure_frou.f90 ------------------------------!!

   ! other files are:  nmopure.f90  nmopure_wrapper.cc  nmopure_wrapper.hh




!<brief_doc>
!------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : NMOPURE        (pure normal moveout primitive)
! Category   : velocity
! Written    : 2003-01-14   by: Tom Stoeckley
! Revised    : 2003-01-14   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : To apply or remove NMO corrections to seismic data.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2003-01-14  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!


      module nmopure_frou_module

      use nmopure_module
      use string_module
      use convert_module
      implicit none
      public

      character(len=100),public,save :: NMOPURE_FROU_IDENT = &
'$Id: nmopure_frou.f90,v 1.1 2002/04/08 19:04:26 Stoeckley beta sps $'

      type :: nmopure_frou_struct
        type(nmopure_struct),pointer :: obj
      end type

      end module nmopure_frou_module


!!-------------------------------- create ---------------------------------!!
!!-------------------------------- create ---------------------------------!!
!!-------------------------------- create ---------------------------------!!


      subroutine nmopure_frou_create (fpoint,ndpt,tstrt,dt,action,      &
                                      order,terpmode,doppler,tracemute, &
                                      error,msg)
      use nmopure_frou_module
      implicit none
      type(nmopure_frou_struct),intent(out) :: fpoint           ! arguments
      integer                  ,intent(in)  :: ndpt             ! arguments
      real                     ,intent(in)  :: tstrt            ! arguments
      real                     ,intent(in)  :: dt               ! arguments
      integer                  ,intent(in)  :: action           ! arguments
      integer                  ,intent(in)  :: order            ! arguments
      integer                  ,intent(in)  :: terpmode         ! arguments
      real                     ,intent(in)  :: doppler          ! arguments
      integer                  ,intent(in)  :: tracemute        ! arguments
      integer                  ,intent(out) :: error            ! arguments
      integer                  ,intent(out) :: msg(*)           ! arguments
      type(nmopure_struct)     ,pointer     :: obj              ! local
      logical                               :: tracemute9       ! local
      logical                               :: error9           ! local
      character(len=80)                     :: msg9             ! local

      call convert_ii2ll  (tracemute, tracemute9)

      call nmopure_create (obj,ndpt,tstrt,dt,action,  &
                           order,terpmode,doppler,tracemute9,error9,msg9)
      fpoint%obj => obj

      call convert_ll2ii  (error9, error)
      call string_cc2hh   (msg9  , msg)

      end subroutine nmopure_frou_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine nmopure_frou_delete (fpoint)
      use nmopure_frou_module
      implicit none
      type(nmopure_frou_struct),intent(inout) :: fpoint          ! argument
      type(nmopure_struct)     ,pointer       :: obj             ! local

      obj => fpoint%obj
      call nmopure_delete (obj)
      fpoint%obj => obj

      end subroutine nmopure_frou_delete


!!----------------------------- velfun -------------------------------------!!
!!----------------------------- velfun -------------------------------------!!
!!----------------------------- velfun -------------------------------------!!


      subroutine nmopure_frou_velfun (fpoint,npicks2,tpicks2,vpicks2, &
                                             npicks4,tpicks4,vpicks4,error,msg)
      use nmopure_frou_module
      implicit none
      type(nmopure_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                  ,intent(in)    :: npicks2          ! arguments
      real                     ,intent(in)    :: tpicks2(npicks2) ! arguments
      real                     ,intent(in)    :: vpicks2(npicks2) ! arguments
      integer                  ,intent(in)    :: npicks4          ! arguments
      real                     ,intent(in)    :: tpicks4(npicks4) ! arguments
      real                     ,intent(in)    :: vpicks4(npicks4) ! arguments
      integer                  ,intent(out)   :: error            ! arguments
      integer                  ,intent(out)   :: msg(*)           ! arguments
      type(nmopure_struct)     ,pointer       :: obj              ! local
      logical                                 :: error9           ! local
      character(len=80)                       :: msg9             ! local

      obj => fpoint%obj
      call nmopure_velfun (obj,npicks2,tpicks2,vpicks2, &
                               npicks4,tpicks4,vpicks4,error9,msg9)

      call convert_ll2ii  (error9, error)
      call string_cc2hh   (msg9  , msg)

      end subroutine nmopure_frou_velfun


!!----------------------------- apply -------------------------------------!!
!!----------------------------- apply -------------------------------------!!
!!----------------------------- apply -------------------------------------!!


      subroutine nmopure_frou_apply (fpoint,offset,offnew,ndpt,tr,mtop,mbottom)
      use nmopure_frou_module
      implicit none
      type(nmopure_frou_struct),intent(inout) :: fpoint           ! arguments
      real                     ,intent(inout) :: offset,offnew    ! arguments
      integer                  ,intent(in)    :: ndpt             ! arguments
      real                     ,intent(inout) :: tr(ndpt)         ! arguments
      integer                  ,intent(inout) :: mtop,mbottom     ! arguments
      type(nmopure_struct)     ,pointer       :: obj              ! local

      obj => fpoint%obj
      call nmopure_apply (obj,offset,offnew,tr,mtop,mbottom)

      end subroutine nmopure_frou_apply


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

