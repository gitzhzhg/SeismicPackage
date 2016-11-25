! <license>
! ------------------------------------------------------------------------------
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
! ------------------------------------------------------------------------------
! </license>

!!-------------------------- TestFortran.f90 ------------------------------!!
!!-------------------------- TestFortran.f90 ------------------------------!!
!!-------------------------- TestFortran.f90 ------------------------------!!

! Main program to test Fortran90 access to CPSeis modules.

  program testfortran

  use xp_module
  use pc_module
  use grid_module

  implicit none

  type(xp_struct),pointer  :: obj
  double precision         :: xorigin
  double precision         :: yorigin
  double precision         :: xwidth
  double precision         :: ywidth
  double precision         :: angle
  integer                  :: handedness
  type(grid_struct)        :: grid
  integer                  :: nwih1
  integer                  :: ndpt1
  integer                  :: numtr1
  real                     :: tstrt
  real                     :: dt
  integer                  :: nwih2
  integer                  :: ndpt2
  integer                  :: numtr2
  double precision,pointer :: hd(:,:)
  real            ,pointer :: tr(:,:)
  integer                  :: ntr
  integer                  :: itr
  integer                  :: i

!!------------------------------- main ------------------------------------!!
!!------------------------------- main ------------------------------------!!
!!------------------------------- main ------------------------------------!!

  nullify (obj)

!!----------initialize parameters:

  xorigin    = 100000.0
  yorigin    = 700000.0
  xwidth     = 100.0
  ywidth     = 200.0
  angle      = 89.0
  handedness = 1

  print *, " "
  print *, "TestFortran: xorigin =================== ", xorigin
  print *, "TestFortran: yorigin =================== ", yorigin
  print *, "TestFortran: xwidth ==================== ", xwidth
  print *, "TestFortran: ywidth ==================== ", ywidth
  print *, "TestFortran: angle ===================== ", angle
  print *, "TestFortran: handedness ================ ", handedness
  print *, " "

  call grid_set_transform (grid, xorigin, yorigin, angle, xwidth, ywidth, handedness)

  nwih1  = 66
  ndpt1  = 2001
  numtr1 = 6
  tstrt  = 0.0
  dt     = 0.004

!!----------create and update:

  call pc_backend_update (6)

  call pc_put        ("win_inc"    , 0.44)
  call pc_put_global ("nwih"       , nwih1)
  call pc_put_global ("ndpt"       , ndpt1)
  call pc_put_global ("numtr"      , numtr1)
  call pc_put_global ("tstrt"      , tstrt)
  call pc_put_global ("dt"         , dt)
  call pc_put_global ("grid"       , grid)

  call cps_startup  ! manhist needs some globals in parameter cache.

  print *, " "
  print *, "Global data cards:"
  call pc_print_global_cards

  call xp_create (obj)

  print *, "Process data cards:"
  call pc_print_process_cards
  print *, "Global data cards:"
  call pc_print_global_cards
  print *, " "

  nwih2  = nwih1
  ndpt2  = ndpt1
  numtr2 = numtr1

  call pc_get_global ("nwih" , nwih2)
  call pc_get_global ("ndpt" , ndpt2)
  call pc_get_global ("numtr", numtr2)

  call pc_backend_execute

!!----------initialize trace arrays:

  allocate (hd(max(nwih1,nwih2), max(numtr1,numtr2)))
  allocate (tr(max(ndpt1,ndpt2), max(numtr1,numtr2)))

!!----------execute:

  ntr = numtr1

  do itr = 1,ntr
      do i = 1,nwih1 ; hd(i,itr) = itr + 11.11 + i - 2 ; enddo
      do i = 1,ndpt1 ; tr(i,itr) = itr + 33.33 + i - 2 ; enddo
      print *, "before ", itr, hd(5,itr), tr(78,itr), tr(79,itr)
  enddo

  call xp (obj, ntr, hd, tr)

  print *, " "
  do itr = 1,ntr
      print *, "after  ", itr, hd(5,itr), tr(78,itr), tr(79,itr)
  enddo

!!----------wrapup:

  call xp_delete (obj)
  call pc_restore

  deallocate (hd)
  deallocate (tr)

  print *, " "
  print *, "finished with fortran test program"

  end program testfortran

!!-------------------------------- end ---------------------------------!!
!!-------------------------------- end ---------------------------------!!
!!-------------------------------- end ---------------------------------!!

