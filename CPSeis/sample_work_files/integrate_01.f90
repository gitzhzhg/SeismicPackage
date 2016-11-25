!                           Start of CPSeis program integrate_01
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
!                       ********** COPYRIGHT NOTICE ************
!                       Licensed by ConocoPhillips Company 2007
!                    Using http://cpseis.org/docs/CPSeis_License.pdf
!                            As an Open-Source Product
!                         No Warranty implied or provided.
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!! GLOBAL DATA MODULE !!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
      module program_global_data
! 
      use named_constants_module
      use cps_module
! 
      use PROJECT_DATA_module
      use JOB_DATA_module
      use RNSYN_module
      use HEADSUM_module
      use INTEGRATE_module
! 
      implicit none
! 
      public
      type(PROJECT_DATA_struct),pointer   :: obj1
      type(JOB_DATA_struct),pointer       :: obj2
      type(RNSYN_struct),pointer          :: obj3
      type(HEADSUM_struct),pointer        :: obj4
      type(INTEGRATE_struct),pointer      :: obj5
! 
      end module program_global_data
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!! PROGRAM MAIN !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
      program integrate_01_cps
! 
      use program_global_data
! 
!!!!! DECLARATIONS:
! 
      double precision, pointer :: hd1(:,:)
      real            , pointer :: tr1(:,:)
! 
      integer :: ntr, ierr
      logical :: error
! 
      nullify (obj1)
      nullify (obj2)
      nullify (obj3)
      nullify (obj4)
      nullify (obj5)
! 
      call cps_start_processing
! 
      call cps_print_rcs_ident (trim(PROJECT_DATA_ident))
      call cps_print_rcs_ident (trim(JOB_DATA_ident))
      call cps_print_rcs_ident (trim(RNSYN_ident))
      call cps_print_rcs_ident (trim(HEADSUM_ident))
      call cps_print_rcs_ident (trim(INTEGRATE_ident))
      call cps_custom_rcs_ident
! 
!!!!! ALLOCATE ARRAYS:
! 
      ntr = 1
      allocate(hd1(64,ntr),stat=ierr)
      if (ierr /= 0) then
        call cps_print ('Unable to allocate hd1')
        error = .true.
        goto 9999
      endif
      allocate(tr1(1001,ntr),stat=ierr)
      if (ierr /= 0) then
        call cps_print ('Unable to allocate tr1')
        error = .true.
        goto 9999
      endif
! 
!!!!! SETUPS:
! 
      call cps_start_setups    ! stops if there is an error.
! 
      call cps_pre_setup; call PROJECT_DATA_create (obj1); call cps_post_setup
      call cps_pre_setup; call JOB_DATA_create     (obj2); call cps_post_setup
      call cps_pre_setup; call RNSYN_create        (obj3); call cps_post_setup
      call cps_pre_setup; call HEADSUM_create      (obj4); call cps_post_setup
      call cps_pre_setup; call INTEGRATE_create    (obj5); call cps_post_setup
! 
      call cps_finish_setups (error); if (error) go to 8001
! 
!!!!! EXECUTION:
! 
      ntr = NEED_TRACES
! 
 10   call cps_top_of_loop  (3)
      call cps_pre_process  (3)
      call RNSYN            (obj3,ntr,hd1,tr1)
      call cps_post_process (3,ntr,hd1,tr1)
      if (ntr == FATAL_ERROR) goto 7001
! 
      call cps_pre_process  (4)
      call HEADSUM          (obj4,ntr,hd1,tr1)
      call cps_post_process (4,ntr,hd1,tr1)
      if (ntr == FATAL_ERROR) goto 7001
! 
      call cps_pre_process  (5)
      call INTEGRATE        (obj5,ntr,hd1,tr1)
      call cps_post_process (5,ntr,hd1,tr1)
      if (ntr == FATAL_ERROR) goto 7001
! 
      if (ntr == NO_MORE_TRACES) goto 8001
      ntr = NEED_TRACES
      goto 10
! 
 7001 error = .true.
! 
!!!!! DELETE:
! 
 8001 continue
      if (associated(obj1)) call PROJECT_DATA_delete (obj1)
      if (associated(obj2)) call JOB_DATA_delete     (obj2)
      if (associated(obj3)) call RNSYN_delete        (obj3)
      if (associated(obj4)) call HEADSUM_delete      (obj4)
      if (associated(obj5)) call INTEGRATE_delete    (obj5)
! 
      deallocate(hd1,stat=ierr)
      if (ierr /= 0) then
        call cps_print ('Unable to deallocate hd1')
        error = .true.
        goto 9999
      endif
      deallocate(tr1,stat=ierr)
      if (ierr /= 0) then
        call cps_print ('Unable to deallocate tr1')
        error = .true.
        goto 9999
      endif
! 
      if (error) go to 9999
 9999 continue
      call cps_finish_processing (error)
! 
      stop
      end program integrate_01_cps
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!! SIGNAL HANDLER !!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
      subroutine cpssig_shutdown (signal)
! 
      use program_global_data
! 
      integer,intent(in) :: signal          !argument
! 
      logical,save       :: flag0 = .true.  !local
      logical,save       :: flag1 = .true.  !local
      logical,save       :: flag2 = .true.  !local
      logical,save       :: flag3 = .true.  !local
      logical,save       :: flag4 = .true.  !local
      logical,save       :: flag5 = .true.  !local
! 
      if (flag0) then
        call cps_write_accounting_signal (signal)
        call cps_print_current_status()
      endif
! 
      if (flag1) then
        flag1 = .false.
        if (associated(obj1)) then
          call cps_print ("+++++++ PROJECT_DATA ipn=1 wrapup called by signal handler")
          call PROJECT_DATA_wrapup (obj1)
        endif
      endif
! 
      if (flag2) then
        flag2 = .false.
        if (associated(obj2)) then
          call cps_print ("+++++++ JOB_DATA ipn=2 wrapup called by signal handler")
          call JOB_DATA_wrapup     (obj2)
        endif
      endif
! 
      if (flag3) then
        flag3 = .false.
        if (associated(obj3)) then
          call cps_print ("+++++++ RNSYN ipn=3 wrapup called by signal handler")
          call RNSYN_wrapup        (obj3)
        endif
      endif
! 
      if (flag4) then
        flag4 = .false.
        if (associated(obj4)) then
          call cps_print ("+++++++ HEADSUM ipn=4 wrapup called by signal handler")
          call HEADSUM_wrapup      (obj4)
        endif
      endif
! 
      if (flag5) then
        flag5 = .false.
        if (associated(obj5)) then
          call cps_print ("+++++++ INTEGRATE ipn=5 wrapup called by signal handler")
          call INTEGRATE_wrapup    (obj5)
        endif
      endif
! 
      if (flag0) then
        flag0 = .false.
        call cps_finish_processing (signal=signal)
      endif
! 
      return
      end subroutine cpssig_shutdown
