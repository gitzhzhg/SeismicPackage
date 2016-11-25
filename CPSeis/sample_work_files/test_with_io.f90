!                           Start of CPS program test_with_io
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
!                       CONFIDENTIAL AND PROPRIETARY INFORMATION
!                                    OF CONOCO INC.
!                            PROTECTED BY THE COPYRIGHT LAW
!                                AS AN UNPUBLISHED WORK
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!! GLOBAL DATA MODULE !!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
      module program_global_data
! 
      use pcps_module
      use pcpsx_module
      use named_constants_module
! 
      use PROJECT_DATA_module
      use JOB_DATA_module
      use SPIKE_module
      use HEADSUM_module
      use WSEP_module
! 
      implicit none
! 
      public
      type(PCPSX_DO_PARALLEL_struct),pointer :: p_obj1
! 
      type(PROJECT_DATA_struct),pointer      :: obj1
      type(JOB_DATA_struct),pointer          :: obj2
      type(SPIKE_struct),pointer             :: obj3
      type(HEADSUM_struct),pointer           :: obj4
      type(WSEP_struct),pointer              :: obj5
      end module program_global_data
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!! PROGRAM MAIN !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
      program test_with_io_cps
! 
      use program_global_data
! 
! 
!!!!! DECLARATIONS:
! 
      double precision, pointer :: hd1(:,:)
      real            , pointer :: tr1(:,:)
! 
      integer :: ntr, ierr
      logical :: error
! 
      nullify (p_obj1)
! 
      nullify (obj1)
      nullify (obj2)
      nullify (obj3)
      nullify (obj4)
      nullify (obj5)
! 
      call pcpsx_start_processing
! 
      call pcpsx_print_rcs_ident (trim(PCPS_ident))
      call pcpsx_print_rcs_ident (trim(PCPSX_ident))
      call pcpsx_print_rcs_ident (trim(PROJECT_DATA_ident))
      call pcpsx_print_rcs_ident (trim(JOB_DATA_ident))
      call pcpsx_print_rcs_ident (trim(SPIKE_ident))
      call pcpsx_print_rcs_ident (trim(HEADSUM_ident))
      call pcpsx_print_rcs_ident (trim(WSEP_ident))
      call pcpsx_custom_rcs_ident
! 
!!!!! ALLOCATE ARRAYS:
! 
      ntr = 1
      allocate(hd1(64,ntr),stat=ierr)
      if (ierr /= 0) then
        call pcps_print ('Unable to allocate hd1')
        error = .true.
        goto 9999
      endif
      allocate(tr1(1001,ntr),stat=ierr)
      if (ierr /= 0) then
        call pcps_print ('Unable to allocate tr1')
        error = .true.
        goto 9999
      endif
! 
!!!!! SETUPS:
! 
      call pcpsx_start_setups    ! stops if there is an error.
! 
      call pcpsx_do_parallel_create (p_obj1,error)
      call pcps_set_send_mode       (PCPS_BOSS_EXECS                ,error)
      call pcps_set_generator_mode  (PCPS_TRACE_GEN                 ,error)
      call pcpsx_do_parallel_init   (p_obj1,error)
      if (error) goto 9999
! 
      call pcpsx_pre_setup     (PCPS_OUTSIDE_PARALLEL)
      call PROJECT_DATA_create (obj1)
      call pcpsx_post_setup
! 
      call pcpsx_pre_setup     (PCPS_OUTSIDE_PARALLEL)
      call JOB_DATA_create     (obj2)
      call pcpsx_post_setup
! 
      call pcpsx_pre_setup     (PCPS_BOSS_PARALLEL)
      call SPIKE_create        (obj3)
      call pcpsx_post_setup
! 
      call pcpsx_pre_setup     (PCPS_OUTSIDE_PARALLEL)
      call HEADSUM_create      (obj4)
      call pcpsx_post_setup
! 
      call pcpsx_pre_setup     (PCPS_OUTSIDE_PARALLEL)
      call WSEP_create         (obj5)
      call pcpsx_post_setup
! 
      call pcpsx_finish_setups (error); if (error) go to 9001
! 
!!!!! EXECUTION:
! 
      ntr = NEED_TRACES
! 
 10   call pcpsx_do_parallel_begin (p_obj1,ntr,hd1,tr1,hd1,tr1)
      if (ntr == LOOP_BACK) goto 0
      if (ntr == SHUT_DOWN) goto 8001
!
 20   call pcpsx_start_loop (p_obj2,ntr)
      if (ntr /= NULL_ACTION) then
        call pcpsx_filter_ntr (3,ntr)
        if (ntr == LOOP_BACK) goto 10
        call pcpsx_pre_process  (3)
        call SPIKE              (obj3,ntr,hd1,tr1)
        call pcpsx_post_process (3,ntr,hd1,tr1)
        if (ntr == FATAL_ERROR) goto 7001
        if (ntr == NEED_TRACES) then
          call pcpsx_do_parallel_end (p_obj1,ntr,hd1,tr1)
          goto 0
        endif
      endif
!
 30   call pcpsx_do_parallel_end   (p_obj1,ntr,hd1,tr1,hd1,tr1)
      if (ntr == LOOP_BACK) goto 0
      if (ntr == SHUT_DOWN) goto 8001
!
      if (ntr /= NULL_ACTION) then
        call pcpsx_pre_process  (4)
        call HEADSUM            (obj4,ntr,hd2,tr2)
        call pcpsx_post_process (4,ntr,hd2,tr2)
        if (ntr == FATAL_ERROR) goto 7001
      endif
!
      if (ntr /= NULL_ACTION) then
        call pcpsx_pre_process  (5)
        call WSEP               (obj5,ntr,hd2,tr2)
        call pcpsx_post_process (5,ntr,hd2,tr2)
        if (ntr == FATAL_ERROR) goto 7001
      endif
!
 7001 call pcpsx_end_loop (p_obj1,ntr)
      if (ntr == LOOP_BACK) goto 30
      if (ntr == GO_BACK) goto 20
      if (ntr /= NO_MORE_TRACES) error = .true.
! 
!!!!! DELETE:
! 
 8001 continue
      call pcpsx_wrapup_processing
      call pcpsx_do_parallel_delete (p_obj1)
! 
 9001 continue
      if (associated(obj1)) call PROJECT_DATA_delete (obj1)
      if (associated(obj2)) call JOB_DATA_delete     (obj2)
      if (associated(obj3)) call SPIKE_delete        (obj3)
      if (associated(obj4)) call HEADSUM_delete      (obj4)
      if (associated(obj5)) call WSEP_delete         (obj5)
! 
      deallocate(hd1,stat=ierr)
      if (ierr /= 0) then
        call pcps_print ('Unable to deallocate hd1')
        error = .true.
        goto 9999
      endif
      deallocate(tr1,stat=ierr)
      if (ierr /= 0) then
        call pcps_print ('Unable to deallocate tr1')
        error = .true.
        goto 9999
      endif
! 
      if (error) go to 9999
 9999 continue
      call pcpsx_finish_processing (error)
! 
      end program test_with_io_cps
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!! SIGNAL HANDLER !!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
      subroutine cpssig_shutdown (signal)
! 
      use program_global_data
      use cps_module
      use cio_module
! 
      integer,intent(in) :: signal          !argument
! 
      integer,parameter  :: kill_sig=12     !SIGUSR2
      integer            :: lun             !local
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
          call cpssig_print ("PROJECT_DATA",1)
          call PROJECT_DATA_wrapup (obj1)
        endif
      endif
! 
      if (flag2) then
        flag2 = .false.
        if (associated(obj2)) then
          call cpssig_print ("JOB_DATA",2)
          call JOB_DATA_wrapup     (obj2)
        endif
      endif
! 
      if (flag3) then
        flag3 = .false.
        if (associated(obj3)) then
          call cpssig_print ("SPIKE",3)
          call SPIKE_wrapup        (obj3)
        endif
      endif
! 
      if (flag4) then
        flag4 = .false.
        if (associated(obj4)) then
          call cpssig_print ("HEADSUM",4)
          call HEADSUM_wrapup      (obj4)
        endif
      endif
! 
      if (flag5) then
        flag5 = .false.
        if (associated(obj5)) then
          call cpssig_print ("WSEP",5)
          call WSEP_wrapup         (obj5)
        endif
      endif
! 
      if (flag0) then
        flag0 = .false.
        if (pcps_current_worker_num.eq.0) &
          call cps_finish_processing (signal=signal)
! 
        call cio_finalize()
        call pfio_exit()
! 
        if(signal.ne.kill_sig) then
          call pcps_kill_parallel_cpus (kill_sig)
        endif
! 
        lun = cps_get_lun()
        close(unit=lun,status="keep")
      endif
      return
      end subroutine cpssig_shutdown
! 
! 
      subroutine cpssig_print (process,ipn)
      use pcps_module
! 
      character(len=*),intent(in) :: process      !argument
      integer         ,intent(in) :: ipn          !argument
      character(len=132)          :: mess         !local
! 
      write(mess,*) "+++++++ ",process," ipn=",ipn,"( worker",pcps_current_worker_num,") wrapup called by signal handler"
      call pcps_print (mess,2)
      return
      end subroutine cpssig_print
