!**************************************************************************
!<CPS_v1 type="PRIMITIVE"/>
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
! Name       : ppio
! Category   : io
! Written    : 2000-11-29  by: Charles C. Burch
! Revised    : 2007-01-03  by: Bill Menger
! Maturity   : production
! Purpose    : Interprocess IO routines for PCPS
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!  This primitive contains a collection of routines which are called
!  to do interprocess communications.
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
! subroutine ppio_init(ierr)
! Basic initializes parallel_processing communication
!  returns ierr(0 if OK)
!
!  integer, intent(out)  :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_done(ierr)
! Basic finalization of parallel_processing communication
!  returns ierr(0 if OK)
!
!  integer, intent(out)  :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_abort(ierr)
! Make best attempt to inform all cpus that some cpu wants to abort the job
!
!  integer, intent(out)  :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_get_proc_num(worker_no)
! Basic parallel_processing communication
!  returns current processor number and ierr(0 if OK)
!
! integer, intent(out) :: worker_no
!-------------------------------------------------------------------------------
! subroutine ppio_get_data_size(worker_no, tag, n_size)
! Get n_size of data to be received from worker_no with tag 
!
!   integer, intent(in)  :: worker_no
!   integer, intent(in)  :: tag
!   integer, intent(out) :: n_size
!-------------------------------------------------------------------------------
! subroutine ppio_sync(ierr)
! Basic parallel_processing communication
!  idle each worker until they get to this point
!  returns ierr (0 if no error)
!
!  integer,intent(out)      :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_sync_no_boss(ierr)
! Basic parallel_processing communication
!  idle each worker (but not the boss) until they get to this point
!  returns ierr (0 if no error)
!
!  integer,intent(out)      :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_send_packed_data(buffer, n, proc, tag, ierr)
! send "n" bytes in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
!   character (len=1), intent(in) :: buffer(:)
!   integer, intent(in)           :: n
!   integer, intent(in)           :: proc
!   integer, intent(in)           :: tag
!   integer, intent(out)          :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_receive_packed_data(buffer, n, proc, tag, ierr)
! receive "n" bytes in buffer from cpu "proc" using "tag"
!   return ierr (0-no error)
!
! character (len=1),intent(out) :: buffer(:)
! integer, intent(in)           :: n
! integer, intent(in)           :: proc
! integer, intent(in)           :: tag
! integer, intent(out)          :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_send_char_data(buffer, n, proc, tag, ierr)
! send "n" chars in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
!   character (len=1), intent(in) :: buffer(:)
!   integer, intent(in)           :: n
!   integer, intent(in)           :: proc
!   integer, intent(in)           :: tag
!   integer, intent(out)          :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_receive_char_data(buffer, n, proc, tag, ierr)
! receive "n" chars in buffer from cpu "proc" using "tag"
!   return ierr (0-no error)
!
! character (len=1),intent(out) :: buffer(:)
! integer, intent(in)           :: n
! integer, intent(in)           :: proc
! integer, intent(in)           :: tag
! integer, intent(out)          :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_send_real_data(buff, n, proc, tag, ierr)
! send "n" reals in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
! real, intent(in)      :: buff(:)
! integer, intent(in)   :: n
! integer, intent(in)   :: proc
! integer, intent(in)   :: tag
! integer, intent(out)  :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_receive_real_data(buff, n, proc, tag, ierr)
! receive "n" reals in buffer from cpu "proc" using "tag"
!   return ierr (o-no error)
!
!   real, intent(out)     :: buff(:)
!   integer, intent(in)   :: n
!   integer, intent(in)   :: proc
!   integer, intent(in)   :: tag
!   integer, intent(out)  :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_send_integer_data(buff, n, proc, tag, ierr)
! send "n" integers in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
! integer, intent(in)   :: buff(:)
! integer, intent(in)   :: n
! integer, intent(in)   :: proc
! integer, intent(in)   :: tag
! integer, intent(out)  :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_receive_integer_data(buffer, n, proc, tag, ierr)
! receive "n" integers in buffer from cpu "proc" using "tag"
!   return ierr (o-no error)
!
! integer, intent(out)  :: buffer(:)
! integer, intent(in)   :: n
! integer, intent(in)   :: proc
! integer, intent(in)   :: tag
! integer, intent(out)  :: ierr
!-------------------------------------------------------------------------------
! subroutine ppio_get_worker_with_data(tag, worker_no)
!  Waits for a worker to finish sending data with tag
!    and return worker_no that has
!
!   integer, intent(out) :: worker_no
!   integer, intent(in)  :: instance
!-------------------------------------------------------------------------------
! subroutine ppio_broadcast_data(buff, n, proc, ierr)
! broadcast n characters from buffer buff to all procs
!  using proc as the root
!
! character (len=1), intent(inout)  :: buff(:)
! integer,           intent(in)     :: n
! integer,           intent(in)     :: proc
! integer,           intent(out)    :: ierr
!-------------------------------------------------------------------------------
!subroutine ppio_get_tag_with_data(worker_no, tag)
! find a tag from worker_no which has sent data
!
! integer, intent(in)  :: worker_no
! integer, intent(out) :: tag
!-------------------------------------------------------------------------------
! subroutine ppio_test_if_worker_done(worker_no, tag, n_size)
! see if a worker has a message with tag that is done
! returns n_size =-1 if not done and message size if done
!
!   integer, intent(in)  :: worker_no
!   integer, intent(in)  :: tag
!   integer, intent(out) :: n_size
!-------------------------------------------------------------------------------
! subroutine ppio_pack(buff, n, packed, n_packed)
! packs n elements in buff into array packed with index n_packed
!
! buff can be character, 1d or 2d integer, real, double, complex or logical
! integer, intent(in)              :: n
! character (len=1), intent(inout) :: packed(:)
! integer, intent(inout)           :: n_packed
!-------------------------------------------------------------------------------
! subroutine ppio_unpack(buff, n, packed, n_packed)
! unpacks n elements into buff from array packed with index n_packed
!
! buff can be character, 1d or 2d integer, real, double, complex or logical
! integer, intent(in)              :: n
! character (len=1), intent(inout) :: packed(:)
! integer, intent(inout)           :: n_packed
!-------------------------------------------------------------------------------
! subroutine ppio_broadcast(root,x)-scalar x
!            ppio_broadcast(root,n,x)-vector,array,3d
!  broadcasts x from pe root to x in all pe-s 
!  x can be character string or numeric
!  numerics can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of x
!-------------------------------------------------------------------------------
! subroutine ppio_gather(root,x,y)-scalar x
!            ppio_gather(root,n,x,y)-vector,array
!  gathers broadcasts x from all pes and gathers them at pe root in y
!  x can be character string or numeric
!  numerics can scalar, vector, or array  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of x
!-------------------------------------------------------------------------------
! subroutine ppio_sum_reduce(root,xin,xout)-scalar
!            ppio_sum_reduce(root,n,xin,xout)-vector,array, 3d
!  sums x_in from all pe-s and returns result in x_out in pe root
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! subroutine ppio_sum_all_reduce(xin,xout)
!            ppio_sum_all_reduce(n,xin,xout)-vector,array, 3d
!  sums x_in from all pe-s and returns result in x_out in all pe-s
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! function ppio_min_reduce(root,x)
!   returns the minimum value of x on all pe-s(value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function ppio_max_reduce(root,x)
!   returns the maximum value of x on all pe-s(value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function ppio_min_all_reduce(x)
!   returns the minimum value of x on all pe-s(value available on all pe-s)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function ppio_max_all_reduce(x)
!   returns the maximum value of x on all pe-s(value available on all pe-s)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!13.  2007-01-03  Bill Menger  Add mpi-initialized test to ppio_init,
!                              added similar test around mpi_finalize.
!12.  2006-04-25  B. Menger    Removed Unused Variables.
!11.  2005-01-31  Stoeckley    Add ppio_sync_no_boss.
!10.  2004-03-15  SMCook       Call umask -- override conservative mpi behavior.
! 9.  2002-05-06  C.C. Burch   Added max_reduce, min_reduce, ppio_abort
! 8.  2001-12-10  C.C. Burch   Added character array to broadcast and
!                               send/receive data.
!                              Added array overflow checks.
!                              Added count tracking of operations
! 7.  2001-10-12  Douglas Hanson Fix (1:, 1:, 1:, 1:n3_inp) bug 
!                              n3_inp should be n4_inp
! 6.  2001-05-18  C.C. Burch   Added more documentation and error messages
!                              Added 4d version sof broadcast, sum_reduce,
!                               sum_all_reduce, send, receive
!                              Removed overloded ppiO_broadcase, sum_reduce,
!                               sum_all_reduce, send, receive
! 5.  2001-04-05  C.C. Burch   Added gather, send_data, receive_data,
!                              and min/max all reduce
! 4.  2001-03-26  C.C. Burch   Added min and max reduce routines
!                              Bug fix for worker value of x_out in reduce rtns
! 3.  2001-03-22  C.C. Burch   Added Doug Handson's broadcast & sum_allreduce
!                              Added reduce routines 
!                              Added support for double precisions and complex
! 2.  2001-03-15  C.C. Burch   Added pack/unpack support for logical and complex
!                              Added ppio_get_data_size
!                              Added error checking to ppio_pack/unpack routines
! 1.  2000-11-27  C.C. Burch   Initial version
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations other than needing MPI library.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! Linux Absoft compiler requires -YEXT_NAMES=LCS -YEXT_SFX=_
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
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!

module ppio_module
  use pcps_module
  use unix_module
  implicit none
!
!  include "mpif_stubs.h"
   include "mpif.h"
!
  private

  public :: ppio_abort

  public :: ppio_broadcast_data
  public :: ppio_broadcast_c0
  public :: ppio_broadcast_c1
  public :: ppio_broadcast_c2
  public :: ppio_broadcast_i0
  public :: ppio_broadcast_i1
  public :: ppio_broadcast_i2
  public :: ppio_broadcast_i3
  public :: ppio_broadcast_i4
  public :: ppio_broadcast_r0
  public :: ppio_broadcast_r1
  public :: ppio_broadcast_r2
  public :: ppio_broadcast_r3
  public :: ppio_broadcast_r4
  public :: ppio_broadcast_d0
  public :: ppio_broadcast_d1
  public :: ppio_broadcast_d2
  public :: ppio_broadcast_d3
  public :: ppio_broadcast_d4
  public :: ppio_broadcast_z0
  public :: ppio_broadcast_z1
  public :: ppio_broadcast_z2
  public :: ppio_broadcast_z3
  public :: ppio_broadcast_z4

  public :: ppio_done
  public :: ppio_dump_counts

  public :: ppio_gather_c0
  public :: ppio_gather_c1
  public :: ppio_gather_c2
  public :: ppio_gather_i0
  public :: ppio_gather_i1
  public :: ppio_gather_i2
  public :: ppio_gather_r0
  public :: ppio_gather_r1
  public :: ppio_gather_r2
  public :: ppio_gather_d0
  public :: ppio_gather_d1
  public :: ppio_gather_d2
  public :: ppio_gather_z0
  public :: ppio_gather_z1
  public :: ppio_gather_z2

  public :: ppio_get_data_size
  public :: ppio_get_tag_with_data
  public :: ppio_get_worker_with_data
  public :: ppio_init

  public :: ppio_max_reduce_i0
  public :: ppio_max_reduce_r0
  public :: ppio_max_reduce_d0

  public :: ppio_min_reduce_i0
  public :: ppio_min_reduce_r0
  public :: ppio_min_reduce_d0

  public :: ppio_max_all_reduce_i0
  public :: ppio_max_all_reduce_r0
  public :: ppio_max_all_reduce_d0

  public :: ppio_min_all_reduce_i0
  public :: ppio_min_all_reduce_r0
  public :: ppio_min_all_reduce_d0

  public :: ppio_pack
  public :: ppio_pack_chars
  public :: ppio_pack_complex
  public :: ppio_pack_double_precision
  public :: ppio_pack_integer
  public :: ppio_pack_logical
  public :: ppio_pack_real
  public :: ppio_pack_complex_2d
  public :: ppio_pack_double_precision_2d
  public :: ppio_pack_integer_2d
  public :: ppio_pack_logical_2d
  public :: ppio_pack_real_2d

  public :: ppio_receive_data_c0
  public :: ppio_receive_data_c1
  public :: ppio_receive_data_c2
  public :: ppio_receive_data_i0
  public :: ppio_receive_data_i1
  public :: ppio_receive_data_i2
  public :: ppio_receive_data_i3
  public :: ppio_receive_data_i4
  public :: ppio_receive_data_r0
  public :: ppio_receive_data_r1
  public :: ppio_receive_data_r2
  public :: ppio_receive_data_r3
  public :: ppio_receive_data_r4
  public :: ppio_receive_data_d0
  public :: ppio_receive_data_d1
  public :: ppio_receive_data_d2
  public :: ppio_receive_data_d3
  public :: ppio_receive_data_d4
  public :: ppio_receive_data_z0
  public :: ppio_receive_data_z1
  public :: ppio_receive_data_z2
  public :: ppio_receive_data_z3
  public :: ppio_receive_data_z4

  public :: ppio_receive_char_data
  public :: ppio_receive_packed_data
  public :: ppio_receive_double_prec_data
  public :: ppio_receive_integer_data
  public :: ppio_receive_real_data

  public :: ppio_send_data_c0
  public :: ppio_send_data_c1
  public :: ppio_send_data_c2
  public :: ppio_send_data_i0
  public :: ppio_send_data_i1
  public :: ppio_send_data_i2
  public :: ppio_send_data_i3
  public :: ppio_send_data_i4
  public :: ppio_send_data_r0
  public :: ppio_send_data_r1
  public :: ppio_send_data_r2
  public :: ppio_send_data_r3
  public :: ppio_send_data_r4
  public :: ppio_send_data_d0
  public :: ppio_send_data_d1
  public :: ppio_send_data_d2
  public :: ppio_send_data_d3
  public :: ppio_send_data_d4
  public :: ppio_send_data_z0
  public :: ppio_send_data_z1
  public :: ppio_send_data_z2
  public :: ppio_send_data_z3
  public :: ppio_send_data_z4

  public :: ppio_send_char_data
  public :: ppio_send_packed_data
  public :: ppio_send_double_prec_data
  public :: ppio_send_integer_data
  public :: ppio_send_real_data

  public :: ppio_start_counting
  public :: ppio_stop_counting

  public :: ppio_sum_all_reduce_i0
  public :: ppio_sum_all_reduce_i1
  public :: ppio_sum_all_reduce_i2
  public :: ppio_sum_all_reduce_i3
  public :: ppio_sum_all_reduce_i4
  public :: ppio_sum_all_reduce_r0
  public :: ppio_sum_all_reduce_r1
  public :: ppio_sum_all_reduce_r2
  public :: ppio_sum_all_reduce_r3
  public :: ppio_sum_all_reduce_r4
  public :: ppio_sum_all_reduce_d0
  public :: ppio_sum_all_reduce_d1
  public :: ppio_sum_all_reduce_d2
  public :: ppio_sum_all_reduce_d3
  public :: ppio_sum_all_reduce_d4
  public :: ppio_sum_all_reduce_z0
  public :: ppio_sum_all_reduce_z1
  public :: ppio_sum_all_reduce_z2
  public :: ppio_sum_all_reduce_z3
  public :: ppio_sum_all_reduce_z4

  public :: ppio_sum_reduce_i0
  public :: ppio_sum_reduce_i1
  public :: ppio_sum_reduce_i2
  public :: ppio_sum_reduce_i3
  public :: ppio_sum_reduce_i4
  public :: ppio_sum_reduce_r0
  public :: ppio_sum_reduce_r1
  public :: ppio_sum_reduce_r2
  public :: ppio_sum_reduce_r3
  public :: ppio_sum_reduce_r4
  public :: ppio_sum_reduce_d0
  public :: ppio_sum_reduce_d1
  public :: ppio_sum_reduce_d2
  public :: ppio_sum_reduce_d3
  public :: ppio_sum_reduce_d4
  public :: ppio_sum_reduce_z0
  public :: ppio_sum_reduce_z1
  public :: ppio_sum_reduce_z2
  public :: ppio_sum_reduce_z3
  public :: ppio_sum_reduce_z4

  public :: ppio_sync
  public :: ppio_sync_no_boss
  public :: ppio_test_if_worker_done

  public :: ppio_unpack
  public :: ppio_unpack_chars
  public :: ppio_unpack_complex
  public :: ppio_unpack_double_precision
  public :: ppio_unpack_integer
  public :: ppio_unpack_logical
  public :: ppio_unpack_real
  public :: ppio_unpack_complex_2d
  public :: ppio_unpack_double_precision_2d
  public :: ppio_unpack_integer_2d
  public :: ppio_unpack_logical_2d
  public :: ppio_unpack_real_2d

  public :: ppio_wait_for_worker

  interface ppio_pack
    module procedure  ppio_pack_chars
    module procedure  ppio_pack_complex
    module procedure  ppio_pack_double_precision
    module procedure  ppio_pack_integer
    module procedure  ppio_pack_logical
    module procedure  ppio_pack_real
    module procedure  ppio_pack_complex_2d
    module procedure  ppio_pack_double_precision_2d
    module procedure  ppio_pack_integer_2d
    module procedure  ppio_pack_logical_2d
    module procedure  ppio_pack_real_2d
  end interface

  interface ppio_unpack
    module procedure  ppio_unpack_chars
    module procedure  ppio_unpack_complex
    module procedure  ppio_unpack_double_precision
    module procedure  ppio_unpack_integer
    module procedure  ppio_unpack_logical
    module procedure  ppio_unpack_real
    module procedure  ppio_unpack_complex_2d
    module procedure  ppio_unpack_double_precision_2d
    module procedure  ppio_unpack_integer_2d
    module procedure  ppio_unpack_logical_2d
    module procedure  ppio_unpack_real_2d
  end interface

  character(len=100),save,public :: PPIO_ident =                               &
   "$Id: ppio.f90,v 1.13 2007/01/03 14:01:44 Menger prod sps $"

!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!

  integer,            public :: ppio_debug        =0 !debug, 0=none
  integer, parameter, public :: PPIO_DEBUG_ENTRY = 1 !print upon entry
  integer, parameter, public :: PPIO_DEBUG_EXIT   =2 !print on exit

! the defaults for the variables below get reset when ppio_init is called

  integer, public   :: ppio_sizeof_char       =1 !size of chars for ppio
  integer, public   :: ppio_sizeof_integer    =4 !size of integers for ppio
  integer, public   :: ppio_sizeof_real       =4 !size of real for ppio
  integer, public   :: ppio_sizeof_double_prec=8 !size of double prec for ppio
  integer, public   :: ppio_sizeof_complex    =8 !size of complex for ppio
  integer, public   :: ppio_sizeof_logical    =1 !size of logical for ppio

  logical           :: ppio_count_flag          = .false.
  integer           :: ppio_send_count          = 0
  integer           :: ppio_receive_count       = 0
  integer           :: ppio_broadcast_count     = 0
  integer           :: ppio_gather_count        = 0
  integer           :: ppio_sum_reduce_count    = 0
  integer           :: ppio_sum_all_reduce_count= 0
  integer           :: ppio_pack_count          = 0
  integer           :: ppio_unpack_count        = 0
  integer           :: ppio_max_reduce_count    = 0
  integer           :: ppio_max_all_reduce_count= 0
  integer           :: ppio_min_reduce_count    = 0
  integer           :: ppio_min_all_reduce_count= 0
  integer           :: ppio_sync_count          = 0
  logical           :: ppio_mpi_initialized_externally     = .false.

  integer,private :: PPIO_COMM_NO_BOSS = 0

 
!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!


contains


!====================PPIO_INIT========================
! Basic initializes parallel_processing communication
!  returns ierr(0 if OK)
!
! Written November 2000 by Charles C Burch
! Modified July 12, 2006 Bill Menger to test mpi_initialized first.
!=====================================================
  subroutine ppio_init(ierr)
    integer, intent(out)  :: ierr

    integer               :: istat,ranks(1),group,newgroup

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_init, cpu=",pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    call unix_umask(22)
    call mpi_initialized(ppio_mpi_initialized_externally,ierr)
    if(ierr.ne.0) then
      write(pcps_message,*) "PPIO:ppio_init, cpu=",pcps_current_worker_num,&
                            " Error checking MPI initialized status. Aborting."
      call pcps_print(pcps_message,2)
      return
    endif
    if(.not. ppio_mpi_initialized_externally) call mpi_init(ierr)
    if(ierr.ne.0) then
      write(pcps_message,*) "PPIO:ppio_init, cpu=",pcps_current_worker_num,&
                            " Error initializing MPI. Aborting."
      call pcps_print(pcps_message,2)
      return
    endif
    call mpi_comm_size(MPI_COMM_WORLD, pcps_num_procs, istat)
    if(ierr.eq.0) ierr=istat

    pcps_num_workers=pcps_num_procs-1
    if(pcps_num_workers.lt.1) then
      pcps_boss_mode=.true.
      pcps_worker_mode=.true.
      pcps_current_worker_num=0
    else
      call ppio_get_proc_num(pcps_current_worker_num)
      pcps_boss_mode=pcps_current_worker_num.eq.0
      pcps_worker_mode=.not.pcps_boss_mode
    endif

!----------get communicator which excludes the boss:
    if (pcps_num_workers >= 1) then
      ranks(1) = 0
      call mpi_comm_group  (MPI_COMM_WORLD, group, istat)
      call mpi_group_excl  (group, 1, ranks, newgroup, istat)
      call mpi_comm_create (MPI_COMM_WORLD, newgroup, PPIO_COMM_NO_BOSS, istat)
    end if
!----------get communicator which excludes the boss (above).

    call mpi_pack_size(1,MPI_CHARACTER,MPI_COMM_WORLD, &
      ppio_sizeof_integer, istat)
    if(ierr.eq.0) ierr=istat

    call mpi_pack_size(1,MPI_INTEGER,MPI_COMM_WORLD, ppio_sizeof_integer, istat)
    if(ierr.eq.0) ierr=istat

    call mpi_pack_size(1,MPI_REAL,MPI_COMM_WORLD, ppio_sizeof_real, istat)
    if(ierr.eq.0) ierr=istat

    call mpi_pack_size(1,MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,                  &
     ppio_sizeof_double_prec, istat)
    if(ierr.eq.0) ierr=istat

    call mpi_pack_size(1,MPI_COMPLEX,MPI_COMM_WORLD, ppio_sizeof_complex, istat)
    if(ierr.eq.0) ierr=istat

    call mpi_pack_size(1,MPI_LOGICAL,MPI_COMM_WORLD, ppio_sizeof_logical, istat)
    if(ierr.eq.0) ierr=istat

    ppio_send_count=0
    ppio_receive_count=0
    ppio_broadcast_count=0
    ppio_gather_count=0  
    ppio_sum_reduce_count=0
    ppio_sum_all_reduce_count=0
    ppio_pack_count=0    
    ppio_unpack_count=0  
    ppio_max_reduce_count=0
    ppio_max_all_reduce_count=0
    ppio_min_reduce_count=0
    ppio_min_all_reduce_count=0
    ppio_sync_count=0 
 
!   print *,"ppio_init, cpu=",pcps_get_worker_num(), ppio_sizeof_integer,&
!    ppio_sizeof_real, ppio_sizeof_char, ppio_sizeof_double_prec

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_init, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_init

!===================PPIO_DUMP_COUNTS==================
! Dump counts of ppio operations 
!
! Written October 2001 by Charles C Burch
!=====================================================
  subroutine ppio_dump_counts(worker_num)
    integer, optional   :: worker_num
 
    if(present(worker_num)) then
      if(worker_num.ge.0 .and. worker_num.ne.pcps_current_worker_num) return
    endif

    call pcps_print(" ",2)
    write(pcps_message,*)  &
     "Dump of ppio operation counts, cpu=",pcps_current_worker_num
    call pcps_print(pcps_message,2)

    if(ppio_send_count.gt.0) then 
      write(pcps_message,*) " ppio_send_count=",ppio_send_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_receive_count.gt.0) then 
      write(pcps_message,*) " ppio_receive_count=", ppio_receive_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_broadcast_count.gt.0) then 
      write(pcps_message,*) " ppio_broadcast_count=", ppio_broadcast_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_sum_reduce_count.gt.0) then 
      write(pcps_message,*) " ppio_sum_reduce_count=", ppio_sum_reduce_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_gather_count.gt.0) then 
      write(pcps_message,*) " ppio_gather_count=", ppio_gather_count  
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_pack_count.gt.0) then 
      write(pcps_message,*) " ppio_pack_count=", ppio_pack_count    
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_unpack_count.gt.0) then 
      write(pcps_message,*) " ppio_unpack_count=", ppio_unpack_count  
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_sum_all_reduce_count.gt.0) then 
      write(pcps_message,*)   &
       " ppio_sum_all_reduce_count=", ppio_sum_all_reduce_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_max_reduce_count.gt.0) then 
      write(pcps_message,*) " ppio_max_reduce_count=", ppio_max_reduce_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_min_reduce_count.gt.0) then 
      write(pcps_message,*) " ppio_min_reduce_count=", ppio_min_reduce_count
      call pcps_print(pcps_message,2)
    endif

    if(ppio_max_all_reduce_count.gt.0) then 
      write(pcps_message,*)  &
       " ppio_max_all_reduce_count=", ppio_max_all_reduce_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_min_all_reduce_count.gt.0) then 
      write(pcps_message,*)  &
       " ppio_min_all_reduce_count=", ppio_min_all_reduce_count
      call pcps_print(pcps_message,2)
    endif
    
    if(ppio_sync_count.gt.0) then 
      write(pcps_message,*) " ppio_sync_count=", ppio_sync_count 
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_dump_counts

!=================PPIO_START_COUNTING=================
! Activate counting ppio operations
!
! Written October 2001 by Charles C Burch
!=====================================================
  subroutine ppio_start_counting()
    ppio_count_flag=.true.
  end subroutine ppio_start_counting

!=================PPIO_STOP_COUNTING==================
! Suspend counting ppio counting 
!
! Written October 2001 by Charles C Burch
!=====================================================
  subroutine ppio_stop_counting()
    ppio_count_flag=.false.
  end subroutine ppio_stop_counting
 
!====================PPIO_DONE===========================
! Basic finalization of parallel_processing communication
!  returns ierr(0 if OK)
!
! Written November 2000 by Charles C Burch
!========================================================
  subroutine ppio_done(ierr)
    integer, intent(out)  :: ierr

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_done=",pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    if(.not. ppio_mpi_initialized_externally) call mpi_finalize(ierr)
    !-- don't call mpi_finalize if we didn't initialize mpi within ppio.!wmm

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_finalize, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_done

!=========================PPIO_ABORT=====================
! Make best attempt to inform everyone a cpu wants to abort
!
! Written January 2002 by Charles C Burch
!========================================================
  subroutine ppio_abort(err)
    integer, intent(in) :: err

    call mpi_abort(MPI_COMM_WORLD,err)
  end subroutine ppio_abort

!==================PPIO_GET_PROC_NUM==================
! Basic parallel_processing communication
!  returns current processor number and ierr(0 if OK)
!
! Written November 2000 by Charles C Burch
!=====================================================
  subroutine ppio_get_proc_num(worker_num)
    integer, intent(out) :: worker_num

    integer              :: ierr

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_get_proc_num, cpu=",&
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    call mpi_comm_rank(MPI_COMM_WORLD,worker_num, ierr)

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_get_proc_num, cpu, worker, ierr=", &
       pcps_current_worker_num, worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_get_proc_num



!----------------PPIO_SYNC_NO_BOSS------------------------
! Basic parallel_processing communication
!  idle each worker (but not the boss) until they get to this point
!  returns ierr (0 if no error)
!
!  Written Aug 2004 by Tom Stoeckley
!  Same as ppio_sync except for the communicator passed to mpi_barrier.
!-------------------------------------------------
  subroutine ppio_sync_no_boss(ierr)
    integer,intent(out)      :: ierr

    if(ppio_count_flag) &
      ppio_sync_count=ppio_sync_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sync, cpu=",pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      call mpi_barrier(PPIO_COMM_NO_BOSS, ierr)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_sync, cpu, worker, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sync_no_boss



!----------------PPIO_SYNC------------------------
! Basic parallel_processing communication
!  idle each worker until they get to this point
!  returns ierr (0 if no error)
!
!  Written Nov 2000 by Charles C Burch
!-------------------------------------------------
  subroutine ppio_sync(ierr)
    integer,intent(out)      :: ierr

    if(ppio_count_flag) &
      ppio_sync_count=ppio_sync_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sync, cpu=",pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      call mpi_barrier(MPI_COMM_WORLD, ierr)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_sync, cpu, worker, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sync

!------------------PPIO_SEND_PACKED_DATA-----------------
! send "n" bytes in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------
  subroutine ppio_send_packed_data(buffer, n, proc, tag, ierr)
    character (len=1), intent(in) :: buffer(:)
    integer, intent(in)           :: n
    integer, intent(in)           :: proc
    integer, intent(in)           :: tag
    integer, intent(out)          :: ierr

    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_packed_data, cpu,proc,tag=", &
        pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      call mpi_send(buffer,n, MPI_PACKED, proc, tag, MPI_COMM_WORLD, ierr)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_send_packed_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_packed_data

!------------------PPIO_RECEIVE_PACKED_DATA---------------
! receive "n" bytes in buffer from cpu "proc" using "tag"
!   return ierr (0-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------
  subroutine ppio_receive_packed_data(buffer, n, proc, tag, ierr)
    character (len=1),intent(out) :: buffer(*)
    integer, intent(in)           :: n
    integer, intent(in)           :: proc
    integer, intent(in)           :: tag
    integer, intent(out)          :: ierr

    integer               :: status(MPI_STATUS_SIZE)

    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_packed_data, cpu, proc, tag=", &
       pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      call mpi_recv(buffer,n,MPI_PACKED, proc, tag,                           &
       MPI_COMM_WORLD, status, ierr)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_receive_packed_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_packed_data

!-------------------PPIO_SEND_CHAR_DATA-------------------
! send "n" chars in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------
  subroutine ppio_send_char_data(buff, n, proc, tag, ierr)
    character (len=1), intent(in) :: buff(:)
    integer, intent(in)            :: n
    integer, intent(in)            :: proc
    integer, intent(in)            :: tag
    integer, intent(out)           :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable :: buffer(:)

    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_char_data, cpu, proc, tag=", &
        pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_char
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_send_char_data")
      endif

      n1=0
      call ppio_pack(buff,n,buffer, n1)
      call ppio_send_packed_data(buffer,n1,proc,tag, ierr)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_send_char_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_char_data

!-------------------PPIO_RECEIVE_CHAR_DATA----------------
! receive "n" chars in buffer from cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------
  subroutine ppio_receive_char_data(buff, n, proc, tag, ierr)
    character (len=1), intent(out) :: buff(:)
    integer, intent(in)            :: n
    integer, intent(in)            :: proc
    integer, intent(in)            :: tag
    integer, intent(out)           :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable ::buffer(:)

    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_char_data, cpu, proc, tag=", &
       pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_char
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_receive_real_data")
      endif
 
      call ppio_receive_packed_data(buffer,n1,proc,tag, ierr)
      n1=0
      if(ierr.eq.0) call ppio_unpack(buff,n,buffer, n1)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_receive_char_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_char_data

!--------------------PPIO_SEND_REAL_DATA-----------------
! send "n" reals in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------
  subroutine ppio_send_real_data(buff, n, proc, tag, ierr)
    real, intent(in)      :: buff(:)
    integer, intent(in)   :: n
    integer, intent(in)   :: proc
    integer, intent(in)   :: tag
    integer, intent(out)  :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable :: buffer(:)

    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_real_data, cpu, proc, tag=", &
       pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_real
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_send_real_data")
      endif

      n1=0
      call ppio_pack(buff,n,buffer, n1)
      call ppio_send_packed_data(buffer,n1,proc,tag, ierr)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_send_real_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_real_data

!-------------------PPIO_RECEIVE_REAL_DATA----------------
! receive "n" reals in buffer from cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------
  subroutine ppio_receive_real_data(buff, n, proc, tag, ierr)
    real, intent(out)     :: buff(:)
    integer, intent(in)   :: n
    integer, intent(in)   :: proc
    integer, intent(in)   :: tag
    integer, intent(out)  :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable ::buffer(:)

    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_real_data, cpu, proc, tag=", &
       pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_real
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_receive_real_data")
      endif

      call ppio_receive_packed_data(buffer,n1,proc,tag, ierr)
      n1=0
      call ppio_unpack(buff,n,buffer, n1)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_receive_real_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_real_data

!--------------------PPIO_SEND_DOUBLE_PREC_DATA-------------------
! send "n" double precs in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!-----------------------------------------------------------------
  subroutine ppio_send_double_prec_data(buff, n, proc, tag, ierr)
    double precision, intent(in) :: buff(:)
    integer, intent(in)          :: n
    integer, intent(in)          :: proc
    integer, intent(in)          :: tag
    integer, intent(out)         :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable :: buffer(:)

    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_send_double_data, cpu, proc, tag=", &
       pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_double_prec
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_send_double_prec_data")
      endif

      n1=0
      call ppio_pack(buff,n,buffer, n1)
      call ppio_send_packed_data(buffer,n1,proc,tag, ierr)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_send_double_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_double_prec_data

!------------------PPIO_RECEIVE_DOUBLE_PREC_DATA----------------
! receive "n" double precs in buffer from cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!----------------------------------------------------------------
  subroutine ppio_receive_double_prec_data(buff, n, proc, tag, ierr)
    double precision, intent(out) :: buff(:)
    integer, intent(in)           :: n
    integer, intent(in)           :: proc
    integer, intent(in)           :: tag
    integer, intent(out)          :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable ::buffer(:)

    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_double_data, cpu, proc, tag=",&
       pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_double_prec
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_receive_double_prec_data")
      endif

      call ppio_receive_packed_data(buffer,n1,proc,tag, ierr)
      n1=0
      call ppio_unpack(buff,n,buffer, n1)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_receive_double_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_double_prec_data

!-------------------PPIO_SEND_INTEGER_DATA----------------
! send "n" integers in buffer to cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------
  subroutine ppio_send_integer_data(buff, n, proc, tag, ierr)
    integer, intent(in)   :: buff(:)
    integer, intent(in)   :: n
    integer, intent(in)   :: proc
    integer, intent(in)   :: tag
    integer, intent(out)  :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable ::buffer(:)

    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_send_integer_data, cpu, proc, tag=",&
       pcps_current_worker_num, proc, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_integer
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_send_integer_data")
      endif

      n1=0
      call ppio_pack(buff,n,buffer, n1)
      call ppio_send_packed_data(buffer,n1,proc,tag, ierr)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_send_integer_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_integer_data

!--------------------PPIO_RECEIVE_INTEGER_DATA-----------------
! receive "n" integers in buffer from cpu "proc" using "tag"
!   return ierr (o-no error)
!
! Written Nov 2000 by Charles C Burch
!--------------------------------------------------------------
  subroutine ppio_receive_integer_data(buff, n, proc, tag, ierr)
    integer, intent(out)  :: buff(:)
    integer, intent(in)   :: n
    integer, intent(in)   :: proc
    integer, intent(in)   :: tag
    integer, intent(out)  :: ierr

    integer                        :: istat, n1
    character (len=1), allocatable ::buffer(:)

    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
        "PPIO Entry:ppio_receive_integer_data, cpu, proc,tag=",&
        pcps_current_worker_num, proc,tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      n1=n*ppio_sizeof_integer
      allocate(buffer(1:n1), stat=ierr)
      if(ierr.ne.0) then
        call pcps_abort(                                                      &
         "Unable to allocate buffer in ppio_receive_integer_data")
      endif

      call ppio_receive_packed_data(buffer,n1,proc,tag, ierr)
      n1=0
      call ppio_unpack(buff,n,buffer, n1)
      deallocate(buffer, stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) "PPIO Exit:ppio_receive_integer_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_integer_data

!---------------------PPIO_GET_WORKER_WITH_DATA------------------------
!  Waits for a worker to finish sending data with tag
!    and return worker_no that has (-1 if error)
!
! Written July 2000 by Charles C Burch
!----------------------------------------------------------------------
  subroutine ppio_get_worker_with_data(tag, worker_no, n_size)
    integer, intent(out) :: worker_no
    integer, intent(in)  :: tag
    integer, intent(out) :: n_size

    integer              :: ierr   
    integer              :: status(MPI_STATUS_SIZE)

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_get_worker_with_data, cpu, tag=",&
       pcps_current_worker_num, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
       worker_no=0
       n_size=0
    else
      call mpi_probe(MPI_ANY_SOURCE, tag, MPI_COMM_WORLD,  status, ierr)
      worker_no=status(MPI_SOURCE)
      if(ierr.ne.0) worker_no=-1

      call mpi_get_count(status, MPI_PACKED, n_size, ierr)
      if(ierr.ne.0) worker_no=-1
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_get_worker_with_data, cpu, worker=", &
       pcps_current_worker_num, worker_no
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_get_worker_with_data

!-------------------------PPIO_WAIT_FOR_WORKER-------------------------
!  Waits for a worker to finish adn rerturn data size (-1, if error)
!
! Written October 2001 by Charles C Burch
!----------------------------------------------------------------------
  subroutine ppio_wait_for_worker(worker_no, n_size)
    integer, intent(in ) :: worker_no
    integer, intent(out) :: n_size

    integer              :: ierr
    integer              :: status(MPI_STATUS_SIZE)

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_wait_for worker, cpu, worker=",&
       pcps_current_worker_num, worker_no
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
       n_size=0
    else
      call mpi_probe(worker_no, MPI_ANY_TAG, MPI_COMM_WORLD,  status, ierr)

      if(ierr.eq.0) then
        call mpi_get_count(status, MPI_PACKED, n_size, ierr)
        if(ierr.ne.0) n_size=-1
      else
        n_size=-1
      endif
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_wait_for_worker, cpu, n_size=", &
       pcps_current_worker_num, n_size
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_wait_for_worker

!-------------------------PPIO_GET_DATA_SIZE---------------------------
! Get n_size of data to be received from worker_no with tag 
!   return -1 for n_size if error
!
! Written February 2001 by Charles C Burch
!----------------------------------------------------------------------
  subroutine ppio_get_data_size(worker_no, tag, n_size)
    integer, intent(in)  :: worker_no
    integer, intent(in)  :: tag
    integer, intent(out) :: n_size

    integer              :: ierr   
    integer              :: status(MPI_STATUS_SIZE)

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_get_data_size, cpu, worker, tag=",&
       pcps_current_worker_num, worker_no, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      n_size=0
    else
      call mpi_probe(worker_no, tag, MPI_COMM_WORLD,  status, ierr)
      call mpi_get_count(status, MPI_PACKED, n_size, ierr)
      if(ierr.ne.0) n_size=-1
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_get_data_size, cpu, size=", &
       pcps_current_worker_num, n_size
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_get_data_size

!----------------PPIO_TEST_IF_WORKER_DONE--------------------
! see if a worker has a message with tag that is done
!  returns n_size =-1 if not done and message size if done
!
! Written March 2001 by Charles C Burch
!-----------------------------------------------------------
  subroutine ppio_test_if_worker_done(worker_no, tag, n_size)
    integer, intent(in)  :: worker_no
    integer, intent(in)  :: tag
    integer, intent(out) :: n_size

    integer              :: ierr
    integer              :: status(MPI_STATUS_SIZE)
    logical              :: flag

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_test_if_worker_done, cpu, worker,tag=",&
       pcps_current_worker_num, worker_no, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      n_size=0
    else
      call mpi_iprobe(worker_no, tag, MPI_COMM_WORLD,  flag, status, ierr)
      if(flag .and. ierr.eq.0) then 
        call mpi_get_count(status, MPI_PACKED, n_size, ierr)
      else
        n_size=-1
      endif
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_test_if_worker_done, cpu, size=", &
       pcps_current_worker_num, n_size
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_test_if_worker_done

!=========================PPIO_BROADCAST_DATA===========================
! broadcast n bytes from buffer buff to all procs using proc as the root
!
! Written November 2000 by Charles C Burch
!=======================================================================
  subroutine ppio_broadcast_data(buff, n, proc, ierr)
    character (len=1), intent(inout)  :: buff(*)
    integer,           intent(in)     :: n
    integer,           intent(in)     :: proc
    integer,           intent(out)    :: ierr

    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_data, cpu, root=", &
       pcps_current_worker_num, proc
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      ierr=0
    else
      call mpi_bcast(buff,n,MPI_PACKED, proc, MPI_COMM_WORLD, ierr)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_data, cpu, ierr=", &
       pcps_current_worker_num, ierr
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_data

!===============PPIO_GET_TAG_WITH_DATA==================
! find a tag from worker_no which has sent data
!  -1 if error
!
! Written November 2000 by Charles C Burch
!=======================================================
  subroutine ppio_get_tag_with_data(worker_no, tag)
    integer, intent(in)  :: worker_no
    integer, intent(out) :: tag

    integer     :: status(MPI_STATUS_SIZE), ierr, worker

    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_get_tag_with_data, cpu, worker=", &
       pcps_current_worker_num,worker_no 
      call pcps_print(pcps_message,2)
    endif

    worker=worker_no
    if(worker.eq.-1)worker=MPI_ANY_SOURCE

    if(pcps_num_procs.le.1) then
      tag=0
    else
      call MPI_probe(worker, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
      if(ierr.eq.0) then
        tag=status(MPI_TAG)
      else
        tag=-1
      endif
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_get_tag_with_data, cpu, tag=", &
       pcps_current_worker_num, tag
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_get_tag_with_data

!-----------------------Pack routines --------------------------

!======================PPIO_PACK_CHARS============================
! packs n chars in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_pack_chars(buff, n, packed, n_packed)
    character (len=1), intent(in)    :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_chars, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1), n,MPI_CHARACTER, packed(1),                         &
     n_packed+n*ppio_sizeof_char, n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_chars"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_chars, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_chars

!===================PPIO_PACK_INTEGER============================
! packs n integers in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_pack_integer(buff, n, packed, n_packed)
    integer, intent(in)              :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_integer, cpu, n=",&
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1), n,MPI_INTEGER, packed(1),                           &
     n_packed+n*ppio_sizeof_integer, n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_integer"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_integer, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_integer

!====================PPIO_PACK_REAL==============================
! packs n reals in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_pack_real(buff, n, packed, n_packed)
    real, intent(in)                 :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_real, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1), n, MPI_REAL, packed(1),n_packed+n*ppio_sizeof_real, &
     n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_real"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_real, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_real

!=================PPIO_PACK_DOUBLE_PRECISION======================
! packs n reals in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_pack_double_precision(buff, n, packed, n_packed)
    double precision, intent(in)     :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_double, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1), n, MPI_DOUBLE_PRECISION,                            &
     packed(1),n_packed+n*ppio_sizeof_double_prec, n_packed,                   &
     MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_double_precision"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_double, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_double_precision

!====================PPIO_PACK_COMPLEX===========================
! packs n complex in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_pack_complex(buff, n, packed, n_packed)
    complex, intent(in)              :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_complex, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1), n,MPI_COMPLEX, packed(1),                         &
     n_packed+n*ppio_sizeof_complex, n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_complex"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_complex, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_complex

!=====================PPIO_PACK_LOGICAL==========================
! packs n logical in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_pack_logical(buff, n, packed, n_packed)
    logical, intent(in)              :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_logical, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1), n,MPI_LOGICAL, packed(1),                         &
     n_packed+n*ppio_sizeof_logical, n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_logical"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_logical, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_logical

!==================PPIO_PACK_INTEGER_2D==========================
! packs n integers in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_pack_integer_2d(buff, n, packed, n_packed)
    integer, intent(in)              :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_integer_2d, cpu, n=",&
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1,1), n,MPI_INTEGER, packed(1),                        &
     n_packed+n*ppio_sizeof_integer, n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_integer_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_integer_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_integer_2d

!====================PPIO_PACK_REAL_2D============================
! packs n reals in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_pack_real_2d(buff, n, packed, n_packed)
    real, intent(in)                 :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_real_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1,1), n, MPI_REAL,packed(1),n_packed+n*ppio_sizeof_real,&
     n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_real_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_real_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_real_2d

!===================PPIO_PACK_DOUBLE_PRECISION_2D=================
! packs n reals in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_pack_double_precision_2d(buff, n, packed, n_packed)
    double precision, intent(in)     :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_double_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1,1), n, MPI_DOUBLE_PRECISION,                         &
     packed(1),n_packed+n*ppio_sizeof_double_prec, n_packed,                   &
     MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_double_precision_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_double_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_double_precision_2d

!================== PPIO_PACK_COMPLEX_2D =========================
! packs n complex in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_pack_complex_2d(buff, n, packed, n_packed)
    complex, intent(in)              :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_complex_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1,1), n,MPI_COMPLEX, packed(1),                         &
     n_packed+n*ppio_sizeof_complex, n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_complex_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_complex_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_complex_2d

!====================PPIO_PACK_LOGICAL_2D========================
! packs n logical in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_pack_logical_2d(buff, n, packed, n_packed)
    logical, intent(in)              :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_pack_count=ppio_pack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_pack_logical_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_pack(buff(1,1), n,MPI_LOGICAL, packed(1),                         &
     n_packed+n*ppio_sizeof_logical, n_packed, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_pack_logical_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_pack_logical_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_pack_logical_2d

!----------------------Unpack routines---------------------------

!===================PPIO_UNPACK_CHARS=============================
! unpacks n chars in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_unpack_chars(buff, n, packed, n_packed)
    character (len=1), intent(in)    :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_chars, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_char, n_packed,       &
     buff(1), n, MPI_CHARACTER, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_chars"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_chars, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_chars

!========================PPIO_UNPACK_INTEGER======================
! unpacks n integers in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_unpack_integer(buff, n, packed, n_packed)
    integer, intent(in)              :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_integer, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_integer, n_packed,       &
     buff(1), n, MPI_INTEGER, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_integer"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_integer, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_integer

!=======================PPIO_UNPACK_REAL=========================
! unpacks n reals in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_unpack_real(buff, n, packed, n_packed)
    real, intent(in)                 :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_real, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_real, n_packed,          &
     buff(1), n, MPI_REAL, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_real"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_real, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_real

!=====================PPIO_UNPACK_DOUBLE_PRECISION=========================
! unpacks n double precisions in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!==========================================================================
  subroutine ppio_unpack_double_precision(buff, n, packed, n_packed)
    double precision, intent(in)     :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_double, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_double_prec, n_packed,   &
     buff(1), n, MPI_DOUBLE_PRECISION, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_double_precision"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_double, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_double_precision

!==================PPIO_UNPACK_COMPLEX===========================
! unpacks n complex in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_unpack_complex(buff,n,packed, n_packed)
    complex, intent(out)             :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_complex, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_complex, n_packed,       &
     buff(1), n, MPI_COMPLEX, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_complex"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_complex, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_complex

!======================PPIO_UNPACK_LOGICAL========================
! unpacks n logical in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_unpack_logical(buff,n,packed, n_packed)
    logical, intent(out)             :: buff(:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_logical, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_logical, n_packed,       &
     buff(1), n, MPI_LOGICAL, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_logical"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_logical, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_logical

!======================PPIO_UNPACK_INTEGER_2D=====================
! unpacks n integers in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_unpack_integer_2d(buff, n, packed, n_packed)
    integer, intent(in)              :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_integer_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_integer, n_packed,     &
     buff(1,1), n, MPI_INTEGER, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_integer_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_integer_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_integer_2d

!==================PPIO_UNPACK_REAL_2D===========================
! unpacks n reals in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!================================================================
  subroutine ppio_unpack_real_2d(buff, n, packed, n_packed)
    real, intent(in)                 :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_real_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_real, n_packed,        &
     buff(1,1), n, MPI_REAL, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_real_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_real_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_real_2d

!=================PPIO_UNPACK_DOUBLE_PRECISION_2D==========================
! unpacks n double precisions in buff into array packed with index n_packed
!
! Written November 2000 by Charles C Burch
!=========================================================================
  subroutine ppio_unpack_double_precision_2d(buff, n, packed, n_packed)
    double precision, intent(in)     :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_double_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_double_prec, n_packed, &
     buff(1,1), n, MPI_DOUBLE_PRECISION, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_double_precision_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_double_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_double_precision_2d

!===================PPIO_UNPACK_COMPLEX_2D=======================
! unpacks n complex in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_unpack_complex_2d(buff,n,packed, n_packed)
    complex, intent(out)             :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_complex_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_complex, n_packed,     &
     buff(1,1), n, MPI_COMPLEX, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_complex_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_complex_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_complex_2d

!==================PPIO_UNPACK_LOGICAL_2D========================
! unpacks n logical in buff into array packed with index n_packed
!
! Written February 2001 by Charles C Burch
!================================================================
  subroutine ppio_unpack_logical_2d(buff,n,packed, n_packed)
    logical, intent(out)             :: buff(:,:)
    integer, intent(in)              :: n
    character (len=1), intent(inout) :: packed(:)
    integer, intent(inout)           :: n_packed

    integer      :: ierr

    if(ppio_count_flag) &
      ppio_unpack_count=ppio_unpack_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_unpack_logical_2d, cpu, n=", &
       pcps_current_worker_num, n
      call pcps_print(pcps_message,2)
    endif

    call mpi_unpack(packed(1), n_packed+n*ppio_sizeof_logical, n_packed,     &
     buff(1,1), n, MPI_LOGICAL, MPI_COMM_WORLD, ierr)

    if(ierr.ne.0) then
      write(pcps_message,*) "Error(",ierr,") in ppio_unpack_logical_2d"
      call pcps_abort(pcps_message)
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_unpack_logical_2d, cpu, n_packed=", &
       pcps_current_worker_num, n_packed
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_unpack_logical_2d

!------------------------broadcast routines-----------------------------

!=========================PPIO_BROADCAST_C0=============================
!  broadcast character string x_dat from pe i_pel to the rest of the Pes
!=======================================================================
  subroutine ppio_broadcast_c0(i_pel, x_dat)
    
    integer,          intent(in  )  :: i_pel
    character(len=*), intent(inout) :: x_dat 
    
    integer                         :: istat, i0_inp, n0_inp, l0_inp
    character(len=1),allocatable    :: c0(:)
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_c0, cpu, root, val=",&
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n0_inp = len(x_dat)
    
      if(pcps_current_worker_num.eq.i_pel)then
        l0_inp = len_trim (x_dat)
        call mpi_bcast(l0_inp, 1, mpi_integer, i_pel, mpi_comm_world, istat)
      else
        call mpi_bcast( l0_inp, 1, mpi_integer, i_pel, mpi_comm_world, istat)
        if(l0_inp.lt.n0_inp) x_dat(l0_inp+1:n0_inp)=' '
      endif

      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat, &
         ") in ppio_broadast_c0-length, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    
      if(l0_inp.gt.0) then
        allocate(c0(l0_inp),stat=istat)
        if(istat.ne.0) then
          write(pcps_message,*)"Error(",istat,  &
           ") in ppio_broadcast_c0 allocating c0, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        if(pcps_current_worker_num.eq.i_pel)then
          do i0_inp = 1 , l0_inp
            c0(i0_inp)=x_dat(i0_inp:i0_inp)
          enddo 
      
          call mpi_bcast(c0,l0_inp, mpi_character, i_pel, mpi_comm_world, istat)
        else 
          call mpi_bcast(c0,l0_inp, mpi_character, i_pel, mpi_comm_world, istat)
          do i0_inp = 1 , l0_inp
            x_dat(i0_inp:i0_inp)= c0(i0_inp)
          enddo
        endif

        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_broadcast_c0-data, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
    
        deallocate(c0,stat=istat)
      endif
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_c0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    return
    
  end subroutine ppio_broadcast_c0

!=====================PPIO_BROADCAST_C1=================================
!  broadcast character string x_dat from pe i_pel to the rest of the Pes
!=======================================================================
  subroutine ppio_broadcast_c1(i_pel, n1_inp, x_dat)
    
    integer,   intent(in  )  :: i_pel, n1_inp
    character, intent(inout) :: x_dat(:) 
    
    integer                  :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_c1, cpu, root, val=",&
       pcps_current_worker_num, i_pel, x_dat(1:n1_inp)
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_c1 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat,n1_inp,mpi_character,i_pel, mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_c1-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif
    endif
 
    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_c1, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    return
    
  end subroutine ppio_broadcast_c1

!=====================PPIO_BROADCAST_C2=================================
!  broadcast character string x_dat from pe i_pel to the rest of the Pes
!=======================================================================
  subroutine ppio_broadcast_c2(i_pel, n2_inp, x_dat)
    
    integer,   intent(in  )  :: i_pel, n2_inp
    character, intent(inout) :: x_dat(:,:) 
    
    integer                  :: istat, n0_inp, n1_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_c2, cpu, root, val=",&
       pcps_current_worker_num, i_pel, x_dat(:,1:n1_inp)
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      if(n0_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_c2 n=",n2_inp,  &
         ", data size=", size(x_dat,2)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat, n0_inp, mpi_character, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_c2-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif
    endif
 
    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_c2, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    return
    
  end subroutine ppio_broadcast_c2

!======================PPIO_BROADCAST_I0================================
!  broadcast integer x_dat from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_i0(i_pel, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(inout) :: x_dat 
    
    integer                :: istat
   
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_i0, cpu, root, val=", &
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      call mpi_bcast(x_dat, 1, mpi_integer, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_i0-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_i0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_i0 

!=========================PPIO_BROADCAST_I1============================
!  broadcast n1_inp elements of 1d integer array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_i1(i_pel, n1_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n1_inp
    integer, intent(inout) :: x_dat(:)
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_i1, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_i1 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat,n1_inp,mpi_integer,i_pel, mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_i1-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_i1, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_i1 

!========================PPIO_BROADCAST_I2==============================
!  broadcast n2_inp elements of 2d integer array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_i2( i_pel, n2_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n2_inp
    integer, intent(inout) :: x_dat(:,:)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_i2, cpu, rot, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      if(n0_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_i2 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat,n0_inp,mpi_integer, i_pel, mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_i2-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_i2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_i2 

!=====================PPIO_BROADCAST_I3================================
!  broadcast n3_inp elements of 3d integer array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_i3(i_pel, n3_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n3_inp
    integer, intent(inout) :: x_dat(:,:,:)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: i3_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_i3, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size (x_dat, 1)
      n2_inp = size (x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_i3 n=",n3_inp,  &
         ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3_inp = 1, n3_inp
        call mpi_bcast(x_dat(:,:,i3_inp), n0_inp, &
                     mpi_integer, i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_broadcast_i3-data, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_i3, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_i3 

!=========================PPIO_BROADCAST_I4=============================
!  broadcast n4_inp elements of 4d integer array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_i4(i_pel, n4_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n4_inp
    integer, intent(inout) :: x_dat(:, :, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_i4, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
       istat=0
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_i4 n=",n4_inp,  &
         ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_bcast(x_dat(:, :, i3_inp, i4_inp), n0_inp, &
                       mpi_integer, i_pel, mpi_comm_world, istat)
        enddo
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_i4, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_i4 

!=======================PPIO_BROADCAST_R0==============================
!  broadcast n1_inp elements of 1d real array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_r0(i_pel, x_dat)
    integer, intent(in  )  :: i_pel
    real,    intent(inout) :: x_dat 
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_r0, cpu, root, val=", &
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      call mpi_bcast(x_dat , 1, mpi_real, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_r0-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_r0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_r0 

!=========================PPIO_BROADCAST_R1============================
!  broadcast n1_inp elements of 1d real array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_r1(i_pel, n1_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n1_inp
    real,    intent(inout) :: x_dat(:)
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_r1, cpu, root, n=", &
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_r1 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat, n1_inp, mpi_real, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_r1-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_r1, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_r1 

!=====================PPIO_BROADCAST_R2=================================
!  broadcast n2_inp elements of 2d real array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_r2(i_pel, n2_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n2_inp
    real,    intent(inout) :: x_dat(:, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_r2, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size (x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      if(n0_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_r2 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat, n0_inp, mpi_real, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
         write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_r2-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_r2, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_r2 

!========================PPIO_BROADCAST_R3==============================
!  broadcast n3_inp elements of 3d real array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_r3(i_pel, n3_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n3_inp
    real,    intent(inout) :: x_dat(:, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: i3_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_r3, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_r3 n=",n3_inp,  &
         ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3_inp = 1 , n3_inp
        call mpi_bcast(x_dat(:, :, i3_inp), n0_inp, &
                       mpi_real, i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_broadcast_r3-data, cpu=",pcps_current_worker_num
           call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_r3, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_r3 

!=====================PPIO_BROADCAST_R4================================
!  broadcast n4_inp elements of 4d real array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_r4(i_pel, n4_inp, x_dat)
    integer, intent(in  ) :: i_pel
    integer, intent(in  ) :: n4_inp
    real,    intent(inout) :: x_dat(:, :, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_r4, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
       istat=0
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_r4 n=",n4_inp,  &
         ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_bcast(x_dat(:, :, i3_inp, i4_inp), n0_inp, &
                       mpi_real, i_pel, mpi_comm_world, istat)
        enddo
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_r4, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_r4 

!========================PPIO_BROADCAST_D0=============================
!  broadcast scaler double x_dat from pe i_pel to the rest of the Pes.
!
!======================================================================
  subroutine ppio_broadcast_d0(i_pel, x_dat)
    integer,          intent(in)    :: i_pel
    double precision, intent(inout) :: x_dat 
    
    integer                         :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_d0, cpu, root, val=", &
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      call mpi_bcast(x_dat ,1, mpi_double_precision, i_pel, mpi_comm_world, &
                   istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_d0-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_d0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    return
    
  end subroutine ppio_broadcast_d0 

!=========================PPIO_BROADCAST_D1=============================
!  broadcast n1_inp elements of 1d double array x_dat 
!  from pe i_pel to the rest of the Pes.
!
!======================================================================
  subroutine ppio_broadcast_d1(i_pel, n1_inp, x_dat)
    integer,          intent(in)    :: i_pel
    integer,          intent(in)    :: n1_inp
    double precision, intent(inout) :: x_dat(:)
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_d1, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_d1 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat, n1_inp, mpi_double_precision, &
                   i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_d1-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_d1, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_d1 

!=======================PPIO_BROADCAST_D2===============================
!  broadcast n2_inp elements of 2d double array x_dat 
!  from pe i_pel to the rest of the Pes.
!
!======================================================================
  subroutine ppio_broadcast_d2(i_pel, n2_inp, x_dat)
    integer,          intent(in)    :: i_pel
    integer,          intent(in)    :: n2_inp
    double precision, intent(inout) :: x_dat(:, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_d2, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      if(n0_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_d2 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat, n0_inp, mpi_double_precision, &
                   i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_d2-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_d2, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_d2 

!========================PPIO_BROADCAST_D3=============================
!  broadcast n3_inp elements of 3d double array x_dat 
!  from pe i_pel to the rest of the Pes.
!
!======================================================================
  subroutine ppio_broadcast_d3(i_pel, n3_inp, x_dat)
    integer,          intent(in)    :: i_pel
    integer,          intent(in)    :: n3_inp
    double precision, intent(inout) :: x_dat(:, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: i3_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadacst_d3, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_d3 n=",n3_inp,  &
         ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3_inp = 1 , n3_inp
        call mpi_bcast(x_dat(:,:,i3_inp), n0_inp, mpi_double_precision, &
                     i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_broadcast_d3-data, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_d3, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_d3 

!======================PPIO_BROADCAST_D4================================
!  broadcast n4_inp elements of 4d double array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_d4(i_pel, n4_inp, x_dat)
    integer,          intent(in  )  :: i_pel
    integer,          intent(in  )  :: n4_inp
    double precision, intent(inout) :: x_dat(:, :, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_d4, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
       istat=0
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_d4 n=",n4_inp,  &
         ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_bcast(x_dat(:, :, i3_inp, i4_inp), n0_inp, &
                       mpi_double_precision, i_pel, mpi_comm_world, istat)
        enddo
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_d4, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_d4 

!=========================PPIO_BROADCAST_Z0=============================
!  broadcast scalar complex x_dat from pe i_pel to the rest of the Pes.
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_z0(i_pel, x_dat)
    integer, intent(in)    :: i_pel
    complex, intent(inout) :: x_dat 
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_z0, cpu, root, val=",&
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      call mpi_bcast(x_dat, 1, mpi_complex, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_z0-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_z0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_z0 

!=========================PPIO_BROADCAST_Z1=============================
!  broadcast n1_inp elements of 1d complex array x_dat 
!  from pe i_pel to the rest of the Pes.
!
!======================================================================
  subroutine ppio_broadcast_z1(i_pel, n1_inp, x_dat)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n1_inp
    complex, intent(inout) :: x_dat(:)
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadacast_z1, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_z1 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat, n1_inp, mpi_complex, i_pel, mpi_comm_world, &
                   istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_z1-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_z1, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_z1 

!========================PPIO_BROADCAST_Z2==============================
!  broadcast n2_inp elements of 2d complex array x_dat 
!  from pe i_pel to the rest of the Pes.
!
!======================================================================
  subroutine ppio_broadcast_z2(i_pel, n2_inp, x_dat)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n2_inp
    complex, intent(inout) :: x_dat(:, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_z2, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      if(n0_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_z2 n=",n1_inp,  &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_bcast(x_dat, n0_inp, mpi_complex, &
                   i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_broadcast_z2-data, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_z2, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_z2 

!=======================PPIO_BROADCAST_Z3===============================
!  broadcast n3_inp elements of 3d complex array x_dat 
!  from pe i_pel to the rest of the Pes.
!
!======================================================================
  subroutine ppio_broadcast_z3(i_pel, n3_inp, x_dat)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n3_inp
    complex, intent(inout) :: x_dat(:, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: i3_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_z3, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      istat=0
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_z3 n=",n3_inp,  &
         ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3_inp = 1 , n3_inp
        call mpi_bcast(x_dat(:, :, i3_inp), n0_inp, mpi_complex, &
                     i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_broadcast_z3-data, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_z3, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_z3 

!=======================PPIO_BROADCAST_Z4===============================
!  broadcast n4_inp elements of 4d complex array x_dat 
!  from pe i_pel to the rest of the Pes.
!======================================================================
  subroutine ppio_broadcast_z4(i_pel, n4_inp, x_dat)
    integer, intent(in  )  :: i_pel
    integer, intent(in  )  :: n4_inp
    complex, intent(inout) :: x_dat(:, :, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    
    if(ppio_count_flag) &
      ppio_broadcast_count=ppio_broadcast_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_broadcast_z4, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
       istat=0
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_broadcast_z4 n=",n4_inp,  &
         ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_bcast(x_dat(:, :, i3_inp, i4_inp), n0_inp, &
                       mpi_complex, i_pel, mpi_comm_world, istat)
        enddo
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_broadcast_z4, cpu=", &
       pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_broadcast_z4 

!--------------------------------sum reduce----------------------------

!==========================PPIO_SUM_REDUCE_I0===========================
!  sum reduce scalar integer x_dat on all pes into scalar x_out on pe 
!  i_pel.    May be inplace
!======================================================================
  subroutine ppio_sum_reduce_i0(i_pel, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: x_dat 
    integer, intent(out) :: x_out 
    
    integer                :: istat
    integer                :: x_tmp 
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_i0, cpu, root, val=", &
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else  
      call mpi_reduce(x_dat, x_tmp, 1, mpi_integer, mpi_sum, i_pel, &
                    mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
      if(i_pel.eq.pcps_current_worker_num) x_out = x_tmp 
    endif


    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_i0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_i0 

!===========================PPIO_SUM_REDUCE_I1==========================
!  sum reduce n1_inp elements of 1d integer array x_dat on  all pes 
!  into integer array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_i1(i_pel, n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n1_inp
    integer, intent(in)  :: x_dat(:)
    integer, intent(out) :: x_out(:)
    
    integer              :: istat
    integer              :: x_tmp(size(x_dat, 1))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_i1, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_i1 n=",n1_inp, &
       ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else 
      call mpi_reduce ( x_dat(1:n1_inp), x_tmp(1:n1_inp), n1_inp, &
                      mpi_integer, mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_i1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) x_out(1:n1_inp) = x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_i1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_i1 

!===========================PPIO_SUM_REDUCE_I2==========================
!  sum reduce n2_inp elements of 2d integer array x_dat on all pes
!  into integer array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_i2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n2_inp
    integer, intent(in)  :: x_dat(:, :)
    integer, intent(out) :: x_out(:, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    integer              :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_i2, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_i2 n=",n2_inp, &
       ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,:n2_inp)
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_reduce(x_dat(1:n1_inp, 1:n2_inp), x_tmp(1:n1_inp, 1:n2_inp),&
                    n0_inp, mpi_integer, mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_i2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) &
        x_out(1:n1_inp, 1:n2_inp) = x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_i2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_i2 

!======================PPIO_SUM_REDUCE_I3==============================
!  sum reduce n3_inp elements of 3d integer array x_dat on all pes
!  into integer array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_i3(i_pel, n3_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n3_inp
    integer, intent(in)  :: x_dat(:, :, :)
    integer, intent(out) :: x_out(:, :, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    integer              :: n2_inp
    integer              :: i3_inp
    integer              :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_i3, cpu, root, n=",&
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_i3 n=",n3_inp, &
       ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,:n3_inp)
    else
      n1_inp = size (x_dat, 1)
      n2_inp = size (x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      do i3_inp = 1 , n3_inp
        call mpi_reduce(x_dat(:, :, i3_inp), &
                      x_tmp, n0_inp, mpi_integer, mpi_sum, &
                      i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_reduce_i3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

      if(i_pel.eq.pcps_current_worker_num) &
        x_out(:, :, i3_inp) = x_tmp(:, :)
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_i3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_i3 

!========================PPIO_SUM_REDUCE_I4=============================
!  sum reduce n4_inp elements of 4d integer array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_i4(i_pel, n4_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n4_inp
    integer, intent(in)  :: x_dat(:, :, :, :)
    integer, intent(out) :: x_out(:, :, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    integer                :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_i4, cpu, root, n=",&
       pcps_current_worker_num, i_pel, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_i4 n=",n4_inp, &
       ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:,1:n4_inp)=x_dat(1:,1:,1:,1:n4_inp)
    else

      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
    
      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_reduce(x_dat(:, :, i3_inp, i4_inp), &
                          x_tmp, n0_inp, &
                          mpi_integer, mpi_sum, i_pel, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_reduce_i4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          if(i_pel.eq.pcps_current_worker_num) &
           x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo 
      enddo
    endif 

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_i4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
    !
  end subroutine ppio_sum_reduce_i4 

!==========================PPIO_SUM_REDUCE_R0==========================
!  sum reduce scalar x_dat on all pes into scalar x_out on pe i_pel.
!  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_r0(i_pel, x_dat, x_out)
    integer, intent(in)  :: i_pel
    real,    intent(in)  :: x_dat 
    real,    intent(out) :: x_out 
    
    integer              :: istat
    real                 :: x_tmp 
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_r0, cpu, root, val=",&
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else 
      call mpi_reduce(x_dat, x_tmp, 1, mpi_real, mpi_sum, i_pel, &
                    mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) x_out = x_tmp 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_r0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_r0

!=========================PPIO_SUM_REDUCE_R1===========================
!  sum reduce n1_inp elements of 1d real array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_r1(i_pel, n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n1_inp
    real,    intent(in)  :: x_dat(:)
    real,    intent(out) :: x_out(:)
    
    integer              :: istat
    real                 :: x_tmp(size(x_dat, 1))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_r1, cpu, roor, n=", &
       pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_r1 n=",n1_inp, &
       ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else 
      call mpi_reduce(x_dat(1:n1_inp), x_tmp(1:n1_inp), n1_inp, &
                    mpi_real, mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_r1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) x_out(1:n1_inp) = x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_r1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_r1 

!=======================PPIO_SUM_REDUCE_R2==============================
!  sum reduce n2_inp elements of 2d real array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_r2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n2_inp
    real,    intent(in)  :: x_dat(:, :)
    real,    intent(out) :: x_out(:, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    real                 :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_r2, cpu, roor, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_r2 n=",n2_inp, &
       ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,:n2_inp)
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_reduce(x_dat(1:n1_inp, 1:n2_inp), x_tmp(1:n1_inp, 1:n2_inp), &
                    n0_inp, mpi_real, mpi_sum, i_pel, mpi_comm_world,     &
                    istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_r2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) &
        x_out(1:n1_inp, 1:n2_inp) = x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_r2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_r2 

!=======================PPIO_SUM_REDUCE_R3==============================
!  sum reduce n3_inp elements of 3d real array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_r3(i_pel, n3_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n3_inp
    real,    intent(in)  :: x_dat(:, :, :)
    real,    intent(out) :: x_out(:, :, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    integer              :: n2_inp
    integer              :: i3_inp
    real                 :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_r3, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_r3 n=",n3_inp, &
       ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,:n3_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      do i3_inp = 1 , n3_inp
        call mpi_reduce(x_dat(:, :, i3_inp), x_tmp, n0_inp, &
                        mpi_real, mpi_sum, i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_reduce_r3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        if(i_pel.eq.pcps_current_worker_num) &
          x_out(:, :, i3_inp) = x_tmp(:, :)
      enddo 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_r3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_r3 

!=======================PPIO_SUM_REDUCE_R4==============================
!  sum reduce n4_inp elements of 4d real array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_r4(i_pel, n4_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n4_inp
    real,    intent(in)  :: x_dat(:, :, :, :)
    real,    intent(out) :: x_out(:, :, :, :)
    !
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    real                   :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_r4, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_r4 n=",n4_inp, &
       ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:,1:n4_inp)=x_dat(1:,1:,1:,1:n4_inp)
    else

      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
    
      if(n1_inp.gt.size(x_dat,dim=1)) then
        write(pcps_message,*) "Data Overflow in ppio_sum_reduce_i1 n=",n1_inp, &
         ", data size=", size(x_dat,dim=1)
        call pcps_abort(pcps_message)
      endif

      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_reduce(x_dat(:, :, i3_inp, i4_inp), x_tmp, n0_inp, &
                        mpi_real, mpi_sum, i_pel, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_reduce_r4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          if(i_pel.eq.pcps_current_worker_num) &
           x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo 
      enddo
    endif 
 
   if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_r4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_r4 

!=========================PPIO_SUM_REDUCE_D0================================
!  sum reduce double scalar x_dat on all pes into scalar x_out on cpu i_pel.
!  may be inplace
!===========================================================================
  subroutine ppio_sum_reduce_d0(i_pel, x_dat, x_out)
    integer,          intent(in)  :: i_pel
    double precision, intent(in)  :: x_dat 
    double precision, intent(out) :: x_out 
    
    integer                       :: istat
    double precision              :: x_tmp
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_d0, cpu, root, val=", &
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_reduce(x_dat, x_tmp, 1, mpi_double_precision, &
                    mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) x_out=x_tmp
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_d0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_d0

!========================PPIO_SUM_REDUCE_D1============================
!  sum reduce n1_inp elements of 1d double array x_dat on all pes
!  into double array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_d1(i_pel, n1_inp, x_dat, x_out)
    integer,          intent(in)  :: i_pel
    integer,          intent(in)  :: n1_inp
    double precision, intent(in)  :: x_dat(:)
    double precision, intent(out) :: x_out(:)
    
    integer                       :: istat
    double precision              :: x_tmp(size(x_dat,1))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_d1, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d1 n=",n1_inp, &
       ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else
      call mpi_reduce(x_dat, x_tmp, n1_inp, mpi_double_precision, & 
                     mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_d1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) x_out(1:n1_inp)=x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_d1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_d1 

!=======================PPIO_SUM_REDUCE_D2=============================
!  sum reduce n2_inp elements of 2d double array x_dat on all pes
!  into real array x_out on pe i_pel. may be inplace
!======================================================================
  subroutine ppio_sum_reduce_d2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)           :: i_pel
    integer, intent(in)           :: n2_inp
    double precision, intent(in)  :: x_dat(:,:)
    double precision, intent(out) :: x_out(:,:)
    
    integer                      :: istat
    integer                      :: n0_inp
    integer                      :: n1_inp
    double precision             :: x_tmp(size(x_dat,1), size(x_dat,2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_d2, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d2 n=",n2_inp, &
       ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,1:n2_inp)
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_reduce(x_dat, x_tmp, n0_inp, mpi_double_precision, &
                    mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_d2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) &
        x_out(1:n1_inp, 1:n2_inp)=x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_d2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_d2 

!=========================PPIO_SUM_REDUCE_D3===========================
!  sum reduce n3_inp elements of 3d double array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_d3(i_pel, n3_inp, x_dat, x_out)
    integer,          intent(in)  :: i_pel
    integer,          intent(in)  :: n3_inp
    double precision, intent(in)  :: x_dat(:, :, :)
    double precision, intent(out) :: x_out(:, :, :)
    
    integer                       :: istat
    integer                       :: n0_inp
    integer                       :: n1_inp
    integer                       :: n2_inp
    integer                       :: i3_inp
    double precision              :: x_tmp(size(x_dat,1), size(x_dat,2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_d3, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d3,n=",n3_inp, &
       ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,1:n3_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      do i3_inp = 1 , n3_inp
         call mpi_reduce(x_dat(:, :, i3_inp), x_tmp, n0_inp, &
            mpi_double_precision, mpi_sum, i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_reduce_d3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        if(i_pel.eq.pcps_current_worker_num) &
          x_out(:, :, i3_inp) = x_tmp(:, :)
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_d3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_d3 

!=========================PPIO_SUM_REDUCE_D4===========================
!  sum reduce n4_inp elements of 4d double array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_d4(i_pel, n4_inp, x_dat, x_out)
    integer, intent(in)           :: i_pel
    integer, intent(in)           :: n4_inp
    double precision, intent(in)  :: x_dat(:, :, :, :)
    double precision, intent(out) :: x_out(:, :, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    double precision       :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_d4, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d4,n=",n4_inp, &
       ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:,1:n4_inp)=x_dat(1:,1:,1:,1:n4_inp)
    else

      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp

      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_reduce(x_dat(:, :, i3_inp, i4_inp), x_tmp, n0_inp, &
                   mpi_double_precision, mpi_sum, i_pel, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_reduce_d4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          if(i_pel.eq.pcps_current_worker_num) &
           x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo 
      enddo
    endif 
 
   if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_d4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_d4 

!===========================PPIO_SUM_REDUCE_Z0=========================
!  sum reduce complex scalar x_dat on all pes  into scalar x_out on 
!  cpu i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_z0(i_pel, x_dat, x_out)
    integer, intent(in)  :: i_pel
    complex, intent(in)  :: x_dat 
    complex, intent(out) :: x_out 
    
    integer              :: istat
    complex              :: x_tmp
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_z0, cpu, root, val=", &
       pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_reduce(x_dat, x_tmp, 1, mpi_complex, &
                    mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_z0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) x_out=x_tmp
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_z0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_z0

!======================PPIO_SUM_REDUCE_Z1==============================
!  sum reduce n1_inp elements of 1d complex array x_dat on all pes
!  into double array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_z1(i_pel, n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n1_inp
    complex, intent(in)  :: x_dat(:)
    complex, intent(out) :: x_out(:)
    
    integer              :: istat
    complex              :: x_tmp(size(x_dat,1))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_z1, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d1,n=",n1_inp, &
       ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else
      call mpi_reduce(x_dat, x_tmp, n1_inp, mpi_complex, & 
                    mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_z1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) x_out(1:n1_inp) = x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_z1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_z1 

!======================PPIO_SUM_REDUCE_Z2==============================
!  sum educe n2_inp elements of 2d complex array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_z2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n2_inp
    complex, intent(in)  :: x_dat(:,:)
    complex, intent(out) :: x_out(:,:)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    complex              :: x_tmp(size(x_dat,1), size(x_dat,2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_z2, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d2,n=",n2_inp, &
       ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,1:n2_inp)
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_reduce(x_dat, x_tmp, n0_inp, mpi_complex, &
                    mpi_sum, i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_reduce_z2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      if(i_pel.eq.pcps_current_worker_num) &
        x_out(1:n1_inp, 1:n2_inp) = x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_z2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_z2 

!============================PPIO_SUM_REDUCE_Z3========================
!  sum reduce n3_inp elements of 3d complex array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_z3(i_pel, n3_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n3_inp
    complex, intent(in)  :: x_dat(:, :, :)
    complex, intent(out) :: x_out(:, :, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    integer              :: n2_inp
    integer              :: i3_inp
    complex              :: x_tmp(size(x_dat,1), size(x_dat,2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_z3, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d3,n=",n3_inp, &
       ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,1:n3_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      do i3_inp = 1 , n3_inp
        call mpi_reduce(x_dat(:, :, i3_inp), x_tmp, n0_inp, mpi_complex, &
           mpi_sum, i_pel, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_reduce_z3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        if(i_pel.eq.pcps_current_worker_num) &
          x_out(:, :, i3_inp) = x_tmp(:, :)
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_z3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_z3 

!=====================PPIO_SUM_REDUCE_Z4===============================
!  sum reduce n4_inp elements of 4d double array x_dat on all pes
!  into real array x_out on pe i_pel.  may be inplace
!======================================================================
  subroutine ppio_sum_reduce_z4(i_pel, n4_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n4_inp
    complex, intent(in)  :: x_dat(:, :, :, :)
    complex, intent(out) :: x_out(:, :, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: n3_inp
    integer                :: i3_inp
    integer                :: i4_inp
    complex                :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_reduce_count=ppio_sum_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_reduce_z4, cpu, root, n=", &
       pcps_current_worker_num, i_pel, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_reduce_d4,n=",n4_inp, &
       ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:,1:n4_inp)=x_dat(1:,1:,1:,1:n4_inp)
    else

      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
    
      do i4_inp = 1 , n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_reduce(x_dat(:, :, i3_inp, i4_inp), x_tmp, n0_inp, &
                        mpi_complex, mpi_sum, i_pel, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_reduce_z4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          if(i_pel.eq.pcps_current_worker_num) &
           x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo 
      enddo
    endif 
 
   if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_reduce_z4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_reduce_z4 

!---------------------------sum all reduce routines--------------------

!==================== PPIO_SUM_ALL_REDUCE_I0===========================
!  sum reduce scalar integer x_dat on all pes
!  into scalar x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_i0(x_dat, x_out)
    integer, intent(in ) :: x_dat 
    integer, intent(out) :: x_out 
    
    integer              :: istat
    integer              :: x_tmp 
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else 
      call mpi_allreduce( x_dat, x_tmp, 1, mpi_integer, mpi_sum, &
                        mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out = x_tmp 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_i0 

!==========================PPIO_SUM_ALL_REDUCE_I1=======================
!  sum reduce n1_inp elements of 1d integer array x_dat on all pes
!  into integer array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_i1(n1_inp, x_dat, x_out)
    integer, intent(in)  :: n1_inp
    integer, intent(in)  :: x_dat(:)
    integer, intent(out) :: x_out(:)
    
    integer              :: istat
    integer              :: x_tmp(size( x_dat,1))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_i1, cpu, n=", &
       pcps_current_worker_num, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_i1,n=",  &
       n1_inp, ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else 
      call mpi_allreduce(x_dat(1:n1_inp), x_tmp(1:n1_inp), n1_inp, &
                       mpi_integer, mpi_sum, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_i1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp) = x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_i1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_i1 

!=======================PPIO_SUM_ALL_REDUCE_I2==========================
!  sum reduce n2_inp elements of 2d integer array x_dat on all pes
!  into integer array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_i2(n2_inp, x_dat, x_out)
    integer, intent(in)  :: n2_inp
    integer, intent(in)  :: x_dat(:, :)
    integer, intent(out) :: x_out(:, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    integer              :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_i2, cpu, n=", &
       pcps_current_worker_num, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_i2,n=",  &
       n2_inp, ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,1:n2_inp)
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_allreduce(x_dat(1:n1_inp, 1:n2_inp), x_tmp(1:n1_inp, 1:n2_inp), &
                       n0_inp, mpi_integer, mpi_sum, mpi_comm_world, &
                       istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_i2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp, 1:n2_inp) = x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_i2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_i2 

!=======================PPIO_SUM_ALL_REDUCE_I3=========================
!  sum reduce n3_inp elements of 3d integer array x_dat on all pes
!  into integer array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_i3(n3_inp, x_dat, x_out)
    integer, intent(in)  :: n3_inp
    integer, intent(in)  :: x_dat(:, :, :)
    integer, intent(out) :: x_out(:, :, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    integer              :: n2_inp
    integer              :: i3_inp
    integer              :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
   
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_i3, cpu, n=", &
       pcps_current_worker_num, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_i3,n=",  &
       n3_inp, ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,1:n3_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
   
      do i3_inp = 1 , n3_inp
        call mpi_allreduce(x_dat(:, :, i3_inp), x_tmp, n0_inp, mpi_integer, &
                         mpi_sum, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_all_reduce_i3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        x_out(:, :, i3_inp) = x_tmp(:, :)
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_i3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_i3 

!=====================PPIO_SUM_ALL_REDUCE_I4===========================
!  sum reduce n4_inp elements of 4d integer array x_dat on all pes
!  into integer array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_i4(n4_inp, x_dat, x_out)
    integer, intent(in)  :: n4_inp
    integer, intent(in)  :: x_dat(:, :, :,:)
    integer, intent(out) :: x_out(:, :, :,:)
    
    integer              :: istat
    integer              :: n0_inp, n1_inp, n2_inp, n3_inp, i3_inp, i4_inp
    integer              :: x_tmp(size(x_dat,1), size(x_dat,2))
    
   
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_i4, cpu, n=", &
       pcps_current_worker_num, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_i4,n=",  &
       n4_inp, ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:, 1:, 1:, 1:n4_inp)=x_dat(1:, 1:, 1:, 1:n4_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
   
      do i4_inp = 1, n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_allreduce(x_dat(:, :, i3_inp, i4_inp), x_tmp, n0_inp, &
             mpi_integer, mpi_sum, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_all_reduce_i4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_i4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_i4 

!======================PPIO_SUM_ALL_REDUCE_R0===========================
!  sum reduce scalar x_dat on all pes
!  into scalar x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_r0(x_dat, x_out)
    real,    intent(in ) :: x_dat 
    real,    intent(out) :: x_out 
    
    integer              :: istat
    real                 :: x_tmp 
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_allreduce(x_dat, x_tmp, 1, mpi_real, mpi_sum, &
                       mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out = x_tmp 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_r0

!======================PPIO_SUM_ALL_REDUCE_R1==========================
!  sum reduce n1_inp elements of 1d real array x_dat on all pes
!  into real array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_r1(n1_inp, x_dat, x_out)
    integer, intent(in)  :: n1_inp
    real,    intent(in)  :: x_dat(:)
    real,    intent(out) :: x_out(:)
    
    integer              :: istat
    real                 :: x_tmp(size ( x_dat, 1))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_r1, cpu, n=", &
       pcps_current_worker_num, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_r1,n=",  &
       n1_inp, ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else
      call mpi_allreduce(x_dat, x_tmp, n1_inp, mpi_real, mpi_sum, &
                       mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_r1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp) = x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_r1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_r1 

!======================PPIO_SUM_ALL_REDUCE_R2===========================
!  sum reduce n2_inp elements of 2d real array x_dat on all pes
!  into real array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_r2(n2_inp, x_dat, x_out)
    integer, intent(in)  :: n2_inp
    real,    intent(in)  :: x_dat(:, :)
    real,    intent(out) :: x_out(:, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    real                 :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_r2, cpu, n=", &
       pcps_current_worker_num, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_r2,n=",  &
       n2_inp, ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,1:n2_inp)
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
     
      call mpi_allreduce(x_dat(1:n1_inp, 1:n2_inp), x_tmp(1:n1_inp, 1:n2_inp), &
                       n0_inp, mpi_real, mpi_sum, mpi_comm_world, &
                       istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_r2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp, 1:n2_inp) = x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_r2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_r2 

!====================PPIO_SUM_ALL_REDUCE_R3============================
!  sum reduce n3_inp elements of 3d real array x_dat on all pes
!  into real array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_r3(n3_inp, x_dat, x_out)
    integer, intent(in)  :: n3_inp
    real,    intent(in ) :: x_dat(:, :, :)
    real,    intent(out) :: x_out(:, :, :)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    integer              :: n2_inp
    integer              :: i3_inp
    real                 :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_r3, cpu, n=", &
       pcps_current_worker_num, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_r3,n=",  &
       n3_inp, ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,1:n3_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      do i3_inp = 1 , n3_inp
        call mpi_allreduce(x_dat(:, :, i3_inp), x_tmp, n0_inp, &
                         mpi_real, mpi_sum, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_all_reduce_r3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        x_out(:, :, i3_inp) = x_tmp(:, :)
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_r3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_r3 

!======================PPIO_SUM_ALL_REDUCE_R4===========================
!  sum reduce n4_inp elements of 4d real array x_dat on all pes
!  into integer array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_r4(n4_inp, x_dat, x_out)
    integer, intent(in)  :: n4_inp
    real,    intent(in)  :: x_dat(:, :, :,:)
    real,    intent(out) :: x_out(:, :, :,:)
    
    integer              :: istat
    integer              :: n0_inp, n1_inp, n2_inp, n3_inp, i3_inp, i4_inp
    real                 :: x_tmp(size(x_dat,1), size(x_dat,2))
    
   
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_r4, cpu, n=", &
       pcps_current_worker_num, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_r4,n=",  &
       n4_inp, ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:, 1:, 1:, 1:n4_inp)=x_dat(1:, 1:, 1:, 1:n4_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
   
      do i4_inp = 1, n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_allreduce(x_dat(:, :, i3_inp, i4_inp), x_tmp, n0_inp, &
            mpi_real, mpi_sum, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_all_reduce_r4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_r4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_r4 

!=========================PPIO_SUM_ALL_REDUCE_D0========================
!  sum reduce double scalar x_dat on all pes
!  into scalar x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_d0(x_dat, x_out)
    double precision, intent(in)  :: x_dat 
    double precision, intent(out) :: x_out 
    
    integer                       :: istat
    double precision              :: x_tmp 
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else 
      call mpi_allreduce(x_dat , x_tmp , 1, mpi_double_precision, &
                       mpi_sum, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out = x_tmp 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_d0

!========================PPIO_SUM_ALL_REDUCE_D1=========================
!  sum reduce n1_inp elements of 1d double array x_dat on all pes
!  into double array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_d1(n1_inp, x_dat, x_out)
    integer,          intent(in)  :: n1_inp
    double precision, intent(in)  :: x_dat(:)
    double precision, intent(out) :: x_out(:)
    
    integer                       :: istat
    double precision              :: x_tmp(size(x_dat, 1))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_d1, cpu, n=", &
       pcps_current_worker_num, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_d1,n=",  &
       n1_inp, ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else
      call mpi_allreduce(x_dat, x_tmp, n1_inp, mpi_double_precision, &
                       mpi_sum, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_d1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp) = x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_d1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_d1 

!======================PPIO_SUM_ALL_REDUCE_D2===========================
!  sum reduce n2_inp elements of 2d double array x_dat on all pes
!  double real array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_d2(n2_inp, x_dat, x_out)
    integer,          intent(in)  :: n2_inp
    double precision, intent(in)  :: x_dat(:, :)
    double precision, intent(out) :: x_out(:, :)
    
    integer                       :: istat
    integer                       :: n0_inp
    integer                       :: n1_inp
    double precision              :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_d2, cpu, n=", &
       pcps_current_worker_num, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_d2,n=",  &
       n2_inp, ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,1:n2_inp)
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_allreduce(x_dat, x_tmp, n0_inp, mpi_double_precision, &
                       mpi_sum, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_d2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp, 1:n2_inp) =  x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_d2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_d2 

!======================PPIO_SUM_ALL_REDUCE_D3==========================
!  sum reduce n3_inp elements of 3d double array x_dat on all pes
!  into double array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_d3(n3_inp, x_dat, x_out)
    integer,          intent(in)  :: n3_inp
    double precision, intent(in)  :: x_dat(:, :, :)
    double precision, intent(out) :: x_out(:, :, :)
    
    integer                       :: istat
    integer                       :: n0_inp
    integer                       :: n1_inp
    integer                       :: n2_inp
    integer                       :: i3_inp
    double precision              :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_d3, cpu, n=", &
       pcps_current_worker_num, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_d3,n=",  &
       n3_inp, ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,1:n3_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      do i3_inp = 1 , n3_inp
        call mpi_allreduce(x_dat(:, :, i3_inp), x_tmp, n0_inp, &
           mpi_double_precision, mpi_sum, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_all_reduce_d3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        x_out(:, :, i3_inp) = x_tmp(:, :)
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_d3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_d3 

!========================PPIO_SUM_ALL_REDUCE_D4========================
!  sum reduce n4_inp elements of 4d double array x_dat on all pes
!  into integer array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_d4(n4_inp, x_dat, x_out)
    integer,          intent(in)  :: n4_inp
    double precision, intent(in)  :: x_dat(:, :, :,:)
    double precision, intent(out) :: x_out(:, :, :,:)
    
    integer              :: istat
    integer              :: n0_inp, n1_inp, n2_inp, n3_inp, i3_inp, i4_inp
    double precision     :: x_tmp(size(x_dat,1), size(x_dat,2))
    
   
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_d4, cpu, n=", &
       pcps_current_worker_num, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_d4,n=",  &
       n4_inp, ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:, 1:, 1:, 1:n4_inp)=x_dat(1:, 1:, 1:, 1:n4_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
   
      do i4_inp = 1, n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_allreduce(x_dat(:, :, i3_inp, i4_inp), x_tmp, n0_inp, &
            mpi_double_precision, mpi_sum, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_all_reduce_d4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_d4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_d4 

!=======================PPIO_SUM_ALL_REDUCE_Z0==========================
!  sum reduce complex scalar x_dat on all pes
!  into scalar x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_z0(x_dat, x_out)
    complex, intent(in)  :: x_dat 
    complex, intent(out) :: x_out 
    
    integer              :: istat
    complex              :: x_tmp 
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_z0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else 
      call mpi_allreduce(x_dat , x_tmp , 1, mpi_complex, &
                       mpi_sum, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_z0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out = x_tmp 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_z0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_z0

!=========================PPIO_SUM_ALL_REDUCE_Z1========================
!  sum reduce n1_inp elements of 1d complex array x_dat on all pes
!  into complex array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_z1(n1_inp, x_dat, x_out)
    integer,    intent(in)  :: n1_inp
    complex,    intent(in)  :: x_dat(:)
    complex,    intent(out) :: x_out(:)
    
    integer                :: istat
    complex                :: x_tmp(size(x_dat, 1))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_z1, cpu, n=", &
       pcps_current_worker_num, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(n1_inp.gt.size(x_dat,dim=1)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_z1,n=",  &
       n1_inp, ", data size=", size(x_dat,dim=1)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp)=x_dat(1:n1_inp)
    else
      call mpi_allreduce(x_dat, x_tmp, n1_inp, mpi_complex, &
                       mpi_sum, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_z1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp) = x_tmp(1:n1_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_z1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_z1 

!=========================PPIO_SUM_ALL_REDUCE_Z2========================
!  sum reduce n2_inp elements of 2d complex array x_dat on all pes
!  complex array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_z2(n2_inp, x_dat, x_out)
    integer,   intent(in)  :: n2_inp
    complex,   intent(in)  :: x_dat(:, :)
    complex,   intent(out) :: x_out(:, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    complex                :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_z2, cpu, n=", &
       pcps_current_worker_num, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(n2_inp.gt.size(x_dat,dim=2)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_z2,n=",  &
       n2_inp, ", data size=", size(x_dat,dim=2)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp)=x_dat(1:,1:n2_inp)
    else
      n1_inp = size(x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_allreduce(x_dat, x_tmp, n0_inp, mpi_complex, &
                       mpi_sum, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_sum_all_reduce_z2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      x_out(1:n1_inp, 1:n2_inp) =  x_tmp(1:n1_inp, 1:n2_inp)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_z2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_z2 

!=======================PPIO_SUM_ALL_REDUCE_Z3=========================
!  sum reduce n3_inp elements of 3d complex array x_dat on all pes
!  into complex array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_z3(n3_inp, x_dat, x_out)
    integer,   intent(in)  :: n3_inp
    complex,   intent(in)  :: x_dat(:, :, :)
    complex,   intent(out) :: x_out(:, :, :)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    integer                :: n2_inp
    integer                :: i3_inp
    complex                :: x_tmp(size(x_dat, 1), size(x_dat, 2))
    
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_z3, cpu, n=", &
       pcps_current_worker_num, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(n3_inp.gt.size(x_dat,dim=3)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_z3,n=",  &
       n3_inp, ", data size=", size(x_dat,dim=3)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:,1:n3_inp)=x_dat(1:,1:,1:n3_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n0_inp = n1_inp * n2_inp
    
      do i3_inp = 1, n3_inp
        call mpi_allreduce(x_dat(:, :, i3_inp), x_tmp, n0_inp, mpi_complex, &
                         mpi_sum, mpi_comm_world, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_sum_all_reduce_z3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 

        x_out(:, :, i3_inp) = x_tmp(:, :)
      enddo 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_z3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_z3 

!========================PPIO_SUM_ALL_REDUCE_Z4========================
!  sum reduce n4_inp elements of 4d complex array x_dat on all pes
!  into integer array x_out and broadcast to all pes.
!  may be inplace
!======================================================================
  subroutine ppio_sum_all_reduce_z4(n4_inp, x_dat, x_out)
    integer, intent(in)  :: n4_inp
    complex, intent(in)  :: x_dat(:, :, :,:)
    complex, intent(out) :: x_out(:, :, :,:)
    
    integer     :: istat
    integer     :: n0_inp, n1_inp, n2_inp, n3_inp, i3_inp, i4_inp
    complex     :: x_tmp(size(x_dat,1), size(x_dat,2))
    
   
    if(ppio_count_flag) &
      ppio_sum_all_reduce_count=ppio_sum_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_sum_all_reduce_z4, cpu, n=", &
       pcps_current_worker_num, n4_inp
      call pcps_print(pcps_message,2)
    endif

    if(n4_inp.gt.size(x_dat,dim=4)) then
      write(pcps_message,*) "Data Overflow in ppio_sum_all_reduce_z4,n=",  &
       n4_inp, ", data size=", size(x_dat,dim=4)
      call pcps_abort(pcps_message)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:, 1:, 1:, 1:n4_inp)=x_dat(1:, 1:, 1:, 1:n4_inp)
    else
      n1_inp = size(x_dat, 1)
      n2_inp = size(x_dat, 2)
      n3_inp = size(x_dat, 3)
      n0_inp = n1_inp * n2_inp
   
      do i4_inp = 1, n4_inp
        do i3_inp = 1 , n3_inp
          call mpi_allreduce(x_dat(:, :, i3_inp, i4_inp), x_tmp, n0_inp, &
             mpi_complex, mpi_sum, mpi_comm_world, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_sum_all_reduce_z4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif 

          x_out(:, :, i3_inp, i4_inp) = x_tmp(:, :)
        enddo
      end do
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_sum_all_reduce_z4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_sum_all_reduce_z4 

!=========================PPIO_MAX_ALL_REDUCE_I0=======================
!  return the maximum of integer x_dat from all pes to all pes.
!======================================================================
  function ppio_max_all_reduce_i0(x_dat) result(x_out)
    integer, intent(in) :: x_dat
    integer             :: x_out
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_max_all_reduce_count=ppio_max_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_max_all_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_allreduce(x_dat,x_out,1,mpi_integer,mpi_max,&
                         mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_max_all_reduce_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_max_all_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_max_all_reduce_i0

!======================PPIO_MAX_ALL_REDUCE_R0==========================
!  return the maximum of real x_dat from all pes to all pes.
!======================================================================
  function ppio_max_all_reduce_r0(x_dat) result(x_out)
    real, intent(in) :: x_dat
    real             :: x_out
    integer          :: istat
    
    if(ppio_count_flag) &
      ppio_max_all_reduce_count=ppio_max_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_max_all_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_allreduce(x_dat,x_out,1,mpi_real,mpi_max,&
                         mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_max_all_reduce_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_max_all_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_max_all_reduce_r0

!=====================PPIO_MAX_ALL_REDUCE_D0===========================
!  return the maximum of double x_dat from all pes to all pes.
!======================================================================
  function ppio_max_all_reduce_d0(x_dat) result(x_out)
    double precision, intent(in) :: x_dat
    double precision             :: x_out
    integer                      :: istat
    
    if(ppio_count_flag) &
      ppio_max_all_reduce_count=ppio_max_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_max_all_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_allreduce(x_dat,x_out,1,mpi_double_precision,mpi_max,&
                         mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_max_all_reduce_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_max_all_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_max_all_reduce_d0

!=======================PPIO_MIN_ALL_REDUCE_I0=========================
!  return the minimum of integer x_dat from all pes to all pes.
!======================================================================
  function ppio_min_all_reduce_i0(x_dat) result(x_out)
    integer, intent(in) :: x_dat
    integer             :: x_out
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_min_all_reduce_count=ppio_min_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_min_all_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_allreduce(x_dat,x_out,1,mpi_integer,mpi_min,&
                         mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_min_all_reduce_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_min_all_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_min_all_reduce_i0

!====================ppio_min_all_reduce_r0============================
!  return the minimum of real x_dat from all pes to all pes.
!======================================================================
  function ppio_min_all_reduce_r0(x_dat) result(x_out)
    real, intent(in) :: x_dat
    real             :: x_out
    integer          :: istat
    
    if(ppio_count_flag) &
      ppio_min_all_reduce_count=ppio_min_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_min_all_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_allreduce(x_dat,x_out,1,mpi_real,mpi_min,&
                         mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_min_all_reduce_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_min_all_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_min_all_reduce_r0

!=====================PPIO_MIN_ALL_REDUCE_D0===========================
!  return the minimum of double x_dat from all pes to all pes.
!======================================================================
  function ppio_min_all_reduce_d0(x_dat) result(x_out)
    double precision, intent(in) :: x_dat
    double precision             :: x_out
    integer                      :: istat
    
    if(ppio_count_flag) &
      ppio_min_all_reduce_count=ppio_min_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_min_all_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out=x_dat
    else
      call mpi_allreduce(x_dat,x_out,1,mpi_double_precision,mpi_min,&
                         mpi_comm_world,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_min_all_reduce_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_min_all_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_min_all_reduce_d0

!======================PPIO_MAX_REDUCE_I0==============================
!  return the maximum of integer x_dat from all pes to root 
!======================================================================
  function ppio_max_reduce_i0(root,x_dat) result(x_out)
    integer, intent(in) :: root, x_dat
    integer             :: x_out
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_max_reduce_count=ppio_max_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_max_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    x_out=x_dat
    if(pcps_num_procs.gt.1) then
      call mpi_reduce(x_dat, x_out, 1, mpi_integer, mpi_max,&
                         root, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_max_reduce_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_max_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_max_reduce_i0

!=======================PPIO_MAX_REDUCE_R0=============================
!  return the maximum of integer x_dat from all pes to root 
!======================================================================
  function ppio_max_reduce_r0(root,x_dat) result(x_out)
    integer, intent(in) :: root
    real, intent(in)    :: x_dat
    real                :: x_out
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_max_reduce_count=ppio_max_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_max_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    x_out=x_dat
    if(pcps_num_procs.gt.1) then
      call mpi_reduce(x_dat, x_out, 1, mpi_real, mpi_max,&
                         root, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_max_reduce_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_max_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_max_reduce_r0

!=======================PPIO_MAX_REDUCE_D0=============================
!  return the maximum of integer x_dat from all pes to root 
!======================================================================
  function ppio_max_reduce_d0(root,x_dat) result(x_out)
    integer, intent(in)         :: root
    double precision,intent(in) :: x_dat
    double precision            :: x_out
    integer                     :: istat
    
    if(ppio_count_flag) &
      ppio_max_reduce_count=ppio_max_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_max_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    x_out=x_dat
    if(pcps_num_procs.gt.1) then
      call mpi_reduce(x_dat, x_out, 1, mpi_double_precision, mpi_max,&
                         root, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_max_reduce_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_max_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_max_reduce_d0

!====================PPIO_MIN_REDUCE_I0================================
!  return the minimum of integer x_dat from all pes to root 
!======================================================================
  function ppio_min_reduce_i0(root,x_dat) result(x_out)
    integer, intent(in) :: root, x_dat
    integer             :: x_out
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_min_reduce_count=ppio_min_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_min_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    x_out=x_dat
    if(pcps_num_procs.gt.1) then
      call mpi_reduce(x_dat, x_out, 1, mpi_integer, mpi_min, &
                         root, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_min_reduce_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_min_reduce_i0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_min_reduce_i0

!========================PPIO_MIN_REDUCE_R0============================
!  return the minimum of integer x_dat from all pes to root 
!======================================================================
  function ppio_min_reduce_r0(root,x_dat) result(x_out)
    integer, intent(in) :: root
    real, intent(in)    :: x_dat
    real                :: x_out
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_min_reduce_count=ppio_min_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_min_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    x_out=x_dat
    if(pcps_num_procs.gt.1) then
      call mpi_reduce(x_dat, x_out, 1, mpi_real, mpi_min, &
                         root, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_min_reduce_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_min_reduce_r0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_min_reduce_r0

!======================PPIO_MIN_REDUCE_D0==============================
!  return the minimum of integer x_dat from all pes to root 
!======================================================================
  function ppio_min_reduce_d0(root,x_dat) result(x_out)
    integer, intent(in)         :: root
    double precision,intent(in) :: x_dat
    double precision            :: x_out
    integer                     :: istat
    
    if(ppio_count_flag) &
      ppio_min_reduce_count=ppio_min_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_min_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_dat
      call pcps_print(pcps_message,2)
    endif

    x_out=x_dat
    if(pcps_num_procs.gt.1) then
      call mpi_reduce(x_dat, x_out, 1, mpi_double_precision, mpi_min, &
                         root, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_min_reduce_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_min_reduce_d0, cpu, val=", &
       pcps_current_worker_num, x_out
      call pcps_print(pcps_message,2)
    endif

    return
  end function ppio_min_reduce_d0

!==========================PPIO_GATHER_C0==============================
! gather scalar character x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_c0(i_pel, x_dat, x_out)
    integer, intent(in)    :: i_pel
    character, intent(in)  :: x_dat 
    character, intent(out) :: x_out(:) 
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_min_all_reduce_count=ppio_min_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_c0, cpu, root, val=", &
        pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1)=x_dat
    else 
      call mpi_gather(x_dat, 1, mpi_character, x_out, 1, mpi_character, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_c0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_c0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_c0 

!===========================PPIO_GATHER_C1=============================
! gather n1_inp 1d characters x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_c1(i_pel, n1_inp, x_dat, x_out)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n1_inp
    character, intent(in)  :: x_dat(:)
    character, intent(out) :: x_out(:,:)
    
    integer                :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_c1, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    else 
      call mpi_gather(x_dat, n1_inp, mpi_character,    &
                      x_out, n1_inp, mpi_character,&
                      i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_c1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_c1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_c1 

!==========================PPIO_GATHER_C2==============================
! gather n1_inp 2d characters x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_c2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n2_inp
    character, intent(in)  :: x_dat(:, :)
    character, intent(out) :: x_out(:, :,:)
    
    integer                :: istat
    integer                :: n0_inp
    integer                :: n1_inp
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_c2, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp,1)=x_dat(1:,:n2_inp)
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_gather(x_dat(1:n1_inp,1:n2_inp), n0_inp, mpi_character, &
                      x_out,                    n0_inp, mpi_character, &
                      i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_c2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_c2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_c2 

!==========================PPIO_GATHER_I0==============================
! gather scalar integer x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_i0(i_pel, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: x_dat 
    integer, intent(out) :: x_out(:) 
    
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_min_all_reduce_count=ppio_min_all_reduce_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_i0, cpu, root, val=", &
        pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1)=x_dat
    else 
      call mpi_gather(x_dat, 1, mpi_integer, x_out, 1, mpi_integer, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_i0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_i0 

!===========================PPIO_GATHER_I1=============================
! gather n1_inp 1d integers x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_i1(i_pel, n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n1_inp
    integer, intent(in)  :: x_dat(:)
    integer, intent(out) :: x_out(:,:)
    
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_i1, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    else 
      call mpi_gather(x_dat, n1_inp, mpi_integer, x_out, n1_inp, mpi_integer, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_i1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_i1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_i1 

!==========================PPIO_GATHER_I2==============================
! gather n1_inp 2d integers x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_i2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n2_inp
    integer, intent(in)  :: x_dat(:, :)
    integer, intent(out) :: x_out(:, :,:)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_i2, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp,1)=x_dat(1:,:n2_inp)
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_gather(x_dat(1:n1_inp,1:n2_inp), n0_inp, mpi_integer, &
                    x_out, n0_inp, mpi_integer, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_i2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_i2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_i2 

!=======================PPIO_GATHER_R0=================================
! gather scalar real x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_r0(i_pel, x_dat, x_out)
    integer, intent(in)  :: i_pel
    real,    intent(in)  :: x_dat 
    real,    intent(out) :: x_out(:) 
    
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_r0, cpu, root, val=", &
        pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1)=x_dat
    else 
      call mpi_gather(x_dat, 1, mpi_real, x_out, 1, mpi_real,    &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_r0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_r0 

!========================PPIO_GATHER_R1================================
! gather n1_inp 1d real x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_r1(i_pel, n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n1_inp
    real,    intent(in)  :: x_dat(:)
    real,    intent(out) :: x_out(:,:)
    
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_r1, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    else 
      call mpi_gather(x_dat, n1_inp, mpi_real, x_out, n1_inp, mpi_real, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_r1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_r1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_r1 

!========================PPIO_GATHER_R2================================
! gather n1_inp 2d real x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_r2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n2_inp
    real,    intent(in)  :: x_dat(:, :)
    real,    intent(out) :: x_out(:, :,:)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_r2, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp,1)=x_dat(1:,:n2_inp)
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_gather(x_dat(1:n1_inp,1:n2_inp), n0_inp, mpi_real, &
                    x_out, n0_inp, mpi_real, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_r2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_r2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_r2 

!=======================PPIO_GATHER_D0=================================
! gather scalar double x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_d0(i_pel, x_dat, x_out)
    integer,          intent(in)  :: i_pel
    double precision, intent(in)  :: x_dat 
    double precision, intent(out) :: x_out(:) 
    
    integer                       :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_d0, cpu, root, val=", &
        pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1)=x_dat
    else 
      call mpi_gather(x_dat, 1, mpi_double_precision,  &
                    x_out, 1, mpi_double_precision, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_d0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_d0 

!=========================PPIO_GATHER_D1===============================
! gather n1_inp 1d double x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_d1(i_pel, n1_inp, x_dat, x_out)
    integer,          intent(in)  :: i_pel
    integer,          intent(in)  :: n1_inp
    double precision, intent(in)  :: x_dat(:)
    double precision, intent(out) :: x_out(:,:)
    
    integer                       :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_d1, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    else 
      call mpi_gather(x_dat, n1_inp, mpi_double_precision, &
                    x_out, n1_inp, mpi_double_precision, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_d1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_d1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_d1 

!=======================PPIO_GATHER_D2=================================
! gather n1_inp 2d double x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_d2(i_pel, n2_inp, x_dat, x_out)
    integer,          intent(in)  :: i_pel
    integer,          intent(in)  :: n2_inp
    double precision, intent(in)  :: x_dat(:, :)
    double precision, intent(out) :: x_out(:, :,:)
    
    integer                       :: istat
    integer                       :: n0_inp
    integer                       :: n1_inp
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_d2, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp,1)=x_dat(1:,:n2_inp)
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_gather(x_dat(1:n1_inp,1:n2_inp), n0_inp, mpi_double_precision, &
                    x_out, n0_inp, mpi_double_precision, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_d2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_d2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_d2 

!========================PPIO_GATHER_Z0================================
! gather scalar complex x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_z0(i_pel, x_dat, x_out)
    integer, intent(in)  :: i_pel
    complex, intent(in)  :: x_dat 
    complex, intent(out) :: x_out(:) 
    
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_z0, cpu, root, val=", &
        pcps_current_worker_num, i_pel, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1)=x_dat
    else
      call mpi_gather(x_dat, 1, mpi_complex, x_out, 1, mpi_complex, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_z0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_z0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_z0 

!=========================PPIO_GATHER_Z1===============================
! gather n1_inp 1d complex  x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_z1(i_pel, n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n1_inp
    complex, intent(in)  :: x_dat(:)
    complex, intent(out) :: x_out(:,:)
    
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_z1, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    else 
      call mpi_gather(x_dat, n1_inp, mpi_complex, x_out, n1_inp, mpi_complex, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_z1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_z1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_z1 

!==========================PPIO_GATHER_Z2==============================
! gather n1_inp 2d complex  x_dat on all pes into x_out in pe i_pel
!======================================================================
  subroutine ppio_gather_z2(i_pel, n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n2_inp
    complex, intent(in)  :: x_dat(:, :)
    complex, intent(out) :: x_out(:, :,:)
    
    integer              :: istat
    integer              :: n0_inp
    integer              :: n1_inp
    
    if(ppio_count_flag) &
      ppio_gather_count=ppio_gather_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_gather_z2, cpu, root, n=", &
        pcps_current_worker_num, i_pel, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.le.1) then
      x_out(1:,1:n2_inp,1)=x_dat(1:,:n2_inp)
    else
      n1_inp = size ( x_dat, 1)
      n0_inp = n1_inp * n2_inp
    
      call mpi_gather(x_dat(1:n1_inp,1:n2_inp), n0_inp, mpi_complex, &
                    x_out, n0_inp, mpi_complex, &
                    i_pel, mpi_comm_world, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_gather_z2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_gather_z2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_gather_z2 

!======================PPIO_SEND_DATA_C0===============================
! send character string x_dat to i_pel using tag
!======================================================================
  subroutine ppio_send_data_c0(i_pel, x_dat, tag)
    integer,          intent(in) :: i_pel, tag
    character(len=*), intent(in) :: x_dat 
    
    integer                      :: istat
    integer                      :: i
    integer                      :: n0_inp
    character(len=1),allocatable :: c0(:)
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_c0,cpu,proc,tag,val=", &
       pcps_current_worker_num, i_pel, tag, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0_inp = len(x_dat)
      allocate(c0(n0_inp),stat=istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in allocate in ppio_send_data_c0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      do i = 1 , n0_inp
        c0(i)=x_dat(i:i)
      enddo 
    
      call mpi_send(c0,n0_inp, mpi_character, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_c0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    
      deallocate(c0,stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_c0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_c0
  
!=====================PPIO_RECEIVE_DATA_C0=============================
! receive character string x_dat to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_c0(i_pel, x_dat, tag)
    integer,          intent(in)  :: i_pel, tag
    character(len=*), intent(out) :: x_dat 
    
    integer                       :: istat
    integer                       :: i
    integer                       :: n0_inp
    integer                       :: status(MPI_STATUS_SIZE)
    character(len=1), allocatable :: c0(:)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_receive_data_c0, cpu, proc, tag=",&
       pcps_current_worker_num, i_pel, tag
      call pcps_print(pcps_message,2)
    endif
 
    if(pcps_num_procs.gt.1) then
      n0_inp = len(x_dat)
      allocate(c0(n0_inp),stat=istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in allocate in ppio_receive_data_c0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      call mpi_recv(c0,n0_inp,mpi_character, i_pel, tag,                       &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_c0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 

      do i = 1 , n0_inp
        x_dat(i:i)=c0(i)
      enddo 
    
      deallocate(c0,stat=istat)
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_c0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_c0

!=========================PPIO_SEND_DATA_C1============================
! send character array x_dat to i_pel using tag
!======================================================================
  subroutine ppio_send_data_c1(i_pel, n1_inp, x_dat, tag)
    integer,   intent(in) :: i_pel, tag, n1_inp
    character, intent(in) :: x_dat(:) 
    
    integer               :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_c1,cpu,proc,tag,val=", &
       pcps_current_worker_num, i_pel, tag, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_c1, n=",n1_inp, &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n1_inp, mpi_character, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_c1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_c1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_c1
  
!========================PPIO_RECEIVE_DATA_C1==========================
! receive character array x_dat to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_c1(i_pel, n1_inp, x_dat, tag)
    integer,    intent(in)  :: i_pel, tag, n1_inp
    character, intent(out) :: x_dat(:) 
    
    integer                :: istat
    integer                :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_receive_data_c1, cpu, proc, tag=",&
       pcps_current_worker_num, i_pel, tag
      call pcps_print(pcps_message,2)
    endif
 
    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_c1, n=",  &
         n1_inp, ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat, n1_inp, mpi_character, i_pel, tag,                  &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_c1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_c1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_c1

!=========================PPIO_SEND_DATA_C2============================
! send character array x_dat to i_pel using tag
!======================================================================
  subroutine ppio_send_data_c2(i_pel, n1_inp, x_dat, tag)
    integer,   intent(in) :: i_pel, tag, n1_inp
    character, intent(in) :: x_dat(:,:)
    
    integer               :: istat, n0_inp
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_c2,cpu,proc,tag,val=", &
       pcps_current_worker_num, i_pel, tag, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0_inp=n1_inp*size(x_dat,dim=1)
      if(n0_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_c2, n=",n1_inp, &
         ", data size=", size(x_dat,2)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n0_inp, mpi_character, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_c2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_c2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_c2
  
!========================PPIO_RECEIVE_DATA_C2==========================
! receive character array x_dat to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_c2(i_pel, n1_inp, x_dat, tag)
    integer,    intent(in)  :: i_pel, tag, n1_inp
    character, intent(out) :: x_dat(:,:)
    
    integer                :: istat, n0_inp
    integer                :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_receive_data_c1, cpu, proc, tag=",&
       pcps_current_worker_num, i_pel, tag
      call pcps_print(pcps_message,2)
    endif
 
    if(pcps_num_procs.gt.1) then
      n0_inp=n1_inp*size(x_dat,dim=1)
      if(n0_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_c2, n=",  &
         n1_inp, ", data size=", size(x_dat,2)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat, n0_inp, mpi_character, i_pel, tag,                  &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_c2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_c2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_c2

!=======================PPIO_SEND_DATA_I0==============================
! send integer x_dat to i_pel using tag
!======================================================================
  subroutine ppio_send_data_i0(i_pel, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: x_dat 
    
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_i0,cpu,proc,tag,val=",&
       pcps_current_worker_num, i_pel, tag, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_send(x_dat, 1, mpi_integer, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_i0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_i0 

!========================PPIO_RECEIVE_DATA_I0==========================
! receive integer x_dat to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_i0(i_pel, x_dat, tag)
    integer, intent(in ) :: i_pel, tag
    integer, intent(out) :: x_dat 
    
    integer              :: status(MPI_STATUS_SIZE)
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_receive_data_i0, cpu, proc, tag=",&
       pcps_current_worker_num, i_pel, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_recv(x_dat,1,mpi_integer, i_pel, tag,                       &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_i0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_i0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_i0 

!======================PPIO_SEND_DATA_I1===============================
! send n1_inp integer values in x_dat to i_pel using tag
!======================================================================
  subroutine ppio_send_data_i1(i_pel, n1_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n1_inp
    integer, intent(in) :: x_dat(:)
    
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_i1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_i1, n=",n1_inp, &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n1_inp, mpi_integer, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_i1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_i1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_i1 

!========================PPIO_RECEIVE_DATA_I1==========================
! receive n1_inp integer values in x_dat to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_i1(i_pel, n1_inp, x_dat, tag)
    integer, intent(in ) :: i_pel, tag
    integer, intent(in ) :: n1_inp
    integer, intent(out) :: x_dat(:)
    
    integer              :: istat
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_i1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_recive_data_i1, n=",&
         n1_inp, ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat,n1_inp,mpi_integer, i_pel, tag,                      &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_i1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_i1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_i1 

!=====================PPIO_SEND_DATA_I2================================
! sends integer values x_dat(:,:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_i2(i_pel, n2_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n2_inp
    integer, intent(in) :: x_dat(:,:)
    
    integer             :: istat
    integer             :: n0
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_i1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size( x_dat,1) * n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_i2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n0, mpi_integer, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
        ") in ppio_send_data_i2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_i2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif
    return
  end subroutine ppio_send_data_i2 

!========================PPIO_RECEIVE_DATA_I2==========================
! receive integer values x_dat(:,:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_i2(i_pel, n2_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n2_inp
    integer, intent(out) :: x_dat(:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_i2, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat,1)* n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_i2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat, n0, mpi_integer,  i_pel, tag,                       &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_i2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_i2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_i2 

!======================PPIO_SEND_DATA_I3===============================
! sends integer values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_i3(i_pel, n3_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n3_inp
    integer, intent(in) :: x_dat(:,:,:)
    
    integer             :: istat
    integer             :: n0
    integer             :: i3
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_i3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_i3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3 = 1, n3_inp
        call mpi_send(x_dat(:,:,i3),n0,mpi_integer,i_pel,tag, &
          MPI_COMM_WORLD,istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_i3, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_i3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_i3 

!=======================PPIO_RECEIVE_DATA_I3===========================
! receives integer values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_i3(i_pel, n3_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n3_inp
    integer, intent(out) :: x_dat(:,:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: i3
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_i3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_i3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3 = 1, n3_inp
        call mpi_recv(x_dat(:,:,i3),n0,mpi_integer, i_pel, tag,       &
          MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_i3, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_i3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_i3 

!=====================PPIO_SEND_DATA_I4================================
! sends integer values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_i4(i_pel, n4_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n4_inp
    integer, intent(in) :: x_dat(:,:,:,:)
    
    integer             :: istat
    integer             :: n0
    integer             :: n3_inp, i3, i4
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_i4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat,3)

      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_i4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1, n4_inp
        do i3 = 1, n3_inp
          call mpi_send(x_dat(:,:,i3,i4),n0,mpi_integer,i_pel,tag, &
           MPI_COMM_WORLD,istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_send_data_i4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_i4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_i4 

!=======================PPIO_RECEIVE_DATA_I4===========================
! receives integer values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_i4(i_pel, n4_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n4_inp
    integer, intent(out) :: x_dat(:,:,:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: n3_inp, i3, i4
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_i4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat, 3)
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_i4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1,n4_inp
        do i3 = 1, n3_inp
          call mpi_recv(x_dat(:,:,i3,i4),n0,mpi_integer, i_pel, tag,       &
            MPI_COMM_WORLD, status, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_receive_data_i4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_i4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_i4 

!================ PPIO_SEND_DATA_R0====================================
! sends real value x_dat to i_pel using tag
!======================================================================
  subroutine ppio_send_data_r0(i_pel, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    real   , intent(in) :: x_dat 
    
    integer             :: istat
   
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_send_data_r0, cpu, proc, tag, val=",&
       pcps_current_worker_num, i_pel, tag, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_send(x_dat, 1, mpi_real   , i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_r0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_r0 

!=====================PPIO_RECEIVE_DATA_R0=============================
! receives real value x_dat to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_r0(i_pel, x_dat, tag)
    integer, intent(in ) :: i_pel, tag
    real   , intent(out) :: x_dat 
    
    integer              :: status(MPI_STATUS_SIZE)
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_receive_data_r0, cpu, proc, tag=",&
       pcps_current_worker_num, i_pel, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_recv(x_dat,1,mpi_real   , i_pel, tag,                       &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_r0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_r0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_r0 

!==================== PPIO_SEND_DATA_R1================================
! sends real values x_dat(:,:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_r1(i_pel, n1_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n1_inp
    real   , intent(in) :: x_dat(:)
    
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_r1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_r1, n=",n1_inp, &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n1_inp, mpi_real   , i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_r1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_r1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_r1 

!=====================PPIO_RECEIVE_DATA_R1=============================
! receives real values x_dat(:,:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_r1(i_pel, n1_inp, x_dat, tag)
    integer, intent(in ) :: i_pel, tag
    integer, intent(in ) :: n1_inp
    real   , intent(out) :: x_dat(:)
    
    integer              :: istat
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_r1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_r1, n=",&
         n1_inp, ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat,n1_inp,mpi_real   , i_pel, tag,                      &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_r1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_r1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_r1 

!====================PPIO_SEND_DATA_R2=================================
! sends real values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_r2(i_pel, n2_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n2_inp
    real   , intent(in) :: x_dat(:,:)
    
    integer             :: istat
    integer             :: n0
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_r2, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size( x_dat,1) * n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_r2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n0, mpi_real   , i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_r2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_r2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_r2 

!====================PPIO_RECEIVE_DATA_R2==============================
! sends real values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_r2(i_pel, n2_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n2_inp
    real   , intent(out) :: x_dat(:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_r2, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat,1)* n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_r2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat, n0, mpi_real,  i_pel, tag,                        &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_r2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_r2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_r2 

!=====================PPIO_SEND_DATA_R3================================
! sends real values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_r3(i_pel, n3_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n3_inp
    real   , intent(in) :: x_dat(:,:,:)
    
    integer             :: istat
    integer             :: n0
    integer             :: i3
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_r3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_r3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3 = 1, n3_inp
        call mpi_send(x_dat(:,:,i3),n0,mpi_real   ,i_pel,tag, &
          MPI_COMM_WORLD,istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_send_data_r3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_r3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_r3 

!================== PPIO_RECEIVE_DATA_R3===============================
! receives real values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_r3(i_pel, n3_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n3_inp
    real   , intent(out) :: x_dat(:,:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: i3
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_r3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_r3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3 = 1, n3_inp
        call mpi_recv(x_dat(:,:,i3),n0,mpi_real   , i_pel, tag,       &
          MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_r3, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_r3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
    
  end subroutine ppio_receive_data_r3 

!====================PPIO_SEND_DATA_R4=================================
! sends real values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_r4(i_pel, n4_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n4_inp
    real,    intent(in) :: x_dat(:,:,:,:)
    
    integer             :: istat
    integer             :: n0
    integer             :: n3_inp, i3, i4
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_r4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat,3)

      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_r4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1, n4_inp
        do i3 = 1, n3_inp
          call mpi_send(x_dat(:,:,i3,i4),n0,mpi_real,i_pel,tag, &
           MPI_COMM_WORLD,istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_send_data_r4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_r4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_r4 

!=======================PPIO_RECEIVE_DATA_R4===========================
! receives real values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_r4(i_pel, n4_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n4_inp
    real,    intent(out) :: x_dat(:,:,:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: n3_inp, i3, i4
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_r4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat, 3)
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_r4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1,n4_inp
        do i3 = 1, n3_inp
          call mpi_recv(x_dat(:,:,i3,i4),n0,mpi_real, i_pel, tag,       &
            MPI_COMM_WORLD, status, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_receive_data_r4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_r4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_r4 

!====================PPIO_SEND_DATA_D0=================================
! sends real values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_d0(i_pel, x_dat, tag)
    integer,          intent(in) :: i_pel, tag
    double precision, intent(in) :: x_dat 
    
    integer                      :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_send_data_d0, cpu, proc, tag, val=",&
       pcps_current_worker_num, i_pel, tag, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_send(x_dat, 1, mpi_double_precision, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_d0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_d0 

!=================PPIO_RECEIVE_DATA_D0=================================
! sends real values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_d0(i_pel, x_dat, tag)
    integer,          intent(in ) :: i_pel, tag
    double precision, intent(out) :: x_dat 
    
    integer                       :: status(MPI_STATUS_SIZE)
    integer                       :: istat
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_receive_data_d0, cpu, proc, tag=",&
       pcps_current_worker_num, i_pel, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_recv(x_dat,1,mpi_double_precision, i_pel, tag,                  &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_d0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_d0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_d0 

!==================PPIO_SEND_DATA_D1===================================
! sends double values x_dat(:n1_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_d1(i_pel, n1_inp, x_dat, tag)
    integer,          intent(in) :: i_pel, tag
    integer,          intent(in) :: n1_inp
    double precision, intent(in) :: x_dat(:)
    
    integer                      :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_d1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_d1, n=",n1_inp, &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n1_inp, mpi_double_precision, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_d1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_d1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_d1 

!==================PPIO_RECEIVE_DATA_D1================================
! receives double values x_dat(:n1_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_d1(i_pel, n1_inp, x_dat, tag)
    integer,          intent(in ) :: i_pel, tag
    integer,          intent(in ) :: n1_inp
    double precision, intent(out) :: x_dat(:)
    
    integer                       :: istat
    integer                       :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_d1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_d1, n=", &
         n1_inp, ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat,n1_inp,mpi_double_precision, i_pel, tag,             &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_d1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_d1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_d1 

!========================PPIO_SEND_DATA_D2=============================
! sends double values x_dat(:,:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_d2(i_pel, n2_inp, x_dat, tag)
    integer,          intent(in) :: i_pel, tag
    integer,          intent(in) :: n2_inp
    double precision, intent(in) :: x_dat(:,:)
    
    integer                      :: istat
    integer                      :: n0
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_d2, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size( x_dat,1) * n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_d2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n0, mpi_double_precision, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_d2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_d2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_d2 

!=====================PPIO_RECEIVE_DATA_D2=============================
! sends double values x_dat(:,:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_d2(i_pel, n2_inp, x_dat, tag)
    integer,          intent(in)  :: i_pel, tag
    integer,          intent(in)  :: n2_inp
    double precision, intent(out) :: x_dat(:,:)
    
    integer                       :: istat
    integer                       :: n0
    integer                       :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_d2, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat,1)* n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_d2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat, n0, mpi_double_precision,  i_pel, tag,              &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_d2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_d2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_d2 

!====================== PPIO_SEND_DATA_D3==============================
! sends double values x_dat(:,:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_d3(i_pel, n3_inp, x_dat, tag)
    integer,          intent(in) :: i_pel, tag
    integer,          intent(in) :: n3_inp
    double precision, intent(in) :: x_dat(:,:,:)
    
    integer                      :: istat
    integer                      :: n0
    integer                      :: i3
   
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_d3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
  
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_d3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

       do i3 = 1, n3_inp
        call mpi_send(x_dat(:,:,i3),n0,mpi_double_precision,i_pel,tag, &
          MPI_COMM_WORLD,istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_send_data_d3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_d3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_d3 

!=========================PPIO_RECEIVE_DATA_D3=========================
! receives double values x_dat(:,:,:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_d3(i_pel, n3_inp, x_dat, tag)
    integer,          intent(in)  :: i_pel, tag
    integer,          intent(in)  :: n3_inp
    double precision, intent(out) :: x_dat(:,:,:)
    
    integer                       :: istat, n0, i3
    integer                       :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_d3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_d3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3 = 1, n3_inp
        call mpi_recv(x_dat(:,:,i3),n0,mpi_double_precision, i_pel, tag,       &
          MPI_COMM_WORLD, status, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
          ") in ppio_receive_data_d3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_d3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_d3 

!=======================PPIO_SEND_DATA_D4=============================
! sends double values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_d4(i_pel, n4_inp, x_dat, tag)
    integer,          intent(in) :: i_pel, tag
    integer,          intent(in) :: n4_inp
    double precision, intent(in) :: x_dat(:,:,:,:)
    
    integer             :: istat
    integer             :: n0
    integer             :: n3_inp, i3, i4
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_d4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat,3)

      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_d4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1, n4_inp
        do i3 = 1, n3_inp
          call mpi_send(x_dat(:,:,i3,i4),n0,mpi_double_precision,i_pel,tag, &
           MPI_COMM_WORLD,istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_send_data_d4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_d4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_d4 

!===========================PPIO_RECEIVE_DATA_D4=======================
! receives double values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_d4(i_pel, n4_inp, x_dat, tag)
    integer,             intent(in)  :: i_pel, tag
    integer,             intent(in)  :: n4_inp
    double precision,    intent(out) :: x_dat(:,:,:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: n3_inp, i3, i4
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_d4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat, 3)
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_d4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1,n4_inp
        do i3 = 1, n3_inp
          call mpi_recv(x_dat(:,:,i3,i4),n0,mpi_double_precision, i_pel, tag,  &
            MPI_COMM_WORLD, status, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_receive_data_d4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_d4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_d4 

!====================PPIO_SEND_DATA_Z0=================================
! send complex value x_dat to i_pel using tag
!======================================================================
  subroutine ppio_send_data_z0(i_pel, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    complex, intent(in) :: x_dat 
    
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_send_data_z0, cpu, proc, tag, val=",&
       pcps_current_worker_num, i_pel, tag, x_dat
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_send(x_dat, 1, mpi_complex, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_z0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_z0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_z0 

!======================PPIO_RECEIVE_DATA_Z0============================
! receives complex value x_dat to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_z0(i_pel, x_dat, tag)
    integer, intent(in ) :: i_pel, tag
    complex, intent(out) :: x_dat 
    
    integer              :: status(MPI_STATUS_SIZE)
    integer              :: istat
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_receive_data_z0, cpu, proc, tag=",&
       pcps_current_worker_num, i_pel, tag
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      call mpi_recv(x_dat,1,mpi_complex, i_pel, tag,                       &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_z0, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_z0, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_z0 

!=======================PPIO_SEND_DATA_Z1==============================
! send complex values x_dat(1:n1_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_z1(i_pel, n1_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n1_inp
    complex, intent(in) :: x_dat(:)
    
    integer             :: istat
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_z1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_z1, n=",n1_inp, &
         ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n1_inp, mpi_complex, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_z1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_z1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_z1 

!=====================PPIO_RECEIVE_DATA_Z1=============================
! receives complex values x_dat(1:n1_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_z1(i_pel, n1_inp, x_dat, tag)
    integer, intent(in ) :: i_pel, tag
    integer, intent(in ) :: n1_inp
    complex, intent(out) :: x_dat(:)
    
    integer              :: istat
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_z1, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n1_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      if(n1_inp.gt.size(x_dat)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_z1, n=", &
         n1_inp, ", data size=", size(x_dat)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat,n1_inp,mpi_complex, i_pel, tag,                      &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_z1, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_z1, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_z1 

!======================PPIO_SEND_DATA_Z2===============================
! sends complex values x_dat(:,1:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_z2(i_pel, n2_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n2_inp
    complex, intent(in) :: x_dat(:,:)
    
    integer             :: istat
    integer             :: n0
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_z2, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size( x_dat,1) * n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_z2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_send(x_dat, n0, mpi_complex, i_pel, tag, &
        MPI_COMM_WORLD, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_send_data_z2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_z2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_z2 

!====================PPIO_RECEIVE_DATA_Z2==============================
! receives complex values x_dat(:,1:n2_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_z2(i_pel, n2_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n2_inp
    complex, intent(out) :: x_dat(:,:)
    
    integer              :: istat, n0
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_z2, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n2_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat,1)* n2_inp
    
      if(n2_inp.gt.size(x_dat,dim=2)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_z2, n=",  &
         n2_inp, ", data size=", size(x_dat,dim=2)
        call pcps_abort(pcps_message)
      endif

      call mpi_recv(x_dat, n0, mpi_complex, i_pel, tag,                        &
       MPI_COMM_WORLD, status, istat)
      if(istat.ne.0) then
        write(pcps_message,*) "Error(",istat,  &
         ") in ppio_receive_data_z2, cpu=",pcps_current_worker_num
        call pcps_abort(pcps_message)
      endif 
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_z2, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_z2 

!=====================PPIO_SEND_DATA_Z3================================
! send complex values x_dat(:,:,1:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_z3(i_pel, n3_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n3_inp
    complex, intent(in) :: x_dat(:,:,:)
    
    integer             :: istat
    integer             :: n0
    integer             :: i3
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_z3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_i3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3 = 1, n3_inp
        call mpi_send(x_dat(:,:,i3),n0,mpi_complex,i_pel,tag, &
          MPI_COMM_WORLD,istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_send_data_z3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_z3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_z3 

!========================PPIO_RECEIVE_DATA_Z3==========================
! receives complex values x_dat(:,:,1:n3_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_z3(i_pel, n3_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n3_inp
    complex, intent(out) :: x_dat(:,:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: i3
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
       "PPIO Entry:ppio_receive_data_z3, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
    
      if(n3_inp.gt.size(x_dat,dim=3)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_z3, n=",  &
         n3_inp, ", data size=", size(x_dat,dim=3)
        call pcps_abort(pcps_message)
      endif

      do i3 = 1, n3_inp
        call mpi_recv(x_dat(:,:,i3),n0,mpi_complex, i_pel, tag,       &
          MPI_COMM_WORLD, status, istat)
        if(istat.ne.0) then
          write(pcps_message,*) "Error(",istat,  &
           ") in ppio_receive_data_z3, cpu=",pcps_current_worker_num
          call pcps_abort(pcps_message)
        endif 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_z3, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_z3 

!===================PPIO_SEND_DATA_Z4==================================
! sends complex values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_send_data_z4(i_pel, n4_inp, x_dat, tag)
    integer, intent(in) :: i_pel, tag
    integer, intent(in) :: n4_inp
    complex, intent(in) :: x_dat(:,:,:,:)
    
    integer             :: istat
    integer             :: n0
    integer             :: n3_inp, i3, i4
    
    if(ppio_count_flag) &
      ppio_send_count=ppio_send_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) "PPIO Entry:ppio_send_data_z4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size (x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat,3)

      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_send_data_z4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1, n4_inp
        do i3 = 1, n3_inp
          call mpi_send(x_dat(:,:,i3,i4),n0,mpi_complex,i_pel,tag, &
           MPI_COMM_WORLD,istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_send_data_z4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_send_data_z4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_send_data_z4 

!=================== PPIO_RECEIVE_DATA_Z4==============================
! receives real values x_dat(:,:,:,:n4_inp) to i_pel using tag
!======================================================================
  subroutine ppio_receive_data_z4(i_pel, n4_inp, x_dat, tag)
    integer, intent(in)  :: i_pel, tag
    integer, intent(in)  :: n4_inp
    complex, intent(out) :: x_dat(:,:,:,:)
    
    integer              :: istat
    integer              :: n0
    integer              :: n3_inp, i3, i4
    integer              :: status(MPI_STATUS_SIZE)
    
    if(ppio_count_flag) &
      ppio_receive_count=ppio_receive_count+1
    
    if(iand(ppio_debug,PPIO_DEBUG_ENTRY).ne.0) then
      write(pcps_message,*) &
      "PPIO Entry:ppio_receive_data_z4, cpu, proc, tag, n=",&
       pcps_current_worker_num, i_pel, tag, n3_inp
      call pcps_print(pcps_message,2)
    endif

    if(pcps_num_procs.gt.1) then
      n0 = size(x_dat, 1)* size (x_dat, 2)
      n3_inp=size(x_dat, 3)
    
      if(n4_inp.gt.size(x_dat,dim=4)) then
        write(pcps_message,*) "Data Overflow in ppio_receive_data_z4, n=",  &
         n4_inp, ", data size=", size(x_dat,dim=4)
        call pcps_abort(pcps_message)
      endif

      do i4=1,n4_inp
        do i3 = 1, n3_inp
          call mpi_recv(x_dat(:,:,i3,i4),n0,mpi_complex, i_pel, tag,       &
            MPI_COMM_WORLD, status, istat)
          if(istat.ne.0) then
            write(pcps_message,*) "Error(",istat,  &
             ") in ppio_receive_data_z4, cpu=",pcps_current_worker_num
            call pcps_abort(pcps_message)
          endif
        enddo 
      enddo
    endif

    if(iand(ppio_debug,PPIO_DEBUG_EXIT).ne.0) then
      write(pcps_message,*) &
       "PPIO Exit:ppio_receive_data_z4, cpu=", pcps_current_worker_num
      call pcps_print(pcps_message,2)
    endif

    return
  end subroutine ppio_receive_data_z4 


!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!

end module ppio_module

!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
