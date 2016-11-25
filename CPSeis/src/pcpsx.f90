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
! Name       : pcpsx
! Category   : main_prog
! Written    : 2000-12-20   by: Charles C. Burch
! Revised    : 2007-12-04   by: Bill Menger
! Maturity   : beta
! Purpose    : Helper routines for the PCPS main program for parallel processing
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!  This primitive contains a collection of routines which are called from
!  the main program which is used for parallel processing.  This main program
!  is built by the job builder.  This primitive encapsulates much of the
!  code which would otherwise have to be built into the main program.  The
!  main program therefore contains only that code which changes from job
!  to job and is dependent on the list of process modules used in the job.
!
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
! No CPS globals are changed
!
!-------------------------------------------------------------------------------
!</global_doc>
 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
!  Header words 1,3 and 4 can be changed through user specified options.
!
!  Scratch header words 61 and 62 are used to temporarily store information
!  when traces are send and received between the boss and workers.
!
!-------------------------------------------------------------------------------
!</header_word_doc>
 
!<calling_doc>
!-------------------------------------------------------------------------------
! pcpsx_init_processing()
! This subroutine is used to initialize parallel processing
!-------------------------------------------------------------------------------
! pcpsx_finish_parallel()
! This subroutine is used to finalize parallel environment
!-------------------------------------------------------------------------------
! subroutine pcpsx_abort(mess, err)
! Make best attenpt to have all cpus abort the job
!   character(len=*),  intent(in) :: mess
!   integer, optional, intent(in) :: err
!-------------------------------------------------------------------------------
! pcpsx_wrapup_processing()
! This subroutine is used to terminate the  parallel processing
!  of traces and to enter the close-down of the parallel executable to stop
!-------------------------------------------------------------------------------
! pcpsx_finish_processing(error)
! This subroutine is used to terminate all  parallel aspects of
! an executable so it can stop
!
! The arguments of this subroutine are
!   logical, intent(out)  :: error ! true if an error occurs
!-------------------------------------------------------------------------------
! pcpsx_restart_processing()
!  Initialize variables for start of a processing loop
!-------------------------------------------------------------------------------
! pcpsx_do_parallel_create(obj, error)
! This subroutine is used to create an objects for a parallelization loop.
!
!  The arguments of this subroutine are:
!   type(pcpsx_do_parallel_struct), pointer, intent(out) :: obj 
!     obj is created in subroutine and initialized to default values.
!   logical, intent(out)  :: error ! true if an error occurs
!-------------------------------------------------------------------------------
! pcpsx_do_parallel_init(obj, error)
! This subroutine is used to initialize variables in a || control
!   object with variables set through variable pcpsx_set_xx_mode calls
!
! The arguments of this subroutine are:
!   type(pcpsx_do_parallel_struct), pointer, intent(inout) :: obj 
!      || control object
!   logical, intent(out)  :: error ! true zero if an error occurs
!-------------------------------------------------------------------------------
! pcpsx_start_loop(obj, ntr)
! This subroutine is used at the start of a main seismic trace
!  processing loop.  It sets ntr to an appropriate value.
!
! The arguments of this subroutine are:
!   type(pcpsx_do_parallel_struct), pointer, intent(out) :: obj
!      obj contains variables related to a processing loop.
!   integer, intent(out)                    :: ntr !NTR to be used as starting
!                                                    value in parallel loop
!-------------------------------------------------------------------------------
! pcpsx_end_loop(obj, error)
! This subroutine is used at the end of a main seismic trace
! processing loop.  It sets ntr to an appropriate value to exit the loop.
! loop back to last loop to get more data or to the top of the loop to input
! more data
!
! The arguments of this subroutine are:
!   type(pcpsx_do_parallel_struct), pointer, intent(out) :: obj
!    obj containing variables related to a parallel loop.
!   integer, intent(out)  :: ntr !NTR to be used as starting
!   logical, intent(out)  :: error ! true if an error occurs
!-------------------------------------------------------------------------------
! pcpsx_do_parallel_begin(obj, ntr, hdrs_in, trcs_in, hdrs_out, trcs_out)
!  This subroutine is used at the beginning of a parallel loop.
!
!
!  The arguments of this subroutine are:
!    type(pcpsx_do_parallel_struct), pointer, intent(out) :: obj
!       obj contains variables related to a parallel loop.
!    integer, intent(out)                    :: ntr !#traces/loop control flag
!    double precision, intent(in)            :: hdrs_in(:,:) !input to || loop
!    real, intent(in)                        :: trcs_in(:,:) !input to || loop
!    double precision, intent(inout)         :: hdrs_out(:,:)!output of || loop
!    real, intent(inout)                     :: trcs_out(:,:)!output of || loop
!
!  Note arrays hdrs_in, hdrs_out, trcs_in, trcs_out must be different
!-------------------------------------------------------------------------------
! pcpsx_do_parallel_end(obj, ntr, hdrs, trcs, hdrs_2, trcs_2)
! This subroutine is used to end a parallelization loop
!
! The arguments of this subroutine are:
!   type(pcpsx_do_parallel_struct), pointer, intent(out) :: obj 
!      || control object
!   integer, intent (inout) :: ntr !number traces or loop control flag
!    --hdrs and trcs are for output of parallelization loop and needs to be
!     the same as the last two arguments of the last do_begin call
!   double precision, intent(inout)  :: hdrs(:,:)
!   real, intent(inout)              :: trcs(:,:)
!    -- hdrs_2 and trcs_2 are optional arguments--they are used when the
!    natural logic of the processes in the parallelized loop are such
!    that the last output headers/traces can not e in tha hdrs/trcs array.
!   In this case include the arrays where the output hdr/trcs are as these
!    optional arguments
!   double precision, intent(in), optional :: hdrs_2(:,:)
!   real, intent(in), optional             :: trcs_2(:,:)
!-------------------------------------------------------------------------------
! pcpsx_filter_ntr(ntr)
!  This subroutine is used to convert any PCPS-specfic NTRs to traditional NTRs
!
!  The arguments of this subroutine are:
!    integer, intent(inout)    :: ntr       !#traces or loop control flag
!-------------------------------------------------------------------------------
! pcpsx_do_parallel_dump
! This subroutine is used to dump variables in the || control object
! This is for diagnostic use only and dumps selected variables which
!   are added to as needed
!
! The arguments of this subroutine are:
!   type(pcpsx_do_parallel_struct), pointer, intent(out) :: obj
!      obj contains || control variables
!-------------------------------------------------------------------------------
! pcpsx_do_parallel_delete(obj)
! This subroutine is used to delete a parallelization control object
!
! The arguments of this subroutine are:
!   type(pcpsx_do_parallel_struct), pointer, intent(inout) :: obj 
!     || control object
!-------------------------------------------------------------------------------
! pcpsx_debug_print(title, ntr, hdrs, trcs)
! This subroutine is used for debugging parallel jobs
!  It prints out the title, the value of ntr 
!   and the first 4 hdr values plus the min and max trace value of ntr traces
!
! The arguments of this subroutine are:
!   character (len=*), intent(in) :: title
!   integer          , intent(in) :: ntr
!   double precision , intent(in) :: hdrs(:,:)
!   real              ,intent(in) :: trcs(:,:)
!-------------------------------------------------------------------------------
! subroutine pcpsx_check_worker_errors(error)
!  sync errors from boss and workers 
!
! The argument error can be integer or logical 
!-------------------------------------------------------------------------------
! subroutine pcpsx_sync_workers(ierr)
! Basic parallel_processing communication
!  idle each worker until they get to this point
!  returns ierr (0 if no error)
!
!  integer,intent(out)      :: ierr
!-------------------------------------------------------------------------------
! subroutine pcpsx_sync_no_boss(ierr)
! Basic parallel_processing communication
!  idle each worker (but not the boss) until they get to this point
!  returns ierr (0 if no error)
!
!  integer,intent(out)      :: ierr
!-------------------------------------------------------------------------------
! subroutine pcpsx_broadcast(root,x)-scalar x
!            pcpsx_broadcast(root,n,x)-vector,array,3d
!  broadcasts x from pe root to x in all pe-s 
!  x can be character string or numeric
!  numerics can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of x
!-------------------------------------------------------------------------------
! subroutine pcpsx_sum_reduce(root,xin,xout)-scalar
!            pcpsx_sum_reduce(root,n,xin,xout)-vector,array, 3d
!  sums x_in from all pe-s and returns result in x_out in pe root
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! subroutine pcpsx_sum_all_reduce(xin,xout)
!            pcpsx_sum_all_reduce(n,xin,xout)-vector,array, 3d
!  sums x_in from all pe-s and returns result in x_out in all pe-s
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! function pcpsx_min_reduce(root, x)
!   returns the minimum value of x on all pe-s(value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_max_reduce(root, x)
!   returns the maximum value of x on all pe-s(value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_min_all_reduce(x)
!   returns the minimum value of x on all pe-s(value available on all pe-s)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_max_all_reduce(x)
!   returns the maximum value of x on all pe-s(value available on all pe-s)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
!  pcpsx_send_data(i_pel,x,tag)  scalar
!  pcpsx_send_data(i_pel,n,x,tag)1,2,3 d-data says do 1:n of last subscript
!    send x to pe i_pel using tag
!  x can be character(scalar only), integer, real, double or complex
!-------------------------------------------------------------------------------
!  pcpsx_receive_data(i_pel,x,tag)  scalar
!  pcpsx_recieve_data(i_pel,n,x,tag)1,2,3 d-data says do 1:n of last subscript
!    receives x from pe i_pel using tag
!  x can be character(scalar only), integer, real, double or complex
!-------------------------------------------------------------------------------
!  pcpsx_gather(i_pel,x_in, x_out)  scalar
!  pcpsx_gather(i_pel,n,x_in,x_out)1,2 d-data 
!   n says do 1:n of last subscript
!   x_out one more dimension than x_in
!     gather x_in from each pe and place in xout in i_pel
!  x can be integer, real, double or complex
!-------------------------------------------------------------------------------
! functions pcpsx_i_pel and pcpsx_n_pel are provided for backward compatibility
!  with migration codes
!  pcpsx_i_pel() returns current cpu number--same as pcps_get_worker_num()
!  pcpsx_n_pel() returns number of cpus --same as pcps_get_num_workers()
!-------------------------------------------------------------------------------
! subroutine pcpsx_broadcast_group(root,n_workers, workers,x)-scalar x
!            pcpsx_broadcast_group(root,n_workers, workers, n,x)-vector,array,3d
!  broadcasts x from pe root to x in workers(1:n_workers)
!  x can be character string or numeric
!  numerics can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of x
!-------------------------------------------------------------------------------
! subroutine pcpsx_sum_reduce_group(root,n_workers, workers,xin,xout)-scalar
!    pcpsx_sum_reduce_group(root,n_workers, workers,n,xin,xout)-vector,array, 3d
!  sums x_in from workers(1:n_workers) and returns result in x_out in pe root
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! subroutine pcpsx_sum_all_reduce_group(n_workers, workers,xin,xout)
!     pcpsx_sum_all_reduce_group(n_workers, workers,n,xin,xout)-vector,array, 3d
!  sums x_in from workers(1:n_workers)all pe-s and 
!    returns value available to workers(1:n_workers)
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! function pcpsx_min_reduce_group(root, n_workers, workers,x)
!   returns the minimum value of x on workers(1:n_workers)
!     (value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_max_reduce_group(root, n_workers, workers,x)
!   returns the maximum value of x on workers(1:n_workers)
!    (value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_min_all_reduce_group(n_workers, workers,x)
!   returns the minimum value of x on workers(1:n_workers)
!    (value available on workers(1:n_workers)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_max_all_reduce_group(n_workers, workers,x)
!   returns the maximum value of x on workers(1:n_workers)
!    (value available on workers(1:n_workers)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! subroutine pcpsx_sum_reduce_group(root,n_workers, workers,xin,xout)-scalar
!    pcpsx_sum_reduce_group(root,n_workers, workers,n,xin,xout)-vector,array, 3d
!  sums x_in from workers(1:n_workers)all pe-s  and
!    returns result in x_out in iworkers(1:n_workers)
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! subroutine pcpsx_sum_all_reduce_group(n_workers, workers,xin,xout)
!     pcpsx_sum_all_reduce_group(n_workers, workers,n,xin,xout)-vector,array, 3d
!  sums x_in from workers(1:n_workers)all pe-s and 
!    returns result in x_out in iworkers(1:n_workers)
!  xin/xout can scalar, vector, array or 3d  and 
!    integer, real, double_precision, complex
!  n tells how many of the last index to use of xin/xout
!-------------------------------------------------------------------------------
! function pcpsx_min_reduce_group(root, n_workers, workers,x)
!   returns the minimum value of x on workers(1:n_workers)
!    (value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_max_reduce_group(root, n_workers, workers,x)
!   returns the maximum value of x on workers(1:n_workers)
!    (value available on root pe)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_min_all_reduce_group(n_workers, workers,x)
!   returns the minimum value of x on workers(1:n_workers)
!      (value available on workers(1:n_workers)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
! function pcpsx_max_all_reduce_group(n_workers, workers,x)
!   returns the maximum value of x on workers(1:n_workers)
!      (value available on workers(1:n_workers)
!   x can be integer, real or double precision
!-------------------------------------------------------------------------------
!  pcpsx_gather_group(i_pel,n_workers, workers, x_in, x_out)  scalar
!  pcpsx_gather_group(i_pel,n,x_in,x_out)1,2 d-data 
!   n says do 1:n of last subscript
!   x_out one more dimension than x_in
!     gather x_in from workers(1:n_workers) and place in xout in i_pel
!  x can be integer, real, double or complex
!-------------------------------------------------------------------------------
! subroutine pcpsx_set_work_group_size(obj, size)
! set up work groups based on a given minimum work group size   
!   type (pcpsx_do_parallel_struct),pointer :: obj
!   integer, intent(in)                     :: size
!-------------------------------------------------------------------------------
! subroutine pcpsx_set_number_work_groups(obj, n)
! set up work groups based on a given number of work groups
!   type (pcpsx_do_parallel_struct),pointer :: obj
!   integer, intent(in)                     :: n
!-------------------------------------------------------------------------------
! subroutine pcpsx_get_work_group_info(instance, work_group_num, first_worker, &
!   num_workers)
! get starting worker index and the number of workers in a given
!  work group number within a parallel do-group
!   integer, intent(in)                     :: instance
!   integer, intent(in)                     :: work_group_num
!   integer, intent(out)                    :: first_worker
!   integer, intent(out)                    :: num_workers
! Note if work_group_num is invalid, first_worker set to -1 and num_workers to 0
!-------------------------------------------------------------------------------
! subroutine pcpsx_get_work_group_sizes(obj, num_work_groups, min_size)
! get number work groups and minimum work group size
!   type (pcpsx_do_parallel_struct),pointer :: obj
!   integer, intent(out)                    :: num_work_groups
!   integer, intent(out)                    :: min_size
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
! 19. 2007-12-04  Bill Menger  Initialized trace, header for ifort compiler.
! 18. 2006-09-11  Goodger      Remove unused variables.
! 17. 2005-01-31  Stoeckley    Add pcpsx_sync_no_boss; fix bug whereby the
!                               boss was not executing update code after
!                               pc_do_not_process_traces() when
!                               pcps_boss_exec_mode == PCPS_BOSS_EXECS.
! 16. 2004-09-01  Bill Menger  Changed temporary "bunch" file to always reserve
!                              space and added error checking on all file oper-
!                              ations.  Set extent size to 100Mbytes for file.
! 15. 2004-03-15  C C Burch    Added SEND_LINE option
! 14. 2003-09-04  Burch CC     Fix pointer bug in pcpsx_do_parallel_delete
! 13. 2002-10-23  Burch CC     Fix NEED_TRACE condition with BOSS_EXECS mode.
! 12. 2002-08-12  Vunderink    Monitor memory usage using a separate pthread.
! 11. 2002-05-06  Burch CC     Added worker group support, pcpsx-abort
!                              Changed to storing info on all parallel groups
!                              to support multiple parallel groups
! 10. 2001-12-10  Burch CC     Removed task data support not being used
!                              Changed bunch group to not use header word 3
!                              Added resequence groups option
!                              Removed a "barrier" in BOSS_EXEC mode
! 9.  2001-07-13  Burch CC     Added support for BOSS_EXECS_GATHER/MERGE
!                 Vunderink    Added support for setting pcps_online_print_mode
!                                in pcpsx_finish_setups PROD.
! 8.  2001-05-18  Burch CC     Simplified bunch logic, added stat debug option
!                              Added support for PCPS_RECEIVE_MERGE mode
!                              Added support for pcps_online_print_mode
!                              added 4d interface of broadcast, sum_reduce,
!                               sum_all_reduce, send and receive
! 7.  2001-04-09  Vunderink    Added OS versions and cpu speeds to parallel
!                                stats
! 6.  2001-04-05  Burch CC     Changes PCPS_BOSS_SENDS_DATA to PCPS_BOSS_EXECS
!                              Added pcpsx_i_pel and pcpsx_n_pel
!                              Added pcpsx_syn_workers, pcpsx_broadcast,
!                              pcpsx_sum_reduce, pcpsx_sum_all_reduce,
!                              pcpsx_min_all_reduce, pcpsx_max_all_reduce and
!                              pcpsx_custom_rcs_ident
! 5.  2001-03-26  Burch CC     Added PCPS_BOSS_SENDS_DATA support
!                              Added pcpsx_check_worker_errors
!                              Forced cpstemp small files to not commit space
! 4.  2001-03-15  Burch CC     Added task handling support
!                              Provide improved load balancing
! 3.  2001-02-05  Burch CC     Added support for PCPS_RECEIVE_ALL_EOF
!                              Modified loopback/filter_ntr handling
!                              Use cpstemp for bunch work files
! 2.  2001-01-10  Burch CC     Added pcpsx_debug_print / fixed Solaris warning
! 1.  2000-12-20  Burch CC     Initial version
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
 
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
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
 
module pcpsx_module
  use cio_module
  use cps_module
  use getlun_module
  use getsys_module
  use named_constants_module
  use pc_module
  use pcps_module
  use ppio_module
  use sizeof_module
  use string_module
  use timer_module
  use mth_module
  use unix_module
 
  implicit none

  private

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

  type,public            :: pcpsx_do_parallel_struct
    integer              :: instance          !index of parallel group 
  end type pcpsx_do_parallel_struct

  type, public           :: pcpsx_parallel_group_struct
    double precision     :: merge_int_1 !merge sort information
    double precision     :: merge_int_2 
    double precision     :: merge_int_3
    double precision     :: merge_inc_1
    double precision     :: merge_inc_2 
    double precision     :: merge_inc_3
    integer              :: merge_hdr_1
    integer              :: merge_hdr_2 
    integer              :: merge_hdr_3  

    integer              :: instance          !index of parallel group
    integer, pointer     :: active_workers(:) !contains active worker_nos
    integer              :: do_eof_state      !0-no eof, 1-eof on input
    integer              :: enddo_eof_state   !0-no eof, 1-eof on input
    integer              :: tr_len_in         !trace length in at do_parallel
    integer              :: hdr_len_in        !header length in at do_parallel
    integer              :: max_ntr_in        !max ntr in allowed
    integer              :: tr_len_out        !trace length at do_parallel_end
    integer              :: hdr_len_out       !header length at do_parallel_end
    integer              :: max_ntr_out       !max ntr allowed at do_par_end
    integer              :: ntr_save          !save ntr foruse w/do_parallel_end
    integer              :: work_group_size0  !minimum work group size
    integer              :: work_group_size1  !number workers for overflow
    integer              :: num_work_groups   !number of work groups
    integer              :: boss_exec_mode    !dist,exec,exec/dist
    integer              :: send_mode         !seq,any,all
    integer              :: alt_send_mode     !Post EOF send mode
    integer              :: receive_mode      !pass,sum,gather,
    integer              :: alt_receive_mode  !Post EOF receive mode
    integer              :: eof_mode          !send eof to 1 worker, to all
    integer              :: generator_mode    !no, yes trace generator procs
    integer              :: alt_generator_mode!Post EOF generator mode
    integer              :: resequence_mode   !-1=no reseq, >=0 resequence
    integer              :: resequence_groups !-1=no reseq, >=0 reseq groups
    integer              :: bunch_mode        !no bunch,traces,groups
    integer              :: alt_bunch_mode    !Post eof bunch mode
    integer              :: sort_mode         !-no sort, 1-yes
    integer              :: entry_mode_beg    ! enter from top/below
    integer              :: entry_mode_end    ! enter from top/below
 
    integer              :: bunch_flnm        !ifile of bunch file
    integer              :: bunch_size        !size sort buffer
    integer              :: group_num         !trace group number
    integer              :: beg_indx          !starting sort index
    integer              :: seq_num           !expected trace/ensemble#
    integer              :: bunch_nkeys       !# of active keys
    integer              :: bunch_keys_size   !size of keys
    integer,     pointer :: keys(:,:)         !trace disk keys
    integer              :: ntr_lb            !line breaks ntr work space
    integer              :: hdr_lb            !header word for line breaks
    double precision, pointer :: sort_hdrs(:,:)!space to store merge hdrs
    real        ,pointer      :: sort_trcs(:,:)!space to store merge trcs
    double precision, pointer :: hdrs_lb(:,:) !work space for line breaks
    real        ,pointer      :: trcs_lb(:,:) !work space for line breaks
    integer              :: group_in          !input group sequence#
    integer              :: group_out         !output group seq#
    integer              :: group_seq         !current group seq# in grp
    integer              :: work_group_in     !input  workgroup when needed
    integer              :: work_group_out    !output workgroup when needed
    integer              :: work_group_buff   !buffer workgroup when needed
    integer              :: line_number       !for checking for line breaks
    integer,     pointer :: worker_groups(:,:)!worker group info
  end type pcpsx_parallel_group_struct

!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!
 
  type(pcpsx_parallel_group_struct), pointer :: instances_info(:)
  integer, public        :: pcpsx_num_par_groups     !number parallel instances
  integer, public        :: pcpsx_last_group         !group last completed
  integer, public        :: pcpsx_current_instance   !current instance
  integer, public        :: pcpsx_active_instance    !active instance data sent
  integer                :: pcpsx_active_position    !active buffer position
  integer, public        :: pcpsx_active_sender      !sender to receive from
  integer, public        :: pcpsx_active_entry_num   !active buffer entry num
  integer, public        :: pcpsx_last_sender        !worker last sent data
  integer, pointer       :: pcpsx_workers_vec(:)     !used for work groups
  integer                :: pcpsx_num_active_workers !num of active workers
  integer, pointer       :: pcpsx_buffer_counts(:)   !buffer instance counts
  integer, pointer       :: pcpsx_active_workers(:)  !Active workers array 
  integer, pointer       :: pcpsx_worker_modes(:,:)  !Worker status array -
!  contains the instance/trace group# each worker is active with, 0-not busy

  integer, public        :: pcpsx_trace_mode=0       !>0 tracks trace actions
  double precision,public:: pcpsx_start_time         !use with unix_wtime
  integer, public        :: pcpsx_debug=0            !debug prints(0=none)
! ---------- debug bits(powers of 2), other to be added as needed ----------
  integer,parameter,public :: PCPSX_DEBUG_BOSS_ONLY    =1
  integer,parameter,public :: PCPSX_DEBUG_DO_PARALLEL  =2
  integer,parameter,public :: PCPSX_DEBUG_SEND_RECEIVE =4
  integer,parameter,public :: PCPSX_DEBUG_PRE_PROCESS  =8

! Codes to indicate how do_parallel_end was entered
  integer, parameter       :: ENTRY_FROM_TOP    =1  ! enter from loop back
  integer, parameter       :: ENTRY_FROM_BELOW  =2  ! entry from parallel group
  
! defined in named constants are NO_MORE_TRACES, FATAL_ERROR and NEED_TRACES 
  integer,parameter,public :: LOOP_BACK         =-3 !code-need traces-loop back
  integer,parameter,public :: NULL_ACTION       =-4 !code-workers ignore traces
  integer,parameter,public :: GO_BACK           =-5 !code-`go to start of loop
  integer,parameter,public :: NO_MORE_TASKS     =-8 !out of tasks (APPARENTLY NOT USED?)
  integer,parameter,public :: SHUT_DOWN         =-9 !code-shut down workers
  integer,parameter        :: NTR_DONOT_SEND_OUT=-10 !code to not send output (APPARENTLY NOT USED?)

  integer, public          :: pcpsx_state            !indicates state
! states: see STATE_... parameters 

  integer                :: pcpsx_init_sw=0          !tracks init/finish states

  integer, pointer       :: pcpsx_parallel_states(:) !indicates in-out parallel
  character (len=1),pointer :: pcpsx_buffer(:)       !general purpose buffer
  integer                :: pcpsx_buffer_size        !buffer_size

  double precision,pointer :: pcpsx_hdr_buff(:,:)    !for sending traces
  real,            pointer :: pcpsx_trc_buff(:,:)    !for sending traces
  
  integer, parameter     :: PCPS_CONTROL_TAG=1000    !tag for control packet
  integer, parameter     :: PCPS_ACCOUNT_TAG=100     !tag for sending accounting
  integer, parameter     :: PCPS_BROADCAST_TAG=101   !tag for broadcast info    
  integer, parameter     :: PCPS_PUT_TAG      =102   !tag for put_data    
 
  integer, parameter     :: NTR_TRACES_MASK=131071   !mask off control bits
!--codes above with bits set indicate more or nomore traces to send out

  integer,parameter   :: HDR_GROUP_NUM    =61     !header word for group num
  integer,parameter   :: HDR_GROUP_NUM_SEQ=62     !header word for group_num_seq

  integer             :: pcpsx_num_traces         !number traces at end_loop
  integer             :: pcpsx_num_dead_traces    !number of dead traces
  integer             :: pcpsx_trace_num          !debug check seq trace #
  real                :: pcpsx_max_lav            !debug maximum lav
  real                :: pcpsx_avg_lav            !debug average lav
  integer,public      :: pcpsx_group_num          !current active group 
  integer,public      :: pcpsx_group_num_seq      !seq# of active group

  public :: pcpsx_abort 
  public :: pcpsx_broadcast
  public :: pcpsx_broadcast_group
  public :: pcpsx_bunch_groups 
  public :: pcpsx_bunch_trace_groups
  public :: pcpsx_bunch_traces
  public :: pcpsx_check_worker_errors
  public :: pcpsx_check_group_errors
  public :: pcpsx_custom_rcs_ident
  public :: pcpsx_debug_print
  public :: pcpsx_debug_stats
  public :: pcpsx_do_parallel_begin
  public :: pcpsx_do_parallel_create
  public :: pcpsx_do_parallel_delete
  public :: pcpsx_do_parallel_end
  public :: pcpsx_do_parallel_init
  public :: pcpsx_dump_group_parms
  public :: pcpsx_dump_hdrs
  public :: pcpsx_dump_trace_list
  public :: pcpsx_end_loop
  public :: pcpsx_expand_buffer
  public :: pcpsx_gather
  public :: pcpsx_gather_group
  public :: pcpsx_get_current_parallel_obj
  public :: pcpsx_get_traces
  public :: pcpsx_get_work_group_sizes
  public :: pcpsx_init_processing
  public :: pcpsx_i_pel
  public :: pcpsx_filter_ntr
  public :: pcpsx_finish_parallel
  public :: pcpsx_finish_processing
  public :: pcpsx_finish_setups
  public :: pcpsx_get_work_group_info
  public :: pcpsx_max_all_reduce
  public :: pcpsx_max_reduce
  public :: pcpsx_max_all_reduce_group
  public :: pcpsx_max_reduce_group
  public :: pcpsx_min_all_reduce
  public :: pcpsx_min_reduce
  public :: pcpsx_min_all_reduce_group
  public :: pcpsx_min_reduce_group
  public :: pcpsx_n_pel
  public :: pcpsx_post_process
  public :: pcpsx_post_setup
  public :: pcpsx_pre_process
  public :: pcpsx_pre_setup
  public :: pcpsx_print_rcs_ident
  public :: pcpsx_put_trace_list
  public :: pcpsx_put_traces
  public :: pcpsx_receive_data
  public :: pcpsx_receive_traces
  public :: pcpsx_restart_processing
  public :: pcpsx_send_data
  public :: pcpsx_send_traces
  public :: pcpsx_set_work_group_size
  public :: pcpsx_set_number_work_groups
  public :: pcpsx_set_trace_mode
  public :: pcpsx_start_loop
  public :: pcpsx_start_processing
  public :: pcpsx_start_setups
  public :: pcpsx_sum_reduce
  public :: pcpsx_sum_reduce_group
  public :: pcpsx_sum_all_reduce
  public :: pcpsx_sum_all_reduce_group
  public :: pcpsx_sync_workers
  public :: pcpsx_sync_no_boss
  public :: pcpsx_sync_group
  public :: pcpsx_which_work_group
  public :: pcpsx_wrapup_processing

! ------------ states to track processing flow and catch errors --------------
  integer,parameter        :: STATE_PRE_INIT            =0
  integer,parameter        :: STATE_POST_INIT           =1

  integer,parameter        :: STATE_PRE_FINISH_SETUP    =2
  integer,parameter        :: STATE_POST_FINISH_SETUP   =3

  integer,parameter        :: STATE_PROC_BEGUN          =4

  integer,parameter        :: STATE_PRE_WRAPUP          =5
  integer,parameter        :: STATE_PRE_FINAL_WRAPUP    =6
  integer,parameter        :: STATE_POST_WRAPUP         =7

  integer,parameter        :: STATE_PRE_FINISH_PROC     =8
  integer,parameter        :: STATE_PRE_FINISH_PARALLEL =9
  integer,parameter        :: STATE_POST_FINISH_PARALLEL=10

! --------------------------- BUNCH NTRS ----------------------------
  integer, parameter       :: BUNCH_EOG        = -3
  integer, parameter       :: BUNCH_NEED_TRACES= -1
  integer, parameter       :: BUNCH_EOF        = -2
  integer, parameter       :: BUNCH_HAVE_DATA  = 1

! ----------------------- ACTIVE_WORKERS STATES ---------------------
  integer, parameter       :: WORKER_ACTIVE    = 0
  integer, parameter       :: WORKER_EOF       =-1
  integer, parameter       :: WORKER_SUSPENDED =-2
  
  character(len=100),save,public :: PCPSX_ident =                              &
   "$Id: pcpsx.f90,v 1.19 2007/12/05 15:05:53 Menger beta sps $"

  interface pcpsx_sync_workers
    module procedure ppio_sync
  end interface

  interface pcpsx_sync_no_boss
    module procedure ppio_sync_no_boss
  end interface

  interface pcpsx_put_trace_list
    module procedure pcpsx_put_trace_list_0
    module procedure pcpsx_put_trace_list_1
    module procedure pcpsx_put_trace_list_2
  end interface
  
  interface pcpsx_expand_buffer
    module procedure pcpsx_expand_buffer_c1
    module procedure pcpsx_expand_buffer_i2
    module procedure pcpsx_expand_buffer_r2
    module procedure pcpsx_expand_buffer_d2
  end interface

  interface pcpsx_check_worker_errors
    module procedure pcpsx_check_worker_errors_i0
    module procedure pcpsx_check_worker_errors_l0
  end interface
  
  interface pcpsx_check_group_errors
    module procedure pcpsx_check_group_errors_i0
    module procedure pcpsx_check_group_errors_l0
  end interface
  
  interface pcpsx_broadcast
    module procedure ppio_broadcast_data
    module procedure ppio_broadcast_c0
    module procedure ppio_broadcast_c1
    module procedure ppio_broadcast_c2
    module procedure ppio_broadcast_i0
    module procedure ppio_broadcast_i1
    module procedure ppio_broadcast_i2
    module procedure ppio_broadcast_i3
    module procedure ppio_broadcast_i4
    module procedure ppio_broadcast_r0
    module procedure ppio_broadcast_r1
    module procedure ppio_broadcast_r2
    module procedure ppio_broadcast_r3
    module procedure ppio_broadcast_r4
    module procedure ppio_broadcast_d0
    module procedure ppio_broadcast_d1
    module procedure ppio_broadcast_d2
    module procedure ppio_broadcast_d3
    module procedure ppio_broadcast_d4
    module procedure ppio_broadcast_z0
    module procedure ppio_broadcast_z1
    module procedure ppio_broadcast_z2
    module procedure ppio_broadcast_z3
    module procedure ppio_broadcast_z4
  end interface 

  interface pcpsx_broadcast_group
    module procedure pcpsx_broadcast_group_c0
    module procedure pcpsx_broadcast_group_c1
    module procedure pcpsx_broadcast_group_c2
    module procedure pcpsx_broadcast_group_i0
    module procedure pcpsx_broadcast_group_i1
    module procedure pcpsx_broadcast_group_i2
    module procedure pcpsx_broadcast_group_i3
    module procedure pcpsx_broadcast_group_i4
    module procedure pcpsx_broadcast_group_r0
    module procedure pcpsx_broadcast_group_r1
    module procedure pcpsx_broadcast_group_r2
    module procedure pcpsx_broadcast_group_r3
    module procedure pcpsx_broadcast_group_r4
    module procedure pcpsx_broadcast_group_d0
    module procedure pcpsx_broadcast_group_d1
    module procedure pcpsx_broadcast_group_d2
    module procedure pcpsx_broadcast_group_d3
    module procedure pcpsx_broadcast_group_d4
    module procedure pcpsx_broadcast_group_z0
    module procedure pcpsx_broadcast_group_z1
    module procedure pcpsx_broadcast_group_z2
    module procedure pcpsx_broadcast_group_z3
    module procedure pcpsx_broadcast_group_z4
  end interface 

  interface pcpsx_gather
    module procedure ppio_gather_c0
    module procedure ppio_gather_c1
    module procedure ppio_gather_c2
    module procedure ppio_gather_i0
    module procedure ppio_gather_i1
    module procedure ppio_gather_i2
    module procedure ppio_gather_r0
    module procedure ppio_gather_r1
    module procedure ppio_gather_r2
    module procedure ppio_gather_d0
    module procedure ppio_gather_d1
    module procedure ppio_gather_d2
    module procedure ppio_gather_z0
    module procedure ppio_gather_z1
    module procedure ppio_gather_z2
  end interface 

  interface pcpsx_gather_group
    module procedure pcpsx_gather_group_c0
    module procedure pcpsx_gather_group_c1
    module procedure pcpsx_gather_group_c2
    module procedure pcpsx_gather_group_i0
    module procedure pcpsx_gather_group_i1
    module procedure pcpsx_gather_group_i2
    module procedure pcpsx_gather_group_r0
    module procedure pcpsx_gather_group_r1
    module procedure pcpsx_gather_group_r2
    module procedure pcpsx_gather_group_d0
    module procedure pcpsx_gather_group_d1
    module procedure pcpsx_gather_group_d2
    module procedure pcpsx_gather_group_z0
    module procedure pcpsx_gather_group_z1
    module procedure pcpsx_gather_group_z2
  end interface 

  interface pcpsx_send_data
    module procedure ppio_send_data_c0
    module procedure ppio_send_data_c1
    module procedure ppio_send_data_c2
    module procedure ppio_send_data_i0
    module procedure ppio_send_data_i1
    module procedure ppio_send_data_i2
    module procedure ppio_send_data_i3
    module procedure ppio_send_data_i4
    module procedure ppio_send_data_r0
    module procedure ppio_send_data_r1
    module procedure ppio_send_data_r2
    module procedure ppio_send_data_r3
    module procedure ppio_send_data_r4
    module procedure ppio_send_data_d0
    module procedure ppio_send_data_d1
    module procedure ppio_send_data_d2
    module procedure ppio_send_data_d3
    module procedure ppio_send_data_d4
    module procedure ppio_send_data_z0
    module procedure ppio_send_data_z1
    module procedure ppio_send_data_z2
    module procedure ppio_send_data_z3
    module procedure ppio_send_data_z4
  end interface 

  interface pcpsx_receive_data
    module procedure ppio_receive_data_c0
    module procedure ppio_receive_data_c1
    module procedure ppio_receive_data_c2
    module procedure ppio_receive_data_i0
    module procedure ppio_receive_data_i1
    module procedure ppio_receive_data_i2
    module procedure ppio_receive_data_i3
    module procedure ppio_receive_data_i4
    module procedure ppio_receive_data_r0
    module procedure ppio_receive_data_r1
    module procedure ppio_receive_data_r2
    module procedure ppio_receive_data_r3
    module procedure ppio_receive_data_r4
    module procedure ppio_receive_data_d0
    module procedure ppio_receive_data_d1
    module procedure ppio_receive_data_d2
    module procedure ppio_receive_data_d3
    module procedure ppio_receive_data_d4
    module procedure ppio_receive_data_z0
    module procedure ppio_receive_data_z1
    module procedure ppio_receive_data_z2
    module procedure ppio_receive_data_z3
    module procedure ppio_receive_data_z4
  end interface 

  interface pcpsx_sum_reduce
    module procedure ppio_sum_reduce_i0
    module procedure ppio_sum_reduce_i1
    module procedure ppio_sum_reduce_i2
    module procedure ppio_sum_reduce_i3
    module procedure ppio_sum_reduce_i4
    module procedure ppio_sum_reduce_r0
    module procedure ppio_sum_reduce_r1
    module procedure ppio_sum_reduce_r2
    module procedure ppio_sum_reduce_r3
    module procedure ppio_sum_reduce_r4
    module procedure ppio_sum_reduce_d0
    module procedure ppio_sum_reduce_d1
    module procedure ppio_sum_reduce_d2
    module procedure ppio_sum_reduce_d3
    module procedure ppio_sum_reduce_d4
    module procedure ppio_sum_reduce_z0
    module procedure ppio_sum_reduce_z1
    module procedure ppio_sum_reduce_z2
    module procedure ppio_sum_reduce_z3
    module procedure ppio_sum_reduce_z4
  end interface 

  interface pcpsx_sum_reduce_group
    module procedure pcpsx_sum_reduce_group_i0
    module procedure pcpsx_sum_reduce_group_i1
    module procedure pcpsx_sum_reduce_group_i2
    module procedure pcpsx_sum_reduce_group_i3
    module procedure pcpsx_sum_reduce_group_i4
    module procedure pcpsx_sum_reduce_group_r0
    module procedure pcpsx_sum_reduce_group_r1
    module procedure pcpsx_sum_reduce_group_r2
    module procedure pcpsx_sum_reduce_group_r3
    module procedure pcpsx_sum_reduce_group_r4
    module procedure pcpsx_sum_reduce_group_d0
    module procedure pcpsx_sum_reduce_group_d1
    module procedure pcpsx_sum_reduce_group_d2
    module procedure pcpsx_sum_reduce_group_d3
    module procedure pcpsx_sum_reduce_group_d4
    module procedure pcpsx_sum_reduce_group_z0
    module procedure pcpsx_sum_reduce_group_z1
    module procedure pcpsx_sum_reduce_group_z2
    module procedure pcpsx_sum_reduce_group_z3
    module procedure pcpsx_sum_reduce_group_z4
  end interface 
  
  interface pcpsx_sum_all_reduce
    module procedure ppio_sum_all_reduce_i0
    module procedure ppio_sum_all_reduce_i1
    module procedure ppio_sum_all_reduce_i2
    module procedure ppio_sum_all_reduce_i3
    module procedure ppio_sum_all_reduce_i4
    module procedure ppio_sum_all_reduce_r0
    module procedure ppio_sum_all_reduce_r1
    module procedure ppio_sum_all_reduce_r2
    module procedure ppio_sum_all_reduce_r3
    module procedure ppio_sum_all_reduce_r4
    module procedure ppio_sum_all_reduce_d0
    module procedure ppio_sum_all_reduce_d1
    module procedure ppio_sum_all_reduce_d2
    module procedure ppio_sum_all_reduce_d3
    module procedure ppio_sum_all_reduce_d4
    module procedure ppio_sum_all_reduce_z0
    module procedure ppio_sum_all_reduce_z1
    module procedure ppio_sum_all_reduce_z2
    module procedure ppio_sum_all_reduce_z3
    module procedure ppio_sum_all_reduce_z4
  end interface 

  interface pcpsx_sum_all_reduce_group
    module procedure pcpsx_sum_all_reduce_group_i0
    module procedure pcpsx_sum_all_reduce_group_i1
    module procedure pcpsx_sum_all_reduce_group_i2
    module procedure pcpsx_sum_all_reduce_group_i3
    module procedure pcpsx_sum_all_reduce_group_i4
    module procedure pcpsx_sum_all_reduce_group_r0
    module procedure pcpsx_sum_all_reduce_group_r1
    module procedure pcpsx_sum_all_reduce_group_r2
    module procedure pcpsx_sum_all_reduce_group_r3
    module procedure pcpsx_sum_all_reduce_group_r4
    module procedure pcpsx_sum_all_reduce_group_d0
    module procedure pcpsx_sum_all_reduce_group_d1
    module procedure pcpsx_sum_all_reduce_group_d2
    module procedure pcpsx_sum_all_reduce_group_d3
    module procedure pcpsx_sum_all_reduce_group_d4
    module procedure pcpsx_sum_all_reduce_group_z0
    module procedure pcpsx_sum_all_reduce_group_z1
    module procedure pcpsx_sum_all_reduce_group_z2
    module procedure pcpsx_sum_all_reduce_group_z3
    module procedure pcpsx_sum_all_reduce_group_z4
  end interface 
  
  interface pcpsx_max_all_reduce
    module procedure ppio_max_all_reduce_i0
    module procedure ppio_max_all_reduce_r0
    module procedure ppio_max_all_reduce_d0
  end interface

  interface pcpsx_max_reduce
    module procedure ppio_max_reduce_i0
    module procedure ppio_max_reduce_r0
    module procedure ppio_max_reduce_d0
  end interface

  interface pcpsx_max_reduce_group
    module procedure pcpsx_max_reduce_group_i0
    module procedure pcpsx_max_reduce_group_r0
    module procedure pcpsx_max_reduce_group_d0
  end interface

  interface pcpsx_max_all_reduce_group
    module procedure pcpsx_max_all_reduce_group_i0
    module procedure pcpsx_max_all_reduce_group_r0
    module procedure pcpsx_max_all_reduce_group_d0
  end interface

  interface pcpsx_min_all_reduce
    module procedure ppio_min_all_reduce_i0
    module procedure ppio_min_all_reduce_r0
    module procedure ppio_min_all_reduce_d0
  end interface

  interface pcpsx_min_reduce
    module procedure ppio_min_reduce_i0
    module procedure ppio_min_reduce_r0
    module procedure ppio_min_reduce_d0
  end interface

  interface pcpsx_min_reduce_group
    module procedure pcpsx_min_reduce_group_i0
    module procedure pcpsx_min_reduce_group_r0
    module procedure pcpsx_min_reduce_group_d0
  end interface

  interface pcpsx_min_all_reduce_group
    module procedure pcpsx_min_all_reduce_group_i0
    module procedure pcpsx_min_all_reduce_group_r0
    module procedure pcpsx_min_all_reduce_group_d0
  end interface

!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!
 
contains
 
! This nmemonics are used inthe comments for the states of ntr
!  >0 : have ntr traces
!  NMT: No more traces
!  FE : Fatal Error
!  NT : Need Traces
!  NA : Null Action
!  LB : Loop Back
 
!===============PCPSX_COMPRESS=================
! compress str1 into str2, skipping blanks
!   str1 and str2 can be the same
!
! Written September 2000 by Charles C Burch
!=============================++===============
  subroutine pcpsx_compress(str1, str2)
    character (len=*), intent(in)  :: str1
    character (len=*), intent(out) :: str2
 
    integer                        :: n1, n2, i1, i2
 
    n1=len(str1)
    n2=len(str2)
    i2=0
    do i1=1,n1
      if(str1(i1:i1).ne.' ') then
        i2=i2+1
        if(i2.gt.n2) return
        str2(i2:i2)=str1(i1:i1)
      endif
    enddo
    if(i2.lt.n2) str2(i2+1:n2)=' '
    return
  end subroutine pcpsx_compress
 
!------------------------ PCPSX_INIT_PROCESSING ------------------------
! initialization for pcps routines
!
! Written July 2000 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_init_processing()
    integer           :: ierr, integer_var
    real              :: real_var
    double precision  :: double_var
    complex           :: complex_var

    integer           :: i,temp
    character(len=40) :: temp_string 

    if(pcpsx_init_sw.ne.0) then
      write(pcps_message,*)  &
       "Invalid init_state(",pcpsx_init_sw,") in pcpsx_init_processing"
      call pcps_abort(pcps_message)
    endif

    pcpsx_init_sw=1
    nullify(pcps_hostnames)
    nullify(pcps_pids)
    nullify(pcpsx_hdr_buff)
    nullify(pcpsx_trc_buff)
    nullify(pcpsx_worker_modes)
    nullify(pcpsx_active_workers)
    nullify(pcpsx_buffer_counts)
    nullify(pcpsx_workers_vec)
    nullify(pcpsx_buffer)
    nullify(pcpsx_parallel_states)
    nullify(instances_info)

    call ppio_init(ierr)
    call pcps_set_worker_info(pcps_current_worker_num, pcps_num_workers, &
     pcps_num_procs)
    pcpsx_start_time=unix_wtime() 

    pcps_sizeof_double_prec=sizeof(double_var)
    pcps_sizeof_real=sizeof(real_var)
    pcps_sizeof_integer=sizeof(integer_var)
    pcps_sizeof_double_prec=sizeof(double_var)
    pcps_sizeof_complex=sizeof(complex_var)

    call unix_get_user_dir(temp_string)
    pcps_user_dir=' '
    do i=1,min(len(temp_string),size(pcps_user_dir))
      pcps_user_dir(i)=temp_string(i:i)
    enddo

    call unix_get_host_name(temp_string)
    pcps_hostname=' '
    do i=1,min(len(temp_string),size(pcps_hostname))
      pcps_hostname(i)=temp_string(i:i)
    enddo

    call unix_get_pid(pcps_pid)
    allocate( pcps_hostnames(1:size(pcps_hostname), 0:pcps_num_procs-1),    &
      stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcps_hostnames in pcpsx_init_processing")
    endif
    
    allocate( pcps_pids(0:pcps_num_procs-1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcps_pids in pcpsx_init_processing")
    endif

    temp = size(pcps_hostname)
    call pcpsx_gather(0, temp, pcps_hostname,    &
      pcps_hostnames(:,0:))
    call pcpsx_broadcast(0, pcps_num_procs, pcps_hostnames(:,0:))
!   print *,"cpu=",pcps_current_worker_num, pcps_hostname,pcps_hostnames

    call pcpsx_gather(0, pcps_pid, pcps_pids(0:))
    call pcpsx_broadcast(0, pcps_num_procs, pcps_pids(0:))
!   print *,"cpu=",pcps_current_worker_num, pcps_pid, pcps_pids

    allocate( pcpsx_hdr_buff(64,1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcpsx_hdr_buff in pcpsx_init_processing")
    endif

    allocate( pcpsx_trc_buff(1024,1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcpsx_trc_buff in pcpsx_init_processing")
    endif

    allocate( pcpsx_worker_modes(1:3,0:pcps_num_procs-1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcpsx_worker_modes in pcpsx_init_processing")
    endif

    allocate( pcpsx_active_workers(1:pcps_num_procs-1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcpsx_active_workers in pcpsx_init_processing")
    endif

    allocate( pcpsx_buffer_counts(1:1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcpsx_buffer_counts in pcpsx_init_processing")
    endif

    allocate(pcpsx_workers_vec(0:pcps_num_procs-1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate workers_vec in pcpsx_init_processing")
    endif

    do i=0,pcps_num_procs-1
      pcpsx_workers_vec(i)=i
    enddo  

    pcpsx_buffer_size=0
    call pcpsx_expand_buffer (pcpsx_buffer, pcpsx_buffer_size, 0, 32000)

    call pcpsx_restart_processing() 
    return
  end subroutine pcpsx_init_processing

!=======================PCPSX_DUMP_DEBUG_TRACE =================
! Dump any collected debug stats or trace tracking info
!
! Written February 2002 by Charles C Burch
!===============================================================
  subroutine pcpsx_dump_debug_trace()
    if(pcpsx_num_traces.gt.0.and.pcps_boss_mode) then   !have debug stats?
      call pcps_print(" ")
      call pcps_print( &
        "------------------------- Debug stats -----------------------")
      write(pcps_message,*) " Number traces processed=",pcpsx_num_traces, &
                            ", number dead traces=",pcpsx_num_dead_traces
      call pcps_print(pcps_message)
      write(pcps_message,*) " Maximum LAV=",pcpsx_max_lav,", average LAV=", &
       pcpsx_avg_lav/max(1,pcpsx_num_traces-pcpsx_num_dead_traces)
      call pcps_print(pcps_message)
    endif

    if(pcpsx_trace_mode.gt.0 .and. pcps_boss_mode) then
      call pcpsx_dump_trace_list(         &
       "------------------ Trace tracking summary ------------------")
      pcpsx_trace_mode=0
    endif
  end subroutine pcpsx_dump_debug_trace

!----------------------PCPSX_RESTART_PROCESSING ----------------------
!  Initia;oze variable for start of a processing loop
!
!  Written February 2002 by Charles C Burch
!---------------------------------------------------------------------
  subroutine pcpsx_restart_processing()
    integer :: ierr

    if(pcpsx_init_sw.ne.1) then
      write(pcps_message,*)  &
       "Invalid init_state(",pcpsx_init_sw,") in pcpsx_restart_processing"
      call pcps_abort(pcps_message)
    endif

    if(pcpsx_num_par_groups.gt.0) then
      deallocate(instances_info, stat=ierr)
      nullify(instances_info)
    endif  
    pcpsx_num_par_groups=0

    pcpsx_last_group=0
    pcpsx_state=0
    pcpsx_active_instance=0
    pcpsx_active_entry_num=0
    pcpsx_num_traces=0
    pcpsx_num_dead_traces=0
    pcpsx_trace_num=-1
    pcpsx_max_lav=0
    pcpsx_avg_lav=0
    pcpsx_group_num=0
 
    pcpsx_worker_modes=-1               !set workers as available
    pcpsx_num_active_workers=0

    call pcpsx_dump_debug_trace()

    return
  end subroutine pcpsx_restart_processing
 
!------------------------PCPSX_WRAPUP_PROCESSING ----------------------
!  Shuts down workers
!
!  Written July 2000 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_wrapup_processing
 
    integer            :: i_proc, ierr
    double precision   :: hdr(1,1)
    real               :: trc(1,1)
 
    integer            :: ntr, enter_state
 
! print *,"pcpsx_wrapup_processing, worker#=",pcps_current_worker_num

    enter_state=pcpsx_state 
    pcpsx_state=STATE_PRE_WRAPUP 

    if(pcps_boss_mode .and.  .not.pcps_worker_mode .and. &
       enter_state.eq.(pcpsx_state-1)) then
 
! --- boss sends message to shut down workers and checks for messages
 
      do i_proc=1, pcps_num_workers
        if(pcpsx_worker_modes(1,i_proc).gt.0)then
          write(pcps_message,*)                                              &
           "Warning: worker (",i_proc,") busy in pcpsx_wrapup_processing"
          call pcps_print(pcps_message)
        else if(pcpsx_worker_modes(1,i_proc).eq.0)then
          write(pcps_message,*)                                              &
           "Warning: worker (",i_proc,") reserved in pcpsx_wrapup_processing"
          call pcps_print(pcps_message)
        endif
      enddo
 
      ntr=SHUT_DOWN
      call pcpsx_put_traces(pcps_num_workers,pcpsx_workers_vec(1:), 1, ntr, &
        hdr, 1, trc,1)
    endif

    pcpsx_state=STATE_PRE_FINAL_WRAPUP
    call pcpsx_sync_workers(ierr)
    pcpsx_state=STATE_POST_WRAPUP
    return
  end subroutine pcpsx_wrapup_processing
 
!=======================PCPSX_FINISH_PARALLEL===================
!  finish up book-keeping for parallel pcps
!
! Written November 2000 by Charles C Burch and Donna K Vunderink
!===============================================================
  subroutine pcpsx_finish_parallel(error)
    logical, intent(inout)        :: error ! True if an error occurs

    integer                :: ierr

    if(pcpsx_init_sw.ne.1) then
      write(pcps_message,*) &
       "pcpsx_finish_parallel called in inproper state(",pcpsx_init_sw,")"
      call pcps_abort(pcps_message)
    endif
    pcpsx_init_sw=2

    call pcpsx_sync_workers(ierr)
    if(.not.error) error=ierr.ne.0

    call ppio_done(ierr)
    if(.not.error) error=ierr.ne.0

    deallocate(pcps_hostnames, stat=ierr)
    deallocate(pcps_pids, stat=ierr)
    deallocate(pcpsx_worker_modes,    stat=ierr)
    deallocate(pcpsx_buffer_counts,   stat=ierr)
    deallocate(pcpsx_active_workers,  stat=ierr)
    deallocate(pcpsx_parallel_states, stat=ierr)
    deallocate(pcpsx_workers_vec,     stat=ierr)

    if(pcpsx_num_par_groups.gt.0) then
      deallocate(instances_info,stat=ierr)
      pcpsx_num_par_groups=0
    endif
    
    if(pcpsx_buffer_size.gt.0) then
      deallocate(pcpsx_buffer, stat=ierr)
      pcpsx_buffer_size=0
    endif

    deallocate(pcpsx_hdr_buff, stat=ierr)
    deallocate(pcpsx_trc_buff, stat=ierr)

    nullify(pcps_hostnames)
    nullify(pcps_pids)
    nullify(pcpsx_worker_modes)
    nullify(pcpsx_buffer_counts)
    nullify(pcpsx_active_workers)
    nullify(pcpsx_parallel_states)
    nullify(pcpsx_workers_vec)
    nullify(instances_info)
    nullify(pcpsx_buffer)
    nullify(pcpsx_hdr_buff)
    nullify(pcpsx_trc_buff)

    call pcpsx_dump_debug_trace()

    return
  end subroutine pcpsx_finish_parallel
 
!=======================PCPSX_DO_PARALLEL_CREATE=====================
! do_begin_create: Creates a do_parallel struct obj and initalizes to
!   default values for use with do_parallel_begin and do_parallel_end
!
! Written July 2000 by Charles C Burch
!=====================================================================
  subroutine pcpsx_do_parallel_create(pobj,error)
 
    type (pcpsx_do_parallel_struct), pointer :: pobj
    logical, intent(out)                     :: error

    integer                                  :: ierr
    type (pcpsx_parallel_group_struct), pointer          :: obj, objs(:)
    
    if(associated(pobj)) deallocate (pobj, stat=ierr)
    allocate(pobj, stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort("Unable to allocate obj in do_begin_create")
    endif
 
    pcpsx_num_par_groups=pcpsx_num_par_groups+1
    pobj%instance=pcpsx_num_par_groups
    
    if(pcpsx_num_par_groups.gt.1) objs=>instances_info

    allocate(instances_info(1:pcpsx_num_par_groups), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort("Unable to allocate instance_info in do_begin_create")
    endif
    
    if(pcpsx_num_par_groups.gt.1) then 
      instances_info(1:pcpsx_num_par_groups-1)= &
        objs(1:pcpsx_num_par_groups-1)
      deallocate(objs, stat=ierr)
    endif

    deallocate(pcpsx_buffer_counts,stat=ierr)
    allocate(pcpsx_buffer_counts(1:pcpsx_num_par_groups), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate pcpsx_buffer_counts in pcpsx_do_begin_create")
    endif

    obj=>instances_info(pcpsx_num_par_groups)
    obj%instance=pcpsx_num_par_groups
    ierr=0
    
    pcps_boss_exec_mode=PCPS_BOSS_DISTRIBUTES
    pcps_receive_mode=PCPS_RECEIVE_PASSTHRU   !set to default values
!   pcps_alt_receive_mode=PCPS_RECEIVE_ALL_EOF 
    pcps_alt_receive_mode=0 
    pcps_send_eof_mode=PCPS_SEND_ONE_EOF
    pcps_send_mode=PCPS_SEND_ROUND_ROBIN
!   pcps_alt_send_mode=PCPS_SEND_ALL_AVAIL
    pcps_alt_send_mode=0
    pcps_generator_mode=PCPS_NO_TRACE_GEN
    pcps_alt_generator_mode=-1
    pcps_resequence_mode=PCPS_NO_RESEQUENCE
    pcps_sort_mode=PCPS_NO_SORT
    pcps_bunch_mode=PCPS_NO_BUNCH
    pcps_alt_bunch_mode=0
 
    obj%tr_len_in=0
    obj%hdr_len_in=0
    obj%tr_len_out=0
    obj%hdr_len_out=0

    obj%num_work_groups=0
 
    obj%do_eof_state=0           !no eof on input
    obj%enddo_eof_state=0        !no eof on input
    obj%bunch_flnm=-1            !bunch file not opened
    obj%bunch_nkeys=0            !bunch keys buffer empty
    obj%bunch_keys_size=0        !size of keys
    obj%bunch_size=-1            !bunch arrays not allocated and file unopened
    obj%ntr_save=NEED_TRACES
    obj%entry_mode_beg=ENTRY_FROM_TOP ! do_parallel_beg enter with data
    obj%entry_mode_end=ENTRY_FROM_TOP ! do_parallel_end enter from do_parallel

    obj%merge_hdr_1=0
    obj%merge_hdr_2=0
    obj%merge_hdr_3=0
    obj%merge_int_1=0
    obj%merge_int_2=0
    obj%merge_int_3=0
    obj%merge_inc_1=1
    obj%merge_inc_2=1
    obj%merge_inc_3=1
    obj%group_in=0
    obj%group_out=1
    obj%group_seq=1
    obj%work_group_in=-2
    obj%work_group_out=-2
    obj%work_group_buff=-2
    obj%line_number=-1
    obj%ntr_lb=NULL_ACTION
    obj%hdr_lb=HDR_RECEIVER_LINE
 
    nullify (obj%active_workers)
    nullify (obj%keys)
    nullify (obj%worker_groups)
    nullify (obj%sort_hdrs)
    nullify (obj%sort_trcs)
    nullify (obj%hdrs_lb)
    nullify (obj%trcs_lb)

    allocate (obj%active_workers(0:pcps_num_workers), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate active workers in do_begin_create")
    endif
    obj%active_workers=WORKER_ACTIVE       !array instances workers active on

    allocate( obj%worker_groups(2,0:pcps_num_procs-1), stat=ierr)
    if(ierr.ne.0) then
      call pcpsx_abort(                                                       &
       "Unable to allocate worker groups in do_begin_create")
    endif
    obj%worker_groups=0               

    error=ierr.ne.0 
    return
  end subroutine pcpsx_do_parallel_create
 
!=============PCPSX_DO_PARALLEL_INIT==============
! initializes for do_parallel_group
!
! Written Sep 2000 by Charles C Burch
!==================================================
  subroutine pcpsx_do_parallel_init(pobj, error)
    type (pcpsx_do_parallel_struct),pointer :: pobj
    logical, intent(inout)                  :: error
 
    integer                                 :: ierr, itemp
    character (len=260)                     :: flnm
    type(pcpsx_parallel_group_struct), pointer          :: obj
 
    obj=>instances_info(pobj%instance)
    ierr=0
!   print *,&
!    "pcpsx_do_parallel_init:send, alt send, rec, alt rec, eof, bunch, sort=", &
!    pcps_send_mode, pcps_alt_send_mode, pcps_receive_mode, &
!    pcps_alt_receive_mode, pcps_send_eof_mode, pcps_bunch_mode, pcps_sort_mode

    if(pcps_alt_send_mode.eq.0)      pcps_alt_send_mode=pcps_send_mode
    if(pcps_alt_receive_mode.eq.0)   pcps_alt_receive_mode=pcps_receive_mode
    if(pcps_alt_generator_mode.lt.0) pcps_alt_generator_mode=pcps_generator_mode
    if(pcps_alt_bunch_mode.eq.0)     pcps_alt_bunch_mode=pcps_bunch_mode
      
    if(pcps_send_mode.eq.PCPS_SEND_ROUND_ROBIN) then
      pcps_send_mode=PCPS_SEND_FIRST_AVAIL
      pcps_bunch_mode=PCPS_BUNCH_TRACE_GROUPS
    
    else if(pcps_send_mode.eq.PCPS_SEND_LINE) then
      pcps_boss_exec_mode=PCPS_BOSS_DISTRIBUTES
      pcps_send_mode=PCPS_SEND_LINE
      pcps_receive_mode=PCPS_RECEIVE_LINE
      pcps_generator_mode=PCPS_TRACE_GEN
      pcps_send_eof_mode=PCPS_SEND_ALL_EOF
      pcps_bunch_mode=PCPS_NO_BUNCH
      pcps_alt_send_mode=PCPS_SEND_ALL
      pcps_alt_receive_mode=PCPS_RECEIVE_ALL_EOF
      pcps_alt_bunch_mode=PCPS_NO_BUNCH
      pcps_alt_generator_mode=PCPS_TRACE_GEN

    else if(pcps_send_mode.eq.PCPS_BOSS_EXECS) then
      pcps_boss_exec_mode=PCPS_BOSS_EXECS

    else if(pcps_send_mode.eq.PCPS_BOSS_CONTROLS) then
      pcps_boss_exec_mode=PCPS_BOSS_CONTROLS

    else if(pcps_send_mode.eq.PCPS_BOSS_EXECS_GATHER)then
      pcps_boss_exec_mode=PCPS_BOSS_EXECS_GATHER
    endif
    
    if(pcps_alt_send_mode.eq.PCPS_SEND_ROUND_ROBIN) then
      pcps_alt_send_mode=PCPS_SEND_FIRST_AVAIL
      pcps_alt_bunch_mode=PCPS_BUNCH_TRACE_GROUPS
    endif
    
    obj%boss_exec_mode=pcps_boss_exec_mode
    if(obj%boss_exec_mode.eq.PCPS_BOSS_EXECS) then
      pcps_send_mode=PCPS_SEND_ALL
      pcps_alt_send_mode=PCPS_SEND_ALL
      pcps_receive_mode=PCPS_NO_RECEIVE
      pcps_alt_receive_mode=PCPS_NO_RECEIVE
      pcps_send_eof_mode=PCPS_SEND_ALL_EOF

    else if(obj%boss_exec_mode.eq.PCPS_BOSS_EXECS_GATHER) then
      pcps_send_mode=PCPS_SEND_ALL
      pcps_alt_send_mode=PCPS_SEND_ALL
      pcps_receive_mode=PCPS_RECEIVE_GATHER
      pcps_alt_receive_mode=PCPS_RECEIVE_GATHER
      pcps_send_eof_mode=PCPS_SEND_ALL_EOF

    else if(obj%boss_exec_mode.eq.PCPS_BOSS_CONTROLS) then
      pcps_send_mode=PCPS_SEND_ALL
      pcps_alt_send_mode=PCPS_SEND_ALL
      pcps_receive_mode=PCPS_NO_RECEIVE
      pcps_alt_receive_mode=PCPS_NO_RECEIVE
      pcps_send_eof_mode=PCPS_SEND_ALL_EOF
    endif

    if(obj%num_work_groups.eq.0) then
      if(pcps_send_mode.eq.PCPS_SEND_ALL .or. &
         pcps_send_mode.eq.PCPS_SEND_ALL_AVAIL) then
        obj%num_work_groups=1
        obj%work_group_size0=pcps_num_workers
        obj%work_group_size1=0
      else
        obj%num_work_groups=pcps_num_workers   !default work group size is 1
        obj%work_group_size0=1
        obj%work_group_size1=0
      endif
    endif
    
    if(pcps_merge_hdr_1.gt.0) then
      call pcps_save_merge_parameters()
      pcps_merge_hdr_1=0
    endif

    obj%receive_mode=pcps_receive_mode
    obj%alt_receive_mode=pcps_alt_receive_mode
 
    obj%send_mode=pcps_send_mode
    obj%alt_send_mode=pcps_alt_send_mode
 
    obj%eof_mode=pcps_send_eof_mode
    obj%generator_mode=pcps_generator_mode
    obj%alt_generator_mode=pcps_alt_generator_mode

    if(pcps_resequence_mode.eq.PCPS_RESEQUENCE_TRACES) then
      obj%resequence_mode  =0
      obj%resequence_groups=-1
    else if(pcps_resequence_mode.eq.PCPS_RESEQUENCE_GROUPS) then
      obj%resequence_mode  =0
      obj%resequence_groups=0
    else
      obj%resequence_mode  =-1
      obj%resequence_groups=-1
    endif

    obj%sort_mode=pcps_sort_mode
    obj%bunch_mode=pcps_bunch_mode
    obj%alt_bunch_mode=pcps_alt_bunch_mode

    if(obj%receive_mode.eq.PCPS_RECEIVE_MERGE) then
      obj%send_mode=PCPS_SEND_FIRST_AVAIL
      obj%bunch_mode=PCPS_BUNCH_MERGE
      obj%generator_mode=PCPS_TRACE_GEN
      obj%eof_mode=PCPS_SEND_ALL_EOF
      obj%alt_receive_mode=PCPS_RECEIVE_MERGE
      obj%alt_send_mode=PCPS_SEND_FIRST_AVAIL
      obj%alt_bunch_mode=PCPS_BUNCH_MERGE
      obj%alt_generator_mode=PCPS_TRACE_GEN
      obj%active_workers(0)=WORKER_EOF
    endif
 
    if(obj%boss_exec_mode.eq.PCPS_BOSS_EXECS        .or. &
       obj%boss_exec_mode.eq.PCPS_BOSS_CONTROLS     .or. &
       obj%boss_exec_mode.eq.PCPS_BOSS_EXECS_GATHER .or. &
       obj%boss_exec_mode.eq.PCPS_BOSS_EXECS_MERGE ) goto 700 

! ----------------------Checking for conflicts-----------------------

    if(obj%send_mode.eq.PCPS_SEND_ALL) then
      if(obj%receive_mode.eq.PCPS_RECEIVE_PASSTHRU) then
        if(pcps_boss_mode) call pcps_print(                                    &
         "Warning-PCPS_SEND_ALL and PCPS_RECEIVE_PASSTHRU incompatible")
        ierr=-5
      endif
    endif
 
    if(obj%alt_send_mode.eq.PCPS_SEND_ALL) then
      if(obj%alt_receive_mode.eq.PCPS_RECEIVE_PASSTHRU) then
        if(pcps_boss_mode) call pcps_print(                                    &
         "Warning-alt PCPS_SEND_ALL and alt PCPS_RECEIVE_PASSTHRU incompatible")
        ierr=-6
      endif
    endif
 
    if(obj%alt_send_mode.eq.PCPS_SEND_ALL_AVAIL) then
      if(obj%alt_receive_mode.eq.PCPS_RECEIVE_PASSTHRU) then
        if(pcps_boss_mode) call pcps_print(                                    &
   "Warning-alt PCPS_SEND_ALL_AVAIL and alt PCPS_RECEIVE_PASSTHRU incompatible")
        ierr=-6
      endif
    endif

    if(obj%eof_mode.eq.PCPS_SEND_ALL_EOF ) then
      if(obj%alt_receive_mode.eq.PCPS_RECEIVE_PASSTHRU) then
        if(pcps_boss_mode) call pcps_print(                                    &
         "Warning-PCPS_SEND_ALL_EOF and alt PCPS_RECEIVE_PASSTHRU incompatible")
        ierr=-7
      endif
 
      if(obj%alt_send_mode.ne.PCPS_SEND_ALL       .and. &
         obj%receive_mode.ne.PCPS_RECEIVE_MODE    .and. &
         obj%alt_send_mode.ne.PCPS_SEND_ALL_AVAIL) then
        if(pcps_boss_mode) call pcps_print(                                    &
     "Warning-PCPS_SEND_ALL_EOF and alt not PCPS_SEND_ALL(_AVAIL) incompatible")
        ierr=-8
      endif
    endif

700 continue
    obj%beg_indx=-1
    obj%seq_num=1
    if(obj%bunch_mode.ne.PCPS_NO_BUNCH    .and. &
       obj%bunch_mode.ne.PCPS_BUNCH_MERGE .and. pcps_boss_mode) then
      write(flnm,*) pcps_user_dir,"/cpstemp/pcpsx_do_parallel_bunch_",        &
       pcps_hostname,"_",pcps_pid,".i",obj%instance
      call pcpsx_compress(flnm, flnm)
      itemp=cio_remove(flnm)
      call cio_set_file_space_commit(PREALLOCATE_FILE_SPACE_ENABLED) 
      itemp = cio_set_file_ext_size(104857600) !--- 100 Mbytes ----!
      obj%bunch_flnm=cio_fopen(trim(flnm),"w")
      if(obj%bunch_flnm.lt.0) then
        write(pcps_message,*) "Error(",obj%bunch_flnm,                       &
         ") in opening bunch_file("//trim(flnm)//") in enddo_init"
        call pcps_print(pcps_message)
        ierr=-3
      endif
    endif
 
    if(.not.error) error=ierr.ne.0
    call pcpsx_check_worker_errors(error)
    return
  end subroutine pcpsx_do_parallel_init

!================= PCPSX_GET_CURRENT_PARALLEL_OBJ ===============
! Get cuen obj for parallel group instance
!
! Written February 2002 by Charles C Burch
!================================================================
  subroutine pcpsx_get_current_parallel_obj(instance, obj)
    integer, intent(in)                         :: instance
    type (pcpsx_parallel_group_struct), pointer :: obj

    if(instance.lt.1. .or. instance.gt.pcpsx_num_par_groups) then
      write(pcps_message,*) &
       "Invalid instance(",instance,") in pcpsx_get_current_parallel_obj"
      call pcps_abort(pcps_message)
    endif

    obj=>instances_info(instance)
    return
  end subroutine pcpsx_get_current_parallel_obj

!=================PCPSX_DO_PARALLEL_DUMP===============
! Diagnostic tool to dump parallel group state
!
! Written July 2000 by Charles C Burch
!======================================================
  subroutine pcpsx_do_parallel_dump(pobj)
    type (pcpsx_do_parallel_struct), pointer :: pobj
 
    type(pcpsx_parallel_group_struct), pointer  :: obj

    obj=>instances_info(pobj%instance)
    
    write(pcps_message,*) "cpu=",pcps_current_worker_num, &
     ", dumping parallel group obj"
    call pcps_print(pcps_message)
    write(pcps_message,*)                                                     &
     "  cpu=",pcps_current_worker_num,", boss_exec_mode=", obj%boss_exec_mode
    call pcps_print(pcps_message)
    write(pcps_message,*)                                                     &
     "  cpu=",pcps_current_worker_num,", send_mode=", obj%send_mode
    call pcps_print(pcps_message)
    write(pcps_message,*)                                                     &
     "  cpu=",pcps_current_worker_num,", receive_mode=", obj%receive_mode
    call pcps_print(pcps_message)
    write(pcps_message,*)                                                     &
     "  cpu=",pcps_current_worker_num,", eof_mode=",obj%eof_mode
    call pcps_print(pcps_message)
    write(pcps_message,*)                                                     &
     "  cpu=",pcps_current_worker_num,", instance=",obj%instance
    call pcps_print(pcps_message)
    write(pcps_message,*)                                                     &
     "  cpu=",pcps_current_worker_num,", eof_state=",obj%do_eof_state
    call pcps_print(pcps_message)
 
    return
  end subroutine pcpsx_do_parallel_dump

!=========================PCPSX_SET_WORK_GROUP_SIZE===================
! set up work groups based on a given minimum work group size   
!
! Written Jan 2002 by Charles C Burch
!=====================================================================
  subroutine pcpsx_set_work_group_size(pobj, size)
    type (pcpsx_do_parallel_struct),pointer :: pobj
    integer, intent(in)                     :: size

    type(pcpsx_parallel_group_struct), pointer          :: obj

    obj=>instances_info(pobj%instance)
    
    obj%work_group_size0=size
    if(obj%work_group_size0.lt.1) obj%work_group_size0=1
    if(obj%work_group_size0.gt.pcps_num_workers) &
      obj%work_group_size0=pcps_num_workers
    obj%num_work_groups=pcps_num_workers/obj%work_group_size0
    obj%work_group_size1= &
     pcps_num_workers-obj%num_work_groups*obj%work_group_size0
    return 
  end subroutine pcpsx_set_work_group_size
    
!=======================PCPSX_SET_NUMBER_WORK_GROUPS==================
! set up work groups based on a given number of work groups
!
! Written Jan 2002 by Charles C Burch
!=====================================================================
  subroutine pcpsx_set_number_work_groups(pobj, n)
    type (pcpsx_do_parallel_struct),pointer :: pobj
    integer, intent(in)                     :: n

    type(pcpsx_parallel_group_struct), pointer          :: obj

    obj=>instances_info(pobj%instance)
    
    obj%num_work_groups=n
    if(obj%num_work_groups.lt.1) obj%num_work_groups=1
    if(obj%num_work_groups.gt.pcps_num_workers) &
      obj%num_work_groups=pcps_num_workers
    obj%work_group_size0=pcps_num_workers/obj%num_work_groups
    obj%work_group_size1= &
     pcps_num_workers-obj%num_work_groups*obj%work_group_size0
    return 
  end subroutine pcpsx_set_number_work_groups

!====================== PCPSX_GET_WORK_GROUP_INFO ====================
! get starting worker index and the number of workers in a given
!  work group number within a parallel do-group s
!
! Written Jan 2002 by Charles C Burch
!=====================================================================
  subroutine pcpsx_get_work_group_info(instance, work_group_num, first_worker, &
    num_workers)
    integer, intent(in)            :: instance
    integer, intent(in)            :: work_group_num
    integer, intent(out)           :: first_worker
    integer, intent(out)           :: num_workers
  
    type(pcpsx_parallel_group_struct), pointer   :: obj

    obj=>instances_info(instance)
    if(work_group_num.lt.1 .or. work_group_num.gt.obj%num_work_groups) then
      first_worker=-1
      num_workers=0
      return
    endif
 
    if(obj%boss_exec_mode.ne.PCPS_BOSS_DISTRIBUTES .or. &
       obj%send_mode.eq.PCPS_SEND_ALL) then
      first_worker=1
      num_workers=pcps_num_workers
      return
    endif

    if(work_group_num.le.obj%work_group_size1) then
      num_workers=obj%work_group_size0+1
      first_worker=(work_group_num-1)*obj%work_group_size0+work_group_num    
    else
      num_workers=obj%work_group_size0
      first_worker=(work_group_num-1)*obj%work_group_size0+&
        obj%work_group_size1+1
    endif
    return
  end subroutine pcpsx_get_work_group_info
    
!==========================PCPSX_WHICH_WORK_GROUP=====================
! get starting worker index and the number of workers in a given
!  work group number within a parallel do-group
!  returns work_group, fisrt_worker and num_workers as -1 on error
!
! Written Jan 2002 by Charles C Burch
!=====================================================================
  subroutine pcpsx_which_work_group(instance, worker_num, work_group, &
   first_worker, num_workers)
    integer, intent(in)            :: instance
    integer, intent(in)            :: worker_num
    integer, intent(out)           :: work_group
    integer, intent(out)           :: first_worker
    integer, intent(out)           :: num_workers

    type (pcpsx_parallel_group_struct),pointer :: obj
    integer                        :: worker_1, n_workers, i_group

    obj=>instances_info(instance)
    
    if(worker_num.lt.1 .or. worker_num.gt.pcps_num_workers) then
      work_group=-1
      first_worker=-1
      num_workers=-1
      return
    endif

    i_group=pcps_num_workers/obj%work_group_size0
    if(i_group.gt.obj%num_work_groups) i_group=obj%num_work_groups
    call pcpsx_get_work_group_info(instance, i_group, worker_1, n_workers)

    do while(worker_num.lt.worker_1 .or. &
             worker_num.ge.worker_1+n_workers)
! --- print *,worker_num, worker_1, n_workers, i_group
      if(worker_1.le.0) &
        call pcpsx_abort("logic error in pcpsx_which_work_group")
      if(worker_num.lt.worker_1) then
        i_group=i_group-1
      else 
        i_group=i_group+1
      endif
      call pcpsx_get_work_group_info(instance, i_group, worker_1, n_workers)
    enddo
   
    work_group=i_group
    first_worker=worker_1
    num_workers=n_workers 
    return
  end subroutine pcpsx_which_work_group
    
!====================== PCPSX_GET_WORK_GROUP_SIZES====================
! get number work groups and minimum work group size
!
! Written Jan 2002 by Charles C Burch
!=====================================================================
  subroutine pcpsx_get_work_group_sizes(instance, num_work_groups, min_size)
    integer, intent(in )          :: instance
    integer, intent(out)          :: num_work_groups
    integer, intent(out)          :: min_size
  
    type (pcpsx_parallel_group_struct),pointer :: obj

    obj=>instances_info(instance)
    num_work_groups=obj%num_work_groups
    min_size=obj%work_group_size0 
    return
  end subroutine pcpsx_get_work_group_sizes
    
!========================PCPSX_START_LOOP=============================
! pcps processing at beginning of seismic processing loop
!   start off with boss with NEED_TRACES unless NO_MORE_TRACES reached
!   start pff with worker with NULL_ACTION
!
! Written Sep 2000 by Charles C Burch
!=====================================================================
  subroutine pcpsx_start_loop(pobj, ntr)
    type (pcpsx_do_parallel_struct),pointer :: pobj
    integer, intent(inout)                  :: ntr

    type(pcpsx_parallel_group_struct), pointer          :: obj

    obj=>instances_info(pobj%instance)
    
    pcpsx_state=STATE_PROC_BEGUN
    pcpsx_current_instance=0 
    if(pcps_boss_mode.and.pcps_worker_mode) then
      ntr=NEED_TRACES              !Only one cpu-both boss and worker
 
    else if(pcps_boss_mode) then             !check for boss mode
      if(pcpsx_active_instance.le.0.and. obj%do_eof_state.eq.0) then
        ntr=NEED_TRACES
      else
        ntr=NULL_ACTION
      endif
!     if(pcpsx_active_instance.eq.0) pcpsx_active_instance=-1
 
    else
! must be in worker mode
 
      ntr=NULL_ACTION
    endif

    return
  end subroutine pcpsx_start_loop
 
!======================PCPSX_DEBUG_STATS==================
! Collect debug statistics
!
! Written Apr 2001 by Charles C Burch
!=========================================================
  subroutine pcpsx_debug_stats(ntr,hdrs, trcs)
    integer, intent(in)            :: ntr
    double precision, intent(in)   :: hdrs(:,:)
    real, intent(in)               :: trcs(:,:)

    integer                        :: i, ihdr

    if(ntr.gt.0) then
      pcpsx_num_traces=pcpsx_num_traces+ntr
      if(pcpsx_trace_num.lt.0) pcpsx_trace_num=hdrs(HDR_SEQUENCE,1)

      do i=1,ntr
        if(hdrs(HDR_SEQUENCE,i).ne.pcpsx_trace_num) then
          ihdr=hdrs(HDR_SEQUENCE,i)
          write(pcps_message,*)  &
           "Debug Warning:Expected trace#=",pcpsx_trace_num,   &
           ", trace# found=",ihdr
          call pcps_print(pcps_message,2) 
        endif

        pcpsx_trace_num=hdrs(HDR_SEQUENCE,i)+1
        if(hdrs(HDR_LAV,i).eq.0) then
          pcpsx_num_dead_traces=pcpsx_num_dead_traces+1
        else
          if(hdrs(HDR_LAV,i).gt.pcpsx_max_lav) pcpsx_max_lav=hdrs(HDR_LAV,i)
          pcpsx_avg_lav=pcpsx_avg_lav+hdrs(HDR_LAV,i)
        endif
      enddo

    endif
    return
  end subroutine pcpsx_debug_stats

!====================PCPSX_END_LOOP=======================
! processing at end of seismic processing loop
!   boss set to LOOP_BACK unless NO_MORE_TRACES reached
!   worker set to LOOP_BACK
!
! Written Sep 2000 by Charles C Burch
!=========================================================
  subroutine pcpsx_end_loop(pobj, ntr)
    type (pcpsx_do_parallel_struct),pointer :: pobj
    integer, intent(inout)                  :: ntr


    type(pcpsx_parallel_group_struct), pointer          :: obj

    obj=>instances_info(pobj%instance)
    
! --- check for FATAL_ERROR 
    if(ntr.eq.FATAL_ERROR) then
      write(pcps_message,*) "FATAL ERROR on worker#",pcps_current_worker_num
      call pcps_abort(pcps_message)
    endif

    if(pcps_boss_mode.and.ntr.gt.0) &
     pcpsx_last_group=iabs(pcpsx_last_group)+1 

    if(pcps_boss_mode.and.pcps_worker_mode) then
      if(ntr.ne.NO_MORE_TRACES) ntr=LOOP_BACK
 
    else if(pcps_worker_mode) then
      ntr=LOOP_BACK
 
    else if(pcps_boss_mode) then
!     if(obj%enddo_eof_state.eq.3) then
!       ntr=NO_MORE_TRACES
!     else
!       ntr=LOOP_BACK
!     endif
      if(ntr.ne.NO_MORE_TRACES) ntr=LOOP_BACK
    else
    endif
 
    obj%ntr_save=NEED_TRACES
    return
  end subroutine pcpsx_end_loop
 
!--------------PCPSX_PRINT_STATE----------------
! diagnostic to print state imnformation
!
! Written Oct 2000 by Charles C Burch
!-----------------------------------------------
  subroutine pcpsx_print_state(flag, pobj, ntr)
    type (pcpsx_do_parallel_struct) :: pobj
    character (len=*),intent(in)    :: flag
    integer, intent(in)             ::  ntr
 
    integer                         :: i, lun=6
 
    type(pcpsx_parallel_group_struct), pointer          :: obj

    obj=>instances_info(pobj%instance)
    
    write(lun,*) "  ntr_save=",obj%ntr_save,           &
     "enter_do=",obj%entry_mode_beg, "enter_end=",obj%entry_mode_end, &
     ", gen md=",obj%generator_mode,                 &
     ", fk_eofst=",obj%do_eof_state, ", ef_eofst=", obj%enddo_eof_state
 
    do i=1, pcps_num_workers
      write(lun,*) "    worker#=",i,", mode=",pcpsx_worker_modes(1:2,i)
    enddo
 
    write(lun,*) ""
 
    return
  end subroutine pcpsx_print_state
 
!=========================PCPSX_FILTER_NTR========================
! Filter PCPS ntrs to appropriate traditional ntrs
!
! Written July 2000 by Charles C Burch
!=================================================================
  subroutine pcpsx_filter_ntr(ipn, ntr)
    integer, intent(in)    :: ipn
    integer, intent(inout) :: ntr

! --print *,"pcpsx_filter_cpu,ntr,ipn,state", &
!   pcps_current_worker_num, ntr, ipn, pcpsx_parallel_states(ipn)
 
    if(pcps_boss_mode) then
      if(pcpsx_parallel_states(ipn).eq.PCPS_OUTSIDE_PARALLEL .or. & 
         pcpsx_parallel_states(ipn).eq.PCPS_BOSS_PARALLEL) then
        if(ntr.eq.LOOP_BACK .or. ntr.eq.NULL_ACTION) ntr=NEED_TRACES
      endif
    else
      if(pcpsx_parallel_states(ipn).eq.PCPS_INSIDE_PARALLEL .or. &
         pcpsx_parallel_states(ipn).eq.PCPS_BOSS_PARALLEL) then
        if(ntr.eq.LOOP_BACK .or. ntr.eq.NULL_ACTION) ntr=NEED_TRACES
      endif
    endif

! --print *,"pcpsx_filter_post:cpu,ntr=", pcps_current_worker_num, ntr 
    return
  end subroutine pcpsx_filter_ntr

!===========================PCPSX_DUMP_GROUP_PARMS=========================
! Dump parallel group parameters
!
! Written July 2000 by Charles C Burch
!==========================================================================
  subroutine pcpsx_dump_group_parms(title)
    character(len=*), intent(in) :: title

    integer           :: i
    character(len=80) :: buff

    if(len_trim(title).gt.0) call pcps_print(title)
    write(buff,*) "Number of workers=",pcps_num_workers
    call pcps_print(buff)

    do i=1, pcpsx_num_par_groups

      write(buff,*) "Parallel Group=",i
      call pcps_print(buff)

      select case (instances_info(i)%boss_exec_mode)
      case(PCPS_BOSS_DISTRIBUTES)
        buff="BOSS_DISTRIBUTES"
      case(PCPS_BOSS_CONTROLS)
        buff="BOSS_CONTROLS"
      case(PCPS_BOSS_EXECS)
        buff="BOSS_EXECS"
      case(PCPS_BOSS_EXECS_GATHER)
        buff="BOSS_EXECS_GATHER"
      case default
        write(buff,*) instances_info(i)%boss_exec_mode
      end select
      buff="  boss_exec_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%send_mode)
      case(PCPS_SEND_FIRST_AVAIL)
        buff="SEND FIRST_AVAILABLE"
      case(PCPS_SEND_ALL)
        buff="SEND_ALL"
      case(PCPS_SEND_ALL_AVAIL)
        buff="SEND_ALL_AVAIL"
      case(PCPS_NO_SEND)
        buff="NO_SEND"
      case(PCPS_SEND_LINE)
        buff="SEND_LINE"
      case default
        write(buff,*) instances_info(i)%send_mode
      end select
      buff="  send_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%receive_mode)
      case(PCPS_RECEIVE_PASSTHRU)
        buff="RECEIVE_PASSTHRU"
      case(PCPS_RECEIVE_SUM)
        buff="RECEIVE_SUM"
      case(PCPS_RECEIVE_GATHER)
        buff="RECEIVE_GATHER"
      case(PCPS_RECEIVE_ALL_EOF)
        buff="RECEIVE_ALL_EOF"
      case(PCPS_GROUP_RECEIVE_SUM)
        buff="GROUP_RECEIVE_SUM"
      case(PCPS_GROUP_RECEIVE_GATHER)
        buff="GROUP_RECEIVE_GATHER"
      case(PCPS_NO_RECEIVE)
        buff="NO_RECEIVE"
      case(PCPS_RECEIVE_MERGE)
        buff="RECEIVE_MERGE"
      case(PCPS_RECEIVE_LINE)
        buff="RECEIVE_LINE"
      case default
        write(buff,*) instances_info(i)%receive_mode
      end select
      buff="  receive_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%generator_mode)
      case(PCPS_NO_TRACE_GEN)
        buff="NO_TRACE_GEN"
      case(PCPS_TRACE_GEN)
        buff="TRACE_GENS"
      case default
        write(buff,*) instances_info(i)%generator_mode
      end select
      buff="  trace_generator_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%bunch_mode)
      case(PCPS_NO_BUNCH)
        buff="NO_BUNCH"
      case(PCPS_BUNCH_TRACES)
        buff="BUNCH_TRACES"
      case(PCPS_BUNCH_GROUPS)
        buff="BUNCH_GROUPS"
      case( PCPS_BUNCH_TRACE_GROUPS)
        buff="BUNCH_TRACE_GROUPS"
      case(PCPS_BUNCH_MERGE)
        buff="BUNCH_MERGE"
      case default
        write(buff,*) instances_info(i)%bunch_mode
      end select
      buff="  bunch_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%eof_mode)
      case(PCPS_SEND_ONE_EOF)
        buff="SEND_ONE_EOF"
      case(PCPS_SEND_ALL_EOF)
        buff="SEND_ALL_EOF"
      case default
        write(buff,*) instances_info(i)%eof_mode
      end select
      buff="  send_eof_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%alt_send_mode)
      case(PCPS_SEND_FIRST_AVAIL)
        buff="SEND FIRST_AVAILABLE"
      case(PCPS_SEND_ALL)
        buff="SEND_ALL"
      case(PCPS_SEND_ALL_AVAIL)
        buff="SEND_ALL_AVAIL"
      case(PCPS_NO_SEND)
        buff="NO_SEND"
      case default
        write(buff,*) instances_info(i)%alt_send_mode
      end select
      buff="  alt_send_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%alt_receive_mode)
      case(PCPS_RECEIVE_PASSTHRU)
        buff="RECEIVE_PASSTHRU"
      case(PCPS_RECEIVE_SUM)
        buff="RECEIVE_SUM"
      case(PCPS_RECEIVE_GATHER)
        buff="RECEIVE_GATHER"
      case(PCPS_RECEIVE_ALL_EOF)
        buff="RECEIVE_ALL_EOF"
      case(PCPS_GROUP_RECEIVE_SUM)
        buff="GROUP_RECEIVE_SUM"
      case(PCPS_GROUP_RECEIVE_GATHER)
        buff="GROUP_RECEIVE_GATHER"
      case(PCPS_NO_RECEIVE)
        buff="NO_RECEIVE"
      case(PCPS_RECEIVE_MERGE)
        buff="RECEIVE_MERGE"
      case default
        write(buff,*) instances_info(i)%alt_receive_mode
      end select
      buff="  alt_receive_mode="//buff
      call pcps_print(buff)

      if(instances_info(i)%resequence_mode  .eq.-1 .and. &
         instances_info(i)%resequence_groups.eq.-1) then
        buff="NO_RESEQUENCE"
      else if(instances_info(i)%resequence_mode  .eq. 0 .and. &
              instances_info(i)%resequence_groups.eq.-1) then
        buff="RESEQUENCE_TRACES"
      else if(instances_info(i)%resequence_mode  .eq. 0 .and. &
              instances_info(i)%resequence_groups.eq. 0) then
        buff="RESEQUENCE_GROUPS"
      else
        buff="unknown"
      endif
      buff="  resequence_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%alt_generator_mode)
      case(PCPS_NO_TRACE_GEN)
        buff="NO_TRACE_GEN"
      case(PCPS_TRACE_GEN)
        buff="TRACE_GENS"
      case default
        write(buff,*) instances_info(i)%generator_mode
      end select
      buff="  alt_trace_generator_mode="//buff
      call pcps_print(buff)

      select case (instances_info(i)%alt_bunch_mode)
      case(PCPS_NO_BUNCH)
        buff="NO_BUNCH"
      case(PCPS_BUNCH_TRACES)
        buff="BUNCH_TRACES"
      case(PCPS_BUNCH_GROUPS)
        buff="BUNCH_GROUPS"
      case( PCPS_BUNCH_TRACE_GROUPS)
        buff="BUNCH_TRACE_GROUPS"
      case(PCPS_RECEIVE_MERGE)
        buff="RECEIVE_MERGE"
      case default
        write(buff,*) instances_info(i)%alt_bunch_mode
      end select
      buff="  alt_bunch_mode="//buff
      call pcps_print(buff)

      call pcps_print(" ")
    enddo
    return
    end subroutine pcpsx_dump_group_parms
 
!==================== PCPSX_DO_PARALLEL_BEGIN ============================
! do_parallel_init handles sending traces to workers and retreiving results
!       also handles workers retreiving traces,
!       but end_do_parallel does the sending
!
!  trcs_in and trlen_in are input traces and length
!  trcs_out and trlen_out are output traces and length
!
! Written July 2000 by Charles C Burch
!==========================================================================
  subroutine pcpsx_do_parallel_begin(pobj,ntr,hdrs_in,trcs_in,hdrs_out,trcs_out)
 
    type (pcpsx_do_parallel_struct),pointer :: pobj
    integer, intent(inout)                  :: ntr
    double precision, pointer               :: hdrs_in(:,:)
    real, pointer                           :: trcs_in(:,:)
    double precision, pointer               :: hdrs_out(:,:)
    real, pointer                           :: trcs_out(:,:)
 
    integer            :: i,hdr_len, tr_len
    integer            :: istat, sender,lb_save
    integer            :: n_sent, work_group, entry_num, max_count
    double precision, pointer               :: hdrs_wrk(:,:)
    real, pointer                           :: trcs_wrk(:,:)
 
    type(pcpsx_parallel_group_struct), pointer          :: obj

    pcpsx_current_instance=pobj%instance
    obj=>instances_info(pcpsx_current_instance)
    
!   print *,"do par begin, cpu#,instance,state,#workers,gen,datamd,ntr=",   &
!    pcps_current_worker_num, obj%instance, obj%do_eof_state,     &
!    obj%generator_mode, obj%entry_mode_beg, ntr
 
    call timer_start(pcps_ntimer)
    if(iand(pcpsx_debug,PCPSX_DEBUG_DO_PARALLEL).ne.0) then
      write(pcps_message,*)"pcpsx_do_parallel_begin, cpu=",   &
       pcps_current_worker_num, &
       ", instance=",obj%instance, ", ntr=", ntr
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif


    if(obj%tr_len_in.eq.0) then
      obj%tr_len_in=size(trcs_in,dim=1)
      obj%hdr_len_in=size(hdrs_in,dim=1)
      obj%max_ntr_in=size(hdrs_in,dim=2)
      obj%tr_len_out=size(trcs_out,dim=1)
      obj%hdr_len_out=size(hdrs_out,dim=1)
      obj%max_ntr_out=size(hdrs_out,dim=2)
      if(obj%send_mode.eq.PCPS_SEND_LINE.and.pcps_boss_mode) then
        if(associated(obj%hdrs_lb)) deallocate(obj%hdrs_lb,stat=istat)
        if(associated(obj%trcs_lb)) deallocate(obj%trcs_lb,stat=istat)
        allocate(obj%hdrs_lb(obj%hdr_len_in,obj%max_ntr_in),stat=istat)
        allocate(obj%trcs_lb(obj%tr_len_in,obj%max_ntr_in),stat=istat)
      endif

! get merge parameters if receive_merge

      if(pcps_boss_mode.and. &
         (obj%receive_mode.eq.PCPS_RECEIVE_MERGE .or. & 
          obj%boss_exec_mode.eq.PCPS_BOSS_EXECS_MERGE)) then
        obj%merge_hdr_1=0
        call pcps_get_merge_parameters(  &
          obj%merge_hdr_1, obj%merge_int_1, obj%merge_inc_1, &
          obj%merge_hdr_2, obj%merge_int_2, obj%merge_inc_2, &
          obj%merge_hdr_3, obj%merge_int_3, obj%merge_inc_3)
        if(obj%merge_hdr_1.le.0) then
          call pcps_print("Warning:merge parameter incorrectly set up")
          obj%merge_hdr_1=1
        endif

        obj%seq_num=0          !trace sequence number
        obj%beg_indx=0         !trace number within group
        obj%group_num=0        !group number
      endif
    endif

    pcps_line_break=-1
!   if(pcps_boss_mode) print *,"pre lb, ntr,ntr_lb=",ntr, obj%ntr_lb
    if(obj%send_mode.eq.PCPS_SEND_LINE.and.pcps_boss_mode) then
      if(ntr.gt.0.or.ntr.eq.NO_MORE_TRACES) then
        if(ntr.eq.NO_MORE_TRACES) then
          if(obj%ntr_lb.gt.0) then
            pcps_line_break=1
          else
            obj%ntr_lb=NO_MORE_TRACES
            pcps_line_break=-1
          endif
        else if(obj%ntr_lb.gt.0) then
          if(hdrs_in(obj%hdr_lb,1).ne.obj%hdrs_lb(obj%hdr_lb,1)) then
            pcps_line_break=1
          else
            pcps_line_break=0
          endif
        else
          pcps_line_break=-1
          obj%ntr_lb=LOOP_BACK
        endif

        i=ntr
        ntr=obj%ntr_lb
        obj%ntr_lb=i
        hdrs_wrk=>hdrs_in
        hdrs_in=>obj%hdrs_lb
        obj%hdrs_lb=>hdrs_wrk     
        trcs_wrk=>trcs_in
        trcs_in=>obj%trcs_lb
        obj%trcs_lb=>trcs_wrk  
      else if(ntr.eq.LOOP_BACK.or.ntr.eq.NEED_TRACES) then
        if(obj%ntr_lb.eq.NO_MORE_TRACES) then
          ntr=NO_MORE_TRACES
          obj%ntr_lb=NULL_ACTION
        endif
      endif             
    endif
    lb_save=pcps_line_break
!   if(pcps_boss_mode) &
!     print *,"post lb, ntr,ntr_lb=",ntr, obj%ntr_lb,pcps_line_break

    if(pcps_boss_mode .and. pcps_worker_mode) then
 
! ---------------ONLY ONE CPU, BOSS AND WORKER MODE ARE TRUE-----------
 
! Valid input ntrs are >0, NMT, NT
      if(ntr.gt.0) then
        if(obj%beg_indx.lt.0) then
          obj%beg_indx=hdrs_in(1,1)
          if(obj%bunch_mode.eq.PCPS_BUNCH_GROUPS) obj%beg_indx=hdrs_in(3,1)
        endif
        call pcpsx_process_generator_mode(obj, ntr)
!      hdrs_out(1:obj%hdr_len_in,1:ntr) = hdrs_in(1:obj%hdr_len_in,1:ntr)
!      trcs_out(1:obj%tr_len_in,1:ntr) = trcs_in(1:obj%tr_len_in,1:ntr)
      else if (ntr.eq.NO_MORE_TRACES) then
        obj%do_eof_state=1
        obj%send_mode=obj%alt_send_mode
        obj%receive_mode=obj%alt_receive_mode
        obj%generator_mode=obj%alt_generator_mode
        obj%bunch_mode=obj%alt_bunch_mode
 
      else if(ntr.eq. NEED_TRACES) then
        ntr=LOOP_BACK
        
      else if(ntr.eq. LOOP_BACK) then
        if(obj%generator_mode.gt.0 .or. obj%do_eof_state.ne.0) ntr=NEED_TRACES

      else
        write(pcps_message,*) "Invalid input ntr(",ntr,") in pcpsx_do_par_begin"
        call pcps_print(pcps_message)
      endif
 
! Valid output ntrs are >0, NMT, LB, NT
      if(ntr.le.0         .and. ntr.ne.NO_MORE_TRACES .and.               &
       ntr.ne.LOOP_BACK .and. ntr.ne.NEED_TRACES) then
        write(pcps_message,*)                                            &
         "Invalid ntr(",ntr,") state in pcpsx_do_par_begin, instance=", &
        obj%instance
        call pcpsx_abort(pcps_message)
      endif
      goto 900
    endif
 
! have a boss and at least one worker
 
    if(pcps_worker_mode) then
 
 
! ------------------------WORKER MODE ------------------
! --- if instance has data, receive data from boss, return
!               - then process it-it gets sent back with do par end
! --- if instance has no data return ntr=NULL_ACTION
 
! Valid ntr are NULL_ACTION(going forward), LOOP_BACK(going backwards)
 
!   print *,"do_par_begin-worker#, instance, ntr=",                           &
!    pcps_current_worker_num, obj%instance, ntr
 
      if(ntr.ne.NULL_ACTION .and. ntr.ne.LOOP_BACK) then
        write(pcps_message,*)                                                &
         "Invalid ntr(",ntr,") state in pcpsx_do_par_begin, instance=", &
         obj%instance
        call pcps_print(pcps_message)
        call pcpsx_abort("pcpsx_do_par_begin worker-mode ntr error")
      endif
 
      if(pcpsx_active_instance.eq.0) then
        call pcpsx_get_traces_info(sender,pcpsx_active_instance, &
         pcpsx_active_position)
      endif
 
!   print *,"worker #, active instance, obj%instance",                         &
!     pcps_current_worker_num, pcpsx_active_instance, obj%instance
      if(pcpsx_active_instance.ne.pcpsx_current_instance) then
        if(ntr.eq.LOOP_BACK) then
          if(pcpsx_current_instance.eq.1) ntr=NULL_ACTION
        else
          ntr=NULL_ACTION
          if(pcpsx_current_instance.eq.pcpsx_num_par_groups) ntr=LOOP_BACK
        endif
        goto 900
      endif

      call pcpsx_get_traces_data(pcpsx_active_position, ntr,    &
       hdrs_in, hdr_len, trcs_in, tr_len)

! Valid ntrs should be >0, NMT, FE, NT, SD
      if(ntr.lt.0         .and. ntr.ne.NO_MORE_TRACES .and.                  &
       ntr.ne.FATAL_ERROR .and. ntr.ne.NEED_TRACES .and.                     &
       ntr.ne.SHUT_DOWN   .and. ntr.ne.NULL_ACTION) then
        write(pcps_message,*)"Invalid received worker ntr(",ntr,             &
         ") in pcpsx_do_parallel_begin"
        call pcpsx_abort(pcps_message)
      endif
 
      if(ntr.eq.NO_MORE_TRACES) then
        obj%send_mode=obj%alt_send_mode
        obj%receive_mode=obj%alt_receive_mode
        obj%generator_mode=obj%alt_generator_mode
        obj%bunch_mode=obj%alt_bunch_mode
      endif
 
      pcpsx_active_instance=0
      goto 900
    endif
 
! ------------------WE ARE NOW IN BOSS MODE--------------------
! generator mode codes: 0=process does not generate traces
!                       1=process can generate traces
!                       2=process might be generating trace-need NT sent
 
!   call pcpsx_print_state("Fork-BOSS enter",pobj,ntr)
!   if(ntr.gt.0) print *,"hd=",(ifix(hdrs_in(1,i)),i=1, ntr)
!   print *,"do par begin-boss enter: inst,state,#wrks,gen,ntr=",    &
!    obj%instance,obj%do_eof_state,  obj%generator_mode, ntr
 
! Valid ntrs should be >0, NMT, NA, LB

! --- keep track trace groups, bunch-setup and checking

    if(ntr.gt.0) then
      obj%group_in=obj%group_in+1
      hdrs_in(HDR_GROUP_NUM,1)=obj%group_in   !set trace group number

      if(obj%beg_indx.lt.0) then     !get starting index for bunching
        obj%beg_indx=hdrs_in(1,1)
        if(obj%bunch_mode.eq.PCPS_BUNCH_GROUPS) obj%beg_indx=hdrs_in(3,1)
        obj%seq_num=obj%beg_indx
      endif

      if(obj%bunch_mode.eq.PCPS_BUNCH_TRACES) then
        if(hdrs_in(1,1).ne.obj%seq_num) then
          write(pcps_message,*) "Warning incoming trace#(",hdrs_in(1,1), &
           ") to pcpsx_do_parallel_begin does not match expected bunch value(",&
           obj%seq_num,")"
          call pcps_print(pcps_message)
        endif
        obj%seq_num=hdrs_in(1,ntr)+1
    
      else if(obj%bunch_mode.eq.PCPS_BUNCH_GROUPS) then
        if(hdrs_in(3,1).ne.obj%seq_num) then
          write(pcps_message,*) "Warning incoming ensemble#(", hdrs_in(3,1), &
           ") to pcpsx_do_parallel_begin does not match expected bunch value(",&
           obj%seq_num,")"
          call pcps_print(pcps_message)
        endif
        obj%seq_num=hdrs_in(3,ntr)+1
      endif
    endif     !end of bunch setup/checking

! --- check for pending receive data

100 obj%ntr_save=NULL_ACTION
!   print *,"100: inst,ntr,eof,act_inst,act_sender,entry=",obj%instance,ntr, &
!     obj%do_eof_state, pcpsx_active_instance,pcpsx_active_sender,           &
!     pcpsx_active_entry_num
!   call pcpsx_dump_status(pcpsx_current_instance, ntr)

    if(ntr.eq.NO_MORE_TRACES) then
      if(obj%do_eof_state.ne.0) &
        call pcps_abort("Invalid EOF state in pcpsx_do_parallel_begin")
      obj%do_eof_state=1
    endif

    if(pcpsx_active_instance.ge.0) then
      if(ntr.gt.0 .or. ntr.eq.NEED_TRACES .or. ntr.eq.NO_MORE_TRACES) then
! ----- Got incomimg data-it takes priority of any requested instance jumping
        pcpsx_active_instance=-1

      else if(pcpsx_active_instance.lt.pcpsx_current_instance) then
! ----- Instance jumping-need to go backwards
        ntr=LOOP_BACK
        goto 900

      else if(pcpsx_active_instance.gt.pcpsx_current_instance) then
! ----- Instance jumping-need to go forward
        ntr=NULL_ACTION
        go to 900

! --- We have now instance jumped to the correct instance 
      else if(obj%enddo_eof_state.eq.3) then
! ----- EOF already received-instance set just so control is set so processes 
! ------after the parallel group can supply any traces they have
        call pcps_abort("Invalid enddo_eof_state in pcpsx_do_parallel_begin")

      else if(pcpsx_active_entry_num.gt.0) then
! --- read data from buffer--intended for boss exec
!       print *,"extract active entry",pcpsx_active_entry_num
        call pcpsx_extract_buffer_data_c(pcpsx_active_entry_num, &
         istat, istat, ntr, hdrs_in, istat, trcs_in, istat, work_group, &
         pcps_line_break)
        pcpsx_active_entry_num=0
      
      else if(pcpsx_active_sender.gt.0) then
! ----- we are are receive instance-get the data and set workers available
!       if trace generator-send NEED TRACES if ntr>0
        ! print *,"do_receive",obj%receive_mode
        call pcpsx_do_parallel_receive(obj, pcpsx_active_sender,  &
         obj%ntr_save, hdrs_out, trcs_out)

!       print *,"RECEIVED-100, sender, ntr=",pcpsx_active_sender, obj%ntr_save

        if(obj%ntr_save.gt.0 .and. obj%generator_mode.eq.PCPS_TRACE_GEN) then
!         print "post receive, send nt"
          call pcpsx_which_work_group(pcpsx_current_instance, &
           pcpsx_active_sender, work_group, istat, istat)
          istat=NEED_TRACES
          call pcpsx_send_active_data(pcpsx_current_instance, work_group, &
            istat, hdrs_in, trcs_in)

        endif  
        pcpsx_active_instance=-1    !inactivate instance jumping

        if(obj%ntr_save.eq.NEED_TRACES .and. obj%do_eof_state.eq.3) then
          write(pcps_message,*) "Parallel group=",obj%instance, &
            ": NEED_TRACES received, but process has been sent NO_MORE_TRACES"
          call pcps_abort(pcps_message)
        endif

      endif  
    endif

! --- send any buffered data where needed workers are available
    call pcpsx_send_ready_buffer_data(n_sent)
!   print *,"Post send_ready: ntr=",ntr, pcpsx_current_instance,n_sent    

    if(ntr.gt.0 .or. ntr.eq.NEED_TRACES .or. ntr.eq.NO_MORE_TRACES) then
      if(pcpsx_get_available_work_group(pcpsx_current_instance,ntr,work_group))&
       then 

! ----- workers available, see if any buffered work
!       print *,"check buffer: wg=",work_group
        call pcpsx_check_buffer_instance(pcpsx_current_instance, work_group, &
         entry_num)
!       print *,"  entry=",entry_num

        if(entry_num.gt.0) then
! ------- workers available and buffered-save current work and get buffer work
!         print *,"put"
          call pcpsx_put_buffer_data(pcpsx_current_instance, obj%group_in, &
           ntr, hdrs_in, obj%hdr_len_in, trcs_in, obj%tr_len_in, 1, work_group)
!         print *,"extract: entry=",entry_num
          call pcpsx_extract_buffer_data_c(entry_num, &
           istat, istat,ntr, hdrs_in, istat, trcs_in, istat, work_group, &
           pcps_line_break)
        endif

! ----- now send work to workers
!       print *,"send: in, work group ntr=",&
!         pcpsx_current_instance, work_group, ntr
        call pcpsx_send_active_data(pcpsx_current_instance, work_group, ntr, &
         hdrs_in, trcs_in)
        n_sent=n_sent+1

! ----- if boss exec goto 900 to start boss execution, else set ntr null action
!       and process any received data
        if(obj%boss_exec_mode.ne.PCPS_BOSS_DISTRIBUTES) goto 900
        ntr=NULL_ACTION

      else 
!------ workers not available, buffer data
!       print *,"put1: wg=",work_group
        call pcpsx_put_buffer_data(pcpsx_current_instance,obj%group_in,  &
         ntr, hdrs_in, obj%hdr_len_in, trcs_in, obj%tr_len_in, 1, work_group)
        ntr=NULL_ACTION
      endif

      if(lb_save.eq.1) then
        obj%work_group_in=-2
        lb_save=-1
      endif
    endif

! - handle any received data
700 if(obj%ntr_save.ne.NULL_ACTION) then
      ntr=NULL_ACTION
      goto 900
    endif  

! - no received data, read more input if buffer not too full and no eof 
    call pcpsx_get_buffer_counts(pcpsx_buffer_counts)
    max_count=maxval(pcpsx_buffer_counts)

    do i=1, pcpsx_num_par_groups
      if(instances_info(i)%do_eof_state.eq.0) exit
    enddo

!    print *, "buffer 1 check, i, max-count, buff(i)=", &
!     i,max_count, pcpsx_buffer_counts(i), pcpsx_num_par_groups

! - Note the only way (i)%do_eof_state==0 and (i-1)%enddo_eof_state!=0
!    is there is a trace generation process between the groups that has
!    been sent NMT  but has not yet returned NMT

    if(i.le.pcpsx_num_par_groups) then
      if(max_count.le.1 .and. pcpsx_buffer_counts(i).eq.0)then
        if(i.eq.1) then
          pcpsx_active_instance=i-1
          ntr=LOOP_BACK
          goto 900
        endif
        if(instances_info(i-1)%enddo_eof_state.eq.3) then
          ntr=LOOP_BACK
          goto 900
        endif
      endif
    endif

! - buffer has sufficient data for eof state-receive some data to free workers
    call pcpsx_get_worker_activity(pcpsx_active_sender, pcpsx_active_instance,&
     pcpsx_active_entry_num)
!   print *,"get worker activity sender, inst, entry:", &
!    pcpsx_active_sender, pcpsx_active_instance, pcpsx_active_entry_num

    ntr=NULL_ACTION
    goto 100

! ------------ ALL DONE AT THIS POINT -------------------------
 
900 if(iand(pcpsx_debug,PCPSX_DEBUG_DO_PARALLEL).ne.0) then
      write(pcps_message,*)" pcpsx_do_parallel_begin:exit, cpu=", &
        pcps_current_worker_num,", ntr=", ntr
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

!   print*," pcpsx_do_parallel_begin:exit, cpu=", pcps_current_worker_num,", &
!    ntr=", ntr
    call timer_stop(pcps_ntimer)
    return
 
  end subroutine pcpsx_do_parallel_begin

!================ PCPSX_GET_WORKER_ACTIVITY ===================
! Find a worker and its instance that has data ready or data
! that needs to be read
! or an buffer entry with boss execs when all workers available
!
! Written February 2002 by Charles C Burch
!===============================================================
  subroutine pcpsx_get_worker_activity(sender, instance, entry_num)
    integer, intent(out) :: sender
    integer, intent(out) :: instance
    integer, intent(out) :: entry_num
 
    integer              :: i, n_size, worker, work_group, ntr

! - see if all workers available
    do i=1, pcps_num_workers
      if(pcpsx_worker_modes(1,i).ge.0) exit
    enddo

    if(i.gt.pcps_num_workers) then
! --- workers available-see if buffer entry for boss execs
      entry_num=0
      do while(.true.)
        call pcpsx_get_buffer_data_c(entry_num, instance, i, ntr, work_group)
        if(entry_num.lt.0) exit

        if(instances_info(instance)%boss_exec_mode.ne.PCPS_BOSS_DISTRIBUTES)&
         then
          if(pcpsx_get_available_work_group(instance, ntr,work_group)) then
            sender=0
            return
          endif
        endif

      enddo
    endif

    entry_num=0
    do i=1, pcps_num_workers
      if(pcpsx_worker_modes(1,i).gt.0) then
        if(instances_info(pcpsx_worker_modes(1,i))%receive_mode.eq. &
            PCPS_NO_RECEIVE) then
          sender=i
          instance=pcpsx_worker_modes(1,sender)
          goto 900
        endif
      endif
    enddo

! - wait for a worker to finish
    call ppio_get_worker_with_data(PCPS_CONTROL_TAG, sender, n_size)

    do i=1, pcpsx_num_active_workers   !trying to balance worker loads
      worker=pcpsx_active_workers(i)   !get oldest busy worker done
      call ppio_test_if_worker_done(worker,PCPS_CONTROL_TAG,n_size)
      if(n_size.le.0) cycle

      !if pcps_send_lines, only allow in and out workers
      instance=pcpsx_worker_modes(1,worker)
      if(instances_info(instance)%send_mode.eq. PCPS_SEND_LINE) then
        if(worker.ne.instances_info(instance)%work_group_in   .and. &
           worker.ne.instances_info(instance)%work_group_buff .and. &
           worker.ne.instances_info(instance)%work_group_out) cycle
      endif
      
      sender=worker
      instance=pcpsx_worker_modes(1,sender)
      goto 900
    enddo

    if(sender.gt.0) then
      instance=pcpsx_worker_modes(1,sender)
      if(instances_info(instance)%send_mode.eq.PCPS_SEND_LINE) then
      ! in pcps_send_line mode, restrict sender to current send worker
      ! buffer worker or worker with oldest data for this instance
      ! give preference to buffer and in worker


        if(instances_info(instance)%work_group_buff.gt.0) then
          if(sender.eq.instances_info(instance)%work_group_buff) goto 900
          call ppio_test_if_worker_done( &
           instances_info(instance)%work_group_buff,PCPS_CONTROL_TAG,n_size)
          if(n_size.gt.0) then 
            sender=instances_info(instance)%work_group_buff
            goto 900
          endif
        endif

        if(instances_info(instance)%work_group_in.gt.0) then
          if(sender.eq.instances_info(instance)%work_group_in) goto 900
          call ppio_test_if_worker_done( &
           instances_info(instance)%work_group_in,PCPS_CONTROL_TAG,n_size)
          if(n_size.gt.0) then 
            sender=instances_info(instance)%work_group_in
            goto 900
          endif
        endif

        if(instances_info(instance)%work_group_out.gt.0) then
          if(sender.eq.instances_info(instance)%work_group_out) goto 900
          if(instances_info(instance)%work_group_buff.gt.0) then
            sender=instances_info(instance)%work_group_buff
          else
            sender=instances_info(instance)%work_group_out
          endif
          goto 900
        endif

        ! got data from instance, but not current send worker and
        ! worker with oldest data-set to worker with oldest data
        do i=1, pcpsx_num_active_workers
          worker=pcpsx_active_workers(i)
          if(pcpsx_worker_modes(1,worker).eq.instance .and. &
             pcpsx_worker_modes(3,worker).lt.pcpsx_worker_modes(3,sender))&
            sender=worker
        enddo
        instances_info(instance)%work_group_out=sender
! ------- print *,"get_worker send-line:",sender
      endif
    else
      instance=-1
    endif

900 continue    
!   print *,"exit pcpsx_get_worker_with data: sender, instance, entry_num=",&
!    sender,instance, entry_num
    return
  end subroutine pcpsx_get_worker_activity

!================ PCPSX_PUT_BUFFER_DATA ===================
! Put data into data input buffer
! isw=0 means to insert at end of buffer list
! isw=1 means to insert at start of buffer list
!
! Written February 2002 by Charles C Burch
!==========================================================
  subroutine pcpsx_put_buffer_data(instance, group, ntr, hdrs, hdr_len, &
   trcs, tr_len, isw, work_group)
    integer,          intent(in) :: instance
    integer,          intent(in) :: group
    integer,          intent(in) :: ntr
    double precision, intent(in) :: hdrs(:,:)  
    integer,          intent(in) :: hdr_len
    real,             intent(in) :: trcs(:,:)
    integer,          intent(in) :: tr_len
    integer,          intent(in) :: isw
    integer, optional,intent(in) :: work_group

    integer                      :: wrkgrp

    if(present(work_group)) then
      wrkgrp=work_group
    else
      wrkgrp=-2        !any worker
    endif

!   print *,"put_buffer_data: ins, wg, ntr=",instance, wrkgrp, ntr

    call pcpsx_put_buffer_data_c(instance,group, ntr, &
           hdrs, hdr_len, trcs, tr_len, wrkgrp, isw, pcps_line_break)
    if(wrkgrp.eq.instances_info(instance)%work_group_in) &
       instances_info(instance)%work_group_buff=wrkgrp
   
    if(pcpsx_trace_mode.gt.0) then
      if(ntr.gt.0) then
        call pcpsx_put_trace_list(instance,hdrs,wrkgrp,"s")
      else if(ntr.eq.NEED_TRACES) then
        call pcpsx_put_trace_list(instance,hdrs,wrkgrp,"G")
      else if(ntr.eq.NO_MORE_TRACES) then
        call pcpsx_put_trace_list(instance,hdrs,0,"B")
      endif
    endif

    return
  end subroutine pcpsx_put_buffer_data

!============ PCPSX_SEND_READY_BUFFER_DATA ================
! Send any buffered data where needed workers are available
!
! Written February 2002 by Charles C Burch
!==========================================================
  subroutine pcpsx_send_ready_buffer_data(n)
    integer, intent(inout)  :: n

    integer :: entry_num, instance, work_group, ntr

    n=0
    do while(.true.)
      call pcpsx_check_buffer_workers(entry_num, instance, work_group, ntr)
      if(entry_num.le.0) return

      call pcpsx_send_buffer_data(entry_num, instance, work_group, ntr)
      n=n+1
    enddo
  end subroutine pcpsx_send_ready_buffer_data
    
!================= PCPSX_SEND_BUFFER_DATA ================
! Send buffered data with entry_num
!
! Written February 2002 by Charles C Burch
!==========================================================
  subroutine pcpsx_send_buffer_data(entry_num, instance, work_group, ntr)
    integer, intent(in) :: entry_num
    integer, intent(in) :: instance
    integer, intent(in) :: work_group
    integer, intent(in) :: ntr

    integer             :: i, istat, lb_save

    if(ntr.gt.0) then
      if(size(pcpsx_hdr_buff,1) .ne. instances_info(instance)%hdr_len_in .or. &
         size(pcpsx_hdr_buff,2) .lt. ntr) then
        deallocate(pcpsx_hdr_buff, stat=istat)
        allocate(pcpsx_hdr_buff(1:instances_info(instance)%hdr_len_in,1:ntr),&
          stat=istat)
        if(istat.ne.0)  &
          call pcpsx_abort("Unable to allocate hdr_buff in  send_buffer-data")
      endif

      if(size(pcpsx_trc_buff,1) .ne. instances_info(instance)%tr_len_in .or. &
         size(pcpsx_trc_buff,2) .lt. ntr) then
        deallocate(pcpsx_trc_buff, stat=istat)
        allocate(pcpsx_trc_buff(1:instances_info(instance)%tr_len_in,1:ntr),&
          stat=istat)
        if(istat.ne.0)  &
          call pcpsx_abort("Unable to allocate trc_buff in  send_buffer-data")
      endif
    endif  

    lb_save=pcps_line_break
    call pcpsx_extract_buffer_data_c(entry_num, istat, istat, istat, &
     pcpsx_hdr_buff, istat, pcpsx_trc_buff, i, istat, pcps_line_break)

    call pcpsx_send_active_data(instance, work_group, ntr, pcpsx_hdr_buff, &
      pcpsx_trc_buff)
    pcps_line_break=lb_save
    if(work_group.eq.instances_info(instance)%work_group_buff) &
       instances_info(instance)%work_group_buff=-2

    return
  end subroutine pcpsx_send_buffer_data

!================= PCPSX_SEND_ACTIVE_DATA ================
!  send out data from instance to given work group
!
! Written February 2002 by Charles C Burch
!==========================================================
  subroutine pcpsx_send_active_data(instance, work_group, ntr, hdr, trc)
    integer,          intent(in) :: instance
    integer,          intent(in) :: work_group
    integer,          intent(in) :: ntr
    double precision, intent(in) :: hdr(:,:)
    real,             intent(in) :: trc(:,:)
    
    integer   :: first_worker, num_workers, i, n, trace_group, w_grp
    integer   :: vec(0:pcps_num_workers)

! - print *,"Pcpsx_send_active:inst, wg, ntr=",instance,work_group, ntr

    w_grp=work_group             !for trace tracking
    if(w_grp.gt.0) then
      call pcpsx_get_work_group_info(instance, work_group, first_worker,&
        num_workers)
    else 
      if(w_grp.ne.-1) print*, "w_grp=",w_grp
      w_grp=-1
      first_worker=1
      num_workers=pcps_num_workers
    endif

    if(ntr.eq.NO_MORE_TRACES) then
      instances_info(instance)%do_eof_state=3
      instances_info(instance)%send_mode=instances_info(instance)%alt_send_mode
      instances_info(instance)%receive_mode= &
       instances_info(instance)%alt_receive_mode
      instances_info(instance)%generator_mode= &
       instances_info(instance)%alt_generator_mode
      instances_info(instance)%bunch_mode= &
       instances_info(instance)%alt_bunch_mode
    endif
   
    if(instances_info(instance)%send_mode.eq.PCPS_NO_SEND) then
      w_grp=0
    else
      n=0     !do not send to workers who have sent EOF
      do i=first_worker, first_worker+num_workers-1
        if(instances_info(instance)%active_workers(i).eq.WORKER_ACTIVE) then
          n=n+1
          vec(n)=i
        endif
      enddo
      
      call pcpsx_put_traces(n,vec(1:), instance, ntr, &
       hdr, instances_info(instance)%hdr_len_in, &
       trc, instances_info(instance)%tr_len_in)
    endif
    
    if(ntr.gt.0) then
      trace_group=hdr(HDR_GROUP_NUM,1)
      instances_info(instance)%worker_groups(1,first_worker)=trace_group
      instances_info(instance)%worker_groups(2,first_worker)=0
    else
      trace_group=ntr
    endif

    call pcpsx_set_workers_busy(instance,trace_group, first_worker,num_workers)

    if(pcpsx_trace_mode.gt.0) then
      if(ntr.gt.0) then
       call pcpsx_put_trace_list(instance,hdr,w_grp,"S")
      else if(ntr.eq.NEED_TRACES) then
       call pcpsx_put_trace_list(instance,hdr,w_grp,"N")
      else if(ntr.eq.NO_MORE_TRACES) then
       call pcpsx_put_trace_list(instance,hdr,w_grp, "E")
      endif
    endif

    return
  end subroutine pcpsx_send_active_data
    
!==============PCPSX_PROCESS_GENERATOR_MODE================
! process generator mode and adjust ntr, obj%generator_mode
!
! Written September 2000 by Charles C Burch
!==========================================================
  subroutine pcpsx_process_generator_mode(obj, ntr)
    type (pcpsx_parallel_group_struct),pointer :: obj
    integer, intent(inout)                  :: ntr
 
! generator mode=0 : processes donot generate traces
!               =1 : processes can generate traces
!               =2 : processes need NMT sent to get generated traces
!               =3 : more traces are available--
! there is a bookkeeping problem with this mode 
!    but no processes currently uses this
 
    if(obj%generator_mode.gt.1) obj%generator_mode=1
    if(ntr.gt.0) then
      if(iand(ntr,PCPS_NTR_HAVE_MORE_TRACES).ne.0) then
        ntr=iand(ntr,NTR_TRACES_MASK)
        if(obj%generator_mode.ge.1 ) obj%generator_mode=2
      elseif (iand(ntr,PCPS_NTR_HAVE_NOMORE_TRACES).ne.0) then
        ntr=iand(ntr,NTR_TRACES_MASK)
      else
        if(obj%generator_mode.ge.1) obj%generator_mode=2
      endif
    endif
    return
  end subroutine pcpsx_process_generator_mode
 
!=================PCPSX_DO_PARALLEL_RECEIVE================
! receive data with pcps
!
! Written Sep 2000 by Charles C Burch
!==========================================================
  subroutine pcpsx_do_parallel_receive(obj, worker_num, ntr, hdrs, trcs)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer, intent(in)                      :: worker_num
    integer, intent(inout)                   :: ntr
    double precision, intent(inout)          :: hdrs(:,:)
    real, intent(inout)                      :: trcs(:,:)
 
    integer       :: first_worker, num_workers, work_group

    if(iand(pcpsx_debug,PCPSX_DEBUG_DO_PARALLEL).ne.0) then
      write(pcps_message,*)" pcpsx_do_parallel_receive: cpu=", &
        pcps_current_worker_num, ", receive_mode=",obj%receive_mode
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    select case (obj%receive_mode)
 
!  --- receive mode = pass through mode
 
    case (PCPS_RECEIVE_PASSTHRU, PCPS_RECEIVE_MERGE)
      call pcpsx_which_work_group(obj%instance, worker_num, work_group, &
       first_worker, num_workers)
      call pcpsx_receive_traces(worker_num,obj%instance,                 &
       ntr,obj%max_ntr_out, hdrs, obj%hdr_len_out, trcs, obj%tr_len_out)
      call pcpsx_set_workers_available(first_worker, num_workers)

!  --- receive mode = receive_line
 
    case (PCPS_RECEIVE_LINE)
      call pcpsx_which_work_group(obj%instance, worker_num, work_group, &
       first_worker, num_workers)
      call pcpsx_receive_traces(worker_num,obj%instance,                 &
       ntr,obj%max_ntr_out, hdrs, obj%hdr_len_out, trcs, obj%tr_len_out)
      call pcpsx_set_workers_reserved(first_worker, num_workers)
      if(work_group.eq.obj%work_group_out) then
        if(ntr.le.0) then 
          obj%work_group_out=-1
          call pcpsx_set_workers_available(first_worker, num_workers)
        endif
      endif

!    --- receive mode=sum
 
    case (PCPS_RECEIVE_SUM, PCPS_GROUP_RECEIVE_SUM)          !sum
      if(obj%receive_mode.eq.PCPS_GROUP_RECEIVE_SUM) then
        call pcpsx_which_work_group(obj%instance, worker_num, work_group, &
         first_worker, num_workers)
      else
        work_group=-1
        first_worker=1
        num_workers=pcps_num_workers
      endif

      call pcpsx_sum_receive_traces(obj,first_worker, num_workers, ntr, &
       obj%max_ntr_out, hdrs, obj%hdr_len_out, trcs, obj%tr_len_out)
      call pcpsx_set_workers_available(first_worker, num_workers)
 
!  --- receive mode=gather
 
    case (PCPS_RECEIVE_GATHER, PCPS_GROUP_RECEIVE_GATHER)
      if(obj%receive_mode.eq.PCPS_GROUP_RECEIVE_GATHER) then    !gather
        call pcpsx_which_work_group(obj%instance, worker_num, work_group, &
         first_worker, num_workers)
      else
        work_group=-1
        first_worker=1
        num_workers=pcps_num_workers
      endif

      call pcpsx_gather_receive_traces(obj,first_worker, num_workers, ntr,&
       obj%max_ntr_out,  hdrs, obj%hdr_len_out, trcs, obj%tr_len_out)
      call pcpsx_set_workers_available(first_worker, num_workers)
 
! --- receive mode=receive all expected to be EOF
! --- this mode is for gather when expecting EOF but to not force
!     using array space needed by job builder when using a gather
 
    case (PCPS_RECEIVE_ALL_EOF)
!     call pcpsx_which_work_group(obj%instance, worker_num, work_group, &
!      first_worker, num_workers)
      work_group=-1
      first_worker=1
      num_workers=pcps_num_workers
      call pcpsx_gather_receive_traces(obj,first_worker, num_workers, ntr,&
       obj%max_ntr_out,  hdrs, obj%hdr_len_out, trcs, obj%tr_len_out)
      call pcpsx_set_workers_available(first_worker, num_workers)
 
      if(ntr.ne.NO_MORE_TRACES) then
        write(pcps_message,*) "PCPS RECEIVE-NO_MORE_TRACES expected but (", &
          ntr,") received-results set to NO_MORE_TRACES"
        call pcps_print(pcps_message)
        ntr=NO_MORE_TRACES
      endif

    case (PCPS_NO_RECEIVE)
     call pcpsx_which_work_group(obj%instance, worker_num, work_group, &
       first_worker, num_workers)
      call pcpsx_set_workers_available(first_worker, num_workers)
      work_group=0

    case default
      write(pcps_message,*)      &
       "Invalid receive mode in pcpsx_do_parallel_receive:"//  &
       " instance, obj%receive mode, obj%send_mode=",            &
       obj%instance, obj%receive_mode, obj%send_mode
      call pcpsx_abort(pcps_message)
    end select

    pcpsx_group_num=obj%worker_groups(1,first_worker)
    obj%worker_groups(2,first_worker)=obj%worker_groups(2,first_worker)+1
    pcpsx_group_num_seq=obj%worker_groups(2,first_worker)
    if(ntr.eq.NEED_TRACES) obj%worker_groups(1,first_worker)=0
    pcpsx_last_sender=work_group
 
    if(pcpsx_trace_mode.gt.0) then
      if(ntr.gt.0) then
       call pcpsx_put_trace_list(obj%instance,hdrs,work_group,"R")
      else if(ntr.eq.NEED_TRACES) then
       call pcpsx_put_trace_list(obj%instance,hdrs,work_group,"n")
      else if(ntr.eq.NO_MORE_TRACES) then
       call pcpsx_put_trace_list(obj%instance,hdrs,work_group,"e")
      endif
    endif

    if(iand(pcpsx_debug,PCPSX_DEBUG_DO_PARALLEL).ne.0) then
      write(pcps_message,*)"  pcpsx_do_parallel_receive-exit: cpu=", &
        pcps_current_worker_num,", ntr=", ntr
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    return
  end subroutine pcpsx_do_parallel_receive
 
!==================PCPSX_DO_PARALLEL_DELETE====================
! wrapup cleans up allocated spaces used by PCPS
!
! Written July 2000 by Charles C Burch
!==============================================================
  subroutine pcpsx_do_parallel_delete(pobj)
    type (pcpsx_do_parallel_struct), pointer :: pobj
 
    integer                                  :: ierr
    type (pcpsx_parallel_group_struct), pointer :: obj
 
    if(.not.associated(pobj)) return
 
    obj=>instances_info(pobj%instance)
    
    if(associated(obj%active_workers))&
                                  deallocate(obj%active_workers,stat=ierr)
    if(associated(obj%worker_groups)) &
                                  deallocate(obj%worker_groups,stat=ierr)
 
    if(associated(obj%keys))      deallocate(obj%keys,stat=ierr)
    if(associated(obj%sort_hdrs)) deallocate(obj%sort_hdrs,stat=ierr)
    if(associated(obj%sort_trcs)) deallocate(obj%sort_trcs,stat=ierr)
    if(associated(obj%hdrs_lb))   deallocate(obj%hdrs_lb,stat=ierr)
    if(associated(obj%trcs_lb))   deallocate(obj%trcs_lb,stat=ierr)

    if(obj%bunch_flnm.ge.0 .and. pcps_boss_mode) then
      ierr=cio_fclose(obj%bunch_flnm,.true.)
      obj%bunch_flnm=-1
    endif
 
    deallocate(pobj,stat=ierr)
    nullify(pobj)
    return
  end subroutine pcpsx_do_parallel_delete
 
!==========================PCPSX_DO_PARALLEL_END========================
! pcpsx_do_parallel_end  handles sending traces from worker to boss
!
!  uses same obj as do_parallel_begin
!  trcs and trlen are input traces and length on input and
!    output traces and length on output
!
! Written July 2000 by Charles C Burch
!======================================================================
  subroutine pcpsx_do_parallel_end(pobj, ntr, hdrs, trcs, hdrs_2, trcs_2)
 
    type (pcpsx_do_parallel_struct), pointer :: pobj
    integer         , intent(inout)          :: ntr
    double precision, intent(inout)          :: hdrs(:,:)
    real            , intent(inout)          :: trcs(:,:)
    real            , intent(inout),optional :: trcs_2(:,:)
    double precision, intent(inout),optional :: hdrs_2(:,:)
 
    integer                                  :: i, i1
    logical                                  :: two_vars_sw
 
    type(pcpsx_parallel_group_struct), pointer          :: obj

    obj=>instances_info(pobj%instance)
    
!   print *,"do_parallel_end,cpu,ntr,state,num_workers,rcv,gen=",      &
!    pcps_current_worker_num, ntr, obj%do_eof_state,          &
!    obj%receive_mode, obj%generator_mode
 
    call timer_start(pcps_ntimer)
    if(iand(pcpsx_debug,PCPSX_DEBUG_DO_PARALLEL).ne.0) then
      write(pcps_message,*) "pcpsx_do_parallel_end, cpu=", &
        pcps_current_worker_num,", ntr=", ntr
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    if(pcps_worker_mode .and. .not.pcps_boss_mode) then
!-------------------- in true worker mode -------------------------------

! -- if in BOSS_EXECS, send tell boss done, set ntr=NULL action, continue

      if(obj%boss_exec_mode.eq.PCPS_BOSS_EXECS) then
        if(ntr.eq.LOOP_BACK .or. ntr.eq.NULL_ACTION) goto 900

!       call pcpsx_send_start(PCPS_BOSS, obj%instance)  !removed 11/01
        ntr=NULL_ACTION
        goto 900
      endif

! Valid ntrs(going forward) are >0, NMT, NT; (backwards) LB
 
      if(ntr.eq.LOOP_BACK .or. ntr.eq.NULL_ACTION) goto 900

!     if(ntr.gt.0) pcpsx_group_num_seq=pcpsx_group_num_seq+1
 
      two_vars_sw=present(hdrs_2).or.present(trcs_2)
      if(two_vars_sw) then
        if( .not.(present(hdrs_2).and.present(trcs_2)) ) then
          write(pcps_message,*)  &
         "Invalid # of arguments called in pcpsx_do_parallel_end-instance", &
           obj%instance
         call pcpsx_abort(pcps_message)
        endif
      
        call pcpsx_send_traces(0,obj%instance, ntr,  hdrs_2,                   &
         obj%hdr_len_out, trcs_2, obj%tr_len_out)
      else
        call pcpsx_send_traces(0,obj%instance, ntr, hdrs,                      &
         obj%hdr_len_out, trcs, obj%tr_len_out)
      endif
 
      ntr=NULL_ACTION
      goto 900
    endif 
 
 
! ---------------------- BOSS MODE -------------------------------

! convert two sets of variable case to one set of variable case

    if(ntr.gt.0 .and. &
     (pcps_worker_mode.or.obj%boss_exec_mode.ne.PCPS_BOSS_DISTRIBUTES)) then
      two_vars_sw=present(hdrs_2).or.present(trcs_2)
      if(two_vars_sw) then
        if( .not.(present(hdrs_2).and.present(trcs_2)) )                   &
          call pcpsx_abort(  &
           "Invalid number of arguments called in pcpsx_do_parallel_end")
        i1=iand(ntr,NTR_TRACES_MASK)
        hdrs(1:obj%hdr_len_out,1:i1) = hdrs_2(1:obj%hdr_len_out,1:i1)
        trcs(1:obj%tr_len_out, 1:i1) = trcs_2(1:obj%tr_len_out, 1:i1)
      endif
    endif

    if(pcps_worker_mode) then
! ------------------------ Dual boss-worker mode ------------------------
      if(ntr.eq.NO_MORE_TRACES) then
        if(obj%enddo_eof_state.ne.0) & 
          call pcpsx_abort("do_parallel_end EOF(boss-worker) Logic error")
        obj%enddo_eof_state=3
        obj%entry_mode_end=ENTRY_FROM_BELOW   ! should not be entered again 

      else if(ntr.gt.0) then
        obj%entry_mode_end=ENTRY_FROM_BELOW   ! enter next time with LOOP_BACK

      elseif(ntr.eq.NEED_TRACES) then
        obj%entry_mode_end=ENTRY_FROM_TOP
        ntr=LOOP_BACK

      else if(ntr.eq.LOOP_BACK) then
        obj%entry_mode_end=ENTRY_FROM_TOP
        if(obj%enddo_eof_state.eq.3) then  ! check to see if NMT received
          ntr=NEED_TRACES
          obj%entry_mode_end=ENTRY_FROM_BELOW
        endif
        
      else
        write(pcps_message,*) &
         "Invalid boss-worker ntr(",ntr,") in pcpsx_do_parallel_end"
        call pcps_abort(pcps_message)
      endif
      goto 900
    endif

! ---------------- WE ARE NOW IN NORMAL BOSS MODE--------------------
! Note: entry_mode ENTRY_FROM_TOP means came from previous parallel group
!                  ENTRY_FROM_BELOW means looping back from after parallel grp
!
    select case(obj%boss_exec_mode)

! --- if BOSS_EXECS mode, check EOF, continue
    case(PCPS_BOSS_EXECS, PCPS_BOSS_EXECS_GATHER, PCPS_BOSS_CONTROLS)  

!     print *,"exec boss, ntr=",ntr, obj%enddo_eof_state, obj%entry_mode_end

      if(ntr.eq.NEED_TRACES .and. obj%entry_mode_end.eq.ENTRY_FROM_BELOW) &
        ntr=LOOP_BACK
      if(ntr.eq.LOOP_BACK .and. obj%enddo_eof_state.ne.0) ntr=NULL_ACTION

      if(ntr.eq.LOOP_BACK) then
        obj%entry_mode_end=ENTRY_FROM_TOP
        goto 900
      else if(ntr.eq.NULL_ACTION) then
        obj%entry_mode_end=ENTRY_FROM_BELOW 
        goto 900
      else
      endif
      
! At this point, entry_mode is ENTRY_FROM_TOP, so have data to receive
! Receive data and set entry_mode for next time we come back
      call pcpsx_do_parallel_receive(obj, 1, ntr, hdrs, trcs)
!     print *,"ntr revceived=",ntr

      if(ntr.eq.NO_MORE_TRACES) then
        obj%enddo_eof_state=3
        obj%entry_mode_end=ENTRY_FROM_BELOW    ! should not reenter
        
      else if(ntr.gt.0) then
        obj%entry_mode_end=ENTRY_FROM_BELOW
!       call pcpsx_process_generator_mode(obj, ntr)
        if(obj%generator_mode.eq.PCPS_TRACE_GEN) then
          i=NEED_TRACES   !prep for getting more data next time thru
          call pcpsx_put_buffer_data(obj%instance,pcpsx_group_num, i, &
           hdrs, obj%hdr_len_in, trcs, obj%tr_len_in, 0)
        endif
        
      else if(ntr.eq.NEED_TRACES) then
        obj%entry_mode_end=ENTRY_FROM_TOP     !prepare for a loop_back
        ntr=LOOP_BACK
      else
        write(pcps_message,*) &
         "Invalid boss ntr(",ntr,") in pcpsx_do_parallel_end"
        call pcps_abort(pcps_message)
      endif

!     call pcpsx_dump_status(obj%instance, ntr)
      goto 900

    case (PCPS_BOSS_EXECS_MERGE)
      call pcpsx_abort("BOSS_EXECS_MERGE No Longer supportted")

    case (PCPS_BOSS_DISTRIBUTES)
      continue
      
    case default
      write(pcps_message,*) &
       "Invalid boss_exec_mode(",obj%boss_exec_mode,") in pcpsx_do_parallel_end"
      call pcps_abort(pcps_message)
    end select

! ---------------- WE ARE NOW IN BOSS_DISTRIBUTES MODE--------------------
!   print *,"normal boss, ntr=",ntr,obj%ntr_save

    if(ntr.eq.NULL_ACTION) ntr=obj%ntr_save
    if(ntr.eq.LOOP_BACK) then
      if(obj%entry_mode_end.eq.ENTRY_FROM_TOP) goto 900
      ntr=NEED_TRACES
    endif  

! --- any LOOP_BACK converted to NEED_TRACES
!     check for EOF, if so, mark EOF found, convert to NEED_TRACES
 
    if(ntr.eq.NO_MORE_TRACES) then
      if(obj%enddo_eof_state.ne.0) & 
        call pcpsx_abort("do_parallel_end EOF Logic error")
 
      obj%enddo_eof_state=1
      if(obj%bunch_mode.eq.PCPS_NO_BUNCH) obj%enddo_eof_state=2
      obj%entry_mode_end=ENTRY_FROM_TOP
      ntr=NEED_TRACES
    endif

    if(ntr.gt.0) then
      if(obj%bunch_mode.ne.PCPS_NO_BUNCH) then
        call pcpsx_bunch(obj,ntr,hdrs,trcs)
        if(ntr.eq.NEED_TRACES) then
          obj%entry_mode_end=ENTRY_FROM_TOP
          ntr=LOOP_BACK
          goto 900
        endif
      endif
      goto 800        !have data from bunch
    endif
    
    if(ntr.eq.NULL_ACTION) then
      obj%entry_mode_end=ENTRY_FROM_BELOW 
      goto 900
    endif
    
    if(ntr.ne.NEED_TRACES) then
      write(pcps_message,*) &
       "Invalid boss_distribute ntr(",ntr,") in pcpsx_do_parallel_end"
      call pcps_abort(pcps_message)
    endif

! ntr is NEED_TRACES at this point

!   if(obj%entry_mode.eq.ENTRY_FROM_TOP .and. obj%generator_mode.ge.1 .and. & 
!      obj%bunch_mode.eq.PCPS_BUNCH_TRACE_GROUPS.and.     &
!      pcpsx_group_num.gt.0) then

    if(obj%entry_mode_end.eq.ENTRY_FROM_TOP) then
      if(obj%bunch_mode.eq.PCPS_BUNCH_TRACE_GROUPS) then
        ntr=BUNCH_EOG
        call pcpsx_bunch_trace_groups(obj,ntr,hdrs,trcs)!indicate end of group
        if(ntr.gt.0) goto 800
      endif
      
      if(obj%do_eof_state.ne.3) then
        ntr=LOOP_BACK
        goto 900
      endif
    endif
 
! --- no input data, try to get data from bunch buffer
 
    if(obj%enddo_eof_state.eq.0) then
      if(obj%bunch_mode.ne.PCPS_NO_BUNCH) then
        ntr=BUNCH_NEED_TRACES
        call pcpsx_bunch(obj,ntr,hdrs,trcs)
        if(ntr.gt.0) goto 800
      endif

      obj%entry_mode_end=ENTRY_FROM_TOP
      ntr=LOOP_BACK
      goto 900
    endif
 
! --- input EOF happened
 
    if(obj%enddo_eof_state.eq.1) then
      if(obj%bunch_mode.ne.PCPS_NO_BUNCH) then
        ntr=BUNCH_EOF
        call pcpsx_bunch(obj,ntr,hdrs,trcs)
        if(ntr.gt.0) goto 800
      endif
      obj%enddo_eof_state=2
    endif

! ---  eof_state>=2
 
    if(obj%enddo_eof_state.eq.2) then
      obj%enddo_eof_state=3
      ntr=NO_MORE_TRACES
      goto 900
    endif
 
! --- eof_state=3
 
    ntr=NULL_ACTION
    goto 900
 
! --------------------------- here, ntr >=0 ------------------------------
 
800 obj%entry_mode_end=ENTRY_FROM_BELOW
    if(obj%sort_mode.eq.PCPS_SORT_TRACES.and.ntr.gt.0) then
      call pcpsx_sort_traces(ntr, hdrs, trcs,1)
    endif
 
    if(obj%resequence_groups.ge.0 .and.ntr.gt.0) then
      obj%resequence_groups=obj%resequence_groups+1
      do i=1,ntr
        hdrs(3,i)=obj%resequence_groups
        hdrs(4,i)=i
      enddo
    endif 

    if(obj%resequence_mode.ge.0.and. ntr.gt.0) then
      do i=1, ntr
        obj%resequence_mode=obj%resequence_mode+1
        hdrs(1,i)=obj%resequence_mode
      enddo
    endif
 
!900 pcpsx_current_instance=0
900 continue
    if(iand(pcpsx_debug,PCPSX_DEBUG_DO_PARALLEL).ne.0) then
      write(pcps_message,*)" pcpsx_do_parallel_end:exit, cpu=", &
        pcps_current_worker_num,", ntr=", ntr
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    call timer_stop(pcps_ntimer)
    return
 
  end subroutine pcpsx_do_parallel_end

!------------------------PCPSX_BUNCH-----------------------------
! choose one of several bunch modes to simply calling bunch logic
!
! Written September 2002 by Charles C Burch
!----------------------------------------------------------------
  subroutine pcpsx_bunch(obj, ntr, hdrs, trcs)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer         , intent(inout)             :: ntr
    double precision, intent(inout)             :: hdrs(:,:)
    real            , intent(inout)             :: trcs(:,:)
   
    select case (obj%bunch_mode) 
    case(PCPS_BUNCH_TRACES)
      call pcpsx_bunch_traces(obj,ntr,hdrs,trcs,1)
    case (PCPS_BUNCH_GROUPS)
      call pcpsx_bunch_groups(obj,ntr,hdrs,trcs,3)
    case (PCPS_BUNCH_TRACE_GROUPS)
      call pcpsx_bunch_trace_groups(obj,ntr,hdrs,trcs)
    case (PCPS_NO_BUNCH)
      continue
    case default
!     Note   PCPS_BUNCH_MERGE no longer active
      write(pcps_message,*) &
       "Invalid bunch_mode(",obj%bunch_mode,") in pcpsx_bunch"
      call pcps_abort(pcps_message)
    end select

    return
  end  subroutine pcpsx_bunch

!------------------------PCPSX_CHECK_BUFFER_WORKERS-------------------
! get first buffered boss-distributes data whose workers are available
! returns entry_num where workers available, 
! returns entry_num<0 if no entries with workers available
!
! Written February 2002 by Charles C Burch
!---------------------------------------------------------------------
  subroutine pcpsx_check_buffer_workers(entry_num, instance, work_group, ntr)
    integer, intent(inout) :: entry_num
    integer, intent(inout) :: instance
    integer, intent(inout) :: work_group
    integer, intent(inout) :: ntr

    integer                :: trace_group
    
    entry_num=0
    do while(.true.)
      call pcpsx_get_buffer_data_c(entry_num, instance, trace_group, ntr, &
                                   work_group)
      if(entry_num.lt.0) exit

      if(instances_info(instance)%boss_exec_mode.ne.PCPS_BOSS_DISTRIBUTES) cycle
 
      if(work_group.eq.-2) then
        if(pcpsx_get_available_work_group(instance, ntr, work_group)) return
      else
        if(pcpsx_is_work_group_available(instance, work_group)) return
      endif

    end do
    
    entry_num=-1        !already<0, but here in case logic changes
    instance=-1
    work_group=-2
    return
  end subroutine pcpsx_check_buffer_workers
  
!--------------------------PCPSX_GET_BUFFER_COUNTS -------------------------
! get count of data buffered
!
! Written February 2002 by Charles C Burch
!---------------------------------------------------------------------------
  subroutine pcpsx_get_buffer_counts(counts)
    integer, intent(out) :: counts(:)

    integer              :: instance, group, ntr
    integer              :: entry_num, work_group
    
    counts=0
    entry_num=0
    do while(.true.)
      call pcpsx_get_buffer_data_c(entry_num, instance, group, ntr, work_group)
      if(entry_num.lt.0) exit
      counts(instance)=counts(instance)+1
    end do

    return
  end subroutine pcpsx_get_buffer_counts

!---------------- PCPSX_CHECK_BUFFER_INSTANCE-------------
! get max instance count of data buffered
!
! Written February 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine pcpsx_check_buffer_instance(inst, wg, entry_num)
    integer, intent(in ) :: inst
    integer, intent(in)  :: wg  
    integer, intent(out) :: entry_num

    integer              :: instance, group, work_group, ntr
    
    entry_num=0
    do while(.true.)
      call pcpsx_get_buffer_data_c(entry_num, instance, group, ntr, work_group)
      if(entry_num.lt.0) exit

      if(inst.eq.instance .and. &
         (work_group.eq.-2 .or. wg.eq.work_group) ) return
    end do
    
    entry_num=-1
    return
  end subroutine pcpsx_check_buffer_instance
  

!----------------------PCPSX_GET_AVAILABLE_WORK_GROUP------------------
! For given instance return available or needed work group, 
! returns true if work group available or false if unavailable
!
! Written February 2002 by Charles C Burch
!----------------------------------------------------------------------
  function pcpsx_get_available_work_group(instance,ntr, work_group) &
   result(avail) 
    integer, intent(in)    :: instance
    integer, intent(in)    :: ntr
    integer, intent(inout) :: work_group
    logical                :: avail

    integer, save          :: wrk_grp=0
    integer                :: i

    avail=.true.
    work_group=-2
    if(ntr.eq.NO_MORE_TRACES .and. &
       instances_info(instance)%eof_mode.eq.PCPS_SEND_ALL_EOF) then
      work_group=-1
    else if(instances_info(instance)%send_mode.eq.PCPS_SEND_ALL) then
      work_group=-1
    else if(instances_info(instance)%send_mode.eq.PCPS_SEND_LINE .and. &
            instances_info(instance)%work_group_in.gt.0) then
      work_group=instances_info(instance)%work_group_in
    endif

    if(work_group.ne.-2) then
! --- note this check for specific work group or all workers
      avail=pcpsx_is_work_group_available(instance, work_group)
      return
    endif
    
! - now see any work group is available
    do i=1, instances_info(instance)%num_work_groups
      wrk_grp=wrk_grp+1
      if(wrk_grp.gt.instances_info(instance)%num_work_groups) wrk_grp=1
      if(pcpsx_is_work_group_available(instance, wrk_grp)) then
        work_group=wrk_grp
        if(instances_info(instance)%send_mode.eq.PCPS_SEND_LINE) &
          instances_info(instance)%work_group_in=work_group
        return
      endif        
    enddo
    
    work_group=-2
    avail=.false.
    return
  end function pcpsx_get_available_work_group
  
!----------------------PCPSX_SET_WORKERS_AVAILABLE----------------------
!  Sets a worker(worker_no) mode to available-for-work
!
! Written July 2000 by Charles C Burch
!-----------------------------------------------------------------------
  subroutine pcpsx_set_workers_available(worker_no, num_workers)
    integer, intent(in)              :: worker_no
    integer, optional, intent(in)    :: num_workers
 
    integer                          :: n_worker, n_workers, i_worker, n1
 
    if(present(num_workers)) then
      n_workers=num_workers
      if(n_workers.lt.1) return
    else
      n_workers=1
    endif

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *) "  pcpsx_set_worker_available:cpu=",  &
       pcps_current_worker_num,                   &
       ", worker_no=", worker_no, ", #workers=", n_workers
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    if(pcps_current_worker_num.gt.0) then
      write(pcps_message,*) "pcpsx_set_worker_available:" //               &
       "pcpsx_set_worker_available called in worker mode"
      call pcpsx_abort(pcps_message)
      return
    endif
 
! --- see if want to set all workers to available
 
    if(worker_no.eq.-1) then
      n_worker=1
      n_workers=pcps_num_workers
    else
      n_worker=worker_no
    endif
 
    if(n_worker.lt.1 .or. (n_worker+n_workers-1).gt.pcps_num_workers) then
      write(pcps_message,*) "pcpsx_set_workers_available:" //                 &
       "FATAL ERROR:Invalid processor number(",n_worker,n_workers,")"
      call pcpsx_abort(pcps_message)
    endif
 
    n_workers=n_worker+n_workers-1
    do i_worker=worker_no,n_workers
      if(pcpsx_worker_modes(1,i_worker).eq.-1) then
        write(pcps_message,*)    &
         "pcpsx_set_worker_available:Attempting to set worker(",   &
         i_worker,") to AVAIL but already is AVAIL"
        call pcpsx_abort(pcps_message)
      endif
 
    enddo

    pcpsx_worker_modes(1:2,n_worker:n_workers)=-1

    n1=pcpsx_num_active_workers
    pcpsx_num_active_workers=0
    do i_worker=1, n1          !trim active worker array
      if(pcpsx_active_workers(i_worker).lt.worker_no .or. &
         pcpsx_active_workers(i_worker).gt.n_workers) then
        pcpsx_num_active_workers=pcpsx_num_active_workers+1
        if(i_worker.ne.pcpsx_num_active_workers) &
          pcpsx_active_workers(pcpsx_num_active_workers)=&
           pcpsx_active_workers(i_worker)
      endif
    enddo 

    return
 end subroutine pcpsx_set_workers_available
 
!----------------------PCPSX_SET_WORKER_RESERVED----------------------
!  Sets a worker(worker_no) mode to reserved
!
! Written July 2000 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_set_workers_reserved(worker_no, num_workers)
    integer, intent(in)              :: worker_no
    integer, optional, intent(in)    :: num_workers
 
    integer                          :: n_worker, n_workers, i_worker, n1
 
    if(present(num_workers)) then
      n_workers=num_workers
      if(n_workers.lt.1) return
    else
      n_workers=1
    endif

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *) "  pcpsx_set_worker_reserved:cpu=",  &
       pcps_current_worker_num,                   &
       ", worker_no=", worker_no, ", #workers=", n_workers
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    if(pcps_current_worker_num.gt.0) then
      write(pcps_message,*) "pcpsx_set_worker_reserved:" //                 &
       "pcpsx_set_worker_reserved called in worker mode"
      call pcpsx_abort(pcps_message)
      return
    endif
 
! --- see if want to set all workers to available
 
    if(worker_no.eq.-1) then
      n_worker=1
      n_workers=pcps_num_workers
    else
      n_worker=worker_no
    endif
 
    if(n_worker.lt.1 .or. (n_worker+n_workers-1).gt.pcps_num_workers) then
      write(pcps_message,*) "pcpsx_set_workers_reserved:" //               &
       "FATAL ERROR:Invalid processor number(",n_worker,n_workers,")"
      call pcpsx_abort(pcps_message)
    endif
 
    n_workers=n_worker+n_workers-1
    do i_worker=worker_no,n_workers
      if(pcpsx_worker_modes(1,i_worker).le.0) then
        write(pcps_message,*)    &
         "pcpsx_set_reserved:Attempting to set worker(",   &
         i_worker,") to reserved but already is reserved/AVAIL"
        call pcpsx_abort(pcps_message)
      endif
 
    enddo

    pcpsx_worker_modes(1:2,n_worker:n_workers)=0

    n1=pcpsx_num_active_workers
    pcpsx_num_active_workers=0
    do i_worker=1, n1          !trim active worker array
      if(pcpsx_active_workers(i_worker).lt.worker_no .or. &
         pcpsx_active_workers(i_worker).gt.n_workers) then
        pcpsx_num_active_workers=pcpsx_num_active_workers+1
        if(i_worker.ne.pcpsx_num_active_workers) &
          pcpsx_active_workers(pcpsx_num_active_workers)=&
           pcpsx_active_workers(i_worker)
      endif
    enddo 

    return
 end subroutine pcpsx_set_workers_reserved
 
!------------------PCPSX_ARE_WORKERS_AVAILABLE---------------------------
! Sees if worker(worker_no:worker_no+num_worker-1) available-for-work
!
! Written February 2002 by Charles C Burch
!----------------------------------------------------------------------
  function pcpsx_are_workers_available(instance, worker_num, num_workers) &
  result(avail)
    integer, intent(in) :: instance
    integer, intent(in) :: worker_num
    integer, intent(in) :: num_workers
    logical             :: avail

    integer             :: i, n1

!   print *,"are workers available: ins=",instance, ", worker#=",worker_num,&
!    ", #workers=",num_workers

    n1=worker_num+num_workers-1
    avail=.false.

    if(instances_info(instance)%send_mode.eq.PCPS_SEND_LINE) then
      !here we allow a worker to be reserved
      do i=worker_num, n1
        if(instances_info(instance)%active_workers(i).eq.WORKER_SUSPENDED .or.&
           pcpsx_worker_modes(1,i).gt.0) return
      enddo
    else  
      !here we count reserved workers as not available
      do i=worker_num, n1
        if(instances_info(instance)%active_workers(i).eq.WORKER_SUSPENDED .or.&
           pcpsx_worker_modes(1,i).ge.0) return
      enddo
    endif

    avail=.true.
    return
  end function pcpsx_are_workers_available

!------------------PCPSX_IS_WORK_GROUP_AVAILABLE---------------------------
! Sees if work group available-for-work
!
! Written July 2003 by Charles C Burch
!----------------------------------------------------------------------
  function pcpsx_is_work_group_available(instance, work_group) result(avail)
    integer, intent(in) :: instance
    integer, intent(in) :: work_group
    logical             :: avail

    integer             :: first_worker, num_workers

    if(work_group.ge.0) then
      call pcpsx_get_work_group_info(instance,work_group, first_worker, &
        num_workers)
    else
      first_worker=1
      num_workers=pcps_num_workers
    endif

    avail=pcpsx_are_workers_available(instance, first_worker, num_workers)
    return
  end function pcpsx_is_work_group_available

!------------------------PCPSX_SET_WORKERS_BUSY------------------------
!  Sets a worker(worker_no) mode to the BUSY with specified instance
!
! Written July 2000 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_set_workers_busy(instance,trace_group,worker_num,num_workers)
    integer, intent(in)           :: worker_num
    integer, intent(in)           :: trace_group
    integer, intent(in)           :: instance
    integer, optional, intent(in) :: num_workers
 
    integer                       :: i_worker, n_worker, n_workers
 
    if(present(num_workers)) then
      n_workers=num_workers
      if(n_workers.lt.1) return
    else
      n_workers=1
    endif  
    n_worker=worker_num
 
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"  pcpsx_set_workers_busy: cpu=",  &
       pcps_current_worker_num,                   &
       ", instance=", instance, ", trace group=",trace_group, &
       ", worker#=", worker_num, ", #workers=", n_workers
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    if(pcps_current_worker_num.gt.0) then
      write(pcps_message,*) "pcpsx_set_proc_busy: called in worker mode(",   &
       pcps_current_worker_num,")"
      call pcpsx_abort(pcps_message)
    endif
 
! --- see if set all workers to busy
 
    if(n_worker.eq.-1) then
      n_worker=1
      n_workers=pcps_num_workers
    endif

    if(n_worker.lt.1 .or. (n_worker+n_workers-1).gt.pcps_num_workers) then
      write(pcps_message,*) "pcpsx_set_workers_busy: " //                     &
       "FATAL ERROR:Invalid processor number(",n_worker,n_workers,")"
      call pcpsx_abort(pcps_message)
    endif
 
    n_workers=n_worker+n_workers-1
    do i_worker=n_worker,n_workers
      if(pcpsx_worker_modes(1,i_worker).gt. 0) then
        write(pcps_message,*)                                                  &
         "pcpsx_set_workers_busy: Attempting to set worker(",                  &
         i_worker,") to BUSY(",instance,"), but already is BUSY(",  &
         pcpsx_worker_modes(1,i_worker),")"
        call pcps_print(pcps_message)
      else
        pcpsx_num_active_workers=pcpsx_num_active_workers+1
        pcpsx_active_workers(pcpsx_num_active_workers)=i_worker
      endif
 
    enddo

    pcpsx_worker_modes(1,n_worker:n_workers)=instance
    pcpsx_worker_modes(2,n_worker:n_workers)=trace_group
    pcpsx_worker_modes(3,n_worker:n_workers)=instances_info(instance)%group_in

    return
  end subroutine pcpsx_set_workers_busy
 
!-------------------------PCPSX_EXPAND_BUFFER-------------------------
! expand a "buffer" of size buffer_size to a size of new_size
!  and copy n_copy bytes.  
!
! Written February 2001 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_expand_buffer_c1 (buffer, buffer_size, n_copy, new_size)
    character (len=1), pointer       :: buffer(:)
    integer          , intent(inout) :: buffer_size
    integer          , intent(in)    :: new_size, n_copy

    character (len=1), pointer       :: copy_buffer(:)
    integer                          :: n, ierr, expanded_size

    if(new_size.le.buffer_size) return

    if(n_copy.le.0 .or. buffer_size.eq.0) then
      if(buffer_size.gt.0) deallocate(buffer,stat=ierr) 
      expanded_size=max(buffer_size+4096, new_size)
      allocate (buffer(1:expanded_size),stat=ierr)
      if(ierr.ne.0) then
        write(pcps_message,*) &
         "error allocating buffer in pcpsx_expand_buffer", ierr
        call pcpsx_abort(pcps_message)
      endif

      buffer_size=expanded_size
      return
    endif

    copy_buffer=>buffer
    n=min(n_copy, buffer_size)
    expanded_size=max(buffer_size+4096, new_size)
    allocate (buffer(1:expanded_size),stat=ierr)
    if(ierr.ne.0) then
      write(pcps_message,*) &
       "error allocating buffer in pcpsx_expand_buffer", ierr
      call pcpsx_abort(pcps_message)
    endif

    buffer_size=expanded_size
    buffer(1:n)=copy_buffer(1:n)
    deallocate(copy_buffer, stat=ierr)
    return

  end subroutine pcpsx_expand_buffer_c1

!-------------------------PCPSX_EXPAND_BUFFER-----------------------
! expand a buffer(keys) of size(n_keys) to a size of n_new
!  and copy n_copy entries.
! Overloaded to support integer, real, double precision
!
! Written February 2002 by Charles C Burch
!---------------------------------------------------------------------
  subroutine pcpsx_expand_buffer_i2(keys, n_keys, n_dim, n_copy, n_new)
    integer, pointer                :: keys(:,:)
    integer         , intent(inout) :: n_keys
    integer         , intent(in)    :: n_copy, n_new, n_dim

    integer, pointer                :: buff(:,:)
    integer                         :: n, istat, n_cpy, n_nw

    if(n_keys.gt.n_new) then
      if(n_dim.eq.size(keys,1)) return
    endif

    n_nw=max(n_keys+10, n_new)
    n_cpy=min(n_keys,n_copy)
    if(n_cpy.le.0) then               !if no copy-simply expand buffer
      if(n_keys.gt.0) deallocate(keys,stat=istat)
      n_keys=n_nw
      allocate(keys(n_dim,n_keys),stat=istat)
      if(istat.ne.0) then
        write(pcps_message,*) &
         "error allocating buffer in pcpsx_expand_buffer_i2", istat
        call pcpsx_abort(pcps_message)
      endif
      return
    endif

! --- allocate copy buffer, copy buffer, deallocate old buff, 
!     alloc new buffer, copy to new buffer, deallocate copy buffer

    n=min(n_dim,size(keys,1))
    buff=>keys
    n_keys=n_nw
    allocate(keys(n_dim,n_keys),stat=istat)
    if(istat.ne.0) then
      write(pcps_message,*) &
       "error allocating buffer in pcpsx_expand_buffer_i2", istat
      call pcpsx_abort(pcps_message)
    endif

    keys(1:n, 1:n_cpy)=buff(1:n, 1:n_cpy)
    deallocate(buff, stat=istat)
    return
  end subroutine pcpsx_expand_buffer_i2
 
  subroutine pcpsx_expand_buffer_r2(keys, n_keys, n_dim, n_copy, n_new)
    real   , pointer                :: keys(:,:)
    integer         , intent(inout) :: n_keys
    integer         , intent(in)    :: n_copy, n_new, n_dim

    real   , pointer                :: buff(:,:)
    integer                         :: n, istat, n_cpy, n_nw

    if(n_keys.gt.n_new) then
      if(n_dim.eq.size(keys,1)) return
    endif

    n_nw=max(n_keys+10, n_new)
    n_cpy=min(n_keys,n_copy)
    if(n_cpy.le.0) then               !if no copy-simply expand buffer
      if(n_keys.gt.0) deallocate(keys,stat=istat)
      n_keys=n_nw
      allocate(keys(n_dim,n_keys),stat=istat)
      if(istat.ne.0) then
        write(pcps_message,*) &
         "error allocating buffer in pcpsx_expand_buffer_r2", istat
        call pcpsx_abort(pcps_message)
      endif
      return
    endif

! --- allocate copy buffer, copy buffer, deallocate old buff, 
!     alloc new buffer, copy to new buffer, deallocate copy buffer

    n=min(n_dim,size(keys,1))
    buff=>keys
    n_keys=n_nw
    allocate(keys(n_dim,n_keys),stat=istat)
    if(istat.ne.0) then
      write(pcps_message,*) &
       "error allocating buffer in pcpsx_expand_buffer_i2", istat
      call pcpsx_abort(pcps_message)
    endif

    keys(1:,1:n_cpy)=buff(1:,1:n_cpy)
    deallocate(buff, stat=istat)
    return
  end subroutine pcpsx_expand_buffer_r2
 
  subroutine pcpsx_expand_buffer_d2(keys, n_keys, n_dim, n_copy, n_new)
    double precision, pointer       :: keys(:,:)
    integer         , intent(inout) :: n_keys
    integer         , intent(in)    :: n_copy, n_new, n_dim

    double precision, pointer       :: buff(:,:)
    integer                         :: n, istat, n_cpy, n_nw

    if(n_keys.gt.n_new) then
      if(n_dim.eq.size(keys,1)) return
    endif

    n_nw=max(n_keys+10, n_new)
    n_cpy=min(n_keys,n_copy)
    if(n_cpy.le.0) then               !if no copy-simply expand buffer
      if(n_keys.gt.0) deallocate(keys,stat=istat)
      n_keys=n_nw
      allocate(keys(n_dim,n_keys),stat=istat)
      if(istat.ne.0) then
        write(pcps_message,*) &
         "error allocating buffer in pcpsx_expand_buffer_d2", istat
        call pcpsx_abort(pcps_message)
      endif
      return
    endif

! --- allocate copy buffer, copy buffer, deallocate old buff, 
!     alloc new buffer, copy to new buffer, deallocate copy buffer

    n=min(n_dim,size(keys,1))
    buff=>keys
    n_keys=n_nw
    allocate(keys(n_dim,n_keys),stat=istat)
    if(istat.ne.0) then
      write(pcps_message,*) &
       "error allocating buffer in pcpsx_expand_buffer_d2", istat
      call pcpsx_abort(pcps_message)
    endif

    keys(1:n, 1:n_cpy)=buff(1:n, 1:n_cpy)
    deallocate(buff, stat=istat)
    return
  end subroutine pcpsx_expand_buffer_d2
 
!------------PCPSX_BROADCAST_TRACES--------------------
! broadcasts group of ntr traces
!
! Written September 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcpsx_broadcast_traces(ntr, max_n, hdr, hdr_len, trc, tr_len)
    integer, intent(inout)           :: ntr
    integer, intent (in)             :: hdr_len, tr_len, max_n
    double precision, intent(inout)  :: hdr(:,:)
    real, intent(inout)              :: trc(:,:)
 
    integer                          :: ierr, ibuff(4), ntr1
    integer                          :: position, i, n

    if(pcps_num_workers.lt.1) then
!     print *,"cpu=",pcps_current_worker_num,", pcpsx_broadcast_traces,ntr=",ntr
      return
    endif
 
    n=ppio_sizeof_integer+                                                     &
     max_n*(hdr_len*ppio_sizeof_double_prec+tr_len*ppio_sizeof_real)
    if(n.gt.pcpsx_buffer_size) &
      call pcpsx_expand_buffer(pcpsx_buffer, pcpsx_buffer_size, 0, n)

    if(pcps_boss_mode) then
 
! --- boss mode
 
      position=0
      ibuff(1)=ntr
      call ppio_pack_integer(ibuff,1,pcpsx_buffer, position)
      call pcpsx_broadcast(pcpsx_buffer, position, 0, ierr)
 
      ntr1=iand(max(ntr,0), NTR_TRACES_MASK)
      if(ntr1.gt.0) then
        position=0
        do i=1, ntr1
          call ppio_pack_double_precision(hdr(1:,i), hdr_len, pcpsx_buffer, &
           position)
          call ppio_pack_real(trc(1:,i), tr_len, pcpsx_buffer, position)
        enddo
        call pcpsx_broadcast(pcpsx_buffer, position, 0, ierr)
      endif
 
    else
 
! --- worker mode
 
      call pcpsx_broadcast(pcpsx_buffer,ppio_sizeof_integer , 0, ierr)
      position=0
      call ppio_unpack_integer(ibuff,1,pcpsx_buffer, position)
      ntr=ibuff(1)
      ntr1=iand(max(ntr,0), NTR_TRACES_MASK)
 
      if(ntr1.gt.0) then
        n=ntr1*(hdr_len*ppio_sizeof_double_prec+tr_len*ppio_sizeof_real)
        call pcpsx_broadcast(pcpsx_buffer, n, 0, ierr)
        position=0
        do i=1, ntr1
          call ppio_unpack_double_precision(hdr(1:,i),hdr_len,pcpsx_buffer,&
           position)
          call ppio_unpack_real(trc(1:,i), tr_len, pcpsx_buffer, position)
        enddo
      endif
    endif
 
    return
  end subroutine pcpsx_broadcast_traces ! (proc, instance, ntr, hdr, trc)
 
!---------------------------PCPSX_SEND_TRACES--------------------------
! ppio_send group of ntr traces of length trlen from processor proc
!
! Written July 2000 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_send_traces(proc, instance, ntr, hdr, hdr_len,              &
     trc, tr_len)
    integer, intent(in)           :: proc, ntr, instance, hdr_len, tr_len
    double precision, intent(in)  :: hdr(:,:)
    real, intent(in)              :: trc(:,:)
 
    integer                       :: ierr, ibuff(3), ntr1
    integer                       :: position, i, n, i1, i2

!   print *,"SEND_TRACES:, cpu, proc, ntr=",pcps_current_worker_num, proc, ntr

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"  pcpsx_send_traces: cpu=",   &
       pcps_current_worker_num,                   &
       ", proc, instance, ntr, hdr_len,trlen, tr#=",        &
       proc,instance,ntr,hdr_len,tr_len
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    ntr1=iand(max(ntr,0), NTR_TRACES_MASK)
!    if(ntr1.gt.0) print *,(ifix(hdr(1,i)), maxval(trc(:,i)),i=1,ntr1)
 
    if(pcps_num_workers.lt.1) return
 
    if(proc.eq.pcps_current_worker_num) then
      write(pcps_message,*) "Warning:",proc, &
        " attempting to send to itself in pcpsx_send_traces"
      call pcps_print(pcps_message,2)
      return
    endif
 
    n=1*ppio_sizeof_integer+                                                   &
     ntr1*(hdr_len*ppio_sizeof_double_prec+tr_len*ppio_sizeof_real)

    if(n.gt.pcpsx_buffer_size) &
      call pcpsx_expand_buffer(pcpsx_buffer, pcpsx_buffer_size, 0, n)
 
    position=0
    ibuff(1)=ntr
    call ppio_pack_integer(ibuff,1,pcpsx_buffer,position)
 
    if(ntr1.gt.0) then
      do i=1, ntr1
        call ppio_pack_double_precision(hdr(1:,i), hdr_len, pcpsx_buffer, &
         position)
        call ppio_pack_real(trc(1:,i), tr_len, pcpsx_buffer, position)
      enddo
    endif

    if(proc.eq.-1) then
      i1=1                     !send to all workers
      i2=pcps_num_workers
    else
      i1=proc                  !send to specific worker
      i2=i1
    endif

    do i=i1,i2
      call ppio_send_packed_data(pcpsx_buffer,position, i, &
       PCPS_CONTROL_TAG, ierr)
      if(ierr.ne.0) then
        write(pcps_message,*) "error in sending in pcpsx_send_traces", ierr
        call pcpsx_abort(pcps_message)
      endif
    enddo

    return
  end subroutine pcpsx_send_traces ! (proc, instance, ntr, hdr, trc)
 
!-------------------------PCPSX_RECEIVE_TRACES-------------------------
!  pcpsx_receive group of ntr traces of length trlen from processor proc
!
! Written July 2000 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_receive_traces(proc, instance, ntr, max_ntr,                &
     hdr, hdr_len, trc, tr_len)
    integer, intent(in)             :: proc, instance,hdr_len, tr_len, max_ntr
    integer, intent(out)            :: ntr
    double precision, intent(inout) :: hdr(:,:)
    real, intent(inout)             :: trc(:,:)
 
    integer, parameter              :: tag=1
    integer                         :: ierr, ibuff(3), ntr1, min_dim
    integer                         :: position, i, n

!   print *,"RECEIVE_TRACES:proc,inst=",proc, instance
 
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"  pcpsx_receive_traces:cpu=",   &
       pcps_current_worker_num,                   &
       ", proc, instance, hdlen,trlen=", proc,instance,hdr_len,tr_len
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    if(pcps_num_workers.lt.1) return

    if(proc.eq.pcps_current_worker_num) then
      write(pcps_message,*) "Warning:",proc, &
        " attempting to receive from itself in pcpsx_receive_traces"
      call pcps_print(pcps_message,2)
      return
    endif
 
    min_dim=min(size(hdr,2),size(trc,2))

    n=1*ppio_sizeof_integer+                                                   &
     max_ntr*(hdr_len*ppio_sizeof_double_prec+tr_len*ppio_sizeof_real)
 
    if(n.gt.pcpsx_buffer_size) &
      call pcpsx_expand_buffer(pcpsx_buffer, pcpsx_buffer_size, 0, n)
 
    call ppio_receive_packed_data(pcpsx_buffer, n, proc,  &
     PCPS_CONTROL_TAG, ierr)
    position=0;
    call ppio_unpack_integer(ibuff,1,pcpsx_buffer,position)
    ntr=ibuff(1)
    ntr1=iand(max(ntr,0), NTR_TRACES_MASK)
    if(ntr1.gt.0) then
      if(ntr1.gt. min_dim) then
        write(pcps_message,*) &
         "dimension of hdr/trc(",min_dim,") exceeded in pcps_receive_traces"
        call pcps_abort(pcps_message)
      endif

      do i=1, ntr1
        call ppio_unpack_double_precision(hdr(1:,i), hdr_len, pcpsx_buffer, &
         position)
        call ppio_unpack_real(trc(1:,i), tr_len, pcpsx_buffer, position)
      enddo
    endif
!   print *,"receive:ntr=",ntr
 
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_receive_traces-exit: cpu=",  &
       pcps_current_worker_num,", ntr=",ntr
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    return
  end subroutine pcpsx_receive_traces
 
!===============PCPSX_SUM_RECEIVE_TRACES================
! receive traces from each worker and sum them together
!
! Written July 2000 by Charles C Burch
!=======================================================
  subroutine pcpsx_sum_receive_traces (obj, first_worker, num_workers, &
     ntr, max_ntr, hdr, hdr_len, trc, tr_len)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer         , intent(in)             :: first_worker
    integer         , intent(in)             :: num_workers
    integer         , intent(inout)          :: ntr
    integer         , intent(in)             :: max_ntr
    double precision, intent(inout)          :: hdr(:,:)
    integer         , intent(in)             :: hdr_len
    real            , intent(inout)          :: trc(:,:)
    integer         , intent(in)             :: tr_len
 
    integer                       :: hd_len2, tr_len2
    integer                       :: i_proc, ntr1, ntr2, ierr
    double precision, allocatable :: hdr_sv(:,:)
    real, allocatable             :: trc_sv(:,:)

    hd_len2=size(hdr,DIM=2)
    allocate(hdr_sv(hdr_len, hd_len2), stat=ierr)
    if(ierr.ne.0) then
      write(pcps_message,*)                                                  &
       "Unable to allocate hdr_sv in pcpsx_sum_receive_traces", ierr
      call pcpsx_abort(pcps_message)
    endif
 
    tr_len2=size(trc,DIM=2)
    allocate(trc_sv(tr_len, tr_len2), stat=ierr)
    if(ierr.ne.0) then
      write(pcps_message,*)                                                  &
       "Unable to allocate trc_sv in pcpsx_sum_receive_traces", ierr
      call pcpsx_abort(pcps_message)
    endif

    ntr1=ntr        !size of output
    ntr=0
    ntr2=0
    do i_proc=first_worker,first_worker+num_workers-1
      if (obj%active_workers(i_proc).ne.WORKER_ACTIVE) then 
        if(ntr2.eq.0) ntr2=NO_MORE_TRACES
        cycle
      endif
  
      if(ntr.eq.0) then
        if(i_proc.gt.0) call pcpsx_receive_traces(i_proc,obj%instance, &
                      ntr1,max_ntr,hdr,hdr_len,trc,tr_len)
        if(ntr1.gt.0) ntr=iand(ntr1,NTR_TRACES_MASK)
        ! print*,"receive sum0:",ntr,ntr1,trc(1,1)
      else
        call pcpsx_receive_traces(i_proc,obj%instance,ntr1,max_ntr,           &
         hdr_sv,hdr_len, trc_sv,tr_len)
        if(ntr1.gt.0) then
          ntr1=iand(ntr1,NTR_TRACES_MASK)
          if(ntr1.gt.ntr) then
            trc(:,ntr+1:ntr1)=0
            ntr=ntr1
          endif
          trc(:,1:ntr1)=trc(:,1:ntr1)+trc_sv(:,1:ntr1)
        endif
      ! print *,"receive_sum1:",ntr,ntr1,trc(1,1),trc_sv(1:1)
      endif

      if(ntr1.le.0) then
        if(ntr2.eq.0) ntr2=ntr1
        if(ntr1.eq.NO_MORE_TRACES)  obj%active_workers(i_proc)=WORKER_EOF  
      endif
    enddo
 
    deallocate (hdr_sv)
    deallocate (trc_sv)
    if(ntr.eq.0) ntr=ntr2
    ! print *,"sum_rec",ntr,trc(1,1)
    return
  end subroutine pcpsx_sum_receive_traces
 
!=============PCPSX_GATHER_RECEIVE_TRACES===============
! Receive traces from each work and concantenate them
!
! Written July 2000 by Charles C Burch
!=======================================================
  subroutine pcpsx_gather_receive_traces (obj, first_worker, num_workers, ntr, &
    max_ntr, hdr, hdr_len, trc, tr_len)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer         , intent(in)             :: first_worker
    integer         , intent(in)             :: num_workers
    integer         , intent(inout)          :: ntr
    integer         , intent(in)             :: max_ntr
    double precision, intent(inout)          :: hdr(:,:)
    integer         , intent(in)             :: hdr_len
    real            , intent(inout)          :: trc(:,:)
    integer         , intent(in)             :: tr_len
 
    integer                                  :: i_proc, ntr1, ntr2

    do i_proc=1, pcps_num_workers     !see if all workers have received NMT
      if(obj%active_workers(i_proc).ne.WORKER_EOF) exit 
    enddo

    if(i_proc.gt.pcps_num_workers) then
      ntr=NO_MORE_TRACES
      return
    endif

    ntr1=ntr
    ntr2=0
    ntr=0

    do i_proc=first_worker, first_worker+num_workers-1
      if (obj%active_workers(i_proc).ne.WORKER_ACTIVE) cycle
      if(i_proc.gt.0) then
        call pcpsx_receive_traces(i_proc, obj%instance, ntr1, max_ntr,         &
         hdr(:,ntr+1:), hdr_len, trc(:,ntr+1:), tr_len)
      endif

      if(ntr1.gt.0) then
        ntr=ntr+iand(ntr1,NTR_TRACES_MASK)
      else
        if(ntr2.eq.0) ntr2=ntr1
! ----- see if this worker is done
        if(ntr1.eq.NO_MORE_TRACES)  obj%active_workers(i_proc)=WORKER_EOF  
      endif

    enddo
 
    if(ntr.eq.0 .and. ntr2.lt.0) ntr=ntr2
    return
  end subroutine pcpsx_gather_receive_traces
 
!========================= PCPSX_BUNCH_TRACES====================
! Gather traces and headers by header word i_hdr (usually 1)
!
! Written October 2000 by Charles C Burch
! New version written March 2001 by Charles C Burch
!================================================================
  subroutine pcpsx_bunch_traces(obj, ntr, hdrs, trcs, i_hdr)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer         , intent(inout)          :: ntr
    integer         , intent(in)             :: i_hdr
    double precision, intent(inout)          :: hdrs(:,:)
    real            , intent(inout)          :: trcs(:,:)
 
    integer       :: i,istat, lrecl, irec, itrace, ntr_in, ierr
 
!   print *,"pcpsx_bunch_header_traces, worker_no, ntr, hdr=",                 &
!    pcps_current_worker_num, ntr, idnint(hdrs(i_hdr,1:max(ntr,1)))
 
! --------------- Set  up work buffers if not already done----------------
 
    if(obj%bunch_size.lt.0) then
      if(obj%hdr_len_out.eq.0) obj%hdr_len_out=size(hdrs, dim=1)
      if(obj%tr_len_out.eq.0) obj%tr_len_out=size(trcs, dim=1)
      obj%bunch_size=size(hdrs, dim=2)
      call pcpsx_expand_buffer(obj%keys,obj%bunch_keys_size,1,0,20)
      obj%keys(1,1:)=-1 
!     print *,"beg_indx, sort_size=",obj%beg_indx, obj%bunch_size
    endif

    if(pcpsx_trace_mode.gt.0 ) then
      if(ntr.gt.0) then 
        call pcpsx_put_trace_list(obj%instance,hdrs(i_hdr,1),-ntr,"b")
      else if(ntr.eq.BUNCH_EOF) then
        call pcpsx_put_trace_list(obj%instance,hdrs(i_hdr,1),0,"b")
      endif
    endif

! -------------------Initialization done, see at EOF ---------------------

    lrecl=pcps_sizeof_double_prec*obj%hdr_len_out+                             &
     pcps_sizeof_real*obj%tr_len_out
 
! --- if at EOF of input, simply extract traces
 
    if(ntr.eq.BUNCH_EOF) then
!     print *,"EOF, beg tr#=",obj%beg_indx
      ntr=0
      do while(ntr.lt.obj%bunch_size)
        irec=-1                !find trace with minumum key
        do i=1,obj%bunch_keys_size
          if(obj%keys(1,i).ge.0) then
            if(irec.lt.0) then
              irec=i
            else
              if(obj%keys(1,i).lt.obj%keys(1,irec)) irec=i
            endif
          endif
        enddo

        if(irec.lt.0) goto 900   !exit if no more traces available

        if(obj%keys(1,irec).ne.obj%beg_indx) then
          write(pcps_message,*) "Warning: bunch output trace(",obj%beg_indx, &
           ") was missing"
          call pcps_print(pcps_message)
        endif  

        obj%keys(1,irec)=-1
        ntr=ntr+1 
        ierr=0
        istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
        ierr=ierr+istat
        istat=cio_fread(hdrs(1,ntr),pcps_sizeof_double_prec,                  &
         obj%hdr_len_out, obj%bunch_flnm)
        ierr=ierr+(istat-obj%hdr_len_out)
        istat=cio_fread(trcs(1,ntr),pcps_sizeof_real,                         &
         obj%tr_len_out, obj%bunch_flnm)
        ierr=ierr+(istat-obj%tr_len_out)
        obj%beg_indx=hdrs(i_hdr,ntr)+1
        if(ierr .ne. 0 ) then ! === we had a read error somehow.
          ntr=FATAL_ERROR
          goto 900 
        endif
      enddo   
      goto 900
    endif

! --- not at eof, Save any incoming traces
 
    ntr_in=ntr
    ntr=0
    if(ntr_in.gt.0) then   !incoming traces
      do itrace=1,ntr_in
        i=hdrs(i_hdr,itrace)
        if(i.eq.obj%beg_indx) then
          ntr=ntr+1            !this trace is the next one expected
          obj%beg_indx=obj%beg_indx+1
          if(ntr.ne.itrace) then
            hdrs(1:,ntr)=hdrs(1:,itrace)
            trcs(1:,ntr)=trcs(1:,itrace)
          endif
          cycle                !process next trace
        endif

        if(i.lt.obj%beg_indx) then           !see if before active data
          write(pcps_message,*) "WARNING:hdr(",i,                 &
           ") less than beg_indx(",obj%beg_indx,                           &
           ") in bunch_traces-trace skipped"
          call pcps_print(pcps_message)
          cycle
        endif

        irec=-1                !find available spot on disk to store this trace
        do i=1,obj%bunch_keys_size
          if(obj%keys(1,i).lt.0) then
            irec=i
            exit
          endif
        enddo

        if(irec.lt.0) then
          irec=obj%bunch_keys_size+1    !all spots used, expand the spots
          call pcpsx_expand_buffer(obj%keys,obj%bunch_keys_size, 1,  &
            obj%bunch_keys_size, obj%bunch_keys_size+5)
          obj%keys(1,irec:obj%bunch_keys_size)=-1
        endif

! --- save key info and write to disk
        obj%keys(1,irec)=hdrs(i_hdr,itrace) 
        ierr=0
        istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
        ierr=ierr+istat
        istat=cio_fwrite(hdrs(1,itrace),pcps_sizeof_double_prec,              &
         obj%hdr_len_out, obj%bunch_flnm)
        ierr=ierr+istat-obj%hdr_len_out
        istat=cio_fwrite(trcs(1,itrace),pcps_sizeof_real,                     &
         obj%tr_len_out, obj%bunch_flnm)
        ierr=ierr+istat-obj%tr_len_out
        if(ierr .ne. 0 ) then ! === we had a write error somehow.
          ntr=FATAL_ERROR
          goto 900 
        endif
      enddo
    endif
 
! --- See if any more outgoing traces ready

    do while(ntr.lt.obj%bunch_size)
      irec=-1
      do i=1,obj%bunch_keys_size
        if(obj%keys(1,i).eq.obj%beg_indx) then
          irec=i                 !found next trace wanted
          exit
        endif
      enddo
      if(irec.lt.0)  goto 900    !exit if next trace not avail

      obj%keys(1,irec)=-1
      ntr=ntr+1
      ierr=0
      istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
      ierr=ierr+istat
      istat=cio_fread(hdrs(1,ntr),pcps_sizeof_double_prec,                &
       obj%hdr_len_out, obj%bunch_flnm)
      ierr=ierr+istat-obj%hdr_len_out
      istat=cio_fread(trcs(1,ntr),pcps_sizeof_real,                       &
       obj%tr_len_out, obj%bunch_flnm)
      ierr=ierr+istat-obj%tr_len_out
      if(ierr .ne. 0 ) then ! === we had a write error somehow.
        ntr=FATAL_ERROR
        goto 900 
      endif
      obj%beg_indx=obj%beg_indx+1
      
    enddo
 
900 if(ntr.eq.0) ntr=NEED_TRACES
    if(pcpsx_trace_mode.gt.0 .and. ntr.gt.0) then 
      call pcpsx_put_trace_list(obj%instance,hdrs(i_hdr,1),ntr,"b")
    endif
    
!   print *,"exit pcpsx_bunch_traces, ntr=",ntr,"hdrs=", &
!    ntr, idnint(hdrs(i_hdr,1:max(ntr,1)))
!   print *," "
    return
  end subroutine pcpsx_bunch_traces
 
!========================== PCPSX_BUNCH_GROUPS =======================
! Gather groups of traces and headers by header word i_hdr (usually 3)
!
! Written November 2000 by Charles C Burch 
! New version written March 2001 by Charles C Burch
!======================================================================
  subroutine pcpsx_bunch_groups(obj, ntr, hdrs, trcs, i_hdr)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer         , intent(inout) :: ntr
    integer         , intent(in)    :: i_hdr
    double precision, intent(inout) :: hdrs(:,:)
    real            , intent(inout) :: trcs(:,:)
 
    integer                         :: i,istat, lrecl, irec, ierr
 
!   print *,"pcpsx_bunch_header_traces, worker_no, ntr, hdr=",                 &
!   pcps_current_worker_num, ntr, idnint(hdrs(i_hdr,1:max(ntr,1)))
 
! --------------- Set  up work buffers if not already done -----------------
 
    if(obj%bunch_size.lt.0) then
      if(obj%hdr_len_out.eq.0) obj%hdr_len_out=size(hdrs, dim=1)
      if(obj%tr_len_out.eq.0) obj%tr_len_out=size(trcs, dim=1)
      obj%bunch_size=size(hdrs, dim=2)

      call pcpsx_expand_buffer(obj%keys,obj%bunch_keys_size,2,0,20)
      obj%keys(1,1:)=-1 
    endif

    if(pcpsx_trace_mode.gt.0 ) then
      if(ntr.gt.0) then 
        call pcpsx_put_trace_list(obj%instance,hdrs(i_hdr,1),-ntr,"b")
      else if(ntr.eq.BUNCH_EOF) then
        call pcpsx_put_trace_list(obj%instance,hdrs(i_hdr,1),0,"b")
      endif
    endif

! ---------- initialization done - now get to actual work---------------
 
    lrecl=(pcps_sizeof_double_prec*obj%hdr_len_out+                            &
     pcps_sizeof_real*obj%tr_len_out     ) * obj%bunch_size
 
! --- if at EOF of input, simply extract remaining groups
 
    if(ntr.eq.BUNCH_EOF) then
!     print *,"EOF, beg tr#=",obj%beg_indx

      irec=-1                !find group with minumum key
      do i=1,obj%bunch_keys_size
        if(obj%keys(1,i).ge.0) then
          if(irec.lt.0) then
            irec=i
          else
            if(obj%keys(1,i).lt.obj%keys(1,irec)) irec=i
          endif
        endif
      enddo

      if(irec.lt.0) then   !see no groups available
        ntr=0
        goto 900
      endif

      if(obj%keys(1,irec).ne.obj%beg_indx) then
        write(pcps_message,*) "Warning: bunch output group(",obj%beg_indx, &
         ") was missing"
        call pcps_print(pcps_message)
      endif  

      ntr=obj%keys(2,irec)     !get next group
      obj%keys(1,irec)=-1
      ierr=0
      istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
      ierr=ierr+istat
      istat=cio_fread(hdrs(1,1),pcps_sizeof_double_prec,                      &
       obj%hdr_len_out*ntr, obj%bunch_flnm)
      ierr=ierr+istat-obj%hdr_len_out*ntr
      istat=cio_fread(trcs(1,1),pcps_sizeof_real,                             &
       obj%tr_len_out*ntr, obj%bunch_flnm)
      ierr=ierr+istat-obj%tr_len_out*ntr
      if(ierr .ne. 0 ) then ! --- error reading
        ntr=FATAL_ERROR
        goto 900
      endif
      obj%beg_indx=hdrs(i_hdr,ntr)+1
      goto 900
    endif
 
! --- now either have incoming traces or requesting traces

    if(ntr.gt.0) then   !incoming traces
! see current traces are expected group-if so setup for next group and return
      i=hdrs(i_hdr,1) 
      if(i.eq.obj%beg_indx) then
        obj%beg_indx=hdrs(i_hdr,ntr)+1  !current traces are those expected
        goto 900
      endif
 
! Save incoming traces to disk as they are not that expected
      irec=-1                !find available spot on disk
      do i=1,obj%bunch_keys_size
        if(obj%keys(1,i).lt.0) then
          irec=i
          exit
        endif
      enddo

      if(irec.lt.0) then
        irec=obj%bunch_keys_size+1    !all spots used, expand the spots
        call pcpsx_expand_buffer(obj%keys,obj%bunch_keys_size,2,  &
          obj%bunch_keys_size,obj%bunch_keys_size+5)
        obj%keys(1,irec:obj%bunch_keys_size)=-1
      endif

      obj%keys(1,irec)=hdrs(i_hdr,1)    !save key info and write to disk
      obj%keys(2,irec)=ntr
      ierr=0
      istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
      ierr=ierr+istat
      istat=cio_fwrite(hdrs(1,1),pcps_sizeof_double_prec,                      &
       obj%hdr_len_out*ntr, obj%bunch_flnm)
      ierr=ierr + istat - obj%hdr_len_out*ntr
      istat=cio_fwrite(trcs(1,1),pcps_sizeof_real,                             &
       obj%tr_len_out*ntr, obj%bunch_flnm)
      ierr=ierr + istat - obj%tr_len_out*ntr
      if(ierr .ne. 0 ) then ! --- error writing
        ntr=FATAL_ERROR
        goto 900
      endif

      ntr=0
      goto 900
    endif

! -- at this point next group requested - try to get next group from disk

    do irec=1,obj%bunch_keys_size
      if(obj%keys(1,irec).eq.obj%beg_indx) then
        ntr=obj%keys(2,irec)       !found it
        obj%keys(1,irec)=-1
        ierr=0
        istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
        ierr=ierr+istat
        istat=cio_fread(hdrs(1,1),pcps_sizeof_double_prec,                    &
         obj%hdr_len_out*ntr, obj%bunch_flnm)
        ierr=ierr+istat - obj%hdr_len_out*ntr
        istat=cio_fread(trcs(1,1),pcps_sizeof_real,                           &
         obj%tr_len_out*ntr, obj%bunch_flnm)
        ierr=ierr+istat - obj%tr_len_out*ntr
        obj%beg_indx=hdrs(i_hdr,ntr)+1
        if(ierr .ne. 0 ) then ! --- error reading
          ntr=FATAL_ERROR
        endif
        goto 900
      endif
    enddo
    ntr=0        !not here
 
900 if(ntr.eq.0) ntr=NEED_TRACES
    if(pcpsx_trace_mode.gt.0 .and. ntr.gt.0) then 
      call pcpsx_put_trace_list(obj%instance,hdrs(i_hdr,1),ntr,"b")
    endif

!   print *,"exit pcpsx_bunch_groups, ntr=",ntr,"hdrs=",                       &
!    ntr, idnint(hdrs(i_hdr,1:max(ntr,1)))
!   print *," "
    return
  end subroutine pcpsx_bunch_groups

!======================PCPSX_BUNCH_TRACE_GROUPS====================
! Gather groups of traces and headers 
!
! Written August 2000 by Charles C Burch 
!==================================================================
  subroutine pcpsx_bunch_trace_groups(obj, ntr, hdrs, trcs)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer         , intent(inout)          :: ntr
    double precision, intent(inout)          :: hdrs(:,:)
    real            , intent(inout)          :: trcs(:,:)

    integer                         :: i, istat, lrecl, irec, min_val,ierr
 
!   print *,"pcpsx_bunch_header_traces, worker_no, ntr, group_num, hdrs=",     &
!   pcps_current_worker_num,ntr,pcpsx_group_num,                               &
!    ( ifix(hdrs(1,i)), i=1,max(ntr,1))
 
! --------------- Set  up work buffers if not already done -----------------
 
    if(obj%bunch_size.lt.0) then
      if(obj%hdr_len_out.eq.0) obj%hdr_len_out=size(hdrs, dim=1)
      if(obj%tr_len_out.eq.0) obj%tr_len_out=size(trcs, dim=1)
      obj%bunch_size=size(hdrs, dim=2)
      obj%group_out=1

      i=max(10,pcps_num_workers)
      call pcpsx_expand_buffer(obj%keys,obj%bunch_keys_size,3,0,i)
      obj%keys(1,1:)=-1 
    endif

    if(pcpsx_trace_mode.gt.0 ) then
      if(ntr.gt.0) then 
        call pcpsx_put_trace_list(obj%instance,hdrs,-ntr,"b")
      else if(ntr.eq.BUNCH_EOF) then
        call pcpsx_put_trace_list(obj%instance,hdrs,0,"b")
      endif
    endif

! ---------- initialization done - now get to actual work---------------
! - Note a ntr=0 meand this is the end of a trace group
 
    lrecl=(pcps_sizeof_double_prec*obj%hdr_len_out+                            &
     pcps_sizeof_real*obj%tr_len_out     ) * obj%bunch_size
 
! ------- if at EOF of input, simply extract remaining groups ----------
 
    if(ntr.eq.BUNCH_EOF) then
!     print *,"EOF, beg tr#=",obj%beg_indx

      do while(.true.)

        irec=-1                !find group with minimum key
        do i=1,obj%bunch_nkeys
          if(obj%keys(1,i).ge.0) then
            if(irec.lt.0) then
              irec=i
              min_val=obj%keys(1,irec)
            else
              if( obj%keys(1,i).lt.min_val .or.  &
                 (obj%keys(1,i).eq.min_val .and. &
                  obj%keys(3,i).lt.obj%keys(3,irec)) ) then
                irec=i
                min_val=obj%keys(1,irec)
              endif
            endif
          endif
        enddo

        if(irec.lt.0) then   !see no groups available
          ntr=NEED_TRACES
          goto 900
        endif

        if(obj%keys(1,irec).ne.obj%group_out) then
          write(pcps_message,*) "Warning: bunch output group(",obj%group_out, &
           ") was missing"
          call pcps_print(pcps_message)
          obj%group_out=obj%keys(1,irec)
        endif
        
        ntr=obj%keys(2,irec)   
        obj%keys(1,irec)=-1

        do while(obj%bunch_nkeys.gt.0)
          if(obj%keys(1,obj%bunch_nkeys).lt.0) then
            obj%bunch_nkeys=obj%bunch_nkeys-1
          else
            exit
          endif
        enddo

        if(ntr.gt.0) then   !reember ntr=0 means end of trace group
          ierr=0
          istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
          ierr=ierr+istat
          istat=cio_fread(hdrs(1,1),pcps_sizeof_double_prec,                  &
           obj%hdr_len_out*ntr, obj%bunch_flnm)
          ierr=ierr+istat - obj%hdr_len_out*ntr
          istat=cio_fread(trcs(1,1),pcps_sizeof_real,                         &
           obj%tr_len_out*ntr, obj%bunch_flnm)
          ierr=ierr+istat - obj%tr_len_out*ntr
          if(obj%generator_mode.eq.0) obj%group_out=obj%group_out+1
          if(ierr .ne. 0 ) ntr=FATAL_ERROR
          goto 900
        endif

      obj%group_out=obj%group_out+1       !next group
      enddo
    endif
 
! -------- now either have incoming traces or requesting traces ---------

!   print *,"bunch_group, ntr, pcpsx_group_num, seq, group_out=",&
!    ntr,pcpsx_group_num,pcpsx_group_num_seq,obj%group_out

    if(ntr.eq.BUNCH_EOG) then     !input ntr was NEED_TRACES means end-of-group
      if(obj%generator_mode.eq.0) then
        ntr=NEED_TRACES
        goto 900
      endif
      ntr=0                        !for data, ntr=0 means end of group
      
      if(obj%group_out.eq.pcpsx_group_num) then !see if in current group
        obj%group_out=obj%group_out+1           !indicates current group done
        ntr=BUNCH_NEED_TRACES                   !convert to retrieve traces
      endif
      pcpsx_group_num_seq=huge(pcpsx_group_num_seq) !make as large as poss.
    endif

    if(ntr.ge.0) then   !incoming traces
! --- see if current traces are expected group-if so return
      if(pcpsx_group_num.eq.obj%group_out) then
        if(obj%generator_mode.eq.0) obj%group_out=obj%group_out+1
        if(ntr.eq.0) ntr=NEED_TRACES
        goto 900
      endif
 
! ------- Save incoming traces to disk as they are not that expected ------
      irec=-1                !find available spot on disk
      do i=1,obj%bunch_keys_size
        if(obj%keys(1,i).lt.0) then
          irec=i
          exit
        endif
      enddo

! ---------- see if all spots used, if so expand the spots ------------
      if(irec.lt.0) then 
        irec=obj%bunch_keys_size+1    
        call pcpsx_expand_buffer(obj%keys,obj%bunch_keys_size,3,  &
          obj%bunch_keys_size,obj%bunch_keys_size+5)
        obj%keys(1,irec:obj%bunch_keys_size)=-1
      endif

! --- nkeys indicate where active
      if(irec.gt.obj%bunch_nkeys) obj%bunch_nkeys=irec

! -------------- save key info and write to disk --------------------
      obj%keys(1,irec)=pcpsx_group_num    
      obj%keys(2,irec)=ntr
      obj%keys(3,irec)=pcpsx_group_num_seq

      if(ntr.gt.0) then
        ierr=0
        istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
        ierr=ierr+istat
        istat=cio_fwrite(hdrs(1,1),pcps_sizeof_double_prec,                  &
         obj%hdr_len_out*ntr, obj%bunch_flnm)
        ierr=ierr+istat - obj%hdr_len_out*ntr
        istat=cio_fwrite(trcs(1,1),pcps_sizeof_real,                         &
         obj%tr_len_out*ntr, obj%bunch_flnm)
        ierr=ierr+istat - obj%tr_len_out*ntr
        if(ierr .ne. 0 ) then
          ntr=FATAL_ERROR
          goto 900
        endif
      endif

      ntr=BUNCH_NEED_TRACES
    endif

! ---- at this point next group requested - see where might be on disk ----
    do while(.true.)
      irec=-1
      do i=1,obj%bunch_nkeys
        if(obj%keys(1,i).eq.obj%group_out) then
          if(irec.lt.0) then
            irec=i
            min_val=obj%keys(3,i)
          else
            if(obj%keys(3,i).lt.min_val) then
               irec=i
               min_val=obj%keys(1,i)
            endif
          endif
        endif
      enddo

      if(irec .lt.0) then
        ntr=NEED_TRACES                  !next group not yet available
        goto 900
      endif

! ------------ next group is available, read it and return it ------------
      ntr=obj%keys(2,irec)
      obj%keys(1,irec)=-1

      do while(obj%bunch_nkeys.gt.0)    !see if can compress n_keys
        if(obj%keys(1,obj%bunch_nkeys).lt.0) then
          obj%bunch_nkeys=obj%bunch_nkeys-1
        else
          exit
        endif
      enddo

      if(ntr.gt.0) then          !read from disk and return
        ierr=0
        istat=cio_fseek(obj%bunch_flnm,lrecl,irec-1,0,0)
        ierr=istat
        istat=cio_fread(hdrs(1,1),pcps_sizeof_double_prec,                    &
         obj%hdr_len_out*ntr, obj%bunch_flnm)
        ierr=ierr+istat - obj%hdr_len_out*ntr
        istat=cio_fread(trcs(1,1),pcps_sizeof_real,                           &
         obj%tr_len_out*ntr, obj%bunch_flnm)
        ierr=ierr+istat - obj%tr_len_out*ntr
        if(obj%generator_mode.eq.0) obj%group_out=obj%group_out+1  !next group
        if(ierr .ne. 0 ) ntr=FATAL_ERROR
        goto 900
      endif

      obj%group_out=obj%group_out+1  !was end of group--set up for next group
    enddo
 
900 if(pcpsx_trace_mode.gt.0 .and. ntr.gt.0) then 
      call pcpsx_put_trace_list(obj%instance,hdrs,ntr,"b")
    endif

!   print *,"exit pcpsx_bunch_trace_groups, ntr=",ntr,"hdrs=",               &
!    (ifix(hdrs(1,i)), maxval(trcs(:,i)), i=1,max(ntr,1))
!   print *," "
    return
  end subroutine pcpsx_bunch_trace_groups

!===================== PCPSX_MERGE_TRACES ==========================
! Merge of pcps output data
! on input ntr, worker_num has info on input source
! on output ntr, worker_num has info on output source
! ntr can only be 1, BUNCH_NEED_TRACES, BUNCH_EOF
! (hdrs_in,trcs_in) can be same as (hdrs_out, trcs_out)
!
! Written February 2002 by Charles C Burch 
!==================================================================
  subroutine pcpsx_merge_traces(obj, worker_num, ntr, hdrs_in, trcs_in, &
                                                      hdrs_out, trcs_out)
    type (pcpsx_parallel_group_struct), pointer :: obj
    integer         , intent(inout)          :: worker_num
    integer         , intent(inout)          :: ntr
    double precision, intent(in   )          :: hdrs_in(:)
    real            , intent(in   )          :: trcs_in(:)
    double precision, intent(out  )          :: hdrs_out(:)
    real            , intent(out  )          :: trcs_out(:)

    integer   :: i, i_min
 
!   print *,"merge_in: ntr, worker, hdrs=",ntr, worker_num,idnint(hdrs_in(1:4))

    if(obj%bunch_keys_size.le.0) then 
      if(obj%hdr_len_out.eq.0) obj%hdr_len_out=size(hdrs_in, dim=1)
      if(obj%tr_len_out.eq.0) obj%tr_len_out=size(trcs_in, dim=1)
      call pcpsx_expand_buffer(obj%keys,obj%bunch_keys_size,4,0,&
       pcps_num_workers+2)
      obj%keys=BUNCH_NEED_TRACES
      if(obj%receive_mode.eq.PCPS_RECEIVE_MERGE) obj%keys(:,1)=BUNCH_EOF
      i=0
      call pcpsx_expand_buffer(obj%sort_hdrs,i,obj%hdr_len_out,0,&
       pcps_num_workers+1)
      i=0
      call pcpsx_expand_buffer(obj%sort_trcs,i,obj%tr_len_out,0,&
       pcps_num_workers+1)
    endif

    if(pcpsx_trace_mode.gt.0 ) then
      if(ntr.gt.0) then 
        call pcpsx_put_trace_list(obj%instance,hdrs_in,-ntr,"b")
      else if(ntr.eq.BUNCH_EOF) then
        call pcpsx_put_trace_list(obj%instance,hdrs_in,0,"b")
      endif
    endif

    if(ntr.gt.0) then
      if(ntr.gt.1) then
        write(pcps_message,*) "ntr>1 in pcpsx_merge-truncated to 1"
        call pcps_print(pcps_message)
        ntr=1
      endif
    
! - keys(1:3,i) has sort keys for data on sort_hdrs(:,i)    
! - codes for keys(4,:) BUNCH_NEED_TRACES, BUNCH_HAVE_DATA, BUNCH_EOF

      if(obj%keys(4,worker_num+1).ne.BUNCH_NEED_TRACES) &
        call pcpsx_abort("data conflict in pcps_merge_traces")
        
      obj%sort_hdrs(:, worker_num+1)=  hdrs_in(:)
      obj%sort_trcs(:, worker_num+1)=  trcs_in(:)

      obj%keys(1,worker_num+1)= &
       mth_bin_number(obj%merge_int_1, obj%merge_inc_1, &
                      obj%sort_hdrs(obj%merge_hdr_1,worker_num+1))
      obj%keys(2,worker_num+1)= &
       mth_bin_number(obj%merge_int_2, obj%merge_inc_2, &
                      obj%sort_hdrs(obj%merge_hdr_2,worker_num+1))
      obj%keys(3,worker_num+1)= &
       mth_bin_number(obj%merge_int_3, obj%merge_inc_3, &
                      obj%sort_hdrs(obj%merge_hdr_3,worker_num+1))
      obj%keys(4,worker_num+1)=BUNCH_HAVE_DATA

    else if(ntr.eq.BUNCH_EOF) then
      if(obj%keys(4,worker_num+1).ne.BUNCH_NEED_TRACES) &
        call pcpsx_abort("data conflict in pcps_merge_traces")
      obj%keys(4,worker_num+1)=BUNCH_EOF
      
    else 
      if(ntr.ne.BUNCH_NEED_TRACES) then
        write(pcps_message,*) "Invalid ntr(",ntr,") in pcpsx_merge_traces"
        call pcps_print(pcps_message)
      endif
    endif

! - try to retrive data
    i_min=-1
    do i=0, pcps_num_workers
      if(obj%keys(4,i+1).eq.BUNCH_EOF) cycle
      if(obj%keys(4,i+1).eq.BUNCH_NEED_TRACES) then
        ntr=NEED_TRACES
        goto 900
      endif
       
      if(i_min.lt.0) then
        i_min=i                 !first time through
        cycle
      endif

! --- now compare merge keys

      if(obj%keys(1,i+1).lt.obj%keys(1,i_min+1)) then
        i_min=i
        cycle
      endif
      if(obj%keys(1,i+1).gt.obj%keys(1,i_min+1)) cycle 

      if(obj%keys(2,i+1).lt.obj%keys(2,i_min+1)) then
        i_min=i
        cycle
      endif
      if(obj%keys(2,i+1).gt.obj%keys(2,i_min+1)) cycle 

      if(obj%keys(3,i+1).lt.obj%keys(3,i_min+1)) then
        i_min=i
        cycle
      endif
      if(obj%keys(3,i+1).gt.obj%keys(3,i_min+1)) cycle 
      if(obj%sort_hdrs(1,i+1).lt.obj%sort_hdrs(1,i_min+1)) i_min=i
    enddo

    if(i_min.lt.0) then
      ntr=NO_MORE_TRACES
      goto 900
    endif

    hdrs_out=obj%sort_hdrs(:,i_min+1)
    trcs_out=obj%sort_trcs(:,i_min+1)
    obj%keys(4,i_min+1)=BUNCH_NEED_TRACES
    worker_num=i_min
    ntr=1
    
900 if(pcpsx_trace_mode.gt.0 .and. ntr.gt.0) then 
      call pcpsx_put_trace_list(obj%instance,hdrs_out,ntr,"b")
    endif

!   print *,"merge_out: ntr, hdrs=",ntr, idnint(hdrs_out(1:4))    
    return
  end subroutine pcpsx_merge_traces

!==============PCPSX_SORT_TRACES=====================
! Sort traces and headers by header word i_hdr
!
! Written August 2000 by Charles C Burch
!====================================================
  subroutine pcpsx_sort_traces(n, hdr, trc, i_hdr)
    integer         , intent(in)    :: n
    integer         , intent(in)    :: i_hdr
    double precision, intent(inout) :: hdr(:,:)
    real            , intent(inout) :: trc(:,:)
 
    double precision, allocatable   :: hdr_sv(:)
    real            , allocatable   :: trc_sv(:)
    integer                         :: n1, i, j1, j2
 
    if(n.lt.1) return
 
    n1=size(hdr,DIM=1)
    allocate (hdr_sv(n1), stat=i)
    if(i.ne.0) then
      write(pcps_message,*)                                                  &
       "Unable to allocate hdr_sv in pcpsx_sort_traces", i
      call pcpsx_abort(pcps_message)
    endif
 
    n1=size(trc,DIM=1)
    allocate (trc_sv(n1), stat=i)
    if(i.ne.0) then
      write(pcps_message,*)                                                   &
       "Unable to allocate trc_sv in pcpsx_sort_traces", i
      call pcpsx_abort(pcps_message)
    endif
 
    n1=n/2
    do while(n1.gt.0)
      do i=n1+1, n
        j1=i-n1
        do while(j1.gt.0)
          j2=j1+n1
          if(hdr(i_hdr,j1).lt.hdr(i_hdr,j2)) exit
          if(hdr(i_hdr,j1).eq.hdr(i_hdr,j2)) then
            if(hdr(i_hdr,1).le.hdr(i_hdr,1)) exit
          endif

          hdr_sv=hdr(:,j1)
          hdr(:,j1)=hdr(:,j2)
          hdr(:,j2)=hdr_sv

          trc_sv=trc(:,j1)
          trc(:,j1)= trc(:,j2)
          trc(:,j2)=trc_sv
          j1=j1-n1
        enddo
      enddo
      n1=n1/2
    enddo
 
    deallocate (hdr_sv)
    deallocate (trc_sv)
    return
  end subroutine pcpsx_sort_traces
 
!!---------------------- PCPSX_START PROCESSING ---------------------------!!
!!------------------------- start processing ------------------------------!!
!!------------------------- start processing ------------------------------!!
 
  subroutine pcpsx_start_processing
    integer        :: ierr

    pcpsx_state=STATE_PRE_INIT
    call pcpsx_init_processing
    call cps_start_processing(pcps_nprocess_list)
    pcps_ntimer=pcps_nprocess_list+1
    pcpsx_state=STATE_POST_INIT
    allocate(pcpsx_parallel_states(1:pcps_nprocess_list), stat=ierr)
    if(ierr.ne.0) &
     call pcpsx_abort("Unable to allocate in pcpsx_start_processing")
    return
 
  end subroutine pcpsx_start_processing
 
!!----------------------- PCPSX_PRINT_RCS_IDENT ---------------------------!!
!!-------------------------- print_rcs_ident ------------------------------!!
!!-------------------------- print_rcs_ident ------------------------------!!
 
  subroutine pcpsx_print_rcs_ident (id)
    character(len=*),intent(in)      :: id                        ! argument

    call cps_print_rcs_ident(id) 
    return
  end subroutine pcpsx_print_rcs_ident
 
!!----------------------- PCPSX_CUSTOM_RCS_IDENT --------------------------!!
!!-------------------------- custom_rcs_ident -----------------------------!!
!!-------------------------- custom_rcs_ident -----------------------------!!
  
  subroutine pcpsx_custom_rcs_ident
 
    call cps_custom_rcs_ident 
    return
 
  end subroutine pcpsx_custom_rcs_ident
 
!!------------------------ PCPSX_START_SETUPS -----------------------------!!
!!--------------------------- start_setups --------------------------------!!
!!--------------------------- start_setups --------------------------------!!
 
  subroutine pcpsx_start_setups
    call cps_start_setups
    return 
  end subroutine pcpsx_start_setups
 
 
!!----------------------- PCPSX_FINISH_SETUPS ------------------------------!!
!!--------------------------- finish_setups --------------------------------!!
!!--------------------------- finish_setups --------------------------------!!
 
 
  subroutine pcpsx_finish_setups (error)
    logical,intent(out)      :: error           ! argument

    character(len=PC_LENGTH) :: pcps_report_opt ! local

    pcps_report_opt = 'INCLUDEWORKERPRINTOUT'
    call pc_get_jdata  ('PCPS_REPORT_OPT',pcps_report_opt)
    call string_to_upper (pcps_report_opt)
    call string_squeeze_blanks (pcps_report_opt)
    if (pcps_report_opt .eq. 'INCLUDEWORKERPRINTOUT') then
      pcps_online_print_mode = 1
    else
      pcps_online_print_mode = 2
    endif

    pcpsx_state=STATE_PRE_FINISH_SETUP 
    call cps_finish_setups(error)
    pcpsx_state=STATE_POST_FINISH_SETUP
    call pcpsx_check_worker_errors(error)
    return
  end subroutine pcpsx_finish_setups
 
!!------------------------- PCPSX_FINISH_PROCESSING -----------------------!!
!!---------------------------- finish_processing --------------------------!!
!!---------------------------- finish_processing --------------------------!!
 
  subroutine pcpsx_finish_processing (error)
    logical,intent(inout)          :: error                 ! argument

    integer                        :: i                     ! local
    integer                        :: inblock               ! local
    integer                        :: outblock              ! local
    integer                        :: istat                 ! local
    integer                        :: maxrss                ! local
    integer                        :: nswap                 ! local
    integer                        :: minflt                ! local
    integer                        :: majflt                ! local
    integer                        :: cnt_time              ! local
    integer                        :: ierr                  ! local
    integer                        :: lun                   ! local
    integer                        :: enter_state           ! local
    
    real                           :: utime                 ! local
    real                           :: stime                 ! local
    real                           :: min_utime             ! local
    real                           :: max_utime             ! local
    real                           :: avg_utime             ! local
    real                           :: sum_utime             ! local
    real                           :: min_stime             ! local
    real                           :: max_stime             ! local
    real                           :: avg_stime             ! local
    real                           :: sum_stime             ! local
    real                           :: sd_time               ! local
    real                           :: var_time              ! local
    real                           :: elapse_time           ! local
    
    real, allocatable              :: cpu_rstats(:,:,:)     ! local
    integer, allocatable           :: cpu_istats(:,:)       ! local
    character,allocatable          :: os_versions(:,:)      ! local
    character (len=12)             :: pcpsx_os_version      ! local

    character (len=132)            :: line                  ! local
    character(len=78),parameter    :: EQUALINE =  &
'=============================================================================='
    integer :: bill1

    enter_state=pcpsx_state
    pcpsx_state=STATE_PRE_FINISH_PROC

    call cps_end_memory_thread()

    if(pcps_num_workers.gt.0) then
      if(pcps_worker_mode) then         !close any worker print files
        lun = cps_get_lun()
        close(unit=lun,status="keep")
      endif
      call pcpsx_sync_workers(ierr)
    endif
    
!--------- gather statistics
 
    allocate(cpu_rstats(3,pcps_nprocess_list+2,0:pcps_num_workers), stat=ierr)
    if(ierr.ne.0)  &
    call pcpsx_abort("Unable to allocate cpu_rstats in pcpsx_finish_processing")

    allocate(cpu_istats(3,0:pcps_num_workers), stat=ierr)
    if(ierr.ne.0)  call pcpsx_abort(&
     "Unable to allocate cpu_istats in pcpsx_finish_processing")
 
    allocate(os_versions(1:12,0:pcps_num_workers), stat=ierr)
    if(ierr.ne.0)  &
      call pcpsx_abort("Unable to os_versions in pcpsx_finish_processing")

    do i=1, pcps_nprocess_list+1   !get stats in column 0
      call timer_fetch (i,cnt_time,                                            &
       min_utime, max_utime, avg_utime, sum_utime,                             &
       min_stime, max_stime, avg_stime, sum_stime,                             &
       var_time, sd_time, elapse_time)
       cpu_rstats(1,i,0)=sum_utime
       cpu_rstats(2,i,0)=sum_stime
       cpu_rstats(3,i,0)=elapse_time
    enddo

    call getsys_usage (maxrss,nswap,minflt,majflt,inblock,outblock,            &
     utime, stime)
    cpu_rstats(1,pcps_nprocess_list+2,0)=utime
    cpu_rstats(2,pcps_nprocess_list+2,0)=stime
    cpu_rstats(3,pcps_nprocess_list+2,0)=0.

    call getsys_cpu_speed (cpu_istats(1,0))
    call cps_memory_max   (cpu_istats(2,0),cpu_istats(3,0))
    
    call getsys_os_version (pcpsx_os_version)
    os_versions(:,0) = ' '
    do i=1,len_trim(pcpsx_os_version)
      os_versions(i,0) = pcpsx_os_version(i:i)
    enddo

    if(pcps_num_workers.gt.0.and.enter_state.eq.(pcpsx_state-1) ) then
      if(pcps_boss_mode) then

        bill1=12
        do i=1, pcps_num_workers       !gather from cpu i into column i
          call pcpsx_receive_data(i,pcps_nprocess_list+2,cpu_rstats(:,:,i), &
            PCPS_ACCOUNT_TAG)
          call pcpsx_receive_data(i,3,cpu_istats(:,i),PCPS_ACCOUNT_TAG)
          call pcpsx_receive_data(i,bill1,os_versions(:,i),PCPS_ACCOUNT_TAG)
        enddo

      else                             !send stats to boss
        call pcpsx_send_data(0,pcps_nprocess_list+2,cpu_rstats(:,:,0), &
         PCPS_ACCOUNT_TAG)
        call pcpsx_send_data(0,3,cpu_istats(:,0),PCPS_ACCOUNT_TAG)
        call pcpsx_send_data(0,12,os_versions(:,0),PCPS_ACCOUNT_TAG)
      endif

    else
      if(pcps_num_workers.gt.0) then
        cpu_rstats(:,:,1:pcps_num_workers)=0
        cpu_istats(:,1:pcps_num_workers)=0
        os_versions(:,1:pcps_num_workers)=' '
      endif
    endif
 
! --- shut down the workers

    pcpsx_state=STATE_PRE_FINISH_PARALLEL

! --- print out the worker print files

    if(pcps_current_worker_num.eq.0) then
      call getlun (lun)
      do i=1,pcps_num_workers
        write(line,'(A,I4.4,A)') "online_",i,".tmp"
        open(unit=lun,file=trim(line),status="unknown", &
           action="readwrite")

        if(pcps_online_print_mode.eq.0 .or. pcps_online_print_mode.eq.1) then
! --- include worker onlin in master online 
          call pcps_print(" ")
          call pcps_print(EQUALINE)
          write(line,*)   "Print information for Worker = ",i
          call pcps_print(trim(line))
          call pcps_print(EQUALINE)

          do
            read(lun,fmt='(a)',iostat=istat) line
            if(istat.ne.0) exit
            call pcps_print(trim(line))
          enddo
        endif

        if(pcps_online_print_mode.eq.0 .or. pcps_online_print_mode.eq.3) then
          close(unit=lun,status="delete")       !delete worker online files
        else
          close(unit=lun,status="keep")         !keep worker online files
        endif

      enddo
    endif

! --- do any final boss printing

    if(pcps_boss_mode)then 
      call cps_finish_processing(error,cpu_rstats,pcps_hostnames,cpu_istats,  &
                                 os_versions)
    endif

    call pcpsx_finish_parallel(error)
    pcpsx_state=STATE_POST_FINISH_PARALLEL
    if(error) call pcpsx_abort("Error in pcpsx_finish_parallel")

    deallocate(cpu_rstats, stat=ierr)
    if(ierr.ne.0)  &
     call pcpsx_abort(  &
      "Unable to deallocate cpu_rstats in pcps_finish_processing")

    deallocate(cpu_istats, stat=ierr)
    if(ierr.ne.0)  &
     call pcpsx_abort( &
      "Unable to deallocate cpu_istats in pcps_finish_processing")
  
    deallocate(os_versions, stat=ierr)
    if(ierr.ne.0)  &
     call pcpsx_abort( &
      "Unable to deallocate os_versions in pcps_finish_processing")
 
    return
 
  end subroutine pcpsx_finish_processing
 
!!------------------------ PCPSX_PRE_SETUP -----------------------------!!
!!--------------------------- pre_setup --------------------------------!!
!!--------------------------- pre_setup --------------------------------!!
 
 
  subroutine pcpsx_pre_setup (p_control)
    integer, intent(in)  :: p_control

    integer   :: i_proc

    pcps_merge_hdr_1=0             !if changed then need to save merge parms

    call cps_pre_setup(pcps_ipn)

    if(pcps_ipn.eq.1 .or. pcps_ipn.eq.2 .or. &
       p_control.eq.PCPS_BOSS_PARALLEL) then
      pcps_backend_exec=.true.
    else if(pcps_boss_exec_mode == PCPS_BOSS_EXECS .and. &  ! added 2004-12-07
            p_control == PCPS_INSIDE_PARALLEL .and. &       ! added 2004-12-07
            PCPS_CURRENT_WORKER_NUM == 0) then              ! added 2004-12-07
      pcps_backend_exec=.true.
    else 
      pcps_backend_exec =                                               &
       (pcps_boss_mode   .and.  p_control.eq.PCPS_OUTSIDE_PARALLEL .or. &
        pcps_worker_mode .and.  p_control.eq.PCPS_INSIDE_PARALLEL)
    endif

    if(.not. pcps_backend_exec) call pc_set_backend_no_exec
    pcpsx_parallel_states(pcps_ipn)=p_control

! --- if inside parallel and have actual workers, 
!      have each worker wait for a start from the boss and 
!        then send a start to the boss from the worker in post_setup
!      the boss will send a start to each worker and wait for a start from it
!     They is intended to help avoid all workers hitting a file simultaneoulsy

    if(pcps_num_procs.gt.1.and.p_control.eq.PCPS_INSIDE_PARALLEL) then
      if(pcps_boss_mode) then
        do i_proc=1,pcps_num_workers
          call pcpsx_send_start(i_proc)     !send start to worker
          call pcpsx_receive_start(i_proc)  !wait for a start from the worker
        enddo
      else
        call pcpsx_receive_start(PCPS_BOSS) !wait for a start from the boss
      endif
    endif

    return
  end subroutine pcpsx_pre_setup
 
 
!!------------------------ PCPSX_POST_SETUP -----------------------------!!
!!--------------------------- post_setup --------------------------------!!
!!--------------------------- post_setup --------------------------------!!
 
 
  subroutine pcpsx_post_setup

    integer    :: ierr

    if(.not.pcps_backend_exec) call pc_set_backend_yes_exec
    
    if(pcps_merge_hdr_1.gt.0) then
      call pcps_save_merge_parameters()
      pcps_merge_hdr_1=0
    endif

    call cps_post_setup

! --- if have actual workers and inside parallel and a worker
!      send a start to the boss that boss expect in pre_setup

    if(pcps_num_procs.gt.1 .and.    &
       pcpsx_parallel_states(pcps_ipn).eq.PCPS_INSIDE_PARALLEL) then
      if(pcps_worker_mode) call pcpsx_send_start(PCPS_BOSS)
    endif

    call pcpsx_sync_workers(ierr)

    return
  end subroutine pcpsx_post_setup
 
 
!!---------------------- PCPSX_PRE_PROCESS -----------------------------!!
!!-------------------------- pre_process -------------------------------!!
!!-------------------------- pre_process -------------------------------!!
 
 
  subroutine pcpsx_pre_process (ipn)
    integer         ,intent(in)       :: ipn                   !argument

    if(iand(pcpsx_debug,PCPSX_DEBUG_PRE_PROCESS).ne.0) then
      write(pcps_message, *)"   pcpsx_pre_process, cpu=", &
       pcps_current_worker_num,", ipn=",ipn
      call pcps_print(pcps_message,2)
    endif 

    call cps_pre_process(ipn)
    pcps_ipn=ipn
    return
  end subroutine pcpsx_pre_process
 
 
!!--------------------- PCPSX_POST_PROCESS ------------------------------!!
!!-------------------------- post_process -------------------------------!!
!!-------------------------- post_process -------------------------------!!
 
 
  subroutine pcpsx_post_process (ipn,ntr,hd,tr)
    integer         ,intent(in)       :: ipn,ntr               ! argument
    double precision,intent(inout)    :: hd(:,:)               ! argument
    real            ,intent(in)       :: tr(:,:)               ! argument
 
    call cps_post_process(ipn,ntr,hd,tr)
    return    
  end subroutine pcpsx_post_process
 
!!---------------------------- PCPSX_DEBUG_PRINT -------------------------!!
!!---------------------------- pcpsx_debug_print -------------------------!!
!!---------------------------- pcpsx_debug_print -------------------------!!

  subroutine pcpsx_debug_print(title, ntr, hdrs, trcs)
    character (len=*), intent(in) :: title
    integer          , intent(in) :: ntr
    double precision , intent(in) :: hdrs(:,:)
    real              ,intent(in) :: trcs(:,:)

    integer             :: i, j
    character (len=160) :: mess

    write(mess,*) trim(title)//": cpu=",pcps_current_worker_num,", ntr=",ntr
    call pcps_print(trim(mess),2)

    if(ntr.gt.0) then
      do i=1,ntr
        write(mess,*) "  hdr(1;4)=",(hdrs(j,i),j=1,4), &
         ",min, max trval=",minval(trcs(1:,i)), maxval(trcs(1:,i))
        call pcps_print(trim(mess),2)
      enddo
    endif

    return
  end subroutine pcpsx_debug_print
 
!!---------------------------- PCPSX_DUMP_HDRS -------------------------!!
!!---------------------------- pcpsx_dump_hdrs -------------------------!!
!!---------------------------- pcpsx_dump_hdrs -------------------------!!
 
  subroutine pcpsx_dump_hdrs(title, ntr, hdr)
    integer, intent(in)           :: ntr
    character (len=*), intent(in) :: title
    double precision,  intent(in) :: hdr(:,:)
 
    integer                       :: i, j, ihdr(4)

    write(pcps_message,*) title//", cpu, ntr=",pcps_current_worker_num,ntr
    call pcps_print(pcps_message,2)

    if(ntr.gt.0) then
      do i=1, ntr

        do j=1,4
          ihdr(j)=hdr(j,i)
        enddo

        write(pcps_message,*) "  i, hdr=",i,ihdr
        call pcps_print(pcps_message,2)

      enddo
    endif
 
    return
  end subroutine pcpsx_dump_hdrs

!-------------PCPSX_SEND_START---------------
! Send "Start" to worker
! 
! Written March 2001 by Charles C Burch
!--------------------------------------------
  subroutine pcpsx_send_start(worker_no, instance_in)
    integer, intent(in)           :: worker_no
    integer, intent(in), optional :: instance_in

    integer           :: ierr, instance, i1, i2, i
    character (len=1) :: buff(1)='G'

    if(pcps_num_procs.eq.1) return
    if(worker_no.eq.pcps_current_worker_num) return 

    if(present(instance_in)) then
      instance=instance_in
    else
      instance=0
    endif

    if(worker_no.eq.PCPS_ALL_WORKERS) then
      if(.not.pcps_boss_mode)  call pcpsx_abort( &
       "send_start not in boss mode but sending to all workers")
      i1=1
      i2=pcps_num_workers
    else
      i1=worker_no
      i2=i1
    endif

    do i=i1,i2
      call ppio_send_char_data(buff,1,i,PCPS_CONTROL_TAG,ierr)
      if(ierr.ne.0) then
        write(pcps_message,*) "Error(",ierr,") in pcpsx_send_start"
        call pcpsx_abort(pcps_message)
      endif
    enddo
    return
  end subroutine pcpsx_send_start

!-------------PCPSX_RECEIVE_START----------------
! Receive "Start" from worker
! 
! Written March 2001 by Charles C Burch
!------------------------------------------------
  subroutine pcpsx_receive_start(worker_no, instance_in)
    integer, intent(in)           :: worker_no
    integer, intent(in), optional :: instance_in

    integer           :: ierr, instance, i1,i2, i
    character (len=1) :: buff(1)='*'

    if(pcps_num_procs.eq.1) return
    if(worker_no.eq.pcps_current_worker_num) return
 
    if(present(instance_in)) then
      instance=instance_in
    else
      instance=0
    endif

    if(worker_no.eq.PCPS_ALL_WORKERS) then
      if(.not.pcps_boss_mode)  call pcpsx_abort( &
       "send_start not in boss mode but sending to all workers")
      i1=1
      i2=pcps_num_workers
    else
      i1=worker_no
      i2=i1
    endif

    do i=i1,i2
      call ppio_receive_char_data(buff,1,i,PCPS_CONTROL_TAG,ierr)
      if(ierr.ne.0 .or. buff(1).ne.'G') then
        write(pcps_message,*)  &
         "Error(",ierr,",",buff(1),") in pcpsx_receive_start"
        call pcpsx_abort(pcps_message)
      endif
    enddo

    return
  end subroutine pcpsx_receive_start

!---------------PCPSX_CHECK_WORKER_ERRORS_L0---------------------
! sync errors from boss and workers 
! 
! Written March 2001 by Charles C Burch
!----------------------------------------------------------------
  subroutine pcpsx_check_worker_errors_l0(error)
    logical, intent(inout) :: error

    integer                :: ierr

    if(pcps_num_procs.le.1) return

    ierr=0
    if(error) ierr=1

    call pcpsx_sum_all_reduce(ierr,ierr)
    error=ierr.ne.0
    return
  end subroutine pcpsx_check_worker_errors_l0    
  
!----------------PCPSX_CHECK_WORKER_ERRORS_I0-------------------
! sync errors from boss and workers 
! 
! Written March 2001 by Charles C Burch
!---------------------------------------------------------------
  subroutine pcpsx_check_worker_errors_i0(error)
    integer, intent(inout) :: error

    integer                :: ierr

    if(pcps_num_procs.le.1) return

    ierr=0
    if(error.ne.0) ierr=1

    call pcpsx_sum_all_reduce(ierr,ierr)

    if(ierr.eq.0) then
      error=0
    else
      error=1
    endif
    return
  end subroutine pcpsx_check_worker_errors_i0    
  
!----------------PCPSX_CHECK_GROUP_ERRORS_I0-------------------
! sync errors from boss and workers 
! 
! Written January 2002 by Charles C Burch
!--------------------------------------------------------------
  subroutine pcpsx_check_group_errors_i0(n_workers, workers, error)
    integer, intent(inout) :: error
    integer, intent(in)    :: n_workers
    integer, intent(in)    :: workers(:)

    integer                :: ierr, i

    if(pcps_num_procs.le.1 .or. n_workers.le.1) return

    do i=1, n_workers
      if(pcps_current_worker_num.eq.workers(i)) exit
    enddo
    if(i.gt.n_workers) return

    ierr=0
    if(error.ne.0) ierr=1

    call pcpsx_sum_all_reduce_group(n_workers, workers,ierr)

    if(ierr.eq.0) then
      error=0
    else
      error=1
    endif

    return
  end subroutine pcpsx_check_group_errors_i0    
  
!----------------PCPSX_CHECK_GROUP_ERRORS_L0-------------------
! sync errors from boss and workers 
! 
! Written January 2002 by Charles C Burch
!--------------------------------------------------------------
  subroutine pcpsx_check_group_errors_l0(n_workers, workers, error)
    logical, intent(inout) :: error
    integer, intent(in)    :: n_workers
    integer, intent(in)    :: workers(:)

    integer                :: ierr, i

    if(pcps_num_procs.le.1 .or. n_workers.le.1) return

    do i=1, n_workers
      if(pcps_current_worker_num.eq.workers(i)) exit
    enddo
    if(i.gt.n_workers) return

    ierr=0
    if(error) ierr=1

    call pcpsx_sum_all_reduce_group(n_workers, workers,ierr)
    error=ierr.ne.0
    return
  end subroutine pcpsx_check_group_errors_l0    
  
!---------------------PCPSX_SYNC_GROUP-------------------------
! sync errors from boss and workers 
! 
! Written January 2002 by Charles C Burch
!--------------------------------------------------------------
  subroutine pcpsx_sync_group(n_workers, workers,error)
    integer, intent(inout) :: error
    integer, intent(in)    :: n_workers
    integer, intent(in)    :: workers(:)

    integer                :: i

    do i=1, n_workers
      if(pcps_current_worker_num.eq.workers(i)) exit
    enddo
    if(i.gt.n_workers) return

    error=0
    if(pcps_num_procs.le.1 .or. n_workers.le.1) return

    call pcpsx_sum_reduce_group(workers(n_workers),n_workers-1, workers,error)
    return
  end subroutine pcpsx_sync_group    
  
!-----------------------------PCPSX_I_PEL------------------------------
! --- pcpsx_i_pel and pcpsx_n_pel added for backwards capatibility with
!      migration codes

  integer function pcpsx_i_pel( )
    
    pcpsx_i_pel = pcps_current_worker_num
    return
  end function pcpsx_i_pel

!-----------------------------PCPSX_N_PEL------------------------------
  integer function pcpsx_n_pel( )
    
    pcpsx_n_pel = pcps_num_procs
    return
  end function pcpsx_n_pel

!---------------------PCPSX_FORM_TEMP_GROUP--------------------
! form array of workers with root only occuring once
! 
! Written January 2002 by Charles C Burch
!--------------------------------------------------------------
  subroutine pcpsx_form_temp_group(root_worker, num_workers, workers, &
   n_workers, wrkers)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(out  ) :: n_workers
    integer, intent(out  ) :: wrkers(:)
  
    integer                :: i
    
    wrkers(1)=root_worker
    n_workers=num_workers+1
    wrkers(2:n_workers)=workers(1:num_workers)
    i=2
    do while(i.le.n_workers)
      if(wrkers(i).eq.root_worker) then
        wrkers(i)=wrkers(n_workers)
        n_workers=n_workers-1
      else
        i=i+1
      endif
    enddo
  
    return
  end subroutine pcpsx_form_temp_group
  
!---------------------PCPSX_BROADCAST_GROUP ROUTINES-----------------------
! broadcast group routines for character, real, integer, double and complex
!
! Written January 2002 by Charles C Burch
!--------------------------------------------------------------------------
  subroutine pcpsx_broadcast_group_c0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    character(len=*), intent(inout) :: dat
    
    integer                :: i, j, n_dat
    character              :: c1(1:len(dat))

    n_dat=len(dat)
    if(pcps_current_worker_num.eq.root_worker) then
      do i=1,n_dat
        c1(i)=dat(i:i)
      enddo
    endif

    call pcpsx_broadcast_group_c1(root_worker, num_workers, workers, n_dat, c1)

    do j=1, num_workers
      if(pcps_current_worker_num.eq.workers(j)) then
        do i=1,n_dat
          dat(i:i)=c1(i)
        enddo
        exit
      endif
    enddo
    return
  end  subroutine pcpsx_broadcast_group_c0

  subroutine pcpsx_broadcast_group_c1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    character, intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_c1

  subroutine pcpsx_broadcast_group_c2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    character, intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_c2

  subroutine pcpsx_broadcast_group_i0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), dat, PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_i0

  subroutine pcpsx_broadcast_group_r0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    real   , intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), dat, PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_r0

  subroutine pcpsx_broadcast_group_d0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    double precision, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), dat, PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_d0

  subroutine pcpsx_broadcast_group_z0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    complex, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), dat, PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_z0

  subroutine pcpsx_broadcast_group_i1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_i1

  subroutine pcpsx_broadcast_group_r1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_r1

  subroutine pcpsx_broadcast_group_d1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_d1

  subroutine pcpsx_broadcast_group_z1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_z1

  subroutine pcpsx_broadcast_group_i2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_i2

  subroutine pcpsx_broadcast_group_r2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_r2

  subroutine pcpsx_broadcast_group_d2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_d2

  subroutine pcpsx_broadcast_group_z2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_z2

  subroutine pcpsx_broadcast_group_i3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_i3

  subroutine pcpsx_broadcast_group_r3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_r3

  subroutine pcpsx_broadcast_group_d3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_d3

  subroutine pcpsx_broadcast_group_z3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_z3

  subroutine pcpsx_broadcast_group_i4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_i4

  subroutine pcpsx_broadcast_group_r4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_r4

  subroutine pcpsx_broadcast_group_d4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_d4

  subroutine pcpsx_broadcast_group_z4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1) , n_workers 

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    
    level=1
    do while(level<n_workers)
      !send from i_send_cpu=1:level to i_send_cpu+level
      i_end=min(level,n_workers-level)
      do i_send_cpu=1, i_end
        i_rec_cpu=i_send_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat, PCPS_BROADCAST_TAG)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
          call pcpsx_receive_data(cpus(i_send_cpu), n_dat, dat, &
           PCPS_BROADCAST_TAG)
        endif  
      enddo  !i_send_cpu
      
      level=2*level
    enddo    !level 
    return
  end  subroutine pcpsx_broadcast_group_z4

!------------------PCPSX_SUM_REDUCE_GROUP ROUTINES-----------------------
! sum reduce  group routines for integer, real, double and complex
!
! Written January 2002 by Charles C Burch
!-----------------------------------------------------------------------
  subroutine pcpsx_sum_reduce_group_i0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    integer                :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_i0

  subroutine pcpsx_sum_reduce_group_r0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    real   , intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    real                   :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_r0

  subroutine pcpsx_sum_reduce_group_d0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    double precision, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    double precision       :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_d0

  subroutine pcpsx_sum_reduce_group_z0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    complex, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    complex                :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_z0

  subroutine pcpsx_sum_reduce_group_i1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    integer                :: dat1(1:n_dat), dat2(1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_i1

  subroutine pcpsx_sum_reduce_group_r1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    real                   :: dat1(1:n_dat), dat2(1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_r1

  subroutine pcpsx_sum_reduce_group_d1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    double precision       :: dat1(1:n_dat), dat2(1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_d1

  subroutine pcpsx_sum_reduce_group_z1(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    complex                :: dat1(1:n_dat), dat2(1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_z1

  subroutine pcpsx_sum_reduce_group_i2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    integer                :: dat1(1:size(dat,1), 1:n_dat)
    integer                :: dat2(1:size(dat,1), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_i2

  subroutine pcpsx_sum_reduce_group_r2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    real                   :: dat1(1:size(dat,1), 1:n_dat)
    real                   :: dat2(1:size(dat,1), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_r2

  subroutine pcpsx_sum_reduce_group_d2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    double precision       :: dat1(1:size(dat,1), 1:n_dat)
    double precision       :: dat2(1:size(dat,1), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_d2

  subroutine pcpsx_sum_reduce_group_z2(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    complex                :: dat1(1:size(dat,1), 1:n_dat)
    complex                :: dat2(1:size(dat,1), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_z2

  subroutine pcpsx_sum_reduce_group_i3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    integer                :: dat1(1:size(dat,1), 1:size(dat,2),1:n_dat)
    integer                :: dat2(1:size(dat,1), 1:size(dat,2),1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_i3

  subroutine pcpsx_sum_reduce_group_r3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    real                   :: dat1(1:size(dat,1), 1:size(dat,2),1:n_dat)
    real                   :: dat2(1:size(dat,1), 1:size(dat,2),1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_r3

  subroutine pcpsx_sum_reduce_group_d3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    double precision       :: dat1(1:size(dat,1), 1:size(dat,2), 1:n_dat)
    double precision       :: dat2(1:size(dat,1), 1:size(dat,2), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_d3

  subroutine pcpsx_sum_reduce_group_z3(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    complex                :: dat1(1:size(dat,1), 1:size(dat,2), 1:n_dat)
    complex                :: dat2(1:size(dat,1), 1:size(dat,2), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_z3

  subroutine pcpsx_sum_reduce_group_i4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    integer                :: dat1(1:size(dat,1), 1:size(dat,2),1:size(dat,3), &
                                   1:n_dat)
    integer                :: dat2(1:size(dat,1), 1:size(dat,2),1:size(dat,3), &
                                   1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_i4

  subroutine pcpsx_sum_reduce_group_r4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    real                   :: dat1(1:size(dat,1), 1:size(dat,2),1:size(dat,3), &
                                   1:n_dat)
    real                   :: dat2(1:size(dat,1), 1:size(dat,2),1:size(dat,3), &
                                   1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_r4

  subroutine pcpsx_sum_reduce_group_d4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    double precision       :: dat1(1:size(dat,1), 1:size(dat,2), &
                                   1:size(dat,3), 1:n_dat)
    double precision       :: dat2(1:size(dat,1), 1:size(dat,2), &
                                   1:size(dat,3), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_d4

  subroutine pcpsx_sum_reduce_group_z4(root_worker, num_workers, workers, &
   n_dat, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:,:,:)
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    complex                :: dat1(1:size(dat,1), 1:size(dat,2), &
                                   1:size(dat,3), 1:n_dat)
    complex                :: dat2(1:size(dat,1), 1:size(dat,2), &
                                   1:size(dat,3), 1:n_dat)

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat(:,:,:,1:n_dat)

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), n_dat, dat1, &
           PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu),n_dat, dat2, &
           PCPS_BROADCAST_TAG)
          dat1=dat1+dat2
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat(:,:,:,1:n_dat)=dat1
    return
  end  subroutine pcpsx_sum_reduce_group_z4


!------------------PCPSX_SUM_ALL_REDUCE_GROUP ROUTINES------------------
! sum all reduce group routines for integer, real, double and complex
!
! Written January 2002 by Charles C Burch
!-----------------------------------------------------------------------
  subroutine pcpsx_sum_all_reduce_group_i0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(inout) :: dat
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_i0

  subroutine pcpsx_sum_all_reduce_group_r0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    real   , intent(inout) :: dat
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_r0

  subroutine pcpsx_sum_all_reduce_group_d0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    double precision, intent(inout) :: dat
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers,  dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_d0

  subroutine pcpsx_sum_all_reduce_group_z0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    complex, intent(inout) :: dat
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers,  dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_z0

  subroutine pcpsx_sum_all_reduce_group_i1(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_i1

  subroutine pcpsx_sum_all_reduce_group_r1(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_r1

  subroutine pcpsx_sum_all_reduce_group_d1(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_d1

  subroutine pcpsx_sum_all_reduce_group_z1(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_z1

  subroutine pcpsx_sum_all_reduce_group_i2(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_i2

  subroutine pcpsx_sum_all_reduce_group_r2(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_r2

  subroutine pcpsx_sum_all_reduce_group_d2(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_d2

  subroutine pcpsx_sum_all_reduce_group_z2(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_z2

  subroutine pcpsx_sum_all_reduce_group_i3(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_i3

  subroutine pcpsx_sum_all_reduce_group_r3(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_r3

  subroutine pcpsx_sum_all_reduce_group_d3(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_d3

  subroutine pcpsx_sum_all_reduce_group_z3(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_z3

  subroutine pcpsx_sum_all_reduce_group_i4(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    integer, intent(inout) :: dat(:,:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_i4

  subroutine pcpsx_sum_all_reduce_group_r4(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    real   , intent(inout) :: dat(:,:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_r4

  subroutine pcpsx_sum_all_reduce_group_d4(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    double precision, intent(inout) :: dat(:,:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_d4

  subroutine pcpsx_sum_all_reduce_group_z4(num_workers, workers, n_dat, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(in   ) :: n_dat
    complex, intent(inout) :: dat(:,:,:,:)
    

    call pcpsx_sum_reduce_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, &
      n_dat, dat)
    return
  end subroutine pcpsx_sum_all_reduce_group_z4

!---------------------PCPSX_MAX_REDUCE_GROUP ROUTINES---------------------
! max reduce group routines for integer, real, and double
!
! Written January 2002 by Charles C Burch
!-------------------------------------------------------------------------
  subroutine pcpsx_max_reduce_group_i0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    integer                :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=max(dat1,dat2)
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_max_reduce_group_i0

  subroutine pcpsx_max_reduce_group_r0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    real   , intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    real                   :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=max(dat1,dat2)
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_max_reduce_group_r0

  subroutine pcpsx_max_reduce_group_d0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    double precision, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    double precision       :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=max(dat1,dat2)
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_max_reduce_group_d0

!---------------------PCPSX_MIN_REDUCE_GROUP ROUTINES---------------------
! min reduce group routines for integer, double and complex
!
! Written January 2002 by Charles C Burch
!-------------------------------------------------------------------------
  subroutine pcpsx_min_reduce_group_i0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    integer                :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=min(dat1,dat2)
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_min_reduce_group_i0

  subroutine pcpsx_min_reduce_group_r0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    real   , intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    real                   :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=min(dat1,dat2)
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_min_reduce_group_r0

  subroutine pcpsx_min_reduce_group_d0(root_worker, num_workers, workers, dat)
    integer, intent(in   ) :: root_worker
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    double precision, intent(inout) :: dat
    
    integer                :: level, i_send_cpu, i_rec_cpu, i_end
    integer                :: cpus(1:num_workers+1), n_workers
    double precision       :: dat1, dat2

    call pcpsx_form_temp_group(root_worker, num_workers,workers, n_workers,cpus)
    dat1=dat

    level=1
    do while(level<n_workers)
      level=2*level
    enddo

    do while(level>1)
      level=level/2
      i_end=min(level,n_workers-level)
      do i_rec_cpu=1, i_end
        i_send_cpu=i_rec_cpu+level
          
        if(pcps_current_worker_num.eq.cpus(i_send_cpu)) then
          call pcpsx_send_data(cpus(i_rec_cpu), dat1, PCPS_BROADCAST_TAG)
!         print *,"send",cpus(i_send_cpu),cpus(i_rec_cpu)
        else if(pcps_current_worker_num.eq.cpus(i_rec_cpu)) then  
!         print *,"rec",cpus(i_rec_cpu),cpus(i_send_cpu)
          call pcpsx_receive_data(cpus(i_send_cpu), dat2, PCPS_BROADCAST_TAG)
          dat1=min(dat1,dat2)
        endif  
      enddo     !i_rec_cpu
    enddo       !level
    
    if(pcps_current_worker_num.eq.root_worker) dat=dat1
    return
  end  subroutine pcpsx_min_reduce_group_d0

!------------------PCPSX_MAX_ALL_REDUCE_GROUP ROUTINES--------------------
! max all reduce group routines for integer, double and real
!
! Written January 2002 by Charles C Burch
!-------------------------------------------------------------------------
  subroutine pcpsx_max_all_reduce_group_i0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(inout) :: dat
    

    call pcpsx_max_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, dat)
    return
  end subroutine pcpsx_max_all_reduce_group_i0

  subroutine pcpsx_max_all_reduce_group_r0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    real   , intent(inout) :: dat
    

    call pcpsx_max_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, dat)
    return
  end subroutine pcpsx_max_all_reduce_group_r0

  subroutine pcpsx_max_all_reduce_group_d0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    double precision, intent(inout) :: dat
    

    call pcpsx_max_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers,  dat)
    return
  end subroutine pcpsx_max_all_reduce_group_d0

!------------------PCPSX_MIN_ALL_REDUCE_GROUP ROUTINES--------------------
! min all reduce group routines for integer, real, and double
!
! Written January 2002 by Charles C Burch
!-------------------------------------------------------------------------
  subroutine pcpsx_min_all_reduce_group_i0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    integer, intent(inout) :: dat
    

    call pcpsx_min_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, dat)
    return
  end subroutine pcpsx_min_all_reduce_group_i0

  subroutine pcpsx_min_all_reduce_group_r0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    real   , intent(inout) :: dat
    

    call pcpsx_min_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers, dat)
    return
  end subroutine pcpsx_min_all_reduce_group_r0

  subroutine pcpsx_min_all_reduce_group_d0(num_workers, workers, dat)
    integer, intent(in   ) :: num_workers
    integer, intent(in   ) :: workers(:)
    double precision, intent(inout) :: dat
    

    call pcpsx_min_reduce_group(workers(num_workers),num_workers-1,workers, dat)
    call pcpsx_broadcast_group(workers(num_workers),num_workers-1,workers,  dat)
    return
  end subroutine pcpsx_min_all_reduce_group_d0

!------------------PCPSX_GATHER_GROUP ROUTINES--------------------
! gather group routines for integer, real, double and complex
!
! Written January 2002 by Charles C Burch
!-----------------------------------------------------------------
  subroutine pcpsx_gather_group_c0(i_pel, n_workers, workers, x_dat, x_out)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n_workers
    integer, intent(in)    :: workers(:)
    character, intent(in)  :: x_dat 
    character, intent(out) :: x_out(:) 
    
    integer                :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1)=x_dat
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), x_out(i+1), PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_c0 

  subroutine pcpsx_gather_group_i0(i_pel, n_workers, workers, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: x_dat 
    integer, intent(out) :: x_out(:) 
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1)=x_dat
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), x_out(i+1), PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_i0 

  subroutine pcpsx_gather_group_r0(i_pel, n_workers, workers, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    real   , intent(in)  :: x_dat 
    real   , intent(out) :: x_out(:) 
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1)=x_dat
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), x_out(i+1), PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_r0 

  subroutine pcpsx_gather_group_d0(i_pel, n_workers, workers, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    double precision, intent(in)  :: x_dat 
    double precision, intent(out) :: x_out(:) 
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1)=x_dat
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), x_out(i+1), PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_d0 

  subroutine pcpsx_gather_group_z0(i_pel, n_workers, workers, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    complex, intent(in)  :: x_dat 
    complex, intent(out) :: x_out(:) 
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1)=x_dat
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), x_out(i+1), PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_z0 

  subroutine pcpsx_gather_group_c1(i_pel, n_workers, workers, &
   n1_inp, x_dat, x_out)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n_workers
    integer, intent(in)    :: workers(:)
    integer, intent(in)    :: n1_inp
    character, intent(in)  :: x_dat(:)
    character, intent(out) :: x_out(:,:)
    
    integer                :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n1_inp, x_out(:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n1_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_c1 

  subroutine pcpsx_gather_group_i1(i_pel, n_workers, workers, &
   n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n1_inp
    integer, intent(in)  :: x_dat(:)
    integer, intent(out) :: x_out(:,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n1_inp, x_out(:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n1_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_i1 

  subroutine pcpsx_gather_group_r1(i_pel, n_workers, workers, &
   n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n1_inp
    real   , intent(in)  :: x_dat(:)
    real   , intent(out) :: x_out(:,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n1_inp, x_out(:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n1_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_r1 

  subroutine pcpsx_gather_group_d1(i_pel, n_workers, workers, &
   n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n1_inp
    double precision, intent(in)  :: x_dat(:)
    double precision, intent(out) :: x_out(:,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n1_inp, x_out(:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n1_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_d1 

  subroutine pcpsx_gather_group_z1(i_pel, n_workers, workers, &
   n1_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n1_inp
    complex, intent(in)  :: x_dat(:)
    complex, intent(out) :: x_out(:,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(1:n1_inp,1)=x_dat(1:n1_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n1_inp, x_out(:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n1_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_z1 

  subroutine pcpsx_gather_group_c2(i_pel, n_workers, workers, &
   n2_inp, x_dat, x_out)
    integer, intent(in)    :: i_pel
    integer, intent(in)    :: n_workers
    integer, intent(in)    :: workers(:)
    integer, intent(in)    :: n2_inp
    character, intent(in)  :: x_dat(:, :)
    character, intent(out) :: x_out(:, :,:)
    
    integer                :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(:,1:n2_inp,1)=x_dat(:,1:n2_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n2_inp, x_out(:,:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n2_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_c2 

  subroutine pcpsx_gather_group_i2(i_pel, n_workers, workers, &
   n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n2_inp
    integer, intent(in)  :: x_dat(:, :)
    integer, intent(out) :: x_out(:, :,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(:,1:n2_inp,1)=x_dat(:,1:n2_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n2_inp, x_out(:,:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n2_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_i2 

  subroutine pcpsx_gather_group_r2(i_pel, n_workers, workers, &
   n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n2_inp
    real   , intent(in)  :: x_dat(:, :)
    real   , intent(out) :: x_out(:, :,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(:,1:n2_inp,1)=x_dat(:,1:n2_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n2_inp, x_out(:,:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n2_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_r2 

  subroutine pcpsx_gather_group_d2(i_pel, n_workers, workers, &
   n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n2_inp
    double precision, intent(in)  :: x_dat(:, :)
    double precision, intent(out) :: x_out(:, :,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(:,1:n2_inp,1)=x_dat(:,1:n2_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n2_inp, x_out(:,:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n2_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_d2 

  subroutine pcpsx_gather_group_z2(i_pel, n_workers, workers, &
   n2_inp, x_dat, x_out)
    integer, intent(in)  :: i_pel
    integer, intent(in)  :: n_workers
    integer, intent(in)  :: workers(:)
    integer, intent(in)  :: n2_inp
    complex, intent(in)  :: x_dat(:, :)
    complex, intent(out) :: x_out(:, :,:)
    
    integer              :: i
    
    if(pcps_current_worker_num.eq.i_pel) x_out(:,1:n2_inp,1)=x_dat(:,1:n2_inp)
    if(pcps_num_procs.le.1) return

    do i=1,n_workers
      if(workers(i).eq.i_pel) cycle
      if(pcps_current_worker_num.eq.i_pel) then
!       print *,"rec",i_pel, workers(i)
        call pcpsx_receive_data(workers(i), n2_inp, x_out(:,:,i+1), &
         PCPS_BROADCAST_TAG)
      else if(pcps_current_worker_num.eq.workers(i)) then
        call pcpsx_send_data(i_pel, n2_inp, x_dat, PCPS_BROADCAST_TAG)
!       print *,"send",i_pel, workers(i)
      endif   
    enddo
  
    return
  end subroutine pcpsx_gather_group_z2 

!-------------------------PCPSX_PUT_DATA_INIT-------------------------
! set up for a put_data using sender inserting sender information
!  addiitonal data can be added to the buffer using ppio_pack routines
!  data is send using a pcpsx_put_data
!
! Written January 2002 by Charles C Burch
!----------------------------------------------------------------------
  subroutine pcpsx_put_data_init(instance, num_workers, workers, size, position)
    integer,   intent(in   ) :: instance
    integer,   intent(in   ) :: num_workers
    integer,   intent(in   ) :: workers(:)
    integer,   intent(in   ) :: size    
    integer,   intent(inout) :: position

    integer                  :: n, ibuff(3)

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_put_init, cpu=", &
       pcps_current_worker_num,     &
       ", size=",size, ", num_workers=",num_workers, ", 1st worker=", workers(1)
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    n=size+(num_workers+3)*ppio_sizeof_integer
    if(n.gt.pcpsx_buffer_size) &
      call pcpsx_expand_buffer(pcpsx_buffer, pcpsx_buffer_size, 0, n)

    position=0
    ibuff(1)=instance
    ibuff(2)=pcps_current_worker_num
    ibuff(3)=num_workers
    call ppio_pack_integer(ibuff,3,pcpsx_buffer,position)
    call ppio_pack_integer(workers,num_workers,pcpsx_buffer,position)
    return
  end subroutine pcpsx_put_data_init

!-------------------------PCPSX_PUT_DATA-------------------------
! send data via a broadcast_group
!
! Written January 2002 by Charles C Burch
!-----------------------------------------------------------------
  subroutine pcpsx_put_data(num_workers, workers, size)
    integer,   intent(in) :: num_workers
    integer,   intent(in) :: workers(:)
    integer,   intent(in) :: size   
 
    integer               :: level
    
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_put_data, cpu=",   &
       pcps_current_worker_num,    &
       ", size=",size, ", num_workers=",num_workers, ", 1st worker=", workers(1)
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    level=1
    do while(level<=num_workers)
      call pcpsx_send_data(workers(level), size, pcpsx_buffer, PCPS_PUT_TAG)
      level=2*level
    enddo    !level 

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"    pcpsx_put_data-exit:cpu=",  &
       pcps_current_worker_num
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    return
  end subroutine pcpsx_put_data

!-------------------------PCPSX_GET_DATA-------------------------
! get data via a broadcast_group and extract sender information
!  additional information is extracted using ppio_unpacks
!
! Written January 2002 by Charles C Burch
!-----------------------------------------------------------------
  subroutine pcpsx_get_data(sender, instance, position)
    integer, intent(out  ) :: sender
    integer, intent(out  ) :: instance
    integer, intent(inout) :: position   
 
    integer                  :: num_workers, ierr, worker_no
    integer, allocatable     :: workers(:)
    integer                  :: ibuff(3), size, level, i_worker

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_get_data, cpu=",   &
        pcps_current_worker_num
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    call ppio_get_worker_with_data(PCPS_PUT_TAG, worker_no, size)
    if(size.gt.pcpsx_buffer_size) &
      call pcpsx_expand_buffer(pcpsx_buffer, pcpsx_buffer_size, 0, size)
    call pcpsx_receive_data(worker_no, size, pcpsx_buffer, PCPS_PUT_TAG)

    position=0;
    call ppio_unpack_integer(ibuff,3,pcpsx_buffer,position)
    instance=ibuff(1)
    sender=ibuff(2)
    num_workers=ibuff(3)

    allocate(workers(1:num_workers),stat=ierr)
    if(ierr.ne.0) &
      call pcpsx_abort("Unable to allocate workers in pcpsx_get_data")
    call ppio_unpack_integer(workers, num_workers, pcpsx_buffer,position)

    do i_worker=1, num_workers
      if(workers(i_worker).eq.pcps_current_worker_num) exit
    enddo
    if(i_worker.gt.num_workers) &
     call pcpsx_abort("logic error in pcpsx_get_data")
   
    level=1
    do while(level<=num_workers)
      if(i_worker.lt.level.and.(i_worker+level).le.num_workers) then
        call pcpsx_send_data(workers(i_worker+level), size, pcpsx_buffer, &
         PCPS_PUT_TAG)
      endif
      level=2*level
    enddo    !level
 
    deallocate(workers, stat=ierr)
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"    pcpsx_get_data-exit: cpu=",   &
       pcps_current_worker_num,&
       ", sender=",sender, ", instance=", instance
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    return
  end subroutine pcpsx_get_data

!-------------------------PCPSX_PUT_TRACES-------------------------
! send trace data using broadcast_group
!
! Written January 2002 by Charles C Burch
!-----------------------------------------------------------------
  subroutine pcpsx_put_traces(num_workers, workers, instance, ntr, &
     hdr, hdr_len, trc, tr_len)
    integer,          intent(in) :: num_workers
    integer,          intent(in) :: workers(:)
    integer,          intent(in) :: ntr, instance, hdr_len, tr_len
    double precision, intent(in) :: hdr(:,:)
    real,             intent(in) :: trc(:,:)
 
    integer                      :: ibuff(4), ntr1
    integer                      :: position, i, n

!   pcpsx_debug=254
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"  pcpsx_put_traces: cpu=",   &
       pcps_current_worker_num, &
       ", instance, ntr, hdr_len, trlen, #wrkrs, 1st wrkr=",&
       instance, ntr, hdr_len, tr_len, num_workers, workers(1)
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
!   pcpsx_debug=0
 
    if(pcps_num_workers.lt.1) return
 
    ntr1=iand(max(ntr,0), NTR_TRACES_MASK)
    n=4*ppio_sizeof_integer+                                                   &
     ntr1*(hdr_len*ppio_sizeof_double_prec+tr_len*ppio_sizeof_real)
    call pcpsx_put_data_init(instance, num_workers, workers, n, position)

    ibuff(1)=ntr
    ibuff(2)=hdr_len
    ibuff(3)=tr_len
    ibuff(4)=pcps_line_break
    call ppio_pack_integer(ibuff,4,pcpsx_buffer,position)
 
    if(ntr1.gt.0) then
      do i=1, ntr1
        call ppio_pack_double_precision(hdr(1:,i), hdr_len, pcpsx_buffer, &
         position)
        call ppio_pack_real(trc(1:,i), tr_len, pcpsx_buffer, position)
      enddo
    endif

    call pcpsx_put_data(num_workers, workers, position)

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_put_traces-exit:cpu=",    &
       pcps_current_worker_num
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    return
  end subroutine pcpsx_put_traces
 
!---------------------PCPSX_GET_TRACES_INFO -------------------------
! get trace data using broadcast_group and extract sender information
!
! Written January 2002 by Charles C Burch
!---------------------------------------------------------------------
  subroutine pcpsx_get_traces_info(sender, instance, position)
    integer, intent(out) :: sender
    integer, intent(out) :: instance
    integer, intent(out) :: position
 
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_get_traces_info:cpu=", & 
       pcps_current_worker_num
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    if(pcps_num_workers.lt.1) return

    call pcpsx_get_data(sender, instance, position)

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"    pcpsx_get_traces-exit: cpu=",  &
       pcps_current_worker_num, ", sender=",sender,", instance=", instance
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    return
  end subroutine pcpsx_get_traces_info

!---------------------PCPSX_GET_TRACES_DATA -------------------------
! extract trace information from a put/get_trace operation
!
! Written January 2002 by Charles C Burch
!---------------------------------------------------------------------
  subroutine pcpsx_get_traces_data(position, ntr, hdr, hdr_len, trc, tr_len)
    integer,          intent(inout) :: position
    integer,          intent(out)   :: ntr
    integer,          intent(out)   :: hdr_len
    integer,          intent(out)   :: tr_len
    double precision, intent(out)   :: hdr(:,:)
    real, intent(out)               :: trc(:,:)
 
    integer                         :: i, ibuff(4), ntr1

    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_get_traces_data:cpu=",  &
        pcps_current_worker_num
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    hdr(1,1) = 0d0 ! -- to satisfy ifort compiler 
    trc(1,1) = 0.0 ! -- to satisfy ifort compiler
    call ppio_unpack_integer(ibuff,4,pcpsx_buffer,position)
    ntr=ibuff(1)
    hdr_len=ibuff(2)
    tr_len=ibuff(3)
    pcps_line_break=ibuff(4)

    if(hdr_len>size(hdr,1) .or. tr_len>size(trc,1) .or. &
       ntr.gt.size(hdr,2) .or. ntr.gt.size(trc,2)) then
      write(pcps_message,*)   &
       "Array overflow in pcpsx_get_traces_data: hdr_len, size(hdr)=", &
       hdr_len, size(hdr,1), "tr_len, size(trc)=",tr_len, size(trc,1), &
       "ntr, size(hdr,2), size(trc,2)=",ntr, size(hdr,2), size(trc,2)
      call pcpsx_abort(pcps_message)
    endif

    ntr1=iand(max(ntr,0), NTR_TRACES_MASK)
    if(ntr1.gt.0) then
      do i=1, ntr1
        call ppio_unpack_double_precision(hdr(1:,i), hdr_len, pcpsx_buffer, &
         position)
        call ppio_unpack_real(trc(1:,i), tr_len, pcpsx_buffer, position)
      enddo
    endif
 
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"    pcpsx_get_traces_data-exit:cpu=",&
       pcps_current_worker_num,                   &
       ", pcpsx_get_traces_data-exit, ntr, hdrlen, trclen=",                  &
       ntr, hdr_len, tr_len
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    return
  end subroutine pcpsx_get_traces_data

!---------------------PCPSX_GET_TRACES_ -------------------------
! recieve traces using a broadcast_group
!
! Written January 2002 by Charles C Burch
!----------------------------------------------------------------
  subroutine pcpsx_get_traces(sender, instance, ntr,                 &
     hdr, hdr_len, trc, tr_len)
    integer, intent(out)          :: sender
    integer, intent(out)          :: instance
    integer, intent(out)          :: ntr
    integer, intent(out)          :: hdr_len
    integer, intent(out)          :: tr_len
    double precision, intent(out) :: hdr(:,:)
    real, intent(out)             :: trc(:,:)
 
    integer                       :: position
 
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"  pcpsx_get_traces: cpu=",  &
       pcps_current_worker_num
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif
 
    if(pcps_num_workers.lt.1) return

    call pcpsx_get_traces_info(sender, instance, position)
    call pcpsx_get_traces_data(position, ntr, hdr, hdr_len, trc, tr_len)
 
    if(iand(pcpsx_debug,PCPSX_DEBUG_SEND_RECEIVE).ne.0) then 
      write(pcps_message, *)"   pcpsx_get_traces-exit:cpu=",   &
       pcps_current_worker_num,                   &
       ", sender, instance, ntr=", sender, instance, ntr
      if(iand(pcpsx_debug,PCPSX_DEBUG_BOSS_ONLY).eq.0 .or. pcps_boss_mode) &
       call pcps_print(pcps_message,2)
    endif

    return
  end subroutine pcpsx_get_traces

!---------------------PCPSX_ABORT -------------------------
! Make best attenpt to have all cpus abort the job
!
! Written January 2002 by Charles C Burch
!----------------------------------------------------------
  subroutine pcpsx_abort(mess, err)
    character(len=*),  intent(in) :: mess
    integer, optional, intent(in) :: err

    integer                       :: error_code
    character(len=512)            :: buff

    if(present(err)) then
      error_code=err
    else
      error_code=1
    endif

    write(buff,*) &
     "PCPS(worker#",pcps_current_worker_num,&
     ") is aborting for the following reason:"
    call pcps_print(buff,2)
    call pcps_print("  "//mess,2)
    call ppio_abort(error_code)
  end subroutine pcpsx_abort

!----------------PCPSX_DUMP_BUFFER_DATA--------------------
! Dump contents of data input buffer
!
! Written February 2002 by Charles C Burch
!----------------------------------------------------------
  subroutine pcpsx_dump_buffer_data(title)
    character(len=*), intent(in) :: title

    character          :: c_title(len(title)), c_flnm(280)
    character(len=280) :: flnm
    integer            :: i, n_title, n_flnm

    n_title=len_trim(title)
    do i=1, n_title
      c_title(i)=title(i:i)
    enddo

    flnm="pcps_buffer_data.tmp"
    n_flnm=len_trim(flnm)
    do i=1, n_flnm
      c_flnm(i)=flnm(i:i)
    enddo

    call pcpsx_dump_buffer_data_c(c_flnm, n_flnm, c_title,n_title)
    i=unix_system("cat "//flnm(1:n_flnm))
    i=cio_remove(flnm(1:n_flnm))
    return
  end subroutine pcpsx_dump_buffer_data

!------------------PCPSX_DUMP_WORKER_STATUS-----------------
! Dump status of the workers
!
! Written February 2002 by Charles C Burch
!----------------------------------------------------------
  subroutine pcpsx_dump_worker_status()
    integer           :: i1, i2
    character(len=80) :: buff

    i1=1
    do while(i1.le.pcps_num_workers)
      do i2=i1+1, pcps_num_workers
        if(pcpsx_worker_modes(1,i2).ne.pcpsx_worker_modes(1,i1)) exit
        if(pcpsx_worker_modes(2,i2).ne.pcpsx_worker_modes(2,i1)) exit
      enddo

      if(pcpsx_worker_modes(1,i1).gt.0) then
        write(pcps_message,*)"Workers ",i1," thru ",i2-1,": instance=", &
         pcpsx_worker_modes(1,i1)

        if(pcpsx_worker_modes(2,i1).eq.NO_MORE_TRACES) then
          write(buff,*) ", ntr=NO_MORE_TRACES"
        else if(pcpsx_worker_modes(2,i1).eq.NEED_TRACES) then
          write(buff,*) ", ntr=NEED_TRACES"
        else
          write(buff,*) ", trace group=",pcpsx_worker_modes(2,i1)
        endif

        pcps_message=trim(pcps_message)//buff
      else if(pcpsx_worker_modes(1,i1).eq.0) then
        write(pcps_message,*)"Workers ",i1," thru ",i2-1,"reserved"
      else
        write(pcps_message,*)"Workers ",i1," thru ",i2-1,"not busy"
      endif

      call pcps_print(pcps_message,2)
      i1=i2
    enddo
    return
  end subroutine pcpsx_dump_worker_status

!--------------------------- pcpsx_set_trace_mode -----------------------------
! set trace mode
! mode=0-default-off
! mode.gt.0 get trace trackin g list at end of job
!  Usually a linked list is used to minimize affecting timing
!  Mode=2 uses disk for data storage which is slower and can change timing
!   behaviors but the collected data can be dumped using a program called
!   "trackdump" in case the job bombs or hangs.
!
! Written February 2002 by Charles C Burch
!------------------------------------------------------------------------------
  subroutine pcpsx_set_trace_mode(mode)
    integer, intent(in) :: mode

    if(pcps_boss_mode.and.pcps_num_procs.gt.1) pcpsx_trace_mode=mode
    return
  end subroutine pcpsx_set_trace_mode

!------------------------ PCPSX_PUT_TRACE_LIST --------------------------------
! put an entry_num into the trace list
!
! Written February 2002 by Charles C Burch
!------------------------------------------------------------------------------
  subroutine pcpsx_put_trace_list_0(instance, hdr, worker, action)
    integer,          intent(in) :: instance
    double precision, intent(in) :: hdr(:)
    integer,          intent(in) :: worker
    character(len=*), intent(in) :: action

    call pcpsx_put_trace_list_2(instance, hdr(HDR_GROUP_NUM), worker, action)
    return
  end subroutine pcpsx_put_trace_list_0

  subroutine pcpsx_put_trace_list_1(instance, hdr, worker, action)
    integer,          intent(in) :: instance
    double precision, intent(in) :: hdr(:,:)
    integer,          intent(in) :: worker
    character(len=*), intent(in) :: action

    call pcpsx_put_trace_list_2(instance, hdr(HDR_GROUP_NUM,1), worker, action)
    return
  end subroutine pcpsx_put_trace_list_1

  subroutine pcpsx_put_trace_list_2(instance, hdr, worker, action)
    integer,          intent(in) :: instance
    double precision, intent(in) :: hdr
    integer,          intent(in) :: worker
    character(len=*), intent(in) :: action

    integer                      :: trace_group

    trace_group=hdr+.5
    call pcpsx_put_trace_list_c(unix_wtime(pcpsx_start_time), trace_group, &
     instance, worker, action, pcpsx_trace_mode)
    return
  end subroutine pcpsx_put_trace_list_2

!------------------------ PCPSX_DUMP_TRACE_LIST ------------------------------
! dump the trace list
!
! Written February 2002 by Charles C Burch
!-----------------------------------------------------------------------------
  subroutine pcpsx_dump_trace_list(title)
    character(len=*), intent(in) :: title

   call pcpsx_dump_trace_list_c(title,len_trim(title))
   return 
  end subroutine pcpsx_dump_trace_list 

!-------------------PCPSX_DUMP_STATUS---------------------
! Dump contents of input data buffer and worker status
!
! Written February 2002 by Charles C Burch
!----------------------------------------------------------
  subroutine pcpsx_dump_status(instance, ntr)
    integer, intent(in) :: instance
    integer, intent(in) :: ntr

    character(len=80)   :: buff

    write(pcps_message,*) "PCPS status: current instance=",instance
    if(ntr.eq.NO_MORE_TRACES) then
      write(buff,*) ", ntr=NO_MORE_TRACES"
    else if(ntr.eq.NEED_TRACES) then
      write(buff,*) ", ntr=NEED_TRACES"
    else if(ntr.eq.LOOP_BACK) then
      write(buff,*) ", ntr=LOOP_BACK"
    else if(ntr.eq.NULL_ACTION) then
      write(buff,*) ", ntr=NULL_ACTION"
    else
      write(buff,*) ", ntr=",ntr
    endif

    pcps_message=trim(pcps_message)//buff
    call pcps_print(pcps_message,2)

    if(pcpsx_last_group.gt.0) then
      write(pcps_message,*) "Last trace group finished=",pcpsx_last_group
      pcpsx_last_group=-pcpsx_last_group
      call pcps_print(pcps_message,2)
    endif

    call pcpsx_dump_buffer_data("PCPS Data Input Buffer status")
    call pcpsx_dump_worker_status()
    call pcps_print(" ",2)
  end subroutine pcpsx_dump_status

!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
 
end module pcpsx_module
 
!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!

