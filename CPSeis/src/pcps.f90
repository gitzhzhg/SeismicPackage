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
! Name       : pcps
! Category   : main_prog
! Written    : 2000-12-08   by: Charles C. Burch
! Revised    : 2007-01-22   by: Bill Menger
! Maturity   : production
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
!  the main program which is used for  prallel processing.  This main program
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
! (integer) function pcps_get_num_procs()
!   returns the number of processors active in a CPS job
!
!-------------------------------------------------------------------------------
! (integer) function pcps_get_num_workers()
!   returns the number of workers active in a CPS job
!   (is zero if only 1 cpu in the job)
!
!-------------------------------------------------------------------------------
! (integer) function pcps_get_worker_num()
!   returns 0 if cpu is the boss and
!           the worker number, if cpu is a worker
!
!-------------------------------------------------------------------------------
! (logical) function pcps_get_boss_mode()
!   returns .true. if current cpu is cpu 0 or the boss, else returns .false.
!
! Note for one cpu PCPS jobs, both boss and worker mode are true
!-------------------------------------------------------------------------------
! (logical) function pcps_get_worker_mode()
!   returns .true. of current cpu is a worker rather than the boss,
!    else returns .false.
!
! Note for one cpu PCPS jobs, both boss and worker mode are true
!-------------------------------------------------------------------------------
! pcps_set_receive_mode(mode, error)
!
! This subroutine is used to set the mode in a parallel group used by the
!   boss for receiving data from the workers.
!
!  The arguments of this subroutine are:
!    integer, intent(in)    :: mode  !defines mode traces are received by boss
!    Valid values are(default PCPS_RECEIVE PASSTHRU): 
!      PCPS_RECEIVE_PASSTHRU-data is received on a worker by worker basis 
!      PCPS_RECEIVE_SUM-data is summed  as it is receieved from each worker
!      PCPS_GROUP_RECEIVE_SUM- data is summed as received from work group 
!      PCPS_RECEIVE_GATHER-data is concatentated as received from each worker
!      PCPS_GROUP_RECEIEVE_GATHER-data is gathers from work group
!      PCPS_RECEIVE_MERGE-data from all the workers is merged as received
!        This mode assumes each workers send their individual traces sorted
!        by the keys define in call pcps_set_merge_parameters
!        This mode will ignore send_mode, alt_send_mode & alt_receive_mode 
!        It also sets TRACE_GEN, NO_BUNCH and SEND_ALL_EOF
!      Cuurently Receive Merge is deactivated
! 
!    logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_alt_receive_mode(mode, error)
!
! This subroutine is used to set the mode in a parallel group used by the boss
!   for sending data to the workers after a NO_MORE_TRACES is received as input 
!   to the do_parallel group
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines mode that traces are received
!   Valid values are (default PCPS_RECEIVE_PASSTHRU): 
!     PCPS_RECEIVE_PASSTHRU-data is received on a worker by worker basis 
!     PCPS_RECEIVE_SUM-data is summed  as it is receieved from each worker 
!     PCPS_GROUP_RECEIVE_SUM- data is summed as received from work group 
!     PCPS_RECEIVE_GATHER-data is concatentated as received from each worker
!     PCPS_GROUP_RECEIEVE_GATHER-data is gathers from work group
!     PCPS_RECEIVE_ALL_EOF-data is received from each worker expected to be EOF
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_boss_exec_mode(mode, error)
!
! This subroutine is used to set the mode in a parallel group by the boss
!   to distibute data or to also exec the processing of the data
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines mode that boss does on data
!    boss_mode indicates how traces are sent to workers & if boss works on data
!   Valid values are (default is PCPS_BOSS_DISTRIBUTES):
!     PCPS_BOSS_DISTRIBUTES-boss sends data to workers but does no work w/data
!     PCPS_BOSS_EXECS-data sent to workers and boss also works on the data
!     PCPS_BOSS_EXECS_GATHER-data sent to workers, boss also works on the data,
!        data is gathered from boss and workers
!     PCPS_BOSS_CONTROLS-data not sent to workers, boss control sending
!
!   In BOSS_EXECS/CONTROL-application needs to form data results
!   IN BOSS_DISTRIBUTES-PCPS gets data from the workers
!
! For compatibility, PCPS_BOSS_EXECS can be used with send mode, which
!  will also set the boss mode
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_send_mode(mode, error)
!
! This subroutine is used to set the mode in a parallel group by the boss
!  for sending data to the workers  
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines mode that traces are sent
!                                     out of a parallelized group
!   Valid values are(default PCPS_SEND_ROUND_ROBIN):
!     PCPS_SEND_ROUND_ROBIN-data sent to a single worker in round robin
!     PCPS_SEND_FIRST_AVAIL-data sent to a single worker that is first available
!     PCPS_SEND_ALL-data is sent to each worker
!     PCPS_BOSS_EXECS-data sent to workers, control transfers to application 
!                     code. receive and eof modes are ignored   
!     PCPS_BOSS_EXECS_MERGE-control transfers to application code -data sent 
!                           to all workers and gathered from workers
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_alt_send_mode(mode, error)
!
! This subroutine is used to set the mode in a parallel group by the boss
!   to send data to the workers after an NO_MORE_TRACES is received as input
!   to the do_parallel group
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines mode that traces are sent
!                                     out of a parallelized group
!    send_mode indicates how traces are sent to workers
!   Valid values are (default is PCPS_SEND_ROUND_ROBIN):
!     PCPS_SEND_ROUND_ROBIN-data sent to a single worker in round robin
!     PCPS_SEND_FIRST_AVAIL-data sent to a single worker that is first available
!     PCPS_SEND_ALL-data is sent to each worker
!     PCPS_SEND_ALL_AVAIL-data sent to all workers that have not sent a EOF
!     If PCPS_SEND_ALL_AVAIL is used,alt receive mode must be PCPS_RECEIVE_GROUP
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_send_eof_mode(mode, error)
!
! This subroutine is used to set the mode in a parallel group by the boss
!  to send NO_MORE_TRACES to the workers after a EOF input is received as
!  input to the do_parallel group
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines mode that EOFs are sent
!                                     out of a parallelized group
!   Valid values are (default is PCPS_SEND_ALL_EOF):
!     PCPS_SEND_ONE_EOF:NMT is sent to first worker only 
!     PCPS_SEND_ALL_EOF:NMT is sent to each worker
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_resequence_mode(mode, error)
!
! This subroutine is used to set the mode in a parallel group bu the boss
!  to control whether received traces are resequenced (header word 1)
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines if traces are resequenced as
!                                     they come out of a parallel goup
!   Valid values are (default is PCPS_NO_RESEQUENCE):
!     PCPS_NO_RESEQUENCE-Traces received by the boss are not resequenced 
!     PCPS_RESEQUENCE_TRACES-Traces received by the boss are resequenced
!     PCPS_RESEQUENCE_GROUPS-header words 1,3,4 are resequenced as received 
!                            by BOSS
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_generator_mode(mode, error)
!
! This subroutine is used to indicate if processes within a parallel
!  group can generate traces or not.
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines if traces are generated by the
!                                    processes in a parallel group
!   Valid values are(default is PCPS_NO_TRACE_GEN:
!     PCPS_NO_TRACE_GEN-no process in a parallel group can not generate traces
!     PCPS_TRACE_GEN-a process in a parallel group can generate traces
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_alt_generator_mode(mode, error)
!
! This subroutine is used to indicate if processes within a parallel
!  group can generate traces or not after receiving an eof.
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines if traces are generated by the
!                                    processes in a parallel group
!   Valid values are(default is PCPS_NO_TRACE_GEN:
!     PCPS_NO_TRACE_GEN-no process in a parallel group can not generate traces
!     PCPS_TRACE_GEN-a process in a parallel group can generate traces
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! *************** This mode is obsolete and should not be used *****************
!                  It is retained just in case for future use
! pcps_set_sort_mode(mode, error)
!
! This subroutine is used to indicate if the boss is to sort received traces 
!   or not using header word 1
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines if traces are sorted or not
!   Valid values are(default is PCPS_NO_SORT):
!     PCPS_NO_SORT-the boss is not to sort the received traces 
!     PCPS_SORT_TRACES-the boss is to sort the received traces using hw 1
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_set_bunch_mode(bunch_mode, error)
!
! This subroutine is used to indicate if the boss is to bunch received traces 
!  or not
!
! The arguments of this subroutine are:
!   integer, intent(in)    :: mode  !defines if traces are bunched not
!   Valid values are(default is PCPS_NO_BUNCH):
!     PCPS_NO_BUNCH-the boss is not to bunch the received traces
!     PCPS_BUNCH_TRACES-the boss is to output the recieved traces in order by
!      header word 1 and save any out of order traces appropriately
!     PCPS_BUNCH_TRACE_GROUPS-the boss is to output the received traces in 
!      groups in order as they were sent to the workers
!
!   logical, intent(out)   :: error  ! true if an error occurs
!-------------------------------------------------------------------------------
! pcps_print(text, flag)
!
! This subroutine prints a message contained in text
!  flag is optional and controls when printing happening
!    not present or 0, prints when in boss mode
!    1, prints only when in worker mode
!    2, prints either in boss or worker mode
!
!  character (len=*), intent(in) :: text
!  integer, optional, intent(in) :: flag
!-------------------------------------------------------------------------------
! subroutine pcps_abort(mess)
! aborts with given message
!
! character (len=*), intent(in) :: mess
!-------------------------------------------------------------------------------
! pcps_errmsg(text)
!
! This subroutine prints a error message contained in text
!
!  character (len=*), intent(in) :: text
!-------------------------------------------------------------------------------
! subroutine pcps_have_more_traces( ntr)
! adjusts ntr to indicate that process
!   has more traces to send
! This routine is optional to use, but it allows PCPS to make advance decisions
!
!   integer, intent(inout) :: ntr
!-------------------------------------------------------------------------------
! subroutine pcps_have_nomore_traces( ntr)
! adjusts ntr to indicate that process
!   has no more traces to send
! This routine is optional to use, but it allows PCPS to make advance decisions
!
!   integer, intent(inout) :: ntr
!-------------------------------------------------------------------------------
! function pcps_i_pel() returns current pe number
!  this function provides same results as pcps_get_worker_num, but is
!  provided for backward capatibility with migration codes
!-------------------------------------------------------------------------------
! function pcps_n_pel() returns number of pe-s
!  this function provides same result as pcps_get_num_procs, but is
!  provided for backward capatibility with migration codes
!-------------------------------------------------------------------------------
! subroutine pcps_get_merge_parameters(hdr_1,init_1,inc_1, &
!                                      hdr_2,init_2,inc_2, hdr_3,init_3,inc_3)
! get merge parameters: hdr_1/2/3, init_1/2/3, inc_1/2/3
!   integer,          intent(out) :: ipn, hdr_1, hdr_2, hdr_3
!   double precision, intent(out) :: init_1, init_2, init_3, inc_1, inc_2, inc_3
!-------------------------------------------------------------------------------
! subroutine pcps_set_merge_parameters(hdr_1,init_1,inc_1, &
!                                      hdr_2,init_2,inc_2, &
!                                      hdr_3,init_3,inc_3, error)
! set merge parameters: hdr_1/2/3, init_1/2/3, inc_1/2/3
!   integer,          intent(in) :: ipn, hdr_1, hdr_2, hdr_3
!   double precision, intent(in) :: init_1, init_2, init_3, inc_1, inc_2, inc_3
!   logical                      :: error
!-------------------------------------------------------------------------------
! Note pcps_line break can be used to determine a trace group is possibly the 
! last trace group of a specific line so a process can start processing it.
!
! If pcps_line_break is -1, this means the programs needs to test for line 
!  breaks.
! If pcps_line_break is 1, this  mean current trace group is the last group of 
! the line and processing of it can start.  The next trace group will be part 
! of a differnt line
! If pcps_line_break is zero, the current trace group is not the last trace 
! group of a line.
!
! PCPS will set the line break flag to 0 or 1, but in non-PCPS mode, the flag
! will be -1
!
! NOTE The merge modes are currently not operational and must not be used
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
!      Date        Author    Description
!      ----        ------    -----------
! 16.  2007-01-22  B Menger  Modified all print statements by removing the *
!                            and including format statements.
! 15.  2004-12-15  M Ried    Added pcps_errmsg
! 14.  2003-09-12  C C Burch Added support of send_line, line_break flag.
!                            Added diagnostic check routines.
! 13.  2002-10-23  Burch CC  Removed get/put_var(now in lnklst)
! 12.  2002-02-11  Burch CC  Added BOSS_EXEC_MODE and work group support
!                            Added pcps_kill_parallel_cpus, pcps_print_f
! 11.  2001-12-10  Burch CC  Added PCPS_RESEQUENCE_GROUPS option 
!                            Added PCPS_BUNCH_TRACE_GROUPS option
!                            PCPS_RESEQUENCE changed to PCPS_RESEQUENCE_TRACES
! 10.  2001-05-25  Burch CC  Added BOSS_EXECS_GATHER/MERGE
!  9.  2001-05-18  Burch CC  Added lnklst_get_var and lnklst_put_var 
!                            Added support for PCPS_RECEIVE_MERGE
!                            Added pcps_online_print_mode
!  8.  2001-04-05  Burch CC  PCPS_BOSS_SENDS_DATA changed to PCPS_BOSS_EXECS 
!  7.  2001-03-26  Burch CC  PCPS_BOSS_SENDS_DATA added
!  6.  2001-03-22  Burch CC  Fixed bug in pcps_n_pel
!  5.  2001-03-20  Burch CC  Added pcps_i_pel and pcps_n_pel for migration codes
!  4.  2001-03-15  Burch CC  Added parameters for task processing
!                            Added pcps_message space for PCPS messages
!  3.  2001-02-05  Burch CC  Added support for PCPS_RECEIVE_ALL_EOF 
!  2.  2000-12-20  Burch CC  Added support for PCPS_SEND_ALL_AVAIL
!  1.  2000-12-08  Burch CC  Initial version
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
! None 
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


module pcps_module
  use lnklst_module
  use named_constants_module

  implicit none

  private

!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!

! The defaults below are replaced when ppio_init is called

  integer, public        :: pcps_num_procs         =1 !#cpus used`
  integer, public        :: pcps_num_workers       =1 !#workers used
  integer, public        :: pcps_current_worker_num=0 !current worker #
  logical, public        :: pcps_boss_mode    =.true. !true if acts as boss
  logical, public        :: pcps_worker_mode  =.true. !true if acts as worker
  integer, public        :: pcps_ipn                  !ipn of current process
  integer, public        :: pcps_pid                  !pid number
  integer, public, pointer :: pcps_pids(:)            !pid number of all pes
  integer, public        :: pcps_ntimer               !ipn for timing
  integer, public        :: pcps_nprocess_list        !#processes in job
  integer, public        :: pcps_line_break=-1        !1 means line break, 
!                           0  means no line break and -1 means not known
  logical, public        :: pcps_backend_exec         !true if process is exec
  character, public      :: pcps_hostname(40)         !hostname
  character, public, pointer :: pcps_hostnames(:,:)   !hostnames of all pes
  character, public      :: pcps_user_dir(80)         !user home directory
  character (len=256),public :: pcps_message          !space for messages

  integer, public        :: pcps_online_print_mode=0        
! The above controls the worker online printing, the modes are:
!   0(default) include worker online in master online-delete worker online files
!   1 include worker online in master online-do not delete worker online files
!   2 do not include worker online in master online-do not delete worker onlines
!   3 do not include worker online in master online-delete worker online files
 
! The defaults below are replaced when pcpsx_init is called

  integer, public        :: pcps_sizeof_integer    =4 !size of integer
  integer, public        :: pcps_sizeof_real       =4 !size of real
  integer, public        :: pcps_sizeof_double_prec=8 !size of double precision
  integer, public        :: pcps_sizeof_complex    =8  !size of complex

! The following are used by the pcps_set_xx routines.
! They are set to defaults when pcpsx_do_parallel_create is called
! They are saved in the do_parallel obj when pcpsx_do_parallel_init is called

  integer, public        :: pcps_receive_mode         !receive mode
  integer, public        :: pcps_alt_receive_mode     !post EOF receive mode
  integer, public        :: pcps_send_eof_mode        !send eof mode
  integer, public        :: pcps_boss_exec_mode       !boss exec mode
  integer, public        :: pcps_send_mode            !send mode
  integer, public        :: pcps_alt_send_mode        !post EOF send mode
  integer, public        :: pcps_resequence_mode      !-no res, 1-reseq
  integer, public        :: pcps_sort_mode            !0-no sort,1 sort\trace#
  integer, public        :: pcps_bunch_mode           !0-no, 1=trace#, 2=group#
  integer, public        :: pcps_alt_bunch_mode       !Post EOF bunch mode
  integer, public        :: pcps_generator_mode    =0 !1 if proc gen traces
  integer, public        :: pcps_alt_generator_mode=0 !Post EOF generator mode

  integer, public        :: pcps_merge_hdr_1, pcps_merge_hdr_2, pcps_merge_hdr_3
  double precision,public:: pcps_merge_int_1, pcps_merge_int_2, pcps_merge_int_3
  double precision,public:: pcps_merge_inc_1, pcps_merge_inc_2, pcps_merge_inc_3
  
! The parameters below are used for the pcps_set_xx routines

  integer,parameter,public :: PCPS_BOSS_DISTRIBUTES=5 !boss send traces to wgs 
  integer,parameter,public :: PCPS_BOSS_CONTROLS   =6 !boss controls data sends
  integer,parameter,public :: PCPS_BOSS_EXECS      =7 !boss execs on sent data 
  integer,parameter,public :: PCPS_BOSS_EXECS_GATHER=8!control transfer to boss-
!                                                     data send and gathered

  integer,parameter,public :: PCPS_SEND_ROUND_ROBIN=1 !send traces round robin
  integer,parameter,public :: PCPS_SEND_FIRST_AVAIL=2 !send traces first avail
  integer,parameter,public :: PCPS_SEND_ALL        =3 !send traces all workers
  integer,parameter,public :: PCPS_SEND_ALL_AVAIL  =4 !send traces to workers
!                                                      that have not sent NMT
  integer,parameter,public :: PCPS_SEND_LINE       =8 !send line first avail
  integer,parameter,public :: PCPS_BOSS_EXECS_MERGE=9 !control transfer to boss-
!                                                     data send and merged
  integer,parameter,public :: PCPS_NO_SEND         =10!bypasses sending data

  integer,parameter,public :: PCPS_RECEIVE_PASSTHRU=1 !receive one worker a time
  integer,parameter,public :: PCPS_RECEIVE_SUM     =2 !sum resukts from each wrk
  integer,parameter,public :: PCPS_RECEIVE_GROUP   =3 !same PCPS_RECEIVE_GATHER
  integer,parameter,public :: PCPS_RECEIVE_GATHER  =3 !group results from wrks
  integer,parameter,public :: PCPS_RECEIVE_ALL_EOF =4 !receive EOF from each wrk
  integer,parameter,public :: PCPS_RECEIVE_MERGE   =5 !merge traces from workers
  integer,parameter,public :: PCPS_GROUP_RECEIVE_SUM   =6!sum traces from wrkgrp
  integer,parameter,public :: PCPS_GROUP_RECEIVE_GATHER=7!gather wrkgrp traces
  integer,parameter,public :: PCPS_RECEIVE_LINE    =8 !receive line-send_line
  integer,parameter,public :: PCPS_NO_RECEIVE      =10!bypass receiving data

  integer,parameter,public :: PCPS_SEND_ONE_EOF    =1 !send EOF to single worker
  integer,parameter,public :: PCPS_SEND_ALL_EOF    =2 !send EOF to each worker

  integer,parameter,public :: PCPS_NO_TRACE_GEN    =0 !||procs do not gen traces
  integer,parameter,public :: PCPS_TRACE_GEN       =1 !||procs generate traces

  integer,parameter,public :: PCPS_NO_RESEQUENCE   =0 !do not resequence traces
! - PCPS_RESEQUENCE(planned to be phased out) changed to PCPS_RESEQUENCE_TRACES
  integer,parameter,public :: PCPS_RESEQUENCE    =1 !resequence traces(obsolete)
  integer,parameter,public :: PCPS_RESEQUENCE_TRACES=1 !resequence traces
  integer,parameter,public :: PCPS_RESEQUENCE_GROUPS=2 !resequence groups

  integer,parameter,public :: PCPS_NO_SORT         =0 !no sorting
  integer,parameter,public :: PCPS_SORT_TRACES     =1 !sort(hw1) worker results

  integer,parameter,public :: PCPS_NO_BUNCH        =0 !no bunch traces from wrks
  integer,parameter,public :: PCPS_BUNCH_TRACES    =1 !bunch by traces (hw1)
  integer,parameter,public :: PCPS_BUNCH_GROUPS    =2 !bunch by groups (hw3)
  integer,parameter,public :: PCPS_BUNCH_TRACE_GROUPS=3 !bunch by boss groups 
  integer,parameter,public :: PCPS_BUNCH_MERGE     =4 !merge traces from workers
 
  integer,parameter,public :: PCPS_OUTSIDE_PARALLEL=0 !process outside parallel
  integer,parameter,public :: PCPS_INSIDE_PARALLEL =1 !process inside parallel
  integer,parameter,public :: PCPS_BOSS_PARALLEL   =2 !process ||, boss sends

! The following are used with pcps_have_more_traces and pcps_have_nomore_traces

  integer,parameter,public :: PCPS_NTR_HAVE_MORE_TRACES  =131072
  integer,parameter,public :: PCPS_NTR_HAVE_NOMORE_TRACES=262144

  integer,parameter,public :: PCPS_BOSS           = 0 !send/rcv task boss
  integer,parameter,public :: PCPS_ANY_WORKER     =-1 !send/rcv task any worker
  integer,parameter,public :: PCPS_ALL_WORKERS    =-2 !send/rcv task all worker
  integer,parameter,public :: PCPS_NEXT_WORKER    =-3 !send/rcv task round robin
  
! Variables for tiny_check
  integer,pointer :: tiny_values(:) 
  integer         :: tiny_n_values=-1, tiny_errs=0, tiny_nt=0, tiny_nmt=0
  integer         :: tiny_passes=0, tiny_n_checks=-1, tiny_mult=1
  real            :: tiny_value_beg=0.0, tiny_value_inc=0.
  real, pointer   :: tiny_checks(:)

  public :: pcps_abort
  public :: pcps_errmsg
  public :: pcps_get_num_procs
  public :: pcps_get_num_workers
  public :: pcps_get_worker_num
  public :: pcps_get_boss_mode
  public :: pcps_get_merge_parameters
  public :: pcps_get_worker_mode
  public :: pcps_have_more_traces
  public :: pcps_have_nomore_traces
  public :: pcps_i_pel
  public :: pcps_kill_parallel_cpus
  public :: pcps_n_pel
  public :: pcps_print
  public :: pcps_set_alt_receive_mode
  public :: pcps_set_alt_send_mode
  public :: pcps_set_boss_exec_mode
  public :: pcps_set_bunch_mode
  public :: pcps_set_generator_mode
  public :: pcps_set_alt_generator_mode
  public :: pcps_save_merge_parameters
  public :: pcps_set_merge_parameters
  public :: pcps_set_receive_mode
  public :: pcps_set_resequence_mode
  public :: pcps_set_send_eof_mode
  public :: pcps_set_send_mode
  public :: pcps_set_sort_mode
  public :: pcps_tiny_check
  public :: pcps_tiny_check_stats
  public :: pcps_tiny_check_init

  interface pcps_tiny_check_init
    module procedure pcps_tiny_check_init0
    module procedure pcps_tiny_check_init1
    module procedure pcps_tiny_check_init2
  end interface

  character(len=100),save,public :: PCPS_ident =                               &
   "$Id: pcps.f90,v 1.16 2007/01/22 14:04:44 Menger prod sps $"

!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!

contains

!----------------- PCPS_GET_NUM_PROCS ------------------
! function(integer) to return number of cpus being useds
!
! Written November 2000 by Charles C Burch
!-------------------------------------------------------
  function pcps_get_num_procs() result(num_procs)
    integer   :: num_procs

    num_procs=pcps_num_procs
    return
  end function pcps_get_num_procs

!-------------- PCPS_GET_NUM_WORKERS ------------------
! function(integer) to return number of workers
!
! Written November 2000 by Charles C Burch
!------------------------------------------------------
  function pcps_get_num_workers() result(num_workers)
    integer   :: num_workers

    num_workers=pcps_num_workers
    return
  end function pcps_get_num_workers

!-------------- PCPS_GET_WORKER_NUM -------------------
! function(integer) to return worker number
!   (boss=0, workers=1,..
!
! Written November 2000 by Charles C Burch
!------------------------------------------------------
  function pcps_get_worker_num() result(worker_num)
    integer   :: worker_num

    worker_num=pcps_current_worker_num
    return
  end function pcps_get_worker_num


!--------------- PCPS_GET_BOSS_MODE -------------------
! function (logical) to return if cpu is boss
!
! Written November 2000 by Charles C Burch
!------------------------------------------------------
  function pcps_get_boss_mode() result(boss_mode)
    logical   :: boss_mode

    boss_mode=pcps_boss_mode
    return
  end function pcps_get_boss_mode

!------------------ PCPS_GET_WORKER_MODE --------------------------
! function (logical) to return if cpu is worker
!  In PCSP mode and with only one cpu, cpu is both boss and worker
!  In non-PCPS with one cpu, cpu is boss and not worker
!
! Written November 2000 by Charles C Burch
!-----------------------------------------------------------------
  function pcps_get_worker_mode() result(worker_mode)
    logical  :: worker_mode

    worker_mode=pcps_worker_mode
    return
  end function pcps_get_worker_mode

!------------- PCPS_SET_BOSS_EXEC_MODE ----------------
! set boss mode
!
! Written January  2002 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_boss_exec_mode(boss_mode,error)

    integer, intent(in)   :: boss_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(boss_mode.eq.PCPS_BOSS_DISTRIBUTES  .or. &
       boss_mode.eq.PCPS_BOSS_CONTROLS     .or. &
       boss_mode.eq.PCPS_BOSS_EXECS_GATHER .or. &
       boss_mode.eq.PCPS_BOSS_EXECS        )then
      pcps_boss_exec_mode=boss_mode
    else
      write(message,'(A,I6,A)')'Invalid boss mode(',boss_mode,                        &
       ') in pcps_set_boss_mode'
      call pcps_print(message)
      error=.true.
      pcps_boss_exec_mode=PCPS_BOSS_DISTRIBUTES
    endif

    return
  end subroutine pcps_set_boss_exec_mode

!-------------------- PCPS_SET_SEND_MODE --------------
! set send mode
!
! Written October 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_send_mode(send_mode,error)

    integer, intent(in)   :: send_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(send_mode.eq.PCPS_SEND_ROUND_ROBIN  .or. &
       send_mode.eq.PCPS_SEND_FIRST_AVAIL  .or. &
       send_mode.eq.PCPS_SEND_ALL          .or. &
!      send_mode.eq.PCPS_BOSS_EXECS_MERGE  .or. &
       send_mode.eq.PCPS_BOSS_EXECS_GATHER .or. &
       send_mode.eq.PCPS_BOSS_CONTROLS     .or. &
       send_mode.eq.PCPS_SEND_LINE         .or. &
       send_mode.eq.PCPS_BOSS_EXECS        ) then
      pcps_send_mode=send_mode
    else
      write(message,'(A,I6,A)')'Invalid send mode(',send_mode,') in pcps_set_send_mode'
      call pcps_print(message)
      error=.true.
    endif

    return

  end subroutine pcps_set_send_mode

!----------- PCPS_SET_ALT_SEND_MODE -------------------
! set send mode post eof
!
! Written October 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_alt_send_mode(send_mode,error)

    integer, intent(in)   :: send_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(send_mode.eq.PCPS_SEND_ROUND_ROBIN .or. &
       send_mode.eq.PCPS_SEND_FIRST_AVAIL .or. &
       send_mode.eq.PCPS_SEND_ALL         .or. &
       send_mode.eq.PCPS_SEND_ALL_AVAIL   .or. &
!      send_mode.eq.PCPS_BOSS_EXECS_MERGE .or. &
       send_mode.eq.PCPS_BOSS_EXECS       ) then
      pcps_alt_send_mode=send_mode
    else
      write(message,'(A,I6,A)')'Invalid send mode(',send_mode,') in set_alt_send_mode'
      call pcps_print(message)
      error=.true.
    endif

    return
  end subroutine pcps_set_alt_send_mode

!------------- PCPS_SET_SEND_EOF_MODE -----------------
! set eof mode
!
! Written October 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_send_eof_mode(eof_mode,error)

    integer, intent(in)   :: eof_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(eof_mode.eq.PCPS_SEND_ONE_EOF .or. eof_mode.eq.PCPS_SEND_ALL_EOF) then
      pcps_send_eof_mode=eof_mode
    else
      write(message,'(A,I6,A)')'Invalid eof mode(',eof_mode,') in set_send_eof_mode'
      call pcps_print(message)
      error=.true.
    endif

    return
  end subroutine pcps_set_send_eof_mode

!------------- PCPS_SET_RECEIVE_MODE ------------------
! set receive mode
!
! Written October 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_receive_mode(receive_mode,error)

    integer, intent(in)   :: receive_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(receive_mode.eq.PCPS_RECEIVE_PASSTHRU     .or. &
       receive_mode.eq.PCPS_RECEIVE_SUM          .or. &
!      receive_mode.eq.PCPS_RECEIVE_MERGE        .or. &
       receive_mode.eq.PCPS_GROUP_RECEIVE_SUM    .or. &
       receive_mode.eq.PCPS_GROUP_RECEIVE_GATHER .or. &
       receive_mode.eq.PCPS_RECEIVE_GATHER)      then
      pcps_receive_mode=receive_mode
    else
      write(message,'(A,I6,A)') 'Invalid receive mode(',receive_mode,') in set_receive_mode'
      call pcps_print(message)
      error=.true.
    endif

    return
  end subroutine pcps_set_receive_mode

!------------ PCPS_SET_ALT_RECEIVE_MODE ---------------
! set receive mode_post_eof
!
! Written October 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_alt_receive_mode(receive_mode,error)

    integer, intent(in)   :: receive_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(receive_mode.eq.PCPS_RECEIVE_PASSTHRU     .or. &
       receive_mode.eq.PCPS_RECEIVE_SUM          .or. &
!      receive_mode.eq.PCPS_RECEIVE_MERGE        .or. &
       receive_mode.eq.PCPS_GROUP_RECEIVE_SUM    .or. &
       receive_mode.eq.PCPS_GROUP_RECEIVE_GATHER .or. &
       receive_mode.eq.PCPS_RECEIVE_GATHER       .or. &
       receive_mode.eq.PCPS_RECEIVE_ALL_EOF   ) then 
      pcps_alt_receive_mode=receive_mode
    else
      write(message,'(A,I6,A)') 'Invalid receive mode_post_eof(',receive_mode,') in set_alt_receive_mode'
      call pcps_print(message)
      error=.true.
    endif

    return
  end subroutine pcps_set_alt_receive_mode
  
!-------------- PCPS_SET_GENERATOR_MODE ---------------
! set  generator mode
!
! Written October 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_generator_mode(generator_mode,error)

    integer, intent(in)   :: generator_mode
    logical, intent(out)  :: error

    if(generator_mode.eq.PCPS_TRACE_GEN) then
      pcps_generator_mode=1
    else
      if(generator_mode.ne.PCPS_NO_TRACE_GEN) then
        error=.true.
      endif
      pcps_generator_mode=0
    endif

    return
  end subroutine pcps_set_generator_mode

!--------------- PCPS_SET_ALT_GENERATOR_MODE ----------
! set alt generator mode
!
! Written January 2002 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_alt_generator_mode(generator_mode,error)

    integer, intent(in)   :: generator_mode
    logical, intent(out)  :: error

    if(generator_mode.eq.PCPS_TRACE_GEN) then
      pcps_alt_generator_mode=1
    else
      if(generator_mode.ne.PCPS_NO_TRACE_GEN) then
        error=.true.
      endif
      pcps_alt_generator_mode=0
    endif

    return
  end subroutine pcps_set_alt_generator_mode

!--------------PCPS_SET_RESEQUENCE_MODE ---------------
! set resequence mode
!
! Written October 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_resequence_mode(resequence_mode,error)

    integer, intent(in)   :: resequence_mode
    logical, intent(out)  :: error

    if(resequence_mode.eq.PCPS_RESEQUENCE_TRACES  .or. &
       resequence_mode.eq.PCPS_NO_RESEQUENCE      .or. &
       resequence_mode.eq.PCPS_RESEQUENCE_GROUPS) then
      pcps_resequence_mode=resequence_mode
    else
      error=.true.
    endif

    return
  end subroutine pcps_set_resequence_mode

!---------------- PCPS_SET_SORT_MODE ------------------
! set sort mode
!
! Written November 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_sort_mode(sort_mode,error)

    integer, intent(in)   :: sort_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(sort_mode.ge.0 .and. sort_mode.le.1) then
      pcps_sort_mode=sort_mode
    else
      write(message,'(A,I6,A)')'Invalid sort mode(',sort_mode,') in pcps_set_sort_mode'
      call pcps_print(message)
      error=.true.
      pcps_sort_mode=0
    endif

    return
  end subroutine pcps_set_sort_mode

!---------------- PCPS_SET_BUNCH_MODE -----------------
! set bunch mode
!
! Written November 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_bunch_mode(bunch_mode,error)

    integer, intent(in)   :: bunch_mode
    logical, intent(out)  :: error

    character (len=80)    :: message

    if(bunch_mode.eq.PCPS_NO_BUNCH     .or. &
       bunch_mode.eq.PCPS_BUNCH_TRACES .or. &
       bunch_mode.eq.PCPS_BUNCH_GROUPS .or. &
       bunch_mode.eq.PCPS_BUNCH_TRACE_GROUPS)then
      pcps_bunch_mode=bunch_mode
    else
      write(message,'(A,I6,A)')'Invalid bunch mode(',bunch_mode,') in pcps_set_bunch_mode'
      call pcps_print(message)
      error=.true.
      pcps_bunch_mode=PCPS_NO_BUNCH
    endif

    return
  end subroutine pcps_set_bunch_mode

!------------ PCPS_SET_MERGE_PARAMETERS ---------------
! set merge parameters 
! Note the keys are the same as used with TSORT
!
! Written May 2001 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_set_merge_parameters(hdr_1,int_1,inc_1, hdr_2,int_2,inc_2, &
                                       hdr_3,int_3,inc_3, error)
    integer,          intent(in)  :: hdr_1, hdr_2, hdr_3
    double precision, intent(in)  :: int_1, int_2, int_3, inc_1, inc_2, inc_3
    logical,          intent(out) :: error

    pcps_merge_hdr_1=hdr_1
    pcps_merge_int_1=int_1
    pcps_merge_inc_1=inc_1

    pcps_merge_hdr_2=hdr_2
    pcps_merge_int_2=int_2
    pcps_merge_inc_2=inc_2

    pcps_merge_hdr_3=hdr_3
    pcps_merge_int_3=int_3
    pcps_merge_inc_3=inc_3

    if(inc_1.eq.0 .or. inc_2.eq.0 .or. inc_3.eq.0) then
      call pcps_abort("ERROR:inc is zero in pcps_set_merge_parameters")
      error=.true.
    endif

    if(hdr_1.lt.0 .or.hdr_2.lt.0 .or. hdr_3.lt.0) then
      call pcps_abort("ERROR:Invalid header words in pcps_set_merge_parameters")
      error=.true.
    endif

    return
  end subroutine pcps_set_merge_parameters

!------------- PCPS_SAVE_MERGE_PARAMETERS -------------
! save merge parameters 
! Note the keys are the same as used with TSORT
!
! Written May 2001 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_save_merge_parameters()
    call lnklst_put_var("PCPS_MERGE_HDR_1", PCPS_MERGE_HDR_1)
    call lnklst_put_var("PCPS_MERGE_INT_1", PCPS_MERGE_INT_1)
    call lnklst_put_var("PCPS_MERGE_INC_1", PCPS_MERGE_INC_1)

    call lnklst_put_var("PCPS_MERGE_HDR_2", PCPS_MERGE_HDR_2)
    call lnklst_put_var("PCPS_MERGE_INT_2", PCPS_MERGE_INT_2)
    call lnklst_put_var("PCPS_MERGE_INC_2", PCPS_MERGE_INC_2)

    call lnklst_put_var("PCPS_MERGE_HDR_3", PCPS_MERGE_HDR_3)
    call lnklst_put_var("PCPS_MERGE_INT_3", PCPS_MERGE_INT_3)
    call lnklst_put_var("PCPS_MERGE_INC_3", PCPS_MERGE_INC_3)
    return
  end subroutine pcps_save_merge_parameters
  
!------------- PCPS_GET_MERGE_PARAMETERS --------------
! get merge parameters 
!
! Written May 2001 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_get_merge_parameters(hdr_1,int_1,inc_1, &
                                       hdr_2,int_2,inc_2, hdr_3,int_3,inc_3)
    integer,          intent(out) :: hdr_1, hdr_2, hdr_3
    double precision, intent(out) :: int_1, int_2, int_3, inc_1, inc_2, inc_3

    call lnklst_get_var("PCPS_MERGE_HDR_1", hdr_1)
    call lnklst_get_var("PCPS_MERGE_INT_1", int_1)
    call lnklst_get_var("PCPS_MERGE_INC_1", inc_1)

    call lnklst_get_var("PCPS_MERGE_HDR_2", hdr_2)
    call lnklst_get_var("PCPS_MERGE_INT_2", int_2)
    call lnklst_get_var("PCPS_MERGE_INC_2", inc_2)

    call lnklst_get_var("PCPS_MERGE_HDR_3", hdr_3)
    call lnklst_get_var("PCPS_MERGE_INT_3", int_3)
    call lnklst_get_var("PCPS_MERGE_INC_3", inc_3)

    return
  end subroutine pcps_get_merge_parameters
  
!----------------- PCPS_PRINT -------------------------
! prints a message in text
! flag is an optional argument to control printing,
!   not present or 0, print by boss only
!   1 print if worker
!   2 print in all cases
!
! Written November 2000 by Charles C Burch
!------------------------------------------------------
  subroutine pcps_print (text, flag)
    character (len=*), intent(in)   :: text
    integer, optional               :: flag

    integer         :: flag_used
    integer,save    :: lun=-1

    if(present(flag)) then
      flag_used=flag
    else
      flag_used=0
    endif

    lun=6   !    if(lun.lt.0) call pc_get_lun(lun)

    select case (flag_used)
    case (0)
      if(pcps_current_worker_num.eq.0) write(lun,'(A)') trim(text)

    case (1)
      if(pcps_num_procs.gt.1 .and. pcps_current_worker_num.eq.1)               &
       write(lun,'(A)') trim(text)
    case (2)
      write(lun,'(A)') trim(text)

    case default
      write(lun,'(A,I6,A)') "Invalid flag(",flag_used,") in pcps_print, input text was:"
      write(lun,'(A)') trim(text)
    end select
    return
  end subroutine pcps_print

!================ PCPS_ABORT ==================
! aborts with given message
!
! Written September 2000 by Charles C Burch
!==============================================
  subroutine pcps_abort(mess)
    character (len=*), intent(in) :: mess

    call pcps_abort_c(mess,len_trim(mess))
  end subroutine pcps_abort

!================ PCPS_ERRMSG ==================
! displays the given error message
!==============================================
  subroutine pcps_errmsg(mess)
    character (len=*), intent(in) :: mess

    call unix_errmsg_c(mess)
  end subroutine pcps_errmsg

!----------------- PCPS_HAVE_MORE_TRACES -------------
! adjusts ntr to indicate that process
!   has more traces to send
!
! Written August 2000 by Charles C Burch
!-----------------------------------------------------
  subroutine pcps_have_more_traces( ntr)
    integer, intent(inout) :: ntr

    if(ntr>0) ntr=ior(ntr,PCPS_NTR_HAVE_MORE_TRACES)
    return
  end subroutine pcps_have_more_traces

!------------- PCPS_HAVE_NOMORE_TRACES ---------------
! adjusts ntr to indicate that process
!   has no more traces to send
!
! Written August 2000 by Charles C Burch
!-----------------------------------------------------
  subroutine pcps_have_nomore_traces( ntr)
    integer, intent(inout) :: ntr

    if(ntr>0) ntr=ior(ntr,PCPS_NTR_HAVE_NOMORE_TRACES)
    return
  end subroutine pcps_have_nomore_traces

!--------------- PCPS_KILL_JOB -----------------------
! kill all cpus(not current cpu) in a PCPS job 
!
! Written January 2002 by Charles C Burch
!-----------------------------------------------------
  subroutine pcps_kill_parallel_cpus(signal)
    integer, intent(in) :: signal
    integer             :: i_cpu

    do i_cpu=0,pcps_num_procs-1
      if(pcps_current_worker_num.ne.i_cpu)                                   &
        call pcps_kill_remote_job(pcps_hostnames(:,i_cpu), &
          size(pcps_hostnames,1), pcps_pids(i_cpu), signal)
    enddo

    return
  end subroutine pcps_kill_parallel_cpus

!----------------- PCPS_I_PEL AND PCPS_N_PEL ------------------------
! --- pcps_i_pel and pcps_n_pel added for backwards capatibility with
!      migration codes

  integer function pcps_i_pel ( )      !  return current pe index
    
    pcps_i_pel = pcps_current_worker_num
    return
  end function pcps_i_pel

  integer function pcps_n_pel ( )      !  return number of pe's 
    
    pcps_n_pel = pcps_num_procs
    return
  end function pcps_n_pel

!===============================================================
! simple diagnostics for pcps test programs
! These are not intended for general use
!
! Written July 2003 by Charles C Burch
!===============================================================
subroutine pcps_tiny_check_init0(n, value_beg, value_inc)
  integer, intent(in) :: n
  real,    intent(in) :: value_beg, value_inc

  integer :: i

  if(.not.pcps_boss_mode) return

  tiny_value_beg=value_beg
  tiny_value_inc=value_inc
  
  if(n.lt.1) then
    print *,"Invalid n in pcps_tiny_check_init"
    stop
  endif

  allocate(tiny_values(n),stat=i)
  tiny_n_values=0

  tiny_passes=0
  tiny_nt=0
  tiny_nmt=0
  tiny_errs=0
  tiny_n_checks=0
  tiny_mult=1
  nullify(tiny_checks)
  return
end subroutine pcps_tiny_check_init0

subroutine pcps_tiny_check_init1(n, n_checks, checks)
  integer, intent(in) :: n, n_checks
  real,    intent(in) :: checks(:)

  integer :: i

  if(.not.pcps_boss_mode) return

  if(n.lt.1) then
    print *,"Invalid n in pcps_tiny_check_init"
    stop
  endif

  if(n_checks.lt.1) then
    print *,"Invalid n_checks in pcps_tiny_check_init"
    stop
  endif

  allocate(tiny_values(n),stat=i)
  tiny_n_values=0

  tiny_n_checks=n_checks
  allocate(tiny_checks(n_checks),stat=i)
  tiny_n_checks=n_checks
  tiny_checks(:)=checks(:) 

  tiny_passes=0
  tiny_nt=0
  tiny_nmt=0
  tiny_errs=0
  tiny_mult=1
  nullify(tiny_checks)
  return
end subroutine pcps_tiny_check_init1

subroutine pcps_tiny_check_init2(n, value_beg, value_inc, mult)
  integer, intent(in) :: n, mult
  real,    intent(in) :: value_beg, value_inc

  integer :: i

  if(.not.pcps_boss_mode) return

  tiny_value_beg=value_beg
  tiny_value_inc=value_inc
  
  if(n.lt.1) then
    print *,"Invalid n in pcps_tiny_check_init"
    stop
  endif

  allocate(tiny_values(n),stat=i)
  tiny_n_values=0

  tiny_passes=0
  tiny_nt=0
  tiny_nmt=0
  tiny_errs=0
  tiny_n_checks=0
  tiny_mult=1
  tiny_mult=mult
  nullify(tiny_checks)
  return
end subroutine pcps_tiny_check_init2

subroutine pcps_tiny_check(ntr, hd, tr)
  integer         ,intent(inout)  :: ntr
  real            ,intent(inout)  :: tr(:,:)
  double precision, intent(inout) :: hd(:,:)

  integer                     :: i
  real                        :: check
 
  if(.not.pcps_boss_mode) return

  if(tiny_n_values.lt.0) then
    print *,"tiny_check not initialized properly"
    stop
  endif
    
  tiny_passes=tiny_passes+1
  if(ntr.gt.0) then
    do i=1,ntr
      if(tiny_n_values.ge.size(tiny_values)) then
        print *,"tiny_value array overflow in check"
        stop
      endif

      tiny_n_values=tiny_n_values+1
      if(tiny_n_checks.gt.0) then
        if(tiny_n_values.gt.tiny_n_checks) then
          check=tiny_checks(mod(tiny_n_values-1,tiny_n_checks)+1)
        else
          check=tiny_checks(tiny_n_values)
        endif
      else
        check=tiny_value_beg+((tiny_n_values-1)/tiny_mult)*tiny_value_inc
      endif

      if(tr(1,i).ne.check) then
        print *,"tiny check value mismatch:n_val, val, tr=",&
         tiny_n_values, check, tr(1,i)
        tiny_errs=tiny_errs+1
      endif
      tiny_values(tiny_n_values)=tr(1,i)
    enddo
 
  else if(ntr.eq.NO_MORE_TRACES) then
    tiny_nmt=tiny_nmt+1

  else if(ntr.eq.NEED_TRACES) then
    tiny_nt=tiny_nt+1

  else 
    print *,"Invalid ntr in tiny_check:",ntr
    stop
  endif
  return
end subroutine pcps_tiny_check 

subroutine pcps_tiny_check_stats()
  integer :: i
 
  if(.not.pcps_boss_mode) return

  if(tiny_passes.eq.0) then
    print *,"No traces processed by tiny_check"
  else
    if(tiny_nmt.ne.1) then
      print *,"Invalid no_more_trace status in tiny_check:",tiny_nmt
    endif

    if(tiny_errs.gt.0) then
      print *,"#errors=",tiny_errs
      do i=1, tiny_n_values
        print *,"i, value=",i,tiny_values(i)
      enddo
    else
      print *,"No errors found by tiny_check, #passes=",tiny_passes, &
               ", #Need_traces=",tiny_nt
    endif
  endif

  if(tiny_n_values.ge.0) then
    deallocate(tiny_values,stat=i)
    tiny_n_values=-1
    nullify(tiny_values)
  endif

  if(tiny_n_checks.gt.0) then
    deallocate(tiny_checks,stat=i)
    tiny_n_checks=-1
    nullify(tiny_checks)
  endif


  return
end subroutine pcps_tiny_check_stats  

!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!

end module pcps_module

!--------------- PCPS_PRINT_F -----------------------
! print string c_str(1:n_str)
! Modeled after Donna Vunderink's cps_print_f
! Written to avoid link conflicts using cps_print_f 
!
! Written January 2002 by Charles C Burch
!----------------------------------------------------
  subroutine pcps_print_f(c_str, n_str)
    use pcps_module
    implicit none

    character, intent(in) :: c_str(*)
    integer,   intent(in) :: n_str

    character(len=n_str)  :: str
    integer               :: i

    do i=1, n_str
      if(c_str(i).eq.char(0)) exit
      str(i:i)=c_str(i)
    enddo

    if(i.eq.1) then
      i=2
      str=" "
    endif

    call pcps_print(str(1:i-1), 2)
    return
  end subroutine pcps_print_f

!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
