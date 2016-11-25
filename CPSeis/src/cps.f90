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
! Name       : cps 
! Category   : main_prog
! Written    : 1999-06-22   by: Tom Stoeckley
! Revised    : 2007-02-15   by: Bill Menger
! Maturity   : beta
! Purpose    : Helper routines for the CPS main program for batch processing.
! Portability: No known limitations, but see notes below.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! 
!  This primitive contains a collection of routines which are called from
!  the main program which is used for batch processing.  This main program
!  is built by the job builder.  This primitive encapsulates much of the
!  code which would otherwise have to be built into the main program.  The
!  main program therefore contains only that code which changes from job
!  to job and is dependent on the list of process modules used in the job.
!
!  See the Specifications for the Back-End Main Program for a representation
!  of how this primitive is used from the main program.
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
!                      INPUT AND OUTPUT ARGUMENTS
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                       CPS_START_PROCESSING
!
! The following subroutine is to be called as the first executable step
! in the batch processing main program, before any setups are perfomed:
!
!                                   opt           opt         opt     opt
!                                    o             o           i       o
!    call cps_start_processing (num_processes,process_names,workfile,error)
!
! This subroutine contains the following arguments:
!
!   integer     num_processes    = number of CPS processes in the job.
!   char(len=*) process_names(:) = list of CPS processes in the job.
!   char(len=*) workfile         = name of workfile or process_parameters file.
!   logical     error            = true if an error occurred.
!
! This subroutine performs the following duties:
!
!   (1) Get project data parameters from the job file.
!   (2) Get job data parameters from the job file.
!   (3) Make a list of all processes in the job from the job file.
!   (4) Writes the "Job started" accounting record.
!   (5) Writes the banner to the reportfile.
!
!-------------------------------------------------------------------------------
!                              CPS_PRINT
!
! The following subroutine is used to print a message from the main program:
!
!                                     i
!                    call cps_print (mess)
!
! This subroutine contains the following argument:
!
!   character(len=*) mess = message text
!
!-------------------------------------------------------------------------------
!                          CPS_PRINT_RCS_IDENT
!
! The following subroutine is used to get the RCS Ident string from the main
! program for printing CPS release information in the reportfile: 
!
!                                           i
!               call cps_print_rcs_ident (ident)
!
! This subroutine contains the following argument:
!
!   character(len=*) ident = RCS Ident string for CPS process
!
!-------------------------------------------------------------------------------
!                          CPS_CUSTOM_RCS_IDENT
!
! The following subroutine is used to read the RCS Ident strings from the file
! "custom_rcs_ident" for printing release information in the reportfile: 
!
!                    call cps_custom_rcs_ident
!
! This subroutine contains no arguments.
!
!-------------------------------------------------------------------------------
!                         CPS_START_SETUPS
!
! The following subroutine is to be called before setups are preformed to
! initialize the parameter cache for a processing loop:
!
!                                       opt
!                                        o
!               call cps_start_setups (error)
!
! This subroutine contains the following argument:
!
!   logical error = true if an error occurred.
!
! This subroutine performs the following duties:
!
!   (1) Initialize the parameter cache.
!
!-------------------------------------------------------------------------------
!                         CPS_FINISH_SETUPS
!
! The following subroutine is to be called after all setups for a loop are 
! perfomed:
!
!                                    o
!          call cps_finish_setups (error)
!
! This subroutine contains the following argument:
!
!   logical error = true if an error occurred in the setups.
!
! This subroutine performs the following duties:
!
!   (1) Checks the parameter cache for errors and aborts the job if there
!       are any.
!
!-------------------------------------------------------------------------------
!                          CPS_FINISH_PROCESSING
!
! The following subroutine is to be called after all processing has been
! completed:
!
!                                           i
!             call cps_finish_processing (error)
!
! This subroutine contains the following argument:
!
!   logical error = true if an error occurred during setup or trace processing.
!
! This subroutine performs the following duties:
!
!   (1) Writes the job's usage information to the reportfile.
!   (2) Calls the HIST module to print the job history in the reportfile.
!   (3) Writes the "Job finished" accounting record.
!
!-------------------------------------------------------------------------------
!                         CPS_SETUP_TOP_OF_LOOP
!
! The following subroutine is to be called at the top of each trace-flow
! loop, before the CPS_PRE_SETUP call for the first process in the loop
! is called:
!
!               call cps_setup_top_of_loop
!
! This subroutine contains no arguments.
!
! This subroutine performs the following duties:
!
!   (1) Currently not used.
!   (2) Initializes the NUMTR global to zero.
!
!
!-------------------------------------------------------------------------------
!                           CPS_PRE_SETUP
!
! The following subroutine is to be called before each process is created:
!
!                    call cps_pre_setup
!
! This subroutine contains no arguments.
!
! This subroutine performs the following duties:
!
!   (1) Clear the parameter cache (except for globals).
!   (2) Get process parameters for this process from the job file.
!   (3) Put process parameters into the parameter cache.
!   (4) Print input process parameters in the reportfile.
!
!-------------------------------------------------------------------------------
!                           CPS_POST_SETUP
!
! The following subroutine is to be called after each process is created:
!
!                    call cps_post_setup
!
! This subroutine contains no arguments.
!
! This subroutine performs the following duties:
!
!   (1) Get process parameters and other information from the parameter cache.
!   (2) Pass process parameters to the history file.
!   (3) Initialize TRSCAN primitive.
!   (3) Print information.
!
!-------------------------------------------------------------------------------
!                           CPS_PRE_PROCESS
!
! The following subroutine is to be called before each process execution
! routine:
!
!                                      i
!               call cps_pre_process (ipn)
!
! This subroutine contains the following argument:
!
!     integer ipn = process number of the following process.
!
! This subroutine performs the following duties:
!
!   (1) Starts timer for following process.
!
!-------------------------------------------------------------------------------
!                           CPS_POST_PROCESS
!
! The following subroutine is to be called after each process in the
! trace-flow loop is called:
!
!                                     i   i  b  i
!             call cps_post_process (ipn,ntr,hd,tr)
!
! This subroutine contains the following arguments:
!
!     integer ipn              = process number of the preceding process.
!     integer ntr              = number of traces output by the preceding
!                                process, or FATAL_ERROR or NEED_TRACES or
!                                NO_MORE_TRACES.
!     double precision hd(:,:) = trace headers output by the preceding process.
!     real             tr(:,:) = traces output by the preceding process.
!
! This subroutine performs the following duties:
!
!   (1) Stops timer for preceding process.
!   (2) Calls TRSCAN primitive to gather statistics on data output by preceding
!       process.
!
!-------------------------------------------------------------------------------
!                           CPS_TOP_OF_LOOP
!
! The following subroutine is to be called at the top of each trace-flow
! loop, before the first process in the loop is called:
!
!                                         i
!                  call cps_top_of_loop (ipn)
!
! This subroutine contains the following argument:
!
!        integer ipn = process number of the following process.
!
! This subroutine performs the following duties:
!
!   (1) Used but currently empty.
!
!-------------------------------------------------------------------------------
!                           CPS_GET_LUN
!
! The following function is to be called to get the logical unit number for
! printing.
!                                   
!                  lun = cps_get_lun()
!
! This subroutine returns the following value:
!
!     integer lun = logical unit number for printing
!
!-------------------------------------------------------------------------------
!                              CPS_ABORT
!
! The following subroutine is used to print a message and then abort the
! program:
!
!                                 i
!                call cps_abort (mess)
!
! This subroutine contains the following argument:
!
!   character(len=*) mess = message text
!
!-------------------------------------------------------------------------------
!                            CPS_ERROR_EXIT
!
! The following subroutine is used to print a message and then abort the
! program:
!
!                                     i
!               call cps_error_exit (mess)
!
! This subroutine contains the following argument:
!
!   character(len=*) mess = message text
!
!-------------------------------------------------------------------------------
!                            CPS_PRINT_CURRENT_STATUS
!
! The following subroutine is used to print location and memory information
! when aborting due to the signal handler: 
!
!               call cps_print_current_status
!
! This subroutine contains no arguments.
!
!-------------------------------------------------------------------------------
!                            CPS_GET_PROJECT_NAME
!
! Gets the current project name, which can be set by an environment variable or
! by a previous call to CPS_SET_PROJECT_NAME.  The variable is:
! CPS_PROJECT_NAME and cannot contain >15 chars, no white space.
!                                             o
!               call cps_get_project_name(project_name)
!               character(len=*), intent(out) :: project_name
!
!-------------------------------------------------------------------------------
!                            CPS_SET_PROJECT_NAME
!
! Sets the current project name, which can also be set by environment variable:
! CPS_PROJECT_NAME and cannot contain >15 chars, no white space.
!                                             i
!               call cps_set_project_name(project_name)
!               character(len=*), intent(in) :: project_name
!
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
! 61. 2007-02-15  Bill Menger  Modified the cio_fopen call to use unique file 
!                              names for each pid, host.
! 60. 2007-01-25  Bill Menger  Removed putsys module reference.
! 59. 2006-10-31  Bill Menger  In the first cio_fopen call, added a "scratch"
!                              flag to remove the file that is created.
!                              Modified parameter file reading to use cio.
! 58. 2006-10-10  D. Glover    Added NULLIFY statements for Intel compiler
! 57. 2006-01-31  B. Menger    Add cps_accounting logger calls.
! 56. 2006-01-10  B. Menger    Removed Unused Variables.
! 55. 2005-07-11  Bill Menger  Modified the way pfio_init() is invoked.
! 54. 2005-06-14  Bill Menger  Add set/get project name calls.
! 53. 2004-12-15  Stoeckley    Shift some printouts so that they will begin
!                               at the same character position for both batch
!                               and icps output to facilitate comparisons.
! 52. 2004-08-26  Michael Ried pcps_print changed to pcps_errmsg to flush
!                              messages
! 51. 2004-08-09  Goodger      Increase array for name of accounting files.
!                              Print error message if unable to open 
!                              job_folder_template.
! 50. 2004-04-27  Goodger      Remove hard coded path /usr/app/cps_log and 
!                              get the log path from the config file.
!                              Get folder_template path from config file.
! 49. 2004-03-15  Goodger      Update banner information on report file.
! 48. 2003-11-18  Stoeckley    Change to not abort the program if the optional
!                                argument ERROR is present in the call to
!                                cps_start_processing.
! 47. 2003-10-03  Stoeckley    Added optional arguments to cps_start_processing;
!                                add optional argument to cps_start_setups;
!                                add initialization of save variables to
!                                cps_start_processing to allow processing to
!                                start more than once; add ability to read
!                                workfile as an alternative to reading the
!                                'process_parameters' file; replace reverse
!                                INDEX intrinsic functions with STRING_INDEX
!                                to make the Portland Group compiler happy.
! 46. 2003-02-27  Vunderink    Reversed all uses of adjustl and trim in
!                                combination to work around new Portland Group
!                                compiler bug.
! 45. 2002-08-22  Vunderink    Fix problem of cps_end_memory_thread being called
!                                by cps and pcpsx finish_processing.
! 44. 2002-08-16  Vunderink    Set pre/post ipn in pre/post_setup.
! 43. 2002-08-10  Vunderink    Added process to signal record in accounting
!                                file, added messages to cps_syslog, and added
!                                dbug_system messages.
! 42. 2002-08-05  Vunderink    Initialize process history in cps_re_setup so
!                                processes can write history cards in setup.
! 41. 2002-07-26  Vunderink    Monitor memory usage using a separate pthread and
!                                added dbug calls in cps_print_current_status.
! 40. 2002-07-19  Vunderink    Increased size of mess character string in
!                                cps_print_current_status.
! 39. 2002-07-10  Schmauch     Make sure we don't index character string
!                                process_card out of range in subroutine
!                                cps_folder_process.  deallocate cards in
!                                subroutine cps_folder.
! 38. 2002-06-19  Vunderink    Modified to record all job nodes and signal in
!                                accounting file
! 37. 2002-06-13  Vunderink    Increased size of host variable in 
!                                write_accounting.
! 36. 2002-06-06  Vunderink    Removed cps_get_pre_post_values and added
!                                cps_print_current_status which prints the
!                                pre, post and memory values (moved from cpssig)
! 35. 2002-04-22  Vunderink    Improved signal handling and added NUM_CPUS to
!                                accounting at start of job
! 34. 2002-01-02  Vunderink    Added signal handler, added memory approximation,
!                                use getsys instead of project_data for userid,
!                                and fix Intel compiler warnings.
! 33. 2001-12-21  Goodger      Insure history is initialized on boss cpu only.
! 32. 2001-11-02  C C Burch    Added printing process elapse time 
! 31. 2001-10-18  Vunderink    Fixed bug reported by intel compiler.
! 30. 2001-08-10  Vunderink    Modified to get job_data parameter TSTAMP_INC
!                                and use to control calling new cps_time_stamp
!                                routine in cps_post_process.
! 29. 2001-08-06  Goodger      Call manhist_initialize from cps_finish_setups.
!                              This solves the problem of starting out with
!                              no histories.
! 28. 2001-08-03  Goodger      Replace history with manhist. Revise history
!                                options.
! 27. 2001-06-14  Vunderink    Made changes in cps_folder for new Sun compiler
!                                added time spent in each process to accounting
!                                records, added abort message to accounting
!                                records and improved abort message sent to
!                                pcps_abort
! 26. 2001-05-29  Vunderink    Added cio_finalize to cps_finish_processing and
!                                added routine cps_print
! 25. 2001-05-02  Vunderink    Fixed cps_write_accounting to use separate files
!                                for beta and production.
! 24. 2001-04-19  Vunderink    Improved the writing accounting of records by
!                                cps_write_accounting and added number of cpus
!                                used to accounting.
! 23. 2001-04-09  Vunderink    Added OS versions and cpu speeds to parallel
!                                stats
! 22. 2001-04-05  Vunderink    Added call to cps_setlinebuf, added OS version
!                                and cpu speed to reportfile printout, and added
!                                cps_custom_rcs_ident.
! 21. 2001-03-07  Vunderink    Fixed header_word_doc end tag
! 20. 2001-02-28  Vunderink    Do not build history if HISTORY_OPT is NO.
! 19. 2001-02-19  Vunderink    Do not close process_parameter file.
! 18. 2001-01-29  Vunderink    Modified cps_write_accounting to get PBS job id.
! 17. 2001-01-02  Vunderink    Changed machine from value on job_data card to
!                              getsys_hostname and fixed write to accounting
!                              file.
! 16. 2000-12-08  Vunderink    Modified for use with parallel processing
! 15. 2000-09-29  Vunderink    Changed to use history module and get history
!                                option from job_data. 
! 14. 2000-09-27  Vunderink    Write account to accounting file, ignore scalar 
!                                parameters with a value of NONE when writing 
!                                folder entry, and if scalar parameter too long
!                                for folder entry, clip beginning.
! 13. 2000-08-24  Vunderink    Clear string before getting parameters from
!                                cardsets or parameter cache
! 12. 2000-08-23  Vunderink    Removed cps_old_xxx routines, added machine to
!                                report file header, and added cps_folder
!                                support.
! 11. 2000-04-25  Vunderink    Fixed bug in writing current history records
! 10. 2000-04-18  Vunderink    Made major enhancement to reportfile that 
!                                required adding routines cps_start_processing, 
!                                cps_print_rcs_ident, cps_write_accounting,
!                                cps_pre_process, cps_post_process and 
!                                cps_elapse_time, modifying cps_start_setups,
!                                cps_finish_setups, cps_finish_processing, 
!                                cps_pre_setup and cps_post_setup, and moving 
!                                previous versions to cps_old_xxx to be deleted
!                                later.
!  9. 2000-03-21  Vunderink    Added jobname to hist_print call and modified
!                                cps_start_setups to call new routine
!                                pc_continue_backend_update if not first loop.
!  8. 2000-03-16  Vunderink    In cps_finish_setups, moved pc_backend_execute
!                                call into conditional IF block so that it is
!                                only called if setups are successful, in
!                                cps_post_setup and cps_finish_processing, added
!                                calls to hist_module, and made some cosmetic
!                                changes to the report file output.
!  7. 2000-02-01  Vunderink    Changed cps_finish_setups to use new parameter
!                                 cache routine pc_backend_execute to clear
!                                 invalid information
!  6. 2000-01-07  Vunderink    Added call to trscan_setup in cps_post_setup.
!                                 Changed hd argument intent from in to inout
!                                 in cps_trscan.  Modified cps_finish_setups
!                                 to clear invalid parameter cache information
!                                 before trace processing commences. 
!  5. 2000-01-04  Vunderink    Added association check to deallocate in
!                                 cps_post_setup 
!  4. 1999-12-21  Vunderink    Added call to trscan and job accounting
!  3. 1999-09-15  Vunderink    Added save attribute to plun in cps_pre_setup 
!  2. 1999-06-28  Vunderink    Modified to read parameter cards
!  1. 1999-06-22  Stoeckley    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! The Portland Group compiler required modifications in the code as follows:
!
!         Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
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


      module cps_module

      use alphasort_module
      use cardset_module
      use cio_module
      use cnfg_module
      use dbug_module
      use getlun_module
      use getsys_module
      use grid_module
      use hist_module
      use manhist_module
      use named_constants_module
      use pc_module
      use pcps_module
      !use putsys_module
      use string_module
      use timer_module
      use trscan_module

      implicit none

      private
      public :: cps_abort
      public :: cps_custom_rcs_ident
      public :: cps_error_exit
      public :: cps_finish_processing
      public :: cps_finish_setups
      public :: cps_get_lun
      public :: cps_post_process
      public :: cps_post_setup
      public :: cps_pre_process
      public :: cps_pre_setup
      public :: cps_print
      public :: cps_print_current_status
      public :: cps_print_rcs_ident
      public :: cps_setup_top_of_loop
      public :: cps_start_processing
      public :: cps_start_setups
      public :: cps_top_of_loop
      public :: cps_write_accounting_signal
      public :: cps_get_project_name
      public :: cps_set_project_name

      character(len=100),public,save :: CPS_IDENT = &
       '$Id: cps.f90,v 1.61 2007/02/16 14:00:19 Menger beta sps $'


!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!


      integer                     ,private,save   :: lun                = 6
      integer                     ,private,save   :: plun               = 0
      integer                     ,private,save   :: loop_count         = 0
      integer                     ,private,save   :: nprocess_list      = 0
      integer                     ,private,save   :: ndpt               = 0
      integer                     ,private,save   :: tstamp_inc         = 0
      integer                     ,private,save   :: max_rss            = 0
      integer                     ,private,save   :: max_vsize          = 0
      integer                     ,private,save   :: num_times_finished = 0
      integer                     ,private,save   :: pre_ipn            = 0
      integer                     ,private,save   :: post_ipn           = 0
      double precision            ,private,save   :: post_hdr           = 0.0
      real                        ,private,save   :: dt                 = 0.0
      real                        ,private,save   :: tstrt              = 0.0
      character(len=20)           ,private,save   :: jobname
      character(len=20)           ,private,save   :: user_name
      character(len=20)           ,private,save   :: machine
      character(len=10)           ,private,save   :: date_started
      character(len=10)           ,private,save   :: time_started
      character(len=10)           ,private,save   :: date_processing_started
      character(len=10)           ,private,save   :: time_processing_started
      character(len=20)   ,pointer,private,save   :: process_list(:)
      type(cardset_struct),pointer,private,save   :: pdata_cardset
      type(cardset_struct),pointer,private,save   :: jdata_cardset
      logical                     ,private,save   :: abort_on_error

      character(len=78)        ,private,parameter :: DASHLINE =  &
'------------------------------------------------------------------------------'
      character(len=78)        ,private,parameter :: EQUALINE =  &
'=============================================================================='
      integer,parameter,private        :: PNAME_LEN     = 20
      integer,parameter,private        :: PARAM_LEN     = 20
      integer,parameter,private        :: MAX_PROC      = 100
      integer,parameter,private        :: MAX_PROC_DATA = 1000

      integer,parameter,private        :: SIGQUIT       = 3
      integer,parameter,private        :: SIGABRT       = 6
      integer,parameter,private        :: SIGBUS        = 7
      integer,parameter,private        :: SIGFPE        = 8
      integer,parameter,private        :: SIGUSR1       = 10
      integer,parameter,private        :: SIGSEGV       = 11
      integer,parameter,private        :: SIGUSR2       = 12
      integer,parameter,private        :: SIGPIPE       = 13
      integer,parameter,private        :: SIGTERM       = 15
      integer,parameter,private        :: SIGXCPU       = 24
      integer,parameter,private        :: SIGXFSZ       = 25

      character(len=8),parameter,private :: CPS_SIGNALS(25) = (/           &
       'Unused  ','Unused  ','SIGQUIT ','Unused  ','Unknown ','SIGABRT ',  &
       'SIGBUS  ','SIGFPE  ','Unused  ','SIGUSR1 ','SIGSEGV ','SIGUSR2 ',  &
       'SIGPIPE ','Unused  ','SIGTERM ','Unused  ','Unused  ','Unused  ',  &
       'Unused  ','Unused  ','Unused  ','Unused  ','Unused  ','SIGXCPU ',  &
       'SIGXFSZ '/)


!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!

!!---------------------------- interfaces --------------------------------!!
!!---------------------------- interfaces --------------------------------!!
!!---------------------------- interfaces --------------------------------!!

      interface
        subroutine cps_get_project_name_c(project_name)
         character,intent(out)  :: project_name
        end subroutine cps_get_project_name_c
      end interface

      interface
        subroutine cps_set_project_name_c(project_name)
         character,intent(in)   :: project_name
        end subroutine cps_set_project_name_c
      end interface

!!----------------end of       interfaces --------------------------------!!
!!----------------end of       interfaces --------------------------------!!
!!----------------end of       interfaces --------------------------------!!



      contains


!!------------------------- start processing ------------------------------!!
!!------------------------- start processing ------------------------------!!
!!------------------------- start processing ------------------------------!!


      subroutine cps_start_processing &
                          (num_processes,process_names,workfile,error)
      implicit none

      integer         ,optional,intent(out) :: num_processes       ! argument
      character(len=*),optional,pointer     :: process_names(:)    ! argument
      character(len=*),optional,intent(in)  :: workfile            ! argument
      logical         ,optional,intent(out) :: error               ! argument

      integer                          :: i,istat                   ! local

      integer                          :: ncards                    ! local
      integer                          :: num_nodes                 ! local
      integer                          :: nhostnames                ! local
      integer                          :: pid                       ! local
      character(len=CARDSET_LENGTH)    :: errmsg                    ! local
      character(len=PC_LENGTH)         :: account                   ! local

      character(len=PC_LENGTH)         :: card,cd                   ! local
      character(len=PC_LENGTH)         :: card_tmp                  ! local
      character(len=10)                :: cncpus                    ! local
      character(len=10)                :: cnum_nodes                ! local
      character(len=20),allocatable    :: hostnames(:)              ! local
      type(cardset_struct),pointer     :: cardset                   ! local
      character(len=10)                :: os_ver                    ! local
      character(len=10)                :: cpid                      ! local
      character(len=FILENAME_LENGTH)   :: filename                  ! local
      logical                        :: reading_process_parameters  ! local

      if (present(error)) then
           error = .false.
      end if

      nullify (cardset) ! jpa

      lun                = 6
      plun               = 0
      loop_count         = 0
      nprocess_list      = 0
      ndpt               = 0
      tstamp_inc         = 0
      max_rss            = 0
      max_vsize          = 0
      num_times_finished = 0
      pre_ipn            = 0
      post_ipn           = 0
      post_hdr           = 0.0
      dt                 = 0.0
      tstrt              = 0.0
      abort_on_error     = .not.present(error)

      call string_date (date_started)
      call string_time (time_started)
      time_started(3:3) = ':'
      time_started(6:6) = ':'

      call cps_setlinebuf()
      call cps_sig_install_handler()
      call cps_start_memory_thread()
      call cpslog_init()
      call cpsacct_init()
      !--- this open will cause pfio_init() to be called
      pid = getsys_pid()
      call string_ii2cc (pid,cpid)
      call getsys_hostname (machine)
      filename='.tmp_'//trim(machine)//'_'//trim(cpid)
      plun = cio_fopen(filename,"w",SCRATCH=.true.)
      if(plun > 0 ) plun = cio_fclose(plun)
      !--- ok, we're done!

      nullify(process_list)
      nullify(pdata_cardset)
      nullify(jdata_cardset)

      if (present(workfile)) then
        filename = workfile
        call cps_print (' ')
        call cps_print (EQUALINE)
        call cps_print ('Starting processing with file '//trim(filename))
        call cps_print (EQUALINE)
        call cps_print (' ')
      else
        filename = 'process_parameters'
      end if

      plun=cio_fopen(filename,'r')
      if(plun == CIO_ERROR) then
           call cps_print ('Error trying to open file '//trim(filename))
           if (present(error)) then
                error = .true.
                return
           else
                stop "error opening process parameters file"
           end if
      end if

      do i=1,2
        reading_process_parameters = .false.
        ncards = 0
        do
          istat=cio_fgetline(card,PC_LENGTH,plun)
          if (istat <  0 ) then
               call cps_print ('error trying to read file '//trim(filename))
               if (present(error)) then
                    error = .true.
                    return
               else
                    stop "error reading process parameters file"
               end if
          end if

          card_tmp = adjustl(card)
          call string_to_upper(card_tmp)

          if (card_tmp .eq. ' ') then
            cycle
          else if (card_tmp(1:10) .eq. '</PROCESS>') then
            exit
          else if (card_tmp(1:13) .eq. '</PARAMETERS>') then
            reading_process_parameters = .false.
          else if (card_tmp(1:13).eq.'<PROCESS NAME' .and. ncards.eq.0) then
            reading_process_parameters = .true.
            if (i == 1) then
              call cardset_create (pdata_cardset)
            else
              call cardset_create (jdata_cardset)
            endif
          else if (card_tmp(1:9) .eq. '<GLOBALS>') then
            reading_process_parameters = .false.
          else if (card_tmp(1:10) .eq. '<CONTROLS>') then
            reading_process_parameters = .false.
          else if (card_tmp(1:12) .eq. '<PARAMETERS>') then
            reading_process_parameters = .true.
          else if (reading_process_parameters) then
            ncards = ncards + 1
            if (i == 1) then
              call cardset_add_card (pdata_cardset,card)
            else
              call cardset_add_card (jdata_cardset,card)
            endif
          else
            cycle
          endif
        enddo
      enddo

      jobname       = ' '
      user_name     = ' '
      machine       = ' '
      account       = ' '
      nprocess_list = 0
      call cardset_get_scalar  (jdata_cardset,'jobname'     ,jobname   ,errmsg)
      call cardset_get_scalar  (jdata_cardset,'tstamp_inc'  ,tstamp_inc,errmsg)
      call getsys_username     (user_name)
      call getsys_hostname     (machine)
      call getsys_os_version   (os_ver)
      call cardset_get_scalar  (pdata_cardset,'account'     ,account  ,errmsg)
      call cardset_get_scalar  (jdata_cardset,'num_nodes'   ,num_nodes,errmsg)
      call cardset_alloc_array (jdata_cardset,'process_list',process_list,  &
                                nprocess_list,errmsg)

      if (present(num_processes)) num_processes = nprocess_list
      if (present(process_names)) then
           if (associated(process_names)) deallocate (process_names)
           allocate (process_names(nprocess_list))
           process_names(:) = process_list(1:nprocess_list)
      end if

      call cps_write_accounting ('Job started')
      if (len_trim(account) .eq. 0) account = 'NONE'
      call string_ii2cc (pcps_num_procs,cncpus)
      call string_ii2cc (num_nodes     ,cnum_nodes)
      call cps_write_accounting ('ACCOUNT = '//trim(account)//  &
                               ', NUM_CPUS = '//trim(cncpus)//  &
                               ', NUM_NODES = '//trim(cnum_nodes))

      if (pcps_num_procs .gt. 1) then
        allocate(hostnames(pcps_num_procs))
        do i=0,pcps_num_procs-1
          write(hostnames(i+1),'(20A1)') pcps_hostnames(1:20,i)
          hostnames(i+1) = adjustl(hostnames(i+1))
        enddo
        call alphasort_sort(hostnames,pcps_num_procs)
        nhostnames = 1
        do i=2,pcps_num_procs                            !Get rid of duplicates
          if (trim(hostnames(i)) .ne. trim(hostnames(nhostnames))) then
            nhostnames = nhostnames + 1
            if (i .ne. nhostnames) hostnames(nhostnames) = hostnames(i)
          endif
        enddo
        call cardset_create (cardset)
        call cardset_put_array (cardset,'HOSTNAMES',hostnames,nhostnames)
        ncards = cardset_num_cards (cardset)
        do i=1,ncards
          call cardset_get_card     (cardset,i,card,errmsg)
          call cps_write_accounting (card)
        enddo
        call cardset_delete (cardset)
        deallocate(hostnames)
      else
        call cps_write_accounting ('HOSTNAMES = ('//trim(machine)//')')
      endif

      call cardset_create (cardset)
      call cardset_put_array (cardset,'PROCESS_LIST',process_list,nprocess_list)
      ncards = cardset_num_cards (cardset)
      do i=1,ncards
        call cardset_get_card     (cardset,i,card,errmsg)
        call cps_write_accounting (card)
      enddo
      call cardset_delete (cardset)
      !project_name="projectB"
      !call cps_set_project_name(project_name)
      !project_name="000000000000000"
      !call cps_get_project_name(project_name)
      !print*,"project name is [",trim(project_name),"]"

      call pcps_print(' ')
      call pcps_print(' ')
      call pcps_print( '            '// &
   '*************************************************************************')
      call pcps_print( '            '// &
   '*************************************************************************')
      call pcps_print( '            '// &
   '** +-----------------------------------------------------------------+ **')
      call pcps_print( '            '// &
   '** | C P S E I S   S E I S M I C   P R O C E S S I N G   S Y S T E M | **')
      call pcps_print( '            '// &
   '** |                                                                 | **')
      call pcps_print( '            '// &
   '** |             Open Source Seismic Processing System               | **')
      call pcps_print( '            '// &
   '** |           Copyright (c) 2007 ConocoPhillips Company             | **')
      call pcps_print( '            '// &
   '** | Permission is hereby granted, free of charge, to any person     | **')
      call pcps_print( '            '// &
   '** | obtaining a copy of this software and associated documentation  | **')
      call pcps_print( '            '// &
   '** | files (the "Software"), to deal in the Software without         | **')
      call pcps_print( '            '// &
   '** | restriction, including without limitation the rights to use,    | **')
      call pcps_print( '            '// &
   '** | copy, modify, merge, publish, distribute, sublicense, and/or    | **')
      call pcps_print( '            '// &
   '** | sell copies of the Software, and to permit persons to whom the  | **')
      call pcps_print( '            '// &
   '** | Software is furnished to do so, subject to the following        | **')
      call pcps_print( '            '// &
   '** | conditions:                                                     | **')
      call pcps_print( '            '// &
   '** |                                                                 | **')
      call pcps_print( '            '// &
   '** | The above copyright notice and this permission notice shall be  | **')
      call pcps_print( '            '// &
   '** | included in all copies or substantial portions of the Software. | **')
      call pcps_print( '            '// &
   '** |                                                                 | **')
      call pcps_print( '            '// &
   '** | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, | **')
      call pcps_print( '            '// &
   '** | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES | **')
      call pcps_print( '            '// &
   '** | OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND        | **')
      call pcps_print( '            '// &
   '** | NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     | **')
      call pcps_print( '            '// &
   '** | HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,    | **')
      call pcps_print( '            '// &
   '** | WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    | **')
      call pcps_print( '            '// &
   '** | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR   | **')
      call pcps_print( '            '// &
   '** | OTHER DEALINGS IN THE SOFTWARE.                                 | **')
      call pcps_print( '            '// &
   '** |                                                                 | **')
      call pcps_print( '            '// &
   '** |                                                                 | **')
      call pcps_print( '            '// &
   '** |                                                                 | **')
      call pcps_print( '            '// &
   '** |                  Licensed by CONOCOPHILLIPS 2007                | **')
      call pcps_print( '            '// &
   '** +-----------------------------------------------------------------+ **')
      call pcps_print( '            '// &
   '**                                                                     **')
      call pcps_print( '            '// &
   '**                                                                     **')

      card_tmp = '** Jobname     = '//trim(jobname)
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card_tmp = '** Userid      = '//trim(user_name)
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '// trim(cd))

      card_tmp = '** Machine     = '//trim(machine)//'  (OS version '//  &
                                                         trim(os_ver)//')'
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '// trim(cd))

      card_tmp = '** Job date    = '//date_started//'  '//time_started
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

cd='**                                                                     **'
      cd = ' '
      call cardset_get_scalar  (pdata_cardset,'project',cd,errmsg)
      card_tmp = '** Project     = '//trim(cd(1:PC_LENGTH-17))
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

cd='**                                                                     **'
      card = ' '
      call cardset_get_scalar  (jdata_cardset,'sub_project',card,errmsg)
      card_tmp = '** Sub-Project = '//trim(card(1:PC_LENGTH-17))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (pdata_cardset,'account',card,errmsg)
      card_tmp = '** Accounting  = '//trim(card(1:PC_LENGTH-17))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      call pcps_print( '            '// &
   '**                                                                     **')

      card = ' '
      call cardset_get_scalar  (jdata_cardset,'trace_length',card,errmsg)
      card_tmp = '** Trace length (seconds)     = '//trim(card(1:PC_LENGTH-32))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '// trim(cd))

      card = ' '
      call cardset_get_scalar  (jdata_cardset,'dt',card,errmsg)
      card_tmp = '** Sample Rate  (seconds)     = '//trim(card(1:PC_LENGTH-32))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (jdata_cardset,'ndpt',card,errmsg)
      card_tmp = '** Data points per trace      = '//trim(card(1:PC_LENGTH-32))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (jdata_cardset,'nwih',card,errmsg)
      card_tmp = '** Words in header            = '//trim(card(1:PC_LENGTH-32))
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (jdata_cardset,'tstrt',card,errmsg)
      card_tmp = '** Trace start time (seconds) = '//trim(card(1:PC_LENGTH-32))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      call pcps_print( '            '// &
   '**                                                                     **')

      card = ' '
      call cardset_get_scalar  (pdata_cardset,'origin_east',card,errmsg)
      card_tmp = '** Easting origin  = '//trim(card(1:PC_LENGTH-21))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (pdata_cardset,'origin_north',card,errmsg)
      card_tmp = '** Northing origin = '//trim(card(1:PC_LENGTH-21))
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (pdata_cardset,'x_grid_dist',card,errmsg)
      card_tmp = '** X Grid Distance = '//trim(card(1:PC_LENGTH-21))
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (pdata_cardset,'y_grid_dist',card,errmsg)
      card_tmp = '** Y Grid Distance = '//trim(card(1:PC_LENGTH-21))
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (pdata_cardset,'angle',card,errmsg)
      card_tmp = '** Angle           = '//trim(card(1:PC_LENGTH-21))
cd='**                                                                     **'
      card     = '**                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      card = ' '
      call cardset_get_scalar  (pdata_cardset,'handedness',card,errmsg)
      card_tmp = '** Handedness      = '//trim(card(1:PC_LENGTH-21))
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '//trim(cd))

      call pcps_print( '            '// &
   '**                                                                     **')

      call pcps_print( '            '// &
   '** ------------------> PROCESS LIST FOR THIS JOB <-------------------- **')
      card_tmp = '**'
      do i=1,nprocess_list-1
        if (len_trim(card_tmp)+2+len_trim(process_list(i)) > 55) then
cd='**                                                                     **'
          cd(1:len_trim(card_tmp)) = card_tmp
          call pcps_print( '            '//trim(cd))
          card_tmp = '**'
        endif
        card_tmp=trim(card_tmp(1:PC_LENGTH-22))//' '//trim(process_list(i))//','
      enddo

      if(len_trim(card_tmp)+2+len_trim(process_list(nprocess_list)) > 55) then
cd='**                                                                     **'
        cd(1:len_trim(card_tmp)) = card_tmp
        call pcps_print( '            '// trim(cd))
        card_tmp = '**'
      endif

      card_tmp=trim(card_tmp(1:PC_LENGTH-21))//' '//process_list(nprocess_list)
cd='**                                                                     **'
      cd(1:len_trim(card_tmp)) = card_tmp
      call pcps_print( '            '// trim(cd))

      call pcps_print( '            '// &
   '**                                                                     **')
      call pcps_print( '            '// &
   '*************************************************************************')
      call pcps_print( '            '// &
   '*************************************************************************')
      call pcps_print(' ')
      call pcps_print(' ')
      call pcps_print(EQUALINE)
      call pcps_print( 'Process             Revision  Date           Time' // &
                  '           Maturity')
      call pcps_print(EQUALINE)

      return

      end subroutine cps_start_processing


!!------------------------------- print -----------------------------------!!
!!------------------------------- print -----------------------------------!!
!!------------------------------- print -----------------------------------!!


      subroutine cps_print (message)
      implicit none

      character(len=*),intent(in)      :: message                   ! argument

      call pcps_print(message,2)

      return
      end subroutine cps_print


!!-------------------------- print_rcs_ident ------------------------------!!
!!-------------------------- print_rcs_ident ------------------------------!!
!!-------------------------- print_rcs_ident ------------------------------!!


      subroutine cps_print_rcs_ident (id)
      implicit none

      character(len=*),intent(in)      :: id                        ! argument

      integer                          :: i                         ! local
      character(len=100)               :: id_tmp                    ! local
      character(len=80)                :: card                      ! local

      lun  = 6

      id_tmp = id
      i = index(id_tmp(1:),' ')                 ! Id

      id_tmp = id_tmp(i+1:)
      i = index(id_tmp(1:),' ')                 ! process
      card(1:20) = id_tmp(1:i-1)

      id_tmp = id_tmp(i+1:)
      i = index(id_tmp,' ')                     ! revision
      card(21:30) = id_tmp(1:i-1)

      id_tmp = id_tmp(i+1:)
      i = index(id_tmp,' ')                     ! date
      card(31:45) = id_tmp(1:i-1)

      id_tmp = id_tmp(i+1:)
      i = index(id_tmp,' ')                     ! time
      card(46:60) = id_tmp(1:i-1)

      id_tmp = id_tmp(i+1:)
      i = index(id_tmp,' ')                     ! user

      id_tmp = id_tmp(i+1:)
      i = index(id_tmp,' ')                     ! prod or beta
      card(61:70) = id_tmp(1:i-1)

      call pcps_print(card(1:70))

      return
      end subroutine cps_print_rcs_ident


!!-------------------------- custom_rcs_ident -----------------------------!!
!!-------------------------- custom_rcs_ident -----------------------------!!
!!-------------------------- custom_rcs_ident -----------------------------!!


      subroutine cps_custom_rcs_ident
      implicit none

      integer                          :: clun                      ! local
      integer                          :: istat                     ! local
      character(len=80)                :: card                      ! local

      call getlun (clun)
      clun=cio_fopen('custom_rcs_ident','r')
      if(clun == CIO_ERROR ) return

      call pcps_print(' ')
      call pcps_print(' ')
      call pcps_print(EQUALINE)
      call pcps_print( 'Custom Code         Revision  Date           Time' // &
                  '           Maturity')
      call pcps_print(EQUALINE)

      do
        istat=cio_fgetline(card,PC_LENGTH,clun)
        if (istat .lt. 0) exit
        card = adjustl(card)
        call cps_print_rcs_ident (card)
      enddo

      istat = cio_fclose (clun)

      return
      end subroutine cps_custom_rcs_ident


!!-------------------------- write_accounting -----------------------------!!
!!-------------------------- write_accounting -----------------------------!!
!!-------------------------- write_accounting -----------------------------!!


      subroutine cps_write_accounting (string,flag)
      implicit none

      character(len=*),intent(in)          :: string                ! argument
      integer         ,intent(in),optional :: flag                  ! argument

      character(len=220+CARDSET_DATACARD_LENGTH) :: card            ! local
      character(len=160)                         :: log_dir
      character(len=10)                          :: date            ! local
      character(len=10)                          :: time            ! local
      character(len=12)                          :: host            ! local
      character(len=10)                          :: request_id      ! local
      character(len=22)                          :: job_id          ! local
      character(len=10)                          :: cpid            ! local
      character(len= 8)                          :: lib             ! local
      integer                                    :: flag_used       ! local
      integer                                    :: pid             ! local
      integer                                    :: istat           ! local
      integer,allocatable                        :: hcard(:)        ! local
      integer                                    :: hcard_len       ! local
      integer                                    :: temp

! ------ flag is an optional argument to control printing,
! ------   not present or 0, print by boss only
! ------   1 print if worker
! ------   2 print in all cases

      if(present(flag)) then
        flag_used=flag
      else
        flag_used=0
      endif

      if (flag_used .gt. 2) return
      if (flag_used .eq. 1 .and. pcps_current_worker_num.eq.0) return
      if (flag_used .eq. 0 .and. pcps_current_worker_num.ne.0) return

      call string_date (date)
      call string_time (time)
      time_started(3:3) = ':'
      time_started(6:6) = ':'

      call cnfg_get_value('cps_log_dir',log_dir)

      job_id = 'NONE'
      call getsys_env ('PBS_JOBID',job_id)
      if (trim(job_id) .eq. 'NONE') then
        call getsys_hostname (host)
        request_id = 'NONE'
        call getsys_env ('PBS_REQID',request_id)
        job_id = trim(request_id)//'.'//trim(host)
      endif

      pid = getsys_pid()
      call string_ii2cc (pid,cpid)

      if (getsys_library() .eq. GETSYS_PRODLIB) then
        lib='[prod] -'
      else
        lib='[test] -'
      endif

      card = trim(lib)//' '//trim(cpid)//' '//trim(job_id)//' '//  &
             trim(jobname)//' '//trim(user_name)//' '//trim(date)//' '//  &
             trim(time)//' '//trim(string)

      temp = len_trim(card)
      hcard_len = string_num_integers(temp)
      allocate(hcard(hcard_len),stat=istat)
      if (istat .ne. 0) return
      call string_cc2hh (card,hcard)

      call cpsacct_message(hcard)                    !Write to cps_acct_log

      deallocate(hcard,stat=istat)

      end subroutine cps_write_accounting


!!--------------------------- start_setups --------------------------------!!
!!--------------------------- start_setups --------------------------------!!
!!--------------------------- start_setups --------------------------------!!


!-- this subroutine must STOP (and not return) if there is an error.

               ! It looks like this routine will never stop
               ! unless there is a failure to open an online
               ! file by a worker cpu.  This has been changed
               ! to detect such an error and to explicitly stop
               ! if the new optional argument is missing, and return
               ! an error flag if the optional argument is present.

      subroutine cps_start_setups (error)
      implicit none
      logical,optional,intent(out) :: error
      character(len=80)            :: card_tmp
      integer                      :: istat

      loop_count = loop_count + 1

      if (loop_count == 1) then
        if(pcps_current_worker_num.gt.0) then
          call getlun(lun)
          write(card_tmp,'(A,I4.4,A)') &
            "online_",pcps_current_worker_num,".tmp"
          open(unit=lun,file=trim(card_tmp),status="unknown", &
               action="readwrite",iostat=istat)
          if (istat /= 0) then
               if (present(error)) then
                    error = .true.
               else
                    stop 'Error opening pcps_current_worker_log'
               endif
          endif
        endif
        call pc_backend_update (lun)
      else
        call pc_continue_backend_update()

        call pcps_print(' ')
        call pcps_print(' ')
        call pcps_print(' ')
        call pcps_print(EQUALINE)
        call pcps_print(EQUALINE)
        write(card_tmp,*) 'Starting Loop ',loop_count
        call pcps_print(trim(adjustl(card_tmp)))
        call pcps_print(EQUALINE)
        call pcps_print(EQUALINE)
        call pcps_print(' ')
        call pcps_print(' ')
      endif
      if (present(error)) error = .false.
      call timer_start (1)

      end subroutine cps_start_setups


!!--------------------------- finish_setups --------------------------------!!
!!--------------------------- finish_setups --------------------------------!!
!!--------------------------- finish_setups --------------------------------!!


      subroutine cps_finish_setups (error)
      implicit none
      logical,intent(out)      :: error                     ! argument

      integer :: istat

!     close(unit=plun,status="keep")
      call timer_stop (1)
      error = pc_previous_error()

      call pc_get_global ('NDPT' ,ndpt)
      call pc_get_global ('DT'   ,dt  )
      call pc_get_global ('TSTRT',tstrt)

      if (pcps_current_worker_num .eq. 0) then
        call manhist_initialize(istat)
        if (istat .ne. 0) then
          call pcps_print('CPS_finish_setups: Unable to initialize history')
        endif
      endif

      call pcps_print(' ')
      call pcps_print(EQUALINE)
      if (error) then
        call pcps_print('++++++ Setup errors occurred ++++++')
        call pcps_print('++++++ Setup errors occurred ++++++')
        call pcps_print('++++++ Setup errors occurred ++++++')
        call pcps_print('Trace processing will not commence.')
        call dbug_set_system_message ('cps_finish_setups - Setup errors')
      else
        call pcps_print('Setups successfully completed.')
        call pcps_print('Now starting to process traces.')
        call pc_backend_execute
        call dbug_set_system_message ('cps_finish_setups - Setup completed')
      end if
      call pcps_print(EQUALINE)
      call pcps_print(' ')
      call pcps_print(' ')

      call string_date (date_processing_started)
      call string_time (time_processing_started)

      return
      end subroutine cps_finish_setups


!!---------------------------- finish_processing --------------------------!!
!!---------------------------- finish_processing --------------------------!!
!!---------------------------- finish_processing --------------------------!!


      subroutine cps_finish_processing (error,cpu_rstats,hostnames,cpu_istats, &
                                        os_versions,signal)
      implicit none
      logical   ,optional,intent(in) :: error                 ! argument
      real      ,optional,intent(in) :: cpu_rstats(:,:,0:)    ! argument
      character ,optional,intent(in) :: hostnames(:,0:)       ! argument
      integer   ,optional,intent(in) :: cpu_istats(:,0:)      ! argument
      character ,optional,intent(in) :: os_versions(:,0:)     ! argument
      integer   ,optional,intent(in) :: signal                ! argument

      logical                       :: prev                          ! local

      integer                       :: i                             ! local
      integer                       :: istat                         ! local
      integer                       :: maxrss                        ! local
      integer                       :: nswap                         ! local
      integer                       :: minflt                        ! local
      integer                       :: majflt                        ! local
      integer                       :: inblock                       ! local
      integer                       :: outblock                      ! local
      integer                       :: cnt_time                      ! local
      integer                       :: cpu_speed                     ! local
      integer                       :: ncards                        ! local
      integer                       :: nprocess_utime                ! local
      integer                       :: nprocess_stime                ! local
      integer                       :: nvsize                        ! local
      integer                       :: nrss                          ! local
      integer,allocatable           :: vsize(:)                      ! local
      integer,allocatable           :: rss(:)                        ! local
      
      real                          :: utime                         ! local
      real                          :: stime                         ! local
      real                          :: min_utime                     ! local
      real                          :: max_utime                     ! local
      real                          :: avg_utime                     ! local
      real                          :: sum_utime                     ! local
      real                          :: min_stime                     ! local
      real                          :: max_stime                     ! local
      real                          :: avg_stime                     ! local
      real                          :: sum_stime                     ! local
      real                          :: var_time                      ! local
      real                          :: sd_time                       ! local
      real                          :: elapse_time                   ! local
      real                          :: max_etime                     ! local
      
      real,allocatable              :: process_utime(:)              ! local
      real,allocatable              :: process_stime(:)              ! local
      
      character(len=10)             :: date_finished                 ! local
      character(len=10)             :: time_finished                 ! local
      character(len=20)             :: cutime                        ! local
      character(len=20)             :: cstime                        ! local
      character(len=20)             :: cetime                        ! local
      character(len=10)             :: cncpus                        ! local


      character(len=15)             :: cvsize                        ! local
      character(len=15)             :: crss                          ! local
      character(len=40)             :: fmt                           ! local
      character(len=132)            :: card_tmp                      ! local
      character(len=PC_LENGTH)      :: history_opt                   ! local
      character(len=PC_LENGTH)      :: rec_keeping                   ! local
      character(len=CARDSET_LENGTH) :: card                          ! local
      character(len=CARDSET_LENGTH) :: errmsg                        ! local
      double precision              :: etime                         ! local
      double precision              :: p_etime                       ! local
      type(cardset_struct),pointer  :: cardset                       ! local

      nullify (cardset) ! jpa

      if (.not.pc_exists()) then
             call pcps_print (' ')
             call pcps_print (EQUALINE)
             call pcps_print ('+++++ Initialization errors occurred +++++')
             call pcps_print ('+++++ Initialization errors occurred +++++')
             call pcps_print ('+++++ Initialization errors occurred +++++')
             call pcps_print ('Setup and processing never started.')
             call pcps_print (EQUALINE)
             return
      end if

      num_times_finished = num_times_finished + 1
      if (num_times_finished .gt. 1) then
        call pcps_print ('++++++++++++++++++++++++++++++++++++++++++')
        call pcps_print ('+++ cps_finish_processing called again +++')
        call pcps_print ('++++++++++++++++++++++++++++++++++++++++++')
        return
      endif

      call string_date (date_finished)
      call string_time (time_finished)
      time_finished(3:3) = ':'
      time_finished(6:6) = ':'

      if (.not. present(cpu_istats)) call cps_end_memory_thread()

      if (present(signal)) then
        call pcps_print ('+++++ Error occurred - Signal Handler Invoked +++++')
        call pcps_print ('+++++ Error occurred - Signal Handler Invoked +++++')
        call pcps_print ('+++++ Error occurred - Signal Handler Invoked +++++')
        call pcps_print ('Job prematurely terminated.')
        call pcps_print (' ')
        if (pcps_num_procs.gt.1) then
          etime = cps_elapse_time (date_started,time_started,date_finished,  &
                                   time_finished)
          call string_dd2cc (etime         ,cetime)
          call string_ii2cc (pcps_num_procs,cncpus)
          call cps_write_accounting ('USER_TIME = 0.0, SYSTEM_TIME = 0.0'// &
                                   ', ELAPSE_TIME = '//trim(cetime)// &
                                   ', NUM_CPUS = '//trim(cncpus))
          call cps_write_accounting ('Job Abort - Signal Handler')
          call cps_write_accounting ('Job finished')
          return
        endif
      else
        prev = pc_previous_error()
        if (.not.prev) then
             call pcps_print (' ')
             call pcps_print (EQUALINE)
             if (.not.present(error)) then
                  call pcps_print ('Processing successfully completed.')
             else if (error) then
                  call pcps_print ('+++++ Processing errors occurred +++++')
                  call pcps_print ('+++++ Processing errors occurred +++++')
                  call pcps_print ('+++++ Processing errors occurred +++++')
                  call pcps_print ('Processing prematurely terminated.')
             else
                  call pcps_print ('Processing successfully completed.')
             end if
             call pcps_print (EQUALINE)
        end if

!--------- print history

        call pcps_print (' ')
        call pcps_print (' ')
        history_opt = 'ALL'
        call pc_get_jdata          ('HISTORY_OPT',history_opt)
        call string_to_upper       (history_opt)
        call string_squeeze_blanks (history_opt)

        if (history_opt .ne. 'NONE') then
          call manhist_phist(history_opt)
        endif
      endif

!--------- gather and print statistics

      call pcps_print(' ')
      call pcps_print(' ')
      call pcps_print(&
       '======================================================================')
      call pcps_print(&
       '                        Individual Process Timings                    ')
      call pcps_print(&
       '======================================================================')
      call pcps_print(&
       'Process                User CPU Time    System CPU Time  Elapsed Time ')
      if(present(cpu_rstats)) then 
        call pcps_print(&
       ' Name                  (tot seconds)     (tot seconds)   (max seconds)')
      else
        call pcps_print(&
       ' Name                    (seconds)         (seconds)       (seconds)')
      endif
      call pcps_print(&
       '-------                -------------    ---------------  -------------')

      nprocess_utime = nprocess_list
      allocate(process_utime(nprocess_utime),stat=istat)
      if (istat .ne. 0) then
        nprocess_utime = 0
      else
        process_utime = 0.0
      endif

      nprocess_stime = nprocess_list
      allocate(process_stime(nprocess_stime),stat=istat)
      if (istat .ne. 0) then
        nprocess_stime = 0
      else
        process_stime = 0.0
      endif

! --- note cpu_rstats(1,i,j) is user   time on worker j for process i
!          cpu_rstats(2,i,j) is system time on worker j for process i
!          cpu_rstats(3,i,j) is elapse time on worker j for process i
!                           j=nprocess_list+1 is for do_parallel
!                           j=nprocess_list+2 is total for whole job

      fmt='(1X,A20,5X,G12.5,5X,G12.5,4x,G12.5)'
      if (present(cpu_rstats)) then
        do i=1,nprocess_list+1      ! +1 to ending index to get DO_PARALLEL
          sum_utime=sum   (cpu_rstats(1,i,0:pcps_num_workers))
          sum_stime=sum   (cpu_rstats(2,i,0:pcps_num_workers))
          max_etime=maxval(cpu_rstats(3,i,0:pcps_num_workers))
          
          if(i.eq.1) then
            write(card_tmp,fmt) &
             'SETUPS              ',sum_utime,sum_stime, max_etime
          elseif(i.eq.2) then
            write(card_tmp,fmt) &
            'TRSCAN              ',sum_utime,sum_stime, max_etime
          elseif(i.eq.pcps_ntimer) then
            write(card_tmp,fmt) &
             'DO_PARALLEL         ',sum_utime,sum_stime, max_etime
          else
            write(card_tmp,fmt) process_list(i),sum_utime,sum_stime,max_etime
          endif
          
          call pcps_print(trim(adjustl(card_tmp)))
          if (nprocess_utime .ge. i) process_utime(i) = sum_utime
          if (nprocess_stime .ge. i) process_stime(i) = sum_stime
        enddo

        if(pcps_num_workers.gt.0) then
          if(pcps_current_worker_num.eq.0) then
            call pcps_print(' ')
            call pcps_print(' ')
            call pcps_print(EQUALINE//'========================')
            call pcps_print( '                                 CPU Usage')
            call pcps_print(EQUALINE//'========================')
            call pcps_print("CPU# ------Hostname------ OS version   "//&
                            "CPU speed User seconds System seconds  "//&
                             "Vsize (kb) RSsize (kb)")

            nvsize = pcps_num_workers+1
            allocate(vsize(nvsize),stat=istat)
            if (istat .ne. 0) then
              nvsize = 0
            else
              do i=0,pcps_num_workers
                vsize(i+1) = cpu_istats(2,i)
              enddo
              call cardset_create    (cardset)
              call cardset_put_array (cardset,'VSIZE_MEMORY',vsize,nvsize)
              ncards = cardset_num_cards (cardset)
              do i=1,ncards
                call cardset_get_card     (cardset,i,card,errmsg)
                call cps_write_accounting (card)
              enddo
              call cardset_delete (cardset)
              deallocate(vsize, stat=i)
            endif

            nrss = pcps_num_workers+1
            allocate(rss(nrss),stat=istat)
            if (istat .ne. 0) then
              nrss = 0
            else
              do i=0,pcps_num_workers
                rss(i+1) = cpu_istats(3,i)
              enddo
              call cardset_create    (cardset)
              call cardset_put_array (cardset,'RSS_MEMORY',rss,nrss)
              ncards = cardset_num_cards (cardset)
              do i=1,ncards
                call cardset_get_card     (cardset,i,card,errmsg)
                call cps_write_accounting (card)
              enddo
              call cardset_delete (cardset)
              deallocate(rss, stat=i)
            endif


            do i=0,pcps_num_workers
              write(card_tmp,  &
                '(I4,1x,20A1,1x,12A1,1x,I7,3x,G12.5,3x,G12.5,1x,I10,2x,I10)') &
                                        i,hostnames(1:20,i), &
                                        os_versions(1:,i), &
                                        cpu_istats(1,i), &
                                        cpu_rstats(1,nprocess_list+2,i),  &
                                        cpu_rstats(2,nprocess_list+2,i),  &
                                        cpu_istats(2,i), &
                                        cpu_istats(3,i)
              call pcps_print(trim(card_tmp))
            enddo
          endif
        endif

      else                      ! Single CPU

        call cps_memory_max(max_vsize,max_rss)
        call string_ii2cc  (max_vsize,cvsize)
        call cps_write_accounting ('VSIZE_MEMORY = ('//trim(cvsize)//')')
        call string_ii2cc  (max_rss,crss)
        call cps_write_accounting ('RSS_MEMORY = ('//trim(crss)//')')

        call timer_fetch (1,cnt_time,    &
                          min_utime, max_utime, avg_utime, sum_utime,    &
                          min_stime, max_stime, avg_stime, sum_stime,    &
                          var_time, sd_time, elapse_time)
        write (card_tmp,fmt) &
         'SETUPS              ', sum_utime,sum_stime,elapse_time
        call pcps_print(trim(adjustl(card_tmp)))
        if (nprocess_utime .gt. 0) process_utime(1) = sum_utime
        if (nprocess_stime .gt. 0) process_stime(1) = sum_stime
        
        call timer_fetch (2,cnt_time,    &
                          min_utime, max_utime, avg_utime, sum_utime,    &
                          min_stime, max_stime, avg_stime, sum_stime,    &
                          var_time, sd_time, elapse_time)
        write (card_tmp,fmt)  &
         'TRSCAN              ',sum_utime,sum_stime,elapse_time
        call pcps_print(trim(adjustl(card_tmp)))
        if (nprocess_utime .gt. 0) process_utime(2) = sum_utime
        if (nprocess_stime .gt. 0) process_stime(2) = sum_stime
        
        do i=3,nprocess_list
          call timer_fetch (i,cnt_time,    &
                            min_utime, max_utime, avg_utime, sum_utime,    &
                            min_stime, max_stime, avg_stime, sum_stime,    &
                            var_time, sd_time, elapse_time)
          write (card_tmp,fmt) process_list(i),sum_utime,sum_stime,elapse_time
          call pcps_print(trim(adjustl(card_tmp)))
          if (nprocess_utime .ge. i) process_utime(i) = sum_utime
          if (nprocess_stime .ge. i) process_stime(i) = sum_stime
        enddo

      endif

      if (nprocess_utime .gt. 0 .and. nprocess_stime .gt. 0) then
        call cardset_create    (cardset)
        call cardset_put_array (cardset,'PROCESS_USER_TIME',process_utime,  &
                                nprocess_list)
        call cardset_put_array (cardset,'PROCESS_SYSTEM_TIME',process_stime,  &
                                nprocess_list)
        ncards = cardset_num_cards (cardset)
        do i=1,ncards
          call cardset_get_card     (cardset,i,card,errmsg)
          call cps_write_accounting (card)
        enddo
        call cardset_delete (cardset)
      endif

      if(nprocess_utime.gt.0) deallocate(process_utime, stat=i)
      if(nprocess_stime.gt.0) deallocate(process_stime, stat=i)
      
      p_etime=cps_elapse_time(date_processing_started,time_processing_started,&
        date_finished, time_finished)

      call string_date (date_finished)
      call string_time (time_finished)
      time_finished(3:3) = ':'
      time_finished(6:6) = ':'
      etime = cps_elapse_time (date_started,time_started,date_finished,  &
                               time_finished)

      call pcps_print (' ')
      call pcps_print (' ')
      call pcps_print &
      ('======================================================================')
      call pcps_print &
      ('                            Job Accounting                            ')
      call pcps_print &
      ('======================================================================')
      write (card_tmp,*) &
        'Job started                 = ',date_started ,'  ',time_started
      call pcps_print(trim(adjustl(card_tmp)))
      
      write (card_tmp,*) &
        'Job finished                = ',date_finished,'  ',time_finished
      call pcps_print(trim(adjustl(card_tmp)))
      
      write (card_tmp,'(1X,A,G12.5,A,G12.5,A)') &
        'Elapse time                 = ',etime,' seconds  (',etime/3600.0, &
                                                                         ' hrs)'
      call pcps_print(trim(adjustl(card_tmp)))
      
      write(card_tmp,'(1X,A,G12.5,A,G12.5,A)') &
        'Elapse processing time      = ',p_etime,' seconds  (',p_etime/3600.0, &
                                                                         ' hrs)'
      call pcps_print(trim(adjustl(card_tmp)))
      
      if (present(cpu_rstats)) then
        utime=sum(cpu_rstats(1,nprocess_list+2,0:pcps_num_workers))
        stime=sum(cpu_rstats(2,nprocess_list+2,0:pcps_num_workers))
        write(card_tmp,'(1X,A,G12.5,A,G12.5,A)') &
          'Total User CPU time         = ',utime,' seconds  (',utime/3600.0, &
                                                                         ' hrs)'
        call pcps_print(trim(adjustl(card_tmp)))

        write(card_tmp,'(1X,A,G12.5,A,G12.5,A)') &
          'Total System CPU time       = ',stime, ' seconds  (',stime/3600.0, &
                                                                         ' hrs)'
        call pcps_print(trim(adjustl(card_tmp)))

      else                      ! Single CPU
        call getsys_usage (maxrss,nswap,minflt,majflt,inblock,outblock,  &
                           utime, stime)
        write (card_tmp,'(1X,A,G12.5,A,G12.5,A)')   &
          'User CPU time               = ',utime,' seconds  (',utime/3600.0, &
                                                                         ' hrs)'
        call pcps_print(trim(adjustl(card_tmp)))

        write (card_tmp,'(1X,A,G12.5,A,G12.5,A)')   &
          'System CPU time             = ',stime,' seconds  (',stime/3600.0, &
                                                                         ' hrs)'
        call pcps_print(trim(adjustl(card_tmp)))
        call getsys_cpu_speed(cpu_speed)

        if (max_vsize .gt. 0) then
          write (card_tmp,'(1X,A,I10,A)')   &
          'Maximum Virtual Memory Size = ',max_vsize,' kb (approximate)'
          call pcps_print(trim(adjustl(card_tmp)))
        endif

        if (max_rss .gt. 0) then
          write (card_tmp,'(1X,A,I10,A)')   &
          'Maximum Resident Set Size   = ',max_rss,' kb (approximate)'
          call pcps_print(trim(adjustl(card_tmp)))
        endif

        if (cpu_speed .gt. 0) then
          write (card_tmp,*) 'CPU speed                   = ',cpu_speed ,' MHz'
          call pcps_print(trim(adjustl(card_tmp)))
        endif
      endif
      call pcps_print (' ')
      call pcps_print (' ')

!--------- write folder

      rec_keeping = ' '
      call pc_get_jdata ('REC_KEEPING',rec_keeping)
      call string_to_upper       (rec_keeping)
      call string_squeeze_blanks (rec_keeping)
      if (trim(rec_keeping) .eq. 'YESJOBSUMMARYFILE') then
        call cps_folder (date_started//'  '//time_started,    &
                         date_finished//'  '//time_finished,  &
                         utime,stime,etime)
      else
        call pcps_print ('NO CPS FOLDER REQUESTED')
      endif

      call pc_restore

      call string_ff2cc (utime         ,cutime)
      call string_ff2cc (stime         ,cstime)
      call string_dd2cc (etime         ,cetime)
      call string_ii2cc (pcps_num_procs,cncpus)

      call cps_write_accounting ('USER_TIME = '//trim(cutime)//  &
                               ', SYSTEM_TIME = '//trim(cstime)// &
                               ', ELAPSE_TIME = '//trim(cetime)// &
                               ', NUM_CPUS = '//trim(cncpus))

      if (present(signal)) then
        call cps_write_accounting ('Job Abort - Signal Handler')
        call cps_write_accounting ('Job finished')
        call pcps_print("++++++++++++++++++++++++++++++++++")
        call pcps_print("CPS aborting for following reason:")
        call pcps_print(trim(CPS_SIGNALS(signal))//' error occurred')
        call pcps_print("++++++++++++++++++++++++++++++++++")
        call cio_finalize()
      else if (prev) then
        call cps_write_accounting ('Job Abort - Setup errors')
        call cps_write_accounting ('Job finished')
        call cio_finalize()
        call cps_error_exit("Setup errors occurred")
      else if (error) then
        call cps_write_accounting ('Job Abort - Processing errors')
        call cps_write_accounting ('Job finished')
        call cio_finalize()
        call cps_error_exit("Processing errors occurred")
      else
        call cps_write_accounting ('Job finished')
        call cio_finalize()
      endif


      end subroutine cps_finish_processing


!!----------------------- setup_top_of_loop -------------------------------!!
!!----------------------- setup_top_of_loop -------------------------------!!
!!----------------------- setup_top_of_loop -------------------------------!!


      subroutine cps_setup_top_of_loop (ipn)
      implicit none
      integer,intent(in)       :: ipn                   ! argument

      call pcps_print(' ')
      call pcps_print(DASHLINE)
      call pcps_print('Top of new loop.')
      call pcps_print(DASHLINE)
      call pc_put_global ('numtr', 0)

      end subroutine cps_setup_top_of_loop


!!--------------------------- pre_setup --------------------------------!!
!!--------------------------- pre_setup --------------------------------!!
!!--------------------------- pre_setup --------------------------------!!


      subroutine cps_pre_setup (current_ipn)
      implicit none
      integer,optional,intent(out)     :: current_ipn        ! argument

      character(len=PC_LENGTH)         :: card               ! local
      integer                          :: i                  ! local
      integer                          :: ipn                ! local
      integer                          :: istat              ! local
      integer                          :: ncards             ! local
      character(len=PC_LENGTH)         :: process_name       ! local
      character(len=PC_LENGTH)         :: card_tmp           ! local
      character(len=CARDSET_LENGTH)    :: errmsg             ! local
      logical                 :: reading_process_parameters  ! local


!--------- get started:

      call pc_next
      ipn = pc_get_ipn()
      pre_ipn = ipn
      if (present(current_ipn)) current_ipn = ipn

      call pcps_print(' ')
      call pcps_print(DASHLINE)

!--------- print process name:

      if (ipn <= nprocess_list) then
        process_name = process_list(ipn)
      else
        process_name = ' '
      endif
      if (process_name /= ' ') then
        write(card_tmp,*) 'Starting setup for process ',ipn,'  ', &
          trim(process_name)
        call pcps_print(trim(adjustl(card_tmp)))
      else
        write(card_tmp,*) 'Starting setup for unspecified process ',ipn
        call pcps_print(trim(adjustl(card_tmp)))
      end if

!--------- initialize history:

      istat = hist_init  (ipn,trim(process_name))
      if (istat == HIST_ERROR) call pcps_print('Error initializing history')


!--------- get process parameters from file:
!--------- print process parameters:
!--------- put parameters into parameter cache:


      if (ipn == 1) then
        call pcps_print(' ')
        call pcps_print('Input parameters:')
        ncards = cardset_num_cards (pdata_cardset)
        do i=1,ncards
          call cardset_get_card (pdata_cardset,i,card,errmsg)
   !      call pcps_print(trim(card))
          call pcps_print('  '//adjustl(card))
          if (i == 1) then
            call pc_put_process_card (card)
           else
            call pc_add_process_card (card)
           endif
        enddo
      else if (ipn == 2) then
        call pcps_print(' ')
        call pcps_print('Input parameters:')
        ncards = cardset_num_cards (jdata_cardset)
        do i=1,ncards
          call cardset_get_card (jdata_cardset,i,card,errmsg)
   !      call pcps_print(trim(card))
          call pcps_print('  '//adjustl(card))
          if (i == 1) then
            call pc_put_process_card (card)
           else
            call pc_add_process_card (card)
           endif
        enddo
      else
        reading_process_parameters = .false.
        ncards = 0
        do
           istat=cio_fgetline(card,PC_LENGTH,plun)
           if (istat <  0 ) then
             call cps_print ('error trying to read parms file')
             stop "error reading process parameters file"
           end if
           card_tmp = adjustl(card)
           call string_to_upper(card_tmp)

           if (card_tmp .eq. ' ') then
             cycle
           else if (card_tmp(1:10) .eq. '</PROCESS>') then
             exit
           else if (card_tmp(1:13) .eq. '</PARAMETERS>') then
             reading_process_parameters = .false.
           else if (card_tmp(1:13).eq.'<PROCESS NAME' .and. ncards.eq.0) then
             reading_process_parameters = .true.
             call pcps_print(' ')
             call pcps_print('Input parameters:')
           else if (card_tmp(1:9) .eq. '<GLOBALS>') then
             reading_process_parameters = .false.
           else if (card_tmp(1:10) .eq. '<CONTROLS>') then
             reading_process_parameters = .false.
           else if (card_tmp(1:12) .eq. '<PARAMETERS>') then
             reading_process_parameters = .true.
           else if (reading_process_parameters) then
             ncards = ncards + 1
   !         call pcps_print(trim(card))
             call pcps_print('  '//adjustl(card))
             if (ncards .eq. 0) then
               call pc_put_process_card (card)
             else
               call pc_add_process_card (card)
             endif
           else
             cycle
           endif
        enddo
      endif
      call pcps_print(' ')
      call dbug_set_system_message ('cps_pre_setup, process = '//process_name)

      return
      end subroutine cps_pre_setup


!!--------------------------- post_setup --------------------------------!!
!!--------------------------- post_setup --------------------------------!!
!!--------------------------- post_setup --------------------------------!!


      subroutine cps_post_setup
      implicit none
      integer                          :: i  ! local
      character(len=PC_LENGTH),pointer :: cards(:)           ! local
      integer                          :: ncards ! local
      integer                          :: ipn,istat          ! local
      character(len=PC_LENGTH)         :: process_name       ! local
      integer                          :: nstore,nscratch    ! local
      integer                          :: vsize,rss          ! local
      character (len=80)               :: card_tmp           ! local


!--------- get parameters from parameter cache:

      ipn = pc_get_ipn()
      post_ipn = ipn

!--------- trscan not needed for project_data and job_data

      if (ipn > 2) call trscan_setup (ipn = ipn, iwhen = 2)

      nullify(cards)
      nstore   = 0
      nscratch = 0

!--------- get process name:

      if (ipn == 1) then
           process_name = 'PROJECT_DATA'
      else if (ipn == 2) then
           process_name = 'JOB_DATA'
           jobname = ' '
           call pc_get_jdata ('jobname', jobname)
      else
           process_name = ' '
           call pc_get_jdata ('process_list', ipn, process_name)
      endif

!--------- write data cards to history file:

      call pc_alloc_process_cards (cards,ncards)
      if (ncards > 0) then
        istat = hist_write (ipn,cards)
        if (istat == HIST_ERROR) call pcps_print('Error writing history cards')
      endif

      call pc_get_control         ('nscratch',nscratch)
      call pc_get_control         ('nstore',nstore)

!--------- print control parameters:

      call pcps_print(' ')
      write(card_tmp,'(1X,A,I10,A)')  &
                     'Scratch storage             = ',nscratch,' words'
      call pcps_print(trim(adjustl(card_tmp)))
      write(card_tmp,'(1X,A,I10,A)')  &
                     'Permanent storage           = ',nstore,' words'
      call pcps_print(trim(adjustl(card_tmp)))

      call cps_memory_c (vsize,rss)
      write(card_tmp,'(1X,A,I10,A)')  &
                     'Current Resident Set Size   = ',rss,' kb'
      call pcps_print(trim(adjustl(card_tmp)))
      write(card_tmp,'(1X,A,I10,A)')  &
                     'Current Virtual Memory Size = ',vsize,' kb'
      call pcps_print(trim(adjustl(card_tmp)))
      call pcps_print(' ')

!--------- print global parameters:

      call pc_alloc_global_cards (cards,ncards)
      call pcps_print(' ')
      if (process_name /= ' ') then
        write(card_tmp,*) 'Globals after setup for process ',ipn,'  ',  &
                            trim(process_name)
        call pcps_print(trim(adjustl(card_tmp)))
      else
        write(card_tmp,*) 'Globals after setup for unspecified process ',ipn
        call pcps_print(trim(adjustl(card_tmp)))
      endif

      if (ncards > 0) then
        do i=1,ncards
          call pcps_print(trim(cards(i)))
        enddo

        istat = hist_write (ipn,'GLOBALS AFTER '//trim(process_name)//':')
        if (istat == HIST_ERROR) call pcps_print('Error writing history')
        istat = hist_write (ipn,cards)
        if (istat == HIST_ERROR) call pcps_print('Error writing history cards')
      endif
      call pcps_print(DASHLINE)
      call pcps_print(' ')

      if (associated(cards)) deallocate(cards)

      return
      end subroutine cps_post_setup


!!-------------------------- pre_process -------------------------------!!
!!-------------------------- pre_process -------------------------------!!
!!-------------------------- pre_process -------------------------------!!


      subroutine cps_pre_process (ipn)
      implicit none
      integer         ,intent(in)       :: ipn                   ! argument

      pre_ipn = ipn
      call timer_start (ipn)

      end subroutine cps_pre_process


!!-------------------------- post_process -------------------------------!!
!!-------------------------- post_process -------------------------------!!
!!-------------------------- post_process -------------------------------!!


      subroutine cps_post_process (ipn,ntr,hd,tr)
      implicit none
      integer         ,intent(in)       :: ipn,ntr               ! argument
      double precision,intent(inout)    :: hd(:,:)               ! argument
      real            ,intent(in)       :: tr(:,:)               ! argument

      integer                           ::i                      ! local
      integer                           ::ihdr1                  ! local
      call timer_stop   (ipn)
      post_ipn = ipn
      post_hdr = hd(1,1)
      call timer_start  (2)
      if(ntr > 0 ) &
      call trscan_print (ipn,ntr,hd,tr,process_list(ipn),2)
      call timer_stop   (2)
      call cps_memory   (ntr,hd)
      if (tstamp_inc > 0 .and. ntr > 0) then
        do i=1,ntr
          ihdr1 = hd(1,i)
          if (mod(ihdr1,tstamp_inc) == 0)  &
            call cps_time_stamp (ipn,hd(1,i),pcps_current_worker_num)
        enddo
      endif

      end subroutine cps_post_process


!!---------------------------- top_of_loop --------------------------------!!
!!---------------------------- top_of_loop --------------------------------!!
!!---------------------------- top_of_loop --------------------------------!!


      subroutine cps_top_of_loop (ipn)
      implicit none
      integer,intent(in)       :: ipn                   ! argument


            !------ empty
            !------ empty
            !------ empty

      end subroutine cps_top_of_loop


!!---------------------------- elapse_time --------------------------------!!
!!---------------------------- elapse_time --------------------------------!!
!!---------------------------- elapse_time --------------------------------!!


      function cps_elapse_time (date1,time1,date2,time2) result (elapse)
      implicit none
      character(len=*),intent(in)            :: date1               ! argument
      character(len=*),intent(in)            :: time1               ! argument
      character(len=*),intent(in)            :: date2               ! argument
      character(len=*),intent(in)            :: time2               ! argument
      double precision                       :: elapse              ! result

      integer                                :: year1               !local
      integer                                :: year2               !local
      integer                                :: month1              !local
      integer                                :: month2              !local
      integer                                :: day1                !local
      integer                                :: day2                !local
      integer                                :: hours1              !local
      integer                                :: hours2              !local
      integer                                :: minutes1            !local
      integer                                :: minutes2            !local
      integer                                :: seconds1            !local
      integer                                :: seconds2            !local

      call string_cc2ii (date1(1:4) ,year1   )
      call string_cc2ii (date1(6:7) ,month1  )
      call string_cc2ii (date1(9:10),day1    )
      call string_cc2ii (time1(1:2) ,hours1  )
      call string_cc2ii (time1(4:5) ,minutes1)
      call string_cc2ii (time1(7:8) ,seconds1)

      call string_cc2ii (date2(1:4) ,year2   )
      call string_cc2ii (date2(6:7) ,month2  )
      call string_cc2ii (date2(9:10),day2    )
      call string_cc2ii (time2(1:2) ,hours2  )
      call string_cc2ii (time2(4:5) ,minutes2)
      call string_cc2ii (time2(7:8) ,seconds2)


      year1 = year1 - 1900
      year2 = year2 - 1900

      month1 = month1 - 1
      month2 = month2 - 1

      call cps_diff_time (elapse,year1,month1,day1,hours1,minutes1,seconds1,  &
                                 year2,month2,day2,hours2,minutes2,seconds2)

      end function cps_elapse_time


!!------------------------------ trscan -----------------------------------!!
!!------------------------------ trscan -----------------------------------!!
!!------------------------------ trscan -----------------------------------!!


      subroutine cps_trscan (ipn,ntr,hd,tr)
      implicit none
      integer         ,intent(in)       :: ipn,ntr               ! argument
      double precision,intent(inout)    :: hd(:,:)               ! argument
      real            ,intent(in)       :: tr(:,:)               ! argument



      call trscan_print (ipn,ntr,hd,tr,process_list(ipn),2)

      end subroutine cps_trscan


!!------------------------------- folder ----------------------------------!!
!!------------------------------- folder ----------------------------------!!
!!------------------------------- folder ----------------------------------!!


      subroutine cps_folder (time_started,time_finished,utime,stime,etime)

      character(len=*) ,intent(in)     :: time_started                 !argument
      character(len=*) ,intent(in)     :: time_finished                !argument
      real             ,intent(in)     :: utime                        !argument
      real             ,intent(in)     :: stime                        !argument
      double precision ,intent(in)     :: etime                        !argument

      character(len=80)                :: card                         !local
      character(len=20)                :: jobname                      !local
      character(len=64)                :: file                         !local
      character(len=80),pointer        :: cards(:)                     !local

      character(len=PNAME_LEN)         :: process                      !local
      character(len=12)                :: traces                       !local
      character(len=12)                :: dead                         !local
      character(len=PARAM_LEN)         :: proc_data(MAX_PROC_DATA)     !local
      character(len=PNAME_LEN)         :: proc(MAX_PROC)               !local
      character(len=PNAME_LEN),pointer :: process_list(:)              !local



      integer                          :: i                            !local
      integer                          :: j                            !local
      integer                          :: i1                           !local
      integer                          :: i2                           !local
      integer                          :: id_sav                       !local
      integer                          :: nproc(MAX_PROC)              !local
      integer                          :: ntotal_proc                  !local
      integer                          :: iproc(MAX_PROC)              !local

      integer                          :: istat                        !local
      integer                          :: ncards                       !local
      integer                          :: ofile                        !local
      integer                          :: nprocess_list                !local


! Get folder name
      card = ' '
      call pc_get_jdata ('SUB_PROJECT',card)
      call string_squeeze_blanks (card)
      file = trim(card(1:57)) // '.folder'

! Load process template list
      call cps_folder_proc_table(proc,nproc,iproc,proc_data,ntotal_proc)

! Open folder file
      call getlun (ofile)
     open (unit=ofile,file=trim(file),access='SEQUENTIAL',status='UNKNOWN')

      nullify(process_list)
      jobname       = ' '
      nprocess_list = 0
      call pc_get_jdata   ('JOBNAME'     , jobname)
      call pc_alloc_jdata ('PROCESS_LIST', process_list, nprocess_list)

      write (ofile,'(A)') ' '
      write (ofile,'(A)') ' '
      write (ofile,'(A28,A)')     'Job Name                  = ',jobname
      write (ofile,'(A28,A)')     'Report Starts             = ',time_started
      write (ofile,'(A28,A)')     'Report Ends               = ',time_finished
      write (ofile,'(A28,G12.5)') 'User CPU Time   (seconds) = ',utime
      write (ofile,'(A28,G12.5)') 'System CPU Time (seconds) = ',stime
      write (ofile,'(A28,G12.5)') 'Elapse Time     (seconds) = ',etime
      write (ofile,'(A)') ' '

      card( 1:12)  = 'Process '
      card(13:13)  = '|'
      card(14:54)  = '                 Parameters'
      card(55:55)  = '|'
      card(56:67)  = ' # of Traces'
      card(68:68)  = '|'
      card(69:80)  = ' Dead Traces'
      write(ofile,'(A)') card
      card( 1:12)  = '------------'
      card(13:13)  = '|'
      card(14:54)  = '-----------------------------------------'
      card(55:55)  = '|'
      card(56:67)  = '------------'
      card(68:68)  = '|'
      card(69:80)  = '------------'
      write(ofile,'(A)') card

      do i=1,nprocess_list
        ncards = hist_numcards(i)
        if (ncards .le. 0) return
        allocate(cards(ncards))
        istat = hist_read(i,ncards,cards)
        process = process_list(i)
        call string_strip_blanks(process)
        traces  = ' '
        dead    = ' '
        do j=1,ncards-1
          i1 = index(cards(j),'AFTER '//trim(process))
          if (i1 .gt. 0) then
            i2 = index(cards(j+1),'TRACES')
            if (i2 .gt. 0) then
              traces = cards(j+1)(i2-13:i2-2)
              i2 = index(cards(j+1),'DEAD')
              dead = cards(j+1)(i2-13:i2-2)
            endif
          endif
        enddo

        card( 1:12)  = process(1:12)
        card(13:13)  = '|'
        card(14:54)  = ' '
        card(55:55)  = '|'
        card(56:67)  = traces
        card(68:68)  = '|'
        card(69:80)  = dead

        call cps_folder_process(cards,id_sav,process,ofile,proc,   &
                                nproc,iproc,proc_data,ntotal_proc, &
                                ncards,card)
        if (associated(cards)) then
          deallocate(cards)
        endif
      enddo

      return
      end subroutine cps_folder


!!---------------------------- folder_process -----------------------------!!
!!---------------------------- folder_process -----------------------------!!
!!---------------------------- folder_process -----------------------------!!


      subroutine cps_folder_process (cards,id,process,ofile,proc,nproc,    &
                                     iproc,proc_data,ntotal_proc,ncards,   &
                                     process_card)

      character(len=80)        ,intent(in)    :: cards(:)              !argument
      integer                  ,intent(inout) :: id                    !argument
      character(len=PNAME_LEN) ,intent(in)    :: process               !argument
      integer                  ,intent(in)    :: ofile                 !argument
      character(len=PNAME_LEN) ,intent(in)    :: proc(:)               !argument
      integer                  ,intent(in)    :: nproc(:)              !argument
      integer                  ,intent(in)    :: iproc(:)              !argument
      character(len=PNAME_LEN) ,intent(inout) :: proc_data(:)          !argument
      integer                  ,intent(in)    :: ntotal_proc           !argument
      integer                  ,intent(in)    :: ncards                !argument
      character(len=80)        ,intent(inout) :: process_card          !argument


      character(len=PARAM_LEN)         :: keyword                      !local
      character(len=CARDSET_LENGTH)    :: value                        !local
      character(len=CARDSET_LENGTH)    :: value_tmp                    !local
      character(len=CARDSET_LENGTH),pointer :: array(:)                !local
      character(len=80)                :: prnt                         !local
      character(len=80)                :: temp                         !local
      character(len=80)                :: errmsg                       !local


      integer                          :: i                            !local
      integer                          :: j                            !local
      integer                          :: k                            !local

      integer                          :: i1                           !local



      integer                          :: ifound                       !local
      integer                          :: num                          !local
      integer                          :: ntemp                        !local
      integer                          :: isrt                         !local
      integer                          :: narray                       !local
      integer                          :: nature                       !local

      logical                          :: first_card                   !local

      type(cardset_struct),pointer     :: cardset                      !local


      prnt(1 :12)  = ' '
      prnt(13:13)  = '|'
      prnt(14:54)  = ' '
      prnt(55:55)  = '|'
      prnt(56:67)  = ' '
      prnt(68:68)  = '|'
      prnt(69:80)  = ' '
      first_card = .true.

!---search for process in proc list

      ifound = 0
      do i=1,ntotal_proc
        if (trim(process) .eq. trim(proc(i))) then
          ifound = i
          isrt   = iproc(i)
          num    = nproc(i)
        endif
      enddo

!---if process not in list write card and return
      if (ifound .eq. 0) then
        write(ofile,'(A)') process_card
        return
      endif

!---process has keywords to search for

      nullify(cardset)
      nullify(array)
      call cardset_create (cardset)
      do i=1,ncards
        i1 = index(cards(i),'*HISTORY IPN: ')
        if (i1 .gt. 0) cycle
        i1 = index(cards(i),'GLOBALS AFTER ')
        if (i1 .gt. 0) exit
        call cardset_add_card (cardset,cards(i))
      enddo


      do i=1,num
        keyword = trim(proc_data(isrt-1+i))
        if (cardset_keyword_present (cardset,keyword)) then
          nature = cardset_nature (cardset,keyword)
          if (nature .eq. CARDSET_SCALAR) then
            value = ' '
            call cardset_get_scalar (cardset,keyword,value,errmsg)
            call string_to_upper (value,value_tmp)
            if (len_trim(value) .eq. 0 .or. trim(value_tmp) .eq. 'NONE') cycle
            j = index(keyword,'PATH')
            if (j .gt. 0) then
              value_tmp = value
!!!           i1 = index        (value_tmp,':',.true.)
              i1 = string_index (value_tmp,':',.true.)
              if (i1.gt.0 .and. i1.lt.len_trim(value)) value = value_tmp(i1+1:)
            endif
            if (first_card) then
              first_card = .false.
            else
              process_card = prnt
            endif
            temp  = trim(keyword)//'='//trim(value(1:79-len_trim(keyword)))
            ntemp = len_trim(temp)
            if (ntemp .le. 41) then
              process_card(14:54) = temp(1:ntemp)
              write (ofile,'(A)') process_card
            else if (len_trim(value) .le. 67) then
              process_card(14:54)   = trim(keyword)//'='
              write (ofile,'(A)') process_card
              process_card = prnt
!New sun compiler had problem with the following statement
!             process_card(14:min(80,14+len_trim(value)-1)) = value
              j = len_trim(value)
              process_card(14:min(80,14+j-1)) = value
              write (ofile,'(A)') process_card
            else
              process_card(14:54)   = trim(keyword)//'='
              write (ofile,'(A)') process_card
              process_card = prnt
              i1 = len_trim(value)-63
!New sun compiler had problem with the following statement
!             process_card(14:min(80,14+len_trim(value)-1)) = '...'//value(i1:)
              j = len_trim(value)
              process_card(14:min(80,14+j-1)) = '...'//value(i1:)
              write (ofile,'(A)') process_card
            endif
          else if (nature .eq. CARDSET_ARRAY) then
            narray = 0
            call cardset_alloc_array (cardset,keyword,array,narray,errmsg)
            if (narray .gt. 0) then
              if (first_card) then
                first_card = .false.
              else
                process_card = prnt
              endif
              temp = trim(keyword)//'=('//trim(array(1)(1:78-len_trim(keyword)))
              ntemp = len_trim(temp)
              if (ntemp .le. 41) then
                process_card(14:54) = temp(1:ntemp)
                write (ofile,'(A)') process_card
              else
                process_card(14:54)   = trim(keyword)//'=('
                write (ofile,'(A)') process_card
                process_card = prnt
                k = min(79, 14+len_trim(array(1))-1)
                process_card(14:k) = array(1)
                if (narray .gt. 1) then
                  process_card(k+1:k+1) = ','
                else
                  process_card(k+1:k+1) = ')'
                endif
                write (ofile,'(A)') process_card
              endif
              do j=2,narray
                process_card = prnt
                k = min(79, 14+len_trim(array(j))-1)
                process_card(14:k) = array(j)
                if (j .ne. narray) then
                  process_card(k+1:k+1) = ','
                else
                  process_card(k+1:k+1) = ')'
                endif
                write (ofile,'(A)') process_card 
              enddo
            endif
          endif
        endif
      enddo
      call cardset_delete (cardset)
      if (associated(array)) deallocate(array)

      if (first_card) write(ofile,'(A)') process_card

      return
      end subroutine cps_folder_process


!!-------------------------- folder_proc_table ----------------------------!!
!!-------------------------- folder_proc_table ----------------------------!!
!!-------------------------- folder_proc_table ----------------------------!!


      subroutine cps_folder_proc_table (proc,nproc,iproc,proc_data,ntotal_proc)

      character(len=PNAME_LEN) ,intent(out) :: proc(:)                 !argument
      integer                  ,intent(out) :: nproc(:)                !argument
      integer                  ,intent(out) :: iproc(:)                !argument
      character(len=PARAM_LEN) ,intent(out) :: proc_data(:)            !argument
      integer                  ,intent(out) :: ntotal_proc             !argument

      character(len=80)                     :: card                   
      character(len=160)                    :: folderfile

      integer                               :: i1,istat 
      integer                               :: i2                      !local
      integer                               :: ifn                     !local
      integer                               :: id_proc                 !local
      integer                               :: id_proc_data            !local

      call getlun (ifn)
      call cnfg_get_value('cps_template_dir',card)
      folderfile=trim(card) // '/' // 'folder_template'
      open (unit=ifn,file=trim(folderfile),action='READ',iostat=istat)
      if(istat.ne.0)then
        print*,' cps_folder_proc_table--> Unable to open ',trim(folderfile)
      endif
            

      id_proc = 0
      id_proc_data = 0
      do
        read (ifn,'(A)',end=200) card
        i1 = index(card,',')
        if (i1 .gt. 1) then
          id_proc = id_proc + 1
          proc(id_proc) = card(1:i1-1)
          nproc(id_proc) = 0
          iproc(id_proc) = id_proc_data + 1
          i2 = 1
          do 
            i2 = index(card(i1+1:),',')
            if (i2 .gt. 0) then
              nproc(id_proc) = nproc(id_proc) + 1
              id_proc_data = id_proc_data + 1
              proc_data(id_proc_data) = card(i1+1:i1+i2-1)
              call string_strip_blanks(proc_data(id_proc_data))
              i1 = i1 + i2
              if (i1 .ge. 79) exit
            else
             exit
            endif
          enddo
        endif
      enddo
  200 continue
      close (unit=ifn)
      ntotal_proc = id_proc

      return
      end subroutine cps_folder_proc_table


!!------------------------------- get_lun ---------------------------------!!
!!------------------------------- get_lun ---------------------------------!!
!!------------------------------- get_lun ---------------------------------!!


      function cps_get_lun () result (lun_out)
      implicit none
      integer :: lun_out                            ! result

      lun_out = lun

      return
      end function cps_get_lun


!!-------------------------------- memory ---------------------------------!!
!!-------------------------------- memory ---------------------------------!!
!!-------------------------------- memory ---------------------------------!!


      subroutine cps_memory (ntr,hd)
      implicit none
      integer         ,intent(in)       :: ntr                   ! argument
      double precision,intent(inout)    :: hd(:,:)               ! argument

      integer                           :: i                     ! local
      integer                           :: n                     ! local
      integer                           :: rss                   ! local
      integer                           :: vsize                 ! local

      if (ntr > 0) then
        do i=1,ntr
          n = hd(1,i)
          if      (n .gt. 1000) then
            if (mod(n,1000) == 0) then
              call cps_memory_c (vsize,rss)
              max_rss   = max(max_rss,rss)
              max_vsize = max(max_vsize,vsize)
            endif
          else if (n .gt. 100 ) then
            if (mod(n,100) == 0) then
              call cps_memory_c (vsize,rss)
              max_rss   = max(max_rss,rss)
              max_vsize = max(max_vsize,vsize)
            endif
          else if (n .gt. 10  ) then
            if (mod(n,10) == 0) then
              call cps_memory_c (vsize,rss)
              max_rss   = max(max_rss,rss)
              max_vsize = max(max_vsize,vsize)
            endif
          else
            call cps_memory_c (vsize,rss)
            max_rss   = max(max_rss,rss)
            max_vsize = max(max_vsize,vsize)
          endif
        enddo
      endif

      return
      end subroutine cps_memory


!!-------------------------------- abort ----------------------------------!!
!!-------------------------------- abort ----------------------------------!!
!!-------------------------------- abort ----------------------------------!!


      subroutine cps_abort (message)
      character(len=*),intent(in),optional :: message               ! argument

      if (present(message)) then
        call pcps_print("++++++++++++++++++++++++++++++++++")
        call pcps_print("CPS aborting for following reason:")
        call pcps_print(message)
        call pcps_print("++++++++++++++++++++++++++++++++++")
      endif
      if (abort_on_error) call cps_abort_c()

      return
      end subroutine cps_abort


!!--------------------------------- exit ----------------------------------!!
!!--------------------------------- exit ----------------------------------!!
!!--------------------------------- exit ----------------------------------!!


      subroutine cps_error_exit (message)
      character(len=*),intent(in),optional :: message               ! argument

      if (present(message)) then
        call pcps_print("++++++++++++++++++++++++++++++++++")
        call pcps_print("CPS aborting for following reason:")
        call pcps_print(message)
        call pcps_print("++++++++++++++++++++++++++++++++++")
      endif
      if (abort_on_error) call cps_error_exit_c()

      return
      end subroutine cps_error_exit


!!------------------------- print_current_status --------------------------!!
!!------------------------- print_current_status --------------------------!!
!!------------------------- print_current_status --------------------------!!


      subroutine cps_print_current_status

      integer              :: rss                               ! local
      integer              :: vsize                             ! local
      character(len=80)    :: ctmp                              ! local
      character(len=256)   :: mess                              ! local
      integer,allocatable  :: htmp(:)                           ! local
      integer              :: htmp_len                          ! local
      integer              :: istat                             ! local
      integer              :: temp

      if (pcps_num_procs.gt.1) then

        write(mess,*) 'At shutdown of worker(',  &
                       pcps_current_worker_num,') --',  &
                      ' cps_pre_process ipn = ',pre_ipn,  &
                      ', cps_post_process ipn = ',post_ipn,' trace = ',post_hdr
        call pcps_errmsg(mess)
        temp = len_trim(mess)
        htmp_len = string_num_integers(temp)             !Write to cps_syslog
        allocate(htmp(htmp_len),stat=istat)              !Write to cps_syslog
        if (istat .eq. 0) then                           !Write to cps_syslog
          call string_cc2hh (mess,htmp)                  !Write to cps_syslog
          call cpslog_message(htmp)                      !Write to cps_syslog
          deallocate(htmp,stat=istat)                    !Write to cps_syslog
        endif                                            !Write to cps_syslog

        call cps_memory_c (vsize,rss)
        write(mess,*) 'At shutdown of worker(',  &
                       pcps_current_worker_num,') --',  &
                      ' Resident Set Size = ',rss,' kb,',  &
                      ' Virtual Memory Size = ',vsize,' kb'
        call pcps_errmsg(mess)
        temp = len_trim(mess)
        htmp_len = string_num_integers(temp          )   !Write to cps_syslog
        allocate(htmp(htmp_len),stat=istat)              !Write to cps_syslog
        if (istat .eq. 0) then                           !Write to cps_syslog
          call string_cc2hh (mess,htmp)                  !Write to cps_syslog
          call cpslog_message(htmp)                      !Write to cps_syslog
          deallocate(htmp,stat=istat)                    !Write to cps_syslog
        endif                                            !Write to cps_syslog

        call dbug_get_system_message (ctmp)
        write(mess,'(1X,A,I4,A,A)') 'Dbug system of worker(',  &
                       pcps_current_worker_num,') -- ',  &
                      ctmp
        call pcps_errmsg(mess)
        temp = len_trim(mess)
        htmp_len = string_num_integers(temp          )   !Write to cps_syslog
        allocate(htmp(htmp_len),stat=istat)              !Write to cps_syslog
        if (istat .eq. 0) then                           !Write to cps_syslog
          call string_cc2hh (mess,htmp)                  !Write to cps_syslog
          call cpslog_message(htmp)                      !Write to cps_syslog
          deallocate(htmp,stat=istat)                    !Write to cps_syslog
        endif                                            !Write to cps_syslog

        call dbug_get_message (ctmp)
        write(mess,'(1X,A,I4,A,A)') 'Dbug of worker(',  &
                       pcps_current_worker_num,') -- ',  &
                      ctmp
        call pcps_errmsg(mess)

      else

        write(mess,*) 'At shutdown --',  &
                      ' cps_pre_process ipn = ',pre_ipn,  &
                      ', cps_post_process ipn = ',post_ipn,' trace = ',post_hdr
        call pcps_errmsg(mess)
        temp = len_trim(mess)
        htmp_len = string_num_integers(temp          )   !Write to cps_syslog
        allocate(htmp(htmp_len),stat=istat)              !Write to cps_syslog
        if (istat .eq. 0) then                           !Write to cps_syslog
          call string_cc2hh (mess,htmp)                  !Write to cps_syslog
          call cpslog_message(htmp)                      !Write to cps_syslog
          deallocate(htmp,stat=istat)                    !Write to cps_syslog
        endif                                            !Write to cps_syslog

        call cps_memory_c (vsize,rss)
        write(mess,*) 'At shutdown --',  &
                      ' Resident Set Size = ',rss,' kb,',  &
                      ' Virtual Memory Size = ',vsize,' kb'
        call pcps_errmsg(mess)
        temp = len_trim(mess)
        htmp_len = string_num_integers(temp          )   !Write to cps_syslog
        allocate(htmp(htmp_len),stat=istat)              !Write to cps_syslog
        if (istat .eq. 0) then                           !Write to cps_syslog
          call string_cc2hh (mess,htmp)                  !Write to cps_syslog
          call cpslog_message(htmp)                      !Write to cps_syslog
          deallocate(htmp,stat=istat)                    !Write to cps_syslog
        endif                                            !Write to cps_syslog

        call dbug_get_system_message (ctmp)
        write(mess,'(1X,A,A)') 'Dbug system -- ',ctmp
        temp = len_trim(mess)
        htmp_len = string_num_integers(temp          )   !Write to cps_syslog
        allocate(htmp(htmp_len),stat=istat)              !Write to cps_syslog
        if (istat .eq. 0) then                           !Write to cps_syslog
          call string_cc2hh (mess,htmp)                  !Write to cps_syslog
          call cpslog_message(htmp)                      !Write to cps_syslog
          deallocate(htmp,stat=istat)                    !Write to cps_syslog
        endif                                            !Write to cps_syslog
        call pcps_errmsg(mess)

        call dbug_get_message (ctmp)
        write(mess,'(1X,A,A)') 'Dbug -- ',ctmp
        call pcps_errmsg(mess)

      endif

      call pcps_errmsg(' ')

      return
      end subroutine cps_print_current_status


!!------------------------ write_accounting_signal ------------------------!!
!!------------------------ write_accounting_signal ------------------------!!
!!------------------------ write_accounting_signal ------------------------!!


      subroutine cps_write_accounting_signal (signal)

      integer,intent(in)          :: signal                     ! argument

      character(len=20)           :: process_name               ! local
      character(len=80)           :: card                       ! local
      integer,allocatable         :: hcard(:)                   ! local
      integer                     :: hcard_len                  ! local
      integer                     :: istat                      ! local
      integer                     :: temp

      if (signal .eq. SIGUSR2 .or. signal .eq. SIGTERM) return


      if (pre_ipn .gt. 0 .and. pre_ipn .le. nprocess_list) then
        process_name = process_list(pre_ipn)
      else
        process_name = ' '
      endif
      call cps_write_accounting ('SIGNAL = '//trim(CPS_SIGNALS(signal))//  &
                                 ', HOST = '//trim(machine)//  &
                                 ', PROCESS = '//trim(process_name),2)

!!    Write to cps_syslog also

      card = 'Error: signal = '//trim(CPS_SIGNALS(signal))//  &
             ', process = '//trim(process_name)
      temp = len_trim(card)
      hcard_len = string_num_integers(temp          )
      allocate(hcard(hcard_len),stat=istat)
      if (istat .eq. 0) then
        call string_cc2hh (card,hcard)
        call cpslog_message(hcard)
        deallocate(hcard,stat=istat)
      endif

      return
      end subroutine cps_write_accounting_signal



      subroutine cps_get_project_name(project_name)
         character(len=*),intent(out)  :: project_name
         character                     :: str(len(project_name)+1)
         call cps_get_project_name_c(str(1))
         project_name=""
         call cio_unform_c_str(project_name,str)
         project_name=trim(project_name)
         !print*,'cps_get_project_name: project_name=[',trim(project_name),']'
      end subroutine cps_get_project_name

      subroutine cps_set_project_name(project_name)
         character(len=*),intent(in )  :: project_name
         character                     :: str(len(project_name)+1)
         integer                       :: temp
         temp = len_trim(project_name)
         call cio_form_c_str(project_name,str,temp)
         call cps_set_project_name_c(str(1))
      end subroutine cps_set_project_name 

!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!

      end module cps_module


!!------------------------------- print_f ---------------------------------!!
!!------------------------------- print_f ---------------------------------!!
!!------------------------------- print_f ---------------------------------!!


      subroutine cps_print_f (message,len_message)

      use pcps_module
      use string_module

      implicit none

      integer,intent(in) :: message(*)                              ! argument
      integer,intent(in) :: len_message                             ! argument

      character(len=132) :: string                                  ! local


      if (len_message .le. 132) then
        call string_hh2cc (message,string)
        call pcps_print   (string,2)
      endif

      return
      end subroutine cps_print_f


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

