!<CPS_v1 type="PROCESS"/>
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
!                         C P S   P R O C E S S
!
! Name       : JOB_DATA
! Category   : miscellaneous
! Written    : 1999-06-15   by: Donna K. Vunderink
! Revised    : 2011-05-16   by: Bill Menger
! Maturity   : beta
! Purpose    : Job-oriented parameter entry for parameters specific to the job.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! 
! The job-oriented parameter entry for all parameters specific to the particular
! job being built or executed.  These parameters are likely to vary among
! different jobs in the same project. 
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! 
! 
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process is a setup only process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process is a setup only process.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       changed
! NDPT     number of sample values in trace      changed
! TSTRT    starting time on trace                changed
! DT       trace sample interval                 changed
! NUM_CPUS number of cpus working in parallel    changed
!
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
! 
! This process is a setup only process.
!
!-------------------------------------------------------------------------------
!</header_word_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY 
!
!       Date      Author       Description
!       ----      ------       -----------
! 74. 2011-05-16  Bill Menger  Modified for simpler specifications on PEs/node
! 73. 2007-05-10  Goodger      Remove queues B1530, B1260W, B1400W, B1800, P.
! 72. 2007-04-10  Bill Menger  Added "calling_program" as a data element.
! 71. 2006-12-04  D. Glover    Added NULLIFY statements for Intel compiler.
! 70. 2006-06-27  Goodger      Add Queue B2390.  Remove B3060.
! 69. 2006-06-05  Bill Menger  Added num_cpus to globals.
! 68. 2006-01-10  B. Menger    Removed Unused Variables.
! 67. 2005-07-11  Goodger      Add B2210 and B2210TD queues.  Remove B1390 and
!                              below.
! 66. 2005-04-11  Goodger      Allow all users to use B1400W and B1260
!                              queues.
! 65. 2005-01-17  Goodger      Add priority parameter.
! 64. 2004-07-22  Goodger      Remove B3060TD1 and B3060TD2 and add B3060.
! 63. 2004-07-13  Goodger      Change PONY queue name to B. Add queues
!                              B3060TD1 and B3060TD2.
! 62. 2004-06-14  Goodger      Add job restart parameter.
!                              Remove SPEED parameter.
! 61. 2004-05-26  Goodger      Fix nuisance invalid queue message if pony.
! 60. 2004-04-21  Goodger      Add queue B2000SQ for Alaska which will run
!                              one job at at time.
! 59. 2004-03-23  Goodger      Change PONY queue from B2800 to B2790.  Add
!                              queue B3060TD.
! 58. 2004-02-25  Goodger      Increase rlocation menu to 3 to accomodate PONY.
! 57. 2004-02-13  Goodger      Get the location from the config file.
!                              Allow single cpu jobs into B queues.
!                              Add location PONY.
! 56  2004-01-28  Goodger      Add queue B1400W for Faqi Liu.
! 55. 2003-12-18  Goodger      Add B2000 queue for Alaska.
! 54. 2003-12-09  Goodger      Add remote location parameter.  Remove B queue.
!                              Do not allow blanks in sub-project field.
!                              Add B800SAS queue.
! 53. 2003-10-20  Goodger      Add queue B1260W.  Allow selection by account
!                              liuf only.  Incorporate Randy Selzler change
!                              of removing a hard-coded reference to poepsn03.
! 52. 2003-09-09  Goodger      Avoid floating point arithmetic problem with
!                              trace_length parameter by converting to
!                              character.
! 51. 2003-09-04  Goodger      Add B1800 queue and allow B800 for Houston.
! 50. 2003-08-01  Goodger      Add B1400TD queue.
! 49. 2003-06-18  Goodger      Add queues B1000 and B1260.
! 48. 2003-06-16  Goodger      Call queue trap for front end only.
! 47. 2003-05-22  Goodger      Add trap for valid queues.
! 46. 2003-05-07  Goodger      Add location Alaska, queues B800 and A. Remove
!                              Ponca only queues.
! 45. 2003-04-16  Goodger      Add B1530 and B1390 queues. Added documentation
!                              to the programming notes section.
! 44. 2003-03-21  Goodger      Remove 250 limit on number of cpus and nodes.
! 43. 2003-03-18  Goodger      Added P queue. Have only Houston queues show up
!                              if running in Houston, and only Ponca queues 
!                              show up in runningIdin Ponca.
! 42. 2002-09-24  Vunderink    Changed MACHINE option from poepsn03 to Solaris.
! 41. 2002-08-30  Vunderink    Added PATHNAME_TRCIO parameter.
! 40. 2002-07-11  Vunderink    Changed STD_LIBS error to warning.
! 39. 2002-06-13  Vunderink    Enable speed parameter for FXSHOT queue.
! 38. 2002-06-06  Vunderink    Added NUM_NODES parameter.
!                              Added FXSHOT queue and changed default for
!                                PCPS_REPORT_OPT.
! 37. 2002-02-27  Vunderink    Added ALPHALIB to libraries.
! 36. 2002-02-05  Vunderink    Added speed parameter.
! 35. 2001-11-29  Vunderink    Increased maximum for num_cpus to 250 and added
!                                file input option to custom_nodes.
! 34. 2001-11-12  Vunderink    Added A2 and S queue and removed warning message.
! 33. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB.
! 32. 2001-08-10  Vunderink    Added job_data_wrapup subroutine and TSTAMP_INC
!                                parameter.
! 31. 2001-08-03  Goodger      Change history options to be the same as TROT.
! 30. 2001-07-03  Vunderink    Added PCPS_REPORT_OPT and DEBUG_LEVEL parameters,
!                                modified usage of B and C queues and eliminated
!                                the D queue, removed special handling of pospx1
!                                and pospx3 nodes, improved automatic setting
!                                of STD_LIBS to CUSTOM, and fixed bug keeping
!                                Ctrl+I from working on custom parameter arrays.
! 29. 2001-03-28  Vunderink    Added CUSTOM_EXEC_B, CUSTOM_EXEC_A, CUSTOM_LAM
!                                and CUSTOM_NODES parameters
! 28. 2001-02-27  Vunderink    Removed unused parameters max_cart and max_mag,
!                                and added a pc_get FRONTEND_PATH in initialize.
! 27. 2001-02-22  Vunderink    Force queue=b5 and num_cpus=1 for nodes pospx1
!                                and pospx3
! 26. 2001-02-15  Vunderink    Added A_short queue and improved queue error
!                                checking
! 25. 2001-01-29  Vunderink    Removed PARALLEL_NODES and changed MACHINE to
!                                Linux
! 24. 2001-01-17  Vunderink    Added warnings if custom fields are filled out
!                                but STDLIBS not equal to CUSTOM or vice versa.
! 23. 2000-12-29  Vunderink    Added parameter PARALLEL_NODES.
! 22. 2000-12-06  Vunderink    Added CHECKLAV to trscan option menu.
! 21. 2000-11-07  Vunderink    Added to machine list.
! 20. 2000-10-12  Vunderink    Added chmod g+rw on folder 
! 19. 2000-10-06  Vunderink    Removed PATHNAME_NODE and PATHNAME_USERID.
! 18. 2000-09-29  Vunderink    Enable HISTORY_OPT.
! 17. 2000-09-18  Vunderink    Fixed bug in pc_put_control call.
! 16. 2000-08-23  Vunderink    Added cps_folder support and added to machine
!                                list.
! 15. 2000-05-12  Vunderink    Added SETUP_ONLY to control parameters
! 14. 2000-04-25  Vunderink    Change std_libs_menu to include PRODLIB, TESTLIB,
!                                and CUSTOM when library source is unknown
! 13. 2000-04-18  Vunderink    Allow pospx3 to be a frontend_node
! 12. 2000-04-17  Vunderink    Added traps for all option menus
! 11. 2000-03-30  Vunderink    Adjusted screen labels for TSTRT and NWIH
! 10. 2000-03-24  Vunderink    Removed TOP.LAY and BOT.LAY includes from gui_def
!                                layout
!  9. 2000-03-07  Vunderink    Set the machine default based on machine list,
!                                fix machine list for frontend move to poepsn03,
!                                removed ndec argument from pc_put calls, and
!                                added parameter NWIH
!  8. 2000-02-23  Vunderink    Remove num_pes parameter, add queue_menu,
!                                modify machine list based on frontend machine,
!                                make queue and num_cpus insensitive until
!                                supported, updated documentation, and added
!                                gui definition to documentation.
!  7. 2000-02-08  Vunderink    Added poeplx04 to machine list
!  6. 2000-02-04  Vunderink    Made parameter, parameter default, and parameter
!                                    character length changes required by
!                                    Chuck I. Burch documentation changes, and
!                                    made HISTORY_OPT insensitive until it is
!                                    supported. 
!  5. 2000-02-02  Vunderink    Fixed code not to exceed 80 characters, changed
!                                    LINE_NAME default to SUB_PROJECT from 
!                                    PROJECT_DATA process, changed STDLIBS 
!                                    choices to be dependent CFE link, restored
!                                    TRSCAN_OPT and changed options to 
!                                    BASIC/ADVANCED, and added poeplx03 to
!                                    machine menu.
!  4. 2000-01-17  Vunderink    Updated documentation and added poepsn03 to
!                                    machine list
!  3. 2000-01-04  Vunderink    Commented out unused parameters TIME_LIMIT, 
!                                    NUM_PES, RESTRT_CHKPT, and TRSCAN_OPT.
!                              Changed QUEUE default to b5.
!                              Added new parameters PATHNAME_NODE, 
!                                    PATHNAME_USERID, and PATHNAME_DIR.
!  2. 1999-09-13  Vunderink    Made parameter cache named constants changes
!  1. 1999-06-16  Vunderink    Initial version.
!
!
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

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process is a setup only processes.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
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

!<gui_def>
!
!<NS JOB_DATA Process>
!
!          [/L/XST]Jobname~~=~~~~~~~~~    [JOBNAME/EN]`SSSSSSSSSSSSSSS
!          [/L/XST]Comment~~=~~~~~~~~     [JOB_COMMENT]`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          [/L/XST]Sub-Project Name~~=~~~ [SUB_PROJECT]`SSSSSSSSSSS             [/XST]Record Keeping~~=~~~[REC_KEEPING]`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          [/L/XST]PDN Userid~~=~~~~~~~~~~[PDN_USERID]`SSSSSSSSSSS             [/XST]PCPS Report Option =[PCPS_REPORT_OPT]`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          [/L/XST]Number of Exec/node=~~~[NUM_CPUS]`IIIIIIIIIII             [/XST]Mail Option~~=~~~~~~[MAIL_OPT]`CCCCCCCCCCCCCC
!          [/L/XST]Number of Nodes~~=~~~~~[NUM_NODES]`IIIIIIIIIII             [/XST]History Option~~=~~~[HISTORY_OPT]`CCCCCCCCCCCCCC
!          [/L/XST]Queue~~=~~~~~~~~~~~    [QUEUE/XST]`CCCCCCCCCCC             [/XST]Standard Libraries =[STD_LIBS]`CCCCCCCCCCCCCC   [/XST]Priority~~=`II
!          [/L/XST]Machine~~=~~~~~~~~~~~~~`CCCCCCCCCCC             [/XST]TRSCAN Option~~=~~~~[TRSCAN_OPT]`CCCCCCCCCCCCCC
!          [/L/XST]Rerunable~~=~~~~~~~~~~~[RERUN]`CCCCCCCCCCC             [/XST]Debug Level~~=~~~~~~[DEBUG_LEVEL]`CCCCCCCCCCCCCC
!          [/L/XST]Time Stamp Increment~~=[TSTAMP_INC]`IIIIIIIIIII             [/XST]Remote Location~~=~~[RLOCATION]`CCCCCCCCCCCCCC
!          [/L/XST]Calling_Program~~~~~~~=`CCCCCCCCCCC                         [/XST]Time Limit~~=~~[TIME_LIMIT]`IIIIII
!          [/L/XST]MailingAddress~~~~~~~~=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!       `- Default PATHNAME Parameter -----------------------------------------------------------------+
!       |
!       |  [/XST]Trace Data Directory~~=[PATHNAME_TRCIO]`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!       |  [/XST]Other Data Directory~~=[PATHNAME_DIR]`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!       `----------------------------------------------------------------------------------------------+
!
!       `- Trace Length and Sample Rate ---------------------------------------------------------------+
!       |
!       | [/XST]Length of traces (seconds) =~~[TRACE_LENGTH]`FFFFFFFFFFF      [/XST]DT (sample interval (seconds)) =[DT]`FFFFFFFFFFF
!       | [/XST]TSTRT (start time (seconds)) =[TSTRT]`FFFFFFFFFFF      [/XST]NDPT (# samples/trace) =~~~~~~~~[NDPT]`IIIIIIIIIII
!       |                                                 [/XST]NWIH (#words in header) = ~~~~~~[NWIH]`IIIIIIIIIII
!       `----------------------------------------------------------------------------------------------+
!
!<NS Custom Parameters>
!
! [/YST]CUSTOM_COMPILE
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! [/YST]CUSTOM_MODULES
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! [/YST]CUSTOM_LINK
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! `- CUSTOM_EXECUTE = mpirun EXEC_B executeable EXEC_A -------------------------------+
!  EXEC_B =[CUSTOM_EXEC_B]`SSSSSSSSSSSSSSSSSSSSSSSSSSS        EXEC_A =[CUSTOM_EXEC_A]`SSSSSSSSSSSSSSSSSSSSSSSSSSS
! `-----------------------------------------------------------------------------------+
!
! Select CUSTOM_NODES...[SELECT_CUSTOM_NODES_PATH]`P      [/XST/C]Custom lamboot ? `CC
! [CUSTOM_NODES_PATH]`SSSSSSSSSSSSSSSSSSSSS
! CUSTOM_NODES
! `SSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSS
!
!<PARMS CUSTOMLAMBOOT[CUSTOM_LAM/XSF]>
!<PARMS CUSTOM_NODES[/XST/YST]>
!<PARMS SELECT_CUSTOM_NODES_PATH[/XST/YST]>
!<PARMS CUSTOM_NODES_PATH[/ML=140/XST/YST]>
!
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="JOBNAME">
!<Tip> Name used to identify the job being built. </Tip>
! Default = 
! Allowed = char*15
! Name used to identify the job being built.
!  
!</Help>
!
!<Help KEYWORD="JOB_COMMENT">
!<Tip> Descriptive text entered by the user as documentation for the job. </Tip>
! Default = 
! Allowed = char*132
! Descriptive text entered by the user as documentation for the job.
!</Help>
!
!<Help KEYWORD="SUB_PROJECT">
!<Tip> Name used to identify the sub-project within the current project. </Tip>
! Default =
! Allowed = char*10
! 
! The name given to the sub-project within the current project.  This field
! is used in the default PDN descriptor to catalog output tape data sets.
!</Help>
!
!<Help KEYWORD="QUEUE">
!<Tip> The batch queue for executing the batch job. </Tip>
! Default = batch
! Allowed = B_multi   
! Allowed = B_single  
! Allowed = test
! Allowed = batch
! The batch queue for executing the batch job.
!</Help>
!
!<Help KEYWORD="MAIL_OPT">
!<Tip> Whether or not to be notified when the job has completed. </Tip>
! Default = NO
! Allowed = NO
! Allowed = YES (end)
! Allowed = SE (start and end)
! Allowed = ABT (abort only)
! 
! If NO, do not send mail.  If YES, send mail when job completes. If SE, send
! mail when job starts and when job ends.  If ABT, send mail only if job aborts.
!</Help>
!
!<Help KEYWORD="HISTORY_OPT">
!<Tip> Select the history option that will be printed to the rpt file. </Tip>
! Default = MODEL
! Allowed = MODEL
! Allowed = ALL
! Allowed = NONE
! Allowed = CURRENT
! 
! If ALL, print all histories.  If NONE, do not print any histories.  If MODEL,
! print a set of histories that model job processing.  If
! CURRENT, print current history only. 
!</Help>
!
!<Help KEYWORD="PDN_USERID">
!<Tip> The user name under which output tape data sets will be cataloged. </Tip>
! Default =
! Allowed = char*10
! This field is used in the default PDN descriptor to catalog output tape data
! sets.
!</Help>
!
!<Help KEYWORD="REC_KEEPING">
!<Tip>Controls whether or not a Job Summary File is created</Tip>
! Default = "No    Job Summary File"
! Allowed = "Yes   Job Summary File"
!           "No    Job Summary File"
! 
!The Job Summary File will be named SUB_PROJECT_NAME.FOLDER and written into the
!current working directory.
!</Help>
!
!
!<Help KEYWORD="PCPS_REPORT_OPT">
!<Tip>Controls disposition worker printout</Tip>
! Default = "Include worker printout"
! Allowed = "Include worker printout"
!           "Do not include worker printout"
!
!Controls whether or not worker printout is included in the final report file.
!</Help>
!
!<Help KEYWORD="TSTAMP_INC">
!<Tip> Number of traces between writing of progress time stamp. </Tip>
! Default = 1000
! Allowed = int >= 0
!
! The batch job will write the current date and time, process number and 
! trace headers 1, 3, and 4 to a file named time_stamp after every additional
! TSTAMP_INC traces are output by a process.
!
! TSTAMP_INC = 0 disables the writes.
!</Help>
!
!<Help KEYWORD="MACHINE">
!<Tip> The computer system for job execution. </Tip>
! Default = Linux
! Allowed = Linux
! Allowed = Solaris
! Allowed = Custom
! The name of the computer system for job execution.
!</Help>
!
!<Help KEYWORD="RERUN">
!<Tip> Whether or not this job is rerunnable. </Tip>
! Default = NO
! Allowed = NO
! Allowed = YES
! If set to YES, PBS may try to rerun the job if it has encountered problems,
! such as node failure.
!</Help>
!
!<Help KEYWORD="NUM_CPUS">
!<Tip> The number of CPUs or PEs (tasks, execs, cores...) requested per node to execute this job. </Tip>
! Default = 1
! Allowed = integer
! The number of CPUs or parallel elements (PEs) (tasks) per node to execute this job.
!</Help>
!
!<Help KEYWORD="NUM_NODES">
!<Tip> The number of machines required to execute this job. </Tip>
! Default = 1
! Allowed = 0 > int <= NUM_CPUS
! The number of machines required to execute this job.
! Example: If there were 12 physical CPUs on each Linux node, then to run on 8 of them per node on 4 nodes:
!   use: NUM_CPUS=8, NUM_NODES=4 (total Number of executables = 4*8=32.
!   to run more execs/node than the physical number of cores, use 0 for NUM_CPUS and the system
!   will balance the request on the NUM_NODES requested.
!</Help>
!
!<Help KEYWORD="STD_LIBS">
!<Tip> Select the libraries needed for this job. </Tip>
! Default = Depends on the library used in linking CFE
! Allowed = "PRODLIB"  if CFE linked with PRODLIB
! Allowed = "BETALIB"  if CFE linked with BETALIB
! Allowed = "ALPHALIB" if CFE linked with ALPHALIB
! Allowed = "CUSTOM"
! Select the libraries needed for this job. 
!</Help>
!
!<Help KEYWORD="PRIORITY">
!<Tip> The priority of job (-5 to 5). </Tip>
! Default = 0
! Allowed = integer
! The priority of the job may range from -5 to 5.
!</Help>
!
!<Help KEYWORD="PATHNAME_DIR">
!<Tip> The default directory name for process PATHNAME parameters. </Tip>
! Default = Front-end directory
! Allowed = char*80
! A valid directory name to use in building a default for process PATHNAME 
! parameters.  PATHNAME_DIR may include sub-directories.
!</Help>
!
!<Help KEYWORD="PATHNAME_TRCIO">
!<Tip>The default directory name for process trace file parameters. </Tip>
! Default = ~userid/cpsdata
! Allowed = char*80
! A valid directory name to use in building a default for process trace file
! PATHNAME parameters.  PATHNAME_TRCIO may include sub-directories.
!</Help>
!
!<Help KEYWORD="TRSCAN_OPT">
!<Tip> Select the mode of TRSCAN operation. </Tip>
! Default = BASIC
! Allowed = BASIC
! Allowed = ADVANCED
! TRSCAN collects statistics on the traces passing thru each process and
! prints them when NTR = NO_MORE_TRACES.  In ADVANCED mode, TRSCAN prints
! a histogram of the data.
!</Help>
!
!<Help KEYWORD="Calling_Program">
!<Tip> This tells the processes what system they are running within.</Tip>
! Default = CPS
! Allowed = CPS
! Allowed = SEISSPACE
! Calling_Program tells each process that needs the information what system
! has invoked the process.  This allows processes to behave appropriately
! for the particular system in which they are running.
!</Help>
!
!<Help KEYWORD="MailingAddress">
!<Tip>  Enter email address for use with the "mail" option.</Tip>
! Default = NO DEFAULT
! Allowed = Any email address
! This is where output status will be sent if using any email options.
!</Help>
!
!<Help KEYWORD="TIME_LIMIT">
!<Tip>Tell system to abort job after TIME_LIMIT seconds.</Tip>
! Default = "3600"
! Allowed = "any + integer"
! This sets the time limit for batch queue submission only.
!</Help>
!
!<Help KEYWORD="DEBUG_LEVEL">
!<Tip>Controls the level of debug printout in the report file</Tip>
! Default = "NO DEBUG"
! Allowed = "NO DEBUG"
!           "LEVEL 1"
!           "LEVEL 2"
!           "LEVEL 3"
!
!Sets the JDATA parameter DEBUG_LEVEL to 0, 1, 2 or 3 for the user desired
!level of debug printout in the report file.  It is up to each individual
!process to decide what is printed. 
!</Help>
!
!<Help KEYWORD="RLOCATION">
!<Tip>Build job for remote location rather than local location</Tip>
! Default = "LOCAL"
! Allowed = "LOCAL"
!           "ALASKA"
!           "PONY"
!
!This information is passed to the job builder.  If RLOCATION is not LOCAL
!the job will be built to be submitted from here and run at the remote site.
!Report file will come back to the local site.
!</Help>
!
!<Help KEYWORD="TRACE_LENGTH">
!<Tip> Length of the trace in trace sample units. </Tip>
! Default = 0.0
! Allowed = real>0.0
! The length of the trace in trace sample units.  NDPT=(TRACE_LENGTH/DT)+1
!</Help>
!
!<Help KEYWORD="TSTRT">
!<Tip> Start time of the trace. </Tip>
! Default = 0.0
! Allowed = real >=0.0
! Start time of the trace in the trace sample units.
!</Help>
!
!<Help KEYWORD="DT">
!<Tip> Sample interval for the data set. </Tip>
! Default = 0.0
! Allowed = real>0.0
! The sample interval for the data set in trace sample units.
!</Help>
!
!<Help KEYWORD="NDPT">
!<Tip> Number of samples in the trace. </Tip>
! Default = 0
! Allowed = int>0
! The number of samples in the trace.  NDPT=(TRACE_LENGTH/DT)+1
!</Help>
!
!<Help KEYWORD="NWIH">
!<Tip> Number of words in the trace header. </Tip>
! Default = 64
! Allowed = int>=64
! The number of words in the trace header.
!</Help>
!
!<Help KEYWORD="CUSTOM_MODULES">
!<Tip> Custom Fortran 90 modules directory. </Tip>
! Default =
! Allowed = char*80
! 
! Allows the user to specify a custom Fortran 90 modules directory to be used
! in compiling the job executable (main). 
!</Help>
!
!<Help KEYWORD="CUSTOM_COMPILE">
!<Tip> Custom compile statements. </Tip>
! Default =
! Allowed = char*80
! 
! Allows the user to specify compile statments to be added to the job before
! the compile and link of the job executable.
!</Help>
!
!<Help KEYWORD="CUSTOM_LINK">
!<Tip> Custom link for building the job executable. </Tip>
! Default =
! Allowed = char*80
!
! Allows the user to specify a custom link for building the job executable
! (main).  When using a custom link, the user must specify all object code
! (except for main.o) and libraries in the desired order. The backslash will
! be added to the end of each line  by the job builder to concatenate them
! into a single command.
!</Help>
!
!<Help KEYWORD="CUSTOM_EXEC_B">
!<Tip> Custom execution of the job executable. </Tip>
! Default =
! Allowed = char*80
!
! Allows the user to specify a custom execution statement.  This feature is
! intended for parallel development but can be used for any job.  The format
! of the execution statement for parallel jobs will be
!
!       mpirun CUSTOM_EXEC_B executable CUSTOM_EXEC_A
!
! where  CUSTOM_EXEC_B and/or CUSTOM_EXEC_A are provided by the user.
!
! The format for single-cpu jobs will be
!
!      CUSTOM_EXEC_B executable CUSTOM_EXEC_A
!
! where  CUSTOM_EXEC_B and/or CUSTOM_EXEC_A are provided by the user.
!</Help>
!
!<Help KEYWORD="CUSTOM_EXEC_A">
!<Tip> Custom execution of the job executable. </Tip>
! Default =
! Allowed = char*80
!
! Allows the user to specify a custom execution statement.  This feature is
! intended for parallel development but can be used for any job.  The format
! of the execution statement for parallel jobs will be
!
!       mpirun CUSTOM_EXEC_B executable CUSTOM_EXEC_A
!
! where  CUSTOM_EXEC_B and/or CUSTOM_EXEC_A are provided by the user.
!
! The format for single-cpu jobs will be
!
!      CUSTOM_EXEC_B executable CUSTOM_EXEC_A
!
! where  CUSTOM_EXEC_B and/or CUSTOM_EXEC_A are provided by the user.
!</Help>
!
!<Help KEYWORD="CUSTOM_LAM">
!<Tip> Do you want the jobfile include a lamboot ? </Tip>
! Default = YES
! Allowed = YES
! Allowed = NO
!
! If YES, the jobfile will include a lamboot using either CUSTOM_NODES or nodes
! provided by the batch queuing software.
!
! If NO, the user is responsible for the lamboot.
!</Help>
!
!<Help KEYWORD="SELECT_CUSTOM_NODES_PATH">
!<Tip> This button accesses the Select CUSTOM_NODES dialog box. </Tip>
!Input CUSTOM_NODES via a file using a file selection dialog.
!</Help>
!
!<Help KEYWORD="CUSTOM_NODES_PATH">
!<Tip> Pathname of file containing a list of custom_nodes. </Tip>
! Default = -
! Allowed = char
!
! Pathname of file containing a list of custom_nodes to be loaded into the
! CUSTOM_NODES array.  This file must be an ascii file with one node name on
! each line. Default directory for file is PATHNAME_DIR and default extension is
! ".lst".
!</Help>
!
!<Help KEYWORD="CUSTOM_NODES">
!<Tip> Custom execution node list. </Tip>
! Default = -
! Allowed = char*12
!
! Allows the user to specify a custom list of nodes to be used by the job.  This
! option may be used for both single-cpu and parallel jobs.
!
! If the job is to be batch and the job is a single-cpu job, the node must be
! known to the batch queuing software.  If the job is to be batch and the job is
! a parallel job, the first node must be known to the batch queuing software and
! all others must not.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module job_data_module

      use cio_module
      use cnfg_module
      use finquire_module
      use getsys_module
      use named_constants_module
      use path_module
      use pathcheck_module
      use pathchoose_module
      use pc_module
      use putsys_module
      use string_module

      implicit none
      private
      public :: job_data_create     ! uses the parameter cache.
      public :: job_data_initialize ! uses the parameter cache.
      public :: job_data_update     ! uses the parameter cache.
      public :: job_data_delete
      public :: job_data_wrapup

      character(len=100),public,save :: job_data_ident = &
!068. 2006-01-10  B. Menger   Removed Unused Variables.
       '$Id: job_data.f90,v 1.73 2007/05/11 13:59:57 Goodger beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: job_data_struct              
 
        private
        logical                       :: skip_wrapup        ! wrapup flag
        character(len=15)             :: jobname            ! process parameter
        character(len=132)            :: job_comment        ! process parameter
        character(len=10)             :: sub_project        ! process parameter
        character(len=8)              :: queue              ! process parameter
        integer                       :: time_limit         ! process parameter
        character(len=4)              :: mail_opt           ! process parameter
        character(len=8)              :: history_opt        ! process parameter
        character(len=10)             :: pdn_userid         ! process parameter
        character(len=10)             :: machine            ! process parameter
        character(len=4)              :: rerun              ! process parameter
        character(len=132)            :: pathname_dir       ! process parameter
        character(len=132)            :: pathname_trcio     ! process parameter
        integer                       :: num_cpus           ! process parameter
        integer                       :: priority
        integer                       :: num_nodes          ! process parameter
        integer                       :: tstamp_inc         ! process parameter
        character(len=24)             :: std_libs           ! process parameter
        character(len=32)             :: rec_keeping        ! process parameter
        character(len=32)             :: pcps_report_opt    ! process parameter
        character(len=8)              :: trscan_opt         ! process parameter
        character(len=9)              :: calling_program    ! process parameter
        character(len=80)             :: mailingaddress     ! process parameter
        character(len=8)              :: debug_level        ! process parameter
        character(len=8)              :: rlocation          ! process parameter
        real                          :: trace_length       ! process parameter
        real                          :: tstrt              ! process parameter
        real                          :: dt                 ! process parameter
        integer                       :: ndpt               ! process parameter
        integer                       :: nwih               ! process parameter
        integer                       :: ncustom_modules    ! process parameter
        character(len=80),pointer     :: custom_modules(:)  ! process parameter
        integer                       :: ncustom_compile    ! process parameter
        character(len=80),pointer     :: custom_compile(:)  ! process parameter
        integer                       :: ncustom_link       ! process parameter
        character(len=80),pointer     :: custom_link(:)     ! process parameter
        character(len=160)            :: custom_exec_b      ! process parameter
        character(len=160)            :: custom_exec_a      ! process parameter
        character(len=4)              :: custom_lam         ! process parameter
        integer                       :: ncustom_nodes      ! process parameter
        character(len=12),pointer     :: custom_nodes(:)    ! process parameter
        character(len=FILENAME_LENGTH):: custom_nodes_path  ! process parameter
        type(pathchoose_struct),pointer :: custom_nodes_dialog
                                                            ! process parameter
        integer                       :: nprocess_list      ! process parameter
        character(len=12),pointer     :: process_list(:)    ! process parameter
 
        integer                       :: nqueue_menu             ! option menu
        character(len=8)              :: queue_menu(17)          ! option menu
        integer                       :: nmail_menu              ! option menu
        character(len=4)              :: mail_menu(4)            ! option menu
        integer                       :: nhistory_menu           ! option menu
        character(len=8)              :: history_menu(4)         ! option menu
        integer                       :: nmachine_menu           ! option menu
        character(len=10)             :: machine_menu(17)        ! option menu
        integer                       :: nstd_libs_menu          ! option menu
        character(len=24)             :: std_libs_menu(4)        ! option menu
        integer                       :: nrec_keeping_menu       ! option menu
        character(len=32)             :: rec_keeping_menu(2)     ! option menu
        integer                       :: npcps_report_opt_menu   ! option menu
        character(len=32)             :: pcps_report_opt_menu(2) ! option menu
        integer                       :: ntrscan_menu            ! option menu
        character(len=8)              :: trscan_menu(3)          ! option menu
        integer                       :: ndebug_level_menu       ! option menu
        character(len=8)              :: debug_level_menu(4)     ! option menu
        integer                       :: nrlocation_menu         ! option menu
        character(len=14)             :: rlocation_menu(3)       ! option menu
        integer                       :: nyesno_menu             ! option menu
        character(len=4)              :: yesno_menu(2)           ! option menu
        integer                       :: nnoyes_menu             ! option menu
        character(len=4)              :: noyes_menu(2)           ! option menu
        integer                       :: n_calling_program_menu  ! option menu
        character(len=9)              :: calling_program_menu(2) ! option menu

        character(len=16)             :: frontend_node      ! dependent variable
        character(len=16)             :: frontend_user      ! dependent variable
        character(len=132)            :: frontend_path      ! dependent variable
        character(len=8)              :: location           ! dependent variable

      end type job_data_struct

      character(len=10) :: thisuserid


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(job_data_struct),pointer,save :: object      ! needed for traps.


      contains


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine job_data_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword   ! argument

      integer                      ::  i         ! local

      integer                      ::  lun       ! local
      integer                      ::  istat     ! local

      character(len=PC_LENGTH)     ::  ctemp1    ! local
      character(len=PC_LENGTH)     ::  ctemp2    ! local
      character(len=PC_LENGTH)     ::  msg       ! local
      logical                      ::  matches   ! local
 
      select case (keyword)

        case ('Calling_Program')
          call string_to_upper (object%Calling_Program)
          if(object%calling_program .eq. 'CPS') then
          elseif( object%calling_program .eq. 'SEISSPACE' ) then
          else
          endif

        case ('MailingAddress')
          call string_squeeze_blanks (object%mailingaddress)
 
        case ('SUB_PROJECT')
          call string_squeeze_blanks (object%sub_project)
          call path_validate_file    (object%sub_project,istat,msg)
          if (istat .eq. PATH_INVALID) then
            call pc_error (msg)
          endif
          if (pc_get_update_state() .eq. PC_GUI) then
            object%rec_keeping = object%rec_keeping_menu(1)
          endif
 
        case ('MAIL_OPT')
          call string_to_upper (object%mail_opt)
          matches = .false.
          do i=1,object%nmail_menu
            if (trim(object%mail_opt) .eq. trim(object%mail_menu(i))) then
              matches = .true.
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_error ('Invalid MAIL_OPT '//trim(object%mail_opt))
            object%mail_opt = trim(object%mail_menu(1))
          endif
 
        case ('HISTORY_OPT')
          call string_to_upper (object%history_opt)
          if(object%history_opt.eq.'YES')object%history_opt='ALL'
          if(object%history_opt.eq.'NO')object%history_opt='NONE'
          if(object%history_opt.eq.'BRIEF')object%history_opt='MODEL'
          if(object%history_opt.eq.'DEL')object%history_opt='CURRENT'
          matches = .false.
          do i=1,object%nhistory_menu
            if (trim(object%history_opt) .eq. trim(object%history_menu(i))) then
              matches = .true.
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_error ('Invalid HISTORY_OPT '//trim(object%history_opt))
            object%history_opt = trim(object%history_menu(1))
          endif
 
        case ('MACHINE')
          matches = .false.
          do i=1,object%nmachine_menu
            if (trim(object%machine) .eq. trim(object%machine_menu(i))) then
              matches = .true.
              if (trim(object%machine) .ne. 'Custom') then
                if (trim(object%custom_lam) .eq. 'YES') then
                  object%custom_lam = 'NO'
                  call pc_warning ('CUSTOM_LAM changed to NO')
                endif
                if (object%ncustom_nodes .gt. 0) then
                  object%ncustom_nodes = 0
                  call pc_warning ('CUSTOM_NODES were cleared')
                endif
              endif
              exit
            endif
          enddo
          if (.not. matches) then
            object%machine = trim(object%machine_menu(1))
            call pc_info ('Machine changed to '//trim(object%machine))
          else
            if (pc_get_update_state() .eq. PC_GUI) then
              call job_data_remove_blank_rows (object%custom_nodes,  &
                                               object%ncustom_nodes)
              if (trim(object%machine)    .eq. 'Custom' .and.  &
                  object%ncustom_nodes    .eq. 0        .and.  &
                  trim(object%custom_lam) .eq. 'NO') then
                call pc_warning  &
                  ('Remember to supply nodes on Custom Parameters screen.')
              endif
            endif
          endif
 
        case ('PDN_USERID')
          call string_to_lower (object%pdn_userid)
          if (object%pdn_userid .eq. ' ') then
             call pc_get_pdata ('USER_NAME', object%pdn_userid)
          endif
 
        case ('PATHNAME_TRCIO')
          call string_squeeze_blanks (object%pathname_trcio,i)
          if (i .eq. 0) then
             object%pathname_trcio ='~'//trim(object%frontend_user)//'/cpsdata/'
          endif
 
        case ('PATHNAME_DIR')
          call string_squeeze_blanks (object%pathname_dir,i)
          if (i .eq. 0) then
             object%pathname_dir = object%frontend_path
          endif

        case ('PRIORITY')
          if (object%priority.gt.5.or.object%priority.lt.-5)then
             call pc_error('Priority range is -5 to 5')
          endif
 
        case ('NUM_CPUS')
          if (object%num_cpus .lt. 0) then
            object%num_cpus = 0
            !call pc_info ('Automatic sensing of NUM_CPUS enabled')
          endif 
          if (pc_get_update_state() .eq. PC_GUI) then
          endif
          if (object%ncustom_nodes .gt. 0) then
            object%queue = 'C'
            do i=1,object%nprocess_list
              if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                  trim(object%process_list(i)) .eq. 'TTROT') then
                if (object%num_cpus .gt. 0) then
                  object%num_cpus  = 1
                  object%num_nodes = 1
                  call pc_warning  &
                        ('Number of processors must be 1 for TTRIN/TTROT jobs')
                  call pc_warning ('Number of processors was changed to 1')
                endif
                call pc_warning ('Running TTRIN/TTROT jobs on custom nodes '// &
                                 'requires special permission')
                exit
              endif
            enddo
          else   !  NOT custom nodes
            if (object%num_cpus.gt.1)then
              if(object%queue.eq.'A'.or.object%queue.eq.'S')then
                call pc_error('QUEUE cannot be A or S if NUM_CPUS over 1')
                call pc_error('QUEUE reset to a B queue')
                object%queue='B'
              endif
            else if (object%num_cpus .eq. 1) then
              object%num_nodes = 1
              do i=1,object%nprocess_list
                if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                    trim(object%process_list(i)) .eq. 'TTROT') then
                  object%queue = 'T'
                  exit
                endif
              enddo
            endif 
            do i=1,object%nprocess_list
              if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                  trim(object%process_list(i)) .eq. 'TTROT') then
                object%num_cpus  = 1
                object%num_nodes = 1
                object%queue     = 'T'
                call pc_warning  &
                     ('Number of processors must be 1 for TTRIN/TTROT jobs')
                call pc_warning ('Number of processors was changed to 1')
                exit
              endif
            enddo
          endif

        case ('NUM_NODES')
          if (object%num_nodes .lt. 0) then
            object%num_nodes = 1
            !#call pc_info ('
          endif 
          if (object%num_nodes .gt. object%num_cpus) then
            object%num_nodes = object%num_cpus
            !call pc_info ('Too many nodes.')
            !call pc_info  &
            !           ('Number of nodes changed to match number of processors')
          endif
          !if (object%num_nodes*12 .lt. object%num_cpus) then
          !  call pc_warning ('There will be more than 12 executables per node')
          !endif
          !if (object%num_nodes*12 .gt. object%num_cpus+1 .and.  &
          !    object%num_nodes   .ne. object%num_cpus) then
          !  ctemp1 = 'multiple'
            !call string_ii2cc (object%num_nodes*12-object%num_cpus,ctemp1)
            !call pc_warning  &
            !     ('There will be '//trim(ctemp1)//' nodes with one executable')
          !endif
          if (object%ncustom_nodes .gt. 0) then
            object%queue = 'C'
            matches = .false.
            do i=1,object%nprocess_list
              if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                  trim(object%process_list(i)) .eq. 'TTROT') then
                if (object%num_nodes .gt. 0) then
                  object%num_nodes = 1
                  call pc_warning  &
                        ('Number of nodes must be 1 for TTRIN/TTROT jobs')
                  call pc_warning ('Number of nodes was changed to 1')
                endif
                call pc_warning ('Running TTRIN/TTROT jobs on custom nodes '// &
                                 'requires special permission')
                matches = .true.
                exit
              endif
            enddo
            if (pc_get_update_state() .eq. PC_GUI) then
              if (.not. matches) then
                if (object%num_nodes .gt. object%ncustom_nodes) then
                  call pc_warning ('Need more nodes in custom node list')
                else if (object%num_nodes .lt. object%ncustom_nodes) then
                  call pc_warning ('Too many nodes in custom node list')
                endif
              endif
            endif
          else
            if (object%queue(1:1).eq.'B')then
              do i=1,object%nprocess_list
                if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                    trim(object%process_list(i)) .eq. 'TTROT') then
                  object%num_nodes = 1
                  object%queue    = 'T'
                  exit
                endif
              enddo
            else if (object%num_nodes .eq. 1) then
              do i=1,object%nprocess_list
                if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                    trim(object%process_list(i)) .eq. 'TTROT') then
                  object%queue = 'T'
                  exit
                endif
              enddo
            else
              do i=1,object%nprocess_list
                if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                    trim(object%process_list(i)) .eq. 'TTROT') then
                  object%num_nodes = 1
                  object%queue    = 'T'
                  call pc_warning  &
                       ('Number of nodes must be 1 for TTRIN/TTROT jobs')
                  call pc_warning ('Number of nodes was changed to 1')
                  exit
                endif
              enddo
            endif
          endif

        case ('TSTAMP_INC')
          if (object%tstamp_inc .lt. 0) then
            object%tstamp_inc = 1000
            call pc_error ('Time Stamp Increment can not be less than 0')
          endif

        case ('STD_LIBS')
          call string_to_upper (object%std_libs)
          if (object%std_libs .eq. 'TESTLIB') object%std_libs = 'BETALIB'
          matches = .false.
          do i=1,object%nstd_libs_menu
            if (trim(object%std_libs) .eq. trim(object%std_libs_menu(i))) then
              matches = .true.
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_warning ('STD_LIBS changed from  '//  &
                              trim(object%std_libs)//    &
                             '  to  '//trim(object%std_libs_menu(1)))
            object%std_libs = trim(object%std_libs_menu(1))
          else
            if (pc_get_update_state() .eq. PC_GUI) then
              call job_data_remove_blank_rows (object%custom_compile,  &
                                               object%ncustom_compile)
              call job_data_remove_blank_rows (object%custom_modules,  &
                                               object%ncustom_modules)
              call job_data_remove_blank_rows (object%custom_link,     &
                                               object%ncustom_link)
              i = max(object%ncustom_compile,object%ncustom_modules,   &
                      object%ncustom_link)
              if (trim(object%std_libs).eq.'CUSTOM' .and. i.eq.0) then
                call pc_warning  &
                  ('Remember to supply parameters on Custom Parameters screen.')
              endif
            endif
          endif
 
        case ('REC_KEEPING')
          ctemp1 = object%rec_keeping
          call string_to_upper       (ctemp1)
          call string_squeeze_blanks (ctemp1)
          matches = .false.
          do i=1,object%nrec_keeping_menu
            ctemp2 = object%rec_keeping_menu(i)
            call string_to_upper       (ctemp2)
            call string_squeeze_blanks (ctemp2)
            if (trim(ctemp1) .eq. trim(ctemp2)) then
              matches = .true.
              object%rec_keeping = object%rec_keeping_menu(i)
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_error ('Invalid REC_KEEPING '//trim(object%rec_keeping))
            object%rec_keeping = trim(object%rec_keeping_menu(1))
          endif

        case ('PCPS_REPORT_OPT')
          ctemp1 = object%pcps_report_opt
          call string_to_upper       (ctemp1)
          call string_squeeze_blanks (ctemp1)
          matches = .false.
          do i=1,object%npcps_report_opt_menu
            ctemp2 = object%pcps_report_opt_menu(i)
            call string_to_upper       (ctemp2)
            call string_squeeze_blanks (ctemp2)
            if (trim(ctemp1) .eq. trim(ctemp2)) then
              matches = .true.
              object%pcps_report_opt = object%pcps_report_opt_menu(i)
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_error  &
                     ('Invalid PCPS_REPORT_OPT '//trim(object%pcps_report_opt))
            object%pcps_report_opt = trim(object%pcps_report_opt_menu(1))
          endif

        case ('TRSCAN_OPT')
          call string_to_upper (object%trscan_opt)
          matches = .false.
          do i=1,object%ntrscan_menu
            if (trim(object%trscan_opt) .eq. trim(object%trscan_menu(i))) then
              matches = .true.
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_error ('Invalid TRSCAN_OPT '//trim(object%trscan_opt))
            object%trscan_opt = trim(object%trscan_menu(1))
          endif

        case ('DEBUG_LEVEL')
          ctemp1 = object%debug_level
          call string_to_upper       (ctemp1)
          call string_squeeze_blanks (ctemp1)
          matches = .false.
          do i=1,object%ndebug_level_menu
            ctemp2 = object%debug_level_menu(i)
            call string_to_upper       (ctemp2)
            call string_squeeze_blanks (ctemp2)
            if (trim(ctemp1) .eq. trim(ctemp2)) then
              matches = .true.
              object%debug_level = object%debug_level_menu(i)
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_error ('Invalid DEBUG_LEVEL '//trim(object%debug_level))
            object%debug_level = trim(object%debug_level_menu(1))
          endif

        case('RLOCATION')
          if(object%rlocation.ne.'LOCAL'.and.object%rlocation.ne.'ALASKA'&
             .and.object%rlocation.ne.'PONY')then
            call pc_error('Invalid RLOCATION ',object%rlocation)
            call pc_error('RLOCATION reset to LOCAL')
            object%rlocation='LOCAL'
          endif

        case ('TRACE_LENGTH')
          if (object%dt .ne. 0.0) then
             object%ndpt = nint(object%trace_length/object%dt) + 1
          endif
 
        case ('DT')
          if (object%dt .ne. 0.0) then
             object%ndpt = nint(object%trace_length/object%dt) + 1
          endif
 
        case ('NDPT')
          object%trace_length = (object%ndpt-1)*object%dt
 
        case ('NWIH')
          if (object%nwih .lt. 64) then
            object%nwih = 64
            call pc_error ('NWIH must be at least 64')
          endif

        case ('CUSTOM_COMPILE')
          call job_data_remove_blank_rows (object%custom_compile,  &
                                           object%ncustom_compile)
          if (pc_get_update_state() .eq. PC_GUI) then
            if (trim(object%std_libs).ne.'CUSTOM' .and.  &
                                               object%ncustom_compile.gt.0) then
              object%std_libs = 'CUSTOM'
              call pc_warning ('Standard Libraries was changed to CUSTOM')
            else
              i = max(object%ncustom_compile,object%ncustom_modules,   &
                      object%ncustom_link)
              if (trim(object%std_libs).eq.'CUSTOM' .and. i.eq.0) then
                object%std_libs = trim(object%std_libs_menu(1))
                call pc_warning ('Standard Libraries was changed to '//  &
                                  trim(object%std_libs))
              endif
            endif
          endif

        case ('CUSTOM_MODULES')
          call job_data_remove_blank_rows (object%custom_modules,  &
                                           object%ncustom_modules)
          if (pc_get_update_state() .eq. PC_GUI) then
            if (trim(object%std_libs).ne.'CUSTOM' .and.  &
                                               object%ncustom_modules.gt.0) then
              object%std_libs = 'CUSTOM'
              call pc_warning ('Standard Libraries was changed to CUSTOM')
            else
              i = max(object%ncustom_compile,object%ncustom_modules,   &
                      object%ncustom_link)
              if (trim(object%std_libs).eq.'CUSTOM' .and. i.eq.0) then
                object%std_libs = trim(object%std_libs_menu(1))
                call pc_warning ('Standard Libraries was changed to '//  &
                                  trim(object%std_libs))
              endif
            endif
          endif

        case ('CUSTOM_LINK')
          call job_data_remove_blank_rows (object%custom_link,  &
                                           object%ncustom_link)
          if (pc_get_update_state() .eq. PC_GUI) then
            if (trim(object%std_libs).ne.'CUSTOM' .and.  &
                                                  object%ncustom_link.gt.0) then
              object%std_libs = 'CUSTOM'
              call pc_warning ('Standard Libraries was changed to CUSTOM')
            else
              i = max(object%ncustom_compile,object%ncustom_modules,   &
                      object%ncustom_link)
              if (trim(object%std_libs).eq.'CUSTOM' .and. i.eq.0) then
                object%std_libs = trim(object%std_libs_menu(1))
                call pc_warning ('Standard Libraries was changed to '//  &
                                  trim(object%std_libs))
              endif
            endif
          endif

        case ('CUSTOM_LAM')
          call string_to_upper (object%custom_lam)
          matches = .false.
          do i=1,object%nyesno_menu
            if (trim(object%custom_lam) .eq. trim(object%yesno_menu(i))) then
              matches = .true.
              exit
            endif
          enddo
          if (.not. matches) then
            call pc_error ('Invalid CUSTOM_LAM '//trim(object%custom_lam))
            object%custom_lam = trim(object%yesno_menu(1))
          else
            if (pc_get_update_state() .eq. PC_GUI) then
              if (trim(object%custom_lam) .eq. 'YES' .and.  &
                  trim(object%machine)    .ne. 'Custom') then
                object%machine = 'Custom'
                call pc_warning ('Machine was changed to Custom')
              endif
              if (object%ncustom_nodes .gt. 0) then
                object%ncustom_nodes = 0
                call pc_warning ('CUSTOM_NODES were cleared')
              else
                if (trim(object%custom_lam) .eq. 'NO') then
                  object%machine = object%machine_menu(1)
                  call pc_warning ('Machine changed to '//trim(object%machine))
                endif
              endif
            endif
          endif

        case ('CUSTOM_NODES_PATH')
          if (pc_get_update_state() .eq. PC_GUI) then
            call pathcheck ('CUSTOM_NODES_PATH',object%custom_nodes_path,'.lst')
            if (trim(object%custom_nodes_path) .eq. PATHCHECK_EMPTY .or.  &
                len_trim(object%custom_nodes_path) .eq. 0) then
              object%custom_nodes_path = ' '
              return
            endif
            lun  = cio_fopen(trim(object%custom_nodes_path),'r')
            if (lun .le. 0) then
              call pc_error  &
                         ('Error opening file '//trim(object%custom_nodes_path))
              return
            endif
            if (object%ncustom_nodes .gt. 0) then
              if (associated(object%custom_nodes))  &
                                                 deallocate(object%custom_nodes)
              allocate(object%custom_nodes(1))
              object%ncustom_nodes = 0
            endif
            do
              istat = cio_fgetline(ctemp1,PC_LENGTH,lun)
              if (istat .lt. 0) exit
                call job_data_append_array_element (object%custom_nodes,  &
                                                    object%ncustom_nodes, &
                                                    ctemp1)
            enddo
            istat = cio_fclose(lun)
            object%custom_nodes_path = ' '
            if (object%ncustom_nodes .gt. 0) then
              if (trim(object%machine) .ne. 'Custom') then
                object%machine = 'Custom'
                call pc_warning ('Machine was changed to Custom')
              endif
              if (trim(object%custom_lam) .eq. 'YES') then
                object%custom_lam = 'NO'
                call pc_warning ('CUSTOM_LAM was changed to NO')
              endif
            else
              if (trim(object%custom_lam) .eq. 'NO' .and.  &
                  trim(object%machine)    .eq. 'Custom') then
                object%machine = object%machine_menu(1)
                call pc_warning ('Machine changed to '//trim(object%machine))
              endif
            endif
          endif

        case ('CUSTOM_NODES')
          call job_data_remove_blank_rows (object%custom_nodes,  &
                                           object%ncustom_nodes)
          if (pc_get_update_state() .eq. PC_GUI) then
            if (object%ncustom_nodes .gt. 0) then
              if (trim(object%machine) .ne. 'Custom') then
                object%machine = 'Custom'
                call pc_warning ('Machine was changed to Custom')
              endif
              if (trim(object%custom_lam) .eq. 'YES') then
                object%custom_lam = 'NO'
                call pc_warning ('CUSTOM_LAM was changed to NO')
              endif
              if (pc_get_update_state() .eq. PC_GUI) then
                if (object%num_nodes .gt. object%ncustom_nodes) then
                  call pc_warning ('Need more nodes in custom node list')
                  call pc_warning ('Does not match "Number of nodes" '//&
                                   'parameter on first screen')
                else if (object%num_nodes .lt. object%ncustom_nodes) then
                  call pc_warning ('Too many nodes in custom node list')
                  call pc_warning ('Does not match "Number of nodes"'//&
                                   'parameter on first screen')
                endif
              endif
            else
              if (trim(object%custom_lam) .eq. 'NO' .and.  &
                  trim(object%machine)    .eq. 'Custom') then
                object%machine = object%machine_menu(1)
                call pc_warning ('Machine changed to '//trim(object%machine))
              endif
            endif
          endif

        case default
 
      end select
 
      return
      end subroutine job_data_trap


      subroutine job_data_etrap (keyword,indx,action)
      implicit none
      character(len=*),intent(in)  ::  keyword   ! argument
      integer         ,intent(in)  ::  indx      ! argument
      integer         ,intent(in)  ::  action    ! argument

      select case (keyword)

        case ('CUSTOM_COMPILE')
          if (pc_get_update_state() .eq. PC_GUI) then
            if (trim(object%std_libs).ne.'CUSTOM' .and.  &
                                               object%ncustom_compile.gt.0) then
              object%std_libs = 'CUSTOM'
              call pc_warning ('Standard Libraries was changed to CUSTOM')
            endif
          endif

        case ('CUSTOM_MODULES')
          if (pc_get_update_state() .eq. PC_GUI) then
            if (trim(object%std_libs).ne.'CUSTOM' .and.  &
                                               object%ncustom_modules.gt.0) then
              object%std_libs = 'CUSTOM'
              call pc_warning ('Standard Libraries was changed to CUSTOM')
            endif
          endif

        case ('CUSTOM_LINK')
          if (pc_get_update_state() .eq. PC_GUI) then
            if (trim(object%std_libs).ne.'CUSTOM' .and.  &
                                                  object%ncustom_link.gt.0) then
              object%std_libs = 'CUSTOM'
              call pc_warning ('Standard Libraries was changed to CUSTOM')
            endif
          endif

        case ('CUSTOM_NODES')
          if (pc_get_update_state() .eq. PC_GUI) then
            if (object%ncustom_nodes .gt. 0) then
              if (trim(object%machine) .ne. 'Custom') then
                object%machine = 'Custom'
                call pc_warning ('Machine was changed to Custom')
              endif
              if (trim(object%custom_lam) .eq. 'YES') then
                object%custom_lam = 'NO'
                call pc_warning ('CUSTOM_LAM was changed to NO')
              endif
            else
              if (trim(object%custom_lam) .eq. 'NO' .and.  &
                  trim(object%machine)    .eq. 'Custom') then
                object%machine = object%machine_menu(1)
                call pc_warning ('Machine changed to '//trim(object%machine))
              endif
            endif
          endif

        case default
 
      end select

      end subroutine job_data_etrap


      subroutine job_data_end_trap
      implicit none
 
      integer                                  :: i                    !local

      if (object%num_nodes .gt. object%num_cpus) then
        object%num_nodes = object%num_cpus
        call pc_info ('Too many nodes.')
        call pc_info ('Number of nodes changed to match number of processors')
      endif

      if (object%ncustom_nodes .gt. 0) then
        if (object%num_nodes .gt. object%ncustom_nodes) then
          call pc_error ('Need more nodes in custom node list')
          call pc_error ('Does not match "Number of nodes" parameter '//&
                         'on first screen')
        else if (object%num_nodes .lt. object%ncustom_nodes) then
          call pc_error ('Too many nodes in custom node list')
          call pc_error ('Does not match "Number of nodes" parameter '//&
                         'on first screen')
        endif
      endif

      if (object%ncustom_nodes.gt.0 .or. trim(object%custom_lam).eq.'YES') then
        object%queue = 'C'
        do i=1,object%nprocess_list
          if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
              trim(object%process_list(i)) .eq. 'TTROT') then
            if (object%num_cpus .gt. 0) then
              object%num_cpus  = 1
              object%num_nodes = 1
              call pc_warning  &
                    ('Number of processors must be 1 for TTRIN/TTROT jobs')
              call pc_warning ('Number of processors was changed to 1')
            endif
            !call pc_warning ('Running TTRIN/TTROT jobs on custom nodes '// &
            !                 'requires special permission')
            exit
          endif
        enddo
      else
        if (object%queue(1:1).eq.'B') then
          do i=1,object%nprocess_list
            if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                trim(object%process_list(i)) .eq. 'TTROT') then
              object%num_cpus  = 1
              object%num_nodes = 1
              object%queue     = 'T'
              exit
            endif
          enddo
        else if (object%num_cpus .eq. 1) then
          object%num_nodes = 1
          do i=1,object%nprocess_list
            if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                trim(object%process_list(i)) .eq. 'TTROT') then
              object%queue = 'T'
              exit
            endif
          enddo
        else
          do i=1,object%nprocess_list
            if (trim(object%process_list(i)) .eq. 'TTRIN' .or.  &
                trim(object%process_list(i)) .eq. 'TTROT') then
              object%num_cpus  = 1
              object%num_nodes = 1
              object%queue     = 'T'
              call pc_warning  &
                         ('Number of processors must be 1 for TTRIN/TTROT jobs')
              call pc_warning ('Number of processors was changed to 1')
              exit
            endif
          enddo
        endif
      endif

      call job_data_remove_blank_rows (object%custom_compile,  &
                                       object%ncustom_compile)
      call job_data_remove_blank_rows (object%custom_modules,  &
                                       object%ncustom_modules)
      call job_data_remove_blank_rows (object%custom_link,  &
                                       object%ncustom_link)
      call job_data_remove_blank_rows (object%custom_nodes,  &
                                       object%ncustom_nodes)

      i = max(object%ncustom_compile,object%ncustom_modules,object%ncustom_link)
      if (trim(object%std_libs).eq.'CUSTOM' .and. i.eq.0) then
        call pc_warning  &
           ('Standard Libraries=CUSTOM, but no custom parameters were supplied')
      else if (trim(object%std_libs).ne.'CUSTOM' .and. i.gt.0) then
        call pc_warning  &
           ('Standard Libraries=' // trim(object%std_libs) //  &
                                         ' but custom parameters were supplied')
      endif

      if (trim(object%machine) .ne. 'Custom') then
        if (trim(object%custom_lam) .eq. 'YES') then
          object%custom_lam = 'NO'
          call pc_warning ('CUSTOM_LAM changed to NO')
        endif
        if (object%ncustom_nodes .gt. 0) then
          object%ncustom_nodes = 0
          call pc_warning ('CUSTOM_NODES were cleared')
        endif
      else
        if (trim(object%custom_lam) .eq. 'YES') then
          if (object%ncustom_nodes .gt. 0) then
            object%ncustom_nodes = 0
            call pc_warning ('CUSTOM_NODES were cleared')
          endif
        else
          if (object%ncustom_nodes .eq. 0) then
            object%machine = trim(object%machine_menu(1))
            call pc_info ('Machine changed to '//trim(object%machine))
          endif
        endif
      endif

      return
      end subroutine job_data_end_trap


!!------------------------- remove_blank_rows ------------------------------!!
!!------------------------- remove_blank_rows ------------------------------!!
!!------------------------- remove_blank_rows ------------------------------!!


      subroutine job_data_remove_blank_rows (array,narray)

      character(len=*),pointer                 :: array(:)
      integer                                  :: narray

      integer                                  :: i                    !local
      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) return
      if (narray .eq. 0) return

      do i = 1, narray
        if (len_trim(array(i)) .ne. 0) cycle
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray-1))
        if (i .gt. 1) then
          array(1:i-1) = temp_array(1:i-1)
          array(i:narray-1) = temp_array(i+1:narray)
        else
          array(1:narray-1) = temp_array(i+1:narray)
        endif
        narray = narray - 1
        deallocate(temp_array)
      enddo

      return
      end subroutine job_data_remove_blank_rows


!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!


      subroutine job_data_append_array_element (array,narray,value)

      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (narray .le. 0) then
        if (associated(array)) deallocate(array)
        narray = 1
        allocate(array(narray))
        array(narray) = value
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+1))
        array(1:narray) = temp_array(1:narray)
        array(narray+1:narray+1) = value
        narray = narray + 1
        deallocate(temp_array)
      endif

      return
      end subroutine job_data_append_array_element


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine job_data_create (obj)
      implicit none
      type(job_data_struct),pointer :: obj       ! arguments

      allocate (obj)


      nullify(obj%custom_modules)
      nullify(obj%custom_compile)
      nullify(obj%custom_link)
      nullify(obj%custom_nodes)
      nullify(obj%process_list)
      nullify(obj%custom_nodes_dialog) ! jpa

      call pathchoose_create (obj%custom_nodes_dialog,'CUSTOM_NODES_PATH','lst')

      call job_data_initialize (obj)

      return
      end subroutine job_data_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine job_data_delete (obj)
      implicit none
      type(job_data_struct),pointer :: obj       ! arguments

      call job_data_wrapup (obj)

      if (associated(obj%custom_modules)) deallocate(obj%custom_modules)
      if (associated(obj%custom_compile)) deallocate(obj%custom_compile)
      if (associated(obj%custom_link)   ) deallocate(obj%custom_link)
      if (associated(obj%custom_nodes))   deallocate(obj%custom_nodes)
      if (associated(obj%process_list)  ) deallocate(obj%process_list)
      call pathchoose_delete (obj%custom_nodes_dialog)

      deallocate(obj)

      return
      end subroutine job_data_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine job_data_initialize (obj)
      implicit none
      type(job_data_struct),target :: obj        ! arguments

      integer                       :: i         ! local
      integer                       :: lnklib    ! local

      obj%jobname                 = ' '
      obj%job_comment             = ' '
      obj%sub_project             = ' '
      obj%queue                   = 'batch'
      obj%time_limit              = 3600
      obj%mail_opt                = 'NO'
      obj%history_opt             = 'MODEL'
      obj%rerun                   = 'NO'
      call getsys_username (obj%pdn_userid)
      thisuserid                  = obj%pdn_userid
      obj%num_cpus                = 0
      obj%num_nodes               = 1
      obj%priority                = 0
      if (.not. pc_process_keyword_present('NUM_NODES')) then
        call pc_get('NUM_CPUS',i)
        !obj%num_nodes = i / 2
        !if (2*obj%num_nodes < i) obj%num_nodes = obj%num_nodes + 1
      endif
      obj%tstamp_inc              = 1000
      call cnfg_get_value('location',obj%location)
!!!      call getsys_env('HOST',obj%location)
      lnklib = getsys_library()
      if (lnklib .eq. GETSYS_PRODLIB) then
        obj%std_libs              = 'PRODLIB'
      else if (lnklib .eq. GETSYS_BETALIB) then
        obj%std_libs              = 'BETALIB'
      else if (lnklib .eq. GETSYS_ALPHALIB) then
        obj%std_libs              = 'ALPHALIB'
      else
        obj%std_libs              = 'CUSTOM'
      endif
      obj%rec_keeping             = 'No    Job Summary File'
      obj%pcps_report_opt         = 'Do not include worker printout'
      obj%trscan_opt              = 'BASIC'
      obj%calling_program         = 'CPS'
      obj%mailingaddress          = ''
      obj%debug_level             = 'NO DEBUG'
      obj%rlocation               = 'LOCAL'
      obj%trace_length            = 0.0
      obj%tstrt                   = 0.0
      obj%dt                      = 0.0
      obj%ndpt                    = 0
      obj%nwih                    = 64

      obj%ncustom_modules         = 0
      obj%ncustom_compile         = 0
      obj%ncustom_link            = 0
      obj%custom_exec_b           = ' '
      obj%custom_exec_a           = ' '
      obj%custom_lam              = 'NO'
      obj%ncustom_nodes           = 0
      obj%custom_nodes_path       = ' '
      obj%nprocess_list           = 0

      obj%nmail_menu              = 4
      obj%mail_menu(1)            = 'NO'
      obj%mail_menu(2)            = 'YES'
      obj%mail_menu(3)            = 'SE'
      obj%mail_menu(4)            = 'ABT'
      obj%nhistory_menu           = 4
      obj%history_menu(1)         = 'MODEL'
      obj%history_menu(2)         = 'ALL'
      obj%history_menu(3)         = 'NONE'
      obj%history_menu(4)         = 'CURRENT'
      if (lnklib .eq. GETSYS_PRODLIB) then
        obj%nstd_libs_menu        = 2
        obj%std_libs_menu(1)      = 'PRODLIB'
        obj%std_libs_menu(2)      = 'CUSTOM'
      else if (lnklib .eq. GETSYS_BETALIB) then
        obj%nstd_libs_menu        = 2
        obj%std_libs_menu(1)      = 'BETALIB'
        obj%std_libs_menu(2)      = 'CUSTOM'
      else if (lnklib .eq. GETSYS_ALPHALIB) then
        obj%nstd_libs_menu        = 2
        obj%std_libs_menu(1)      = 'ALPHALIB'
        obj%std_libs_menu(2)      = 'CUSTOM'
      else
        obj%nstd_libs_menu        = 3
        obj%std_libs_menu(1)      = 'PRODLIB'
        obj%std_libs_menu(2)      = 'BETALIB'
        obj%std_libs_menu(3)      = 'CUSTOM'
      endif
      obj%nrec_keeping_menu       = 2
      obj%rec_keeping_menu(1)     = 'YES   Job Summary File'
      obj%rec_keeping_menu(2)     = 'NO    Job Summary File'
      obj%npcps_report_opt_menu   = 2
      obj%pcps_report_opt_menu(1) = 'Do not include worker printout'
      obj%pcps_report_opt_menu(2) = 'Include worker printout'
      obj%ntrscan_menu            = 3
      obj%trscan_menu(1)          = 'BASIC'
      obj%trscan_menu(2)          = 'ADVANCED'
      obj%trscan_menu(3)          = 'CHECKLAV'
      obj%ndebug_level_menu       = 4
      obj%debug_level_menu(1)     = 'NO DEBUG'
      obj%debug_level_menu(2)     = 'LEVEL 1'
      obj%debug_level_menu(3)     = 'LEVEL 2'
      obj%debug_level_menu(4)     = 'LEVEL 3'
      obj%nrlocation_menu         = 3
      obj%rlocation_menu(1)       = 'LOCAL'
      obj%rlocation_menu(2)       = 'YourLocation'
      obj%rlocation_menu(3)       = 'ref: job_data.f90'
      obj%nyesno_menu             = 2
      obj%yesno_menu(1)           = 'YES'
      obj%yesno_menu(2)           = 'NO'
      obj%nnoyes_menu             = 2
      obj%noyes_menu(1)           = 'NO'
      obj%noyes_menu(2)           = 'YES'
      obj%n_calling_program_menu  = 2
      obj%calling_program_menu(1) = 'CPS'
      obj%calling_program_menu(2) = 'SEISSPACE'

      if (pc_get_update_state() .eq. PC_BACKEND) then
        call getsys_netinfo     (obj%frontend_node,  &
                                 obj%frontend_user,  &
                                 obj%frontend_path)
      else
        call getsys_hostname        (obj%frontend_node)
        call getsys_username        (obj%frontend_user)
        call getsys_current_dir     (obj%frontend_path)
        call pc_get ('FRONTEND_PATH',obj%frontend_path)
      endif

      obj%machine                 = 'Linux'
      obj%nmachine_menu           = 2
      obj%machine_menu(1)         = 'Linux'
      obj%machine_menu(2)         = 'Custom'
      !obj%machine_menu(3)         = 'Solaris'


      obj%nqueue_menu             = 4
      obj%queue_menu(1)          = 'batch'
      obj%queue_menu(2)          = 'B_multi'
      obj%queue_menu(3)          = 'B_single'
      obj%queue_menu(4)          = 'test'
      
      obj%pathname_trcio          = '~'//trim(obj%frontend_user)//'/cpsdata/'
      obj%pathname_dir            = obj%frontend_path

      call job_data_update (obj)

      return
      end subroutine job_data_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine job_data_update (obj)
      implicit none
      type(job_data_struct),target   :: obj                         ! arguments

      character(len=FILENAME_LENGTH) :: ctemp                       ! local
      character(len=1024)            :: cmds(2)                     ! local
      character(len=32)              :: ctime
      integer                        :: i                           ! local
      integer                        :: nscratch                    ! local
      integer                        :: nstore                      ! local
 

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!


      ctemp = obj%custom_nodes_path
      if (pathchoose_update(obj%custom_nodes_dialog,obj%custom_nodes_path))  &
                                                                          return
      if (trim(ctemp) .ne. trim(obj%custom_nodes_path)) call job_data_trap   &
                                                           ('CUSTOM_NODES_PATH')

      call pc_get ('JOBNAME'           ,obj%jobname        )
      call pc_get ('JOB_COMMENT'       ,obj%job_comment    )
      call pc_get ('SUB_PROJECT'       ,obj%sub_project    , job_data_trap)
      call pc_get ('QUEUE'             ,obj%queue          )
      call pc_get ('TIME_LIMIT'        ,obj%time_limit     )
      call pc_get ('MAIL_OPT'          ,obj%mail_opt       , job_data_trap)
      call pc_get ('HISTORY_OPT'       ,obj%history_opt    , job_data_trap)
      call pc_get ('PDN_USERID'        ,obj%pdn_userid     )
      call pc_get ('MACHINE'           ,obj%machine        , job_data_trap)
      call pc_get ('RERUN'             ,obj%rerun          )      
      call pc_get ('PATHNAME_TRCIO'    ,obj%pathname_trcio , job_data_trap)
      call pc_get ('PATHNAME_DIR'      ,obj%pathname_dir   , job_data_trap)
      call pc_get ('NUM_CPUS'          ,obj%num_cpus       , job_data_trap)
      call pc_get ('NUM_NODES'         ,obj%num_nodes      , job_data_trap)
      call pc_get ('PRIORITY'          ,obj%priority       , job_data_trap)
      call pc_get ('TSTAMP_INC'        ,obj%tstamp_inc     , job_data_trap)
      call pc_get ('STD_LIBS'          ,obj%std_libs       , job_data_trap)
      call pc_get ('REC_KEEPING'       ,obj%rec_keeping    , job_data_trap)
      call pc_get ('PCPS_REPORT_OPT'   ,obj%pcps_report_opt, job_data_trap)
      call pc_get ('TRSCAN_OPT'        ,obj%trscan_opt     , job_data_trap)
      call pc_get ('CALLING_PROGRAM'   ,obj%calling_program, job_data_trap)
      call pc_get ('MAILINGADDRESS'    ,obj%mailingaddress , job_data_trap)
      call pc_get ('DEBUG_LEVEL'       ,obj%debug_level    , job_data_trap)
      call pc_get ('RLOCATION'         ,obj%rlocation      , job_data_trap)
      call pc_get ('TRACE_LENGTH'      ,obj%trace_length   , job_data_trap)
      call pc_get ('TSTRT'             ,obj%tstrt          )
      call pc_get ('DT'                ,obj%dt             , job_data_trap)
      call pc_get ('NDPT'              ,obj%ndpt           , job_data_trap)
      call pc_get ('NWIH'              ,obj%nwih           , job_data_trap)
      call pc_get ('CUSTOM_EXEC_B'     ,obj%custom_exec_b  , job_data_trap)
      call pc_get ('CUSTOM_EXEC_A'     ,obj%custom_exec_a  , job_data_trap)
      call pc_get ('CUSTOM_LAM'        ,obj%custom_lam     , job_data_trap)
      call pc_get ('CUSTOM_NODES_PATH' ,obj%custom_nodes_path  &
                                                           , job_data_trap)

      call pc_alloc ('CUSTOM_MODULES'  ,obj%custom_modules ,  &
                                        obj%ncustom_modules, job_data_etrap)
      call pc_alloc ('CUSTOM_COMPILE'  ,obj%custom_compile ,  &
                                        obj%ncustom_compile, job_data_etrap)
      call pc_alloc ('CUSTOM_LINK'     ,obj%custom_link    ,  &
                                        obj%ncustom_link   , job_data_etrap)
      call pc_alloc ('CUSTOM_NODES'    ,obj%custom_nodes   ,  &
                                        obj%ncustom_nodes  , job_data_etrap)
      call pc_alloc ('PROCESS_LIST'    ,obj%process_list   , obj%nprocess_list)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      nscratch = 0
      nstore   = 0

      cmds(1)  = 'cat '//trim(obj%sub_project)//'.folder >> '//   &
                  trim(obj%frontend_path)//trim(obj%sub_project)//'.folder'
      cmds(2)  = 'chmod -f g+rw '//trim(obj%frontend_path)//  &
                  trim(obj%sub_project)//'.folder'

      call pc_call_array_trap ('CUSTOM_MODULES', job_data_trap)
      call pc_call_array_trap ('CUSTOM_COMPILE', job_data_trap)
      call pc_call_array_trap ('CUSTOM_LINK'   , job_data_trap)
      call pc_call_array_trap ('CUSTOM_NODES'  , job_data_trap)

      call pc_call_end_trap (job_data_end_trap)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      if (object%num_cpus  .lt. 1) object%num_cpus  = 1
      if (object%num_nodes .lt. 1) object%num_nodes = 1
      if (object%num_cpus  .eq. 1) object%num_nodes = 1

      if (obj%ncustom_nodes.gt.0 .or. trim(obj%custom_lam).eq.'YES') then
        obj%queue         = 'C'
        obj%nqueue_menu   = 1
        obj%queue_menu(1) = 'C'
      else
        if (object%num_cpus .eq. 1) then  !  All queues available
          obj%nqueue_menu             = 4
          obj%queue_menu(1)          = 'batch'
          obj%queue_menu(2)          = 'B_multi'
          obj%queue_menu(3)          = 'B_single'
          obj%queue_menu(4)          = 'test'
          do i=1,obj%nprocess_list
            if (trim(obj%process_list(i)) .eq. 'TTRIN' .or.  &
                trim(obj%process_list(i)) .eq. 'TTROT') then
              obj%queue         = 'T'
              obj%nqueue_menu   = 1
              obj%queue_menu(1) = 'T'
              exit
            endif
          enddo
        else   ! num cpus gt 1 - A and S queues not allowed.
          obj%nqueue_menu             = 4
          obj%queue_menu(1)          = 'batch'
          obj%queue_menu(2)          = 'B_multi'
          obj%queue_menu(3)          = 'B_single'
          obj%queue_menu(4)          = 'test'
        endif
      endif

      i=pc_get_update_state()
      if(i.ne.PC_BACKEND)call job_data_queue_trap('queue')


      call pc_put_options_field ('QUEUE'          ,obj%queue_menu          ,  &
                                                   obj%nqueue_menu)
      call pc_put_options_field ('MAIL_OPT'       ,obj%mail_menu           ,  &
                                                   obj%nmail_menu)
      call pc_put_options_field ('HISTORY_OPT'    ,obj%history_menu        ,  &
                                                   obj%nhistory_menu)
      call pc_put_options_field ('MACHINE'        ,obj%machine_menu        ,  &
                                                   obj%nmachine_menu)
      call pc_put_options_field ('RERUN'          ,obj%noyes_menu          ,  &
                                                   obj%nnoyes_menu)
      call pc_put_options_field ('STD_LIBS'       ,obj%std_libs_menu       ,  &
                                                   obj%nstd_libs_menu)
      call pc_put_options_field ('TRSCAN_OPT'     ,obj%trscan_menu         ,  &
                                                   obj%ntrscan_menu)
      call pc_put_options_field ('CALLING_PROGRAM',obj%calling_program_menu,  &
                                                   obj%n_calling_program_menu)
      call pc_put_options_field ('REC_KEEPING'    ,obj%rec_keeping_menu    ,  &
                                                   obj%nrec_keeping_menu)
      call pc_put_options_field ('PCPS_REPORT_OPT',obj%pcps_report_opt_menu,  &
                                                   obj%npcps_report_opt_menu)
      call pc_put_options_field ('DEBUG_LEVEL'    ,obj%debug_level_menu    ,  &
                                                   obj%ndebug_level_menu)
      call pc_put_options_field ('RLOCATION  '    ,obj%rlocation_menu    ,  &
                                                   obj%nrlocation_menu)
      call pc_put_options_field ('CUSTOM_LAM'     ,obj%yesno_menu          ,  &
                                                   obj%nyesno_menu)

      call pc_put ('JOBNAME'           ,obj%jobname        )
      call pc_put ('JOB_COMMENT'       ,obj%job_comment    )
      call pc_put ('SUB_PROJECT'       ,obj%sub_project    )
      call pc_put ('QUEUE'             ,obj%queue          )
      call pc_put ('TIME_LIMIT'        ,obj%time_limit     )
      call pc_put ('MAIL_OPT'          ,obj%mail_opt       )
      call pc_put ('HISTORY_OPT'       ,obj%history_opt    )
      call pc_put ('PDN_USERID'        ,obj%pdn_userid     )
      call pc_put ('MACHINE'           ,obj%machine        )
      call pc_put ('RERUN'             ,obj%rerun          )
      call pc_put ('PATHNAME_TRCIO'    ,obj%pathname_trcio )
      call pc_put ('PATHNAME_DIR'      ,obj%pathname_dir   )
      call pc_put ('NUM_CPUS'          ,obj%num_cpus       )
      call pc_put ('NUM_NODES'         ,obj%num_nodes      )
      call pc_put ('PRIORITY'          ,obj%priority       )
      call pc_put ('TSTAMP_INC'        ,obj%tstamp_inc     )
      call pc_put ('STD_LIBS'          ,obj%std_libs       )
      call pc_put ('REC_KEEPING'       ,obj%rec_keeping    )
      call pc_put ('PCPS_REPORT_OPT'   ,obj%pcps_report_opt)
      call pc_put ('TRSCAN_OPT'        ,obj%trscan_opt     )
      call pc_put ('CALLING_PROGRAM'   ,obj%calling_program)
      call pc_put ('MAILINGADDRESS'    ,obj%mailingaddress )
      call pc_put ('DEBUG_LEVEL'       ,obj%debug_level    )
      call pc_put ('RLOCATION'         ,obj%rlocation      )
!           use character to avoid floating arithmetic problems
      write(ctime,*)obj%trace_length
      call string_cc2ff(ctime,obj%trace_length)
      call pc_put ('TRACE_LENGTH'      ,obj%trace_length   )
      call pc_put ('TSTRT'             ,obj%tstrt          )
      call pc_put ('DT'                ,obj%dt             )
      call pc_put ('NDPT'              ,obj%ndpt           )
      call pc_put ('NWIH'              ,obj%nwih           )
      call pc_put ('CUSTOM_EXEC_B'     ,obj%custom_exec_b  )
      call pc_put ('CUSTOM_EXEC_A'     ,obj%custom_exec_a  )
      call pc_put ('CUSTOM_LAM'        ,obj%custom_lam     )

      call pc_put ('CUSTOM_MODULES'    ,obj%custom_modules ,obj%ncustom_modules)
      call pc_put ('CUSTOM_COMPILE'    ,obj%custom_compile ,obj%ncustom_compile)
      call pc_put ('CUSTOM_LINK'       ,obj%custom_link    ,obj%ncustom_link   )
      call pc_put ('CUSTOM_NODES'      ,obj%custom_nodes   ,obj%ncustom_nodes  )

      call pc_put_gui_only ('CUSTOM_NODES_PATH' ,obj%custom_nodes_path)

      call pc_put_process ('PROCESS_LIST' ,obj%process_list,obj%nprocess_list)

      call pc_put_jdata ('JOBNAME'        ,obj%jobname        )
      call pc_put_jdata ('JOB_COMMENT'    ,obj%job_comment    )
      call pc_put_jdata ('SUB_PROJECT'    ,obj%sub_project    )
      call pc_put_jdata ('QUEUE'          ,obj%queue          )
      call pc_put_jdata ('TIME_LIMIT'     ,obj%time_limit     )
      call pc_put_jdata ('MAIL_OPT'       ,obj%mail_opt       )
      call pc_put_jdata ('HISTORY_OPT'    ,obj%history_opt    )
      call pc_put_jdata ('PDN_USERID'     ,obj%pdn_userid     )
      call pc_put_jdata ('MACHINE'        ,obj%machine        )
      call pc_put_jdata ('RERUN'          ,obj%rerun          )
      call pc_put_jdata ('PATHNAME_TRCIO' ,obj%pathname_trcio )
      call pc_put_jdata ('PATHNAME_DIR'   ,obj%pathname_dir   )
      call pc_put_jdata ('NUM_CPUS'       ,obj%num_cpus       )
      call pc_put_jdata ('NUM_NODES'      ,obj%num_nodes      )
      call pc_put_jdata ('PRIORITY'       ,obj%priority       )
      call pc_put_jdata ('TSTAMP_INC'     ,obj%tstamp_inc     )
      call pc_put_jdata ('STD_LIBS'       ,obj%std_libs       )
      call pc_put_jdata ('REC_KEEPING'    ,obj%rec_keeping    )
      call pc_put_jdata ('PCPS_REPORT_OPT',obj%pcps_report_opt)
      call pc_put_jdata ('TRSCAN_OPT'     ,obj%trscan_opt     )
      call pc_put_jdata ('CALLING_PROGRAM',obj%calling_program)
      call pc_put_jdata ('MAILINGADDRESS' ,obj%mailingaddress )
      select case (trim(obj%debug_level))
        case ('LEVEL 1')
          call pc_put_jdata ('DEBUG_LEVEL',1)
        case ('LEVEL 2')
          call pc_put_jdata ('DEBUG_LEVEL',2)
        case ('LEVEL 3')
          call pc_put_jdata ('DEBUG_LEVEL',3)
        case default
          call pc_put_jdata ('DEBUG_LEVEL',0)
      end select
      call pc_put_jdata ('RLOCATION'      ,obj%rlocation      )
      call pc_put_jdata ('TRACE_LENGTH'   ,obj%trace_length   )
      call pc_put_jdata ('TSTRT'          ,obj%tstrt          )
      call pc_put_jdata ('DT'             ,obj%dt             )
      call pc_put_jdata ('NDPT'           ,obj%ndpt           )
      call pc_put_jdata ('NWIH'           ,obj%nwih           )
      call pc_put_jdata ('CUSTOM_EXEC_B'  ,obj%custom_exec_b  )
      call pc_put_jdata ('CUSTOM_EXEC_A'  ,obj%custom_exec_a  )
      call pc_put_jdata ('CUSTOM_LAM'     ,obj%custom_lam     )

      call pc_put_jdata('CUSTOM_MODULES',obj%custom_modules,obj%ncustom_modules)
      call pc_put_jdata('CUSTOM_COMPILE',obj%custom_compile,obj%ncustom_compile)
      call pc_put_jdata('CUSTOM_LINK'   ,obj%custom_link   ,obj%ncustom_link   )
      call pc_put_jdata('CUSTOM_NODES'  ,obj%custom_nodes  ,obj%ncustom_nodes  )
      call pc_put_jdata('PROCESS_LIST ' ,obj%process_list  ,obj%nprocess_list  )
 
      call pc_put_jdata ('FRONTEND_NODE'  ,obj%frontend_node )
      call pc_put_jdata ('FRONTEND_USER'  ,obj%frontend_user )
      call pc_put_jdata ('FRONTEND_PATH'  ,obj%frontend_path )
 
      call pc_put_global ('TSTRT'         ,obj%tstrt)
      call pc_put_global ('DT'            ,obj%dt   )
      call pc_put_global ('NDPT'          ,obj%ndpt )
      call pc_put_global ('NWIH'          ,obj%nwih )
      call pc_put_global ('NUM_CPUS'      ,obj%num_cpus )
 
      call pc_put_control ('NSCRATCH'     ,nscratch )
      call pc_put_control ('NSTORE'       ,nstore   )
      call pc_put_control ('SETUP_ONLY'   ,.true.   )

      if (trim(obj%rec_keeping) .eq. trim(obj%rec_keeping_menu(1))) then
        call pc_put_control ('CMD_AFTER_BSCRIPT',cmds,2 )
      endif


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine job_data_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

      subroutine job_data_queue_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: i
!           There may be some jobs around with queue names that are no
!             longer valid
      DO i=1,object%nqueue_menu
        if(object%queue.eq.object%queue_menu(i))go to 50
      ENDDO
      call pc_error('Invalid queue name ',object%queue)
!            Reset if Pony- error message is a nuisance
      if(trim(object%location).eq.'pony'.or.trim(object%rlocation).eq.'PONY') &
             object%queue=object%queue_menu(1)
 50   continue
!
      end subroutine job_data_queue_trap


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine job_data_wrapup (obj)
      implicit none
      type(job_data_struct),pointer        :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine job_data_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module job_data_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

