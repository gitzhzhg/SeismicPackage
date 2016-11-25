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

!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S  P R O G R A M   F I L E
!
! Name       : buildjob
! Category   : stand-alone
! Written    : 1999-07-08   by: Donna K. Vunderink
! Revised    : 2009-06-13   by: Bill Menger
! Maturity   : beta
! Purpose    : Subroutine that does the work of building the jobfile
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!171. 2009-06-13  Bill Menger  Modified for gfortran64 cpseis open source.
!170. 2007-05-31  Goodger      Replace sort with awk script catuniq.awk.  
!                              Due to the way a blank gets sorted, sometimes
!                              the mom node is not the first in the list
!                              with the sort command.
!169. 2007-05-03  Goodger      Add TODI92.
!168. 2006-09-06  Goodger      Set number of cpus back to one if there are
!                              loops with no parallel processes in job, and
!                              print message after process list.
!167. 2005-10-26  Goodger      Added LD_ASSUME_KERNEL=2.4.1.  This gets rid
!                              of the 'incorrectly built binary' message.
!166. 2005-08-23  Goodger      Fix module flag on linuxi platforms.  
!165. 2005-08-09  Goodger      Add platforms linuxab90, linuxab90_debug,
!                              linuxab90_xeon.
!164. 2005-08-03  Goodger      Remove space after the -I for the module 
!                              for the 64linuxp platform.  This is a syntax
!                              error.
!163. 2005-08-02  Goodger      Write out card for pcps_boss_exec_mode.
!162. 2005-05-17  Goodger      Add queue ODI91.
!161. 2005-05-03  Goodger      Add platforms 64linuxp52 and 64linuxp52_debug.
!160. 2005-04-18  Goodger      Use xeon library for B3060 queue.
!159. 2005-03-15  Goodger      Add platforms linuxg95 and linuxg95_debug.
!                              Change -DLINUX to -DLINUXA, -DLINUXI, or 
!                              -DLINUXP as appropriate.
!158. 2005-02-16  Goodger      Temporarily force useicps flag to be false, 
!                              while troubleshooting icps.
!157. 2005-01-20  Goodger      Execute script checknfs.sh on mpi jobs to
!                              determine if there are mount problems.
!156. 2004-12-08  Goodger      Change file that is sourced for intel compiles.
!155. 2004-11-04  Goodger      Add platforms linuxi81 and lunxi81_debug.
!154. 2004-08-31  Goodger      Use getsys to determine libraries if std_libs
!                              is custom.
!153. 2004-08-27  Goodger      Use the std_libs parameter for determining 
!                              the version of icps to use, rather than using
!                              getsys.
!152. 2004-08-12  Goodger      Move the killall hold_authmounts.sh card so
!                              that it will execute when a job aborts as well
!                              as when a job completes successfully.
!151. 2004-08-05  Goodger      Add pbs card for priority.
!150. 2004-08-04  Goodger      For single cpu jobs, use icps for job execution
!                              rather than making an executable.
!149. 2004-07-26  Goodger      Use linuxab80_xeon platform if location pony.
!148. 2004-07-13  Goodger      Allow B queue. Was being reset to B1800.
!147. 2004-07-12  SMCook       Added pre_WS3.0 to link path and LD_LIBRARY_PATH
!                               to stay compatible with new 2003.12 Landmark.
!146. 2004-07-01  Goodger      Added killall hold_automounts.sh. This process
!                              running was preventing the batchtmp directories
!                              from being deleted.
!145. 2004-06-04  Goodger      Execute script scrub_Aq_temp on A queue jobs.
!                              Build and execute hold_automounts script on
!                              parallel jobs. Build pbs card for rerun=no.
!144. 2004-05-18  Goodger      Add 64sol62 platform.
!143. 2004-05-11  Goodger      Use linuxab80_xeon library if queues B2790 or
!                              B3060TD.
!142. 2004-04-07  Goodger      Add ulimit -s unlimited.
!141. 2004-04-07  Goodger      Added umask 002.
!140. 2004-04-01  Goodger      Fix bug with not getting ppn=2 on the pbs card.
!                              Needed to trim before concatenate.
!139. 2004-03-26  Goodger      Remove call to job_data and get that information
!                              from the .wrk file.  This saves having to relink
!                              the job builder each time job_data is changed.
!                              Get the PONY config file.  Get pbs type from
!                              config file. Change /usr/local/bin/tcsh to
!                              /bin/tcsh. Scrub single cpu B queue jobs.
!138. 2004-03-03  Goodger      Add ppn=2 to single cpu B queue jobs.  This
!                              will force only one job per node.
!137. 2004-02-27  Goodger      Insure a property card is specified.  Otherwise,
!                              nodes of any property can get used.
!136. 2004-01-27  Goodger      Use beta versions of compression libraries for
!                              alpha and beta jobs.
!135. 2004-01-26  Goodger      Remove the #PBS nodes -l card from Alaska
!                              tape jobs. Get location from config file.
!                              versions of the compression libraries.
!134. 2004-01-16  Goodger      Allow submission of non-parallel jobs to the
!                              queue specified by job_data.
!133. 2004-01-05  Goodger      Skip timestamp files if remote location.
!132. 2003-12-19  Goodger      Output timestamp files to working directory
!                              indicating when a job started, and completed or
!                              aborted.
!131. 2003-12-02  Goodger      Do not build scrub if queue B1260W.  Use
!                              cps_main_host from config file to determine
!                              location.
!130. 2003-12-01  Goodger      Use A queue for Alaska tape jobs. Get platform
!                              default from config file. Get RLOCATION
!                              parameter from job_data and use that to build
!                              a job to be run at a remote location.
!                              Rename B queue to B1800.
!129. 2003-10-10  Goodger      Tape queues Todi74 and Todi90.
!128. 2003-10-02  Goodger      Add platform linuxab80_prof.
!127. 2003-09-22  Goodger      Remove exception of module directory for
!                              portland.
!                              Change name of liblmrk.so to liblmrkprod.so.
!126. 2003-09-19  SMCook       Added liblmrk.so-related code needed to handle
!                               production and alpha cases.  Also, the
!                               appropriate lmrk stubs file is now linked in
!                               when 'requires_landmark' is false.
!125. 2003-09-16  Goodger      Change linuxp platform to use -I as module
!                              flag.  Remove link needed by old version.
!                              Use houston compiler node.
!124. 2003-09-10  C C Burch    Change hd/tr allocatable declarations to pointer.
!123. 2003-09-02  SMCook       Major Landmark-related changes -- no longer needs
!                               to link to Landmark devkit at runtime, and now
!                               can run Landmark jobs on Linux platform.
!122. 2003-07-18  Goodger      Change default platform to linuxab80.
!                              Set OWHOME to /appl/ow per Steve Cook.
!121. 2003-07-14  SMCook       Set and export OWHOME to suppress an annoying/
!                               misleading warning with LMRK-related jobs.
!120. 2003-06-26  SMCook       Fixed error in revision date in brief_doc.
!119. 2003-06-26  SMCook       Added 'chmod 755 $TMPDIR' in effort to clear up
!                               permissions problems with files such as online
!                               and time_stamp in batchtmp directories.  Tight
!                               permissions make them invisible to jobmon.
!118. 2003-06-06  Goodger      Get the bin directory path from the config file.
!                              Print node names to report file.
!                              Add library to solaris jobs.
!                              Add platforms linuxab80 and linuxab80_debug.
!117. 2003-05-10  Goodger      Get batchtmp directory and node file path from
!                              the config file. This change requires beta config
!                              file revision 15.
!116. 2003-05-07  Goodger      Fix so do not need to change the job builder
!                              each time a Bxxx queue is added.
!115. 2003-05-05  SMCook       Added libgcc.a to Landmark link command to fix
!                               undefined symbols problem (__floatdidf and
!                               __eprintf).
!114. 2003-04-29  SMCook       Changed \ to \\ in Landmark compile & link to
!                               correct Houston portability problem.  This is a
!                               temp fix -- rsh_compile logic needs revision.
!113. 2003-04-15  SMCook       Landmark-related changes...
!                              Removed LAMHOME from cfecustom link command.  (A
!                               naming conflict exists between the lam library
!                               and a Landmark devkit library.  This problem has
!                               long since been addressed in cfebld & cfebldbeta
!                               except for the custom case).
!                              Hardcoded /usr/lib/libC.so.5 for Landmark jobs
!                               (-lC didn't work in Houston).
!                              Changed LD_LIBRARY_PATH /SunOS/lib to lib/SunOS.
!                               Pure luck that the old way worked (correct libs
!                               fortuitously picked up elsewhere in the path).
!112. 2003-04-08  Goodger      Add queues B1390, B1530, B1400TEST.
!111. 2003-04-03  Goodger      Set env variables cps_config_alpha and
!                              cps_config_beta. Add solaris directories sol62
!                              and sol62_debug.  Remove directories sol and
!                              sol_new.
!110. 2003-03-24  Goodger      Honor P queue in wrk file.
!109. 2003-03-13  SMCook       Eliminate hardcoded Landmark-related variables
!                               LM_LICENSE_FILE, OWHOME, etc., replace with
!                               portable cps_config.dat approach.
!108. 2003-02-13  Goodger      Use batchtmp nodes rather than /home to run
!                              jobs.
!107. 2003-01-27  Goodger      Remove --which from command_timeout.
!106. 2003-01-22  Goodger      Add /usr/applinux/bin to PATH
!105. 2003-01-17  Goodger      Run jobs out of users home directories.
!104. 2003-01-16  Goodger      Remove setenv of ABSOFT for Houston.
!103. 2002-11-13  Goodger      Added environmental variable for landmark.
!102. 2002-09-24  Vunderink    Changed linuxp to new Portland Group compiler,
!                                changed machine from poepsn03 to Solaris, and
!                                nullified process and parallel object pointers.
!101. 2002-09-09  Vunderink    Increased length of character variable lnklib.
!100. 2002-08-26  Vunderink    Fixed 8000 label problem for setup-only jobs.
! 99. 2002-08-20  Vunderink    Added close of scratch file unit.
! 98. 2002-08-12  Vunderink    Fixed linuxp platform link and modified to call
!                                process deletes if setup error.
! 97. 2002-08-03  Vunderink    extra_libs needs -lm for Absoft and Portland
!                                compilers but not Intel, and split link command
!                                after -Ls to shorten line.
! 96. 2002-07-30  Vunderink    Remove LAMHOME from library path if using
!                                mpi_stubs.
! 95. 2002-07-29  Vunderink    Modified to use MPI version 6.5.6.
! 94. 2002-07-23  Vunderink    Added exporting cps_config_file, modified to
!                                use cnfg module to get directories, and fixed
!                                linuxi and linuxi_debug platforms to allow
!                                custom code.
! 93. 2002-07-22  Vunderink    Added scrub_tmp to B and S queue jobs, fixed
!                                custom debug compiles on Solaris platforms,
!                                fixed custom C code compiles with debug on all
!                                platforms, modified linuxi and linuxi_debug
!                                platform compile options, and forced linuxi
!                                and linuxi_debug platform parallel jobs to use
!                                MPI 6.5.6.
! 92. 2002-06-20  Vunderink    Added call to cps_write_accounting_signal in
!                                cpssig and added exporting PBS_JOBID in mpirun.
! 91. 2002-06-14  Vunderink    Re-structured job to have internal cpssig routine
!                                that calls wrapup for each process.
! 90. 2002-06-13  Vunderink    Added machine speed selection to FXSHOT queue.
! 89. 2002-06-07  Vunderink    Added support for different mpi versions.
! 88. 2002-06-06  Vunderink    Added num_nodes support and added -p $TMDIR to
!                                linuxab75 and linuxab75_debug compiles.
! 87. 2002-05-20  Vunderink    Force a request for B800Mz to general B nodes.
! 86. 2002-05-17  Vunderink    Changed default platform for prodlib to linuxab75
! 85. 2002-05-14  Vunderink    Break parallel group after process that needs
!                                resequence.
! 84. 2002-05-13  Vunderink    Reinitialize need_extra_io after parallel_end.
! 83. 2002-05-08  Vunderink    Made changes required by PCPS_RESEQUENCE being
!                                replaced by PCPS_RESEQUENCE_TRACES and _GROUPS.
! 82. 2002-05-04  Vunderink    Changed linux_new platform to linuxab75, added
!                                linuxab75_debug platform, changed default
!                                platform for betalib to linuxab75, added ABSOFT
!                                environment variable to linux platform
!                                compiles, added command_timeout to lam
!                                commands, and fixed labeling in parallel jobs
!                                when setup_only in loop.
! 81. 2002-04-27  Vunderink    Fixed labeling when two parallel loops are back
!                                to back.
! 80. 2002-04-11  Vunderink    Fixed trace/header array allocation and parallel
!                                object creation in multi-loop parallel jobs,
!                                added pcpsx_restart_processing between loops,
!                                added ulimit for core and redirect standardout
!                                for lamboot to err.out
! 79. 2002-04-04  Vunderink    Fixed O3 option on gcc compiles
! 78. 2002-02-28  Vunderink    Added ALPHALIB.
! 77. 2002-02-26  Vunderink    Remove -YEXT_NAMES=LCS -YEXT_SFX=_ compile
!                                options on mpi main.
! 76. 2002-02-22  Vunderink    Change compile options on linux_new.
! 75. 2002-02-14  Vunderink    Fixed bug in test for link existence on Solaris.
! 74. 2002-02-13  Vunderink    Fixed bug in removing old link in cpsjobs when
!                                a job is resubmitted by PBS.
! 73. 2002-02-08  Vunderink    Added 10 second sleep before lamclean.
! 72. 2002-02-05  Vunderink    Use SPEED parameter from JOB_DATA to request
!                                nodes from PBS.
! 71. 2001-12-27  Vunderink    Added /opt/SUNWspro/bin to PATH and set
!                                LD_LIBRARY_PATH for all solaris jobs.
! 70. 2001-12-20  Vunderink    Fixed bug in -p new for linux and added -p linuxi
!                                and -p linuxi_debug.
! 69. 2001-12-17  Vunderink    Added pthread library to link, create cpsjobs
!                                directory and make link to batchtmp, added
!                                TMEDIA control parameter, and changed name to
!                                buildjob.
! 68. 2001-11-12  Vunderink    Added queues A2 and S and changed parallel
!                                property breaks to 16 and 64.
! 67. 2001-11-08  Vunderink    Enhanced parallel logic to build jobs requiring
!                                multiple parallel objects.
! 66. 2001-11-07  Vunderink    Added support for platform option.
! 65. 2001-11-02  Vunderink    Flag as abort if core exists and implement
!                                a monthly abort file.
! 64. 2001-11-01  Vunderink    Added /usr/ucb to PATH.
! 63. 2001-10-29  Vunderink    Added mpi_stubs to Solaris link and ignore
!                                numcpus if Landmark job.
! 62. 2001-10-03  Vunderink    Put "Job Abort" string in report file so that
!                                grep can be used to find aborts, do not
!                                delete batchtmp directory if core file present
!                                and make .compile file supply default compiler
!                                and options if not supplied.
! 61. 2001-09-27  Vunderink    Removed -g from solaris
! 60. 2001-09-26  Vunderink    Fixed problem with setup_only process.
! 59. 2001-09-10  Vunderink    Made changes required to rename TESTLIB to
!                                BETALIB and added support for .compile files.
! 58. 2001-08-14  Vunderink    Ignore custom_lam if batch
! 57. 2001-08-09  Vunderink    Abort job build if blank/invalid process in
!                                workfile.
! 56. 2001-08-02  Vunderink    Removed forcing poepsn03 to use NQS and added
!                                PBS support to Solaris platform.
! 55. 2001-07-31  Vunderink    Fixed logic problem in write_parallel, cleaned
!                                up old unused variables, and fixed trace/header
!                                array sizing.
! 54. 2001-07-05  Vunderink    Put custom nodes and custom lam jobs in C queue
!                                and put all other parallel jobs in B queue
! 53. 2001-06-22  Vunderink    Added same_speed variable
! 52. 2001-06-12  Vunderink    Set to exit if executable aborts
! 51. 2001-06-08  Vunderink    Use script for rsh compile in order to retry
!                                on failure, redirect standard output and error
!                                on mkdir command and added
!                                PCPS_BOSS_EXECS_MERGE to parallel jobs.
! 50. 2001-06-04  Vunderink    Changed single cpu jobs to allocate trace and
!                                header arrays, enhanced parallel jobs to
!                                support PCPS_BOSS_EXECS_GATHER, and added
!                                generator mode to PCPS_BOSS_EXECS.
! 49. 2001-06-01  Vunderink    Pass PBS_REQID to parallel executables
! 48. 2001-05-15  Vunderink    Added OWHOME variable to Landmark jobs.
! 47. 2001-05-14  Vunderink    Added Landmark support
! 46. 2001-05-07  Vunderink    Use PBS_REQID for temporary directory and
!                                if SETUP_ONLY, set NEED_REQUEST and NEED_LABEL
!                                to NO.
! 45. 2001-05-02  Vunderink    Make Linux link always add MPI libraries
! 44. 2001-04-06  Vunderink    Fixed problem with custom_rcs_ident and grep
! 43. 2001-04-06  Vunderink    Added building custom_rcs_ident file.
! 42. 2001-03-30  Vunderink    Changed PCPS_BOSS_SENDS_DATA to PCPS_BOSS_EXECS
!                                and made changes needed for parallel error
!                                trapping.
! 41. 2001-03-28  Vunderink    Fixed mpirun for interactive parallel job, added
!                                PCPS_PROCESS_TASKS, CUSTOM_LAM, CUSTOM_NODES,
!                                CUSTOM_EXEC_B, CUSTOM_EXEC_A, and
!                                PCPS_BOSS_SENDS_DATA support, and
!                                fixed missing parallel_end if last process is
!                                is in do_parallel.
! 40. 2001-03-13  Vunderink    Removed all lowercasing of jobname.
! 39. 2001-02-26  Vunderink    Hardcoded pospx1 and pospx3 to use NQS and
!                                removed some old code for special tape
!                                handling
! 38. 2001-02-22  Vunderink    Added library fwcd to link for compression
!                                software.
! 37. 2001-02-15  Vunderink    Added support for A_short queue.
! 36. 2001-02-06  Vunderink    Made changes in parallel jobs.
! 35. 2001-01-31  Vunderink    Fixed problem with control character in multi-cpu
!                                job files.
! 34. 2001-01-23  Vunderink    Made changed required by PBS queuing system.
! 33. 2001-01-12  Vunderink    Fixed problems in parallel job building.
! 32. 2001-01-04  Vunderink    Setup TMPDIR for use with mpirun and added
!                                cluster node support for parallel jobs on PBS.
! 31. 2000-12-20  Vunderink    Changed name to build_job, made changes for
!                                CPS standards, added parallel logic and added
!                                batch system choice.
! 30. 2000-12-08  Vunderink    Fixed to use library linked with when custom
!                                cards empty.
! 29. 2000-12-04  Vunderink    Fixed to check error status after each loop in
!                                a multi-loop job.  Changed to use library
!                                linked with when adding to custom link.
! 28. 2000-11-08  Vunderink    Add support for poepld nodes
! 27. 2000-10-13  Vunderink    Route parameter cache messages to scratch file,
!                                if all custom cards are empty, change STDLIBS
!                                to TESTLIB
! 26. 2000-10-04  Vunderink    Remove special handling of poepsn03
! 25. 2000-09-18  Vunderink    Increase character string for storing commands
!                                to add to batch job.
! 24. 2000-09-15  Vunderink    Add SAVE to trace and header arrays.
! 23. 2000-09-14  Vunderink    Initialize variable nobjs to zero.
! 22. 2000-09-07  Vunderink    Fix linux interactive to have RSH_COMPILE
!                                parameter
! 21. 2000-09-07  Vunderink    Add parameter RSH_COMPILE to job, add unalias to
!                                job, do rsh for tape jobs only on poepsn03,
!                                and do not build remote compile for linux
!                                interactive jobs if built on linux with
!                                compiler.
! 20. 2000-08-25  Vunderink    Fixed problem of tmpdir not being set on
!                                interactive jobs built for poeplx nodes.
! 19. 2000-08-24  Vunderink    Added support for non-disk linux nodes and
!                                modified custom_link to add libraries.
! 18. 2000-08-22  Vunderink    Added libraries blzw and jos to link for
!                                compression software.
! 17. 2000-07-31  Vunderink    Changed solaris link to use socket and nsl
!                                libraries instead of xnet.
! 16. 2000-07-27  Vunderink    Rcp _netinfo file to pospt1 for tape jobs.
! 15. 2000-07-21  Vunderink    Added /usr/app/bin to PATH, remove blank rows
!                                from custom parameters in JOB_DATA, get PID at
!                                run time for temporary directory, and add
!                                support for tape jobs to pospt1.
! 14. 2000-06-20  Vunderink    Parameter cache change for return a global value
!                                when one not present required checking to see
!                                if present before doing a pc_get_global.
! 13. 2000-06-20  Vunderink    Add extra_libs to custom link when no link
!                                command is specified.
! 12. 2000-06-15  Vunderink    Added support for using a PLATFORM environment
!                                variable in custom compiles and link, added
!                                support for supplying compiler and default
!                                compiler options in custom compiles, and make
!                                compile options for main the same as used on
!                                the library linked with.
! 11. 2000-05-23  Vunderink    Added support for SETUP_ONLY, CMD_BEFORE_EXE,
!                                CMD_AFTER_EXE, CMD_BEFORE_BSCRIPT, and
!                                CMD_AFTER_BSCRIPT control parameters,
!                                removed -g from compile of main and removed
!                                standard input redirect from execution.
! 10. 2000-04-27  Vunderink    Changed linux compiler node to poeplc01
!  9. 2000-04-19  Vunderink    Changed routine write_main to use new cps module
!                                routines to generate a better reportfile and
!                                gather statistics, remove -eo option on linux
!                                qsub and route standard error to file.
!  8. 2000-04-18  Vunderink    Added xnet library to solaris link
!  7. 2000-03-14  Vunderink    Move deletion of temporary directory out of IF
!                                block for mail_opt
!  6. 2000-03-03  Vunderink    Removed QSUB time limit card for default ostype
!  5. 2000-02-25  Vunderink    Create compile scrip in temporary directory
!  4. 2000-02-25  Vunderink    Make user directory on scratch disk for poeplx
!                                machines
!  3. 2000-02-25  Vunderink    Changed poeplx machines to run jobs on scratch
!                                disk.
!  2. 2000-02-05  Vunderink    Made changes required by STD_LIBS change and
!                                modified to do rsh for compile on poeplx nodes.
!  1. 1999-07-08  Vunderink    Initial version.
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
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
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
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module buildjob_module

      use cardset_module
      use cio_module
      use cnfg_module
      use pc_module
      use project_data_module
      use getsys_module
      use putsys_module
      use string_module
      use path_module

      implicit none

      private
      public :: buildjob

      integer, parameter :: STDOUT               = 6
      integer, parameter :: NUM_TAGS             = 4
      integer, parameter :: TAG_MAX_LEN          = 20
      integer, parameter :: PNAME_MAX            = 12
      integer, parameter :: GLOBALS_TAG          = 1
      integer, parameter :: CONTROLS_TAG         = 2
      integer, parameter :: PARAMETERS_TAG       = 3
      integer, parameter :: DATA_CARD_TAG        = 4
      integer, parameter :: MAX_IO_PAIRS         = 100
      integer, parameter :: PCPSNAME_MAX         = 30

      type CARD
        character(len=PC_LENGTH) :: data
        type(CARD),pointer       :: next
      end type CARD

      type CARD_POINTER
        type(CARD),pointer :: card
      end type CARD_POINTER

      type PROCESS
        character(len=PNAME_MAX)               :: name
        integer                                :: nname
        integer                                :: pid
        character(len=4)                       :: cpid
        integer                                :: ncpid
        character(len=4)                       :: parallel_safe
        character(len=4)                       :: need_request
        character(len=4)                       :: need_label
        character(len=4)                       :: twosets
        character(len=4)                       :: setup_only
        character(len=4)                       :: requires_landmark
        character(len=4)                       :: signal_handler
        character(len=PCPSNAME_MAX)            :: pcps_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_send_eof_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_resequence_mode
        character(len=PCPSNAME_MAX)            :: pcps_generator_mode
        character(len=PCPSNAME_MAX)            :: pcps_bunch_mode
        character(len=PCPSNAME_MAX)            :: pcps_boss_exec_mode
        character(len=PCPSNAME_MAX)            :: pcps_process_tasks
        integer                                :: ltr
        integer                                :: lhd
        integer                                :: ltr2
        integer                                :: lhd2
        integer                                :: ntr
        integer                                :: nscratch
        integer                                :: nstore
        integer                                :: ndisk
        integer                                :: ntapes
        character(len=8)                       :: tmedia
        integer                                :: n_io_pairs
        character(len=4)                       :: iftd
        logical                                :: start_loop
        logical                                :: start_fork
        integer                                :: parallel_group
        integer                                :: label
        integer                                :: request
        integer                                :: parallel_begin
        integer                                :: parallel_end
        type(CARD_POINTER)                     :: tag_card_start(NUM_TAGS)
        type(CARD_POINTER)                     :: tag_card_last(NUM_TAGS)
        type(PROCESS),pointer                  :: next
      end type PROCESS

      type PGROUP
        integer                                :: job_loop
        character(len=PCPSNAME_MAX)            :: pcps_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_send_eof_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_resequence_mode
        character(len=PCPSNAME_MAX)            :: pcps_generator_mode
        character(len=PCPSNAME_MAX)            :: pcps_bunch_mode
        character(len=PCPSNAME_MAX)            :: pcps_boss_exec_mode
        type(PGROUP),pointer                   :: next
      end type PGROUP

      character(len=80), save                  :: main_host
      character(len=80), save                  :: pbs_type
      character(len=80), save                  :: rcp_node
      character(len=80) ,save                  :: platform_default_linux
      character(len=80) ,save                  :: platform_default_sol
      character(len=80) ,save                  :: sps_home
      character(len=80) ,save                  :: sps_install
      character(len=80) ,save                  :: cpseis_arch
      character(len=80) ,save                  :: cps_log
      character(len=80) ,save                  :: batchtmp_nodes
      character(len=80) ,save                  :: batchtmp_dirs
      character(len=80) ,save                  :: binpath
      integer           ,save                  :: total_memory
      character(len=24) ,save                  :: account
      character(len=15) ,save                  :: jobname
      character(len=16) ,save                  :: frontend_node
      character(len=16) ,save                  :: frontend_user
      character(len=132),save                  :: frontend_path
      character(len=132),save                  :: thisbatchtmp
      character(len=4)  ,save                  :: mail_opt
      character(len=80) ,save                  :: mailingaddress
      character(len=8)  ,save                  :: queue
      character(len=8)  ,save                  :: rlocation,location
      integer           ,save                  :: time_limit
      character(len=10) ,save                  :: machine,whichicps
      character(len=4)  ,save                  :: rerun,speed
      integer           ,save                  :: num_cpus,priority
      integer           ,save                  :: num_nodes = 0
      character(len=24) ,save                  :: std_libs
      integer           ,save                  :: ncustom_modules
      character(len=PC_LENGTH) ,pointer,save   :: custom_modules(:)
      integer           ,save                  :: ncustom_compile
      character(len=PC_LENGTH) ,pointer,save   :: custom_compile(:)
      integer           ,save                  :: ncustom_link
      character(len=PC_LENGTH) ,pointer,save   :: custom_link(:)
      character(len=PC_LENGTH)                 :: custom_exec_b
      character(len=PC_LENGTH)                 :: custom_exec_a
      character(len=4)                         :: custom_lam
      integer           ,save                  :: ncustom_nodes
      character(len=12)        ,pointer,save   :: custom_nodes(:)
      character(len=132),save                  :: tmpdir
      logical           ,save                  :: rsh_compile
      logical           ,save                  :: requires_landmark
      integer           ,save                  :: max_tapes
      character(len=8)  ,save                  :: tape_media,tapequeue
      logical           ,save                  :: same_speed
      integer           ,save                  :: max_pid_digits
      logical           ,save                  :: anycustom,useicps

      character(len=100),public :: buildjob_ident = &
        '$Id: buildjob.f90,v 1.170 2007/05/31 15:17:54 Goodger beta sps $'

      contains

!!------------------------------- buildjob ---------------------------------!!
!!------------------------------- buildjob ---------------------------------!!
!!------------------------------- buildjob ---------------------------------!!

      subroutine buildjob (workfile,jobfile,mode,batch,customp,custom_mpi,istat)

      character(len=*),intent(in)               :: workfile           ! argument
      character(len=*),intent(in)               :: jobfile            ! argument
      character(len=*),intent(in)               :: mode               ! argument
      character(len=*),intent(in)               :: batch              ! argument
      character(len=*),intent(in)               :: customp            ! argument
      character(len=*),intent(in)               :: custom_mpi         ! argument
      integer         ,intent(inout)            :: istat              ! argument

      integer                                   :: lun           = 6  ! local
      integer                                   :: wunit         = 1  ! local
      integer                                   :: junit         = 2  ! local
      integer                                   :: i                  ! local
      integer                                   :: i1                 ! local
      integer                                   :: i2                 ! local
      integer                                   :: ostype,ntrin=0     ! local
      character(len=132)                        :: jobmode            ! local
      character(len=4)                          :: batchsystem        ! local
      character(len=132)                        :: platform           ! local
      character(len=132)                        :: mpi_version        ! local
      character(len=132)                        :: current_card       ! local
      character(len=132)                        :: ctmp               ! local
      integer                                   :: process_count      ! local
      type(PROCESS),pointer                     :: start              ! local
      type(PROCESS),pointer                     :: last               ! local
      type(PROCESS),pointer                     :: current            ! local
      type(CARD_POINTER)                        :: tag_card           ! local
      logical                                   :: invalid_process    ! local
      logical                                   :: first,entireloop
      logical                                   :: do_parallel        ! local
      logical                                   :: sorryNotParallel
      logical                                   :: tag_flag(NUM_TAGS) ! local
      character(len=TAG_MAX_LEN)                :: tags(NUM_TAGS)     ! local
      integer                                   :: ntags(NUM_TAGS)    ! local


      custom_exec_b   = ' '
      custom_exec_a   = ' '
      ncustom_modules = 0
      ncustom_compile = 0
      ncustom_link    = 0
      ncustom_nodes   = 0
      nullify(custom_modules)
      nullify(custom_compile)
      nullify(custom_link)
      nullify(custom_nodes)

      tags(1)  = ' <GLOBALS>'
      tags(2)  = ' <CONTROLS>'
      tags(3)  = ' <PARAMETERS>'
      tags(4)  = ' <DATA_CARD>'
      ntags(1) = 10
      ntags(2) = 11
      ntags(3) = 13
      ntags(4) = 12

      jobmode = mode
      call string_to_upper (jobmode)
      if (jobmode(1:1) .eq. 'I') then
        jobmode    = 'INTERACTIVE'
        custom_lam = 'YES'
      else
        jobmode    = 'BATCH'
        custom_lam = 'NO'
      endif

      batchsystem = batch
      call string_to_upper (batchsystem)
      if (batchsystem(1:1) .eq. 'P') then
        batchsystem = 'PBS'
      else
        batchsystem = 'NQS'
      endif

      call path_parse (trim(workfile),frontend_user,frontend_node,  &
                       frontend_path,ctmp)

      open(unit=wunit,file=workfile,status='OLD' ,access='SEQUENTIAL',err=900)
      open(unit=2,file=jobfile ,status='UNKNOWN',access='SEQUENTIAL',err=901)
      open(unit=3              ,status='SCRATCH',access='SEQUENTIAL',err=904,  &
           form='FORMATTED')

      process_count = 0
      nullify(start)
      nullify(last)

      read(wunit,'(A80)',err=902,end=100) current_card
      call string_strip_blanks(current_card,i)
      call string_to_upper(current_card)
      if (current_card(1:i) .ne. '<CPS_V1TYPE="WORKFILE"/>') then
        write(STDOUT,*) 'File is not a CPS Workfile'
        return
      endif

      invalid_process = .false.
! read wrk file loop
      do
        read(wunit,'(A80)',err=902,end=100) current_card
        if (current_card(1:1) .eq. '<') then
          if (current_card(1:10) .eq. '</PROCESS>') cycle
          allocate(current,stat=istat)
          if (istat .ne. 0) goto 903
          do i=1,NUM_TAGS          
            nullify(current%tag_card_start(i)%card)
            nullify(current%tag_card_last(i)%card)
          enddo
          nullify(current%next)
          call string_strip_blanks(current_card)
          call string_to_upper(current_card)
          i1 = index(current_card,'<PROCESSNAME="') ! <PROCESS name=  in job file
          if (i1 .gt. 0) then
            i1 = i1+14
            i2 = index(current_card(i1:),'"')
            if (i2 .gt. 1) then
              current%name  = current_card(i1:i1+i2-2)
            else
              current%name  = 'NONE'
            endif
          else
              current%name  = 'NONE'
          endif
          call string_strip_blanks(current%name,current%nname)
          process_count = process_count + 1
          current%pid   = process_count
          write(current%cpid,'(I4)') current%pid
          call string_strip_blanks(current%cpid,current%ncpid)
          if (current%name .eq. 'NONE') then
            invalid_process = .true.
            write(STDOUT,*) 'Invalid process '//current%cpid
          endif
          current%parallel_safe         = 'NO'
          current%need_request          = 'NO'
          current%need_label            = 'NO'
          current%twosets               = 'NO'
          current%setup_only            = 'NO'
          current%requires_landmark     = 'NO'
          current%signal_handler        = 'NO'
          current%pcps_send_mode        = 'PCPS_SEND_ROUND_ROBIN'
          current%pcps_receive_mode     = 'PCPS_RECEIVE_PASSTHRU'
          current%pcps_send_eof_mode    = 'PCPS_SEND_ALL_EOF'
          current%pcps_alt_send_mode    = 'PCPS_SEND_ROUND_ROBIN'
          current%pcps_alt_receive_mode = 'PCPS_RECEIVE_PASSTHRU'
          current%pcps_resequence_mode  = 'PCPS_NO_RESEQUENCE'
          current%pcps_generator_mode   = 'PCPS_NO_TRACE_GEN'
          current%pcps_bunch_mode       = 'PCPS_BUNCH_TRACE_GROUPS'
          current%pcps_boss_exec_mode   = ' '
          current%ltr                   = 0
          current%lhd                   = 0
          current%ltr2                  = 0
          current%lhd2                  = 0
          current%ntr                   = 0
          current%nscratch              = 0
          current%nstore                = 0
          current%ndisk                 = 0
          current%ntapes                = 0
          current%tmedia                = '3590'
          current%n_io_pairs            = 0
          current%start_loop            = .FALSE.
          current%start_fork            = .FALSE.
          current%parallel_group        = 0
          current%label                 = 0
          current%request               = 0
          current%parallel_begin        = 0
          current%parallel_end          = 0
          if (trim(current%name) .eq. 'PROJECT_DATA' .or.  &
              trim(current%name) .eq. 'JOB_DATA') current%setup_only   = 'YES'
          call buildjob_store(current,start,last)
          do i=1,NUM_TAGS    
            tag_flag(i) = .FALSE.
          enddo
        else if (current_card(1:2) .eq. ' <') then    ! a start tag
          do i=1,NUM_TAGS
            tag_flag(i) = .FALSE.
          enddo
          do i=1,NUM_TAGS
            if (current_card(1:ntags(i)) .eq. tags(i)(1:ntags(i))) then
              tag_flag(i) = .TRUE.
              exit
            endif
          enddo
        else
          ctmp = current_card
          call string_strip_blanks(ctmp)
          if (ctmp(1:1) .eq. ' ') cycle
          do i=1,NUM_TAGS
            if (tag_flag(i)) then
              allocate(tag_card%card,stat=istat)
              if (istat .ne. 0) goto 903
              tag_card%card%data = current_card(3:)
              call buildjob_store_card(tag_card,                   &
                                        current%tag_card_start(i),  &
                                        current%tag_card_last(i))
              exit
            endif
          enddo
        endif
      enddo

  100 continue  ! end of wrk file

      write(ctmp,'(I4)') process_count
      call string_strip_blanks(ctmp,max_pid_digits)

      if (invalid_process) then
        write(STDOUT,*) ' '
        write(STDOUT,*) 'Job build ABORTED'
        return
      endif

      call pc_frontend_update          (3)
      call buildjob_load_job_data     (junit, start, wunit)
      call buildjob_load_project_data (junit, start)

!     Do parallel ??
      do_parallel = .false.
      same_speed  = .false.
      entireloop  = .false.
      sorryNotParallel = .false.
      if (num_cpus .gt. 1) then
        current => start
        do
          if (.not. associated(current)) then
            entireloop=.true.
            exit
          endif
          tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
          first = .TRUE.
          call pc_next
          do
            if (.not. associated(tag_card%card)) exit
            if (first) then
              call pc_put_control_card(tag_card%card%data)
              first = .FALSE.
            else
              call pc_add_control_card(tag_card%card%data)
            endif
            tag_card%card => tag_card%card%next
          enddo
          call pc_get_control ('PARALLEL_SAFE',current%parallel_safe)
!    If trin, it is starting a new loop.  Each loop must have a
!      parallel process to be parallel safe
      if(current%name.eq.'TRIN')then
        ntrin=ntrin+1
        if(ntrin.gt.1.and..not.do_parallel)then
          sorryNotParallel=.true.
          exit
        endif
        do_parallel=.false.
      endif
      
          call string_to_upper (current%parallel_safe)
          if (trim(current%parallel_safe) .eq. 'YES') then
            do_parallel = .true.
!!!            if (same_speed) exit
          endif
          call pc_get_control ('PCPS_SEND_MODE',current%pcps_send_mode)
          call string_to_upper (current%pcps_send_mode)
          if (index(current%pcps_send_mode,'PCPS_BOSS_EXECS') .gt. 0) then
            same_speed = .true.
!!!            if (do_parallel) exit
          endif
          current => current%next
        enddo
        if(.not.do_parallel.and.entireloop)sorryNotParallel=.true.
      endif

!     Uses Tape Drives ??
      max_tapes  = 0
      tape_media = '3590'
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo
        call pc_get_control ('NTAPES',current%ntapes)
        max_tapes  = max(max_tapes, current%ntapes)
        if (current%ntapes .gt. 0) then
          call pc_get_control ('TMEDIA',current%tmedia)
          tape_media = current%tmedia
          call pc_get_control('TAPEQUEUE',tapequeue)
        endif
        current => current%next
      enddo


      if (do_parallel .and. max_tapes .gt. 0) do_parallel = .false.

!     Requires Landmark ??
      requires_landmark = .false.
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo
        call pc_get_control ('REQUIRES_LANDMARK',current%requires_landmark)
        call string_to_upper (current%requires_landmark)
        if (trim(current%requires_landmark) .eq. 'YES') then
          requires_landmark = .true.
          useicps=.false.
          exit
        endif
        current => current%next
      enddo

      !commented by SMCook - Landmark no longer so restrictive
      !if (requires_landmark) then
      !  do_parallel = .false.
      !  same_speed  = .false.
      !  machine     = 'Solaris'
      !  num_cpus    = 1
      !  num_nodes   = 1
      !  max_tapes   = 0
      !endif

      if (trim(machine).eq.'Custom' .and. ncustom_nodes.gt.0) then
        machine = custom_nodes(1)
      else if (trim(custom_lam) .eq. 'YES'   .and.  &
               trim(jobmode)    .eq. 'BATCH' .and.  &
               ncustom_nodes    .eq. 0       ) then
         custom_lam = 'NO'
         write(STDOUT,*) ' '
         write(STDOUT,*) 'WARNING - custom_lam ignored for batch'
         write(STDOUT,*) ' '
      endif

      if (.not. do_parallel) then
        num_cpus  = 1
        num_nodes = 1
      endif

      if (trim(custom_mpi) .ne. 'default'   .and.  &
          trim(custom_mpi) .ne. 'new'       .and.  &
          trim(custom_mpi) .ne. '7.0.6'   ) then
        write(STDOUT,*) 'Mpi version not supported'
        return
      endif
      if (trim(custom_mpi) .eq. 'default' ) then
        mpi_version = 'mpich2'
      else if (trim(custom_mpi) .eq. 'new') then
        mpi_version = 'lam'
      else
        mpi_version = custom_mpi
      endif
      if (trim(customp) .ne. 'default'           .and.  &
          trim(customp) .ne. 'debug'             .and.  &
          trim(customp) .ne. 'new'               .and.  &
          trim(customp) .ne. 'linux'             .and.  &
          trim(customp) .ne. 'linux_debug'       .and.  &
          trim(customp) .ne. 'linuxab75'         .and.  &
          trim(customp) .ne. 'linuxab75_debug'   .and.  &
          trim(customp) .ne. 'linuxab80'         .and.  &
          trim(customp) .ne. 'linuxab80_debug'   .and.  &
          trim(customp) .ne. 'linuxab80_prof '   .and.  &
          trim(customp) .ne. 'linuxab80_xeon '   .and.  &
          trim(customp) .ne. 'linuxab90'         .and.  &
          trim(customp) .ne. 'linuxab90_debug'   .and.  &
          trim(customp) .ne. 'linuxab90_xeon '   .and.  &
          trim(customp) .ne. 'sol70    '         .and.  &
          trim(customp) .ne. 'sol70_debug    '   .and.  &
          trim(customp) .ne. 'linuxp'            .and.  &
          trim(customp) .ne. 'linuxi81'            .and.  &
          trim(customp) .ne. 'linuxi81_debug'      .and.  &
          trim(customp) .ne. 'sol62'             .and.  &
          trim(customp) .ne. '64sol62'           .and.  &
          trim(customp) .ne. 'gfortran32'           .and.  &
          trim(customp) .ne. 'gfortran32_debug'     .and.  &
          trim(customp) .ne. 'gfortran64'           .and.  &
          trim(customp) .ne. 'gfortran64_debug'     .and.  &
          trim(customp) .ne. '64linuxp52'           .and.  &
          trim(customp) .ne. '64linuxp52_debug'     .and.  &
          trim(customp) .ne. 'linuxi91'     .and.  &
          trim(customp) .ne. 'sol62_debug' )then
        write (STDOUT,*) 'Platform not supported'
        return
      endif
      call string_to_upper (machine,ctmp)
      if (trim(ctmp) .eq. 'LINUX') then
        ostype  = GETSYS_LINUX
      else if (trim(ctmp) .eq. 'SOLARIS') then
        ostype  = GETSYS_SOLARIS
      else if (trim(ctmp).eq.'CUSTOM') then
        if (ncustom_nodes.gt.0) then
          ostype  = getsys_machine(machine)
        else
           ostype  = GETSYS_LINUX
        endif
      else
        ostype = getsys_machine(machine)
      endif
      if (trim(customp) .eq. 'default') then
        select case (ostype)
          case (GETSYS_LINUX)
            platform = trim(platform_default_linux)
            if(queue(1:5).eq.'B3060'.or.location.eq.'pony'.or.&
              rlocation.eq.'PONY')&
            then
              platform='linuxi91'
            endif
          case (GETSYS_SOLARIS)
            platform = trim(platform_default_sol)
          case default
            platform = 'unknown'
        end select
      else if (trim(customp) .eq. 'debug') then
        select case (ostype)
          case (GETSYS_LINUX)
            platform = 'linuxi91'
          case (GETSYS_SOLARIS)
            platform = 'sol62_debug'
          case default
            platform = 'unknown'
        end select
      else if (trim(customp) .eq. 'new') then
        select case (ostype)
          case (GETSYS_LINUX)
            platform = 'linuxi91'
          case (GETSYS_SOLARIS)
            platform = 'sol62'
          case default
            platform = 'unknown'
        end select
      else
        platform = customp
        select case (ostype)
          case (GETSYS_LINUX)
            i = index(platform,'linux')
            if (i .eq. 0) platform = 'unknown'
          case (GETSYS_SOLARIS)
            i = index(platform,'sol')
            if (i .eq. 0) platform = 'unknown'
          case default
            platform = 'unknown'
        end select
      endif
      if (trim(platform) .eq. 'unknown') then
        write(stdout,*) 'Invalid platform'
        return
      endif
      if(trim(platform).ne.trim(platform_default_linux))useicps=.false.

      call buildjob_write_setup (junit,start,trim(jobmode),trim(batchsystem), &
                                  trim(platform),trim(mpi_version))



      if (do_parallel) then
        call buildjob_write_parallel (junit,start)
      else
        call buildjob_write_main     (junit,start)
      endif
      if(sorryNotParallel)then
        print*,' Parallel process not found in TRIN loop ',ntrin
        print*,' Job will be set to use only 1 cpu.'
      endif
      call buildjob_write_data (junit,start)
      call buildjob_write_jcl  (junit,start,trim(jobmode),trim(batchsystem),  &
                                 trim(platform),trim(mpi_version))

      close(unit=wunit)
      close(unit=2)
      close(unit=3)

      if (trim(jobmode) .eq. 'INTERACTIVE') then
        call putsys_cmd ('chmod +x ' // jobfile)
      endif

      return

  900 write(STDOUT,*) 'Workfile not found'
      return
  901 write(STDOUT,*) 'Can not open jobfile'
      return
  902 write(STDOUT,*) 'Error reading workfile'
      return
  903 write(STDOUT,*) 'Out of memory'
      return
  904 write(STDOUT,*) 'Could not open parameter cache'
      return
      end subroutine buildjob


!!---------------------------- buildjob_store ------------------------------!!
!!---------------------------- buildjob_store ------------------------------!!
!!---------------------------- buildjob_store ------------------------------!!


      subroutine buildjob_store(current,start,last)

      type(PROCESS),pointer                     :: current            ! argument
      type(PROCESS),pointer                     :: start              ! argument
      type(PROCESS),pointer                     :: last               ! argument

      if (.not. associated(last)) then
        start => current
      else
        last%next => current
      endif
      nullify(current%next)
      last => current

      return
      end subroutine buildjob_store


!!------------------------- buildjob_store_card ----------------------------!!
!!------------------------- buildjob_store_card ----------------------------!!
!!------------------------- buildjob_store_card ----------------------------!!


      subroutine buildjob_store_card(current,start,last)

      type(CARD_POINTER)                        :: current            ! argument
      type(CARD_POINTER)                        :: start              ! argument
      type(CARD_POINTER)                        :: last               ! argument

      if (.not. associated(last%card)) then
        start%card => current%card
      else
        last%card%next => current%card
      endif
      nullify(current%card%next)
      last%card => current%card

      return
      end subroutine buildjob_store_card


!!----------------------- buildjob_write_parallel --------------------------!!
!!----------------------- buildjob_write_parallel --------------------------!!
!!----------------------- buildjob_write_parallel --------------------------!!


      subroutine buildjob_write_parallel(junit,start)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument

      integer                         :: i                            ! local
      integer                         :: j                            ! local
      integer                         :: k                            ! local
      integer                         :: j1                           ! local
      integer                         :: j2                           ! local
      type(PROCESS),pointer           :: current                      ! local
      type(PROCESS),pointer           :: loop_start_process           ! local
      type(PROCESS),pointer           :: previous                     ! local
      type(PGROUP),pointer            :: current_pgroup               ! local
      type(PGROUP),pointer            :: loop_start_pgroup            ! local
      type(PGROUP),pointer            :: start_pgroup                 ! local
      type(PGROUP),pointer            :: last_pgroup                  ! local
      logical                         :: first                        ! local
      logical                         :: null_action                  ! local
      logical                         :: in_fork                      ! local
      logical                         :: do_parallel                  ! local
      logical                         :: need_extra_io                ! local
      logical                         :: found                        ! local
      integer                         :: label                        ! local
      integer                         :: last_label      = 0          ! local
      integer                         :: last_fork       = 0          ! local
      integer                         :: last_loop       = 0          ! local
      type(CARD_POINTER)              :: tag_card                     ! local
      character(len=PCPSNAME_MAX)     :: pcps_send_mode               ! local
      character(len=PCPSNAME_MAX)     :: pcps_receive_mode            ! local
      character(len=PCPSNAME_MAX)     :: pcps_send_eof_mode           ! local
      character(len=PCPSNAME_MAX)     :: pcps_alt_send_mode           ! local
      character(len=PCPSNAME_MAX)     :: pcps_alt_receive_mode        ! local
      character(len=PCPSNAME_MAX)     :: pcps_resequence_mode         ! local
      character(len=PCPSNAME_MAX)     :: pcps_generator_mode          ! local
      character(len=PCPSNAME_MAX)     :: pcps_bunch_mode              ! local
      character(len=PCPSNAME_MAX)     :: pcps_boss_exec_mode          ! local
      character(len=TAG_MAX_LEN)      :: tags(NUM_TAGS)               ! local
      character(len=PCPSNAME_MAX+1)   :: pblank                       ! local
      character(len=80)               :: ctmp                         ! local
      integer                         :: nctmp                        ! local
      character(len=80)               :: ctmp2                        ! local
      integer                         :: nctmp2                       ! local
      character(len=80)               :: ctmp3                        ! local
      integer                         :: nctmp3                       ! local
      character(len=80)               :: ctmp4                        ! local
      integer                         :: nctmp4                       ! local
      character(len=80)               :: ctmp5                        ! local
      integer                         :: nctmp5                       ! local
      integer                         :: nwih                         ! local
      integer                         :: ndpt                         ! local
      integer                         :: ntr                          ! local
      integer                         :: nscratch_max      = 0        ! local
      integer                         :: nstore_max        = 0        ! local
      integer                         :: ndisk_max         = 0        ! local
      integer                         :: ncart_max         = 0        ! local
      integer                         :: nmag_max          = 0        ! local
      integer                         :: nloops            = 0        ! local
      integer                         :: n_io_pairs        = 0        ! local
      integer                         :: n_io_pairs_total  = 0        ! local
      integer                         :: nstore_loop       = 0        ! local
      integer                         :: nscratch_loop     = 0        ! local
      integer                         :: n_do_parallel     = 0        ! local
      integer                         :: n_parallel_groups = 0        ! local
      integer                         :: n_parallel_groups_start = 0  ! local
      integer                         :: n_parallel_groups_end = 0    ! local
      integer                         :: i1                           ! local
      integer                         :: i2                           ! local
      integer                         :: ntr_x(MAX_IO_PAIRS)          ! local
      integer                         :: hd_x_len(MAX_IO_PAIRS)       ! local
      integer                         :: tr_x_len(MAX_IO_PAIRS)       ! local
      integer                         :: tr_parallel(MAX_IO_PAIRS)    ! local

      do i=1,PCPSNAME_MAX+1
        pblank(i:i) = ' '
      enddo

      nullify(start_pgroup)
      nullify(last_pgroup)

      tr_parallel      = 0
      ntr_x            = 0
      hd_x_len         = 0
      tr_x_len         = 0

      nwih             = 0
      ndpt             = 0
      ntr              = 0
      label            = 0
      i                = 0
      in_fork          = .false.
      nullify(previous)
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        i = i + 1
        write(STDOUT,*) 'process #',i,' = ',current%name(1:current%nname)
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        call pc_get_control ('PARALLEL_SAFE'       ,current%parallel_safe      )
        call pc_get_control ('PCPS_SEND_MODE'      ,current%pcps_send_mode     )
        call pc_get_control ('PCPS_RECEIVE_MODE'   ,current%pcps_receive_mode  )
        call pc_get_control ('PCPS_SEND_EOF_MODE'  ,current%pcps_send_eof_mode )
        call pc_get_control ('PCPS_ALT_SEND_MODE'  ,current%pcps_alt_send_mode )
        call pc_get_control ('PCPS_ALT_RECEIVE_MODE',  &
                                                  current%pcps_alt_receive_mode)
        call pc_get_control ('PCPS_RESEQUENCE_MODE' ,  &
                                                  current%pcps_resequence_mode )
        call pc_get_control ('PCPS_GENERATOR_MODE' ,current%pcps_generator_mode)
        call pc_get_control ('PCPS_BUNCH_MODE'     ,current%pcps_bunch_mode    )
        call pc_get_control ('PCPS_BOSS_EXEC_MODE' ,current%pcps_boss_exec_mode)
        call pc_get_control ('NEED_REQUEST'        ,current%need_request       )
        call pc_get_control ('NEED_LABEL'          ,current%need_label         )
        call pc_get_control ('TWOSETS'             ,current%twosets            )
        call pc_get_control ('SETUP_ONLY'          ,current%setup_only         )
        call pc_get_control ('SIGNAL'              ,current%signal_handler     )
        call pc_get_control ('NSCRATCH'            ,current%nscratch           )
        call pc_get_control ('NSTORE'              ,current%nstore             )
        call pc_get_control ('NDISK'               ,current%ndisk              )
        call pc_get_control ('IFTD'                ,current%iftd               )

        call string_to_upper (current%parallel_safe       )
        call string_to_upper (current%need_request        )
        call string_to_upper (current%need_label          )
        call string_to_upper (current%twosets             )
        call string_to_upper (current%setup_only          )
        call string_to_upper (current%iftd                )

        if (current%setup_only .eq. 'YES') then
         current%need_request = 'NO'
         current%need_label   = 'NO'
         current%twosets      = 'NO'
        endif

        tag_card%card => current%tag_card_start(GLOBALS_TAG)%card
        first   = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_global_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_global_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        current%ltr  = ndpt
        current%lhd  = nwih

        if (pc_global_keyword_present('NWIH'))  call pc_get_global('NWIH' ,nwih)
        if (pc_global_keyword_present('NDPT'))  call pc_get_global('NDPT' ,ndpt)
        if (pc_global_keyword_present('NUMTR')) call pc_get_global('NUMTR',ntr )
        current%ntr  = ntr

        if (current%twosets(1:1) .eq. 'Y') then
          current%ltr2 = ndpt
          current%lhd2 = nwih
        else
          current%ltr  = ndpt
          current%lhd  = nwih
        endif

        nscratch_max = max(nscratch_max , current%nscratch)
        nstore_max   = nstore_max + current%nstore

        if (current%setup_only .eq. 'YES') then
          current  => current%next
          cycle
        endif

        if ((nloops .eq. 0 .and. current%ntr .ne. 0) .or.                     &
            (current%need_label(1:1) .eq. 'Y' .and.                           &
             current%need_request(1:1) .eq. 'N')) then
          nloops = nloops + 1
          current%start_loop = .TRUE.
          n_io_pairs = n_io_pairs + 1
        endif

        if (current%parallel_safe(1:1) .eq. 'Y') then
          if (n_parallel_groups .eq. 0) then
            n_parallel_groups      = n_parallel_groups + 1
            current%parallel_group = n_parallel_groups
            allocate(current_pgroup)
            current_pgroup%job_loop              = nloops
            current_pgroup%pcps_send_mode        = current%pcps_send_mode
            current_pgroup%pcps_receive_mode     = current%pcps_receive_mode
            current_pgroup%pcps_send_eof_mode    = current%pcps_send_eof_mode
            current_pgroup%pcps_alt_send_mode    = current%pcps_alt_send_mode
            current_pgroup%pcps_alt_receive_mode = current%pcps_alt_receive_mode
            current_pgroup%pcps_resequence_mode  = current%pcps_resequence_mode
            current_pgroup%pcps_generator_mode   = current%pcps_generator_mode
            current_pgroup%pcps_bunch_mode       = current%pcps_bunch_mode
            current_pgroup%pcps_boss_exec_mode   = current%pcps_boss_exec_mode
            nullify(current_pgroup%next)
            if (.not. associated(last_pgroup)) then
              start_pgroup => current_pgroup
            else
              last_pgroup%next => current_pgroup
            endif
            last_pgroup => current_pgroup
          else
            if (.not. in_fork  &
                .or.           &
                current_pgroup%pcps_send_mode   .ne.current%pcps_send_mode  &
                .or.           &
                current_pgroup%pcps_receive_mode.ne.current%pcps_receive_mode  &
                .or.           &
                current_pgroup%pcps_resequence_mode.ne.'PCPS_NO_RESEQUENCE')then
              if (in_fork) then
                label = label + 10           ! Label for do_parallel_end
                current%parallel_end = label
                in_fork = .false.
                if (need_extra_io) then
                  n_io_pairs = n_io_pairs + 1
                  tr_x_len(n_io_pairs) = tr_x_len(n_io_pairs - 1)
                  hd_x_len(n_io_pairs) = hd_x_len(n_io_pairs - 1)
                  ntr_x(n_io_pairs)    = ntr_x(n_io_pairs - 1)
                endif
                tr_parallel(n_do_parallel) = n_io_pairs
              endif
              need_extra_io = .true.
              n_parallel_groups      = n_parallel_groups + 1
              current%parallel_group = n_parallel_groups
              in_fork                = .false.
              allocate(current_pgroup)
              current_pgroup%job_loop             = nloops
              current_pgroup%pcps_send_mode       = current%pcps_send_mode
              current_pgroup%pcps_receive_mode    = current%pcps_receive_mode
              current_pgroup%pcps_send_eof_mode   = current%pcps_send_eof_mode
              current_pgroup%pcps_alt_send_mode   = current%pcps_alt_send_mode
              current_pgroup%pcps_alt_receive_mode=current%pcps_alt_receive_mode
              current_pgroup%pcps_resequence_mode = current%pcps_resequence_mode
              current_pgroup%pcps_generator_mode  = current%pcps_generator_mode
              current_pgroup%pcps_bunch_mode      = current%pcps_bunch_mode
              current_pgroup%pcps_boss_exec_mode  = current%pcps_boss_exec_mode
              nullify(current_pgroup%next)
              if (.not. associated(last_pgroup)) then
                start_pgroup => current_pgroup
              else
                last_pgroup%next => current_pgroup
              endif
              last_pgroup => current_pgroup
            endif
          endif
          if (.not. in_fork) then
            current%start_fork    = .true.
            label = label + 10                   ! Label for do_parallel_begin
            current%parallel_begin = label
            n_do_parallel = n_do_parallel + 1
            in_fork = .true.
          else
            current_pgroup%pcps_receive_mode     = current%pcps_receive_mode
            current_pgroup%pcps_alt_receive_mode = current%pcps_alt_receive_mode
          endif
          if (current%pcps_resequence_mode.ne.'PCPS_NO_RESEQUENCE' .and.  &
              current_pgroup%pcps_resequence_mode.eq.'PCPS_NO_RESEQUENCE')  &
              current_pgroup%pcps_resequence_mode = current%pcps_resequence_mode
          if (current%pcps_generator_mode  .eq. 'PCPS_TRACE_GEN'   )  &
            current_pgroup%pcps_generator_mode  = 'PCPS_TRACE_GEN'
        else                             ! Not parallel safe
          if (in_fork) then
            label = label + 10           ! Label for do_parallel_end
            current%parallel_end = label
            in_fork = .false.
            if (need_extra_io) then
              n_io_pairs = n_io_pairs + 1
              tr_x_len(n_io_pairs) = tr_x_len(n_io_pairs - 1)
              hd_x_len(n_io_pairs) = hd_x_len(n_io_pairs - 1)
              ntr_x(n_io_pairs)    = ntr_x(n_io_pairs - 1)
            endif
            tr_parallel(n_do_parallel) = n_io_pairs
          endif
          need_extra_io = .true.
        endif

        if (current%need_request(1:1) .eq. 'Y') then
          current%request = label
        endif

        if (current%need_label(1:1) .eq. 'Y') then
          label = label + 10
          current%label = label
        endif

        if (current%twosets(1:1) .eq. 'Y') then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
          n_io_pairs = n_io_pairs + 1
          tr_x_len(n_io_pairs) = current%ltr2
          hd_x_len(n_io_pairs) = current%lhd2
          if (in_fork) need_extra_io = .false.
        else if (current%ntr.ne.0) then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
        endif

        if (current%ntr.gt.0)  then
          if (current%start_loop) then
            ntr_x(n_io_pairs) = current%ntr
          else
            ntr_x(n_io_pairs) = max(ntr_x(n_io_pairs),current%ntr)
          endif
        endif
        current%n_io_pairs = n_io_pairs
        previous => current
        current  => current%next
      enddo

      if (in_fork) then              ! Still in fork when out of processes
        in_fork = .false.
        if (need_extra_io) then
          n_io_pairs = n_io_pairs + 1
          tr_x_len(n_io_pairs) = tr_x_len(n_io_pairs - 1)
          hd_x_len(n_io_pairs) = hd_x_len(n_io_pairs - 1)
          ntr_x(n_io_pairs)    = ntr_x(n_io_pairs - 1)
        endif
        tr_parallel(n_do_parallel) = n_io_pairs
        previous%n_io_pairs = n_io_pairs
      endif
      need_extra_io = .true.

      total_memory = 5242880 + nscratch_max + nstore_max

      write(junit,'(A)') &
      '! *************** Start of CPS program ' // trim(jobname)//' ***************'
      write(junit,'(A)') &
      '! ********************** COPYRIGHT NOTICE **********************************'
      write(junit,'(A)') &
      '! Created by OpenCPS as a derivitive work'
      write(junit,'(A)') &
      '! The OpenCPS system is Licensed by ConocoPhillips 2007 using the'
      write(junit,'(A)') &
      '! MIT-style Open-source license at http://cpseis.org/docs/CPSeis_License.pdf'
      write(junit,'(A)') &
      '! **************************************************************************'
      write(junit,'(A)') &
      '! This Program was created from work file '//trim(jobname)//'.wrk using'
      write(junit,'(A)') &
      '! the program cfebld.  See buildjob.f90 for the engine behind cfebld.'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!! GLOBAL DATA MODULE !!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      module program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      use pcps_module'
      write(junit,'(A)') &
      '      use pcpsx_module'
      write(junit,'(A)') &
      '      use named_constants_module'
      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      use ' // current%name(1:current%nname) // '_module'
        current => current%next
      enddo
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      implicit none'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      public'
      do i=1,n_parallel_groups
        call string_ii2cc (i,ctmp)
        write(junit,'(A)') &
      '      type(PCPSX_DO_PARALLEL_struct),pointer :: p_obj' // trim(ctmp)
      enddo

      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      type(' // current%name(1:current%nname) //      &
        '_struct),pointer  ' // pblank(1:16-current%nname) //  &
        ':: obj' // current%cpid(1:current%ncpid)
        current => current%next
      enddo

      write(junit,'(A)') &
      '      end module program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!! PROGRAM MAIN !!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      program '//trim(jobname)//'_cps'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      use program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!! DECLARATIONS:'
      write(junit,'(A)') &
      '! '

      do i = 1,n_io_pairs
        write(ctmp,'(''tr'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp,nctmp)
        write(ctmp2,'(''hd'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp2,nctmp2)
        write(junit,'(A)') &
        '      double precision, pointer :: ' // ctmp2(1:nctmp2)
        write(junit,'(A)') &
        '      real            , pointer :: ' // ctmp(1:nctmp)
      enddo

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      integer :: ntr, ierr'
      write(junit,'(A)') &
      '      logical :: error'

      write(junit,'(A)') &
      '! '

      do i=1,n_parallel_groups
        call string_ii2cc (i,ctmp)
        write(junit,'(A)') '      nullify (p_obj'//trim(ctmp)//')'
      enddo

      write(junit,'(A)') &
      '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)')   &
          '      nullify (obj'//current%cpid(1:current%ncpid)//')'
        current => current%next
      enddo

      write(junit,'(A)') &
      '! '

      write(junit,'(A)') &
      '      call pcpsx_start_processing'

      write(junit,'(A)') &
      '! '

      write(junit,'(A)') &
      '      call pcpsx_print_rcs_ident (trim(PCPS_ident))'
      write(junit,'(A)') &
      '      call pcpsx_print_rcs_ident (trim(PCPSX_ident))'
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      call pcpsx_print_rcs_ident '//  &
        '(trim('//current%name(1:current%nname)//'_ident))'
        current => current%next
      enddo
      write(junit,'(A)') '      call pcpsx_custom_rcs_ident'

      n_io_pairs_total = n_io_pairs
      n_io_pairs       = 0
      loop_start_process => start
      n_parallel_groups = 0
      loop_start_pgroup => start_pgroup
      do i = 1,max(nloops,1)
        n_io_pairs = n_io_pairs + 1
        if (.not. associated(loop_start_process)) exit
        current => loop_start_process
        do_parallel = .false.
        if (i .gt. 1) then
          write(junit,'(A)') '!'
          write(junit,'(A)') '      call pcpsx_restart_processing()'
          write(junit,'(A)') '!'
        endif
        do
          if (.not. associated(current)) exit
          if (trim(current%parallel_safe).eq.'YES' .and. .not.do_parallel)then
            do_parallel = .true.
            write(junit,'(A)') &
            '! '
            write(junit,'(A)') &
            '!!!!! ALLOCATE ARRAYS:'
            write(junit,'(A)') &
            '! '
            j1 = max(loop_start_process%n_io_pairs,1)
            j2 = j1
            current => loop_start_process
            first = .TRUE.
            do
              if (.not. associated(current)) exit
              if (current%start_loop) then
                if (first) then
                  first = .FALSE.
                else
                  exit
                endif
              endif
              j2 = current%n_io_pairs
              current => current%next
            enddo
            current => loop_start_process
            do j = j1,j2
              call string_ii2cc (j          , ctmp )
              call string_ii2cc (ntr_x(j)   , ctmp2)
              call string_ii2cc (hd_x_len(j), ctmp3)
              call string_ii2cc (tr_x_len(j), ctmp4)
              do k = 1, n_do_parallel
                if (tr_parallel(k) .eq. j) then
                  found = .true.
                  exit
                endif
              enddo
              if (found .and.  &
                 (trim(current%pcps_receive_mode).eq.'PCPS_RECEIVE_GROUP'.or.  &
                  trim(current%pcps_alt_receive_mode).eq.  &
                                                     'PCPS_RECEIVE_GROUP'.or.  &
                  trim(current%pcps_send_mode).eq.'PCPS_BOSS_EXECS_GATHER'.or. &
                  trim(current%pcps_alt_send_mode).eq.  &
                                                 'PCPS_BOSS_EXECS_GATHER')) then
                write(junit,'(A)')  &
               '      ntr = '//trim(ctmp2)//' * max(1,pcps_get_num_workers())'
              else
                write(junit,'(A)')  &
                '      ntr = '//trim(ctmp2)
              endif
              write(junit,'(A)')  &
              '      allocate(hd'//trim(ctmp)//'('//trim(ctmp3)//','//  &
                              'ntr),stat=ierr)'
              write(junit,'(A)')  &
              '      if (ierr /= 0) then'
              write(junit,'(A)')  &
           '        call pcps_print (''Unable to allocate hd'//trim(ctmp)//''')'
              write(junit,'(A)')  &
              '        error = .true.'
              write(junit,'(A)')  &
              '        goto 9999'
              write(junit,'(A)')  &
              '      endif'
              write(junit,'(A)')  &
              '      allocate(tr'//trim(ctmp)//'('//trim(ctmp4)//','//  &
                              'ntr),stat=ierr)'
              write(junit,'(A)')  &
              '      if (ierr /= 0) then'
              write(junit,'(A)')  &
           '        call pcps_print (''Unable to allocate tr'//trim(ctmp)//''')'
              write(junit,'(A)')  &
              '        error = .true.'
              write(junit,'(A)')  &
              '        goto 9999'
              write(junit,'(A)')  &
              '      endif'
              write(junit,'(A)') &
              '! '
            enddo
          endif
          current => current%next
        enddo

        write(junit,'(A)') &
        '!!!!! SETUPS:'
        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '      call pcpsx_start_setups    ! stops if there is an error.'
        write(junit,'(A)') &
        '! '

        first = .TRUE.
        do_parallel = .false.
        current_pgroup => loop_start_pgroup
        j = n_parallel_groups
        n_parallel_groups_start = j + 1
        do
          if (.not. associated(current_pgroup)) exit
          if (current_pgroup%job_loop .gt. i) then
            loop_start_pgroup => current_pgroup
            exit
          endif
          j = j + 1
          call string_ii2cc (j,ctmp)
          write(junit,'(A)') '      call pcpsx_do_parallel_create (p_obj'//  &
                                    trim(ctmp)//',error)'
          if (trim(current_pgroup%pcps_send_mode).eq.'PCPS_BOSS_EXECS'   .or.  &
              trim(current_pgroup%pcps_send_mode).eq.'PCPS_BOSS_EXECS_GATHER'  &
                                                                         .or.  &
              trim(current_pgroup%pcps_send_mode).eq.'PCPS_BOSS_EXECS_MERGE')  &
                                                                            then
            write(junit,'(A)')  &
                        '      call pcps_set_send_mode       ('//         &
                               trim(current_pgroup%pcps_send_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                   &
                               len_trim(current_pgroup%pcps_send_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_generator_mode  ('//              &
                               trim(current_pgroup%pcps_generator_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_generator_mode))// &
                               ',error)'
          else
            write(junit,'(A)')  &
                        '      call pcps_set_send_mode       ('//         &
                               trim(current_pgroup%pcps_send_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                   &
                               len_trim(current_pgroup%pcps_send_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_receive_mode    ('//             &
                               trim(current_pgroup%pcps_receive_mode)//       &
                               pblank(1:PCPSNAME_MAX+1-                       &
                               len_trim(current_pgroup%pcps_receive_mode))//  &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_send_eof_mode   ('//             &
                               trim(current_pgroup%pcps_send_eof_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                       &
                               len_trim(current_pgroup%pcps_send_eof_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_alt_send_mode   ('//              &
                               trim(current_pgroup%pcps_alt_send_mode)//       &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_alt_send_mode))//  &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_alt_receive_mode('//              &
                               trim(current_pgroup%pcps_alt_receive_mode)//    &
                               pblank(1:PCPSNAME_MAX+1-                        &
                              len_trim(current_pgroup%pcps_alt_receive_mode))//&
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_resequence_mode ('//              &
                               trim(current_pgroup%pcps_resequence_mode)//     &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_resequence_mode))//&
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_generator_mode  ('//              &
                               trim(current_pgroup%pcps_generator_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_generator_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_bunch_mode      ('//           &
                               trim(current_pgroup%pcps_bunch_mode)//       &
                               pblank(1:PCPSNAME_MAX+1-                     &
                               len_trim(current_pgroup%pcps_bunch_mode))//  &
                               ',error)'
           if(trim(current_pgroup%pcps_boss_exec_mode).ne.' ')then
            write(junit,'(A)')  &
                        '      call pcps_set_boss_exec_mode  ('//           &
                               trim(current_pgroup%pcps_boss_exec_mode)//   &
                               pblank(1:PCPSNAME_MAX+1-                     &
                               len_trim(current_pgroup%pcps_boss_exec_mode))//&
                               ',error)'
           endif
          endif
          write(junit,'(A)') '      call pcpsx_do_parallel_init   (p_obj'//  &
                                    trim(ctmp)//',error)'
          write(junit,'(A)') '      if (error) goto 9999'
          write(junit,'(A)') '! '
          current_pgroup => current_pgroup%next
        enddo
        n_parallel_groups_end = j

        current => loop_start_process
        first   = .TRUE.
        need_extra_io = .true.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          if (current%parallel_safe(1:1) .eq. 'Y') then
            if (trim(current%pcps_send_mode) .eq. 'PCPS_BOSS_EXECS' .or.  &
                trim(current%pcps_send_mode) .eq. 'PCPS_BOSS_EXECS_GATHER' .or.&
                trim(current%pcps_send_mode) .eq. 'PCPS_BOSS_EXECS_MERGE') then
              write(junit,'(A)') &
              '      call pcpsx_pre_setup     (PCPS_BOSS_PARALLEL)'
            else
              write(junit,'(A)') &
              '      call pcpsx_pre_setup     (PCPS_INSIDE_PARALLEL)'
            endif
          else
            write(junit,'(A)') &
            '      call pcpsx_pre_setup     (PCPS_OUTSIDE_PARALLEL)'
          endif
          write(junit,'(A)') &
          '      call ' // current%name(1:current%nname) //&
          '_create'// pblank(1:PNAME_MAX+1-current%nname) // '(obj' //  &
          current%cpid(1:current%ncpid) // ')'
          write(junit,'(A)') '      call pcpsx_post_setup'
            write(junit,'(A)') '! '
          current => current%next
        enddo
        write(ctmp,'(I10)') i + 9000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        '      call pcpsx_finish_setups (error); if (error) go to '//trim(ctmp)
        write(junit,'(A)') &
        '! '
        if (nloops .gt. 0) then

          write(junit,'(A)') &
          '!!!!! EXECUTION:'
          write(junit,'(A)') &
          '! '
          write(junit,'(A)') &
          '      ntr = NEED_TRACES'
          write(junit,'(A)') &
          '! '

          nullify(previous)
          current => loop_start_process
          first             = .true.
          in_fork           = .false.
          null_action       = .false.
          n_do_parallel     = n_parallel_groups_start - 1
          do
            if (.not. associated(current)) exit
            if (current%start_loop) then
              if (first) then
                first = .FALSE.
              else
                exit
              endif
            endif
            if (current%setup_only.ne.'YES') then
              if (current%parallel_safe(1:1) .eq. 'Y') then
                if (in_fork .and. current%start_fork) then
                  write(ctmp,'(I10)') current%parallel_end
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups,ctmp2)
                  call string_ii2cc (tr_parallel(n_do_parallel),ctmp3)
                  call string_ii2cc (n_io_pairs,ctmp4)
                  if (.not. need_extra_io) then
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp4)//&
                  ',tr'//trim(ctmp4)//')'
                  else
                    n_io_pairs = n_io_pairs + 1
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//    &
                  ',tr'//trim(ctmp3)//',hd'//trim(ctmp4)//    &
                  ',tr'//trim(ctmp4)//')'
                  endif
                  call string_ii2cc (last_fork,ctmp)
                  write(junit,'(A)') &
                  '      if (ntr == LOOP_BACK) goto ' //  trim(ctmp)
                  write(ctmp,'(I10)') i + 8000
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
                  write(junit,'(A)') '!'
                  last_fork  = current%parallel_begin
                  last_label = current%parallel_end
                  in_fork = .false.
                  need_extra_io = .true.
                endif
                if (.not. in_fork) then
                  n_do_parallel     = n_do_parallel     + 1
                  n_parallel_groups = n_parallel_groups + 1
                  in_fork = .true.
                  if (null_action) then
                    write(junit,'(A)') '      endif'
                    write(junit,'(A)') '! '
                    null_action = .false.
                  endif
                  write(ctmp,'(I10)') current%parallel_begin
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups,ctmp2)
                  call string_ii2cc (n_io_pairs,ctmp3)
                  call string_ii2cc (tr_parallel(n_do_parallel),ctmp4)
                  write(junit,'(A)') &
                ' ' // ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_begin ('//&
                'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//',tr'//&
                trim(ctmp3)//',hd'//trim(ctmp4)//',tr'//trim(ctmp4)//')'
                  call string_ii2cc (last_label,ctmp)
                  write(junit,'(A)') &
                  '      if (ntr == LOOP_BACK) goto ' // trim(ctmp)
                  write(ctmp,'(I10)') i + 8000
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
                  write(junit,'(A)') '!'
                  last_fork  = current%parallel_begin
                  last_label = max(current%label,current%parallel_begin)
                endif
              else
                if (in_fork) then
                  write(ctmp,'(I10)') current%parallel_end
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups,ctmp2)
                  call string_ii2cc (tr_parallel(n_do_parallel),ctmp3)
                  call string_ii2cc (n_io_pairs,ctmp4)
                  if (.not. need_extra_io) then
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp4)//&
                  ',tr'//trim(ctmp4)//')'
                  else
                    n_io_pairs = n_io_pairs + 1
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//    &
                  ',tr'//trim(ctmp3)//',hd'//trim(ctmp4)//    &
                  ',tr'//trim(ctmp4)//')'
                  endif
                  call string_ii2cc (last_fork,ctmp)
                  write(junit,'(A)') &
                  '      if (ntr == LOOP_BACK) goto ' //  trim(ctmp)
                  write(ctmp,'(I10)') i + 8000
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
                  write(junit,'(A)') '!'
                  last_fork  = current%parallel_end
                  last_label = max(current%label,current%parallel_end)
                  in_fork = .false.
                  need_extra_io = .true.
                endif
              endif
              if (current%need_label(1:1) .eq. 'Y') then
                if (current%start_loop) then
                  write(ctmp,'(I10)') current%label
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups+1,ctmp2)
                  write(junit,'(A)') &
                  ' ' // ctmp(1:max(nctmp,5))//'call pcpsx_start_loop ('// &
                  'p_obj' // trim(ctmp2) // ',ntr)'
                  last_loop  = current%label
                  last_fork  = 0
                  last_label = max(current%label,current%parallel_end)
                else
                  write(ctmp,'(I10)') current%label
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  ' ' // ctmp(1:max(nctmp,5)) // 'continue'
                  last_label = max(current%label,current%parallel_end)
                endif
              endif
              if (current%twosets(1:1) .eq. 'Y') then
                if (in_fork) need_extra_io = .false.
                if (.not. null_action) then
                  write(junit,'(A)') '      if (ntr /= NULL_ACTION) then'
                  null_action = .true.
                endif
                if (current%need_request(1:1) .eq. 'Y') then
                  call string_ii2cc (current%request,ctmp)
                  write(junit,'(A)') '        call pcpsx_filter_ntr (' //  &
                                                   trim(current%cpid) // ',ntr)'
                  write(junit,'(A)') '        if (ntr == LOOP_BACK) goto ' //  &
                                                trim(ctmp)
                endif
                write(junit,'(A)') '        call pcpsx_pre_process  (' //    &
                                                       trim(current%cpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                n_io_pairs = n_io_pairs + 1
                write(ctmp2,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp2,nctmp2)
                write(junit,'(A)') &
                '        call ' // current%name(1:current%nname) //            &
                pblank(1:PNAME_MAX+1-current%nname) // '      (obj' //         &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ',hd' // ctmp2(1:nctmp2) // ',tr' // &
                ctmp2(1:nctmp2) // ')'
                write(junit,'(A)') &
                '        call pcpsx_post_process (' //                  &
                current%cpid(1:current%ncpid) // ',ntr,hd' //         &
                ctmp2(1:nctmp2) // ',tr' // ctmp2(1:nctmp2) // ')'
              else
                if (.not. null_action) then
!                 write(junit,'(A)') '! '
                  write(junit,'(A)') '      if (ntr /= NULL_ACTION) then'
                  null_action = .true.
                endif
                if (current%need_request(1:1) .eq. 'Y') then
                  call string_ii2cc (current%request,ctmp)
                  write(junit,'(A)') '        call pcpsx_filter_ntr (' //  &
                                                   trim(current%cpid) // ',ntr)'
                  write(junit,'(A)') '        if (ntr == LOOP_BACK) goto ' //  &
                                                trim(ctmp)
                endif
                write(junit,'(A)') &
                '        call pcpsx_pre_process  (' //    &
                current%cpid(1:current%ncpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '        call ' // current%name(1:current%nname) //            &
                pblank(1:PNAME_MAX+1-current%nname) // '      (obj' //         &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ')'
                write(junit,'(A)') &
                '        call pcpsx_post_process (' //              &
                current%cpid(1:current%ncpid) // ',ntr,hd' //     &
                ctmp(1:nctmp) // ',tr' // ctmp(1:nctmp) // ')'
              endif
              if (current%need_request(1:1) .eq. 'Y') then
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                if (.not. in_fork) then
                  write(ctmp2,'(I10)') current%request
                  call string_strip_blanks(ctmp2,nctmp2)
                  write(junit,'(A)') &
                  '      if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp) //  &
                     ';  if (ntr == NEED_TRACES) goto ' // ctmp2(1:nctmp2)
                else
                  write(ctmp4,'(I10)') last_fork
                  call string_strip_blanks(ctmp4,nctmp4)
                  write(junit,'(A)') &
                  '        if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp)
                  write(junit,'(A)') &
                  '        if (ntr == NEED_TRACES) then'
                  call string_ii2cc (n_parallel_groups,ctmp)
                  call string_ii2cc (n_io_pairs,ctmp3)
                  write(junit,'(A)') &
                  '          call pcpsx_do_parallel_end ('//&
                  'p_obj' // trim(ctmp) // ',ntr,hd'//trim(ctmp3)//&
                  ',tr'//trim(ctmp3)//')'
                  write(junit,'(A)') &
                  '          goto ' // ctmp4(1:nctmp4)
                  write(junit,'(A)') &
                  '        endif'
                endif
                if (null_action) then
                  write(junit,'(A)') '      endif'
                  write(junit,'(A)') '!'
                  null_action = .false.
                else
                  write(junit,'(A)') '!'
                endif
              else
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '        if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp)
                if (null_action) then
                  write(junit,'(A)') '      endif'
                  write(junit,'(A)') '!'
                  null_action = .false.
                else
                  write(junit,'(A)') '!'
                endif
              endif
            endif
            previous  => current
            current   => current%next
          enddo

          if (in_fork) then
!           write(ctmp,'(I10)') current%parallel_end
            last_label = last_label + 10
            write(ctmp,'(I10)') last_label
            call string_strip_blanks(ctmp,nctmp)
            call string_ii2cc (n_parallel_groups,ctmp2)
            call string_ii2cc (tr_parallel(n_do_parallel),ctmp3)
            call string_ii2cc (n_io_pairs,ctmp4)
            if (.not. need_extra_io) then
              write(junit,'(A)')  &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp4)//&
                  ',tr'//trim(ctmp4)//')'
            else
              n_io_pairs = n_io_pairs + 1
              write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//    &
                  ',tr'//trim(ctmp3)//',hd'//trim(ctmp4)//    &
                  ',tr'//trim(ctmp4)//')'
            endif
            call string_ii2cc (last_fork,ctmp)
            write(junit,'(A)') &
                            '      if (ntr == LOOP_BACK) goto ' //  trim(ctmp)
            write(ctmp,'(I10)') i + 8000
            call string_strip_blanks(ctmp,nctmp)
            write(junit,'(A)') &
                            '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
            write(junit,'(A)') '!'
!           last_fork  = current%parallel_end
!           last_label = max(current%label,current%parallel_end)
            in_fork = .false.
            need_extra_io = .true.
          endif

          write(ctmp,'(I10)') i + 7000
          call string_strip_blanks(ctmp,nctmp)
          call string_ii2cc (n_parallel_groups,ctmp2)
          write(junit,'(A)') &
          ' '//ctmp(1:max(nctmp,5))//'call pcpsx_end_loop (p_obj'//trim(ctmp2) &
          //',ntr)'
          write(ctmp,'(I10)') last_label
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      if (ntr == LOOP_BACK) goto ' // ctmp(1:nctmp)
          write(ctmp,'(I10)') last_loop
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      if (ntr == GO_BACK) goto ' // ctmp(1:nctmp)
          write(junit,'(A)') &
          '      if (ntr /= NO_MORE_TRACES) error = .true.'
          write(junit,'(A)') &
          '! '
          current => loop_start_process
          first = .TRUE.
        endif

        write(junit,'(A)') &
        '!!!!! DELETE:'
        write(junit,'(A)') &
        '! '
        write(ctmp,'(I10)') i + 8000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        ' ' //ctmp(1:max(nctmp,5)) // 'continue'

        write(junit,'(A)') &
        '      call pcpsx_wrapup_processing'

        do j=n_parallel_groups_start,n_parallel_groups_end
           call string_ii2cc (j,ctmp)
           write(junit,'(A)') &
          '      call pcpsx_do_parallel_delete (p_obj'// trim(ctmp) // ')'
        enddo
        write(junit,'(A)') &
        '! '

        write(ctmp,'(I10)') i + 9000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        ' ' //ctmp(1:max(nctmp,5)) // 'continue'

        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          write(junit,'(A)') &
          '      if (associated(obj'//current%cpid(1:current%ncpid)//  &
          '))'//pblank(1:max_pid_digits+1-current%ncpid)           //  &
          'call ' // current%name(1:current%nname) // '_delete'    //  &
          pblank(1:PNAME_MAX+1-current%nname) // '(obj' //             &
          current%cpid(1:current%ncpid) // ')'
          current => current%next
        enddo

        write(junit,'(A)') &
        '! '
        do j = j1,j2
          call string_ii2cc (j, ctmp )
          write(junit,'(A)')  &
          '      deallocate(hd'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
         '        call pcps_print (''Unable to deallocate hd'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)')  &
          '      deallocate(tr'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
         '        call pcps_print (''Unable to deallocate tr'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)') &
          '! '
        enddo
        write(junit,'(A)') &
        '      if (error) go to 9999'

        if (.not. loop_start_process%start_loop) then
          do
            loop_start_process => loop_start_process%next
            if (.not. associated(loop_start_process)) exit
            if (loop_start_process%start_loop) exit
          enddo
          if (.not. associated(loop_start_process)) exit
        endif

        loop_start_process => loop_start_process%next
        do
          if (.not. associated(loop_start_process)) exit
          if (loop_start_process%start_loop) exit
          loop_start_process => loop_start_process%next
        enddo
      enddo

      write(junit,'(A)') &
      ' 9999 continue'
      write(junit,'(A)') &
      '      call pcpsx_finish_processing (error)'

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      end program '//trim(jobname)//'_cps'


      write(junit,'(A)') '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!! SIGNAL HANDLER !!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      subroutine cpssig_shutdown (signal)'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      use program_global_data'
      write(junit,'(A)') '      use cps_module'
      write(junit,'(A)') '      use cio_module'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      integer,intent(in) :: signal          !argument'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      integer,parameter  :: kill_sig=12     !SIGUSR2'
      write(junit,'(A)') '      integer            :: lun             !local'
      write(junit,'(A)') '      logical,save       :: flag0 = .true.  !local'

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      logical,save       :: flag'//trim(current%cpid)//  &
        ' = .true.  !local'
        current => current%next
      enddo

      write(junit,'(A)') '! '

      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        call cps_write_accounting_signal (signal)'
      write(junit,'(A)') '        call cps_print_current_status()'
      write(junit,'(A)') '      endif'

      write(junit,'(A)') '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      if (flag'//trim(current%cpid)//') then'
        write(junit,'(A)') &
        '        flag'//trim(current%cpid)//' = .false.'
        write(junit,'(A)') &
        '        if (associated(obj'//current%cpid(1:current%ncpid)//')) then'
        write(junit,'(A)') &
        '          call cpssig_print ("'//current%name(1:current%nname)// &
                   '",'//current%cpid(1:current%ncpid)//')'
        if (current%signal_handler .eq. 'YES') then
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//',signal=signal)'
        else
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//')'
        endif
        write(junit,'(A)') &
        '        endif'
        write(junit,'(A)') &
        '      endif'
        write(junit,'(A)') &
        '! '
        current => current%next
      enddo
      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        flag0 = .false.'
      write(junit,'(A)') '        if (pcps_current_worker_num.eq.0) &'
      write(junit,'(A)') '          call cps_finish_processing (signal=signal)'
      write(junit,'(A)') '! '
      write(junit,'(A)') '        call cio_finalize()'
      write(junit,'(A)') '        call pfio_exit()'
      write(junit,'(A)') '! '
      write(junit,'(A)') '        if(signal.ne.kill_sig) then'
      write(junit,'(A)') '          call pcps_kill_parallel_cpus (kill_sig)'
      write(junit,'(A)') '        endif'
      write(junit,'(A)') '! '
      write(junit,'(A)') '        lun = cps_get_lun()'
      write(junit,'(A)') '        close(unit=lun,status="keep")'
      write(junit,'(A)') '      endif'
      write(junit,'(A)') '      return'
      write(junit,'(A)') '      end subroutine cpssig_shutdown'
      write(junit,'(A)') '! '
      write(junit,'(A)') '! '

      write(junit,'(A)') '      subroutine cpssig_print (process,ipn)'
      write(junit,'(A)') '      use pcps_module'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      character(len=*),intent(in) :: process      '//&
                                '!argument'
      write(junit,'(A)') '      integer         ,intent(in) :: ipn          '//&
                                '!argument'
      write(junit,'(A)') '      character(len=132)          :: mess         '//&
                                '!local'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      write(mess,*) "+++++++ ",process," ipn=",'//  &
                         'ipn,"( worker",pcps_current_worker_num,") wrapup '//&
                         'called by signal handler"'
      write(junit,'(A)') '      call pcps_print (mess,2)'
      write(junit,'(A)') '      return'
      write(junit,'(A)') '      end subroutine cpssig_print'

      return
      end subroutine buildjob_write_parallel


!!------------------------- buildjob_write_main ----------------------------!!
!!------------------------- buildjob_write_main ----------------------------!!
!!------------------------- buildjob_write_main ----------------------------!!


      subroutine buildjob_write_main(junit,start)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument

      integer                               :: i                      ! local
      integer                               :: j                      ! local
      integer                               :: j1                     ! local
      integer                               :: j2                     ! local
      type(PROCESS),pointer                 :: current                ! local
      type(PROCESS),pointer                 :: loop_start_process     ! local
      logical                               :: first                  ! local
      integer                               :: label                  ! local
      integer                               :: last_label             ! local
      type(CARD_POINTER)                    :: tag_card               ! local
      character(len=TAG_MAX_LEN)            :: tags(NUM_TAGS)         ! local
      character(len=PNAME_MAX+1)            :: pblank                 ! local
      character(len=80)                     :: ctmp                   ! local
      integer                               :: nctmp                  ! local
      character(len=80)                     :: ctmp2                  ! local
      integer                               :: nctmp2                 ! local
      character(len=80)                     :: ctmp3                  ! local
      integer                               :: nctmp3                 ! local
      character(len=80)                     :: ctmp4                  ! local
      integer                               :: nctmp4                 ! local
      integer                               :: nwih                   ! local
      integer                               :: ndpt                   ! local
      integer                               :: ntr                    ! local
      integer                               :: nscratch_max    = 0    ! local
      integer                               :: nstore_max      = 0    ! local
      integer                               :: ndisk_max       = 0    ! local
      integer                               :: ncart_max       = 0    ! local
      integer                               :: nmag_max        = 0    ! local
      integer                               :: nloops          = 0    ! local
      integer                               :: n_io_pairs      = 0    ! local
      integer                               :: n_io_pairs_total= 0    ! local
      integer                               :: nstore_loop     = 0    ! local
      integer                               :: nscratch_loop   = 0    ! local
      integer                               :: i1                     ! local
      integer                               :: i2                     ! local
      integer                               :: ntr_x(MAX_IO_PAIRS)    ! local
      integer                               :: hd_x_len(MAX_IO_PAIRS) ! local
      integer                               :: tr_x_len(MAX_IO_PAIRS) ! local

      do i=1,PNAME_MAX+1
        pblank(i:i) = ' '
      enddo


      ntr_x     = 0
      hd_x_len  = 0
      tr_x_len  = 0

      nwih      = 0
      ndpt      = 0
      ntr       = 0
      label     = 0
      i         = 0
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        i = i + 1
        write(STDOUT,*) 'process #',i,' = ',current%name(1:current%nname)
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        call pc_get_control ('NEED_REQUEST'  ,current%need_request  )
        call pc_get_control ('NEED_LABEL'    ,current%need_label    )
        call pc_get_control ('TWOSETS'       ,current%twosets       )
        call pc_get_control ('SETUP_ONLY'    ,current%setup_only    )
        call pc_get_control ('SIGNAL'        ,current%signal_handler)
        call pc_get_control ('NSCRATCH'      ,current%nscratch      )
        call pc_get_control ('NSTORE'        ,current%nstore        )
        call pc_get_control ('NDISK'         ,current%ndisk         )
        call pc_get_control ('IFTD'          ,current%iftd          )

        call string_to_upper (current%need_request  )
        call string_to_upper (current%need_label    )
        call string_to_upper (current%twosets       )
        call string_to_upper (current%setup_only    )
        call string_to_upper (current%signal_handler)
        call string_to_upper (current%iftd          )

        if (current%setup_only .eq. 'YES') then
         current%need_request = 'NO'
         current%need_label   = 'NO'
         current%twosets      = 'NO'
        endif

        tag_card%card => current%tag_card_start(GLOBALS_TAG)%card
        first = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_global_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_global_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        current%ltr  = ndpt
        current%lhd  = nwih

        if (pc_global_keyword_present('NWIH')) call pc_get_global ('NWIH' ,nwih)
        if (pc_global_keyword_present('NDPT')) call pc_get_global ('NDPT' ,ndpt)
        if (pc_global_keyword_present('NUMTR')) call pc_get_global('NUMTR',ntr )
        current%ntr  = ntr

        if (current%twosets(1:1) .eq. 'Y') then
          current%ltr2 = ndpt
          current%lhd2 = nwih
        else
          current%ltr  = ndpt
          current%lhd  = nwih
        endif

        nscratch_max = max(nscratch_max , current%nscratch)
        nstore_max   = nstore_max + current%nstore

        if (current%setup_only .eq. 'NO') then
          if ((nloops .eq. 0 .and. current%ntr .ne. 0) .or.            &
              (current%need_label(1:1) .eq. 'Y' .and.                  &
               current%need_request(1:1) .eq. 'N')) then
            nloops = nloops + 1
            current%start_loop = .TRUE.
            n_io_pairs = n_io_pairs + 1
          endif
        endif

        if (current%need_request(1:1) .eq. 'Y') then
          current%request = label
        endif

        if (current%need_label(1:1) .eq. 'Y') then
          label = label + 10
          current%label = label
        endif

        if (current%twosets(1:1) .eq. 'Y') then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
          n_io_pairs = n_io_pairs + 1
          tr_x_len(n_io_pairs) = current%ltr2
          hd_x_len(n_io_pairs) = current%lhd2
        else if (current%ntr.ne.0) then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
        endif

        if (current%ntr.gt.0)  then
          if (current%start_loop) then
            ntr_x(n_io_pairs) = current%ntr
          else
            ntr_x(n_io_pairs) = max(ntr_x(n_io_pairs),current%ntr)
          endif
        endif

        current%n_io_pairs = n_io_pairs
        current => current%next
      enddo

      total_memory = 5242880 + nscratch_max + nstore_max

      write(junit,'(A)') &
      '!                           Start of CPSeis program ' // trim(jobname)
      write(junit,'(A)') &
      '!                       ********** COPYRIGHT NOTICE ************'
      write(junit,'(A)') &
      '!                       Licensed by ConocoPhillips Company 2007'
      write(junit,'(A)') &
      '!                    Using http://cpseis.org/docs/CPSeis_License.pdf'
      write(junit,'(A)') &
      '!                            As an Open-Source Product'
      write(junit,'(A)') &
      '!                         No Warranty implied or provided.'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!! GLOBAL DATA MODULE !!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      module program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      use named_constants_module'
      write(junit,'(A)') &
      '      use cps_module'
      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      use ' // current%name(1:current%nname) // '_module'
        current => current%next
      enddo
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      implicit none'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      public'
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      type(' // current%name(1:current%nname) //               &
        '_struct),pointer  ' // pblank(1:PNAME_MAX+1-current%nname) //  &
        ':: obj' // current%cpid(1:current%ncpid)
        current => current%next
      enddo
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      end module program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!! PROGRAM MAIN !!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      program '//trim(jobname)//'_cps'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      use program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!! DECLARATIONS:'
      write(junit,'(A)') &
      '! '

      do i = 1,n_io_pairs
        write(ctmp,'(''tr'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp,nctmp)
        write(ctmp2,'(''hd'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp2,nctmp2)
        write(junit,'(A)') &
        '      double precision, pointer :: ' // ctmp2(1:nctmp2)
        write(junit,'(A)') &
        '      real            , pointer :: ' // ctmp(1:nctmp)
      enddo

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      integer :: ntr, ierr'
      write(junit,'(A)') &
      '      logical :: error'

      write(junit,'(A)') &
      '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)')  &
          '      nullify (obj'//current%cpid(1:current%ncpid)//')'
        current => current%next
      enddo

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      call cps_start_processing'

      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      call cps_print_rcs_ident '//  &
        '(trim('//current%name(1:current%nname)//'_ident))'
        current => current%next
      enddo
      write(junit,'(A)') '      call cps_custom_rcs_ident'

      n_io_pairs_total = n_io_pairs
      n_io_pairs = 0
      loop_start_process => start
      do i = 1,max(nloops,1)
        n_io_pairs = n_io_pairs + 1
        if (.not. associated(loop_start_process)) exit
        current => loop_start_process
        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '!!!!! ALLOCATE ARRAYS:'
        write(junit,'(A)') &
        '! '
        j1 = max(loop_start_process%n_io_pairs,1)
        j2 = j1
        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          j2 = current%n_io_pairs
          current => current%next
        enddo
        do j = j1,j2
          call string_ii2cc (j          , ctmp )
          call string_ii2cc (ntr_x(j)   , ctmp2)
          call string_ii2cc (hd_x_len(j), ctmp3)
          call string_ii2cc (tr_x_len(j), ctmp4)
          write(junit,'(A)')  &
          '      ntr = '//trim(ctmp2)
          write(junit,'(A)')  &
          '      allocate(hd'//trim(ctmp)//'('//trim(ctmp3)//','//  &
                          'ntr),stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to allocate hd'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)')  &
          '      allocate(tr'//trim(ctmp)//'('//trim(ctmp4)//','//  &
                          'ntr),stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to allocate tr'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
        enddo

        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '!!!!! SETUPS:'
        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '      call cps_start_setups    ! stops if there is an error.'
        write(junit,'(A)') &
        '! '
        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          write(junit,'(A)') &
          '      call cps_pre_setup; call '//current%name(1:current%nname)//&
          '_create'//pblank(1:PNAME_MAX+1-current%nname)//'(obj'//          &
          current%cpid(1:current%ncpid)//');'//                             &
          pblank(1:max_pid_digits+1-current%ncpid) //'call cps_post_setup'
          current => current%next
        enddo
        write(junit,'(A)') &
        '! '
        write(ctmp,'(I10)') i + 8000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        '      call cps_finish_setups (error); if (error) go to '//ctmp(1:nctmp)
        write(junit,'(A)') &
        '! '
        if (nloops .gt. 0) then
          write(junit,'(A)') &
          '!!!!! EXECUTION:'
          write(junit,'(A)') &
          '! '
          write(junit,'(A)') &
          '      ntr = NEED_TRACES'
          write(junit,'(A)') &
          '! '

          current => loop_start_process
          first = .TRUE.
          do
            if (.not. associated(current)) exit
            if (current%start_loop) then
              if (first) then
                first = .FALSE.
              else
                exit
              endif
            endif
            if (current%setup_only.ne.'YES') then
              if (current%need_label(1:1) .eq. 'Y') then
                write(ctmp,'(I10)') current%label
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                ' ' // ctmp(1:max(nctmp,5)) // 'call cps_top_of_loop  (' //  &
                current%cpid(1:current%ncpid) // ')'
                last_label = current%label
              endif
              if (current%twosets(1:1) .eq. 'Y') then
                write(junit,'(A)') &
                '      call cps_pre_process  (' //    &
                current%cpid(1:current%ncpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                n_io_pairs = n_io_pairs + 1
                write(ctmp2,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp2,nctmp2)
                write(junit,'(A)') &
                '      call ' // current%name(1:current%nname) //              &
                pblank(1:PNAME_MAX+1-current%nname) // '    (obj' //           &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ',hd' // ctmp2(1:nctmp2) // ',tr' // &
                ctmp2(1:nctmp2) // ')'
                write(junit,'(A)') &
                '      call cps_post_process (' //                  &
                current%cpid(1:current%ncpid) // ',ntr,hd' //       &
                ctmp2(1:nctmp2) // ',tr' // ctmp2(1:nctmp2) // ')'
              else
                write(junit,'(A)') &
                '      call cps_pre_process  (' //    &
                current%cpid(1:current%ncpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '      call ' // current%name(1:current%nname) //              &
                pblank(1:PNAME_MAX+1-current%nname) // '    (obj' //           &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ')'
                write(junit,'(A)') &
                '      call cps_post_process (' //              &
                current%cpid(1:current%ncpid) // ',ntr,hd' //   &
                ctmp(1:nctmp) // ',tr' // ctmp(1:nctmp) // ')'
              endif
              if (current%need_request(1:1) .eq. 'Y') then
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                write(ctmp2,'(I10)') current%request
                call string_strip_blanks(ctmp2,nctmp2)
                write(junit,'(A)') &
                '      if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp) //  &
                   ';  if (ntr == NEED_TRACES) goto ' // ctmp2(1:nctmp2)
              else
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '      if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp)
              endif
              write(junit,'(A)') &
              '! '
            endif
            current => current%next
          enddo
          write(ctmp,'(I10)') i + 8000
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      if (ntr == NO_MORE_TRACES) goto ' // ctmp(1:nctmp)
          write(junit,'(A)') &
          '      ntr = NEED_TRACES'
          write(ctmp,'(I10)') last_label
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      goto ' // ctmp(1:nctmp)
          write(junit,'(A)') &
          '! '
          write(ctmp,'(I10)') i + 7000
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          ' ' // ctmp(1:max(nctmp,5)) // 'error = .true.'
          current => loop_start_process
          first = .TRUE.
          write(junit,'(A)') &
          '! '
        endif

        write(junit,'(A)') &
        '!!!!! DELETE:'
        write(junit,'(A)') &
        '! '
        write(ctmp,'(I10)') i + 8000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        ' ' //ctmp(1:max(nctmp,5)) // 'continue'
        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          write(junit,'(A)') &
          '      if (associated(obj'//current%cpid(1:current%ncpid)//  &
          '))'//pblank(1:max_pid_digits+1-current%ncpid)           //  &
          'call ' // current%name(1:current%nname) // '_delete'    //  &
          pblank(1:PNAME_MAX+1-current%nname) // '(obj' //             &
          current%cpid(1:current%ncpid) // ')'
          current => current%next
        enddo

        write(junit,'(A)') &
        '! '
        do j = j1,j2
          call string_ii2cc (j,ctmp)
          write(junit,'(A)')  &
          '      deallocate(hd'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to deallocate hd'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)')  &
          '      deallocate(tr'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to deallocate tr'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)') &
          '! '
        enddo
        write(junit,'(A)') &
        '      if (error) go to 9999'

        if (.not. loop_start_process%start_loop) then
          do
            loop_start_process => loop_start_process%next
            if (.not. associated(loop_start_process)) exit
            if (loop_start_process%start_loop) exit
          enddo
          if (.not. associated(loop_start_process)) exit
        endif

        loop_start_process => loop_start_process%next
        do
          if (.not. associated(loop_start_process)) exit
          if (loop_start_process%start_loop) exit
          loop_start_process => loop_start_process%next
        enddo
      enddo

      write(junit,'(A)') &
      ' 9999 continue'
      write(junit,'(A)') &
      '      call cps_finish_processing (error)'

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      stop'
      write(junit,'(A)') &
      '      end program '//trim(jobname)//'_cps'

      write(junit,'(A)') '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!! SIGNAL HANDLER !!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      subroutine cpssig_shutdown (signal)'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      use program_global_data'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      integer,intent(in) :: signal          !argument'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      logical,save       :: flag0 = .true.  !local'

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      logical,save       :: flag'//trim(current%cpid)//  &
        ' = .true.  !local'
        current => current%next
      enddo

      write(junit,'(A)') '! '

      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        call cps_write_accounting_signal (signal)'
      write(junit,'(A)') '        call cps_print_current_status()'
      write(junit,'(A)') '      endif'

      write(junit,'(A)') '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      if (flag'//trim(current%cpid)//') then'
        write(junit,'(A)') &
        '        flag'//trim(current%cpid)//' = .false.'
        write(junit,'(A)') &
        '        if (associated(obj'//current%cpid(1:current%ncpid)//')) then'
        write(junit,'(A)') &
        '          call cps_print ("+++++++ '//current%name(1:current%nname)// &
                   ' ipn='//current%cpid(1:current%ncpid)//  &
                   ' wrapup called by signal handler")'
        if (current%signal_handler .eq. 'YES') then
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//',signal=signal)'
        else
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//')'
        endif
        write(junit,'(A)') &
        '        endif'
        write(junit,'(A)') &
        '      endif'
        write(junit,'(A)') &
        '! '
        current => current%next
      enddo
      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        flag0 = .false.'
      write(junit,'(A)') '        call cps_finish_processing (signal=signal)'
      write(junit,'(A)') '      endif'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      return'
      write(junit,'(A)') '      end subroutine cpssig_shutdown'


      return
      end subroutine buildjob_write_main


!!------------------------- buildjob_write_setup ---------------------------!!
!!------------------------- buildjob_write_setup ---------------------------!!
!!------------------------- buildjob_write_setup ---------------------------!!


      subroutine buildjob_write_setup (junit,start,jobmode,batchsystem,  &
                                       platform,mpi_version)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument
      character(len=*)     ,intent(in)          :: jobmode            ! argument
      character(len=*)     ,intent(in)          :: batchsystem        ! argument
      character(len=*)     ,intent(in)          :: platform           ! argument
      character(len=*)     ,intent(in)          :: mpi_version        ! argument

      integer                                   :: i                  ! local
      integer                                   :: j                  ! local
      integer                                   :: i1                 ! local
      integer                                   :: i2                 ! local
      integer                                   :: i3                 ! local
      character(len=10)                         :: hostname           ! local
      character(len=80)                         :: ctmp,ctmp1
      integer                                   :: nctmp,nctmp1       ! local
      integer                                   :: ostype             ! local
      integer                                   :: time_per_pe        ! local
      integer                                   :: hours              ! local
      integer                                   :: seconds_left       ! local
      integer                                   :: minutes            ! local
      integer                                   :: seconds            ! local
      character(len=3)                          :: chours             ! local
      character(len=2)                          :: cminutes           ! local
      character(len=2)                          :: cseconds           ! local
      character(len=132)                        :: compile_script     ! local
      logical                                   :: first              ! local
      type(PROCESS),pointer                     :: current            ! local
      type(CARD_POINTER)                        :: tag_card           ! local

      if(len_trim(platform) .lt. 6 ) stop 'buildjob.f90:3161 platform < 6 chars'
      call string_to_upper (machine,ctmp)
      if (trim(ctmp) .eq. 'LINUX') then
        machine = 'Linux'
        ostype  = GETSYS_LINUX
      else if (trim(ctmp) .eq. 'SOLARIS') then
        machine = 'Solaris'
        ostype  = GETSYS_SOLARIS
      else if (trim(ctmp).eq.'CUSTOM') then
        if (ncustom_nodes.gt.0) then
          machine = custom_nodes(1)
          ostype  = getsys_machine(machine)
        else
           machine = 'Linux'
           ostype  = GETSYS_LINUX
        endif
      else
        ostype = getsys_machine(machine)
      endif
      if(machine.eq.'Linux')then
        call cnfg_get_value ('bin_path_linux1',binpath)
      else
        call cnfg_get_value ('bin_path_solaris1',binpath)
      endif

      select case (jobmode)

        case ('INTERACTIVE')
          write(junit,'(A)') '#!/bin/sh'
        case default
          select case (ostype)
            case (GETSYS_LINUX)
              call string_strip_blanks(frontend_path,i3)
              if (batchsystem .eq. 'NQS') then
                write(junit,'(A)') '# QSUB -o ' // trim(frontend_path) //  &
                                   trim(jobname) // '.rpt.#'
                write(junit,'(A)') '# QSUB -e ' // trim(frontend_path) //  &
                                   trim(jobname) // '.err.#'
              endif
            case default
              if (batchsystem .eq. 'NQS') then
                write(junit,'(A,I8)') '# QSUB -r ' // trim(jobname) //  &
                                      ' -eo'
                call string_strip_blanks(frontend_path,i3)
                write(junit,'(A)') '# QSUB -o ' // trim(frontend_path) //  &
                                   trim(jobname) // '.rpt.#'
              endif
          end select

          if (batchsystem .eq. 'NQS') then
            write(junit,'(A)') '# QSUB -s /bin/sh'
            if (mail_opt .eq. 'SE') write(junit,'(A)')'# QSUB -mb -me'
            write(junit,'(A,A)')  '# QSUB -q ',queue
          else
!           wmm
            write(junit,'(A)') '#PBS -S /bin/sh'
            write(junit,'(A)') '#PBS -N '//trim(jobname)
            if (mail_opt .eq. 'SE') write(junit,'(A)')'#PBS -m be -M '//trim(mailingaddress)
            if (mail_opt .eq. 'YES') write(junit,'(A)')'#PBS -m bea -M '//trim(mailingaddress)
            if (mail_opt .eq. 'ABT') write(junit,'(A)')'#PBS -m a -M '//trim(mailingaddress)
            if(time_limit > 0 ) then
      	      write(ctmp1,'(I10)') time_limit
              call string_strip_blanks(ctmp1,nctmp1)
              write(junit,'(A)') '#PBS -l walltime='//ctmp1(:nctmp1)
            endif

            if (ncustom_nodes .gt. 0) then
              write(junit,'(A)')  '#PBS -q C'
              if (num_cpus .eq. 1) then
                write(junit,'(A)')  '#PBS -l nodes='//trim(custom_nodes(1))
              else
                write(junit,'(A)')  '#PBS -l nodes='//trim(custom_nodes(1))//':ppn=2'
              endif
            else if (trim(custom_lam) .eq. 'YES') then
              write(junit,'(A)')  '#PBS -q C'
            else if (trim(machine) .eq. 'Solaris') then
              write(junit,'(A)')  '#PBS -l nodes=1:Solaris'
            else if (max_tapes .gt. 0) then
              if(location.eq.'alaska'.or.rlocation.eq.'ALASKA')then
                write(junit,'(A)')  '#PBS -q A'
              else
                if (trim(tapequeue) .eq. 'TODI90') then
                  write(junit,'(A)')  '#PBS -q TODI90'
                  write(junit,'(A)')  '#PBS -l nodes=1:TODI90'
                else if(trim(tapequeue) .eq. 'TODI91')then
                  write(junit,'(A)')  '#PBS -q TODI91'
                  write(junit,'(A)')  '#PBS -l nodes=1:TODI91'
                else if(trim(tapequeue) .eq. 'TODI92')then
                  write(junit,'(A)')  '#PBS -q TODI92'
                  write(junit,'(A)')  '#PBS -l nodes=1:TODI92'
                else
                  write(junit,'(A)')  '#PBS -q TODI74'
                  write(junit,'(A)')  '#PBS -l nodes=1:TODI74'
                endif
              endif
            else if (num_cpus .eq. 1) then
            else    
              call string_ii2cc (num_nodes,ctmp)

!----------------------------------------------------------------------------------------wmm
              write(junit,'(A)') '#PBS -q ' // trim(queue)
              if (num_cpus == 0 ) num_cpus=num_cpus/num_nodes
      	      write(ctmp1,'(I10)') num_cpus
              call string_strip_blanks(ctmp1,nctmp)
              write(junit,'(A)') '#PBS -l nodes='//trim(ctmp)//':ppn='//ctmp1(1:nctmp)
      	      write(ctmp1,'(I10)') num_cpus*num_nodes
      	      call string_strip_blanks(ctmp1,nctmp1)
      	      write(junit,'(A)') 'export NCPUS=' // ctmp1(1:nctmp1)
!---------------------------------------------------------------------------------------
!              if (same_speed) then
!                write(junit,'(A)') '#PBS -v SAME_SPEED=true'
!              else
!                write(junit,'(A)') '#PBS -v SAME_SPEED=false'
!              endif
             endif
          endif
        if(rerun.eq.'NO')write(junit,'(A)') '#PBS -r n'
        write(junit,'(A,I2)')'#PBS -p ',priority

      end select

      if (batchsystem .eq. 'NQS') then
        call string_ii2cc (max_tapes,ctmp)
        write(junit,'(A)') '# JOBNAME='//trim(jobname)//' MACHINE='//  &
                            trim(machine)//' TAPES='//trim(ctmp)
      endif

      write(junit,'(A)') 'unalias -a'
      write(junit,'(A)') '. ~/.bashrc'
      !write(junit,'(A)') 'ulimit -a'
      write(junit,'(A)') 'ulimit -s unlimited'
      write(junit,'(A)') 'ulimit -c unlimited'
      write(junit,'(A)') 'umask 0002'
      write(junit,'(A)') 'export PATH=$PATH:' // trim(binpath)

      write(junit,'(A)')'export cps_config_file=${CPSEIS_INSTALL_DIR}/etc/${CPSEIS_ARCH}/cps_config.dat'

      print*,'CPSeis Installation Directory = ',trim(sps_install)
      print*,'mpi_version = ',trim(mpi_version)

      !write(junit,'(A)') 'OWHOME=/appl/ow'
      !write(junit,'(A)') 'export OWHOME'

      write(junit,'(A)') 'export PLATFORM=${CPSEIS_ARCH}'
      write(ctmp,'(I10)') num_cpus
      call string_strip_blanks(ctmp,nctmp)
      write(junit,'(A)') 'export NCPUS=' // ctmp1(1:nctmp1)
      write(junit,'(A)') 'export USERNAME=`whoami`'
      write(junit,'(A)') 'export PID=$$'

      if (batchsystem .eq. 'PBS') then
        write(junit,'(A)') 'if [ -z $PBS_JOBID ] ; then'
        write(junit,'(A)') '  export PBS_JOBID=$HOSTNAME.$USERNAME.$PID'
	write(junit,'(A)') '  export PBS_REQID=$PID'
        write(junit,'(A)') 'else'
        write(junit,'(A)') '  export PBS_REQID=`echo $PBS_JOBID | awk ''{split(\$1,arr,".");print arr[1]}''`'
        write(junit,'(A)') 'fi'
      endif


      rsh_compile = .false.
      compile_script = ""

      select case (jobmode)
        case ('INTERACTIVE')
          useicps=.false.
          call getsys_current_dir (tmpdir)
          i = len_trim(tmpdir)
          if (tmpdir(i:i) .eq. '/') tmpdir(i:i) = ' '
          if (ostype .eq. GETSYS_LINUX) then
            write(junit,'(A)') 'export TMPDIR='//trim(tmpdir)
            rsh_compile = .true.
            compile_script = trim(tmpdir)//'/'//trim(jobname)//'.compile'
            write(junit,'(A)') 'export RSHCOMPILE='//trim(compile_script)
          endif

        case default    ! BATCH wmm
          tmpdir = ''
          if(batchtmp_nodes(1:4).ne.'NONE')then
           write(junit,'(A)') 'DISKNODELIST=' // trim(batchtmp_nodes)
           write(junit,'(A)') '  NDISKNODES=`cat $DISKNODELIST | wc -l`'
           write(junit,'(A)') '  export NDISKNODES'
           write(junit,'(A)') '  NODEINDEX=`expr $PID % $NDISKNODES`'
           write(junit,'(A)') '  if [ "$((NODEINDEX==0))" == "1" ] ; then'
           write(junit,'(A)') '    NODEINDEX=1'
           write(junit,'(A)') '  fi'
           write(junit,'(A)') '  export NODEINDEX'
           write(junit,'(A)') '  export DISKNODE=`cat $DISKNODELIST | ' //  &
                              '(n=0; while [ "$n" -lt $NODEINDEX ]; ' //  &
                              'do read oneline; n=\`expr $n + 1\`; done; ' //  &
                              'echo $oneline)`'
           write(junit,'(A)') '  '
          endif
          if(batchtmp_nodes(1:4).ne.'NONE')then
            ctmp = '$DISKNODE/batchtmp/$USERNAME'
          else
            ctmp = '/tmp/$HOSTNAME/batchtmp'//trim('$USERNAME')
          endif
          thisbatchtmp=ctmp
          tmpdir = trim(ctmp) // '/' // trim(jobname) // '_$PBS_REQID'
          write(junit,'(A)') 'export TMPDIR='//trim(tmpdir)
          i = index(machine,'Linux')
          if (i .gt. 0 .or. platform(1:3) .eq. 'sol') then
            rsh_compile = .true.
            compile_script = trim(tmpdir)//'/'//trim(jobname)//'.compile'
            write(junit,'(A)') 'export RSHCOMPILE='//trim(compile_script)
          endif
          write(junit,'(A)') 'if [ -d '     // trim(ctmp)   // ' ] ; then'
          write(junit,'(A)') '  mkdir '     // trim(tmpdir) // ' 1>online 2>&1'
          write(junit,'(A)') '  chmod 755 ' // trim(tmpdir) // ' 1>online 2>&1'
          write(junit,'(A)') 'else'
          write(junit,'(A)') '  mkdir '     // trim(ctmp)   // ' 1>online 2>&1'
          write(junit,'(A)') '  mkdir '     // trim(tmpdir) // ' 1>online 2>&1'
          write(junit,'(A)') '  chmod 755 ' // trim(tmpdir) // ' 1>online 2>&1'
          write(junit,'(A)') 'fi'
          write(junit,'(A)') 'cd ' // trim(tmpdir)
          write(junit,'(A)') 'if [ -d $HOME/cpsjobs ] ; then'
          write(junit,'(A)') '  if [ -h $HOME/cpsjobs/'//trim(jobname)//'_$PBS_REQID ]; then'
          write(junit,'(A)') '    rm  -f $HOME/cpsjobs/'//trim(jobname)//'_$PBS_REQID'
          write(junit,'(A)') '  fi'
          write(junit,'(A)') '  ln -s '//trim(tmpdir)//' $HOME/cpsjobs/'//trim(jobname)//'_$PBS_REQID'
          write(junit,'(A)') 'else'
          write(junit,'(A)') '  mkdir $HOME/cpsjobs'
          write(junit,'(A)') '  ln -s '//trim(tmpdir)//' $HOME/cpsjobs/'//trim(jobname)//'_$PBS_REQID'
          write(junit,'(A)') 'fi'

      end select

      call string_strip_blanks(frontend_user,i1)
      call string_strip_blanks(frontend_node,i2)
      call string_strip_blanks(frontend_path,i3)
      write(junit,'(A)') 'echo ''' // trim(frontend_user) // ' ' //  &
     &       trim(frontend_node) // ' ' // trim(frontend_path) &
     &       // ''' > _netinfo'

      write(junit,'(A)') 'cat >bscript << ''/EOS'''
      write(junit,'(A)') 'cat > ' // trim(jobname) // '.f90 << ''/EOF'''

      return
      end subroutine buildjob_write_setup


!!------------------------- buildjob_write_data ----------------------------!!
!!------------------------- buildjob_write_data ----------------------------!!
!!------------------------- buildjob_write_data ----------------------------!!


      subroutine buildjob_write_data(junit,start)


      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument

      integer                                   :: i                  ! local
      logical                                   :: first              ! local
      character(len=80)                         :: ctmp               ! local
      integer                                   :: nctmp              ! local
      type(PROCESS),pointer                     :: current            ! local
      type(CARD_POINTER)                        :: tag_card           ! local
      character(len=TAG_MAX_LEN)                :: tags(NUM_TAGS)     ! local
      integer                                   :: ntags(NUM_TAGS)    ! local

      tags(1)  = ' <GLOBALS>'
      tags(2)  = ' <CONTROLS>'
      tags(3)  = ' <PARAMETERS>'
      tags(4)  = ' <DATA_CARD>'
      ntags(1) = 10
      ntags(2) = 11
      ntags(3) = 13
      ntags(4) = 12

      write(junit,'(A)') '/EOF'
      write(junit,'(A)') 'cat >process_parameters << ''/EOFA'''

      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(PARAMETERS_TAG)%card
        first = .TRUE.
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            write(junit,'(A)') '<PROCESS name="' //   &
                                 current%name(1:current%nname) // '">'
            first = .FALSE.
          endif
          write(junit,'(A)') trim(tag_card%card%data)
          tag_card%card => tag_card%card%next
        enddo
        write(junit,'(A)') '</PROCESS>'
        current => current%next
      enddo

      write(junit,'(A)') '/EOFA'

      return
      end subroutine buildjob_write_data


!!----------------- ------- buildjob_write_jcl -----------------------------!!
!!----------------- ------- buildjob_write_jcl -----------------------------!!
!!----------------- ------- buildjob_write_jcl -----------------------------!!


      subroutine buildjob_write_jcl (junit,start,jobmode,batchsystem,  &
                                     platform,mpi_version)

      use getsys_module

      integer              ,intent(in)          :: junit               !argument
      type(PROCESS),pointer                     :: start               !argument
      character(len=*)     ,intent(in)          :: jobmode             !argument
      character(len=*)     ,intent(in)          :: batchsystem         !argument
      character(len=*)     ,intent(in)          :: platform            !argument
      character(len=*)     ,intent(in)          :: mpi_version         !argument

      integer                                   :: i                   !local
      integer                                   :: j                   !local
      integer                                   :: i1                  !local
      integer                                   :: i2                  !local
      integer                                   :: i3                  !local
      integer                                   :: ostype,whichlib
      integer                                   :: ncmd                !local
      integer                                   :: lun                 !local
      integer                                   :: istat               !local
      character(len=1024)                       :: ctmp                !local
      character(len=1024)                       :: cmd                 !local
      character(len=1024)                       :: cmd1                !local
      character(len=1024)                       :: cmd2                !local
      character(len=160)                        :: c_cmda              !local
      character(len=160)                        :: c_cmdt              !local
      character(len=160)                        :: c_cmdp              !local
      character(len=160)                        :: f90_cmda            !local
      character(len=160)                        :: f90_cmdt            !local
      character(len=160)                        :: f90_cmdp            !local
      character(len=8)                          :: lnklib              !local
      character(len=8)                          :: fwcd,blzw,jos
      character(len=80)                         :: mpirun_cmd          !local
      character(len=80)                         :: f90_cmd             !local
      character(len=80)                         :: f90_mpi             !local
      character(len=160),allocatable             :: extra_libs(:)       !local
      integer                                   :: nextra_libs = 0     !local
      character(len=160),allocatable             :: extra_lmrk(:)       !local
      integer                                   :: nextra_lmrk = 0     !local
      character(len=80)                         :: lmrk_dot_so = ''    !local
      character(len=80)                         :: libs_dir            !local
      character(len=20)                         :: mods_opt            !local
      integer                                   :: nmods_opt           !local
      character(len=80)                         :: mods_dirp           !local
      character(len=80)                         :: mods_dirt           !local
      character(len=80)                         :: mods_dira           !local
      character(len=1024)                       :: exe_cmd             !local
      character(len=80),pointer                 :: objs(:)             !local
      character(len=80)                         :: swdev_home          !local
      character(len=80)                         :: devkit_home         !local
      character(len=80)                         :: oracle_home         !local
      character(len=80)                         :: ow_home             !local
      integer                                   :: nobjs = 0           !local
      logical                                   :: no_lnklib           !local
      logical                                   :: no_moddir           !local
      logical                                   :: compile_file        !local
      logical                                   :: mpi_stubs           !local

      whichlib=getsys_library()

      fwcd=''
      blzw=''
      jos=''
      whichicps='icps'
      call string_to_upper (machine,ctmp)
      if (trim(ctmp) .eq. 'LINUX') then
        machine = 'Linux'
        ostype  = GETSYS_LINUX
      else if (trim(ctmp) .eq. 'SOLARIS') then
        machine = 'Solaris'
        ostype  = GETSYS_SOLARIS
      else if (trim(ctmp).eq.'CUSTOM') then
        if (ncustom_nodes.gt.0) then
          machine = custom_nodes(1)
          ostype  = getsys_machine(machine)
        else
           machine = 'Linux'
           ostype  = GETSYS_LINUX
        endif
      else
        ostype = getsys_machine(machine)
      endif
      if (num_cpus .gt. 1) then
          mpirun_cmd =  &
               'mpirun -machinefile $PBS_NODEFILE -envall -n $NCPUS '
!        if (num_cpus .ge. 2*num_nodes .or. num_cpus .eq. num_nodes) then
!          mpirun_cmd =  &
!               'mpirun -machinefile $PBS_NODEFILE -envall -n $NCPUS '
!        else
!          i = num_nodes - 1
!          call string_ii2cc (i,ctmp)
!          mpirun_cmd = &
!               'mpirun -envall -n '//  &
!               '$NCPUS n0-'//trim(ctmp)
!          j = (num_cpus - num_nodes) / (num_nodes - 1)
!          if (j .ge. 1) then
!            do i=1,j
!              mpirun_cmd = trim(mpirun_cmd)//',1-'//trim(ctmp)
!            enddo
!          endif
!          i = (num_cpus  - num_nodes) - j*(num_nodes - 1)
!          if (i .gt. 0) then
!            call string_ii2cc (i,ctmp)
!            mpirun_cmd = trim(mpirun_cmd)//',1-'//trim(ctmp)
!          endif
!        endif
      else
        mpirun_cmd = ''
      endif

      if(requires_landmark) then
          lmrk_dot_so = '/liblmrk.so'

        select case (ostype)
          case (GETSYS_LINUX)
            call cnfg_get_value ('LANDMARK_OWHOME_LINUX', ow_home)

            nextra_lmrk = 5
            allocate(extra_lmrk(nextra_lmrk))
            extra_lmrk(1) = trim(sps_home) // '/lib/' &
                            // trim(platform) // trim(lmrk_dot_so) // ' \\'
            extra_lmrk(2) = '-L' // trim(ow_home) // '/lib \\'
            extra_lmrk(3) = '-L' // trim(ow_home) // '/lib/pre_WS3.0 \\'
            extra_lmrk(4) = '-L' // trim(ow_home) // '/SeisWorks/lib \\'
            extra_lmrk(5) = '-lgcc_s -lstdc++ -lwtc8 -lclntsh -lgpr_ctk \\'

          case (GETSYS_SOLARIS)
            call cnfg_get_value ('LANDMARK_OWHOME_SOL', ow_home)

            nextra_lmrk = 3
            allocate(extra_lmrk(nextra_lmrk))
            extra_lmrk(1) = trim(sps_home) // '/lib/' &
                            // trim(platform) // trim(lmrk_dot_so) // ' \\'
            extra_lmrk(2) = '-L' // trim(ow_home) // '/lib \\ '
            extra_lmrk(3) = '-L' // trim(ow_home) // '/SeisWorks/lib \\'

        end select
      else
        !nextra_lmrk = 1
        !allocate(extra_lmrk(nextra_lmrk))
        !select case (ostype)
        !  case (GETSYS_LINUX)
        !  extra_lmrk(1) = trim(sps_home) // '/lib/' &
        !                // trim(platform) // '/liblmrk_stubs.a \\'
        !  case (GETSYS_SOLARIS)
        !  extra_lmrk(1) = trim(sps_home) // '/lib/' &
        !                // trim(platform) // '/liblmrk_stubs.a \'
        !end select
      end if

      mpi_stubs  = .false.
      mods_dirp  = '${CPSEIS_INSTALL_DIR}/platforms/${CPSEIS_ARCH}'
      mods_dirt  = mods_dirp
      mods_dira  = mods_dirp
      f90_cmd       = '${CPSEIS_F90_COMPILER} -g -O2 '
      f90_mpi       = trim(f90_cmd)
      f90_cmdp      = trim(f90_cmd)//' -c -I'//trim(mods_dirp)
      f90_cmdt      = f90_cmdp
      f90_cmda      = f90_cmdp
      c_cmdp        = '${CPSEIS_C_COMPILER} -g -O2 -Wall -DLINUXI -I${CPSEIS_INSTALL_DIR}/include -c'
      c_cmdt        = c_cmdp
      c_cmda        = c_cmdp
      mods_opt      = '-I'
      nmods_opt     = 2
      nextra_libs   = 3
      allocate(extra_libs(nextra_libs))
      extra_libs(1) = '-L${CPSEIS_INSTALL_DIR}/platforms/${CPSEIS_ARCH}/lib -lcps'
      extra_libs(2) = '-L${FFTW_DIR}/lib -lfftw -lrfftw'
      if ( mpi_version == 'lam' ) then
        extra_libs(3) = '-L${LAM_DIR}/lib -lmpi -llam -llamf77mpi -lutil -lpthread -lm'
      else if ( mpi_version == 'mpich2' ) then
        extra_libs(3) = '-L${MPICH_DIR}/lib -lmpich -lfmpich -lutil -lpthread -lm'
      else
        extra_libs(3) = '-lutil -lpthread -lm'
      endif

      libs_dir= '${CPSEIS_INSTALL_DIR}/platforms/${CPSEIS_ARCH}/lib '
      if (num_cpus .eq. 1) then
       useicps=.false.  ! useicps is now always false
       if(useicps)then
         exe_cmd='icps '//trim(jobname)//'.wrk 1>online 2>&1'
       else
        if (len_trim(custom_exec_b) .gt. 0) then
          exe_cmd = trim(custom_exec_b)//' '//trim(jobname)//'.exe'
        else
          exe_cmd = './' // trim(jobname) // '.exe'
        endif
        if (len_trim(custom_exec_a) .gt. 0) then
          if (trim(jobmode) .eq. 'INTERACTIVE') then
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' '//trim(custom_exec_a)
          else
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' '//trim(custom_exec_a)//' 1>online 2>&1'
          endif
        else
          if (trim(jobmode) .eq. 'INTERACTIVE') then
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' | tee online'
          else
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' 1>online 2>&1'
          endif
        endif
       endif
      else   !  more than 1 cpu
        if (len_trim(custom_exec_b) .gt. 0) then
          exe_cmd = 'mpirun -machinefile $PBS_NODEFILE -envall '//trim(custom_exec_b)//' $TMPDIR/'//trim(jobname)//'.exe'
        else
          exe_cmd = trim(mpirun_cmd)//' $TMPDIR/'//trim(jobname)//'.exe'
        endif
        if (len_trim(custom_exec_a) .gt. 0) then
          if (trim(jobmode) .eq. 'INTERACTIVE') then
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' '//trim(custom_exec_a)
          else
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' '//trim(custom_exec_a)//' 1>online 2>&1'
          endif
        else
          if (trim(jobmode) .eq. 'INTERACTIVE') then
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' | tee '//trim(jobname)//'.rpt.0'
          else
            i = len_trim(exe_cmd)
            exe_cmd(i+1:) = ' 1>online 2>&1'
          endif
        endif

      endif

      !lnklib       = '-lcps'
      no_lnklib    = .false.

      write(junit,'(A)') '#set -x'
      write(junit,'(A)') 'unalias -a'
      write(junit,'(A)') '. ~/.bashrc 2> /dev/null '

      if (rsh_compile) then
       if(.NOT.useicps)then
        write(junit,'(A)') 'echo "#!/bin/sh -x" > $RSHCOMPILE'
        write(junit,'(A)') 'echo "#set -x " >> $RSHCOMPILE'
        write(junit,'(A)') 'echo "unalias -a " >> $RSHCOMPILE'
        write(junit,'(A)') 'echo ". ~/.bashrc 2> /dev/null " >> $RSHCOMPILE'
        write(junit,'(A)') 'echo "cd ' // trim(tmpdir) // '" >> $RSHCOMPILE'
       endif
        select case (trim(platform))
          case ('sample')
            write(junit,'(A)')  &
            'echo "source /opt/intel_fc_80/bin/ifortvars.csh"' // &
            ' >> $RSHCOMPILE'
        end select
      endif

      select case (trim(std_libs))

        case ('CUSTOM')

! ------- Custom Compile

          compile_file = .false.

          if (ncustom_compile .gt. 0) then
            nullify(objs)
            nobjs = 0
            cmd   = ''
            ncmd  = 0
            i     = 0
            do
              if (.not. compile_file) then
                i = i + 1
                if (i .gt. ncustom_compile) exit
                j = index(trim(custom_compile(i)),'\',.true.)
                if (j .eq. len_trim(custom_compile(i))) then
                  cmd(ncmd+1:) = custom_compile(i)(1:j-1)
                  ncmd = ncmd + len_trim(custom_compile(i)) - 1
                  cycle
                else
                  cmd(ncmd+1:) = custom_compile(i)
                  ncmd = ncmd + len_trim(custom_compile(i))
                endif
              endif
              j = index(cmd,'.compile',.true.)
              if (j .gt. 0 .or. compile_file) then
                if (.not. compile_file) then
                  j = index(cmd,'$PLATFORM')
                  if (j .gt. 0) then
                    ctmp = cmd
                    select case (ostype)
                      case (GETSYS_LINUX)
                        cmd = ctmp(1:j-1)//trim(platform)//ctmp(j+9:)
                      case (GETSYS_SOLARIS)
                        cmd = ctmp(1:j-1)//trim(platform)//ctmp(j+9:)
                    end select
                  endif
                  lun = cio_fopen(trim(cmd),"r")
                  if (lun .lt. 100) then
                    write (STDOUT,*)  &
                      'Error opening file '//trim(cmd)//'  lun=',lun
                    cycle
                  endif
                endif
                if (lun .ge. 100) ncmd = cio_fgetline(cmd,1024,lun)
                if (ncmd .lt. 0) then
                  istat = cio_fclose(lun)
                  compile_file = .false.
                  cycle
                else
                  compile_file = .true.
                endif
              endif
              j = index(trim(cmd),' ')
              if (j .eq. 0) then
                ctmp = cmd
                j = len_trim(cmd)
                  if (j .gt. 4 .and. cmd(j-3:j) .eq. '.f90') then
                    cmd = trim(f90_cmda)//' '//trim(ctmp)
                  else if (j .gt. 2 .and. cmd(j-1:j) .eq. '.c') then
                    cmd = trim(c_cmda)//' '//trim(ctmp)
                endif
              endif
              i1 = index(cmd,' -o ')
              if (i1 .gt. 0) then
                i1 = i1 + 4
                i2 = index(cmd(i1:),' ')
                if (i2 .eq. 0) i2 = len_trim(cmd)
                call buildjob_append_element (objs,nobjs,cmd(i1:i2))
              else
                i2 = index(cmd,'.f90')
                if (i2 .eq. 0) i2 = index(cmd,'.c')
                if (i2 .gt. 0) then
                  i2 = i2 - 1
                  i1 = index(cmd(1:i2),'/',.true.)
                  i1 = max(i1,index(cmd(1:i2),' ',.true.))
                  if (i1 .gt. 0) then
                    call buildjob_append_element(objs,nobjs,cmd(i1+1:i2)//'.o')
                  endif
                endif
              endif
              if (rsh_compile) then
                if (index(trim(cmd),'\') .eq. len_trim(cmd))  &
                  cmd = trim(cmd) // '\'
                write(junit,'(A)') 'echo "' // trim(cmd) // '" >> $RSHCOMPILE'
              else
                write(junit,'(A)') trim(cmd)
              endif
              cmd  = ''
              ncmd = 0
            enddo
          endif

          !if(requires_landmark) then
          !  cmd = trim(f90_cmd) // ' -o ' // trim(jobname) // '.exe' //  &
          !        ' -L' // trim(libs_dir)
          !else if (num_cpus .eq. 1) then

          cmd = trim(f90_mpi) // ' -o ' // trim(jobname) // '.exe' //  &
                    ' -I' // trim(mods_dirp)
          if(.NOT.useicps)then
            write(junit,'(A)') 'echo "' // trim(cmd) // ' \\" >> $RSHCOMPILE'
          else
            write(junit,'(A)') trim(cmd) // ' \'
          endif

          cmd = trim(jobname) // '.f90'
          if(.NOT.useicps)then
            write(junit,'(A)') 'echo "' //trim(cmd)//' \\" >> $RSHCOMPILE'
          else
            write(junit,'(A)') trim(cmd) // ' \'
          endif

! ------- Add Custom Compile Object Code to link

          if (nobjs .gt. 0) then
            do i=1,nobjs
              cmd = trim(objs(i))
              if (rsh_compile) then
                write(junit,'(A)') 'echo "' //trim(cmd)//' \\" >> $RSHCOMPILE'
              else
                write(junit,'(A)') trim(cmd) // ' \'
              endif
            enddo
          endif

! ------- Custom Link

          if (ncustom_link .gt. 0) then
            do i=1,ncustom_link
              cmd = trim(custom_link(i))
              if (rsh_compile) then
                write(junit,'(A)') 'echo "' //trim(cmd)//' \\" >> $RSHCOMPILE'
              else
                write(junit,'(A)') trim(cmd) // ' \'
              endif
            enddo

            if (no_lnklib) then
              cmd = trim(libs_dir) // '/libcps.a'
              if (rsh_compile) then
                write(junit,'(A)') 'echo "' //trim(cmd)//' \\" >> $RSHCOMPILE'
              else
                write(junit,'(A)') trim(cmd) // ' \'
              endif
              write(STDOUT,*) ' '
              write(STDOUT,*) &
                   '**************************WARNING**************************'
              write(STDOUT,*) &
                   'No prodlib.a, betalib.a and alphalib.a found in CUSTOM_LINK'
              write(STDOUT,*) 'Adding ' // trim(cmd)
              write(STDOUT,*) &
                   '**************************WARNING**************************'
              write(STDOUT,*) ' '
            endif

          else                                  ! Default Link

            cmd = trim(libs_dir) // '/libcps.a'
            write(STDOUT,*) ' '
            write(STDOUT,*) &
                       '************************WARNING************************'
            write(STDOUT,*) 'No CUSTOM_LINK found in JOB_DATA'
            write(STDOUT,*) 'Using ' // trim(cmd)
            write(STDOUT,*) &
                       '************************WARNING************************'
            write(STDOUT,*) ' '
          endif
          if (rsh_compile) then     ! case custom
              do i= 1,nextra_libs-1
                write(junit,'(A)') 'echo "'//trim(extra_libs(i))// '\\" >> $RSHCOMPILE'
              enddo
              write(junit,'(A)') 'echo "'//trim(extra_libs(i))// '" >> $RSHCOMPILE'
          else
            if (nextra_libs > 0 .or. nextra_lmrk > 0) then
              write(junit,'(A)') trim(cmd) // ' \'
              do i= 1,nextra_lmrk
                write(junit,'(A)') trim(extra_lmrk(i))
              enddo
              do i= 1,nextra_libs
                write(junit,'(A)') trim(extra_libs(i))
              enddo
            else
              write(junit,'(A)') trim(cmd)
            endif
          endif

        case default  !('PRODLIB')
          cmd1 = trim(f90_cmd) // ' -o ' // trim(jobname) // '.exe '
          cmd2 = mods_opt(1:nmods_opt)//trim(mods_dirp)//' '//trim(jobname)//'.f90'
          if(.NOT.useicps)then
           if (rsh_compile)then
            if (nextra_libs > 0 .or. nextra_lmrk > 0) then
              write(junit,'(A)') 'echo "' // trim(cmd1) // ' \\" >> $RSHCOMPILE'
              write(junit,'(A)') 'echo "' // trim(cmd2) // ' \\" >> $RSHCOMPILE'
              do i= 1,nextra_lmrk
                write(junit,'(A)') 'echo "'//trim(extra_lmrk(i))//   &
                                                             '" >> $RSHCOMPILE'
              enddo
              do i= 1,nextra_libs-1
                write(junit,'(A)') 'echo "'//trim(extra_libs(i))//' \\" >> $RSHCOMPILE'
              enddo
              write(junit,'(A)') 'echo "'//trim(extra_libs(i))//' " >> $RSHCOMPILE'
            else
              write(junit,'(A)') 'echo "' // trim(cmd1) // ' \\" >> $RSHCOMPILE'
              write(junit,'(A)') 'echo "' // trim(cmd2) // '" >> $RSHCOMPILE'
            endif
          else
            if (nextra_libs > 0 .or. nextra_lmrk > 0) then
              write(junit,'(A)') trim(cmd1) // ' \'
              write(junit,'(A)') trim(cmd2) // ' \'
              do i= 1,nextra_lmrk
                write(junit,'(A)') trim(extra_lmrk(i))
              enddo
              do i= 1,nextra_libs
                write(junit,'(A)') trim(extra_libs(i))
              enddo
            else
              write(junit,'(A)') trim(cmd1) // ' \'
              write(junit,'(A)') trim(cmd2)
            endif
           endif
          endif
      end select

      if(requires_landmark) then
        !write(junit,'(A)') 'LD_LIBRARY_PATH='   //            &
        !                   trim(swdev_home)     // '/lib:' // &
        !                   trim(devkit_home)    // '/lib:' // &
        !                   trim(oracle_home)    // '/lib:' // &
        !                   trim(ow_home)        // '/lib'
        !write(junit,'(A)') 'export LD_LIBRARY_PATH'
        write(junit,'(A)') 'LD_LIBRARY_PATH='   //            &
                           trim(ow_home) // '/SeisWorks/lib:' // &
                           trim(ow_home) // '/lib:' // &
                           trim(ow_home) // '/lib/pre_WS3.0'
        write(junit,'(A)') 'export LD_LIBRARY_PATH'
      end if

      if (rsh_compile) then
       if(.NOT.useicps)then
        write(junit,'(A)') 'chmod u+x $RSHCOMPILE'
        write(junit,'(A)') 'sleep 4'
       endif
        if (trim(platform) .eq. 'linuxi81' .or.  &
                 trim(platform) .eq. 'linuxi81_debug') then
          write(junit,'(A)') 'rsh hoeplc04 $RSHCOMPILE'
        else if (trim(platform) .eq. 'linuxp') then
          write(junit,'(A)') 'rsh hoeplc01 $RSHCOMPILE'
        else if (trim(platform) .eq. 'sol62') then
          write(junit,'(A)') 'rsh hotce03 $RSHCOMPILE'
        else if (trim(platform) .eq. 'sol62_debug') then
          write(junit,'(A)') 'rsh hotce03 $RSHCOMPILE'
        else if (trim(platform) .eq. '64linuxp52') then
          write(junit,'(A)') 'rsh hoeplt04 $RSHCOMPILE'
        else if (trim(platform) .eq. 'linuxab90') then
          write(junit,'(A)') 'rsh hoeplc03 $RSHCOMPILE'
        else
          if(.NOT.useicps)then
            write(junit,'(A)')  &
                     trim(sps_install)//'/scripts/rsh2compiler $RSHCOMPILE'
          endif
        endif
        write(junit,'(A)') 'sleep 4'
      endif

      if (nobjs .gt. 0 .or. ncustom_link .gt. 0) then
        write(junit,'(A)') 'set +e'
        write(junit,'(A)') 'if [ -f custom_rcs_ident ] ; then'
        write(junit,'(A)') 'rm -f custom_rcs_ident'
        write(junit,'(A)') 'fi'
      endif

      if (nobjs .gt. 0) then
        do i=1,nobjs
          write(junit,'(A)') 'ident -q '//trim(objs(i))//  &
                                         ' | grep Id: >> custom_rcs_ident'
        enddo
      endif
      if (ncustom_link .gt. 0) then
        do i=1,ncustom_link
          write(junit,'(A)') 'ident -q '//trim(custom_link(i))//  &
                                         ' | grep Id: >> custom_rcs_ident'
        enddo
      endif

      call buildjob_write_cmd(junit,start,'CMD_BEFORE_EXE')
      if (num_cpus .eq. 1) then
        if (requires_landmark) then
          !write(junit,'(A)')  &
          !'LD_LIBRARY_PATH=/usr/openwin/lib:/usr/dt/lib:/usr/app/sparc-sun-'//&
          !'solaris/lib:' // trim(swdev_home) // '/lib/SunOS'
          !write(junit,'(A)') 'export LD_LIBRARY_PATH'
        else if (ostype .eq. GETSYS_SOLARIS) then
          write(junit,'(A)')  &
          'LD_LIBRARY_PATH=/usr/openwin/lib:/usr/dt/lib:/usr/app/sparc-sun-'// &
          'solaris/lib:/usr/lib'
          write(junit,'(A)') 'export LD_LIBRARY_PATH'
        endif
        if (trim(jobmode) .ne. 'INTERACTIVE') then
          if (trim(queue) .ne. 'A'.and.queue(1:1).ne.'T') then
            write(junit,'(A)')  &
               trim(sps_install)//'/scripts/scrub_tmp $HOSTNAME >> err.out'
          endif
        endif
        write(junit,'(A)') 'set -e'
        if(trim(rlocation).eq.'LOCAL')then
          write(junit,'(A)') 'touch ' // trim(frontend_path)//trim(jobname)&
                   //'_$PBS_REQID.started '
        endif
        write(junit,'(A)') trim(exe_cmd)
        write(junit,'(A)') 'set +e'
      else
       if(mpi_version == 'lam' ) then
        if (trim(custom_lam) .eq. 'NO') then
          write(junit,'(A)') 'COMMAND_TIMEOUT="command_timeout"'
          write(junit,'(A)') '######## Set up the LAM/MPI environment ########'
          if (batchsystem .eq. 'NQS') then
            write(junit,'(A)') 'cat >lamhosts <<EOFL'
            if (ncustom_nodes.gt.0) then
              do i=1,ncustom_nodes
                write(junit,'(A)') trim(custom_nodes(i))
              enddo
            else
                write(junit,'(A)') '$HOSTNAME'
            endif
            write(junit,'(A)') 'EOFL'
          else
            if (ncustom_nodes.gt.0) then
              write(junit,'(A)') 'cat >lamhosts <<EOFL'
              do i=1,ncustom_nodes
                write(junit,'(A)') trim(custom_nodes(i))
              enddo
              write(junit,'(A)') 'EOFL'
            else
              write(junit,'(A)') 'sort -u <$PBS_NODEFILE > machines'
              write(junit,'(A)') 'echo " machines file..." >> err.out'
              write(junit,'(A)') 'cat machines >> err.out'
            endif
          endif
          if (trim(jobmode) .ne. 'INTERACTIVE')then
            write(junit,'(A)')  &
              trim(sps_install)//'/scripts/scrub_all lamhosts $PBS_JOBID'//&
              ' >> err.out'
            if ((queue(1:1).eq.'B'.or.queue(1:1).eq.'P').and.&
                 trim(queue).ne.'B1260W') then
              write(junit,'(A)')  &
                 trim(sps_install)//'/scripts/scrub_tmp lamhosts >> err.out'
            endif
            if((location.eq.'houston').and.(queue(1:1).eq.'B' .or. &
                                            queue(1:1).eq.'P'))then
              write(junit,'(A)') trim(binpath) // '/bldmntscr' 
              write(junit,'(A)') 'chmod 755 ./hold_automounts.sh'
              write(junit,'(A)') trim(binpath) // '/multi_rsh '//&
                                 '50 10 ' // trim(tmpdir) //&
                                 '/lamhosts "' // trim(tmpdir) // &
                                 '/hold_automounts.sh >& /dev/null &"'
            endif
          endif
          write(junit,'(A)') '######## Set up trap for exit ########'
          write(junit,'(A)') &
               'trap "$COMMAND_TIMEOUT 900 wipe lamhosts;'//  &
               'exit 1" 1 2 3 4 5 6 7 8 9 10 12 13 14 15 16'
          !write(junit,'(A)') trim(binpath) // '/' //&
          !                   'multi_rsh 50 10 ' // trim(tmpdir) // &
          !                   '/lamhosts "' // &
          !                    trim(binpath) //&
          !                   '/checknfs.sh ' //&
          !                    trim(tmpdir) // ' ' // '$LAMHOME"'
          write(junit,'(A)') 'if [ -f ' //&
                              trim(tmpdir) // '/core ] ; then'
          write(junit,'(A)') 'exit'
          write(junit,'(A)') 'fi'
          write(junit,'(A)') '######## Get ready to run the job ########'
          write(junit,'(A)') &
           '$COMMAND_TIMEOUT 1800 $LAMHOME/bin/lamboot -v lamhosts >> err.out'
          if(trim(rlocation).eq.'LOCAL')then
            write(junit,'(A)') 'touch ' // trim(frontend_path)//trim(jobname)&
                     //'_$PBS_REQID.started '
          endif
          write(junit,'(A)') trim(exe_cmd)
          write(junit,'(A)') '######## lamclean and wipe ########'
          write(junit,'(A)') 'sleep 4'
          write(junit,'(A)')  &
            '$COMMAND_TIMEOUT 900 $LAMHOME/bin/lamclean >> err.out'
          write(junit,'(A)')  &
            '$COMMAND_TIMEOUT 900 $LAMHOME/bin/wipe lamhosts >> err.out'
          write(junit,'(A)') '######## all done ########'
        else
          if(trim(rlocation).eq.'LOCAL')then
            write(junit,'(A)') 'touch ' // trim(frontend_path)//trim(jobname)&
                     //'_$PBS_REQID.started '
          endif
          write(junit,'(A)') trim(exe_cmd)
        endif
       elseif(mpi_version == 'mpich2' ) then
         write(junit,'(A)') 'sort -u <$PBS_NODEFILE > nodelist'
         write(junit,'(A)') 'echo "nodelist file..." >> err.out'
         write(junit,'(A)') 'cat nodelist >> err.out'
         write(junit,'(A)') "N_NODES=$(wc nodelist | awk '{print $1}')"
         write(junit,'(a)') "N_PE=$(wc $PBS_NODEFILE | awk '{print $1}')" 
!        print*,' num_cpus=',num_cpus,' num_nodes=',num_nodes
         write(junit,'(a)') 'echo "N_PE=$N_PE, N_NODES=$N_NODES" >>err.out'
         write(junit,'(A)') 'mpdboot -n $N_NODES -f nodelist --verbose >> err.out'
         write(junit,'(A)') 'mpdtrace -l >>err.out'
         write(junit,'(A)') 'mpdringtest >>err.out'
         if(trim(rlocation).eq.'LOCAL')then
           write(junit,'(A)') 'touch ' // trim(frontend_path)//trim(jobname)&
             //'_$PBS_REQID.started '
         endif
         write(junit,'(A)') trim(exe_cmd)
       else
        ! open mpi here? 
       endif
      endif
      call buildjob_write_cmd(junit,start,'CMD_AFTER_EXE')
      write(junit,'(A)') '/EOS'

      call buildjob_write_cmd(junit,start,'CMD_BEFORE_BSCRIPT')

      if (trim(jobmode) .eq. 'INTERACTIVE') then
        write(junit,'(A)') 'sh -ae bscript'
        return
      endif


      write(junit,'(A)') 'IABORT=0'
      write(junit,'(A)') 'sh -ae bscript 2>>err.out || IABORT=1'
      write(junit,'(A)') 'if [ -f core ] ; then'
      write(junit,'(A)') '  IABORT=1'
      write(junit,'(A)') 'fi'
      write(junit,'(A)') 'if [ "$IABORT" -eq 1 ] ; then'
      write(junit,'(A)') '  a=`date +%D`'
      write(junit,'(A)') '  b=`date +%R`'
      write(junit,'(A)') '  YMDATE=`date +%Y_%m`'
      if (batchsystem .eq. 'NQS') then
        write(junit,'(A)') '  echo $QSUB_REQID $LOGNAME ''' //   &
                            trim(jobname) //                   &
                           ''' $a $b >> '//trim(cps_log)//'/cps_aborted_$YMDATE'
        write(junit,'(A)') '  banner "JOB ABORT"'
        if(trim(rlocation).eq.'LOCAL')then
          write(junit,'(A)') '  touch ' // trim(frontend_path)//trim(jobname)&
                   //'_$PBS_REQID.aborted '
        endif
      else
        write(junit,'(A)') '  echo $PBS_REQID $LOGNAME ''' //   &
                            trim(jobname) //                   &
                           ''' $a $b >> '//trim(cps_log)//'/cps_aborted_$YMDATE'
        write(junit,'(A)') '  echo '' '' > ' //  &
                           'temp_report_file'
        write(junit,'(A)') '  banner "JOB ABORT" >>' //  &
                           'temp_report_file'
        if(trim(rlocation).eq.'LOCAL')then
          write(junit,'(A)') '  touch ' // trim(frontend_path)//trim(jobname)&
                   //'_$PBS_REQID.aborted '
        endif
      endif

      if (mail_opt.eq.'ABT' .or. mail_opt.eq.'YES') then
        select case (ostype)
        case (GETSYS_SOLARIS)
            write(junit,'(A)') '  echo "see subject" > ctemp'
            write(junit,'(A)') '  mailx -s "Job ' //        &
                                trim(jobname) //          &
                               ' aborted" $LOGNAME < ctemp'

        case (GETSYS_LINUX)
            !write(junit,'(A)') '  echo "see subject" > ctemp'
            write(junit,'(A)') '  mail -s "Job ' //         &
                                trim(jobname) //          &
                               ' aborted" '//trim(mailingaddress)//',$LOGNAME < temp_report_file'
        case default
        end select
      endif

      if(trim(rlocation).eq.'LOCAL')then
        write(junit,'(A)') 'else'
        write(junit,'(A)') '  touch ' // trim(frontend_path)//trim(jobname)&
                    //'_$PBS_REQID.completed '
      endif
      write(junit,'(A)') 'fi'

      call buildjob_write_cmd(junit,start,'CMD_AFTER_BSCRIPT')

      select case (trim(batchsystem))
        case ('NQS')
          write(junit,'(A)') 'echo '' '' >> online'
          write(junit,'(A)') 'cat online'
          write(junit,'(A)') 'echo ''$TMPDIR -> '' $TMPDIR > online'
          write(junit,'(A)') 'ls -Ll $TMPDIR  >> online'
          write(junit,'(A)') 'echo '' '' >> online'
          write(junit,'(A)') 'cat online'
          write(junit,'(A)') 'cat err.out'

        case default   ! PBS
          write(junit,'(A)') 'echo '' '' >> online'
          if (len_trim(tmpdir) .gt. 0) then
            write(junit,'(A)') 'echo "' // trim(tmpdir) // '"  >> online'
            write(junit,'(A)') 'ls -Ll ' // trim(tmpdir) // '  >> online'
          else
            write(junit,'(A)') 'echo ''$TMPDIR -> '' $TMPDIR >> online'
            write(junit,'(A)') 'ls -Ll $TMPDIR  >> online'
          endif
          write(junit,'(A)') 'echo '' '' >> online'
          write(junit,'(A)') 'cat err.out >> online'
          write(junit,'(A)') 'if [ -f cfeseries.out ] ; then'
          write(junit,'(A)') '  cat cfeseries.out >> '//  &
                           'temp_report_file'
          write(junit,'(A)') 'fi'
          write(junit,'(A)') 'cat online >> '//  &
                           'temp_report_file'
          write(junit,'(A)') 'if [ "$IABORT" -eq 1 ] ; then'
          write(junit,'(A)') '  echo "JOB ABORT" >> '// &
                           'temp_report_file'
          write(junit,'(A)') 'fi'
      end select

      if (mail_opt.eq.'YES') then
        select case (ostype)
          case (GETSYS_SOLARIS)
            write(junit,'(A)') 'if [ "$IABORT" -eq 0 ] ; then'
            write(junit,'(A)') '  mailx -s "Job ' //        &
                                trim(jobname) //          &
                               ' completed" $LOGNAME < temp_report_file'
            write(junit,'(A)') 'fi'

          case (GETSYS_LINUX)
            write(junit,'(A)') 'if [ "$IABORT" -eq 0 ] ; then'
            write(junit,'(A)') '  mail -s "Job ' //         &
                                trim(jobname) //          &
                               ' completed" '//trim(mailingaddress)//',$LOGNAME < temp_report_file'
            write(junit,'(A)') 'fi'

          case default
        end select
      endif

      if (trim(batchsystem) .eq. 'PBS') then
        if (len_trim(tmpdir) .gt. 0) then
          write(junit,'(A)') 'if [ -f core ] ; then'
          write(junit,'(A)') '  echo " " >> '// &
                           'temp_report_file'
          write(junit,'(A)') '  echo "Found core file" >> '// &
                           'temp_report_file'
          write(junit,'(A)') '  echo "'//trim(tmpdir)//' was not removed" >> '// &
                           'temp_report_file'
          write(junit,'(A)') '  FOUND_CORE=1'
          write(junit,'(A)') 'else'
          write(junit,'(A)') '  FOUND_CORE=0'
          write(junit,'(A)') 'fi'
          write(junit,'(A)') 'cd'
          write(junit,'(A)') 'sleep 4'
          if(rlocation.eq.'LOCAL')then
            write(junit,'(A)') 'cp ' // trim(tmpdir) // &
                 '/temp_report_file ' // &
                 trim(frontend_path)//trim(jobname)&
                 //'.rpt.$PBS_REQID '
          else if(rlocation.ne.'PONY')then
            write(junit,'(A)') 'rcp ' // trim(tmpdir) //&
                 '/temp_report_file ' // &
                 trim(frontend_user) // '@' // trim(rcp_node) // ':"' // &
                 trim(frontend_path)// trim(jobname)//'.rpt.$PBS_REQID"'
            write(junit,'(A)')'rsh ' // trim(rcp_node)//' -l ' // &
                 trim(frontend_user) // ' "chmod 644 ' // &
                 trim(frontend_path)//trim(jobname)//'.rpt.$PBS_REQID"'
          else
            write(junit,'(A)') 'cp ' // trim(tmpdir) //&
                 '/temp_report_file ' // &
                 '$HOME/' // trim(jobname)//'.rpt.$PBS_REQID'
            write(junit,'(A)')'chmod 644 ' // '$HOME/' // trim(jobname)//&
                              '.rpt.$PBS_REQID'
          endif
          write(junit,'(A)') '#killall hold_automounts.sh'
          write(junit,'(A)') 'if [ "$FOUND_CORE" -eq 0 ] ; then'
          write(junit,'(A)') '  rm -R ' // trim(tmpdir)
          write(junit,'(A)') '  rm $HOME/cpsjobs/'//trim(jobname)//'_$PBS_REQID'
          write(junit,'(A)') 'fi'
        endif
      endif

      return
      end subroutine buildjob_write_jcl


!!------------------------- buildjob_write_cmd -----------------------------!!
!!------------------------- buildjob_write_cmd -----------------------------!!
!!------------------------- buildjob_write_cmd -----------------------------!!


      subroutine buildjob_write_cmd(junit,start,tag)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument
      character(len=*)     ,intent(in)          :: tag                ! argument

      type(PROCESS),pointer                     :: current            ! local
      type(CARD_POINTER)                        :: tag_card           ! local
      logical                                   :: first              ! local
      character(len=1024),pointer               :: array(:)           ! local
      integer                                   :: narray             ! local
      integer                                   :: i                  ! local

      nullify(array)
      call pc_frontend_update (3)
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .true.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo
        narray = 0
        call pc_alloc_control (tag,array,narray)
        if (narray .gt. 0) then
          do i=1,narray
            write(junit,'(A)') trim(array(i))
          enddo
        endif
        current => current%next
      enddo

      call pc_restore
      if (associated(array)) deallocate(array)

      return
      end subroutine buildjob_write_cmd


!!--------------------------- load_job_data --------------------------------!!
!!--------------------------- load_job_data --------------------------------!!
!!--------------------------- load_job_data --------------------------------!!


      subroutine buildjob_load_job_data(junit,start,wunit)

      integer              ,intent(in)                 :: junit       ! argument
      type(PROCESS),pointer                            :: start       ! argument
      integer :: wunit

!!      type(JOB_DATA_STRUCT),pointer                    :: job_obj     ! local
      type(PROCESS),pointer                            :: current     ! local
      type(CARD_POINTER)                               :: tag_card    ! local
      logical                                          :: first       ! local
      character(len=80)                                :: ctmp        ! local
      integer :: istat,whichlib

      call pc_next
      call pc_set_ipn (2)
      current => start
      do
        if (.not. associated(current)) exit
        if (current%name(1:current%nname) .eq. 'JOB_DATA') then
          tag_card%card => current%tag_card_start(PARAMETERS_TAG)%card
          first = .TRUE.
          do
            if (.not. associated(tag_card%card)) exit
            if (first) then
              call pc_put_process_card(tag_card%card%data)
              first = .FALSE.
            else
              call pc_add_process_card(tag_card%card%data)
            endif
            tag_card%card => tag_card%card%next
          enddo
          exit
        endif
        current => current%next
      enddo


!!!      call job_data_create (job_obj)
      rlocation='LOCAL'
      call buildjob_jobdatainfo(rlocation,jobname,machine,speed,&
          num_cpus,num_nodes,std_libs,frontend_user,frontend_node,&
          frontend_path,mail_opt,queue,time_limit,custom_exec_b,&
          custom_exec_a,custom_lam,ncustom_modules,custom_modules,&
          ncustom_compile,custom_compile,ncustom_link,custom_link,&
          ncustom_nodes,custom_nodes,wunit,rerun,priority)
      anycustom=.false.
      useicps=.true.
      if(ncustom_modules+ncustom_compile+ncustom_link.gt.0)anycustom=.true.
      if(num_cpus.gt.1.or.anycustom.or.requires_landmark)useicps=.false.
!!      call pc_get_jdata ('RLOCATION'     ,rlocation)
      whichlib=getsys_library()  ! 1=prod, 2=beta, 3=alpha
!         putsys_env must be called before any call to cnfg
      if(rlocation.eq.'ALASKA')then
!                Get alaska config file
       select case(whichlib)
        case(GETSYS_ALPHALIB)
         call putsys_env('cps_config_file_alpha',&
                      '/home/sps/offsite/alaska/cps_config_alpha.dat',&
                         istat)
        case(GETSYS_BETALIB)
         call putsys_env('cps_config_file_beta',&
                      '/home/sps/offsite/alaska/cps_config_beta.dat',&
                         istat)
        case(GETSYS_PRODLIB)
         call putsys_env('cps_config_file',&
                         '/home/sps/offsite/alaska/cps_config.dat',&
                          istat)
       end select
      endif
      if(rlocation.eq.'PONY')then
!                Get pony config file
        select case(whichlib)
         case(GETSYS_ALPHALIB)
          call putsys_env('cps_config_file_alpha',&
                       '/home/sps/offsite/pony/cps_config_alpha.dat',&
                          istat)
         case(GETSYS_BETALIB)
          call putsys_env('cps_config_file_beta',&
                          '/home/sps/offsite/pony/cps_config_beta.dat',&
                           istat)
         case(GETSYS_PRODLIB)
          call putsys_env('cps_config_file',&
                          '/home/sps/offsite/pony/cps_config.dat',&
                           istat)
        end select
      endif
      call cnfg_get_value ('location',location)
      call cnfg_get_value ('cps_pbs_type',pbs_type)
      call cnfg_get_value ('cps_main_host',main_host)
      call cnfg_get_value ('cps_rcp_node',rcp_node)
      call cnfg_get_value ('cps_platform_default_linux',platform_default_linux)
      call cnfg_get_value ('cps_platform_default_sol',platform_default_sol)
      call cnfg_get_value ('sps_home_dir'   ,sps_home   )
      call cnfg_get_value ('sps_install_dir',sps_install)
      call cnfg_get_value ('cps_log_dir'    ,cps_log    )
      call cnfg_get_value ('cps_batchtmp_nodes_file',batchtmp_nodes)
      call cnfg_get_value ('cps_batchtmp_dir',batchtmp_dirs)
      if(platform_default_linux.eq.' ')platform_default_linux='linuxab80'
      if(platform_default_sol.eq.' ')platform_default_sol='sol62'


      call buildjob_remove_blank_rows (custom_modules ,ncustom_modules)
      call buildjob_remove_blank_rows (custom_compile ,ncustom_compile)
      call buildjob_remove_blank_rows (custom_link    ,ncustom_link   )
      call buildjob_remove_blank_rows (custom_nodes   ,ncustom_nodes  )

      if (trim(std_libs).eq.'CUSTOM' .and. &
          ncustom_compile.eq.0 .and. ncustom_link.eq.0) then
        if (getsys_library() .eq. GETSYS_PRODLIB) then
          std_libs = 'PRODLIB'
        else if (getsys_library() .eq. GETSYS_BETALIB) then
          std_libs = 'BETALIB'
        else
          std_libs = 'ALPHALIB'
        endif
        write(STDOUT,*) 'WARNING WARNING -- No custom compile or link specified'
        write(STDOUT,*) '                   Changing to '//trim(std_libs)
      else if (trim(std_libs).eq.'TESTLIB') then
        std_libs = 'BETALIB'
      endif

      if (num_nodes .eq. 0) then
        num_nodes = num_cpus / 2
        if (num_nodes*2 .lt. num_cpus) num_nodes = num_nodes + 1
      endif

      call string_to_upper (custom_lam)
      call string_to_upper (machine,ctmp)
      if (trim(ctmp)       .eq. 'CUSTOM' .and.  &
          trim(custom_lam) .eq. 'NO'     .and.  &
          ncustom_nodes    .eq. 0        ) then
        machine = 'Linux'
        write(STDOUT,*) 'WARNING WARNING -- No custom nodes or lam specified'
        write(STDOUT,*) '                   Changing machine to Linux'
      endif

!!!      call job_data_delete (job_obj)

      return
      end subroutine buildjob_load_job_data


!!------------------------- load_project_data ------------------------------!!
!!------------------------- load_project_data ------------------------------!!
!!------------------------- load_project_data ------------------------------!!


      subroutine buildjob_load_project_data(junit,start)

      integer              ,intent(in)                 :: junit       ! argument
      type(PROCESS),pointer                            :: start       ! argument

      type(PROJECT_DATA_STRUCT),pointer                :: project_obj ! local
      type(PROCESS),pointer                            :: current     ! local
      type(CARD_POINTER)                               :: tag_card    ! local
      logical                                          :: first       ! local

      call pc_next
      call pc_set_ipn (1)
      current => start
      do
        if (.not. associated(current)) exit
        if (current%name(1:current%nname) .eq. 'PROJECT_DATA') then
          tag_card%card => current%tag_card_start(PARAMETERS_TAG)%card
          first = .TRUE.
          do
            if (.not. associated(tag_card%card)) exit
            if (first) then
              call pc_put_process_card(tag_card%card%data)
              first = .FALSE.
            else
              call pc_add_process_card(tag_card%card%data)
            endif
            tag_card%card => tag_card%card%next
          enddo
          exit
        endif
        current => current%next
      enddo

      call project_data_create (project_obj)
      call pc_get_pdata ('ACCOUNT' ,account)
      call project_data_delete (project_obj)


      return
      end subroutine buildjob_load_project_data


!!------------------------- remove_blank_rows ------------------------------!!
!!------------------------- remove_blank_rows ------------------------------!!
!!------------------------- remove_blank_rows ------------------------------!!


      subroutine buildjob_remove_blank_rows (array,narray)

      character(len=*),pointer                 :: array(:)            ! argument
      integer                 ,intent(inout)   :: narray              ! argument

      integer                                  :: i                   ! local
      character(len=PC_LENGTH),allocatable     :: temp_array(:)       ! local

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
      end subroutine buildjob_remove_blank_rows


!!--------------------------- append_element -------------------------------!!
!!--------------------------- append_element -------------------------------!!
!!--------------------------- append_element -------------------------------!!


      subroutine buildjob_append_element (array,narray,value)
      implicit none
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
      end subroutine buildjob_append_element

      subroutine buildjob_jobdatainfo(rlocation,jobname,machine,speed,&
          num_cpus,num_nodes,std_libs,frontend_user,frontend_node,&
          frontend_path,mail_opt,queue,time_limit,custom_exec_b,&
          custom_exec_a,custom_lam,ncustom_modules,custom_modules,&
          ncustom_compile,custom_compile,ncustom_link,custom_link,&
          ncustom_nodes,custom_nodes,wrklun,rerun,priority)

      character(len=*),pointer :: custom_modules(:),custom_compile(:),&
          custom_link(:),&
          custom_nodes(:)
      character(len=*) :: rlocation,jobname,machine,speed,std_libs,&
          frontend_user,frontend_path,mail_opt,queue,custom_exec_b,&
          custom_exec_a,custom_lam,frontend_node,rerun
      integer :: num_cpus,num_nodes,time_limit,ncustom_modules,ncustom_compile
      integer :: ncustom_link,ncustom_nodes,wrklun,priority
      type(cardset_struct),pointer :: cobj

      character(len=80) :: card,msg
      logical :: found=.false.
      integer :: istat,k

      rewind wrklun
      nullify(cobj)

      call cardset_create(cobj)
      DO
        read(wrklun,'(A)',iostat=istat)card
        if(istat.lt.0)return
        if(found)then
          k=index(card,'</PROCESS>')
          if(k.ne.0)exit
          call cardset_add_card(cobj,card)
        else
          k=index(card,'<PROCESS name="JOB_DATA">')
          if(k.ne.0)found=.true.
        endif
      ENDDO

      call getsys_hostname(frontend_node)
      call getsys_username(frontend_user)
      if (frontend_path .eq. 'NONE') then
        call getsys_current_dir(frontend_path)
      endif

      call cardset_get_scalar(cobj,'RLOCATION',rlocation,msg)
      call cardset_get_scalar (cobj,'JOBNAME'       ,jobname,msg)
      call cardset_get_scalar (cobj,'MACHINE'       ,machine,msg)
      if (machine(1:5) .eq. 'hotce') machine = 'Solaris'
      speed='TBD'
      call cardset_get_scalar (cobj,'RERUN'         ,rerun,msg)
      call cardset_get_scalar (cobj,'NUM_CPUS'      ,num_cpus,msg)
      call cardset_get_scalar (cobj,'NUM_NODES'     ,num_nodes,msg)
      priority=0
      call cardset_get_scalar (cobj,'PRIORITY'      ,priority,msg)
      call cardset_get_scalar (cobj,'STD_LIBS'      ,std_libs,msg)
      call string_to_upper (std_libs)
      call cardset_get_scalar (cobj,'MAIL_OPT'      ,mail_opt,msg)
      call cardset_get_scalar (cobj,'MAILINGADDRESS',mailingaddress,msg)
      call cardset_get_scalar (cobj,'QUEUE'         ,queue,msg)
!!!      if(queue.eq.'B')queue='B1800'
      call cardset_get_scalar (cobj,'TIME_LIMIT'    ,time_limit,msg)
      call cardset_get_scalar (cobj,'CUSTOM_EXEC_B' ,custom_exec_b,msg)
      call cardset_get_scalar (cobj,'CUSTOM_EXEC_A' ,custom_exec_a,msg)
      call cardset_get_scalar (cobj,'CUSTOM_LAM'    ,custom_lam,msg)

      call cardset_alloc_array (cobj,'CUSTOM_MODULES',custom_modules,&
                                 ncustom_modules,msg)
      call cardset_alloc_array (cobj,'CUSTOM_COMPILE',custom_compile,&
                                 ncustom_compile,msg)
      call cardset_alloc_array (cobj,'CUSTOM_LINK',custom_link,ncustom_link,msg)
      call cardset_alloc_array (cobj,'CUSTOM_NODES',custom_nodes,ncustom_nodes,&
                                msg)

    end subroutine buildjob_jobdatainfo


    end module buildjob_module
