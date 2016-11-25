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
! Name       : cfe
! Category   : cfe
! Written    : 1999-08-25   by: Donna K. Vunderink
! Revised    : 2008-03-25  by: Bill Menger
! Maturity   : beta
! Purpose    : CPS Front-End
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!                    WORKFILE_BUILDER tabbed screen
!                    ------------------------------
!  (Allows the user to build and edit workfiles, build jobfiles from workfiles
!   and submit jobs to a batch queue.)
!
!
! Starting a New workfile
! -----------------------
!
! Enter the name of the new workfile in the Current Workfile Name text field and
! return.  No extension is needed.  Workfile names must start with an
! alphabetic, consist of alphabetics, numerics and underscores (_) and have no
! more than 15 characters.
!
! (Entering a name that is the same as an existing workfile will bring up the
! processes in the existing file.  Any changes made will modify the existing
! file.)
!
! PROJECT_DATA and JOB_DATA processes automatically appear when the name of the
! Current Workfile is entered.  Select one of these process names with mouse
! button 1, press the ProcessScreen button and the parameter screens will
! appear, allowing you to enter the necessary information.  The Help Line will
! give you information about each parameter.
!
!
! Selecting an Old Workfile
! -------------------------
!
! Processes may be added to the Current Workfile from either the CPS Processes
! list or the Old Workfile list.  You may select an old workfile for display by
! entering its name in the Old Workfile Name text field or by double clicking
! on the Select Old Workfile button, which brings up the Old Workfile file
! selection box.  In addition, the arrows allow you to cycle through a buffer
! of all the files that have been selected.
!
!
! Selecting Processes
! -------------------
!
! You can select one or more processes (a range) within one of the three
! scrollable lists as follows.
!
! Select a single process by clicking on the process name with mouse button 1.
!
! Select a sequence of consecutive processes by clicking and dragging with
! mouse button 1.
!
! Select a sequence of non-consecutive processes by pressing the ctrl key and
! clicking the process names with mouse button 1.
!
! The first range selected will be the copy range; it will be highlighted green.
! The second range selected will be the destination range; it will be
! highlighted red.
!
! Users will be able to select no more than two ranges and have them active at
! the same time.  If two ranges are active, they may both be in the Current
! Workfile List or one in the Current Workfile List and one in either the CPS
! Processes List or the Old Workfile List.  In no case will two active ranges
! overlap.
!
!
! De-selecting Processes
! ----------------------
!
! If any one of the action buttons is pressed (Append, Insert After, Replace,
! Insert Blank, Delete or Process Screen) the desired action is taken and then
! the selected range or ranges are automatically de-selected.
!
! Ranges can also be de-selected by pressing the Clear Selections button.
!
!
! Conflicts Involving Selected Processes
! --------------------------------------
!
! Sometimes the range or ranges selected may not be consistent with a requested
! action.  These rules govern such conflicts.
!
! If a multiple-process range is selected when only a single selected process is
! expected, the last process in the range will be used as the selected process.
!
! If two ranges are selected when only one is expected, the first range selected
! (the copy range) will be used as the selected range.
!
!
! Adding Processes to the New Workfile
! ------------------------------------
!
! You can add processes to the new workfile in any of the following ways.
!
!   Copy All        To copy all the old workfile processes, press the Copy All
!                   button.  This causes all the processes in the selected old
!                   workfile to be copied over to the current workfile
!                   (including Project_Data and Job_Data entries).
!
!   Append          To append one or more processes to the current workfile,
!                   select the copy range in any of the three scrollable lists.
!                   Press the Append  button.  The selected processes will be
!                   appended to the current workfile.
!
!   Insert After    Select the copy range in any of the three scrollable lists
!                   and the destination process in the current workfile.  Press
!                   the Insert After button.  The processes in the copy range
!                   will be inserted after the destination process in the
!                   current workfile.
!
!   Replace         Select the copy range in any of the three scrollable lists
!                   and the destination range in the current workfile.  Press
!                   the Replace button.  The destination range in the current
!                   workfile will be replaced by the selected copy range..
!
!   Insert Blank    Select a process in the current workfile.  Press the
!                   Insert Blank button.  A blank field is inserted after the
!                   selected process in the current workfile.  You can then
!                   type in the desired process name.
!
!
! Deleting Processes from the New Workfile
! ----------------------------------------
!
! Select a range of processes in the current workfile, then press the Delete
! button.  The selected processes are deleted from the current workfile.
!
!
! Editing Parameters of Processes in the New Workfile
! ---------------------------------------------------
!
! Select a process in any scrollable list and press the Process Screen button.
! The parameter entry screen for that process pops up.  Such pop up parameter
! screens are display-only if selected from the Old Workfile List or the CPS
! Processes List.  You can then select defaults with the Defaults button and
! enter or modify parameter values.  The Help Line will give you information
!  about each parameter.
!
! Project_Data and Job_Data entries behave the same as process parameter screen
! entries with regard to default selection and parameter entry and modification.
!
! Within a parameter entry screen, normally you can visit parameter fields in
! any order although it is usually a good idea to start from the top and work
! toward the bottom of the screen.  Mouse button 1 activates a scalar field;
! ctrl-I inserts a blank row after the current row in an array.
!
!
! WARNING ON SETTING PROCESS PARAMETERS:
! -------------------------------------
!
! The preferred method of setting process parameters is to visit the parameter
! entry screen and set parameter values for each process immediately after it
! has been added to the current workfile.  It is also possible to add all the
! processes to the workfile and then go through the processes sequentially
! setting the parameters  this method may trigger error messages due to required
! parameters that have not been set.  In any event it is a good idea to develop
! the habit of always visiting the parameter screens the same way so you do not
! inadvertently miss some.
!
!
! Alternatives to Button Presses
! ------------------------------
!
! The mouse button 3 pop up window displays a menu that includes all the actions
! of the buttons on the Workfile Builder Screen.  Choice of a menu option from
! the mouse button 3 pop up window is equivalent to pressing the same button on
! the Workfile Builder screen.
!
!
!
!-------------------------------------------------------------------------------
!
!                   MULTI-WORKFILE BUILDER tabbed screen
!                   ------------------------------------
!
!  (Allows the user to build and edit multiple workfiles, build jobfiles from
!  multiple workfiles and submit multiple jobs to a batch queue.)
!
!
! Building multiple workfiles has four steps:
!
!     1.  Select the template workfile you wish to use.
!
!     2.  Enter the total number of workfiles to be built and generate names
!     for them.
!
!     3.  Determine which parameters will vary among the workfiles and generate
!     their values.
!
!     4.  Build the workfiles, build the jobfiles and submit your jobs.
!
! To help guide you through this documentation, these four steps are marked
! where appropriate.
!
!
! Multiple Workfiles and Template Workfiles
! -----------------------------------------
!
! "Multiple Workfiles" all have the same processes in the same order as the
! template workfile, but they have different jobnames and may have different
! values of process parameters.
!
! The "Template Workfile" is a pre-existing workfile that will be used as a
! model for your multiple workfiles.  It is an ordinary workfile built with the
! Workfile Builder tabbed screen (or the Multi-Workfile Builder tabbed screen).
!
!
! Selecting Your Template Workfile  [STEP 1.]
! --------------------------------
!
! YOU MUST SELECT A TEMPLATE WORKFILE FIRST, BEFORE TAKING ANY OTHER ACTION ON
! THE MULTI-WORKFILE BUILDER SCREEN.
!
! Select the template workfile by pressing the Select Template Workfile button,
! which brings up the Select Template Workfile dialog box.  Then select the
! template workfile in the usual way.  Alternatively, you may select a template
! workfile by entering its name in the Template Jobname text field.
!
! Selecting a template workfile causes the template workfile process list to be
! displayed in the Template Workfile scrollable list.
!
! THE MULTI-WORKFILE BUILDER NEVER CHANGES THE TEMPLATE WORKFILE.
!
!
! The .mwb File
! -------------
!
! For reasons of efficiency, the Multi-Workfile Builder does not build
! workfiles on the fly but instead creates and uses a file with the same name
! as the template workfile, but with a .mwb extension.  This file is written to
! the current directory.  This .mwb file contains all the information that you
! have entered into the Multi-Workfile Builder in a session for a particular
! template workfile.
!
! If you select a template workfile that already has a .mwb file associated
! with it, the Multi-Workfile Builder will automatically load the information
! from that .mwb file.  (If you want to start from scratch, just press the New
! MWB Session button.)
!
!
! Setting Names for Multiple Workfiles  [STEP 2.]
! ------------------------------------
!
! First enter the desired number of multiple workfiles in the Total to Build
! text field just above the Workfiles to Build scrollable list.  Then press the
! Increment Jobname button which brings up the Build Jobnames screen.
!
!
! Setting Parameter Values for Multiple Workfiles  [STEP 3.]
! -----------------------------------------------
!
! Two methods are provided for specifying parameter values for multiple
! workfiles:  a special method is provided for pathnames and another method is
! provided for all other parameters.
!
! Follow these steps to specify PATHNAME parameters for multiple workfiles.
!
!     1.  Press the All Path Parameters button.  This loads all the pathname
!     parameter information from the template workfile into the Changed
!     Parameters scrollable list at the bottom of the Multi-Workfile Builder
!     tabbed screen.  Note that there is one line for each parameter and that
!     the line also shows the appropriate IPN number, Process name and Keyword.
!
!     2.  Now identify the parameters you want to delete from the Changed
!     Parameters list.  (Parameters you delete from this list will have the SAME
!     value for all your multiple workfiles.)  Click on the row of a parameter
!     you want to delete and press the Delete Change button; this removes the
!     selected row from the Changed Parameters list.
!
!     You can delete all the rows from the Changed Parameters list by
!     pressing the Delete All Changes button.
!
!     3.  Clicking on a row and pressing the Parameter Screen button brings up
!     the Build Parameter Values screen, which allows you to specify parameter
!     values for the parameter whose row you highlighted.
!
!     (The Build Parameter Values screen is very similar to the Build Jobnames
!     screen and operates in a similar fashion.)
!
!     4.  Specify parameter values with the Build Parameter Values screen.
!
!
! Follow these steps to specify other (non-pathname) parameters for multiple
! workfiles.
!
!     1.  Click on a process in the Template Workfile list to highlight the
!     selected process.
!
!     2.  Pressing the Process Screen button brings up the Process Parameter
!     screen for the process you highlighted.
!
!     3.  Clicking on a parameter on the Process Parameter screen brings up the
!     Build Parameter Values screen, which allows you to specify parameter
!     values for the parameter you selected.
!
!     4.  Specify parameter values with the Build Parameter Values screen.
!     Pressing OK accepts these parameter values.
!
!
! Submitting and Cleaning Up  [STEP 4.]
! --------------------------
!
! The last steps using the Multi-Workfile Builder screen consist of building
! your multiple workfiles, submitting them and a few other details.  Here are
! the possible actions.
!
!     Write        Press the Write Report button to write an MWB Report file
!     Report*      summarizing the parameter value changes in your multiple
!                  workfiles.  This text file has the same name as your
!                  template workfile with a .prn extension.  It is a space
!                  delimited file that Excel can conveniently read and display.
!
!     Run Traps*   Press the Run Traps button to run the parameter traps for
!                  your multiple workfiles.  This may be time consuming if
!                  there are many workfiles.
!
!     Write        Press the Write Workfiles button to generate the actual
!     Workfiles*   multiple workfiles from the information stored in the .mwb
!                  file for the current session.  (Traps are run automatically
!                  if they have not been run on the current .mwb information.)
!
!     Build Jobs*  Press the Build Jobs button to build jobs from the multiple
!                  workfiles.  (Traps are run automatically if they have not
!                  been run on the current .mwb information.  New workfiles are
!                  built automatically if existing ones are not consistent with
!                  the current .mwb information.)
!
!     Submit Jobs  Press the Submit Jobs button to bring up the Select Jobs
!                  dialog box to transfer jobs to the Submit Jobs tabbed screen.
!                  (Traps are run automatically if they have not been run on
!                  the current .mwb information.  New workfiles are built
!                  automatically if existing ones are not consistent with the
!                  current .mwb information.  New jobs are built automatically
!                  if existing ones are not consistent with the current .mwb
!                  information.)
!
!     New MWB      Press the New MWB Session button to clear all the
!     Session      information from the Multi-Workfile Builder screen except
!                  the template workfile.
!
! The actions shown with an "*" above are marked with an "X" in the field to
! the left of the button if the latest instance of that action is consistent
! with the current .mwb information.  The "X" is removed if the latest instance
! of the action is not consistent with the current .mwb information.
!
!
!
!-------------------------------------------------------------------------------
!
!                       SUBMIT_JOB tabbed screen
!                       ------------------------
!  (Allows the user to submit jobs to a batch queue and to specify job series
!   information about jobs being submitted.)
!
!
! There are two methods of submitting your job:  using the Submit Job tabbed
! screen and using the Submit Job button on the Workfile Builder tabbed screen.
!
!
! Using the Submit Job tabbed Screen
! ----------------------------------
!
! 1.  Specify the desired job series name, if any, for the job(s) being
!     submitted by entering the desired name in the Job Series Name text field.
!     A job series allows the user to specify a execution sequence for the jobs.
!     This is optional but highly recommended when submitting a large number
!     of jobs.
!
!     If a job series aborts, the Reload Series button can be used to
!     automatically reload the jobnames, series count and next series inform-
!     ation into the scrollable lists.
!
!     Also, a currently executing job series can be modified by using the
!     Reload Series button.  The user can modify the jobnames, series count
!     and next series scrollable lists and then press the Update Series button
!     to replace the information in the job series file.
!
!     If Independent Series is set to no and a job in a job series aborts, no
!     subsequent jobs in that series will be submitted.  If Independent Series
!     is set to yes, all jobs in the series are submitted regardless of whether
!     any of them abort.
!
! 2.  Enter one or more jobnames in the Jobname scrollable list using one of the
!     following methods.
!
!     A.  Accept the default, which is the most recently built job,
!
!     B.  Manually enter the jobnames in the scrollable list,
!
!     C.  Select a jobname from the Jobname file selection dialog box, or
!
!     D.  Press the Jobname List button to read in existing list file into the
!         scrollable list.
!
! (Users can specify names of workfiles for which no jobfiles yet exist.
! In this case jobfiles will be built automatically prior to submission.)
!
! 3.  Enter the job series count numbers in the Series Count scrollable list
!     to specify the execution sequence for jobs with the same job series name.
!     (This is necessary only if a job series name has been specified.)
!     There are two ways of entering the Series Count scrollable list.
!
!     A. Manually enter the count numbers in the scrollable list,
!
!     B. Press the Series Count List button to read in existing list file into
!        the scrollable list.
!
! 4.  Enter, in the Next Series scrollable list, the name of another job series
!     to start on completion of the job being submitted.  (This is optional.)
!     There are two ways of entering the Next Series scrollable list.
!
!     A. Manually enter the next series names in the scrollable list,
!
!     B. Press the Next Series List button to read in existing list file into
!        the scrollable list.
!
! 5.  Optionally enter desired job start date and time in the Submit Date and
!     Submit Time text entry fields.  (These fields default to the current
!     date and time.)
!
! 6.  Return to 1. and enter information for more jobs if desired.
!
! 7.  Press the Submit button to submit jobs in the list and clear the Submit
!     Job screen or press the Clear button to clear the Submit Job screen only.
!
! Visit the Session Log tabbed screen if you wish to check on workfiles or
! jobfiles built and submitted in the current session.
!
!
!
!-------------------------------------------------------------------------------
!
!                       SESSION_LOG tabbed screen
!                       -------------------------
!  (Displays to the user session log information relating to the current CFE
!   session.)
!
!
!  Information shown is:
!
!       JOBS            Names of workfiles created in the current session,
!
!       BUILT           Time each jobfile was built,
!
!       SUBMITTED       Time each job was submitted, and
!
!       JOB_SERIES      Job series information on each job.
!
!
! Information on each job is shown on one row of the scrollable lists.
!
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
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
!053. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 52. 2006-01-10  B. Menger    Removed Unused Variables.
! 51. 2005-02-28  Goodger      Put full path on cfebld.  Remote installs
!                              have trouble finding it.
! 50. 2004-08-23  SMCook       Incorporated mfilebox (multi-file selection).
! 49. 2004-05-26  SMCook       Removed hard-coded icps command for the custom
!                               case.  For the custom code case, the icps
!                               command path is now resolved in cfewb.f90.
! 48. 2004-03-10  SMCook       Incorporated variable prog_icps to launch ICPS.
! 47. 2003-11-18  SMCook       Beta and alpha versions now use cfesubbeta and
!                               cfesubalpha.
! 46. 2003-09-29  Stoeckley    Show node name and platform on title bar for
!                               cfecustom.
! 45. 2003-09-15  Stoeckley    Changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 44. 2003-03-05  SMCook       Modified prog_cfesub to use explicit full path.
!                               ostype is determined by getsys_ostype, then
!                               the bin path is obtained from cps_config.dat.
!                               Change was prompted by portability problems
!                               with cfesj (implements delayed job submittals).
! 43. 2002-10-17  Vunderink    Enabled access to the CIU application.
! 42. 2002-09-23  Vunderink    Remove directory and filter actions when filebox
!                               is used for SELECTCURRENT.
! 41. 2002-09-13  Vunderink    Made changes for Submit screen List buttons.
! 40. 2002-09-11  Vunderink    Made changes for Select Current Workfile button
!                               and enabled JOBMON startup.
! 39. 2002-07-25  Vunderink    Modified to use cfejava_module
! 38. 2002-05-08  Vunderink    Changed name of cfebldtest to cfebldbeta
! 37. 2002-03-11  Vunderink    For cfecustom, check cps library and choose
!                               appropriate job builder program.
! 36. 2002-02-28  Vunderink    Made changes for cfealpha
! 35. 2002-01-04  Vunderink    Made changes for buildlist tool.
! 34. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB.
! 33. 2001-08-09  Vunderink    Remove programmer preferences from cfe and
!                               cfetest by using separate xml for cfecustom
! 32. 2001-07-02  Vunderink    Added CPS header display.
! 31. 2001-06-25  Vunderink    Fixed bug in MWB file selection boxes
! 30. 2001-06-11  Vunderink    Added support of FILEBOX windows in processes
! 29. 2001-04-12  Vunderink    Updated documentation
! 28. 2001-03-12  Vunderink    Added support for series reload filebox
! 27. 2001-02-13  Vunderink    Removed -pbs from cfebld and cfesub
! 26. 2001-02-01  Vunderink    Added tools Xtpioadmin, cbyt, va, csv and cfg
! 25. 2001-01-23  Vunderink    Modified to use PBS batch queuing for cfetest
!                               and cfecustom
! 24. 2000-12-19  Vunderink    Added documentation for multi-workfile builder.
! 23. 2000-12-13  Vunderink    Added support for RESET button on parameter
!                               screens.
! 22. 2000-12-05  Vunderink    Allow everyone access to the multi-workfile
!                               builder, delete the mwb_param object after
!                               the OK or CANCEL button is pressed, change to
!                               allow the MWB jobs to build screen to use the
!                               mwb_param object, added PARAMETERLIST to the
!                               mwb_param screen, and removed spreadsheets
!                               from the MWB screen.
! 21. 2000-11-16  Vunderink    Fixed bug in selecting the current process from
!                               the PROCESSLIST combobox.
! 20. 2000-10-18  Vunderink    Moved userid into cfestruct
! 19. 2000-10-16  Vunderink    Allow vundedk and wardrcj access to MWB.
! 18. 2000-10-08  Vunderink    Added HelpSection
! 17. 2000-09-06  Vunderink    Changed "Working Directory" to
!                               "Working Environment"
! 16. 2000-09-05  Vunderink    Added support for DOC.
! 15. 2000-09-04  Vunderink    Uppercase keyword strings to reduce use of
!                               string_to_upper.
! 14. 2000-08-15  Vunderink    Removed use of cfe_constants, changed character
!                               variables to use PC_LENGTH, and added
!                               multi-workfile builder
! 13. 2000-06-13  Vunderink    Added initializing process_maturity flag
! 12. 2000-05-26  Vunderink    Added execution mode check
! 11. 2000-05-09  Vunderink    Fixed bug in changing processes via the arrow
!                               buttons, made required argument change to
!                               cfewb_save_defaults call, and added call to
!                               new routines to get and save the session log.
! 10. 2000-04-25  Vunderink    Do not call process_update for OLDPROCESS and
!                               LISTPROCESS windows
!  9. 2000-04-24  Vunderink    Added call to cfegui_set_main_window in
!                               cfe_create
!  8. 2000-04-24  Vunderink    Changed character variables to use named constant
!                               CFE_LENGTH, initialized new last_xxxx variables
!                               in cfe_initialize, added calls to cfemenu
!                               module for UserPreferences button, added call
!                               to cfesj_update when the SubmitJobScreen is
!                               entered, added Help button and removed Reset
!                               on cfemenu type windows, fixed bug in changing
!                               process parameter window via the ProcessList
!                               option menu and arrows, and moved code to
!                               support the SubmitJobScreen to cfesj module
!                               and added calls to cfesj routines.
!  7. 2000-04-05  Vunderink    Move cfe_initialize call after create calls in
!                               cfe_create
!  6. 2000-03-08  Vunderink    Make changes to support having a cfebldtest and
!                               cfesubtest program, and do not allow an OK if
!                               there is an error in cfemenu_execute.
!  5. 2000-02-29  Vunderink    Make argument changes to cfegui_showhelp call
!  4. 2000-02-23  Vunderink    Made changes needed by moving lnklib to
!                               cfe_struct
!  3. 2000-02-16  Vunderink    Fixed action taken when OK,Apply,Cancel,Reset
!                               are pressed on child window of process. If
!                               no file is selected in select old workfile,
!                               do not call cfewb.  Added display of working
!                               directory to main screen.  Added setting title
!                               to CFE, CFEtest, or CFEcustom.  Fixed
!                               PROCESSLIST, PROCESSLISTLEFT, and
!                               PROCESSLISTRIGHT keywords on PROCESS screens
!                               to update the current process before create a
!                               new one.
!  2. 2000-02-03  Vunderink    Added call to cfewb_delete_process when OK or
!                               Cancel button pressed, and made improvements
!                               on Submit Job screen.
!  1. 1999-08-25  Vunderink    Initial version.
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


!-------------------------------------------------------------------------------
!<gui_def>
!
!<TA>
!<NS TopScreen>
!
!   WorkingDir`P
!
!</TA>
!
!<NS>
!
!   WorkfileBuilder`P
!   MultiBuilder `P
!   Submit_Job`P
!   SessionLog`P
!   BuildJobfile`P
!   BuildList`P
!   Exit`P
!   EditWorkfile`P
!   EditJobfile`P
!   EditFile`P
!   ViewWorkfile`P
!   ViewJobfile`P
!   ViewReportfile`P
!   ViewFile`P
!   ViewCPSheaders`P
!   ChangeDirectory`P
!   ToolbarEditor`P
!   UserPreferences`P
!   ProgPreferences`P
!   CIU`P
!   XPBS`P
!   XTPIOADMIN`P
!   CBYT`P
!   VA`P
!   CSV`P
!   CFG`P
!   JOBMON`P
!   ApplicationHelp`P
!   ApplicationVersion`P
!   DOC`P
!
!   CIUTB`P
!   DOCTB`P
!   ViewWorkfileTB`P
!   ViewJobfileTB`P
!   ViewReportfileTB`P
!   XPBSTB`P
!   CBYTTB`P
!   VATB`P
!   JOBMONTB`P
!   ViewCPSheadersTB`P
!   BuildListTB`P
!
!<NS WorkfileBuilderScreen>
!
!   ProcessDefaults`P
!   ProcessScreen`P
!   CopyAll`P
!   Append`P
!   Delete`P
!   Replace`P
!   InsertAfter`P
!   InsertBlank`P
!   ClearSelection`P
!   Undo`P
!   ClearCurrent`P
!   ClearWorkfileBuffer`P
!   BuildJob`P
!   SubmitJob`P
!   selectJob`P
!   shiftOldLeft`P
!   oldJobName`P
!   shiftOldRight`P
!   oldJobComment`P
!   oldProcessList`P
!   selectCurrent`P
!   currentJobName`P
!   currentProcessList`P
!   processSubset`P
!   subsetProcessList`P
!
!<NS MultiBuilderScreen>
!
!   Proceed2Parms`P
!   PathParameters`P
!   WriteReportFlag`P
!   AllTrapsFlag`P
!   WriteWorkfilesFlag`P
!   BuildJobsFlag`P
!   WriteReport`P
!   AllTraps`P
!   WriteWorkfiles`P
!   BuildJobs`P
!   SubmitJobs`P
!   NewMWBSession`P
!   selectTemplate`P
!   templateJobName`P
!   templateProcessList`P
!   MBincJobname`P
!   totalToBuild`P
!   jobnameList`P
!   MWBipn`P
!   MWBprocess`P
!   MWBkeyword`P
!   MWBParmShow`P
!   MWBParmDelete`P
!   MWBParmClear`P
!
!<NS SubmitJobScreen>
!
!   SJSeries`P
!   SJReloadSeries`P
!   SJIndependent`P
!   SJDate`P
!   SJTime`P
!   SJJobSelect`P
!   SJJobs`P
!   SJCount`P
!   SJNext`P
!   SJSubmit`P
!   SJClear`P
!   SJIncrement`P
!   SJUpdateSeries`P
!
!<NS SessionLogScreen>
!
!   SLJobs`P
!   SLBuilt`P
!   SLSubmitted`P
!   SLSeries`P
!   SLCount`P
!   SLNext`P
!
!</gui_def>
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="WorkingDir">
!<Tip> The node, userid and working directory for this cfe session. </Tip>
! This is a view-only field.
!</Help>
!
!<Help KEYWORD="WorkfileBuilder">
!<Tip> Make the Workfile Builder tabbed screen active. </Tip>
! Allows user to create and edit workfiles.
!</Help>
!
!<Help KEYWORD="MultiBuilder">
!<Tip> Make the Multi-Workfile Builder tabbed screen active. </Tip>
! Allows user to build and edit multiple workfiles, build jobfiles from
! multiple workfiles and submit multiple jobs to a batch queue.
!</Help>
!
!<Help KEYWORD="BuildJobfile">
!<Tip> Build jobfile from current workfile. </Tip>
! Allows user to build a jobfile given a workfile name.
!</Help>
!
!<Help KEYWORD="BuildList">
!<Tip> Build a list file. </Tip>
! Allows user to build a list file.
!</Help>
!
!<Help KEYWORD="SessionLog">
!<Tip> Make the Session Log tabbed screen active. </Tip>
! Displays daily jobs built and submitted information.
!</Help>
!
!<Help KEYWORD="Submit_Job">
!<Tip> Make the Submit Job tabbed screen active. </Tip>
! Allows user to submit workfiles and jobfiles.
!</Help>
!
!<Help KEYWORD="Exit">
!<Tip> Exit CFE. </Tip>
! Exit the program.
!</Help>
!
!<Help KEYWORD="EditWorkfile">
!<Tip> Edit the current workfile with a text editor. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="EditJobfile">
!<Tip> Edit the current jobfile with a text editor. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="Editfile">
!<Tip> Edit any file (no default) with a text editor. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="ViewWorkfile">
!<Tip> View the current workfile with a text editor in read-only mode. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="ViewWorkfileTB">
!<Tip> View the current workfile with a text editor in read-only mode. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="ViewJobfile">
!<Tip> View the current jobfile with a text editor in read-only mode. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="ViewJobfileTB">
!<Tip> View the current jobfile with a text editor in read-only mode. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="ViewReportfile">
!<Tip> View the current reportfile with a text editor in read-only mode. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="ViewReportfileTB">
!<Tip> View the current reportfile with a text editor in read-only mode. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="Viewfile">
!<Tip> View any file (no default) with a text editor in read-only mode. </Tip>
! The text editor can be selected in User Preferences.
!</Help>
!
!<Help KEYWORD="ViewCPSheaders">
!<Tip> Allows the user to view the CPS trace header definition. </Tip>
! Displays the CPS trace header definition.
!</Help>
!
!<Help KEYWORD="ChangeDirectory">
!<Tip> Allows the user to access the Change Working Directory screen. </Tip>
! The working directory is where current workfiles are created and where
! jobfiles are built.  The working directory defaults to the directory where cfe
! was started.
!</Help>
!
!<Help KEYWORD="ToolbarEditor">
!<Tip> Allows the user to access the Toolbar Editor screen. </Tip>
!
!</Help>
!
!<Help KEYWORD="UserPreferences">
!<Tip> Allows the user to access the User Preferences screen. </Tip>
! Users can select the preferred text editor and make other choices.
!</Help>
!
!<Help KEYWORD="ProgPreferences">
!<Tip> Allows the user to access the Programmer Preferences screen. </Tip>
! Allows the programmer to specify custom xml for process screens.
!</Help>
!
!<Help KEYWORD="CIU">
!<Tip> Allows the user to access the CPS interactive utilities program. </Tip>
! Starts the CIU application.
!</Help>
!
!<Help KEYWORD="CIUTB">
!<Tip> Allows the user to access the CPS interactive utilities program. </Tip>
! Starts the CIU application.
!</Help>
!
!<Help KEYWORD="XPBS">
!<Tip> Allows the user to access the XPBS batch queue status utility. </Tip>
! Starts the XPBS application.
!</Help>
!
!<Help KEYWORD="XPBSTB">
!<Tip> Allows the user to access the XPBS batch queue status utility. </Tip>
! Starts the XPBS application.
!</Help>
!
!<Help KEYWORD="XTPIOADMIN">
!<Tip> Allows the user to access the XTPIOADMIN tape admin utility. </Tip>
! Starts the XTPIOADMIN application.
!</Help>
!
!<Help KEYWORD="CBYT">
!<Tip> Allows the user to access the CBYT trace file viewing utility. </Tip>
! Starts the CBYT application.
!</Help>
!
!<Help KEYWORD="CBYTTB">
!<Tip> Allows the user to access the CBYT trace file viewing utility. </Tip>
! Starts the CBYT application.
!</Help>
!
!<Help KEYWORD="VA">
!<Tip> Allows the user to access the VA velocity analysis utility. </Tip>
! Starts the VA application.
!</Help>
!
!<Help KEYWORD="VATB">
!<Tip> Allows the user to access the VA velocity analysis utility. </Tip>
! Starts the VA application.
!</Help>
!
!<Help KEYWORD="CSV">
!<Tip> Allows the user to access the CSV cube slice viewing utility. </Tip>
! Starts the CSV application.
!</Help>
!
!<Help KEYWORD="CFG">
!<Tip> Allows the user to access the CFG field geometry utility. </Tip>
! Starts the CFG application.
!</Help>
!
!<Help KEYWORD="JOBMON">
!<Tip> Allows the user to access the JOBMON job monitoring utility. </Tip>
! Starts the JOBMON application.
!</Help>
!
!<Help KEYWORD="JOBMONTB">
!<Tip> Allows the user to access the JOBMON job monitoring utility. </Tip>
! Starts the JOBMON application.
!</Help>
!
!<Help KEYWORD="ViewCPSheadersTB">
!<Tip> Allows the user to view the CPS trace header definition. </Tip>
! Displays the CPS trace header definition.
!</Help>
!
!<Help KEYWORD="BuildListTB">
!<Tip> Build a list file. </Tip>
! Allows user to build a list file.  List files are text files with a single
! column of values.
!</Help>
!
!<Help KEYWORD="ApplicationHelp">
!<Tip> Allows the user to access the pop-up Help Window. </Tip>
! The Help Window allows access to three kinds of help:  Parameter Help, CPS
! Process Help and Application (CFE) Help.
!</Help>
!
!<Help KEYWORD="ApplicationVersion">
!<Tip> Displays the version number of the CFE executable. </Tip>
! Display the version of the CFE gui software.
!</Help>
!
!<Help KEYWORD="DOC">
!<Tip> Allows the user to access the cfeDOC (documentation) utility. </Tip>
! The cpsDOC utility is accessed via a Web browser.  This option will start
! Netscape to access cpsDOC.
!</Help>
!
!<Help KEYWORD="DOCTB">
!<Tip> Allows the user to access the cpsDOC (documentation) utility. </Tip>
! The cpsDOC utility is accessed via a Web browser.  This option will start
! Netscape to access cpsDOC.
!</Help>
!
!
!
!<Help KEYWORD="ProcessDefaults">
!<Tip> Combo box to choose from options for process parameter defaults. </Tip>
! Process parameter options are: System (those listed in CPS documentation),
! Project and User.   Users may establish their own Project and User defaults.
!</Help>
!
!<Help KEYWORD="ProcessScreen">
!<Tip> Pops up the parameter screen of the selected process </Tip>
! To view/edit the parameters associated with a process, select the process.
! Press the ProcessScreen button.  A new window with the parameters for the
! process will appear.  If the process was in the current workfile scrollable
! list, the parameters will be editable.  Otherwise, they are view only.
!</Help>
!
!<Help KEYWORD="CopyAll">
!<Tip> Button to copy the old workfile to the current workfile. </Tip>
! Project_Data and Job_Data from the old workfile are also copied to the
! current workfile.
!</Help>
!
!<Help KEYWORD="Append">
!<Tip> Append selected processes to the current workfile. </Tip>
! To append one or more processes the the current workfile, select the copy
! select the copy range in any of the three scrollable lists.  Press the Append
! button.  The selected processes will be appended to the current workfile.
!</Help>
!
!<Help KEYWORD="Delete">
!<Tip> Delete highlighted processes in the current workfile. </Tip>
! To delete one or more processes from the current workfile, select a range of
! processes in the current workfile.  Press the Delete button.  The selected
! processes are deleted from the current workfile.
!</Help>
!
!<Help KEYWORD="Replace">
!<Tip> Replace process in the current workfile with the selected process. </Tip>
! To replace one or more processes in the current workfile, select the copy
! range in any of the three scrollable list and the destrination range in the
! current workfile.  Press the Replace button.  The destination range in the
! current workfile will be replaced by the selected copy range.
!</Help>
!
!<Help KEYWORD="InsertAfter">
!<Tip> Insert processes after the highlighted process. </Tip>
! To insert one or more processes after a process in the current workfile,
! select the copy range in any of the three scrollable lists and the destination
! process in the current workfile.  Press the InsertAfter button.  The processes
! in the copy range will be inserted after the destination process in the
! current workfile.
!</Help>
!
!<Help KEYWORD="InsertBlank">
!<Tip> Insert blank row after the highlighted process. </Tip>
! To insert a blank row after a process in the current workfile, select a
! process in the current workfile.  Press the InsertBlank button.  A blank row
! is inserted after the selected process in the current workfile.  You can then
! type the name of a desired process on the row.
!</Help>
!
!<Help KEYWORD="ClearSelection">
!<Tip> Button that clears all selections. </Tip>
! De-selects any green or red highlighted selection.
!</Help>
!
!<Help KEYWORD="Undo">
!<Tip> Button that reverses the latest action. </Tip>
! To undo the last InsertAfter, Replace, Append, or Delete action on the current
! workfile, press the Undo button.
!</Help>
!
!<Help KEYWORD="ClearCurrent">
!<Tip> Button that clears all entries in the current workfile. </Tip>
! Pressing the ClearCurrent button clears the current workfile name and
! process list so that you can start/load a new current workfile.
!</Help>
!
!<Help KEYWORD="ClearWorkfileBuffer">
!<Tip> Button that clears all entries in the old workfile buffer. </Tip>
! Each time an old workfile is displayed, it is added to the old workfiles
! buffer.  Pressing the ClearWorkfileBuffer button clears the buffer.
!</Help>
!
!<Help KEYWORD="BuildJob">
!<Tip> Button that builds a jobfile from the current workfile. </Tip>
! The BuildJob button will build a jobfile from the current workfile.  Before
! building the job, all process parameter traps are run to make sure the
! jobfile would run if submitted.
!</Help>
!
!<Help KEYWORD="SubmitJob">
!<Tip> Button that submits the current jobfile to a batch queue. </Tip>
! The SubmitJob button will submit the jobfile associated with the current
! workfile to the batch queue specified in the jobfile.  It will automatically
! build the jobfile, prior to submission, if the user has not done so.  If you
! use the SubmitJob button, you will not be able to specify job series, submit
! date or submit time.
!</Help>
!
!<Help KEYWORD="selectJob">
!<Tip> This button accesses the Select Old Workfile dialog box. </Tip>
! Choose an old workfile via a file selection dialog.
!</Help>
!
!<Help KEYWORD="shiftOldLeft">
!<Tip> Button that displays the previous old workfile in the buffer.</Tip>
!
!</Help>
!
!<Help KEYWORD="oldJobName">
!<Tip> Name of selected old workfile.  Arrows scroll through buffer. </Tip>
! Text field displaying name of the old workfile selected from the buffer of
! all files that have been selected.  You can enter names directly in the text
! field at the extreme right end.  Clearing the workfile that shows in the
! field deletes it from the buffer.
!</Help>
!
!<Help KEYWORD="shiftOldRight">
!<Tip> Button that displays the next old workfile in the buffer.</Tip>
!
!</Help>
!
!<Help KEYWORD="oldJobComment">
!<Tip> Comment from the selected old workfile. </Tip>
! This is a view-only field.
!</Help>
!
!<Help KEYWORD="oldProcessList">
!<Tip> List of processes in the selected old workfile. </Tip>
! This is a view-only list.
!</Help>
!
!<Help KEYWORD="selectCurrent">
!<Tip> This button accesses the Select Current Workfile dialog box. </Tip>
! Choose an current workfile via a file selection dialog.
!</Help>
!
!<Help KEYWORD="currentJobName">
!<Tip> Name of the current workfile. </Tip>
! Current Workfile Name must start with an alpha, consist solely of alphas,
! numerics and underscores (_) and have no more than 15 characters.
!</Help>
!
!<Help KEYWORD="currentProcessList">
!<Tip> List of processes in the current workfile. </Tip>
! PROJECT_DATA and JOB_DATA processes automatically appear when the name if the
! current workfile is entered.  They must be the first two processes in every
! workfile.
!</Help>
!
!<Help KEYWORD="processSubset">
!<Tip> Combo box for choosing subset of CPS processes to list. </Tip>
! Default = All_Processes
! Allowed = All_Processes
!           amplitude_mod
!           diagnostics
!           filters
!           headers
!           inversion
!           io
!           migrations
!           miscellaneous
!           multi_component
!           plot
!           sorts
!           stacks
!           statics
!           synthetics
!           transforms
!           velocity_Analysis
!
! Choose a category of CPS processes to view.
!</Help>
!
!<Help KEYWORD="subsetProcessList">
!<Tip> List of all CPS processes available (or a subset). </Tip>
! List is a view only list.
!</Help>
!
!
!
!<Help KEYWORD="Proceed2Parms">
!<Tip> Pops up the parameter screen of the selected process. </Tip>
! Pops up the parameter screen of the selected process so you can select a
! parameter.  The Build Parameter Values screen allows you to vary the
! parameter values.
!</Help>
!
!<Help KEYWORD="PathParameters">
!<Tip>Button to copy all PATH parameters to the Changed Parameters list. </Tip>
! Pressing the All Path Parameters button loads all the pathname parameters in
! the template workfile into the Changed Parameters scrollable list.
!</Help>
!
!<Help KEYWORD="WriteReportFlag">
!<Tip> Indicates whether the last MWB Report is up to date. </Tip>
! An "X" is present if the latest MWB Report is consistent with the current
! .mwb information (that is, is up to date).
!</Help>
!
!<Help KEYWORD="AllTrapsFlag">
!<Tip> Indicates if traps have been run on the latest parameter values. </Tip>
! An "X" is present if traps have been run on the latest parameter values.
!</Help>
!
!<Help KEYWORD="WriteWorkfilesFlag">
!<Tip> Indicates whether latest workfiles are up to date.</Tip>
! An "X" is present if the latest workfiles are consistent with the current
! .mwb information (that is, are up to date).
!</Help>
!
!<Help KEYWORD="BuildJobsFlag">
!<Tip> Indicates whether latest jobs are up to date.</Tip>
! An "X" is present if the latest jobs are consistent with the current .mwb
! information (that is, are up to date).
!</Help>
!
!<Help KEYWORD="WriteReport">
!<Tip> Button that writes an MWB Report file of all parameter changes. </Tip>
! The MWB Report file sumarizes the parameter value changes in your multiple
! workfiles.  This text file has the same name as your template workfile with a
! .prn extension.  It is a space delimited file that Excel can conveniently
! read and display.
!</Help>
!
!<Help KEYWORD="AllTraps">
!<Tip> Button that runs parameter traps on all parameter values. </Tip>
! The Run Traps button runs parameter traps on all the parameter values in your
! multiple workfiles.  This may be time consuming if you have many workfiles.
!</Help>
!
!<Help KEYWORD="WriteWorkfiles">
!<Tip> Button that writes the workfiles with your parameter changes. </Tip>
! Button that writes the workfiles using your specified jobnames and parameter
! values.  (This information was stored in the .mwb file.)
!
! Traps are run automatically if they have not have been run on the latest
! parameter values.
!</Help>
!
!<Help KEYWORD="BuildJobs">
!<Tip> Button that builds the jobfiles from the current workfiles. </Tip>
!
! Traps are run automatically if they have not have been run on the latest
! parameter values.  New workfiles are built automatically if existing ones are
! not consistent with the current .mwb information.
!</Help>
!
!<Help KEYWORD="SubmitJobs">
!<Tip> Button that transfers the latest jobfiles to the Submit screen. </Tip>
! Button that brings up the Select Jobs dialog box so you can transfer the
! latest jobfiles to the Submit screen for submission to a batch queue.
!
! Traps are run automatically if they have not have been run on the latest
! parameter values.  New workfiles are built automatically if existing ones are
! not consistent with the current .mwb information.  New jobs are built
! automatically if existing ones are not consistent with the current .mwb
! information.)
!</Help>
!
!<Help KEYWORD="NewMWBSession">
!<Tip> Button that clears all MWB information except the template WF. </Tip>
! Button that clears all information from the MWB screen except the template
! workfile.
!</Help>
!
!<Help KEYWORD="selectTemplate">
!<Tip> Button that brings up the Select Template Workfile dialog box. </Tip>
!
!</Help>
!
!<Help KEYWORD="templateJobName">
!<Tip> Text field showing name of selected template workfile. </Tip>
!
!</Help>
!
!<Help KEYWORD="templateProcessList">
!<Tip> Scrollable list of processes in the selected template workfile. </Tip>
!
!</Help>
!
!<Help KEYWORD="MBincJobname">
!<Tip> Button that accesses the Build Jobnames screen. </Tip>
!
!</Help>
!
!<Help KEYWORD="totalToBuild">
!<Tip> Text field showing the total number of new workfiles to build. </Tip>
! Enter the desired number of new workfiles to build.
!</Help>
!
!<Help KEYWORD="jobnameList">
!<Tip> List of jobnames for the new workfiles to build. </Tip>
!
!</Help>
!
!<Help KEYWORD="MWBipn">
!<Tip> Process number (IPN) for the changed parameter. </Tip>
!
!</Help>
!
!<Help KEYWORD="MWBprocess">
!<Tip> Process name for the changed parameter. </Tip>
!
!</Help>
!
!<Help KEYWORD="MWBkeyword">
!<Tip> Keyword for the changed parameter. </Tip>
!
!</Help>
!
!<Help KEYWORD="MWBParmShow">
!<Tip> Button that accesses the Build Parameter Values screen. </Tip>
! The Build Parameter Values screen allows you to specify how the parameter
! values change.
!</Help>
!
!<Help KEYWORD="MWBParmDelete">
!<Tip> Button that deletes the highlighted parameter change. </Tip>
! Click on the desired parameter row in the Changed Parameters list and press
! the Delete Changes button to delete that parameter from the Changed
! Parameters list.
!</Help>
!
!<Help KEYWORD="MWBParmClear">
!<Tip> Button that deletes all parameters from Changed Parameters list. </Tip>
!
!</Help>
!
!
!
!<Help KEYWORD="SJSeries">
!<Tip> Job series name for the current list of jobs. </Tip>
! Default = -
! Allowed = char 10
! Job series name for those jobs you wish to execute in a secified sequence.
! Entering a job series name is optional.
!</Help>
!
!<Help KEYWORD="SJReloadSeries">
!<Tip>This button displays the old series file selection dialog box.</Tip>
! Choose an old series file via the file selection dialog.
!</Help>
!
!<Help KEYWORD="SJIndependent">
!<Tip>Combo box to set series as dependent or independent.</Tip>
! Default = NO
! Allowed = YES
!           NO
! A dependent series requires that a job complete sucessfully before starting
! the next job(s) in the series.  In a independent series, the next job(s) are
! started even if the current job aborts.
!</Help>
!
!<Help KEYWORD="SJDate">
!<Tip> Starting date for the submitted job(s). </Tip>
! Default = current date
! Allowed = MM_DD_YYYY
!</Help>
!
!<Help KEYWORD="SJTime">
!<Tip> Starting time for the submitted job(s). </Tip>
! Default = current time
! Allowed = HH_MM
!</Help>
!
!<Help KEYWORD="SJJobSelect">
!<Tip> JOBNAME dialog box allows selection of a workfile or jobfile. </Tip>
! Default = -
! Allowed = char 15
! The JOBNAME dialog box allows the user to select workfiles or jobfiles to be
! submitted.
!</Help>
!
!<Help KEYWORD="SJJobs">
!<Tip> List of workfiles or jobfiles to be submitted. </Tip>
! Default = -
! Allowed = char 15
! Users may enter names directly in the list or use the dialog box.
!</Help>
!
!<Help KEYWORD="SJCount">
!<Tip> List of job series count numbers for jobs being submitted. </Tip>
! Default = -
! Allowed = int > 0
! A job series count number determines the execution sequence of jobs with the
! same job series name.  A job with a job series count of 1 is the first to
! execute in that job series.
!
! Users must enter numbers directly in the list.  Entering a job series count
! is optional.
!</Help>
!
!<Help KEYWORD="SJNext">
!<Tip> List of next job series names. </Tip>
! Default = -
! Allowed = char 10
! List of names of job series to begin execution upon completion of the job
! being submitted.
!
! Users must enter names directly in the list.  Entering a next job series name
! is optional.
!
! Job series you wish to start with a Next Job Series listing should be
! submitted in advance with the Submit Job screen with minimum Job Series Count
! of 2.  It will not start until a job completes that listed it in the Next Job
! Series list.
!</Help>
!
!<Help KEYWORD="SJSubmit">
!<Tip> This button submits the listed jobs and clears the screen. </Tip>
! Default =
! Allowed =
! Jobs will be submitted only when list entries are complete.
!</Help>
!
!<Help KEYWORD="SJClear">
!<Tip> This button clears all entries on the screen. </Tip>
! Default =
! Allowed =
!</Help>
!
!<Help KEYWORD="SJIncrement">
!<Tip> This button accesses the Increment Jobname pop-up dialog box. </Tip>
! Default =
! Allowed =
!</Help>
!
!<Help KEYWORD="SJUpdateSeries">
!<Tip>This button updates the series file and clears the screen.</Tip>
! No jobs are submitted.  The series file is written as displayed.
!</Help>
!
!
!
!<Help KEYWORD="SLJobs">
!<Tip> List of names of workfiles created in this session. </Tip>
! List is view-only.
!</Help>
!
!<Help KEYWORD="SLBuilt">
!<Tip> List of times when each jobfile in this session was built. </Tip>
! List is view-only.
!</Help>
!
!<Help KEYWORD="SLSubmitted">
!<Tip> List of times when each jobfile in this session was submitted. </Tip>
! List is view-only.
!</Help>
!
!<Help KEYWORD="SLSeries">
!<Tip> List of job series names for each job submitted. </Tip>
! List is view-only.
!</Help>
!
!<Help KEYWORD="SLCount">
!<Tip> List of job series count values for each job submitted. </Tip>
! List is view-only.
!</Help>
!
!<Help KEYWORD="SLNext">
!<Tip> List of next job series entries for each job submitted. </Tip>
! List is view-only.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module cfe_module

      use buildlist_module
      use cfebuildlist_module
      use cfegui_module
      use cfemenu_module
      use cfemwb_module
      use cfeprocess_module
      use cfesj_module
      use cfesl_module
      use cfestruct_module
      use cfeutil_module
      use cfewb_module
      use cfewindow_module
      use cfejava_module
      use cnfg_module
      use filebox_module
      use mfilebox_module
      use getsys_module
      use incjobname_module
      use pc_module
      use process_module

      implicit none

      private
      public :: cfe_create
      public :: cfe_initialize
      public :: cfe_update
      public :: cfe_delete

      character(len=100),public,save :: cfe_ident = &
       '$Id: cfe.f90,v 1.53 2006/09/18 13:32:40 Glover prod sps $'


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine cfe_create (obj)
      implicit none
      type(cfestruct),pointer        :: obj                           !argument



      integer                        :: window_id                     !local

      type(cfewindow_struct),pointer :: windowCurrent                 !local
      character(len=PC_LENGTH)       :: ctemp                         !local
      character(len=PC_LENGTH)       :: xml_name                      !local
      character(len=PC_LENGTH)       :: title                         !local
      character(len=10)              :: node                          !local

      integer                        :: ostype                        !local
      character(len=120)             :: binpath1                      !local

      nullify (windowCurrent)
      allocate (obj)

      nullify (obj%buildjob_dialog) ! jpa
      nullify (obj%editwork_dialog) ! jpa
      nullify (obj%editjob_dialog) ! jpa
      nullify (obj%editfile_dialog) ! jpa
      nullify (obj%viewwork_dialog) ! jpa
      nullify (obj%viewjob_dialog) ! jpa
      nullify (obj%viewreport_dialog) ! jpa
      nullify (obj%viewfile_dialog) ! jpa
      nullify (obj%cwd_dirs) ! jpa
      nullify (obj%custom_xml_temp) ! jpa
      nullify (obj%custom_xml) ! jpa
      nullify (obj%selectJobDialog) ! jpa
      nullify (obj%mselectJobDialog) ! jpa
      nullify (obj%oldProcessList) ! jpa
      nullify (obj%selectCurrentDialog) ! jpa
      nullify (obj%currentProcessList) ! jpa
      nullify (obj%subsetProcessList) ! jpa
      nullify (obj%oldWorkfileDisplayed) ! jpa
      nullify (obj%oldWorkfileList) ! jpa
      nullify (obj%currentWorkfile) ! jpa
      nullify (obj%undo_indexes) ! jpa
      nullify (obj%sourceProcessId) ! jpa
      nullify (obj%destinationProcessId) ! jpa
      nullify (obj%SJjobs_dialog) ! jpa
      nullify (obj%SJseries_dialog) ! jpa
      nullify (obj%SJlist_dialog) ! jpa
      nullify (obj%SJincjobname_dialog) ! jpa
      nullify (obj%SJjobs) ! jpa
      nullify (obj%SJcount) ! jpa
      nullify (obj%SJnext) ! jpa
      nullify (obj%SJjobs_temp) ! jpa
      nullify (obj%SJcount_temp) ! jpa
      nullify (obj%SJnext_temp) ! jpa
      nullify (obj%SLJobs) ! jpa
      nullify (obj%SLBuilt) ! jpa
      nullify (obj%SLSubmitted) ! jpa
      nullify (obj%SLSeries) ! jpa
      nullify (obj%SLCount) ! jpa
      nullify (obj%SLNext) ! jpa
      nullify (obj%templateProcessList) ! jpa
      nullify (obj%jobnameList) ! jpa
      nullify (obj%MWBipn) ! jpa
      nullify (obj%MWBprocess) ! jpa
      nullify (obj%MWBkeyword) ! jpa
      nullify (obj%MWBlist) ! jpa
      nullify (obj%templateJobDialog) ! jpa
      nullify (obj%templateWorkfile) ! jpa
      nullify (obj%incjobnameDialog) ! jpa
      nullify (obj%submitDialog) ! jpa
      nullify (obj%jobnameList_temp) ! jpa
      nullify (obj%masterBuffer) ! jpa
      nullify (obj%masterCardset) ! jpa

      call cfewindow_clear
      call cfewindow_create          (windowCurrent)
      call cfewindow_set_window_type (windowCurrent,'MAIN')
      call cfewindow_set_current     (windowCurrent)
      call cfewindow_get_window_id   (windowCurrent,window_id)
      call cfegui_set_main_window    (window_id)


      call getsys_username (obj%userid)
      call cfewb_create    (obj)
      call cfemwb_create   (obj)
      call cfesj_create    (obj)
      call cfesl_create    (obj)
      call cfemenu_create  (obj)

      call cfe_initialize  (obj)

      call cfeutil_get_session     (obj)
      call cfeutil_get_session_log (obj)


      call getsys_hostname(node)

      ostype = getsys_ostype();

      select case(ostype)
        case (GETSYS_SOLARIS)
          call cnfg_get_value("bin_path_solaris1", binpath1)
          !binpath1 = '/usr/app/sparc-sun-solaris/bin'
        case (GETSYS_LINUX)
          call cnfg_get_value("bin_path_linux1", binpath1)
          !binpath1 = '/usr/app/ix86-intel-linux/bin'
        case default
          call pc_error('Unknown ostype = ', ostype)
          return
      end select


      if (cfeutil_is_custom()) then
             xml_name        = 'guiCfeCustom.xml'
           if (obj%lnklib .eq. CFE_PRODLIB) then
             title           = 'prodlib'
             obj%prog_cfebld = trim(binpath1)//'/cfebld'
             obj%prog_cfesub = trim(binpath1)//'/cfesub'
             !obj%prog_icps is resolved in cfewb.f90
           else if (obj%lnklib .eq. CFE_BETALIB) then
             title           = 'betalib'
             obj%prog_cfebld = trim(binpath1)//'/cfebldbeta'
             obj%prog_cfesub = trim(binpath1)//'/cfesubbeta'
             !obj%prog_icps is resolved in cfewb.f90
           else if (obj%lnklib .eq. CFE_ALPHALIB) then
             title           = 'alphalib'
             obj%prog_cfebld = trim(binpath1)//'/cfebldalpha'
             obj%prog_cfesub = trim(binpath1)//'/cfesubalpha'
             !obj%prog_icps is resolved in cfewb.f90
           else   ! Open Source Version Here
             title           = 'Open Source Version'
             obj%prog_cfebld = trim(binpath1)//'/cfebld'
             obj%prog_cfesub = trim(binpath1)//'/cfesub'
             !obj%prog_icps is resolved in cfewb.f90
           end if
           title = 'CFEcustom - '//trim(title)//' - '//trim(node)// &
                            ' - '//cfeutil_get_platform()
      else
             xml_name        = 'guiCfe.xml'
           if (obj%lnklib .eq. CFE_PRODLIB) then
             title           = 'CFE'
             obj%prog_cfebld = trim(binpath1)//'/cfebld'
             obj%prog_cfesub = trim(binpath1)//'/cfesub'
             obj%prog_icps   = trim(binpath1)//'/icps_script'
           else if (obj%lnklib .eq. CFE_BETALIB) then
             title           = 'CFEbeta'
             obj%prog_cfebld = trim(binpath1)//'/cfebldbeta'
             obj%prog_cfesub = trim(binpath1)//'/cfesubbeta'
             obj%prog_icps   = trim(binpath1)//'/icpsbeta'
           else if (obj%lnklib .eq. CFE_ALPHALIB) then
             title           = 'CFEalpha'
             obj%prog_cfebld = trim(binpath1)//'/cfebldalpha'
             obj%prog_cfesub = trim(binpath1)//'/cfesubalpha'
             obj%prog_icps   = trim(binpath1)//'/icpsalpha'
           else   ! Open Source Version Here
             title           = 'Open Source Version'
             obj%prog_cfebld = trim(binpath1)//'/cfebld'
             obj%prog_cfesub = trim(binpath1)//'/cfesub'
             obj%prog_icps   = trim(binpath1)//'/icps_script'
           end if
      end if


      call pc_get ('INITIALIZEAPP',ctemp)
      call string_to_upper (ctemp)
      if (trim(ctemp) .eq. 'DIRECTAPPLICATION') then
        call cfejava_set_execution_mode (CFEJAVA_DIRECTAPP)
        obj%exemode = CFEJAVA_DIRECTAPP
      else
        call cfejava_set_execution_mode (CFEJAVA_CSAPP)
        obj%exemode = CFEJAVA_CSAPP
      endif
      call pc_remove_gui_action ('INITIALIZEAPP','MODIFYFIELD')

      call pc_put_gui_only ('WORKINGDIR','Working Environment = '//  &
                            trim(obj%userid)//'@'//trim(node)//':'//   &
                            trim(obj%working_dir))

      call cfegui_create (window_id,trim(title),trim(xml_name),.false.)

      return
      end subroutine cfe_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine cfe_delete (obj)
      implicit none
      type(cfestruct),pointer        :: obj                           !argument

      type(cfewindow_struct),pointer :: windowCurrent                 !local

      call cfewb_delete          (obj)
      call cfemwb_delete         (obj)
      call cfesj_delete          (obj)
      call cfesl_delete          (obj)
      call cfemenu_delete        (obj)
      call cfegui_delete         (obj%mainWindowID,'TERMINATEAPP')
      call cfewindow_get_current (windowCurrent)
      call cfewindow_delete      (windowCurrent)

      deallocate(obj)

      return
      end subroutine cfe_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine cfe_initialize (obj)
      implicit none
      type(cfestruct),pointer :: obj       ! arguments

      obj%exemode            = CFE_UNKNOWN
      obj%ScreenId           = 1
      obj%last_jobfile       = ' '
      obj%last_reportfile    = ' '
      obj%last_wb_action_mb3 = .false.

      obj%lnklib = cfeutil_library()
      call getsys_current_dir (obj%working_dir)

      if (obj%lnklib .eq. CFE_PRODLIB) then
        obj%process_maturity = PROCESS_production
      else if (obj%lnklib .eq. CFE_BETALIB) then
        obj%process_maturity = PROCESS_ALPHA
      else
        obj%process_maturity = PROCESS_RAW
      endif
      call process_set_view (obj%process_maturity)


      call cfewb_initialize   (obj)
      call cfemwb_initialize  (obj)
      call cfesj_initialize   (obj)
      call cfesl_initialize   (obj)

      call cfewb_update       (obj)
      call cfemwb_update      (obj)
      call cfesj_update       (obj)
      call cfesl_update       (obj)
      call cfemenu_initialize (obj)

      return
      end subroutine cfe_initialize


!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!


      subroutine cfe_update (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      type(cfewindow_struct),pointer      :: windowCurrent            !local
      integer                             :: window_id                !local
      character(len=PC_LENGTH)            :: window_type              !local
      type(cfewindow_struct),pointer      :: windowParent             !local
      character(len=PC_LENGTH)            :: parent_type              !local
      character(len=PC_LENGTH)            :: parent_keyword           !local
      integer                             :: parent_index             !local
      type(process_struct),pointer        :: process                  !local
      type(process_struct),pointer        :: previous                 !local
      type(buildlist_struct),pointer      :: parameter                !local
      character(len=PC_LENGTH)            :: ctemp                    !local
      character(len=PC_LENGTH),pointer    :: atemp(:)                 !local
      integer                             :: natemp                   !local
      integer                             :: itemp                    !local
      integer                             :: process_id               !local
      integer                             :: indx                     !local
      logical                             :: err                      !local

      object => obj                                        ! needed for traps

      call cfewindow_get_current (windowCurrent)
      if (.not. associated(windowCurrent)) then
        call pc_error ('Invalid Window')
        return
      endif

      call cfewindow_get_window_id   (windowCurrent,window_id)
      call cfewindow_get_window_type (windowCurrent,window_type)

      select case (trim(window_type))
        case ('MAIN')
          if      (pc_gui_action_present('WorkfileBuilder' ,'ButtonPress') .or.&
                 pc_gui_action_present('WorkfileBuilderTB' ,'ButtonPress')) then
            obj%ScreenId = 1
            call pc_jump_screen ('WORKFILEBUILDERSCREEN')

          else if (pc_gui_action_present('BuildJobfile'    ,'ButtonPress') .or.&
                   pc_gui_action_present('BuildJobfileTB'  ,'ButtonPress')) then
            call cfemenu_option (obj,'BUILDJOBFILE')

          else if (pc_gui_action_present('MultiBuilder'    ,'ButtonPress') .or.&
                   pc_gui_action_present('MultiBuilderTB'  ,'ButtonPress')) then
            obj%ScreenId = 2
            call pc_jump_screen ('MULTIBUILDERSCREEN')

          else if (pc_gui_action_present('Submit_Job'      ,'ButtonPress') .or.&
                   pc_gui_action_present('Submit_JobTB'    ,'ButtonPress')) then
            obj%ScreenId = 3
            call pc_jump_screen ('SUBMITJOBSCREEN')

          else if (pc_gui_action_present('SessionLog'      ,'ButtonPress') .or.&
                   pc_gui_action_present('SessionLogTB'    ,'ButtonPress')) then
            obj%ScreenId = 4
            call pc_jump_screen ('SESSIONLOGSCREEN')

          else if (pc_gui_action_present('Exit'            ,'ButtonPress') .or.&
                   pc_gui_action_present('ExitTB'          ,'ButtonPress')) then
            call cfeutil_save_session     (obj)
            call cfeutil_save_session_log (obj)
            call cfe_delete (obj)

          else if (pc_gui_action_present('EditWorkfile'    ,'ButtonPress') .or.&
                   pc_gui_action_present('EditWorkfileTB'  ,'ButtonPress')) then
            call cfemenu_option (obj,'EDITWORKFILE')

          else if (pc_gui_action_present('EditJobfile'     ,'ButtonPress') .or.&
                   pc_gui_action_present('EditJobfileTB'   ,'ButtonPress')) then
            call cfemenu_option (obj,'EDITJOBFILE')

          else if (pc_gui_action_present('EditFile'        ,'ButtonPress') .or.&
                   pc_gui_action_present('EditFileTB'      ,'ButtonPress')) then
            call cfemenu_option (obj,'EDITFILE')

          else if (pc_gui_action_present('ViewWorkfile'    ,'ButtonPress') .or.&
                   pc_gui_action_present('ViewWorkfileTB'  ,'ButtonPress')) then
            call cfemenu_option (obj,'VIEWWORKFILE')

          else if (pc_gui_action_present('ViewJobfile'     ,'ButtonPress') .or.&
                   pc_gui_action_present('ViewJobfileTB'   ,'ButtonPress')) then
            call cfemenu_option (obj,'VIEWJOBFILE')

          else if (pc_gui_action_present('ViewReportfile'  ,'ButtonPress') .or.&
                   pc_gui_action_present('ViewReportfileTB','ButtonPress')) then
            call cfemenu_option (obj,'VIEWREPORTFILE')

          else if (pc_gui_action_present('ViewFile'        ,'ButtonPress') .or.&
                   pc_gui_action_present('ViewFileTB'      ,'ButtonPress')) then
            call cfemenu_option (obj,'VIEWFILE')

          else if (pc_gui_action_present('ViewCPSheaders'  ,'ButtonPress') .or.&
                   pc_gui_action_present('ViewCPSheadersTB','ButtonPress')) then
            call cfemenu_option (obj,'VIEWCPSHEADERS')

          else if (pc_gui_action_present('ChangeDirectory' ,'ButtonPress') .or.&
                 pc_gui_action_present('ChangeDirectoryTB' ,'ButtonPress')) then
            call cfemenu_option (obj,'CHANGEDIRECTORY')
            call cfemenu_update (obj,'CHANGEDIRECTORY')

          else if (pc_gui_action_present('ToolbarEditor'   ,'ButtonPress') .or.&
                   pc_gui_action_present('ToolbarEditorTB' ,'ButtonPress')) then
            call pc_error ('This option is not supported at this time')

          else if (pc_gui_action_present('UserPreferences' ,'ButtonPress') .or.&
                 pc_gui_action_present('UserPreferencesTB' ,'ButtonPress')) then
            call cfemenu_option (obj,'USERPREFERENCES')
            call cfemenu_update (obj,'USERPREFERENCES')

          else if (pc_gui_action_present('ProgPreferences' ,'ButtonPress') .or.&
                 pc_gui_action_present('ProgPreferencesTB' ,'ButtonPress')) then
            call cfemenu_option (obj,'PROGPREFERENCES')
            call cfemenu_update (obj,'PROGPREFERENCES')

          else if (pc_gui_action_present('BuildList'       ,'ButtonPress') .or.&
                   pc_gui_action_present('BuildListTB'     ,'ButtonPress')) then
            call cfemenu_option (obj,'BUILDLIST')

          else if (pc_gui_action_present('XPBS'            ,'ButtonPress') .or.&
                   pc_gui_action_present('XPBSTB'          ,'ButtonPress')) then
            call cfemenu_execute (obj,'XPBS',err)

          else if (pc_gui_action_present('XTPIOADMIN'      ,'ButtonPress') .or.&
                   pc_gui_action_present('XTPIOADMINTB'    ,'ButtonPress')) then
            call cfemenu_execute (obj,'XTPIOADMIN',err)

          else if (pc_gui_action_present('CIU'            ,'ButtonPress') .or.&
                   pc_gui_action_present('CIUTB'          ,'ButtonPress')) then
            call cfemenu_execute (obj,'CIU',err)

          else if (pc_gui_action_present('CBYT'            ,'ButtonPress') .or.&
                   pc_gui_action_present('CBYTTB'          ,'ButtonPress')) then
            call cfemenu_execute (obj,'CBYT',err)

          else if (pc_gui_action_present('VA'              ,'ButtonPress') .or.&
                   pc_gui_action_present('VATB'            ,'ButtonPress')) then
            call cfemenu_execute (obj,'VA',err)

          else if (pc_gui_action_present('CSV'             ,'ButtonPress') .or.&
                   pc_gui_action_present('CSVTB'           ,'ButtonPress')) then
            call cfemenu_execute (obj,'CSV',err)

          else if (pc_gui_action_present('CFG'             ,'ButtonPress') .or.&
                   pc_gui_action_present('CFGTB'           ,'ButtonPress')) then
            call cfemenu_execute (obj,'CFG',err)

          else if (pc_gui_action_present('JOBMON'          ,'ButtonPress') .or.&
                   pc_gui_action_present('JOBMONTB'        ,'ButtonPress')) then
            call cfemenu_execute (obj,'JOBMON',err)

          else if (pc_gui_action_present('ApplicationHelp' ,'ButtonPress') .or.&
                 pc_gui_action_present('ApplicationHelpTB' ,'ButtonPress')) then
            call cfegui_showhelp()

          else if (pc_gui_action_present('ApplicationVersion',                 &
                                                            'ButtonPress') .or.&
              pc_gui_action_present('ApplicationVersionTB' ,'ButtonPress')) then
            call cfemenu_option (obj,'APPLICATIONVERSION')

          else if (pc_gui_action_present('DOC'             ,'ButtonPress') .or.&
                   pc_gui_action_present('DOCTB'           ,'ButtonPress')) then
            call cfemenu_execute (obj,'DOC',err)

          else if (pc_gui_action_present('WorkfileBuilderScreen',              &
                                                            'EnterScreen')) then
            obj%ScreenId = 1

          else if (pc_gui_action_present('MultiBuilderScreen',                 &
                                                            'EnterScreen')) then
            obj%ScreenId = 2
            call cfemwb_update (obj)

          else if (pc_gui_action_present('SubmitJobScreen' ,'EnterScreen')) then
            obj%ScreenId = 3
            call cfesj_update (obj)

          else if (pc_gui_action_present('SessionLogScreen','EnterScreen')) then
            obj%ScreenId = 4

          else if (obj%ScreenId.eq.1) then
            call cfewb_update (obj)

          else if (obj%ScreenId.eq.2) then
            call cfemwb_update (obj)

          else if (obj%ScreenId.eq.3) then
            call cfesj_update (obj)
          endif

        case ('CFE_MENU')
          call cfewindow_get_keyword (windowCurrent ,parent_keyword)

          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            call cfemenu_execute (obj,parent_keyword,err)
            if (.not. err) then
              call cfegui_delete        (window_id,'Close')
              call cfewindow_delete     (windowCurrent)
              call cfewindow_get_current(windowCurrent)
              call cfewb_update         (obj)
            endif

          else if (pc_gui_action_present('Apply'           ,'ButtonPress')) then
            call cfemenu_execute      (obj,parent_keyword,err)

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            call cfegui_delete        (window_id,'Close')
            call cfewindow_delete     (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call cfegui_showhelp()

          else
            call cfemenu_update       (obj,parent_keyword)
          endif

        case ('PROCESS')
          call cfewindow_get_keyword (windowCurrent ,parent_keyword)
          call cfewindow_get_index   (windowCurrent ,parent_index)
          call cfewindow_get_process (windowCurrent ,process)

          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            call cfewindow_get_parent (windowCurrent,windowParent)
            call cfewindow_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROCESS') then
              call cfeprocess_update (process,parent_keyword,parent_index, &
                                          err)
              if (.not. err) then
                call cfeprocess_delete    (process,parent_keyword)
                call cfegui_delete        (window_id,'Close')
                call cfewindow_delete     (windowCurrent)
                call cfewindow_get_current(windowCurrent)
              endif

            else                 ! Child Window of Process

              nullify(previous)
              call process_get_previous      (process,previous)
              call process_copy_global_cards (previous,process)
              call process_update            (process,from_cards=.false.,  &
                                              frontend=.true.,found_errors=err)
              if (.not. err) then
                call cfegui_delete        (window_id,'Close')
                call cfewindow_delete     (windowCurrent)
                call cfewindow_get_current(windowCurrent)
              endif
            endif

          else if (pc_gui_action_present('Apply'           ,'ButtonPress')) then
            call cfewindow_get_parent (windowCurrent,windowParent)
            call cfewindow_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROCESS') then
              call cfeprocess_update (process,parent_keyword,parent_index, &
                                       err)

            else                 ! Child Window of Process

              nullify(previous)
              call process_get_previous      (process,previous)
              call process_copy_global_cards (previous,process)
              call process_update            (process,from_cards=.false.,  &
                                              frontend=.true.,found_errors=err)
            endif

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            call cfewindow_get_parent (windowCurrent,windowParent)
            call cfewindow_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROCESS') then
              call cfeprocess_delete (process,parent_keyword)
            endif
            call cfegui_delete        (window_id,'Close')
            call cfewindow_delete     (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else if (pc_gui_action_present('Reset'           ,'ButtonPress')) then
            call cfeprocess_reset (process, parent_keyword, parent_index)

          else if (pc_gui_action_present('ProjectDefault'  ,'ButtonPress')) then
            call cfewb_save_defaults (process,'PROJECT DEFAULTS',  &
                                       obj%working_dir)

          else if (pc_gui_action_present('UserDefault'     ,'ButtonPress')) then
            call cfewb_save_defaults (process,'USER DEFAULTS',  &
                                       obj%working_dir)

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call cfegui_showhelp()

          else if (pc_gui_action_present('ProcessDefaults' ,'ModifyField')) then
            call cfewb_change_defaults (process)

          else if (pc_gui_action_present('ProcessList'     ,'ModifyField')) then
            call pc_get ('ProcessList',ctemp)
            if (len_trim(ctemp) .gt. 0) then
              call string_cc2ii(trim(ctemp),process_id)
              call process_get_ipn (process,itemp)
              if (itemp .ne. process_id) then
                call cfeprocess_update (process,parent_keyword,parent_index, &
                                         err)
                if (.not. err) then
                  call cfeprocess_delete (process,parent_keyword)
                  call cfeprocess_create ('CURRENTPROCESSLIST',process_id,  &
                                           .true.)
                endif
              endif
            endif

          else if (pc_gui_action_present('ProcessListLeft' ,'ButtonPress')) then
            if (parent_index-1 .lt. 1) then
              call pc_error ('End of Job')
            else
              process_id = parent_index - 1
              call cfeprocess_update (process,parent_keyword,parent_index, &
                                       err)
              if (.not. err) then
                call cfeprocess_delete (process,parent_keyword)
                call cfeprocess_create ('CURRENTPROCESSLIST',process_id,  &
                                         .true.)
              endif
            endif

          else if (pc_gui_action_present('ProcessListRight','ButtonPress')) then
            if (parent_index+1 .gt. obj%NcurrentProcessList) then
              call pc_error ('End of Job')
            else
              process_id = parent_index + 1
              call cfeprocess_update (process,parent_keyword,parent_index, &
                                       err)
              if (.not. err) then
                call cfeprocess_delete (process,parent_keyword)
                call cfeprocess_create ('CURRENTPROCESSLIST',process_id,  &
                                         .true.)
              endif
            endif

          else if (pc_gui_action_present('Close'           ,'ButtonPress')) then
            call cfewindow_get_parent (windowCurrent,windowParent)
            call cfewindow_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROCESS') then
              call cfeprocess_delete (process,parent_keyword)
            endif
            call cfegui_delete        (window_id,'CLOSE')
            call cfewindow_delete     (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else
            nullify(previous)
            call process_get_previous      (process,previous)
            call process_copy_global_cards (previous,process)
            call process_update            (process)
          endif

        case ('OLDPROCESS')
          call cfewindow_get_process (windowCurrent ,process)
          call cfewindow_get_index   (windowCurrent ,parent_index)

          if      (pc_gui_action_present('Close'           ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else if (pc_gui_action_present('ProjectDefault'  ,'ButtonPress')) then
            call cfewb_save_defaults (process,'PROJECT DEFAULTS',  &
                                       obj%working_dir)

          else if (pc_gui_action_present('UserDefault'     ,'ButtonPress')) then
            call cfewb_save_defaults (process, 'USER DEFAULTS',  &
                                       obj%working_dir)

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call cfegui_showhelp()

          else if (pc_gui_action_present('ProcessList'     ,'ModifyField')) then
            call pc_get ('ProcessList',ctemp)
            if (len_trim(ctemp) .gt. 0) then
              call string_cc2ii(trim(ctemp),process_id)
              call cfeprocess_create ('OLDPROCESSLIST',process_id,.true.)
            endif

          else if (pc_gui_action_present('ProcessListLeft' ,'ButtonPress')) then
            if (parent_index-1 .lt. 1) then
              call pc_error ('End of Job')
            else
              process_id = parent_index - 1
              call cfeprocess_create ('OLDPROCESSLIST',process_id,.true.)
            endif

          else if (pc_gui_action_present('ProcessListRight','ButtonPress')) then
            if (parent_index+1 .gt. obj%NoldProcessList) then
              call pc_error ('End of Job')
            else
              process_id = parent_index + 1
              call cfeprocess_create ('OLDPROCESSLIST',process_id,.true.)
            endif

          endif

        case ('LISTPROCESS')
          call cfewindow_get_process        (windowCurrent ,process)
          if      (pc_gui_action_present('Close'           ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call process_delete   (process)
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call cfegui_showhelp()
          endif

        case ('TEMPLATEPROCESS')
          call cfewindow_get_process (windowCurrent ,process)
          call cfewindow_get_index   (windowCurrent ,parent_index)

          if      (pc_gui_action_present('Close'           ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call cfegui_showhelp()

          else if (pc_gui_action_present('ProcessList'     ,'ModifyField')) then
            call pc_get ('ProcessList',ctemp)
            if (len_trim(ctemp) .gt. 0) then
              call string_cc2ii(trim(ctemp),process_id)
              call cfeprocess_create ('TEMPLATEPROCESSLIST',process_id,  &
                                       .true.)
            endif

          else if (pc_gui_action_present('ProcessListLeft' ,'ButtonPress')) then
            if (parent_index-1 .lt. 1) then
              call pc_error ('End of Job')
            else
              process_id = parent_index - 1
              call cfeprocess_create ('TEMPLATEPROCESSLIST',process_id,  &
                                       .true.)
            endif

          else if (pc_gui_action_present('ProcessListRight','ButtonPress')) then
            if (parent_index+1 .gt. obj%NtemplateProcessList) then
              call pc_error ('End of Job')
            else
              process_id = parent_index + 1
              call cfeprocess_create ('TEMPLATEPROCESSLIST',process_id,  &
                                       .true.)
            endif

          else
            call cfebuildlist_create (obj,process)

          endif

        case ('PARAMETER')
          call cfewindow_get_process   (windowCurrent ,process)
          call cfewindow_get_parameter (windowCurrent ,parameter)

          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            if (associated(process)) then
              call cfemwb_parameter_update (obj,process,parameter)
              call cfebuildlist_delete     (parameter)
            else
              call buildlist_get_name        (parameter,parent_keyword)
              nullify(atemp)
              call buildlist_alloc_valuelist (parameter,atemp,natemp)
              if (natemp .gt. 0) then
                call pc_put_gui (parent_keyword,'ModifyIndex'    ,1)
                call pc_put_gui (parent_keyword,'ReplaceElements',atemp,natemp)
                if (associated(atemp)) deallocate(atemp)
              endif
              call cfemwb_update       (obj)
              call cfebuildlist_delete (parameter)
            endif
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)
          else if (pc_gui_action_present('Apply'           ,'ButtonPress')) then
            call cfemwb_parameter_update (obj,process,parameter)
          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            call cfebuildlist_delete   (parameter)
            call cfegui_delete         (window_id,'CLOSE')
            call cfewindow_delete      (windowCurrent)
            call cfewindow_get_current (windowCurrent)
          else if (pc_gui_action_present('Close'           ,'ButtonPress')) then
            call cfebuildlist_delete   (parameter)
            call cfegui_delete         (window_id,'CLOSE')
            call cfewindow_delete      (windowCurrent)
            call cfewindow_get_current (windowCurrent)
          else if (pc_gui_action_present('Reset'           ,'ButtonPress')) then
            call cfebuildlist_reset (obj,process,parameter)
          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call cfegui_showhelp()
          else if (pc_gui_action_present('ParameterList'   ,'ModifyField')) then
            call pc_get ('ParameterList',ctemp)
            call pc_remove_gui_action ('ParameterList','ModifyField')
            indx = index(ctemp,' ')-1
            if (indx .gt. 0) then
              call string_cc2ii(ctemp(1:indx),itemp)
              if (itemp .ne. obj%MWBparmselected) then
                call cfemwb_parameter_update (obj,process,parameter)
                obj%MWBparmselected = itemp
                call cfebuildlist_delete (parameter)
                call cfebuildlist_create (obj,  &
                                         obj%MWBipn(obj%MWBparmselected),  &
                                         obj%MWBprocess(obj%MWBparmselected),  &
                                         obj%MWBkeyword(obj%MWBparmselected),  &
                                         .true.)
              endif
            endif
          else if (pc_gui_action_present('RunTraps'        ,'ButtonPress')) then
            call cfemwb_parameter_run_traps (obj,process,parameter)
          else
            call cfebuildlist_update (parameter)
          endif


        case ('MFILEBOX')

          call cfewindow_get_keyword (windowCurrent ,parent_keyword)
          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)
            select case (trim(parent_keyword))

              case ('OLDJOBNAME')
                if(associated(atemp)) deallocate(atemp)
                call mfilebox_selections (obj%mselectJobDialog, atemp, natemp)
                call cfewb_append_to_worklist(atemp, natemp)
                call cfewb_update (obj)

              case default

                call cfewindow_get_parameter (windowCurrent ,parameter)
                if (associated(parameter)) then
                  call pc_put_gui (trim(parent_keyword),'WindowType','MFILEBOX')
                  call buildlist_update (parameter)
                else
                  call pc_put_gui (trim(parent_keyword),'WindowType','MFILEBOX')
                  nullify(previous)
                  call cfewindow_get_process     (windowCurrent,process)
                  call process_get_previous      (process,previous)
                  call process_copy_global_cards (previous,process)
                  call process_update            (process)
                endif

            end select

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else

            select case (trim(parent_keyword))

              case ('OLDJOBNAME')
                call mfilebox_update (obj%mselectJobDialog)

              case default
                call cfewindow_get_parameter (windowCurrent ,parameter)
                if (associated(parameter)) then
                  call pc_put_gui (trim(parent_keyword),'WindowType','MFILEBOX')
                  call buildlist_update (parameter)
                else
                  call pc_put_gui (trim(parent_keyword),'WindowType','MFILEBOX')
                  nullify(previous)
                  call cfewindow_get_process     (windowCurrent,process)
                  call process_get_previous      (process,previous)
                  call process_copy_global_cards (previous,process)
                  call process_update            (process)
                endif

            end select

          endif


        case ('FILEBOX')

          call cfewindow_get_keyword (windowCurrent ,parent_keyword)
          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)
            select case (trim(parent_keyword))
              case ('CURRENTJOBNAME')
                call filebox_selection (obj%selectCurrentDialog, ctemp)
                if (len_trim(ctemp) .gt. 0) then
                  call pc_put_gui (parent_keyword,'ModifyField',ctemp)
                  call cfewb_update     (obj)
                else
                  call pc_warning ('No Current Workfile was selected')
                endif

              case ('TEMPLATEJOBNAME')
                call filebox_selection (obj%templateJobDialog, ctemp)
                if (len_trim(ctemp) .gt. 0) then
                  call pc_put_gui (parent_keyword,'ModifyField',ctemp)
                  call cfemwb_update (obj)
                else
                  call pc_warning ('No Template Workfile was selected')
                endif

              case ('SJJOBS')
                call cfesj_execute_filebox (obj)
                call cfesj_update          (obj)

              case ('SJJOBSLIST')
                call filebox_selection (obj%SJlist_dialog,obj%SJjobslist)
                call pc_put_gui (parent_keyword,'ModifyField',obj%SJjobslist)
                call cfesj_update     (obj)

              case ('SJCOUNTLIST')
                call filebox_selection (obj%SJlist_dialog,obj%SJcountlist)
                call pc_put_gui (parent_keyword,'ModifyField',obj%SJcountlist)
                call cfesj_update     (obj)

              case ('SJNEXTLIST')
                call filebox_selection (obj%SJlist_dialog,obj%SJnextlist)
                call pc_put_gui (parent_keyword,'ModifyField',obj%SJnextlist)
                call cfesj_update     (obj)

              case ('SJSERIES')
                call filebox_selection (obj%SJseries_dialog,obj%SJseries)
                call pc_put_gui (parent_keyword,'ModifyField',obj%SJseries)
                call cfesj_update     (obj)

              case ('BUILDJOBFILE')
                call filebox_selection (obj%buildjob_dialog, obj%buildjobfile)
                call pc_put_gui (parent_keyword,'ModifyField',obj%buildjobfile)
                call cfemenu_update   (obj,'BUILDJOBFILE')

              case ('EDITWORKFILE')
                call filebox_selection (obj%editwork_dialog, obj%editworkfile)
                call pc_put_gui (parent_keyword,'ModifyField',obj%editworkfile)
                call cfemenu_update   (obj,'EDITWORKFILE')

              case ('EDITJOBFILE')
                call filebox_selection (obj%editjob_dialog, obj%editjobfile)
                call pc_put_gui (parent_keyword,'ModifyField',obj%editjobfile)
                call cfemenu_update   (obj,'EDITJOBFILE')

              case ('EDITFILE')
                call filebox_selection (obj%editfile_dialog, obj%editfile)
                call pc_put_gui (parent_keyword,'ModifyField',obj%editfile)
                call cfemenu_update   (obj,'EDITFILE')

              case ('VIEWWORKFILE')
                call filebox_selection (obj%viewwork_dialog, obj%viewworkfile)
                call pc_put_gui (parent_keyword,'ModifyField',obj%viewworkfile)
                call cfemenu_update   (obj,'VIEWWORKFILE')

              case ('VIEWJOBFILE')
                call filebox_selection (obj%viewjob_dialog, obj%viewjobfile)
                call pc_put_gui (parent_keyword,'ModifyField',obj%viewjobfile)
                call cfemenu_update   (obj,'VIEWJOBFILE')

              case ('VIEWREPORTFILE')
                call filebox_selection(obj%viewreport_dialog,obj%viewreportfile)
                call pc_put_gui(parent_keyword,'ModifyField',obj%viewreportfile)
                call cfemenu_update   (obj,'VIEWREPORTFILE')

              case ('VIEWFILE')
                call filebox_selection (obj%viewfile_dialog, obj%viewfile)
                call pc_put_gui (parent_keyword,'ModifyField',obj%viewfile)
                call cfemenu_update   (obj,'VIEWFILE')

              case default

                call cfewindow_get_parameter (windowCurrent ,parameter)
                if (associated(parameter)) then
                  call pc_put_gui (trim(parent_keyword),'WindowType','FILEBOX')
                  call buildlist_update (parameter)
                else
                  call pc_put_gui (trim(parent_keyword),'WindowType','FILEBOX')
                  nullify(previous)
                  call cfewindow_get_process    (windowCurrent ,process)
                  call process_get_previous      (process,previous)
                  call process_copy_global_cards (previous,process)
                  call process_update            (process)
                endif
            end select

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)

          else
            select case (trim(parent_keyword))
              case ('CURRENTJOBNAME')
                call filebox_update (obj%selectCurrentDialog)
                call pc_remove_gui_action ('FILEBOX_DIRECTORIES',  &
                                                              'REPLACEELEMENTS')
                call pc_remove_gui_action ('FILEBOX_FILTER'     ,'MODIFYFIELD' )

              case ('TEMPLATEJOBNAME')
                call filebox_update (obj%templateJobDialog)

              case ('BUILDJOBFILE')
                call filebox_update (obj%buildjob_dialog)

              case ('SJJOBS')
                call mfilebox_update (obj%SJjobs_dialog)

              case ('SJJOBSLIST')
                call filebox_update (obj%SJlist_dialog)

              case ('SJCOUNTLIST')
                call filebox_update (obj%SJlist_dialog)

              case ('SJNEXTLIST')
                call filebox_update (obj%SJlist_dialog)

              case ('SJSERIES')
                call filebox_update (obj%SJseries_dialog)

              case ('EDITWORKFILE')
                call filebox_update (obj%editwork_dialog)

              case ('EDITJOBFILE')
                call filebox_update (obj%editjob_dialog)

              case ('EDITFILE')
                call filebox_update (obj%editfile_dialog)

              case ('VIEWWORKFILE')
                call filebox_update (obj%viewwork_dialog)

              case ('VIEWJOBFILE')
                call filebox_update (obj%viewjob_dialog)

              case ('VIEWREPORTFILE')
                call filebox_update (obj%viewreport_dialog)

              case ('VIEWFILE')
                call filebox_update (obj%viewfile_dialog)

              case default
                call cfewindow_get_parameter (windowCurrent ,parameter)
                if (associated(parameter)) then
                  call pc_put_gui (trim(parent_keyword),'WindowType','FILEBOX')
                  call buildlist_update (parameter)
                else
                  call pc_put_gui (trim(parent_keyword),'WindowType','FILEBOX')
                  nullify(previous)
                  call cfewindow_get_process    (windowCurrent ,process)
                  call process_get_previous      (process,previous)
                  call process_copy_global_cards (previous,process)
                  call process_update            (process)
                endif
            end select
          endif

        case ('INCJOBNAME')
          call cfewindow_get_keyword (windowCurrent ,parent_keyword)
          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            call cfegui_delete    (window_id,'CLOSE')
            call cfewindow_delete (windowCurrent)
            call cfewindow_get_current(windowCurrent)
            select case (trim(parent_keyword))
              case ('MBINCJOBNAME')
                call incjobname_selection (obj%incjobnameDialog,  &
                                             obj%jobnameList,obj%NjobnameList)
                call cfemwb_update (obj)

              case ('SJJOBS')
                call cfesj_execute_incjobname (obj)
                call cfesj_update (obj)

              case ('SUBMITJOBS')
                call cfemwb_execute_submitjobs (obj)
            end select

          else if (pc_gui_action_present('Preview'         ,'ButtonPress')) then
            select case (trim(parent_keyword))
              case ('MBINCJOBNAME')
                call incjobname_selection (obj%incjobnameDialog,  &
                                             obj%jobnameList,obj%NjobnameList)
                call cfemwb_update (obj)
                call cfewindow_get_parent (windowCurrent,windowParent)
                call cfewindow_set_current(windowParent)

              case ('SJJOBS')
                call cfesj_execute_incjobname (obj)
                call cfesj_update (obj)
                call cfewindow_get_parent (windowCurrent,windowParent)
                call cfewindow_set_current(windowParent)
            end select

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            select case (trim(parent_keyword))
              case ('MBINCJOBNAME')
                call cfegui_delete    (window_id,'CLOSE')
                call cfewindow_delete (windowCurrent)
                call cfewindow_get_current(windowCurrent)
                call pc_put_gui ('JOBNAMELIST','ReplaceElements',  &
                                  obj%jobnameList_temp,obj%NjobnameList_temp)
                call cfemwb_update (obj)

              case ('SJJOBS')
                call cfegui_delete    (window_id,'CLOSE')
                call cfewindow_delete (windowCurrent)
                call cfewindow_get_current(windowCurrent)
                call cfesj_reset_incjobname (obj)
                call cfesj_update (obj)

              case ('SUBMITJOBS')
                call cfegui_delete    (window_id,'CLOSE')
                call cfewindow_delete (windowCurrent)
                call cfewindow_get_current(windowCurrent)
            end select

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call cfegui_showhelp()

          else
            select case (trim(parent_keyword))
              case ('MBINCJOBNAME')
                call incjobname_update (obj%incjobnameDialog)

              case ('SJJOBS')
                call incjobname_update (obj%SJincjobname_dialog)

              case ('SUBMITJOBS')
                call incjobname_update (obj%submitDialog)
            end select
          endif

      end select


      return
      end subroutine cfe_update


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfe_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

