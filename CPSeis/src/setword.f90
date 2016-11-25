!<CPS_v1 type="PROCESS"/>
!!------------------------------ setword.f90 --------------------------------!!
!!------------------------------ setword.f90 --------------------------------!!
!!------------------------------ setword.f90 --------------------------------!!

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
! Name       : SETWORD   (SET Header WORDs) [Includes former AZMTH and SETW64.]
! Category   : headers
! Written    : 1988-09-14   by: Tom Stoeckley
! Revised    : 2007-11-06   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Reset header word values based on various calculations.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! IF RUN IN PARALLEL MODE, HDR_SEQUENCE IS NOT MODIFIED on INPUT!  IT MUST BE
! set BEFORE running SETWORD in PARALLEL. (You could select "Non-Parallel" to
! do that task, then run it again in parallel later in the job flow.)
!
! SETWORD will set the HDR_SEQUENCE word (header word #1) before any other
! action is performed.  This value may be modified by SETWORD tasks before
! being output from SETWORD, if HDR_SEQUENCE (hwd# 1) is selected as output
! header number for any such task.  This function cannot be disabled.
!
! SETWORD provides extensive options for manipulating header words using
! various forms of header arithmetic.  Users may choose a sequence of tasks 
! (maximum of 200) from the available tasks.  Tasks may be chosen in any 
! desired order and individual tasks may be repeated consecutively within the 
! chosen sequence.  SETWORD will execute the tasks in the order specified.  
! Each task is executed and header words updated prior to the next task being 
! executed.
!
! Each task consists of an identifying keyword and an associated set of 
! parameters necessary to describe the desired action.  Once a task is 
! chosen and its associated parameters specified, that task and its values 
! are displayed on the GUI.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
! Users may rely on the output value in header word 1 (trace sequence number) 
! from this process, because SETWORD sets this header immediately upon input 
! of the trace (in case it was not properly set by the previous process).
! This is set irrespective of the value of HDR_FLAG.  NOTE:  Header word 1
! may be specified as the output word of a task, which would clobber the
! default sequential behavior of header word 1.
!
! User Defined Headers 
! In addition to the user defined header words 48 - 55, header words 65 and 
! above are also available as user defined header words.  These are useful in 
! header arithmetic procedures when it is desirable to have access to original 
! values of headers that will be changed later, as a temporary "store" of 
! header values, to save floating point values of headers that will be rounded 
! to whole numbers, for holding intermediate results of complicated 
! calculations and other uses.
!
! Job Data parameter NWIH (Number of Words In Header) must be set to include
! all header words you wish to use.
!
! Note on Repeated Header Words 
! SETWORD tasks are written so that the result of a calculation may overwrite
! one of the header words used in the calculation.  For example, ABS(7,7) is 
! permitted and means that the value in HDR 7 is replaced by its absolute value.
!
! Sequence
! All tasks are performed in the sequence selected, with the default sequence
! word (HDR_SEQUENCE/Hwd# 1) being set before any other operation.
!
! Flagword 
! Additional flexibility is provided by the flagword option (HDR_FLAG).  If
! HDR_FLAG > 0, then only those traces that are flagged will be affected by
! SETWORD (except the trace sequence number in Header Word 1).
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a single-trace.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process may alter header words.
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED      
!
! Name       Description                             Action taken
! ----       -----------                             ------------
! NWIH       number of words in trace header         used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!   1     HDR_SEQUENCE               Renumbered (UNLESS "PARALLEL MODE")
!         HDR_FLAG                   flagword
! any     any                        any header may be used or changed
!   6     HDR_OFFSET                 Changed for OFFSET_ABS and others
!  11     HDR_SOURCE_XLOC            Used to determine Azimuth and others
!  12     HDR_SOURCE_YLOC            Used to determine Azimuth and others
!  14     HDR_RECEIVER_XLOC          Used to determine Azimuth and others
!  15     HDR_RECEIVER_YLOC          Used to determine Azimuth and others
!  17     HDR_MIDPOINT_XLOC          Changed for BIN_CENT and others
!  18     HDR_MIDPOINT_YLOC          Changed for BIN_CENT and others
!  46     HDR_SOURCE_GP              Used to set HDR_OFFSET
!  47     HDR_RECEIVER_GP            Used to set HDR_OFFSET
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       REVISION HISTORY FOR SETWORD               
!
!     Date       Author       Description
!     ----       ------       -----------
! 63. 2007-11-06 Stoeckley    Add a blank option to the list of options for the
!                              FUNCTION parameter.  This is a temporary
!                              workaround for the fact that old SS flows
!                              containing SETWORD often have the FUNCTION
!                              parameter set to blank.  This was the result of
!                              old versions of SETWORD which set that parameter
!                              to a blank when it was not needed.  In order
!                              for such flows to be read in SSdev or SSnightly,
!                              the blank option is added to the list so the
!                              parameter cache will not complain.  The complaint
!                              would interrupt the opening of the flow by SSdev
!                              or SSnightly because the menu building would
!                              throw an exception.
! 62. 2007-07-12 Stoeckley    Add test as to whether the process is running in
!                              CPS or SeisSpace and use that to set some
!                              parameters visible/sensitive or not; remove
!                              the "only" clauses on "use" statements; remove
!                              unnecessary comments which made the code harder
!                              to read.
! 61. 2007-06-12 Stoeckley    Remove START and END buttons from the menu; fix
!                              problem whereby the list of functions for
!                              GEN_EXPR was not showing up in SeisSpace; made
!                              several parameters invisible to keep people
!                              from trying to edit them in SeisSpace, and
!                              replaced them with corresponding info parameters
!                              for clarity; added warning not to try to insert
!                              or delete rows in the arrays; added buttons to
!                              allow the user to move to previous or next tasks
!                              for editing.
! 60. 2007-06-05 Stoeckley    Add rowgroup information to the HelpSection.
! 59. 2006-10-31 Stoeckley    Remove parameter cache calls to register buttons
!                              since the parameter cache now deals with this
!                              automatically when pc_pressed is called.
! 58. 2006-04-11 Stoeckley    Added parameter cache calls to register buttons.
!057. 2006-01-10  B. Menger   Removed Unused Variables.
! 56. 2005-10-10 Goodger      Change argument list in routine parse from intent
!                             out to intent inout to satisfy absoft 9.0 
!                             compiler.
! 55. 2005-05-05 Bill Menger  Allow it to run parallel, disabling SEQUENCE when
!                             in parallel mode.
! 54. 2004-08-23 Stoeckley    Fix bug whereby trace sequence number was not
!                              being incremented properly when using HDR_FLAG.
! 53. 2002-09-30 Stoeckley    Make the general expression work even when a
!                             negative value is being raised to a power, so
!                             long as that power is an integer.
! 52. 2002-08-27 SMCook       Minor gui def changes only.
! 51. 2002-06-24 C C Burch    Made non-parallel as obj%seq not parallel safe
! 50. 2002-05-06 Vunderink    Added parallel control parameters
! 49. 2002-04-22  Baumel      Set OFFSET_SIGNED using header words 33 and 35
!                             (X grid locations) instead of 46 and 47 (ground
!                             positions); also fix operation of STRETCH option.
! 48. 2002-03-15  Selzler     Increase max number of tasks to 200.
! 47. 2002-02-06  Selzler     Clarified help text for UNPACK per user request.
! 46. 2002-01-25  Selzler     Correct serious bug (in 45) with GUI buttons.
! 45. 2002-01-18  Selzler     Added TRUNCATE and UNPACK functionality.
! 44. 2001-11-30  Selzler     Added handedness parameter to GRID_USER task.
!                             Added new SURV_USER task (variation on SURV_GLOB
!                             and GRID_USER)
! 43. 2001-10-11  SMCook      Minor gui_def changes only (changed /XSF to /XST
!                             for all arrays).
! 42. 2001-06-19  Selzler     Corrected problem when origin and destination
!                             header specified the same word. PRODUCTION.
! 41. 2001-05-30  Selzler     Added a few comments while bug hunting.
!                             No functional changes.
! 40. 2001-04-02  Brad Kruse  Request for scratch memory.  Added 'scratch'
!                             header numbers -50 to -1.
! 39. 2001-03-22  Brad Kruse  Bug report #335.  Corrected GEN_EXPR to
!                             evaluate the EXP parameters before the COEF's.
! 38. 2000-12-13  Brad Kruse  Bug report #141,156, and 192.  When using the 
!                             GEN_EXPR task the parameter field turns blank.
!                             Recoded SETWORD_STR_TASKS to reduce trimming
!                             character strings for catenation.
!                             Bug report #115 (Very cumbersome and confusing).
!                             Limited array and arrayset lengths to eliminate
!                             the trailing blank entry.  Adjusted prompt string
!                             length and field size on the GUI to minimize
!                             horizontal scrolling (only partially successful).
!                             This completes changes done 2000-10-31.
! 37. 2000-12-11  Brad Kruse  Change name for wrapup flag to SKIP_WRAPUP for
!                             clarity, and slightly change how it is used.
! 36. 2000-11-15 Brad Kruse   Request #177, Add SINGLE_VALUE task.
! 35. 2000-10-31 Brad Kruse   Request #141, Setword interface is confusing.
!                             Removed Up/Down buttons, rearranged current
!                             task fields, and labeled task buttons 'insert'.
! 34. 2000-10-16 Brad Kruse   Request #127 to add SURV_GLOB function; similar 
!                             to GRID_GLOB, SURV_GLOB will reset the survey 
!                             coordinates from the grid locations
! 33. 2000-10-05 Brad Kruse   Bug report #110.  STRETCH parameter 'HDR_SET' is 
!                             reset to value of parameter 'HDR_A'.  
!                             Added display only helps for gui informational 
!                             fields: TASK, PARAM, TASKSEQ, TASKCNT.
! 32. 2000-09-14 Brad Kruse   Bug report #69.  Update accepted a blank SMR value
!                             for GRID_GLOB, instead of raising a warning.
! 31. 2000-04-19 Kruse      Clean up dangling '~' on layout.
!                           Rearrange writing tstr (t) in setword_str_tasks to
!                           avoid failing on Pacific Group/Linux compiler.
! 30. 2000-03-22 Kruse      Remove layout includes 'top.lay' and 'bot.lay'
! 29. 2000-03-21 Kruse      New CPS Testing Report, 3/17.  Remove all 
!                           default header word numbers in tasks.  Default HW
!                           set to '1' for all tasks.
! 28. 2000-03-02 Kruse      Revise GUI by splitting tasks into BEFORE_TASKS,
!.                          'Current Task', and AFTER_TASKS. Provide GUI 
!.                          details for Current Task parameters.
! 27. 2000-02-21 Kruse      Correct insert and delete GUI actions
! 26. 2000-02-08 Kruse      Initialize r_stat in setword_parse_task_parms to
!                           prevent spurious error messages.
! 25. 2000-02-07 Kruse      Correct GUI processing for 'Delete Task'
! 24. 2000-02-04 Kruse      Correct frontend update bug.
! 23. 2000-01-26 Kruse      Corrected bug that disregarded PC action cards 
!.                          for first call to setword_update.
! 22. 2000-01-25 Kruse      Corrected extraneous angle-bracket characters in 
!.                          the help section.  Added GUI functions PC_INSERT and
!.                          PC_MODIFY.  
! 21. 2000-01-20 Kruse      Implemented functions GEN_EXPR, LIN_COMB, BIN_CENT,
!.                          GRID_GLOB, GRID_USER, OFFSET_RECALC, STRETCH.
! 20. 2000-01-17 Kruse      Converted to new system.  Added SETW64, AZMTH.
!.                          Removed ASSIGN to another process (ASSIGN_HDR?)
!.                          Initial functions implemented: ABS, AZMTH, COPY, 
!.                          COUNTER, MAX, MIN, OFFSET_ABS, OFFSET_SIGNED, 
!.                          REG_INC, ROUND, SWAP
! 19. 1998-11-11 Goodger    Begin using fortran90 compiler.
! 18. 1997-10-27 Vunderink  Documentation change only.
! 17. 1997-10-15 Goodger    Move variable GLAST to portion of common   
!.                          block which will get replaced by REPPI.
! 16. 1997-08-25 Vunderink  Add new GROUP option. 
! 15. 1994-06-07 Troutt     Add new option: XYBIN=YES,NO.
! 14. 1994-02-08 Troutt     Documentation change only.  TWOHEAD option 
!.                          screen order changed to match new CFE.  The
!.                          front-end has been changed to handle the 6
!.                          values for TWOHEAD in alpha form in order to
!.                          allow more significant digits to be input
!.                          than EZ-ED can handle for real values.
! 13. 1993-10-28 Cooper     OFFSET=RE-CALC to re-calculate offset based
!.                          on difference of x and y source and receiver.
! 12. 1992-01-13 Stoeckley  Change keyword OFFSET to use headers 46 and
!.                          47 (rather than 11 and 14).
! 11. 1991-04-23 Troutt     Put GETP ahead of N=0 printout so that correct
!.                          KOUNT gets reported.
! 10. 1990-04-25 Stoeckley  Fix bug in GRID option.
! 9.  1989-08-04 Stoeckley  Add keyword NEAR, expand keyword SET, and
!.                          enforce contents of header word 1.
! 8.  1989-07-25 Stoeckley  Add keywords OFFSET, SELHDR, SETHDR, BINSIZ,
!.                          PRESET, BINCEN, VALNEW.
! 7.  1989-06-20 Stoeckley  Add keywords ABS and TWOHEAD.
! 6.  1989-06-13 Stoeckley  Fix bug in OLD and AGO that caused HB# to
!.                          always be the same as HA#.
! 5.  1989-05-15 Stoeckley  Change use of FLAGWORD.
! 4.  1989-01-27 Stoeckley  Fix FLAGWORD documentation.
! 3.  1988-11-30 Stoeckley  Delete TRANS,ALLTRANS; add GRID,FLAGWORD.
! 2.  1988-10-29 Ball       NWIH and NWPT Conversion        
! 1.  1988-09-14 Stoeckley  Original Version
!
!
!                      REVISION HISTORY FOR SETW64               
!
!     Date       Author    Description
!     ----       ------    -----------
! 3.  1998-11-03 Rozell    CFT to FIX conversion.
! 2.  1998-02-26 Hanson    Replace * with X for multiplication becaus of DCODE
! 1.  1997-07-02 Hanson    Original version
!
!
!                       REVISION HISTORY FOR AZMTH 
!
! 2.  1998-12-15 Vunderink Begin using the f90 compiler.
! 1.  1993-09-30 Burch     Original Version
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
! This process uses a single set of trace and header arrays.
!
!                       o
!  call setword_create (obj)
!
!                       b
!  call setword_delete (obj)
!
!                           b
!  call setword_initialize (obj)
!
!                       b
!  call setword_update (obj)
!
!                b    i    b   i
!  call setword (obj, ntr, hd, tr)
!
!                       b
!  call setword_wrapup (obj)
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.

! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
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
! 1. The strings input from the parameter cache are interpreted, the list
!    of interpreted task sub-structures is updated, then a new set of 
!    strings is created to write back to the parameter cache.  This applies
!    to BEFORE_TASKS, the Current Task (VALUE), and AFTER_TASKS.
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
!                               LAYOUT
!<gui_def>
!<NS SETWORD Process/NC=86/NR=28>
!            SET Header WORDs Process (includes former AZMTH and SETW64).
!  PARALLEL_MODE = `CC
!                BEFORE_TASKS[/XST/YST]
!  HDR_FLAG      `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!    [HF]`III    `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!                                        `--------------------------------------
!  [TN]`AAAAAAAAAAAA   Current Task[/L]       Insert (After Current Task)
!   Task `I of `I      Function `CCCC     GRID_USER`P     AZMTH`P COUNTER`P
!                                         GRID_GLOB`P     ABS`P   REG_INC`P
!  Prompts   Values                       SURV_USER`P     COPY`P  SINGLE_VALUE`P
!  `XXXXXXXXX`AAAAAAAAAAAAAAAAAAAAAA---   SURV_GLOB`P     MIN`P   LIN_COMB`P
!  `XXXXXXXXX`AAAAAAAAAAAAAAAAAAAAAA---   OFFSET_RECALC`P MAX`P   GEN_EXPR`P
!  `XXXXXXXXX`AAAAAAAAAAAAAAAAAAAAAA---   OFFSET_SIGNED`P ROUND`P STRETCH`P
!  `XXXXXXXXX`AAAAAAAAAAAAAAAAAAAAAA---   OFFSET_ABS`P    SWAP`P  BIN_CENT`P
!  `XXXXXXXXX`AAAAAAAAAAAAAAAAAAAAAA---   TRUNCATE`P              UNPACK`P
!  `XXXXXXXXX`AAAAAAAAAAAAAAAAAAAAAA---                                         
!  `XXXXXXXXX`AAAAAAAAAAAAAAAAAAAAAA---  `--------------------------------------
!
!                AFTER_TASKS[/XST/YST]
! DELETE~~[/2]`P `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----
! CURRENT TASK   `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----
!                `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----
!                `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----
!                `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----
!<PARMS HF[HDR_FLAG]>
!<PARMS Task[TASKSEQ/EN/C]>
!<PARMS of[TASKCNT/EN/C]>
!<PARMS TN[TASK/L/EN]>
!<PARMS Values[VALUE]>
!<PARMS Prompts[PARAM/EN/R]>
!<PARMS PROMPTS_ARRAYSET[/XST/YST]>
!<PARMS DELETECURRENTTASK[DELETE/2/C]>
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="PARALLEL_MODE">
!<Tip> If YES, runs parallel but doesn't resequence traces on input.</Tip>
! Default = "YES"
! Allowed = "YES or NO"
!
! If Yes then will run parallel but will not resequence traces on input.
!
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! 
! If HDR_FLAG = 0 then all traces are processed.  Otherwise only traces with 
! a flag set in header word HDR_FLAG are processed. 
!
!</Help>
!
!<Help KEYWORD="BEFORE_TASKS">
!<Tip>List of tasks preceding the Current Task.</Tip>
! 
! This is a sequenced list of tasks that take place before the Current Task.
! Entries cannot be modified directly.  Click on the desired entry to 
! position that entry as the Current Task to modify.
!
! Defined tasks are specified by the list of pushbuttons.
!
! In addition, the START task will either be the first task, or may be the
! Current Task if BEFORE_TASKS is empty.  The END task is always the last
! entry in AFTER_TASKS.
! 
! Up to 200 of the supported tasks can be listed, in any order.  The limit
! includes the Current Task, and tasks listed in BEFORE_TASKS and AFTER_TASKS.
!
! Parameters for each task are listed with the task name.
! The format is one set of parentheses surrounding the 
! sequence of 'keyword=value' pairs, with a 'comma-space' (", ") to separate 
! keyword/value pairs.
!</Help>
!
!<Help KEYWORD="PREVIOUS TASK">
!<Tip>Make the previous task the Current Task.</Tip>
! Prompt = select previous task
!</Help>
!
!<Help KEYWORD="NEXT TASK">
!<Tip>Make the next task the Current Task.</Tip>
! Prompt = select next task
!</Help>
!
!<Help KEYWORD="DELETE">
!<Tip>Delete the Current Task.</Tip>
! Prompt = delete current task
!
! Replace the Current Task with the top task from the AFTER_TASKS list,
! and remove the copied task from the top of the AFTER_TASKS list.
!
! The bottom task of the BEFORE_TASKS list is moved to the Current Task
! if the top of the AFTER_TASKS is the 'END' task.
!
! The DELETE key is ignored if the Current Task is the 'START' task.
!
!</Help>

!<Help KEYWORD="AFTER_TASKS">
!<Tip>List of tasks following the Current Task.</Tip>
! 
! This is a sequenced list of tasks that take place after the Current Task.
! Entries cannot be modified directly.  Click on a desired task to 
! position that entry as the Current Task to modify.
!
! Defined tasks are specified by the list of pushbuttons.
!
! See BEFORE_TASKS for more explanation.  The END task cannot be modified or
! deleted.  It will always be the last task in the list.
!
!</Help>
!
!<Help KEYWORD="FUNCTION">
!<Tip>Function selection to be applied as part of the GEN_EXPR task.</Tip>
! Prompt = current task function
!
! Default = IDENT (identity function)
!
! This selection is only used when the Current Task is GEN_EXPR.
!
!</Help>
!
!<Help KEYWORD="VALUE">
!<Tip>Parameter values for the selected Current Task</Tip>
!
! Values of parameters for the Current Function are given in order, 
! and may be real, header word numbers, or strings (for operator selection)
! such as '+', '-', '/', '*'.
!
! Header numbers are limited to 1 to number of words in header (NWIH), 
! and -50 to -1 for internal scratch header words.
!</Help>
!
!<Help KEYWORD="ABS">
!<Tip> Write the absolute value of header word HDR_FROM into hwd# HDR_TO</Tip>
!
! Take the absolute value of header word HDR_FROM and place it in HDR_TO. HDR_TO
! may be equal to HDR_FROM.
!
! Action: ABS (HDR_TO = 48, HDR_FROM = 49)
!
! Allowed: HDR_TO, HDR_FROM = 1 -  NWIH
!</Help>
!
!<Help KEYWORD="AZMTH">
!<Tip> Calculate source to receiver azimuth and place in a header word. </Tip>
!
! Calculate source to receiver azimuth and place in a header word.
! The angle, 0-360 degrees, clockwise from north to the direction from source 
! location to receiver location of a 3D trace is placed in header word HDR_SET.
! A constant angle, ANG_ADD, may be added to the calculated angle to change 
! the reference direction.
!
! Action: AZMTH (HDR_SET = 48, ANG_ADD = 0.0)
!
! Allowed: HDR_SET = 1 - NWIH
! Allowed: ANG_ADD = -360.0 - +360.0 (real) degrees
!</Help>
!
!<Help KEYWORD="BIN_CENT">
!<Tip> Reset midpoint surveyed coordinates to center of stack bins. </Tip>
!
! Reset midpoint surveyed coordinates (header words 17 & 18) to center of stack
! bins.  (Typically used in 3D surveys post-stack.)
!
! Action: BIN_CENT ()
!</Help>
! 
!<Help KEYWORD="COPY">
!<Tip> Copy the value in HDR_FROM to hwd# HDR_TO.</Tip>
!
! Copy the value in HDR_FROM and place it in HDR_TO.
!
! Action: COPY (HDR_TO = 48, HDR_FROM = 49)
!
! Allowed: HDR_TO, HDR_FROM = 1 - NWIH
!</Help>
!
!<Help KEYWORD="COUNTER">
!<Tip> Set a header word by a counter. </Tip>
!
! Set a header word by a counter.
! Set header word HDR_SET to the value VAL_INIT until the value of HDR_A 
! changes, then increment the value of HDR_SET by VAL_INC for each subsequent 
! group of traces having the same value for HDR_A.
!
! Action: COUNTER (HDR_SET = 48, VAL_INIT = 0.0, HDR_A = 49, VAL_INC = 0.0)
!
! Allowed: HDR_SET, HDR_A = 1 - NWIH
! Allowed: VAL_INIT, VAL_INC = (real)
!</Help>
!
!<Help KEYWORD="GEN_EXPR">
!<Tip>Hwd# HDR_SET is set to a general arithmetic expression</Tip>
!
! Header word HDR_SET is reset to the value of the general arithmetic 
! expression:
!
! VALUE(HDR_SET) = FUNCT{
!                     COEF_A * (VALUE(HDR_A) ** EXP_A)
!                  OPER 
!                     COEF_B * (VALUE(HDR_B) ** EXP_B) + CONST
!                       }.
!
! Where FUNCT can be any one of (ABS, MIN, MAX, INT, NINT, SIGN, SQRT, SIN, COS,
! TAN, ASIN, ACOS, ATAN, IDENT, D2R, R2D).  MIN/MAX is the minimum/maximum 
! of the pre-existing value of HDR_SET and the calculated value of FUNCT.  
! The argument of the trigonometric functions is in radians.  D2R converts the
! calculated value from Degrees to Radians; R2D back-converts from Radians to
! Degrees.  IDENT is an identity, or no-operation function; the calculated 
! value is set to HDR_SET.
!
! OPER can be any one of (+, -, *, /)
!
! Action: GEN_EXPR (FUNCT  = ABS, HDR_SET = 49, 
!                   COEF_A = 1.0, HDR_A   = 48, EXP_A = 1.0, OPER = '+', 
!                   COEF_B = 1.0, HDR_B   = 49, EXP_B = 1.0, CONST = 0.0)
!
! Allowed: FUNCT = one of (ABS, MIN, MAX, INT, NINT, SIGN, SQRT, SIN, COS,
!                          TAN, ASIN, ACOS, ATAN)
! Allowed: HDR_SET, HDR_A, HDR_B = 1 - NWIH
! Allowed: COEF_A, COEF_B, CONST = (real)
! Allowed: EXP_A, EXP_B = -300.0 - +300.0 (real)
! Allowed: OPER = + - / * (character)
!</Help>
!
!<Help KEYWORD="GRID_GLOB">
!<Tip>Recalc Source, Midpoint or Receiver grid coords from grid global </Tip>
!
! Recalculate one or more of Source, Midpoint or Receiver grid coordinates from
! the grid global, depending on whether SMR includes 's', 'm', and/or 'r'.
!
! TASK: GRID_GLOB (SMR = "")
!
! Allowed: SMR = String containing 's' (for source), 'm' (for midpoint) 
!                and / or 'r' (for receiver).  Specifies which grid 
!                coordinates to recalculate
!</Help>
!
!<Help KEYWORD="SURV_GLOB">
!<Tip>Recalc Source, Midpoint or Receiver survey locs from grid global </Tip>
!
! Recalculate one or more of Source, Midpoint or Receiver survey locations 
! from the grid global and coordinates, depending on whether SMR includes 
! 's', 'm', and/or 'r'.
!
! TASK: SURV_GLOB (SMR = "")
!
! Allowed: SMR = String containing 's' (for source), 'm' (for midpoint) 
!                and / or 'r' (for receiver).  Specifies which survey 
!                locations to recalculate
!</Help>
!
!<Help KEYWORD="GRID_USER">
!<Tip> Recalculate SMR grid coordinates from user parameters.</Tip>
!
! Recalculate certain grid coordinates from user parameters.
! GRID_USER is identical to GRID_GLOB except that the calculations are based 
! on user-supplied grid transform parameters, not the grid
! global.  XORG and YORG are the grid origins in the surveyed coordinate 
! system, angle is the angle in degrees measured from the easting direction 
! (East) counterclockwise to the positive inline coordinate direction and 
! BIN_WID_INL and BIN_WID_CRL are the stack bin widths in the inline and 
! crossline directions and LR_HANDED is Left of Right handed.
!
! Action: GRID_USER (SMR = "", XORG = 0.0, YORG = 0.0, ANGLE = 0.0, 
!                    BIN_WID_INL = 1.0, BIN_WID_CRL = 1.0, LR_HANDED = "R")
!
! Allowed: SMR = String containing 's' (for source), 'm' (for midpoint) 
!        and / or 'r' (for receiver).  Specifies which grid coordinates to 
!        recalculate
! Allowed: XORG, YORG, BIN_WID_INL, BIN_WID_CRL = (real)
! Allowed: ANGLE = -360.0 - 360.0 -- Angle in degrees of easting.
! Allowed: LR_HANDED = String containing 'L' (Left handed) or 'R' (Right
!        handed) coordinate tranform.
!</Help>
!
!<Help KEYWORD="SURV_USER">
!<Tip>Recalc Source, Midpoint or Receiver survey locs from user parms </Tip>
!
! Recalculate one or more of Source, Midpoint or Receiver survey locations 
! from the user parms and coordinates, depending on whether SMR includes 
! 's', 'm', and/or 'r'.
!
! SURV_USER is identical to SURV_GLOB except that the calculations are based 
! on user-supplied grid transform parameters, not the grid global.
! XORG and YORG are the grid origins in the surveyed coordinate 
! system, angle is the angle in degrees measured from the easting direction 
! (East) counterclockwise to the positive inline coordinate direction and 
! BIN_WID_INL and BIN_WID_CRL are the stack bin widths in the inline and 
! crossline directions and LR_HANDED is Left of Right handed.
!
! Action: SURV_USER (SMR = "", XORG = 0.0, YORG = 0.0, ANGLE = 0.0, 
!                    BIN_WID_INL = 1.0, BIN_WID_CRL = 1.0, LR_HANDED = "R")
!
! Allowed: SMR = String containing 's' (for source), 'm' (for midpoint) 
!        and / or 'r' (for receiver).  Specifies which grid coordinates to 
!        recalculate
! Allowed: XORG, YORG, BIN_WID_INL, BIN_WID_CRL = (real)
! Allowed: ANGLE = -360.0 - 360.0 -- Angle in degrees of easting.
! Allowed: LR_HANDED = String containing 'L' (Left handed) or 'R' (Right
!        handed) coordinate tranform.
!</Help>
!
!<Help KEYWORD="LIN_COMB">
!<Tip>Set hwd# HDR_SET as a linear combination of HDR_A and HDR_B</Tip>
!
! Set header word HDR_SET as a linear combination of header words HDR_A and 
! HDR_B as follows:
!
!      VALUE(HDR_SET) = VALUE(HDR_A)*COEF_A + VALUE(HDR_B)*COEF_B + CONST. 
!
! Action: LIN_COMB (HDR_SET = 48, HDR_A = 49, COEF_A = 1.0, HDR_B = 50, 
!                   COEF_B = 1.0, CONST = 0.0) 
!
! Allowed: HDR_SET, HDR_A, HDR_B = 1 - NWIH
! Allowed: COEF_A, COEF_B = (real)
!</Help>
!
!<Help KEYWORD="MAX">
!<Tip> Take the MAX of two header words. </Tip>
! 
! Compare the values of header word HDR_A and HDR_B and place the 
! algebraically larger value in HDR_TO.  This option allows a comparison 
! within a single trace only.
!
! Action: MAX (HDR_TO = 48, HDR_A = 49, HDR_B = 50)
!
! Allowed: HDR_TO, HDR_A, HDR_B = 1 - NWIH
!</Help>
!
!<Help KEYWORD="MIN">
!<Tip> Take the MIN of two header words.</Tip>
!
! Compare the values of header word HDR_A and HDR_B and place the 
! algebraically smaller value in HDR_TO.  This option allows a comparison 
! within a single trace only.
!
! Action: MIN (HDR_TO = 48, HDR_A = 49, HDR_B = 50)
!
! Allowed: HDR_TO, HDR_A, HDR_B = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OFFSET_RECALC">
!<Tip>Recalculate offset from source and receiver surveyed coordinates.</Tip>
! 
! Recalculate offset from source and receiver surveyed coordinates.
!  VALUE(HDR_OFFSET) 
!    = sqrt ( ( value(HDR_RECEIVER_XLOC) - value(HDR_SOURCE_XLOC) ) ** 2
!             + ( value(HDR_RECEIVER_YLOC) - value(HDR_SOURCE_YLOC) ) ** 2 )
!
! Action: OFFSET_RECALC ()
!</Help>
!
!<Help KEYWORD="OFFSET_ABS">
!<Tip>Convert from signed to unsigned offset.</Tip>
! 
! Convert from signed to unsigned offset.
!  value(HDR_OFFSET) = ABS (value(HDR_OFFSET))
!
! Action: OFFSET_ABS ()
!</Help>
!
!<Help KEYWORD="OFFSET_SIGNED">
!<Tip> Convert from unsigned to signed OFFSET (+ or -).</Tip>
! 
! Convert from unsigned to signed offset (+ or -).
! Signed offset (header word 6) is negative if the receiver X grid
! position (header word 35) is smaller than the source X grid position
! (header word 33) (pulling the cable) and positive otherwise (pushing
! the cable).
!  SIGN (Value(HDR_OFFSET)) = SIGN (Value(HDR_RECEIVER_XGRID) 
!                                   - Value(HDR_SOURCE_XGRID))
!
! Action: OFFSET_SIGNED ()
!</Help>
!
!<Help KEYWORD="REG_INC">
!<Tip> Reset header word value by a regular increment scheme.</Tip>
!
! Set header word HDR_SET to the value VAL_INIT for the first NUM_TR traces, 
! then increment the value by VAL_INC for each subsequent group of NUM_TR 
! traces.  The traces do not have to be gathered into any particular groups.  
! If HDR_OPT is specified (optional) and non-zero, then header word HDR_OPT is
! set to the trace number within the group.
!  VALUE (HDR_SET) = VAL_INIT + n * VAL_INC
!  if (HDR_OPT > 0) VALUE (HDR_OPT) = n
!  n = n + 1
!  if (n >= NUM_TR) n = 0
!
! Action: REG_INC (HDR_SET = 48, VAL_INIT = 0.0, NUM_TR = 1, VAL_INC = 0.0, 
!                  HDR_OPT = 0) 
!
! Allowed: HDR_SET = 1 - NWIH
! Allowed: HDR_OPT = 0 - NWIH (optional)
! Allowed: VAL_INIT, VAL_INC = (real)
! Allowed: NUM_TR  = integer >= 1
!</Help>
!
!<Help KEYWORD="ROUND">
!<Tip> Round the value of a header word to the nearest whole number.</Tip>
! 
! Round the value in header word HDR_FROM to the nearest whole number and 
! place it in HDR_TO. HDR_TO may be equal to HDR_FROM.
!  value (HDR_TO) = ANINT (value (HDR_FROM))
!
! Action: ROUND (HDR_TO = 48, HDR_FROM = 49)
!
! Allowed: HDR_TO, HDR_FROM = 1 - NWIH
!</Help>
!
!<Help KEYWORD="SINGLE_VALUE">
!<Tip> Set a header to a value.</Tip>
! 
! place the value in HDR_TO.
!  value (HDR_TO) = value
!
! Action: SINGLE_VALUE (HDR_TO = 48, CONST = 5)
!
! Allowed: HDR_TO = 1 - NWIH
!</Help>
!
!<Help KEYWORD="STRETCH">
!<Tip> Reset header word value by a linear stretch scheme.</Tip>
!
! Calculate header word HDR_SET from a linear function of header word HDR_A, 
! such that values VAL_A1 and VAL_A2 (in header word HDR_A) will be converted 
! to values BV1 and BV2 respectively, and placed into header word HDR_SET.
! If the last value (HDR_SET) is zero or omitted, the result is put back into 
! the same header word (HDR_A).
!        Slope        =  (BV2 - BV1) / (VAL_A2 - VAL_A1)
!    Value(HDR_SET)   =   BV1  +  (Value(HDR_A) - VAL_A1) * Slope
!
! Action: STRETCH (HDR_A = 48, VAL_A1 = 0.0, VAL_A2 = 1.0,
!                  BV1 = 0.0, BV2 = 1.0, HDR_SET = 0)
!
! Allowed: HDR_A = 1 - NWIH
! Allowed: HDR_SET = 0 - NWIH (optional)
! Allowed: VAL_A1, VAL_A2, BV1, BV2 = (real); VAL_A2 /= VAL_A1
!</Help>
!
!<Help KEYWORD="SWAP">
!<Tip> Exchange the values of two header words.</Tip>
!
! Take the value in header word HDR_FROM and place it in HDR_TO.  Take the 
! value originally in header word HDR_TO and place it in HDR_FROM.
!
! Action: SWAP (HDR_TO = 48, HDR_FROM = 49)
!
! Allowed: HDR_TO, HDR_FROM = 1 - NWIH
!</Help>
!
!<Help KEYWORD="UNPACK">
!<Tip> Unpack digits from base 10 integer representation of value.</Tip>
!
! Unpack digits from base 10 integer representation of value.
! HDR_FROM and HDR_TO are source and destination header words respectively.
! NUM_SHIFT is the number of digits to shift to the right (i.e. number of
!   digits to ignore or a power of 10 scale factor).
! NUM_KEEP is the maximum number of significant digits to keep.
!
! Example: assume HDR-X has the value 123456789 and needs to be split
! so HDR-Y equals 123 and HDR-Z equals 6789 (the "45" is not accessed).
! UNPACK must be run twice, with the following parameters.
!
!   HDR_TO     HDR-Y
!   HDR_FROM   HDR-X
!   NUM_SHIFT  6
!   NUM_KEEP   3
!
!   and
!
!   HDR_TO     HDR-Z
!   HDR_FROM   HDR-X
!   NUM_SHIFT  0
!   NUM_KEEP   4
!
!   Note: the original HDR-X value could have been created with the formula
!     HDR-X = (123 * 1000000) + (45 * 10000) + (6789 * 1)
!
! Note: only positive integers should be encoded and decoded.
! The maximum positive value for a segy 32 bit integer is 2,147,483,647
! and only 32,767 for a 16 bit integer.
!
! Action: UNPACK (HDR_TO = 48, HDR_FROM = 49, NUM_SHIFT = 0, NUM_KEEP = 3)
!
! Allowed: HDR_TO, HDR_FROM = 1 - NWIH.
! Allowed: 0 <= NUM_SHIFT <= 15 and 1 <= NUM_KEEP <= 16 - NUM_SHIFT.
!</Help>
!
!<Help KEYWORD="TRUNCATE">
!<Tip> Write the truncated value of header word HDR_FROM into hwd# HDR_TO</Tip>
!
! Truncate the fractional portion of header word HDR_FROM and
! place it in HDR_TO. HDR_TO may be equal to HDR_FROM.
! For example, if the FROM value is 3.14 then the TO value is 3.
!
! Action: TRUNCATE (HDR_TO = 48, HDR_FROM = 49)
!
! Allowed: HDR_TO, HDR_FROM = 1 -  NWIH
!</Help>
!
!<Help KEYWORD="current task name" TYPE= "DISPLAY_ONLY">
!<Tip> Name of the current task.</Tip>
!</Help>
!
!<Help KEYWORD="TASK" TYPE= "DISPLAY_ONLY">
!<Tip> Name of the current task.</Tip>
!</Help>
!
!<Help KEYWORD="PARAM" TYPE= "DISPLAY_ONLY">
!<Tip> Labels of the values in the current task.</Tip>
!</Help>
!
!<Help KEYWORD="current task number" TYPE= "DISPLAY_ONLY">
!<Tip> Sequence number of the current task in the task list.</Tip>
!</Help>
!
!<Help KEYWORD="TASKSEQ" TYPE= "DISPLAY_ONLY">
!<Tip> Sequence number of the current task in the task list.</Tip>
!</Help>
!
!<Help KEYWORD="total number of tasks" TYPE= "DISPLAY_ONLY">
!<Tip> Count of tasks in sequence.</Tip>
!</Help>
!
!<Help KEYWORD="TASKCNT" TYPE= "DISPLAY_ONLY">
!<Tip> Count of tasks in sequence.</Tip>
!</Help>
!
!<Help KEYWORD="WARNING" TYPE= "DISPLAY_ONLY">
!<Tip> Do not try to insert or delete any rows in any of the arrays.</Tip>
! If you try to insert or delete any rows in any of the arrays in this
! tool in the SeisSpace Navigator, you will get malfunctions of this
! tool, including several error messages, and you might crash the
! Navigator.  If this occurs, the only correction is to delete this
! tool, re-insert it, and reset the parameters.  If you have an older
! version of the flow, you can open that older version and go from
! there.  If you crash the Navigator, you will not be able to re-start
! it if your preferences are set to re-open any previously-opened flows
! upon startup.  To change your preferences, follow the instructions
! below.  Then you will be able to re-start the Navigator, but after you
! do so, be sure not to re-open the flow which caused the crash.  Instead,
! open an earlier version of the flow.
!
! To change your SeisSpace preferences so that the Navigator will not
! open any flows upon startup:
!
! Edit the following file in your home directory:
!         ~/SeisSpace/.seisspace
! Change this line:
!   <par name="Navigator.opensFlowOnStartup" type="string"> true </par>
! to look like this:
!   <par name="Navigator.opensFlowOnStartup" type="string"> false </par>
!</Help>
!
! rowgroup =  ABS            AZMTH          BIN_CENT       COPY
! rowgroup =  COUNTER        GEN_EXPR       GRID_GLOB      GRID_USER
! rowgroup =  LIN_COMB       MAX            MIN            OFFSET_ABS
! rowgroup =  OFFSET_RECALC  OFFSET_SIGNED  REG_INC        ROUND
! rowgroup =  SINGLE_VALUE   STRETCH        SURV_GLOB      SURV_USER
! rowgroup =  SWAP           TRUNCATE       UNPACK
!
! rowgroup =  PREVIOUS^TASK  NEXT^TASK  DELETE
!
!</HelpSection>
!
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module setword_module
  use pc_module
  use named_constants_module
  use grid_module
  use string_module
  use mth_module
  implicit none
  private

  public :: setword_create
  public :: setword_initialize
  public :: setword_update
  public :: setword_delete
  public :: setword
  public :: setword_wrapup

  character(len=100), public, save :: SETWORD_IDENT = &
'$Id: setword.f90,v 1.63 2007/11/07 14:51:10 Stoeckley beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

  integer, parameter :: max_tasks    =   200 + 2
  integer, parameter :: max_line_len =  1020   ! Lines encoded with task & vals
  integer, parameter :: max_vals     =   10    ! Dimension of prompt_strs and
                                               ! value_strs
  integer, parameter :: max_pline_len =  60    ! Partial, continued lines.
                                               ! Character length of
                                               ! before_tasks and after_tasks
                                               ! array elements.
  integer, parameter :: max_plines   = max_tasks * max_line_len / max_pline_len
                                               ! Dimension of before_tasks and
                                               ! after_tasks character arrays.
                                               ! Dimension of before_task_map
                                               ! and after_task_map int arrays.

  ! Symbolic name of tasks that user's may request.
  character (len = *), parameter :: T_ABS           = 'ABS          '
  character (len = *), parameter :: T_AZMTH         = 'AZMTH        '
  character (len = *), parameter :: T_BIN_CENT      = 'BIN_CENT     '
  character (len = *), parameter :: T_COPY          = 'COPY         '
  character (len = *), parameter :: T_COUNTER       = 'COUNTER      '
  character (len = *), parameter :: T_END           = 'END          '
  character (len = *), parameter :: T_GEN_EXPR      = 'GEN_EXPR     '
  character (len = *), parameter :: T_GRID_GLOB     = 'GRID_GLOB    '
  character (len = *), parameter :: T_GRID_USER     = 'GRID_USER    '
  character (len = *), parameter :: T_LIN_COMB      = 'LIN_COMB     '
  character (len = *), parameter :: T_MAX           = 'MAX          '
  character (len = *), parameter :: T_MIN           = 'MIN          '
  character (len = *), parameter :: T_OFFSET_ABS    = 'OFFSET_ABS   '
  character (len = *), parameter :: T_OFFSET_RECALC = 'OFFSET_RECALC'
  character (len = *), parameter :: T_OFFSET_SIGNED = 'OFFSET_SIGNED'
  character (len = *), parameter :: T_REG_INC       = 'REG_INC      '
  character (len = *), parameter :: T_ROUND         = 'ROUND        '
  character (len = *), parameter :: T_SINGLE_VALUE  = 'SINGLE_VALUE '
  character (len = *), parameter :: T_START         = 'START        '
  character (len = *), parameter :: T_STRETCH       = 'STRETCH      '
  character (len = *), parameter :: T_SURV_GLOB     = 'SURV_GLOB    '
  character (len = *), parameter :: T_SURV_USER     = 'SURV_USER    '
  character (len = *), parameter :: T_SWAP          = 'SWAP         '
  character (len = *), parameter :: T_TRUNCATE      = 'TRUNCATE     '
  character (len = *), parameter :: T_UNPACK        = 'UNPACK       '

  ! Index associated with symbolic task names.
  integer, parameter             :: IT_ABS           = 1
  integer, parameter             :: IT_AZMTH         = IT_ABS + 1
  integer, parameter             :: IT_BIN_CENT      = IT_AZMTH + 1
  integer, parameter             :: IT_COPY          = IT_BIN_CENT + 1
  integer, parameter             :: IT_COUNTER       = IT_COPY + 1
  integer, parameter             :: IT_END           = IT_COUNTER + 1
  integer, parameter             :: IT_GEN_EXPR      = IT_END + 1
  integer, parameter             :: IT_GRID_GLOB     = IT_GEN_EXPR + 1
  integer, parameter             :: IT_GRID_USER     = IT_GRID_GLOB + 1
  integer, parameter             :: IT_LIN_COMB      = IT_GRID_USER + 1
  integer, parameter             :: IT_MAX           = IT_LIN_COMB + 1
  integer, parameter             :: IT_MIN           = IT_MAX + 1
  integer, parameter             :: IT_OFFSET_ABS    = IT_MIN + 1
  integer, parameter             :: IT_OFFSET_RECALC = IT_OFFSET_ABS + 1
  integer, parameter             :: IT_OFFSET_SIGNED = IT_OFFSET_RECALC + 1
  integer, parameter             :: IT_REG_INC       = IT_OFFSET_SIGNED + 1
  integer, parameter             :: IT_ROUND         = IT_REG_INC + 1
  integer, parameter             :: IT_SINGLE_VALUE  = IT_ROUND + 1
  integer, parameter             :: IT_START         = IT_SINGLE_VALUE + 1
  integer, parameter             :: IT_STRETCH       = IT_START + 1
  integer, parameter             :: IT_SURV_GLOB     = IT_STRETCH + 1
  integer, parameter             :: IT_SURV_USER     = IT_SURV_GLOB + 1
  integer, parameter             :: IT_SWAP          = IT_SURV_USER + 1
  integer, parameter             :: IT_TRUNCATE      = IT_SWAP + 1
  integer, parameter             :: IT_UNPACK        = IT_TRUNCATE + 1
  integer, parameter             :: IT_LAST          = IT_UNPACK

  ! Array of task names, subscripted by task index.
  character (len = *), dimension (IT_LAST), parameter :: TASK_NAMES    &
    = (/ T_ABS, T_AZMTH, T_BIN_CENT, T_COPY, T_COUNTER, T_END,         &
         T_GEN_EXPR, T_GRID_GLOB, T_GRID_USER, T_LIN_COMB, T_MAX,      &
         T_MIN, T_OFFSET_ABS, T_OFFSET_RECALC, T_OFFSET_SIGNED,        &
         T_REG_INC, T_ROUND, T_SINGLE_VALUE, T_START, T_STRETCH,       &
         T_SURV_GLOB, T_SURV_USER, T_SWAP, T_TRUNCATE, T_UNPACK /)

  ! Number of arguments required for each task, subscripted by task index.
  integer, dimension (IT_LAST), parameter    &
    :: IT_ITEM_CNT = (/ 2,                   &   ! ABS
                        2,                   &   ! AZMTH
                        0,                   &   ! BIN_CENT
                        2,                   &   ! COPY
                        4,                   &   ! COUNTER
                        0,                   &   ! END
                       10,                   &   ! GEN_EXPR
                        1,                   &   ! GRID_GLOB
                        7,                   &   ! GRID_USER
                        6,                   &   ! LIN_COMB
                        3,                   &   ! MAX
                        3,                   &   ! MIN
                        0,                   &   ! OFFSET_ABS
                        0,                   &   ! OFFSET_RECALC
                        0,                   &   ! OFFSET_SIGNED
                        5,                   &   ! REG_INC
                        2,                   &   ! ROUND
                        2,                   &   ! SINGLE_VALUE
                        0,                   &   ! START
                        6,                   &   ! STRETCH
                        1,                   &   ! SURV_GLOB
                        7,                   &   ! SURV_USER
                        2,                   &   ! SWAP
                        2,                   &   ! TRUNCATE
                        4 /)                     ! UNPACK

  ! Symbolic name of General Expression Functions that user's may request.
  character (len = *), parameter :: GE_F_ABS   = 'ABS  '
  character (len = *), parameter :: GE_F_ACOS  = 'ACOS '
  character (len = *), parameter :: GE_F_ASIN  = 'ASIN '
  character (len = *), parameter :: GE_F_ATAN  = 'ATAN '
  character (len = *), parameter :: GE_F_COS   = 'COS  '
  character (len = *), parameter :: GE_F_D2R   = 'D2R  '
  character (len = *), parameter :: GE_F_IDENT = 'IDENT'
  character (len = *), parameter :: GE_F_INT   = 'INT  '
  character (len = *), parameter :: GE_F_MAX   = 'MAX  '
  character (len = *), parameter :: GE_F_MIN   = 'MIN  '
  character (len = *), parameter :: GE_F_NINT  = 'NINT '
  character (len = *), parameter :: GE_F_R2D   = 'R2D  '
  character (len = *), parameter :: GE_F_SIGN  = 'SIGN '
  character (len = *), parameter :: GE_F_SIN   = 'SIN  '
  character (len = *), parameter :: GE_F_SQRT  = 'SQRT '
  character (len = *), parameter :: GE_F_TAN   = 'TAN  '

  ! Index associated with symbolic General Expression Functions.
  integer, parameter             :: GE_IF_ABS   = 1
  integer, parameter             :: GE_IF_ACOS  = GE_IF_ABS   + 1
  integer, parameter             :: GE_IF_ASIN  = GE_IF_ACOS  + 1
  integer, parameter             :: GE_IF_ATAN  = GE_IF_ASIN  + 1
  integer, parameter             :: GE_IF_COS   = GE_IF_ATAN  + 1
  integer, parameter             :: GE_IF_D2R   = GE_IF_COS   + 1
  integer, parameter             :: GE_IF_IDENT = GE_IF_D2R   + 1
  integer, parameter             :: GE_IF_INT   = GE_IF_IDENT + 1
  integer, parameter             :: GE_IF_MAX   = GE_IF_INT   + 1
  integer, parameter             :: GE_IF_MIN   = GE_IF_MAX   + 1
  integer, parameter             :: GE_IF_NINT  = GE_IF_MIN   + 1
  integer, parameter             :: GE_IF_R2D   = GE_IF_NINT  + 1
  integer, parameter             :: GE_IF_SIGN  = GE_IF_R2D   + 1
  integer, parameter             :: GE_IF_SIN   = GE_IF_SIGN  + 1
  integer, parameter             :: GE_IF_SQRT  = GE_IF_SIN   + 1
  integer, parameter             :: GE_IF_TAN   = GE_IF_SQRT  + 1
  integer, parameter             :: GE_IF_LAST  = GE_IF_TAN

  ! Array of General Expressions Functions, subscripted by G.E.F. index.
! character (len = *), dimension (0:GE_IF_LAST), parameter :: GE_FUNC_NAMES   &
!   = (/ GE_F_IDENT, GE_F_ABS,   GE_F_ACOS, GE_F_ASIN, GE_F_ATAN, GE_F_COS,   &
!        GE_F_D2R,   GE_F_IDENT, GE_F_INT,  GE_F_MAX,  GE_F_MIN,  GE_F_NINT,  &
!        GE_F_R2D,   GE_F_SIGN,  GE_F_SIN,  GE_F_SQRT, GE_F_TAN /)

! The following list of options has a blank option added.  This is a temporary
! workaround for the fact that old SS flows containing SETWORD often have the
! FUNCTION parameter set to blank.  This was the result of old versions of
! SETWORD which set that parameter to a blank when it was not needed.  In order
! for such flows to be read in SSdev or SSnightly, the blank option is added
! to the list so the parameter cache will not complain.

  character (len = *), dimension (0:GE_IF_LAST+1), parameter :: GE_FUNC_NAMES   &
    = (/ GE_F_IDENT, GE_F_ABS,   GE_F_ACOS, GE_F_ASIN, GE_F_ATAN, GE_F_COS,   &
         GE_F_D2R,   GE_F_IDENT, GE_F_INT,  GE_F_MAX,  GE_F_MIN,  GE_F_NINT,  &
         GE_F_R2D,   GE_F_SIGN,  GE_F_SIN,  GE_F_SQRT, GE_F_TAN, '     ' /)

  ! Task structure, complete description of one task.
  type task_struct

    sequence

    ! Note: structure members are arranged from largest to smallest
    ! to avoid compiler warnings regarding performance degradation.

    double precision    :: coef_a       ! Coefficient "a"                      8
    double precision    :: exp_a        ! Exponent "a".                        8
    double precision    :: coef_b       ! Coefficient "b"                      8
    double precision    :: exp_b        ! Exponent "b".                        8
    double precision    :: const        ! Constant "const".                    8

    double precision    :: exp_recip    ! = 1.0 / (10^NUM_SHIFT)               8
    double precision    :: dig_recip    ! = 1.0 / (10^NUM_KEEP)                8
    double precision    :: dig_factor   ! = 10^NUM_KEEP                        8

    integer             :: id           ! Index of symbolic name and arg count 4
    integer             :: hdr_set      ! Header word that is set by this task.4
    integer             :: hdr_a        ! Header word "a".                     4
    integer             :: hdr_b        ! Header word "b".                     4
    integer             :: ri_limit     ! NUM_TR limit between REG_INC changes.4
    integer             :: func         ! Function index for General Expr task.4
    integer             :: count        ! -------------                        4

    integer             :: num_shift    ! base 10 power for UNPACK             4
    integer             :: num_keep     ! maximum number of digits for UNPACK  4
    ! SMR (Source, Midpoint, Receiver) for GRID_GLOB, SURV_GLOB, GRID_USER
    ! and SURV_USER
    logical             :: smr_s        ! Compute source   grid coordinate.    4
    logical             :: smr_m        ! Compute Midpoint grid coordinate.    4
    logical             :: smr_r        ! Compute Receiver grid coordinate.    4
    character (len = 1) :: oper         ! General Expr operator (+, -, *, /).  1
    character (len = 1) :: handed       ! 'L' (Left) or 'R' (Right) handed.    1
    character (len = 6) :: dummy        ! put here for intel compiler          6

  end type task_struct

  type,public :: setword_struct

    private

    logical                         :: skip_wrapup ! wrapup flag.

    ! - Common globals

    integer                         :: nwih      ! Common globals, nwih
    type (grid_struct)              :: grid      ! Common globals xorg, yorg, dx

    ! - Process parameters

    character (len=3)               :: parallel_mode ! default=YES
    logical                         :: hdr_flag  ! .true. iff HDR_FLAG_VAL ok.
    integer                         :: hdr_flag_val ! Trace selector hdr word.
    integer                         :: seq       ! Trace sequence num (hdr 1).
    integer                         :: lun       ! In case we wish to print.

    ! - The list of tasks

    character (len = len (GE_FUNC_NAMES(1))) :: this_func  ! FUNCTION name for
                                                 ! use in General Expressions.
    character (len = len (TASK_NAMES(1)))    :: this_task ! TASK name of the
                                                ! currently selected task.
    character (len = max_line_len)  :: task_strs       (max_tasks)
    character (len = max_pline_len) :: after_tasks     (max_plines)
    character (len = max_pline_len) :: before_tasks    (max_plines)
    integer                         :: after_task_map  (max_plines)
    integer                         :: before_task_map (max_plines)
    integer                         :: current_task
    integer                         :: next_current_task
    integer                         :: num_after_tasks
    integer                         :: num_before_tasks
    integer                         :: num_tasks    ! Number of entries in
                                                    ! TASK array.
    type (grid_struct)              :: task_grid  (max_tasks)  ! Grid info.
                                                    ! One entry per task.
    type (task_struct)              :: task       (max_tasks)  ! Task info.
                                                    ! One entry per task.

    ! - Persistent card images

    integer                         :: num_vals     ! Live VALUE element cnt.
    character (len = 12)            :: prompt_strs (max_vals) ! PARAM strings
    character (len = 32)            :: value_strs  (max_vals) ! VALUE strings

  end type setword_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


  type(setword_struct),pointer,save :: object      ! needed for traps.

  type (task_struct), save :: empty_task
  type (task_struct), save :: end_task
  type (task_struct), save :: start_task

  integer, parameter :: scratch_num_50 = -50
  integer, parameter :: scratch_num_1  = -1

  integer, parameter         :: n_opt_parallel_mode = 2
  character(len=3),parameter :: opt_parallel_mode(n_opt_parallel_mode) &
        = (/'YES','NO '/)
contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


  subroutine setword_create (obj)

    type (setword_struct), pointer :: obj       ! arguments

    nullify(obj)
    allocate (obj)

    call setword_initialize (obj)

  end subroutine setword_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


  subroutine setword_delete (obj)

    type(setword_struct),pointer :: obj       ! arguments

    call setword_wrapup (obj)
    deallocate(obj)

  end subroutine setword_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


  subroutine setword_initialize (obj)

    type(setword_struct), intent (inout) :: obj       ! arguments

    obj%skip_wrapup = .true.
                             !
                             ! - task_struct
                             !coef_a, exp_a, coef_b, exp_b, const,
                             !exp_recip, dig_recip, dig_factor,
                             !id, hdr_set, hdr_a, hdr_b, 
                             !ri_limit, func, count,
                             !num_shift, num_keep,
                             !smr_s, smr_m, smr_r,
                             !oper, handed
    empty_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,             &
                              0.0, 0.0, 0.0,                       &
                              0,           0,  0,  0,              &
                              0, 0, 0, 0, 0,                       &
                              .false., .false., .false.,  &
                              ' ', ' ',' ')
    end_task   = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,             &
                              0.0, 0.0, 0.0,                       &
                              IT_END,      0,  0,  0,              &
                              0, 0, 0, 0, 0,                       &
                              .false., .false., .false.,  &
                              ' ', ' ',' ')
    start_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,             &
                              0.0, 0.0, 0.0,                       &
                              IT_START,    0,  0,  0,              &
                              0, 0, 0, 0, 0,                       &
                              .false., .false., .false.,  &
                              ' ', ' ',' ')

    ! - Common globals

    obj%nwih = 0     ! will have to test later to make sure has been reset.
    call grid_initialize (obj%grid)

    ! - Process parameters

    obj%parallel_mode     = 'NO'
    obj%hdr_flag          = .false.
    obj%hdr_flag_val      = 0

    obj%seq               = 1                 ! HDR_SEQUENCE counter
    obj%task              = empty_task
    obj%task_grid         = obj%grid

    obj%this_func         = GE_F_IDENT
    obj%this_task         = 'START'
    obj%num_vals          = 0
    obj%value_strs        = ''
    obj%prompt_strs       = ''
    obj%task (1)          = start_task
    obj%task (2)          = end_task
    obj%num_tasks         = 2
    obj%current_task      = 1
    obj%next_current_task = obj%current_task
    obj%task_strs         = ''
    obj%task_strs (1)     = '#1. START ()'
    obj%task_strs (2)     = '#2. END ()'
    obj%after_tasks       = ''
    obj%after_task_map    = 1
    obj%num_after_tasks   = 1
    obj%after_tasks (1)   = obj%task_strs (2)
    obj%after_task_map (1)= 2
    obj%before_tasks      = ''
    obj%num_before_tasks  = 0
    obj%before_task_map   = 0

    call setword_update (obj)

  end subroutine setword_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


  subroutine setword_update (obj)

    type(setword_struct),intent(inout),target :: obj        ! arguments

    character (len = max_line_len)  :: str
    integer                         :: i

    integer                         :: indices

    integer                         :: status
    integer                         :: task_id
    integer                         :: num_vals_keep
    character (len = 32)            :: value_strs_keep(max_vals)
    logical                         :: am_in_cps
    character(len=32)               :: std_libs

    object          => obj      ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!

    std_libs = "none"
    call pc_get_jdata ("std_libs", std_libs)
    am_in_cps = (std_libs /= "none")

    call pc_register_array_names (keyword = 'PROMPTS_ARRAYSET',          &
                                  arrays  = (/ 'PARAM', 'VALUE' /),    &
                                  narrays = 2)

    call pc_get_global (keyword = 'NWIH',    &
                        scalar  = obj%nwih)  ! number of header words.

    call pc_get_global (keyword = 'GRID',     &
                        scalar  = obj%grid)  ! grid transform structure.

    call pc_get (keyword = 'HDR_FLAG',    &
                 scalar  = obj%hdr_flag_val)

    obj%hdr_flag  = (obj%hdr_flag_val > 0)    &
                    .and. (obj%hdr_flag_val <= obj%nwih)

    ! - Verify HDR_FLAG

    if (.not. obj%hdr_flag) then

      if (obj%hdr_flag_val /= 0) then
        call pc_info ('SETWORD: HDR_FLAG must be 1 to NWIH (', obj%nwih,    &
                      ' or zero (0) if disabled.  Resetting to zero from ', &
                      obj%hdr_flag_val)
        obj%hdr_flag_val = 0
      end if

    end if

    obj%lun = pc_get_lun()

    call pc_get('parallel_mode' , obj%parallel_mode,setword_parallel_mode_trap)


    ! - Fetch the Current task

    call pc_get (keyword = 'TASKCNT',             &
                 scalar  = obj%num_tasks)

    call pc_get (keyword = 'TASKSEQ',             &
                 scalar  = obj%current_task)

    obj%next_current_task = obj%current_task

    call pc_get (keyword = 'TASK',                &
                 scalar  = obj%this_task)

    call string_to_upper (obj%this_task)

    call pc_get (keyword   = 'AFTER_TASKS',       &
                 array     = obj%after_tasks,     &
                 nelements = obj%num_after_tasks)

    call pc_get (keyword   = 'BEFORE_TASKS',      &
                 array     = obj%before_tasks,    &
                 nelements = obj%num_before_tasks)

    num_vals_keep = obj%num_vals
    value_strs_keep = obj%value_strs
    call pc_get (keyword   = 'VALUE',             &
                 array     = obj%value_strs,      &
                 nelements = obj%num_vals,        &
                 etrap     = setword_value_element_trap)
!   if (obj%num_vals /= num_vals_keep) obj%value_strs = value_strs_keep
!   obj%num_vals = num_vals_keep

    call pc_get (keyword = 'FUNCTION',            &
                 scalar  = obj%this_func)
    call string_to_upper (obj%this_func)

! The following line is added to reset this parameter to a valid option
! if it is blank.  This is a temporary workaround as described above where
! the options for this parameter are specified.

    if (obj%this_func == '     ') obj%this_func = 'IDENT'

    !!----------------- rebuild the task_strs task strings -----------------!!
    !!----------------- rebuild the task_strs task strings -----------------!!
    !!----------------- rebuild the task_strs task strings -----------------!!

    write (str, *) "#", obj%current_task, ". " // trim (obj%this_task) // " ("

    if (obj%this_task == T_GEN_EXPR) then

      obj%task (obj%current_task)%func                          &
        = setword_identify_task (task = trim (obj%this_func),   &
                                 list = GE_FUNC_NAMES(1:),      &
                                 num  = GE_IF_LAST)

      str = trim (str) // "Func = " // trim (obj%this_func) // ", "

    end if

    str = trim (str) // trim (obj%prompt_strs (1))    &
                     // " =" // trim (obj%value_strs (1))

    do i = 2, obj%num_vals
        str = trim (str)  // ", v =" // trim (obj%value_strs (i))
    end do

    str = trim (str) // " )"
    call setword_pack_plines (obj = obj)

    obj%task_strs (obj%current_task) = str

    call setword_parse_task_parms (obj    = obj, &
                                   status = status)
    if (status /= 0) then
      call pc_error ('setword_update: error returned by '    &
                     // 'setword_parse_task_parms for Current task', status)
      call pc_error ('setword_update: Current task is ', obj%current_task, &
                     ' ' // trim (str))
    end if


    !!------------------------ read KeyPress buttons -----------------------!!
    !!------------------------ read KeyPress buttons -----------------------!!
    !!------------------------ read KeyPress buttons -----------------------!!


    ! - Check for 'jump' selections to tasks Above or Below
    !   'ItemSelected' or ...

    if (pc_gui_action_present ('BEFORE_TASKS', 'ItemSelected')) then

      call pc_get_gui (keyword = 'BEFORE_TASKS',    &
                       action  = 'ItemSelected',    &
                       indx    = 1,    &
                       element = indices)

      obj%next_current_task = obj%before_task_map (indices)

    end if

    if (pc_gui_action_present ('AFTER_TASKS', 'ItemSelected')) then

      call pc_get_gui (keyword = 'AFTER_TASKS',     &
                       action  = 'ItemSelected',    &
                       indx    = 1,                 &
                       element = indices)

      obj%next_current_task = obj%after_task_map (indices)

    end if

    ! - Check to see if any 'insert an action' buttons were pressed

    do task_id = 1, IT_LAST

      if (task_id == IT_START) cycle
      if (task_id == IT_END)   cycle

      if (pc_pressed (keyword = trim (TASK_NAMES(task_id)))) then

        obj%next_current_task = obj%next_current_task + 1
        call setword_add_task (obj       = obj,              &
                               task_strs = obj%task_strs,    &
                               task_id   = task_id)

      end if

    end do

    call pc_put ('parallel_mode',obj%parallel_mode)
    call pc_put ('HDR_FLAG', obj%hdr_flag_val)


    ! - Navigator buttons

    if (pc_pressed ('previous task')) then
        if (obj%current_task > 2) obj%next_current_task = obj%current_task - 1
    endif

    if (pc_pressed ('next task')) then
        if (obj%current_task < obj%num_tasks - 1) obj%next_current_task = obj%current_task + 1
    endif

    if (pc_pressed ('DELETE')) then

      if (obj%current_task == 1) then

        call pc_warning ("** Cannot delete the leading 'START' task")

      else if (obj%current_task == obj%num_tasks) then

        call pc_warning ("** Cannot delete the ending 'END' task")

      else

        obj%num_tasks = obj%num_tasks - 1
        do i = obj%current_task,  obj%num_tasks
          obj%task      (i) = obj%task      (i+1)
          obj%task_grid (i) = obj%task_grid (i+1)
          obj%task_strs (i) = obj%task_strs (i+1)
        end do

      end if

    end if

    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!


    ! - Verify global data

    if (obj%nwih == 0) then
      call pc_warning ("Setword: Number of words in header "    &
                       // "(JOB DATA) was not set")
    end if

    ! - Make sure the first task is START

    if (obj%task(1)%id /= IT_START) then

      if (obj%num_tasks < max_tasks) obj%num_tasks = obj%num_tasks + 1
      obj%next_current_task = obj%next_current_task + 1

      call setword_insert_task (obj, 1, start_task, '# 1. START ()')

      call pc_warning ('SETWORD: First task must be the START task.  Added')

    end if

    ! - Check that the number of tasks is OK

    if (obj%num_tasks > max_tasks) then

      call pc_warning ('SETWORD: More actions are specified (',     &
                       obj%num_tasks, ') than are allowed ',  max_tasks)

      obj%num_tasks           = max_tasks
      obj%task(obj%num_tasks) = end_task

    end if

    ! - Make sure the last task is END

    if (obj%task(obj%num_tasks)%id /= IT_END) then

      if (obj%num_tasks < max_tasks) then
        obj%num_tasks = obj%num_tasks + 1
      else
        call pc_error ("setword_update: Too many tasks; the last is"    &
                       // " replaced with END")
      end if

      write (obj%task_strs (obj%num_tasks), *) "#", obj%num_tasks, ". END ()"
      obj%task(obj%num_tasks) = end_task

      call pc_warning ('SETWORD: Last task must be the END task.  Added')

    end if

    ! - Now derive the task-strings from the tasks.

    if (obj%num_tasks <= 0) then
      obj%num_tasks    = 2
      obj%next_current_task = 1
      obj%task (1) = start_task
      obj%task (2) = end_task
    end if

    ! - Translate tasks from binary to strings, then the strings
    !   to continue-lines plines (partial lines)

    call setword_str_tasks (tasks  = obj%task (1:obj%num_tasks),   &
                            ntasks = obj%num_tasks,                &
                            tstr   = obj%task_strs)

    obj%current_task = max (1, min (a1 = obj%num_tasks - 1,     &
                                    a2 = obj%next_current_task))

    call setword_format_plines (obj)


    !!---------------------- write parameters ------------------------------!!
    !!---------------------- write parameters ------------------------------!!
    !!---------------------- write parameters ------------------------------!!

    call pc_put_options_field &
                   ('PARALLEL_MODE',opt_parallel_mode,n_opt_parallel_mode)

    call pc_put_options_field ('FUNCTION', GE_FUNC_NAMES (1:), GE_IF_LAST)

    call pc_put_global ('NWIH',  obj%nwih)  ! number of header words.
    call pc_put_global ('GRID',  obj%grid)  ! grid transform structure.

    call pc_put_control ('ntapes',       0)          ! default 0
    call pc_put_control ('need_request', .false.)    ! default false
    call pc_put_control ('need_label',   .false.)    ! default false
    call pc_put_control ('twosets',      .false.)    ! default false
    call pc_put_control ('nscratch',     0)          ! default 0
    call pc_put_control ('nstore',       0)          ! default 0
    call pc_put_control ('iftd',         .false.)    ! default false
    call pc_put_control ('ndisk',        0)          ! default 0
    call pc_put_control ('setup_only',   .false.)    ! default .false.

    call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
    call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
    call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
    call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
    call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
    call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')

    ! - Update the Current Task

    call pc_put_gui_only ("warning", "do not try to insert or delete any rows in any of the arrays")

    call pc_put ('BEFORE_TASKS', obj%before_tasks, obj%num_before_tasks)
    call pc_put_minsize_array (keyword = 'BEFORE_TASKS',    &
                               minsize = obj%num_before_tasks)
    call pc_put_maxsize_array (keyword = 'BEFORE_TASKS',    &
                               maxsize = obj%num_before_tasks)

    call pc_put ('TASKSEQ',  obj%current_task)
    call pc_put ('TASK',     obj%this_task)

    call pc_put_gui_only ('current task name',     obj%this_task)

    call pc_put_sensitive_field_flag ('FUNCTION'     , obj%this_task == T_GEN_EXPR)
    call pc_put_sensitive_array_flag ('PARAM'        , am_in_cps)
    call pc_put_sensitive_array_flag ('BEFORE_TASKS' , am_in_cps)
    call pc_put_sensitive_array_flag ('AFTER_TASKS'  , am_in_cps)
    call pc_put_visible_flag         ('TASK'         , am_in_cps)
    call pc_put_visible_flag         ('TASKSEQ'      , am_in_cps)
    call pc_put_visible_flag         ('TASKCNT'      , am_in_cps)

    call pc_put ('FUNCTION',     obj%this_func)
    call pc_put ('PARAM',    obj%prompt_strs, obj%num_vals)
    call pc_put ('VALUE',    obj%value_strs,  obj%num_vals)

    call pc_put_minsize_arrayset  (keyword = 'PARAM_ARRAYSET',    &
                                   minsize = obj%num_vals)
    call pc_put_maxsize_arrayset  (keyword = 'PARAM_ARRAYSET',    &
                                   maxsize = obj%num_vals)

    call pc_put ('TASKCNT',  obj%num_tasks)

    call pc_put_gui_only ('current task number',  obj%current_task)
    call pc_put_gui_only ('total number of tasks',  obj%num_tasks)

    call pc_put ('AFTER_TASKS',  obj%after_tasks,  obj%num_after_tasks)
    call pc_put_minsize_array (keyword = 'AFTER_TASKS',    &
                               minsize = obj%num_after_tasks)
    call pc_put_maxsize_array (keyword = 'AFTER_TASKS',    &
                               maxsize = obj%num_after_tasks)

    !!------------------- prepare for execution ----------------------------!!
    !!------------------- prepare for execution ----------------------------!!
    !!------------------- prepare for execution ----------------------------!!


    if (pc_do_not_process_traces()) return

    obj%skip_wrapup = .false.


    !!--------------------- finish update ----------------------------------!!
    !!--------------------- finish update ----------------------------------!!
    !!--------------------- finish update ----------------------------------!!

  end subroutine setword_update


  !!----------------------------- traps ------------------------------------!!
  !!----------------------------- traps ------------------------------------!!
  !!----------------------------- traps ------------------------------------!!

! Made parallel again (Menger) (but I won't allow resetting of seq if so.)
! Made non parallel as any process using obj%seq is not parallel safe
! Later, one could decide when parallel safe and when it is not or a
! PCPS call can be made to get a sequence number consistent across cpus


  subroutine setword_parallel_mode_trap (parallel_mode)
    character (len=*), intent (in) :: parallel_mode
    select case (object%parallel_mode)
      case('NO ')
        call pc_put_control('PARALLEL_SAFE'        ,.false.)
      case('YES')
        call pc_put_control('PARALLEL_SAFE'        ,.true.)
      case default
        call pc_info ('SETWORD: parallel_mode must be YES or NO.  If YES, '//&
        'then header_one is not set to sequence number by setword, it is '//&
        'assumed to have been set by TRIN or other process.  It can be reset'//&
        ' but you cannot')
        call pc_info ('assume the internal sequence number of setword is '//&
        'the actual trace sequence number, since each worker process will '//&
        'have its own internal sequence number.  If you need to rely on '//&
        'hdr1, then set to NO,')
        call pc_info (' otherwise, YES allows parallel.')
        object%parallel_mode='NO'
 !      call pc_put ('parallel_mode',object%parallel_mode)
 !      call pc_jump_field(parallel_mode) 
    end select
  end subroutine setword_parallel_mode_trap
  

  !!---------------------- setword_value_element_trap ----------------------!!
  !!---------------------- setword_value_element_trap ----------------------!!
  !!---------------------- setword_value_element_trap ----------------------!!


  subroutine setword_value_element_trap (keyword,indx,action)

    character (len=*), intent (in) :: keyword           ! arguments
    integer,           intent (in) :: indx              ! arguments
    integer,           intent (in) :: action            ! arguments
                       ! action is PC_INSERT or PC_REMOVE or PC_MODIFY.

    integer :: i

    if (action == PC_INSERT) then

      ! - Undo the shift, leave the copied value in place

      object%num_vals = object%num_vals - 1

      do i = indx + 1, object%num_vals - 1
        object%value_strs (i) = object%value_strs (i + 1)
      end do

    else if (action == PC_REMOVE) then

      ! - First, undo the shift

      object%num_vals = object%num_vals + 1

      do i = indx+1, object%num_vals
        object%value_strs (i) = object%value_strs (i - 1)
      end do

      ! - then clear the value

      object%value_strs (indx) = ''

    end if

  end subroutine setword_value_element_trap



  !!------------------------- setword_insert_task --------------------------!!
  !!------------------------- setword_insert_task --------------------------!!
  !!------------------------- setword_insert_task --------------------------!!

  subroutine setword_insert_task (obj, indx, new_task, new_str)

    type (setword_struct), intent (inout) :: obj
    integer,               intent (in)    :: indx              ! arguments
    type (task_struct),    intent (in)    :: new_task
    character (len = *),   intent (in)    :: new_str

    integer :: i
    integer :: t

    if (indx <= 1) t = 2
    if (indx > obj%num_tasks) then
      t = obj%num_tasks
    else
      t = indx
    end if

    if (obj%num_tasks < max_tasks) obj%num_tasks = obj%num_tasks + 1

    do i = obj%num_tasks, t + 1, -1

      obj%task      (i) = obj%task      (i - 1)
      obj%task_grid (i) = obj%task_grid (i - 1)
      obj%task_strs (i) = obj%task_strs (i - 1)

    end do

    obj%task      (t) = new_task
    obj%task_grid (t) = obj%grid
    obj%task_strs (t) = new_str

  end subroutine setword_insert_task

  !!------------------------- setword_pack_plines --------------------------!!
  !!------------------------- setword_pack_plines --------------------------!!
  !!------------------------- setword_pack_plines --------------------------!!

  subroutine setword_pack_plines (obj)

    type (setword_struct), intent (inout) :: obj

    integer :: i
    integer :: p
    integer :: t

    obj%task_strs = ''
    t = 0

    do p = 1, obj%num_before_tasks

      ! - Look for the 'beginning of task' (#) marker

      if (index (obj%before_tasks(p), '#') > 0) t = t + 1

      obj%task_strs (t) = trim (obj%task_strs (t))     &
                          // ' '                  &
                          // trim (adjustl (obj%before_tasks (p)))

    end do

    ! - CURRENT task

    if (obj%this_task /= '     ') then

      t = t + 1

      if (obj%num_vals > 0) then

        if (obj%this_task == T_GEN_EXPR) then

          write (obj%task_strs (t), *)                                 &
             '#', obj%current_task, '. ', trim (obj%this_task) , ' (', &
             obj%this_func, ', ',                                      &
             (trim (obj%value_strs (i)), ', ', i = 1, obj%num_vals-1), &
              trim (obj%value_strs (obj%num_vals)), ')'

        else

          write (obj%task_strs (t), *)                                   &
             '#', obj%current_task, '. ', trim (obj%this_task) , ' (',   &
             (trim (obj%value_strs (i)), ', ', i = 1, obj%num_vals-1),   &
              trim (obj%value_strs (obj%num_vals)), ')'

        end if

      else

        write (obj%task_strs (t), *) '#', obj%current_task, '. ()'

      end if

    end if

    ! - AFTER tasks

    do p = 1, obj%num_after_tasks

      if (index (obj%after_tasks(p), '#') > 0) t = t + 1

      obj%task_strs (t) = trim (obj%task_strs (t))     &
                          // ' '                   &
                          // trim (adjustl(obj%after_tasks(p)))

    end do

    if (t > 0) obj%num_tasks = t

  end subroutine setword_pack_plines


  !!------------------------- setword_format_plines --------------------------!!
  !!------------------------- setword_format_plines --------------------------!!
  !!------------------------- setword_format_plines --------------------------!!

  subroutine setword_format_plines (obj)

    type (setword_struct), intent (inout) :: obj

    character (len = max_line_len) :: str
    integer :: cLoc
    integer :: eLoc
    integer :: p
    integer :: pLoc
    integer :: t
    integer :: v

    p = 0

    do t = 1, obj%current_task - 1

      ! - Split the string on spaces and commas

      str = obj%task_strs (t)

      do while (len_trim (str) > max_pline_len)

        pLoc = index (string    = str (1:max_pline_len),    &
                      substring = ",",                      &
                      back      = .true.)

        if (pLoc < 10) pLoc = max_pline_len
        p = p + 1
        obj%before_tasks    (p) = str (1:pLoc)
        obj%before_task_map (p) = t
        str = '     ' // str (pLoc + 1:)

      end do

      p = p + 1
      obj%before_tasks    (p) = str
      obj%before_task_map (p) = t

    end do

    obj%num_before_tasks = p
    obj%before_task_map (p+1:) = 1

    ! - The current task

    t               = obj%current_task
    str             = obj%task_strs (t)
    obj%this_task   = TASK_NAMES    (obj%task (t)%id)
    v               = 0
    obj%prompt_strs = ''
    obj%value_strs  = ''
    obj%num_vals    = 0

    pLoc = index (str, '(')
    if (pLoc > 0) str =str (pLoc+1:)

    ! - 'Move' the GEN_EXPR function to the combo box

    if (obj%task (t)%id == IT_GEN_EXPR) then
      pLoc = index (str, ',')
      if (pLoc > 0) str = str (pLoc+1:)
      obj%this_func = GE_FUNC_NAMES (obj%task (t)%func)
    end if

    if (index (str, ')') > 2) then

      do while (index (str, ',') > 1)

        v = v + 1
        eLoc = index (str, '=')
        if (eLoc > 0) obj%prompt_strs (v) = str (1:eLoc - 1)

        cLoc = index (str, ',')
        obj%value_strs (v) = adjustl (str (eLoc + 1:cLoc - 1))
        str = str (cLoc + 1:)

      end do

      v = v + 1
      eLoc = index (str, '=')
      if (eLoc > 0) obj%prompt_strs (v) = str (1:eLoc - 1)

      cLoc = index (str, ')')
      obj%value_strs (v) = adjustl (str (eLoc + 1:cLoc - 1))

    end if

    obj%num_vals = v

    do v = 1, obj%num_vals

      obj%prompt_strs (v) = adjustr (obj%prompt_strs (v))
      obj%value_strs  (v) = adjustl (obj%value_strs  (v))

    end do

    p = 0

    do t = obj%current_task + 1, obj%num_tasks

      ! - Split the string on spaces and commas

      str = obj%task_strs (t)

      do while (len_trim (str) > max_pline_len)

        pLoc = index (string    = str (1:max_pline_len),    &
                      substring = ",",                      &
                      back      = .true.)

        if (pLoc < 10) pLoc = max_pline_len
        p = p + 1
        obj%after_tasks    (p) = str (1:pLoc)
        obj%after_task_map (p) = t
        str = '     ' // str (pLoc + 1:)

      end do

      p = p + 1
      obj%after_tasks    (p) = str
      obj%after_task_map (p) = t

    end do

    obj%num_after_tasks = p
    obj%after_task_map (p+1:) = t

  end subroutine setword_format_plines


  !!---------------------------- setword_parse -----------------------------!!
  !!---------------------------- setword_parse -----------------------------!!
  !!---------------------------- setword_parse -----------------------------!!


  subroutine setword_parse (a_str, list, item_cnt, ierr)

    character (len = *), intent (in)  :: a_str
    character (len = *), intent (inout) :: list (:)
    integer,             intent (out) :: item_cnt
    integer,             intent (out) :: ierr

    character (len = max_line_len) :: str
    character :: ch
    integer   :: ch_len
    integer   :: ch_pos
    integer   :: i
    integer   :: list_len
    integer   :: p_pos

    item_cnt = 1
    ch_pos   = 1
    ch_len   = len (string = list (1))
    list_len = size (list)
    list     = ' '
    ierr     = 0

    p_pos = max (a1 = index (a_str, '('), a2 = 1)
    str   = a_str (1:)

    do i = 1, len_trim (string = str)

      if (ch_pos > ch_len) then

        call pc_error ('Overflowed string contents '                          &
                       // list (item_cnt) // ' -- parameters too long in '    &
                       // trim (str))
        ierr = -99
        return

      else

        ch = str (i:i)

        select case (ch)
          case (' ', '(', ')', '"', "'")
          case ('=')
            if (item_cnt <= list_len) list (item_cnt) = ' '
            ch_pos = 1
          case (',')
            if (item_cnt < list_len) then
              item_cnt = item_cnt + 1
            else
              ierr = ierr - 1
            end if
            ch_pos = 1
          case default
            list (item_cnt) (ch_pos:ch_pos) = ch
            ch_pos                          = ch_pos + 1
        end select

      end if

    end do

    if ((item_cnt + ch_pos) == 2) item_cnt = 0
    if (ierr == 0) ierr = list_len - item_cnt
    if (ierr /= 0) call pc_error ("setword_parse: str " // trim (str)     &
                                  // " -- len ", list_len, ", ", item_cnt)

  end subroutine setword_parse


  !!------------------------ setword_identify_task -------------------------!!
  !!------------------------ setword_identify_task -------------------------!!
  !!------------------------ setword_identify_task -------------------------!!

  function setword_identify_task (task, list, num) result (task_id)

    character (len = *), intent (in) :: task
    character (len = *), intent (in) :: list (:)
    integer,             intent (in) :: num
    integer                          :: task_id

    integer :: i

    task_id = 0

    do i = 1, num
      if (llt (task, list(i))) then
        exit
      else if (.not. lgt (task, list(i))) then
        task_id = i
        exit
      end if
    end do

  end function setword_identify_task

  !!--------------------------- setword_add_task ---------------------------!!
  !!--------------------------- setword_add_task ---------------------------!!
  !!--------------------------- setword_add_task ---------------------------!!

  subroutine setword_add_task (obj, task_strs, task_id)

    type (setword_struct), intent (inout) :: obj
    character (len = *),   intent (inout) :: task_strs (:)
    integer,               intent (in)    :: task_id

    type (task_struct) :: my_task

    if (obj%num_tasks >= max_tasks) then
      obj%num_tasks = max_tasks
      !
      call pc_error ("Maximum (", max_tasks, ") tasks have "   &
                       // "already been defined.")
      return
    end if

    ! - Set task default parameters

    if ((task_id < 1) .or. (task_id > IT_LAST)) then

      call pc_error ("setword_add_task:  Unknown task_id (", task_id,    &
                     ") is not between 1 and ", IT_LAST)

    else

      select case (task_id)
        case (IT_ABS)
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_ABS,      1,  1,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_AZMTH)
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_AZMTH,    1,  0,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_BIN_CENT)      
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_BIN_CENT,    0,  0,  0,                        &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_COPY)          
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_COPY,     1,  1,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_COUNTER)       
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_COUNTER,    1,  1,  0,                         &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_END)           
          my_task = end_task
        case (IT_GEN_EXPR)      
          my_task = task_struct (1.0, 1.0, 1.0, 1.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_GEN_EXPR,    1,  1,  1,                        &
                       0, GE_IF_IDENT, 0, 0, 0,                          &
                       .false., .false., .false.,                        &
                       '+', ' ',' ')
        case (IT_GRID_GLOB)     
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_GRID_GLOB,0,  0,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_GRID_USER)     
          my_task = task_struct (0.0, 1.0, 0.0, 1.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_GRID_USER,1,  1,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_LIN_COMB)      
          my_task = task_struct (1.0, 0.0, 1.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_LIN_COMB, 1,  1,  1,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_MAX)           
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_MAX,      1,  1,  1,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_MIN)           
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_MIN,      1,  1,  1,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_OFFSET_ABS)    
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_OFFSET_ABS,0,  0,  0,                          &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_OFFSET_RECALC) 
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_OFFSET_RECALC,0,  0,  0,                       &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_OFFSET_SIGNED) 
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_OFFSET_SIGNED,0,  0,  0,                       &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_REG_INC)       
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_REG_INC,  1,  0,  0,                           &
                       1, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_ROUND)         
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_ROUND,    1,  1,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_SINGLE_VALUE)  
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_SINGLE_VALUE,1,  0,  0,                        &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_START)         
          my_task = start_task
        case (IT_STRETCH)       
          my_task = task_struct (0.0, 0.0, 1.0, 1.0, 1.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_STRETCH,  0,  1,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_SURV_GLOB)     
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_SURV_GLOB,0,  0,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_SURV_USER)     
          my_task = task_struct (0.0, 1.0, 0.0, 1.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_SURV_USER,1,  1,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_SWAP)          
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_SWAP,     1,  1,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_TRUNCATE)
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_TRUNCATE,48, 49,  0,                           &
                       0, 0, 0, 0, 0,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case (IT_UNPACK)
          my_task = task_struct (0.0, 0.0, 0.0, 0.0, 0.0,                &
                       0.0, 0.0, 0.0,                                    &
                       IT_UNPACK,  48, 49,  0,                           &
                       0, 0, 0, 0, 3,                                    &
                       .false., .false., .false.,                        &
                       ' ', ' ',' ')
        case default
          my_task = empty_task
          call pc_error ("setword_add_task: Unknown task ID ", task_id)
      end select

      if (task_id == IT_GRID_USER .or. task_id == IT_SURV_USER) then
        call grid_initialize (obj = obj%task_grid (obj%next_current_task))
      end if

    end if

    if (obj%next_current_task > obj%num_tasks) then

      obj%next_current_task = obj%num_tasks

    end if

    call setword_insert_task (obj       = obj,                      &
                              indx      = obj%next_current_task,    &
                              new_task  = my_task,                  &
                              new_str   = '# DUMMY ()')

  end subroutine setword_add_task


  !!-------------------------- setword_str_tasks ---------------------------!!
  !!-------------------------- setword_str_tasks ---------------------------!!
  !!-------------------------- setword_str_tasks ---------------------------!!

  subroutine setword_str_tasks (tasks, ntasks, tstr)

    type (task_struct),  dimension (:), intent (in)  :: tasks
    integer,                            intent (in)  :: ntasks
    character (len = *), dimension (:), intent (out) :: tstr

    character (len =            1)     :: smr_m
    character (len =            1)     :: smr_r
    character (len =            1)     :: smr_s
    character (len = max_line_len    ) :: params
    character (len = max_line_len * 4) :: p_string
    integer                            :: t
    integer                            :: tlen

    tlen = size (tasks)

    do t = 1, ntasks

      params = ""

      select case (tasks (t)%id)
        case (IT_ABS)
          write (params, *)    &
            "(HDR_TO = ",       tasks (t)%hdr_set,    &
            ", HDR_FROM = ",    tasks (t)%hdr_a,      &
            ")"

        case (IT_AZMTH)
          write (params, *)    &
            "(HDR_TO = ",      tasks (t)%hdr_set,    &
            ", ANG_ADD = ",    tasks (t)%const,      &
            ")"

        case (IT_BIN_CENT)
          params = "( )"

        case (IT_COPY)
          write (params, *)    &
            "(HDR_TO = ",    tasks (t)%hdr_set,    &
            ", HDR_FROM = ", tasks (t)%hdr_a,      &
            ")"

        case (IT_COUNTER)
          write (params, *)   &
            "(HDR_TO =",     tasks (t)%hdr_set,    &
            ", VAL_INIT = ", tasks (t)%coef_a,     &
            ", HDR_A = ",    tasks (t)%hdr_a,      &
            ", VAL_INC = ",  tasks (t)%coef_b,     &
             ")"

        case (IT_END)
          params = "( )"

        case (IT_GEN_EXPR)
          write (params, *)     &
            "(FUNCT = ", trim (GE_FUNC_NAMES (tasks (t)%func)),    &
            ", HDR_SET = ", tasks (t)%hdr_set,                     &
            ", COEF_A = ",  tasks (t)%coef_a,                      &
            ", HDR_A = ",   tasks (t)%hdr_a,                       &
            ", EXP_A = ",   tasks (t)%exp_a,                       &
            ", OPER = ",    tasks (t)%oper,                        &
            ", COEF_B = ",  tasks (t)%coef_b,                      &
            ", HDR_B = ",   tasks (t)%hdr_b,                       &
            ", EXP_B = ",   tasks (t)%exp_b,                       &
            ", CONST = ",   tasks (t)%const,                       &
            ")"

        case (IT_GRID_GLOB)
          if (tasks (t)%smr_s .or. tasks (t)%smr_m .or. tasks (t)%smr_r) then
            if (tasks (t)%smr_s) then; smr_s = 'S'; else ; smr_s = ' '; end if
            if (tasks (t)%smr_m) then; smr_m = 'M'; else ; smr_m = ' '; end if
            if (tasks (t)%smr_r) then; smr_r = 'R'; else ; smr_r = ' '; end if
            params = "(SMR = " // smr_s // smr_m // smr_r // ")"
          else
            params = "(SMR = none)"
          end if

        case (IT_GRID_USER)
          if (tasks (t)%smr_s .or. tasks (t)%smr_m .or. tasks (t)%smr_r) then
            if (tasks (t)%smr_s) then; smr_s = 'S'; else ; smr_s = ' '; end if
            if (tasks (t)%smr_m) then; smr_m = 'M'; else ; smr_m = ' '; end if
            if (tasks (t)%smr_r) then; smr_r = 'R'; else ; smr_r = ' '; end if
            params = "(SMR = " // smr_s // smr_m // smr_r
          else
            params = "(SMR = none"
          end if

          write (params, *)    &
            trim (string = params),                  &
            ", XORG = ",        tasks (t)%coef_a,    &
            ", YORG = ",        tasks (t)%coef_b,    &
            ", ANGLE = ",       tasks (t)%const,     &
            ", BIN_WID_INL = ", tasks (t)%exp_a,     &
            ", BIN_WID_CRL = ", tasks (t)%exp_b,     &
            ", LR_HANDED = ",   tasks (t)%handed,    &
            ")"

        case (IT_LIN_COMB)
          write (params, *)    &
            "(HDR_SET = ", tasks (t)%hdr_set,                      &
            ", HDR_A = ",  tasks (t)%hdr_a,                        &
            ", COEF_A = ", setword_dble (r = tasks (t)%coef_a),    &
            ", HDR_B = ",  tasks (t)%hdr_b,                        &
            ", COEF_B = ", setword_dble (r = tasks (t)%coef_b),    &
            ", CONST = ",  setword_dble (r = tasks (t)%const),     &
            ")"

        case (IT_MAX)
          write (params, *)    &
            "(HDR_TO =",  tasks (t)%hdr_set,    &
            ", HDR_A = ", tasks (t)%hdr_a,      &
            ", HDR_B = ", tasks (t)%hdr_b,      &
            ")"

        case (IT_MIN)
          write (params, *)    &
            "(HDR_TO = ", tasks (t)%hdr_set,    &
            ", HDR_A = ", tasks (t)%hdr_a,      &
            ", HDR_B = ", tasks (t)%hdr_b,      &
            ")"

        case (IT_OFFSET_ABS)
          params = "( )"

        case (IT_OFFSET_RECALC)
          params = "( )"

        case (IT_OFFSET_SIGNED)
          params = "( )"

        case (IT_REG_INC)
         write (params, *)    &
            "(HDR_SET = ",   tasks (t)%hdr_set,    &
            ", VAL_INIT = ", tasks (t)%coef_a,     &
            ", NUM_TR = ",   tasks (t)%ri_limit,   &
            ", VAL_INC = ",  tasks (t)%coef_b,     &
            ", HDR_OPT = ",  tasks (t)%hdr_a,      &
            ")"

        case (IT_ROUND)
          write (params, *)    &
            "(HDR_TO = ",    tasks (t)%hdr_set,    &
            ", HDR_FROM = ", tasks (t)%hdr_a,      &
            ")"

        case (IT_SINGLE_VALUE)
          write (params, *)    &
            "(HDR_TO = ", tasks (t)%hdr_set,    &
            ", CONST = ", tasks (t)%const,      &
            ")"

        case (IT_START)
          params = "( )"

        case (IT_STRETCH)
          write (params, *)    &
            "(HDR_A = ",    tasks (t)%hdr_a,     &
            ", VAL_A1 = ",  tasks (t)%coef_a,    &
            ", VAL_A2 = ",  tasks (t)%coef_b,    &
            ", BV1 = ",     tasks (t)%exp_a,     &
            ", BV2 = ",     tasks (t)%exp_b,     &
            ", HDR_SET = ", tasks (t)%hdr_set,   &
            ")"

        case (IT_SURV_GLOB)
          if (tasks (t)%smr_s .or. tasks (t)%smr_m .or. tasks (t)%smr_r) then
            if (tasks (t)%smr_s) then; smr_s = 'S'; else ; smr_s = ' '; end if
            if (tasks (t)%smr_m) then; smr_m = 'M'; else ; smr_m = ' '; end if
            if (tasks (t)%smr_r) then; smr_r = 'R'; else ; smr_r = ' '; end if
            params = "(SMR = " // smr_s // smr_m // smr_r // ")"
          else
            params = "(SMR = none)"
          end if

        case (IT_SURV_USER)
          if (tasks (t)%smr_s .or. tasks (t)%smr_m .or. tasks (t)%smr_r) then
            if (tasks (t)%smr_s) then; smr_s = 'S'; else ; smr_s = ' '; end if
            if (tasks (t)%smr_m) then; smr_m = 'M'; else ; smr_m = ' '; end if
            if (tasks (t)%smr_r) then; smr_r = 'R'; else ; smr_r = ' '; end if
            params = "(SMR = " // smr_s // smr_m // smr_r
          else
            params = "(SMR = none"
          end if

          write (params, *)    &
            trim (string = params),                  &
            ", XORG = ",        tasks (t)%coef_a,    &
            ", YORG = ",        tasks (t)%coef_b,    &
            ", ANGLE = ",       tasks (t)%const,     &
            ", BIN_WID_INL = ", tasks (t)%exp_a,     &
            ", BIN_WID_CRL = ", tasks (t)%exp_b,     &
            ", LR_HANDED = ",   tasks (t)%handed,    &
            ")"

        case (IT_SWAP)
          write (params, *)     &
            "(HDR_TO = ",    tasks (t)%hdr_set,    &
            ", HDR_FROM = ", tasks (t)%hdr_a,      &
            ")"

        case (IT_TRUNCATE)
          write (params, *)    &
            "(HDR_TO = ",       tasks (t)%hdr_set,    &
            ", HDR_FROM = ",    tasks (t)%hdr_a,      &
            ")"

        case (IT_UNPACK)
          write (params, *)    &
            "(HDR_TO = ",       tasks (t)%hdr_set,    &
            ", HDR_FROM = ",    tasks (t)%hdr_a,      &
            ", NUM_SHIFT = ",   tasks (t)%num_shift,   &
            ", NUM_KEEP = ",    tasks (t)%num_keep,     &
            ")"

        case default
          params = "( )"

      end select
      
      write (p_string, *) '# ', t, '. ' // trim (TASK_NAMES (tasks (t)%id)),  &
                          ' ' // trim (params)
      tstr (t) = p_string (1:max_line_len)

    end do

    do t = ntasks + 1, tlen
      tstr (t) = ''
    end do

  end subroutine setword_str_tasks


  !!----------------------------- setword_dble -----------------------------!!
  !!----------------------------- setword_dble -----------------------------!!
  !!----------------------------- setword_dble -----------------------------!!

  function setword_dble (r) result (str)

    double precision, intent (in) :: r
    character (len = 40)          :: str

    integer :: i

    if (r == 0.0) then
      str = '0.'
    else
      write (str, '(f40.16)') r
      do i = 40, 40-16, -1
        if (str (i:i) == '0') then
          str (i:i) = ' '
        else
          exit
        end if
      end do
      str = adjustl (str)
    end if

  end function setword_dble


  !!----------------------- setword_parse_task_parms -----------------------!!
  !!----------------------- setword_parse_task_parms -----------------------!!
  !!----------------------- setword_parse_task_parms -----------------------!!

  subroutine setword_parse_task_parms (obj, status)

    type (setword_struct), intent (inout) :: obj
    integer,               intent (out)   :: status

    character (len =  10)           :: smr_str
    character (len =  10)           :: lr_str
    character (len =  28)           :: vlist (15)
    character (len = max_line_len)  :: task_str
    integer                         :: ch
    integer                         :: item_cnt
    integer                         :: n
    integer                         :: p1
    integer                         :: parse_err
    integer                         :: r_stat

    integer                         :: task_id
    integer                         :: task_num
    type (task_struct)              :: my_task

    status = 0

  loop_thru_task_parms:    &
    do task_num = 1, obj%num_tasks
      !
      task_str = obj%task_strs (task_num)
      call string_to_upper (task_str)
      n = 0
      p1 = index (task_str, '(')

    loop_parse_taskname:   &
      do ch = 1, p1
        select case (task_str (ch:ch))
        case ('#', ' ', '0':'9', '.')
        case default; n = ch; exit loop_parse_taskname
        end select
      end do loop_parse_taskname

      if (n == 0) then
        call pc_error ('setword_parse_task_parms: Cannot interpret task line ' &
                       // trim (task_str))
        call pc_error ('Error in task #', task_num)
        status = -1
        cycle loop_thru_task_parms
      end if
      task_id = setword_identify_task (task = trim (task_str(n:p1-1)),     &
                                       list = TASK_NAMES,                  &
                                       num  = IT_LAST)

      if (task_id <= 0) then
        call pc_error ('setword_parse_task_parms: Cannot interpret task '     &
                       // trim (task_str))
        call pc_error ('Error in task #', task_num)
        call pc_error ('Error in task '// trim (task_str))
        status = -2
        cycle loop_thru_task_parms
      end if

      call setword_parse (a_str    = task_str (p1:),                      &
                          list     = vlist (1:IT_ITEM_CNT (task_id)),     &
                          item_cnt = item_cnt,                            &
                          ierr     = parse_err)

      if (parse_err /= 0) then 

        if (parse_err < 0) item_cnt = item_cnt - parse_err
        call pc_error ('setword_parse_task_parms: '    &
                       // 'Cannot interpret task parameters.')
        call pc_error ('  Found ', item_cnt,    &
                       ' parameters instead of ', IT_ITEM_CNT (task_id))
        call pc_error ('  Parsing ', trim (task_str (p1:)))
        status = -3
        cycle loop_thru_task_parms

      end if

      my_task = obj%task (task_num)
      my_task%id = task_id

      r_stat = 0

      select case (task_id)
        case (IT_ABS)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_To value for ABS')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_From value for ABS')

        case (IT_AZMTH)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for AZMTH')
          read (vlist (2), *, iostat = r_stat) my_task%const

        case (IT_BIN_CENT)

        case (IT_COPY)
          r_stat = 0
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_To value for COPY')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_From value for COPY')

        case (IT_COUNTER)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for COUNTER')
          read (vlist (2), *, iostat = r_stat) my_task%coef_a
          call setword_read_hdr (str   = vlist (3),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for COUNTER')
          read (vlist (4), *, iostat = r_stat) my_task%coef_b

        case (IT_END)
          r_stat = 0

        case (IT_GEN_EXPR)
          call string_to_upper (vlist (1))
          my_task%func = setword_identify_task (task = trim (vlist (1)),   &
                                                list = GE_FUNC_NAMES(1:),  &
                                                num  = GE_IF_LAST)
          if (my_task%func <= 0) then
            call pc_error ('setword: Cannot interpret GEN_EXPR Function '  &
                           // vlist (1) // '.  Error in task #', task_num)
            status = -11
          end if

          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for GEN_EXPR')

          read (vlist (3), *, iostat = r_stat) my_task%coef_a
          if (r_stat /= 0) then
            my_task%coef_a = 0.0
            call pc_error ('SETWORD: Could not interpret real coefficient '    &
                           // 'value A: ', vlist(3))
          end if

          call setword_read_hdr (str   = vlist (4),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for GEN_EXPR')
          read (vlist (5), *, iostat = r_stat) my_task%exp_a
          if (my_task%exp_a < 0.0) then
            call pc_error ("Exponent Exp_A cannot be negative",   &
                            my_task%exp_a)
            status = -12
            my_task%exp_a = abs (my_task%exp_a)
          end if

          my_task%oper = vlist (6)
          read (vlist (7), *, iostat = r_stat) my_task%coef_b
          call setword_read_hdr (str   = vlist (8),                  &
                                 hdr   = my_task%hdr_b,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_B value for GEN_EXPR')
          read (vlist ( 9), *, iostat = r_stat) my_task%exp_b
          if (my_task%exp_b < 0.0) then
            call pc_error ("Exponent Exp_B cannot be negative",   &
                            my_task%exp_b)
            status = -13
            my_task%exp_b = abs (my_task%exp_b)
          end if
          read (vlist (10), *, iostat = r_stat) my_task%const

        case (IT_GRID_GLOB)
          smr_str = vlist (1)
          my_task%smr_s                                              &
            = (index (string = smr_str, substring = 's') > 0)        &
              .or. (index (string = smr_str, substring = 'S') > 0)
          my_task%smr_m                                              &
            = (index (string = smr_str, substring = 'm') > 0)        &
              .or. (index (string = smr_str, substring = 'M') > 0)
          my_task%smr_r                                              &
            = (index (string = smr_str, substring = 'r') > 0)        &
              .or. (index (string = smr_str, substring = 'R') > 0)

          if (.not. (my_task%smr_s     &
                     .or. my_task%smr_m     &
                     .or. my_task%smr_r)) then
            call pc_warning ('SETWORD: setword_parse_task_parms -- '    &
                             // 'GRID_GLOB: No SMR grid selected')
          end if

        case (IT_GRID_USER)
          smr_str = vlist (1)
          read (vlist (2), *, iostat = r_stat) my_task%coef_a
          read (vlist (3), *, iostat = r_stat) my_task%coef_b
          read (vlist (4), *, iostat = r_stat) my_task%const
          read (vlist (5), *, iostat = r_stat) my_task%exp_a
          read (vlist (6), *, iostat = r_stat) my_task%exp_b
          lr_str = trim(vlist (7))

          my_task%smr_s                                              &
            = (index (string = smr_str, substring = 's') > 0)        &
              .or. (index (string = smr_str, substring = 'S') > 0)
          my_task%smr_m                                              &
            = (index (string = smr_str, substring = 'm') > 0)        &
              .or. (index (string = smr_str, substring = 'M') > 0)
          my_task%smr_r                                              &
            = (index (string = smr_str, substring = 'r') > 0)        &
              .or. (index (string = smr_str, substring = 'R') > 0)

          if (.not. (my_task%smr_s     &
                     .or. my_task%smr_m     &
                     .or. my_task%smr_r)) then
            call pc_warning ('SETWORD: setword_parse_task_parms -- '    &
                             // 'GRID_USER: No SMR grid selected')
          end if

          call string_to_upper(lr_str)

          if(lr_str(1:1) == 'L') then
            my_task%handed = 'L'
            call grid_set_left_handed_transform         &
                (obj     = obj%task_grid (task_num),    &
                 xorigin = my_task%coef_a,              &
                 yorigin = my_task%coef_b,              &
                 angle   = my_task%const,               &
                 xwidth  = my_task%exp_a,               &
                 ywidth  = my_task%exp_b)
          else if(lr_str(1:1) == 'R') then
            my_task%handed = 'R'
            call grid_set_right_handed_transform        &
                (obj     = obj%task_grid (task_num),    &
                 xorigin = my_task%coef_a,              &
                 yorigin = my_task%coef_b,              &
                 angle   = my_task%const,               &
                 xwidth  = my_task%exp_a,               &
                 ywidth  = my_task%exp_b)
          else
            call pc_error('SETWORD: GRID_USER, invalid LR_HANDED value')
            my_task%handed = 'R'
          end if

        case (IT_LIN_COMB)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for LIN_COMB')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for LIN_COMB')
          read (vlist (3), *, iostat = r_stat) my_task%coef_a
          call setword_read_hdr (str   = vlist (4),                  &
                                 hdr   = my_task%hdr_b,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_B value for LIN_COMB')
          read (vlist (5), *, iostat = r_stat) my_task%coef_b
          read (vlist (6), *, iostat = r_stat) my_task%const

        case (IT_MAX)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for MAX')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for MAX')
          call setword_read_hdr (str   = vlist (3),                  &
                                 hdr   = my_task%hdr_b,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_B value for MAX')

        case (IT_MIN)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for MIN')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for MIN')
          call setword_read_hdr (str   = vlist (3),                  &
                                 hdr   = my_task%hdr_b,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_B value for MIN')

        case (IT_OFFSET_ABS)

        case (IT_OFFSET_RECALC)

        case (IT_OFFSET_SIGNED)

        case (IT_REG_INC)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for REG_INC')
          read (vlist (2), *, iostat = r_stat) my_task%coef_a
          read (vlist (3), *, iostat = r_stat) my_task%ri_limit
          read (vlist (4), *, iostat = r_stat) my_task%coef_b
          call setword_read_hdr (str   = vlist (5),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 0,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Opt value for REG_INC')

        case (IT_ROUND)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for ROUND')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for ROUND')

        case (IT_SINGLE_VALUE)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for SINGLE_VALUE')
          read (vlist (2), *, iostat = r_stat) my_task%const

        case (IT_START)
          r_stat = 0

        case (IT_STRETCH)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for STRETCH')
          read (vlist (2), *, iostat = r_stat) my_task%coef_a
          read (vlist (3), *, iostat = r_stat) my_task%coef_b
          read (vlist (4), *, iostat = r_stat) my_task%exp_a
          read (vlist (5), *, iostat = r_stat) my_task%exp_b
          call setword_read_hdr (str   = vlist (6),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 0,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for STRETCH')

          if (my_task%hdr_set <= 0) my_task%hdr_set = my_task%hdr_a
          if (mth_compare (my_task%coef_a, my_task%coef_b) /= 0) then
            my_task%const = (my_task%exp_b - my_task%exp_a)     &
                            / (my_task%coef_b - my_task%coef_a)
          else
            my_task%const = 0.0d0
          end if

        case (IT_SURV_GLOB)
          smr_str = vlist (1)
          my_task%smr_s                                              &
            = (index (string = smr_str, substring = 's') > 0)        &
              .or. (index (string = smr_str, substring = 'S') > 0)
          my_task%smr_m                                              &
            = (index (string = smr_str, substring = 'm') > 0)        &
              .or. (index (string = smr_str, substring = 'M') > 0)
          my_task%smr_r                                              &
            = (index (string = smr_str, substring = 'r') > 0)        &
              .or. (index (string = smr_str, substring = 'R') > 0)

          if (.not. (my_task%smr_s     &
                     .or. my_task%smr_m     &
                     .or. my_task%smr_r)) then
            call pc_warning ('SETWORD: setword_parse_task_parms -- '    &
                             // 'SURV_GLOB: No SMR grid selected')
          end if

        case (IT_SURV_USER)
          smr_str = vlist (1)
          read (vlist (2), *, iostat = r_stat) my_task%coef_a
          read (vlist (3), *, iostat = r_stat) my_task%coef_b
          read (vlist (4), *, iostat = r_stat) my_task%const
          read (vlist (5), *, iostat = r_stat) my_task%exp_a
          read (vlist (6), *, iostat = r_stat) my_task%exp_b
          lr_str = trim(vlist (7))

          my_task%smr_s                                              &
            = (index (string = smr_str, substring = 's') > 0)        &
              .or. (index (string = smr_str, substring = 'S') > 0)
          my_task%smr_m                                              &
            = (index (string = smr_str, substring = 'm') > 0)        &
              .or. (index (string = smr_str, substring = 'M') > 0)
          my_task%smr_r                                              &
            = (index (string = smr_str, substring = 'r') > 0)        &
              .or. (index (string = smr_str, substring = 'R') > 0)

          if (.not. (my_task%smr_s     &
                     .or. my_task%smr_m     &
                     .or. my_task%smr_r)) then
            call pc_warning ('SETWORD: setword_parse_task_parms -- '    &
                             // 'SURV_USER: No SMR grid selected')
          end if

          call string_to_upper(lr_str)

          if(lr_str(1:1) == 'L') then
            my_task%handed = 'L'
            call grid_set_left_handed_transform         &
                (obj     = obj%task_grid (task_num),    &
                 xorigin = my_task%coef_a,              &
                 yorigin = my_task%coef_b,              &
                 angle   = my_task%const,               &
                 xwidth  = my_task%exp_a,               &
                 ywidth  = my_task%exp_b)
          else if(lr_str(1:1) == 'R') then
            my_task%handed = 'R'
            call grid_set_right_handed_transform        &
                (obj     = obj%task_grid (task_num),    &
                 xorigin = my_task%coef_a,              &
                 yorigin = my_task%coef_b,              &
                 angle   = my_task%const,               &
                 xwidth  = my_task%exp_a,               &
                 ywidth  = my_task%exp_b)
          else
            call pc_error('SETWORD: SURV_USER, invalid LR_HANDED value')
            my_task%handed = 'R'
          end if

        case (IT_SWAP)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_Set value for SWAP')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_A value for SWAP')

        case (IT_TRUNCATE)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_To value for TRUNCATE')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_From value for TRUNCATE')

        case (IT_UNPACK)
          call setword_read_hdr (str   = vlist (1),                  &
                                 hdr   = my_task%hdr_set,            &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_To value for UNPACK')
          call setword_read_hdr (str   = vlist (2),                  &
                                 hdr   = my_task%hdr_a,              &
                                 first = 1,                          &
                                 nwih  = obj%nwih,                   &
                                 label = 'Hdr_From value for UNPACK')

          read (vlist (3), *, iostat = r_stat) my_task%num_shift

          if(my_task%num_shift < 0) then
            call pc_error('SETWORD: UNPACK: invalid NUM_SHIFT (negative)')
            my_task%num_shift = 0
          else if(my_task%num_shift > 15) then
            call pc_error('SETWORD: UNPACK: invalid NUM_SHIFT (> 15)')
            my_task%num_shift = 3
          end if

          read (vlist (4), *, iostat = r_stat) my_task%num_keep

          if(my_task%num_keep < 1) then
            call pc_error('SETWORD: UNPACK: invalid NUM_KEEP (< 1)')
            my_task%num_keep = 1
          else if(my_task%num_keep > 16 - my_task%num_shift) then
            call pc_error('SETWORD: UNPACK: invalid NUM_KEEP &
              &(> 16 - NUM_SHIFT)')
            my_task%num_keep = 16 - my_task%num_shift
          end if

          my_task%exp_recip  = 1.0 / (dble(10.0)**my_task%num_shift)
          my_task%dig_recip  = 1.0 / (dble(10.0)**my_task%num_keep)
          my_task%dig_factor =        dble(10.0)**my_task%num_keep

        case default
          r_stat = 0
          call pc_error ('setword_parse_task_parms: Could not interpret task ' &
                     // trim (obj%task_strs (task_num)),        &
                     task_id)
          status = -14
          my_task%id = 0
      end select

      if (r_stat /= 0) then
        call pc_error (msg1 = 'Error encountered reading parameters',    &
                       var1 = r_stat,                                    &
                       msg2 = '   Task ' // trim (obj%task_strs(task_num)))
      else
        obj%task (task_num) = my_task
      end if

    end do loop_thru_task_parms

  end subroutine setword_parse_task_parms


  !!--------------------------- setword_read_hdr ---------------------------!!
  !!--------------------------- setword_read_hdr ---------------------------!!
  !!--------------------------- setword_read_hdr ---------------------------!!

  subroutine setword_read_hdr (str, hdr, first, nwih, label)

    character (len = *), intent (in)    :: str
    integer,             intent (inout) :: hdr
    integer,             intent (in)    :: first
    integer,             intent (in)    :: nwih
    character (len = *), intent (in)    :: label

    integer :: ierr
    integer :: hdr_tmp

    read (str, *, iostat = ierr) hdr_tmp

    if (ierr /= 0) then
      call pc_error (trim (str) // " is not a valid header number"     &
                     // "  I/O Error ", ierr, " occurred at " // label)
    else if (hdr_tmp < scratch_num_50) then
      call pc_error (trim (str) // " cannot be less than ", scratch_num_50,   &
                     " for scratch " // label)
    else if ((hdr_tmp > scratch_num_1) .and. (hdr_tmp < first)) then
      call pc_error (trim (str) // " cannot be less than ", first,     &
                     " for " // label)
    else if (hdr_tmp > nwih) then
      call pc_error (trim (str) // " cannot be greater than ", nwih,   &
                     " for " // label)
    else
      hdr = hdr_tmp
    end if

  end subroutine setword_read_hdr


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


  subroutine setword (obj, ntr, hd, tr)

    type (setword_struct), intent (inout) :: obj
    integer,               intent (inout) :: ntr
    double precision,      intent (inout) :: hd (:,:)
    real,                  intent (in)    :: tr (:,:)

    double precision   :: delx
    double precision   :: dely
    double precision   :: theta
    double precision   :: hdr_dint, hdr_basis, hdr_upper
    double precision   :: hdr_set
    double precision   :: hdr_a
    double precision   :: hdr_b
    double precision   :: scratch (scratch_num_50:scratch_num_1)
    double precision   :: var1, var2
    integer            :: initial_ntr
    integer            :: t
    integer            :: trace
    type (task_struct) :: my_task

    initial_ntr = ntr

  loop_traces:   &
    do trace = 1, initial_ntr

      !---------menger added the if-test here for parallel mode ---------
      if(obj%parallel_mode /= 'YES') &
        hd (HDR_SEQUENCE, trace) = obj%seq  ! set the sequence for the trace

      ! - Clear the scratch area

      scratch = 0.0d0

      ! - Check the flag word

      if (obj%hdr_flag .and. (hd (obj%hdr_flag_val, trace) == 0.0d0)) then
        obj%seq = obj%seq + 1     ! added 2004-06-07
        cycle loop_traces
      end if

    loop_tasks:   &
      do t = 1, obj%num_tasks

        ! - Set task default parameters

        my_task = obj%task(t)

        if (my_task % hdr_a > 0) then
          hdr_a = hd (my_task%hdr_a, trace)
        else if (my_task % hdr_a <= scratch_num_1) then
          hdr_a = scratch (my_task % hdr_a)
        else
          hdr_a = 0.0
        end if

        if (my_task % hdr_b > 0) then
          hdr_b = hd (my_task%hdr_b, trace)
        else if (my_task % hdr_b <= scratch_num_1) then
          hdr_b = scratch (my_task % hdr_b)
        else
          hdr_b = 0.0
        end if

        if (my_task % hdr_set > 0) then
          hdr_set = hd (my_task%hdr_set, trace)
        else if (my_task % hdr_set <= scratch_num_1) then
          hdr_set = scratch (my_task % hdr_set)
        else
          hdr_set = 0.0
        end if

        select case (my_task%id)
        case (IT_ABS)
          hdr_set = abs (a = hdr_a)

        case (IT_AZMTH)
          delx = hd (HDR_RECEIVER_XLOC, trace) - hd (HDR_SOURCE_XLOC, trace) 
          dely = hd (HDR_RECEIVER_YLOC, trace) - hd (HDR_SOURCE_YLOC, trace) 
          if (dely == 0.0) then 
            if (delx > 0.0) then 
              theta = 90.0 
            else if (delx < 0.0) then 
              theta = 270.0 
            else 
              theta = 0.0 
            end if 
          else 
            theta = atan2 (delx,dely) * DEGREES_PER_RADIAN 
          end if 

          theta = mod (theta + my_task%const, 360.0d0) 
          if (theta < 0.0) theta = theta + 360.0d0 
          hdr_set = theta 

        case (IT_BIN_CENT)
          call grid_get_bin_centers                                &
                 (obj         = obj%grid,                          &
                  xgrid       = hd (HDR_MIDPOINT_XGRID, trace),    &
                  ygrid       = hd (HDR_MIDPOINT_YGRID, trace),    &
                  xbin_center = hd (HDR_MIDPOINT_XLOC,  trace),    &
                  ybin_center = hd (HDR_MIDPOINT_YLOC,  trace))

        case (IT_COPY)
          hdr_set = hdr_a

        case (IT_COUNTER)
          if (hdr_a /= my_task%const) then
            obj%task(t)%const  = hdr_a
            if (obj%seq > 1) then
              obj%task(t)%coef_a = my_task%coef_a + my_task%coef_b
            end if
          end if

          hdr_set = obj%task(t)%coef_a

        case (IT_END)
          exit loop_tasks

        case (IT_GEN_EXPR)
          if (my_task%exp_a == nint(my_task%exp_a)) then
               var1 = my_task%coef_a * (hdr_a ** nint(my_task%exp_a))
          else
               var1 = my_task%coef_a * (hdr_a ** my_task%exp_a)
          end if

          if (my_task%exp_b == nint(my_task%exp_b)) then
               var2 = my_task%coef_b * (hdr_b ** nint(my_task%exp_b))
          else
               var2 = my_task%coef_b * (hdr_b ** my_task%exp_b)
          end if

          hdr_set                                                        &
            = setword_gen_expr (func    = my_task%func,                  &
                                var1    = var1,                          &
                                oper    = my_task%oper,                  &
                                var2    = var2,                          &
                                const   = my_task%const,                 &
                                hdr_set = hdr_set)

        case (IT_GRID_GLOB)
          if (my_task%smr_s) then
            call grid_get_grid_coords                        &
                   (obj   = obj%grid,                        &
                    xloc  = hd (HDR_SOURCE_XLOC,  trace),    &
                    yloc  = hd (HDR_SOURCE_YLOC,  trace),    &
                    xgrid = hd (HDR_SOURCE_XGRID, trace),    &
                    ygrid = hd (HDR_SOURCE_YGRID, trace))
          end if

          if (my_task%smr_m) then
            call grid_get_grid_coords                          &
                   (obj   = obj%grid,                          &
                    xloc  = hd (HDR_MIDPOINT_XLOC,  trace),    &
                    yloc  = hd (HDR_MIDPOINT_YLOC,  trace),    &
                    xgrid = hd (HDR_MIDPOINT_XGRID, trace),    &
                    ygrid = hd (HDR_MIDPOINT_YGRID, trace))
          end if

          if (my_task%smr_r) then
            call grid_get_grid_coords                          &
                   (obj   = obj%grid,                          &
                    xloc  = hd (HDR_RECEIVER_XLOC,  trace),    &
                    yloc  = hd (HDR_RECEIVER_YLOC,  trace),    &
                    xgrid = hd (HDR_RECEIVER_XGRID, trace),    &
                    ygrid = hd (HDR_RECEIVER_YGRID, trace))
          end if

        case (IT_GRID_USER)
          if (my_task%smr_s) then
            call grid_get_grid_coords                        &
                   (obj   = obj%task_grid (t),               &
                    xloc  = hd (HDR_SOURCE_XLOC,  trace),    &
                    yloc  = hd (HDR_SOURCE_YLOC,  trace),    &
                    xgrid = hd (HDR_SOURCE_XGRID, trace),    &
                    ygrid = hd (HDR_SOURCE_YGRID, trace))
          end if

          if (my_task%smr_m) then
            call grid_get_grid_coords                          &
                   (obj   = obj%task_grid (t),                 &
                    xloc  = hd (HDR_MIDPOINT_XLOC,  trace),    &
                    yloc  = hd (HDR_MIDPOINT_YLOC,  trace),    &
                    xgrid = hd (HDR_MIDPOINT_XGRID, trace),    &
                    ygrid = hd (HDR_MIDPOINT_YGRID, trace))
          end if

          if (my_task%smr_r) then
            call grid_get_grid_coords                          &
                   (obj   = obj%task_grid (t),                 &
                    xloc  = hd (HDR_RECEIVER_XLOC,  trace),    &
                    yloc  = hd (HDR_RECEIVER_YLOC,  trace),    &
                    xgrid = hd (HDR_RECEIVER_XGRID, trace),    &
                    ygrid = hd (HDR_RECEIVER_YGRID, trace))
          end if

        case (IT_LIN_COMB)
          hdr_set = setword_gen_expr (func    = GE_IF_IDENT,              &
                                      var1    = my_task%coef_a * hdr_a,   &
                                      oper    = '+',                      &
                                      var2    = my_task%coef_b * hdr_b,   &
                                      const   = my_task%const,            &
                                      hdr_set = hdr_set)

        case (IT_MAX)
          hdr_set = max (a1 = hdr_a,  &
                         a2 = hdr_b)
        case (IT_MIN)
          hdr_set = min (a1 = hdr_a,  &
                         a2 = hdr_b)
        case (IT_OFFSET_ABS)
          hd (HDR_OFFSET, trace) = abs (a = hd (HDR_OFFSET, trace))

        case (IT_OFFSET_RECALC)
          delx = hd (HDR_RECEIVER_XLOC, trace) - hd (HDR_SOURCE_XLOC, trace) 
          dely = hd (HDR_RECEIVER_YLOC, trace) - hd (HDR_SOURCE_YLOC, trace) 
          hd (HDR_OFFSET, trace) = sqrt (delx ** 2 + dely ** 2)

        case (IT_OFFSET_SIGNED)
          hd (HDR_OFFSET, trace) = SIGN (a = hd (HDR_OFFSET, trace),         &
                                         b = hd (HDR_RECEIVER_XGRID, trace)  &
                                             - hd (HDR_SOURCE_XGRID, trace))
        case (IT_REG_INC)
          if (my_task%func >= my_task%ri_limit) then
            my_task%func = 1
            my_task%count = my_task%count + 1
            obj%task(t)%count = my_task%count  ! Preserve the update
          else
            my_task%func = my_task%func + 1
          end if

          obj%task(t)%func = my_task%func  ! Preserve the update

          ! Set the trace counter within the group, iff non-zero header word.
          if (my_task % hdr_a > 0) then
            hd (my_task%hdr_a, trace) = dble (my_task%func)
          else if (my_task % hdr_a <= scratch_num_1) then
            scratch (my_task % hdr_a) = dble (my_task%func)
          end if

          hdr_set = my_task%coef_a + dble (my_task%count) * my_task%coef_b

        case (IT_ROUND)
          hdr_set = anint (a = hdr_a)

        case (IT_SINGLE_VALUE)
          hdr_set = my_task%const

        case (IT_START)

        case (IT_STRETCH)
          hdr_set  = my_task%exp_a                 &
                     + (hdr_a - my_task%coef_a)    &
                       * my_task%const

        case (IT_SURV_GLOB)
          if (my_task%smr_s) then
            call grid_get_survey_coords                      &
                   (obj   = obj%grid,                        &
                    xgrid = hd (HDR_SOURCE_XGRID, trace),    &
                    ygrid = hd (HDR_SOURCE_YGRID, trace),    &
                    xloc  = hd (HDR_SOURCE_XLOC,  trace),    &
                    yloc  = hd (HDR_SOURCE_YLOC,  trace))
          end if

          if (my_task%smr_m) then
            call grid_get_survey_coords                        &
                   (obj   = obj%grid,                          &
                    xgrid = hd (HDR_MIDPOINT_XGRID, trace),    &
                    ygrid = hd (HDR_MIDPOINT_YGRID, trace),    &
                    xloc  = hd (HDR_MIDPOINT_XLOC,  trace),    &
                    yloc  = hd (HDR_MIDPOINT_YLOC,  trace))
          end if

          if (my_task%smr_r) then
            call grid_get_survey_coords                        &
                   (obj   = obj%grid,                          &
                    xgrid = hd (HDR_RECEIVER_XGRID, trace),    &
                    ygrid = hd (HDR_RECEIVER_YGRID, trace),    &
                    xloc  = hd (HDR_RECEIVER_XLOC,  trace),    &
                    yloc  = hd (HDR_RECEIVER_YLOC,  trace))
          end if

        case (IT_SURV_USER)
          if (my_task%smr_s) then
            call grid_get_survey_coords                        &
                   (obj   = obj%task_grid (t),               &
                    xgrid = hd (HDR_SOURCE_XGRID, trace),    &
                    ygrid = hd (HDR_SOURCE_YGRID, trace),    &
                    xloc  = hd (HDR_SOURCE_XLOC,  trace),    &
                    yloc  = hd (HDR_SOURCE_YLOC,  trace))
          end if

          if (my_task%smr_m) then
            call grid_get_survey_coords                          &
                   (obj   = obj%task_grid (t),                 &
                    xgrid = hd (HDR_MIDPOINT_XGRID, trace),    &
                    ygrid = hd (HDR_MIDPOINT_YGRID, trace),    &
                    xloc  = hd (HDR_MIDPOINT_XLOC,  trace),    &
                    yloc  = hd (HDR_MIDPOINT_YLOC,  trace))
          end if

          if (my_task%smr_r) then
            call grid_get_survey_coords                          &
                   (obj   = obj%task_grid (t),                 &
                    xgrid = hd (HDR_RECEIVER_XGRID, trace),    &
                    ygrid = hd (HDR_RECEIVER_YGRID, trace),    &
                    xloc  = hd (HDR_RECEIVER_XLOC,  trace),    &
                    yloc  = hd (HDR_RECEIVER_YLOC,  trace))
          end if

        case (IT_SWAP)
          if (my_task % hdr_a > 0) then
            hd (my_task%hdr_a, trace) = hdr_set
          else if (my_task % hdr_a <= scratch_num_1) then
            scratch (my_task % hdr_a) = hdr_set
          end if
          hdr_set = hdr_a

        case (IT_TRUNCATE)
          hdr_set = dint (a = hdr_a)

        case (IT_UNPACK)
          ! Example: extract 345, given hdr=1234567.8, num_shift=2, num_keep=3
          hdr_dint = abs(dint (a = hdr_a))                ! = 1234567
          hdr_basis = dint(hdr_dint  * my_task%exp_recip) ! = 12345
          hdr_upper = dint(hdr_basis * my_task%dig_recip) ! = 12
          hdr_upper = hdr_upper * my_task%dig_factor      ! = 12000
          hdr_set = hdr_basis - hdr_upper                 ! =   345

        case default
          ntr = FATAL_ERROR
          call pc_error (msg1 = 'Setword: Unknown task ID ',   &
                         var1 = my_task%id)
          exit loop_traces
        end select

        if (my_task % hdr_set > 0) then
          hd (my_task%hdr_set, trace) = hdr_set
        else if (my_task % hdr_set <= scratch_num_1) then
          scratch (my_task % hdr_set) = hdr_set
        end if

      end do loop_tasks

      obj%seq = obj%seq + 1

    end do loop_traces

    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      call setword_wrapup (obj)
    end if

  end subroutine setword

  !!--------------------------- setword_gen_expr ---------------------------!!
  !!--------------------------- setword_gen_expr ---------------------------!!
  !!--------------------------- setword_gen_expr ---------------------------!!

  function setword_gen_expr (func, var1, oper, var2, const, hdr_set)   &
                             result (out_val)

    integer,             intent (in) :: func
    double precision,    intent (in) :: var1
    character (len = *), intent (in) :: oper
    double precision,    intent (in) :: var2
    double precision,    intent (in) :: const
    double precision,    intent (in) :: hdr_set
    double precision                 :: out_val

    select case (oper)
      case ('+')    ; out_val = var1 + var2 + const
      case ('-')    ; out_val = var1 - var2 + const
      case ('/')    
        if (var2 == 0.0d0) then
          out_val = const
        else
          out_val = var1 / var2 + const
        end if
      case ('*')    ; out_val = var1 * var2 + const
      case default  ; out_val = var2 + const
        call pc_error ('setword_gen_expr: Unknown operation: ' // oper)
    end select

    select case (func)
      case (GE_IF_ABS)   ; out_val = abs   (a  = out_val)
      case (GE_IF_ACOS)  ; out_val = acos  (x  = out_val)
      case (GE_IF_ASIN)  ; out_val = asin  (x  = out_val)
      case (GE_IF_ATAN)  ; out_val = atan  (x  = out_val)
      case (GE_IF_COS)   ; out_val = cos   (x  = out_val)
      case (GE_IF_D2R)   ; out_val = out_val * RADIANS_PER_DEGREE
      case (GE_IF_IDENT) ; out_val = out_val            ! Straight assignment!!
      case (GE_IF_INT)   ; out_val = aint  (a  = out_val)
      case (GE_IF_MAX)   ; out_val = max   (a1 = out_val, a2 = hdr_set)
      case (GE_IF_MIN)   ; out_val = min   (a1 = out_val, a2 = hdr_set)
      case (GE_IF_NINT)  ; out_val = anint (a  = out_val)
      case (GE_IF_R2D)   ; out_val = out_val * DEGREES_PER_RADIAN
      case (GE_IF_SIGN)  ; out_val = sign  (a  = out_val, b  = hdr_set)
      case (GE_IF_SIN)   ; out_val = sin   (x  = out_val)
      case (GE_IF_SQRT)  ; out_val = sqrt  (x  = out_val)
      case (GE_IF_TAN)   ; out_val = tan   (x  = out_val)
      case default
    end select

  end function setword_gen_expr


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


  subroutine setword_wrapup (obj)

    type(setword_struct),intent(inout) :: obj       ! arguments

    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.

  end subroutine setword_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module setword_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!